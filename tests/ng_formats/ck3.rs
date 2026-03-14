use crate::support::{
    assert_slice_and_reader, push_f32_raw, push_f64_raw, push_field, push_i32, push_lexeme,
};
use jomini::{
    Error, Utf8Encoding,
    binary::LexemeId,
    binary::ng::{
        BinaryTokenFormat, BinaryValueFormat, ParserState, PdxVisitor, TokenResult, ValueResult,
    },
};
use serde::{Deserialize, de::Error as _};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct FieldId(u16);

impl FieldId {
    const fn new(index: u16) -> Self {
        Self(index)
    }

    const fn value(self) -> u16 {
        self.0
    }
}

fn resolve_name<'de, V>(
    field: FieldId,
    visitor: V,
    format: &Ck3Format,
) -> Result<ValueResult<V::Value, V>, Error>
where
    V: PdxVisitor<'de>,
{
    match format.fields.resolve_field(field) {
        Some(name) => Ok(ValueResult::Value(visitor.visit_str(name)?)),
        None => Err(Error::custom(format!(
            "unknown field token 0x{:x}",
            field.value()
        ))),
    }
}

fn utf8_scalar<'a>(data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
    match Utf8Encoding::decode(data) {
        Cow::Borrowed(x) => Ok(Cow::Borrowed(x)),
        Cow::Owned(x) => Ok(Cow::Owned(x)),
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Ck3Token<'a> {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Scalar(&'a [u8]),
    I32(i32),
    F32([u8; 4]),
    F64([u8; 8]),
}

#[derive(Clone, Copy, Default)]
struct Ck3Fields;

impl Ck3Fields {
    fn resolve_field(self, token: FieldId) -> Option<&'static str> {
        match token.value() {
            0x00ee => Some("version"),
            0x4001 => Some("value32"),
            0x4002 => Some("value64"),
            0x5000 => Some("alive_data"),
            0x5001 => Some("gold"),
            _ => None,
        }
    }
}

struct Ck3Format {
    fields: Ck3Fields,
    pending_field: Option<FieldId>,
    pending_version: bool,
    version: i32,
    scope_stack: Vec<FieldId>,
}

impl Default for Ck3Format {
    fn default() -> Self {
        Self {
            fields: Ck3Fields,
            pending_field: None,
            pending_version: false,
            version: 0,
            scope_stack: Vec::new(),
        }
    }
}

impl Ck3Format {
    const VERSION_FIELD: FieldId = FieldId::new(0x00ee);
    const ALIVE_DATA_FIELD: FieldId = FieldId::new(0x5000);
    const GOLD_FIELD: FieldId = FieldId::new(0x5001);

    fn modern(&self) -> bool {
        self.version >= 2
    }

    fn on_field(&mut self, field: FieldId) {
        self.pending_field = Some(field);
        self.pending_version = field == Self::VERSION_FIELD;
    }

    fn on_i32(&mut self, value: i32) {
        if self.pending_version {
            self.version = value;
            self.pending_version = false;
        }
        self.pending_field = None;
    }

    fn on_scalar_value(&mut self) {
        self.pending_version = false;
        self.pending_field = None;
    }

    fn on_open(&mut self) {
        if let Some(field) = self.pending_field.take() {
            self.scope_stack.push(field);
        }
        self.pending_version = false;
    }

    fn on_close(&mut self) {
        let _ = self.scope_stack.pop();
        self.pending_version = false;
        self.pending_field = None;
    }

    fn f64_uses_q49_15(&self) -> bool {
        self.pending_field == Some(Self::GOLD_FIELD)
            && self.scope_stack.last().copied() == Some(Self::ALIVE_DATA_FIELD)
    }
}

impl BinaryTokenFormat for Ck3Format {
    type Token<'a> = Ck3Token<'a>;

    fn next_token<'a>(
        &mut self,
        reader: &'a mut ParserState,
    ) -> Result<TokenResult<Self::Token<'a>>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(TokenResult::MoreData);
        };

        match id {
            LexemeId::OPEN => {
                self.on_open();
                cursor.consume();
                Ok(TokenResult::Token(Ck3Token::Open))
            }
            LexemeId::CLOSE => {
                self.on_close();
                cursor.consume();
                Ok(TokenResult::Token(Ck3Token::Close))
            }
            LexemeId::EQUAL => {
                cursor.consume();
                Ok(TokenResult::Token(Ck3Token::Equal))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(data) = cursor.read_len_prefixed() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                self.on_scalar_value();
                Ok(TokenResult::Token(Ck3Token::Scalar(data)))
            }
            LexemeId::I32 => {
                let Some(data) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                let value = i32::from_le_bytes(data);
                self.on_i32(value);
                Ok(TokenResult::Token(Ck3Token::I32(value)))
            }
            LexemeId::F32 => {
                let Some(data) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                self.on_scalar_value();
                Ok(TokenResult::Token(Ck3Token::F32(data)))
            }
            LexemeId::F64 => {
                let Some(data) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                self.on_scalar_value();
                Ok(TokenResult::Token(Ck3Token::F64(data)))
            }
            id => {
                cursor.consume();
                let field = FieldId::new(id.0);
                self.on_field(field);
                Ok(TokenResult::Token(Ck3Token::Field(field)))
            }
        }
    }

    fn on_open(&mut self) {
        Ck3Format::on_open(self);
    }

    fn on_close(&mut self) {
        Ck3Format::on_close(self);
    }
}

impl BinaryValueFormat for Ck3Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        utf8_scalar(data)
    }

    fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
            let Some(data) = cursor.read_len_prefixed() else {
                return Ok(ValueResult::MoreData(visitor));
            };
            cursor.consume();
            self.on_scalar_value();
            let value = match self.decode_scalar(data)? {
                Cow::Borrowed(x) => visitor.visit_str(x)?,
                Cow::Owned(x) => visitor.visit_string(x)?,
            };
            Ok(ValueResult::Value(value))
        } else if matches!(
            id,
            LexemeId::OPEN
                | LexemeId::CLOSE
                | LexemeId::EQUAL
                | LexemeId::I32
                | LexemeId::F32
                | LexemeId::F64
        ) {
            self.deserialize_any(reader, visitor)
        } else {
            cursor.consume();
            let field = FieldId::new(id.0);
            self.on_field(field);
            resolve_name(field, visitor, self)
        }
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        match id {
            LexemeId::OPEN => {
                self.on_open();
                cursor.consume();
                Ok(ValueResult::Open(visitor))
            }
            LexemeId::I32 => {
                let Some(data) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                let value = i32::from_le_bytes(data);
                self.on_i32(value);
                Ok(ValueResult::Value(visitor.visit_i32(value)?))
            }
            LexemeId::F32 => {
                let Some(raw) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                let value = if self.modern() {
                    i32::from_le_bytes(raw) as f32 / 1000.0
                } else {
                    f32::from_le_bytes(raw)
                };
                self.on_scalar_value();
                Ok(ValueResult::Value(visitor.visit_f32(value)?))
            }
            LexemeId::F64 => {
                let Some(raw) = cursor.read_bytes::<8>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                let value = if self.f64_uses_q49_15() {
                    let val = i64::from_le_bytes(raw) as f64 / 32768.0;
                    (val * 100_000.0).round() / 100_000.0
                } else if self.modern() {
                    i64::from_le_bytes(raw) as f64 / 100_000.0
                } else {
                    f64::from_le_bytes(raw)
                };
                self.on_scalar_value();
                Ok(ValueResult::Value(visitor.visit_f64(value)?))
            }
            id => {
                cursor.consume();
                let field = FieldId::new(id.0);
                self.on_field(field);
                resolve_name(field, visitor, self)
            }
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
struct Ck3Data {
    version: i32,
    value32: f32,
    value64: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Ck3IgnoredVersionData {
    value32: f32,
    value64: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Ck3AliveData {
    gold: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Ck3ScopedGoldData {
    version: i32,
    gold: f64,
    alive_data: Ck3AliveData,
}

#[test]
fn ck3_version_can_switch_float_decoding_without_changing_widths() {

    let mut legacy = Vec::new();
    push_field(&mut legacy, 0x00ee);
    push_lexeme(&mut legacy, LexemeId::EQUAL);
    push_i32(&mut legacy, 1);
    push_field(&mut legacy, 0x4001);
    push_lexeme(&mut legacy, LexemeId::EQUAL);
    push_f32_raw(&mut legacy, 1.5f32.to_le_bytes());
    push_field(&mut legacy, 0x4002);
    push_lexeme(&mut legacy, LexemeId::EQUAL);
    push_f64_raw(&mut legacy, 2.25f64.to_le_bytes());

    let legacy_actual: Ck3Data = assert_slice_and_reader(&legacy, Ck3Format::default);
    assert_eq!(legacy_actual.value32, 1.5);
    assert_eq!(legacy_actual.value64, 2.25);

    let mut modern = Vec::new();
    push_field(&mut modern, 0x00ee);
    push_lexeme(&mut modern, LexemeId::EQUAL);
    push_i32(&mut modern, 2);
    push_field(&mut modern, 0x4001);
    push_lexeme(&mut modern, LexemeId::EQUAL);
    push_f32_raw(&mut modern, 3140i32.to_le_bytes());
    push_field(&mut modern, 0x4002);
    push_lexeme(&mut modern, LexemeId::EQUAL);
    push_f64_raw(&mut modern, 271_828i64.to_le_bytes());

    let modern_actual: Ck3Data = assert_slice_and_reader(&modern, Ck3Format::default);
    assert_eq!(modern_actual.value32, 3.14);
    assert_eq!(modern_actual.value64, 2.71828);
}

#[test]
fn ck3_ignored_version_still_switches_float_decoding() {

    let mut data = Vec::new();
    push_field(&mut data, 0x00ee);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 2);
    push_field(&mut data, 0x4001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 3140i32.to_le_bytes());
    push_field(&mut data, 0x4002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, 271_828i64.to_le_bytes());

    let actual: Ck3IgnoredVersionData =
        assert_slice_and_reader(&data, Ck3Format::default);
    assert_eq!(actual.value32, 3.14);
    assert_eq!(actual.value64, 2.71828);
}

#[test]
fn ck3_alive_data_gold_can_use_q49_15_while_other_gold_uses_decimal_fixed_point() {

    let mut data = Vec::new();
    push_field(&mut data, 0x00ee);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 2);

    push_field(&mut data, 0x5001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, 123_456i64.to_le_bytes());

    push_field(&mut data, 0x5000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_field(&mut data, 0x5001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (2i64 * 32768).to_le_bytes());
    push_lexeme(&mut data, LexemeId::CLOSE);

    let actual: Ck3ScopedGoldData = assert_slice_and_reader(&data, Ck3Format::default);
    assert_eq!(actual.gold, 1.23456);
    assert_eq!(actual.alive_data.gold, 2.0);
}
