use crate::support::{
    assert_slice_and_reader, push_f32_raw, push_f64_raw, push_field, push_i32, push_lexeme,
};
use jomini::{
    Error, Utf8Encoding,
    binary::LexemeId,
    binary::ng::{
        BinaryConfig, BinaryTokenFormat, BinaryValueFormat, FieldId, FieldResolver, ParserState,
        PdxVisitor, StreamToken, StructureKind, TokenResult, ValueResult,
    },
};
use serde::{Deserialize, de::Error as _};
use std::borrow::Cow;

fn resolve_name<'de, V, RES>(
    field: FieldId,
    visitor: V,
    config: &BinaryConfig<RES>,
) -> Result<ValueResult<V::Value, V>, Error>
where
    V: PdxVisitor<'de>,
    RES: FieldResolver,
{
    match config.field_resolver().resolve_field(field) {
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

impl StreamToken for Ck3Token<'_> {
    fn structure(&self) -> StructureKind {
        match self {
            Ck3Token::Open => StructureKind::Open,
            Ck3Token::Close => StructureKind::Close,
            Ck3Token::Equal => StructureKind::Equal,
            _ => StructureKind::Value,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct Ck3Fields;

impl FieldResolver for Ck3Fields {
    fn resolve_field(&self, token: FieldId) -> Option<&str> {
        match token.value() {
            0x00ee => Some("version"),
            0x4001 => Some("value32"),
            0x4002 => Some("value64"),
            _ => None,
        }
    }
}

#[derive(Default)]
struct Ck3Format {
    pending_version: bool,
    version: i32,
}

impl Ck3Format {
    fn modern(&self) -> bool {
        self.version >= 2
    }
}

impl BinaryTokenFormat for Ck3Format {
    type Token<'a> = Ck3Token<'a>;

    fn next_token<'a>(
        &mut self,
        reader: &'a mut ParserState,
    ) -> Result<TokenResult<Self::Token<'a>>, Error> {
        let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
            return Ok(TokenResult::MoreData);
        };
        let id = LexemeId::new(u16::from_le_bytes(id_bytes));

        match id {
            LexemeId::OPEN => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Ck3Token::Open))
            }
            LexemeId::CLOSE => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Ck3Token::Close))
            }
            LexemeId::EQUAL => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Ck3Token::Equal))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(header) = reader.peek_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                let len = u16::from_le_bytes([header[2], header[3]]);
                if reader.len() < 4 + len as usize {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(4) };
                let data = reader.read_slice(len).ok_or_else(Error::eof)?;
                Ok(TokenResult::Token(Ck3Token::Scalar(data)))
            }
            LexemeId::I32 => {
                if reader.len() < 6 {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(2) };
                let data = reader.read_exact::<4>()?;
                let value = i32::from_le_bytes(data);
                if self.pending_version {
                    self.version = value;
                    self.pending_version = false;
                }
                Ok(TokenResult::Token(Ck3Token::I32(value)))
            }
            LexemeId::F32 => {
                if reader.len() < 6 {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(2) };
                let data = reader.read_exact::<4>()?;
                Ok(TokenResult::Token(Ck3Token::F32(data)))
            }
            LexemeId::F64 => {
                if reader.len() < 10 {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(2) };
                let data = reader.read_exact::<8>()?;
                Ok(TokenResult::Token(Ck3Token::F64(data)))
            }
            id => {
                unsafe { reader.consume(2) };
                self.pending_version = id.0 == 0x00ee;
                Ok(TokenResult::Token(Ck3Token::Field(FieldId::new(id.0))))
            }
        }
    }
}

impl BinaryValueFormat for Ck3Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        utf8_scalar(data)
    }

    fn deserialize_identifier<'de, V: PdxVisitor<'de>, RES: FieldResolver>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig<RES>,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        let id = LexemeId::new(u16::from_le_bytes(id_bytes));
        if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
            let Some(header) = reader.peek_bytes::<4>() else {
                return Ok(ValueResult::MoreData(visitor));
            };
            let len = u16::from_le_bytes([header[2], header[3]]);
            if reader.len() < 4 + len as usize {
                return Ok(ValueResult::MoreData(visitor));
            }
            unsafe { reader.consume(4) };
            let data = reader.read_slice(len).ok_or_else(Error::eof)?;
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
            self.deserialize_any(reader, visitor, config)
        } else {
            unsafe { reader.consume(2) };
            self.pending_version = id.0 == 0x00ee;
            resolve_name(FieldId::new(id.0), visitor, config)
        }
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>, RES: FieldResolver>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig<RES>,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        let id = LexemeId::new(u16::from_le_bytes(id_bytes));
        match id {
            LexemeId::OPEN => {
                unsafe { reader.consume(2) };
                Ok(ValueResult::Open(visitor))
            }
            LexemeId::I32 => {
                if reader.len() < 6 {
                    return Ok(ValueResult::MoreData(visitor));
                }
                unsafe { reader.consume(2) };
                let value = i32::from_le_bytes(reader.read_exact::<4>()?);
                if self.pending_version {
                    self.version = value;
                    self.pending_version = false;
                }
                Ok(ValueResult::Value(visitor.visit_i32(value)?))
            }
            LexemeId::F32 => {
                if reader.len() < 6 {
                    return Ok(ValueResult::MoreData(visitor));
                }
                unsafe { reader.consume(2) };
                let raw = reader.read_exact::<4>()?;
                let value = if self.modern() {
                    i32::from_le_bytes(raw) as f32 / 1000.0
                } else {
                    f32::from_le_bytes(raw)
                };
                Ok(ValueResult::Value(visitor.visit_f32(value)?))
            }
            LexemeId::F64 => {
                if reader.len() < 10 {
                    return Ok(ValueResult::MoreData(visitor));
                }
                unsafe { reader.consume(2) };
                let raw = reader.read_exact::<8>()?;
                let value = if self.modern() {
                    i64::from_le_bytes(raw) as f64 / 100_000.0
                } else {
                    f64::from_le_bytes(raw)
                };
                Ok(ValueResult::Value(visitor.visit_f64(value)?))
            }
            id => {
                unsafe { reader.consume(2) };
                resolve_name(FieldId::new(id.0), visitor, config)
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

    let legacy_actual: Ck3Data = assert_slice_and_reader(&legacy, Ck3Format::default, Ck3Fields);
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

    let modern_actual: Ck3Data = assert_slice_and_reader(&modern, Ck3Format::default, Ck3Fields);
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
        assert_slice_and_reader(&data, Ck3Format::default, Ck3Fields);
    assert_eq!(actual.value32, 3.14);
    assert_eq!(actual.value64, 2.71828);
}
