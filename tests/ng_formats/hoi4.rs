use crate::support::{
    assert_slice_and_reader, push_bool, push_f32_raw, push_f64_raw, push_field, push_i32,
    push_lexeme, push_quoted, push_u32, push_unquoted,
};
use jomini::{
    binary::ng::{
        BinaryTokenFormat, BinaryValueFormat, ParserState, PdxVisitor, TokenReader, TokenResult,
        ValueResult,
    },
    binary::LexemeId,
    Error, Utf8Encoding,
};
use serde::{de::Error as _, Deserialize};
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

fn utf8_scalar<'a>(data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
    match Utf8Encoding::decode(data) {
        Cow::Borrowed(x) => Ok(Cow::Borrowed(x)),
        Cow::Owned(x) => Ok(Cow::Owned(x)),
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Hoi4Token<'a> {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    Quoted(&'a [u8]),
    Unquoted(&'a [u8]),
    LegacyFixedPoint([u8; 4]),
    ModernFixedPoint([u8; 8]),
    F64(f64),
}

#[derive(Clone, Copy, Default)]
struct Hoi4Fields;

impl Hoi4Fields {
    fn resolve_field(self, token: FieldId) -> Option<&'static str> {
        match token.value() {
            0x349d => Some("save_version"),
            0x3001 => Some("metric"),
            0x3002 => Some("weight"),
            0x3003 => Some("flag"),
            0x3004 => Some("count"),
            0x3005 => Some("big_count"),
            0x3006 => Some("delta64"),
            0x3007 => Some("label"),
            0x3008 => Some("word"),
            _ => None,
        }
    }
}

struct Hoi4Format {
    fields: Hoi4Fields,
    pending_save_version: bool,
    save_version: i32,
}

impl Default for Hoi4Format {
    fn default() -> Self {
        Self {
            fields: Hoi4Fields,
            pending_save_version: false,
            save_version: 0,
        }
    }
}

impl Hoi4Format {
    fn decode_legacy_f64(raw: [u8; 4]) -> f64 {
        i32::from_le_bytes(raw) as f64 / 1000.0
    }

    fn decode_modern_f64(raw: [u8; 8]) -> f64 {
        i64::from_le_bytes(raw) as f64 / 100_000.0
    }

    fn decode_legacy_f32(raw: [u8; 4]) -> f32 {
        Self::decode_legacy_f64(raw) as f32
    }

    fn decode_modern_f32(raw: [u8; 8]) -> f32 {
        Self::decode_modern_f64(raw) as f32
    }

    fn decode_f64(raw: [u8; 8]) -> f64 {
        let val = i64::from_le_bytes(raw) as f64 / 32768.0;
        (val * 100_000.0).floor() / 100_000.0
    }

    fn modern_f32(&self) -> bool {
        self.save_version >= 30
    }

    fn track_field(&mut self, field: FieldId) {
        self.pending_save_version = field.value() == 0x349d;
    }

    fn visit_i32(&mut self, raw: [u8; 4]) -> i32 {
        let value = i32::from_le_bytes(raw);
        if self.pending_save_version {
            self.save_version = value;
            self.pending_save_version = false;
        }
        value
    }

    fn resolve_name<'de, V>(
        &self,
        field: FieldId,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error>
    where
        V: PdxVisitor<'de>,
    {
        match self.fields.resolve_field(field) {
            Some(name) => Ok(ValueResult::Value(visitor.visit_str(name)?)),
            None => Err(Error::custom(format!(
                "unknown field token 0x{:x}",
                field.value()
            ))),
        }
    }

    fn parse_str_token<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };

        if !matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
            return self.deserialize_any(reader, visitor);
        }

        let Some(data) = cursor.read_len_prefixed() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        let value = match self.decode_scalar(data)? {
            Cow::Borrowed(x) => visitor.visit_str(x)?,
            Cow::Owned(x) => visitor.visit_string(x)?,
        };
        Ok(ValueResult::Value(value))
    }
}

impl BinaryTokenFormat for Hoi4Format {
    type Token<'a> = Hoi4Token<'a>;

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
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::Open))
            }
            LexemeId::CLOSE => {
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::Close))
            }
            LexemeId::EQUAL => {
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::Equal))
            }
            LexemeId::BOOL => {
                let Some(bytes) = cursor.read_bytes::<1>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::Bool(bytes[0] != 0)))
            }
            LexemeId::U32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::U32(u32::from_le_bytes(
                    bytes,
                ))))
            }
            LexemeId::U64 => {
                let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::U64(u64::from_le_bytes(
                    bytes,
                ))))
            }
            LexemeId::I32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::I32(self.visit_i32(bytes))))
            }
            LexemeId::I64 => {
                let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::I64(i64::from_le_bytes(
                    bytes,
                ))))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(data) = cursor.read_len_prefixed() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                if id == LexemeId::QUOTED {
                    Ok(TokenResult::Token(Hoi4Token::Quoted(data)))
                } else {
                    Ok(TokenResult::Token(Hoi4Token::Unquoted(data)))
                }
            }
            LexemeId::F32 => {
                if self.modern_f32() {
                    let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                        return Ok(TokenResult::MoreData);
                    };
                    cursor.consume();
                    Ok(TokenResult::Token(Hoi4Token::ModernFixedPoint(bytes)))
                } else {
                    let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                        return Ok(TokenResult::MoreData);
                    };
                    cursor.consume();
                    Ok(TokenResult::Token(Hoi4Token::LegacyFixedPoint(bytes)))
                }
            }
            LexemeId::F64 => {
                let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Hoi4Token::F64(Self::decode_f64(bytes))))
            }
            id => {
                cursor.consume();
                let field = FieldId::new(id.0);
                self.track_field(field);
                Ok(TokenResult::Token(Hoi4Token::Field(field)))
            }
        }
    }
}

impl BinaryValueFormat for Hoi4Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        utf8_scalar(data)
    }

    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        self.parse_str_token(reader, visitor)
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
            self.parse_str_token(reader, visitor)
        } else if matches!(
            id,
            LexemeId::OPEN
                | LexemeId::CLOSE
                | LexemeId::EQUAL
                | LexemeId::BOOL
                | LexemeId::U32
                | LexemeId::U64
                | LexemeId::I32
                | LexemeId::I64
                | LexemeId::F32
                | LexemeId::F64
        ) {
            self.deserialize_any(reader, visitor)
        } else {
            let field = FieldId::new(id.0);
            cursor.consume();
            self.track_field(field);
            self.resolve_name(field, visitor)
        }
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        match self.next_token(reader)? {
            TokenResult::MoreData => Ok(ValueResult::MoreData(visitor)),
            TokenResult::Token(Hoi4Token::Open) => Ok(ValueResult::Open(visitor)),
            TokenResult::Token(Hoi4Token::Close | Hoi4Token::Equal) => {
                Err(Error::custom("unexpected structural token"))
            }
            TokenResult::Token(Hoi4Token::Bool(x)) => {
                Ok(ValueResult::Value(visitor.visit_bool(x)?))
            }
            TokenResult::Token(Hoi4Token::U32(x)) => Ok(ValueResult::Value(visitor.visit_u32(x)?)),
            TokenResult::Token(Hoi4Token::U64(x)) => Ok(ValueResult::Value(visitor.visit_u64(x)?)),
            TokenResult::Token(Hoi4Token::I32(x)) => Ok(ValueResult::Value(visitor.visit_i32(x)?)),
            TokenResult::Token(Hoi4Token::I64(x)) => Ok(ValueResult::Value(visitor.visit_i64(x)?)),
            TokenResult::Token(Hoi4Token::Quoted(data) | Hoi4Token::Unquoted(data)) => {
                let value = match self.decode_scalar(data)? {
                    Cow::Borrowed(x) => visitor.visit_str(x)?,
                    Cow::Owned(x) => visitor.visit_string(x)?,
                };
                Ok(ValueResult::Value(value))
            }
            TokenResult::Token(Hoi4Token::LegacyFixedPoint(raw)) => Ok(ValueResult::Value(
                visitor.visit_f32(Self::decode_legacy_f32(raw))?,
            )),
            TokenResult::Token(Hoi4Token::ModernFixedPoint(raw)) => Ok(ValueResult::Value(
                visitor.visit_f32(Self::decode_modern_f32(raw))?,
            )),
            TokenResult::Token(Hoi4Token::F64(x)) => Ok(ValueResult::Value(visitor.visit_f64(x)?)),
            TokenResult::Token(Hoi4Token::Field(id)) => self.resolve_name(id, visitor),
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
struct Hoi4Data {
    save_version: i32,
    metric: f32,
    weight: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Hoi4IgnoredVersionData {
    metric: f32,
    weight: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Hoi4CommonData {
    flag: bool,
    count: u32,
    big_count: u64,
    delta64: i64,
    label: String,
    word: String,
}

fn push_u64(buf: &mut Vec<u8>, value: u64) {
    push_lexeme(buf, LexemeId::U64);
    buf.extend_from_slice(&value.to_le_bytes());
}

fn push_i64(buf: &mut Vec<u8>, value: i64) {
    push_lexeme(buf, LexemeId::I64);
    buf.extend_from_slice(&value.to_le_bytes());
}

fn hoi4_legacy_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x349d);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 29);
    push_field(&mut data, 0x3001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x3002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, 123_456i64.to_le_bytes());
    data
}

fn hoi4_modern_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x349d);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 30);
    push_field(&mut data, 0x3001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::F32);
    data.extend_from_slice(&123_456i64.to_le_bytes());
    push_field(&mut data, 0x3002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, 123_456i64.to_le_bytes());
    data
}

fn hoi4_common_tokens_fixture() -> Vec<u8> {
    let mut data = hoi4_modern_fixture();
    push_field(&mut data, 0x3003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_bool(&mut data, true);
    push_field(&mut data, 0x3004);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_u32(&mut data, 42);
    push_field(&mut data, 0x3005);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_u64(&mut data, 1u64 << 40);
    push_field(&mut data, 0x3006);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i64(&mut data, -9_876_543_210);
    push_field(&mut data, 0x3007);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_quoted(&mut data, b"quoted");
    push_field(&mut data, 0x3008);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"plain");
    data
}

#[test]
fn hoi4_save_version_29_uses_legacy_f32_layout() {
    let data = hoi4_legacy_fixture();

    let actual: Hoi4Data = assert_slice_and_reader(&data, Hoi4Format::default);
    assert_eq!(actual.save_version, 29);
    assert_eq!(actual.metric, 1.234);
    assert!((actual.weight - 3.76757).abs() < 1e-9);
}

#[test]
fn hoi4_save_version_30_switches_to_modern_f32_layout() {
    let data = hoi4_modern_fixture();

    let actual: Hoi4Data = assert_slice_and_reader(&data, Hoi4Format::default);
    assert_eq!(actual.save_version, 30);
    assert_eq!(actual.metric, 1.23456);
    assert!((actual.weight - 3.76757).abs() < 1e-9);
}

#[test]
fn hoi4_ignored_save_version_still_switches_f32_layout_at_30() {
    let data = hoi4_modern_fixture();

    let actual: Hoi4IgnoredVersionData = assert_slice_and_reader(&data, Hoi4Format::default);
    assert_eq!(actual.metric, 1.23456);
    assert!((actual.weight - 3.76757).abs() < 1e-9);
}

#[test]
fn hoi4_deserializes_common_scalars_and_strings() {
    let data = hoi4_common_tokens_fixture();

    let actual: Hoi4CommonData = assert_slice_and_reader(&data, Hoi4Format::default);
    assert_eq!(
        actual,
        Hoi4CommonData {
            flag: true,
            count: 42,
            big_count: 1u64 << 40,
            delta64: -9_876_543_210,
            label: String::from("quoted"),
            word: String::from("plain"),
        }
    );
}

#[test]
fn hoi4_token_stream_switches_f32_variant_at_save_version_30() {
    let data = hoi4_modern_fixture();
    let mut reader = TokenReader::from_slice(&data, Hoi4Format::default());

    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x349d))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::I32(30));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3001))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::ModernFixedPoint(123_456i64.to_le_bytes())
    );
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3002))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::F64(Hoi4Format::decode_f64(123_456i64.to_le_bytes()))
    );
}

#[test]
fn hoi4_token_stream_covers_common_scalar_variants() {
    let data = hoi4_common_tokens_fixture();
    let mut reader = TokenReader::from_slice(&data, Hoi4Format::default());

    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x349d))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::I32(30));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3001))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::ModernFixedPoint(123_456i64.to_le_bytes())
    );
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3002))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::F64(Hoi4Format::decode_f64(123_456i64.to_le_bytes()))
    );
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3003))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Bool(true));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3004))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::U32(42));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3005))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::U64(1u64 << 40));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3006))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::I64(-9_876_543_210));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3007))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Quoted(b"quoted"));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3008))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Unquoted(b"plain"));
}
