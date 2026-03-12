use crate::support::{
    assert_slice_and_reader, push_f32_raw, push_f64_raw, push_field, push_i32, push_lexeme,
};
use jomini::{
    Error, Utf8Encoding,
    binary::LexemeId,
    binary::ng::{
        BinaryConfig, BinaryTokenFormat, BinaryValueFormat, FieldId, FieldResolver, ParserState,
        PdxVisitor, TokenReader, TokenResult, ValueResult,
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
enum Hoi4Token<'a> {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Scalar(&'a [u8]),
    I32(i32),
    LegacyF32([u8; 4]),
    ModernF32([u8; 8]),
    F64([u8; 8]),
}

#[derive(Clone, Copy, Default)]
struct Hoi4Fields;

impl FieldResolver for Hoi4Fields {
    fn resolve_field(&self, token: FieldId) -> Option<&str> {
        match token.value() {
            0x349d => Some("save_version"),
            0x3001 => Some("metric"),
            0x3002 => Some("weight"),
            _ => None,
        }
    }
}

#[derive(Default)]
struct Hoi4Format {
    pending_save_version: bool,
    save_version: i32,
}

impl Hoi4Format {
    fn modern_f32(&self) -> bool {
        self.save_version > 30
    }

    fn decode_f64(raw: [u8; 8]) -> f64 {
        let val = i64::from_le_bytes(raw) as f64 / 32768.0;
        (val * 100_000.0).floor() / 100_000.0
    }
}

impl BinaryTokenFormat for Hoi4Format {
    type Token<'a> = Hoi4Token<'a>;

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
                Ok(TokenResult::Token(Hoi4Token::Open))
            }
            LexemeId::CLOSE => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Hoi4Token::Close))
            }
            LexemeId::EQUAL => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Hoi4Token::Equal))
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
                Ok(TokenResult::Token(Hoi4Token::Scalar(data)))
            }
            LexemeId::I32 => {
                if reader.len() < 6 {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(2) };
                let data = reader.read_exact::<4>()?;
                let value = i32::from_le_bytes(data);
                if self.pending_save_version {
                    self.save_version = value;
                    self.pending_save_version = false;
                }
                Ok(TokenResult::Token(Hoi4Token::I32(value)))
            }
            LexemeId::F32 => {
                if self.modern_f32() {
                    if reader.len() < 10 {
                        return Ok(TokenResult::MoreData);
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<8>()?;
                    Ok(TokenResult::Token(Hoi4Token::ModernF32(data)))
                } else {
                    if reader.len() < 6 {
                        return Ok(TokenResult::MoreData);
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<4>()?;
                    Ok(TokenResult::Token(Hoi4Token::LegacyF32(data)))
                }
            }
            LexemeId::F64 => {
                if reader.len() < 10 {
                    return Ok(TokenResult::MoreData);
                }
                unsafe { reader.consume(2) };
                let data = reader.read_exact::<8>()?;
                Ok(TokenResult::Token(Hoi4Token::F64(data)))
            }
            id => {
                unsafe { reader.consume(2) };
                self.pending_save_version = id.0 == 0x349d;
                Ok(TokenResult::Token(Hoi4Token::Field(FieldId::new(id.0))))
            }
        }
    }
}

impl BinaryValueFormat for Hoi4Format {
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
            self.pending_save_version = id.0 == 0x349d;
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
                if self.pending_save_version {
                    self.save_version = value;
                    self.pending_save_version = false;
                }
                Ok(ValueResult::Value(visitor.visit_i32(value)?))
            }
            LexemeId::F32 => {
                if self.modern_f32() {
                    if reader.len() < 10 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                } else if reader.len() < 6 {
                    return Ok(ValueResult::MoreData(visitor));
                }
                unsafe { reader.consume(2) };
                let value = if self.modern_f32() {
                    i64::from_le_bytes(reader.read_exact::<8>()?) as f32 / 100_000.0
                } else {
                    i32::from_le_bytes(reader.read_exact::<4>()?) as f32 / 1000.0
                };
                Ok(ValueResult::Value(visitor.visit_f32(value)?))
            }
            LexemeId::F64 => {
                if reader.len() < 10 {
                    return Ok(ValueResult::MoreData(visitor));
                }
                unsafe { reader.consume(2) };
                Ok(ValueResult::Value(
                    visitor.visit_f64(Self::decode_f64(reader.read_exact::<8>()?))?,
                ))
            }
            id => {
                unsafe { reader.consume(2) };
                resolve_name(FieldId::new(id.0), visitor, config)
            }
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

fn hoi4_legacy_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x349d);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 30);
    push_field(&mut data, 0x3001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x3002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (2i64 * 32768).to_le_bytes());
    data
}

fn hoi4_modern_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x349d);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 31);
    push_field(&mut data, 0x3001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::F32);
    data.extend_from_slice(&123_456i64.to_le_bytes());
    push_field(&mut data, 0x3002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (3i64 * 32768).to_le_bytes());
    data
}

#[test]
fn hoi4_save_version_30_uses_legacy_f32_layout() {
    let data = hoi4_legacy_fixture();

    let actual: Hoi4Data = assert_slice_and_reader(&data, Hoi4Format::default, Hoi4Fields);
    assert_eq!(actual.save_version, 30);
    assert_eq!(actual.metric, 1.234);
    assert_eq!(actual.weight, 2.0);
}

#[test]
fn hoi4_save_version_31_switches_f32_layout() {
    let data = hoi4_modern_fixture();

    let actual: Hoi4Data = assert_slice_and_reader(&data, Hoi4Format::default, Hoi4Fields);
    assert_eq!(actual.save_version, 31);
    assert_eq!(actual.metric, 1.23456);
    assert_eq!(actual.weight, 3.0);
}

#[test]
fn hoi4_ignored_save_version_still_switches_f32_layout() {
    let data = hoi4_modern_fixture();

    let actual: Hoi4IgnoredVersionData =
        assert_slice_and_reader(&data, Hoi4Format::default, Hoi4Fields);
    assert_eq!(actual.metric, 1.23456);
    assert_eq!(actual.weight, 3.0);
}

#[test]
fn hoi4_token_stream_switches_f32_variant_after_save_version() {
    let data = hoi4_modern_fixture();
    let mut reader = TokenReader::from_slice(&data, Hoi4Format::default());

    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x349d))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::I32(31));
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::Field(FieldId::new(0x3001))
    );
    assert_eq!(reader.read_token().unwrap(), Hoi4Token::Equal);
    assert_eq!(
        reader.read_token().unwrap(),
        Hoi4Token::ModernF32(123_456i64.to_le_bytes())
    );
}
