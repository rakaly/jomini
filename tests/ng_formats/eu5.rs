use crate::support::{
    assert_slice_and_reader, push_f64_raw, push_field, push_fixed5_i16, push_lexeme,
    push_lookup_u8, push_unquoted,
};
use jomini::{
    Error, Utf8Encoding,
    binary::ng::{
        BinaryTokenFormat, BinaryValueFormat, ParserState, PdxVisitor, TokenReader, TokenResult,
        ValueResult,
    },
    binary::{FailedResolveStrategy, LexemeId},
};
use serde::{Deserialize, de::Error as _};
use std::{borrow::Cow, collections::HashMap};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LookupIndex(u32);

impl LookupIndex {
    const fn new(index: u32) -> Self {
        Self(index)
    }

    const fn value(self) -> u32 {
        self.0
    }
}

fn resolve_lookup<'de, V>(
    lookup: Option<&str>,
    index: LookupIndex,
    strategy: FailedResolveStrategy,
    visitor: V,
) -> Result<ValueResult<V::Value, V>, Error>
where
    V: PdxVisitor<'de>,
{
    match lookup {
        Some(value) => Ok(ValueResult::Value(visitor.visit_str(value)?)),
        None => match strategy {
            FailedResolveStrategy::Error => Err(Error::custom(format!(
                "unknown lookup token {}",
                index.value()
            ))),
            FailedResolveStrategy::Stringify => Ok(ValueResult::Value(
                visitor.visit_string(index.value().to_string())?,
            )),
            FailedResolveStrategy::Ignore => Ok(ValueResult::Value(
                visitor.visit_str("__internal_identifier_ignore")?,
            )),
        },
    }
}

fn fixed5_raw(reader: &mut ParserState, id: LexemeId) -> Result<i64, Error> {
    let offset = id.0 - LexemeId::FIXED5_ZERO.0;
    let is_negative = offset > 7;
    let byte_count = offset - (is_negative as u16 * 7);
    if byte_count == 0 {
        return Ok(0);
    }

    let data = reader.read_slice(byte_count).ok_or_else(Error::eof)?;
    let mut buf = [0u8; 8];
    buf[..byte_count as usize].copy_from_slice(data);
    let raw = u64::from_le_bytes(buf) as i64;
    Ok(if is_negative { -raw } else { raw })
}

fn utf8_scalar<'a>(data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
    match Utf8Encoding::decode(data) {
        Cow::Borrowed(x) => Ok(Cow::Borrowed(x)),
        Cow::Owned(x) => Ok(Cow::Owned(x)),
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Eu5Token<'a> {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Scalar(&'a [u8]),
    Lookup(LookupIndex),
    I32(i32),
    RawF64([u8; 8]),
    Fixed5(i64),
}

#[derive(Clone, Copy, Default)]
struct Eu5Fields;

impl Eu5Fields {
    fn resolve_field(self, token: FieldId) -> Option<&'static str> {
        match token.value() {
            0x1000 => Some("name"),
            0x1001 => Some("country"),
            0x1002 => Some("growth"),
            0x1003 => Some("balance"),
            _ => None,
        }
    }
}

struct Eu5Format {
    fields: Eu5Fields,
    failed_resolve_strategy: FailedResolveStrategy,
    lookups: HashMap<u32, &'static str>,
}

impl Eu5Format {
    fn new() -> Self {
        Self {
            fields: Eu5Fields,
            failed_resolve_strategy: FailedResolveStrategy::Error,
            lookups: HashMap::from([(7, "FRA")]),
        }
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
            None => match self.failed_resolve_strategy {
                FailedResolveStrategy::Error => Err(Error::custom(format!(
                    "unknown field token 0x{:x}",
                    field.value()
                ))),
                FailedResolveStrategy::Stringify => Ok(ValueResult::Value(
                    visitor.visit_string(format!("0x{:x}", field.value()))?,
                )),
                FailedResolveStrategy::Ignore => Ok(ValueResult::Value(
                    visitor.visit_str("__internal_identifier_ignore")?,
                )),
            },
        }
    }

    fn decode_f64(raw: i64) -> f64 {
        let x = raw as f64;
        let eps = f64::from(f32::EPSILON);
        (x + (eps * x.signum())).trunc() / 100_000.0
    }
}

impl BinaryTokenFormat for Eu5Format {
    type Token<'a> = Eu5Token<'a>;

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
                Ok(TokenResult::Token(Eu5Token::Open))
            }
            LexemeId::CLOSE => {
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Close))
            }
            LexemeId::EQUAL => {
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Equal))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(data) = cursor.read_len_prefixed() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Scalar(data)))
            }
            LexemeId::LOOKUP_U8 => {
                let Some(data) = cursor.read_bytes::<1>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Lookup(LookupIndex::new(
                    data[0] as u32,
                ))))
            }
            LexemeId::I32 => {
                let Some(data) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::I32(i32::from_le_bytes(data))))
            }
            LexemeId::F64 => {
                let Some(data) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::RawF64(data)))
            }
            id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let byte_count = offset - (is_negative as u16 * 7);
                let raw = if byte_count == 0 {
                    0
                } else {
                    let Some(data) = cursor.read_slice(byte_count) else {
                        return Ok(TokenResult::MoreData);
                    };
                    let mut buf = [0u8; 8];
                    buf[..byte_count as usize].copy_from_slice(data);
                    let raw = u64::from_le_bytes(buf) as i64;
                    if is_negative { -raw } else { raw }
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Fixed5(raw)))
            }
            id => {
                cursor.consume();
                Ok(TokenResult::Token(Eu5Token::Field(FieldId::new(id.0))))
            }
        }
    }
}

impl BinaryValueFormat for Eu5Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        utf8_scalar(data)
    }

    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
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
            _ => {
                let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                let id = LexemeId::new(u16::from_le_bytes(id_bytes));
                return if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
                    Ok(ValueResult::MoreData(visitor))
                } else {
                    self.deserialize_any(reader, visitor)
                };
            }
        }
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
            self.deserialize_str(reader, visitor)
        } else if matches!(
            id,
            LexemeId::OPEN | LexemeId::EQUAL | LexemeId::CLOSE | LexemeId::I32 | LexemeId::F64
        ) || matches!(
            id,
            LexemeId::LOOKUP_U8
                | LexemeId::LOOKUP_U8_ALT
                | LexemeId::LOOKUP_U16
                | LexemeId::LOOKUP_U16_ALT
                | LexemeId::LOOKUP_U24
        ) || (id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56)
        {
            self.deserialize_any(reader, visitor)
        } else {
            cursor.consume();
            self.resolve_name(FieldId::new(id.0), visitor)
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
                cursor.consume();
                Ok(ValueResult::Open(visitor))
            }
            LexemeId::I32 => {
                let Some(data) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(
                    visitor.visit_i32(i32::from_le_bytes(data))?,
                ))
            }
            LexemeId::F64 => {
                let Some(data) = cursor.read_bytes::<8>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                let raw = i64::from_le_bytes(data);
                Ok(ValueResult::Value(
                    visitor.visit_f64(Self::decode_f64(raw))?,
                ))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => self.deserialize_str(reader, visitor),
            LexemeId::LOOKUP_U8 => {
                let Some(data) = cursor.read_bytes::<1>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                let index = LookupIndex::new(data[0] as u32);
                resolve_lookup(
                    self.lookups.get(&index.value()).copied(),
                    index,
                    self.failed_resolve_strategy,
                    visitor,
                )
            }
            id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let byte_count = offset - (is_negative as u16 * 7);
                if cursor.len() < byte_count as usize {
                    return Ok(ValueResult::MoreData(visitor));
                }
                cursor.consume();
                let raw = fixed5_raw(reader, id)?;
                Ok(ValueResult::Value(
                    visitor.visit_f64(Self::decode_f64(raw))?,
                ))
            }
            id => {
                cursor.consume();
                self.resolve_name(FieldId::new(id.0), visitor)
            }
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
struct Eu5Data {
    name: String,
    country: String,
    growth: f64,
    balance: f64,
}

fn eu5_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x1000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"Europa");
    push_field(&mut data, 0x1001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lookup_u8(&mut data, 7);
    push_field(&mut data, 0x1002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_fixed5_i16(&mut data, 750);
    push_field(&mut data, 0x1003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, 123_456i64.to_le_bytes());
    data
}

#[test]
fn eu5_deserializes_lookup_fixed_and_utf8_scalars() {
    let data = eu5_fixture();
    let actual: Eu5Data = assert_slice_and_reader(&data, Eu5Format::new);
    assert_eq!(actual.name, "Europa");
    assert_eq!(actual.country, "FRA");
    assert_eq!(actual.growth, -0.0075);
    assert_eq!(actual.balance, 1.23456);
}

#[test]
fn eu5_token_stream_exposes_lookup_and_fixed5_tokens() {
    let mut data = Vec::new();
    push_lookup_u8(&mut data, 7);
    push_fixed5_i16(&mut data, 750);

    let mut reader = TokenReader::from_slice(&data, Eu5Format::new());

    assert_eq!(
        reader.read_token().unwrap(),
        Eu5Token::Lookup(LookupIndex::new(7))
    );
    assert_eq!(reader.read_token().unwrap(), Eu5Token::Fixed5(-750));
}
