use crate::support::{
    assert_slice_and_reader, push_bool, push_f32_raw, push_f64_raw, push_field, push_i32,
    push_lexeme, push_lookup_u8, push_quoted, push_u32, push_unquoted,
};
use jomini::{
    Error, Windows1252Encoding,
    binary::ng::{
        BinaryTokenFormat, BinaryValueFormat, ParserState, PdxVisitor, TokenReader, TokenResult,
        ValueResult, from_slice,
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

fn resolve_name<'de, V>(
    field: FieldId,
    visitor: V,
    format: &Eu4Format,
) -> Result<ValueResult<V::Value, V>, Error>
where
    V: PdxVisitor<'de>,
{
    match format.fields.get(&field.value()).map(String::as_str) {
        Some(name) => Ok(ValueResult::Value(visitor.visit_str(name)?)),
        None => match format.failed_resolve_strategy {
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

fn eu4_scalar<'a>(data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
    if let Some(prefix) = data.first() {
        if (0x10..=0x13).contains(prefix) {
            if (data.len() - 1) % 2 != 0 {
                return Err(Error::custom("invalid escaped text length"));
            }

            let mut text = String::new();
            for chunk in data[1..].chunks_exact(2) {
                let cp = u16::from_le_bytes([chunk[0], chunk[1]]) as u32;
                let ch = char::from_u32(cp)
                    .ok_or_else(|| Error::custom("invalid escaped text code point"))?;
                text.push(ch);
            }

            return Ok(Cow::Owned(text));
        }
    }

    Ok(Windows1252Encoding::decode(data))
}

#[derive(Debug, Clone, PartialEq)]
enum Eu4Token<'a> {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Bool(bool),
    U32(u32),
    I32(i32),
    Quoted(&'a [u8]),
    Unquoted(&'a [u8]),
    F32([u8; 4]),
    F64([u8; 8]),
}

#[derive(Debug, Clone, PartialEq)]
enum Eu4TokenOwned {
    Open,
    Close,
    Equal,
    Field(FieldId),
    Bool(bool),
    U32(u32),
    I32(i32),
    Quoted(Vec<u8>),
    Unquoted(Vec<u8>),
    F32([u8; 4]),
    F64([u8; 8]),
}

impl From<Eu4Token<'_>> for Eu4TokenOwned {
    fn from(value: Eu4Token<'_>) -> Self {
        match value {
            Eu4Token::Open => Eu4TokenOwned::Open,
            Eu4Token::Close => Eu4TokenOwned::Close,
            Eu4Token::Equal => Eu4TokenOwned::Equal,
            Eu4Token::Field(id) => Eu4TokenOwned::Field(id),
            Eu4Token::Bool(v) => Eu4TokenOwned::Bool(v),
            Eu4Token::U32(v) => Eu4TokenOwned::U32(v),
            Eu4Token::I32(v) => Eu4TokenOwned::I32(v),
            Eu4Token::Quoted(v) => Eu4TokenOwned::Quoted(v.to_vec()),
            Eu4Token::Unquoted(v) => Eu4TokenOwned::Unquoted(v.to_vec()),
            Eu4Token::F32(v) => Eu4TokenOwned::F32(v),
            Eu4Token::F64(v) => Eu4TokenOwned::F64(v),
        }
    }
}

type Eu4Fields = HashMap<u16, String>;

fn eu4_fields() -> Eu4Fields {
    HashMap::from([
        (0x2000, String::from("label")),
        (0x2001, String::from("han")),
        (0x2002, String::from("score32")),
        (0x2003, String::from("score64")),
        (0x2004, String::from("flag")),
        (0x2005, String::from("count")),
        (0x2006, String::from("delta")),
        (0x2007, String::from("word")),
        (0x2008, String::from("nested")),
        (0x2009, String::from("inner")),
        (0x200a, String::from("members")),
    ])
}

struct Eu4Format {
    fields: Eu4Fields,
    failed_resolve_strategy: FailedResolveStrategy,
}

impl Eu4Format {
    fn new(fields: Eu4Fields) -> Self {
        Self {
            fields,
            failed_resolve_strategy: FailedResolveStrategy::Error,
        }
    }

    fn with_failed_resolve_strategy(mut self, strategy: FailedResolveStrategy) -> Self {
        self.failed_resolve_strategy = strategy;
        self
    }
}

impl Default for Eu4Format {
    fn default() -> Self {
        Self::new(eu4_fields())
    }
}

impl Eu4Format {
    fn decode_f32(raw: [u8; 4]) -> f32 {
        i32::from_le_bytes(raw) as f32 / 1000.0
    }

    fn decode_f64(raw: [u8; 8]) -> f64 {
        let val = i64::from_le_bytes(raw) as f64 / 32768.0;
        (val * 100_000.0).round() / 100_000.0
    }

    fn parse_str_token<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<<V as PdxVisitor<'de>>::Value, V>, Error> {
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

impl BinaryTokenFormat for Eu4Format {
    type Token<'a> = Eu4Token<'a>;

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
                Ok(TokenResult::Token(Eu4Token::Open))
            }
            LexemeId::CLOSE => {
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::Close))
            }
            LexemeId::EQUAL => {
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::Equal))
            }
            LexemeId::BOOL => {
                let Some(bytes) = cursor.read_bytes::<1>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::Bool(bytes[0] != 0)))
            }
            LexemeId::U32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::U32(u32::from_le_bytes(bytes))))
            }
            LexemeId::I32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::I32(i32::from_le_bytes(bytes))))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(data) = cursor.read_len_prefixed() else {
                    return Ok(TokenResult::MoreData);
                };

                cursor.consume();
                if id == LexemeId::QUOTED {
                    Ok(TokenResult::Token(Eu4Token::Quoted(data)))
                } else {
                    Ok(TokenResult::Token(Eu4Token::Unquoted(data)))
                }
            }
            LexemeId::F32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::F32(bytes)))
            }
            LexemeId::F64 => {
                let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::F64(bytes)))
            }
            id => {
                cursor.consume();
                Ok(TokenResult::Token(Eu4Token::Field(FieldId::new(id.0))))
            }
        }
    }

    fn skip_value(
        &mut self,
        state: &mut ParserState,
        fill: &mut impl FnMut(&mut ParserState) -> Result<usize, Error>,
    ) -> Result<(), Error> {
        // Phase 1: consume the first token. Scalars return immediately;
        // OPEN breaks into the container scan below.
        loop {
            let mut cursor = state.token_cursor();
            let Some(id) = cursor.read_lexeme() else {
                if fill(state)? == 0 {
                    return Err(Error::eof());
                }
                continue;
            };

            match id {
                LexemeId::OPEN => {
                    cursor.consume();
                    break;
                }
                LexemeId::CLOSE => {
                    cursor.consume();
                    return Ok(());
                }
                LexemeId::BOOL => {
                    if cursor.read_bytes::<1>().is_some() {
                        cursor.consume();
                        return Ok(());
                    }

                    if fill(state)? == 0 {
                        return Err(Error::eof());
                    }
                    continue;
                }
                LexemeId::I32 | LexemeId::F32 | LexemeId::U32 => {
                    if cursor.read_bytes::<4>().is_some() {
                        cursor.consume();
                        return Ok(());
                    }

                    if fill(state)? == 0 {
                        return Err(Error::eof());
                    }
                    continue;
                }
                LexemeId::F64 => {
                    if cursor.read_bytes::<8>().is_some() {
                        cursor.consume();
                        return Ok(());
                    }

                    if fill(state)? == 0 {
                        return Err(Error::eof());
                    }
                    continue;
                }
                LexemeId::QUOTED | LexemeId::UNQUOTED => {
                    if cursor.read_len_prefixed().is_none() {
                        if fill(state)? == 0 {
                            return Err(Error::eof());
                        }
                        continue;
                    }
                    cursor.consume();
                    return Ok(());
                }
                _ => {
                    // field ID or EQUAL: 2 bytes
                    cursor.consume();
                    return Ok(());
                }
            }
        }

        // Phase 2: container scan — scalars are skipped without any depth
        // check; only OPEN/CLOSE affect depth.
        let mut depth: usize = 1;
        loop {
            let mut cursor = state.token_cursor();
            let committed = loop {
                let committed = cursor.checkpoint();
                let Some(id) = cursor.read_lexeme() else {
                    break committed;
                };

                match id {
                    LexemeId::CLOSE => {
                        depth -= 1;
                        if depth == 0 {
                            cursor.consume();
                            return Ok(());
                        }
                    }
                    LexemeId::OPEN => {
                        depth += 1;
                    }
                    LexemeId::BOOL => {
                        if cursor.read_bytes::<1>().is_none() {
                            break committed;
                        }
                    }
                    LexemeId::I32 | LexemeId::F32 | LexemeId::U32 => {
                        if cursor.read_bytes::<4>().is_none() {
                            break committed;
                        }
                    }
                    LexemeId::F64 => {
                        if cursor.read_bytes::<8>().is_none() {
                            break committed;
                        }
                    }
                    LexemeId::QUOTED | LexemeId::UNQUOTED => {
                        if cursor.read_len_prefixed().is_none() {
                            break committed;
                        }
                    }
                    _ => {
                        // field ID or EQUAL: only the lexeme bytes are consumed.
                    }
                }
            };

            cursor.consume_to(committed);
            if fill(state)? == 0 {
                return Err(Error::eof());
            }
        }
    }
}

impl BinaryValueFormat for Eu4Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        eu4_scalar(data)
    }

    fn deserialize_bool<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if id != LexemeId::BOOL {
            return self.deserialize_any(reader, visitor);
        }
        let Some(bytes) = cursor.read_bytes::<1>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        Ok(ValueResult::Value(visitor.visit_bool(bytes[0] != 0)?))
    }

    fn deserialize_u32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if id != LexemeId::U32 {
            return self.deserialize_any(reader, visitor);
        }
        let Some(bytes) = cursor.read_bytes::<4>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        Ok(ValueResult::Value(
            visitor.visit_u32(u32::from_le_bytes(bytes))?,
        ))
    }

    fn deserialize_i32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if id != LexemeId::I32 {
            return self.deserialize_any(reader, visitor);
        }
        let Some(bytes) = cursor.read_bytes::<4>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        Ok(ValueResult::Value(
            visitor.visit_i32(i32::from_le_bytes(bytes))?,
        ))
    }

    fn deserialize_f32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if id != LexemeId::F32 {
            return self.deserialize_any(reader, visitor);
        }
        let Some(bytes) = cursor.read_bytes::<4>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        Ok(ValueResult::Value(
            visitor.visit_f32(Self::decode_f32(bytes))?,
        ))
    }

    fn deserialize_f64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> Result<ValueResult<V::Value, V>, Error> {
        let mut cursor = reader.token_cursor();
        let Some(id) = cursor.read_lexeme() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        if id != LexemeId::F64 {
            return self.deserialize_any(reader, visitor);
        }
        let Some(bytes) = cursor.read_bytes::<8>().copied() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        cursor.consume();
        Ok(ValueResult::Value(
            visitor.visit_f64(Self::decode_f64(bytes))?,
        ))
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
        let field = FieldId::new(id.0);
        if let Some(name) = self.fields.get(&field.value()).map(String::as_str) {
            cursor.consume();
            return Ok(ValueResult::Value(visitor.visit_str(name)?));
        }
        if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
            self.parse_str_token(reader, visitor)
        } else {
            self.deserialize_any(reader, visitor)
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
            LexemeId::BOOL => {
                let Some(bytes) = cursor.read_bytes::<1>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(visitor.visit_bool(bytes[0] != 0)?))
            }
            LexemeId::U32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(
                    visitor.visit_u32(u32::from_le_bytes(bytes))?,
                ))
            }
            LexemeId::I32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(
                    visitor.visit_i32(i32::from_le_bytes(bytes))?,
                ))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => self.parse_str_token(reader, visitor),
            LexemeId::F32 => {
                let Some(bytes) = cursor.read_bytes::<4>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(
                    visitor.visit_f32(Self::decode_f32(bytes))?,
                ))
            }
            LexemeId::F64 => {
                let Some(bytes) = cursor.read_bytes::<8>().copied() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                cursor.consume();
                Ok(ValueResult::Value(
                    visitor.visit_f64(Self::decode_f64(bytes))?,
                ))
            }
            id => {
                cursor.consume();
                resolve_name(FieldId::new(id.0), visitor, self)
            }
        }
    }
}

#[derive(Debug, Deserialize, PartialEq)]
struct Eu4Data {
    label: String,
    han: String,
    score32: f32,
    score64: f64,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Eu4CommonData {
    flag: bool,
    count: u32,
    delta: i32,
    word: String,
    nested: Eu4NestedData,
}

#[derive(Debug, Deserialize, PartialEq)]
struct Eu4NestedData {
    inner: i32,
}

fn eu4_scalar_rules_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_quoted(&mut data, &[0xa7, b'G']);
    push_field(&mut data, 0x2001);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_quoted(&mut data, &[0x10, 0x60, 0x4f, 0x7d, 0x59]);
    push_field(&mut data, 0x2002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x2003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (12i64 * 32768).to_le_bytes());
    data
}

fn eu4_lookup_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lookup_u8(&mut data, 7);
    data
}

fn eu4_common_tokens_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2004);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_bool(&mut data, true);
    push_field(&mut data, 0x2005);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_u32(&mut data, 42);
    push_field(&mut data, 0x2006);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, -7);
    push_field(&mut data, 0x2000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_quoted(&mut data, b"quoted");
    push_field(&mut data, 0x2007);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"plain");
    push_field(&mut data, 0x2008);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_field(&mut data, 0x2009);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 9);
    push_lexeme(&mut data, LexemeId::CLOSE);
    push_field(&mut data, 0x2002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x2003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (12i64 * 32768).to_le_bytes());
    data
}

fn eu4_iteration_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2000);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_quoted(&mut data, &[0xa7, b'G']);
    push_field(&mut data, 0x2004);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_bool(&mut data, true);
    push_field(&mut data, 0x2008);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_field(&mut data, 0x2009);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 9);
    push_lexeme(&mut data, LexemeId::CLOSE);
    push_field(&mut data, 0x2002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x2003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (12i64 * 32768).to_le_bytes());
    data
}

fn eu4_common_object_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2004);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_bool(&mut data, true);
    push_field(&mut data, 0x2005);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_u32(&mut data, 42);
    push_field(&mut data, 0x2006);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, -7);
    push_field(&mut data, 0x2007);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"plain");
    push_field(&mut data, 0x2008);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_field(&mut data, 0x2009);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 9);
    push_lexeme(&mut data, LexemeId::CLOSE);
    data
}

fn eu4_speculative_primitives_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2004);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_bool(&mut data, true);
    push_field(&mut data, 0x2005);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_u32(&mut data, 42);
    push_field(&mut data, 0x2006);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, -7);
    push_field(&mut data, 0x2007);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"plain");
    push_field(&mut data, 0x2002);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f32_raw(&mut data, 1234i32.to_le_bytes());
    push_field(&mut data, 0x2003);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_f64_raw(&mut data, (12i64 * 32768).to_le_bytes());
    data
}

fn eu4_unknown_field_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2010);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 7);
    data
}

fn eu4_nested_sequence_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_unquoted(&mut data, b"AAA");
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_field(&mut data, 0x200a);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_lexeme(&mut data, LexemeId::OPEN);
    push_i32(&mut data, 1);
    push_i32(&mut data, 2);
    push_lexeme(&mut data, LexemeId::CLOSE);
    push_field(&mut data, 0x2007);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_unquoted(&mut data, b"plain");
    push_lexeme(&mut data, LexemeId::CLOSE);
    data
}

mod deserialize {
    use super::*;
    use std::collections::HashMap;

    #[derive(Debug, Deserialize, PartialEq)]
    struct Eu4IgnoredNestedData {
        flag: bool,
        score32: f32,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct Eu4SeqInMapValue {
        members: Vec<i32>,
        word: String,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    struct Eu4SpeculativePrimitives {
        flag: bool,
        count: u32,
        delta: i32,
        word: String,
        score32: f32,
        score64: f64,
    }

    #[test]
    fn deserializes_windows1252_chinese_scalars_and_fixed_float_rules() {
        let data = eu4_scalar_rules_fixture();

        let actual: Eu4Data = assert_slice_and_reader(&data, Eu4Format::default);
        assert_eq!(actual.label, "\u{a7}G");
        assert_eq!(actual.han, "你好");
        assert_eq!(actual.score32, 1.234);
        assert_eq!(actual.score64, 12.0);
    }

    #[test]
    fn rejects_lookup_tokens() {
        #[derive(Debug, Deserialize)]
        struct Eu4LookupData {
            #[serde(rename = "label")]
            _label: String,
        }

        let data = eu4_lookup_fixture();

        let err = from_slice::<Eu4LookupData, _>(&data, Eu4Format::default()).unwrap_err();
        assert!(err.to_string().contains("unknown field token"));
    }

    #[test]
    fn deserializes_common_tokens_and_nested_objects() {
        let data = eu4_common_object_fixture();

        let actual: Eu4CommonData = assert_slice_and_reader(&data, Eu4Format::default);
        assert_eq!(
            actual,
            Eu4CommonData {
                flag: true,
                count: 42,
                delta: -7,
                word: String::from("plain"),
                nested: Eu4NestedData { inner: 9 },
            }
        );
    }

    #[test]
    fn speculatively_deserializes_primitives_across_small_chunks() {
        let data = eu4_speculative_primitives_fixture();

        let from_bytes: Eu4SpeculativePrimitives = from_slice(&data, Eu4Format::default()).unwrap();
        let from_stream: Eu4SpeculativePrimitives = jomini::binary::ng::from_reader(
            crate::support::ChunkedReader::new(&data, 2),
            Eu4Format::default(),
        )
        .unwrap();

        assert_eq!(from_stream, from_bytes);
        assert_eq!(
            from_stream,
            Eu4SpeculativePrimitives {
                flag: true,
                count: 42,
                delta: -7,
                word: String::from("plain"),
                score32: 1.234,
                score64: 12.0,
            }
        );
    }

    #[test]
    fn ignores_nested_objects_and_continues_parsing() {
        let data = eu4_common_tokens_fixture();

        let actual: Eu4IgnoredNestedData = assert_slice_and_reader(&data, Eu4Format::default);
        assert_eq!(
            actual,
            Eu4IgnoredNestedData {
                flag: true,
                score32: 1.234,
            }
        );
    }

    #[test]
    fn ignores_nested_objects_with_split_length_prefixed_strings() {
        let mut data = Vec::new();
        push_field(&mut data, 0x2004);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_bool(&mut data, true);
        push_field(&mut data, 0x2008);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_lexeme(&mut data, LexemeId::OPEN);
        push_field(&mut data, 0x2000);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_quoted(&mut data, b"abcdef");
        push_lexeme(&mut data, LexemeId::CLOSE);
        push_field(&mut data, 0x2002);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_f32_raw(&mut data, 1234i32.to_le_bytes());

        let from_bytes: Eu4IgnoredNestedData = from_slice(&data, Eu4Format::default()).unwrap();
        let from_stream: Eu4IgnoredNestedData = jomini::binary::ng::from_reader(
            crate::support::ChunkedReader::new(&data, 21),
            Eu4Format::default(),
        )
        .unwrap();

        assert_eq!(from_stream, from_bytes);
        assert_eq!(
            from_stream,
            Eu4IgnoredNestedData {
                flag: true,
                score32: 1.234,
            }
        );
    }

    #[test]
    fn ignores_nested_objects_with_split_fixed_width_tokens_after_lexeme() {
        let mut data = Vec::new();
        push_field(&mut data, 0x2004);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_bool(&mut data, true);
        push_field(&mut data, 0x2008);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_lexeme(&mut data, LexemeId::OPEN);
        push_field(&mut data, 0x2004);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_bool(&mut data, false);
        push_lexeme(&mut data, LexemeId::CLOSE);
        push_field(&mut data, 0x2002);
        push_lexeme(&mut data, LexemeId::EQUAL);
        push_f32_raw(&mut data, 1234i32.to_le_bytes());

        let from_bytes: Eu4IgnoredNestedData = from_slice(&data, Eu4Format::default()).unwrap();
        let from_stream: Eu4IgnoredNestedData = jomini::binary::ng::from_reader(
            crate::support::ChunkedReader::new(&data, 19),
            Eu4Format::default(),
        )
        .unwrap();

        assert_eq!(from_stream, from_bytes);
        assert_eq!(
            from_stream,
            Eu4IgnoredNestedData {
                flag: true,
                score32: 1.234,
            }
        );
    }

    #[test]
    fn consumes_sequence_close_before_returning_to_parent_map() {
        let data = eu4_nested_sequence_fixture();

        let actual: HashMap<String, Eu4SeqInMapValue> =
            assert_slice_and_reader(&data, Eu4Format::default);
        assert_eq!(
            actual.get("AAA"),
            Some(&Eu4SeqInMapValue {
                members: vec![1, 2],
                word: String::from("plain"),
            })
        );
    }
}

mod token_stream {
    use super::*;

    #[test]
    fn covers_common_tokens() {
        let data = eu4_common_tokens_fixture();
        let mut reader = TokenReader::from_slice(&data, Eu4Format::default());

        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2004))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Bool(true));
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2005))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::U32(42));
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2006))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::I32(-7));
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2000))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Quoted(b"quoted"));
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2007))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Unquoted(b"plain"));
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2008))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Open);
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2009))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token().unwrap(), Eu4Token::I32(9));
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Close);
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2002))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::F32(1234i32.to_le_bytes())
        );
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::Field(FieldId::new(0x2003))
        );
        assert_eq!(reader.read_token().unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token().unwrap(),
            Eu4Token::F64((12i64 * 32768).to_le_bytes())
        );
    }

    #[test]
    fn can_be_iterated() {
        let data = eu4_iteration_fixture();
        let mut reader = TokenReader::from_slice(&data, Eu4Format::default());
        let mut tokens = Vec::new();

        loop {
            let Some(token) = reader.next_token().unwrap().map(Eu4TokenOwned::from) else {
                break;
            };
            tokens.push(token);
        }

        assert_eq!(
            tokens,
            vec![
                Eu4TokenOwned::Field(FieldId::new(0x2000)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::Quoted(vec![0xa7, b'G']),
                Eu4TokenOwned::Field(FieldId::new(0x2004)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::Bool(true),
                Eu4TokenOwned::Field(FieldId::new(0x2008)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::Open,
                Eu4TokenOwned::Field(FieldId::new(0x2009)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::I32(9),
                Eu4TokenOwned::Close,
                Eu4TokenOwned::Field(FieldId::new(0x2002)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::F32(1234i32.to_le_bytes()),
                Eu4TokenOwned::Field(FieldId::new(0x2003)),
                Eu4TokenOwned::Equal,
                Eu4TokenOwned::F64((12i64 * 32768).to_le_bytes()),
            ]
        );
    }
}

mod unresolved_fields {
    use super::*;

    #[test]
    fn uses_stringify_strategy() {
        let data = eu4_unknown_field_fixture();
        let format =
            Eu4Format::default().with_failed_resolve_strategy(FailedResolveStrategy::Stringify);

        let actual: HashMap<String, i32> = from_slice(&data, format).unwrap();

        assert_eq!(actual.get("0x2010"), Some(&7));
    }

    #[test]
    fn uses_ignore_strategy() {
        let data = eu4_unknown_field_fixture();
        let format =
            Eu4Format::default().with_failed_resolve_strategy(FailedResolveStrategy::Ignore);

        let actual: HashMap<String, i32> = from_slice(&data, format).unwrap();

        assert_eq!(actual.get("__internal_identifier_ignore"), Some(&7));
    }

    #[test]
    fn uses_error_strategy() {
        let data = eu4_unknown_field_fixture();
        let format =
            Eu4Format::default().with_failed_resolve_strategy(FailedResolveStrategy::Error);

        let err = from_slice::<HashMap<String, i32>, _>(&data, format).unwrap_err();

        assert!(err.to_string().contains("unknown field token"));
    }
}
