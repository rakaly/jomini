use crate::support::{
    assert_slice_and_reader, push_bool, push_f32_raw, push_f64_raw, push_field, push_i32,
    push_lexeme, push_lookup_u8, push_quoted, push_u32, push_unquoted,
};
use jomini::{
    Error, Windows1252Encoding,
    binary::ng::{
        BinaryConfig, BinaryTokenFormat, BinaryValueFormat, FieldId, FieldResolver, ParserState,
        PdxVisitor, StreamToken, StructureKind, TokenReader, TokenResult, ValueResult, from_slice,
        from_slice_with_config,
    },
    binary::{FailedResolveStrategy, LexemeId},
};
use serde::{Deserialize, de::Error as _};
use std::{borrow::Cow, collections::HashMap};

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
        None => match config.failed_resolve_strategy() {
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

impl StreamToken for Eu4Token<'_> {
    fn structure(&self) -> StructureKind {
        match self {
            Eu4Token::Open => StructureKind::Open,
            Eu4Token::Close => StructureKind::Close,
            Eu4Token::Equal => StructureKind::Equal,
            _ => StructureKind::Value,
        }
    }
}

#[derive(Clone, Copy, Default)]
struct Eu4Fields;

impl FieldResolver for Eu4Fields {
    fn resolve_field(&self, token: FieldId) -> Option<&str> {
        match token.value() {
            0x2000 => Some("label"),
            0x2001 => Some("han"),
            0x2002 => Some("score32"),
            0x2003 => Some("score64"),
            0x2004 => Some("flag"),
            0x2005 => Some("count"),
            0x2006 => Some("delta"),
            0x2007 => Some("word"),
            0x2008 => Some("nested"),
            0x2009 => Some("inner"),
            _ => None,
        }
    }
}

#[derive(Default)]
struct Eu4Format;

impl Eu4Format {
    fn decode_f32(raw: [u8; 4]) -> f32 {
        i32::from_le_bytes(raw) as f32 / 1000.0
    }

    fn decode_f64(raw: [u8; 8]) -> f64 {
        let val = i64::from_le_bytes(raw) as f64 / 32768.0;
        (val * 100_000.0).round() / 100_000.0
    }
    
    fn deserialize_str<'de, V: PdxVisitor<'de>>(&mut self, reader: &mut ParserState, visitor: V) -> Result<ValueResult<<V as PdxVisitor<'de>>::Value, V>, Error> {
        let Some(header) = reader.peek_bytes::<4>() else {
            return Ok(ValueResult::MoreData(visitor));
        };
        let len = u16::from_le_bytes([header[2], header[3]]);
        let Some(data) = reader.read_slice(4 + len) else {
            return Ok(ValueResult::MoreData(visitor));
        };
        let value = match self.decode_scalar(&data[4..])? {
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
        let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
            return Ok(TokenResult::MoreData);
        };
        let id = LexemeId::new(u16::from_le_bytes(id_bytes));

        match id {
            LexemeId::OPEN => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Eu4Token::Open))
            }
            LexemeId::CLOSE => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Eu4Token::Close))
            }
            LexemeId::EQUAL => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Eu4Token::Equal))
            }
            LexemeId::BOOL => {
                let Some(bytes) = reader.read_bytes::<3>() else {
                    return Ok(TokenResult::MoreData);
                };
                Ok(TokenResult::Token(Eu4Token::Bool(bytes[2] != 0)))
            }
            LexemeId::U32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(TokenResult::MoreData);
                };
                let data = [bytes[2], bytes[3], bytes[4], bytes[5]];
                Ok(TokenResult::Token(Eu4Token::U32(u32::from_le_bytes(data))))
            }
            LexemeId::I32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(TokenResult::MoreData);
                };
                let data = [bytes[2], bytes[3], bytes[4], bytes[5]];
                Ok(TokenResult::Token(Eu4Token::I32(i32::from_le_bytes(data))))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(header) = reader.peek_bytes::<4>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                let len = u16::from_le_bytes([header[2], header[3]]);
                let Some(bytes) = reader.read_slice(len + 4) else {
                    return Ok(TokenResult::MoreData);
                };
                let data = &bytes[4..];
                if id == LexemeId::QUOTED {
                    Ok(TokenResult::Token(Eu4Token::Quoted(data)))
                } else {
                    Ok(TokenResult::Token(Eu4Token::Unquoted(data)))
                }
            }
            LexemeId::F32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(TokenResult::MoreData);
                };
                let data = [bytes[2], bytes[3], bytes[4], bytes[5]];
                Ok(TokenResult::Token(Eu4Token::F32(data)))
            }
            LexemeId::F64 => {
                let Some(bytes) = reader.read_bytes::<10>() else {
                    return Ok(TokenResult::MoreData);
                };
                let data = [bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7], bytes[8], bytes[9]];
                Ok(TokenResult::Token(Eu4Token::F64(data)))
            }
            id => {
                unsafe { reader.consume(2) };
                Ok(TokenResult::Token(Eu4Token::Field(FieldId::new(id.0))))
            }
        }
    }
}

impl BinaryValueFormat for Eu4Format {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        eu4_scalar(data)
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

        let field = FieldId::new(u16::from_le_bytes(id_bytes));
        if let Some(name) = config.field_resolver().resolve_field(field) {
            unsafe { reader.consume(2) };
            return Ok(ValueResult::Value(visitor.visit_str(name)?))
        };

        let id = LexemeId::new(u16::from_le_bytes(id_bytes));
        if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
            self.deserialize_str(reader, visitor)
        } else {
            self.deserialize_any(reader, visitor, config)
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
            LexemeId::BOOL => {
                let Some(bytes) = reader.read_bytes::<3>() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                Ok(ValueResult::Value(visitor.visit_bool(bytes[3] != 0)?))
            }
            LexemeId::U32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                Ok(ValueResult::Value(visitor.visit_u32(
                    u32::from_le_bytes([bytes[2], bytes[3], bytes[4], bytes[5]]),
                )?))
            }
            LexemeId::I32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                Ok(ValueResult::Value(visitor.visit_i32(
                    i32::from_le_bytes([bytes[2], bytes[3], bytes[4], bytes[5]]),
                )?))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => self.deserialize_str(reader, visitor),
            LexemeId::F32 => {
                let Some(bytes) = reader.read_bytes::<6>() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                Ok(ValueResult::Value(visitor.visit_f32(Self::decode_f32(
                    [bytes[2], bytes[3], bytes[4], bytes[5]],
                ))?))
            }
            LexemeId::F64 => {
                let Some(bytes) = reader.read_bytes::<10>() else {
                    return Ok(ValueResult::MoreData(visitor));
                };
                Ok(ValueResult::Value(visitor.visit_f64(Self::decode_f64(
                    [
                        bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7], bytes[8],
                        bytes[9],
                    ],
                ))?))
            }
            id => {
                unsafe { reader.consume(2) };
                resolve_name(FieldId::new(id.0), visitor, config)
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

fn eu4_unknown_field_fixture() -> Vec<u8> {
    let mut data = Vec::new();
    push_field(&mut data, 0x2010);
    push_lexeme(&mut data, LexemeId::EQUAL);
    push_i32(&mut data, 7);
    data
}

mod deserialize {
    use super::*;

    #[test]
    fn deserializes_windows1252_chinese_scalars_and_fixed_float_rules() {
        let data = eu4_scalar_rules_fixture();

        let actual: Eu4Data = assert_slice_and_reader(&data, Eu4Format::default, Eu4Fields);
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
        let err = from_slice::<Eu4LookupData, _, _>(&data, Eu4Format, Eu4Fields).unwrap_err();
        assert!(err.to_string().contains("unknown field token"));
    }

    #[test]
    fn deserializes_common_tokens_and_nested_objects() {
        let data = eu4_common_object_fixture();

        let actual: Eu4CommonData = assert_slice_and_reader(&data, Eu4Format::default, Eu4Fields);
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
}

mod token_stream {
    use super::*;

    #[test]
    fn covers_common_tokens_and_shared_skip() {
        let data = eu4_common_tokens_fixture();
        let mut reader = TokenReader::from_slice(&data);
        let mut format = Eu4Format::default();

        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2004))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Bool(true)
        );
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2005))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::U32(42));
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2006))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::I32(-7));
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2000))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Quoted(b"quoted")
        );
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2007))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Unquoted(b"plain")
        );
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2008))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Open);
        reader.skip_container(&mut format).unwrap();
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2002))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::F32(1234i32.to_le_bytes())
        );
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::Field(FieldId::new(0x2003))
        );
        assert_eq!(reader.read_token(&mut format).unwrap(), Eu4Token::Equal);
        assert_eq!(
            reader.read_token(&mut format).unwrap(),
            Eu4Token::F64((12i64 * 32768).to_le_bytes())
        );
    }

    #[test]
    fn can_be_iterated() {
        let data = eu4_iteration_fixture();
        let mut reader = TokenReader::from_slice(&data);
        let mut format = Eu4Format::default();
        let mut tokens = Vec::new();

        loop {
            let Some(token) = reader
                .next_token(&mut format)
                .unwrap()
                .map(Eu4TokenOwned::from)
            else {
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

        let actual: HashMap<String, i32> = from_slice_with_config(
            &data,
            Eu4Format::default(),
            BinaryConfig::new(()).with_failed_resolve_strategy(FailedResolveStrategy::Stringify),
        )
        .unwrap();

        assert_eq!(actual.get("0x2010"), Some(&7));
    }

    #[test]
    fn uses_ignore_strategy() {
        let data = eu4_unknown_field_fixture();

        let actual: HashMap<String, i32> = from_slice_with_config(
            &data,
            Eu4Format::default(),
            BinaryConfig::new(()).with_failed_resolve_strategy(FailedResolveStrategy::Ignore),
        )
        .unwrap();

        assert_eq!(actual.get("__internal_identifier_ignore"), Some(&7));
    }

    #[test]
    fn uses_error_strategy() {
        let data = eu4_unknown_field_fixture();

        let err = from_slice_with_config::<HashMap<String, i32>, _, _>(
            &data,
            Eu4Format::default(),
            BinaryConfig::new(()).with_failed_resolve_strategy(FailedResolveStrategy::Error),
        )
        .unwrap_err();

        assert!(err.to_string().contains("unknown field token"));
    }
}
