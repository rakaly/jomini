use crate::BinaryError;
use crate::document::{document_from_slice, BorrowedBinaryValue, ScalarValue, ValueEntry};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::borrow::Cow;
use std::collections::HashMap;
use std::error;
use std::fmt;

#[derive(Debug)]
pub enum BinaryDeError {
    UnexpectedEvent(String),
    UnknownToken(u16),
    Parsing(BinaryError),
    Message(String),
}

impl fmt::Display for BinaryDeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryDeError::UnknownToken(x) => write!(f, "unknown token encountered: 0x{:x}", x),
            BinaryDeError::UnexpectedEvent(x) => write!(f, "unexpected event: {}", x),
            _ => write!(f, "binary deserialization error"),
        }
    }
}

impl error::Error for BinaryDeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            BinaryDeError::Parsing(x) => Some(x),
            _ => None,
        }
    }
}

impl de::Error for BinaryDeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        BinaryDeError::Message(msg.to_string())
    }
}

impl From<BinaryError> for BinaryDeError {
    fn from(error: BinaryError) -> Self {
        BinaryDeError::Parsing(error)
    }
}

/// Resolves binary 16bit tokens to field names
pub trait TokenResolver {
    fn resolve(&self, token: u16) -> Option<&str>;
}

impl<S, V> TokenResolver for HashMap<u16, V, S>
where
    S: ::std::hash::BuildHasher,
    V: AsRef<str>,
{
    fn resolve(&self, token: u16) -> Option<&str> {
        self.get(&token).map(|x| x.as_ref())
    }
}

impl<'a, S, V> TokenResolver for &'a HashMap<u16, V, S>
where
    S: ::std::hash::BuildHasher,
    V: AsRef<str>,
{
    fn resolve(&self, token: u16) -> Option<&str> {
        self.get(&token).map(|x| x.as_ref())
    }
}

/// Customize how the deserializer reacts when a token can't be resolved
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FailedResolveStrategy {
    /// Return an error
    Error,

    /// Stringify the token as hexidecimal
    Stringify,

    /// Ignore the token
    Ignore,
}

/// Builds a tweaked binary deserializer
#[derive(Debug)]
pub struct BinaryDeserializerBuilder {
    failed_resolve_strategy: FailedResolveStrategy,
}

impl Default for BinaryDeserializerBuilder {
    fn default() -> Self {
        BinaryDeserializerBuilder::new()
    }
}

impl BinaryDeserializerBuilder {
    pub fn new() -> Self {
        BinaryDeserializerBuilder {
            failed_resolve_strategy: FailedResolveStrategy::Ignore,
        }
    }

    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.failed_resolve_strategy = strategy;
        self
    }

    pub fn from_slice<'a, 'b, RES, T>(
        &'b self,
        data: &'a [u8],
        resolver: RES,
    ) -> Result<T, BinaryDeError>
    where
        T: Deserialize<'a>,
        RES: TokenResolver,
    {
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
        };

        let doc = document_from_slice(data)?;
        let mut deserializer = RootDeserializer {
            doc: &doc,
            config: &config,
        };
        T::deserialize(&mut deserializer)
    }
}

pub fn from_slice<'a, RES, T>(data: &'a [u8], resolver: RES) -> Result<T, BinaryDeError>
where
    T: Deserialize<'a>,
    RES: TokenResolver,
{
    BinaryDeserializerBuilder::new()
        .on_failed_resolve(FailedResolveStrategy::Ignore)
        .from_slice(data, resolver)
}

struct BinaryConfig<RES> {
    resolver: RES,
    failed_resolve_strategy: FailedResolveStrategy,
}

struct RootDeserializer<'b, 'a: 'b, RES> {
    doc: &'b Vec<ValueEntry<'a>>,
    config: &'b BinaryConfig<RES>,
}

impl<'b, 'de, 'r, RES: TokenResolver> de::Deserializer<'de>
    for &'r mut RootDeserializer<'b, 'de, RES>
{
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(BinaryDeError::Message(String::from(
            "root deserializer can only work with key value pairs",
        )))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(BinaryMap::new(self.config, self.doc))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

struct BinaryMap<'c, 'a: 'c, 'de: 'a, RES: 'a> {
    config: &'a BinaryConfig<RES>,
    doc: &'c Vec<ValueEntry<'de>>,
    field_idx: usize,
    seen: Vec<u8>,
    values: Vec<usize>,
}

impl<'c, 'a, 'de, RES> BinaryMap<'c, 'a, 'de, RES> {
    fn new(config: &'a BinaryConfig<RES>, doc: &'c Vec<ValueEntry<'de>>) -> Self {
        let seen = vec![0; doc.len()];
        BinaryMap {
            config,
            doc,
            field_idx: 0,
            seen,
            values: Vec::new(),
        }
    }
}

impl<'c, 'de, 'a, RES: TokenResolver> MapAccess<'de> for BinaryMap<'c, 'a, 'de, RES> {
    type Error = BinaryDeError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        self.values.clear();
        while self.field_idx < self.doc.len() {
            if self.seen[self.field_idx] == 0 {
                let key = &self.doc[self.field_idx].key;
                self.seen[self.field_idx] = 1;

                self.values.push(self.field_idx);
                for (i, x) in self.doc[self.field_idx..].iter().skip(1).enumerate() {
                    if &x.key == key {
                        self.seen[1 + i + self.field_idx] = 1;
                        self.values.push(1 + i + self.field_idx);
                    }
                }

                return seed
                    .deserialize(KeyDeserializer {
                        key,
                        config: self.config,
                    })
                    .map(Some);
            }

            self.field_idx += 1;
        }

        Ok(None)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            value_indices: &self.values,
            doc: &self.doc,
            config: self.config,
        })
    }
}

pub struct KeyDeserializer<'c, 'b: 'c, 'de: 'b, RES> {
    config: &'b BinaryConfig<RES>,
    key: &'c ScalarValue<'de>,
}

fn visit_key<'c, 'b: 'c, 'de: 'b, RES: TokenResolver, V: Visitor<'de>>(
    scalar: &'c ScalarValue<'de>,
    config: &'b BinaryConfig<RES>,
    visitor: V,
) -> Result<V::Value, BinaryDeError> {
    match scalar {
        ScalarValue::Bool(x) => visitor.visit_bool(*x),
        ScalarValue::U32(x) => visitor.visit_u32(*x),
        ScalarValue::U64(x) => visitor.visit_u64(*x),
        ScalarValue::I32(x) => visitor.visit_i32(*x),
        ScalarValue::Text(x) => match x.to_utf8() {
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
            Cow::Owned(s) => visitor.visit_string(s),
        },
        ScalarValue::F32(x) => visitor.visit_f32(*x),
        ScalarValue::Q16(x) => visitor.visit_f32(*x),
        ScalarValue::Token(s) => match config.resolver.resolve(*s) {
            Some(id) => visitor.visit_str(id),
            None => match config.failed_resolve_strategy {
                FailedResolveStrategy::Error => Err(BinaryDeError::UnknownToken(*s)),
                FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{:x}", s)),
                FailedResolveStrategy::Ignore => visitor.visit_str("__internal_identifier_ignore"),
            },
        },
    }
}

impl<'c, 'b, 'de, RES: TokenResolver> de::Deserializer<'de> for KeyDeserializer<'c, 'b, 'de, RES> {
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visit_key(self.key, self.config, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum ignored_any identifier struct
    }
}

pub struct ValueDeserializer<'c, 'b: 'c, 'de: 'b, RES> {
    config: &'b BinaryConfig<RES>,
    value_indices: &'c Vec<usize>,
    doc: &'c Vec<ValueEntry<'de>>,
}

impl<'c, 'b, 'de, RES: TokenResolver> de::Deserializer<'de>
    for ValueDeserializer<'c, 'b, 'de, RES>
{
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.doc[self.value_indices[0]].value {
            BorrowedBinaryValue::Scalar(scalar) => visit_key(scalar, self.config, visitor),
            BorrowedBinaryValue::Object(obj) => {
                visitor.visit_map(BinaryMap::new(&self.config, &obj))
            }
            BorrowedBinaryValue::Array(x) => visitor.visit_seq(BinarySequence {
                values: x,
                config: self.config,
                idx: 0,
            }),
            BorrowedBinaryValue::Hsv(first, second, third) => visitor.visit_seq(HsvSequence {
                pos: 0,
                first: *first,
                second: *second,
                third: *third,
            }),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.doc[self.value_indices[0]].value {
            BorrowedBinaryValue::Array(x) => visitor.visit_seq(BinarySequence {
                values: x,
                config: self.config,
                idx: 0,
            }),
            BorrowedBinaryValue::Scalar(_) | BorrowedBinaryValue::Object(_) => {
                visitor.visit_seq(SpanningSequence {
                    doc: self.doc,
                    config: self.config,
                    value_indices: self.value_indices,
                    idx: 0,
                })
            }
            BorrowedBinaryValue::Hsv(first, second, third) => visitor.visit_seq(HsvSequence {
                pos: 0,
                first: *first,
                second: *second,
                third: *third,
            }),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct
        map enum identifier struct
    }
}

pub struct HsvSequence {
    first: u32,
    second: u32,
    third: u32,
    pos: u8,
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut HsvSequence {
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.pos {
            1 => visitor.visit_u32(self.first),
            2 => visitor.visit_u32(self.second),
            3 => visitor.visit_u32(self.third),
            _ => Err(BinaryDeError::Message(String::from(
                "unreachable hsv sequence",
            ))),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct seq option newtype_struct ignored_any
        map enum identifier struct tuple_struct tuple
    }
}

impl<'de> SeqAccess<'de> for HsvSequence {
    type Error = BinaryDeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.pos == 4 {
            Ok(None)
        } else {
            self.pos += 1;
            seed.deserialize(self).map(Some)
        }
    }
}

pub struct BinarySequence<'c, 'b: 'c, 'de: 'b, RES> {
    config: &'b BinaryConfig<RES>,
    values: &'c Vec<BorrowedBinaryValue<'de>>,
    idx: usize,
}

impl<'c, 'b, 'de, 'r, RES: TokenResolver> de::Deserializer<'de>
    for &'r mut BinarySequence<'c, 'b, 'de, RES>
{
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.values[self.idx - 1] {
            BorrowedBinaryValue::Scalar(scalar) => visit_key(scalar, self.config, visitor),
            BorrowedBinaryValue::Object(obj) => {
                visitor.visit_map(BinaryMap::new(&self.config, &obj))
            }
            BorrowedBinaryValue::Array(vals) => visitor.visit_seq(BinarySequence {
                values: vals,
                config: self.config,
                idx: 0,
            }),
            BorrowedBinaryValue::Hsv(_, _, _) => Err(BinaryDeError::Message(String::from(
                "unable to support hsv sequence",
            ))),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'c, 'b, 'de, RES: TokenResolver> SeqAccess<'de> for BinarySequence<'c, 'b, 'de, RES> {
    type Error = BinaryDeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.values.len() {
            Ok(None)
        } else {
            self.idx += 1;
            seed.deserialize(self).map(Some)
        }
    }
}

pub struct SpanningSequence<'c, 'b: 'c, 'de: 'b, RES> {
    config: &'b BinaryConfig<RES>,
    doc: &'c Vec<ValueEntry<'de>>,
    value_indices: &'c Vec<usize>,
    idx: usize,
}

impl<'c, 'b, 'de, 'r, RES: TokenResolver> de::Deserializer<'de>
    for &'r mut SpanningSequence<'c, 'b, 'de, RES>
{
    type Error = BinaryDeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.doc[self.value_indices[self.idx - 1]].value {
            BorrowedBinaryValue::Scalar(scalar) => visit_key(scalar, self.config, visitor),
            BorrowedBinaryValue::Object(obj) => {
                visitor.visit_map(BinaryMap::new(&self.config, &obj))
            }
            BorrowedBinaryValue::Array(vals) => visitor.visit_seq(BinarySequence {
                values: vals,
                config: self.config,
                idx: 0,
            }),
            BorrowedBinaryValue::Hsv(first, second, third) => visitor.visit_seq(HsvSequence {
                pos: 0,
                first: *first,
                second: *second,
                third: *third,
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'c, 'b, 'de, RES: TokenResolver> SeqAccess<'de> for SpanningSequence<'c, 'b, 'de, RES> {
    type Error = BinaryDeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.value_indices.len() {
            Ok(None)
        } else {
            self.idx += 1;
            seed.deserialize(self).map(Some)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[test]
    fn test_single_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "ENG".to_string()
            }
        );
    }

    #[test]
    fn test_borrowed_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: &'a str,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: "ENG" });
    }

    #[test]
    fn test_cow_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: Cow<'a, str>,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Cow::Borrowed("ENG"),
            }
        );
    }

    #[test]
    fn test_false_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));
        map.insert(0x284c, String::from("no"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "no".to_string()
            }
        );
    }

    #[test]
    fn test_i32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x59, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 89 });
    }

    #[test]
    fn test_u32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 89 });
    }

    #[test]
    fn test_u64_event() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let mut map = HashMap::new();
        map.insert(0x326b, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 128 });
    }

    #[test]
    fn test_f32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0d, 0x00, 0x17, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 0.023 });
    }

    #[test]
    fn test_q16_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x67, 0x01, 0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 1.78732 });
    }

    #[test]
    fn test_string1_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_string2_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_multiple_top_level_events() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
            field2: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));
        map.insert(0x284d, String::from("field2"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("no"),
                field2: String::from("yes"),
            }
        );
    }

    #[test]
    fn test_string_array() {
        let data = [
            0xe1, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x0a, 0x00, 0x41, 0x72, 0x74, 0x20,
            0x6f, 0x66, 0x20, 0x57, 0x61, 0x72, 0x0f, 0x00, 0x14, 0x00, 0x43, 0x6f, 0x6e, 0x71,
            0x75, 0x65, 0x73, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x50, 0x61, 0x72, 0x61, 0x64, 0x69,
            0x73, 0x65, 0x0f, 0x00, 0x0b, 0x00, 0x52, 0x65, 0x73, 0x20, 0x50, 0x75, 0x62, 0x6c,
            0x69, 0x63, 0x61, 0x0f, 0x00, 0x11, 0x00, 0x57, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x20,
            0x6f, 0x66, 0x20, 0x4e, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            dlc_enabled: Vec<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x2ee1, String::from("dlc_enabled"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                dlc_enabled: vec![
                    String::from("Art of War"),
                    String::from("Conquest of Paradise"),
                    String::from("Res Publica"),
                    String::from("Wealth of Nations"),
                ],
            }
        );
    }

    #[test]
    fn test_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00, 0x00, 0x00, 0xc7, 0x2e,
            0x01, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x00, 0x00, 0xc8, 0x2e, 0x01, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
            second: i32,
            third: i32,
            fourth: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2ec9, String::from("savegame_version"));
        map.insert(0x28e2, String::from("first"));
        map.insert(0x28e3, String::from("second"));
        map.insert(0x2ec7, String::from("third"));
        map.insert(0x2ec8, String::from("fourth"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version {
                    first: 1,
                    second: 11,
                    third: 4,
                    fourth: 0,
                }
            }
        );
    }

    #[test]
    fn test_numerical_identifiers() {
        let data = [
            0x0c, 0x00, 0x59, 0x00, 0x00, 0x00, 0x01, 0x00, 0x0c, 0x00, 0x1e, 0x00, 0x00, 0x00,
        ];

        let map: HashMap<u16, String> = HashMap::new();
        let actual: HashMap<i32, i32> = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual.len(), 1);
        assert_eq!(actual.get(&89), Some(&30));
    }

    #[test]
    fn test_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x0b, 0x00]);
        data.extend_from_slice(b"1444.11.11\n");

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            flags: HashMap<String, String>,
        }

        let mut map = HashMap::new();
        map.insert(0x29cc, String::from("flags"));

        let mut expected_map = HashMap::new();
        expected_map.insert(
            String::from("schools_initiated"),
            String::from("1444.11.11\n"),
        );

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                flags: expected_map
            }
        )
    }

    #[test]
    fn test_no_equal_object() {
        let data = [
            0xf1, 0x36, 0x03, 0x00, 0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            map_area_data: MapData,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MapData {
            #[serde(alias = "type")]
            _type: String,
        }

        let mut map = HashMap::new();
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x00e1, String::from("type"));
        map.insert(0x28be, String::from("general"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                map_area_data: MapData {
                    _type: String::from("general"),
                }
            }
        );
    }

    #[test]
    fn test_empty_array() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(alias = "type")]
            _type: Vec<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x00e1, String::from("type"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { _type: vec![] });
    }

    #[test]
    fn test_array_of_objects() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4c, 0x28, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            technology_group: Vec<Data>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Data {
            map_area_data: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2863, String::from("technology_group"));
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                technology_group: vec![
                    Data {
                        map_area_data: String::from("no")
                    },
                    Data {
                        map_area_data: String::from("yes")
                    }
                ]
            }
        );
    }

    #[test]
    fn test_skip_unwanted() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28, 0x04, 0x00, 0x04, 0x00,
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x326b, String::from("field1"));
        map.insert(0x2863, String::from("technology_group"));
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 128 });
    }

    #[test]
    fn test_consecutive_fields() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x14, 0x00, 0x5a, 0x00, 0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00,
            0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Vec<u32>,
            second: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x28e3, "second");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: vec![89, 90],
                second: 11
            }
        );
    }

    #[test]
    fn test_error_unresolved_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let map: HashMap<u16, String> = HashMap::new();
        let actual: Result<MyStruct, _> = BinaryDeserializerBuilder::new()
            .on_failed_resolve(FailedResolveStrategy::Error)
            .from_slice(&data[..], &map);
        assert!(actual.is_err());
    }

    #[test]
    fn test_stringify_unresolved_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let map: HashMap<u16, String> = HashMap::new();
        let actual: HashMap<String, &str> = BinaryDeserializerBuilder::new()
            .on_failed_resolve(FailedResolveStrategy::Stringify)
            .from_slice(&data[..], &map)
            .unwrap();
        let mut expected = HashMap::new();
        expected.insert(String::from("0x2d82"), "ENG");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_optional_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Some("ENG".to_string())
            }
        );
    }

    #[test]
    fn test_tuple_struct_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47, 0x0f,
            0x00, 0x03, 0x00, 0x4f, 0x4e, 0x47, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: MyFlags,
        }

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyFlags(String, String);

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: MyFlags("ENG".to_string(), "ONG".to_string())
            }
        );
    }

    #[test]
    fn test_tuple_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47, 0x0f,
            0x00, 0x03, 0x00, 0x4f, 0x4e, 0x47, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: (String, String),
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: ("ENG".to_string(), "ONG".to_string())
            }
        );
    }

    #[test]
    fn test_newtype_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: MyString,
        }

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyString(String);

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: MyString("ENG".to_string())
            }
        );
    }

    #[test]
    fn test_consecutive_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x04, 0x00, 0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00,
            0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00,
            0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Vec<Version>,
            field1: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x2ec9, "savegame_version");
        map.insert(0x28e2, "first");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: vec![Version { first: 1 }, Version { first: 2 }],
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_consecutive_nested_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d,
            0x01, 0x00, 0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00, 0xc9, 0x2e,
            0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x0c, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Vec<Version>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            field: Vec<MyField>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyField {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "first");
        map.insert(0x2ec9, "savegame_version");
        map.insert(0x28e2, "field");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: vec![
                    Version {
                        field: vec![MyField { first: 2 }]
                    },
                    Version {
                        field: vec![MyField { first: 3 }]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_non_consecutive_fields() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x14, 0x00, 0x5a, 0x00, 0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00,
            0x00, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x5b, 0x00, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Vec<u32>,
            second: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x28e3, "second");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: vec![89, 90, 91],
                second: 11
            }
        );
    }

    #[test]
    fn test_meta() {
        let data = &include_bytes!("../../../../assets/fixtures/meta.bin")["EU4bin".len()..];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct Meta {
            campaign_id: String,
        }

        let mut map = HashMap::new();
        map.insert(0x337f, "campaign_id");

        let actual: Meta = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            Meta {
                campaign_id: String::from("72ce90e3-eff3-4be4-9395-f1c3d33fd1c7"),
            }
        );
    }
}
