use crate::{
    de::ColorSequence, BinaryFlavor, BinaryTape, BinaryToken, DefaultFlavor, DeserializeError,
    DeserializeErrorKind, Error, FailedResolveStrategy, TokenResolver,
};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::borrow::Cow;

/// A structure to deserialize binary data into Rust values.
///
/// By default, if a token is unable to be resolved then it will be ignored by the default.
/// Construct a custom instance through the `builder` method to tweak this behavior.
///
/// The example below demonstrates multiple ways to deserialize data
///
/// ```
/// use jomini::{BinaryDeserializer, BinaryTape};
/// use serde::Deserialize;
/// use std::collections::HashMap;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructA {
///   #[serde(flatten)]
///   b: StructB,
///
///   #[serde(flatten)]
///   c: StructC,
/// }
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructB {
///   field1: String,
/// }
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructC {
///   field2: String,
/// }
///
/// let data = [
///    0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
///    0x83, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x48,
/// ];
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, String::from("field1"));
/// map.insert(0x2d83, String::from("field2"));
///
/// // the data can be parsed and deserialized in one step
/// let a: StructA = BinaryDeserializer::from_slice(&data[..], &map)?;
/// assert_eq!(a, StructA {
///   b: StructB { field1: "ENG".to_string() },
///   c: StructC { field2: "ENH".to_string() },
/// });
///
/// // or split into two steps, whatever is appropriate.
/// let tape = BinaryTape::from_slice(&data[..])?;
/// let b: StructB = BinaryDeserializer::from_tape(&tape, &map)?;
/// let c: StructC = BinaryDeserializer::from_tape(&tape, &map)?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: "ENH".to_string() });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub struct BinaryDeserializer;

impl BinaryDeserializer {
    /// Create a builder to custom binary deserialization
    pub fn builder() -> BinaryDeserializerBuilder<DefaultFlavor> {
        BinaryDeserializerBuilder::with_flavor(DefaultFlavor)
    }

    /// A customized builder for a certain flavor of binary data
    pub fn builder_flavor<F>(flavor: F) -> BinaryDeserializerBuilder<F>
    where
        F: BinaryFlavor,
    {
        BinaryDeserializerBuilder::with_flavor(flavor)
    }

    /// Deserialize a structure from the given binary data. Convenience method
    /// that combines parsing and deserialization in one step
    pub fn from_slice<'de, 'res: 'de, RES, T>(
        data: &'de [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'de>,
        RES: TokenResolver,
    {
        BinaryDeserializer::builder()
            .on_failed_resolve(FailedResolveStrategy::Ignore)
            .from_slice(data, resolver)
    }

    /// Deserialize a structure from the already parsed binary data. Useful for when
    /// one needs to deserialize a single set of data into more than one structure.
    pub fn from_tape<'de, 'res: 'de, RES, T>(
        tape: &BinaryTape<'de>,
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'de>,
        RES: TokenResolver,
    {
        BinaryDeserializer::builder()
            .on_failed_resolve(FailedResolveStrategy::Ignore)
            .from_tape(tape, resolver)
    }
}

/// Build a tweaked binary deserializer
///
/// ```
/// use jomini::{BinaryDeserializer, BinaryTape, FailedResolveStrategy};
/// use serde::Deserialize;
/// use std::collections::HashMap;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructB {
///   field1: String,
/// }
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructC {
///   field2: String,
/// }
///
/// let data = [
///    0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
///    0x83, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x48,
/// ];
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, String::from("field1"));
/// map.insert(0x2d83, String::from("field2"));
///
/// let mut deserializer = BinaryDeserializer::builder();
/// deserializer.on_failed_resolve(FailedResolveStrategy::Error);
///
/// let tape = BinaryTape::from_slice(&data[..])?;
/// let b: StructB = deserializer.from_tape(&tape, &map)?;
/// let c: StructC = deserializer.from_tape(&tape, &map)?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: "ENH".to_string() });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Debug)]
pub struct BinaryDeserializerBuilder<F> {
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
}

impl<F> BinaryDeserializerBuilder<F>
where
    F: BinaryFlavor,
{
    /// Create a new builder instance
    pub fn with_flavor(flavor: F) -> Self {
        BinaryDeserializerBuilder {
            failed_resolve_strategy: FailedResolveStrategy::Ignore,
            flavor,
        }
    }

    /// Set the behavior when a unknown token is encountered
    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.failed_resolve_strategy = strategy;
        self
    }

    /// Deserialize the given binary tape
    pub fn from_tape<'a, 'b, 'c, 'res: 'a, RES, T>(
        &'b self,
        tape: &'c BinaryTape<'a>,
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'a>,
        RES: TokenResolver,
    {
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
        };

        let mut deserializer = RootDeserializer {
            tokens: tape.tokens(),
            config: &config,
        };
        Ok(T::deserialize(&mut deserializer)?)
    }

    /// Convenience method for parsing and deserializing binary data in a single step
    pub fn from_slice<'a, 'b, 'res: 'a, RES, T>(
        &'b self,
        data: &'a [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'a>,
        RES: TokenResolver,
    {
        let tape = BinaryTape::from_slice(data)?;
        Ok(self.from_tape(&tape, resolver)?)
    }
}

struct BinaryConfig<'res, RES> {
    resolver: &'res RES,
    failed_resolve_strategy: FailedResolveStrategy,
}

struct RootDeserializer<'b, 'a: 'b, 'res: 'a, RES> {
    tokens: &'b [BinaryToken<'a>],
    config: &'b BinaryConfig<'res, RES>,
}

impl<'b, 'de, 'r, 'res, RES: TokenResolver> de::Deserializer<'de>
    for &'r mut RootDeserializer<'b, 'de, 'res, RES>
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "root deserializer can only work with key value pairs",
            )),
        })
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(BinaryMap::new(
            self.config,
            self.tokens,
            0,
            self.tokens.len(),
        ))
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

struct BinaryMap<'c, 'a: 'c, 'de: 'a, 'res: 'de, RES: 'a> {
    config: &'a BinaryConfig<'res, RES>,
    tokens: &'c [BinaryToken<'de>],
    tape_idx: usize,
    end_idx: usize,
    value_ind: usize,
}

impl<'c, 'a, 'de, 'res: 'de, RES> BinaryMap<'c, 'a, 'de, 'res, RES> {
    fn new(
        config: &'a BinaryConfig<'res, RES>,
        tokens: &'c [BinaryToken<'de>],
        tape_idx: usize,
        end_idx: usize,
    ) -> Self {
        BinaryMap {
            config,
            tokens,
            tape_idx,
            end_idx,
            value_ind: 0,
        }
    }
}

impl<'c, 'de, 'a, 'res: 'de, RES: TokenResolver> MapAccess<'de>
    for BinaryMap<'c, 'a, 'de, 'res, RES>
{
    type Error = DeserializeError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if self.tape_idx < self.end_idx {
            let current_idx = self.tape_idx;

            self.value_ind = self.tape_idx + 1;
            let next_key = match self.tokens[self.value_ind] {
                BinaryToken::Array(x) => x,
                BinaryToken::Object(x) => x,
                _ => self.value_ind,
            };

            self.tape_idx = next_key + 1;
            seed.deserialize(KeyDeserializer {
                tape_idx: current_idx,
                tokens: self.tokens,
                config: self.config,
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            value_ind: self.value_ind,
            tokens: &self.tokens,
            config: self.config,
        })
    }
}

struct KeyDeserializer<'b, 'de: 'b, 'res: 'de, RES> {
    config: &'b BinaryConfig<'res, RES>,
    tokens: &'b [BinaryToken<'de>],
    tape_idx: usize,
}

fn visit_key<'c, 'b: 'c, 'de: 'b, 'res: 'de, RES: TokenResolver, V: Visitor<'de>>(
    tape_idx: usize,
    tokens: &'b [BinaryToken<'de>],
    config: &'b BinaryConfig<'res, RES>,
    visitor: V,
) -> Result<V::Value, DeserializeError> {
    match tokens[tape_idx] {
        BinaryToken::Object(_)
        | BinaryToken::Array(_)
        | BinaryToken::End(_)
        | BinaryToken::Rgb(_) => Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from("unable to deserialize key type")),
        }),
        BinaryToken::Bool(x) => visitor.visit_bool(x),
        BinaryToken::U32(x) => visitor.visit_u32(x),
        BinaryToken::U64(x) => visitor.visit_u64(x),
        BinaryToken::I32(x) => visitor.visit_i32(x),
        BinaryToken::Text(x) => match x.to_utf8() {
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
            Cow::Owned(s) => visitor.visit_string(s),
        },
        BinaryToken::F32_1(x) => visitor.visit_f32(x),
        BinaryToken::F32_2(x) => visitor.visit_f32(x),
        BinaryToken::Token(s) => match config.resolver.resolve(s) {
            Some(id) => visitor.visit_borrowed_str(id),
            None => match config.failed_resolve_strategy {
                FailedResolveStrategy::Error => Err(DeserializeError {
                    kind: DeserializeErrorKind::UnknownToken { token_id: s },
                }),
                FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{:x}", s)),
                FailedResolveStrategy::Ignore => {
                    visitor.visit_borrowed_str("__internal_identifier_ignore")
                }
            },
        },
    }
}

impl<'b, 'de, 'res: 'de, RES: TokenResolver> de::Deserializer<'de>
    for KeyDeserializer<'b, 'de, 'res, RES>
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visit_key(self.tape_idx, self.tokens, self.config, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum ignored_any identifier struct
    }
}

struct ValueDeserializer<'c, 'b: 'c, 'de: 'b, 'res: 'de, RES> {
    config: &'b BinaryConfig<'res, RES>,
    value_ind: usize,
    tokens: &'c [BinaryToken<'de>],
}

impl<'c, 'b, 'de, 'res: 'de, RES: TokenResolver> de::Deserializer<'de>
    for ValueDeserializer<'c, 'b, 'de, 'res, RES>
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Array(x) => visitor.visit_seq(BinarySequence {
                config: self.config,
                tokens: self.tokens,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
            }),
            BinaryToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(**x)),
            BinaryToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(&self.config, self.tokens, idx + 1, *x))
            }
            BinaryToken::End(_x) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered end when trying to deserialize",
                )),
            }),
            _ => visit_key(idx, self.tokens, self.config, visitor),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Array(x) => visitor.visit_seq(BinarySequence {
                config: self.config,
                tokens: self.tokens,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
            }),
            BinaryToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(**x)),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered non-array when trying to deserialize array",
                )),
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

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(&self.config, self.tokens, idx + 1, *x))
            }

            // An array is supported if it is empty
            BinaryToken::Array(x) => {
                visitor.visit_map(BinaryMap::new(&self.config, self.tokens, idx + 1, *x))
            }
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered unexpected token when trying to deserialize map",
                )),
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct
        enum identifier
    }
}

struct BinarySequence<'b, 'de: 'b, 'res: 'de, RES> {
    config: &'b BinaryConfig<'res, RES>,
    tokens: &'b [BinaryToken<'de>],
    idx: usize,
    de_idx: usize,
    end_idx: usize,
}

impl<'b, 'de, 'r, 'res: 'de, RES: TokenResolver> de::Deserializer<'de>
    for &'r mut BinarySequence<'b, 'de, 'res, RES>
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.tokens[self.de_idx] {
            BinaryToken::Object(x) => visitor.visit_map(BinaryMap::new(
                self.config,
                self.tokens,
                self.de_idx + 1,
                *x,
            )),
            BinaryToken::Array(x) => visitor.visit_seq(BinarySequence {
                config: self.config,
                tokens: self.tokens,
                de_idx: 0,
                idx: self.de_idx + 1,
                end_idx: *x,
            }),
            BinaryToken::End(_x) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered unexpected token when trying to deserialize map",
                )),
            }),
            _ => visit_key(self.de_idx, self.tokens, self.config, visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'b, 'de, 'res: 'de, RES: TokenResolver> SeqAccess<'de> for BinarySequence<'b, 'de, 'res, RES> {
    type Error = DeserializeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.end_idx {
            Ok(None)
        } else {
            let next_key = match self.tokens[self.idx] {
                BinaryToken::Array(x) => x,
                BinaryToken::Object(x) => x,
                _ => self.idx,
            };

            self.de_idx = self.idx;
            self.idx = next_key + 1;
            seed.deserialize(self).map(Some)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use jomini_derive::JominiDeserialize;
    use serde::{de::Deserializer, Deserialize};
    use std::collections::HashMap;
    use std::fmt;

    fn from_slice<'a, 'res: 'a, RES, T>(data: &'a [u8], resolver: &'res RES) -> Result<T, Error>
    where
        T: Deserialize<'a>,
        RES: TokenResolver,
    {
        BinaryDeserializer::from_slice(data, resolver)
    }

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
        data.extend_from_slice(&0x0004u16.to_le_bytes());

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            flags: HashMap<String, String>,
        }

        let mut map = HashMap::new();
        map.insert(0x29cc, String::from("flags"));

        let mut expected_map = HashMap::new();
        expected_map.insert(
            String::from("schools_initiated"),
            String::from("1444.11.11"),
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
    fn test_escaped_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x16, 0x00]);
        data.extend_from_slice(br#"Joe \"Captain\" Rogers"#);
        data.extend_from_slice(&0x0004u16.to_le_bytes());

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            flags: HashMap<String, String>,
        }

        let mut map = HashMap::new();
        map.insert(0x29cc, String::from("flags"));

        let mut expected_map = HashMap::new();
        expected_map.insert(
            String::from("schools_initiated"),
            String::from(r#"Joe "Captain" Rogers"#),
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

        #[derive(JominiDeserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
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
        let actual: Result<MyStruct, _> = BinaryDeserializer::builder()
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
        let actual: HashMap<String, &str> = BinaryDeserializer::builder()
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

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
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

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            savegame_version: Vec<Version>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct Version {
            #[jomini(duplicated)]
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
    fn test_consecutive_empty_nested_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00, 0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00,
            0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x03, 0x00,
            0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            savegame_version: Vec<Version>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct Version {
            #[jomini(duplicated)]
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
                    Version { field: vec![] },
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

        #[derive(JominiDeserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
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
        let data = &include_bytes!("../../tests/fixtures/meta.bin")["EU4bin".len()..];

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

    #[test]
    fn test_deserialize_rgb() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x053a, "color");

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: Color {
                    red: 110,
                    blue: 27,
                    green: 27
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: Color,
        }

        #[derive(Debug, PartialEq)]
        struct Color {
            red: i32,
            blue: i32,
            green: i32,
        }

        impl<'de> Deserialize<'de> for Color {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct ColorVisitor;

                impl<'de> Visitor<'de> for ColorVisitor {
                    type Value = Color;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a color")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::SeqAccess<'de>,
                    {
                        let r = seq.next_element::<i32>()?.expect("red to be present");
                        let g = seq.next_element::<i32>()?.expect("green to be present");
                        let b = seq.next_element::<i32>()?.expect("blue to be present");

                        if seq.next_element::<de::IgnoredAny>()?.is_some() {
                            Err(de::Error::custom("unexpected extra rgb value"))
                        } else {
                            Ok(Color {
                                red: r,
                                blue: b,
                                green: g,
                            })
                        }
                    }
                }

                deserializer.deserialize_seq(ColorVisitor)
            }
        }
    }
}
