use crate::{
    de::ColorSequence, DeserializeError, DeserializeErrorKind, Encoding, Error, Scalar, TextTape,
    TextToken, Utf8Encoding, Windows1252Encoding,
};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::borrow::Cow;

/// A structure to deserialize text data into Rust values.
///
/// By default, if a token is unable to be resolved then it will be ignored by the default.
/// Construct a custom instance through the `builder` method to tweak this behavior.
///
/// The example below demonstrates multiple ways to deserialize data
///
/// ```
/// use jomini::{TextDeserializer, TextTape};
/// use serde::Deserialize;
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
/// let data = b"field1=ENG field2=ENH";
///
/// // the data can be parsed and deserialized in one step
/// let a: StructA = TextDeserializer::from_windows1252_slice(&data[..])?;
/// assert_eq!(a, StructA {
///   b: StructB { field1: "ENG".to_string() },
///   c: StructC { field2: "ENH".to_string() },
/// });
///
/// // or split into two steps, whatever is appropriate.
/// let tape = TextTape::from_slice(&data[..])?;
/// let b: StructB = TextDeserializer::from_windows1252_tape(&tape)?;
/// let c: StructC = TextDeserializer::from_windows1252_tape(&tape)?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: "ENH".to_string() });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub struct TextDeserializer;

impl TextDeserializer {
    /// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
    pub fn from_windows1252_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer::from_windows1252_tape(&tape)?)
    }

    /// Deserialize the given text tape assuming quoted strings are windows1252 encoded.
    pub fn from_windows1252_tape<'b, 'a: 'b, T>(tape: &'b TextTape<'a>) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        Self::from_encoded_tape(tape, Windows1252Encoding::new())
    }

    /// Convenience method for parsing the given text data and deserializing as utf8 encoded.
    pub fn from_utf8_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer::from_utf8_tape(&tape)?)
    }

    /// Deserialize the given text tape assuming quoted strings are utf8 encoded.
    pub fn from_utf8_tape<'b, 'a: 'b, T>(tape: &'b TextTape<'a>) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        Self::from_encoded_tape(tape, Utf8Encoding::new())
    }

    /// Deserialize the given text tape assuming quoted strings can be decoded
    /// according to the given encoder
    pub fn from_encoded_tape<'b, 'a: 'b, T, E>(
        tape: &'b TextTape<'a>,
        encoding: E,
    ) -> Result<T, Error>
    where
        T: Deserialize<'a>,
        E: Encoding,
    {
        let mut root = RootDeserializer {
            tokens: tape.tokens(),
            encoding: &encoding,
        };
        Ok(T::deserialize(&mut root)?)
    }
}

#[derive(Debug)]
pub struct RootDeserializer<'b, 'a: 'b, E> {
    tokens: &'b [TextToken<'a>],
    encoding: &'b E,
}

impl<'b, 'de, 'r, E> de::Deserializer<'de> for &'r mut RootDeserializer<'b, 'de, E>
where
    E: Encoding,
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
            &self.tokens,
            0,
            self.tokens.len(),
            self.encoding,
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

struct BinaryMap<'a, 'de: 'a, E> {
    tokens: &'a [TextToken<'de>],
    tape_idx: usize,
    end_idx: usize,
    value_ind: usize,
    encoding: &'a E,
}

impl<'a, 'de, E> BinaryMap<'a, 'de, E>
where
    E: Encoding,
{
    fn new(tokens: &'a [TextToken<'de>], tape_idx: usize, end_idx: usize, encoding: &'a E) -> Self {
        BinaryMap {
            tokens,
            tape_idx,
            end_idx,
            value_ind: 0,
            encoding,
        }
    }
}

impl<'de, 'a, E> MapAccess<'de> for BinaryMap<'a, 'de, E>
where
    E: Encoding,
{
    type Error = DeserializeError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            if self.tape_idx < self.end_idx {
                let current_idx = self.tape_idx;

                self.value_ind = self.tape_idx + 1;
                let next_key = match self.tokens[self.value_ind] {
                    TextToken::Array(x) => x,
                    TextToken::Object(x) => x,
                    TextToken::End(_) => {
                        // this really shouldn't happen but if it does we just
                        // move our sights to the end token and continue on
                        self.tape_idx = self.value_ind;
                        continue;
                    }
                    _ => self.value_ind,
                };

                debug_assert!(next_key + 1 > self.tape_idx);
                self.tape_idx = next_key + 1;

                return seed
                    .deserialize(KeyDeserializer {
                        tape_idx: current_idx,
                        tokens: self.tokens,
                        encoding: self.encoding,
                    })
                    .map(Some);
            } else {
                return Ok(None);
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            value_ind: self.value_ind,
            tokens: &self.tokens,
            encoding: self.encoding,
        })
    }
}

fn ensure_scalar<'a>(s: &TextToken<'a>) -> Result<Scalar<'a>, DeserializeError> {
    match s {
        TextToken::Scalar(s) => Ok(*s),
        x => Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(format!("{:?} is not a scalar", x)),
        }),
    }
}

struct KeyDeserializer<'b, 'de: 'b, E> {
    tokens: &'b [TextToken<'de>],
    tape_idx: usize,
    encoding: &'b E,
}

impl<'b, 'de, E> KeyDeserializer<'b, 'de, E> {
    fn new(tokens: &'b [TextToken<'de>], tape_idx: usize, encoding: &'b E) -> Self {
        KeyDeserializer {
            tokens,
            tape_idx,
            encoding,
        }
    }
}

impl<'b, 'de: 'b, E> de::Deserializer<'de> for KeyDeserializer<'b, 'de, E>
where
    E: Encoding,
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from("can't deserialize map as key")),
        })
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from("can't deserialize seq as key")),
        })
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "can't deserialize struct as key",
            )),
        })
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

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx])
            .and_then(|s| visitor.visit_string(self.encoding.decode(s.view_data()).into_owned()))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx]).and_then(|s| {
            match self.encoding.decode(s.view_data()) {
                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            }
        })
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx])
            .and_then(|x| Ok(x.to_bool()?))
            .and_then(|x| visitor.visit_bool(x))
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx])
            .and_then(|x| Ok(x.to_u64()?))
            .and_then(|x| visitor.visit_u64(x))
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx])
            .and_then(|x| Ok(x.to_i64()?))
            .and_then(|x| visitor.visit_i64(x))
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ensure_scalar(&self.tokens[self.tape_idx])
            .and_then(|x| Ok(x.to_f64()?))
            .and_then(|x| visitor.visit_f64(x))
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i128<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_u128<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        todo!()
    }
}

#[derive(Debug)]
struct ValueDeserializer<'b, 'de: 'b, E> {
    value_ind: usize,
    tokens: &'b [TextToken<'de>],
    encoding: &'b E,
}

impl<'b, 'de, E> de::Deserializer<'de> for ValueDeserializer<'b, 'de, E>
where
    E: Encoding,
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                tokens: self.tokens,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
                encoding: self.encoding,
            }),
            TextToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(**x)),
            TextToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.tokens, idx + 1, *x, self.encoding))
            }
            TextToken::End(_x) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered end when trying to deserialize",
                )),
            }),
            _ => self.deserialize_str(visitor),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                tokens: self.tokens,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
                encoding: self.encoding,
            }),
            TextToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(**x)),
            TextToken::End(_x) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered end when trying to deserialize",
                )),
            }),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered non-seq when trying to deserialize seq",
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

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_string(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_str(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_bool(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_u64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_u32(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_u16(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_u8(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_i32(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_i16(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_i8(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_f64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.value_ind, self.encoding).deserialize_f32(visitor)
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
            TextToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.tokens, idx + 1, *x, self.encoding))
            }

            // An array is supported if it is empty
            TextToken::Array(x) => {
                visitor.visit_map(BinaryMap::new(self.tokens, idx + 1, *x, self.encoding))
            }
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered unexpected token when trying to deserialize map",
                )),
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        i128 u128 char
        bytes byte_buf unit unit_struct identifier
        enum
    }
}

#[derive(Debug)]
struct BinarySequence<'b, 'de: 'b, E> {
    tokens: &'b [TextToken<'de>],
    idx: usize,
    de_idx: usize,
    end_idx: usize,
    encoding: &'b E,
}

impl<'b, 'de, 'r, E> de::Deserializer<'de> for &'r mut BinarySequence<'b, 'de, E>
where
    E: Encoding,
{
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.tokens[self.de_idx] {
            TextToken::Object(x) => visitor.visit_map(BinaryMap::new(
                self.tokens,
                self.de_idx + 1,
                *x,
                self.encoding,
            )),
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                tokens: self.tokens,
                de_idx: 0,
                idx: self.de_idx + 1,
                end_idx: *x,
                encoding: self.encoding,
            }),
            TextToken::Rgb(_) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("unable to deserialize rgb")),
            }),
            TextToken::Scalar(_x) => self.deserialize_str(visitor),
            TextToken::End(_x) => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered end when trying to deserialize",
                )),
            }),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_string(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_str(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_bool(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_u64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_u32(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_u16(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_u8(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_i32(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_i16(visitor)
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_i8(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_f64(visitor)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        KeyDeserializer::new(self.tokens, self.de_idx, self.encoding).deserialize_f32(visitor)
    }

    serde::forward_to_deserialize_any! {
        i128 u128 char
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'b, 'de, E> SeqAccess<'de> for BinarySequence<'b, 'de, E>
where
    E: Encoding,
{
    type Error = DeserializeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.end_idx {
            Ok(None)
        } else {
            let next_key = match self.tokens[self.idx] {
                TextToken::Array(x) => x,
                TextToken::Object(x) => x,
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
    use serde::{
        de::{self, Deserializer},
        Deserialize,
    };
    use std::collections::HashMap;
    use std::fmt;

    fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        Ok(TextDeserializer::from_windows1252_slice(data)?)
    }

    #[test]
    fn test_single_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "ENG".to_string()
            }
        );
    }

    #[test]
    fn test_borrowed_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: &'a str,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: "ENG" });
    }

    #[test]
    fn test_cow_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: Cow<'a, str>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Cow::Borrowed("ENG")
            }
        );
    }

    #[test]
    fn test_escaped_field() {
        let data = br#"name = "Joe \"Captain\" Rogers""#;

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            name: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                name: r#"Joe "Captain" Rogers"#.to_string(),
            }
        );
    }

    #[test]
    fn test_false_field() {
        let data = b"field1=no";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: bool,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: false });
    }

    #[test]
    fn test_true_field() {
        let data = b"field1=yes";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: bool,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: true });
    }

    #[test]
    fn test_u64_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u32_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u32,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u8_field() {
        let data = b"field1=100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u8,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: 100 });
    }

    #[test]
    fn test_u16_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u16,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_i8_field() {
        let data = b"field1=-100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i8,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -100 });
    }

    #[test]
    fn test_i16_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i16,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i32_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i32,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i64_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_f32_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -100.535 });
    }

    #[test]
    fn test_f64_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f64,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: -100.535 });
    }

    #[test]
    fn test_multiple_to_level_events() {
        let data = b"field1=yes\r\nfield2=no";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: bool,
            field2: bool,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: true,
                field2: false,
            }
        );
    }

    #[test]
    fn test_string_array() {
        let data = include_bytes!("../../tests/fixtures/string-array.txt");
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            dlc_enabled: Vec<String>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                dlc_enabled: vec![
                    String::from("Conquest of Paradise"),
                    String::from("Wealth of Nations"),
                    String::from("Res Publica"),
                    String::from("Art of War"),
                    String::from("El Dorado"),
                ]
            }
        );
    }

    #[test]
    fn test_nested_object() {
        let data = include_bytes!("../../tests/fixtures/savegame.txt");

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: u32,
            second: u32,
            third: u32,
            forth: u32,
            name: Option<String>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version {
                    first: 1,
                    second: 29,
                    third: 5,
                    forth: 0,
                    name: Some(String::from("Manchu")),
                }
            }
        );
    }

    #[test]
    fn test_empty_array() {
        let data = b"discovered_by = {}";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            discovered_by: Vec<String>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                discovered_by: Vec::new(),
            }
        );
    }

    #[test]
    fn test_array_of_objects() {
        let data = include_bytes!("../../tests/fixtures/campaign_stats.txt");

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            campaign_stats: Vec<Stat>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Stat {
            id: u32,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                campaign_stats: vec![Stat { id: 0 }, Stat { id: 1 },]
            }
        );
    }

    #[test]
    fn test_skip_unwanted() {
        let data = b"a = b\r\nc = d\r\ne = f";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            c: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                c: String::from("d"),
            }
        );
    }

    #[test]
    fn test_consecutive_fields() {
        let data = b"a = b\r\nc = d1\r\nc = d2\r\ne = f";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            a: String,
            #[jomini(duplicated)]
            c: Vec<String>,
            e: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                a: String::from("b"),
                c: vec![String::from("d1"), String::from("d2")],
                e: String::from("f"),
            }
        );
    }

    #[test]
    fn test_non_consecutive_fields() {
        let data = b"c = d1\r\na = b\r\nc = d2\r\ne = f";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            a: String,
            #[jomini(duplicated)]
            c: Vec<String>,
            e: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                a: String::from("b"),
                c: vec![String::from("d1"), String::from("d2")],
                e: String::from("f"),
            }
        );
    }

    #[test]
    fn test_empty_consecutive_fields() {
        let data = b"data = { }";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            data: MyData,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyData {
            #[jomini(duplicated)]
            c: Vec<String>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                data: MyData { c: Vec::new() }
            }
        );
    }

    #[test]
    fn test_optional_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Some(String::from("ENG")),
            }
        );
    }

    #[test]
    fn test_deserialize_hashmap() {
        let data = b"-1=a\r\n-2=b";

        let actual: HashMap<i32, String> = from_slice(&data[..]).unwrap();
        let mut expected = HashMap::new();
        expected.insert(-1, String::from("a"));
        expected.insert(-2, String::from("b"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_deserialize_nested_hashmap() {
        let data = b"provinces={ -1={ name=\"abc\" } }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            provinces: HashMap<i32, Province>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Province {
            name: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        let mut expected = HashMap::new();
        expected.insert(
            -1,
            Province {
                name: String::from("abc"),
            },
        );
        assert_eq!(
            actual,
            MyStruct {
                provinces: expected
            }
        );
    }

    #[test]
    fn test_empty_objects() {
        let data = b"a={foo={bar=val} {} { } me=you}";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            a: MyNest,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyNest {
            foo: MyFoo,
            me: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyFoo {
            bar: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                a: MyNest {
                    foo: MyFoo {
                        bar: String::from("val"),
                    },
                    me: String::from("you"),
                }
            }
        );
    }

    #[test]
    fn test_empty_objects2() {
        let data = b"foo={bar=val {}} me=you";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            foo: MyFoo,
            me: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyFoo {
            bar: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                foo: MyFoo {
                    bar: String::from("val"),
                },
                me: String::from("you"),
            }
        );
    }

    #[test]
    fn test_spanning_objects() {
        let data = b"army={name=abc} army={name=def}";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            army: Vec<Army>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Army {
            name: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                army: vec![
                    Army {
                        name: String::from("abc")
                    },
                    Army {
                        name: String::from("def")
                    },
                ]
            }
        );
    }

    #[test]
    fn test_deserialize_rgb() {
        let data = b"color = rgb { 100 200 150 } ";

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: Color {
                    red: 100,
                    green: 200,
                    blue: 150
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
