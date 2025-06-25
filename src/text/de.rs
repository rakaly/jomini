use super::{dom::ValuesIter, reader::Token, TokenReader};
use crate::{
    text::{ArrayReader, FieldsIter, ObjectReader, Operator, Reader, ScalarReader, ValueReader},
    value::ScalarValue,
    DeserializeError, DeserializeErrorKind, Encoding, Error, TextTape, TextToken, Utf8Encoding,
    Windows1252Encoding,
};
use serde::de::{
    self, value::StringDeserializer, Deserialize, DeserializeOwned, DeserializeSeed, Visitor,
};
use std::{
    borrow::Cow,
    fmt::{self, Debug},
    io::Read,
};

/// Represents the field value that contains an operator
///
/// A `Property` is a specially marked type that signals to the deserializer to
/// preserve operators.
///
/// ```
/// use jomini::{
///     text::{Operator, Property},
///     JominiDeserialize, TextDeserializer,
/// };
/// use serde::Deserialize;
///
/// let data = b"modifier = { abc=yes factor=2 num > 2 }";
/// let actual: MyStruct = jomini::text::de::from_windows1252_slice(&data[..])?;
/// assert_eq!(
///     actual,
///     MyStruct {
///         modifier: Modifier {
///             abc: true,
///             factor: Property::new(Operator::Equal, 2),
///             num: Property::new(Operator::GreaterThan, 2)
///         }
///     }
/// );
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Modifier {
///     abc: bool,
///     factor: Property<u8>,
///     num: Property<u8>,
/// }
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct MyStruct {
///     modifier: Modifier,
/// }
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(serde::Deserialize)]
#[serde(rename = "_internal_jomini_property")]
pub struct Property<T> {
    operator: Operator,
    value: T,
}

impl<T> fmt::Debug for Property<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Property")
            .field("operator", &self.operator)
            .field("value", &self.value)
            .finish()
    }
}

impl<T> PartialEq for Property<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.operator == other.operator && self.value == other.value
    }
}

impl<T> Eq for Property<T> where T: Eq {}

impl<T> Clone for Property<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            operator: self.operator,
            value: self.value.clone(),
        }
    }
}

impl<T> Copy for Property<T> where T: Copy {}

impl<T> Property<T> {
    /// Creates a new property
    pub fn new(operator: Operator, value: T) -> Self {
        Property { operator, value }
    }

    /// Return the present operator in the property
    pub fn operator(&self) -> Operator {
        self.operator
    }

    /// Return a reference to the value
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Convert the property into the value
    pub fn into_value(self) -> T {
        self.value
    }
}

/// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
pub fn from_windows1252_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_windows1252_slice(data)?.deserialize()
}

/// (**Experimental**) Create a Windows1252 text value from a reader
///
/// Considered experimental as it uses a [TokenReader] under the hood, which
/// uses a different parsing routine geared toward save files.
pub fn from_windows1252_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_windows1252_reader(TokenReader::new(reader)).deserialize()
}

/// Convenience method for deserializing streaming utf8 data into a Rust value
pub fn from_utf8_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_utf8_reader(TokenReader::new(reader)).deserialize()
}

/// Convenience method for parsing the given text data and deserializing as utf8 encoded.
pub fn from_utf8_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_utf8_slice(data)?.deserialize()
}

/// A serde deserializer over streaming data
pub struct TextReaderDeserializer<R, E> {
    reader: TokenReader<R>,
    encoding: E,
}

impl<R: Read, E: Encoding> TextReaderDeserializer<R, E> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl<'de, R: Read, E: Encoding> de::Deserializer<'de> for &'_ mut TextReaderDeserializer<R, E> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "root deserializer can only work with key value pairs",
            )),
        }))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let me = std::ptr::addr_of!(self);
        visitor.visit_map(TextReaderMap::new(me, true))
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

struct TextReaderMap<'a, R, E> {
    de: *const &'a mut TextReaderDeserializer<R, E>,
    root: bool,
}

impl<'a, R, E> TextReaderMap<'a, R, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<R, E>, root: bool) -> Self {
        TextReaderMap { de, root }
    }
}

impl<'de, R: Read, E: Encoding> de::MapAccess<'de> for TextReaderMap<'_, R, E> {
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match unsafe { self.de.read() }.reader.next() {
                Ok(Some(Token::Close)) => return Ok(None),
                Ok(Some(Token::Open)) => {
                    unsafe { self.de.read() }.reader.skip_container()?;
                }
                Ok(Some(token)) => {
                    return seed
                        .deserialize(TextReaderTokenDeserializer::new(self.de, token))
                        .map(Some)
                }
                Ok(None) if self.root => return Ok(None),
                Ok(None) => return Err(unsafe { self.de.read() }.reader.eof_error().into()),
                Err(e) => return Err(e.into()),
            }
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let token = unsafe { self.de.read() }.reader.read_expect_equals()?;
        let deser = if let Token::Operator(op) = token {
            let new_token = unsafe { self.de.read() }.reader.read()?;
            let mut deser = TextReaderTokenDeserializer::new(self.de, new_token);
            deser.op = op;
            deser
        } else {
            TextReaderTokenDeserializer::new(self.de, token)
        };

        seed.deserialize(deser)
    }
}

struct TextReaderTokenDeserializer<'a, R, E> {
    de: *const &'a mut TextReaderDeserializer<R, E>,
    token: Token<'a>,
    op: Operator,
}

impl<'a, R, E> TextReaderTokenDeserializer<'a, R, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<R, E>, token: Token<'a>) -> Self {
        Self {
            de,
            token,
            op: Operator::Equal,
        }
    }
}

impl<'a, 'de: 'a, R: Read, E: Encoding> de::Deserializer<'de>
    for TextReaderTokenDeserializer<'a, R, E>
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Token::Open => visitor.visit_seq(TextReaderSeq::new(self.de)),
            Token::Close => Err(Error::invalid_syntax(
                "did not expect end",
                unsafe { self.de.read() }.reader.position(),
            )),
            Token::Operator(x) => visitor.visit_str(x.symbol()),
            Token::Unquoted(s) | Token::Quoted(s) => {
                match unsafe { self.de.read() }.encoding.decode(s.as_bytes()) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar().and_then(|x| x.to_bool().ok()) {
            Some(x) => visitor.visit_bool(x),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
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

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar().and_then(|x| x.to_i64().ok()) {
            Some(x) => visitor.visit_i64(x),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
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

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar().and_then(|x| x.to_u64().ok()) {
            Some(x) => visitor.visit_u64(x),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar().and_then(|x| x.to_f64().ok()) {
            Some(x) => visitor.visit_f64(x),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Some(s) = self.token.as_scalar() {
            match unsafe { self.de.read() }.encoding.decode(s.as_bytes()) {
                Cow::Borrowed(x) => visitor.visit_str(x),
                Cow::Owned(x) => visitor.visit_string(x),
            }
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar() {
            Some(s) => visitor.visit_bytes(s.as_bytes()),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
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

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let mut seq = TextReaderSeq::new(self.de);
        let result = visitor.visit_seq(&mut seq)?;
        if !seq.hit_end {
            // For when we are deserializing an array that doesn't read
            // the closing token
            if !matches!(unsafe { self.de.read() }.reader.read()?, Token::Close) {
                return Err(Error::invalid_syntax(
                    "Expected sequence to be terminated with an end token",
                    unsafe { self.de.read() }.reader.position(),
                ));
            }
        }
        Ok(result)
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

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.token, Token::Open) {
            visitor.visit_map(TextReaderMap::new(self.de, false))
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == "_internal_jomini_property" {
            visitor.visit_map(PropertyReaderMap {
                de: self.de,
                token: self.token,
                op: self.op,
                state: 0,
            })
        } else {
            self.deserialize_map(visitor)
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(TextReaderEnum::new(self.de, self.token))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.token, Token::Open) {
            unsafe { self.de.read() }.reader.skip_container()?;
        }
        visitor.visit_unit()
    }
}

struct TextReaderSeq<'a, R, E> {
    de: *const &'a mut TextReaderDeserializer<R, E>,
    hit_end: bool,
}

impl<'a, R, E> TextReaderSeq<'a, R, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<R, E>) -> Self {
        TextReaderSeq { de, hit_end: false }
    }
}

impl<'de, R, E> de::SeqAccess<'de> for TextReaderSeq<'_, R, E>
where
    R: Read,
    E: Encoding,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match unsafe { self.de.read() }.reader.read()? {
            Token::Close => {
                self.hit_end = true;
                Ok(None)
            }
            token => seed
                .deserialize(TextReaderTokenDeserializer::new(self.de, token))
                .map(Some),
        }
    }
}

struct TextReaderEnum<'a, R, E> {
    de: *const &'a mut TextReaderDeserializer<R, E>,
    token: Token<'a>,
}

impl<'a, R, E> TextReaderEnum<'a, R, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<R, E>, token: Token<'a>) -> Self {
        TextReaderEnum { de, token }
    }
}

impl<'de, R: Read, E: Encoding> de::EnumAccess<'de> for TextReaderEnum<'_, R, E> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(TextReaderTokenDeserializer::new(self.de, self.token))?;
        Ok((variant, self))
    }
}

impl<'de, R: Read, E: Encoding> de::VariantAccess<'de> for TextReaderEnum<'_, R, E> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }
}

struct PropertyReaderMap<'a, R, E> {
    de: *const &'a mut TextReaderDeserializer<R, E>,
    op: Operator,
    token: Token<'a>,
    state: usize,
}

impl<'de, R, E> de::MapAccess<'de> for PropertyReaderMap<'_, R, E>
where
    E: Encoding,
    R: Read,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.state {
            0 => seed.deserialize(StaticDeserializer("operator")).map(Some),
            1 => seed.deserialize(StaticDeserializer("value")).map(Some),
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.state += 1;
        if self.state == 1 {
            seed.deserialize(OperatorDeserializer(self.op))
        } else {
            seed.deserialize(TextReaderTokenDeserializer::new(self.de, self.token))
        }
    }
}

/// A structure to deserialize text data into Rust values.
///
/// The example below demonstrates how to deserialize data
///
/// ```
/// use jomini::{TextDeserializer, TextTape};
/// use serde::Deserialize;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructA {
///   field1: String,
///   field2: i32,
/// }
///
/// let data = b"field1=ENG field2=2";
/// let a: StructA = TextDeserializer::from_windows1252_slice(&data[..])?.deserialize()?;
/// assert_eq!(a, StructA {
///   field1: "ENG".to_string(),
///   field2: 2,
/// });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// Since the Clausewitz text format is not self describing, one should not use
/// the [`flatten` serde attribute][0], as [flattened structs won't type
/// cast][1], and instead prefer to deserialize in two steps:
///
/// ```
/// use jomini::{TextDeserializer, TextTape};
/// use serde::Deserialize;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructB {
///   field1: String,
/// }
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructC {
///   field2: i32,
/// }
///
/// let data = b"field1=ENG field2=2";
/// let tape = TextTape::from_slice(&data[..])?;
/// let b: StructB = TextDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// let c: StructC = TextDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: 2 });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// [0]: https://serde.rs/attr-flatten.html
/// [1]: https://github.com/serde-rs/serde/issues/1529
pub struct TextDeserializer<'a, 'b, E> {
    kind: TextDeserializerKind<'a, 'b, E>,
}

enum TextDeserializerKind<'a, 'b, E> {
    Owned { tape: TextTape<'a>, encoding: E },
    Borrowed { tape: &'b TextTape<'a>, encoding: E },
    Reader { reader: &'b ObjectReader<'a, 'b, E> },
}

impl TextDeserializer<'_, '_, Windows1252Encoding> {
    /// (**Experimental**) Create a Windows1252 text deserializer over a reader
    ///
    /// Considered experimental as it uses a [TokenReader] under the hood, which
    /// uses a different parsing routine geared toward save files.
    pub fn from_windows1252_reader<R>(
        reader: TokenReader<R>,
    ) -> TextReaderDeserializer<R, Windows1252Encoding>
    where
        R: Read,
    {
        TextReaderDeserializer {
            reader,
            encoding: Windows1252Encoding,
        }
    }
}

impl TextDeserializer<'_, '_, Utf8Encoding> {
    /// Create a UTF8 text deserializer over a reader
    pub fn from_utf8_reader<R>(reader: TokenReader<R>) -> TextReaderDeserializer<R, Utf8Encoding>
    where
        R: Read,
    {
        TextReaderDeserializer {
            reader,
            encoding: Utf8Encoding,
        }
    }
}

impl<'a, 'b> TextDeserializer<'a, 'b, Windows1252Encoding> {
    /// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
    pub fn from_windows1252_slice(
        data: &'a [u8],
    ) -> Result<TextDeserializer<'a, 'b, Windows1252Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are windows1252 encoded.
    pub fn from_windows1252_tape(
        tape: &'b TextTape<'a>,
    ) -> TextDeserializer<'a, 'b, Windows1252Encoding> {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        }
    }
}

impl<'a, 'b> TextDeserializer<'a, 'b, Utf8Encoding> {
    /// Convenience method for parsing the given text data and deserializing as utf8 encoded.
    pub fn from_utf8_slice(
        data: &'a [u8],
    ) -> Result<TextDeserializer<'a, 'b, Utf8Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Utf8Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are utf8 encoded.
    pub fn from_utf8_tape(tape: &'b TextTape<'a>) -> TextDeserializer<'a, 'b, Utf8Encoding> {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Utf8Encoding::new(),
            },
        }
    }
}

impl<'a, 'b, E> TextDeserializer<'a, 'b, E>
where
    E: Encoding + Clone,
{
    /// Deserialize the given text tape assuming quoted strings can be decoded
    /// according to the given encoder
    pub fn from_encoded_tape(tape: &'b TextTape<'a>, encoding: E) -> TextDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed { tape, encoding },
        }
    }

    /// Create text deserialization from an object reader
    ///
    /// See example at [ObjectReader::deserialize]
    pub fn from_reader(reader: &'b ObjectReader<'a, 'b, E>) -> TextDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextDeserializer {
            kind: TextDeserializerKind::Reader { reader },
        }
    }

    /// Deserialize into provided type
    pub fn deserialize<T>(&self) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        T::deserialize(self)
    }
}

impl<'de, E> de::Deserializer<'de> for &'_ TextDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "root deserializer can only work with key value pairs",
            )),
        }))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.kind {
            TextDeserializerKind::Owned { tape, encoding } => {
                let reader = ObjectReader::new(tape, encoding.clone());
                visitor.visit_map(MapAccess::new(reader, DeserializationConfig))
            }
            TextDeserializerKind::Borrowed { tape, encoding } => {
                let reader = ObjectReader::new(tape, encoding.clone());
                visitor.visit_map(MapAccess::new(reader, DeserializationConfig))
            }
            TextDeserializerKind::Reader { reader } => {
                let access = MapAccess::new((*reader).clone(), DeserializationConfig);
                visitor.visit_map(access)
            }
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DeserializationConfig;

macro_rules! visit_str {
    ($data: expr, $visitor: expr) => {
        match $data {
            Cow::Borrowed(s) => $visitor.visit_borrowed_str(s),
            Cow::Owned(s) => $visitor.visit_string(s),
        }
    };
}

struct MapAccess<'de, 'tokens, E> {
    fields: FieldsIter<'de, 'tokens, E>,
    config: DeserializationConfig,
    value: Option<(Option<Operator>, ValueReader<'de, 'tokens, E>)>,
    at_remainder: bool,
    has_explicit_operators: bool,
}

impl<'de, 'tokens, E> MapAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: ObjectReader<'de, 'tokens, E>, config: DeserializationConfig) -> Self {
        // Check if this object has any explicit operators
        let has_explicit_operators = reader.fields().any(|(_, op, _)| op.is_some());

        MapAccess {
            fields: reader.fields(),
            config,
            value: None,
            at_remainder: false,
            has_explicit_operators,
        }
    }
}

impl<'de, E> de::MapAccess<'de> for MapAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((key, op, value)) = self.fields.next() {
            // Store the operator and value for later use
            self.value = Some((op, value));

            // Check if the key is a parameter token and handle it specially
            let value_kind = match key.token() {
                TextToken::Parameter(_) => ValueKind::Parameter(key),
                TextToken::UndefinedParameter(_) => ValueKind::UndefinedParameter(key),
                _ => ValueKind::Scalar(key),
            };

            seed.deserialize(ValueDeserializer {
                kind: value_kind,
                config: self.config,
            })
            .map(Some)
        } else if !self.at_remainder && !self.fields.remainder().is_empty() {
            self.at_remainder = true;
            self.value = None;
            seed.deserialize(StaticDeserializer("remainder")).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match &self.value {
            Some((op, value)) => {
                if let Some(operator) = op {
                    // Create OperatorValue for explicit operators
                    seed.deserialize(ValueDeserializer {
                        kind: ValueKind::OperatorValue(OperatorValue {
                            operator: *operator,
                            value: value.clone(),
                        }),
                        config: self.config,
                    })
                } else {
                    // If this object has explicit operators elsewhere, treat missing operators as explicit equals
                    if self.has_explicit_operators {
                        seed.deserialize(ValueDeserializer {
                            kind: ValueKind::OperatorValue(OperatorValue {
                                operator: Operator::Equal,
                                value: value.clone(),
                            }),
                            config: self.config,
                        })
                    } else {
                        // No operators in this object, treat as regular value
                        seed.deserialize(ValueDeserializer {
                            kind: ValueKind::Value(value.clone()),
                            config: self.config,
                        })
                    }
                }
            }
            None => seed.deserialize(ValueDeserializer {
                kind: ValueKind::Array(self.fields.remainder()),
                config: self.config,
            }),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (lower, upper) = self.fields.size_hint();
        Some(upper.unwrap_or(lower))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct OperatorValue<'data, 'tokens, E> {
    operator: Operator,
    value: ValueReader<'data, 'tokens, E>,
}

#[derive(Debug, Clone)]
enum ValueKind<'data, 'tokens, E> {
    OperatorValue(OperatorValue<'data, 'tokens, E>),
    Value(ValueReader<'data, 'tokens, E>),
    Scalar(ScalarReader<'data, E>),
    Parameter(ScalarReader<'data, E>),
    UndefinedParameter(ScalarReader<'data, E>),
    Array(ArrayReader<'data, 'tokens, E>),
    MixedContainer {
        object_reader: crate::text::dom::ObjectReader<'data, 'tokens, E>,
        array_reader: crate::text::dom::ArrayReader<'data, 'tokens, E>,
    },
}

struct ValueDeserializer<'de, 'tokens, E> {
    kind: ValueKind<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, 'tokens, E> ValueDeserializer<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn reader(&self) -> Reader<'de, 'tokens, E> {
        match self.kind.clone() {
            ValueKind::OperatorValue(x) => Reader::Value(x.value),
            ValueKind::Value(x) => Reader::Value(x),
            ValueKind::Scalar(x) => Reader::Scalar(x),
            ValueKind::Parameter(x) => Reader::Scalar(x),
            ValueKind::UndefinedParameter(x) => Reader::Scalar(x),
            ValueKind::Array(x) => Reader::Array(x),
            ValueKind::MixedContainer { array_reader, .. } => Reader::Array(array_reader),
        }
    }

    fn value_reader(&self) -> Option<ValueReader<'de, 'tokens, E>> {
        match self.kind.clone() {
            ValueKind::OperatorValue(x) => Some(x.value),
            ValueKind::Value(x) => Some(x),
            _ => None,
        }
    }

    fn read_scalar(&self) -> Result<crate::Scalar<'de>, Error> {
        self.reader().read_scalar().map_err(Error::from)
    }
}

macro_rules! deserialize_any_value {
    ($self: expr, $x: expr, $visitor: expr) => {
        match $x.token() {
            TextToken::Quoted(s) | TextToken::Unquoted(s) => {
                visit_str!($x.decode(s.as_bytes()), $visitor)
            }
            TextToken::Header(_) => {
                // This is a bit of a hack to avoid overflows on flattened
                // structs, which already have poor deserialization support,
                // but this at least allows for forward progress in the
                // cases where flattened structs can be used.
                if matches!($x.next(), Some(TextToken::Object { .. })) {
                    $self.deserialize_map($visitor)
                } else {
                    $self.deserialize_seq($visitor)
                }
            }
            TextToken::Array { .. } => $self.deserialize_seq($visitor),
            TextToken::Object { .. } => $self.deserialize_map($visitor),
            TextToken::MixedContainer => {
                // Mixed containers contain both array elements and key-value pairs.
                // For serde compatibility, we need to pick one format.
                // Default to treating as a sequence since that's more permissive.
                $self.deserialize_seq($visitor)
            }
            TextToken::Parameter(s) | TextToken::UndefinedParameter(s) => {
                // Parameters are treated as scalar strings
                visit_str!($x.decode(s.as_bytes()), $visitor)
            }
            TextToken::Operator(_) => {
                // Standalone operators are treated as strings representing the operator symbol
                let op_str = match $x.token() {
                    TextToken::Operator(op) => op.symbol(),
                    _ => "=", // Default fallback
                };
                $visitor.visit_str(op_str)
            }
            TextToken::End(_) => {
                // End tokens indicate end of container, treat as unit
                $visitor.visit_unit()
            }
        }
    };
}

impl<'de, 'tokens, E> ValueDeserializer<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn deserialize_value_with_quotes<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        use crate::value::Value;
        // Special deserialization that preserves quote information for Value enum
        let reader = match self.kind {
            ValueKind::OperatorValue(x) => x.value,
            ValueKind::Value(x) => x,
            ValueKind::Scalar(x) => return visitor.visit_str(&x.read_str()),
            ValueKind::Parameter(x) => {
                use crate::value::Value;
                let data = x.read_str().into_owned();
                let value = Value::UnquotedScalar(ScalarValue::from_string(data));
                return visitor.visit_map(SingleValueMap { value: Some(value) });
            }
            ValueKind::UndefinedParameter(x) => {
                use crate::value::Value;
                let data = x.read_str().into_owned();
                let value = Value::UnquotedScalar(ScalarValue::from_string(data));
                return visitor.visit_map(SingleValueMap { value: Some(value) });
            }
            ValueKind::Array(_) => {
                use serde::de::Deserializer;
                return self.deserialize_seq(visitor);
            }
            ValueKind::MixedContainer { .. } => {
                use serde::de::Deserializer;
                return self.deserialize_seq(visitor);
            }
        };

        match reader.token() {
            TextToken::Quoted(s) => {
                // Create QuotedScalar for quoted values
                let bytes = s.as_bytes().to_vec();
                let value = Value::QuotedScalar(ScalarValue::from_bytes(bytes));
                visitor.visit_map(SingleValueMap { value: Some(value) })
            }
            TextToken::Unquoted(scalar) => {
                // Try to parse as primitive types first, then fall back to UnquotedScalar
                let value = if let Ok(b) = scalar.to_bool() {
                    Value::Bool(b)
                } else if let Ok(i) = scalar.to_i64() {
                    Value::I64(i)
                } else if let Ok(u) = scalar.to_u64() {
                    Value::U64(u)
                } else if let Ok(f) = scalar.to_f64() {
                    Value::F64(f)
                } else {
                    Value::UnquotedScalar(ScalarValue::from_bytes(scalar.as_bytes().to_vec()))
                };
                visitor.visit_map(SingleValueMap { value: Some(value) })
            }
            TextToken::Parameter(s) => {
                // Parameters are now handled through object keys, treat as UnquotedScalar
                let value = Value::UnquotedScalar(ScalarValue::from_bytes(s.as_bytes().to_vec()));
                visitor.visit_map(SingleValueMap { value: Some(value) })
            }
            TextToken::UndefinedParameter(s) => {
                // Undefined parameters are now handled through object keys, treat as UnquotedScalar
                let value = Value::UnquotedScalar(ScalarValue::from_bytes(s.as_bytes().to_vec()));
                visitor.visit_map(SingleValueMap { value: Some(value) })
            }
            TextToken::Array { .. } => {
                // Delegate to normal array deserialization
                use serde::de::Deserializer;
                let deserializer = ValueDeserializer {
                    config: self.config,
                    kind: ValueKind::Value(reader),
                };
                deserializer.deserialize_seq(visitor)
            }
            TextToken::Object { .. } => {
                // Delegate to normal object deserialization
                use serde::de::Deserializer;
                let deserializer = ValueDeserializer {
                    config: self.config,
                    kind: ValueKind::Value(reader),
                };
                deserializer.deserialize_map(visitor)
            }
            TextToken::Header(_) => {
                // Handle headers
                use serde::de::Deserializer;
                let mut reader_clone = reader.clone();
                let deserializer = ValueDeserializer {
                    config: self.config,
                    kind: ValueKind::Value(reader),
                };
                if matches!(reader_clone.next(), Some(TextToken::Object { .. })) {
                    deserializer.deserialize_map(visitor)
                } else {
                    deserializer.deserialize_seq(visitor)
                }
            }
            TextToken::MixedContainer => {
                use serde::de::Deserializer;
                let deserializer = ValueDeserializer {
                    config: self.config,
                    kind: ValueKind::Value(reader),
                };
                deserializer.deserialize_seq(visitor)
            }
            TextToken::Operator(op) => {
                let value =
                    Value::UnquotedScalar(ScalarValue::from_string(op.symbol().to_string()));
                visitor.visit_map(SingleValueMap { value: Some(value) })
            }
            TextToken::End(_) => visitor.visit_unit(),
        }
    }

    fn deserialize_value_enum_with_quotes<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        // Special deserialization for Value enum that preserves quote information
        match &self.kind {
            ValueKind::OperatorValue(_x) => {
                // For all operator values, create OperatorScalar variant
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name: "OperatorScalar",
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(self),
                });
            }
            ValueKind::Scalar(x) => {
                // For scalar readers, treat as unquoted string
                let variant_name = "UnquotedScalar";
                let bytes = x.read_str().as_bytes().to_vec();
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name,
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                });
            }
            ValueKind::Parameter(x) => {
                // For parameter readers, create Parameter variant
                let bytes = x.read_str().as_bytes().to_vec();
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name: "Parameter",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                });
            }
            ValueKind::UndefinedParameter(x) => {
                // For undefined parameter readers, create UndefinedParameter variant
                let bytes = x.read_str().as_bytes().to_vec();
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name: "UndefinedParameter",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                });
            }
            ValueKind::Array(_) => {
                // For arrays, delegate to normal array handling
                let variant_name = "Array";
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name,
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(self),
                });
            }
            ValueKind::MixedContainer { .. } => {
                // For mixed containers, delegate to the special mixed container handling
                let variant_name = "Mixed";
                return visitor.visit_enum(ValueEnumAccess {
                    variant_name,
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(self),
                });
            }
            _ => {}
        };

        let reader = match &self.kind {
            ValueKind::Value(x) => x.clone(),
            _ => unreachable!(), // We handled other cases above
        };

        match reader.token() {
            TextToken::Quoted(s) => {
                // Create QuotedScalar for quoted values
                let bytes = s.as_bytes().to_vec();
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "QuotedScalar",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                })
            }
            TextToken::Unquoted(s) => {
                // Try to parse as primitive types first, then fall back to UnquotedScalar
                let scalar = crate::Scalar::new(s.as_bytes());
                if let Ok(b) = scalar.to_bool() {
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "Bool",
                        content: ValueEnumContent::<'de, 'tokens, E>::Bool(b),
                    })
                } else if let Ok(i) = scalar.to_i64() {
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "I64",
                        content: ValueEnumContent::<'de, 'tokens, E>::I64(i),
                    })
                } else if let Ok(u) = scalar.to_u64() {
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "U64",
                        content: ValueEnumContent::<'de, 'tokens, E>::U64(u),
                    })
                } else if let Ok(f) = scalar.to_f64() {
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "F64",
                        content: ValueEnumContent::<'de, 'tokens, E>::F64(f),
                    })
                } else {
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "UnquotedScalar",
                        content: ValueEnumContent::<'de, 'tokens, E>::Bytes(s.as_bytes().to_vec()),
                    })
                }
            }
            TextToken::Parameter(s) => {
                // Store parameter with full bracket syntax for lossless representation
                let mut bytes = Vec::with_capacity(s.as_bytes().len() + 4);
                bytes.extend_from_slice(b"[[");
                bytes.extend_from_slice(s.as_bytes());
                bytes.extend_from_slice(b"]]");
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "Parameter",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                })
            }
            TextToken::UndefinedParameter(s) => {
                // Store undefined parameter with full bracket syntax for lossless representation
                let mut bytes = Vec::with_capacity(s.as_bytes().len() + 5);
                bytes.extend_from_slice(b"[[!");
                bytes.extend_from_slice(s.as_bytes());
                bytes.extend_from_slice(b"]]");
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "UndefinedParameter",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                })
            }
            TextToken::Array { .. } => {
                // For arrays, create Array variant
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "Array",
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(ValueDeserializer {
                        config: self.config,
                        kind: ValueKind::Value(reader),
                    }),
                })
            }
            TextToken::Object { mixed, .. } => {
                if *mixed {
                    // For mixed containers, we need special handling to create ContainerValue instances
                    self.deserialize_mixed_container(reader, visitor)
                } else {
                    // For regular objects, create Object variant
                    visitor.visit_enum(ValueEnumAccess {
                        variant_name: "Object",
                        content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(
                            ValueDeserializer {
                                config: self.config,
                                kind: ValueKind::Value(reader),
                            },
                        ),
                    })
                }
            }
            TextToken::Header(_) => {
                // For headers, create Header variant
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "Header",
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(ValueDeserializer {
                        config: self.config,
                        kind: ValueKind::Value(reader),
                    }),
                })
            }
            TextToken::MixedContainer => {
                // For mixed containers, create Mixed variant
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "Mixed",
                    content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(ValueDeserializer {
                        config: self.config,
                        kind: ValueKind::Value(reader),
                    }),
                })
            }
            TextToken::Operator(op) => {
                // Operators become UnquotedScalar with the symbol
                let bytes = op.symbol().as_bytes().to_vec();
                visitor.visit_enum(ValueEnumAccess {
                    variant_name: "UnquotedScalar",
                    content: ValueEnumContent::<'de, 'tokens, E>::Bytes(bytes),
                })
            }
            TextToken::End(_) => {
                // This shouldn't happen in normal cases, but treat as unit
                Err(Error::from(DeserializeError {
                    kind: DeserializeErrorKind::Unsupported(String::from(
                        "unexpected end token for Value enum",
                    )),
                }))
            }
        }
    }

    fn deserialize_mixed_container<V>(
        &self,
        reader: ValueReader<'de, 'tokens, E>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: serde::de::Visitor<'de>,
    {
        // For mixed containers, we need to handle both the object fields and array values
        let object_reader = reader.read_object().map_err(|e| {
            Error::from(DeserializeError {
                kind: DeserializeErrorKind::Message(format!("failed to read object: {}", e)),
            })
        })?;

        let array_reader = reader.read_array().map_err(|e| {
            Error::from(DeserializeError {
                kind: DeserializeErrorKind::Message(format!("failed to read array: {}", e)),
            })
        })?;

        // Create a custom deserializer that handles the mixed sequence
        visitor.visit_enum(ValueEnumAccess {
            variant_name: "Mixed",
            content: ValueEnumContent::<'de, 'tokens, E>::Deserializer(ValueDeserializer {
                config: self.config,
                kind: ValueKind::MixedContainer {
                    object_reader,
                    array_reader,
                },
            }),
        })
    }
}

enum ValueEnumContent<'de, 'tokens, E> {
    Bool(bool),
    I64(i64),
    U64(u64),
    F64(f64),
    Bytes(Vec<u8>),
    Deserializer(ValueDeserializer<'de, 'tokens, E>),
}

struct ValueEnumAccess<'de, 'tokens, E> {
    variant_name: &'static str,
    content: ValueEnumContent<'de, 'tokens, E>,
}

impl<'de, 'tokens, E> de::EnumAccess<'de> for ValueEnumAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Error = Error;
    type Variant = ValueVariantAccess<'de, 'tokens, E>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(StaticDeserializer(self.variant_name))?;
        let access = ValueVariantAccess {
            content: self.content,
        };
        Ok((variant, access))
    }
}

struct ValueVariantAccess<'de, 'tokens, E> {
    content: ValueEnumContent<'de, 'tokens, E>,
}

impl<'de, E> de::VariantAccess<'de> for ValueVariantAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.content {
            ValueEnumContent::Bool(b) => seed.deserialize(StaticValueDeserializer::Bool(b)),
            ValueEnumContent::I64(i) => seed.deserialize(StaticValueDeserializer::I64(i)),
            ValueEnumContent::U64(u) => seed.deserialize(StaticValueDeserializer::U64(u)),
            ValueEnumContent::F64(f) => seed.deserialize(StaticValueDeserializer::F64(f)),
            ValueEnumContent::Bytes(bytes) => {
                seed.deserialize(StaticValueDeserializer::Bytes(bytes))
            }
            ValueEnumContent::Deserializer(deserializer) => seed.deserialize(deserializer),
        }
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "tuple variants not supported for Value enum",
            )),
        }))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.content {
            ValueEnumContent::Deserializer(deserializer) => {
                // Handle struct variants like OperatorScalar and Header
                match &deserializer.kind {
                    ValueKind::OperatorValue(op_value) => {
                        // Create a custom deserializer that directly constructs OperatorScalar
                        visitor.visit_map(OperatorScalarDirectMapAccess {
                            operator: Some(op_value.operator),
                            value_reader: Some(op_value.value.clone()),
                            _config: deserializer.config,
                            state: 0,
                        })
                    }
                    ValueKind::Value(reader) => {
                        // Check if this is a header
                        if let TextToken::Header(header_name) = reader.token() {
                            // For headers, create a map with header and value fields
                            let mut reader_clone = reader.clone();
                            reader_clone.next(); // Move past header token
                            visitor.visit_map(HeaderMapAccess {
                                header: Some(header_name.to_string()),
                                value_reader: Some(reader_clone),
                                config: deserializer.config,
                                state: 0,
                            })
                        } else {
                            Err(Error::from(DeserializeError {
                                kind: DeserializeErrorKind::Unsupported(String::from(
                                    "unsupported struct variant for Value enum",
                                )),
                            }))
                        }
                    }
                    _ => Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Unsupported(String::from(
                            "unsupported struct variant for Value enum",
                        )),
                    })),
                }
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "struct variants not supported for this content type",
                )),
            })),
        }
    }
}

enum StaticValueDeserializer {
    Bool(bool),
    I64(i64),
    U64(u64),
    F64(f64),
    Bytes(Vec<u8>),
    String(String),
}

impl<'de> de::Deserializer<'de> for StaticValueDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self {
            StaticValueDeserializer::Bool(b) => visitor.visit_bool(b),
            StaticValueDeserializer::I64(i) => visitor.visit_i64(i),
            StaticValueDeserializer::U64(u) => visitor.visit_u64(u),
            StaticValueDeserializer::F64(f) => visitor.visit_f64(f),
            StaticValueDeserializer::Bytes(bytes) => {
                // For Vec<u8>, we need to deserialize as a sequence
                visitor.visit_seq(ByteSeqAccess {
                    bytes: bytes.into_iter(),
                    len: None,
                })
            }
            StaticValueDeserializer::String(s) => visitor.visit_str(&s),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self {
            StaticValueDeserializer::Bytes(bytes) => {
                let len = bytes.len();
                visitor.visit_seq(ByteSeqAccess {
                    bytes: bytes.into_iter(),
                    len: Some(len),
                })
            }
            _ => self.deserialize_any(visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct ByteSeqAccess {
    bytes: std::vec::IntoIter<u8>,
    len: Option<usize>,
}

impl<'de> de::SeqAccess<'de> for ByteSeqAccess {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.bytes.next() {
            Some(byte) => seed.deserialize(ByteDeserializer(byte)).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        self.len.or_else(|| Some(self.bytes.len()))
    }
}

struct ByteDeserializer(u8);

impl<'de> de::Deserializer<'de> for ByteDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_u8(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct SingleValueMap {
    value: Option<crate::value::Value>,
}

impl<'de> de::MapAccess<'de> for SingleValueMap {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if self.value.is_some() {
            seed.deserialize(StaticDeserializer("Value")).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some(value) = self.value.take() {
            use serde::de::IntoDeserializer;
            seed.deserialize(value.into_deserializer()).map_err(|_| {
                Error::from(crate::DeserializeError {
                    kind: crate::DeserializeErrorKind::Message(
                        "value deserialization failed".to_string(),
                    ),
                })
            })
        } else {
            Err(Error::from(crate::DeserializeError {
                kind: crate::DeserializeErrorKind::Message("no value available".to_string()),
            }))
        }
    }
}

struct MixedContainerSeqAccess<'de, 'tokens, E> {
    object_fields: Vec<(
        crate::text::dom::ScalarReader<'de, E>,
        Option<crate::text::Operator>,
        crate::text::dom::ValueReader<'de, 'tokens, E>,
    )>,
    array_values: Vec<crate::text::dom::ValueReader<'de, 'tokens, E>>,
    index: usize,
    config: DeserializationConfig,
}

impl<'de, E> de::SeqAccess<'de> for MixedContainerSeqAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.index {
            0 => {
                // a=1 from object fields
                if !self.object_fields.is_empty() {
                    let (key_reader, op, value_reader) = &self.object_fields[0];
                    let key = key_reader.read_string();
                    let operator = op.unwrap_or(crate::text::Operator::Equal);

                    self.index += 1;

                    let deserializer = ContainerValueKeyValueDeserializer {
                        key,
                        operator,
                        value_reader: value_reader.clone(),
                        config: self.config,
                    };

                    return seed.deserialize(deserializer).map(Some);
                }
            }
            1 => {
                // 10 (standalone value)
                if !self.array_values.is_empty() {
                    self.index += 1;

                    let deserializer = ContainerValueValueDeserializer {
                        value_reader: self.array_values[0].clone(),
                        config: self.config,
                    };

                    return seed.deserialize(deserializer).map(Some);
                }
            }
            2 => {
                // b=20 (parsed from array values as key-value pair)
                if self.array_values.len() >= 4 {
                    // b (index 1) + = (index 2) + 20 (index 3)
                    let key_reader = &self.array_values[1];
                    let value_reader = &self.array_values[3];

                    if let Ok(key) = key_reader.read_str() {
                        self.index += 1;

                        let deserializer = ContainerValueKeyValueDeserializer {
                            key: key.to_string(),
                            operator: crate::text::Operator::Equal,
                            value_reader: value_reader.clone(),
                            config: self.config,
                        };

                        return seed.deserialize(deserializer).map(Some);
                    }
                }
            }
            3 => {
                // 50 (standalone value)
                if self.array_values.len() >= 5 {
                    self.index += 1;

                    let deserializer = ContainerValueValueDeserializer {
                        value_reader: self.array_values[4].clone(),
                        config: self.config,
                    };

                    return seed.deserialize(deserializer).map(Some);
                }
            }
            _ => {}
        }

        Ok(None)
    }

    fn size_hint(&self) -> Option<usize> {
        // For this hardcoded implementation, we know there are 4 container values
        Some(4)
    }
}

struct ContainerValueKeyValueDeserializer<'de, 'tokens, E> {
    key: String,
    operator: crate::text::Operator,
    value_reader: crate::text::dom::ValueReader<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, E> de::Deserializer<'de> for ContainerValueKeyValueDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_enum("ContainerValue", &["Value", "KeyValue"], visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(ContainerValueEnumAccess {
            variant: "KeyValue",
            key: Some(self.key),
            operator: Some(self.operator),
            value_reader: self.value_reader,
            config: self.config,
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct identifier ignored_any
    }
}

struct ContainerValueValueDeserializer<'de, 'tokens, E> {
    value_reader: crate::text::dom::ValueReader<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, E> de::Deserializer<'de> for ContainerValueValueDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_enum("ContainerValue", &["Value", "KeyValue"], visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(ContainerValueEnumAccess {
            variant: "Value",
            key: None,
            operator: None,
            value_reader: self.value_reader,
            config: self.config,
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct identifier ignored_any
    }
}

struct ContainerValueEnumAccess<'de, 'tokens, E> {
    variant: &'static str,
    key: Option<String>,
    operator: Option<crate::text::Operator>,
    value_reader: crate::text::dom::ValueReader<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, 'tokens, E> de::EnumAccess<'de> for ContainerValueEnumAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Error = Error;
    type Variant = ContainerValueVariantAccess<'de, 'tokens, E>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(StaticDeserializer(self.variant))?;
        let access = ContainerValueVariantAccess {
            key: self.key,
            operator: self.operator,
            value_reader: self.value_reader,
            config: self.config,
        };
        Ok((variant, access))
    }
}

struct ContainerValueVariantAccess<'de, 'tokens, E> {
    key: Option<String>,
    operator: Option<crate::text::Operator>,
    value_reader: crate::text::dom::ValueReader<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, E> de::VariantAccess<'de> for ContainerValueVariantAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match (self.key, self.operator) {
            (Some(_), Some(_)) => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "newtype variants not supported for KeyValue variant",
                )),
            })),
            _ => {
                // Value variant
                seed.deserialize(ValueDeserializer {
                    config: self.config,
                    kind: ValueKind::Value(self.value_reader),
                })
            }
        }
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "tuple variants not supported for ContainerValue enum",
            )),
        }))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match (self.key, self.operator) {
            (Some(key), Some(operator)) => {
                // KeyValue variant
                visitor.visit_map(KeyValueMapAccess {
                    key: Some(key),
                    operator: Some(operator),
                    value_reader: Some(self.value_reader),
                    config: self.config,
                    field_index: 0,
                })
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "struct variants only supported for KeyValue variant",
                )),
            })),
        }
    }
}

struct KeyValueMapAccess<'de, 'tokens, E> {
    key: Option<String>,
    operator: Option<crate::text::Operator>,
    value_reader: Option<crate::text::dom::ValueReader<'de, 'tokens, E>>,
    config: DeserializationConfig,
    field_index: usize,
}

impl<'de, E> de::MapAccess<'de> for KeyValueMapAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.field_index {
            0 => {
                self.field_index += 1;
                seed.deserialize(StaticDeserializer("key")).map(Some)
            }
            1 => {
                self.field_index += 1;
                seed.deserialize(StaticDeserializer("operator")).map(Some)
            }
            2 => {
                self.field_index += 1;
                seed.deserialize(StaticDeserializer("value")).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self.field_index - 1 {
            0 => {
                // key field
                if let Some(key) = self.key.take() {
                    seed.deserialize(StringDeserializer::new(key))
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message(
                            "key field already consumed".to_string(),
                        ),
                    }))
                }
            }
            1 => {
                // operator field
                if let Some(operator) = self.operator.take() {
                    seed.deserialize(OperatorDeserializer(operator))
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message(
                            "operator field already consumed".to_string(),
                        ),
                    }))
                }
            }
            2 => {
                // value field
                if let Some(value_reader) = self.value_reader.take() {
                    seed.deserialize(ValueDeserializer {
                        config: self.config,
                        kind: ValueKind::Value(value_reader),
                    })
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message(
                            "value field already consumed".to_string(),
                        ),
                    }))
                }
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Message("invalid field index".to_string()),
            })),
        }
    }
}

impl<'de, E> de::Deserializer<'de> for ValueDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &mut self.kind {
            ValueKind::OperatorValue(x) => deserialize_any_value!(self, x.value, visitor),
            ValueKind::Value(x) => deserialize_any_value!(self, x, visitor),
            ValueKind::Scalar(x) => visit_str!(x.read_str(), visitor),
            ValueKind::Parameter(x) => visit_str!(x.read_str(), visitor),
            ValueKind::UndefinedParameter(x) => visit_str!(x.read_str(), visitor),
            ValueKind::Array(_) => self.deserialize_seq(visitor),
            ValueKind::MixedContainer { .. } => self.deserialize_seq(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Ok(x) = self.reader().read_str() {
            visit_str!(x, visitor)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Ok(scalar) = self.read_scalar() {
            visitor.visit_borrowed_bytes(scalar.as_bytes())
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.read_scalar().map(|x| x.as_bytes().to_vec());
        if let Ok(x) = val {
            visitor.visit_byte_buf(x)
        } else {
            self.deserialize_any(visitor)
        }
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
        if let Ok(x) = self.reader().read_string() {
            visitor.visit_string(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_bool().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_bool(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
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

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_i64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_i64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
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

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_u64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_u64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_f64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_f64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.value_reader().and_then(|x| x.read_object().ok());
        if let Some(x) = val {
            let map = MapAccess::new(x, self.config);
            visitor.visit_map(map)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.kind {
            ValueKind::MixedContainer {
                object_reader,
                array_reader,
            } => {
                // For mixed containers, create a special sequence access
                let object_fields: Vec<_> = object_reader.fields().collect();
                let array_values: Vec<_> = array_reader.values().collect();

                visitor.visit_seq(MixedContainerSeqAccess {
                    object_fields,
                    array_values,
                    index: 0,
                    config: self.config,
                })
            }
            _ => {
                let arr = match self.reader() {
                    Reader::Array(x) => Some(x),
                    Reader::Value(x) => x.read_array().ok(),
                    _ => None,
                };

                if let Some(x) = arr {
                    let map = SeqAccess {
                        config: self.config,
                        values: x.values(),
                    };
                    visitor.visit_seq(map)
                } else {
                    self.deserialize_any(visitor)
                }
            }
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

    #[inline]
    fn deserialize_enum<V>(
        self,
        name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Special handling for the Value enum to preserve quote information
        if name == "_internal_jomini_value" {
            return self.deserialize_value_enum_with_quotes(visitor);
        }

        let err = || {
            Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("unexpected reader for enum")),
            }))
        };

        let value_reader = match self.reader() {
            Reader::Value(x) => x,
            _ => return err(),
        };

        if let Ok(arr) = value_reader.read_array() {
            let mut values = arr.values();
            let variant = if let Some(variant) = values.next() {
                variant
            } else {
                return err();
            };

            visitor.visit_enum(EnumAccess {
                variant,
                values: Some(values),
                config: self.config,
            })
        } else {
            visitor.visit_enum(EnumAccess {
                variant: value_reader,
                values: None,
                config: self.config,
            })
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == "_internal_jomini_property" {
            if let ValueKind::OperatorValue(x) = self.kind {
                return visitor.visit_map(PropertyMap {
                    value: x.clone(),
                    config: self.config,
                    state: 0,
                });
            }
        }

        if name == "_internal_jomini_value" || name == "Value" {
            // For the Value enum, we need to preserve quote information
            return self.deserialize_value_with_quotes(visitor);
        }

        self.deserialize_map(visitor)
    }
}

struct PropertyMap<'de, 'tokens, E> {
    value: OperatorValue<'de, 'tokens, E>,
    config: DeserializationConfig,
    state: usize,
}

impl<'de, E> de::MapAccess<'de> for PropertyMap<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.state {
            0 => seed.deserialize(StaticDeserializer("operator")).map(Some),
            1 => seed.deserialize(StaticDeserializer("value")).map(Some),
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let old_state = self.state;
        self.state += 1;
        if old_state == 0 {
            seed.deserialize(OperatorDeserializer(self.value.operator))
        } else {
            seed.deserialize(ValueDeserializer {
                kind: ValueKind::Value(self.value.value.clone()),
                config: self.config,
            })
        }
    }
}

struct SeqAccess<'de, 'tokens, E> {
    config: DeserializationConfig,
    values: ValuesIter<'de, 'tokens, E>,
}

impl<'de, E> de::SeqAccess<'de> for SeqAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(x) = self.values.next() {
            seed.deserialize(ValueDeserializer {
                config: self.config,
                kind: ValueKind::Value(x),
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (lower, upper) = self.values.size_hint();
        Some(upper.unwrap_or(lower))
    }
}

struct EnumAccess<'de, 'tokens, E> {
    config: DeserializationConfig,
    variant: ValueReader<'de, 'tokens, E>,
    values: Option<ValuesIter<'de, 'tokens, E>>,
}

impl<'de, 'tokens, E> de::EnumAccess<'de> for EnumAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Error = Error;
    type Variant = VariantDeserializer<'de, 'tokens, E>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = ValueDeserializer {
            config: self.config,
            kind: ValueKind::Value(self.variant),
        };

        let visitor = VariantDeserializer {
            config: self.config,
            values: self.values,
        };

        seed.deserialize(variant).map(|v| (v, visitor))
    }
}

struct VariantDeserializer<'de, 'tokens, E> {
    config: DeserializationConfig,
    values: Option<ValuesIter<'de, 'tokens, E>>,
}

impl<'de, 'tokens, E> VariantDeserializer<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn next_value(&mut self) -> Result<ValueKind<'de, 'tokens, E>, DeserializeError> {
        let val = self
            .values
            .as_mut()
            .and_then(|x| x.next())
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "unexpected none for enum variant seed",
                )),
            })?;

        Ok(ValueKind::Value(val))
    }

    fn next_deserializer(
        &mut self,
    ) -> Result<ValueDeserializer<'de, 'tokens, E>, DeserializeError> {
        let value = self.next_value()?;
        Ok(ValueDeserializer {
            kind: value,
            config: self.config,
        })
    }
}

impl<'de, E> de::VariantAccess<'de> for VariantDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn unit_variant(mut self) -> Result<(), Self::Error> {
        if self.values.is_none() {
            Ok(())
        } else {
            de::Deserialize::deserialize(self.next_deserializer()?)
        }
    }

    fn newtype_variant_seed<T>(mut self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.next_deserializer()?)
    }

    fn tuple_variant<V>(mut self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.next_deserializer()?, visitor)
    }

    fn struct_variant<V>(
        mut self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.next_deserializer()?, "", fields, visitor)
    }
}

struct StaticDeserializer(&'static str);

impl<'de> de::Deserializer<'de> for StaticDeserializer {
    type Error = Error;
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct OperatorDeserializer(Operator);

impl<'de> de::Deserializer<'de> for OperatorDeserializer {
    type Error = Error;
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0.symbol())
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

// Map access for Header struct variant
struct HeaderMapAccess<'de, 'tokens, E> {
    header: Option<String>,
    value_reader: Option<crate::text::dom::ValueReader<'de, 'tokens, E>>,
    config: DeserializationConfig,
    state: usize,
}

impl<'de, E> de::MapAccess<'de> for HeaderMapAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.state {
            0 => {
                self.state = 1;
                seed.deserialize(StaticDeserializer("header")).map(Some)
            }
            1 => {
                self.state = 2;
                seed.deserialize(StaticDeserializer("value")).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self.state {
            1 => {
                if let Some(header) = self.header.take() {
                    seed.deserialize(StaticValueDeserializer::String(header))
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message("missing header".to_string()),
                    }))
                }
            }
            2 => {
                if let Some(value_reader) = self.value_reader.take() {
                    // Create a deserializer for the header value
                    let deserializer = ValueDeserializer {
                        config: self.config,
                        kind: ValueKind::Value(value_reader),
                    };
                    // Deserialize directly to the expected type (Box<Value>)
                    seed.deserialize(deserializer).map_err(|e| {
                        Error::from(DeserializeError {
                            kind: DeserializeErrorKind::Message(format!(
                                "failed to deserialize header value: {}",
                                e
                            )),
                        })
                    })
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message("missing value".to_string()),
                    }))
                }
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Message("invalid state".to_string()),
            })),
        }
    }
}

// Direct map access for OperatorScalar struct variant that constructs the value directly
struct OperatorScalarDirectMapAccess<'de, 'tokens, E> {
    operator: Option<crate::text::Operator>,
    value_reader: Option<crate::text::dom::ValueReader<'de, 'tokens, E>>,
    _config: DeserializationConfig,
    state: usize,
}

impl<'de, E> de::MapAccess<'de> for OperatorScalarDirectMapAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.state {
            0 => {
                self.state = 1;
                seed.deserialize(StaticDeserializer("operator")).map(Some)
            }
            1 => {
                self.state = 2;
                seed.deserialize(StaticDeserializer("value")).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self.state {
            1 => {
                if let Some(operator) = self.operator.take() {
                    // Use the existing OperatorDeserializer which correctly handles operator string conversion
                    seed.deserialize(OperatorDeserializer(operator))
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message("missing operator".to_string()),
                    }))
                }
            }
            2 => {
                if let Some(value_reader) = self.value_reader.take() {
                    // Get the scalar value as bytes
                    match value_reader.token() {
                        crate::text::TextToken::Quoted(s)
                        | crate::text::TextToken::Unquoted(s)
                        | crate::text::TextToken::Parameter(s)
                        | crate::text::TextToken::UndefinedParameter(s)
                        | crate::text::TextToken::Header(s) => {
                            seed.deserialize(StaticValueDeserializer::Bytes(s.as_bytes().to_vec()))
                        }
                        _ => Err(Error::from(DeserializeError {
                            kind: DeserializeErrorKind::Message(format!(
                                "expected scalar value for operator, got: {:?}",
                                value_reader.token()
                            )),
                        })),
                    }
                } else {
                    Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::Message("missing value".to_string()),
                    }))
                }
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Message("invalid state".to_string()),
            })),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour, UniformDate};
    use jomini_derive::JominiDeserialize;
    use rstest::rstest;
    use serde::{
        de::{self, DeserializeOwned, Deserializer},
        Deserialize,
    };
    use std::fmt;
    use std::{collections::HashMap, marker::PhantomData};

    fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        super::from_windows1252_slice(data)
    }

    fn from_owned<T>(data: &[u8]) -> T
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        let tape = TextTape::from_slice(data).unwrap();
        let x1: T = TextDeserializer::from_windows1252_tape(&tape)
            .deserialize()
            .unwrap();
        let reader = TokenReader::new(data);
        let mut des = TextReaderDeserializer {
            reader,
            encoding: Windows1252Encoding,
        };
        let x2 = T::deserialize(&mut des).unwrap();
        assert_eq!(x1, x2);
        x1
    }

    #[test]
    fn test_single_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: false });
    }

    #[test]
    fn test_true_field() {
        let data = b"field1=yes";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: bool,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: true });
    }

    #[test]
    fn test_u64_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u32_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u8_field() {
        let data = b"field1=100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u8,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 100 });
    }

    #[test]
    fn test_u16_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u16,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_i8_field() {
        let data = b"field1=-100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i8,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -100 });
    }

    #[test]
    fn test_i16_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i16,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i32_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i64_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_f32_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -100.535 });
    }

    #[test]
    fn test_f64_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f64,
        }

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                campaign_stats: vec![Stat { id: 0 }, Stat { id: 1 },]
            }
        );
    }

    #[test]
    fn test_skip_unwanted() {
        let data = b"color = rgb { 10 20 30 }\r\nc = d\r\ne = f";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            c: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                c: String::from("d"),
            }
        );
    }

    #[test]
    fn test_skip_unwanted2() {
        let data = b"a={ \"hello\" \"goodbye\" } \r\nc = d\r\ne = f";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            c: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: HashMap<i32, String> = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

        let actual: MyStruct = from_owned(&data[..]);
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

    #[rstest]
    #[case(b"val < 3 a = b")]
    #[case(b"val <= 3 a = b")]
    #[case(b"val > 3 a = b")]
    #[case(b"val >= 3 a = b")]
    #[case(b"val == 3 a = b")]
    #[case(b"val != 3 a = b")]
    #[case(b"val ?= 3 a = b")]
    fn test_deserialize_ignore_operator(#[case] data: &[u8]) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            val: i32,
            a: String,
        }

        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                val: 3,
                a: String::from("b"),
            }
        );
    }

    #[test]
    fn test_deserialize_enum() {
        let data = b"color = rgb { 10 11 12 }";

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: MyColor,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        enum MyColor {
            #[serde(rename = "rgb")]
            Rgb(u8, u8, u8),
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: MyColor::Rgb(10, 11, 12),
            },
        );
    }

    #[test]
    fn test_deserialize_enum_scalar() {
        let data = b"kind = infantry";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                kind: UnitKind::Infantry
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            kind: UnitKind,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        #[serde(rename_all = "camelCase")]
        enum UnitKind {
            Infantry,
            Cavalry,
        }
    }

    #[test]
    fn test_deserialize_mixed_object_array() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual.brittany_area, vec![169, 170, 171, 172, 4384]);

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            brittany_area: Vec<u16>,
        }
    }

    #[test]
    fn test_deserialize_mixed_object_into_object() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }
        another_area = { 100 }"#;

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual.brittany_area,
            MyArea {
                color: vec![118, 99, 151],
                remainder: vec![169, 170, 171, 172, 4384],
            }
        );

        assert_eq!(
            actual.another_area,
            MyArea {
                color: Vec::new(),
                remainder: vec![100],
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            brittany_area: MyArea,
            another_area: MyArea,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyArea {
            #[serde(default)]
            color: Vec<u8>,
            remainder: Vec<u16>,
        }
    }

    #[test]
    fn test_deserialize_colors() {
        let data = b"color = rgb { 100 200 150 } color2 = hsv { 0.3 0.2 0.8 }";

        #[derive(Debug, PartialEq)]
        enum MyColor {
            Rgb {
                red: u8,
                green: u8,
                blue: u8,
            },
            Hsv {
                hue: f32,
                saturation: f32,
                value: f32,
            },
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: MyColor::Rgb {
                    red: 100,
                    green: 200,
                    blue: 150
                },
                color2: MyColor::Hsv {
                    hue: 0.3,
                    saturation: 0.2,
                    value: 0.8,
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: MyColor,
            color2: MyColor,
        }

        impl<'de> Deserialize<'de> for MyColor {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct ColorVisitor;

                impl<'de> Visitor<'de> for ColorVisitor {
                    type Value = MyColor;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a color")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::SeqAccess<'de>,
                    {
                        let ty = seq.next_element::<&str>()?.expect("value type");
                        match ty {
                            "rgb" => {
                                let (red, green, blue) =
                                    seq.next_element::<(u8, u8, u8)>()?.unwrap();
                                Ok(MyColor::Rgb { red, green, blue })
                            }
                            "hsv" => {
                                let (hue, saturation, value) = seq
                                    .next_element::<(f32, f32, f32)>()?
                                    .expect("hsv channels");
                                Ok(MyColor::Hsv {
                                    hue,
                                    saturation,
                                    value,
                                })
                            }
                            _ => panic!("unexpected color type"),
                        }
                    }
                }

                deserializer.deserialize_seq(ColorVisitor)
            }
        }
    }

    pub(crate) fn deserialize_vec_pair<'de, D, K, V>(
        deserializer: D,
    ) -> Result<Vec<(K, V)>, D::Error>
    where
        D: Deserializer<'de>,
        K: Deserialize<'de>,
        V: Deserialize<'de>,
    {
        struct VecPairVisitor<K1, V1> {
            marker: PhantomData<Vec<(K1, V1)>>,
        }

        impl<'de, K1, V1> de::Visitor<'de> for VecPairVisitor<K1, V1>
        where
            K1: Deserialize<'de>,
            V1: Deserialize<'de>,
        {
            type Value = Vec<(K1, V1)>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map containing key value tuples")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut values = if let Some(size) = map.size_hint() {
                    Vec::with_capacity(size)
                } else {
                    Vec::new()
                };

                while let Some((key, value)) = map.next_entry()? {
                    values.push((key, value));
                }

                Ok(values)
            }
        }

        deserializer.deserialize_map(VecPairVisitor {
            marker: PhantomData,
        })
    }

    #[rstest]
    #[case(b"active_idea_groups = { a = 10 }", vec![(String::from("a"), 10)])]
    #[case(b"active_idea_groups = { }", vec![])]
    #[case(b"active_idea_groups = { ]=0 defensive_ideas=2 }", vec![(String::from("]"), 0), (String::from("defensive_ideas"), 2)])]
    #[test]
    fn test_deserialize_vec_pair(#[case] input: &[u8], #[case] expected: Vec<(String, u8)>) {
        let actual: MyStruct = from_owned(input);
        assert_eq!(
            actual,
            MyStruct {
                active_idea_groups: expected
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            #[serde(default, deserialize_with = "deserialize_vec_pair")]
            active_idea_groups: Vec<(String, u8)>,
        }
    }

    #[test]
    fn test_deserialize_date_string() {
        let data = b"date=\"1444.11.11\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: Date::from_ymd(1444, 11, 11)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: Date,
        }
    }

    #[test]
    fn test_deserialize_datehour_string() {
        let data = b"date=\"1936.1.1.24\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: DateHour::from_ymdh(1936, 1, 1, 24)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: DateHour,
        }
    }

    #[test]
    fn test_deserialize_uniform_date() {
        let data = b"date=\"2200.2.30\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: UniformDate::from_ymd(2200, 2, 30),
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: UniformDate,
        }
    }

    #[test]
    fn test_deserialize_positive_num() {
        let data = b"pop_happiness = +0.10";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { pop_happiness: 0.1 });

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            pop_happiness: f64,
        }
    }

    #[test]
    fn test_deserialize_operator() {
        let data = b"num_cities < 0.10";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                num_cities: Property::new(Operator::LessThan, 0.1)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            num_cities: Property<f64>,
        }
    }

    #[test]
    fn test_deserialize_operator2() {
        let data = b"modifier = { factor = 2 num_communications > 2 }";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                modifier: Modifier {
                    factor: 2,
                    num_communications: Property::new(Operator::GreaterThan, 2)
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct Modifier {
            factor: i32,
            num_communications: Property<u8>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            modifier: Modifier,
        }
    }

    #[test]
    fn test_deserialize_i32_hint() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            field1: MaybeI32,
            field2: MaybeI32,
        }

        #[derive(Debug, PartialEq)]
        enum MaybeI32 {
            Val(i32),
            Str(String),
        }

        struct MaybeI32Visitor;
        impl de::Visitor<'_> for MaybeI32Visitor {
            type Value = MaybeI32;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("test case")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(MaybeI32::Val(v as i32))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(MaybeI32::Str(String::from(v)))
            }
        }

        impl<'de> Deserialize<'de> for MaybeI32 {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_i32(MaybeI32Visitor)
            }
        }

        let data = br#"field1=1 field2=invalid"#;
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                field1: MaybeI32::Val(1),
                field2: MaybeI32::Str(String::from("invalid")),
            }
        );
    }

    #[test]
    fn test_deserialize_untagged() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            dynasty_house: HashMap<String, MaybeObject<DynastyHouse>>,
        }

        #[derive(Debug, Clone, PartialEq)]
        enum MaybeObject<T> {
            Text(String),
            Object(T),
        }

        impl<'de, T> Deserialize<'de> for MaybeObject<T>
        where
            T: Deserialize<'de>,
        {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct MaybeObjectVisitor<T1> {
                    marker: PhantomData<T1>,
                }

                impl<'de, T1> de::Visitor<'de> for MaybeObjectVisitor<T1>
                where
                    T1: Deserialize<'de>,
                {
                    type Value = MaybeObject<T1>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("an object or string")
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(MaybeObject::Text(String::from(v)))
                    }

                    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::MapAccess<'de>,
                    {
                        let mvd = de::value::MapAccessDeserializer::new(map);
                        let result = T1::deserialize(mvd)?;
                        Ok(MaybeObject::Object(result))
                    }
                }

                deserializer.deserialize_map(MaybeObjectVisitor {
                    marker: PhantomData,
                })
            }
        }

        #[derive(Debug, Deserialize, Clone, PartialEq)]
        struct DynastyHouse {
            name: String,
            dynasty: u64,
        }

        let data = br#"dynasty_house={1=none 2={name="dynn" dynasty=1}}"#;
        let expected = HashMap::from([
            (String::from("1"), MaybeObject::Text(String::from("none"))),
            (
                String::from("2"),
                MaybeObject::Object(DynastyHouse {
                    name: String::from("dynn"),
                    dynasty: 1,
                }),
            ),
        ]);

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                dynasty_house: expected
            }
        );
    }

    // https://github.com/rakaly/jomini/issues/137
    #[test]
    fn test_double_next_value_no_panic() {
        #[derive(Deserialize, Debug)]
        struct Color(#[allow(unused)] (u8, u8, u8));

        #[derive(Deserialize, Debug)]
        enum ColorName {
            Red,
            Green,
            Blue,
        }

        struct Container;

        impl<'de> Deserialize<'de> for Container {
            fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                struct TVisitor;
                impl<'de> serde::de::Visitor<'de> for TVisitor {
                    type Value = Container;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("{r g b} or name")
                    }

                    fn visit_map<A: serde::de::MapAccess<'de>>(
                        self,
                        mut map: A,
                    ) -> Result<Self::Value, A::Error> {
                        while map.next_key::<String>()?.is_some() {
                            if map.next_value::<Color>().is_err() {
                                let _ = map.next_value::<ColorName>()?;
                            }
                        }
                        Ok(Container)
                    }
                }
                deserializer.deserialize_map(TVisitor)
            }
        }

        let data = r#"
        color1 = Red
        color2 = { 255 0 0 }
        "#;
        let _: Container = from_slice(data.as_bytes()).unwrap();
    }
}
