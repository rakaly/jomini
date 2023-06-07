use super::reader::ValuesIter;
use crate::{
    text::{ArrayReader, FieldsIter, ObjectReader, Operator, Reader, ScalarReader, ValueReader},
    DeserializeError, DeserializeErrorKind, Encoding, Error, TextTape, TextToken, Utf8Encoding,
    Windows1252Encoding,
};
use serde::de::{self, Deserialize, DeserializeSeed, Visitor};
use std::{
    borrow::Cow,
    fmt::{self, Debug},
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

/// Convenience method for parsing the given text data and deserializing as utf8 encoded.
pub fn from_utf8_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_utf8_slice(data)?.deserialize()
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
    tape: TextDeserializerKind<'a, 'b>,
    encoding: E,
}

enum TextDeserializerKind<'a, 'b> {
    Owned(TextTape<'a>),
    Borrowed(&'b TextTape<'a>),
}

impl<'a, 'b> TextDeserializer<'a, 'b, Windows1252Encoding> {
    /// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
    pub fn from_windows1252_slice(
        data: &'a [u8],
    ) -> Result<TextDeserializer<Windows1252Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            tape: TextDeserializerKind::Owned(tape),
            encoding: Windows1252Encoding::new(),
        })
    }

    /// Deserialize the given text tape assuming quoted strings are windows1252 encoded.
    pub fn from_windows1252_tape(
        tape: &'b TextTape<'a>,
    ) -> TextDeserializer<'a, 'b, Windows1252Encoding> {
        TextDeserializer {
            tape: TextDeserializerKind::Borrowed(tape),
            encoding: Windows1252Encoding::new(),
        }
    }
}

impl<'a, 'b> TextDeserializer<'a, 'b, Utf8Encoding> {
    /// Convenience method for parsing the given text data and deserializing as utf8 encoded.
    pub fn from_utf8_slice(data: &'a [u8]) -> Result<TextDeserializer<Utf8Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            tape: TextDeserializerKind::Owned(tape),
            encoding: Utf8Encoding::new(),
        })
    }

    /// Deserialize the given text tape assuming quoted strings are utf8 encoded.
    pub fn from_utf8_tape(tape: &'b TextTape<'a>) -> TextDeserializer<'a, 'b, Utf8Encoding> {
        TextDeserializer {
            tape: TextDeserializerKind::Borrowed(tape),
            encoding: Utf8Encoding::new(),
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
            tape: TextDeserializerKind::Borrowed(tape),
            encoding,
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

impl<'de, 'a, 'tokens, E> de::Deserializer<'de> for &'a TextDeserializer<'de, 'tokens, E>
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
        match &self.tape {
            TextDeserializerKind::Owned(x) | &TextDeserializerKind::Borrowed(x) => {
                let reader = ObjectReader::new(x, self.encoding.clone());
                visitor.visit_map(MapAccess::new(reader, DeserializationConfig))
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
    value: Option<OperatorValue<'de, 'tokens, E>>,
    at_remainder: bool,
}

impl<'de, 'tokens, E> MapAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: ObjectReader<'de, 'tokens, E>, config: DeserializationConfig) -> Self {
        MapAccess {
            fields: reader.fields(),
            config,
            value: None,
            at_remainder: false,
        }
    }
}

impl<'de, 'tokens, E> de::MapAccess<'de> for MapAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((key, op, value)) = self.fields.next() {
            self.value = Some(OperatorValue {
                operator: op.unwrap_or(Operator::Equal),
                value,
            });

            seed.deserialize(ValueDeserializer {
                kind: ValueKind::Scalar(key),
                config: self.config,
            })
            .map(Some)
        } else if !self.at_remainder && !self.fields.remainder().is_empty() {
            self.at_remainder = true;
            seed.deserialize(StaticDeserializer("remainder")).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        if !self.at_remainder {
            let value = self.value.take().unwrap();
            seed.deserialize(ValueDeserializer {
                kind: ValueKind::OperatorValue(value),
                config: self.config,
            })
        } else {
            let remainder = self.fields.remainder();
            seed.deserialize(ValueDeserializer {
                kind: ValueKind::Array(remainder),
                config: self.config,
            })
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
    Array(ArrayReader<'data, 'tokens, E>),
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
            ValueKind::Array(x) => Reader::Array(x),
        }
    }

    fn value_reader(&self) -> Option<ValueReader<'de, 'tokens, E>> {
        match self.kind.clone() {
            ValueKind::OperatorValue(x) => Some(x.value),
            ValueKind::Value(x) => Some(x),
            _ => None,
        }
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
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "unsupported value reader token",
                )),
            })),
        }
    };
}

impl<'de, 'tokens, E> de::Deserializer<'de> for ValueDeserializer<'de, 'tokens, E>
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
            ValueKind::Array(_) => self.deserialize_seq(visitor),
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
        visit_str!(self.reader().read_str()?, visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_bytes(self.reader().read_scalar()?.as_bytes())
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_byte_buf(self.reader().read_scalar()?.as_bytes().to_vec())
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
        visitor.visit_string(self.reader().read_string()?)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.reader().read_scalar()?.to_bool()?)
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
        visitor.visit_i64(self.reader().read_scalar()?.to_i64()?)
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
        visitor.visit_u64(self.reader().read_scalar()?.to_u64()?)
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
        visitor.visit_f64(self.reader().read_scalar()?.to_f64()?)
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
        if let Some(x) = self.value_reader() {
            let map = MapAccess::new(x.read_object()?, self.config);
            visitor.visit_map(map)
        } else {
            Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "can only deserialize an object as a map",
                )),
            }))
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.reader() {
            Reader::Value(x) => {
                let map = SeqAccess {
                    config: self.config,
                    values: x.read_array()?.values(),
                };
                visitor.visit_seq(map)
            }
            Reader::Array(x) => {
                let map = SeqAccess {
                    config: self.config,
                    values: x.values(),
                };
                visitor.visit_seq(map)
            }
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "unexpected reader for sequence",
                )),
            })),
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
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
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

        self.deserialize_map(visitor)
    }
}

struct PropertyMap<'de, 'tokens, E> {
    value: OperatorValue<'de, 'tokens, E>,
    config: DeserializationConfig,
    state: usize,
}

impl<'de, 'tokens, E> de::MapAccess<'de> for PropertyMap<'de, 'tokens, E>
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

impl<'de, 'tokens, E> de::SeqAccess<'de> for SeqAccess<'de, 'tokens, E>
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

impl<'de, 'tokens, E> de::VariantAccess<'de> for VariantDeserializer<'de, 'tokens, E>
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

#[cfg(test)]
mod tests {
    use crate::common::{Date, DateHour, UniformDate};

    use super::*;
    use jomini_derive::JominiDeserialize;
    use serde::{
        de::{self, Deserializer},
        Deserialize,
    };
    use std::fmt;
    use std::{collections::HashMap, marker::PhantomData};

    fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        Ok(super::from_windows1252_slice(data)?)
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
    fn test_deserialize_ignore_operator() {
        let data = b"val > 3 a = b";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            val: i32,
            a: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

    #[test]
    fn test_deserialize_vec_pair() {
        let data = b"active_idea_groups = { a = 10 }";

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                active_idea_groups: vec![(String::from("a"), 10)]
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            #[serde(default, deserialize_with = "deserialize_vec_pair")]
            active_idea_groups: Vec<(String, u8)>,
        }
    }

    #[test]
    fn test_deserialize_vec_pair_empty() {
        let data = b"active_idea_groups = {}";

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                active_idea_groups: Vec::new()
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { pop_happiness: 0.1 });

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            pop_happiness: f64,
        }
    }

    #[test]
    fn test_deserialize_operator() {
        let data = b"num_cities < 0.10";

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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
}
