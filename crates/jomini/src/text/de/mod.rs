//! Serde deserialization of text data.
//!
//! [`TextDeserializer`] deserializes text data from a slice or from a reader
//! as the data is read. From a slice, strings can borrow from the data, and
//! from a reader, only a fixed size window of the data is held in memory.
//! The functions [`from_windows1252_slice`], [`from_utf8_slice`],
//! [`from_windows1252_reader`], and [`from_utf8_reader`] are shorthands for
//! it with an encoding.
//!
//! [`TextTapeDeserializer`] deserializes a [`TextTape`] that was already
//! parsed, such as one that is also read with an [`ObjectReader`].
//!
//! ```
//! use jomini::{Windows1252Encoding, text::de::TextDeserializer};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Save {
//!     player: String,
//!     prestige: f64,
//! }
//!
//! let data = b"player=ENG prestige=12.5";
//! let expected = Save { player: String::from("ENG"), prestige: 12.5 };
//!
//! let save: Save = jomini::text::de::from_windows1252_slice(data)?;
//! assert_eq!(save, expected);
//!
//! let save: Save = jomini::text::de::from_windows1252_reader(&data[..])?;
//! assert_eq!(save, expected);
//!
//! let encoding = Windows1252Encoding::new();
//! let save: Save = TextDeserializer::from_reader_with_buf(&data[..], encoding, vec![0; 64])
//!     .deserialize()?;
//! assert_eq!(save, expected);
//! # Ok::<(), jomini::Error>(())
//! ```

use super::{
    dom::ValuesIter,
    wrappers::{HEADERED, KEY_HEADER, KEY_QUOTED, OptionDe, QUOTED, WrapperMap},
};
use crate::{
    DeserializeError, DeserializeErrorKind, Encoding, Error, TextTape, TextToken, Utf8Encoding,
    Windows1252Encoding,
    text::{ArrayReader, FieldsIter, ObjectReader, Operator, Reader, ScalarReader, ValueReader},
};
use serde::de::{self, Deserialize, DeserializeOwned, DeserializeSeed, IntoDeserializer, Visitor};
use std::{borrow::Cow, fmt::Debug, io::Read};

mod cursor;

pub use self::cursor::{ReaderSource, SliceSource, TextDeserializer};

/// Represents the field value that contains an operator
///
/// A `Property` is a specially marked type that signals to the deserializer to
/// preserve operators.
///
/// Each text deserializer of this crate gives the operator. A field with no
/// operator, such as `key { ... }`, has [`Operator::Equal`]. A value that is
/// not the value of a field, such as an element of a sequence, has no
/// operator, so a `Property` of it is an error.
///
/// ```
/// # #[cfg(feature = "derive")] {
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use jomini::text::{Operator, Property};
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
/// # Ok(())
/// # }
/// # }
/// ```
#[derive(serde::Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[serde(rename = "_internal_jomini_property")]
pub struct Property<T> {
    operator: Operator,
    value: T,
}

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

    /// Return a mutable reference to the value
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Convert the property into the value
    pub fn into_value(self) -> T {
        self.value
    }
}

/// Deserialize Windows-1252 text data from a slice.
///
/// Strings can borrow from `data`, as in `&'a str` fields. This is
/// [`TextDeserializer::from_slice`] with a [`Windows1252Encoding`].
///
/// ```
/// use serde::Deserialize;
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Save<'a> {
///     player: &'a str,
///     prestige: f64,
/// }
///
/// let save: Save = jomini::text::de::from_windows1252_slice(b"player=ENG prestige=12.5")?;
/// assert_eq!(save, Save { player: "ENG", prestige: 12.5 });
/// # Ok::<(), jomini::Error>(())
/// ```
pub fn from_windows1252_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_slice(data, Windows1252Encoding::new()).deserialize()
}

/// Deserialize Windows-1252 text data from a reader.
///
/// The data is deserialized as it is read through a fixed size buffer, so
/// it isn't held in memory in full. This is
/// [`TextDeserializer::from_reader`] with a [`Windows1252Encoding`].
pub fn from_windows1252_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_reader(reader, Windows1252Encoding::new()).deserialize()
}

/// Deserialize UTF-8 text data from a reader.
///
/// The data is deserialized as it is read through a fixed size buffer, so
/// it isn't held in memory in full. This is
/// [`TextDeserializer::from_reader`] with a [`Utf8Encoding`].
pub fn from_utf8_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_reader(reader, Utf8Encoding::new()).deserialize()
}

/// Deserialize UTF-8 text data from a slice.
///
/// Strings can borrow from `data`, as in `&'a str` fields. This is
/// [`TextDeserializer::from_slice`] with a [`Utf8Encoding`].
pub fn from_utf8_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_slice(data, Utf8Encoding::new()).deserialize()
}

/// A serde deserializer over a parsed [`TextTape`].
///
/// Use it to deserialize a tape that is already on hand, such as one that is
/// also read with an [`ObjectReader`] or deserialized into several types. To
/// deserialize a slice or a reader, [`TextDeserializer`] and the
/// [`from_windows1252_slice`] family of functions read the data without
/// building a tape first.
///
/// A tape normalizes the data before it is deserialized, so the two read
/// some data differently. A tape can't be deserialized at the root with
/// `deserialize_any`. [`TextDeserializer`] reads what the lossless parser,
/// `jomini::text::syntax::parse`, reads, or gives an error that names it,
/// for these data, which a tape reads:
///
/// - a value without a key in a map that is not a struct, as in
///   `x = { b c }` read into a map, where a struct reads it into its
///   `remainder` field, as a tape does
/// - an object read as a sequence, as with the scalars that trail its
///   fields, as in `area = { color = { 1 2 3 } 169 170 }`, or as
///   `{ a = 1 }` read as `[a, =, 1]`, and an operator as a value
/// - an object as a key, as in `obj = { { a = b }={ 1 2 3 } }`, into a
///   sequence of keys and values
/// - a header before a map, as in `o = rgb { s = 1 }`
/// - an enum written as a block, as in `a = { rgb = { 1 2 3 } }` or
///   `a = { rgb { 1 2 3 } }`, whose first item is the variant, and an enum
///   variant with data whose header has no block or an empty one
/// - the elements after those of a tuple, as in `rgb { 1 2 3 4 }` read
///   into three numbers, which a tape drops
/// - a parameter block or code block of script, as in
///   `[[scope] set_variable = x ]`, which a tape reads as a field named
///   after the parameter, a scalar that starts with a bracket, as in
///   `picture = [GetName]`, which a tape splits at the brackets, and a `!`
///   or `?` that is not part of an operator
/// - a value read again after its read failed, as a retry in another type,
///   which a tape reads again
///
/// And [`TextDeserializer`] reads these differently from a tape:
///
/// - an empty block in an array, as in `intel = { {} { civilian = -10 } }`,
///   is an element, also with `deserialize_any`, so a positional list keeps
///   its indices, where a tape drops the empty blocks at the start
/// - a value without a key that starts an object, as the length of a
///   sparse array in a save, `income_taxes={ 80 0=278.75 1=433.88 }`,
///   starts its remainder, where a tape gives an error, and so does a block
///   at the start of the data that is not empty
/// - the end of the data closes every block that is open, where a tape
///   closes only one object
/// - an error carries the byte offset of the token that caused it
///
/// Since the Clausewitz text format is not self describing, one should not use
/// the [`flatten` serde attribute][0], as [flattened structs won't type
/// cast][1], and instead prefer to deserialize in two steps:
///
/// ```
/// use jomini::{TextTape, text::de::TextTapeDeserializer};
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
/// let b: StructB = TextTapeDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// let c: StructC = TextTapeDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: 2 });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// [0]: https://serde.rs/attr-flatten.html
/// [1]: https://github.com/serde-rs/serde/issues/1529
pub struct TextTapeDeserializer<'a, 'b, E> {
    kind: TextDeserializerKind<'a, 'b, E>,
}

enum TextDeserializerKind<'a, 'b, E> {
    Owned { tape: TextTape<'a>, encoding: E },
    Borrowed { tape: &'b TextTape<'a>, encoding: E },
    Reader { reader: &'b ObjectReader<'a, 'b, E> },
}

impl<'a, 'b> TextTapeDeserializer<'a, 'b, Windows1252Encoding> {
    /// Parse `data` into a [`TextTape`] to deserialize, decoding quoted strings as Windows-1252.
    pub fn from_windows1252_slice(
        data: &'a [u8],
    ) -> Result<TextTapeDeserializer<'a, 'b, Windows1252Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextTapeDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are windows1252 encoded.
    pub fn from_windows1252_tape(
        tape: &'b TextTape<'a>,
    ) -> TextTapeDeserializer<'a, 'b, Windows1252Encoding> {
        TextTapeDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        }
    }
}

impl<'a, 'b> TextTapeDeserializer<'a, 'b, Utf8Encoding> {
    /// Parse `data` into a [`TextTape`] to deserialize, decoding quoted strings as UTF-8.
    pub fn from_utf8_slice(
        data: &'a [u8],
    ) -> Result<TextTapeDeserializer<'a, 'b, Utf8Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextTapeDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Utf8Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are utf8 encoded.
    pub fn from_utf8_tape(tape: &'b TextTape<'a>) -> TextTapeDeserializer<'a, 'b, Utf8Encoding> {
        TextTapeDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Utf8Encoding::new(),
            },
        }
    }
}

impl<'a, 'b, E> TextTapeDeserializer<'a, 'b, E>
where
    E: Encoding + Clone,
{
    /// Deserialize the given text tape assuming quoted strings can be decoded
    /// according to the given encoder
    pub fn from_encoded_tape(tape: &'b TextTape<'a>, encoding: E) -> TextTapeDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextTapeDeserializer {
            kind: TextDeserializerKind::Borrowed { tape, encoding },
        }
    }

    /// Create text deserialization from an object reader
    ///
    /// See example at [ObjectReader::deserialize]
    pub fn from_reader(reader: &'b ObjectReader<'a, 'b, E>) -> TextTapeDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextTapeDeserializer {
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

impl<'de, E> de::Deserializer<'de> for &'_ TextTapeDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(
                "root deserializer can only work with key value pairs",
            ),
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
                let access = MapAccess {
                    fields: reader.fields(),
                    config: DeserializationConfig,
                    value: None,
                    at_remainder: false,
                };
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

    /// An owned value of the root is the block of the whole tape.
    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "syntax")]
        if name == crate::text::syntax::de::VALUE {
            use crate::text::{
                dom::raw,
                syntax::{
                    OwnedValue,
                    value::{tape, visit_owned},
                },
            };
            let root = || {
                OwnedValue::Block(match &self.kind {
                    TextDeserializerKind::Owned { tape, encoding } => {
                        tape::block(tape.tokens(), 0, tape.tokens().len(), true, encoding)
                    }
                    TextDeserializerKind::Borrowed { tape, encoding } => {
                        tape::block(tape.tokens(), 0, tape.tokens().len(), true, encoding)
                    }
                    TextDeserializerKind::Reader { reader } => {
                        let (tokens, start, end, encoding) = raw::object(reader);
                        tape::block(tokens, start, end, true, encoding)
                    }
                })
            };
            return visit_owned(root, visitor);
        }
        let _ = name;
        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
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
            Some(x) => seed.deserialize(ValueDeserializer {
                kind: ValueKind::OperatorValue(x.clone()),
                config: self.config,
            }),
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

    fn read_scalar(&self) -> Result<crate::Scalar<'de>, Error> {
        self.reader().read_scalar().map_err(Error::from)
    }

    /// Return `true` when the value is a quoted scalar.
    fn is_quoted(&self) -> bool {
        let token = match &self.kind {
            ValueKind::OperatorValue(x) => x.value.token(),
            ValueKind::Value(x) => x.token(),
            ValueKind::Scalar(x) => x.token(),
            ValueKind::Array(_) => return false,
        };
        matches!(token, TextToken::Quoted(_))
    }

    /// Copy the value into an owned value.
    #[cfg(feature = "syntax")]
    fn owned(&self) -> crate::text::syntax::OwnedValue {
        use crate::text::{
            dom::raw,
            syntax::{OwnedScalar, OwnedValue, value::tape},
        };
        match &self.kind {
            ValueKind::OperatorValue(OperatorValue { value: x, .. }) | ValueKind::Value(x) => {
                let (tokens, index, encoding) = raw::value(x);
                tape::value(tokens, index, encoding)
            }
            ValueKind::Scalar(x) => {
                let text = x.read_string();
                OwnedValue::Scalar(match x.token() {
                    TextToken::Quoted(_) => OwnedScalar::Quoted(text),
                    _ => tape::unquoted(text),
                })
            }
            ValueKind::Array(x) => {
                let (tokens, start, end, encoding) = raw::array(x);
                OwnedValue::Block(tape::block(tokens, start, end, false, encoding))
            }
        }
    }

    /// The header and the block of a headered block. The block keeps the
    /// operator of the field, as in `a > rgb { 1 2 }`.
    fn headered(&self) -> Option<(Self, Self)> {
        let value = self.value_reader()?;
        if !matches!(value.token(), TextToken::Header(_)) {
            return None;
        }
        let mut parts = value.read_array().ok()?.values();
        let header = parts.next()?;
        let block = parts.next()?;
        let block = match &self.kind {
            ValueKind::OperatorValue(x) => ValueKind::OperatorValue(OperatorValue {
                operator: x.operator,
                value: block,
            }),
            _ => ValueKind::Value(block),
        };
        let part = |kind| ValueDeserializer {
            kind,
            config: self.config,
        };
        Some((part(ValueKind::Value(header)), part(block)))
    }
}

macro_rules! deserialize_any_value {
    ($self: expr, $x: expr, $visitor: expr) => {
        match $x.token() {
            TextToken::Quoted(_) | TextToken::Unquoted(_) => {
                visit_str!($x.read_str().map_err(Error::from)?, $visitor)
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
                kind: DeserializeErrorKind::Unsupported("unsupported value reader token"),
            })),
        }
    };
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
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => {
                let quoted = self.is_quoted().into_deserializer();
                visitor.visit_map(WrapperMap::new(KEY_QUOTED, quoted, self))
            }
            HEADERED => match self.headered() {
                Some((header, block)) => {
                    visitor.visit_map(WrapperMap::new(KEY_HEADER, OptionDe(Some(header)), block))
                }
                None => {
                    let header = OptionDe::<Self>(None);
                    visitor.visit_map(WrapperMap::new(KEY_HEADER, header, self))
                }
            },
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => {
                crate::text::syntax::value::visit_owned(|| self.owned(), visitor)
            }
            _ => visitor.visit_newtype_struct(self),
        }
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
                kind: DeserializeErrorKind::Unsupported("unexpected reader for enum"),
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
        if name == "_internal_jomini_property"
            && let ValueKind::OperatorValue(x) = self.kind
        {
            return visitor.visit_map(PropertyMap {
                value: x.clone(),
                config: self.config,
                state: 0,
            });
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
            .ok_or(DeserializeError {
                kind: DeserializeErrorKind::Unsupported("unexpected none for enum variant seed"),
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

#[cfg(all(test, feature = "derive"))]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour, UniformDate};
    use crate::text::{Headered, MaybeQuoted};
    use jomini_derive::JominiDeserialize;
    use rstest::rstest;
    use serde::{
        Deserialize,
        de::{self, DeserializeOwned, Deserializer},
    };
    use std::fmt;
    use std::{collections::HashMap, marker::PhantomData};

    fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a> + PartialEq + Debug,
    {
        let result = super::from_windows1252_slice(data);
        #[cfg(feature = "syntax")]
        if let Ok(expected) = &result {
            check_syntax_tree(data, expected);
        }
        result
    }

    fn from_tape<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        TextTapeDeserializer::from_windows1252_slice(data)?.deserialize()
    }

    /// Deserialize `data` from a tape, and assert that the other engines give
    /// the same value, as [`from_text`] does.
    fn from_owned<T>(data: &[u8]) -> T
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        let tape = TextTape::from_slice(data).unwrap();
        let expected: T = TextTapeDeserializer::from_windows1252_tape(&tape)
            .deserialize()
            .unwrap();
        assert_eq!(from_text::<T>(data), expected, "tape");
        #[cfg(feature = "syntax")]
        check_tape_value(data, &expected);
        expected
    }

    /// Deserialize `data` with `TextDeserializer` from a slice, and assert
    /// that it gives the same value from a reader and from readers with
    /// small buffers that hold the longest token, and that the syntax tree
    /// and the owned values of the syntax tree and of `TextDeserializer`
    /// give it too.
    fn from_text<T>(data: &[u8]) -> T
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        let expected: T = super::from_windows1252_slice(data).unwrap();
        let actual: T = super::from_windows1252_reader(data).unwrap();
        assert_eq!(actual, expected, "reader");
        for size in [8, 16, 64] {
            let buf = vec![0; size];
            let encoding = Windows1252Encoding::new();
            match TextDeserializer::from_reader_with_buf(data, encoding, buf).deserialize::<T>() {
                Ok(actual) => assert_eq!(actual, expected, "reader with {size} byte buffer"),
                Err(err) => assert!(
                    matches!(err.kind(), crate::ErrorKind::BufferTooSmall),
                    "reader with {size} byte buffer: {err}"
                ),
            }
        }
        #[cfg(feature = "syntax")]
        {
            check_syntax_tree(data, &expected);
            check_syntax_value(data, &expected);
            check_owned_value(data, &expected);
        }
        expected
    }

    /// The deserializer of the syntax tree must give the same result as the
    /// deserializer of the tape.
    #[cfg(feature = "syntax")]
    fn check_syntax_tree<'a, T>(data: &'a [u8], expected: &T)
    where
        T: Deserialize<'a> + PartialEq + Debug,
    {
        let parse = crate::text::syntax::parse(data);
        let actual: T = parse
            .document(Windows1252Encoding::new())
            .deserialize()
            .unwrap();
        assert_eq!(&actual, expected, "the syntax tree deserializer differs");
    }

    /// A value from the syntax tree must deserialize into the same result.
    #[cfg(feature = "syntax")]
    fn check_syntax_value<T>(data: &[u8], expected: &T)
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        use crate::text::syntax::OwnedBlock;
        let parse = crate::text::syntax::parse(data);
        let value = OwnedBlock::from_tree(parse.tree(), Windows1252Encoding::new());
        let borrowed = T::deserialize(&value).unwrap();
        assert_eq!(
            &borrowed, expected,
            "the borrowed value deserializer differs"
        );
        let owned = T::deserialize(value).unwrap();
        assert_eq!(&owned, expected, "the owned value deserializer differs");
    }

    /// The owned block of the tape must deserialize into the same result.
    #[cfg(feature = "syntax")]
    fn check_tape_value<T>(data: &[u8], expected: &T)
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        use crate::text::syntax::OwnedBlock;
        let tape = TextTape::from_slice(data).unwrap();
        let block: OwnedBlock = TextTapeDeserializer::from_windows1252_tape(&tape)
            .deserialize()
            .unwrap();
        let actual = T::deserialize(&block).unwrap();
        assert_eq!(&actual, expected, "the owned value of the tape differs");
    }

    /// The owned block of `TextDeserializer` from a slice and from readers
    /// must deserialize into the same result.
    #[cfg(feature = "syntax")]
    fn check_owned_value<T>(data: &[u8], expected: &T)
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        use crate::text::syntax::OwnedBlock;
        let encoding = Windows1252Encoding::new();
        let block: OwnedBlock = TextDeserializer::from_slice(data, encoding)
            .deserialize()
            .unwrap();
        let actual = T::deserialize(&block).unwrap();
        assert_eq!(&actual, expected, "the owned value of a slice differs");
        for size in [8, 64] {
            let buf = vec![0; size];
            let block: OwnedBlock = TextDeserializer::from_reader_with_buf(data, encoding, buf)
                .deserialize()
                .unwrap();
            let actual = T::deserialize(&block).unwrap();
            assert_eq!(&actual, expected, "the owned value of a reader differs");
        }
    }

    #[test]
    fn test_maybe_quoted() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            a: MaybeQuoted<String>,
            b: MaybeQuoted<String>,
            c: MaybeQuoted<u32>,
            d: MaybeQuoted<Vec<u32>>,
        }

        let data = b"a = \"@text\"\nb = @text\nc = 3\nd = { 1 2 }";
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                a: MaybeQuoted::Quoted(String::from("@text")),
                b: MaybeQuoted::Unquoted(String::from("@text")),
                c: MaybeQuoted::Unquoted(3),
                d: MaybeQuoted::Unquoted(vec![1, 2]),
            }
        );
    }

    #[test]
    fn test_maybe_quoted_keys() {
        let data = b"\"a\" = 1 b = 2";
        let actual: HashMap<MaybeQuoted<String>, u32> = from_owned(&data[..]);
        let expected = HashMap::from([
            (MaybeQuoted::Quoted(String::from("a")), 1),
            (MaybeQuoted::Unquoted(String::from("b")), 2),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_headered() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: Headered<Vec<u8>>,
            plain: Headered<Vec<u8>>,
            hsv: Headered<[f32; 3]>,
        }

        let data = b"color = rgb { 1 2 3 } plain = { 4 5 6 } hsv = hsv { 0.5 0.25 1.0 }";
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                color: Headered::new(Some(String::from("rgb")), vec![1, 2, 3]),
                plain: Headered::new(None, vec![4, 5, 6]),
                hsv: Headered::new(Some(String::from("hsv")), [0.5, 0.25, 1.0]),
            }
        );
    }

    #[test]
    fn test_headered_scalar() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            name: Headered<String>,
            other: Option<u32>,
        }

        // A scalar with no block is the value, and the reader looks past it
        // for a block.
        let data = b"name = blue other = 1";
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual.name, Headered::new(None, String::from("blue")));
        assert_eq!(actual.other, Some(1));

        // A scalar that an empty block follows is the value.
        let data = b"name = blue { } other = 1";
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual.name, Headered::new(None, String::from("blue")));
        assert_eq!(actual.other, Some(1));

        // So is a scalar that an open block at the end of the data follows.
        let data = b"name = blue {";
        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual.name, Headered::new(None, String::from("blue")));
        let actual: MyStruct = from_windows1252_reader(&data[..]).unwrap();
        assert_eq!(actual.name, Headered::new(None, String::from("blue")));
    }

    #[test]
    fn test_headered_streaming_refill() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            a: Headered<Vec<u8>>,
            b: Headered<Vec<u8>>,
            c: Option<Headered<[u8; 3]>>,
            q: MaybeQuoted<AnyValue>,
            name: Headered<String>,
        }

        // Each buffer size splits the input at a different place, so some
        // header is read past a refill.
        let data = b"a = rgb { 1 2 3 } b = { 4 5 6 } c = hsv { 7 8 9 } q = rgb { 1 2 } name = blue";
        let expected = MyStruct {
            a: Headered::new(Some(String::from("rgb")), vec![1, 2, 3]),
            b: Headered::new(None, vec![4, 5, 6]),
            c: Some(Headered::new(Some(String::from("hsv")), [7, 8, 9])),
            q: MaybeQuoted::Unquoted(AnyValue::Seq(vec![
                AnyValue::Str(String::from("1")),
                AnyValue::Str(String::from("2")),
            ])),
            name: Headered::new(None, String::from("blue")),
        };
        assert_eq!(from_owned::<MyStruct>(&data[..]), expected);
        let encoding = Windows1252Encoding::new();
        for size in 12..=data.len() + 1 {
            let actual = TextDeserializer::from_reader_with_buf(&data[..], encoding, vec![0; size])
                .deserialize::<MyStruct>()
                .unwrap();
            assert_eq!(actual, expected, "buffer size {size}");
        }
    }

    #[test]
    fn test_headered_operator() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            a: Headered<Property<Vec<u8>>>,
            other: u32,
        }

        // The block of a header keeps the operator of the field, which a
        // tape doesn't.
        let data = b"a > rgb { 1 2 } other = 1";
        let actual: MyStruct = from_text(&data[..]);
        let expected = Headered::new(
            Some(String::from("rgb")),
            Property::new(Operator::GreaterThan, vec![1, 2]),
        );
        assert_eq!(actual.a, expected);
        assert_eq!(actual.other, 1);
    }

    /// A quoted scalar is not a header, so the block after it is a block in
    /// key position, which is skipped. The syntax parser reads the block as
    /// a value without a key, which starts the remainder, a difference of
    /// game files only.
    #[test]
    fn test_headered_quoted() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            a: Headered<MaybeQuoted<String>>,
            other: u32,
        }

        let data = b"a = \"rgb\" { 1 2 } other = 1";
        let expected = Headered::new(None, MaybeQuoted::Quoted(String::from("rgb")));
        let actual: MyStruct = super::from_windows1252_slice(&data[..]).unwrap();
        assert_eq!((&actual.a, actual.other), (&expected, 1));
        let actual: MyStruct = from_windows1252_reader(&data[..]).unwrap();
        assert_eq!((&actual.a, actual.other), (&expected, 1));
    }

    #[test]
    fn test_headered_only_field_values() {
        // A header is only recognized for the value of a field, so a
        // headered block in a sequence is an error as with a tape.
        #[derive(Deserialize, Debug, PartialEq)]
        struct List {
            list: Vec<Headered<Vec<u8>>>,
        }
        let data = b"list = { rgb { 1 2 3 } { 4 5 6 } }";
        assert!(from_windows1252_reader::<List, _>(&data[..]).is_err());
        assert!(super::from_windows1252_slice::<List>(&data[..]).is_err());
        assert!(from_tape::<List>(&data[..]).is_err());

        // A key with a block after it is a key with no header.
        let data = b"a { x = 1 } b = 2";
        let actual: HashMap<Headered<String>, AnyValue> = from_owned(&data[..]);
        let expected = HashMap::from([
            (
                Headered::new(None, String::from("a")),
                AnyValue::Map(vec![(String::from("x"), AnyValue::Str(String::from("1")))]),
            ),
            (
                Headered::new(None, String::from("b")),
                AnyValue::Str(String::from("2")),
            ),
        ]);
        assert_eq!(actual, expected);
    }

    /// The elements of a sequence, empty blocks too, are unquoted unless
    /// they are quoted scalars, and have no header.
    #[test]
    fn test_wrappers_in_sequences() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            names: Vec<MaybeQuoted<String>>,
            blocks: Vec<Headered<MaybeQuoted<Vec<u8>>>>,
        }

        let data = b"names = { a \"b\" c } blocks = { { 1 } { 2 3 } }";
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                names: vec![
                    MaybeQuoted::Unquoted(String::from("a")),
                    MaybeQuoted::Quoted(String::from("b")),
                    MaybeQuoted::Unquoted(String::from("c")),
                ],
                blocks: vec![
                    Headered::new(None, MaybeQuoted::Unquoted(vec![1])),
                    Headered::new(None, MaybeQuoted::Unquoted(vec![2, 3])),
                ],
            }
        );

        // An empty block in a sequence is an element, which the tape drops.
        let data = b"blocks = { {} { 2 3 } }";
        let expected = vec![
            Headered::new(None, MaybeQuoted::Unquoted(vec![])),
            Headered::new(None, MaybeQuoted::Unquoted(vec![2, 3])),
        ];
        let actual: MyStructBlocks = from_slice(&data[..]).unwrap();
        assert_eq!(actual.blocks, expected);
        let actual: MyStructBlocks = from_windows1252_reader(&data[..]).unwrap();
        assert_eq!(actual.blocks, expected);

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStructBlocks {
            blocks: Vec<Headered<MaybeQuoted<Vec<u8>>>>,
        }
    }

    /// `TextDeserializer` makes the owned value that the syntax parser
    /// makes, from a slice and from a reader, as a field value and at the
    /// root.
    #[cfg(feature = "syntax")]
    #[test]
    fn test_owned_value() {
        use crate::text::syntax::{OwnedBlock, OwnedValue};

        let data = b"a = { 1 2 } b = \"x\" c = rgb { 1 2 3 } d > 5 e = { f = { g = h } } i = j {} k = { # }\n l } m = @x n = { {} 1 }";
        let encoding = Windows1252Encoding::new();
        let parse = crate::text::syntax::parse(&data[..]);
        let root = OwnedBlock::from_tree(parse.tree(), encoding);
        let fields: HashMap<String, OwnedValue> = root
            .fields()
            .map(|f| (f.key.as_str().to_string(), f.value.clone()))
            .collect();

        let actual: OwnedBlock = TextDeserializer::from_slice(&data[..], encoding)
            .deserialize()
            .unwrap();
        assert_eq!(actual, root);
        let actual: HashMap<String, OwnedValue> = super::from_windows1252_slice(&data[..]).unwrap();
        assert_eq!(actual, fields);
        for size in 8..=data.len() + 1 {
            let buf = vec![0; size];
            let actual = TextDeserializer::from_reader_with_buf(&data[..], encoding, buf)
                .deserialize::<HashMap<String, OwnedValue>>()
                .unwrap();
            assert_eq!(actual, fields, "buffer size {size}");
            let buf = vec![0; size];
            let actual = TextDeserializer::from_reader_with_buf(&data[..], encoding, buf)
                .deserialize::<OwnedBlock>()
                .unwrap();
            assert_eq!(actual, root, "buffer size {size}");
        }
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
        let actual: MyStruct = from_tape(&data[..]).unwrap();
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
    fn test_escaped_backslashes() {
        let data = br#"
            path = "C:\\Users\\me"
            model = "gfx\models\ships\cog.mesh"
            mixed = "gfx\\interface\English_flag.tga"
            old_tooltip = "Traditions: +50%\n"
            new_tooltip = "Traditions: +50%\\n"
            backslash = "\\"
            nested = "division_template = \"Anarchist Militia\""
            unquoted = gfx\\icon.dds
        "#;

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            path: String,
            model: String,
            mixed: String,
            old_tooltip: String,
            new_tooltip: String,
            backslash: String,
            nested: String,
            unquoted: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                path: r"C:\Users\me".to_string(),
                model: r"gfx\models\ships\cog.mesh".to_string(),
                mixed: r"gfx\interface\English_flag.tga".to_string(),
                old_tooltip: r"Traditions: +50%\n".to_string(),
                new_tooltip: r"Traditions: +50%\n".to_string(),
                backslash: r"\".to_string(),
                nested: r#"division_template = "Anarchist Militia""#.to_string(),
                unquoted: r"gfx\\icon.dds".to_string(),
            }
        );
    }

    #[test]
    fn test_escaped_keys_and_any_values() {
        let data = br#""C:\\Users" = "say \"hi\"" plain = "a\\b""#;
        let actual: HashMap<String, String> = from_owned(&data[..]);
        let expected = HashMap::from([
            (r"C:\Users".to_string(), r#"say "hi""#.to_string()),
            ("plain".to_string(), r"a\b".to_string()),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_written_backslashes_round_trip() {
        let path = r#"C:\Users\me "and" \\server\"#;
        let mut out = Vec::new();
        let mut writer = crate::TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"path").unwrap();
        writer.write_quoted(path.as_bytes()).unwrap();
        assert_eq!(out, br#"path="C:\\Users\\me \"and\" \\\\server\\""#);

        let actual: HashMap<String, String> = from_owned(&out);
        assert_eq!(
            actual,
            HashMap::from([("path".to_string(), path.to_string())])
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
        let data = include_bytes!("../../../tests/fixtures/string-array.txt");
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
        let data = include_bytes!("../../../tests/fixtures/savegame.txt");

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
        let data = include_bytes!("../../../tests/fixtures/campaign_stats.txt");

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

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                color: MyColor::Rgb(10, 11, 12),
            },
        );
    }

    #[test]
    fn test_close_at_root() {
        let data = b"c={ x=y } } a=1 } } b=2 }";

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            a: u32,
            b: u32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { a: 1, b: 2 });
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

        let actual: MyStruct = from_tape(&data[..]).unwrap();
        assert_eq!(actual.brittany_area, vec![169, 170, 171, 172, 4384]);

        // The `TextDeserializer` reads the block as a sequence of each token,
        // and the error names the lossless parser.
        let err = from_slice::<MyStruct>(&data[..]).unwrap_err();
        assert_eq!(
            err.to_string(),
            "invalid type: string \"color\", expected u16; TextDeserializer doesn't read an object as a sequence, as the scalars that trail its fields or an object used as a key, which jomini::text::syntax::parse reads (offset: 33)"
        );

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

        let actual: MyStruct = from_owned(&data[..]);
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
    fn test_deserialize_color() {
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

        #[derive(Debug, PartialEq, Deserialize)]
        #[serde(rename_all = "lowercase")]
        enum ColorHeader {
            Rgb,
            Hsv,
        }

        let actual: MyStruct = from_owned(&data[..]);

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
                D: de::Deserializer<'de>,
            {
                struct ColorVisitor;

                impl<'de> de::Visitor<'de> for ColorVisitor {
                    type Value = MyColor;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a color sequence")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::SeqAccess<'de>,
                    {
                        let ty = seq.next_element::<ColorHeader>()?.expect("color type");
                        match ty {
                            ColorHeader::Rgb => {
                                let (red, green, blue) =
                                    seq.next_element::<(u8, u8, u8)>()?.unwrap();
                                Ok(MyColor::Rgb { red, green, blue })
                            }
                            ColorHeader::Hsv => {
                                let (hue, saturation, value) = seq
                                    .next_element::<(f32, f32, f32)>()?
                                    .expect("hsv channels");
                                Ok(MyColor::Hsv {
                                    hue,
                                    saturation,
                                    value,
                                })
                            }
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

        #[derive(Debug, PartialEq)]
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
        let _: Container = from_tape(data.as_bytes()).unwrap();

        // A value is read once: the `TextDeserializer` read `Red` for the
        // `Color` that failed, so the second `next_value` is an error at
        // `Red`, rather than a read of the key after it.
        let err = from_slice::<Container>(data.as_bytes()).err().unwrap();
        assert_eq!(
            err.to_string(),
            "TextDeserializer doesn't read a value again after its read failed, which jomini::text::syntax::parse does (offset: 18)"
        );
    }

    #[test]
    fn test_object_template_deserialize_basic() {
        let data = br"obj={ { a = b }={ 1 2 3 } }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct ObjectKey {
            a: String,
        }

        // Test with tuple deserialization - treats array elements as tuple fields
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (ObjectKey, Vec<i32>),
        }

        let actual: MyStruct = from_tape(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (ObjectKey { a: "b".to_string() }, vec![1, 2, 3])
            }
        );

        // The `TextDeserializer` doesn't read the operator as an element,
        // and the error names the lossless parser.
        let err = from_slice::<MyStruct>(&data[..]).unwrap_err();
        assert_eq!(
            err.to_string(),
            "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 15)"
        );
    }

    #[test]
    fn test_object_template_deserialize_array_key() {
        let data = br"obj={ { foo }={ 1000 } }";

        // Test with tuple deserialization
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (Vec<String>, Vec<i32>),
        }

        let actual: MyStruct = from_tape(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (vec!["foo".to_string()], vec![1000])
            }
        );
    }

    #[test]
    fn test_object_template_deserialize_scalar_value() {
        let data = br"obj={ { a = b }=16 }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct ObjectKey {
            a: String,
        }

        // Test with tuple deserialization
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (ObjectKey, i32),
        }

        let actual: MyStruct = from_tape(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (ObjectKey { a: "b".to_string() }, 16)
            }
        );
    }

    #[test]
    fn test_object_template_deserialize_multiple_entries() {
        let data = br"obj={ { id = 31 type = admin }=16 { id = 32 type = diplo }=18 }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct TemplateKey {
            id: i32,
            #[serde(rename = "type")]
            key_type: String,
        }

        #[derive(PartialEq, Debug)]
        struct ObjectTemplateList(Vec<(TemplateKey, i32)>);

        impl<'de> Deserialize<'de> for ObjectTemplateList {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                use serde::de::{SeqAccess, Visitor};

                struct ObjectTemplateVisitor;

                impl<'de> Visitor<'de> for ObjectTemplateVisitor {
                    type Value = ObjectTemplateList;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("a sequence of alternating objects and values")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let mut entries = Vec::new();

                        while let Some(key) = seq.next_element::<TemplateKey>()? {
                            if let Some(value) = seq.next_element::<i32>()? {
                                entries.push((key, value));
                            } else {
                                return Err(serde::de::Error::custom("Expected value after key"));
                            }
                        }

                        Ok(ObjectTemplateList(entries))
                    }
                }

                deserializer.deserialize_seq(ObjectTemplateVisitor)
            }
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: ObjectTemplateList,
        }

        let actual: MyStruct = from_tape(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: ObjectTemplateList(vec![
                    (
                        TemplateKey {
                            id: 31,
                            key_type: "admin".to_string()
                        },
                        16
                    ),
                    (
                        TemplateKey {
                            id: 32,
                            key_type: "diplo".to_string()
                        },
                        18
                    ),
                ])
            }
        );
    }

    #[test]
    fn test_deserialize_bytes() {
        #[derive(PartialEq, Debug)]
        struct ByteField(Vec<u8>);

        impl<'de> Deserialize<'de> for ByteField {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct ByteVisitor;

                impl<'de> Visitor<'de> for ByteVisitor {
                    type Value = ByteField;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("bytes")
                    }

                    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v.to_vec()))
                    }

                    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v))
                    }
                }

                deserializer.deserialize_bytes(ByteVisitor)
            }
        }

        // Test data with escape sequences that would normally be processed
        let data = br#"raw_field="Joe \"Captain\" Rogers " string_field="Joe \"Captain\" Rogers ""#;

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            raw_field: ByteField,
            string_field: String,
        }

        // A syntax value keeps the decoded text, not the raw bytes, so this
        // test does not use `from_owned`.
        let tape = TextTape::from_slice(&data[..]).unwrap();
        let actual: MyStruct = TextTapeDeserializer::from_windows1252_tape(&tape)
            .deserialize()
            .unwrap();
        #[cfg(feature = "syntax")]
        check_syntax_tree(&data[..], &actual);
        assert_eq!(actual.raw_field.0, b"Joe \\\"Captain\\\" Rogers ");
        assert_eq!(actual.string_field, "Joe \"Captain\" Rogers");
    }

    const EMPTY_KEY_FIRST: &[u8] =
        b"cooldowns={\n\t=1825.1.11\n\ttrade_favors_for_men=1826.6.22\n}";
    const EMPTY_KEY_LAST: &[u8] = b"cooldowns={\n\ttrade_favors_for_men=1826.6.22\n\t=1825.1.11\n}";
    const EMPTY_KEY_ONLY: &[u8] = b"cooldowns={\n\t=1825.6.28\n}";
    const EMPTY_KEY_EXACT: &[u8] =
        b"idea_dates={\n\t==-5000.1.1\n\tnoble_knights=1461.11.17\n\tmilitary_traditions=1473.5.13\n}";

    #[rstest]
    #[case(EMPTY_KEY_FIRST, Some("1826.6.22"))]
    #[case(EMPTY_KEY_LAST, Some("1826.6.22"))]
    #[case(EMPTY_KEY_ONLY, None)]
    fn test_empty_key_struct(#[case] data: &[u8], #[case] expected: Option<&str>) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            cooldowns: Cooldowns,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            trade_favors_for_men: Option<String>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct JominiStruct {
            cooldowns: JominiCooldowns,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct JominiCooldowns {
            trade_favors_for_men: Option<String>,
        }

        let expected = expected.map(String::from);
        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                cooldowns: Cooldowns {
                    trade_favors_for_men: expected.clone()
                }
            }
        );

        let actual: JominiStruct = from_owned(data);
        assert_eq!(
            actual,
            JominiStruct {
                cooldowns: JominiCooldowns {
                    trade_favors_for_men: expected
                }
            }
        );
    }

    #[rstest]
    #[case(EMPTY_KEY_FIRST, &[("", "1825.1.11"), ("trade_favors_for_men", "1826.6.22")])]
    #[case(EMPTY_KEY_LAST, &[("trade_favors_for_men", "1826.6.22"), ("", "1825.1.11")])]
    #[case(EMPTY_KEY_ONLY, &[("", "1825.6.28")])]
    fn test_empty_key_map(#[case] data: &[u8], #[case] expected: &[(&str, &str)]) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyMap {
            cooldowns: HashMap<String, String>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyPairs {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            cooldowns: Vec<(String, String)>,
        }

        let expected: Vec<(String, String)> = expected
            .iter()
            .map(|(k, v)| (String::from(*k), String::from(*v)))
            .collect();

        let actual: MyMap = from_owned(data);
        assert_eq!(
            actual,
            MyMap {
                cooldowns: expected.iter().cloned().collect()
            }
        );

        let actual: MyPairs = from_owned(data);
        assert_eq!(
            actual,
            MyPairs {
                cooldowns: expected
            }
        );
    }

    #[test]
    fn test_empty_key_property() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            idea_dates: Vec<(String, Property<String>)>,
        }

        let actual: MyStruct = from_owned(EMPTY_KEY_EXACT);
        assert_eq!(
            actual,
            MyStruct {
                idea_dates: vec![
                    (
                        String::from(""),
                        Property::new(Operator::Exact, String::from("-5000.1.1"))
                    ),
                    (
                        String::from("noble_knights"),
                        Property::new(Operator::Equal, String::from("1461.11.17"))
                    ),
                    (
                        String::from("military_traditions"),
                        Property::new(Operator::Equal, String::from("1473.5.13"))
                    ),
                ]
            }
        );

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            cooldowns: Vec<(String, Property<String>)>,
        }

        let actual: Cooldowns = from_owned(EMPTY_KEY_LAST);
        assert_eq!(
            actual,
            Cooldowns {
                cooldowns: vec![
                    (
                        String::from("trade_favors_for_men"),
                        Property::new(Operator::Equal, String::from("1826.6.22"))
                    ),
                    (
                        String::from(""),
                        Property::new(Operator::Equal, String::from("1825.1.11"))
                    ),
                ]
            }
        );
    }

    #[derive(Debug, PartialEq)]
    enum AnyValue {
        Str(String),
        Seq(Vec<AnyValue>),
        Map(Vec<(String, AnyValue)>),
    }

    struct AnyVisitor;

    impl<'de> de::Visitor<'de> for AnyVisitor {
        type Value = AnyValue;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("any value")
        }

        fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
            Ok(AnyValue::Str(String::from(v)))
        }

        fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            let mut values = Vec::new();
            while let Some(value) = seq.next_element()? {
                values.push(value);
            }
            Ok(AnyValue::Seq(values))
        }

        fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
            let mut entries = Vec::new();
            while let Some(entry) = map.next_entry()? {
                entries.push(entry);
            }
            Ok(AnyValue::Map(entries))
        }
    }

    impl<'de> Deserialize<'de> for AnyValue {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            deserializer.deserialize_any(AnyVisitor)
        }
    }

    /// Writes `{key=value}` for maps and `[a b]` for sequences.
    impl fmt::Display for AnyValue {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                AnyValue::Str(x) => f.write_str(x),
                AnyValue::Seq(values) => {
                    f.write_str("[")?;
                    for (i, value) in values.iter().enumerate() {
                        let sep = if i == 0 { "" } else { " " };
                        write!(f, "{sep}{value}")?;
                    }
                    f.write_str("]")
                }
                AnyValue::Map(entries) => {
                    f.write_str("{")?;
                    for (i, (key, value)) in entries.iter().enumerate() {
                        let sep = if i == 0 { "" } else { " " };
                        write!(f, "{sep}{key}={value}")?;
                    }
                    f.write_str("}")
                }
            }
        }
    }

    #[test]
    fn test_empty_key_deserialize_any() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            idea_dates: Vec<(AnyValue, AnyValue)>,
        }

        let actual: MyStruct = from_owned(EMPTY_KEY_EXACT);
        assert_eq!(
            actual,
            MyStruct {
                idea_dates: vec![
                    (
                        AnyValue::Str(String::from("")),
                        AnyValue::Str(String::from("-5000.1.1"))
                    ),
                    (
                        AnyValue::Str(String::from("noble_knights")),
                        AnyValue::Str(String::from("1461.11.17"))
                    ),
                    (
                        AnyValue::Str(String::from("military_traditions")),
                        AnyValue::Str(String::from("1473.5.13"))
                    ),
                ]
            }
        );

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            cooldowns: AnyValue,
        }

        let actual: Cooldowns = from_owned(EMPTY_KEY_FIRST);
        assert_eq!(
            actual,
            Cooldowns {
                cooldowns: AnyValue::Map(vec![
                    (String::from(""), AnyValue::Str(String::from("1825.1.11"))),
                    (
                        String::from("trade_favors_for_men"),
                        AnyValue::Str(String::from("1826.6.22"))
                    ),
                ])
            }
        );
    }

    /// The root through `deserialize_map` on a tape, which has no
    /// `deserialize_any` for the root.
    #[derive(Debug, PartialEq)]
    struct TapeRoot(AnyValue);

    impl<'de> Deserialize<'de> for TapeRoot {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            deserializer.deserialize_map(AnyVisitor).map(TapeRoot)
        }
    }

    /// `deserialize_any` from a slice, and from readers with buffers small
    /// enough that refills land inside the lookahead and inside blocks whose
    /// first token was replayed. `tape` is whether a tape gives the same
    /// value; it rejects fields after trailing scalars and operators in
    /// arrays.
    #[rstest]
    #[case::block_kind(
        b"object={ a=b c<2 } array={ 1 2 } empty={} objects={ { a=b } { c=d } } empty_key={\n=b }",
        "{object={a=b c=2} array=[1 2] empty=[] objects=[{a=b} {c=d}] empty_key={=b}}",
        true
    )]
    // An empty block is an element of a sequence, which a tape drops at the
    // start, and not a value in an object.
    #[case::empty_blocks(
        b"leading={ {} {} 3 } only={ {} {} } object={ {} a=b }",
        "{leading=[[] [] 3] only=[[] []] object={a=b}}",
        false
    )]
    // A `key { }` field is a field after the first, but as the first item a
    // scalar followed by a block makes the block an array.
    #[case::key_block_field(
        b"later={ a=b c { d=e } } first={ c { d=e } }",
        "{later={a=b c={d=e}} first=[c {d=e}]}",
        true
    )]
    // A header is only recognized for a field value.
    #[case::header(
        b"color = rgb { 1 2 3 } object = obj { a = b } empty = x {} list = { rgb { 1 } }",
        "{color=[1 2 3] object={a=b} empty=x list=[rgb [1]]}",
        true
    )]
    #[case::comment(
        b"object={ a # note\n = b }\ncolor = rgb # note\n { 1 }",
        "{object={a=b} color=[1]}",
        true
    )]
    #[case::exact_operator(b"object={ a == b c ==d }", "{object={a=b c=d}}", true)]
    #[case::remainder(b"object={ a=b c d }", "{object={a=b remainder=[c d]}}", true)]
    #[case::field_after_remainder(
        b"object={ a=b c d=e }",
        "{object={a=b remainder=[c d = e]}}",
        false
    )]
    #[case::operator_in_array(b"array={ a b=c }", "{array=[a b = c]}", false)]
    #[case::root(b"a=b }\nc={ d=e } f=rgb { 1 }", "{a=b c={d=e} f=[1]}", true)]
    #[case::root_remainder(b"a=b c d", "{a=b remainder=[c d]}", false)]
    #[case::empty(b"", "{}", true)]
    #[case::refill_in_lookahead(
        b"first={ a_long_key\n\n\t\t= value }\nsecond={ alpha # comment\n  beta }\ncolor = a_header_scalar  \n\n  { 1 2 }\nlast=z",
        "{first={a_long_key=value} second=[alpha beta] color=[1 2] last=z}",
        true
    )]
    #[case::refill_after_replay(
        b"list={ alpha beta gamma delta epsilon zeta eta theta }\nobj={ k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6 }\nrem={ a=b c d e f g h i j k l }\ncolor=hdr { 1 2 3 4 5 6 7 8 9 10 11 12 }",
        "{list=[alpha beta gamma delta epsilon zeta eta theta] obj={k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6} rem={a=b remainder=[c d e f g h i j k l]} color=[1 2 3 4 5 6 7 8 9 10 11 12]}",
        true
    )]
    fn test_any(#[case] data: &[u8], #[case] expected: &str, #[case] tape: bool) {
        let actual: AnyValue = from_slice(data).unwrap();
        assert_eq!(actual.to_string(), expected);
        let encoding = Windows1252Encoding::new();
        for size in 20..=40 {
            let actual = TextDeserializer::from_reader_with_buf(data, encoding, vec![0; size])
                .deserialize::<AnyValue>()
                .unwrap();
            assert_eq!(actual.to_string(), expected, "buffer size {size}");
        }

        // The lossless parser recovers from some data that `TextTape`
        // rejects.
        let tape_value = from_tape::<TapeRoot>(data).map(|x| x.0.to_string());
        if tape || !cfg!(all(jomini_syntax_tape, feature = "syntax")) {
            assert_eq!(tape_value.ok().as_deref() == Some(expected), tape);
        }
    }

    /// An operator in key position is a field with an empty key also when
    /// it does not start a line. The lossless parser reads only an operator
    /// that starts a line as a field with an empty key, so its tape differs.
    #[test]
    fn test_any_empty_key_mid_line() {
        let data = b"empty_key={ =b }";
        let expected = "{empty_key={=b}}";
        let actual: AnyValue = super::from_windows1252_slice(data).unwrap();
        assert_eq!(actual.to_string(), expected);
        let encoding = Windows1252Encoding::new();
        for size in 16..=data.len() + 1 {
            let actual = TextDeserializer::from_reader_with_buf(&data[..], encoding, vec![0; size])
                .deserialize::<AnyValue>()
                .unwrap();
            assert_eq!(actual.to_string(), expected, "buffer size {size}");
        }

        let tape: TapeRoot = from_tape(data).unwrap();
        if cfg!(all(jomini_syntax_tape, feature = "syntax")) {
            assert_eq!(tape.0.to_string(), "{empty_key=[b]}");
        } else {
            assert_eq!(tape.0.to_string(), expected);
        }
    }

    #[cfg(feature = "json")]
    #[test]
    fn test_any_json_value() {
        let data = b"a={ b=1 c={ 1 2 } } d=rgb { 3 4 } e={}";
        let actual: serde_json::Value = from_windows1252_reader(&data[..]).unwrap();
        assert_eq!(
            actual,
            serde_json::json!({
                "a": { "b": "1", "c": ["1", "2"] },
                "d": ["3", "4"],
                "e": [],
            })
        );
    }

    #[test]
    fn test_empty_key_sequence() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            cooldowns: Vec<String>,
        }

        // The tape gives the key and the value of each field.
        let actual: MyStruct = from_tape(EMPTY_KEY_FIRST).unwrap();
        assert_eq!(
            actual.cooldowns,
            vec!["", "1825.1.11", "trade_favors_for_men", "1826.6.22"]
        );

        // The deserializer doesn't read an object as a sequence, and the
        // error names the lossless parser.
        let expected = "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 13)";
        let err = from_slice::<MyStruct>(EMPTY_KEY_FIRST).unwrap_err();
        assert_eq!(err.to_string(), expected);
        let err = from_windows1252_reader::<MyStruct, _>(EMPTY_KEY_FIRST).unwrap_err();
        assert_eq!(err.to_string(), expected);
    }

    #[test]
    fn test_operator_on_next_line_after_key() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            name: String,
            #[serde(deserialize_with = "deserialize_vec_pair")]
            block: Vec<(String, String)>,
        }

        let data = b"name\n= \"x\"\nblock={\n\ta = b\n\t= c\n}";
        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                name: String::from("x"),
                block: vec![
                    (String::from("a"), String::from("b")),
                    (String::from(""), String::from("c")),
                ]
            }
        );
    }

    #[test]
    fn test_reader_skips_field_with_block_key() {
        let data = b"obj={ { a = b }={ 1 2 3 } { c = d }=16 e=f }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: HashMap<String, String>,
        }

        let actual: MyStruct = from_windows1252_reader(&data[..]).unwrap();
        let expected = HashMap::from([(String::from("e"), String::from("f"))]);
        assert_eq!(actual, MyStruct { obj: expected });
    }
    type Header = (String, Vec<String>);

    fn header() -> Header {
        let numbers = ["10", "20", "30", "40", "50", "60", "70", "80", "90"];
        (String::from("rgb"), numbers.map(String::from).to_vec())
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct HeaderNewtype(Header);

    #[derive(Deserialize, PartialEq, Debug, Default)]
    #[serde(default)]
    struct HeaderFields {
        value: Option<Header>,
        property: Option<Property<Header>>,
        keys: HashMap<Header, String>,
        elements: Vec<Header>,
        options: Vec<Option<Header>>,
        newtypes: Vec<HeaderNewtype>,
    }

    /// A header and its block read into a sequence type, in each position
    /// that a header can take, with buffers small enough that the reader
    /// refills inside the block while the header is held.
    #[rstest]
    #[case::value(b"value = rgb { 10 20 30 40 50 60 70 80 90 }", HeaderFields {
        value: Some(header()),
        ..HeaderFields::default()
    })]
    #[case::property(b"property < rgb { 10 20 30 40 50 60 70 80 90 }", HeaderFields {
        property: Some(Property::new(Operator::LessThan, header())),
        ..HeaderFields::default()
    })]
    #[case::key(b"keys = { rgb { 10 20 30 40 50 60 70 80 90 } = x }", HeaderFields {
        keys: HashMap::from([(header(), String::from("x"))]),
        ..HeaderFields::default()
    })]
    #[case::element(b"elements = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
        elements: vec![header()],
        ..HeaderFields::default()
    })]
    #[case::option_element(b"options = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
        options: vec![Some(header())],
        ..HeaderFields::default()
    })]
    #[case::newtype_element(b"newtypes = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
        newtypes: vec![HeaderNewtype(header())],
        ..HeaderFields::default()
    })]
    fn test_header_seq_streaming_refill(#[case] data: &[u8], #[case] expected: HeaderFields) {
        // The syntax parser reads a scalar and a block first in a block as a
        // field, a difference of game files only.
        let actual: HeaderFields = super::from_windows1252_slice(data).unwrap();
        assert_eq!(actual, expected);
        let encoding = Windows1252Encoding::new();
        for size in 16..=24 {
            let actual: HeaderFields =
                TextDeserializer::from_reader_with_buf(data, encoding, vec![0; size])
                    .deserialize()
                    .unwrap();
            assert_eq!(actual, expected, "buffer size {size}");
        }
    }
}
