//! Wrapper types that keep data that a plain serde type does not: the quotes
//! of a scalar and the header of a block.
//!
//! Each text deserializer of this crate knows these types: `TextDeserializer`,
//! the streaming reader, the deserializer of the lossless tree, and the
//! deserializer of an owned value. A different deserializer does not know
//! the quotes or the header, so the wrapper returns an error and does not
//! guess. `#[serde(flatten)]` and `#[serde(untagged)]` also hide them,
//! because serde first copies the data into its own model.
//!
//! A wrapper type asks the deserializer for a newtype struct with a private
//! name. A deserializer that knows the name gives a map with two entries:
//! a part under a private key, such as the quotes or the header, and then
//! the value under [`KEY_VALUE`]. The wrapper reads only a map with these
//! keys, so ordinary data from a different deserializer is never protocol
//! data.

use serde::de::{
    self, Deserialize, DeserializeSeed, Deserializer, MapAccess, Unexpected, Visitor,
    value::BorrowedStrDeserializer,
};
use std::{fmt, marker::PhantomData};

/// The serde name of [`MaybeQuoted`].
pub(crate) const QUOTED: &str = "$__jomini_private_MaybeQuoted";
/// The serde name of [`Headered`].
pub(crate) const HEADERED: &str = "$__jomini_private_Headered";

/// The error when a deserializer does not know the quotes.
const UNKNOWN_QUOTES: &str = "MaybeQuoted needs a jomini text deserializer; \
    serde(flatten), serde(untagged), and other deserializers do not keep the quotes";
/// The error when a deserializer does not know the header.
const UNKNOWN_HEADER: &str = "Headered needs a jomini text deserializer; \
    serde(flatten), serde(untagged), and other deserializers do not keep the header";

/// The key of the value in each private map.
pub(crate) const KEY_VALUE: &str = "$__jomini_private_value";
/// The key of the quotes, a `bool`.
pub(crate) const KEY_QUOTED: &str = "$__jomini_private_quoted";
/// The key of the header, an optional scalar.
pub(crate) const KEY_HEADER: &str = "$__jomini_private_header";
/// The key of the byte range, a sequence of a start and an end.
#[cfg(feature = "lossless")]
pub(crate) const KEY_RANGE: &str = "$__jomini_private_range";

/// The private map that a deserializer gives to a wrapper type: the part
/// under `key`, then the value under [`KEY_VALUE`].
pub(crate) struct WrapperMap<P, V> {
    key: &'static str,
    part: Option<P>,
    value: Option<V>,
}

impl<P, V> WrapperMap<P, V> {
    pub(crate) fn new(key: &'static str, part: P, value: V) -> Self {
        WrapperMap {
            key,
            part: Some(part),
            value: Some(value),
        }
    }
}

impl<'de, P, V, E> MapAccess<'de> for WrapperMap<P, V>
where
    P: Deserializer<'de, Error = E>,
    V: Deserializer<'de, Error = E>,
    E: de::Error,
{
    type Error = E;

    fn next_key_seed<K: DeserializeSeed<'de>>(&mut self, seed: K) -> Result<Option<K::Value>, E> {
        let key = if self.part.is_some() {
            self.key
        } else if self.value.is_some() {
            KEY_VALUE
        } else {
            return Ok(None);
        };
        seed.deserialize(BorrowedStrDeserializer::new(key))
            .map(Some)
    }

    fn next_value_seed<S: DeserializeSeed<'de>>(&mut self, seed: S) -> Result<S::Value, E> {
        if let Some(part) = self.part.take() {
            seed.deserialize(part)
        } else if let Some(value) = self.value.take() {
            seed.deserialize(value)
        } else {
            Err(E::custom("a jomini wrapper type has two parts"))
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(usize::from(self.part.is_some()) + usize::from(self.value.is_some()))
    }
}

/// A deserializer for an optional part, such as a header.
pub(crate) struct OptionDe<D>(pub(crate) Option<D>);

impl<'de, D: Deserializer<'de>> Deserializer<'de> for OptionDe<D> {
    type Error = D::Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, D::Error> {
        match self.0 {
            Some(d) => visitor.visit_some(d),
            None => visitor.visit_none(),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// Read the private map of a wrapper type: the part under `key`, then the
/// value.
pub(crate) fn read_parts<'de, A, P, T>(mut map: A, key: &'static str) -> Result<(P, T), A::Error>
where
    A: MapAccess<'de>,
    P: Deserialize<'de>,
    T: Deserialize<'de>,
{
    expect_key(&mut map, key)?;
    let part = map.next_value()?;
    expect_key(&mut map, KEY_VALUE)?;
    let value = map.next_value()?;
    Ok((part, value))
}

fn expect_key<'de, A: MapAccess<'de>>(map: &mut A, key: &'static str) -> Result<(), A::Error> {
    match map.next_key_seed(PrivateKey(key))? {
        Some(()) => Ok(()),
        None => Err(de::Error::custom(format_args!(
            "expected the private key `{key}`"
        ))),
    }
}

/// Accepts only one private key.
struct PrivateKey(&'static str);

impl<'de> DeserializeSeed<'de> for PrivateKey {
    type Value = ();

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<(), D::Error> {
        deserializer.deserialize_identifier(self)
    }
}

impl Visitor<'_> for PrivateKey {
    type Value = ();

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the private key `{}`", self.0)
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<(), E> {
        if v == self.0 {
            Ok(())
        } else {
            Err(E::invalid_value(Unexpected::Str(v), &self))
        }
    }
}

/// A scalar that tells if it had quotes in the source.
///
/// This type solves the problem of variable expansion: `@name` is a
/// variable, but `"@name"` is text.
///
/// ```
/// use jomini::text::{MaybeQuoted, de::from_utf8_slice};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct File {
///     a: MaybeQuoted<String>,
///     b: MaybeQuoted<String>,
/// }
///
/// let file: File = from_utf8_slice(b"a = \"@text\"\nb = @text").unwrap();
/// assert_eq!(file.a, MaybeQuoted::Quoted(String::from("@text")));
/// assert_eq!(file.b, MaybeQuoted::Unquoted(String::from("@text")));
/// ```
///
/// A value that is not a scalar, such as a block, is
/// [`Unquoted`](MaybeQuoted::Unquoted).
///
/// Only a text deserializer of this crate knows the quotes. A different
/// deserializer gives an error, and so does a field in a
/// `#[serde(flatten)]` or `#[serde(untagged)]` container:
///
/// ```
/// use jomini::text::{MaybeQuoted, de::from_utf8_slice};
/// use serde::Deserialize;
/// use std::collections::HashMap;
///
/// #[derive(Deserialize, Debug)]
/// struct File {
///     #[serde(flatten)]
///     rest: HashMap<String, MaybeQuoted<String>>,
/// }
///
/// let error = from_utf8_slice::<File>(b"a = \"@text\"").unwrap_err();
/// assert!(error.to_string().contains("MaybeQuoted needs a jomini text deserializer"));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MaybeQuoted<T> {
    /// The scalar had quotes, as in `"text"`.
    Quoted(T),
    /// The scalar had no quotes, as in `text`.
    Unquoted(T),
}

impl<T> MaybeQuoted<T> {
    /// Return `true` when the scalar had quotes.
    pub fn is_quoted(&self) -> bool {
        matches!(self, MaybeQuoted::Quoted(_))
    }

    /// The value.
    pub fn value(&self) -> &T {
        match self {
            MaybeQuoted::Quoted(x) | MaybeQuoted::Unquoted(x) => x,
        }
    }

    /// The value, mutable.
    pub fn value_mut(&mut self) -> &mut T {
        match self {
            MaybeQuoted::Quoted(x) | MaybeQuoted::Unquoted(x) => x,
        }
    }

    /// Discard the quote information.
    pub fn into_value(self) -> T {
        match self {
            MaybeQuoted::Quoted(x) | MaybeQuoted::Unquoted(x) => x,
        }
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for MaybeQuoted<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct QuotedVisitor<T>(PhantomData<T>);

        impl<'de, T: Deserialize<'de>> Visitor<'de> for QuotedVisitor<T> {
            type Value = MaybeQuoted<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a value from a jomini text deserializer")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                _deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                Err(de::Error::custom(UNKNOWN_QUOTES))
            }

            fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
                let (quoted, value): (bool, T) = read_parts(map, KEY_QUOTED)?;
                Ok(if quoted {
                    MaybeQuoted::Quoted(value)
                } else {
                    MaybeQuoted::Unquoted(value)
                })
            }
        }

        deserializer.deserialize_newtype_struct(QUOTED, QuotedVisitor(PhantomData))
    }
}

/// A value and the header in front of its block, as `rgb` in
/// `color = rgb { 10 20 30 }`.
///
/// The value is the block without the header. When the value has no
/// header, [`header`](Headered::header) is `None` and the value is the full
/// value.
///
/// Only a text deserializer of this crate knows the header. A different
/// deserializer gives an error, and so does a field in a
/// `#[serde(flatten)]` or `#[serde(untagged)]` container. The streaming
/// reader cannot look at the next token, so it reads a scalar as a header
/// and expects a block after it.
///
/// ```
/// use jomini::text::{Headered, de::from_utf8_slice};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Country {
///     color: Headered<[u8; 3]>,
///     revolutionary_colors: Headered<[u8; 3]>,
/// }
///
/// let source = b"color = rgb { 10 20 30 } revolutionary_colors = { 1 2 3 }";
/// let country: Country = from_utf8_slice(source).unwrap();
/// assert_eq!(country.color.header().map(String::as_str), Some("rgb"));
/// assert_eq!(*country.color.value(), [10, 20, 30]);
/// assert_eq!(country.revolutionary_colors.header(), None);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Headered<T, H = String> {
    header: Option<H>,
    value: T,
}

impl<T, H> Headered<T, H> {
    /// Make a value with a header.
    pub fn new(header: Option<H>, value: T) -> Self {
        Headered { header, value }
    }

    /// The header, such as `rgb` or `hsv`.
    pub fn header(&self) -> Option<&H> {
        self.header.as_ref()
    }

    /// The value without the header.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// The value without the header, mutable.
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Discard the header.
    pub fn into_value(self) -> T {
        self.value
    }

    /// The header and the value.
    pub fn into_parts(self) -> (Option<H>, T) {
        (self.header, self.value)
    }
}

impl<'de, T, H> Deserialize<'de> for Headered<T, H>
where
    T: Deserialize<'de>,
    H: Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct HeaderVisitor<T, H>(PhantomData<(T, H)>);

        impl<'de, T, H> Visitor<'de> for HeaderVisitor<T, H>
        where
            T: Deserialize<'de>,
            H: Deserialize<'de>,
        {
            type Value = Headered<T, H>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a value from a jomini text deserializer")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                _deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                Err(de::Error::custom(UNKNOWN_HEADER))
            }

            fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
                let (header, value): (Option<H>, T) = read_parts(map, KEY_HEADER)?;
                Ok(Headered::new(header, value))
            }
        }

        deserializer.deserialize_newtype_struct(HEADERED, HeaderVisitor(PhantomData))
    }
}
