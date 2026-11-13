//! [`Spanned`] keeps the byte range of a value.
//!
//! The wrapper asks the deserializer for a newtype struct with a private
//! name. The tree deserializer knows the name and gives the range. A
//! different deserializer does not know the name and gives only the inner
//! value. Then the range is `None`.

use super::engine::SPANNED;
use crate::text::lossless::{TextRange, TextSize};
use crate::text::wrappers::{KEY_RANGE, read_parts};
use serde::de::{self, Deserialize, Deserializer, MapAccess, Visitor};
use std::{fmt, marker::PhantomData};

/// A value and its byte range in the source.
///
/// ```
/// use jomini::text::lossless::de::{Spanned, from_utf8_slice};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct File {
///     name: Spanned<String>,
/// }
///
/// let source = b"name = \"Paris\"";
/// let file: File = from_utf8_slice(source).unwrap();
/// let range = file.name.range().unwrap();
/// assert_eq!(&source[range.to_range()], b"\"Paris\"");
/// assert_eq!(file.name.value(), "Paris");
/// ```
///
/// A different deserializer, such as `TextDeserializer` or the deserializer
/// of an owned value, gives no range: [`range`](Spanned::range) is `None`.
///
/// As `serde_spanned` does, a `Spanned` compares, orders, and hashes as its
/// value. The range has no effect, so the same value at two positions is
/// equal:
///
/// ```
/// use jomini::text::lossless::de::{Spanned, from_utf8_slice};
/// use std::collections::HashMap;
///
/// let map: HashMap<Spanned<String>, u32> = from_utf8_slice(b"a = 1 b = 2")?;
/// assert_eq!(map[&Spanned::new(None, String::from("b"))], 2);
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    range: Option<TextRange>,
    value: T,
}

impl<T> Spanned<T> {
    /// Make a value with a range.
    pub fn new(range: Option<TextRange>, value: T) -> Self {
        Spanned { range, value }
    }

    /// The byte range of the value in the source. For a field, this is the
    /// range of the value, not of the key.
    pub fn range(&self) -> Option<TextRange> {
        self.range
    }

    /// The value.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// The value, mutable.
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }

    /// Discard the range.
    pub fn into_value(self) -> T {
        self.value
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: PartialOrd> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<T: Ord> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.value.cmp(&other.value)
    }
}

impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<S: std::hash::Hasher>(&self, state: &mut S) {
        self.value.hash(state);
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for Spanned<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct SpannedVisitor<T>(PhantomData<T>);

        impl<'de, T: Deserialize<'de>> Visitor<'de> for SpannedVisitor<T> {
            type Value = Spanned<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a value")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                T::deserialize(deserializer).map(|value| Spanned::new(None, value))
            }

            fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<Self::Value, A::Error> {
                let ((start, end), value): ((u32, u32), T) = read_parts(map, KEY_RANGE)?;
                if start > end {
                    return Err(de::Error::custom("range start is greater than range end"));
                }
                let range = TextRange::new(TextSize::new(start), TextSize::new(end));
                Ok(Spanned::new(Some(range), value))
            }
        }

        deserializer.deserialize_newtype_struct(SPANNED, SpannedVisitor(PhantomData))
    }
}
