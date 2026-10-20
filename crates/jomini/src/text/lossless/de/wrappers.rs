//! Wrapper types that keep data that serde does not model: quotes,
//! operators, headers, and byte ranges.
//!
//! Each wrapper asks the deserializer for a newtype struct with a private
//! name. The tree deserializer (and the deserializer of a
//! [`Value`](crate::text::lossless::value::Value)) knows the name and gives
//! the extra data. A different deserializer, such as `TextDeserializer` or
//! `serde_json`, does not know the name and gives only the inner value. Then
//! the wrapper uses a fallback: no quotes, no operator, no header, no range.

use super::engine::{HEADERED, QUOTED, SPANNED, WITH_OP};
use crate::text::{Operator, lossless::TextRange};
use serde::de::{self, Deserialize, Deserializer, SeqAccess, Visitor};
use std::{fmt, marker::PhantomData};

fn missing<E: de::Error>(index: usize) -> E {
    E::invalid_length(index, &"the parts of a jomini wrapper type")
}

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
/// A different deserializer gives no range: [`range`](Spanned::range) is
/// `None`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let start: u32 = seq.next_element()?.ok_or_else(|| missing(0))?;
                let end: u32 = seq.next_element()?.ok_or_else(|| missing(1))?;
                let value: T = seq.next_element()?.ok_or_else(|| missing(2))?;
                Ok(Spanned::new(Some(TextRange::new(start, end)), value))
            }
        }

        deserializer.deserialize_newtype_struct(SPANNED, SpannedVisitor(PhantomData))
    }
}

/// A scalar that tells if it had quotes in the source.
///
/// This type solves the problem of variable expansion: `@name` is a
/// variable, but `"@name"` is text.
///
/// ```
/// use jomini::text::lossless::de::{MaybeQuoted, from_utf8_slice};
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
/// [`Unquoted`](MaybeQuoted::Unquoted). A different deserializer does not
/// know about quotes, so its values are also `Unquoted`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
                f.write_str("a scalar")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                T::deserialize(deserializer).map(MaybeQuoted::Unquoted)
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let quoted: bool = seq.next_element()?.ok_or_else(|| missing(0))?;
                let value: T = seq.next_element()?.ok_or_else(|| missing(1))?;
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

/// The value of a field and the operator of the field.
///
/// The operator is `None` when the field has no operator, as in
/// `key { ... }`. This type is like [`Property`](crate::text::Property), but
/// it keeps the difference between `=` and no operator, and it works with a
/// different deserializer (the operator is then `None`).
///
/// ```
/// use jomini::text::{Operator, lossless::de::{WithOp, from_utf8_slice}};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Trigger {
///     age: WithOp<u32>,
///     limit: WithOp<Vec<u32>>,
/// }
///
/// let trigger: Trigger = from_utf8_slice(b"age >= 16 limit { 1 2 }").unwrap();
/// assert_eq!(trigger.age.op(), Some(Operator::GreaterThanEqual));
/// assert_eq!(*trigger.age.value(), 16);
/// assert_eq!(trigger.limit.op(), None);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WithOp<T> {
    op: Option<Operator>,
    value: T,
}

impl<T> WithOp<T> {
    /// Make a value with an operator.
    pub fn new(op: Option<Operator>, value: T) -> Self {
        WithOp { op, value }
    }

    /// The operator of the field.
    pub fn op(&self) -> Option<Operator> {
        self.op
    }

    /// The value.
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Discard the operator.
    pub fn into_value(self) -> T {
        self.value
    }
}

impl<'de, T: Deserialize<'de>> Deserialize<'de> for WithOp<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct OpVisitor<T>(PhantomData<T>);

        impl<'de, T: Deserialize<'de>> Visitor<'de> for OpVisitor<T> {
            type Value = WithOp<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a value")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                T::deserialize(deserializer).map(|value| WithOp::new(None, value))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let op: Option<Operator> = seq.next_element()?.ok_or_else(|| missing(0))?;
                let value: T = seq.next_element()?.ok_or_else(|| missing(1))?;
                Ok(WithOp::new(op, value))
            }
        }

        deserializer.deserialize_newtype_struct(WITH_OP, OpVisitor(PhantomData))
    }
}

/// A value and the header in front of its block, as `rgb` in
/// `color = rgb { 10 20 30 }`.
///
/// The value is the block without the header. When the value has no
/// header, [`header`](Headered::header) is `None` and the value is the full
/// value. A different deserializer gives no header.
///
/// ```
/// use jomini::text::lossless::de::{Headered, from_utf8_slice};
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
                f.write_str("a value")
            }

            fn visit_newtype_struct<D: Deserializer<'de>>(
                self,
                deserializer: D,
            ) -> Result<Self::Value, D::Error> {
                T::deserialize(deserializer).map(|value| Headered::new(None, value))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
                let header: Option<H> = seq.next_element()?.ok_or_else(|| missing(0))?;
                let value: T = seq.next_element()?.ok_or_else(|| missing(1))?;
                Ok(Headered::new(header, value))
            }
        }

        deserializer.deserialize_newtype_struct(HEADERED, HeaderVisitor(PhantomData))
    }
}
