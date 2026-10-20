//! **Experimental** serde deserialization that reads the syntax tree.
//!
//! This deserializer reads a [`SyntaxTree`] directly. It does not make a
//! [`TextTape`](crate::TextTape) first. It has the same behavior as
//! [`TextDeserializer`](crate::TextDeserializer) on real game files, and it
//! adds these features:
//!
//! - Errors tell the location: the byte range, the line and column, the
//!   path of keys, and the file name when you give one. See [`Error`].
//! - Wrapper types keep data that serde does not model: [`MaybeQuoted`]
//!   keeps the quotes, [`WithOp`] keeps the operator, [`Headered`] keeps the
//!   header (`rgb` in `rgb { 1 2 3 }`), and [`Spanned`] keeps the byte range.
//! - The owned [`Value`](super::value::Value) keeps all of the document.
//! - [`IgnoredAny`](serde::de::IgnoredAny) skips a value of any size in
//!   constant time.
//!
//! ```
//! use jomini::text::lossless::de::from_windows1252_slice;
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Area {
//!     color: Vec<u8>,
//!     remainder: Vec<u32>,
//! }
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Areas {
//!     brittany_area: Area,
//! }
//!
//! let source = b"brittany_area = { color = { 118 99 151 } 169 170 171 }";
//! let areas: Areas = from_windows1252_slice(source)?;
//! assert_eq!(areas.brittany_area.color, vec![118, 99, 151]);
//! assert_eq!(areas.brittany_area.remainder, vec![169, 170, 171]);
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! # Rules
//!
//! The deserializer uses the rules of `TextDeserializer`:
//!
//! - Duplicate keys are separate map entries in source order. Use
//!   `#[jomini(duplicated)]` of [`JominiDeserialize`](crate::JominiDeserialize)
//!   to collect them.
//! - A container is an object when its first entry is a field with an
//!   operator. Else it is an array. A map over an object gives the fields up
//!   to the first bare value, then the key `remainder` with the rest.
//! - A sequence over a block with fields and values (a mixed container)
//!   starts at the first bare value.
//! - A sequence over a headered block (`rgb { 1 2 3 }`) gives the header and
//!   then the block.
//! - A field with no operator (`key { ... }`) is a field.
//!
//! # Syntax errors
//!
//! The parser recovers from syntax errors. [`from_slice`] and the other
//! functions that parse the source return the first syntax error, but they
//! ignore an extra closing brace (`}`), as `TextTape` does. To deserialize
//! a tree that has errors, make a [`Deserializer`] from the tree.

mod engine;
mod error;
mod tree;
mod wrappers;

pub use self::error::{Error, ErrorKind};
pub use self::wrappers::{Headered, MaybeQuoted, Spanned, WithOp};

use self::engine::ValueDe;
use self::tree::{TreeCtx, TreeDoc};
use super::{Flavor, Parse, SyntaxErrorKind, SyntaxTree, parse_with};
use crate::{Encoding, Utf8Encoding, Windows1252Encoding};
use serde::de::{self, Deserialize, Visitor};

/// Parse `source` and deserialize it into `T`.
///
/// The data of `T` can borrow from `source`. The encoding decodes the
/// scalars into strings.
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::de::from_slice};
/// use std::collections::HashMap;
///
/// let source = b"a = 1 b = 2";
/// let map: HashMap<&str, u32> = from_slice(source, Utf8Encoding::new())?;
/// assert_eq!(map["b"], 2);
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
pub fn from_slice<'a, T, E>(source: &'a [u8], encoding: E) -> Result<T, Error>
where
    T: Deserialize<'a>,
    E: Encoding,
{
    from_slice_with(source, Flavor::default(), encoding)
}

/// Parse `source` with a [`Flavor`] and deserialize it into `T`.
pub fn from_slice_with<'a, T, E>(source: &'a [u8], flavor: Flavor, encoding: E) -> Result<T, Error>
where
    T: Deserialize<'a>,
    E: Encoding,
{
    let parse = parse_with(source, flavor);
    check_errors(&parse.tree())?;
    Deserializer::new(parse.borrowed_tree(), encoding).deserialize()
}

/// Parse UTF-8 `source` and deserialize it into `T`.
pub fn from_utf8_slice<'a, T>(source: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    from_slice(source, Utf8Encoding::new())
}

/// Parse Windows-1252 `source` and deserialize it into `T`.
pub fn from_windows1252_slice<'a, T>(source: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    from_slice(source, Windows1252Encoding::new())
}

/// Deserialize a tree into `T`. This function does not examine the syntax
/// errors of the tree.
pub fn from_tree<'a, T, E>(tree: SyntaxTree<'_, 'a>, encoding: E) -> Result<T, Error>
where
    T: Deserialize<'a>,
    E: Encoding,
{
    Deserializer::new(tree, encoding).deserialize()
}

/// Add the line, the column, and the file name to an error.
#[cold]
pub(crate) fn finish(error: Error, source: &[u8], file: Option<&str>) -> Error {
    let error = error.locate(source);
    match file {
        Some(file) if error.file().is_none() => error.with_file(file),
        _ => error,
    }
}

/// Return the first syntax error that `TextTape` does not ignore.
fn check_errors(tree: &SyntaxTree<'_, '_>) -> Result<(), Error> {
    let error = tree
        .errors()
        .iter()
        .find(|e| e.kind != SyntaxErrorKind::UnmatchedCloseBrace);
    match error {
        Some(error) => Err(Error::syntax(error).locate(tree.source())),
        None => Ok(()),
    }
}

impl<'a> Parse<&'a [u8]> {
    /// The [`SyntaxTree`] view with the lifetime of the borrowed source.
    ///
    /// The text of the tree can then live longer than the `Parse`, as the
    /// data that the deserializer borrows from the source.
    pub fn borrowed_tree(&self) -> SyntaxTree<'_, 'a> {
        SyntaxTree {
            data: &self.data,
            source: self.source,
        }
    }
}

/// A serde deserializer for a [`SyntaxTree`].
///
/// ```
/// use jomini::{Windows1252Encoding, text::lossless::{parse, de::Deserializer}};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Meta {
///     date: String,
/// }
///
/// let tree = parse(b"date = 1444.11.11");
/// let meta: Meta = Deserializer::new(tree.tree(), Windows1252Encoding::new())
///     .with_file("meta.txt")
///     .deserialize()?;
/// assert_eq!(meta.date, "1444.11.11");
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
pub struct Deserializer<'t, 'a, E> {
    ctx: TreeCtx<'t, 'a, E>,
    file: Option<String>,
}

impl<'t, 'a, E: Encoding> Deserializer<'t, 'a, E> {
    /// Make a deserializer for `tree`. The encoding decodes the scalars.
    pub fn new(tree: SyntaxTree<'t, 'a>, encoding: E) -> Self {
        Deserializer {
            ctx: TreeCtx::new(tree, encoding),
            file: None,
        }
    }

    /// Set the file name that the errors show.
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// Deserialize the tree into `T`.
    pub fn deserialize<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        T::deserialize(self)
    }

    #[cold]
    fn finish(&self, error: Error) -> Error {
        finish(error, self.ctx.tree().source(), self.file.as_deref())
    }
}

macro_rules! forward_root {
    ($($method:ident ( $($arg:ident : $ty:ty),* ))*) => {$(
        fn $method<V: Visitor<'a>>(self, $($arg: $ty,)* visitor: V) -> Result<V::Value, Error> {
            let doc = TreeDoc::new(&self.ctx);
            let root = ValueDe::root(doc, doc.root());
            de::Deserializer::$method(root, $($arg,)* visitor).map_err(|e| self.finish(e))
        }
    )*};
}

impl<'a, E: Encoding> de::Deserializer<'a> for &Deserializer<'_, 'a, E> {
    type Error = Error;

    forward_root! {
        deserialize_any()
        deserialize_bool()
        deserialize_i8()
        deserialize_i16()
        deserialize_i32()
        deserialize_i64()
        deserialize_i128()
        deserialize_u8()
        deserialize_u16()
        deserialize_u32()
        deserialize_u64()
        deserialize_u128()
        deserialize_f32()
        deserialize_f64()
        deserialize_char()
        deserialize_str()
        deserialize_string()
        deserialize_bytes()
        deserialize_byte_buf()
        deserialize_option()
        deserialize_unit()
        deserialize_unit_struct(name: &'static str)
        deserialize_newtype_struct(name: &'static str)
        deserialize_seq()
        deserialize_tuple(len: usize)
        deserialize_tuple_struct(name: &'static str, len: usize)
        deserialize_map()
        deserialize_struct(name: &'static str, fields: &'static [&'static str])
        deserialize_enum(name: &'static str, variants: &'static [&'static str])
        deserialize_identifier()
        deserialize_ignored_any()
    }
}
