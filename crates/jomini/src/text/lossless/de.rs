//! **Experimental** serde deserialization that reads the syntax tree.
//!
//! [`TextDeserializer`](crate::TextDeserializer) reads a
//! [`TextTape`](crate::TextTape). This module reads the lossless
//! [`SyntaxTree`] directly and uses the same rules, so a type that derives
//! [`Deserialize`] needs no change. Because the tree knows the position of
//! each value, an [`Error`] tells the line, the column, and the path of keys.
//!
//! This deserializer is for the game and mod files that a tool also checks or
//! changes. To only deserialize a large save file, use
//! [`from_windows1252_reader`](crate::text::de::from_windows1252_reader): it
//! streams the file, so it is faster and uses less memory.
//!
//! # Deserialize a file
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
//!
//! let error = from_windows1252_slice::<Areas>(b"brittany_area = {\n  color = blue\n}").unwrap_err();
//! assert_eq!(
//!     error.to_string(),
//!     r#"2:11: brittany_area.color[0]: invalid type: string "blue", expected u8"#
//! );
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! [`from_slice`] and the other functions that parse the source return the
//! first syntax error. They ignore an extra closing brace (`}`), as
//! `TextTape` does.
//!
//! # Read a file that has syntax errors
//!
//! The parser recovers from syntax errors, so an editor can read the data
//! before and after an error. [`from_tree`] and [`Document`] read a tree
//! and do not examine its syntax errors. They fail only when the data does
//! not match the type. To find the syntax errors, read
//! [`Parse::errors`](super::Parse::errors), or call [`Document::check`],
//! which gives the first error as an [`Error`].
//!
//! # Read part of a file
//!
//! To read only some values, find them with the typed AST of the tree
//! ([`Root`](super::Root), [`Block`](super::Block), [`Field`](super::Field),
//! [`Value`](super::Value)). Then give each value to a [`Document`]. It
//! decodes the value or deserializes it into a serde type. Its errors also
//! tell the file name:
//!
//! ```
//! use jomini::{Utf8Encoding, text::lossless::{parse, de::Document}};
//!
//! let tree = parse(b"state = { id = 1 provinces = { 3838 9851 } }");
//! let doc = Document::new(tree.tree(), Utf8Encoding::new()).with_file("1-France.txt");
//! let state = doc.block(doc.require(doc.root(), b"state")?)?;
//!
//! let provinces: Vec<u32> = doc.deserialize_value(doc.require(state, b"provinces")?)?;
//! assert_eq!(provinces, [3838, 9851]);
//!
//! let error = doc.require(state, b"capital").unwrap_err();
//! assert_eq!(error.to_string(), "1-France.txt:1:9: state: missing key `capital`");
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! # Keep a document
//!
//! An [`OwnedBlock`](super::OwnedBlock) holds a whole document as data. It
//! owns its text, so you can keep it after the source is gone. It keeps the
//! quotes, the variables, the operators, the headers, duplicate keys, and
//! the order of the items. It does not keep the comments and the
//! whitespace. For them, keep the tree.
//!
//! A block deserializes as the root of a document. Thus it gives the same
//! result as the tree:
//!
//! ```
//! use jomini::{Utf8Encoding, text::lossless::{parse, OwnedBlock, de::from_tree}};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Areas {
//!     brittany: Vec<u32>,
//! }
//!
//! let tree = parse(b"brittany { 169 170 }");
//! let root = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
//! drop(tree);
//!
//! assert_eq!(Areas::deserialize(&root)?, Areas { brittany: vec![169, 170] });
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! # Keep what a serde type loses
//!
//! A plain serde type does not keep the quotes, the operator, the header
//! (`rgb` in `rgb { 1 2 3 }`), or the position of a value. These wrapper
//! types keep one of them:
//!
//! - [`MaybeQuoted`](crate::text::MaybeQuoted) keeps the quotes.
//! - [`Property`](crate::text::Property) keeps the operator.
//! - [`Headered`](crate::text::Headered) keeps the header.
//! - [`Spanned`] keeps the byte range. Only the tree knows the position of
//!   a value.
//!
//! The wrapper types also work with `TextDeserializer`. An
//! [`OwnedBlock`](super::OwnedBlock) keeps all of these, except the
//! position.
//!
//! In `#[serde(flatten)]` or `#[serde(untagged)]` containers, serde first
//! copies the data into its own model, which has no quotes and no headers.
//! Then `MaybeQuoted` and `Headered` return an error, and an owned value
//! is unquoted text with `=` operators.
//!
//! [`IgnoredAny`](serde::de::IgnoredAny) skips a value of any size in
//! constant time.
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
//! - A tuple or an array reads the elements it needs, and the rest of the
//!   sequence has no effect, as with `TextDeserializer`. Thus a `[u8; 3]`
//!   reads `{ 1 2 3 255 }`. To reject more elements, read a `Vec`.
//! - A CK3 reader directive (`@:insert name`) is not an entry. In the value
//!   of a field, as in `x = @:load_variable name`, it is the text of the
//!   directive. The deserializer does not expand a directive.

mod document;
mod engine;
mod error;
mod tree;
mod wrappers;

pub use self::document::Document;
pub use self::error::{Error, ErrorKind, PathSegment};
pub use self::wrappers::Spanned;

pub(crate) use self::engine::{
    Doc, DocDe, Item, TAG_BLOCK, TAG_CALC, TAG_FIELD, TAG_HEADERED, TAG_PARAMETER, TAG_QUOTED,
    TAG_UNQUOTED, TAG_VALUE, TAG_VARIABLE, VALUE, ValueDe, View,
};

use self::tree::{TreeCtx, TreeDoc};
use super::{Flavor, SyntaxErrorKind, SyntaxTree, parse_with};
use crate::{Encoding, Utf8Encoding, Windows1252Encoding};
use serde::de::Deserialize;

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
    Document::new(parse.tree(), encoding).deserialize()
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
/// errors of the tree. To find them, read
/// [`Parse::errors`](super::Parse::errors), or call [`Document::check`].
///
/// Use this function to read a file that has syntax errors, such as a file
/// that a person is changing in an editor. The parser recovers from the
/// errors, so the data before and after an error is available:
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::{parse, de::{Document, from_tree, from_utf8_slice}}};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Modifier {
///     tax: u32,
/// }
///
/// #[derive(Deserialize)]
/// struct Building {
///     cost: u32,
///     modifier: Modifier,
/// }
///
/// // The block has no closing brace.
/// let source = b"cost = 100\nmodifier = { tax = 1";
/// assert!(from_utf8_slice::<Building>(source).is_err());
///
/// let parse = parse(source);
/// assert_eq!(parse.errors()[0].message(), "unclosed '{'");
/// let building: Building = from_tree(parse.tree(), Utf8Encoding::new())?;
/// let error = Document::new(parse.tree(), Utf8Encoding::new()).check().unwrap_err();
/// assert_eq!(error.to_string(), "2:21: unclosed '{'");
/// assert_eq!((building.cost, building.modifier.tax), (100, 1));
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
pub fn from_tree<'a, T, E>(tree: SyntaxTree<'_, 'a>, encoding: E) -> Result<T, Error>
where
    T: Deserialize<'a>,
    E: Encoding,
{
    Document::new(tree, encoding).deserialize()
}

/// Read the root of a tree into an owned block.
pub(crate) fn tree_block<E: Encoding>(tree: SyntaxTree<'_, '_>, encoding: E) -> super::OwnedBlock {
    let ctx = TreeCtx::new(tree, encoding);
    let doc = TreeDoc::new(&ctx);
    super::value::ValueBuilder::new(doc).block(doc.root())
}

/// Add the file name to an error that has none.
pub(crate) fn with_file(error: Error, file: Option<&str>) -> Error {
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
