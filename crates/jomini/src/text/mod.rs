//! Types for parsing Clausewitz text.
//!
//! # Choosing a text deserializer
//!
//! Enable the `serde` feature to deserialize into Rust types.
//!
//! | Input | Use |
//! |---|---|
#![cfg_attr(
    feature = "serde",
    doc = "| Saves and other data you only need to deserialize | [`TextDeserializer`] from a [slice](TextDeserializer::from_slice) or [reader](TextDeserializer::from_reader) |"
)]
#![cfg_attr(
    not(feature = "serde"),
    doc = "| Saves and other data you only need to deserialize | `TextDeserializer` from a slice or reader |"
)]
#![cfg_attr(
    all(feature = "syntax", feature = "serde"),
    doc = "| Game and mod files that need diagnostics, recovery, or script syntax | [`syntax::parse`], then [`deserialize(encoding)`](syntax::Parse::deserialize) or [`document(encoding)`](syntax::Parse::document) |"
)]
#![cfg_attr(
    feature = "serde",
    doc = "| An already parsed [`TextTape`] | [`de::TextTapeDeserializer`] |"
)]
//!
//! `TextDeserializer` reads as it deserializes, keeping memory use low and
//! avoiding the cost of building a tree. Enable the `syntax` feature for a
//! tree that keeps every byte for formatting and editing.
//! The tree also supports diagnostics with line, column, and key path,
//! error recovery, and variable resolution.
//!
//! ## Where the deserializers differ
//!
//! For input the syntax parser accepts, `TextDeserializer` yields the same
//! value as `syntax::parse(data).deserialize(encoding)` or an error that names
//! `jomini::text::syntax::parse`. This includes errors for parameter blocks
//! (`[[x] …]`), bracketed scalars (`[GetName]`), and reading an object as a sequence.
//!
//! Known exceptions to that contract:
//!
//! | Construct | `TextDeserializer` | `syntax` |
//! |---|---|---|
//! | Attached operator in `x.holder?= { }` | Reads `x.holder?` as the key | Reads `?=` as the operator |
//! | `x { }` as the first nonempty item or in a block without other fields | Reads a field or a headered block | Can read the scalar and block as separate items |
//! | Bare block where a key is expected | Skips the block | Keeps it as a bare value |
//! | Operator immediately after a block | Can use the block as the key | Reads an empty key or a bare value |
//! | Line-start operator after a value in a sequence | Reads the operator and value | Includes an empty key |
//! | Calculation followed by a block | Can use the calculation as the key | Keeps the calculation as a value |
//!
//! Streaming also accepts an empty key in the middle of a line and closes open
//! blocks at end of input. The tree reports syntax errors for these cases.
//! A recovering `document(encoding)` can still read the unclosed blocks.
//!
//! For lower-level access, use [`TokenReader`] to stream tokens or [`TextTape`]
//! for legacy tape readers.

#[cfg(feature = "serde")]
pub mod de;
mod dom;
mod escape;
mod flavor;
mod fnv;
#[cfg(feature = "syntax")]
pub mod lint;
mod operator;
mod reader;
#[cfg(feature = "syntax")]
pub mod syntax;
mod tape;
#[cfg(feature = "serde")]
mod wrappers;
mod writer;

#[cfg(feature = "serde")]
#[doc(inline)]
pub use self::de::{Property, TextDeserializer};
pub use self::dom::{
    ArrayReader, FieldGroupsIter, FieldsIter, GroupEntry, GroupEntryIter, ObjectReader, Reader,
    ScalarReader, ValueReader, ValuesIter,
};
pub(crate) use self::escape::decode_quoted;
pub use self::escape::unescape;
pub use self::flavor::TextFlavor;
pub use self::operator::*;
pub use self::tape::{TextTape, TextTapeParser, TextToken};
#[cfg(feature = "serde")]
pub use self::wrappers::{Headered, MaybeQuoted};
pub use self::writer::*;
pub use crate::{ReaderError, ReaderErrorKind};
pub use reader::{Token, TokenKind, TokenReader};
