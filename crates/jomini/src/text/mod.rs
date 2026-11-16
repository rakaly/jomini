//! Types for parsing clausewitz plaintext input
//!
//! To deserialize text with serde:
//!
//! | To deserialize | Use |
//! |---|---|
//! | saves, and any data that you only read | [`TextDeserializer::from_slice`] or [`TextDeserializer::from_reader`], or the shortcuts [`de::from_windows1252_slice`], [`de::from_utf8_slice`], [`de::from_windows1252_reader`], and [`de::from_utf8_reader`] |
#![cfg_attr(
    feature = "syntax",
    doc = "| game and mod files: errors with a line, a column, and the path of keys, `@variables`, the byte range of a value, files with syntax errors, and the syntax of game files that `TextDeserializer` gives an error for | [`syntax::parse`]`(data)`, then [`deserialize(encoding)`](syntax::Parse::deserialize) or [`document(encoding)`](syntax::Parse::document) |"
)]
#![cfg_attr(
    not(feature = "syntax"),
    doc = "| game and mod files: errors with a line, a column, and the path of keys, `@variables`, the byte range of a value, files with syntax errors, and the syntax of game files that `TextDeserializer` gives an error for | `jomini::text::syntax::parse(data)`, then `deserialize(encoding)` or `document(encoding)`, with the `syntax` feature |"
)]
//! | a [`TextTape`] that is already parsed | [`de::TextTapeDeserializer`] |
//!
//! [`TextDeserializer`] reads what the syntax parser,
//! `jomini::text::syntax::parse`, reads, with the same values, or gives an
//! error that names it, for the constructs of game files that it doesn't
//! read, such as the parameter blocks of script. It also reads data that
//! the syntax parser rejects: an operator that starts a field with an empty
//! key in the middle of a line, and a block that the end of the data leaves
//! open, which `syntax::parse(data).document(encoding)` reads too. And it
//! reads a few constructs of game files that no save has differently, since
//! telling them apart would cost work for each field: `?=` attached to its
//! key, as in `x.holder?= { }`; a key that a block follows without an
//! operator, as in `x { }`, first in a block or in a block without fields;
//! a block where a key is expected, after a block or a quoted scalar, or
//! first in a block, which is skipped; an operator right after a block,
//! as in `{ a = b }={ 1 2 3 }`; an operator at the start of a line after a
//! value in a sequence, which the syntax parser reads as a field with an
//! empty key; and a calculation that a block follows, as in `@[ x ] { }`.
//!
//! If the serde deserialization API is too high level, one can build
//! abstractions ontop of:
//!  - [TextTape::from_slice]: Realizes a pseudo AST onto
//!    a linear tape. Cleans up and normalizes data.
//!  - [TokenReader]: An incremental text lexer designed for handling large
//!    saves in a memory efficient manner.
//!
//! Some additional APIs are available to make working with a [TextTape] more
//! ergonomic for DOM-like use cases.
//! - [FieldGroupsIter]
//! - [FieldsIter]
//! - [ValuesIter]
//!
#![cfg_attr(
    feature = "serde",
    doc = r#"
A plain serde type does not keep the operator, the quotes, or the header of
a value. To keep them, wrap the type of the field in [`Property`],
[`MaybeQuoted`], or [`Headered`].
"#
)]
#![cfg_attr(
    feature = "syntax",
    doc = r#"
When you need positions, comments, or recovery from syntax errors in game and
mod files, use the **experimental** [`syntax`] module. It parses text into a
lossless tree that keeps every byte, so a tool can extract data, format,
edit, or check a file.
"#
)]

#[cfg(feature = "serde")]
pub mod de;
mod dom;
mod escape;
mod flavor;
mod fnv;
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
