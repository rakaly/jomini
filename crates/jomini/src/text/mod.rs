//! Types for parsing clausewitz plaintext input
//!
//! To deserialize text with serde:
//!
//! | To deserialize | Use |
//! |---|---|
//! | a slice or a reader | [`TextDeserializer::from_slice`] or [`TextDeserializer::from_reader`], or the shortcuts [`de::from_windows1252_slice`], [`de::from_utf8_slice`], [`de::from_windows1252_reader`], and [`de::from_utf8_reader`] |
//! | a [`TextTape`] that is already parsed | [`de::TextTapeDeserializer`] |
//!
//! [`TextDeserializer`] reads what the lossless parser,
//! `jomini::text::syntax::parse`, reads, or gives an error that names it,
//! for the constructs of game files that it doesn't read, such as
//! parameter blocks of script. Read those files with the lossless parser.
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
    feature = "syntax",
    doc = r#"
When you need positions, comments, or recovery from syntax errors in game and
mod files, use the **experimental** [`syntax`] module. It parses text into a
tree that keeps every byte, so a tool can extract data, format, edit, or check
a file.
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
pub use self::writer::*;
pub use crate::{ReaderError, ReaderErrorKind};
pub use reader::{Token, TokenKind, TokenReader};
