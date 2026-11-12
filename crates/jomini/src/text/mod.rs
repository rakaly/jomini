//! Types for parsing clausewitz plaintext input
//!
//! Main text deserialization APIs:
//!  - [TextDeserializer::from_utf8_slice](crate::text::de::TextDeserializer::from_utf8_slice):
//!    Deserialize game and save files from a slice of data.
//!  - [TextDeserializer::from_utf8_reader](crate::text::de::TextDeserializer::from_utf8_reader):
//!    Deserialize streaming data with lower memory use for large models. See
//!    [TokenReader] for details about the streaming parser.
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
    feature = "lossless",
    doc = r#"
When you need positions, comments, or recovery from syntax errors in game and
mod files, use the **experimental** [`lossless`] module. It parses text into a
tree that keeps every byte, so a tool can extract data, format, edit, or check
a file.
"#
)]

/// text deserialization
#[cfg(feature = "serde")]
pub mod de;
mod dom;
mod fnv;
#[cfg(feature = "lossless")]
pub mod lossless;
mod operator;
mod reader;
mod tape;
#[cfg(feature = "serde")]
mod wrappers;
mod writer;

#[cfg(feature = "serde")]
#[doc(inline)]
pub use self::de::Property;
pub use self::dom::{
    ArrayReader, FieldGroupsIter, FieldsIter, GroupEntry, GroupEntryIter, ObjectReader, Reader,
    ScalarReader, ValueReader, ValuesIter,
};
pub use self::operator::*;
pub use self::tape::{TextTape, TextTapeParser, TextToken};
#[cfg(feature = "serde")]
pub use self::wrappers::{Headered, MaybeQuoted};
pub use self::writer::*;
pub use crate::{ReaderError, ReaderErrorKind};
pub use reader::{Token, TokenKind, TokenReader};
