//! Types for parsing clausewitz plaintext input
//!
//! Main text deserialization APIs:
//!  - [TextDeserializer::from_utf8_slice](crate::text::de::TextDeserializer::from_utf8_slice):
//!    Deserialize game and save files from a slice of data.
//!  - [TextDeserializer::from_utf8_reader](crate::text::de::TextDeserializer::from_utf8_reader):
//!    (**experimental**) much more memory efficient deserializer that is geared
//!    towards deserializing large models like those found in save files.  
//!
//! If the serde deserialization API is too high level, one can build
//! abstractions ontop of:
//!  - [TextTape::from_slice]: Realizes a pseudo AST onto
//!    a linear tape. Cleans up and normalizes data.
//!  - [TokenReader]: (**experimental** unlike the [binary
//!    equivalent](crate::binary::TokenReader)) an incremental text lexer
//!    designed for handling large saves in a memory efficient manner. It can
//!    lex game files, but the best API for exposing esoteric game file syntax
//!    has not yet been developed.
//!
//! Some additional APIs are available to make working with a [TextTape] more
//! ergonomic for DOM-like use cases.
//! - [FieldGroupsIter]
//! - [FieldsIter]
//! - [ValuesIter]

/// text deserialization
#[cfg(feature = "derive")]
pub mod de;
mod dom;
mod fnv;
mod operator;
mod reader;
mod tape;
mod writer;

#[cfg(feature = "derive")]
#[doc(inline)]
pub use self::de::Property;
pub use self::dom::{
    ArrayReader, FieldGroupsIter, FieldsIter, GroupEntry, GroupEntryIter, ObjectReader, Reader,
    ScalarReader, ValueReader, ValuesIter,
};
pub use self::operator::*;
pub use self::tape::{TextTape, TextTapeParser, TextToken};
pub use self::writer::*;
pub use reader::{ReaderError, ReaderErrorKind, Token, TokenReader, TokenReaderBuilder};
