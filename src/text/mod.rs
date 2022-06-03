//! Types for parsing clausewitz plaintext input
//!
//! See the top level module documentation for an overview that includes parsing
//! and deserializing text.
//!
//! For more examples of the mid-level DOM-like API, see
//! [FieldGroupsIter](crate::text::FieldGroupsIter),
//! [FieldsIter](crate::text::FieldsIter), and
//! [ValuesIter](crate::text::ValuesIter)
#[cfg(feature = "derive")]
mod de;
mod fnv;
mod operator;
mod reader;
mod tape;
mod writer;

#[cfg(feature = "derive")]
pub use self::de::TextDeserializer;
pub use self::operator::*;
pub use self::reader::{
    ArrayReader, FieldGroupsIter, FieldsIter, GroupEntry, GroupEntryIter, ObjectReader, Reader,
    ScalarReader, ValueReader, ValuesIter,
};
pub use self::tape::{TextTape, TextTapeParser, TextToken};
pub use self::writer::*;
