pub(crate) mod ascii;
mod binary;
mod core;
mod data;
pub mod de;
pub mod nommer;
mod scalar;
pub mod scratch;
mod text;

pub mod document;

pub use self::binary::{BinaryReader, BinaryReaderBuilder, BinaryReadingError, EventRecord};
pub use self::core::{
    BinaryError, BinaryErrorKind, BinaryEvent, BinaryEventsIter, BinaryParseEvent, BinaryParser,
};
pub use self::scalar::Scalar;
pub use self::text::{Operator, TextEvent, TextParser, TextParserState};
