pub mod de;
pub mod document;
mod parser;
mod reader;
mod tape;
mod resolver;
mod errors;

pub use self::parser::{
    BinaryError, BinaryErrorKind, BinaryEvent, BinaryEventsIter, BinaryParseEvent, BinaryParser,
};
pub use self::reader::{BinaryReader, BinaryReaderBuilder, BinaryReadingError, EventRecord};
pub use self::tape::{BinTape, BinaryToken};
pub use self::resolver::{FailedResolveStrategy, TokenResolver};
pub use self::errors::BinaryDeError;
