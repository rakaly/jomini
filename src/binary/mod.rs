//! Types for parsing clausewitz binary input
//!
//! Main binary deserialization APIs:
//!  - [BinaryFlavor::deserialize_slice]
//!  - [BinaryFlavor::deserialize_reader]
//!
//! If the serde deserialization API is too high level, one can build
//! abstractions ontop of.
//!  - [BinaryTape::from_slice]: Realizes a pseudo AST onto
//!    a linear tape. Cleans up and normalizes data.
//!  - [TokenReader]: an incremental binary lexer designed for handling large
//!    saves in a memory efficient manner.

/// binary deserialization
#[cfg(feature = "derive")]
pub mod de;
mod flavor;
mod lexer;
mod reader;
mod resolver;
mod rgb;
mod tape;

pub use self::flavor::BinaryFlavor;
pub use self::lexer::{LexError, LexemeId, Lexer, LexerError, Token};
pub use self::reader::{ReaderError, ReaderErrorKind, TokenReader, TokenReaderBuilder};
pub use self::resolver::{FailedResolveStrategy, TokenResolver};
pub use self::rgb::*;
pub use self::tape::{BinaryTape, BinaryTapeParser, BinaryToken};
