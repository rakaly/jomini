//! Types for parsing clausewitz binary input
//!
//! Main binary deserialization APIs:
//!  - [BinaryFlavor::deserialize_slice]
//!  - [BinaryFlavor::deserialize_reader]
//!
//! If the serde deserialization API is too high level, one can build
//! abstractions ontop of.
//!  - [TokenReader]: An incremental binary lexer designed for handling large
//!    saves in a memory efficient manner.
//!  - [Lexer]: The lowest level, a zero cost binary data scanner over a byte
//!    slice.
//!
//! ## Direct identifier deserialization with `token` attribute
//!
//! There may be some performance loss during binary deserialization as
//! tokens are resolved to strings via a `TokenResolver` and then matched against the
//! string representations of a struct's fields.
//!
//! We can fix this issue by directly encoding the expected token value into the struct:
//!
//! ```rust
//! # #[cfg(feature = "derive")] {
//! # use jomini::{Encoding, JominiDeserialize, Windows1252Encoding, binary::BinaryFlavor};
//! # use std::{borrow::Cow, collections::HashMap};
//! #
//! # #[derive(Debug, Default)]
//! # pub struct BinaryTestFlavor;
//! #
//! # impl BinaryFlavor for BinaryTestFlavor {
//! #     fn visit_f32(&self, data: [u8; 4]) -> f32 {
//! #         f32::from_le_bytes(data)
//! #     }
//! #
//! #     fn visit_f64(&self, data: [u8; 8]) -> f64 {
//! #         f64::from_le_bytes(data)
//! #     }
//! # }
//! #
//! # impl Encoding for BinaryTestFlavor {
//! #     fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
//! #         Windows1252Encoding::decode(data)
//! #     }
//! # }
//! #
//! # let data = [ 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47 ];
//! #
//! #[derive(JominiDeserialize, PartialEq, Debug)]
//! struct MyStruct {
//!     #[jomini(token = 0x2d82)]
//!     field1: String,
//! }
//!
//! // Empty token to string resolver
//! let map = HashMap::<u16, String>::new();
//!
//! let actual: MyStruct = BinaryTestFlavor.deserialize_slice(&data[..], &map)?;
//! assert_eq!(actual, MyStruct { field1: "ENG".to_string() });
//! # }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! Couple notes:
//!
//! - This does not obviate need for the token to string resolver as tokens may be used as values.
//! - If the `token` attribute is specified on one field on a struct, it must be specified on all fields of that struct.

/// binary deserialization
#[cfg(feature = "serde")]
pub mod de;
#[cfg(feature = "serde")]
pub use self::de::{BinaryDeserializer, BinaryDeserializerBuilder};

mod flavor;
mod lexer;
pub mod ng;
mod reader;
mod resolver;
mod rgb;

pub use self::flavor::BinaryFlavor;
pub use self::lexer::{LexError, LexemeId, Lexer, LexerError, Token, TokenKind};
pub use self::reader::{ReaderError, ReaderErrorKind, TokenReader, TokenReaderBuilder};
pub use self::resolver::{BasicTokenResolver, FailedResolveStrategy, TokenResolver};
pub use self::rgb::*;
