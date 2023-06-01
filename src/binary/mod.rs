//! Types for parsing clausewitz binary input
//!
//! See the top level module documentation for an overview that includes parsing
//! and deserializing binary data.

/// binary deserialization
#[cfg(feature = "derive")]
pub mod de;
mod flavor;
mod resolver;
mod rgb;
mod tape;
mod tokens;

pub use self::flavor::BinaryFlavor;
pub use self::resolver::{FailedResolveStrategy, TokenResolver};
pub use self::rgb::*;
pub use self::tape::{BinaryTape, BinaryTapeParser, BinaryToken};
