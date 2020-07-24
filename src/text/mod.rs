#[cfg(feature = "derive")]
pub mod de;
mod errors;
mod tape;

#[cfg(feature = "derive")]
pub use self::de::TextDeserializer;
pub use self::errors::{TextError, TextErrorKind};
pub use self::tape::{TextTape, TextToken};
