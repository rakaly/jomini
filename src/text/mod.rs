#[cfg(feature = "derive")]
pub mod de;
mod tape;

#[cfg(feature = "derive")]
pub use self::de::TextDeserializer;
pub use self::tape::{TextTape, TextToken};
