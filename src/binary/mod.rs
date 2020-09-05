#[cfg(feature = "derive")]
mod de;
mod flavor;
mod resolver;
mod tape;

#[cfg(feature = "derive")]
pub use self::de::{BinaryDeserializer, BinaryDeserializerBuilder};
pub use self::flavor::{BinaryFlavor, DefaultFlavor};
pub use self::resolver::{FailedResolveStrategy, TokenResolver};
pub use self::tape::{BinaryTape, BinaryTapeParser, BinaryToken};
