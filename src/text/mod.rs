pub mod de;
mod errors;
mod parser;
mod tape;

pub use self::errors::{TextError, TextErrorKind};
pub use self::parser::{TextEvent, TextParser, TextParserState};
pub use self::tape::{TextTape, TextToken};
