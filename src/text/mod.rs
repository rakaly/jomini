mod parser;
mod tape;

pub use self::tape::{TextTape, TextToken};
pub use self::parser::{TextEvent, TextParserState, TextParser};
