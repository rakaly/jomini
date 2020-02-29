mod parser;
mod tape;

pub use self::parser::{TextEvent, TextParser, TextParserState};
pub use self::tape::{TextTape, TextToken};
