use std::error;
use std::fmt;

#[derive(Debug)]
pub enum BinaryDeError {
    UnresolvedToken(u16),
    StackExhausted,
    StackEmpty,
    EarlyEof,
    Message(String),
}

impl fmt::Display for BinaryDeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryDeError::StackExhausted => write!(f, "stack exhausted"),
            BinaryDeError::StackEmpty => write!(f, "stack empty"),
            BinaryDeError::UnresolvedToken(x) => write!(f, "unknown token encountered: 0x{:x}", x),
            _ => write!(f, "binary deserialization error"),
        }
    }
}

impl error::Error for BinaryDeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

#[cfg(feature = "serde")]
impl serde::de::Error for BinaryDeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        BinaryDeError::Message(msg.to_string())
    }
}
