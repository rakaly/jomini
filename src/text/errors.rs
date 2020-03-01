use crate::Scalar;
use serde::de;
use std::error;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum TextErrorKind {
    Eof,
    Separator(u8),
    Message(String),
}

#[derive(Debug)]
pub struct TextError {
    pub kind: TextErrorKind,
}

impl Display for TextError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.kind {
            TextErrorKind::Eof => write!(f, "unexpected end of file"),
            TextErrorKind::Separator(x) => {
                write!(f, "unexpected separator: {}", Scalar::new(&[x]).to_utf8())
            }
            TextErrorKind::Message(ref x) => write!(f, "text parsing error: {}", x),
        }
    }
}

impl error::Error for TextError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl de::Error for TextError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        TextError {
            kind: TextErrorKind::Message(msg.to_string()),
        }
    }
}
