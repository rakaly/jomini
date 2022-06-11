use crate::ScalarError;
use std::fmt;

/// An error that can occur when processing data
#[derive(Debug)]
pub struct Error(Box<ErrorKind>);

impl Error {
    pub(crate) fn new(kind: ErrorKind) -> Error {
        Error(Box::new(kind))
    }

    pub(crate) fn eof() -> Error {
        Self::new(ErrorKind::Eof)
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &ErrorKind {
        &self.0
    }

    /// Returns the byte offset that the error occurs (if available)
    pub fn offset(&self) -> Option<usize> {
        self.0.offset()
    }
}

/// Specific type of error
#[derive(Debug)]
pub enum ErrorKind {
    /// Unexpected end of input
    Eof,

    /// Too many close delimiters were encountered
    StackEmpty {
        /// The byte offset where the stack became empty
        offset: usize,
    },

    /// Expected a close delimiter after encountering an empty opener
    InvalidEmptyObject {
        /// The byte offset where the invalid empty object was encountered
        offset: usize,
    },

    /// Invalid syntax encountered
    InvalidSyntax {
        /// An error message describing the invalid syntax
        msg: String,

        /// The byte offset where the invalid syntax was encountered
        offset: usize,
    },

    /// An error occurred when deserializing the data
    Deserialize(DeserializeError),

    /// An error occurred when performing IO.
    Io(std::io::Error),
}

impl ErrorKind {
    /// The byte offset where the invalid syntax was encountered
    pub fn offset(&self) -> Option<usize> {
        match *self {
            ErrorKind::StackEmpty { offset, .. } => Some(offset),
            ErrorKind::InvalidEmptyObject { offset, .. } => Some(offset),
            ErrorKind::InvalidSyntax { offset, .. } => Some(offset),
            _ => None,
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match *self.0 {
            ErrorKind::Deserialize(ref err) => Some(err),
            ErrorKind::Io(ref err) => Some(err),
            _ => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            ErrorKind::Eof => write!(f, "unexpected end of file"),
            ErrorKind::StackEmpty { offset } => write!(f,
                "stack empty, too many close tokens encountered (offset: {})", offset
            ),
            ErrorKind::InvalidEmptyObject { offset } => write!(f,
                "expected first token after an empty object started to be a close group (offset: {})", offset
            ),
            ErrorKind::InvalidSyntax { ref msg, offset } => write!(f,
                "invalid syntax encountered: {} (offset: {})", msg, offset
            ),
            ErrorKind::Deserialize(ref err) => write!(f, "deserialize error: {}", err),
            ErrorKind::Io(ref err) => write!(f, "io error: {}", err),
        }
    }
}

impl From<DeserializeError> for Error {
    fn from(error: DeserializeError) -> Self {
        Error::new(ErrorKind::Deserialize(error))
    }
}

/// A Serde deserialization error.
#[derive(Debug, PartialEq)]
pub struct DeserializeError {
    pub(crate) kind: DeserializeErrorKind,
}

impl DeserializeError {
    /// Return the underlying error kind.
    pub fn kind(&self) -> &DeserializeErrorKind {
        &self.kind
    }
}

/// The type of a Serde deserialization error.
#[derive(Debug, PartialEq)]
pub enum DeserializeErrorKind {
    /// A generic Serde deserialization error
    Message(String),

    /// Requested serde operation is unsupported
    Unsupported(String),

    /// Error converting underlying data to desired format
    Scalar(ScalarError),

    /// An unknown binary token was encountered
    UnknownToken {
        /// The unknown 16bit token
        token_id: u16,
    },
}

impl std::error::Error for DeserializeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.kind {
            DeserializeErrorKind::Scalar(ref err) => Some(err),
            _ => None,
        }
    }
}

impl std::fmt::Display for DeserializeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            DeserializeErrorKind::Message(ref msg) => write!(f, "{}", msg),
            DeserializeErrorKind::Unsupported(ref msg) => {
                write!(f, "unsupported deserializer method: {}", msg)
            }
            DeserializeErrorKind::Scalar(ref e) => e.fmt(f),
            DeserializeErrorKind::UnknownToken { token_id } => {
                write!(f, "unknown binary token encountered (id: {})", token_id)
            }
        }
    }
}

#[cfg(feature = "serde")]
impl serde::de::Error for DeserializeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        DeserializeError {
            kind: DeserializeErrorKind::Message(msg.to_string()),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::new(ErrorKind::Io(error))
    }
}

impl From<ScalarError> for DeserializeError {
    fn from(error: ScalarError) -> Self {
        DeserializeError {
            kind: DeserializeErrorKind::Scalar(error),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Error;

    #[test]
    fn test_size_error_struct() {
        assert!(std::mem::size_of::<Error>() <= 8);
    }
}
