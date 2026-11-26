use crate::{
    ParserError, ScalarError,
    binary::{LexError, LexerError},
};
use std::{fmt, mem::MaybeUninit};

/// An error that can occur when processing data
pub struct Error(Box<ErrorImpl>);

struct ErrorImpl {
    kind: ErrorKind,

    /// The byte offset of an error whose kind has none.
    offset: Option<usize>,
}

impl Error {
    pub(crate) fn new(kind: ErrorKind) -> Error {
        Error(Box::new(ErrorImpl { kind, offset: None }))
    }

    /// Set the offset of an error that has none, so that the most specific
    /// offset, set closest to where the error was raised, is kept.
    #[cold]
    pub(crate) fn or_offset(mut self, offset: usize) -> Error {
        if self.offset().is_none() {
            self.0.offset = Some(offset);
        }
        self
    }

    /// Swap in `full`, the error of the reader that this error stands for,
    /// and locate it at the position the reader error was returned at if it
    /// has no offset. An I/O error stands for a reader error of the same kind,
    /// and an end of the data for a reader error of
    /// [`std::io::ErrorKind::UnexpectedEof`], as a reader error is never a
    /// clean end. Any other error is kept, as the reader error was then
    /// handled before it.
    #[cfg(feature = "serde")]
    #[cold]
    #[inline(never)]
    pub(crate) fn restore_io(mut self, full: Option<(std::io::Error, usize)>) -> Error {
        let Some((full, position)) = full else {
            return self;
        };
        let stands_for = match &self.0.kind {
            ErrorKind::Io(kind_only) => kind_only.kind() == full.kind(),
            ErrorKind::Eof => full.kind() == std::io::ErrorKind::UnexpectedEof,
            _ => false,
        };
        if !stands_for {
            return self;
        }
        self.0.kind = ErrorKind::Io(full);
        self.or_offset(position)
    }

    #[inline(never)]
    #[cold]
    pub(crate) fn eof() -> Error {
        Self::new(ErrorKind::Eof)
    }

    #[cold]
    pub(crate) fn invalid_syntax<T>(msg: T, position: usize) -> Error
    where
        T: Into<String>,
    {
        Self::new(ErrorKind::InvalidSyntax {
            msg: msg.into(),
            offset: position,
        })
    }

    pub(crate) fn deserialize(kind: DeserializeErrorKind) -> Self {
        Self::new(ErrorKind::Deserialize(DeserializeError { kind }))
    }

    #[cfg(feature = "serde")]
    pub(crate) fn deserialize_msg(msg: impl Into<Box<str>>) -> Self {
        Self::deserialize(DeserializeErrorKind::Message(msg.into()))
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// Unwrap this error into its underlying type
    ///
    /// The kind does not keep every offset that [`offset`](Self::offset)
    /// returns, so read the offset first when it is needed.
    pub fn into_kind(self) -> ErrorKind {
        self.0.kind
    }

    /// Returns the byte offset that the error occurs (if available)
    pub fn offset(&self) -> Option<usize> {
        self.0.kind.offset().or(self.0.offset)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0.offset {
            Some(offset) => f
                .debug_struct("Error")
                .field("kind", &self.0.kind)
                .field("offset", &offset)
                .finish(),
            None => f.debug_tuple("Error").field(&self.0.kind).finish(),
        }
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

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferTooSmall,
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
        match self.0.kind {
            ErrorKind::Deserialize(ref err) => Some(err),
            ErrorKind::Io(ref err) => Some(err),
            _ => None,
        }
    }
}

/// An error whose kind has no offset is followed by the offset it was given.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.kind.fmt_kind(f)?;
        match self.0.offset {
            Some(offset) => write!(f, " (offset: {})", offset),
            None => Ok(()),
        }
    }
}

impl ErrorKind {
    fn fmt_kind(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorKind::Eof => write!(f, "unexpected end of file"),
            ErrorKind::StackEmpty { offset } => write!(
                f,
                "stack empty, too many close tokens encountered (offset: {})",
                offset
            ),
            ErrorKind::InvalidEmptyObject { offset } => write!(
                f,
                "expected first token after an empty object started to be a close group (offset: {})",
                offset
            ),
            ErrorKind::InvalidSyntax { ref msg, offset } => write!(
                f,
                "invalid syntax encountered: {} (offset: {})",
                msg, offset
            ),
            ErrorKind::Deserialize(ref err) => fmt::Display::fmt(err, f),
            ErrorKind::Io(ref err) => write!(f, "io error: {}", err),
            ErrorKind::BufferTooSmall => {
                write!(f, "token exceeds buffer capacity")
            }
        }
    }
}

impl From<DeserializeError> for Error {
    fn from(error: DeserializeError) -> Self {
        Error::new(ErrorKind::Deserialize(error))
    }
}

impl From<LexerError> for Error {
    fn from(value: LexerError) -> Self {
        match value.kind() {
            LexError::Eof => Error::eof(),
            _ => Error::new(ErrorKind::InvalidSyntax {
                msg: format!("{}", value.kind()),
                offset: value.position(),
            }),
        }
    }
}

/// The specific reader error kind, shared by text and binary [TokenReader](crate::text::TokenReader)s.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReaderErrorKind {
    /// An underlying error from a [Read](std::io::Read)er
    Read(std::io::ErrorKind),

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferTooSmall,

    /// An early end of the data encountered
    Eof,

    /// The binary data is corrupted (invalid RGB token)
    InvalidRgb,
}

/// A lexing error over a [Read](std::io::Read) implementation, returned by
/// text and binary [TokenReader](crate::text::TokenReader)s.
pub struct ReaderError {
    // MaybeUninit strips all inhabited niches from ReaderErrorKind, preventing
    // niche-optimization on a Result of a small type, like a token kind. Without it,
    // the compiler produces a niche-encoded 8-byte Result that requires extra
    // bit manipulation in hot loops (~2x instruction count regression).
    kind: MaybeUninit<ReaderErrorKind>,
    position: u32,
}

impl ReaderError {
    #[inline]
    pub(crate) fn new(position: usize, kind: ReaderErrorKind) -> Self {
        ReaderError {
            kind: MaybeUninit::new(kind),
            position: position.min(u32::MAX as usize) as u32,
        }
    }

    /// Return the byte position where the error occurred
    pub fn position(&self) -> usize {
        self.position as usize
    }

    /// Return the error kind
    pub fn kind(&self) -> ReaderErrorKind {
        // SAFETY: kind is always initialized via new()
        unsafe { self.kind.assume_init() }
    }

    /// Consume self and return the error kind
    #[must_use]
    pub fn into_kind(self) -> ReaderErrorKind {
        // SAFETY: kind is always initialized via new()
        unsafe { self.kind.assume_init() }
    }
}

impl fmt::Debug for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReaderError")
            .field("kind", &self.kind())
            .field("position", &self.position)
            .finish()
    }
}

impl fmt::Display for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            ReaderErrorKind::Read(kind) => {
                write!(
                    f,
                    "failed to read past position: {}: {}",
                    self.position(),
                    kind
                )
            }
            ReaderErrorKind::BufferTooSmall => {
                write!(
                    f,
                    "token exceeds buffer capacity at position: {}",
                    self.position()
                )
            }
            ReaderErrorKind::Eof => {
                write!(f, "unexpected end of file at position: {}", self.position())
            }
            ReaderErrorKind::InvalidRgb => {
                write!(f, "invalid rgb at position: {}", self.position())
            }
        }
    }
}

impl std::error::Error for ReaderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl From<ParserError> for Error {
    fn from(value: ParserError) -> Self {
        match value {
            ParserError::Io(std::io::ErrorKind::UnexpectedEof) => Error::eof(),
            ParserError::BufferTooSmall => Error::new(ErrorKind::BufferTooSmall),
            ParserError::Io(kind) => Error::new(ErrorKind::Io(std::io::Error::from(kind))),
        }
    }
}

impl From<ReaderError> for Error {
    fn from(value: ReaderError) -> Self {
        let pos = value.position();
        match value.into_kind() {
            ReaderErrorKind::Read(kind) => {
                Error::new(ErrorKind::Io(std::io::Error::from(kind))).or_offset(pos)
            }
            ReaderErrorKind::BufferTooSmall => Error::new(ErrorKind::BufferTooSmall).or_offset(pos),
            ReaderErrorKind::Eof => Error::eof().or_offset(pos),
            ReaderErrorKind::InvalidRgb => Error::invalid_syntax("invalid rgb", pos),
        }
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
    Message(Box<str>),

    /// Requested serde operation is unsupported
    Unsupported(&'static str),

    /// Error converting underlying data to desired format
    Scalar(ScalarError),

    /// An unknown binary token was encountered
    UnknownToken {
        /// The unknown 32bit token
        token_id: u32,
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
            DeserializeErrorKind::Unsupported(msg) => {
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
impl serde::de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::deserialize_msg(msg.to_string())
    }
}

#[cfg(feature = "serde")]
impl serde::de::Error for DeserializeError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        DeserializeError {
            kind: DeserializeErrorKind::Message(msg.to_string().into_boxed_str()),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::new(ErrorKind::Io(error))
    }
}

impl From<ScalarError> for Error {
    fn from(error: ScalarError) -> Self {
        Error::deserialize(DeserializeErrorKind::Scalar(error))
    }
}

#[cfg(test)]
mod tests {
    use super::{Error, ReaderError, ReaderErrorKind};

    #[test]
    fn test_size_error_struct() {
        assert!(std::mem::size_of::<Error>() <= 8);
    }

    #[test]
    fn reader_error_is_eight_bytes() {
        assert_eq!(std::mem::size_of::<ReaderError>(), 8);
        assert_eq!(
            ReaderError::new(123, ReaderErrorKind::Read(std::io::ErrorKind::Interrupted)).kind(),
            ReaderErrorKind::Read(std::io::ErrorKind::Interrupted)
        );
        // Verify large positions saturate safely
        assert_eq!(
            ReaderError::new(usize::MAX, ReaderErrorKind::Eof).position(),
            u32::MAX as usize
        );
    }

    #[cfg(feature = "serde")]
    #[test]
    fn restore_io_keeps_an_error_that_does_not_stand_for_the_reader_error() {
        use super::ErrorKind;
        use std::io;

        // A reader error of another kind was handled before this error.
        let kind_only = Error::new(ErrorKind::Io(io::ErrorKind::Other.into()));
        let full = io::Error::new(io::ErrorKind::InvalidData, "stale");
        let err = kind_only.restore_io(Some((full, 7)));
        assert_eq!(err.to_string(), "io error: other error");

        // Only a reader that ends early stands for an end of the data.
        let err = Error::eof().restore_io(Some((io::Error::other("stale"), 7)));
        assert!(matches!(err.kind(), ErrorKind::Eof));
        assert_eq!(err.offset(), None);
    }

    #[cfg(feature = "serde")]
    #[test]
    fn test_no_duplicate_deserialize_prefix() {
        use serde::de::Error as _;
        let inner = Error::custom("io error: something bad");
        let mid = Error::custom(inner.to_string());
        let outer = Error::custom(mid.to_string());
        assert_eq!(inner.to_string(), mid.to_string());
        assert_eq!(mid.to_string(), outer.to_string());
        assert_eq!(outer.to_string(), "io error: something bad");
    }
}
