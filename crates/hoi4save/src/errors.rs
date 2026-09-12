use std::{fmt, io};

use jomini::binary;

/// A Hoi4 Error
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct Hoi4Error(#[from] Box<Hoi4ErrorKind>);

impl Hoi4Error {
    pub(crate) fn new(kind: Hoi4ErrorKind) -> Hoi4Error {
        Hoi4Error(Box::new(kind))
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &Hoi4ErrorKind {
        &self.0
    }
}

impl From<Hoi4ErrorKind> for Hoi4Error {
    fn from(err: Hoi4ErrorKind) -> Self {
        Hoi4Error::new(err)
    }
}

/// Specific type of error
#[derive(thiserror::Error, Debug)]
pub enum Hoi4ErrorKind {
    #[error("unable to parse due to: {0}")]
    Parse(#[from] jomini::Error),

    #[error("unable to deserialize due to: {0}")]
    Deserialize(#[source] jomini::Error),

    #[error("unknown binary token encountered: {token_id:#x}")]
    UnknownToken { token_id: u16 },

    #[error("unable to deserialize due to: {msg}. This shouldn't occur as this is a deserializer wrapper")]
    DeserializeImpl { msg: String },

    #[error("expected the binary integer: {0} to be parsed as a date")]
    InvalidDate(i32),

    #[error("unknown header")]
    UnknownHeader,

    #[error("country tags must be 3 letters in length")]
    CountryTagIncorrectSize,

    #[error("country tags must contain only ascii letters")]
    CountryTagInvalidCharacters,

    #[error("unexpected end of file")]
    Eof,

    #[error("io error: {0}")]
    Io(#[from] io::Error),
}

impl serde::de::Error for Hoi4Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Hoi4Error::new(Hoi4ErrorKind::DeserializeImpl {
            msg: msg.to_string(),
        })
    }
}

impl From<io::Error> for Hoi4Error {
    fn from(value: io::Error) -> Self {
        Self::from(Hoi4ErrorKind::from(value))
    }
}

impl From<jomini::Error> for Hoi4Error {
    fn from(value: jomini::Error) -> Self {
        Self::from(Hoi4ErrorKind::from(value))
    }
}

impl From<binary::ReaderError> for Hoi4Error {
    fn from(value: binary::ReaderError) -> Self {
        Self::from(jomini::Error::from(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_error_test() {
        assert_eq!(std::mem::size_of::<Hoi4Error>(), 8);
    }
}
