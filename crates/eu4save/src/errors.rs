use crate::file::Eu4FileEntryName;
use jomini::binary;
use std::{fmt, io};

/// An EU4 Error
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct Eu4Error(#[from] Box<Eu4ErrorKind>);

impl Eu4Error {
    pub(crate) fn new(kind: Eu4ErrorKind) -> Eu4Error {
        Eu4Error(Box::new(kind))
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &Eu4ErrorKind {
        &self.0
    }
}

impl From<Eu4ErrorKind> for Eu4Error {
    fn from(err: Eu4ErrorKind) -> Self {
        Eu4Error::new(err)
    }
}

/// Specific type of error
#[derive(thiserror::Error, Debug)]
pub enum Eu4ErrorKind {
    #[error("zip error: {0}")]
    Zip(#[from] rawzip::Error),

    #[error("unknown header found in zip entry. Must be EU4txt or EU4bin")]
    ZipHeader,

    #[error("unknown header found in file. Must be EU4txt, EU4bin, or a zip file.")]
    UnknownHeader,

    #[error("unrecognized zip compression method")]
    UnknownCompression,

    #[error("unable to parse due to: {0}")]
    Parse(#[source] jomini::Error),

    #[error("unable to deserialize due to: {msg}. This shouldn't occur as this is a deserializer wrapper")]
    DeserializeImpl { msg: String },

    #[error("unable to deserialize due to: {0}")]
    Deserialize(#[from] jomini::DeserializeError),

    #[error("unknown binary token encountered: {token_id:#x}")]
    UnknownToken { token_id: u32 },

    #[error("country tags must be 3 letters in length")]
    CountryTagIncorrectSize,

    #[error("country tags must contain only ascii letters")]
    CountryTagInvalidCharacters,

    #[error("expected the binary integer: {0} to be parsed as a date")]
    InvalidDate(i32),

    #[error("expected {0} file to exist within zip")]
    MissingFile(Eu4FileEntryName),

    #[error("io error: {0}")]
    Io(#[from] io::Error),

    #[error("invalid syntax: {0}")]
    InvalidSyntax(String),
}

impl From<jomini::Error> for Eu4Error {
    fn from(value: jomini::Error) -> Self {
        if let jomini::ErrorKind::Deserialize(_) = value.kind() {
            let jomini::ErrorKind::Deserialize(x) = value.into_kind() else {
                unreachable!("Error should be a deserialize error")
            };

            if let jomini::DeserializeErrorKind::UnknownToken { token_id } = x.kind() {
                Eu4Error::new(Eu4ErrorKind::UnknownToken {
                    token_id: *token_id,
                })
            } else {
                Eu4Error::new(Eu4ErrorKind::Deserialize(x))
            }
        } else {
            Eu4Error::new(Eu4ErrorKind::Parse(value))
        }
    }
}

impl From<io::Error> for Eu4Error {
    fn from(value: io::Error) -> Self {
        Eu4Error::from(Eu4ErrorKind::from(value))
    }
}

impl From<binary::ReaderError> for Eu4Error {
    fn from(value: binary::ReaderError) -> Self {
        Self::from(jomini::Error::from(value))
    }
}

impl serde::de::Error for Eu4Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Eu4Error::new(Eu4ErrorKind::DeserializeImpl {
            msg: msg.to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_error_test() {
        assert_eq!(std::mem::size_of::<Eu4Error>(), 8);
    }
}
