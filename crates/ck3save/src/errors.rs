use jomini::binary;
use std::io;

/// A Ck3 Error
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct Ck3Error(#[from] Box<Ck3ErrorKind>);

impl Ck3Error {
    pub(crate) fn new(kind: Ck3ErrorKind) -> Ck3Error {
        Ck3Error(Box::new(kind))
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &Ck3ErrorKind {
        &self.0
    }
}

impl From<Ck3ErrorKind> for Ck3Error {
    fn from(err: Ck3ErrorKind) -> Self {
        Ck3Error::new(err)
    }
}

/// Specific type of error
#[derive(thiserror::Error, Debug)]
pub enum Ck3ErrorKind {
    #[error("unable to deserialize due to: {0}")]
    Deserialize(#[source] jomini::Error),

    #[error("error while writing output: {0}")]
    Writer(#[source] jomini::Error),

    #[error("unknown binary token encountered: {token_id:#x}")]
    UnknownToken { token_id: u32 },

    #[error("file envelope error: {0}")]
    Envelope(#[from] jomini::envelope::EnvelopeError),

    #[error("parsing error: {0}")]
    Jomini(#[from] jomini::Error),

    #[error("invalid header")]
    InvalidHeader,

    #[error("expected the binary integer: {0} to be parsed as a date")]
    InvalidDate(i32),

    #[error("io error: {0}")]
    Io(#[from] io::Error),

    #[error("invalid syntax: {0}")]
    InvalidSyntax(String),
}

impl From<jomini::Error> for Ck3Error {
    fn from(value: jomini::Error) -> Self {
        if let jomini::ErrorKind::Deserialize(_) = value.kind() {
            let jomini::ErrorKind::Deserialize(x) = value.into_kind() else {
                unreachable!()
            };

            let kind = match x.kind() {
                &jomini::DeserializeErrorKind::UnknownToken { token_id } => {
                    Ck3ErrorKind::UnknownToken { token_id }
                }
                _ => Ck3ErrorKind::Deserialize(x.into()),
            };
            Ck3Error::new(kind)
        } else {
            Ck3Error::new(Ck3ErrorKind::Jomini(value))
        }
    }
}

impl From<io::Error> for Ck3Error {
    fn from(value: io::Error) -> Self {
        Ck3Error::from(Ck3ErrorKind::from(value))
    }
}

impl From<binary::ReaderError> for Ck3Error {
    fn from(value: binary::ReaderError) -> Self {
        Self::from(jomini::Error::from(value))
    }
}

impl From<jomini::envelope::EnvelopeError> for Ck3Error {
    fn from(value: jomini::envelope::EnvelopeError) -> Self {
        Ck3Error::from(Ck3ErrorKind::from(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_error_test() {
        assert_eq!(std::mem::size_of::<Ck3Error>(), 8);
    }
}
