use jomini::binary;
use std::io;

/// A Imperator Error
#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct ImperatorError(#[from] Box<ImperatorErrorKind>);

impl ImperatorError {
    pub(crate) fn new(kind: ImperatorErrorKind) -> ImperatorError {
        ImperatorError(Box::new(kind))
    }

    /// Return the specific type of error
    pub fn kind(&self) -> &ImperatorErrorKind {
        &self.0
    }
}

impl From<ImperatorErrorKind> for ImperatorError {
    fn from(err: ImperatorErrorKind) -> Self {
        ImperatorError::new(err)
    }
}

/// Specific type of error
#[derive(thiserror::Error, Debug)]
pub enum ImperatorErrorKind {
    #[error("unable to deserialize due to: {0}")]
    Deserialize(#[source] jomini::Error),

    #[error("file envelope error: {0}")]
    Envelope(#[from] jomini::envelope::EnvelopeError),

    #[error("parsing error: {0}")]
    Jomini(#[from] jomini::Error),

    #[error("error while writing output: {0}")]
    Writer(#[source] jomini::Error),

    #[error("unknown binary token encountered: {token_id:#x}")]
    UnknownToken { token_id: u32 },

    #[error("invalid header")]
    InvalidHeader,

    #[error("io error: {0}")]
    Io(#[from] io::Error),

    #[error("invalid syntax: {0}")]
    InvalidSyntax(String),

    #[error("expected the binary integer: {0} to be parsed as a date")]
    InvalidDate(i32),
}

impl From<jomini::Error> for ImperatorError {
    fn from(value: jomini::Error) -> Self {
        if let jomini::ErrorKind::Deserialize(_) = value.kind() {
            let jomini::ErrorKind::Deserialize(x) = value.into_kind() else {
                unreachable!()
            };

            let kind = match x.kind() {
                &jomini::DeserializeErrorKind::UnknownToken { token_id } => {
                    ImperatorErrorKind::UnknownToken { token_id }
                }
                _ => ImperatorErrorKind::Deserialize(x.into()),
            };
            ImperatorError::new(kind)
        } else {
            ImperatorError::new(ImperatorErrorKind::Jomini(value))
        }
    }
}

impl From<io::Error> for ImperatorError {
    fn from(value: io::Error) -> Self {
        ImperatorError::from(ImperatorErrorKind::from(value))
    }
}

impl From<binary::ReaderError> for ImperatorError {
    fn from(value: binary::ReaderError) -> Self {
        Self::from(jomini::Error::from(value))
    }
}

impl From<jomini::envelope::EnvelopeError> for ImperatorError {
    fn from(value: jomini::envelope::EnvelopeError) -> Self {
        ImperatorError::from(ImperatorErrorKind::from(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn size_of_error_test() {
        assert_eq!(std::mem::size_of::<ImperatorError>(), 8);
    }
}
