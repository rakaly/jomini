/// Error type for envelope operations
#[derive(Debug)]
pub struct EnvelopeError {
    kind: EnvelopeErrorKind,
}

impl EnvelopeError {
    /// Returns true if this error is due to a missing ZIP entry
    pub fn is_missing_entry(&self) -> bool {
        matches!(self.kind, EnvelopeErrorKind::ZipMissingEntry(_))
    }
}

impl From<EnvelopeErrorKind> for EnvelopeError {
    fn from(kind: EnvelopeErrorKind) -> Self {
        EnvelopeError { kind }
    }
}

impl From<std::io::Error> for EnvelopeError {
    fn from(error: std::io::Error) -> Self {
        EnvelopeError {
            kind: EnvelopeErrorKind::Io(error),
        }
    }
}

/// Specific kind of envelope error
#[derive(Debug)]
pub enum EnvelopeErrorKind {
    /// IO error during file operations
    Io(std::io::Error),
    /// Error from ZIP archive processing
    Zip(rawzip::Error),
    /// Invalid or corrupted save file header
    InvalidHeader,
    /// ZIP archive missing a requested entry
    ZipMissingEntry(String),
    /// ZIP entry uses unsupported compression method
    ZipUnsupportedCompression,
}

impl std::error::Error for EnvelopeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            EnvelopeErrorKind::Io(err) => Some(err),
            EnvelopeErrorKind::Zip(err) => Some(err),
            _ => None,
        }
    }
}

impl std::fmt::Display for EnvelopeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            EnvelopeErrorKind::Io(err) => write!(f, "IO error: {}", err),
            EnvelopeErrorKind::Zip(err) => write!(f, "Zip error: {}", err),
            EnvelopeErrorKind::InvalidHeader => write!(f, "Invalid header"),
            EnvelopeErrorKind::ZipMissingEntry(path) => write!(f, "Zip missing entry: {}", path),
            EnvelopeErrorKind::ZipUnsupportedCompression => {
                write!(f, "Zip unsupported compression method")
            }
        }
    }
}
