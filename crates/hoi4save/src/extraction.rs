/// Describes the format of the save before decoding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    /// Binary save
    Binary,

    /// Plaintext save
    Plaintext,
}
