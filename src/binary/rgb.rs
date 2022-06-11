/// Extracted color info
///
/// This is only for the binary format. RGB values that are in plaintext are
/// behind a [TextToken::Header](crate::TextToken::Header) of `rgb`
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rgb {
    /// Red channel
    pub r: u32,

    /// Green channel
    pub g: u32,

    /// Blue channel
    pub b: u32,
}
