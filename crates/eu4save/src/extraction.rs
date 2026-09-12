use std::fmt;

/// Describes the format of the save before decoding
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify), tsify(into_wasm_abi))]
#[cfg_attr(feature = "specta", derive(specta::Type))]
pub enum Encoding {
    /// Plaintext
    #[serde(rename = "text")]
    Text,

    /// Plaintext documents within a zip file
    #[serde(rename = "textzip")]
    TextZip,

    /// Binary documents within a zip file
    #[serde(rename = "binzip")]
    BinaryZip,

    /// Binary
    #[serde(rename = "binary")]
    Binary,
}

impl Encoding {
    pub fn as_str(&self) -> &'static str {
        match self {
            Encoding::Text => "text",
            Encoding::TextZip => "textzip",
            Encoding::BinaryZip => "binzip",
            Encoding::Binary => "binary",
        }
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Encoding::BinaryZip | Encoding::Binary)
    }

    pub fn is_text(&self) -> bool {
        matches!(self, Encoding::TextZip | Encoding::Text)
    }

    pub fn is_zip(&self) -> bool {
        matches!(self, Encoding::BinaryZip | Encoding::TextZip)
    }

    pub fn zipify(&self) -> Encoding {
        match self {
            Encoding::Text | Encoding::TextZip => Encoding::TextZip,
            Encoding::BinaryZip | Encoding::Binary => Encoding::BinaryZip,
        }
    }
}

impl fmt::Display for Encoding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
