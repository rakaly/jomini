use crate::envelope::errors::{EnvelopeError, EnvelopeErrorKind};
use std::io::Write;

/// The kind of save file
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SaveHeaderKind {
    /// uncompressed text
    Text,

    /// uncompressed binary
    Binary,

    /// uncompressed text metadata header with compressed text gamestate
    UnifiedText,

    /// uncompressed binary metadata header with compressed binary gamestate
    UnifiedBinary,

    /// metadata is stored within the zip file
    SplitText,

    /// metadata is stored within the zip file
    SplitBinary,

    /// An unknown type
    Other(u16),
}

impl SaveHeaderKind {
    /// Creates a SaveHeaderKind from a numeric value
    pub fn new(kind: u16) -> SaveHeaderKind {
        match kind {
            0 => SaveHeaderKind::Text,
            1 => SaveHeaderKind::Binary,
            2 => SaveHeaderKind::UnifiedText,
            3 => SaveHeaderKind::UnifiedBinary,
            4 => SaveHeaderKind::SplitText,
            5 => SaveHeaderKind::SplitBinary,
            x => SaveHeaderKind::Other(x),
        }
    }

    /// Returns the numeric value of this header kind
    pub fn value(&self) -> u16 {
        match self {
            SaveHeaderKind::Text => 0,
            SaveHeaderKind::Binary => 1,
            SaveHeaderKind::UnifiedText => 2,
            SaveHeaderKind::UnifiedBinary => 3,
            SaveHeaderKind::SplitText => 4,
            SaveHeaderKind::SplitBinary => 5,
            SaveHeaderKind::Other(x) => *x,
        }
    }

    /// Returns true if this header kind indicates binary encoding
    pub fn is_binary(&self) -> bool {
        matches!(
            self,
            SaveHeaderKind::Binary | SaveHeaderKind::UnifiedBinary | SaveHeaderKind::SplitBinary
        )
    }

    /// Returns true if this header kind indicates text encoding
    pub fn is_text(&self) -> bool {
        matches!(
            self,
            SaveHeaderKind::Text | SaveHeaderKind::UnifiedText | SaveHeaderKind::SplitText
        )
    }
}

/// The first line of the save file
///
/// For a breakdown of the fields, see the PDX Unlimiter source:
///
/// <https://github.com/crschnick/pdx_unlimiter/blob/6363689c8a89a73bc5db4cca4eff249261807d38/pdxu-io/src/main/java/com/crschnick/pdxu/io/savegame/ModernHeader.java#L7-L25>
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SaveHeader {
    unknown: [u8; 2],
    kind: SaveHeaderKind,
    random: [u8; 8],
    meta_len: u64,
    header_len: usize,
}

impl SaveHeader {
    pub(crate) const SIZE: usize = 24;

    /// Creates a SaveHeader by parsing from a byte slice
    pub fn from_slice(data: &[u8]) -> Result<Self, EnvelopeError> {
        let data: &[u8; Self::SIZE] = data
            .first_chunk()
            .ok_or_else(|| EnvelopeError::from(EnvelopeErrorKind::InvalidHeader))?;

        if !matches!(&data[..3], [b'S', b'A', b'V']) {
            return Err(EnvelopeErrorKind::InvalidHeader.into());
        }

        // These try_into calls cannot fail because of the slice length check above
        let unknown = data[3..5].try_into().unwrap();
        let kind_hex =
            std::str::from_utf8(&data[5..7]).map_err(|_| EnvelopeErrorKind::InvalidHeader)?;
        let kind =
            u16::from_str_radix(kind_hex, 16).map_err(|_| EnvelopeErrorKind::InvalidHeader)?;
        let random = data[7..15].try_into().unwrap();

        let meta_hex =
            std::str::from_utf8(&data[15..23]).map_err(|_| EnvelopeErrorKind::InvalidHeader)?;
        let meta_len =
            u64::from_str_radix(meta_hex, 16).map_err(|_| EnvelopeErrorKind::InvalidHeader)?;

        let header_len = if data[23] == b'\r' && data.get(24) == Some(&b'\n') {
            25
        } else if data[23] == b'\n' {
            24
        } else {
            return Err(EnvelopeErrorKind::InvalidHeader.into());
        };

        Ok(SaveHeader {
            unknown,
            kind: SaveHeaderKind::new(kind),
            random,
            meta_len,
            header_len,
        })
    }

    /// Returns the save file kind (text/binary and compression info)
    pub fn kind(&self) -> SaveHeaderKind {
        self.kind
    }

    /// Sets the save file kind
    pub fn set_kind(&mut self, kind: SaveHeaderKind) {
        self.kind = kind;
    }

    /// Returns the length of the header line in bytes
    pub fn header_len(&self) -> usize {
        self.header_len
    }

    /// Returns the length of the metadata section in bytes
    pub fn metadata_len(&self) -> u64 {
        self.meta_len
    }

    /// Sets the metadata section length in bytes
    pub fn set_metadata_len(&mut self, len: u64) {
        self.meta_len = len
    }

    /// Writes the header to a writer in the save file format
    pub fn write<W>(&self, mut writer: W) -> std::io::Result<()>
    where
        W: Write,
    {
        writer.write_all(b"SAV")?;
        writer.write_all(&self.unknown)?;
        write!(writer, "{0:02x}", self.kind.value())?;
        writer.write_all(&self.random)?;
        write!(writer, "{0:08x}", self.meta_len)?;
        if self.header_len() == 25 {
            writer.write_all(b"\r")?;
        }
        writer.write_all(b"\n")?;
        Ok(())
    }
}

impl std::fmt::Display for SaveHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = Vec::new();
        self.write(&mut buf).map_err(|_| std::fmt::Error)?;
        let s = std::str::from_utf8(&buf).map_err(|_| std::fmt::Error)?;
        f.write_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_save_header() {
        let data = b"SAV0102a40f789f000067c4\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();

        assert_eq!(header.kind(), SaveHeaderKind::UnifiedText);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 26564);

        let mut out = Vec::new();
        header.write(&mut out).unwrap();

        assert_eq!(&out, data);
    }

    #[test]
    fn test_save_header_allow_crlf() {
        let data = b"SAV0102a40f789f000067c4\r\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();

        assert_eq!(header.kind(), SaveHeaderKind::UnifiedText);
        assert_eq!(header.header_len(), 25);
        assert_eq!(header.metadata_len(), 26564);

        let mut out = Vec::new();
        header.write(&mut out).unwrap();

        assert_eq!(&out, b"SAV0102a40f789f000067c4\r\n");
    }

    #[test]
    fn test_split_vic3() {
        // Vic3 save where the metadata is stored within the zip file
        let data = b"SAV010580b859da00000000\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();

        assert_eq!(header.kind(), SaveHeaderKind::SplitBinary);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 0);
    }

    #[test]
    fn test_debug_vic3() {
        // Vic3 save where everything is plain text
        let data = b"SAV010078544999000003bc\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();

        assert_eq!(header.kind(), SaveHeaderKind::Text);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 956);
    }

    #[test]
    fn test_eu5_ironman_header() {
        // EU5 ironman save where the binary header is precedes the zip
        let data = b"SAV0103daabb23800062a40\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();
        assert_eq!(header.kind(), SaveHeaderKind::UnifiedBinary);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 404032);
    }

    #[test]
    fn test_eu5_debug_header() {
        // EU5 debug save where everything is plaintext, no zip
        let data = b"SAV0100797de2430004dc53\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();
        assert_eq!(header.kind(), SaveHeaderKind::Text);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 318547);
    }

    #[test]
    fn test_ck3_binary_autosave_header() {
        // CK3 binary autosave
        let data = b"SAV0101ad23696300004c29\n";
        let header = SaveHeader::from_slice(&data[..]).unwrap();
        assert_eq!(header.kind(), SaveHeaderKind::Binary);
        assert_eq!(header.header_len(), 24);
        assert_eq!(header.metadata_len(), 19497);
    }
}
