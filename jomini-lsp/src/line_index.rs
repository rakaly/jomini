//! Byte-offset ↔ LSP [`Position`] conversion.
//!
//! jomini's ranges are raw half-open **byte** offsets over `&[u8]`, and its
//! [`lossless::LineIndex`] finds the line and the byte column. LSP positions are
//! 0-based `(line, character)` where `character` counts UTF-16 code units (the
//! protocol default) or, when the client agrees during `initialize` negotiation,
//! UTF-8 bytes. This module converts the columns in both directions.
//!
//! **Encoding caveat.** We treat document bytes as UTF-8 — which is what a client
//! sends in a `textDocument/did{Open,Change}`. Game files on disk may be
//! Windows-1252; for the ASCII-dominated content of keys, numbers, and tags the
//! conversion is exact, and a fully encoding-aware mapping is a follow-up (the
//! lint layer punts on encoding the same way today).

use jomini::text::lossless::{self, LineCol};
use lsp_types::{Position, PositionEncodingKind};

/// Which unit an LSP `character` column counts.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PositionEncoding {
    /// LSP's default: `character` counts UTF-16 code units.
    Utf16,
    /// Negotiated when the client advertises it: `character` counts UTF-8 bytes,
    /// so the mapping is a straight subtraction (rust-analyzer prefers this).
    Utf8,
}

impl PositionEncoding {
    /// The matching LSP capability value to echo back in `initialize`.
    pub fn to_lsp(self) -> PositionEncodingKind {
        match self {
            PositionEncoding::Utf16 => PositionEncodingKind::UTF16,
            PositionEncoding::Utf8 => PositionEncodingKind::UTF8,
        }
    }
}

/// jomini's byte [`lossless::LineIndex`] for one document plus its bytes, to
/// convert byte columns to and from the columns of the negotiated encoding.
pub struct LineIndex<'a> {
    text: &'a [u8],
    lines: &'a lossless::LineIndex,
    encoding: PositionEncoding,
}

impl<'a> LineIndex<'a> {
    /// `lines` must be the line index of `text`.
    pub fn new(text: &'a [u8], lines: &'a lossless::LineIndex, encoding: PositionEncoding) -> Self {
        debug_assert_eq!(lines, &lossless::LineIndex::new(text));
        LineIndex {
            text,
            lines,
            encoding,
        }
    }

    /// The [`Position`] of byte `offset` (clamped to the document end).
    pub fn position(&self, offset: u32) -> Position {
        let offset = offset.min(self.text.len() as u32);
        let LineCol { line, col } = self.lines.line_col(offset);
        let line_start = (offset - col) as usize;
        let character = self.encode_col(&self.text[line_start..offset as usize]);
        Position { line, character }
    }

    /// The byte offset of `position` (clamped into range).
    pub fn offset(&self, position: Position) -> u32 {
        let Some(range) = self.lines.line_range(position.line) else {
            return self.text.len() as u32;
        };
        let line_start = range.start() as usize;
        let mut line_end = range.end() as usize;
        // Exclude the line terminator from the slice, so a column past the visible
        // content clamps to end-of-line rather than jumping onto the next line's
        // first byte (the rust-analyzer convention).
        if line_end > line_start && self.text[line_end - 1] == b'\n' {
            line_end -= 1;
            if line_end > line_start && self.text[line_end - 1] == b'\r' {
                line_end -= 1;
            }
        }
        let col_bytes = self.decode_col(&self.text[line_start..line_end], position.character);
        (line_start + col_bytes) as u32
    }

    /// The end-of-document position — the `end` of a whole-document range.
    pub fn end_position(&self) -> Position {
        self.position(self.text.len() as u32)
    }

    /// Code units (per the encoding) spanned by `slice` — i.e. the column of the
    /// offset at the end of `slice` within its line.
    fn encode_col(&self, slice: &[u8]) -> u32 {
        match self.encoding {
            PositionEncoding::Utf8 => slice.len() as u32,
            // For non-UTF-8 bytes the replacement char is one UTF-16 unit, which
            // keeps columns sane for the ASCII-dominated content we target.
            PositionEncoding::Utf16 => String::from_utf8_lossy(slice)
                .chars()
                .map(|c| c.len_utf16() as u32)
                .sum(),
        }
    }

    /// Inverse of [`encode_col`]: the byte length of the prefix of `line` spanning
    /// `character` code units (clamped to the line; a column landing inside a
    /// multi-unit char rounds up to that char's end).
    fn decode_col(&self, line: &[u8], character: u32) -> usize {
        match self.encoding {
            PositionEncoding::Utf8 => (character as usize).min(line.len()),
            PositionEncoding::Utf16 => {
                let text = String::from_utf8_lossy(line);
                let mut units = 0u32;
                let mut bytes = 0usize;
                for c in text.chars() {
                    if units >= character {
                        break;
                    }
                    units += c.len_utf16() as u32;
                    bytes += c.len_utf8();
                }
                bytes.min(line.len())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pos(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn ascii_offsets_and_positions_round_trip() {
        let lines = lossless::LineIndex::new(b"abc\ndef");
        let li = LineIndex::new(b"abc\ndef", &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(0), pos(0, 0));
        assert_eq!(li.position(3), pos(0, 3)); // the '\n'
        assert_eq!(li.position(4), pos(1, 0)); // 'd'
        assert_eq!(li.position(6), pos(1, 2));
        assert_eq!(li.offset(pos(1, 0)), 4);
        assert_eq!(li.offset(pos(1, 2)), 6);
        // Past-end positions clamp.
        assert_eq!(li.offset(pos(99, 99)), 7);
        assert_eq!(li.position(999), pos(1, 3));
    }

    #[test]
    fn utf16_counts_code_units_not_bytes() {
        // "é" is 2 UTF-8 bytes but 1 UTF-16 unit.
        let text = "é = 1".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = LineIndex::new(text, &lines, PositionEncoding::Utf16);
        // After 'é' (byte offset 2) the UTF-16 column is 1.
        assert_eq!(li.position(2), pos(0, 1));
        assert_eq!(li.offset(pos(0, 1)), 2);
    }

    #[test]
    fn utf8_counts_bytes() {
        let text = "é = 1".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = LineIndex::new(text, &lines, PositionEncoding::Utf8);
        assert_eq!(li.position(2), pos(0, 2));
        assert_eq!(li.offset(pos(0, 2)), 2);
    }

    #[test]
    fn utf16_handles_astral_surrogate_pairs() {
        // "😀" is 4 UTF-8 bytes and 2 UTF-16 units.
        let text = "😀x".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = LineIndex::new(text, &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(4), pos(0, 2)); // after the emoji
        assert_eq!(li.offset(pos(0, 2)), 4);
        assert_eq!(li.position(5), pos(0, 3)); // after 'x'
    }

    #[test]
    fn crlf_line_starts() {
        let lines = lossless::LineIndex::new(b"a\r\nb");
        let li = LineIndex::new(b"a\r\nb", &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(0), pos(0, 0));
        assert_eq!(li.position(1), pos(0, 1)); // '\r' stays on line 0
        assert_eq!(li.position(3), pos(1, 0)); // 'b' on line 1
    }

    #[test]
    fn out_of_range_column_clamps_to_end_of_line_not_next_line() {
        // A column past the visible content of line 0 must land at end-of-content
        // (byte 1, after 'a'), NOT at the start of line 1 (byte 3, the 'b'). The
        // line terminator (\r\n) is excluded from the clamp.
        let lines = lossless::LineIndex::new(b"a\r\nb");
        let li = LineIndex::new(b"a\r\nb", &lines, PositionEncoding::Utf16);
        assert_eq!(li.offset(pos(0, 99)), 1);
        assert_eq!(li.offset(pos(0, 1)), 1); // in-range columns are unchanged
        // A '\n'-only line behaves the same.
        let lines = lossless::LineIndex::new(b"abc\ndef");
        let li = LineIndex::new(b"abc\ndef", &lines, PositionEncoding::Utf16);
        assert_eq!(li.offset(pos(0, 99)), 3); // end of "abc", before '\n'
    }
}
