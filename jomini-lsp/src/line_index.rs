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

use jomini::text::lossless::{self, LineCol, TextSize};
use lsp_types::{Position, PositionEncodingKind};
use std::rc::Rc;

/// Which unit an LSP `character` column counts.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PositionEncoding {
    /// LSP's default: `character` counts UTF-16 code units.
    Utf16,
    /// The client supports it: `character` counts UTF-8 bytes, the same as a
    /// byte column.
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
    /// The lines that are not ASCII, in order. See [`non_ascii_lines`].
    non_ascii: Rc<[u32]>,
    encoding: PositionEncoding,
}

/// The lines of `text` that are not ASCII, in order. `lines` must be the line
/// index of `text`.
///
/// On an ASCII line, a UTF-16 column is the byte column. Thus a conversion
/// on an ASCII line does not examine the text of the line, also when the
/// line is very long.
pub fn non_ascii_lines(text: &[u8], lines: &lossless::LineIndex) -> Rc<[u32]> {
    if text.is_ascii() {
        return Rc::from([]);
    }
    (0..lines.line_count())
        .filter(|&line| {
            lines
                .line_text_range(line)
                .is_some_and(|range| !text[range.to_range()].is_ascii())
        })
        .collect()
}

impl<'a> LineIndex<'a> {
    /// `lines` must be the line index of `text`, and `non_ascii` must be the
    /// [`non_ascii_lines`] of `text`.
    pub fn new(
        text: &'a [u8],
        lines: &'a lossless::LineIndex,
        non_ascii: Rc<[u32]>,
        encoding: PositionEncoding,
    ) -> Self {
        LineIndex {
            text,
            lines,
            non_ascii,
            encoding,
        }
    }

    /// `true` when the UTF-16 columns of `line` are not its byte columns.
    fn needs_utf16_walk(&self, line: u32) -> bool {
        self.encoding == PositionEncoding::Utf16 && self.non_ascii.binary_search(&line).is_ok()
    }

    /// The [`Position`] of byte `offset` (clamped to the document end).
    pub fn position(&self, offset: TextSize) -> Position {
        let offset = offset.min(TextSize::of(self.text));
        let LineCol { line, col } = self.lines.line_col(offset);
        if !self.needs_utf16_walk(line) {
            return Position {
                line,
                character: col,
            };
        }
        let line_start = offset.to_usize() - col as usize;
        let character = encode_utf16_col(&self.text[line_start..offset.to_usize()]);
        Position { line, character }
    }

    /// The byte offset of `position` (clamped into range).
    pub fn offset(&self, position: Position) -> TextSize {
        // The range has no line break, so a column after the end of the text
        // clamps to the end of this line, not to the start of the next line.
        let Some(range) = self.lines.line_text_range(position.line) else {
            return TextSize::of(self.text);
        };
        let line_start = range.start().to_usize();
        let line = &self.text[range];
        let col_bytes = if self.needs_utf16_walk(position.line) {
            decode_utf16_col(line, position.character)
        } else {
            (position.character as usize).min(line.len())
        };
        TextSize::new((line_start + col_bytes) as u32)
    }

    /// The end-of-document position — the `end` of a whole-document range.
    pub fn end_position(&self) -> Position {
        self.position(TextSize::of(self.text))
    }
}

/// The number of UTF-16 code units in `slice`, which is the column at the end
/// of `slice` when `slice` starts at the line start.
fn encode_utf16_col(slice: &[u8]) -> u32 {
    // For non-UTF-8 bytes the replacement char is one UTF-16 unit, which
    // keeps columns sane for the ASCII-dominated content we target.
    String::from_utf8_lossy(slice)
        .chars()
        .map(|c| c.len_utf16() as u32)
        .sum()
}

/// The inverse of [`encode_utf16_col`]: the number of bytes in the first
/// `character` UTF-16 code units of `line`. The result is at most the length
/// of the line. A column inside a character moves to the end of that
/// character.
fn decode_utf16_col(line: &[u8], character: u32) -> usize {
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

#[cfg(test)]
mod tests {
    use super::*;

    fn view<'a>(
        text: &'a [u8],
        lines: &'a lossless::LineIndex,
        encoding: PositionEncoding,
    ) -> LineIndex<'a> {
        LineIndex::new(text, lines, non_ascii_lines(text, lines), encoding)
    }

    #[test]
    fn only_the_lines_that_are_not_ascii_are_listed() {
        let text = "a\né\rb\r\n😀\nc".as_bytes();
        let lines = lossless::LineIndex::new(text);
        assert_eq!(&*non_ascii_lines(text, &lines), [1, 3]);
        assert_eq!(
            &*non_ascii_lines(b"a\nb", &lossless::LineIndex::new(b"a\nb")),
            [0u32; 0]
        );
    }

    #[test]
    fn an_ascii_line_after_a_line_that_is_not_ascii_uses_byte_columns() {
        let text = "é\nabc".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = view(text, &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(TextSize::new(2)), pos(0, 1));
        assert_eq!(li.position(TextSize::new(5)), pos(1, 2));
        assert_eq!(li.offset(pos(1, 2)), TextSize::new(5));
        assert_eq!(li.offset(pos(1, 99)), TextSize::new(6));
    }

    fn pos(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn ascii_offsets_and_positions_round_trip() {
        let lines = lossless::LineIndex::new(b"abc\ndef");
        let li = view(b"abc\ndef", &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(TextSize::new(0)), pos(0, 0));
        assert_eq!(li.position(TextSize::new(3)), pos(0, 3)); // the '\n'
        assert_eq!(li.position(TextSize::new(4)), pos(1, 0)); // 'd'
        assert_eq!(li.position(TextSize::new(6)), pos(1, 2));
        assert_eq!(li.offset(pos(1, 0)), TextSize::new(4));
        assert_eq!(li.offset(pos(1, 2)), TextSize::new(6));
        // Past-end positions clamp.
        assert_eq!(li.offset(pos(99, 99)), TextSize::new(7));
        assert_eq!(li.position(TextSize::new(999)), pos(1, 3));
    }

    #[test]
    fn utf16_counts_code_units_not_bytes() {
        // "é" is 2 UTF-8 bytes but 1 UTF-16 unit.
        let text = "é = 1".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = view(text, &lines, PositionEncoding::Utf16);
        // After 'é' (byte offset 2) the UTF-16 column is 1.
        assert_eq!(li.position(TextSize::new(2)), pos(0, 1));
        assert_eq!(li.offset(pos(0, 1)), TextSize::new(2));
    }

    #[test]
    fn utf8_counts_bytes() {
        let text = "é = 1".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = view(text, &lines, PositionEncoding::Utf8);
        assert_eq!(li.position(TextSize::new(2)), pos(0, 2));
        assert_eq!(li.offset(pos(0, 2)), TextSize::new(2));
    }

    #[test]
    fn utf16_handles_astral_surrogate_pairs() {
        // "😀" is 4 UTF-8 bytes and 2 UTF-16 units.
        let text = "😀x".as_bytes();
        let lines = lossless::LineIndex::new(text);
        let li = view(text, &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(TextSize::new(4)), pos(0, 2)); // after the emoji
        assert_eq!(li.offset(pos(0, 2)), TextSize::new(4));
        assert_eq!(li.position(TextSize::new(5)), pos(0, 3)); // after 'x'
    }

    #[test]
    fn crlf_line_starts() {
        let lines = lossless::LineIndex::new(b"a\r\nb");
        let li = view(b"a\r\nb", &lines, PositionEncoding::Utf16);
        assert_eq!(li.position(TextSize::new(0)), pos(0, 0));
        assert_eq!(li.position(TextSize::new(1)), pos(0, 1)); // '\r' stays on line 0
        assert_eq!(li.position(TextSize::new(3)), pos(1, 0)); // 'b' on line 1
    }

    #[test]
    fn out_of_range_column_clamps_to_end_of_line_not_next_line() {
        // A column after the text of line 0 goes to byte 1, after `a`, not to
        // byte 3, the `b` of line 1.
        let lines = lossless::LineIndex::new(b"a\r\nb");
        let li = view(b"a\r\nb", &lines, PositionEncoding::Utf16);
        assert_eq!(li.offset(pos(0, 99)), TextSize::new(1));
        assert_eq!(li.offset(pos(0, 1)), TextSize::new(1)); // in-range columns are unchanged
        // A `\n` or a lone `\r` line break is the same.
        for text in [&b"abc\ndef"[..], b"abc\rdef"] {
            let lines = lossless::LineIndex::new(text);
            let li = view(text, &lines, PositionEncoding::Utf16);
            assert_eq!(li.offset(pos(0, 99)), TextSize::new(3), "{text:?}");
        }
    }
}
