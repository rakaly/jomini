//! Convert byte offsets to line and column numbers.

use super::{TextRange, TextSize};

/// A zero-based line and a zero-based byte column. See [`LineIndex`] for
/// the line breaks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineCol {
    /// The line, from 0.
    pub line: u32,
    /// The byte offset from the start of the line.
    pub col: u32,
}

impl std::fmt::Display for LineCol {
    /// Write the position as `line:col`, from 1, as error messages do.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

/// The start offset of each line of a source.
///
/// The ranges of the syntax tree are byte offsets. An error message or an
/// editor needs a line and a column instead. The index stores the start of each
/// line, so a conversion is a binary search. The index does not borrow the
/// source, so you can keep it next to an owned source or a cache.
///
/// A line ends after a `\n`, a `\r\n`, or a `\r` alone. The column counts bytes
/// from the start of the line. To get a UTF-16 column, decode the bytes of the
/// line up to the column with the encoding of the source, then count the UTF-16
/// code units.
///
/// ```
/// use jomini::text::lossless::{LineCol, LineIndex};
///
/// let source = b"a = 1\nb = \"x";
/// let index = LineIndex::new(source);
/// let tree = jomini::text::lossless::parse(source);
/// let error = &tree.errors()[0];
/// let at = index.line_col(error.range.start());
/// assert_eq!(at, LineCol { line: 1, col: 4 });
/// assert_eq!(at.to_string(), "2:5");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    /// The byte offset of the start of each line. The first value is 0.
    line_starts: Vec<u32>,
    /// One bit for each line that ends with `\r\n`. The vector ends at the
    /// last such line, so it is empty for a source without `\r\n`.
    crlf_lines: Vec<u64>,
    /// The length of the source.
    len: u32,
}

impl LineIndex {
    /// Make the index of `source`.
    ///
    /// # Panics
    ///
    /// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
    pub fn new(source: &[u8]) -> Self {
        let len = u32::try_from(source.len()).expect("source is larger than 4 GiB");
        let mut line_starts = vec![0];
        let mut crlf_lines: Vec<u64> = Vec::new();
        let mut i = 0;
        while i < source.len() {
            match source[i] {
                b'\n' => line_starts.push(i as u32 + 1),
                b'\r' if source.get(i + 1) == Some(&b'\n') => {
                    let line = line_starts.len() - 1;
                    if crlf_lines.len() <= line / 64 {
                        crlf_lines.resize(line / 64 + 1, 0);
                    }
                    crlf_lines[line / 64] |= 1 << (line % 64);
                    i += 1;
                    line_starts.push(i as u32 + 1);
                }
                b'\r' => line_starts.push(i as u32 + 1),
                _ => {}
            }
            i += 1;
        }
        LineIndex {
            line_starts,
            crlf_lines,
            len,
        }
    }

    /// The number of lines. An empty source has one line.
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// The line and column of `offset`. An offset after the end of the
    /// source gives the position of the end.
    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let offset = offset.to_u32().min(self.len);
        let line = self.line_starts.partition_point(|&start| start <= offset) - 1;
        LineCol {
            line: line as u32,
            col: offset - self.line_starts[line],
        }
    }

    /// The byte offset of `at`. Return `None` when the line does not exist or
    /// the column is after the end of the line. The column of the end of the
    /// source is valid, but the column after a line break is not.
    pub fn offset(&self, at: LineCol) -> Option<TextSize> {
        let range = self.line_range(at.line)?;
        let offset = range.start.checked_add(at.col)?;
        // The end of the source is a column of the last line only.
        (offset < range.end || (offset == self.len && at.line + 1 == self.line_count()))
            .then_some(TextSize::new(offset))
    }

    /// The range of `line`, with its line break. Return `None` when the line
    /// does not exist.
    pub fn line_range(&self, line: u32) -> Option<TextRange> {
        let start = *self.line_starts.get(line as usize)?;
        let end = self
            .line_starts
            .get(line as usize + 1)
            .copied()
            .unwrap_or(self.len);
        Some(TextRange::span(start, end))
    }

    /// The range of `line`, without its line break. Return `None` when the
    /// line does not exist.
    ///
    /// ```
    /// use jomini::text::lossless::LineIndex;
    ///
    /// let source = b"a = 1\r\nb = 2";
    /// let index = LineIndex::new(source);
    /// let line = index.line_text_range(0).unwrap();
    /// assert_eq!(&source[line], b"a = 1");
    /// ```
    pub fn line_text_range(&self, line: u32) -> Option<TextRange> {
        let range = self.line_range(line)?;
        // Each line but the last ends with a line break.
        let line_break = if line + 1 == self.line_count() {
            0
        } else if self.ends_with_crlf(line) {
            2
        } else {
            1
        };
        Some(TextRange::span(range.start, range.end - line_break))
    }

    fn ends_with_crlf(&self, line: u32) -> bool {
        let line = line as usize;
        self.crlf_lines
            .get(line / 64)
            .is_some_and(|bits| bits >> (line % 64) & 1 == 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn column_after_final_line_break_is_not_valid() {
        let index = LineIndex::new(b"ab\n");
        assert_eq!(index.offset(LineCol { line: 0, col: 3 }), None);
        assert_eq!(
            index.offset(LineCol { line: 1, col: 0 }),
            Some(TextSize::new(3))
        );
    }

    #[test]
    fn line_col_round_trip() {
        let source = b"a = 1\r\nb = 2\n\nc";
        let index = LineIndex::new(source);
        assert_eq!(index.line_count(), 4);
        for offset in 0..=source.len() as u32 {
            let at = index.line_col(TextSize::new(offset));
            assert_eq!(
                index.offset(at),
                Some(TextSize::new(offset)),
                "{offset} {at:?}"
            );
        }
        assert_eq!(
            index.line_col(TextSize::new(0)),
            LineCol { line: 0, col: 0 }
        );
        assert_eq!(
            index.line_col(TextSize::new(6)),
            LineCol { line: 0, col: 6 }
        ); // the `\n`
        assert_eq!(
            index.line_col(TextSize::new(7)),
            LineCol { line: 1, col: 0 }
        );
        assert_eq!(
            index.line_col(TextSize::new(14)),
            LineCol { line: 3, col: 0 }
        );
        assert_eq!(
            index.line_col(TextSize::new(15)),
            LineCol { line: 3, col: 1 }
        );
        assert_eq!(
            index.line_col(TextSize::new(100)),
            LineCol { line: 3, col: 1 }
        );
    }

    #[test]
    fn line_ranges() {
        let index = LineIndex::new(b"ab\ncd");
        assert_eq!(index.line_range(0), Some(TextRange::span(0, 3)));
        assert_eq!(index.line_range(1), Some(TextRange::span(3, 5)));
        assert_eq!(index.line_range(2), None);
        assert_eq!(index.offset(LineCol { line: 0, col: 3 }), None);
        assert_eq!(index.offset(LineCol { line: 0, col: 4 }), None);
        // The end of the source is not on the first line.
        assert_eq!(index.offset(LineCol { line: 0, col: 5 }), None);
        assert_eq!(
            index.offset(LineCol { line: 1, col: 2 }),
            Some(TextSize::new(5))
        );
        assert_eq!(index.offset(LineCol { line: 2, col: 0 }), None);
    }

    #[test]
    fn line_text_ranges_leave_out_each_line_break() {
        let source = b"a\r\nbb\ncc\rd\r\n";
        let index = LineIndex::new(source);
        let lines: Vec<&[u8]> = (0..index.line_count())
            .map(|line| &source[index.line_text_range(line).unwrap()])
            .collect();
        assert_eq!(lines, [&b"a"[..], b"bb", b"cc", b"d", b""]);
        assert_eq!(index.line_text_range(5), None);
        assert_eq!(
            LineIndex::new(b"").line_text_range(0),
            Some(TextRange::span(0, 0))
        );
    }

    #[test]
    fn line_text_ranges_after_many_crlf_lines() {
        let source = b"x\r\n".repeat(70);
        let index = LineIndex::new(&source);
        assert_eq!(index.line_count(), 71);
        for line in 0..70 {
            let start = line * 3;
            assert_eq!(
                index.line_text_range(line),
                Some(TextRange::span(start, start + 1))
            );
        }
        assert_eq!(index.line_text_range(70), Some(TextRange::span(210, 210)));
    }

    #[test]
    fn lone_carriage_return() {
        let source = b"a = 1\rb = \"x";
        let index = LineIndex::new(source);
        assert_eq!(index.line_count(), 2);
        assert_eq!(
            index.line_col(TextSize::new(6)),
            LineCol { line: 1, col: 0 }
        );
        let tree = crate::text::lossless::parse(source);
        let at = index.line_col(tree.errors()[0].range.start());
        assert_eq!(at.to_string(), "2:5");
    }

    #[test]
    fn empty_source() {
        let index = LineIndex::new(b"");
        assert_eq!(index.line_count(), 1);
        assert_eq!(
            index.line_col(TextSize::new(0)),
            LineCol { line: 0, col: 0 }
        );
        assert_eq!(
            index.offset(LineCol { line: 0, col: 0 }),
            Some(TextSize::new(0))
        );
    }
}
