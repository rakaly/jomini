//! Convert byte offsets to line and column numbers.
//!
//! The ranges of the syntax tree are byte offsets. A person, an error message,
//! or an editor needs a line and a column. A [`LineIndex`] stores the start of
//! each line, so a conversion is a binary search.
//!
//! A line ends after a `\n`, so `\r\n` also ends a line. The column counts
//! bytes from the start of the line. An editor that counts UTF-16 code units
//! can convert the bytes of the line before the column.

use super::TextRange;

/// A zero-based line and a zero-based byte column.
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
/// The index does not borrow the source, so you can keep it next to an owned
/// source or a cache.
///
/// ```
/// use jomini::text::syntax::{LineCol, LineIndex};
///
/// let source = b"a = 1\nb = \"x";
/// let index = LineIndex::new(source);
/// let tree = jomini::text::syntax::parse(source);
/// let error = &tree.errors()[0];
/// let at = index.line_col(error.range.start());
/// assert_eq!(at, LineCol { line: 1, col: 4 });
/// assert_eq!(at.to_string(), "2:5");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    /// The byte offset of the start of each line. The first value is 0.
    line_starts: Vec<u32>,
    /// The length of the source.
    len: u32,
}

impl LineIndex {
    /// Make the index of `source`.
    pub fn new(source: &[u8]) -> Self {
        let len = u32::try_from(source.len()).expect("source is larger than 4 GiB");
        let mut line_starts = vec![0];
        line_starts.extend(
            source
                .iter()
                .enumerate()
                .filter(|&(_, &b)| b == b'\n')
                .map(|(i, _)| i as u32 + 1),
        );
        LineIndex { line_starts, len }
    }

    /// The number of lines. An empty source has one line.
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// The line and column of `offset`. An offset after the end of the
    /// source gives the position of the end.
    pub fn line_col(&self, offset: u32) -> LineCol {
        let offset = offset.min(self.len);
        let line = self.line_starts.partition_point(|&start| start <= offset) - 1;
        LineCol {
            line: line as u32,
            col: offset - self.line_starts[line],
        }
    }

    /// The byte offset of `at`. Return `None` when the line does not exist or
    /// the column is after the end of the line.
    pub fn offset(&self, at: LineCol) -> Option<u32> {
        let range = self.line_range(at.line)?;
        let offset = range.start().checked_add(at.col)?;
        (offset <= range.end()).then_some(offset)
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
        Some(TextRange::new(start, end))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_col_round_trip() {
        let source = b"a = 1\r\nb = 2\n\nc";
        let index = LineIndex::new(source);
        assert_eq!(index.line_count(), 4);
        for offset in 0..=source.len() as u32 {
            let at = index.line_col(offset);
            assert_eq!(index.offset(at), Some(offset), "{offset} {at:?}");
        }
        assert_eq!(index.line_col(0), LineCol { line: 0, col: 0 });
        assert_eq!(index.line_col(6), LineCol { line: 0, col: 6 }); // the `\n`
        assert_eq!(index.line_col(7), LineCol { line: 1, col: 0 });
        assert_eq!(index.line_col(14), LineCol { line: 3, col: 0 });
        assert_eq!(index.line_col(15), LineCol { line: 3, col: 1 });
        assert_eq!(index.line_col(100), LineCol { line: 3, col: 1 });
    }

    #[test]
    fn line_ranges() {
        let index = LineIndex::new(b"ab\ncd");
        assert_eq!(index.line_range(0), Some(TextRange::new(0, 3)));
        assert_eq!(index.line_range(1), Some(TextRange::new(3, 5)));
        assert_eq!(index.line_range(2), None);
        assert_eq!(index.offset(LineCol { line: 0, col: 4 }), None);
        assert_eq!(index.offset(LineCol { line: 2, col: 0 }), None);
    }

    #[test]
    fn empty_source() {
        let index = LineIndex::new(b"");
        assert_eq!(index.line_count(), 1);
        assert_eq!(index.line_col(0), LineCol { line: 0, col: 0 });
        assert_eq!(index.offset(LineCol { line: 0, col: 0 }), Some(0));
    }
}
