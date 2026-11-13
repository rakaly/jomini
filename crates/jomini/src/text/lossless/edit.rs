//! Changes to a source at the byte ranges of one parse.

use super::TextRange;
use std::fmt;

/// A change to a source: the bytes in `range` become `new_text`.
///
/// An insertion has an empty range, and a deletion has empty text. The text
/// is bytes, so it can hold Windows-1252 text.
///
/// The range is an offset in the source that you parsed. After one edit, the
/// ranges of the old tree do not match the new source. Thus collect all of
/// the edits from one parse, and give them to [`apply_edits`] together.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct TextEdit {
    /// The bytes of the source to replace.
    pub range: TextRange,
    /// The bytes that replace them.
    pub new_text: Vec<u8>,
}

impl TextEdit {
    /// An edit that replaces the bytes in `range` with `text`. The text can be
    /// a `&str`, a byte string, a `&[u8]`, or a `Vec<u8>`.
    #[must_use]
    pub fn new(range: TextRange, text: impl Into<Vec<u8>>) -> Self {
        TextEdit {
            range,
            new_text: text.into(),
        }
    }
}

/// The reason that [`apply_edits`] did not apply the edits.
///
/// The check does not find all stale ranges. A range from a source that an
/// earlier edit changed is usually in bounds, and then `apply_edits` changes
/// the wrong bytes. To prevent this, give all of the edits from one parse to
/// one call.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum EditError {
    /// The range of an edit goes past the end of the source.
    OutOfBounds {
        /// The range of the edit.
        range: TextRange,
        /// The length of the source.
        len: usize,
    },

    /// Two edits change the same bytes, such as an edit of a block and an
    /// edit of a field in that block. An insertion at the start or at the end
    /// of a different edit does not overlap it.
    Overlap {
        /// The range of the edit that starts first.
        first: TextRange,
        /// The range of the edit that starts in `first`.
        second: TextRange,
    },
}

impl fmt::Display for EditError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EditError::OutOfBounds { range, len } => write!(
                f,
                "the edit at {range} goes past the end of the source ({len} bytes)"
            ),
            EditError::Overlap { first, second } => {
                write!(f, "the edit at {second} overlaps the edit at {first}")
            }
        }
    }
}

impl std::error::Error for EditError {}

/// Apply `edits` to `source` in one pass, and return the new source.
///
/// `edits` can be a slice, an array, or a `Vec` of edits, or the `Option` that
/// [`Parse::closing_edit`](super::Parse::closing_edit) gives. Each range must
/// be an offset in `source`, so give all of the edits from one parse to one
/// call. The order of `edits` is not important, but two insertions at the
/// same offset stay in the order of `edits`.
///
/// ```
/// use jomini::text::lossless::{apply_edits, TextEdit, TextRange, TextSize};
///
/// let source = b"base_tax = 3";
/// let edits = [
///     TextEdit::new(TextRange::new(TextSize::new(11), TextSize::new(12)), "10"),
///     TextEdit::new(TextRange::empty(TextSize::new(0)), "# Changed\n"),
/// ];
/// assert_eq!(apply_edits(source, &edits)?, b"# Changed\nbase_tax = 10");
/// # Ok::<(), jomini::text::lossless::EditError>(())
/// ```
///
/// # Errors
///
/// Returns an error, and applies no edit, if two edits overlap or if a range
/// goes past the end of `source`. A stale range that is in bounds does not
/// give an error.
pub fn apply_edits<'e>(
    source: &[u8],
    edits: impl IntoIterator<Item = &'e TextEdit>,
) -> Result<Vec<u8>, EditError> {
    let mut sorted: Vec<&TextEdit> = edits.into_iter().collect();
    sorted.sort_by_key(|edit| (edit.range.start(), edit.range.end()));

    let added: usize = sorted.iter().map(|edit| edit.new_text.len()).sum();
    let mut out = Vec::with_capacity(source.len() + added);
    let mut previous: Option<TextRange> = None;
    for edit in sorted {
        let range = edit.range;
        if range.end().to_usize() > source.len() {
            return Err(EditError::OutOfBounds {
                range,
                len: source.len(),
            });
        }
        let copied = previous.map_or(0, |first| first.end().to_usize());
        if let Some(first) = previous
            && range.start() < first.end()
        {
            return Err(EditError::Overlap {
                first,
                second: range,
            });
        }
        out.extend_from_slice(&source[copied..range.start().to_usize()]);
        out.extend_from_slice(&edit.new_text);
        previous = Some(range);
    }
    let copied = previous.map_or(0, |last| last.end().to_usize());
    out.extend_from_slice(&source[copied..]);
    Ok(out)
}
