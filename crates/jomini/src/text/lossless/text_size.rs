//! Byte offsets and byte ranges in the source.

use std::fmt;

/// A byte offset in the source.
///
/// The offset is a `u32`, so a source must be smaller than 4 GiB. Use
/// [`TextSize::to_usize`] or `usize::from` to index the source.
///
/// ```
/// use jomini::text::lossless::TextSize;
///
/// let start = TextSize::new(4);
/// let end = start + TextSize::of(b"name");
/// assert_eq!(u32::from(end), 8);
/// assert_eq!(end.to_string(), "8");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct TextSize(pub(super) u32);

impl TextSize {
    /// Make an offset from a raw `u32`.
    #[inline]
    pub const fn new(offset: u32) -> Self {
        TextSize(offset)
    }

    /// The length of `bytes` as an offset.
    ///
    /// # Panics
    ///
    /// Panics if `bytes` is 4 GiB or larger.
    #[inline]
    pub fn of(bytes: &[u8]) -> Self {
        match u32::try_from(bytes.len()) {
            Ok(len) => TextSize(len),
            Err(_) => panic!("text is larger than 4 GiB"),
        }
    }

    /// The offset as a `u32`.
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self.0
    }

    /// The offset as a `usize`, to index the source.
    #[inline]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }

    /// Add `other`, or return `None` on overflow.
    #[inline]
    pub const fn checked_add(self, other: TextSize) -> Option<TextSize> {
        match self.0.checked_add(other.0) {
            Some(sum) => Some(TextSize(sum)),
            None => None,
        }
    }

    /// Subtract `other`, or return `None` when `other` is larger.
    #[inline]
    pub const fn checked_sub(self, other: TextSize) -> Option<TextSize> {
        match self.0.checked_sub(other.0) {
            Some(diff) => Some(TextSize(diff)),
            None => None,
        }
    }
}

impl fmt::Display for TextSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<u32> for TextSize {
    #[inline]
    fn from(offset: u32) -> Self {
        TextSize(offset)
    }
}

impl From<TextSize> for u32 {
    #[inline]
    fn from(size: TextSize) -> Self {
        size.0
    }
}

impl From<TextSize> for usize {
    #[inline]
    fn from(size: TextSize) -> Self {
        size.to_usize()
    }
}

impl TryFrom<usize> for TextSize {
    type Error = std::num::TryFromIntError;

    #[inline]
    fn try_from(offset: usize) -> Result<Self, Self::Error> {
        u32::try_from(offset).map(TextSize)
    }
}

impl std::ops::Add for TextSize {
    type Output = TextSize;

    /// # Panics
    ///
    /// Panics on overflow when debug assertions are on, the same as `u32`.
    #[inline]
    fn add(self, rhs: TextSize) -> TextSize {
        TextSize(self.0 + rhs.0)
    }
}

impl std::ops::Sub for TextSize {
    type Output = TextSize;

    #[inline]
    fn sub(self, rhs: TextSize) -> TextSize {
        TextSize(self.0 - rhs.0)
    }
}

impl std::ops::AddAssign for TextSize {
    #[inline]
    fn add_assign(&mut self, rhs: TextSize) {
        self.0 += rhs.0;
    }
}

impl std::ops::SubAssign for TextSize {
    #[inline]
    fn sub_assign(&mut self, rhs: TextSize) {
        self.0 -= rhs.0;
    }
}

/// A half-open byte range `start..end` in the source.
///
/// Index the source with the range to get its bytes:
///
/// ```
/// use jomini::text::lossless::{parse, TextRange, TextSize};
///
/// let source = b"name = Jean";
/// let parse = parse(source);
/// let key = parse.ast().fields().next().unwrap().key();
/// let range = key.text_range();
/// assert_eq!(range, TextRange::new(TextSize::new(0), TextSize::new(4)));
/// assert_eq!(&source[range], b"name");
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct TextRange {
    pub(super) start: u32,
    pub(super) end: u32,
}

impl TextRange {
    /// Make a range.
    ///
    /// # Panics
    ///
    /// Panics if `start` is greater than `end`.
    #[inline]
    pub const fn new(start: TextSize, end: TextSize) -> Self {
        assert!(start.0 <= end.0, "range start is greater than range end");
        TextRange {
            start: start.0,
            end: end.0,
        }
    }

    /// Make a range of `len` bytes at `offset`.
    #[inline]
    pub const fn at(offset: TextSize, len: TextSize) -> Self {
        TextRange::new(offset, TextSize(offset.0 + len.0))
    }

    /// Make a zero-width range at `offset`.
    #[inline]
    pub const fn empty(offset: TextSize) -> Self {
        TextRange {
            start: offset.0,
            end: offset.0,
        }
    }

    /// Make a range from raw offsets. The caller makes sure that
    /// `start <= end`.
    #[inline]
    pub(crate) const fn span(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        TextRange { start, end }
    }

    /// The first byte offset of the range.
    #[inline]
    pub const fn start(self) -> TextSize {
        TextSize(self.start)
    }

    /// The byte offset after the last byte of the range.
    #[inline]
    pub const fn end(self) -> TextSize {
        TextSize(self.end)
    }

    /// The number of bytes in the range.
    #[inline]
    pub const fn len(self) -> TextSize {
        TextSize(self.end - self.start)
    }

    /// Return `true` when the range has no bytes.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Return `true` when `offset` is in `start..end`.
    #[inline]
    pub const fn contains(self, offset: TextSize) -> bool {
        self.start <= offset.0 && offset.0 < self.end
    }

    /// Return `true` when `offset` is in `start..=end`.
    #[inline]
    pub const fn contains_inclusive(self, offset: TextSize) -> bool {
        self.start <= offset.0 && offset.0 <= self.end
    }

    /// Return `true` when `other` is fully inside this range.
    #[inline]
    pub const fn contains_range(self, other: TextRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// The smallest range that contains this range and `other`.
    ///
    /// ```
    /// use jomini::text::lossless::{TextRange, TextSize};
    ///
    /// let a = TextRange::at(TextSize::new(2), TextSize::new(3));
    /// let b = TextRange::empty(TextSize::new(9));
    /// assert_eq!(a.cover(b).to_string(), "2..9");
    /// ```
    #[inline]
    pub const fn cover(self, other: TextRange) -> TextRange {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end > other.end {
            self.end
        } else {
            other.end
        };
        TextRange { start, end }
    }

    /// The bytes that are in both ranges, or `None` when the ranges have no
    /// common offset. Two ranges that only touch give an empty range.
    #[inline]
    pub const fn intersect(self, other: TextRange) -> Option<TextRange> {
        let start = if self.start > other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end < other.end {
            self.end
        } else {
            other.end
        };
        if start <= end {
            Some(TextRange { start, end })
        } else {
            None
        }
    }

    /// The range as a `usize` range, to index the source.
    #[inline]
    pub const fn to_range(self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl From<TextRange> for std::ops::Range<usize> {
    #[inline]
    fn from(range: TextRange) -> Self {
        range.to_range()
    }
}

impl std::ops::Index<TextRange> for [u8] {
    type Output = [u8];

    #[inline]
    fn index(&self, range: TextRange) -> &[u8] {
        &self[range.to_range()]
    }
}

impl std::ops::Index<TextRange> for str {
    type Output = str;

    #[inline]
    fn index(&self, range: TextRange) -> &str {
        &self[range.to_range()]
    }
}

/// Write the range as `start..end`.
impl fmt::Display for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// Write the range as `start..end`, the same as [`Display`](fmt::Display).
impl fmt::Debug for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
