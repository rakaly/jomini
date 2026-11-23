use crate::{Error, ErrorKind};
use std::{
    fmt,
    io::{self, Read},
};

/// Bytes resolved from a span: borrowed for the lifetime of the input, or
/// copied into a buffer that lives only as long as the borrow of the source.
pub enum Reference<'de, 's> {
    /// Bytes of the input.
    Borrowed(&'de [u8]),

    /// Bytes in the source's buffer.
    Copied(&'s [u8]),
}

/// The bytes that a [`TextDeserializer`](super::TextDeserializer) reads: a
/// [`SliceSource`] or a [`ReaderSource`]. The trait can't be named outside
/// of the crate, so it can't be implemented there.
///
/// A source exposes a window of bytes that have not been consumed. Offsets
/// into the window stay valid until bytes are consumed: [`refill`] appends
/// to the window and keeps the bytes in it.
///
/// [`refill`]: Source::refill
pub trait Source<'de> {
    /// The bytes that have not been consumed.
    fn window(&self) -> &[u8];

    /// Drop the first `amt` bytes of the window.
    fn consume(&mut self, amt: usize);

    /// Append more data to the window, keeping the bytes in it. Returns
    /// `false` when there is no more data.
    fn refill(&mut self) -> Result<bool, Error>;

    /// Whether the window holds all of the data that remains.
    fn eof(&self) -> bool;

    /// Drop `window[from..to]`, keeping the bytes before and after it, so
    /// offsets before `from` stay valid. Only called when the window does
    /// not hold all of the data.
    fn remove(&mut self, from: usize, to: usize);

    /// The offset in the data of `window[at]`, for an `at` before the bytes
    /// that were removed since they were consumed.
    fn offset(&self, at: usize) -> usize;

    /// Keep the error of `refill` for a buffer that is too small.
    fn too_small(&mut self, err: &Error);

    /// The first error that was kept.
    fn error(&self) -> Option<Error>;

    /// The bytes of `window[start..end]`.
    fn resolve(&self, start: usize, end: usize) -> Reference<'de, '_>;
}

/// The source of a [`TextDeserializer`](super::TextDeserializer) that reads a slice,
/// whose bytes are borrowed for `'de`.
pub struct SliceSource<'de> {
    rest: &'de [u8],
    len: usize,
}

impl<'de> SliceSource<'de> {
    /// Read the bytes of `data`.
    #[inline]
    pub(crate) fn new(data: &'de [u8]) -> Self {
        SliceSource {
            rest: data,
            len: data.len(),
        }
    }
}

impl fmt::Debug for SliceSource<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SliceSource")
            .field("offset", &self.offset(0))
            .field("len", &self.len)
            .finish()
    }
}

impl<'de> Source<'de> for SliceSource<'de> {
    #[inline(always)]
    fn window(&self) -> &[u8] {
        self.rest
    }

    #[inline(always)]
    fn consume(&mut self, amt: usize) {
        self.rest = &self.rest[amt..];
    }

    #[inline(always)]
    fn refill(&mut self) -> Result<bool, Error> {
        Ok(false)
    }

    #[inline(always)]
    fn eof(&self) -> bool {
        true
    }

    fn remove(&mut self, _from: usize, _to: usize) {
        unreachable!("a slice holds all of its data");
    }

    fn offset(&self, at: usize) -> usize {
        self.len - self.rest.len() + at
    }

    #[inline(always)]
    fn too_small(&mut self, _err: &Error) {}

    #[inline(always)]
    fn error(&self) -> Option<Error> {
        None
    }

    #[inline(always)]
    fn resolve(&self, start: usize, end: usize) -> Reference<'de, '_> {
        Reference::Borrowed(&self.rest[start..end])
    }
}

/// The source of a [`TextDeserializer`](super::TextDeserializer) that reads from a
/// reader into a fixed size buffer.
///
/// A refill moves the window to the front of the buffer and reads after it,
/// so the window can't grow past the buffer: a token, and any lookahead past
/// it, must fit in the buffer.
pub struct ReaderSource<R> {
    reader: R,
    buf: Box<[u8]>,

    /// The window is `buf[start..end]`, and `start <= end <= buf.len()`.
    start: usize,
    end: usize,

    /// The number of bytes consumed before the front of the buffer.
    shifted: usize,
    eof: bool,

    /// The ranges of bytes removed from the window that bytes in it are
    /// before, as the offset in the data of the first byte removed and the
    /// number of bytes, in the order of the data. The bytes before a range
    /// were moved forward over it, so their offsets are lower than their
    /// place in the window gives by the bytes removed after them.
    holes: Vec<(usize, usize)>,

    /// The offset of the first error of a buffer that is too small.
    too_small: Option<usize>,
}

impl<R: Read> ReaderSource<R> {
    /// Read from `reader` with a 32 KiB buffer.
    pub(crate) fn new(reader: R) -> Self {
        Self::with_buf(reader, vec![0; 32 * 1024])
    }

    /// Read from `reader` into `buf`, whose length is the capacity.
    pub(crate) fn with_buf(reader: R, buf: Vec<u8>) -> Self {
        ReaderSource {
            reader,
            buf: buf.into_boxed_slice(),
            start: 0,
            end: 0,
            shifted: 0,
            eof: false,
            holes: Vec::new(),
            too_small: None,
        }
    }
}

impl<R> ReaderSource<R> {
    /// The offset in the data of the front of the window.
    #[inline]
    fn position(&self) -> usize {
        self.shifted + self.start
    }
}

impl<R> fmt::Debug for ReaderSource<R> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ReaderSource")
            .field("offset", &self.position())
            .field("capacity", &self.buf.len())
            .field("eof", &self.eof)
            .finish_non_exhaustive()
    }
}

impl<'de, R: Read> Source<'de> for ReaderSource<R> {
    #[inline(always)]
    #[allow(unsafe_code)]
    fn window(&self) -> &[u8] {
        debug_assert!(self.start <= self.end && self.end <= self.buf.len());
        // SAFETY: `start <= end <= buf.len()`: `consume` caps `start` at
        // `end`, and `refill` and `remove` keep both within the buffer.
        unsafe { self.buf.get_unchecked(self.start..self.end) }
    }

    /// Consuming more than the window holds empties it.
    #[inline(always)]
    fn consume(&mut self, amt: usize) {
        self.start = std::cmp::min(self.start + amt, self.end);
    }

    #[inline(never)]
    fn refill(&mut self) -> Result<bool, Error> {
        if self.eof {
            return Ok(false);
        }

        self.buf.copy_within(self.start..self.end, 0);
        self.shifted += self.start;
        self.end -= self.start;
        self.start = 0;
        if self.end == self.buf.len() {
            // The token or lookahead that doesn't fit starts the window.
            let err = Error::new(ErrorKind::BufferTooSmall);
            return Err(err.or_offset(self.offset(0)));
        }

        // Read until the buffer is full, so a token that spans refills is
        // scanned again a bounded number of times, however few bytes each
        // read returns.
        let before = self.end;
        loop {
            match self.reader.read(&mut self.buf[self.end..]) {
                Ok(0) => {
                    self.eof = true;
                    return Ok(self.end > before);
                }
                Ok(read) => {
                    // `window` relies on a reader not claiming to have read
                    // more than the buffer has room for.
                    assert!(read <= self.buf.len() - self.end);
                    self.end += read;
                    if self.end == self.buf.len() {
                        return Ok(true);
                    }
                }
                Err(e) if e.kind() == io::ErrorKind::Interrupted => {}
                Err(e) => {
                    let err = Error::new(ErrorKind::Io(e));
                    return Err(err.or_offset(self.position() + self.end));
                }
            }
        }
    }

    #[inline(always)]
    fn eof(&self) -> bool {
        self.eof
    }

    /// Moves the bytes before the range forward over it, and keeps the
    /// range, so that the offsets of the bytes before it stay exact.
    fn remove(&mut self, from: usize, to: usize) {
        assert!(from <= to && self.start + to <= self.end);
        if from == to {
            return;
        }

        // A range that no byte of the window is before no longer matters.
        let front = self.offset(0);
        self.holes.retain(|&(at, _)| at > front);

        let at = self.offset(from);
        self.buf
            .copy_within(self.start..self.start + from, self.start + to - from);
        self.start += to - from;
        let i = self.holes.partition_point(|&(x, _)| x < at);
        self.holes.insert(i, (at, to - from));
    }

    /// The place of a byte in the window is its offset in the data plus
    /// the lengths of the ranges removed after it.
    fn offset(&self, at: usize) -> usize {
        let place = self.position() + at;
        let mut after: usize = self.holes.iter().map(|&(_, len)| len).sum();
        for &(start, len) in &self.holes {
            if place.saturating_sub(after) < start {
                return place.saturating_sub(after);
            }
            after -= len;
        }
        place
    }

    #[cold]
    fn too_small(&mut self, err: &Error) {
        self.too_small = self.too_small.or(err.offset());
    }

    #[inline]
    fn error(&self) -> Option<Error> {
        let offset = self.too_small?;
        Some(Error::new(ErrorKind::BufferTooSmall).or_offset(offset))
    }

    #[inline(always)]
    fn resolve(&self, start: usize, end: usize) -> Reference<'de, '_> {
        Reference::Copied(&self.window()[start..end])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A reader that returns one byte per read.
    struct Trickle<'a>(&'a [u8]);

    impl Read for Trickle<'_> {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            match (self.0.split_first(), buf.first_mut()) {
                (Some((&x, rest)), Some(out)) => {
                    *out = x;
                    self.0 = rest;
                    Ok(1)
                }
                _ => Ok(0),
            }
        }
    }

    /// A refill reads until the buffer is full, however few bytes each read
    /// returns, and keeps the bytes that were not consumed.
    #[test]
    fn test_refill_keeps_window() {
        let mut source = ReaderSource::with_buf(Trickle(b"abcdef"), vec![0; 4]);
        assert!(Source::refill(&mut source).unwrap());
        assert_eq!(Source::window(&source), b"abcd");
        assert!(!Source::eof(&source));
        Source::consume(&mut source, 1);
        assert!(Source::refill(&mut source).unwrap());
        assert_eq!(Source::window(&source), b"bcde");
        let err = Source::refill(&mut source).unwrap_err();
        assert!(matches!(err.kind(), ErrorKind::BufferTooSmall));
        Source::consume(&mut source, 4);
        assert!(Source::refill(&mut source).unwrap());
        assert_eq!(Source::window(&source), b"f");
        assert!(Source::eof(&source));
        assert!(!Source::refill(&mut source).unwrap());
        assert_eq!(Source::offset(&source, 0), 5);
    }

    #[test]
    fn test_remove() {
        let mut source = ReaderSource::with_buf(&b"abcdefgh"[..], vec![0; 8]);
        assert!(Source::refill(&mut source).unwrap());
        Source::consume(&mut source, 1);
        Source::remove(&mut source, 2, 5);
        assert_eq!(Source::window(&source), b"bcgh");
        assert_eq!(Source::offset(&source, 1), 2);
        assert_eq!(Source::offset(&source, 2), 6);

        // A second removal at the same place adds to the removed bytes.
        Source::remove(&mut source, 2, 3);
        assert_eq!(Source::window(&source), b"bch");
        assert_eq!(Source::offset(&source, 0), 1);
        assert_eq!(Source::offset(&source, 2), 7);

        // Bytes after the range are consumed at their offset.
        Source::consume(&mut source, 2);
        assert_eq!(Source::window(&source), b"h");
        assert_eq!(Source::offset(&source, 0), 7);
        Source::remove(&mut source, 0, 1);
        assert_eq!(Source::window(&source), b"");
        assert_eq!(Source::offset(&source, 0), 8);
    }

    /// Ranges removed at different places, the later one first, keep the
    /// offsets of the bytes between them exact.
    #[test]
    fn test_remove_two_ranges() {
        let mut source = ReaderSource::with_buf(&b"abcdefgh"[..], vec![0; 8]);
        assert!(Source::refill(&mut source).unwrap());
        Source::remove(&mut source, 5, 6);
        assert_eq!(Source::window(&source), b"abcdegh");
        Source::remove(&mut source, 1, 3);
        assert_eq!(Source::window(&source), b"adegh");
        let offsets: Vec<usize> = (0..5).map(|at| Source::offset(&source, at)).collect();
        assert_eq!(offsets, [0, 3, 4, 6, 7]);

        Source::consume(&mut source, 2);
        assert_eq!(Source::window(&source), b"egh");
        let offsets: Vec<usize> = (0..3).map(|at| Source::offset(&source, at)).collect();
        assert_eq!(offsets, [4, 6, 7]);
    }
}
