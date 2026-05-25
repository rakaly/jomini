use std::io::{self, Read};
use std::marker::PhantomData;
use std::ptr;

use crate::binary::LexemeId;

const DEFAULT_CAPACITY: usize = 32 * 1024;

/// Error returned by [`ParserSource`] operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParserError {
    /// The streaming refill buffer is too small to hold the requested block.
    BufferTooSmall,

    /// An underlying reader returned an IO error, including
    /// [`io::ErrorKind::UnexpectedEof`] when the source ended prematurely.
    Io(io::ErrorKind),
}

impl ParserError {
    /// Constructs a [`ParserError`] representing unexpected end-of-stream.
    #[inline]
    pub fn eof() -> Self {
        ParserError::Io(io::ErrorKind::UnexpectedEof)
    }

    /// Returns true if this error indicates unexpected end-of-stream.
    #[inline]
    pub fn is_eof(&self) -> bool {
        matches!(self, ParserError::Io(io::ErrorKind::UnexpectedEof))
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::BufferTooSmall => write!(f, "requested read exceeds buffer capacity"),
            ParserError::Io(kind) => write!(f, "io error: {}", kind),
        }
    }
}

impl std::error::Error for ParserError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl From<io::Error> for ParserError {
    fn from(value: io::Error) -> Self {
        ParserError::Io(value.kind())
    }
}

enum Backing<'a> {
    Stream {
        buffer: Vec<u8>,
        source: Box<dyn Read + 'a>,
    },
    Owned {
        #[expect(dead_code)] // must keep it alive
        owner: Box<dyn AsRef<[u8]> + 'a>,
    },
}

/// A byte-level parser unifying zero-copy slice parsing and streaming.
///
/// Two construction modes are supported:
///
/// - [`ParserSource::from_slice`] for a borrowed in-memory slice (zero-copy).
/// - [`ParserSource::from_reader`] for a streaming source backed by an internal
///   buffer of a fixed maximum capacity.
///
/// All accessors that may need more data transparently refill when in streaming
/// mode.
///
/// [`ParserSource`] is tailored to data that is being read from a compressed
/// source, like how many save files are structured.
///
/// # Example
///
/// A typical pull loop: keep reading fixed-size records until the parser
/// reaches a clean EOF. The same code works for both slice and streaming
/// inputs.
///
/// ```
/// use jomini::{ParserError, ParserSource};
///
/// fn sum_u32_records(parser: &mut ParserSource<'_>) -> Result<u64, ParserError> {
///     let mut total = 0u64;
///     while let Some(bytes) = parser.try_take::<4>()? {
///         total += u32::from_le_bytes(*bytes) as u64;
///     }
///     Ok(total)
/// }
///
/// let data = [1u32, 2, 3, 4]
///     .iter()
///     .flat_map(|x| x.to_le_bytes())
///     .collect::<Vec<_>>();
/// let mut parser = ParserSource::from_slice(&data);
/// assert_eq!(sum_u32_records(&mut parser).unwrap(), 10);
/// ```
pub struct ParserSource<'a> {
    ptr: *const u8,
    end: *const u8,
    base_ptr: *const u8, // Immutable anchor tracking the true start of memory
    total_bytes_parsed: usize,
    streaming: Option<Box<Backing<'a>>>,
    _marker: PhantomData<&'a [u8]>,
}

impl std::fmt::Debug for ParserSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ParserSource")
            .field("remaining", &self.window_len())
            .field("position", &self.position())
            .field("streaming", &self.streaming.is_some())
            .finish()
    }
}

impl<'a> ParserSource<'a> {
    /// Zero-copy initialization for in-memory byte slices.
    pub fn from_slice(data: &'a [u8]) -> Self {
        let start = data.as_ptr();
        Self {
            ptr: start,
            end: unsafe { start.add(data.len()) },
            base_ptr: start,
            total_bytes_parsed: 0,
            streaming: None,
            _marker: PhantomData,
        }
    }

    /// Zero-copy initialization that takes ownership of the underlying buffer.
    ///
    /// ```
    /// use jomini::ParserSource;
    ///
    /// let mut parser = ParserSource::from_owned(vec![1u8, 2, 3, 4]);
    /// assert_eq!(parser.take::<4>().unwrap(), &[1u8, 2, 3, 4]);
    /// ```
    pub fn from_owned<T: AsRef<[u8]> + 'a>(owner: T) -> Self {
        let boxed: Box<dyn AsRef<[u8]> + 'a> = Box::new(owner);
        let slice = (*boxed).as_ref();
        let start = slice.as_ptr();
        let len = slice.len();
        Self {
            ptr: start,
            end: unsafe { start.add(len) },
            base_ptr: start,
            total_bytes_parsed: 0,
            streaming: Some(Box::new(Backing::Owned { owner: boxed })),
            _marker: PhantomData,
        }
    }

    /// Streaming initialization with the default buffer capacity.
    ///
    /// # Performance
    ///
    /// Reader constructors internally manages a buffer to optimize parsing
    /// throughput. To avoid the memory overhead of double-buffering, prefer
    /// passing a raw reader or decompression stream directly instead of
    /// wrapping it in `BufReader`.
    pub fn from_reader<R: Read + 'a>(source: R) -> Self {
        Self::from_reader_with_buf(source, vec![0u8; DEFAULT_CAPACITY])
    }

    /// Streaming initialization with a caller-provided buffer.
    ///
    /// This lets callers control capacity or reuse an existing allocation.
    ///
    /// ```
    /// use jomini::ParserSource;
    ///
    /// let buffer = vec![0; 1024];
    /// let _parser = ParserSource::from_reader_with_buf(&b"EU4bin"[..], buffer);
    /// ```
    pub fn from_reader_with_buf<R>(source: R, buffer: Vec<u8>) -> Self
    where
        R: Read + 'a,
    {
        let vec = buffer;
        let start = vec.as_ptr();
        Self {
            ptr: start,
            end: start, // Starts empty to force an immediate first chunk read
            base_ptr: start,
            total_bytes_parsed: 0,
            streaming: Some(Box::new(Backing::Stream {
                buffer: vec,
                source: Box::new(source),
            })),
            _marker: PhantomData,
        }
    }

    /// Guarantees the required bytes are present in the current window, refilling as necessary.
    #[inline(always)]
    pub fn ensure_bytes(&mut self, required: usize) -> Result<(), ParserError> {
        let available = unsafe { self.end.offset_from_unsigned(self.ptr) };
        if available >= required {
            return Ok(());
        }
        self.refill_slow(required)
    }

    /// Returns how many total bytes have been read.
    #[inline(always)]
    pub fn position(&self) -> usize {
        let consumed_in_current_window = unsafe { self.ptr.offset_from_unsigned(self.base_ptr) };
        self.total_bytes_parsed + consumed_in_current_window
    }

    /// Advances the parse cursor by `bytes` without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller must ensure `bytes <= self.window_len()`; typically by first
    /// calling [`ensure_bytes`](Self::ensure_bytes).
    #[inline(always)]
    pub unsafe fn advance_unchecked(&mut self, bytes: usize) {
        self.ptr = unsafe { self.ptr.add(bytes) };
    }

    /// Advances the parse cursor by `bytes` within the current window.
    ///
    /// Returns `false` if `bytes` exceeds the current contiguous unread window.
    #[inline(always)]
    pub fn advance(&mut self, bytes: usize) -> bool {
        if bytes <= self.window_len() {
            unsafe {
                self.advance_unchecked(bytes);
            }
            true
        } else {
            false
        }
    }

    /// Current window length (no refill).
    #[inline(always)]
    pub fn window_len(&self) -> usize {
        unsafe { self.end.offset_from_unsigned(self.ptr) }
    }

    /// Returns the current unread window without refilling.
    #[inline(always)]
    pub fn window(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, self.window_len()) }
    }

    /// Returns a slice of the next `len` bytes without bounds checking or
    /// advancing the cursor.
    ///
    /// # Safety
    ///
    /// The caller must ensure `len <= self.window_len()`; typically by first
    /// calling [`ensure_bytes`](Self::ensure_bytes).
    #[inline(always)]
    pub unsafe fn get_window_unchecked(&self, len: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, len) }
    }

    /// Refills the current window from the underlying reader.
    ///
    /// Any unread bytes in the current window are preserved and moved to the
    /// front of the internal buffer before reading. Returns the number of new
    /// bytes read from the underlying reader. Slice and owned sources cannot be
    /// refilled, so they return `Ok(0)`.
    #[inline]
    pub fn refill(&mut self) -> Result<usize, ParserError> {
        let (buffer, source) = match self.streaming.as_deref_mut() {
            Some(Backing::Owned { .. }) | None => return Ok(0),
            Some(Backing::Stream { buffer, source }) => (buffer, source),
        };

        let unparsed_len = unsafe { self.end.offset_from_unsigned(self.ptr) };
        if unparsed_len >= buffer.len() {
            return Err(ParserError::BufferTooSmall);
        }

        let consumed_in_window = unsafe { self.ptr.offset_from_unsigned(self.base_ptr) };
        self.total_bytes_parsed += consumed_in_window;

        let internal_buffer_start = buffer.as_mut_ptr();
        if unparsed_len > 0 {
            unsafe {
                ptr::copy(self.ptr, internal_buffer_start, unparsed_len);
            }
        }

        self.ptr = internal_buffer_start;
        self.end = unsafe { internal_buffer_start.add(unparsed_len) };

        let bytes_written = source.read(&mut buffer[unparsed_len..])?;
        self.end = unsafe { self.end.add(bytes_written) };
        Ok(bytes_written)
    }

    /// Peek at the next `N` bytes without advancing.
    ///
    /// Returns `Ok(None)` only at clean EOF when there are no bytes available
    /// for the next item. If a partial item is present, returns
    /// [`ParserError::eof`]. Refills internally as needed.
    #[inline(always)]
    pub fn peek<const N: usize>(&mut self) -> Result<Option<&[u8; N]>, ParserError> {
        if self.window_len() >= N {
            unsafe { return Ok(Some(&*self.ptr.cast::<[u8; N]>())) };
        }
        self.peek_slow::<N>()
    }

    #[inline(never)]
    fn peek_slow<const N: usize>(&mut self) -> Result<Option<&[u8; N]>, ParserError> {
        match self.ensure_bytes(N) {
            Ok(()) => unsafe { Ok(Some(&*self.ptr.cast::<[u8; N]>())) },
            Err(ParserError::Io(io::ErrorKind::UnexpectedEof)) if self.window_len() == 0 => {
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    /// Reads the next `N` bytes as a fixed-size array, advancing the cursor.
    ///
    /// Returns [`ParserError::eof`] if fewer than `N` bytes remain even after
    /// refilling.
    #[inline(always)]
    pub fn take<const N: usize>(&mut self) -> Result<&[u8; N], ParserError> {
        self.ensure_bytes(N)?;
        unsafe {
            let array_ref = &*self.ptr.cast::<[u8; N]>();
            self.advance_unchecked(N);
            Ok(array_ref)
        }
    }

    /// Attempts to read the next `N` bytes as a fixed-size array.
    ///
    /// Returns `Ok(None)` only at clean EOF when there are no bytes available
    /// for the next item. If a partial item is present, returns
    /// [`ParserError::eof`].
    #[inline(always)]
    pub fn try_take<const N: usize>(&mut self) -> Result<Option<&[u8; N]>, ParserError> {
        if self.window_len() >= N {
            unsafe {
                let array_ref = &*self.ptr.cast::<[u8; N]>();
                self.advance_unchecked(N);
                return Ok(Some(array_ref));
            }
        }
        self.try_take_slow::<N>()
    }

    #[inline(never)]
    fn try_take_slow<const N: usize>(&mut self) -> Result<Option<&[u8; N]>, ParserError> {
        match self.ensure_bytes(N) {
            Ok(()) => unsafe {
                let array_ref = &*self.ptr.cast::<[u8; N]>();
                self.advance_unchecked(N);
                Ok(Some(array_ref))
            },
            Err(ParserError::Io(io::ErrorKind::UnexpectedEof)) if self.window_len() == 0 => {
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    /// Reads the next `n` bytes as a slice, advancing the cursor.
    ///
    /// Returns [`ParserError::eof`] if fewer than `n` bytes remain even after
    /// refilling.
    #[inline(always)]
    pub fn take_bytes(&mut self, n: usize) -> Result<&[u8], ParserError> {
        self.ensure_bytes(n)?;
        unsafe {
            let slice = std::slice::from_raw_parts(self.ptr, n);
            self.advance_unchecked(n);
            Ok(slice)
        }
    }

    /// Attempts to read the next `n` bytes as a slice.
    ///
    /// Returns `Ok(None)` only at clean EOF when there are no bytes available
    /// for the next item. If a partial item is present, returns
    /// [`ParserError::eof`].
    #[inline(always)]
    pub fn try_take_bytes(&mut self, n: usize) -> Result<Option<&[u8]>, ParserError> {
        if self.window_len() >= n {
            unsafe {
                let slice = std::slice::from_raw_parts(self.ptr, n);
                self.advance_unchecked(n);
                return Ok(Some(slice));
            }
        }
        self.try_take_bytes_slow(n)
    }

    #[inline(never)]
    fn try_take_bytes_slow(&mut self, n: usize) -> Result<Option<&[u8]>, ParserError> {
        match self.ensure_bytes(n) {
            Ok(()) => unsafe {
                let slice = std::slice::from_raw_parts(self.ptr, n);
                self.advance_unchecked(n);
                Ok(Some(slice))
            },
            Err(ParserError::Io(io::ErrorKind::UnexpectedEof)) if self.window_len() == 0 => {
                Ok(None)
            }
            Err(e) => Err(e),
        }
    }

    #[inline(never)]
    fn refill_slow(&mut self, required: usize) -> Result<(), ParserError> {
        // Owned buffers expose the entire payload up front; if we got here the
        // request is bigger than what remains, so the answer is EOF — do *not*
        // slide, because the data lives in the caller's allocation, not in a
        // mutable refill buffer.
        let (buffer, source) = match self.streaming.as_deref_mut() {
            Some(Backing::Owned { .. }) | None => return Err(ParserError::eof()),
            Some(Backing::Stream { buffer, source }) => (buffer, source),
        };

        if required > buffer.len() {
            return Err(ParserError::BufferTooSmall);
        }

        // 1. Commit metrics before transforming pointer arrays
        let consumed_in_window = unsafe { self.ptr.offset_from_unsigned(self.base_ptr) };
        self.total_bytes_parsed += consumed_in_window;

        // 2. Identify unparsed trailing slices to save
        let unparsed_len = unsafe { self.end.offset_from_unsigned(self.ptr) };
        let internal_buffer_start = buffer.as_mut_ptr();

        if unparsed_len > 0 {
            unsafe {
                // Slide data down to base alignment
                ptr::copy(self.ptr, internal_buffer_start, unparsed_len);
            }
        }

        // 3. Align tracking parameters to the head of the buffer
        self.ptr = internal_buffer_start;
        self.end = unsafe { internal_buffer_start.add(unparsed_len) };

        // 4. Top up the buffer until it is completely full, or the stream
        //    dries up. Only fail with EOF if we couldn't satisfy `required`.
        loop {
            let current_buffer_len =
                unsafe { self.end.offset_from_unsigned(internal_buffer_start) };
            if current_buffer_len >= buffer.len() {
                return Ok(());
            }

            let target_slice = &mut buffer[current_buffer_len..];
            let bytes_written = source.read(target_slice)?;

            if bytes_written == 0 {
                let available = unsafe { self.end.offset_from_unsigned(self.ptr) };
                if available >= required {
                    return Ok(());
                }
                return Err(ParserError::eof());
            }

            self.end = unsafe { self.end.add(bytes_written) };
        }
    }
}

/// Jomini binary-format helpers for [`ParserSource`].
pub trait BinarySourceExt {
    /// Reads a Jomini binary byte string.
    ///
    /// Binary strings are encoded as a little-endian `u16` byte length followed by
    /// that many raw bytes. The returned bytes are not decoded; callers should use
    /// the active game encoding or format to interpret them.
    fn read_bstr(&mut self) -> Result<&[u8], ParserError>;

    /// Reads the next two bytes as a Jomini binary lexeme id.
    fn read_lexeme_id(&mut self) -> Result<LexemeId, ParserError>;

    /// Peeks at the next two bytes as a Jomini binary lexeme id without advancing.
    fn peek_lexeme_id(&mut self) -> Result<Option<LexemeId>, ParserError>;
}

impl BinarySourceExt for ParserSource<'_> {
    #[inline]
    fn read_bstr(&mut self) -> Result<&[u8], ParserError> {
        self.ensure_bytes(2)?;
        let len = u16::from_le_bytes(unsafe { *self.ptr.cast::<[u8; 2]>() }) as usize;
        self.ensure_bytes(2 + len)?;
        unsafe {
            self.advance_unchecked(2);
            let data = std::slice::from_raw_parts(self.ptr, len);
            self.advance_unchecked(len);
            Ok(data)
        }
    }

    #[inline]
    fn read_lexeme_id(&mut self) -> Result<LexemeId, ParserError> {
        Ok(LexemeId::new(u16::from_le_bytes(*self.take::<2>()?)))
    }

    #[inline]
    fn peek_lexeme_id(&mut self) -> Result<Option<LexemeId>, ParserError> {
        Ok(self
            .peek::<2>()?
            .map(|x| LexemeId::new(u16::from_le_bytes(*x))))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{self, Read};

    /// A [`Read`] adapter that hands out at most `chunk` bytes per call,
    /// stressing the refill paths of [`ParserSource::from_reader`].
    ///
    /// Use `chunk = 1` to force a refill between every byte.
    struct ChunkedReader {
        data: Vec<u8>,
        pos: usize,
        chunk: usize,
    }

    impl ChunkedReader {
        fn new(data: Vec<u8>, chunk: usize) -> Self {
            assert!(chunk > 0);
            Self {
                data,
                pos: 0,
                chunk,
            }
        }
    }

    impl Read for ChunkedReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let remaining = self.data.len() - self.pos;
            let n = remaining.min(buf.len()).min(self.chunk);
            buf[..n].copy_from_slice(&self.data[self.pos..self.pos + n]);
            self.pos += n;
            Ok(n)
        }
    }

    struct FailingReader;

    impl Read for FailingReader {
        fn read(&mut self, _: &mut [u8]) -> io::Result<usize> {
            Err(io::Error::new(io::ErrorKind::InvalidData, "read failed"))
        }
    }

    /// Builds matched slice/stream parsers over the same input.
    fn parsers(
        data: &[u8],
        chunk: usize,
        capacity: usize,
    ) -> (ParserSource<'_>, ParserSource<'static>) {
        let slice = ParserSource::from_slice(data);
        let stream = ParserSource::from_reader_with_buf(
            ChunkedReader::new(data.to_vec(), chunk),
            vec![0; capacity],
        );
        (slice, stream)
    }

    #[test]
    fn parser_error_classifies_io_error() {
        use std::error::Error as _;

        assert!(std::mem::size_of::<ParserError>() <= 2);

        let err = ParserError::from(io::Error::other("boom"));
        assert_eq!(err, ParserError::Io(io::ErrorKind::Other));
        assert_eq!(err.to_string(), "io error: other error");
        assert!(err.source().is_none());
    }

    #[test]
    fn empty_input_is_immediately_eof() {
        let mut s = ParserSource::from_slice(&[]);
        assert!(s.try_take::<1>().unwrap().is_none());

        let mut r = ParserSource::from_reader_with_buf(ChunkedReader::new(vec![], 1), vec![0; 16]);
        assert!(r.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn try_take_returns_some_when_window_has_remaining_bytes() {
        let mut parser = ParserSource::from_slice(&[1, 2, 3]);
        assert_eq!(parser.try_take::<1>().unwrap(), Some(&[1]));
    }

    #[test]
    fn slice_ensure_bytes_past_end_returns_eof() {
        let mut s = ParserSource::from_slice(&[1, 2, 3]);
        let err = s.ensure_bytes(4).unwrap_err();
        assert_eq!(err, ParserError::Io(io::ErrorKind::UnexpectedEof));
    }

    #[test]
    fn read_fixed_parity_byte_at_a_time() {
        let data: Vec<u8> = (0..50u8).collect();
        let (mut slice, mut stream) = parsers(&data, 1, 16);

        for _ in 0..10 {
            let a = *slice.take::<5>().unwrap();
            let b = *stream.take::<5>().unwrap();
            assert_eq!(a, b);
            assert_eq!(slice.position(), stream.position());
        }
        assert!(slice.try_take::<1>().unwrap().is_none());
        assert!(stream.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn peek_does_not_advance_and_matches_read() {
        let data: Vec<u8> = (0..20u8).collect();
        let (mut slice, mut stream) = parsers(&data, 1, 8);

        let ps = *slice.peek::<4>().unwrap().unwrap();
        let pr = *stream.peek::<4>().unwrap().unwrap();
        assert_eq!(ps, pr);
        // Peeking should not advance.
        assert_eq!(slice.position(), 0);
        assert_eq!(stream.position(), 0);

        let rs = *slice.take::<4>().unwrap();
        let rr = *stream.take::<4>().unwrap();
        assert_eq!(ps, rs);
        assert_eq!(pr, rr);
    }

    #[test]
    fn peek_propagates_non_eof_read_errors() {
        let mut parser = ParserSource::from_reader_with_buf(FailingReader, vec![0; 8]);
        let err = parser.peek::<1>().unwrap_err();

        assert_ne!(err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(err, ParserError::Io(io::ErrorKind::InvalidData));
    }

    #[test]
    fn peek_at_eof_returns_none() {
        let data = [1u8, 2, 3];
        let (mut slice, mut stream) = parsers(&data, 1, 8);
        slice.take::<3>().unwrap();
        stream.take::<3>().unwrap();
        assert!(slice.peek::<4>().unwrap().is_none());
        assert!(stream.peek::<4>().unwrap().is_none());
    }

    #[test]
    fn peek_errors_on_partial_item() {
        let data = [1u8, 2, 3];
        let (mut slice, mut stream) = parsers(&data, 1, 8);

        let slice_err = slice.peek::<4>().unwrap_err();
        let stream_err = stream.peek::<4>().unwrap_err();
        assert_eq!(slice_err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(stream_err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(slice.window(), &[1, 2, 3]);
        assert_eq!(stream.window(), &[1, 2, 3]);
    }

    #[test]
    fn try_take_returns_none_only_at_clean_eof() {
        let data = [1u8, 2, 3, 4];
        let (mut slice, mut stream) = parsers(&data, 1, 8);

        assert_eq!(slice.try_take::<2>().unwrap(), Some(&[1, 2]));
        assert_eq!(stream.try_take::<2>().unwrap(), Some(&[1, 2]));
        assert_eq!(slice.try_take_bytes(2).unwrap(), Some(&[3, 4][..]));
        assert_eq!(stream.try_take_bytes(2).unwrap(), Some(&[3, 4][..]));
        assert!(slice.try_take::<1>().unwrap().is_none());
        assert!(stream.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn try_take_errors_on_partial_item() {
        let data = [1u8, 2, 3];
        let (mut slice, mut stream) = parsers(&data, 1, 8);

        let slice_err = slice.try_take::<4>().unwrap_err();
        let stream_err = stream.try_take::<4>().unwrap_err();
        assert_eq!(slice_err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(stream_err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(slice.window(), &[1, 2, 3]);
        assert_eq!(stream.window(), &[1, 2, 3]);
    }

    #[test]
    fn length_prefixed_slice_parity_across_refills() {
        // Three length-prefixed payloads. The streaming buffer (capacity 8) is
        // intentionally smaller than the largest payload+prefix, so the slow
        // path's "slide unparsed bytes down" branch is exercised.
        let mut data = Vec::new();
        for payload in [b"abcd".as_slice(), b"hi", b"streaming!!"] {
            data.extend_from_slice(&(payload.len() as u16).to_le_bytes());
            data.extend_from_slice(payload);
        }

        // Use a tiny buffer (16 bytes) and 1-byte chunks to force many refills.
        let (mut slice, mut stream) = parsers(&data, 1, 16);

        for expected in [b"abcd".as_slice(), b"hi", b"streaming!!"] {
            let a = slice.read_bstr().unwrap().to_vec();
            let b = stream.read_bstr().unwrap().to_vec();
            assert_eq!(a, expected);
            assert_eq!(b, expected);
        }
        assert!(slice.try_take::<1>().unwrap().is_none());
        assert!(stream.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn length_prefix_preserved_when_body_unavailable() {
        // Stream: 2-byte length prefix says 10, then truncated body of 4 bytes.
        let mut data = Vec::new();
        data.extend_from_slice(&10u16.to_le_bytes());
        data.extend_from_slice(b"abcd");

        let mut stream =
            ParserSource::from_reader_with_buf(ChunkedReader::new(data.clone(), 1), vec![0; 32]);

        let err = stream.read_bstr().unwrap_err();
        assert_eq!(err, ParserError::Io(io::ErrorKind::UnexpectedEof));

        // The contract: cursor remains intact, so the length prefix is still
        // visible to a subsequent peek.
        let prefix = stream.peek::<2>().unwrap().unwrap();
        assert_eq!(u16::from_le_bytes(*prefix), 10);
    }

    #[test]
    fn position_is_cumulative_across_refills() {
        let data: Vec<u8> = (0..100u8).collect();
        let mut stream =
            ParserSource::from_reader_with_buf(ChunkedReader::new(data.clone(), 1), vec![0; 8]);

        let mut consumed = 0usize;
        while stream.try_take::<1>().unwrap().is_some() {
            consumed += 1;
            assert_eq!(stream.position(), consumed);
        }
        assert_eq!(consumed, data.len());
    }

    #[test]
    fn window_and_window_len_reflect_consumed_bytes() {
        let data = [1u8, 2, 3, 4, 5];
        let mut slice = ParserSource::from_slice(&data);
        assert_eq!(slice.window_len(), 5);
        assert_eq!(slice.window(), &data);
        slice.take::<2>().unwrap();
        assert_eq!(slice.window_len(), 3);
        assert_eq!(slice.window(), &[3, 4, 5]);
    }

    #[test]
    fn advance_within_window_moves_cursor_and_subsequent_reads_follow() {
        let mut parser = ParserSource::from_slice(&[10, 20, 30, 40, 50]);
        assert!(parser.advance(2));
        assert_eq!(parser.window_len(), 3);
        assert_eq!(parser.position(), 2);
        assert_eq!(*parser.take::<2>().unwrap(), [30, 40]);
        assert_eq!(parser.window(), &[50]);
    }

    #[test]
    fn refill_preserves_unread_bytes_and_reads_once() {
        let data: Vec<u8> = (0..10u8).collect();
        let mut parser =
            ParserSource::from_reader_with_buf(ChunkedReader::new(data, 3), vec![0; 8]);

        assert_eq!(parser.refill().unwrap(), 3);
        assert_eq!(parser.window(), &[0, 1, 2]);
        assert!(parser.advance(2));
        assert_eq!(parser.position(), 2);

        assert_eq!(parser.refill().unwrap(), 3);
        assert_eq!(parser.position(), 2);
        assert_eq!(parser.window(), &[2, 3, 4, 5]);
    }

    #[test]
    fn refill_returns_zero_at_stream_eof() {
        let mut parser =
            ParserSource::from_reader_with_buf(ChunkedReader::new(vec![1, 2], 8), vec![0; 8]);

        assert_eq!(parser.refill().unwrap(), 2);
        assert_eq!(parser.take::<2>().unwrap(), &[1, 2]);
        assert_eq!(parser.refill().unwrap(), 0);
        assert_eq!(parser.position(), 2);
        assert_eq!(parser.window_len(), 0);
    }

    #[test]
    fn refill_on_slice_and_owned_sources_returns_zero() {
        let mut slice = ParserSource::from_slice(&[1, 2, 3]);
        let mut owned = ParserSource::from_owned(vec![1u8, 2, 3]);

        assert_eq!(slice.refill().unwrap(), 0);
        assert_eq!(slice.window(), &[1, 2, 3]);
        assert_eq!(owned.refill().unwrap(), 0);
        assert_eq!(owned.window(), &[1, 2, 3]);
    }

    #[test]
    fn refill_errors_when_unread_window_fills_stream_buffer() {
        let mut parser =
            ParserSource::from_reader_with_buf(ChunkedReader::new(vec![1, 2, 3, 4], 8), vec![0; 4]);

        assert_eq!(parser.refill().unwrap(), 4);
        let err = parser.refill().unwrap_err();
        assert_eq!(err, ParserError::BufferTooSmall);
        assert_eq!(parser.window(), &[1, 2, 3, 4]);
        assert_eq!(parser.position(), 0);
    }

    #[test]
    fn advance_past_window_returns_false_and_does_not_move() {
        let mut parser = ParserSource::from_slice(&[1, 2, 3]);
        assert!(!parser.advance(4));
        assert_eq!(parser.window_len(), 3);
        assert_eq!(parser.window(), &[1, 2, 3]);
        assert_eq!(parser.position(), 0);
    }

    #[test]
    fn advance_does_not_refill_streaming_window() {
        // Streaming parser starts with an empty window — advance must not
        // trigger a refill, so a non-zero request fails until the window is
        // materialized by another call (e.g. ensure_bytes).
        let data: Vec<u8> = (0..10u8).collect();
        let mut stream =
            ParserSource::from_reader_with_buf(ChunkedReader::new(data, 1), vec![0; 8]);
        assert_eq!(stream.window_len(), 0);
        assert!(!stream.advance(1));

        stream.ensure_bytes(4).unwrap();
        let window = stream.window_len();
        assert!(!stream.advance(window + 1));
        assert!(stream.advance(window));
        assert_eq!(stream.position(), window);
    }

    #[test]
    fn get_slice_unchecked_returns_current_window_prefix() {
        let mut parser = ParserSource::from_slice(&[1, 2, 3, 4]);
        parser.ensure_bytes(3).unwrap();

        let slice = unsafe { parser.get_window_unchecked(3) };
        assert_eq!(slice, &[1, 2, 3]);
        assert_eq!(parser.position(), 0);
    }

    #[test]
    fn ensure_bytes_errors_when_request_exceeds_capacity() {
        let mut stream =
            ParserSource::from_reader_with_buf(ChunkedReader::new(vec![0u8; 32], 1), vec![0; 8]);
        let err = stream.ensure_bytes(16).unwrap_err();
        assert_eq!(err, ParserError::BufferTooSmall);
    }

    #[test]
    fn from_owned_vec_parity_with_slice() {
        let data: Vec<u8> = (0..50u8).collect();
        let mut slice = ParserSource::from_slice(&data);
        let mut owned = ParserSource::from_owned(data.clone());

        for _ in 0..10 {
            let a = *slice.take::<5>().unwrap();
            let b = *owned.take::<5>().unwrap();
            assert_eq!(a, b);
            assert_eq!(slice.position(), owned.position());
        }
        assert!(slice.try_take::<1>().unwrap().is_none());
        assert!(owned.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn from_owned_array_is_static() {
        // `[u8; N]` owned by value — confirms the bound accepts non-Vec owners
        // and that the returned parser is `'static`.
        fn requires_static(_: &ParserSource<'static>) {}
        let owned = ParserSource::from_owned([1u8, 2, 3, 4]);
        requires_static(&owned);
        let mut owned = owned;
        assert_eq!(*owned.take::<4>().unwrap(), [1, 2, 3, 4]);
        assert!(owned.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn from_owned_arc_keeps_data_alive() {
        use std::sync::Arc;
        let arc: Arc<[u8]> = Arc::from(vec![10u8, 20, 30, 40, 50].into_boxed_slice());
        let mut owned = ParserSource::from_owned(Arc::clone(&arc));
        drop(arc); // owner inside ParserSource must keep allocation alive
        assert_eq!(*owned.take::<5>().unwrap(), [10, 20, 30, 40, 50]);
        assert!(owned.try_take::<1>().unwrap().is_none());
    }

    #[test]
    fn from_owned_ensure_bytes_past_end_returns_eof_without_slide() {
        // After consuming a prefix, requesting more than remains must return
        // EOF without disturbing the cursor: the tail of the owner's slice
        // should still be visible via as_slice().
        let mut owned = ParserSource::from_owned(vec![1u8, 2, 3, 4, 5]);
        owned.take::<2>().unwrap();
        let tail_ptr_before = owned.window().as_ptr();
        let err = owned.ensure_bytes(10).unwrap_err();
        assert_eq!(err, ParserError::Io(io::ErrorKind::UnexpectedEof));
        assert_eq!(owned.window(), &[3, 4, 5]);
        // No slide: the slice still points into the original owner allocation.
        assert_eq!(owned.window().as_ptr(), tail_ptr_before);
    }

    #[test]
    fn from_owned_static_bytes() {
        static DATA: &[u8] = b"hello world";
        let mut owned = ParserSource::from_owned(DATA);
        assert_eq!(*owned.take::<5>().unwrap(), *b"hello");
        owned.take::<1>().unwrap();
        assert_eq!(*owned.take::<5>().unwrap(), *b"world");
        assert!(owned.try_take::<1>().unwrap().is_none());
    }
}
