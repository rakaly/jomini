use crate::Scalar;
use std::{io::Read, marker::PhantomData, ops::Range};

#[derive(Debug)]
pub struct BufferWindow {
    pub buf: Box<[u8]>,

    start_buf: *const u8,

    // start of window into buffer
    pub start: *const u8,

    // end of window into buffer
    pub end: *const u8,

    // number of consumed bytes from prior reads
    pub prior_reads: usize,
}

pub enum BufferError {
    Io(std::io::Error),
    BufferFull,
}

impl BufferWindow {
    #[inline]
    pub fn from_slice(data: &[u8]) -> Self {
        Self {
            buf: Box::new([]),
            start_buf: data.as_ptr(),
            start: data.as_ptr(),
            end: data.as_ptr_range().end,
            prior_reads: 0,
        }
    }

    #[inline]
    pub fn advance_to(&mut self, ptr: *const u8) {
        debug_assert!((self.start..=self.end).contains(&ptr));
        self.start = ptr;
    }

    #[inline]
    pub fn advance(&mut self, amt: usize) {
        let ptr = unsafe { self.start.add(amt) };
        debug_assert!((self.start..=self.end).contains(&ptr));
        self.start = ptr;
    }

    #[inline]
    pub fn window(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.start, self.window_len()) }
    }

    #[inline]
    pub fn window_len(&self) -> usize {
        unsafe { self.end.offset_from(self.start) as usize }
    }

    #[inline]
    pub fn position(&self) -> usize {
        self.prior_reads + self.consumed_data()
    }

    #[inline]
    pub fn consumed_data(&self) -> usize {
        unsafe { self.start.offset_from(self.start_buf) as usize }
    }

    #[inline]
    pub fn get(&self, range: Range<*const u8>) -> Scalar {
        debug_assert!(range.start >= self.start_buf);
        debug_assert!(range.end <= self.end);
        let len = unsafe { range.end.offset_from(range.start) as usize };
        let sl = unsafe { std::slice::from_raw_parts(range.start, len) };
        Scalar::new(sl)
    }

    /// This seems similar to `BufRead::fill_buf`, but whereas the `BufRead`
    /// will only call the underlying read if the buffer is currently empty,
    /// this function will copy over the bytes that haven't been consumed to the
    /// start.
    #[inline]
    pub fn fill_buf(&mut self, mut reader: impl Read) -> Result<usize, BufferError> {
        let carry_over = self.window_len();
        if carry_over >= self.buf.len() {
            return Ok(0);
        }

        // Copy over the unconsumed bytes to the start of the buffer
        if carry_over != 0 {
            if carry_over >= self.buf.len() {
                return Err(BufferError::BufferFull);
            }
            self.buf.copy_within(self.consumed_data().., 0);
        }

        self.prior_reads += self.consumed_data();
        self.start = self.buf.as_ptr();
        self.end = unsafe { self.buf.as_ptr().add(carry_over) };

        // Have the reader start filling in bytes after unconsumed bytes
        match reader.read(&mut self.buf[carry_over..]) {
            Ok(r) => {
                self.start = self.buf.as_ptr();
                self.end = unsafe { self.end.add(r) };
                Ok(r)
            }
            Err(e) => Err(BufferError::Io(e)),
        }
    }
}

#[derive(Debug)]
pub struct BufferWindowBuilder {
    buffer: Option<Box<[u8]>>,
    buffer_len: usize,
}

impl Default for BufferWindowBuilder {
    fn default() -> Self {
        // Default buffer size of 32 KiB, same size that flate2 uses.
        let buffer_len = 32 * 1024;
        Self {
            buffer: None,
            buffer_len,
        }
    }
}

impl BufferWindowBuilder {
    #[inline]
    pub fn buffer(mut self, val: Box<[u8]>) -> BufferWindowBuilder {
        self.buffer = Some(val);
        self
    }

    #[inline]
    pub fn buffer_len(mut self, val: usize) -> BufferWindowBuilder {
        self.buffer_len = val;
        self
    }

    #[inline]
    pub fn build(self) -> BufferWindow {
        let init_len = self.buffer_len;
        let buf = self
            .buffer
            .unwrap_or_else(|| vec![0; init_len].into_boxed_slice());
        let start = buf.as_ptr_range().start;
        let end = buf.as_ptr_range().start;
        let start_buf = start;
        BufferWindow {
            buf,
            start,
            start_buf,
            end,
            prior_reads: 0,
        }
    }
}

/// An no-op read implementation used for TokenReaders
#[derive(Debug)]
pub struct SliceReader<'a>(PhantomData<&'a [u8]>);

impl<'a> SliceReader<'a> {
    pub(crate) fn new(_data: &'a [u8]) -> Self {
        SliceReader(PhantomData)
    }
}

impl<'a> Read for SliceReader<'a> {
    fn read(&mut self, _buf: &mut [u8]) -> std::io::Result<usize> {
        Ok(0)
    }
}
