use std::io::{self, Read};
use std::marker::PhantomData;
use std::ptr;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEof,
    Io(io::Error),
}

impl From<io::Error> for ParserError {
    fn from(err: io::Error) -> Self {
        ParserError::Io(err)
    }
}

pub type SliceParser<'a> = ParserBuf<'a>;
pub type StreamingParser = ParserBuf<'static>;

/// Heavy operations are completely isolated here, away from the hot path
struct StreamingState {
    buffer: Vec<u8>,
    source: Box<dyn Read + 'static>,
}

pub struct ParserBuf<'a> {
    // --- HOT PATH BOUNDARIES (40 Bytes total layout - Fits in 1 Cache Line) ---
    ptr: *const u8,
    end: *const u8,
    base_ptr: *const u8, // Immutable anchor tracking the true start of memory
    total_bytes_parsed: usize,

    // Niche optimized: Exactly 8 bytes on the stack, null pointer if None
    streaming: Option<Box<StreamingState>>,
    _marker: PhantomData<&'a [u8]>,
}

impl<'a> ParserBuf<'a> {
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

    /// Streaming initialization for massive multi-gigabyte or compressed files.
    pub fn from_stream(source: Box<dyn Read + 'static>, capacity: usize) -> Self {
        let vec = vec![0u8; capacity];
        let start = vec.as_ptr();
        Self {
            ptr: start,
            end: start, // Starts empty to force an immediate first chunk read
            base_ptr: start,
            total_bytes_parsed: 0,
            streaming: Some(Box::new(StreamingState {
                buffer: vec,
                source,
            })),
            _marker: PhantomData,
        }
    }

    /// High-performance check for structured boundary limits (e.g., matching keys)
    #[inline(always)]
    pub fn is_eof(&mut self) -> Result<bool, ParserError> {
        if self.ptr != self.end {
            return Ok(false);
        }
        self.refill_and_check_empty()
    }

    /// Guaranteed contiguous memory validation. Crucial for speculative parsing.
    #[inline(always)]
    pub fn ensure_bytes(&mut self, required: usize) -> Result<(), ParserError> {
        let available = unsafe { self.end.offset_from(self.ptr) } as usize;
        if available >= required {
            return Ok(());
        }
        self.refill_slow(required)
    }

    /// On-demand metric progress tracking with absolute zero hot-path write overhead.
    #[inline(always)]
    pub fn total_bytes_read(&self) -> usize {
        let consumed_in_current_window = unsafe { self.ptr.offset_from(self.base_ptr) } as usize;
        self.total_bytes_parsed + consumed_in_current_window
    }

    #[inline(always)]
    pub unsafe fn advance_unchecked(&mut self, bytes: usize) {
        self.ptr = unsafe { self.ptr.add(bytes) };
    }

    #[inline(always)]
    pub unsafe fn get_slice_unchecked(&self, len: usize) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr, len) }
    }

    #[inline(always)]
    pub fn read<const N: usize>(&mut self) -> Result<&[u8; N], ParserError> {
        self.ensure_bytes(N)?;

        unsafe {
            let array_ref = &*(self.ptr as *const [u8; N]);
            self.advance_unchecked(N);
            Ok(array_ref)
        }
    }

    /// Atomically reads a length-prefixed byte slice.
    /// If the full body hasn't arrived, the parser state is left completely intact.
    #[inline(always)]
    pub fn read_length_prefixed_slice(&mut self) -> Result<&[u8], ParserError> {
        const LEN: usize = 2; // Assuming a 2-byte length prefix (u16)  

        // Step 1: Ensure length prefix is present
        self.ensure_bytes(LEN)?;

        // Step 2: Speculatively peek the length without advancing `self.ptr`
        let len = unsafe {
            let mut bytes = [0u8; LEN];
            ptr::copy_nonoverlapping(self.ptr, bytes.as_mut_ptr(), LEN);
            u16::from_le_bytes(bytes) as usize
        };

        // Step 3: Enforce presence of BOTH prefix and body together.
        // If this invokes a refill, the length prefix is preserved because self.ptr hasn't moved!
        self.ensure_bytes(LEN + len)?;

        // Step 4: Both are guaranteed to be in memory. Slice and advance safely.
        unsafe {
            let body_ptr = self.ptr.add(LEN);
            self.advance_unchecked(len); // Skip past the body payload
            Ok(std::slice::from_raw_parts(body_ptr, len))
        }
    }

    #[inline(never)]
    fn refill_and_check_empty(&mut self) -> Result<bool, ParserError> {
        let state = match &mut self.streaming {
            Some(s) => s,
            None => return Ok(true),
        };

        self.total_bytes_parsed += unsafe { self.ptr.offset_from(self.base_ptr) } as usize;

        let internal_buffer_start = state.buffer.as_mut_ptr();
        self.ptr = internal_buffer_start;
        self.end = internal_buffer_start;
        // self.base_ptr remains fully immutable pointing to the vec start.

        let bytes_written = state.source.read(&mut state.buffer[..])?;
        if bytes_written == 0 {
            return Ok(true);
        }

        self.end = unsafe { internal_buffer_start.add(bytes_written) };
        Ok(false)
    }

    #[inline(never)]
    fn refill_slow(&mut self, required: usize) -> Result<(), ParserError> {
        let state = match &mut self.streaming {
            Some(s) => s,
            None => return Err(ParserError::UnexpectedEof),
        };

        if required > state.buffer.len() {
            panic!("Requested block size exceeds maximum allocated capacity of ParserBuf!");
        }

        // 1. Commit metrics before transforming pointer arrays
        let consumed_in_window = unsafe { self.ptr.offset_from(self.base_ptr) } as usize;
        self.total_bytes_parsed += consumed_in_window;

        // 2. Identify unparsed trailing slices to save
        let unparsed_len = unsafe { self.end.offset_from(self.ptr) } as usize;
        let internal_buffer_start = state.buffer.as_mut_ptr();

        if unparsed_len > 0 {
            unsafe {
                // Slide data down to base alignment
                ptr::copy(self.ptr, internal_buffer_start, unparsed_len);
            }
        }

        // 3. Align tracking parameters to the head of the buffer
        self.ptr = internal_buffer_start;
        self.end = unsafe { internal_buffer_start.add(unparsed_len) };

        // 4. Continuously exhaust short-reads until constraint bounds are met
        loop {
            let available = unsafe { self.end.offset_from(self.ptr) } as usize;
            if available >= required {
                return Ok(());
            }

            let current_buffer_len =
                unsafe { self.end.offset_from(internal_buffer_start) } as usize;
            if current_buffer_len >= state.buffer.len() {
                return Err(ParserError::UnexpectedEof);
            }

            // Top up remaining buffer capacity
            let target_slice = &mut state.buffer[current_buffer_len..];
            let bytes_written = state.source.read(target_slice)?;

            if bytes_written == 0 {
                return Err(ParserError::UnexpectedEof); // Stream dried up mid-token processing
            }

            self.end = unsafe { self.end.add(bytes_written) };
        }
    }
}
