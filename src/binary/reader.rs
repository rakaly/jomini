use super::{
    lexer::{read_id, read_string, read_token},
    LexError, LexemeId, LexerError, Token,
};
use std::{fmt, io::Read};

/// [Lexer](crate::binary::Lexer) that works over a [Read] implementation
///
/// Example of computing the max nesting depth using a [TokenReader].
///
/// ```rust
/// use jomini::binary::{TokenReader, Token};
/// let data = [0x2d, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x04, 0x00];
/// let mut reader = TokenReader::new(&data[..]);
/// let mut max_depth = 0;
/// let mut current_depth = 0;
/// while let Some(token) = reader.next()? {
///   match token {
///     Token::Open => {
///       current_depth += 1;
///       max_depth = max_depth.max(current_depth);
///     }
///     Token::Close => current_depth -= 1,
///     _ => {}
///   }
/// }
/// assert_eq!(max_depth, 2);
/// # Ok::<(), jomini::binary::ReaderError>(())
/// ```
#[derive(Debug)]
pub struct TokenReader<R> {
    reader: R,
    max_buf_size: usize,
    buf: Vec<u8>,
    data: *const u8,
    data_end: *const u8,
    prior_reads: usize,
}

impl<R> TokenReader<R>
where
    R: Read,
{
    /// Convenience method for constructing the default token reader
    #[inline]
    pub fn new(reader: R) -> Self {
        TokenReader::builder().build(reader)
    }

    /// Returns the byte position of the data stream that has been processed.
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, Token};
    /// let mut reader = TokenReader::new(&[0xd2, 0x28, 0xff][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.position(), 2);
    /// ```
    #[inline]
    pub fn position(&self) -> usize {
        self.prior_reads + self.consumed_data()
    }

    #[inline]
    fn consumed_data(&self) -> usize {
        unsafe { self.data.offset_from(self.buf.as_ptr()) as usize }
    }

    #[inline]
    fn data_len(&self) -> usize {
        unsafe { self.data_end.offset_from(self.data) as usize }
    }

    #[inline(always)]
    fn next_opt(&mut self) -> (Option<Token>, Option<ReaderError>) {
        loop {
            let data_len = self.data_len();
            let inp = unsafe { std::slice::from_raw_parts(self.data, data_len) };
            match read_token(inp) {
                Ok((tok, new_data)) => {
                    self.data = new_data.as_ptr();
                    return (Some(tok), None);
                }
                Err(LexError::Eof) => {}
                Err(e) => {
                    return (None, Some(self.lex_error(e)));
                }
            }

            let consumed = self.consumed_data();
            self.buf.copy_within(consumed..consumed + data_len, 0);
            match self.reader.read(&mut self.buf[data_len..]) {
                Ok(read) => {
                    if read > 0 {
                        self.prior_reads += consumed;
                        self.data_end = unsafe { self.buf.as_ptr().add(data_len + read) };
                        self.data = self.buf.as_ptr();
                    } else if data_len == 0 {
                        // If we read nothing and there is no data left to parse
                        // we are done
                        return (None, None);
                    } else if data_len < self.buf.len() {
                        return (None, Some(self.lex_error(LexError::Eof)));
                    } else {
                        // If we parsing didn't progress, there must be a token that spans
                        // multiple buffers, so we will need to grow the buffer.

                        // Unless the buffer is already at it's max size
                        if self.buf.len() >= self.max_buf_size {
                            return (None, Some(self.max_buffer_error()));
                        }

                        // Expand the buffer and set data to new start of our buffer
                        let new_len = self.buf.len().saturating_mul(2).min(self.max_buf_size);
                        self.buf.resize(new_len, 0);
                        self.data = self.buf.as_ptr();
                        self.data_end = unsafe { self.data.add(data_len) };
                    }
                }
                Err(e) => {
                    return (None, Some(self.io_error(e)));
                }
            }
        }
    }

    /// Advance a given number of bytes and return them.
    ///
    /// The internal buffer must be large enough to accomodate all bytes.
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, LexError, ReaderErrorKind};
    /// let mut reader = TokenReader::new(&b"EU4bin"[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4bin"[..]);
    /// assert!(matches!(reader.read_bytes(1).unwrap_err().kind(), ReaderErrorKind::Lexer {
    ///     cause: LexError::Eof
    /// }));
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
        while self.data_len() < bytes {
            let data_len = self.data_len();
            let consumed: usize = self.consumed_data();
            self.buf.copy_within(consumed..consumed + data_len, 0);
            match self.reader.read(&mut self.buf[data_len..]) {
                Ok(read) => {
                    if read > 0 {
                        self.prior_reads += consumed;
                        self.data_end = unsafe { self.buf.as_ptr().add(data_len + read) };
                        self.data = self.buf.as_ptr();
                    } else {
                        return Err(self.lex_error(LexError::Eof));
                    }
                }
                Err(e) => return Err(self.io_error(e)),
            }
        }

        let input = unsafe { std::slice::from_raw_parts(self.data, bytes) };
        self.data = unsafe { self.data.add(bytes) };
        Ok(input)
    }

    /// Advance through the containing block until the closing token is consumed
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, Token};
    /// let mut reader = TokenReader::new(&[
    ///     0xd2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00,
    ///     0x04, 0x00, 0x04, 0x00, 0xff, 0xff
    /// ][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.read().unwrap(), Token::Equal);
    /// assert_eq!(reader.read().unwrap(), Token::Open);
    /// assert!(reader.skip_container().is_ok());
    /// assert_eq!(reader.read().unwrap(), Token::Id(0xffff));
    /// ```
    #[inline]
    pub fn skip_container(&mut self) -> Result<(), ReaderError> {
        let mut depth = 1;
        loop {
            loop {
                let data_len = self.data_len();
                let input = unsafe { std::slice::from_raw_parts(self.data, data_len) };
                let (id, data) = match read_id(input) {
                    Ok((x, data)) => (x, data),
                    Err(_) => break,
                };

                match id {
                    LexemeId::CLOSE => {
                        self.data = data.as_ptr();
                        depth -= 1;
                        if depth == 0 {
                            return Ok(());
                        }
                    }
                    LexemeId::OPEN => {
                        self.data = data.as_ptr();
                        depth += 1
                    }
                    LexemeId::BOOL => match data.get(1..) {
                        Some(d) => self.data = d.as_ptr(),
                        None => break,
                    },
                    LexemeId::F32 | LexemeId::U32 | LexemeId::I32 => match data.get(4..) {
                        Some(d) => self.data = d.as_ptr(),
                        None => break,
                    },
                    LexemeId::F64 | LexemeId::I64 | LexemeId::U64 => match data.get(8..) {
                        Some(d) => self.data = d.as_ptr(),
                        None => break,
                    },
                    LexemeId::QUOTED | LexemeId::UNQUOTED => match read_string(data) {
                        Ok((_, d)) => self.data = d.as_ptr(),
                        Err(_) => break,
                    },
                    _ => self.data = data.as_ptr(),
                }
            }

            let data_len = self.data_len();
            let consumed: usize = self.consumed_data();
            self.buf.copy_within(consumed..consumed + data_len, 0);
            match self.reader.read(&mut self.buf[data_len..]) {
                Ok(read) => {
                    if read > 0 {
                        self.prior_reads += consumed;
                        self.data_end = unsafe { self.buf.as_ptr().add(data_len + read) };
                        self.data = self.buf.as_ptr();
                    } else if data_len < self.buf.len() {
                        return Err(self.lex_error(LexError::Eof));
                    } else {
                        // If we parsing didn't progress, there must be a token that spans
                        // multiple buffers, so we will need to grow the buffer.

                        // Unless the buffer is already at it's max size
                        if self.buf.len() >= self.max_buf_size {
                            return Err(self.max_buffer_error());
                        }

                        // Expand the buffer and set data to new start of our buffer
                        let new_len = self.buf.len().saturating_mul(2).min(self.max_buf_size);
                        self.buf.resize(new_len, 0);
                        self.data = self.buf.as_ptr();
                        self.data_end = unsafe { self.data.add(data_len) };
                    }
                }
                Err(e) => {
                    return Err(self.io_error(e));
                }
            }
        }
    }

    /// Consume the token reader and return the internal buffer and reader. This
    /// allows the buffer to be reused.
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader};
    /// let data = b"EU4bin";
    /// let mut reader = TokenReader::new(&data[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &data[..]);
    ///
    /// let (buf, _) = reader.into_parts();
    /// let data = b"HOI4bin";
    /// let mut reader = TokenReader::builder().buffer(buf).build(&data[..]);
    /// assert_eq!(reader.read_bytes(7).unwrap(), &data[..]);
    /// ```
    #[inline]
    pub fn into_parts(self) -> (Vec<u8>, R) {
        (self.buf, self.reader)
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn unlikely_read(&mut self) -> Result<Token, ReaderError> {
        self.read()
    }

    /// Read the next token in the stream. Will error if not enough data remains
    /// to decode a token.
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, Token, ReaderErrorKind, LexError};
    /// let mut reader = TokenReader::new(&[
    ///     0xd2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00
    /// ][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.read().unwrap(), Token::Equal);
    /// assert_eq!(reader.read().unwrap(), Token::Open);
    /// assert_eq!(reader.read().unwrap(), Token::Close);
    /// assert!(matches!(reader.read().unwrap_err().kind(), ReaderErrorKind::Lexer {
    ///     cause: LexError::Eof
    /// }));
    /// ```
    #[inline(always)]
    pub fn read(&mut self) -> Result<Token, ReaderError> {
        // Workaround for borrow checker :(
        let s = unsafe { &mut *(self as *mut TokenReader<R>) };
        match self.next_opt() {
            (Some(x), _) => Ok(x),
            (None, None) => Err(s.lex_error(LexError::Eof)),
            (None, Some(e)) => Err(e),
        }
    }

    /// Read a token, returning none when all the data has been consumed
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, Token};
    /// let mut reader = TokenReader::new(&[
    ///     0xd2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00
    /// ][..]);
    /// assert_eq!(reader.next().unwrap(), Some(Token::Id(0x28d2)));
    /// assert_eq!(reader.next().unwrap(), Some(Token::Equal));
    /// assert_eq!(reader.next().unwrap(), Some(Token::Open));
    /// assert_eq!(reader.next().unwrap(), Some(Token::Close));
    /// assert_eq!(reader.next().unwrap(), None);
    /// ```
    #[inline(always)]
    pub fn next(&mut self) -> Result<Option<Token>, ReaderError> {
        match self.next_opt() {
            (Some(x), _) => Ok(Some(x)),
            (None, None) => Ok(None),
            (None, Some(e)) => Err(e),
        }
    }

    #[cold]
    #[inline(never)]
    fn max_buffer_error(&self) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::MaxBufferReached,
        }
    }

    #[cold]
    #[inline(never)]
    fn lex_error(&self, e: LexError) -> ReaderError {
        ReaderError::from(e.at(self.position()))
    }

    #[cold]
    #[inline(always)]
    fn io_error(&self, e: std::io::Error) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::Read { cause: e },
        }
    }
}

impl TokenReader<()> {
    /// Initializes a default [TokenReaderBuilder]
    pub fn builder() -> TokenReaderBuilder {
        TokenReaderBuilder::default()
    }
}

#[derive(Debug)]
pub struct TokenReaderBuilder {
    buffer: Option<Vec<u8>>,
    init_buffer_len: usize,
    max_buffer_len: usize,
}

impl Default for TokenReaderBuilder {
    fn default() -> Self {
        Self {
            buffer: None,
            init_buffer_len: 32 * 1024, // default buffer size in flate2
            max_buffer_len: 64 * 1024,
        }
    }
}

impl TokenReaderBuilder {
    pub fn buffer(mut self, val: Vec<u8>) -> TokenReaderBuilder {
        self.buffer = Some(val);
        self
    }

    pub fn init_buffer_len(mut self, val: usize) -> TokenReaderBuilder {
        self.init_buffer_len = val;
        self
    }

    pub fn max_buffer_len(mut self, val: usize) -> TokenReaderBuilder {
        self.max_buffer_len = val;
        self.init_buffer_len = val.min(self.init_buffer_len);
        self
    }

    pub fn build<R>(self, reader: R) -> TokenReader<R> {
        let init_len = self.init_buffer_len;
        let buf = self.buffer.unwrap_or_else(|| vec![0; init_len]);
        let data = buf.as_ptr();
        let data_end = buf.as_ptr();
        TokenReader {
            reader,
            max_buf_size: self.max_buffer_len.max(buf.len()),
            buf,
            data,
            data_end,
            prior_reads: 0,
        }
    }
}

#[derive(Debug)]
pub enum ReaderErrorKind {
    Read { cause: std::io::Error },
    MaxBufferReached,
    Lexer { cause: LexError },
}

#[derive(Debug)]
pub struct ReaderError {
    position: usize,
    kind: ReaderErrorKind,
}

impl ReaderError {
    pub fn position(&self) -> usize {
        self.position
    }

    pub fn kind(&self) -> &ReaderErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn into_kind(self) -> ReaderErrorKind {
        self.kind
    }
}

impl std::error::Error for ReaderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            ReaderErrorKind::Read { cause } => Some(cause),
            _ => None,
        }
    }
}

impl std::fmt::Display for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ReaderErrorKind::Read { .. } => {
                write!(f, "failed to read past position: {}", self.position)
            }
            ReaderErrorKind::MaxBufferReached => {
                write!(f, "max buffer size exceeded at position: {}", self.position)
            }
            ReaderErrorKind::Lexer { cause } => {
                write!(f, "{} at position: {}", cause, self.position)
            }
        }
    }
}

impl From<LexerError> for ReaderError {
    fn from(value: LexerError) -> Self {
        ReaderError {
            position: value.position(),
            kind: ReaderErrorKind::Lexer {
                cause: value.into_kind(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_reader(data: &[u8], expected: &[Token]) {
        fn eq<R>(mut reader: TokenReader<R>, expected: &[Token])
        where
            R: Read,
        {
            for token in expected {
                assert_eq!(reader.next().unwrap(), Some(*token));
            }
            assert_eq!(reader.next().unwrap(), None);
        }

        eq(TokenReader::new(data), expected);

        let data_with_header: Vec<_> = b"EU4bin".iter().chain(data).copied().collect();
        let mut reader = TokenReader::new(data_with_header.as_slice());
        assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4bin"[..]);
        eq(reader, expected);

        eq(
            TokenReader::builder().init_buffer_len(5).build(data),
            expected,
        );
    }

    #[test]
    fn test_binary_token_reader() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00];
        test_reader(
            &data,
            &[Token::Id(0x00e1), Token::Equal, Token::Open, Token::Close],
        );
    }

    #[test]
    fn test_not_enough_data() {
        let mut reader = TokenReader::new(&[0x43][..]);
        assert!(matches!(
            reader.read().unwrap_err().kind(),
            &ReaderErrorKind::Lexer {
                cause: LexError::Eof
            }
        ));
    }
}
