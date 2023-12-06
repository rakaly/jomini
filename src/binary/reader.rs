use super::{
    lexer::{read_id, read_string, read_token},
    BinToken, LexError, LexemeId, LexerError,
};
use std::{fmt, io::Read};

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
    fn next_opt(&mut self) -> (Option<BinToken>, Option<ReaderError>) {
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
                    } else if self.data == self.buf.as_ptr() {
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
                    } else {
                        return (None, Some(self.lex_error(LexError::Eof)));
                    }
                }
                Err(e) => {
                    return (None, Some(self.io_error(e)));
                }
            }
        }
    }

    #[inline]
    pub fn skip_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
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

    #[inline]
    pub fn skip_token(&mut self, token: BinToken) -> Result<(), ReaderError> {
        if token != BinToken::Open {
            return Ok(());
        }

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
                    LexemeId::QUOTED_STRING | LexemeId::UNQUOTED_STRING => {
                        match read_string(data) {
                            Ok((_, d)) => self.data = d.as_ptr(),
                            Err(_) => break,
                        }
                    }
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
                    } else if self.data == self.buf.as_ptr() {
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
                    } else {
                        return Err(self.lex_error(LexError::Eof));
                    }
                }
                Err(e) => {
                    return Err(self.io_error(e));
                }
            }
        }
    }

    #[inline]
    fn into_parts(self) -> (Vec<u8>, R) {
        (self.buf, self.reader)
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn unlikely_read(&mut self) -> Result<BinToken, ReaderError> {
        self.read()
    }

    #[inline(always)]
    pub fn read(&mut self) -> Result<BinToken, ReaderError> {
        // Workaround for borrow checker :(
        let s = unsafe { &mut *(self as *mut TokenReader<R>) };
        match self.next_opt() {
            (Some(x), _) => Ok(x),
            (None, None) => Err(s.lex_error(LexError::Eof)),
            (None, Some(e)) => Err(e),
        }
    }

    #[inline(always)]
    pub fn next(&mut self) -> Result<Option<BinToken>, ReaderError> {
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
enum ReaderErrorKind {
    Read { cause: std::io::Error },
    MaxBufferReached,
    Lexer { cause: LexError },
}

#[derive(Debug)]
pub struct ReaderError {
    position: usize,
    kind: ReaderErrorKind,
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

    fn test_reader(data: &[u8], expected: &[BinToken]) {
        fn eq<R>(mut reader: TokenReader<R>, expected: &[BinToken])
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
        assert_eq!(reader.skip_bytes(6).unwrap(), &b"EU4bin"[..]);
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
            &[
                BinToken::Other(0x00e1),
                BinToken::Equal,
                BinToken::Open,
                BinToken::Close,
            ],
        );
    }
}
