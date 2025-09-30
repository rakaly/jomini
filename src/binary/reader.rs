use super::{
    LexError, LexemeId, LexerError, Token,
    lexer::{read_id, read_string, read_token},
};
use crate::buffer::{BufferError, BufferWindow, BufferWindowBuilder};
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
///
/// The tokens yielded from a [TokenReader] are not fully parsed. Some things to
/// be aware of:
///
/// - Ghost objects will not be skipped (eg: `foo={ {} a=b }`).
/// - [TokenReader] can not inform the caller if the container is an array or
///   object (or neither).
///
///  This is a much more raw view of the data that can be used to construct
/// higher level parsers, melters, and deserializers that operate over a stream
/// of data.
///
/// [TokenReader] operates over a fixed size buffer, so using a
/// [BufRead](std::io::BufRead) affords no benefits. An error will be returned
/// for tokens that are impossible to fit within the buffer (eg: if the provided
/// with 100 byte buffer but there is a binary string that is 101 bytes long).
#[derive(Debug)]
pub struct TokenReader<R> {
    reader: R,
    buf: BufferWindow,
}

impl TokenReader<()> {
    /// Read from a byte slice without memcpy's
    #[inline]
    pub fn from_slice(data: &[u8]) -> TokenReader<&'_ [u8]> {
        TokenReader {
            reader: data,
            buf: BufferWindow::from_slice(data),
        }
    }
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
        self.buf.position()
    }

    /// Advance a given number of bytes and return them.
    ///
    /// The internal buffer must be large enough to accomodate all bytes.
    ///
    /// ```rust
    /// use jomini::binary::{TokenReader, LexError, ReaderErrorKind};
    /// let mut reader = TokenReader::new(&b"EU4bin"[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4bin"[..]);
    /// assert!(matches!(reader.read_bytes(1).unwrap_err().kind(), ReaderErrorKind::Lexer(LexError::Eof)));
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
        while self.buf.window_len() < bytes {
            match self.buf.fill_buf(&mut self.reader) {
                Ok(0) => return Err(self.lex_error(LexError::Eof)),
                Ok(_) => {}
                Err(e) => return Err(self.buffer_error(e)),
            }
        }

        Ok(self.buf.split(bytes))
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
            let mut window = self.buf.window();
            while let Ok((id, data)) = read_id(window) {
                match id {
                    LexemeId::CLOSE => {
                        depth -= 1;
                        if depth == 0 {
                            self.buf.advance_to(data.as_ptr());
                            return Ok(());
                        }
                        window = data;
                    }
                    LexemeId::OPEN => {
                        window = data;
                        depth += 1
                    }
                    LexemeId::BOOL => match data.get(1..) {
                        Some(d) => window = d,
                        None => break,
                    },
                    LexemeId::F32 | LexemeId::U32 | LexemeId::I32 => match data.get(4..) {
                        Some(d) => window = d,
                        None => break,
                    },
                    LexemeId::F64 | LexemeId::I64 | LexemeId::U64 => match data.get(8..) {
                        Some(d) => window = d,
                        None => break,
                    },
                    LexemeId::QUOTED | LexemeId::UNQUOTED => match read_string(data) {
                        Ok((_, d)) => window = d,
                        Err(_) => break,
                    },
                    _ => window = data,
                }
            }

            self.buf.advance_to(window.as_ptr());
            match self.buf.fill_buf(&mut self.reader) {
                Ok(0) => return Err(self.lex_error(LexError::Eof)),
                Ok(_) => {}
                Err(e) => return Err(self.buffer_error(e)),
            }
        }
    }

    /// Consume the token reader and return the internal buffer and reader. This
    /// allows the buffer to be reused.
    ///
    /// ```rust
    /// use jomini::binary::TokenReader;
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
    pub fn into_parts(self) -> (Box<[u8]>, R) {
        (self.buf.buf, self.reader)
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
    /// assert!(matches!(reader.read().unwrap_err().kind(), ReaderErrorKind::Lexer(LexError::Eof)));
    /// ```
    #[inline]
    pub fn read(&mut self) -> Result<Token<'_>, ReaderError> {
        let s = std::ptr::addr_of!(self);
        self.next()?
            .ok_or_else(|| unsafe { s.read().lex_error(LexError::Eof) })
    }

    fn refill_next(&mut self) -> Result<Option<Token<'_>>, ReaderError> {
        match self.buf.fill_buf(&mut self.reader) {
            Ok(0) if self.buf.window_len() == 0 => Ok(None),
            Ok(0) => Err(self.lex_error(LexError::Eof)),
            Ok(_) => self.next(),
            Err(e) => Err(self.buffer_error(e)),
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
    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Option<Token<'_>>, ReaderError> {
        let window = unsafe { std::slice::from_raw_parts(self.buf.start, self.buf.window_len()) };
        match read_token(window) {
            Ok((tok, new_data)) => {
                self.buf.advance_to(new_data.as_ptr());
                Ok(Some(tok))
            }
            Err(LexError::Eof) => self.refill_next(),
            Err(e) => Err(self.lex_error(e)),
        }
    }

    #[cold]
    #[inline(never)]
    fn buffer_error(&self, e: BufferError) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::from(e),
        }
    }

    #[cold]
    #[inline(never)]
    fn lex_error(&self, e: LexError) -> ReaderError {
        ReaderError::from(e.at(self.position()))
    }
}

impl TokenReader<()> {
    /// Initializes a default [TokenReaderBuilder]
    pub fn builder() -> TokenReaderBuilder {
        TokenReaderBuilder::default()
    }
}

/// Creates a binary token reader
#[derive(Debug, Default)]
pub struct TokenReaderBuilder {
    buffer: BufferWindowBuilder,
}

impl TokenReaderBuilder {
    /// Set the fixed size buffer to the given buffer
    ///
    /// See [buffer_len](Self::buffer_len) for more information
    #[inline]
    pub fn buffer(mut self, val: Box<[u8]>) -> TokenReaderBuilder {
        self.buffer = self.buffer.buffer(val);
        self
    }

    /// Set the length of the buffer if no buffer is provided
    ///
    /// The size of the buffer must be large enough to decode an entire binary
    /// token, not just the contained binary data. For instance, for quoted
    /// scalars there are 4 bytes of additional data to the token (2 bytes for
    /// token discriminant and 2 to the string size).
    ///
    /// With how the binary format is laid out, a minimal buffer size that can
    /// handle all inputs can be derived
    ///
    /// ```rust
    /// use jomini::binary::TokenReader;
    /// let len = usize::from(u16::MAX) + 4;
    /// let reader = TokenReader::builder().buffer_len(len);
    /// # let _reader2 = reader;
    /// ```
    #[inline]
    pub fn buffer_len(mut self, val: usize) -> TokenReaderBuilder {
        self.buffer = self.buffer.buffer_len(val);
        self
    }

    /// Create a binary token reader around a given reader.
    #[inline]
    pub fn build<R>(self, reader: R) -> TokenReader<R> {
        let buf = self.buffer.build();
        TokenReader { reader, buf }
    }
}

/// The specific binary reader error type.
#[derive(Debug)]
pub enum ReaderErrorKind {
    /// An underlying error from a [Read]er
    Read(std::io::Error),

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferFull,

    /// The data is corrupted
    Lexer(LexError),
}

/// An binary lexing error over a `Read` implementation
#[derive(Debug)]
pub struct ReaderError {
    position: usize,
    kind: ReaderErrorKind,
}

impl ReaderError {
    /// Return the byte position where the error occurred
    pub fn position(&self) -> usize {
        self.position
    }

    /// Return a reference the error kind
    pub fn kind(&self) -> &ReaderErrorKind {
        &self.kind
    }

    /// Consume self and return the error kind
    #[must_use]
    pub fn into_kind(self) -> ReaderErrorKind {
        self.kind
    }
}

impl std::error::Error for ReaderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            ReaderErrorKind::Read(cause) => Some(cause),
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
            ReaderErrorKind::BufferFull => {
                write!(f, "max buffer size exceeded at position: {}", self.position)
            }
            ReaderErrorKind::Lexer(cause) => {
                write!(f, "{} at position: {}", cause, self.position)
            }
        }
    }
}

impl From<LexerError> for ReaderError {
    fn from(value: LexerError) -> Self {
        ReaderError {
            position: value.position(),
            kind: ReaderErrorKind::Lexer(value.into_kind()),
        }
    }
}

impl From<BufferError> for ReaderErrorKind {
    fn from(value: BufferError) -> Self {
        match value {
            BufferError::Io(x) => ReaderErrorKind::Read(x),
            BufferError::BufferFull => ReaderErrorKind::BufferFull,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Scalar, binary::Rgb};
    use rstest::*;

    #[rstest]
    #[case(&[
        Token::Id(0x2838),
        Token::Equal,
        Token::Open,
        Token::Id(0x2863),
        Token::Equal,
        Token::Unquoted(Scalar::new(b"western")),
        Token::Quoted(Scalar::new(b"1446.5.31")),
        Token::Equal,
        Token::Id(0x2838),
        Token::Close,
    ])]
    #[case(&[
        Token::Id(0x2ec9),
        Token::Equal,
        Token::Open,
        Token::Id(0x28e2),
        Token::Equal,
        Token::I32(1),
        Token::Id(0x28e3),
        Token::Equal,
        Token::I32(11),
        Token::Id(0x2ec7),
        Token::Equal,
        Token::I32(4),
        Token::Id(0x2ec8),
        Token::Equal,
        Token::I32(0),
        Token::Close,
    ])]
    #[case(&[
        Token::Id(0x053a),
        Token::Equal,
        Token::Rgb(Rgb {
            r: 110,
            g: 28,
            b: 27,
            a: None
        })
    ])]
    #[case(&[
        Token::Id(0x053a),
        Token::Equal,
        Token::Rgb(Rgb {
            r: 110,
            g: 28,
            b: 27,
            a: Some(128),
        })
    ])]
    #[case(&[
        Token::Id(0x326b), Token::Equal, Token::U64(128),
        Token::Id(0x326b), Token::Equal, Token::I64(-1),
        Token::Id(0x2d82), Token::Equal, Token::F64([0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
        Token::Id(0x2d82), Token::Equal, Token::F32([0x8f, 0xc2, 0x75, 0x3e]),
        Token::Id(0x2d82), Token::Equal, Token::U32(89)
    ])]
    fn test_roundtrip(#[case] input: &[Token]) {
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        for tok in input {
            tok.write(&mut writer).unwrap();
        }

        let data = writer.into_inner();

        // `Read`
        let mut reader = TokenReader::new(data.as_slice());
        for (i, e) in input.iter().enumerate() {
            assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
        }

        reader.read().unwrap_err();
        assert_eq!(reader.position(), data.len());

        // `from_slice`
        let mut reader = TokenReader::from_slice(data.as_slice());
        for (i, e) in input.iter().enumerate() {
            assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
        }

        reader.read().unwrap_err();
        assert_eq!(reader.position(), data.len());

        // reader buffer size
        for i in 30..40 {
            let mut reader = TokenReader::builder().buffer_len(i).build(data.as_slice());
            for e in input {
                assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
            }

            reader.read().unwrap_err();
            assert_eq!(reader.position(), data.len());
        }
    }

    #[test]
    fn test_not_enough_data() {
        let mut reader = TokenReader::new(&[0x43][..]);
        assert!(matches!(
            reader.read().unwrap_err().kind(),
            &ReaderErrorKind::Lexer(LexError::Eof)
        ));
    }
}
