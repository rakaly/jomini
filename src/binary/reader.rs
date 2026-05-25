use super::{LexError, LexemeId, LexerError, ParserError, ParserSource, Token, lexer::read_rgb};
use crate::{
    Scalar,
    binary::{Rgb, lexer::TokenKind},
    util::get_split,
};
use std::{
    fmt,
    io::{self, Read},
    mem::MaybeUninit,
};

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
/// This is a much more raw view of the data that can be used to construct
/// higher level parsers, melters, and deserializers that operate over a stream
/// of data.
///
/// [TokenReader] operates over a fixed size buffer, so using a
/// [BufRead](std::io::BufRead) affords no benefits.
pub struct TokenReader<'a> {
    source: ParserSource<'a>,
    data: [u8; 8],
}

impl<'a> TokenReader<'a> {
    /// Convenience method for constructing the default token reader.
    #[inline]
    pub fn new<R>(reader: R) -> Self
    where
        R: Read + 'a,
    {
        TokenReader::from_source(ParserSource::from_reader(reader))
    }

    /// Construct a token reader with a caller-provided streaming buffer.
    ///
    /// ```
    /// use jomini::binary::{Token, TokenReader};
    ///
    /// let buffer = vec![0; 128];
    /// let mut reader = TokenReader::from_reader_with_buf(&[0xd2, 0x28][..], buffer);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// ```
    #[inline]
    pub fn from_reader_with_buf<R>(reader: R, buffer: Vec<u8>) -> Self
    where
        R: Read + 'a,
    {
        TokenReader::from_source(ParserSource::from_reader_with_buf(reader, buffer))
    }
}

impl<'a> TokenReader<'a> {
    /// Read from a byte slice without memcpy's.
    #[inline]
    pub fn from_slice(data: &'a [u8]) -> Self {
        TokenReader::from_source(ParserSource::from_slice(data))
    }

    /// Create a token reader from an existing parser source.
    #[inline]
    pub fn from_source(source: ParserSource<'a>) -> Self {
        TokenReader {
            source,
            data: [0; 8],
        }
    }

    /// Returns the byte position of the data stream that has been processed.
    ///
    /// ```
    /// use jomini::binary::{Token, TokenReader};
    ///
    /// let mut reader = TokenReader::new(&[0xd2, 0x28, 0xff][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.position(), 2);
    /// ```
    #[inline]
    pub fn position(&self) -> usize {
        self.source.position()
    }

    #[inline]
    fn ensure_bytes(&mut self, required: usize) -> Result<(), ReaderError> {
        self.source
            .ensure_bytes(required)
            .map_err(|e| self.parser_error(e))
    }

    /// Advance a given number of bytes and return them.
    ///
    /// The internal buffer must be large enough to accommodate all bytes.
    ///
    /// ```
    /// use jomini::binary::{LexError, ReaderErrorKind, TokenReader};
    ///
    /// let mut reader = TokenReader::new(&b"EU4bin"[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4bin"[..]);
    /// assert!(matches!(
    ///     reader.read_bytes(1).unwrap_err().kind(),
    ///     ReaderErrorKind::Lexer(LexError::Eof),
    /// ));
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
        // SAFETY: the source borrow ends with `take` on the error path
        let s = std::ptr::addr_of!(self);
        self.source
            .take_bytes(bytes)
            .map_err(|e| unsafe { s.read().parser_error(e) })
    }

    /// Advance through the containing block until the closing token is consumed.
    ///
    /// ```
    /// use jomini::binary::{Token, TokenReader};
    ///
    /// let mut reader = TokenReader::new(&[
    ///     0xd2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00,
    ///     0x04, 0x00, 0x04, 0x00, 0xff, 0xff,
    /// ][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.read().unwrap(), Token::Equal);
    /// assert_eq!(reader.read().unwrap(), Token::Open);
    /// reader.skip_container().unwrap();
    /// assert_eq!(reader.read().unwrap(), Token::Id(0xffff));
    /// ```
    #[inline]
    pub fn skip_container(&mut self) -> Result<(), ReaderError> {
        let mut depth = 1usize;
        loop {
            let token = self.read_token()?;
            match token {
                TokenKind::Close => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                TokenKind::Open => depth += 1,
                _ => {}
            }
        }
    }

    /// Read the next token in the stream. Will error if not enough data remains
    /// to decode a token.
    ///
    /// ```
    /// use jomini::binary::{LexError, ReaderErrorKind, Token, TokenReader};
    ///
    /// let mut reader = TokenReader::new(&[
    ///     0xd2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00,
    /// ][..]);
    /// assert_eq!(reader.read().unwrap(), Token::Id(0x28d2));
    /// assert_eq!(reader.read().unwrap(), Token::Equal);
    /// assert_eq!(reader.read().unwrap(), Token::Open);
    /// assert_eq!(reader.read().unwrap(), Token::Close);
    /// assert!(matches!(
    ///     reader.read().unwrap_err().kind(),
    ///     ReaderErrorKind::Lexer(LexError::Eof),
    /// ));
    /// ```
    #[inline]
    pub fn read(&mut self) -> Result<Token<'_>, ReaderError> {
        // SAFETY: borrow of `self` ends with `next()`
        let s = std::ptr::addr_of!(self);
        self.next()?
            .ok_or_else(|| unsafe { s.read().lex_error(LexError::Eof) })
    }

    /// Read a token, returning none when all the data has been consumed.
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
        match self.next_token()? {
            Some(kind) => Ok(Some(self.token_from_kind(kind))),
            None => Ok(None),
        }
    }

    /// Construct a [Token] from a [TokenKind] using stored data.
    #[inline]
    fn token_from_kind(&self, kind: TokenKind) -> Token<'_> {
        match kind {
            TokenKind::Open => Token::Open,
            TokenKind::Close => Token::Close,
            TokenKind::Equal => Token::Equal,
            TokenKind::U32 => Token::U32(self.u32_data()),
            TokenKind::U64 => Token::U64(self.u64_data()),
            TokenKind::I32 => Token::I32(self.i32_data()),
            TokenKind::Bool => Token::Bool(self.bool_data()),
            TokenKind::Quoted => Token::Quoted(unsafe { self.scalar_data() }),
            TokenKind::Unquoted => Token::Unquoted(unsafe { self.scalar_data() }),
            TokenKind::F32 => Token::F32(self.f32_data()),
            TokenKind::F64 => Token::F64(self.f64_data()),
            TokenKind::Rgb => Token::Rgb(self.rgb_data()),
            TokenKind::I64 => Token::I64(self.i64_data()),
            TokenKind::Lookup => Token::Lookup(self.lookup_data()),
            TokenKind::Id => Token::Id(self.token_id()),
        }
    }

    /// Returns the id for the most recently decoded token.
    #[inline]
    pub fn token_id(&self) -> u16 {
        u16::from_le_bytes([self.data[0], self.data[1]])
    }

    /// Returns scalar bytes for the most recently decoded scalar token.
    ///
    /// # Safety
    ///
    /// The most recent token must be [`TokenKind::Quoted`] or
    /// [`TokenKind::Unquoted`], and the underlying source must not have been
    /// advanced past the token's backing bytes except by `TokenReader`.
    #[inline]
    pub unsafe fn scalar_data(&self) -> Scalar<'_> {
        let len = u16::from_le_bytes([self.data[0], self.data[1]]) as usize;
        let data = unsafe { self.source.window().as_ptr().byte_sub(len) };
        Scalar::new(unsafe { std::slice::from_raw_parts(data, len) })
    }

    /// Returns `u64` data for the most recently decoded token.
    #[inline]
    pub fn u64_data(&self) -> u64 {
        u64::from_le_bytes(self.data)
    }

    /// Returns `i64` data for the most recently decoded token.
    #[inline]
    pub fn i64_data(&self) -> i64 {
        i64::from_le_bytes(self.data)
    }

    /// Returns raw `f64` bytes for the most recently decoded token.
    #[inline]
    pub fn f64_data(&self) -> [u8; 8] {
        self.data
    }

    /// Returns `u32` data for the most recently decoded token.
    #[inline]
    pub fn u32_data(&self) -> u32 {
        u32::from_le_bytes([self.data[0], self.data[1], self.data[2], self.data[3]])
    }

    /// Returns `i32` data for the most recently decoded token.
    #[inline]
    pub fn i32_data(&self) -> i32 {
        i32::from_le_bytes([self.data[0], self.data[1], self.data[2], self.data[3]])
    }

    /// Returns raw `f32` bytes for the most recently decoded token.
    #[inline]
    pub fn f32_data(&self) -> [u8; 4] {
        [self.data[0], self.data[1], self.data[2], self.data[3]]
    }

    /// Returns boolean data for the most recently decoded token.
    #[inline]
    pub fn bool_data(&self) -> bool {
        self.data[0] != 0
    }

    /// Returns lookup data for the most recently decoded token.
    #[inline]
    pub fn lookup_data(&self) -> u32 {
        u32::from_le_bytes([self.data[0], self.data[1], self.data[2], 0])
    }

    /// Returns RGB data for the most recently decoded token.
    ///
    /// # Safety
    ///
    /// It is undefined behavior if this method is called and the previous
    /// [`Self::next_token`] or [`Self::read_token`] did not return [`TokenKind::Rgb`].
    #[inline]
    pub fn rgb_data(&self) -> Rgb {
        let size = self.data[0] as usize;
        let data = unsafe { self.source.window().as_ptr().byte_sub(size) };
        let data = unsafe { std::slice::from_raw_parts(data, size) };
        let (result, _data) = read_rgb(data).expect("valid rgb data");
        result
    }

    #[inline]
    fn next_token_fast(&mut self, window: &[u8]) -> Option<TokenKind> {
        let (id, rest) = get_split::<2>(window).unwrap();
        let lexeme = LexemeId::new(u16::from_le_bytes(*id));
        match lexeme {
            LexemeId::OPEN => {
                self.source.advance(2);
                Some(TokenKind::Open)
            }
            LexemeId::CLOSE => {
                self.source.advance(2);
                Some(TokenKind::Close)
            }
            LexemeId::EQUAL => {
                self.source.advance(2);
                Some(TokenKind::Equal)
            }
            LexemeId::U32 | LexemeId::I32 | LexemeId::F32 => {
                self.data[..4].copy_from_slice(&rest[..4]);
                self.source.advance(6);
                if lexeme == LexemeId::F32 {
                    Some(TokenKind::F32)
                } else if lexeme == LexemeId::U32 {
                    Some(TokenKind::U32)
                } else {
                    Some(TokenKind::I32)
                }
            }
            LexemeId::U64 | LexemeId::I64 | LexemeId::F64 => {
                self.data[..8].copy_from_slice(&rest[..8]);
                self.source.advance(10);
                if lexeme == LexemeId::F64 {
                    Some(TokenKind::F64)
                } else if lexeme == LexemeId::U64 {
                    Some(TokenKind::U64)
                } else {
                    Some(TokenKind::I64)
                }
            }
            LexemeId::BOOL => {
                self.data[0] = rest[0];
                self.source.advance(3);
                Some(TokenKind::Bool)
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let (len_data, rest) = get_split::<2>(rest).unwrap();
                let len = u16::from_le_bytes(*len_data) as usize;
                rest.get(len..)?;
                self.data[0..2].copy_from_slice(len_data);
                self.source.advance(4 + len);
                if lexeme == LexemeId::UNQUOTED {
                    Some(TokenKind::Unquoted)
                } else {
                    Some(TokenKind::Quoted)
                }
            }
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                self.data = [0; 8];
                self.data[0] = rest[0];
                self.source.advance(3);
                Some(TokenKind::Lookup)
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                self.data = [0; 8];
                self.data[0..2].copy_from_slice(&rest[..2]);
                self.source.advance(4);
                Some(TokenKind::Lookup)
            }
            LexemeId::LOOKUP_U24 => {
                self.data = [0; 8];
                self.data[0..3].copy_from_slice(&rest[..3]);
                self.source.advance(5);
                Some(TokenKind::Lookup)
            }
            LexemeId::RGB => None,
            lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
                let offset = lexeme.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let byte_count = offset - (is_negative as u16 * 7);
                let mut buf = [0u8; 8];
                buf[..byte_count as usize].copy_from_slice(&rest[..byte_count as usize]);
                let sign = 1i64 - (is_negative as i64) * 2;
                self.data = (u64::from_le_bytes(buf) as i64 * sign).to_le_bytes();
                self.source.advance(2 + byte_count as usize);
                Some(TokenKind::F64)
            }
            _ => {
                self.data[..2].copy_from_slice(id);
                self.source.advance(2);
                Some(TokenKind::Id)
            }
        }
    }

    /// Reads the next token kind without constructing a borrowed [`Token`].
    #[inline]
    pub fn read_token(&mut self) -> Result<TokenKind, ReaderError> {
        match self.next_token()? {
            Some(t) => Ok(t),
            None => Err(self.lex_error(LexError::Eof)),
        }
    }

    fn next_token_slow(&mut self, id: [u8; 2]) -> Result<TokenKind, ReaderError> {
        let lexeme = LexemeId::new(u16::from_le_bytes(id));
        let kind = match lexeme {
            LexemeId::OPEN => {
                self.source.advance(2);
                TokenKind::Open
            }
            LexemeId::CLOSE => {
                self.source.advance(2);
                TokenKind::Close
            }
            LexemeId::EQUAL => {
                self.source.advance(2);
                TokenKind::Equal
            }
            LexemeId::U32 | LexemeId::I32 | LexemeId::F32 => {
                self.ensure_bytes(6)?;
                let data = unsafe { self.source.get_window_unchecked(6) };
                self.data[..4].copy_from_slice(&data[2..6]);
                self.source.advance(6);
                if lexeme == LexemeId::F32 {
                    TokenKind::F32
                } else if lexeme == LexemeId::U32 {
                    TokenKind::U32
                } else {
                    TokenKind::I32
                }
            }
            LexemeId::U64 | LexemeId::I64 | LexemeId::F64 => {
                self.ensure_bytes(10)?;
                let data = unsafe { self.source.get_window_unchecked(10) };
                self.data[..8].copy_from_slice(&data[2..10]);
                self.source.advance(10);
                if lexeme == LexemeId::F64 {
                    TokenKind::F64
                } else if lexeme == LexemeId::U64 {
                    TokenKind::U64
                } else {
                    TokenKind::I64
                }
            }
            LexemeId::BOOL => {
                self.ensure_bytes(3)?;
                let data = unsafe { self.source.get_window_unchecked(3) };
                self.data[0] = data[2];
                self.source.advance(3);
                TokenKind::Bool
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                self.ensure_bytes(4)?;
                let data = unsafe { self.source.get_window_unchecked(4) };
                let len_data = [data[2], data[3]];
                let len = u16::from_le_bytes(len_data) as usize;
                self.ensure_bytes(4 + len)?;
                self.data[0..2].copy_from_slice(&len_data);
                self.source.advance(4 + len);
                if lexeme == LexemeId::UNQUOTED {
                    TokenKind::Unquoted
                } else {
                    TokenKind::Quoted
                }
            }
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                self.ensure_bytes(3)?;
                let data = unsafe { self.source.get_window_unchecked(3) };
                self.data = [0; 8];
                self.data[0] = data[2];
                self.source.advance(3);
                TokenKind::Lookup
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                self.ensure_bytes(4)?;
                let data = unsafe { self.source.get_window_unchecked(4) };
                self.data = [0; 8];
                self.data[0..2].copy_from_slice(&data[2..4]);
                self.source.advance(4);
                TokenKind::Lookup
            }
            LexemeId::LOOKUP_U24 => {
                self.ensure_bytes(5)?;
                let data = unsafe { self.source.get_window_unchecked(5) };
                self.data = [0; 8];
                self.data[0..3].copy_from_slice(&data[2..5]);
                self.source.advance(5);
                TokenKind::Lookup
            }
            LexemeId::RGB => {
                self.ensure_bytes(24)?;
                let data = self.source.window();
                match read_rgb(&data[2..]) {
                    Ok((_rgb, rest)) => {
                        let size = rest.as_ptr() as usize - data[2..].as_ptr() as usize;
                        self.data[0] = size as u8;
                        self.source.advance(2 + size);
                        TokenKind::Rgb
                    }
                    Err(LexError::Eof) => {
                        self.ensure_bytes(30)?;
                        let data = self.source.window();
                        let (_rgb, rest) = read_rgb(&data[2..]).map_err(|e| self.lex_error(e))?;
                        let size = rest.as_ptr() as usize - data[2..].as_ptr() as usize;
                        self.data[0] = size as u8;
                        self.source.advance(2 + size);
                        TokenKind::Rgb
                    }
                    Err(e) => return Err(self.lex_error(e)),
                }
            }
            lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
                let offset = lexeme.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let byte_count = offset - (is_negative as u16 * 7);
                self.ensure_bytes(2 + byte_count as usize)?;
                let data = unsafe { self.source.get_window_unchecked(2 + byte_count as usize) };
                let mut buf = [0u8; 8];
                buf[..byte_count as usize].copy_from_slice(&data[2..]);
                let sign = 1i64 - (is_negative as i64) * 2;
                self.data = (u64::from_le_bytes(buf) as i64 * sign).to_le_bytes();
                self.source.advance(2 + byte_count as usize);
                TokenKind::F64
            }
            _ => {
                self.data[..2].copy_from_slice(&id);
                self.source.advance(2);
                TokenKind::Id
            }
        };

        Ok(kind)
    }

    /// Reads the next token kind, returning `None` when all data is consumed.
    #[inline]
    pub fn next_token(&mut self) -> Result<Option<TokenKind>, ReaderError> {
        // The raw-pointer reborrow side-steps the borrow checker. This is safe
        // as long as the "next_token_fast" doesn't cause a refill.
        let window = self.source.window();
        if window.len() >= 16
            && let Some(kind) = self.next_token_fast(unsafe { &*(window as *const [u8]) })
        {
            return Ok(Some(kind));
        }

        let id = match self.source.peek::<2>() {
            Ok(Some(id)) => *id,
            Ok(None) => return Ok(None),
            Err(e) => return Err(self.parser_error(e)),
        };
        self.next_token_slow(id).map(Some)
    }

    #[cold]
    #[inline(never)]
    fn parser_error(&self, e: ParserError) -> ReaderError {
        match e {
            ParserError::Io(io::ErrorKind::UnexpectedEof) => self.lex_error(LexError::Eof),
            ParserError::BufferTooSmall => {
                ReaderError::new(self.position(), ReaderErrorKind::BufferTooSmall)
            }
            ParserError::Io(kind) => ReaderError::new(self.position(), ReaderErrorKind::Read(kind)),
        }
    }

    #[cold]
    #[inline(never)]
    fn lex_error(&self, e: LexError) -> ReaderError {
        ReaderError::from(e.at(self.position()))
    }
}

/// The specific binary reader error type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReaderErrorKind {
    /// An underlying error from a [Read]er
    Read(std::io::ErrorKind),

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferTooSmall,

    /// The data is corrupted
    Lexer(LexError),
}

/// An binary lexing error over a `Read` implementation
pub struct ReaderError {
    // MaybeUninit strips all niche optimizations on Result<Option<TokenKind>,
    // ReaderError>. Without it, the compiler produces a niche-encoded 8-byte
    // Result that requires extra bit manipulation in hot loops (~2x instruction
    // count regression).
    kind: MaybeUninit<ReaderErrorKind>,
    position: u32,
}

impl ReaderError {
    #[inline]
    fn new(position: usize, kind: ReaderErrorKind) -> Self {
        ReaderError {
            kind: MaybeUninit::new(kind),
            position: position.min(u32::MAX as usize) as u32,
        }
    }

    /// Return the byte position where the error occurred
    pub fn position(&self) -> usize {
        self.position as usize
    }

    /// Return the error kind
    pub fn kind(&self) -> ReaderErrorKind {
        // SAFETY: kind is always initialized via new()
        unsafe { self.kind.assume_init() }
    }

    /// Consume self and return the error kind
    #[must_use]
    pub fn into_kind(self) -> ReaderErrorKind {
        // SAFETY: kind is always initialized via new()
        unsafe { self.kind.assume_init() }
    }
}

impl std::fmt::Debug for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ReaderError")
            .field("kind", &self.kind())
            .field("position", &self.position)
            .finish()
    }
}

impl std::error::Error for ReaderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl std::fmt::Display for ReaderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind() {
            ReaderErrorKind::Read(kind) => {
                write!(
                    f,
                    "failed to read past position: {}: {}",
                    self.position(),
                    kind
                )
            }
            ReaderErrorKind::BufferTooSmall => {
                write!(
                    f,
                    "token exceeds buffer capacity at position: {}",
                    self.position()
                )
            }
            ReaderErrorKind::Lexer(cause) => {
                write!(f, "{} at position: {}", cause, self.position())
            }
        }
    }
}

impl From<LexerError> for ReaderError {
    fn from(value: LexerError) -> Self {
        ReaderError::new(value.position(), ReaderErrorKind::Lexer(value.into_kind()))
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
    #[case(&[
        Token::Id(0x2d82),
        Token::Equal,
        Token::Lookup(0),
        Token::Id(0x2d82),
        Token::Equal,
        Token::Lookup(255),
        Token::Id(0x2d82),
        Token::Equal,
        Token::Lookup(0),
        Token::Id(0x2d82),
        Token::Equal,
        Token::Lookup(65535),
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
            let mut reader = TokenReader::from_reader_with_buf(data.as_slice(), vec![0; i]);
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
            ReaderErrorKind::Lexer(LexError::Eof)
        ));
    }

    #[test]
    fn reader_error_is_eight_bytes() {
        assert_eq!(std::mem::size_of::<ReaderError>(), 8);
        assert_eq!(
            ReaderError::new(123, ReaderErrorKind::Read(std::io::ErrorKind::Interrupted)).kind(),
            ReaderErrorKind::Read(std::io::ErrorKind::Interrupted)
        );
    }
}
