use super::{
    BinarySourceExt, LexError, LexemeId, LexerError, ParserError, ParserSource, Rgb, Token,
    lexer::read_rgb,
};
use crate::{ReaderError, ReaderErrorKind, Scalar};
use std::io::Read;

/// The size of an id and the widest payload of a constant size.
const CHUNK: usize = 10;

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
/// The data of a [Read] goes through a buffer of a fixed size, so using a
/// [BufRead](std::io::BufRead) affords no benefits. A slice is not buffered.
///
/// HoI4 saves since version 30 have an `F32` with a payload of eight bytes,
/// which the reader does not know.
#[derive(Debug)]
pub struct TokenReader<'a> {
    source: ParserSource<'a>,
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

    /// Construct a token reader with a caller-provided streaming buffer, whose
    /// capacity is the length of the vector. A token must fit in it.
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
        TokenReader { source }
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

    /// The source that the reader decodes tokens from.
    ///
    /// Between tokens, the source can be used to read lexemes directly with
    /// [`BinarySourceExt`](crate::binary::BinarySourceExt).
    #[inline]
    pub fn source_mut(&mut self) -> &mut ParserSource<'a> {
        &mut self.source
    }

    /// Consumes the reader and returns the source, positioned after the last
    /// token.
    #[inline]
    pub fn into_source(self) -> ParserSource<'a> {
        self.source
    }

    #[inline(always)]
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
    /// use jomini::binary::{ReaderErrorKind, TokenReader};
    ///
    /// let mut reader = TokenReader::new(&b"EU4bin"[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4bin"[..]);
    /// assert!(matches!(
    ///     reader.read_bytes(1).unwrap_err().kind(),
    ///     ReaderErrorKind::Eof,
    /// ));
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
        self.ensure_bytes(bytes)?;
        match self.source.take_bytes(bytes) {
            Ok(data) => Ok(data),
            Err(_) => unreachable!("the bytes are in the window"),
        }
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
        self.source.skip_value(LexemeId::OPEN)
    }

    /// Read the next token in the stream. Will error if not enough data remains
    /// to decode a token.
    ///
    /// ```
    /// use jomini::binary::{ReaderErrorKind, Token, TokenReader};
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
    ///     ReaderErrorKind::Eof,
    /// ));
    /// ```
    #[inline]
    pub fn read(&mut self) -> Result<Token<'_>, ReaderError> {
        // The position of a clean end is known before the token borrows `self`.
        let eof = self.position();
        match self.next()? {
            Some(token) => Ok(token),
            None => Err(ReaderError::new(eof, ReaderErrorKind::Eof)),
        }
    }

    /// Read a token, returning none when all the data has been consumed.
    ///
    /// A token that is cut off is an error that leaves the reader at the start
    /// of the token. After an error of the underlying reader, the reader is
    /// also there.
    ///
    /// The token borrows the reader, also when the data is a slice, so it ends
    /// before the next call. To collect the tokens of a slice, use a
    /// [`Lexer`](crate::binary::Lexer).
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
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Option<Token<'_>>, ReaderError> {
        // Nearly always the window holds an id and the widest payload of a
        // constant size, so that one check is enough for most tokens.
        let chunk = match self.source.window().first_chunk::<CHUNK>() {
            Some(chunk) => *chunk,
            None => match self.fill_chunk()? {
                Some(chunk) => chunk,
                None => return Ok(None),
            },
        };

        // Each arm advances by its own constant. The branch predictor then gives
        // the address of the next token, which else waits for the length.
        let [a, b, x @ ..] = chunk;
        let id = LexemeId::new(u16::from_le_bytes([a, b]));
        let token = match id {
            LexemeId::OPEN => {
                self.advance(2);
                Token::Open
            }
            LexemeId::CLOSE => {
                self.advance(2);
                Token::Close
            }
            LexemeId::EQUAL => {
                self.advance(2);
                Token::Equal
            }
            LexemeId::U32 => {
                self.advance(6);
                Token::U32(u32::from_le_bytes([x[0], x[1], x[2], x[3]]))
            }
            LexemeId::I32 => {
                self.advance(6);
                Token::I32(i32::from_le_bytes([x[0], x[1], x[2], x[3]]))
            }
            LexemeId::F32 => {
                self.advance(6);
                Token::F32([x[0], x[1], x[2], x[3]])
            }
            LexemeId::U64 => {
                self.advance(10);
                Token::U64(u64::from_le_bytes(x))
            }
            LexemeId::I64 => {
                self.advance(10);
                Token::I64(i64::from_le_bytes(x))
            }
            LexemeId::F64 => {
                self.advance(10);
                Token::F64(x)
            }
            LexemeId::BOOL => {
                self.advance(3);
                Token::Bool(x[0] != 0)
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED | LexemeId::STR_SAV03 => {
                let len = usize::from(u16::from_le_bytes([x[0], x[1]]));
                let data = Scalar::new(&self.read_bytes(4 + len)?[4..]);
                return Ok(Some(if id == LexemeId::QUOTED {
                    Token::Quoted(data)
                } else {
                    Token::Unquoted(data)
                }));
            }
            LexemeId::EMPTY_STRING => {
                self.advance(2);
                Token::Quoted(Scalar::new(b""))
            }
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT | LexemeId::LOOKUP_U8_SAV03 => {
                self.advance(3);
                Token::Lookup(u32::from(x[0]))
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT | LexemeId::LOOKUP_U16_SAV03 => {
                self.advance(4);
                Token::Lookup(u32::from(u16::from_le_bytes([x[0], x[1]])))
            }
            LexemeId::LOOKUP_U24 | LexemeId::LOOKUP_U24_ALT | LexemeId::LOOKUP_U24_SAV03 => {
                self.advance(5);
                Token::Lookup(u32::from_le_bytes([x[0], x[1], x[2], 0]))
            }
            LexemeId::LOOKUP_U32 | LexemeId::LOOKUP_U32_ALT | LexemeId::LOOKUP_U32_SAV03 => {
                self.advance(6);
                Token::Lookup(u32::from_le_bytes([x[0], x[1], x[2], x[3]]))
            }
            LexemeId::RGB => return self.take_rgb().map(|rgb| Some(Token::Rgb(rgb))),
            id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let len = usize::from(offset - u16::from(is_negative) * 7);
                // A mask takes the magnitude without a variable length copy.
                let magnitude = u64::from_le_bytes(x) & ((1 << (len * 8)) - 1);
                let sign = 1i64 - i64::from(is_negative) * 2;
                self.advance(2 + len);
                Token::F64((magnitude as i64 * sign).to_le_bytes())
            }
            id => {
                self.advance(2);
                Token::Id(id.0)
            }
        };

        Ok(Some(token))
    }

    /// Advance past bytes that are in the window.
    #[inline(always)]
    fn advance(&mut self, len: usize) {
        let advanced = self.source.advance(len);
        debug_assert!(advanced, "the bytes are in the window");
    }

    /// The bytes of the next token near the end of the window, with zeros after
    /// the data, or `None` at the end. The payload of a token is in the window
    /// after this if it has a constant size.
    #[cold]
    #[inline(never)]
    fn fill_chunk(&mut self) -> Result<Option<[u8; CHUNK]>, ReaderError> {
        let id = match self.source.peek_lexeme_id() {
            Ok(Some(id)) => id,
            Ok(None) => return Ok(None),
            Err(e) => return Err(self.parser_error(e)),
        };
        self.ensure_bytes(2 + id.payload_len())?;
        if matches!(
            id,
            LexemeId::QUOTED | LexemeId::UNQUOTED | LexemeId::STR_SAV03
        ) {
            self.ensure_bytes(4)?;
        }

        let window = self.source.window();
        let len = window.len().min(CHUNK);
        let mut chunk = [0; CHUNK];
        chunk[..len].copy_from_slice(&window[..len]);
        Ok(Some(chunk))
    }

    /// Takes an rgb lexeme: an id and a block of three or four channels.
    #[cold]
    #[inline(never)]
    fn take_rgb(&mut self) -> Result<Rgb, ReaderError> {
        self.ensure_bytes(24)?;
        let result = match read_rgb(&self.source.window()[2..]) {
            Err(LexError::Eof) => {
                self.ensure_bytes(30)?;
                read_rgb(&self.source.window()[2..])
            }
            result => result,
        };
        let available = self.source.window_len() - 2;
        let (rgb, size) = match result {
            Ok((rgb, rest)) => (rgb, available - rest.len()),
            Err(e) => return Err(self.lex_error(e)),
        };
        self.source.advance(2 + size);
        Ok(rgb)
    }

    #[cold]
    #[inline(never)]
    fn parser_error(&self, e: ParserError) -> ReaderError {
        e.at(self.position())
    }

    #[cold]
    #[inline(never)]
    fn lex_error(&self, e: LexError) -> ReaderError {
        ReaderError::from(e.at(self.position()))
    }
}

impl From<LexerError> for ReaderError {
    fn from(value: LexerError) -> Self {
        let pos = value.position();
        let kind = match value.into_kind() {
            LexError::Eof => ReaderErrorKind::Eof,
            LexError::InvalidRgb => ReaderErrorKind::InvalidRgb,
        };
        ReaderError::new(pos, kind)
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
            ReaderErrorKind::Eof
        ));
    }

    fn assert_tokens(data: &[u8], expected: &[Token]) {
        // slice reader
        let mut reader = TokenReader::new(data);
        for (i, e) in expected.iter().enumerate() {
            assert_eq!(reader.read().unwrap(), *e, "failure at token idx: {}", i);
        }
        reader.read().unwrap_err();
        assert_eq!(reader.position(), data.len());

        // from_slice reader
        let mut reader = TokenReader::from_slice(data);
        for (i, e) in expected.iter().enumerate() {
            assert_eq!(reader.read().unwrap(), *e, "failure at token idx: {}", i);
        }
        reader.read().unwrap_err();
        assert_eq!(reader.position(), data.len());

        // buffered reader across a range of buffer sizes
        for buf_size in 30..40 {
            let mut reader = TokenReader::from_reader_with_buf(data, vec![0; buf_size]);
            for (i, e) in expected.iter().enumerate() {
                assert_eq!(reader.read().unwrap(), *e, "failure at token idx: {}", i);
            }
            reader.read().unwrap_err();
            assert_eq!(reader.position(), data.len());
        }
    }

    #[test]
    fn test_longest_string_fits_in_default_buffer() {
        let text = vec![b'a'; usize::from(u16::MAX)];
        let mut data = vec![0x0f, 0x00, 0xff, 0xff];
        data.extend_from_slice(&text);
        let mut reader = TokenReader::new(data.as_slice());
        assert_eq!(reader.read().unwrap(), Token::Quoted(Scalar::new(&text)));
    }

    #[test]
    fn test_truncated_token_leaves_reader_at_its_start() {
        let data = [0x01, 0x00, 0x14, 0x00, 0x59];
        let mut reader = TokenReader::from_slice(&data);
        assert_eq!(reader.read().unwrap(), Token::Equal);
        let err = reader.read().unwrap_err();
        assert_eq!(err.kind(), ReaderErrorKind::Eof);
        assert_eq!(reader.position(), 2);
    }

    /// Every cut of the data, and every size of the buffer that holds a token,
    /// gives the same tokens as a lexer and stops at the start of the cut token.
    #[test]
    fn test_window_ends_match_lexer() {
        let mut data = Vec::new();
        for id in 0x0317..=0x0324u16 {
            data.extend(id.to_le_bytes());
            data.extend([0x81; 8]);
        }
        for id in (0x0d3e..=0x0d4fu16).chain(0x0d5a..=0x0d5d) {
            data.extend(id.to_le_bytes());
            data.extend([0x92; 8]);
        }
        for id in [0x0c, 0x0d, 0x0e, 0x14, 0x167, 0x29c, 0x317] {
            data.extend(u16::to_le_bytes(id));
            data.extend([0x73; 8]);
        }
        data.extend([0x0f, 0, 3, 0, b'a', b'b', b'c', 0x17, 0, 0, 0, 0x42, 0x0d]);

        let step = if cfg!(miri) { 17 } else { 1 };
        for cut in (0..=data.len()).step_by(step) {
            for capacity in [10, 11, 13, 24, 27, 64] {
                let data = &data[..cut];
                let mut lexer = crate::binary::Lexer::new(data);
                let mut reader = TokenReader::from_reader_with_buf(data, vec![0; capacity]);
                loop {
                    let position = lexer.position();
                    assert_eq!(reader.position(), position);
                    match (lexer.next_token(), reader.next()) {
                        (Ok(Some(a)), Ok(Some(b))) => assert_eq!(a, b),
                        (Ok(None), Ok(None)) => break,
                        (Err(_), Err(b)) => {
                            assert_eq!(b.position(), position);
                            break;
                        }
                        (a, b) => panic!("{a:?} {b:?}"),
                    }
                }
            }
        }
    }

    #[test]
    fn test_empty_string_token() {
        // The EU5 empty-string lexeme (0x0d42) carries no payload and decodes
        // to an empty quoted scalar.
        assert_tokens(&[0x42, 0x0d], &[Token::Quoted(Scalar::new(b""))]);
    }

    #[test]
    fn test_empty_string_then_lookup_stays_aligned() {
        // EMPTY_STRING (2 bytes, no payload) immediately followed by a
        // LOOKUP_U24(1) must keep the reader aligned.
        assert_tokens(
            &[0x42, 0x0d, 0x41, 0x0d, 0x01, 0x00, 0x00],
            &[Token::Quoted(Scalar::new(b"")), Token::Lookup(1)],
        );
    }

    #[test]
    fn test_lookup_u32_tokens() {
        // LOOKUP_U32 (0x0d3f): 4-byte little-endian index. The high byte (0x12)
        // exercises the full 32-bit width.
        assert_tokens(
            &[0x3f, 0x0d, 0x78, 0x56, 0x34, 0x12],
            &[Token::Lookup(0x1234_5678)],
        );
    }

    #[test]
    fn test_lookup_alt_tokens() {
        // The alt lookup variants decode to the same value as their non-alt
        // counterparts. LOOKUP_U24_ALT (0x0d45): 3 bytes, LOOKUP_U32_ALT
        // (0x0d46): 4 bytes.
        assert_tokens(
            &[0x45, 0x0d, 0x56, 0x34, 0x12],
            &[Token::Lookup(0x0012_3456)],
        );
        assert_tokens(
            &[0x46, 0x0d, 0xdd, 0xcc, 0xbb, 0xaa],
            &[Token::Lookup(0xaabb_ccdd)],
        );
    }

    #[test]
    fn test_sav03_lookup_and_string_tokens() {
        // SAV03 lookups use the same payload widths as the standard
        // lookup tokens. STR_SAV03 uses a u16 length and raw bytes.
        assert_tokens(
            &[
                0x5a, 0x0d, 0x01, // LOOKUP_U8_SAV03(1)
                0x5b, 0x0d, 0x02, 0x00, // LOOKUP_U16_SAV03(2)
                0x5c, 0x0d, 0x03, 0x00, 0x00, // LOOKUP_U24_SAV03(3)
                0x5d, 0x0d, 0x04, 0x00, 0x00, 0x00, // LOOKUP_U32_SAV03(4)
                0x5e, 0x0d, 0x07, 0x00, b'd', b'y', b'n', b'a', b'm', b'i', b'c',
            ],
            &[
                Token::Lookup(1),
                Token::Lookup(2),
                Token::Lookup(3),
                Token::Lookup(4),
                Token::Unquoted(Scalar::new(b"dynamic")),
            ],
        );
    }

    #[test]
    fn test_lookup_widths_stay_aligned() {
        // A run of every lookup width back-to-back must keep the reader aligned.
        assert_tokens(
            &[
                0x40, 0x0d, 0x01, // LOOKUP_U8(1)
                0x43, 0x0d, 0x02, // LOOKUP_U8_ALT(2)
                0x3e, 0x0d, 0x03, 0x00, // LOOKUP_U16(3)
                0x44, 0x0d, 0x04, 0x00, // LOOKUP_U16_ALT(4)
                0x41, 0x0d, 0x05, 0x00, 0x00, // LOOKUP_U24(5)
                0x45, 0x0d, 0x06, 0x00, 0x00, // LOOKUP_U24_ALT(6)
                0x3f, 0x0d, 0x07, 0x00, 0x00, 0x00, // LOOKUP_U32(7)
                0x46, 0x0d, 0x08, 0x00, 0x00, 0x00, // LOOKUP_U32_ALT(8)
            ],
            &[
                Token::Lookup(1),
                Token::Lookup(2),
                Token::Lookup(3),
                Token::Lookup(4),
                Token::Lookup(5),
                Token::Lookup(6),
                Token::Lookup(7),
                Token::Lookup(8),
            ],
        );
    }
}
