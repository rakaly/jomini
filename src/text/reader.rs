use super::Operator;
use crate::{
    ParserError, ParserSource, Scalar,
    data::is_boundary,
    util::{contains_zero_byte, count_chunk, leading_whitespace, repeat_byte},
};
use std::io::Read;

/// Text token, the raw form of [TextToken](crate::text::TextToken)
///
/// This binary tokens contains the yielded raw tokens, and won't match open and
/// close tokens, nor does it make a determination if open and close represents
/// an array, object, or both.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    /// '{' or '['
    Open,

    /// '{' or ']'
    Close,

    /// An operator (eg: `foo=bar`)
    Operator(Operator),

    /// value that is not surrounded by quotes
    Unquoted(Scalar<'a>),

    /// value that is quoted
    Quoted(Scalar<'a>),
}

/// Kind of text token represented by a [TokenReader].
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    /// '{' or '['
    Open,

    /// '{' or ']'
    Close,

    /// An operator (eg: `foo=bar`)
    Operator(Operator),

    /// value that is not surrounded by quotes
    Unquoted,

    /// value that is quoted
    Quoted,
}

impl<'a> Token<'a> {
    /// Return as token as a scalar
    #[inline]
    pub fn as_scalar(&self) -> Option<Scalar<'a>> {
        match self {
            Token::Quoted(s) | Token::Unquoted(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Debug)]
enum Utf8Bom {
    Unknown,
    NotPresent,
    Present,
}

#[derive(Debug)]
enum ParseState {
    None,
    Quote,
    Unquoted,
}

/// Scan a [Read] implementation for text [Token]s
///
/// Example of computing the max nesting depth using a [TokenReader].
///
/// ```rust
/// use jomini::text::{TokenReader, Token};
/// let data = b"foo={{ id=3 } {} { id = 4 }}";
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
/// # Ok::<(), jomini::text::ReaderError>(())
/// ```
///
/// Unlike a [TextTape](crate::TextTape), which will skip ghost objects, pair
/// open and close tokens together, and recognize if a container is an object,
/// array, or mixed -- the tokens yielded from a [TokenReader] are not fully
/// formed. This is a much more raw view of the data that can be used to
/// construct higher level parsers and deserializers that operate over a stream
/// of data.
///
/// The [TokenReader] is considered **experimental**, as it uses a different
/// parsing algorithm geared towards parsing large save files. Ergonomic
/// equivalents for more esoteric game syntax (like parameter definitions) have
/// not yet been finalized. Game files can still be parsed with the experimental
/// APIs, but these APIs may change in the future based on feedback. Since the
/// binary format is not used for game files, the
/// [binary::TokenReader](crate::binary::TokenReader) is not considered
/// experimental)
///
/// [TokenReader] operates over a fixed size buffer, so using a
/// [BufRead](std::io::BufRead) affords no benefits. An error will be returned
/// for tokens that are impossible to fit within the buffer (eg: if the provided
/// with 100 byte buffer but there is a binary string that is 101 bytes long).
#[derive(Debug)]
pub struct TokenReader<'a> {
    source: ParserSource<'a>,
    utf8: Utf8Bom,
    scalar_start: usize,
    scalar_len: usize,
}

impl<'a> TokenReader<'a> {
    /// Read from a byte slice without memcpy's
    #[inline]
    pub fn from_slice(data: &'a [u8]) -> Self {
        Self::from_source(ParserSource::from_slice(data))
    }

    /// Create a new text reader
    #[inline]
    pub fn new<R>(reader: R) -> Self
    where
        R: Read + 'a,
    {
        Self::from_source(ParserSource::from_reader(reader))
    }

    /// Construct a text reader with a caller-provided streaming buffer.
    #[inline]
    pub fn from_reader_with_buf<R>(reader: R, buffer: Vec<u8>) -> Self
    where
        R: Read + 'a,
    {
        Self::from_source(ParserSource::from_reader_with_buf(reader, buffer))
    }

    /// Create a token reader from an existing parser source.
    #[inline]
    pub fn from_source(source: ParserSource<'a>) -> Self {
        TokenReader {
            source,
            utf8: Utf8Bom::Unknown,
            scalar_start: 0,
            scalar_len: 0,
        }
    }

    /// Returns the byte position of the data stream that has been processed.
    ///
    /// ```rust
    /// use jomini::{Scalar, text::{TokenReader, Token}};
    /// let mut reader = TokenReader::new(&b"date=1444.11.11"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"date")));
    /// assert_eq!(reader.position(), 4);
    /// ```
    #[inline]
    pub fn position(&self) -> usize {
        self.source.position()
    }

    #[inline]
    fn advance(&mut self, amt: usize) {
        let advanced = self.source.advance(amt);
        debug_assert!(advanced);
    }

    #[inline]
    unsafe fn advance_to(&mut self, window_start: *const u8, ptr: *const u8) {
        let amt = unsafe { ptr.offset_from(window_start) as usize };
        self.advance(amt);
    }

    #[inline]
    fn window_ptrs(&self) -> (*const u8, *const u8) {
        let window = self.source.as_slice();
        let start = window.as_ptr();
        (start, unsafe { start.add(window.len()) })
    }

    #[inline]
    fn set_scalar(&mut self, start: *const u8, end: *const u8) {
        debug_assert!(start <= end);
        self.scalar_start = start as usize;
        self.scalar_len = unsafe { end.offset_from(start) as usize };
    }

    #[inline]
    fn scalar_data(&self) -> Scalar<'_> {
        Scalar::new(unsafe {
            std::slice::from_raw_parts(self.scalar_start as *const u8, self.scalar_len)
        })
    }

    #[inline]
    fn token_from_kind(&self, kind: TokenKind) -> Token<'_> {
        match kind {
            TokenKind::Open => Token::Open,
            TokenKind::Close => Token::Close,
            TokenKind::Operator(op) => Token::Operator(op),
            TokenKind::Unquoted => Token::Unquoted(self.scalar_data()),
            TokenKind::Quoted => Token::Quoted(self.scalar_data()),
        }
    }

    #[cold]
    #[inline(always)]
    fn parser_error(&self, e: ParserError) -> ReaderError {
        ReaderError::new(self.position(), ReaderErrorKind::from(e))
    }

    unsafe fn next_kind_refill(
        &mut self,
        state: ParseState,
        carry_over: usize,
        offset: usize,
    ) -> Result<Option<TokenKind>, ReaderError> {
        unsafe {
            let (window_start, window_end) = self.window_ptrs();
            self.advance_to(window_start, window_end.sub(carry_over));
            match self.source.refill() {
                Ok(0) => match state {
                    ParseState::None => {
                        // if we carried over data that isn't a comment, we
                        // should have made forward progress.
                        if carry_over == 0 || self.source.as_slice().first() == Some(&b'#') {
                            self.advance(carry_over);
                            Ok(None)
                        } else {
                            Err(self.eof_error())
                        }
                    }
                    ParseState::Quote => Err(self.eof_error()),
                    ParseState::Unquoted => {
                        let (start, end) = self.window_ptrs();
                        self.advance_to(start, end);
                        self.set_scalar(start, end);
                        Ok(Some(TokenKind::Unquoted))
                    }
                },
                Ok(_) => match state {
                    ParseState::None => self.next_kind_fallback(),
                    ParseState::Quote => {
                        let (window_start, end) = self.window_ptrs();
                        let mut ptr = window_start.add(offset);

                        while ptr < end {
                            if *ptr == b'\\' {
                                let advance = end.offset_from(ptr).min(2);
                                ptr = ptr.offset(advance);
                            } else if *ptr != b'"' {
                                ptr = ptr.add(1);
                            } else {
                                self.advance_to(window_start, ptr.add(1));
                                self.set_scalar(window_start, ptr);
                                return Ok(Some(TokenKind::Quoted));
                            }
                        }

                        // buffer or prior read too small
                        let len = self.source.remaining();
                        self.next_kind_refill(ParseState::Quote, len, len)
                    }
                    ParseState::Unquoted => {
                        let (window_start, end) = self.window_ptrs();
                        let mut ptr = window_start.add(offset);
                        while ptr < end {
                            if !is_boundary(*ptr) {
                                ptr = ptr.add(1);
                            } else {
                                self.advance_to(window_start, ptr);
                                self.set_scalar(window_start, ptr);
                                return Ok(Some(TokenKind::Unquoted));
                            }
                        }

                        // buffer or prior read too small
                        let len = self.source.remaining();
                        self.next_kind_refill(ParseState::Unquoted, len, len)
                    }
                },
                Err(e) => Err(self.parser_error(e)),
            }
        }
    }

    unsafe fn next_kind_fallback(&mut self) -> Result<Option<TokenKind>, ReaderError> {
        unsafe {
            let (window_start, end) = self.window_ptrs();
            let mut ptr = window_start;

            loop {
                if ptr == end {
                    return self.next_kind_refill(ParseState::None, 0, 0);
                }

                match *ptr {
                    b' ' | b'\t' | b'\n' | b'\r' | b';' => ptr = ptr.add(1),
                    b'#' => {
                        let start_ptr = ptr;
                        loop {
                            ptr = ptr.add(1);
                            if ptr == end {
                                let carry_over = end.offset_from(start_ptr) as usize;
                                return self.next_kind_refill(ParseState::None, carry_over, 0);
                            } else if *ptr == b'\n' {
                                break;
                            }
                        }
                    }
                    b'{' => {
                        self.advance_to(window_start, ptr.add(1));
                        return Ok(Some(TokenKind::Open));
                    }
                    b'}' => {
                        self.advance_to(window_start, ptr.add(1));
                        return Ok(Some(TokenKind::Close));
                    }
                    b'"' => {
                        ptr = ptr.add(1);
                        let start_ptr = ptr;
                        loop {
                            if ptr == end {
                                let carry_over = end.offset_from(start_ptr) as usize;
                                return self.next_kind_refill(
                                    ParseState::Quote,
                                    carry_over,
                                    carry_over,
                                );
                            }

                            if *ptr == b'\\' {
                                let advance = end.offset_from(ptr).min(2);
                                ptr = ptr.offset(advance);
                                if ptr == end {
                                    let carry_over = end.offset_from(start_ptr) as usize;
                                    return self.next_kind_refill(
                                        ParseState::Quote,
                                        carry_over,
                                        carry_over.max(2) - 2,
                                    );
                                }
                            } else if *ptr != b'"' {
                                ptr = ptr.add(1);
                            } else {
                                self.advance_to(window_start, ptr.add(1));
                                self.set_scalar(start_ptr, ptr);
                                return Ok(Some(TokenKind::Quoted));
                            }
                        }
                    }
                    b'@' => {
                        let start_ptr = ptr;
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        }

                        if *ptr == b'[' {
                            ptr = ptr.add(1);
                            loop {
                                if ptr == end {
                                    let carry_over = end.offset_from(start_ptr) as usize;
                                    return self.next_kind_refill(ParseState::None, carry_over, 0);
                                } else if *ptr == b']' {
                                    ptr = ptr.add(1);
                                    self.advance_to(window_start, ptr);
                                    self.set_scalar(start_ptr, ptr);
                                    return Ok(Some(TokenKind::Unquoted));
                                } else {
                                    ptr = ptr.add(1);
                                }
                            }
                        } else {
                            loop {
                                if ptr == end {
                                    let carry_over = end.offset_from(start_ptr) as usize;
                                    return self.next_kind_refill(
                                        ParseState::Unquoted,
                                        carry_over,
                                        carry_over,
                                    );
                                } else if !is_boundary(*ptr) {
                                    ptr = ptr.add(1);
                                } else {
                                    self.advance_to(window_start, ptr);
                                    self.set_scalar(start_ptr, ptr);
                                    return Ok(Some(TokenKind::Unquoted));
                                }
                            }
                        }
                    }
                    b'=' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        } else if *ptr != b'=' {
                            self.advance_to(window_start, ptr);
                            return Ok(Some(TokenKind::Operator(Operator::Equal)));
                        } else {
                            self.advance_to(window_start, ptr.add(1));
                            return Ok(Some(TokenKind::Operator(Operator::Exact)));
                        }
                    }
                    b'<' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        } else if *ptr != b'=' {
                            self.advance_to(window_start, ptr);
                            return Ok(Some(TokenKind::Operator(Operator::LessThan)));
                        } else {
                            self.advance_to(window_start, ptr.add(1));
                            return Ok(Some(TokenKind::Operator(Operator::LessThanEqual)));
                        }
                    }
                    b'!' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        }

                        if *ptr == b'=' {
                            ptr = ptr.add(1);
                        }

                        self.advance_to(window_start, ptr);
                        return Ok(Some(TokenKind::Operator(Operator::NotEqual)));
                    }
                    b'?' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        }

                        if *ptr == b'=' {
                            ptr = ptr.add(1);
                        }

                        self.advance_to(window_start, ptr);
                        return Ok(Some(TokenKind::Operator(Operator::Exists)));
                    }
                    b'>' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            return self.next_kind_refill(ParseState::None, 1, 0);
                        }

                        if *ptr != b'=' {
                            self.advance_to(window_start, ptr);
                            return Ok(Some(TokenKind::Operator(Operator::GreaterThan)));
                        } else {
                            self.advance_to(window_start, ptr.add(1));
                            return Ok(Some(TokenKind::Operator(Operator::GreaterThanEqual)));
                        }
                    }
                    b'\xef' if matches!(self.utf8, Utf8Bom::Unknown) => {
                        match self.source.as_slice().get(..3) {
                            Some([0xef, 0xbb, 0xbf]) => {
                                self.utf8 = Utf8Bom::Present;
                                ptr = ptr.add(3);
                            }
                            Some(_) => self.utf8 = Utf8Bom::NotPresent,
                            None => {
                                return self.next_kind_refill(
                                    ParseState::None,
                                    self.source.remaining(),
                                    0,
                                );
                            }
                        }
                    }
                    _ => {
                        let start_ptr = ptr;
                        loop {
                            ptr = ptr.add(1);
                            if ptr == end {
                                let carry_over = end.offset_from(start_ptr) as usize;
                                return self.next_kind_refill(
                                    ParseState::Unquoted,
                                    carry_over,
                                    carry_over,
                                );
                            } else if is_boundary(*ptr) {
                                self.advance_to(window_start, ptr);
                                self.set_scalar(start_ptr, ptr);
                                return Ok(Some(TokenKind::Unquoted));
                            }
                        }
                    }
                }
            }
        }
    }

    #[inline]
    unsafe fn next_kind_opt(&mut self) -> Result<Option<TokenKind>, ReaderError> {
        unsafe {
            let (window_start, end) = self.window_ptrs();
            let mut ptr = window_start;

            if end.offset_from(ptr) < 9 {
                return self.next_kind_fallback();
            }

            // 3.4 million newlines followed by an average of 3.3 tabs
            let data = ptr.cast::<u64>().read_unaligned().to_le();
            ptr = ptr.add(leading_whitespace(data) as usize);

            // unquoted values are by far the most frequent token (~72% in
            // eu4 saves), so dispatch on them first. Loop unrolling minimizes
            // accesses to the boundary lookup table.
            if matches!(*ptr, b'a'..=b'z' | b'0'..=b'9' | b'A'..=b'Z' | b'-') {
                let start_ptr = ptr;
                let mut opt_ptr = start_ptr.add(1);
                while end.offset_from(opt_ptr) > 8 {
                    for _ in 0..8 {
                        if is_boundary(*opt_ptr) {
                            self.advance_to(window_start, opt_ptr);
                            self.set_scalar(start_ptr, opt_ptr);

                            // for space delimited arrays, advance one
                            if *opt_ptr == b' ' {
                                self.advance(1);
                            }

                            return Ok(Some(TokenKind::Unquoted));
                        }
                        opt_ptr = opt_ptr.add(1);
                    }
                }

                // optimization failed, fallback to inner parsing loop
            } else if *ptr == b'{' {
                self.advance_to(window_start, ptr.add(1));
                return Ok(Some(TokenKind::Open));
            } else if *ptr == b'}' {
                self.advance_to(window_start, ptr.add(1));
                return Ok(Some(TokenKind::Close));
            } else if *ptr == b'\"' {
                let start_ptr = ptr.add(1);
                let mut opt_ptr = start_ptr;
                let mut escaped = false;
                while end.offset_from(opt_ptr) > 8 {
                    let data = opt_ptr.cast::<u64>().read_unaligned().to_le();
                    escaped |= contains_zero_byte(data ^ repeat_byte(b'\\'));

                    // http://0x80.pl/notesen/2023-03-06-swar-find-any.html#faster-swar-procedure
                    let mask = repeat_byte(0x7f);
                    let lobits = data & mask;
                    let x0 = (lobits ^ repeat_byte(b'\"')) + mask;
                    let t0 = x0 | data;
                    let t1 = t0 & repeat_byte(0x80);
                    let t2 = t1 ^ repeat_byte(0x80);

                    if t2 != 0 {
                        let quote_ind = t2.trailing_zeros() >> 3;

                        if !escaped {
                            opt_ptr = opt_ptr.add(quote_ind as usize);
                            self.advance_to(window_start, opt_ptr.add(1));
                            self.set_scalar(start_ptr, opt_ptr);
                            return Ok(Some(TokenKind::Quoted));
                        } else {
                            break;
                        }
                    } else {
                        opt_ptr = opt_ptr.add(8);
                    }
                }

                // optimization failed, fallback to inner parsing loop
            } else if *ptr == b'=' {
                // `=` is the most common operator; handle the single-byte Equal
                // case inline so it doesn't round-trip through the fallback loop.
                let next = ptr.add(1);
                if next != end && *next != b'=' {
                    self.advance_to(window_start, next);
                    return Ok(Some(TokenKind::Operator(Operator::Equal)));
                }
                // `==` (Exact) or `=` at the window edge: defer to fallback
            }

            self.next_kind_fallback()
        }
    }

    /// Advance a given number of bytes and return them.
    ///
    /// The internal buffer must be large enough to accomodate all bytes.
    ///
    /// ```rust
    /// use jomini::text::{TokenReader, ReaderErrorKind};
    /// let mut reader = TokenReader::new(&b"EU4txt"[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &b"EU4txt"[..]);
    /// assert!(matches!(reader.read_bytes(1).unwrap_err().kind(), ReaderErrorKind::Eof));
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&[u8], ReaderError> {
        while self.source.remaining() < bytes {
            match self.source.refill() {
                Ok(0) => return Err(self.eof_error()),
                Ok(_) => {}
                Err(e) => return Err(self.parser_error(e)),
            }
        }

        match self.source.take(bytes) {
            Ok(data) => Ok(data),
            Err(_) => unreachable!("read_bytes ensured bytes are available"),
        }
    }

    /// Advance through the containing block until the closing token is consumed
    ///
    /// ```rust
    /// use jomini::{Scalar, text::{TokenReader, Token, Operator}};
    /// let mut reader = TokenReader::new(&b"foo={{bar={}}} qux=1"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"foo")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Open);
    /// assert!(reader.skip_container().is_ok());
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"qux")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"1")));
    /// ```
    #[inline]
    pub fn skip_container(&mut self) -> Result<(), ReaderError> {
        enum SkipState {
            None,
            Quote,
            Comment,
        }

        let mut state = SkipState::None;
        let mut depth = 1;
        let (mut window_start, mut end) = self.window_ptrs();
        let mut ptr = window_start;
        loop {
            unsafe {
                'refill: loop {
                    match state {
                        SkipState::None => 'new_state: loop {
                            while end.offset_from(ptr) > 8 {
                                // process 8 bytes at a time, which reduced
                                // latency of this function in EU4 saves by 50%
                                // (a 7% reduction overall).
                                let data = ptr.cast::<u64>().read_unaligned();
                                let has_quote = contains_zero_byte(data ^ repeat_byte(b'"'));
                                let has_comment = contains_zero_byte(data ^ repeat_byte(b'#'));
                                if has_quote || has_comment {
                                    break;
                                }

                                let has_close = contains_zero_byte(data ^ repeat_byte(b'}'));
                                let closes = if has_close {
                                    count_chunk(data, b'}') as i32
                                } else {
                                    0
                                };

                                let new_depth = depth - closes;
                                if new_depth < 1 {
                                    break;
                                }
                                depth = new_depth;

                                let has_open = contains_zero_byte(data ^ repeat_byte(b'{'));
                                let opens = if has_open {
                                    count_chunk(data, b'{') as i32
                                } else {
                                    0
                                };

                                depth += opens;
                                ptr = ptr.add(8);
                            }

                            if ptr == end {
                                break 'refill;
                            }

                            let val = *ptr;
                            ptr = ptr.add(1);
                            match val {
                                b'{' => depth += 1,
                                b'}' => {
                                    depth -= 1;
                                    if depth == 0 {
                                        self.advance_to(window_start, ptr);
                                        return Ok(());
                                    }
                                }
                                b'"' => {
                                    state = SkipState::Quote;
                                    break 'new_state;
                                }
                                b'#' => {
                                    state = SkipState::Comment;
                                    break 'new_state;
                                }
                                _ => {}
                            }
                        },
                        SkipState::Quote => loop {
                            if ptr == end {
                                break 'refill;
                            }

                            if *ptr == b'\\' {
                                if end.offset_from(ptr) <= 2 {
                                    break 'refill;
                                }
                                ptr = ptr.add(2);
                            } else if *ptr != b'"' {
                                ptr = ptr.add(1);
                            } else {
                                ptr = ptr.add(1);
                                state = SkipState::None;
                                break;
                            }
                        },
                        SkipState::Comment => loop {
                            if ptr == end {
                                break 'refill;
                            }

                            if *ptr == b'\n' {
                                ptr = ptr.add(1);
                                state = SkipState::None;
                                break;
                            }

                            ptr = ptr.add(1)
                        },
                    }
                }
            }

            unsafe {
                self.advance_to(window_start, ptr);
            }
            match self.source.refill() {
                Ok(0) => return Err(self.eof_error()),
                Err(e) => return Err(self.parser_error(e)),
                Ok(_) => {
                    (window_start, end) = self.window_ptrs();
                    ptr = window_start;
                }
            }
        }
    }

    /// Skip any trailing data associated with the unquoted value. Useful for
    /// skipping an unquoted value that may be serving as a header.
    ///
    /// In the below example the `rgb { 1 2 3 }` will first be parsed as
    /// unquoted `rgb`, but the `{ 1 2 3 }` needs to be skipped as well as it is
    /// tied to `rgb`.
    ///
    /// ```rust
    /// use jomini::{Scalar, text::{TokenReader, Token, Operator}};
    /// let mut reader = TokenReader::new(&b"color = rgb { 1 2 3 }  foo=bar"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"color")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"rgb")));
    /// assert!(reader.skip_unquoted_value().is_ok());
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"foo")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"bar")));
    /// ```
    #[inline]
    pub fn skip_unquoted_value(&mut self) -> Result<(), ReaderError> {
        loop {
            unsafe {
                let (window_start, end) = self.window_ptrs();
                let mut ptr = window_start;

                if end.offset_from(ptr) >= 4 {
                    let word = ptr.cast::<u32>().read_unaligned().to_le();

                    // 50% of EU4 values followed by this whitespace sequence
                    if word == 0x0909090A {
                        // \n\t\t\t
                        ptr = ptr.add(4);
                    }
                }

                while ptr < end {
                    match *ptr {
                        b'{' => {
                            self.advance_to(window_start, ptr.add(1));
                            return self.skip_container();
                        }
                        b' ' | b'\t' | b'\n' | b'\r' | b';' => {
                            ptr = ptr.add(1);
                        }
                        _ => return Ok(()),
                    }
                }

                self.advance_to(window_start, end);
                match self.source.refill() {
                    Ok(0) => return Ok(()),
                    Err(e) => return Err(self.parser_error(e)),
                    Ok(_) => {}
                }
            }
        }
    }

    /// Consume the token reader and return the parser source.
    #[inline]
    pub fn into_source(self) -> ParserSource<'a> {
        self.source
    }

    /// Read the next token in the stream. Will error if not enough data remains
    /// to decode a token.
    ///
    /// ```rust
    /// use jomini::{Scalar, text::{TokenReader, Token, ReaderErrorKind, Operator}};
    /// let mut reader = TokenReader::new(&b"date=1444.11.11"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"date")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"1444.11.11")));
    /// assert!(matches!(reader.read().unwrap_err().kind(), ReaderErrorKind::Eof));
    /// ```
    #[inline]
    pub fn read(&mut self) -> Result<Token<'_>, ReaderError> {
        let s = std::ptr::addr_of!(self);
        match self.next_token() {
            Ok(Some(x)) => Ok(unsafe { (*s).token_from_kind(x) }),
            Ok(None) => Err(unsafe { (*s).eof_error() }),
            Err(e) => Err(e),
        }
    }

    #[inline]
    pub(crate) fn read_expect_equals(&mut self) -> Result<Token<'_>, ReaderError> {
        match self.source.as_slice().first() {
            Some(b'=') => {
                self.advance(1);
                Ok(Token::Operator(Operator::Equal))
            }
            _ => self.read(),
        }
    }

    /// Read a token, returning none when all the data has been consumed
    ///
    /// ```rust
    /// use jomini::{Scalar, text::{TokenReader, Token, Operator}};
    /// let mut reader = TokenReader::new(&b"date=1444.11.11"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"date")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"1444.11.11")));
    /// assert_eq!(reader.next().unwrap(), None);
    /// ```
    #[inline]
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<Option<Token<'_>>, ReaderError> {
        let s = std::ptr::addr_of!(self);
        self.next_token()
            .map(|x| x.map(|kind| unsafe { (*s).token_from_kind(kind) }))
    }

    /// Read a token kind, returning none when all the data has been consumed.
    #[inline]
    pub fn next_token(&mut self) -> Result<Option<TokenKind>, ReaderError> {
        unsafe { self.next_kind_opt() }
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn eof_error(&self) -> ReaderError {
        ReaderError::new(self.position(), ReaderErrorKind::Eof)
    }
}

/// The specific text reader error type.
#[derive(Debug)]
pub enum ReaderErrorKind {
    /// An underlying error from a [Read]er
    Read,

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferTooSmall,

    /// An early end of the data encountered
    Eof,
}

impl From<ParserError> for ReaderErrorKind {
    #[inline]
    fn from(value: ParserError) -> Self {
        match value {
            ParserError::Eof => ReaderErrorKind::Eof,
            ParserError::BufferTooSmall => ReaderErrorKind::BufferTooSmall,
            ParserError::Io => ReaderErrorKind::Read,
        }
    }
}

/// An text lexing error over a `Read` implementation
#[derive(Debug)]
pub struct ReaderError {
    // Keep ReaderError to one word to minimize the impact of error handling on
    // hot paths. The top byte stores ReaderErrorKind and the lower 56 bits
    // store position.
    packed: u64,
}

impl std::fmt::Display for ReaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind() {
            ReaderErrorKind::Read => {
                write!(f, "failed to read past position: {}", self.position())
            }
            ReaderErrorKind::BufferTooSmall => {
                write!(
                    f,
                    "token exceeds buffer capacity at position: {}",
                    self.position()
                )
            }
            ReaderErrorKind::Eof => {
                write!(f, "unexpected end of file at position: {}", self.position())
            }
        }
    }
}

impl std::error::Error for ReaderError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl ReaderError {
    const KIND_SHIFT: u64 = 56;
    const POSITION_MASK: u64 = (1 << Self::KIND_SHIFT) - 1;

    #[inline]
    fn new(position: usize, kind: ReaderErrorKind) -> Self {
        ReaderError {
            packed: Self::pack(position, kind),
        }
    }

    #[inline]
    fn pack(position: usize, kind: ReaderErrorKind) -> u64 {
        ((kind.tag() as u64) << Self::KIND_SHIFT) | ((position as u64) & Self::POSITION_MASK)
    }

    /// Return the byte position where the error occurred
    pub fn position(&self) -> usize {
        (self.packed & Self::POSITION_MASK) as usize
    }

    /// Return the error kind
    pub fn kind(&self) -> ReaderErrorKind {
        ReaderErrorKind::from_tag((self.packed >> Self::KIND_SHIFT) as u8)
    }

    /// Consume self and return the error kind
    #[must_use]
    pub fn into_kind(self) -> ReaderErrorKind {
        self.kind()
    }
}

impl ReaderErrorKind {
    #[inline]
    const fn tag(self) -> u8 {
        match self {
            ReaderErrorKind::Read => 0,
            ReaderErrorKind::BufferTooSmall => 1,
            ReaderErrorKind::Eof => 2,
        }
    }

    #[inline]
    const fn from_tag(tag: u8) -> Self {
        match tag {
            0 => ReaderErrorKind::Read,
            1 => ReaderErrorKind::BufferTooSmall,
            2 => ReaderErrorKind::Eof,
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(b"\"hello world\"")]
    #[case(b" \"hello world\"")]
    #[case(b"  \"hello world\"")]
    #[case(b"\t\"hello world\"")]
    #[case(b"\t\t\"hello world\"")]
    #[case(b"\r\n\"hello world\"")]
    #[case(b"\r\n\r\n\"hello world\"")]
    #[case(b"\n\"hello world\"")]
    #[case(b"\n\n\"hello world\"")]
    #[case(b" ; \"hello world\"")]
    #[case(b" # good morning\n \"hello world\"")]
    #[case(b" # good morning\r\n \"hello world\"")]
    fn test_whitespace_quoted_scalar(#[case] input: &[u8]) {
        let mut reader = TokenReader::new(input);
        assert_eq!(
            reader.read().unwrap(),
            Token::Quoted(Scalar::new(b"hello world"))
        );
        assert!(reader.read().is_err());
    }

    #[rstest]
    #[case(b" a=b ", &[
        Token::Unquoted(Scalar::new(b"a")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"b")),
    ])]
    #[case(b" open={1 2}", &[
        Token::Unquoted(Scalar::new(b"open")),
        Token::Operator(Operator::Equal),
        Token::Open,
        Token::Unquoted(Scalar::new(b"1")),
        Token::Unquoted(Scalar::new(b"2")),
        Token::Close,
    ])]
    #[case(b"field1=-100.535 ", &[
        Token::Unquoted(Scalar::new(b"field1")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"-100.535")),
    ])]
    #[case(b"field1=-100.535", &[
        Token::Unquoted(Scalar::new(b"field1")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"-100.535")),
    ])]
    #[case(b"dlc_enabled={\n\t\"Cop\"\n\t\"WoN\"\n\t\"RP\"\n\t\"AoW\"\n\t\"ED\"\n}", &[
        Token::Unquoted(Scalar::new(b"dlc_enabled")),
        Token::Operator(Operator::Equal),
        Token::Open,
        Token::Quoted(Scalar::new(b"Cop")),
        Token::Quoted(Scalar::new(b"WoN")),
        Token::Quoted(Scalar::new(b"RP")),
        Token::Quoted(Scalar::new(b"AoW")),
        Token::Quoted(Scalar::new(b"ED")),
        Token::Close,
    ])]
    #[case(br#""foo"="bar" "3"="1444.11.11""#, &[
        Token::Quoted(Scalar::new(b"foo")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"bar")),
        Token::Quoted(Scalar::new(b"3")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"1444.11.11")),
    ])]
    #[case(br#""foo"="bar"3="1444.11.11""#, &[
        Token::Quoted(Scalar::new(b"foo")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"bar")),
        Token::Unquoted(Scalar::new(b"3")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"1444.11.11")),
    ])]
    #[case(br#"custom_name="THE !@#$%^&*( '\"LEGION\"')""#, &[
        Token::Unquoted(Scalar::new(b"custom_name")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(br#"THE !@#$%^&*( '\"LEGION\"')"#)),
    ])]
    // Preventative measures to ensure we don't regress on imperator color codes
    #[case(b"custom_name=\"ab \x15D ( ID: 691 )\x15!\"", &[
        Token::Unquoted(Scalar::new(b"custom_name")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"ab \x15D ( ID: 691 )\x15!")),
    ])]
    // test_no_equal_object_event
    #[case(b"foo{bar=qux}", &[
        Token::Unquoted(Scalar::new(b"foo")),
        Token::Open,
        Token::Unquoted(Scalar::new(b"bar")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"qux")),
        Token::Close,
    ])]
    // test_array_of_objects
    #[case(b"stats={{id=0 type=general} {id=1 type=admiral}}", &[
        Token::Unquoted(Scalar::new(b"stats")),
        Token::Operator(Operator::Equal),
        Token::Open,
        Token::Open,
        Token::Unquoted(Scalar::new(b"id")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"0")),
        Token::Unquoted(Scalar::new(b"type")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"general")),
        Token::Close,
        Token::Open,
        Token::Unquoted(Scalar::new(b"id")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"1")),
        Token::Unquoted(Scalar::new(b"type")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"admiral")),
        Token::Close,
        Token::Close,
    ])]
    // test_no_ws_comment
    #[case(b"foo=abc#def\nbar=qux", &[
        Token::Unquoted(Scalar::new(b"foo")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"abc")),
        Token::Unquoted(Scalar::new(b"bar")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"qux")),
    ])]
    // test_bom
    #[case(b"\xef\xbb\xbf#hello", &[])]
    // test_period_in_identifiers
    #[case(b"flavor_tur.8=yes", &[
        Token::Unquoted(Scalar::new(b"flavor_tur.8")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"yes")),
    ])]
    // test_dashed_identifiers From stellaris saves
    #[case(b"dashed-identifier=yes", &[
        Token::Unquoted(Scalar::new(b"dashed-identifier")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"yes")),
    ])]
    // test_colon_values
    #[case(b"province_id = event_target:agenda_province", &[
        Token::Unquoted(Scalar::new(b"province_id")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"event_target:agenda_province")),
    ])]
    // test_parameter_syntax_with_values
    // the new syntax to pass parameters to script values is explained in
    // stellaris: common/script_values/00_script_values.txt
    #[case(b"mult = value:job_weights_research_modifier|JOB|head_researcher|", &[
        Token::Unquoted(Scalar::new(b"mult")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(
            b"value:job_weights_research_modifier|JOB|head_researcher|"
        )),
    ])]
    // test_variables
    #[case(b"@planet_standard_scale = 11", &[
        Token::Unquoted(Scalar::new(b"@planet_standard_scale")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"11")),
    ])]
    // test_variables_value
    #[case(b"window_name = @default_window_name", &[
        Token::Unquoted(Scalar::new(b"window_name")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"@default_window_name")),
    ])]
    // test_interpolated_variable
    #[case(b"position = { @[1-leopard_x] @leopard_y }", &[
        Token::Unquoted(Scalar::new(b"position")),
        Token::Operator(Operator::Equal),
        Token::Open,
        Token::Unquoted(Scalar::new(b"@[1-leopard_x]")),
        Token::Unquoted(Scalar::new(b"@leopard_y")),
        Token::Close,
    ])]
    // test_unquoted_non_ascii More vic2 shenanigans
    #[case(b"jean_jaur\xe8s = bar ", &[
        Token::Unquoted(Scalar::new(b"jean_jaur\xe8s")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"bar")),
    ])]
    // test_skip_semicolon
    #[case(b"value=\"win\"; a=b", &[
        Token::Unquoted(Scalar::new(b"value")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"win")),
        Token::Unquoted(Scalar::new(b"a")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"b")),
    ])]
    // test_semicolon_as_whitespace
    #[case(b"foo = 0.3;", &[
        Token::Unquoted(Scalar::new(b"foo")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"0.3")),
    ])]
    // test_multiple_semicolons_as_whitespace
    #[case(b"a = 1; b = 2;; c = 3;", &[
        Token::Unquoted(Scalar::new(b"a")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"1")),
        Token::Unquoted(Scalar::new(b"b")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"2")),
        Token::Unquoted(Scalar::new(b"c")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"3")),
    ])]
    // test_consecutive_semicolons
    #[case(b";;;key = value;;;", &[
        Token::Unquoted(Scalar::new(b"key")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"value")),
    ])]
    fn test_input(#[case] input: &[u8], #[case] expected: &[Token]) {
        let mut reader = TokenReader::new(input);
        for (i, e) in expected.iter().enumerate() {
            assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
        }

        reader.read().unwrap_err();
        assert_eq!(reader.position(), input.len());

        let mut reader = TokenReader::from_slice(input);
        for (i, e) in expected.iter().enumerate() {
            assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
        }

        reader.read().unwrap_err();
        assert_eq!(reader.position(), input.len());
    }

    #[rstest]
    #[case(b"foo={ bar = \"baz\" @[1-qux] } # comment\nx?=1 y==2 z<=3")]
    #[case(b"\xef\xbb\xbf@var = value:job|head| \"escaped \\\" quote\"")]
    fn test_token_kind_matches_token(#[case] input: &[u8]) {
        fn kind(token: Token<'_>) -> TokenKind {
            match token {
                Token::Open => TokenKind::Open,
                Token::Close => TokenKind::Close,
                Token::Operator(op) => TokenKind::Operator(op),
                Token::Unquoted(_) => TokenKind::Unquoted,
                Token::Quoted(_) => TokenKind::Quoted,
            }
        }

        let mut token_reader = TokenReader::from_slice(input);
        let mut kind_reader = TokenReader::from_slice(input);

        loop {
            let token = token_reader.next().unwrap().map(kind);
            let token_kind = kind_reader.next_token().unwrap();
            assert_eq!(token, token_kind);

            if token.is_none() {
                break;
            }
        }
    }

    #[rstest]
    #[case(b"   hello=  butIsaytoYou", &[
        Token::Unquoted(Scalar::new(b"hello")),
        Token::Operator(Operator::Equal),
        Token::Unquoted(Scalar::new(b"butIsaytoYou")),
    ])]
    #[case(b"  \"lovely\"=  \"who is it\"", &[
        Token::Quoted(Scalar::new(b"lovely")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(b"who is it")),
    ])]
    #[case(br#"  "name"=  "\"jolly\" john""#, &[
        Token::Quoted(Scalar::new(b"name")),
        Token::Operator(Operator::Equal),
        Token::Quoted(Scalar::new(br#"\"jolly\" john"#)),
    ])]
    fn test_refill(#[case] input: &[u8], #[case] expected: &[Token]) {
        let min_buffer_size = expected
            .iter()
            .filter_map(|x| match x {
                Token::Unquoted(s) => Some(s.as_bytes().len()),
                Token::Quoted(s) => Some(s.as_bytes().len()),
                _ => None,
            })
            .max()
            .unwrap()
            + 1;

        for i in min_buffer_size..min_buffer_size + 10 {
            let mut reader = TokenReader::from_reader_with_buf(input, vec![0; i]);
            for e in expected.iter() {
                assert_eq!(*e, reader.read().unwrap());
            }

            assert!(reader.read().is_err());
        }
    }

    #[rstest]
    #[case(b"a=b c=d } done")]
    #[case(br#"a=alongervalue c=d } done"#)]
    #[case(br#"a="a long quoted value" c=d } done"#)]
    #[case(br#"a="a long \"quoted value\" with escapes" c=d } done"#)]
    #[case(br#"a={"an object" { "nested array" }} c=d } done"#)]
    fn test_skip_container(#[case] input: &[u8]) {
        for i in 8..16 {
            let mut reader = TokenReader::from_reader_with_buf(input, vec![0; i]);
            reader.skip_container().unwrap();

            assert_eq!(
                reader.read().unwrap(),
                Token::Unquoted(Scalar::new(b"done"))
            );
        }
    }

    #[rstest]
    #[case(b"\"\\")]
    fn test_crash_regression(#[case] input: &[u8]) {
        let mut reader = TokenReader::new(input);
        while let Ok(Some(_)) = reader.next() {}
    }

    #[test]
    fn reader_error_size() {
        assert_eq!(std::mem::size_of::<ReaderError>(), 8);
    }
}
