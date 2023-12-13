use super::Operator;
use crate::{
    buffer::{BufferError, BufferWindow, BufferWindowBuilder},
    data::is_boundary,
    Scalar,
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
/// [TokenReader] operates over a fixed size buffer, so using a
/// [BufRead](std::io::BufRead) affords no benefits. An error will be returned
/// for tokens that are impossible to fit within the buffer (eg: if the provided
/// with 100 byte buffer but there is a binary string that is 101 bytes long).
#[derive(Debug)]
pub struct TokenReader<R> {
    reader: R,
    buf: BufferWindow,
    utf8: Utf8Bom,
}

impl<R> TokenReader<R>
where
    R: Read,
{
    /// Create a new text reader
    #[inline]
    pub fn new(reader: R) -> Self {
        TokenReader::builder().build(reader)
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
        self.buf.position()
    }

    #[inline(always)]
    unsafe fn next_opt(&mut self) -> (Option<Token>, Option<ReaderError>) {
        #[derive(Debug)]
        enum ParseState {
            None,
            Quote,
            Unquoted,
        }

        let mut state = ParseState::None;
        let mut ptr = self.buf.start;
        loop {
            let end = self.buf.end;
            let (carry_over, offset) = match state {
                ParseState::None => 'eof: loop {
                    if ptr == end {
                        break (0, 0);
                    }

                    'inner: loop {
                        match *ptr {
                            c @ b' ' | c @ b'\t' => {
                                ptr = ptr.add(1);
                                loop {
                                    if ptr == end {
                                        break 'eof (0, 0);
                                    }

                                    if *ptr != c {
                                        break;
                                    }

                                    ptr = ptr.add(1)
                                }
                            }
                            b'\n' | b'\r' | b';' => {
                                ptr = ptr.add(1);
                                break 'inner;
                            }
                            b'#' => {
                                let start_ptr = ptr;
                                ptr = ptr.add(1);
                                loop {
                                    if ptr == end {
                                        let carry_over = end.offset_from(start_ptr) as usize;
                                        break 'eof (carry_over, 0);
                                    }

                                    if *ptr == b'\n' {
                                        break;
                                    }

                                    ptr = ptr.add(1)
                                }
                            }
                            b'{' => {
                                self.buf.advance_to(ptr.add(1));
                                return (Some(Token::Open), None);
                            }
                            b'}' => {
                                self.buf.advance_to(ptr.add(1));
                                return (Some(Token::Close), None);
                            }
                            b']' => {
                                self.buf.advance_to(ptr.add(1));
                                return (Some(Token::Close), None);
                            }
                            b'[' => {
                                self.buf.advance_to(ptr.add(1));
                                return (Some(Token::Open), None);
                            }
                            b'"' => {
                                ptr = ptr.add(1);
                                let start_ptr = ptr;
                                loop {
                                    if ptr == end {
                                        state = ParseState::Quote;
                                        let carry_over = end.offset_from(start_ptr) as usize;
                                        break 'eof (carry_over, carry_over);
                                    }

                                    if *ptr == b'\\' {
                                        ptr = ptr.add(2);
                                        if ptr >= end {
                                            state = ParseState::Quote;
                                            let carry_over = end.offset_from(start_ptr) as usize;
                                            break 'eof (carry_over, carry_over.max(2) - 2);
                                        }
                                    } else if *ptr != b'"' {
                                        ptr = ptr.add(1);
                                    } else {
                                        self.buf.advance_to(ptr.add(1));
                                        let scalar = self.buf.get(start_ptr..ptr);
                                        return (Some(Token::Quoted(scalar)), None);
                                    }
                                }
                            }
                            b'@' => {
                                let start_ptr = ptr;
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr == b'[' {
                                    ptr = ptr.add(1);
                                    loop {
                                        if ptr == end {
                                            let carry_over = end.offset_from(start_ptr) as usize;
                                            break 'eof (carry_over, 0);
                                        } else if *ptr == b']' {
                                            ptr = ptr.add(1);
                                            self.buf.advance_to(ptr);
                                            let scalar = self.buf.get(start_ptr..ptr);
                                            return (Some(Token::Unquoted(scalar)), None);
                                        } else {
                                            ptr = ptr.add(1);
                                        }
                                    }
                                } else {
                                    loop {
                                        if ptr == end {
                                            let carry_over = end.offset_from(start_ptr) as usize;
                                            state = ParseState::Unquoted;
                                            break 'eof (carry_over, carry_over);
                                        } else if !is_boundary(*ptr) {
                                            ptr = ptr.add(1);
                                        } else {
                                            self.buf.advance_to(ptr);
                                            let scalar = self.buf.get(start_ptr..ptr);
                                            return (Some(Token::Unquoted(scalar)), None);
                                        }
                                    }
                                }
                            }
                            b'=' => {
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr != b'=' {
                                    self.buf.advance_to(ptr);
                                    return (Some(Token::Operator(Operator::Equal)), None);
                                } else {
                                    self.buf.advance_to(ptr.add(1));
                                    return (Some(Token::Operator(Operator::Exact)), None);
                                }
                            }
                            b'<' => {
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr != b'=' {
                                    self.buf.advance_to(ptr);
                                    return (Some(Token::Operator(Operator::LessThan)), None);
                                } else {
                                    self.buf.advance_to(ptr.add(1));
                                    return (Some(Token::Operator(Operator::LessThanEqual)), None);
                                }
                            }
                            b'!' => {
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr == b'=' {
                                    ptr = ptr.add(1);
                                }

                                self.buf.advance_to(ptr);
                                return (Some(Token::Operator(Operator::NotEqual)), None);
                            }
                            b'?' => {
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr == b'=' {
                                    ptr = ptr.add(1);
                                }

                                self.buf.advance_to(ptr);
                                return (Some(Token::Operator(Operator::Exists)), None);
                            }
                            b'>' => {
                                ptr = ptr.add(1);
                                if ptr == end {
                                    break 'eof (1, 0);
                                }

                                if *ptr != b'=' {
                                    self.buf.advance_to(ptr);
                                    return (Some(Token::Operator(Operator::GreaterThan)), None);
                                } else {
                                    self.buf.advance_to(ptr.add(1));
                                    return (
                                        Some(Token::Operator(Operator::GreaterThanEqual)),
                                        None,
                                    );
                                }
                            }
                            b'\xef' if matches!(self.utf8, Utf8Bom::Unknown) => {
                                match self.buf.window().get(..3) {
                                    Some([0xef, 0xbb, 0xbf]) => {
                                        self.utf8 = Utf8Bom::Present;
                                        ptr = ptr.add(3);
                                        break 'inner;
                                    }
                                    Some(_) => self.utf8 = Utf8Bom::NotPresent,
                                    None => break 'eof (self.buf.window_len(), 0),
                                }
                            }
                            _ => {
                                let start_ptr = ptr;
                                ptr = ptr.add(1);
                                loop {
                                    if ptr == end {
                                        state = ParseState::Unquoted;
                                        let carry_over = end.offset_from(start_ptr) as usize;
                                        break 'eof (carry_over, carry_over);
                                    } else if !is_boundary(*ptr) {
                                        ptr = ptr.add(1);
                                    } else {
                                        self.buf.advance_to(ptr);
                                        let scalar = self.buf.get(start_ptr..ptr);
                                        return (Some(Token::Unquoted(scalar)), None);
                                    }
                                }
                            }
                        }
                    }
                },
                ParseState::Quote { .. } => {
                    while ptr < end {
                        if *ptr == b'\\' {
                            ptr = ptr.add(2);
                        } else if *ptr != b'"' {
                            ptr = ptr.add(1);
                        } else {
                            self.buf.advance_to(ptr.add(1));
                            let scalar = self.buf.get(self.buf.buf.as_ptr()..ptr);
                            return (Some(Token::Quoted(scalar)), None);
                        }
                    }

                    // buffer or prior read too small
                    (self.buf.window_len(), self.buf.window_len())
                }
                ParseState::Unquoted { .. } => {
                    while ptr < end {
                        if !is_boundary(*ptr) {
                            ptr = ptr.add(1);
                        } else {
                            self.buf.advance_to(ptr);
                            let scalar = self.buf.get(self.buf.buf.as_ptr()..ptr);
                            return (Some(Token::Unquoted(scalar)), None);
                        }
                    }

                    // buffer or prior read too small
                    (self.buf.window_len(), self.buf.window_len())
                }
            };

            self.buf.advance_to(self.buf.end.sub(carry_over));
            match self.buf.fill_buf(&mut self.reader) {
                Ok(0) => match state {
                    ParseState::None => {
                        // if we carried over data that isn't a comment, we
                        // should have made forward progress.
                        if carry_over == 0 || *self.buf.start == b'#' {
                            return (None, None);
                        } else {
                            return (None, Some(self.eof_error()));
                        }
                    }
                    ParseState::Quote { .. } => return (None, Some(self.eof_error())),
                    ParseState::Unquoted { .. } => {
                        let scalar = std::slice::from_raw_parts(self.buf.start, carry_over);
                        self.buf.advance_to(self.buf.end);
                        return (Some(Token::Unquoted(Scalar::new(scalar))), None);
                    }
                },
                Ok(_) => ptr = self.buf.start.add(offset),
                Err(e) => return (None, Some(self.buffer_error(e))),
            }
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
        while self.buf.window_len() < bytes {
            match self.buf.fill_buf(&mut self.reader) {
                Ok(0) => return Err(self.eof_error()),
                Ok(_) => {}
                Err(e) => return Err(self.buffer_error(e)),
            }
        }

        let input = unsafe { std::slice::from_raw_parts(self.buf.start, bytes) };
        self.buf.advance(bytes);
        Ok(input)
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
        let mut ptr = self.buf.start;
        loop {
            let end = self.buf.end;
            unsafe {
                'refill: loop {
                    match state {
                        SkipState::None => 'new_state: loop {
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
                                        self.buf.advance_to(ptr);
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
                            if ptr >= end {
                                break 'refill;
                            }

                            if *ptr == b'\\' {
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

            self.buf.advance_to(self.buf.end);
            let overread = unsafe { ptr.offset_from(end) } as usize;
            match self.buf.fill_buf(&mut self.reader) {
                Ok(0) => return Err(self.eof_error()),
                Err(e) => return Err(self.buffer_error(e)),
                Ok(_) => ptr = unsafe { self.buf.start.add(overread) },
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
                let mut ptr = self.buf.start;
                let end = self.buf.end;

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
                            self.buf.advance_to(ptr.add(1));
                            return self.skip_container();
                        }
                        b' ' | b'\t' | b'\n' | b'\r' | b';' => {
                            ptr = ptr.add(1);
                        }
                        _ => return Ok(()),
                    }
                }

                self.buf.advance_to(end);
                match self.buf.fill_buf(&mut self.reader) {
                    Ok(0) => return Ok(()),
                    Err(e) => return Err(self.buffer_error(e)),
                    Ok(_) => {}
                }
            }
        }
    }

    /// Consume the token reader and return the internal buffer and reader. This
    /// allows the buffer to be reused.
    ///
    /// ```rust
    /// use jomini::text::{TokenReader};
    /// let data = b"EU4txt";
    /// let mut reader = TokenReader::new(&data[..]);
    /// assert_eq!(reader.read_bytes(6).unwrap(), &data[..]);
    ///
    /// let (buf, _) = reader.into_parts();
    /// let data = b"HOI4txt";
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
    /// use jomini::{Scalar, text::{TokenReader, Token, ReaderErrorKind, Operator}};
    /// let mut reader = TokenReader::new(&b"date=1444.11.11"[..]);
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"date")));
    /// assert_eq!(reader.read().unwrap(), Token::Operator(Operator::Equal));
    /// assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"1444.11.11")));
    /// assert!(matches!(reader.read().unwrap_err().kind(), ReaderErrorKind::Eof));
    /// ```
    #[inline(always)]
    pub fn read(&mut self) -> Result<Token, ReaderError> {
        // Workaround for borrow checker :(
        let s = unsafe { &mut *(self as *mut TokenReader<R>) };
        match unsafe { self.next_opt() } {
            (Some(x), _) => Ok(x),
            (None, None) => Err(s.eof_error()),
            (None, Some(e)) => Err(e),
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
    #[inline(always)]
    pub fn next(&mut self) -> Result<Option<Token>, ReaderError> {
        match unsafe { self.next_opt() } {
            (Some(x), _) => Ok(Some(x)),
            (None, None) => Ok(None),
            (None, Some(e)) => Err(e),
        }
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn eof_error(&self) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::Eof,
        }
    }

    #[cold]
    #[inline(always)]
    fn buffer_error(&self, e: BufferError) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::from(e),
        }
    }
}

impl TokenReader<()> {
    /// Initializes a default [TokenReaderBuilder]
    pub fn builder() -> TokenReaderBuilder {
        TokenReaderBuilder::default()
    }
}

/// Creates a text token reader
#[derive(Debug, Default)]
pub struct TokenReaderBuilder {
    buffer: BufferWindowBuilder,
}

impl TokenReaderBuilder {
    /// Set the fixed size buffer to the given buffer
    #[inline]
    pub fn buffer(mut self, val: Box<[u8]>) -> TokenReaderBuilder {
        self.buffer = self.buffer.buffer(val);
        self
    }

    /// Set the length of the buffer if no buffer is provided
    #[inline]
    pub fn buffer_len(mut self, val: usize) -> TokenReaderBuilder {
        self.buffer = self.buffer.buffer_len(val);
        self
    }

    /// Create a text token reader around a given reader.
    #[inline]
    pub fn build<R>(self, reader: R) -> TokenReader<R> {
        let buf = self.buffer.build();
        TokenReader {
            reader,
            buf,
            utf8: Utf8Bom::Unknown,
        }
    }
}

/// The specific text reader error type.
#[derive(Debug)]
pub enum ReaderErrorKind {
    /// An underlying error from a [Read]er
    Read(std::io::Error),

    /// The internal buffer does not have enough room to store data for the next
    /// token
    BufferFull,

    /// An early end of the data encountered
    Eof,
}

impl From<BufferError> for ReaderErrorKind {
    #[inline]
    fn from(value: BufferError) -> Self {
        match value {
            BufferError::Io(x) => ReaderErrorKind::Read(x),
            BufferError::BufferFull => ReaderErrorKind::BufferFull,
        }
    }
}

/// An text lexing error over a `Read` implementation
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
    fn test_input(#[case] input: &[u8], #[case] expected: &[Token]) {
        let mut reader = TokenReader::new(input);
        for (i, e) in expected.iter().enumerate() {
            assert_eq!(*e, reader.read().unwrap(), "failure at token idx: {}", i);
        }

        reader.read().unwrap_err();
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
            let mut reader = TokenReader::builder().buffer_len(i).build(input);
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
            let mut reader = TokenReader::builder().buffer_len(i).build(input);
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
}
