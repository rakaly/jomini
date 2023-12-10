use std::io::Read;

use super::Operator;
use crate::{data::is_boundary, Scalar};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    Open,
    Close,
    Operator(Operator),
    Unquoted(Scalar<'a>),
    Quoted(Scalar<'a>),
    // UndefinedParameter(Scalar<'a>),
}

impl<'a> Token<'a> {
    pub fn as_scalar(&self) -> Option<Scalar<'a>> {
        match self {
            Token::Quoted(s) | Token::Unquoted(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LexError<'a> {
    Eof(Option<Scalar<'a>>),
}

pub(crate) fn read_token(data: &[u8]) -> Result<(Token, &[u8]), LexError> {
    let rng = data.as_ptr_range();
    let mut ptr = rng.start;
    let end = rng.end;

    'eof: while ptr < end {
        'inner: loop {
            unsafe {
                match *ptr {
                    c @ b' ' | c @ b'\t' => {
                        ptr = ptr.add(1);
                        loop {
                            if ptr == end {
                                break 'eof;
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
                        ptr = ptr.add(1);
                        loop {
                            if ptr == end {
                                break 'eof;
                            }

                            if *ptr == b'\n' {
                                break;
                            }

                            ptr = ptr.add(1)
                        }
                    }
                    b'{' => {
                        ptr = ptr.add(1);
                        let rem_len = end.offset_from(ptr) as usize;
                        let rest = std::slice::from_raw_parts(ptr, rem_len);
                        return Ok((Token::Open, rest));
                    }
                    b'}' => {
                        ptr = ptr.add(1);
                        let rem_len = end.offset_from(ptr) as usize;
                        let rest = std::slice::from_raw_parts(ptr, rem_len);
                        return Ok((Token::Close, rest));
                    }
                    b']' => {}
                    b'"' => {
                        ptr = ptr.add(1);
                        let start_ptr = ptr;
                        loop {
                            if ptr >= end {
                                break 'eof;
                            }

                            if *ptr == b'\\' {
                                ptr = ptr.add(2);
                            } else if *ptr != b'"' {
                                ptr = ptr.add(1);
                            } else {
                                let tok_len = ptr.offset_from(start_ptr) as usize;
                                ptr = ptr.add(1);
                                let rem_len = end.offset_from(ptr) as usize;
                                let scalar =
                                    Scalar::new(std::slice::from_raw_parts(start_ptr, tok_len));
                                let rest = std::slice::from_raw_parts(ptr, rem_len);
                                return Ok((Token::Quoted(scalar), rest));
                            }
                        }
                    }
                    b'@' => {
                        todo!()
                    }
                    b'=' => {
                        ptr = ptr.add(1);
                        if ptr == end {
                            break 'eof;
                        }

                        if *ptr != b'=' {
                            let rem_len = end.offset_from(ptr) as usize;
                            let rest = std::slice::from_raw_parts(ptr, rem_len);
                            return Ok((Token::Operator(Operator::Equal), rest));
                        } else {
                            ptr = ptr.add(1);
                            let rem_len = end.offset_from(ptr) as usize;
                            let rest = std::slice::from_raw_parts(ptr, rem_len);
                            return Ok((Token::Operator(Operator::Exact), rest));
                        }
                    }
                    b'<' => todo!(),
                    b'!' => todo!(),
                    b'?' => todo!(),
                    b'>' => todo!(),
                    _ => {
                        let start_ptr = ptr;
                        ptr = ptr.add(1);
                        loop {
                            if ptr == end {
                                let tok_len = ptr.offset_from(start_ptr) as usize;
                                let scalar =
                                    Scalar::new(std::slice::from_raw_parts(start_ptr, tok_len));
                                return Err(LexError::Eof(Some(scalar)));
                            } else if !is_boundary(*ptr) {
                                ptr = ptr.add(1);
                            } else {
                                let tok_len = ptr.offset_from(start_ptr) as usize;
                                let rem_len = end.offset_from(ptr) as usize;
                                let scalar =
                                    Scalar::new(std::slice::from_raw_parts(start_ptr, tok_len));
                                let rest = std::slice::from_raw_parts(ptr, rem_len);
                                return Ok((Token::Unquoted(scalar), rest));
                            }
                        }
                    }
                }
            }
        }
    }

    Err(LexError::Eof(None))
}

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
    unsafe fn next_opt(&mut self) -> (Option<Token>, Option<ReaderError>) {
        #[derive(Debug)]
        enum ParseState {
            None,
            Quote,
            Unquoted,
        }

        let mut state = ParseState::None;
        let mut ptr = self.data;
        loop {
            let end = self.data_end;
            let (carry_over, offset) = match state {
                ParseState::None => {
                    'eof: loop {
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
                                    ptr = ptr.add(1);
                                    loop {
                                        if ptr == end {
                                            todo!();
                                            // break 'eof;
                                        }

                                        if *ptr == b'\n' {
                                            break;
                                        }

                                        ptr = ptr.add(1)
                                    }
                                }
                                b'{' => {
                                    self.data = ptr.add(1);
                                    return (Some(Token::Open), None);
                                }
                                b'}' => {
                                    self.data = ptr.add(1);
                                    return (Some(Token::Close), None);
                                }
                                b']' => todo!(),
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
                                                let carry_over =
                                                    end.offset_from(start_ptr) as usize;
                                                break 'eof (carry_over, carry_over.min(2) - 2);
                                            }
                                        } else if *ptr != b'"' {
                                            ptr = ptr.add(1);
                                        } else {
                                            let tok_len = ptr.offset_from(start_ptr) as usize;
                                            let scalar =
                                                std::slice::from_raw_parts(start_ptr, tok_len);
                                            self.data = ptr.add(1);
                                            return (
                                                Some(Token::Quoted(Scalar::new(scalar))),
                                                None,
                                            );
                                        }
                                    }
                                }
                                b'@' => {
                                    todo!()
                                }
                                b'=' => {
                                    ptr = ptr.add(1);
                                    if ptr == end {
                                        break 'eof (1, 0);
                                    }

                                    if *ptr != b'=' {
                                        self.data = ptr;
                                        return (Some(Token::Operator(Operator::Equal)), None);
                                    } else {
                                        self.data = ptr.add(1);
                                        return (Some(Token::Operator(Operator::Exact)), None);
                                    }
                                }
                                b'<' => {
                                    ptr = ptr.add(1);
                                    if ptr == end {
                                        break 'eof (1, 0);
                                    }

                                    if *ptr != b'=' {
                                        self.data = ptr;
                                        return (Some(Token::Operator(Operator::LessThan)), None);
                                    } else {
                                        self.data = ptr.add(1);
                                        return (Some(Token::Operator(Operator::LessThanEqual)), None);
                                    }
                                },
                                b'!' => {
                                    ptr = ptr.add(1);
                                    if ptr == end {
                                        break 'eof (1, 0);
                                    }

                                    if *ptr == b'=' {
                                        ptr = ptr.add(1);
                                    }

                                    self.data = ptr;
                                    return (Some(Token::Operator(Operator::NotEqual)), None);
                                },
                                b'?' => {
                                    ptr = ptr.add(1);
                                    if ptr == end {
                                        break 'eof (1, 0);
                                    }

                                    if *ptr == b'=' {
                                        ptr = ptr.add(1);
                                    }

                                    self.data = ptr;
                                    return (Some(Token::Operator(Operator::Exists)), None);
                                },
                                b'>' => {
                                    ptr = ptr.add(1);
                                    if ptr == end {
                                        break 'eof (1, 0);
                                    }

                                    if *ptr != b'=' {
                                        self.data = ptr;
                                        return (Some(Token::Operator(Operator::GreaterThan)), None);
                                    } else {
                                        self.data = ptr.add(1);
                                        return (Some(Token::Operator(Operator::GreaterThanEqual)), None);
                                    }
                                },
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
                                            let tok_len = ptr.offset_from(start_ptr) as usize;
                                            self.data = ptr;
                                            let scalar =
                                                std::slice::from_raw_parts(start_ptr, tok_len);
                                            return (
                                                Some(Token::Unquoted(Scalar::new(scalar))),
                                                None,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                ParseState::Quote { .. } => {
                    while ptr < end {
                        if *ptr == b'\\' {
                            ptr = ptr.add(2);
                        } else if *ptr != b'"' {
                            ptr = ptr.add(1);
                        } else {
                            let tok_len = ptr.offset_from(self.data) as usize;
                            let scalar = std::slice::from_raw_parts(self.data, tok_len);
                            self.data = ptr.add(1);
                            return (Some(Token::Quoted(Scalar::new(scalar))), None);
                        }
                    }

                    // buffer or prior read too small
                    (self.data_len(), self.data_len())
                }
                ParseState::Unquoted { .. } => {
                    while ptr < end {
                        if !is_boundary(*ptr) {
                            ptr = ptr.add(1);
                        } else {
                            let tok_len = ptr.offset_from(self.data) as usize;
                            let scalar = std::slice::from_raw_parts(self.data, tok_len);
                            self.data = ptr;
                            return (Some(Token::Unquoted(Scalar::new(scalar))), None);
                        }
                    }

                    // buffer or prior read too small
                    (self.data_len(), self.data_len())
                }
            };

            self.data_end
                .sub(carry_over)
                .copy_to(self.buf.as_mut_ptr(), carry_over);


            match self.reader.read(&mut self.buf[carry_over..]) {
                Ok(read) => {
                    if read > 0 {
                        self.prior_reads +=
                            (self.data_end.offset_from(self.buf.as_ptr()) as usize) - carry_over;
                        self.data_end = self.buf.as_ptr().add(carry_over + read);
                        self.data = self.buf.as_ptr();
                        ptr = self.data.add(offset);
                    } else if carry_over != self.buf.len() {
                        match state {
                            ParseState::None => return (None, None),
                            ParseState::Quote { .. } => {
                                self.data = ptr;
                                return (None, Some(self.eof_error()));
                            }
                            ParseState::Unquoted { .. } => {
                                self.prior_reads +=
                                    self.data_end.offset_from(self.buf.as_ptr()) as usize;
                                let scalar =
                                    std::slice::from_raw_parts(self.buf.as_ptr(), carry_over);
                                self.data = self.buf.as_ptr().add(carry_over);
                                self.data_end = self.data;
                                return (Some(Token::Unquoted(Scalar::new(scalar))), None);
                            }
                        }
                    } else {
                        // If we parsing didn't progress, there must be a token that spans
                        // multiple buffers, so we will need to grow the buffer.
                        todo!()
                    }
                }
                Err(e) => {
                    return (None, Some(self.io_error(e)));
                }
            }
        }
    }

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
                        return Err(self.eof_error());
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
    pub fn skip_container(&mut self) -> Result<(), ReaderError> {
        enum SkipState {
            None,
            Quote,
            Comment,
        }

        let mut state = SkipState::None;
        let mut depth = 1;
        let mut ptr = self.data;
        loop {
            let end = self.data_end;
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
                                        self.data = ptr;
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

            let overread = unsafe { ptr.offset_from(end) } as usize;
            match self.reader.read(&mut self.buf) {
                Ok(read) => {
                    if read > 0 {
                        self.prior_reads += self.consumed_data();
                        self.data_end = unsafe { self.buf.as_ptr().add(read) };
                        self.data = unsafe { self.buf.as_ptr().add(overread) };
                        ptr = self.data;
                    } else {
                        return Err(self.eof_error());
                    }
                }
                Err(e) => {
                    return Err(self.io_error(e));
                }
            }
        }
    }

    pub fn skip_unquoted_value(&mut self) -> Result<(), ReaderError> {
        Ok(())
    }

    #[inline]
    pub fn into_parts(self) -> (Vec<u8>, R) {
        (self.buf, self.reader)
    }

    #[cold]
    #[inline(never)]
    pub(crate) fn unlikely_read(&mut self) -> Result<Token, ReaderError> {
        self.read()
    }

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
    fn max_buffer_error(&self) -> ReaderError {
        ReaderError {
            position: self.position(),
            kind: ReaderErrorKind::MaxBufferReached,
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
    Eof,
}

#[derive(Debug)]
pub struct ReaderError {
    position: usize,
    kind: ReaderErrorKind,
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
        let (token, rest) = read_token(input).unwrap();
        assert_eq!(token, Token::Quoted(Scalar::new(b"hello world")));
        assert_eq!(rest, &[]);
    }

    #[test]
    fn test_terminating_scalar() {
        let er = read_token(b"  ab").unwrap_err();
        assert_eq!(er, LexError::Eof(Some(Scalar::new(b"ab"))));
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
    fn test_input(#[case] input: &[u8], #[case] expected: &[Token]) {
        let mut data = input;
        let mut reader = TokenReader::builder().build(input);
        for e in expected.iter() {
            let (actual, rest) = match read_token(data) {
                Ok((actual, rest)) => (actual, rest),
                Err(LexError::Eof(Some(s))) => (Token::Unquoted(s), &[][..]),
                x => panic!("{:?}", x),
            };

            assert_eq!(*e, actual);
            assert_eq!(*e, reader.read().unwrap());
            data = rest;
        }

        assert!(read_token(data).is_err());
        assert!(reader.read().is_err());
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
            let mut reader = TokenReader::builder().init_buffer_len(i).build(input);
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
            let mut reader = TokenReader::builder().init_buffer_len(i).build(input);
            reader.skip_container().unwrap();

            assert_eq!(reader.read().unwrap(), Token::Unquoted(Scalar::new(b"done")));
        }
    }
}
