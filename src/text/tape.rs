use crate::{data::is_boundary, ObjectReader, Utf8Encoding, Windows1252Encoding};
use crate::{Error, ErrorKind, Scalar};
use std::fmt::Display;

/// An operator token
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Operator {
    /// A `<` token
    LessThan,

    /// A `<=` token
    LessThanEqual,

    /// A `>` token
    GreaterThan,

    /// A `>=` token
    GreaterThanEqual,

    /// A `!=` token
    NotEqual,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Operator::LessThan => f.write_str("<"),
            Operator::GreaterThan => f.write_str(">"),
            Operator::LessThanEqual => f.write_str("<="),
            Operator::GreaterThanEqual => f.write_str(">="),
            Operator::NotEqual => f.write_str("!="),
        }
    }
}

/// Represents a valid text value
#[derive(Debug, Clone, PartialEq)]
pub enum TextToken<'a> {
    /// Index of the `TextToken::End` that signifies this array's termination
    Array(usize),

    /// Index of the `TextToken::End` that signifies this objects's termination
    ///
    /// Typically in the tape the value immediately follows a key token. However,
    /// this is not guaranteed so always check if the end has been reached before
    /// trying to decode a value. There are two main situations where this is not
    /// guaranteed:
    ///
    /// - A non-equal operator (eg: `a > b` will be parsed to 3 instead of 2 tokens)
    /// - Array trailers (eg: `a = {10} 0 1 2`)
    Object(usize),

    /// Index of the `TextToken::End` that signifies this objects's termination
    ///
    /// A hidden object occurs where the first element is part of an array:
    ///
    /// ```ignore
    /// a = { 10 a=b c=d}
    /// ```
    ///
    /// In the above example, a and c would be part of the hidden object
    HiddenObject(usize),

    /// Extracted unquoted scalar value
    Unquoted(Scalar<'a>),

    /// Extracted quoted scalar value
    Quoted(Scalar<'a>),

    /// A parameter scalar
    ///
    /// Only seen so far in EU4. From the patch notes:
    ///
    /// > Scripted triggers or effects now support conditional compilation on arguments provided to them.
    /// > You can now check for if an argument is defined or not and make the script look entirely different based on that.
    /// > Syntax is `[[var_name] code here ]` for if variable is defined
    ///
    /// ```ignore
    /// generate_advisor = { [[scaled_skill] if = { } ] }
    /// ```
    Parameter(Scalar<'a>),

    /// An undefined parameter, see Parameter variant for more info.
    ///
    /// Syntax for undefined variable:
    ///
    /// ```ignore
    /// [[!var_name] code here ]
    /// ```
    UndefinedParameter(Scalar<'a>),

    /// A present, but non-equal operator token
    Operator(Operator),

    /// Index of the start of this object
    End(usize),

    /// The header token of the subsequent scalar. For instance, given
    ///
    /// ```ignore
    /// color = rgb { 100 200 50 }
    /// ```
    ///
    /// `rgb` would be a the header followed by a 3 element array
    Header(Scalar<'a>),
}

impl<'a> TextToken<'a> {
    /// Returns the scalar if the token contains a scalar
    ///
    /// ```
    /// use jomini::{Scalar, TextToken};
    /// assert_eq!(TextToken::Unquoted(Scalar::new(b"abc")).as_scalar(), Some(Scalar::new(b"abc")));
    /// assert_eq!(TextToken::Quoted(Scalar::new(b"abc")).as_scalar(), Some(Scalar::new(b"abc")));
    /// assert_eq!(TextToken::Header(Scalar::new(b"rgb")).as_scalar(), Some(Scalar::new(b"rgb")));
    /// assert_eq!(TextToken::Object(2).as_scalar(), None);
    /// ```
    pub fn as_scalar(&self) -> Option<Scalar<'a>> {
        match self {
            TextToken::Header(s)
            | TextToken::Unquoted(s)
            | TextToken::Quoted(s)
            | TextToken::Parameter(s)
            | TextToken::UndefinedParameter(s) => Some(*s),
            _ => None,
        }
    }
}

/// Creates a parser that a writes to a text tape
#[derive(Debug, Default)]
pub struct TextTapeParser;

impl TextTapeParser {
    /// Create a text parser
    pub fn new() -> Self {
        TextTapeParser::default()
    }

    /// Parse the text format and return the data tape
    pub fn parse_slice(self, data: &[u8]) -> Result<TextTape, Error> {
        let mut res = TextTape::default();
        self.parse_slice_into_tape(data, &mut res)?;
        Ok(res)
    }

    /// Parse the text format into the given tape.
    pub fn parse_slice_into_tape<'a>(
        self,
        data: &'a [u8],
        tape: &mut TextTape<'a>,
    ) -> Result<(), Error> {
        let token_tape = &mut tape.token_tape;
        token_tape.clear();
        token_tape.reserve(data.len() / 5);
        let mut state = ParserState {
            data,
            original_length: data.len(),
            token_tape,
            utf8_bom: false,
        };

        state.parse()?;
        tape.utf8_bom = state.utf8_bom;

        Ok(())
    }
}

struct ParserState<'a, 'b> {
    data: &'a [u8],
    original_length: usize,
    token_tape: &'b mut Vec<TextToken<'a>>,
    utf8_bom: bool,
}

/// Houses the tape of tokens that is extracted from plaintext data
#[derive(Debug, Default)]
pub struct TextTape<'a> {
    token_tape: Vec<TextToken<'a>>,
    utf8_bom: bool,
}

impl<'a> TextTape<'a> {
    /// Creates a windows 1252 object reader from the parsed tape
    pub fn windows1252_reader(&self) -> ObjectReader<Windows1252Encoding> {
        ObjectReader::new(&self, Windows1252Encoding::new())
    }

    /// Creates a utf-8 object reader from the parsed tape
    pub fn utf8_reader(&self) -> ObjectReader<Utf8Encoding> {
        ObjectReader::new(&self, Utf8Encoding::new())
    }
}

#[derive(Debug, PartialEq)]
enum ParseState {
    Key,
    KeyValueSeparator,
    ObjectValue,
    ArrayValue,
    ParseOpen,
    FirstValue,
}

/// I'm not smart enough to figure out the behavior of handling escape sequences when
/// when scanning multi-bytes, so this fallback is for when I was to reset and
/// process bytewise. It is much slower, but escaped strings should be rare enough
/// that this shouldn't be an issue
fn parse_quote_scalar_fallback(d: &[u8]) -> Result<(Scalar, &[u8]), Error> {
    let mut pos = 1;
    while pos < d.len() {
        if d[pos] == b'\\' {
            pos += 2;
        } else if d[pos] == b'"' {
            let scalar = Scalar::new(&d[1..pos]);
            return Ok((scalar, &d[pos + 1..]));
        } else {
            pos += 1;
        }
    }

    Err(Error::eof())
}

#[cfg(not(target_arch = "x86_64"))]
fn parse_quote_scalar(d: &[u8]) -> Result<(Scalar, &[u8]), Error> {
    use crate::util::{contains_zero_byte, repeat_byte};
    let sd = &d[1..];
    unsafe {
        let start_ptr = sd.as_ptr();
        let end_ptr = start_ptr.add(sd.len() / 8 * 8);

        let mut ptr = start_ptr;
        while ptr < end_ptr {
            let acc = (ptr as *const u64).read_unaligned();
            if contains_zero_byte(acc ^ repeat_byte(b'\\')) {
                break;
            } else if contains_zero_byte(acc ^ repeat_byte(b'"')) {
                while *ptr != b'"' {
                    ptr = ptr.offset(1);
                }

                let offset = sub(ptr, start_ptr);
                let (scalar, rest) = sd.split_at(offset);
                let s = Scalar::new(scalar);
                return Ok((s, &rest[1..]));
            }
            ptr = ptr.offset(8);
        }
    }

    parse_quote_scalar_fallback(d)
}

#[cfg(target_arch = "x86_64")]
fn parse_quote_scalar(d: &[u8]) -> Result<(Scalar, &[u8]), Error> {
    #[target_feature(enable = "sse2")]
    unsafe fn inner(d: &[u8]) -> Result<(Scalar, &[u8]), Error> {
        // This is a re-implementation of memchr for a few reasons:
        //   - We maintain zero dependencies
        //   - memchr is optimized for finding a needle in a large haystack so we don't need the
        //   following performance improvements from memchr:
        //     - avx2 (there's no perf difference between sse2 and avx2 for our input)
        //     - aligned loads (we use unaligned)
        //     - loop unrolling
        use core::arch::x86_64::*;
        let haystack = &d[1..];
        let start_ptr = haystack.as_ptr();
        let mut ptr = start_ptr;
        let loop_size = std::mem::size_of::<__m128i>();
        let end_ptr = haystack[haystack.len()..].as_ptr().sub(loop_size);
        let quote = _mm_set1_epi8(b'"' as i8);
        let slash = _mm_set1_epi8(b'\\' as i8);

        while ptr <= end_ptr {
            let reg = _mm_loadu_si128(ptr as *const __m128i);
            let slash_found = _mm_cmpeq_epi8(slash, reg);
            if _mm_movemask_epi8(slash_found) != 0 {
                break;
            }

            let quote_found = _mm_cmpeq_epi8(quote, reg);
            let mask = _mm_movemask_epi8(quote_found);
            if mask != 0 {
                let at = sub(ptr, start_ptr);
                let end_idx = at + (mask.trailing_zeros() as usize);
                let scalar = std::slice::from_raw_parts(start_ptr, end_idx);
                let scalar = Scalar::new(scalar);
                return Ok((scalar, &haystack[end_idx + 1..]));
            }

            ptr = ptr.add(loop_size);
        }

        parse_quote_scalar_fallback(d)
    }

    // from memchr: "SSE2 is avalbale on all x86_64 targets, so no CPU feature detection is necessary"
    unsafe { inner(&d) }
}

#[inline]
fn split_at_scalar_fallback(d: &[u8]) -> (Scalar, &[u8]) {
    let start_ptr = d.as_ptr();
    let end_ptr = unsafe { start_ptr.add(d.len()) };

    let nind = unsafe { forward_search(start_ptr, end_ptr, is_boundary) };
    let mut ind = nind.unwrap_or_else(|| d.len());

    // To work with cases where we have "==bar" we ensure that found index is at least one
    ind = std::cmp::max(ind, 1);
    let (scalar, rest) = d.split_at(ind);
    (Scalar::new(scalar), rest)
}

#[cfg(not(target_arch = "x86_64"))]
#[inline]
fn split_at_scalar(d: &[u8]) -> (Scalar, &[u8]) {
    split_at_scalar_fallback(d)
}

#[cfg(target_arch = "x86_64")]
#[inline]
fn split_at_scalar(d: &[u8]) -> (Scalar, &[u8]) {
    #[target_feature(enable = "sse2")]
    #[inline]
    #[allow(overflowing_literals)]
    unsafe fn inner(d: &[u8]) -> (Scalar, &[u8]) {
        use core::arch::x86_64::*;
        let start_ptr = d.as_ptr();
        let loop_size = std::mem::size_of::<__m128i>();
        let end_ptr = d[d.len()..].as_ptr().sub(loop_size);
        let mut ptr = start_ptr;

        // Here we use SIMD instructions to detect certain bytes.
        // The method used is described here:
        // http://0x80.pl/articles/simd-byte-lookup.html
        //
        // Loop partially auto-generated by the author's python code:
        // https://github.com/WojciechMula/simd-byte-lookup
        //
        // Interestingly the naive approach had the best performance.
        // To save myself some future time, here is the nibble diagram
        // of the characters that define a boundary character
        //
        //    lo / hi nibble
        //    +----------------- | ---------------
        //    | 0 1 2 3 4 5 6 7  | 8 9 a b c d e f
        //  --+----------------- | ---------------
        //  0 | . . x . . . . .  | . . . . . . . .
        //  1 | . . x . . . . .  | . . . . . . . .
        //  2 | . . . . . . . .  | . . . . . . . .
        //  3 | . . x . . . . .  | . . . . . . . .
        //  4 | . . . . . . . .  | . . . . . . . .
        //  5 | . . . . . . . .  | . . . . . . . .
        //  6 | . . . . . . . .  | . . . . . . . .
        //  7 | . . . . . . . .  | . . . . . . . .
        //  8 | . . . . . . . .  | . . . . . . . .
        //  9 | x . . . . . . .  | . . . . . . . .
        //  a | x . . . . . . .  | . . . . . . . .
        //  b | x . . . . x . x  | . . . . . . . .
        //  c | x . . x . . . .  | . . . . . . . .
        //  d | x . . x . x . x  | . . . . . . . .
        //  e | . . . x . . . .  | . . . . . . . .
        //  f | . . . . . . . .  | . . . . . . . .
        //
        // \t = 0x09
        // \n = 0x0a
        // \v = 0x0b *
        // \f = 0x0c *
        // \r = 0x0d
        // sp = 0x20
        //  ! = 0x21 *
        //  # = 0x23
        //  < = 0x3c
        //  = = 0x3d
        //  > = 0x3e
        //  [ = 0x5b
        //  ] = 0x5d
        //  { = 0x7b
        //  } = 0x7d
        // * = unknown if boundary character. Can be removed for perf
        while ptr <= end_ptr {
            let input = _mm_loadu_si128(ptr as *const __m128i);
            let t0 = _mm_cmpeq_epi8(input, _mm_set1_epi8(9));
            let mut result = t0;
            let t1 = _mm_cmpeq_epi8(input, _mm_set1_epi8(10));
            result = _mm_or_si128(result, t1);
            let t2 = _mm_cmpeq_epi8(input, _mm_set1_epi8(13));
            result = _mm_or_si128(result, t2);
            let t3 = _mm_cmpeq_epi8(input, _mm_set1_epi8(32));
            result = _mm_or_si128(result, t3);
            let t4 = _mm_cmpeq_epi8(input, _mm_set1_epi8(35));
            result = _mm_or_si128(result, t4);
            let t5 = _mm_cmpeq_epi8(input, _mm_set1_epi8(60));
            result = _mm_or_si128(result, t5);
            let t6 = _mm_cmpeq_epi8(input, _mm_set1_epi8(61));
            result = _mm_or_si128(result, t6);
            let t7 = _mm_cmpeq_epi8(input, _mm_set1_epi8(62));
            result = _mm_or_si128(result, t7);
            let t8 = _mm_cmpeq_epi8(input, _mm_set1_epi8(123));
            result = _mm_or_si128(result, t8);
            let t9 = _mm_cmpeq_epi8(input, _mm_set1_epi8(125));
            result = _mm_or_si128(result, t9);
            let t10 = _mm_cmpeq_epi8(input, _mm_set1_epi8(91));
            result = _mm_or_si128(result, t10);
            let t11 = _mm_cmpeq_epi8(input, _mm_set1_epi8(93));
            result = _mm_or_si128(result, t11);

            let found_mask = _mm_movemask_epi8(result);
            if found_mask != 0 {
                let at = sub(ptr, start_ptr);
                let end_idx = at + (found_mask.trailing_zeros() as usize);
                let end_idx = std::cmp::max(end_idx, 1);
                let scalar = std::slice::from_raw_parts(start_ptr, end_idx);
                let scalar = Scalar::new(scalar);
                return (scalar, &d[end_idx..]);
            }
            ptr = ptr.add(loop_size);
        }

        split_at_scalar_fallback(d)
    }

    // from memchr: "SSE2 is avalbale on all x86_64 targets, so no CPU feature detection is necessary"
    unsafe { inner(&d) }
}

impl<'a> TextTape<'a> {
    /// Creates a new text tape
    pub fn new() -> Self {
        Default::default()
    }

    /// Convenience method for creating a text parser and parsing the given input
    pub fn from_slice(data: &[u8]) -> Result<TextTape<'_>, Error> {
        TextTapeParser.parse_slice(data)
    }

    /// Returns a parser for text data
    pub fn parser() -> TextTapeParser {
        TextTapeParser
    }

    /// Return the parsed tokens
    pub fn tokens(&self) -> &[TextToken<'a>] {
        self.token_tape.as_slice()
    }

    /// Return if there was a UTF8 BOM in the data
    pub fn utf8_bom(&self) -> bool {
        self.utf8_bom
    }
}

impl<'a, 'b> ParserState<'a, 'b> {
    fn offset(&self, data: &[u8]) -> usize {
        self.original_length - data.len()
    }

    /// Skips whitespace that may terminate the file
    #[inline]
    fn skip_ws_t(&mut self, data: &'a [u8]) -> Option<&'a [u8]> {
        unsafe {
            let start_ptr = data.as_ptr();
            let end_ptr = start_ptr.add(data.len());

            let mut ptr = start_ptr;
            while ptr < end_ptr {
                match *ptr {
                    b' ' | b'\t' | b'\n' | b'\r' | b';' => {}
                    b'#' => {
                        ptr = ptr.offset(1);
                        while ptr < end_ptr && *ptr != b'\n' {
                            ptr = ptr.offset(1);
                        }
                    }
                    _ => {
                        let rest = std::slice::from_raw_parts(ptr, sub(end_ptr, ptr));
                        return Some(rest);
                    }
                }
                ptr = ptr.offset(1);
            }
        }

        None
    }

    #[inline]
    fn parse_quote_scalar(&mut self, d: &'a [u8]) -> Result<&'a [u8], Error> {
        let (scalar, rest) = parse_quote_scalar(d)?;
        self.token_tape.push(TextToken::Quoted(scalar));
        Ok(rest)
    }

    #[inline(never)]
    fn parse_variable(&mut self, d: &'a [u8]) -> Result<&'a [u8], Error> {
        // detect if the variable is interpolated
        if d.get(1).map_or(false, |&x| x == b'[') {
            let mut pos = 2;
            while pos < d.len() {
                if d[pos] == b']' {
                    let (scalar, rest) = d.split_at(pos + 1);
                    let scalar = Scalar::new(scalar);
                    self.token_tape.push(TextToken::Unquoted(scalar));
                    return Ok(rest);
                } else {
                    pos += 1;
                }
            }

            Err(Error::eof())
        } else {
            let (scalar, rest) = split_at_scalar(d);
            self.token_tape.push(TextToken::Unquoted(scalar));
            Ok(rest)
        }
    }

    #[inline]
    fn parse_scalar(&mut self, d: &'a [u8]) -> &'a [u8] {
        let (scalar, rest) = split_at_scalar(d);
        self.token_tape.push(TextToken::Unquoted(scalar));
        rest
    }

    #[inline]
    fn parse_key_value_separator(&mut self, d: &'a [u8]) -> &'a [u8] {
        // Most key values are separated by an equal sign but there are some fields like
        // map_area_data that does not have a separator.
        //
        // ```
        // map_area_data{
        //   brittany_area={
        //   # ...
        // ```
        //
        // Additionally it's possible for there to be heterogenus objects:
        //
        // ```
        // brittany_area = { color = { 10 10 10 } 100 200 300 }
        // ```
        //
        // These are especially tricky, but essentially this function's job is to skip the equal
        // token (the 99.9% typical case) if possible.
        if d[0] == b'=' {
            &d[1..]
        } else {
            self.parse_key_value_separator_unlikely(d)
        }
    }

    #[inline(never)]
    fn parse_key_value_separator_unlikely(&mut self, d: &'a [u8]) -> &'a [u8] {
        if d[0] == b'<' {
            if d.get(1).map_or(false, |c| *c == b'=') {
                self.token_tape
                    .push(TextToken::Operator(Operator::LessThanEqual));
                &d[2..]
            } else {
                self.token_tape
                    .push(TextToken::Operator(Operator::LessThan));
                &d[1..]
            }
        } else if d[0] == b'>' {
            if d.get(1).map_or(false, |c| *c == b'=') {
                self.token_tape
                    .push(TextToken::Operator(Operator::GreaterThanEqual));
                &d[2..]
            } else {
                self.token_tape
                    .push(TextToken::Operator(Operator::GreaterThan));
                &d[1..]
            }
        } else if d[0] == b'!' && d.get(1).map_or(false, |c| *c == b'=') {
            self.token_tape
                .push(TextToken::Operator(Operator::NotEqual));
            &d[2..]
        } else {
            d
        }
    }

    /// Clear previously parsed data and parse the given data
    #[inline]
    pub fn parse(&mut self) -> Result<(), Error> {
        let mut data = self.data;
        let mut state = ParseState::Key;

        self.utf8_bom = data.get(..3).map_or(false, |x| x == [0xef, 0xbb, 0xbf]);
        if self.utf8_bom {
            data = &data[3..];
        }

        // This variable keeps track of outer array when we're parsing a hidden object.
        // A hidden object textually looks like:
        //     levels={ 10 0=2 1=2 }
        // which we will translate into
        //     levels={ 10 { 0=2 1=2 } }
        // with the help of this variable. As when we'll only see one END token to signify
        // both the end of the array and object, but we'll produce two TextToken::End.
        let mut array_ind_of_hidden_obj = None;

        // Records if an object key does not have an operator for detecting objecty trailers:
        // brittany_area = { color = { 10 10 10 } 100 200 300 }
        let mut lack_operator = false;
        let mut in_trailer = false;

        let mut parent_ind = 0;
        loop {
            let d = match self.skip_ws_t(data) {
                Some(d) => d,
                None => {
                    if state != ParseState::Key {
                        return Err(Error::eof());
                    }

                    if parent_ind == 0 {
                        return Ok(());
                    } else {
                        // Support for files that don't have enough closing brackets (ugh)
                        let grand_ind = match self.token_tape.get(parent_ind) {
                            Some(TextToken::Array(x)) => *x,
                            Some(TextToken::Object(x)) => *x,
                            _ => 0,
                        };

                        if grand_ind == 0 {
                            let end_idx = self.token_tape.len();
                            self.token_tape.push(TextToken::End(parent_ind));
                            self.token_tape[parent_ind] = TextToken::Object(end_idx);
                            return Ok(());
                        } else {
                            return Err(Error::eof());
                        }
                    }
                }
            };

            data = d;
            match state {
                ParseState::Key => {
                    match data[0] {
                        b'}' | b']' => {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(x)) => *x,
                                Some(TextToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            let end_idx = self.token_tape.len();
                            if parent_ind == 0 && grand_ind == 0 {
                                // Allow extraneous close braces to support malformatted game files (ugh)
                                data = &data[1..];
                                continue;
                            }

                            self.token_tape.push(TextToken::End(parent_ind));
                            if let Some(array_ind) = array_ind_of_hidden_obj.take() {
                                self.token_tape[parent_ind] = TextToken::HiddenObject(end_idx);

                                let end_idx = self.token_tape.len();
                                self.token_tape.push(TextToken::End(array_ind));

                                // Grab the grand parent from the outer array. Even though the logic should
                                // be more strict (ie: throwing an error when if the parent array index doesn't exist,
                                // or if the parent doesn't exist), but since hidden objects are such a rather rare
                                // occurrence, it's better to be flexible
                                let grand_ind =
                                    if let Some(parent) = self.token_tape.get_mut(array_ind) {
                                        let grand_ind = match parent {
                                            TextToken::Array(x) => *x,
                                            _ => 0,
                                        };
                                        *parent = TextToken::Array(end_idx);
                                        grand_ind
                                    } else {
                                        0
                                    };

                                state = match self.token_tape.get(grand_ind) {
                                    Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                    Some(TextToken::Object(_x)) => ParseState::Key,
                                    _ => ParseState::Key,
                                };
                                parent_ind = grand_ind;
                            } else {
                                self.token_tape[parent_ind] = TextToken::Object(end_idx);
                                parent_ind = grand_ind;
                            }

                            data = &data[1..];
                        }

                        // Empty object or token header
                        b'{' => {
                            data = self.skip_ws_t(&data[1..]).ok_or_else(Error::eof)?;
                            if data[0] == b'}' {
                                data = &data[1..];
                                continue;
                            }

                            if let Some(last) = self.token_tape.last_mut() {
                                if let TextToken::Unquoted(header) = last {
                                    if array_ind_of_hidden_obj.is_none() {
                                        *last = TextToken::Header(*header);
                                        self.token_tape.push(TextToken::Array(0));
                                        state = ParseState::ParseOpen;
                                        continue;
                                    }
                                }
                            }

                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                offset: self.offset(data),
                                msg: String::from("invalid syntax for token headers"),
                            }));
                        }

                        b'[' => {
                            data = self.parse_parameter_definition(
                                data,
                                &mut parent_ind,
                                &mut state,
                                false,
                                array_ind_of_hidden_obj.is_some(),
                            )?;
                        }

                        b'"' => {
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::KeyValueSeparator;
                        }

                        b'@' => {
                            data = self.parse_variable(data)?;
                            state = ParseState::KeyValueSeparator;
                        }

                        _ => {
                            data = self.parse_scalar(data);
                            state = ParseState::KeyValueSeparator;
                        }
                    }
                }
                ParseState::KeyValueSeparator => {
                    let new_data = self.parse_key_value_separator(data);
                    lack_operator = new_data.len() == data.len();
                    data = new_data;
                    state = ParseState::ObjectValue;
                }
                ParseState::ObjectValue => {
                    match data[0] {
                        b'{' => {
                            if let Some(array_ind) = array_ind_of_hidden_obj.take() {
                                // before we error, we should check if we previously parsed an empty array
                                // `history={{} 1444.11.11={core=AAA}}`
                                // so we're going to go back up the stack until we see our parent object
                                // and ensure that everything along the way is an empty array

                                let mut start = self.token_tape.len() - 3;
                                while start > array_ind {
                                    match self.token_tape[start] {
                                        TextToken::End(x) if x == start - 1 => {
                                            start -= 2;
                                        }
                                        _ => {
                                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                                offset: self.offset(data) - 2,
                                                msg: String::from(
                                                    "header values inside a hidden object are unsupported",
                                                ),
                                            }));
                                        }
                                    }
                                }

                                let empty_objects_to_remove = self.token_tape.len() - 2 - array_ind;

                                let grand_ind = match self.token_tape[array_ind] {
                                    TextToken::Array(x) => x,
                                    _ => 0,
                                };

                                for _ in 0..empty_objects_to_remove {
                                    self.token_tape.remove(self.token_tape.len() - 3);
                                }

                                parent_ind = array_ind;
                                self.token_tape[parent_ind] = TextToken::Object(grand_ind);
                            }

                            self.token_tape.push(TextToken::Array(0));
                            state = ParseState::ParseOpen;
                            data = &data[1..];
                        }

                        b'}' => {
                            // Encountering a `}` for an object value has never been encountered in the wild
                            // but it makes sense to interpret it as an array trailer of one element long.
                            if parent_ind == 0 {
                                return Err(Error::new(ErrorKind::StackEmpty {
                                    offset: self.offset(data),
                                }));
                            }

                            let ind = self.token_tape.len() - 1;
                            if array_ind_of_hidden_obj.is_some()
                                || !matches!(
                                    self.token_tape[ind],
                                    TextToken::Unquoted(_) | TextToken::Quoted(_)
                                )
                            {
                                return Err(Error::new(ErrorKind::InvalidSyntax {
                                    msg: String::from("complex trailers are not supported"),
                                    offset: self.offset(data),
                                }));
                            }

                            self.token_tape.insert(ind, TextToken::Array(parent_ind));
                            parent_ind = ind;
                            state = ParseState::ArrayValue;
                            in_trailer = true;
                        }

                        b'"' => {
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::Key;
                        }
                        b'@' => {
                            data = self.parse_variable(data)?;
                            state = ParseState::Key;
                        }
                        _ => {
                            if lack_operator && parent_ind != 0 {
                                if array_ind_of_hidden_obj.is_some() {
                                    return Err(Error::new(ErrorKind::InvalidSyntax {
                                        msg: String::from("complex trailers are not supported"),
                                        offset: self.offset(data),
                                    }));
                                }

                                let ind = self.token_tape.len() - 1;
                                self.token_tape.insert(ind, TextToken::Array(parent_ind));
                                parent_ind = ind;
                                state = ParseState::ArrayValue;
                                in_trailer = true;
                            } else {
                                data = self.parse_scalar(data);
                                state = ParseState::Key;
                            }
                        }
                    }
                    lack_operator = false;
                }
                ParseState::ParseOpen => {
                    match data[0] {
                        // Empty array
                        b'}' => {
                            let ind = self.token_tape.len() - 1;
                            state = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            self.token_tape[ind] = TextToken::Array(ind + 1);
                            self.token_tape.push(TextToken::End(ind));
                            data = &data[1..];
                        }

                        // start of a parameter definition
                        b'[' => {
                            data = self.parse_parameter_definition(
                                data,
                                &mut parent_ind,
                                &mut state,
                                true,
                                array_ind_of_hidden_obj.is_some(),
                            )?;
                        }

                        // array of objects or another array
                        b'{' => {
                            let ind = self.token_tape.len() - 1;
                            self.token_tape[ind] = TextToken::Array(parent_ind);
                            parent_ind = ind;
                            state = ParseState::ArrayValue;
                        }
                        b'"' => {
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::FirstValue;
                        }
                        b'@' => {
                            data = self.parse_variable(data)?;
                            state = ParseState::FirstValue;
                        }
                        _ => {
                            data = self.parse_scalar(data);
                            state = ParseState::FirstValue;
                        }
                    }
                }
                ParseState::FirstValue => match data[0] {
                    b'=' | b'>' | b'<' => {
                        let ind = self.token_tape.len() - 2;
                        self.token_tape[ind] = TextToken::Object(parent_ind);
                        parent_ind = ind;
                        state = ParseState::KeyValueSeparator;
                    }
                    _ => {
                        let ind = self.token_tape.len() - 2;
                        self.token_tape[ind] = TextToken::Array(parent_ind);
                        parent_ind = ind;
                        state = ParseState::ArrayValue;
                    }
                },
                ParseState::ArrayValue => match data[0] {
                    b'{' => {
                        if in_trailer {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("complex trailers are not supported"),
                                offset: self.offset(data) - 1,
                            }));
                        }

                        self.token_tape.push(TextToken::Array(0));
                        state = ParseState::ParseOpen;
                        data = &data[1..];
                    }
                    b'}' => {
                        if in_trailer {
                            let parent_obj_ind = if let Some(TextToken::Array(x)) =
                                self.token_tape.get(parent_ind)
                            {
                                *x
                            } else {
                                panic!("expected array");
                            };

                            let grand_ind = match self.token_tape.get(parent_obj_ind) {
                                Some(TextToken::Array(x)) => *x,
                                Some(TextToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            let end_idx = self.token_tape.len();
                            self.token_tape[parent_ind] = TextToken::Array(end_idx);
                            self.token_tape[parent_obj_ind] = TextToken::Object(end_idx + 1);
                            self.token_tape.push(TextToken::End(parent_ind));
                            self.token_tape.push(TextToken::End(parent_obj_ind));
                            parent_ind = grand_ind;
                            in_trailer = false;
                        } else {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(x)) => *x,
                                Some(TextToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            if parent_ind == 0 && grand_ind == 0 {
                                return Err(Error::new(ErrorKind::StackEmpty {
                                    offset: self.offset(data),
                                }));
                            }

                            let end_idx = self.token_tape.len();
                            self.token_tape[parent_ind] = TextToken::Array(end_idx);
                            self.token_tape.push(TextToken::End(parent_ind));
                            parent_ind = grand_ind;
                        }

                        data = &data[1..];
                    }
                    b'"' => {
                        data = self.parse_quote_scalar(data)?;
                        state = ParseState::ArrayValue;
                    }
                    b'@' => {
                        data = self.parse_variable(data)?;
                        state = ParseState::ArrayValue;
                    }
                    b'=' => {
                        // CK3 introduced hidden object inside lists so we work around it by trying to
                        // make the object explicit, but we first check to see if we have any prior
                        // array values
                        if self.token_tape.len() - parent_ind <= 1
                            || matches!(
                                self.token_tape[self.token_tape.len() - 1],
                                TextToken::End(_)
                            )
                            || in_trailer
                        {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("hidden object must start with a key"),
                                offset: self.offset(data) - 1,
                            }));
                        }

                        let hidden_object = TextToken::Object(parent_ind);
                        array_ind_of_hidden_obj = Some(parent_ind);
                        parent_ind = self.token_tape.len() - 1;
                        self.token_tape
                            .insert(self.token_tape.len() - 1, hidden_object);
                        state = ParseState::ObjectValue;
                        data = &data[1..];
                    }
                    _ => {
                        data = self.parse_scalar(data);
                        state = ParseState::ArrayValue;
                    }
                },
            }
        }
    }

    fn parse_parameter_definition(
        &mut self,
        data: &'a [u8],
        parent_ind: &mut usize,
        state: &mut ParseState,
        initial: bool,
        inside_hidden_object: bool,
    ) -> Result<&'a [u8], Error> {
        if inside_hidden_object {
            return Err(Error::new(ErrorKind::InvalidSyntax {
                offset: self.offset(data),
                msg: String::from("parameter definitions inside hidden objects are not allowed"),
            }));
        }

        if !matches!(data.get(1), Some(&x) if x == b'[') {
            return Err(Error::new(ErrorKind::InvalidSyntax {
                offset: self.offset(data),
                msg: String::from("expected start of parameter definition"),
            }));
        }

        // This is for parse_open to know and we signal that a parameter
        // definition means an object as parameters should be uniquely named
        if initial {
            let ind = self.token_tape.len() - 1;
            self.token_tape[ind] = TextToken::Object(*parent_ind);
            *parent_ind = ind;
        }

        let is_undefined = matches!(data.get(2), Some(&x) if x == b'!');
        let data = data
            .get(2 + is_undefined as usize..)
            .ok_or_else(Error::eof)?;

        if data.is_empty() {
            return Err(Error::eof());
        }

        let (scalar, data) = split_at_scalar(&data);
        if !matches!(data.get(0), Some(&x) if x == b']') {
            return Err(Error::new(ErrorKind::InvalidSyntax {
                offset: self.offset(data),
                msg: String::from("expected end of parameter name"),
            }));
        }

        let data = &data[1..];

        let token = if is_undefined {
            TextToken::UndefinedParameter(scalar)
        } else {
            TextToken::Parameter(scalar)
        };

        self.token_tape.push(token);

        // now we have to determine if are looking at a parameter value or
        // parameter object. We know when we are looking at a parameter value
        // when `]` is encountered first after the value else it's a key
        let data = self.skip_ws_t(data).ok_or_else(Error::eof)?;
        let (key_or_value, data) = split_at_scalar(data);
        let data = self.skip_ws_t(data).ok_or_else(Error::eof)?;
        if data[0] == b']' {
            self.token_tape.push(TextToken::Unquoted(key_or_value));
            *state = ParseState::Key;
            Ok(&data[1..])
        } else {
            let grand_ind = *parent_ind;
            *parent_ind = self.token_tape.len();
            self.token_tape.push(TextToken::Object(grand_ind));
            self.token_tape.push(TextToken::Unquoted(key_or_value));
            *state = ParseState::KeyValueSeparator;
            Ok(data)
        }
    }
}

fn sub(a: *const u8, b: *const u8) -> usize {
    debug_assert!(a >= b);
    (a as usize) - (b as usize)
}

#[inline(always)]
unsafe fn forward_search<F: Fn(u8) -> bool>(
    start_ptr: *const u8,
    end_ptr: *const u8,
    confirm: F,
) -> Option<usize> {
    let mut ptr = start_ptr;
    while ptr < end_ptr {
        if confirm(*ptr) {
            return Some(sub(ptr, start_ptr));
        }
        ptr = ptr.offset(1);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'a>(data: &'a [u8]) -> Result<TextTape<'a>, Error> {
        TextTape::from_slice(data)
    }

    #[test]
    fn test_size_of_text_token() {
        let token_size = std::mem::size_of::<TextToken>();
        assert!(token_size <= 24);
        assert_eq!(
            token_size,
            std::mem::size_of::<Scalar>() + std::mem::size_of::<usize>()
        );
    }

    #[test]
    fn test_binary_tokens_dont_need_to_be_dropped() {
        assert!(!std::mem::needs_drop::<TextToken>());
    }

    #[test]
    fn test_simple_event() {
        let data = b"foo=bar";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"bar")),
            ]
        );
    }

    #[test]
    fn test_simple_event_with_spaces() {
        let data = b"  \t\t foo =bar \r\ndef=\tqux";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"def")),
                TextToken::Unquoted(Scalar::new(b"qux")),
            ]
        );
    }

    #[test]
    fn test_scalars_with_quotes() {
        let data = br#""foo"="bar" "3"="1444.11.11""#;
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Quoted(Scalar::new(b"foo")),
                TextToken::Quoted(Scalar::new(b"bar")),
                TextToken::Quoted(Scalar::new(b"3")),
                TextToken::Quoted(Scalar::new(b"1444.11.11")),
            ]
        );
    }

    #[test]
    fn test_scalars_with_quoteas_delimiters() {
        let data = br#""foo"="bar"3="1444.11.11""#;
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Quoted(Scalar::new(b"foo")),
                TextToken::Quoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"3")),
                TextToken::Quoted(Scalar::new(b"1444.11.11")),
            ]
        );
    }

    #[test]
    fn test_escaped_quotes() {
        let data = br#"name = "Joe \"Captain\" Rogers""#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Quoted(Scalar::new(br#"Joe \"Captain\" Rogers"#)),
            ]
        );
    }

    #[test]
    fn test_escaped_quotes_short() {
        let data = br#"name = "J Rogers \"a""#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Quoted(Scalar::new(br#"J Rogers \"a"#)),
            ]
        );
    }

    #[test]
    fn test_escaped_quotes_crazy() {
        let data = br#"custom_name="THE !@#$%^&*( '\"LEGION\"')""#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"custom_name")),
                TextToken::Quoted(Scalar::new(br#"THE !@#$%^&*( '\"LEGION\"')"#)),
            ]
        );
    }

    #[test]
    fn test_quotes_with_escape_sequences() {
        // Preventative measures to ensure we don't regress on imperator color codes
        let data = b"custom_name=\"ab \x15D ( ID: 691 )\x15!\"";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"custom_name")),
                TextToken::Quoted(Scalar::new(b"ab \x15D ( ID: 691 )\x15!")),
            ]
        );
    }

    #[test]
    fn test_numbers_are_scalars() {
        let data = b"foo=1.000";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"1.000")),
            ]
        );
    }

    #[test]
    fn test_object_event() {
        let data = b"foo={bar=qux}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"qux")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_object_multi_field_event() {
        let data = b"foo={bar=1 qux=28}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Unquoted(Scalar::new(b"qux")),
                TextToken::Unquoted(Scalar::new(b"28")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_text_parser_tape() {
        let mut tape = TextTape::new();

        let data = b"foo={bar=1 qux=28}";
        TextTape::parser()
            .parse_slice_into_tape(data, &mut tape)
            .unwrap();

        assert_eq!(
            tape.tokens(),
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Unquoted(Scalar::new(b"qux")),
                TextToken::Unquoted(Scalar::new(b"28")),
                TextToken::End(1),
            ]
        );

        let data2 = b"foo2={bar2=3 qux2=29}";
        TextTape::parser()
            .parse_slice_into_tape(data2, &mut tape)
            .unwrap();

        assert_eq!(
            tape.tokens(),
            vec![
                TextToken::Unquoted(Scalar::new(b"foo2")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"bar2")),
                TextToken::Unquoted(Scalar::new(b"3")),
                TextToken::Unquoted(Scalar::new(b"qux2")),
                TextToken::Unquoted(Scalar::new(b"29")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_event() {
        let data = b"versions={\r\n\t\"1.28.3.0\"\r\n}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"versions")),
                TextToken::Array(3),
                TextToken::Quoted(Scalar::new(b"1.28.3.0")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_multievent() {
        let data = b"versions={\r\n\t\"1.28.3.0\"\r\n foo}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"versions")),
                TextToken::Array(4),
                TextToken::Quoted(Scalar::new(b"1.28.3.0")),
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_no_equal_object_event() {
        let data = b"foo{bar=qux}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"qux")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_no_equal_object_field() {
        let data = br#"
        dlc002 = {
            name = "Arachnoid Portrait Pack"
            steam_id = "447680"
            gog_store_id = ""
            paradoxplaza_store_url ""
            category = "content_pack"
            show = no
            recommendations = {}
        }"#;
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"dlc002")),
                TextToken::Object(17),
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Quoted(Scalar::new(b"Arachnoid Portrait Pack")),
                TextToken::Unquoted(Scalar::new(b"steam_id")),
                TextToken::Quoted(Scalar::new(b"447680")),
                TextToken::Unquoted(Scalar::new(b"gog_store_id")),
                TextToken::Quoted(Scalar::new(b"")),
                TextToken::Unquoted(Scalar::new(b"paradoxplaza_store_url")),
                TextToken::Quoted(Scalar::new(b"")),
                TextToken::Unquoted(Scalar::new(b"category")),
                TextToken::Quoted(Scalar::new(b"content_pack")),
                TextToken::Unquoted(Scalar::new(b"show")),
                TextToken::Unquoted(Scalar::new(b"no")),
                TextToken::Unquoted(Scalar::new(b"recommendations")),
                TextToken::Array(16),
                TextToken::End(15),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_array() {
        let data = b"discovered_by={}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"discovered_by")),
                TextToken::Array(2),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_of_objects() {
        let data = b"stats={{id=0 type=general} {id=1 type=admiral}}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"stats")),
                TextToken::Array(14),
                TextToken::Object(7),
                TextToken::Unquoted(Scalar::new(b"id")),
                TextToken::Unquoted(Scalar::new(b"0")),
                TextToken::Unquoted(Scalar::new(b"type")),
                TextToken::Unquoted(Scalar::new(b"general")),
                TextToken::End(2),
                TextToken::Object(13),
                TextToken::Unquoted(Scalar::new(b"id")),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Unquoted(Scalar::new(b"type")),
                TextToken::Unquoted(Scalar::new(b"admiral")),
                TextToken::End(8),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects() {
        let data = b"history={{} 1629.11.10={core=AAA}}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"history")),
                TextToken::Object(7),
                TextToken::Unquoted(Scalar::new(b"1629.11.10")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"core")),
                TextToken::Unquoted(Scalar::new(b"AAA")),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_multi_objects() {
        let data = b"history={{} {} 1629.11.10={core=AAA}}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"history")),
                TextToken::Object(7),
                TextToken::Unquoted(Scalar::new(b"1629.11.10")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"core")),
                TextToken::Unquoted(Scalar::new(b"AAA")),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects2() {
        let data = b"foo={bar=val {}} me=you";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"val")),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"me")),
                TextToken::Unquoted(Scalar::new(b"you")),
            ]
        );
    }

    #[test]
    fn test_empty_objects3() {
        let data = b"foo={bar=val { } a=b} me=you";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"val")),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"me")),
                TextToken::Unquoted(Scalar::new(b"you")),
            ]
        );
    }

    #[test]
    fn test_spanning_objects() {
        let data = b"army={name=abc} army={name=def}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"army")),
                TextToken::Object(4),
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"abc")),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"army")),
                TextToken::Object(9),
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"def")),
                TextToken::End(6),
            ]
        );
    }

    #[test]
    fn test_object_array_trailer() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"brittany_area")),
                TextToken::Object(15),
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Array(7),
                TextToken::Unquoted(Scalar::new(b"118")),
                TextToken::Unquoted(Scalar::new(b"99")),
                TextToken::Unquoted(Scalar::new(b"151")),
                TextToken::End(3),
                TextToken::Array(14),
                TextToken::Unquoted(Scalar::new(b"169")),
                TextToken::Unquoted(Scalar::new(b"170")),
                TextToken::Unquoted(Scalar::new(b"171")),
                TextToken::Unquoted(Scalar::new(b"172")),
                TextToken::Unquoted(Scalar::new(b"4384")),
                TextToken::End(8),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_object_array_trailer_single_element() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169
        }"#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"brittany_area")),
                TextToken::Object(11),
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Array(7),
                TextToken::Unquoted(Scalar::new(b"118")),
                TextToken::Unquoted(Scalar::new(b"99")),
                TextToken::Unquoted(Scalar::new(b"151")),
                TextToken::End(3),
                TextToken::Array(10),
                TextToken::Unquoted(Scalar::new(b"169")),
                TextToken::End(8),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_regression() {
        let data = [0, 32, 34, 0];
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_regression2() {
        let data = [0, 4, 33, 0];
        let _ = parse(&data[..]);
    }

    #[test]
    fn test_too_heavily_nested() {
        let mut data = Vec::new();
        data.extend_from_slice(b"foo=");
        for _ in 0..100000 {
            data.extend_from_slice(b"{");
        }
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_no_ws_comment() {
        let data = b"foo=abc#def\nbar=qux";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"abc")),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"qux")),
            ]
        );
    }

    #[test]
    fn test_bom() {
        let data = b"\xef\xbb\xbf#hello";
        let tape = parse(&data[..]).unwrap();

        assert_eq!(tape.token_tape, vec![]);
        assert!(tape.utf8_bom);
    }

    #[test]
    fn test_period_in_identifiers() {
        let data = b"flavor_tur.8=yes";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"flavor_tur.8")),
                TextToken::Unquoted(Scalar::new(b"yes")),
            ]
        );
    }

    #[test]
    fn test_dashed_identifiers() {
        // From stellaris saves
        let data = b"dashed-identifier=yes";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"dashed-identifier")),
                TextToken::Unquoted(Scalar::new(b"yes")),
            ]
        );
    }

    #[test]
    fn test_colon_values() {
        let data = b"province_id = event_target:agenda_province";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"province_id")),
                TextToken::Unquoted(Scalar::new(b"event_target:agenda_province")),
            ]
        );
    }

    #[test]
    fn test_variables() {
        let data = b"@planet_standard_scale = 11";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"@planet_standard_scale")),
                TextToken::Unquoted(Scalar::new(b"11")),
            ]
        );
    }

    #[test]
    fn test_interpolated_variable() {
        let data = b"position = { @[1-leopard_x] @leopard_y }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"position")),
                TextToken::Array(4),
                TextToken::Unquoted(Scalar::new(b"@[1-leopard_x]")),
                TextToken::Unquoted(Scalar::new(b"@leopard_y")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_variable_value() {
        let data = b"window_name = @default_window_name";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"window_name")),
                TextToken::Unquoted(Scalar::new(b"@default_window_name")),
            ]
        );
    }

    #[test]
    fn test_equal_identifier() {
        let data = br#"=="bar""#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"=")),
                TextToken::Quoted(Scalar::new(b"bar")),
            ]
        );
    }

    #[test]
    fn test_many_line_comment() {
        let mut data = Vec::new();
        data.extend_from_slice(b"foo=1.000\n");
        for _ in 0..100000 {
            data.extend_from_slice(b"# this is a comment\n");
        }
        data.extend_from_slice(b"foo=2.000\n");

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"1.000")),
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"2.000")),
            ]
        );
    }

    #[test]
    fn test_terminating_comment() {
        let data = b"# boo\r\n# baa\r\nfoo=a\r\n# bee";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Unquoted(Scalar::new(b"a")),
            ]
        );
    }

    #[test]
    fn test_rgb_trick() {
        let data = b"name = rgb ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"rgb")),
            ]
        );
    }

    #[test]
    fn test_rgb_trick2() {
        let data = b"name = rgb type = 4713";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"rgb")),
                TextToken::Unquoted(Scalar::new(b"type")),
                TextToken::Unquoted(Scalar::new(b"4713")),
            ]
        );
    }

    #[test]
    fn test_rgb_trick3() {
        let data = b"name = rgbeffect";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"rgbeffect")),
            ]
        );
    }

    #[test]
    fn test_rgb() {
        let data = b"color = rgb { 100 200 150 } ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Header(Scalar::new(b"rgb")),
                TextToken::Array(6),
                TextToken::Unquoted(Scalar::new(b"100")),
                TextToken::Unquoted(Scalar::new(b"200")),
                TextToken::Unquoted(Scalar::new(b"150")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_hsv_trick() {
        let data = b"name = hsv ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"hsv")),
            ]
        );
    }

    #[test]
    fn test_hsv_trick2() {
        let data = b"name = hsv type = 4713";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"hsv")),
                TextToken::Unquoted(Scalar::new(b"type")),
                TextToken::Unquoted(Scalar::new(b"4713")),
            ]
        );
    }

    #[test]
    fn test_hsv_trick3() {
        let data = b"name = hsveffect";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"name")),
                TextToken::Unquoted(Scalar::new(b"hsveffect")),
            ]
        );
    }

    #[test]
    fn test_hsv() {
        let data = b"color = hsv { 0.43 0.86 0.61 } ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Header(Scalar::new(b"hsv")),
                TextToken::Array(6),
                TextToken::Unquoted(Scalar::new(b"0.43")),
                TextToken::Unquoted(Scalar::new(b"0.86")),
                TextToken::Unquoted(Scalar::new(b"0.61")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_hsv360() {
        let data = b"color = hsv360{ 25 75 63 }";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Header(Scalar::new(b"hsv360")),
                TextToken::Array(6),
                TextToken::Unquoted(Scalar::new(b"25")),
                TextToken::Unquoted(Scalar::new(b"75")),
                TextToken::Unquoted(Scalar::new(b"63")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_cylindrical() {
        let data = b"color = cylindrical{ 150 3 0 }";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Header(Scalar::new(b"cylindrical")),
                TextToken::Array(6),
                TextToken::Unquoted(Scalar::new(b"150")),
                TextToken::Unquoted(Scalar::new(b"3")),
                TextToken::Unquoted(Scalar::new(b"0")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_hex() {
        let data = b"color = hex { aabbccdd }";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Header(Scalar::new(b"hex")),
                TextToken::Array(4),
                TextToken::Unquoted(Scalar::new(b"aabbccdd")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_list_header() {
        let data = b"mild_winter = LIST { 3700 3701 }";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"mild_winter")),
                TextToken::Header(Scalar::new(b"LIST")),
                TextToken::Array(5),
                TextToken::Unquoted(Scalar::new(b"3700")),
                TextToken::Unquoted(Scalar::new(b"3701")),
                TextToken::End(2),
            ]
        );
    }

    #[test]
    fn test_heterogenous_list() {
        let data = b"levels={ 10 0=2 1=2 } foo={bar=qux}";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"levels")),
                TextToken::Array(9),
                TextToken::Unquoted(Scalar::new(b"10")),
                TextToken::HiddenObject(8),
                TextToken::Unquoted(Scalar::new(b"0")),
                TextToken::Unquoted(Scalar::new(b"2")),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Unquoted(Scalar::new(b"2")),
                TextToken::End(3),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Object(14),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::Unquoted(Scalar::new(b"qux")),
                TextToken::End(11),
            ]
        );
    }

    #[test]
    fn test_hidden_object() {
        let data = b"16778374={ levels={ 10 0=2 1=2 } }";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"16778374")),
                TextToken::Object(12),
                TextToken::Unquoted(Scalar::new(b"levels")),
                TextToken::Array(11),
                TextToken::Unquoted(Scalar::new(b"10")),
                TextToken::HiddenObject(10),
                TextToken::Unquoted(Scalar::new(b"0")),
                TextToken::Unquoted(Scalar::new(b"2")),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Unquoted(Scalar::new(b"2")),
                TextToken::End(5),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_hidden_object_needs_key() {
        let data = b"a{{}=}";
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_objects_in_hidden_objects_not_supported() {
        let data = b"u{1 a={0=1}";
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_hidden_objects_with_headers_not_supported() {
        let data = b"s{{c d=a{b=}}";
        assert!(TextTape::from_slice(&data[..]).is_err());
    }

    #[test]
    fn test_less_than_equal_operator() {
        let data = b"scope:attacker.primary_title.tier <= tier_county";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"scope:attacker.primary_title.tier")),
                TextToken::Operator(Operator::LessThanEqual),
                TextToken::Unquoted(Scalar::new(b"tier_county")),
            ]
        );
    }

    #[test]
    fn test_less_than_operator() {
        let data = b"count < 2";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"count")),
                TextToken::Operator(Operator::LessThan),
                TextToken::Unquoted(Scalar::new(b"2")),
            ]
        );
    }

    #[test]
    fn test_greater_than_operator() {
        let data = b"age > 16";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"age")),
                TextToken::Operator(Operator::GreaterThan),
                TextToken::Unquoted(Scalar::new(b"16")),
            ]
        );
    }

    #[test]
    fn test_greater_than_equal_operator() {
        let data = b"intrigue >= high_skill_rating";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"intrigue")),
                TextToken::Operator(Operator::GreaterThanEqual),
                TextToken::Unquoted(Scalar::new(b"high_skill_rating")),
            ]
        );
    }

    #[test]
    fn test_not_equal_operator() {
        let data = b"count != 2";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"count")),
                TextToken::Operator(Operator::NotEqual),
                TextToken::Unquoted(Scalar::new(b"2")),
            ]
        );
    }

    #[test]
    fn test_operator_in_object() {
        let data = b"value={ date < 1941.7.7 is_preparing=true }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"value")),
                TextToken::Object(7),
                TextToken::Unquoted(Scalar::new(b"date")),
                TextToken::Operator(Operator::LessThan),
                TextToken::Unquoted(Scalar::new(b"1941.7.7")),
                TextToken::Unquoted(Scalar::new(b"is_preparing")),
                TextToken::Unquoted(Scalar::new(b"true")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_extraneous_closing_bracket() {
        let data = b"foo = { bar } } ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"foo")),
                TextToken::Array(3),
                TextToken::Unquoted(Scalar::new(b"bar")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_extraneous_closing_bracket2() {
        let data = b"a{} b r}";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Array(2),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"r")),
            ]
        );
    }

    #[test]
    fn test_extraneous_closing_bracket3() {
        let data = b"a c}}";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"c")),
            ]
        );
    }

    #[test]
    fn test_extraneous_bracket() {
        // from eu4 verona.txt
        let data = br#"color = { 121 163 114 } } army_names = {"Armata di $PROVINCE$"}"#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Array(5),
                TextToken::Unquoted(Scalar::new(b"121")),
                TextToken::Unquoted(Scalar::new(b"163")),
                TextToken::Unquoted(Scalar::new(b"114")),
                TextToken::End(1),
                TextToken::Unquoted(Scalar::new(b"army_names")),
                TextToken::Array(9),
                TextToken::Quoted(Scalar::new(b"Armata di $PROVINCE$")),
                TextToken::End(7),
            ]
        );
    }

    #[test]
    fn test_missing_bracket() {
        let data = br#"BRA_GAR_01 = { ordered = { 1 = { "%da Divisao de Infantaria" } }"#;
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"BRA_GAR_01")),
                TextToken::Object(9),
                TextToken::Unquoted(Scalar::new(b"ordered")),
                TextToken::Object(8),
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::Array(7),
                TextToken::Quoted(Scalar::new(b"%da Divisao de Infantaria")),
                TextToken::End(5),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_unquoted_non_ascii() {
        // More vic2 shenanigans
        let data = b"jean_jaur\xe8s = bar ";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"jean_jaur\xe8s")),
                TextToken::Unquoted(Scalar::new(b"bar")),
            ]
        );
    }

    #[test]
    fn test_trailer_in_object_array() {
        let data = b"a {{b=c d c}}";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Array(10),
                TextToken::Object(9),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"c")),
                TextToken::Array(8),
                TextToken::Unquoted(Scalar::new(b"d")),
                TextToken::Unquoted(Scalar::new(b"c")),
                TextToken::End(5),
                TextToken::End(2),
                TextToken::End(1)
            ]
        );
    }

    #[test]
    fn test_parameter_value() {
        let data = b"generate_advisor = { [[effect] $effect$ ] }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"generate_advisor")),
                TextToken::Object(4),
                TextToken::Parameter(Scalar::new(b"effect")),
                TextToken::Unquoted(Scalar::new(b"$effect$")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_parameter_object() {
        let data = b"generate_advisor = { [[scaled_skill] a=b ] }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"generate_advisor")),
                TextToken::Object(7),
                TextToken::Parameter(Scalar::new(b"scaled_skill")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_parameter_object2() {
        let data = b"generate_advisor = { [[add] if = { a=b }] }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"generate_advisor")),
                TextToken::Object(10),
                TextToken::Parameter(Scalar::new(b"add")),
                TextToken::Object(9),
                TextToken::Unquoted(Scalar::new(b"if")),
                TextToken::Object(8),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::End(5),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_parameter_object3() {
        let data = b"generate_advisor = { [[add] if = { a=b }] [[remove] if={c=d}] }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"generate_advisor")),
                TextToken::Object(18),
                TextToken::Parameter(Scalar::new(b"add")),
                TextToken::Object(9),
                TextToken::Unquoted(Scalar::new(b"if")),
                TextToken::Object(8),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::End(5),
                TextToken::End(3),
                TextToken::Parameter(Scalar::new(b"remove")),
                TextToken::Object(17),
                TextToken::Unquoted(Scalar::new(b"if")),
                TextToken::Object(16),
                TextToken::Unquoted(Scalar::new(b"c")),
                TextToken::Unquoted(Scalar::new(b"d")),
                TextToken::End(13),
                TextToken::End(11),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_undefined_parameter_object() {
        let data = b"generate_advisor = { [[!scaled_skill] a=b ] }";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"generate_advisor")),
                TextToken::Object(7),
                TextToken::UndefinedParameter(Scalar::new(b"scaled_skill")),
                TextToken::Object(6),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_text_number_plus() {
        let data = b"pop_happiness = +0.10";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"pop_happiness")),
                TextToken::Unquoted(Scalar::new(b"+0.10")),
            ]
        );
    }

    #[test]
    fn test_skip_semicolon() {
        let data = b"value=\"win\"; a=b";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Unquoted(Scalar::new(b"value")),
                TextToken::Quoted(Scalar::new(b"win")),
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"b")),
            ]
        );
    }

    #[test]
    fn test_parameter_eof() {
        let data = b"[[";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn test_deny_parameter_in_hidden_object() {
        let data = b"s{a b=c[[a]}";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn deny_complex_trailer() {
        let data = b"a{b=c a d {t}}";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn deny_complex_trailer2() {
        let data = b"s{{b=c<:d=}=}}";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn deny_complex_trailer3() {
        let data = b"a{c=d b>}";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn deny_trailer_inside_hidden_object() {
        let data = b"k{a=a { ta { b } a=z=aP } } } a }";
        let err = TextTape::from_slice(data).unwrap_err();
        match err.kind() {
            ErrorKind::InvalidSyntax { offset, .. } => {
                assert_eq!(*offset, 21);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn incomplete_object_fail_to_parse() {
        let data = b"T&}";
        TextTape::from_slice(data).unwrap_err();
    }

    #[test]
    fn test_initial_end_does_not_panic() {
        let res = parse(&b"}"[..]);
        assert!(res.is_ok() || res.is_err());
    }
}
