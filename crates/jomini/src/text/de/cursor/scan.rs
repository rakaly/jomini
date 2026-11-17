//! Token scanning over a window of bytes.
//!
//! The functions here never consume: they take the window and an offset into
//! it and return offsets. A token is reported as a span, so the caller
//! decides when the bytes are consumed, and an offset stays valid for as long
//! as the bytes before it are kept in the window.
//!
//! When the window ends before a token does, the result asks for more data
//! instead of guessing; `eof` tells the scanner that no more data will come.

use crate::{
    data::is_boundary,
    text::Operator,
    util::{contains_zero_byte, leading_whitespace, repeat_byte},
};

/// What a token is, without its bytes. The operator of an operator token is
/// in [`Tok::op`], so that the kind is a plain byte and checking for a
/// scalar is a single comparison.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Kind {
    Unquoted,
    Quoted,
    Operator,
    Close,
    Open,
}

/// A token as offsets into a window: the scalar is `window[start..end]`
/// (without the quotes of a quoted scalar), and the token, including the
/// whitespace before it, takes up `window[..next]`.
///
/// The offsets are 32 bits, so that a token fits in two registers; a token
/// and the whitespace before it can't take up 4 GiB. The scalar is held as
/// its start and length, so that its end can't be before its start and
/// slicing it checks only its end.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Tok {
    pub kind: Kind,

    /// The operator of an operator token, and `=` for other tokens.
    pub op: Operator,
    start: u32,
    len: u32,
    next: u32,
}

impl Tok {
    /// An empty unquoted scalar at `at` that takes up no bytes.
    #[inline]
    pub fn empty(at: usize) -> Self {
        Tok::new(Kind::Unquoted, at, at, at)
    }

    /// A close token at `at`.
    #[inline]
    pub fn close(at: usize) -> Self {
        Tok::punct(Kind::Close, at, 1)
    }

    /// The close of no bytes at `at` that is read as the value of a key that
    /// has none, which leaves the close of the block to be read.
    #[inline]
    pub fn no_value(at: usize) -> Self {
        Tok::punct(Kind::Close, at, 0)
    }

    /// The token after `by` bytes before it are consumed from the window.
    #[inline]
    pub fn shifted(self, by: usize) -> Self {
        Tok {
            start: self.start - by as u32,
            next: self.next - by as u32,
            ..self
        }
    }

    /// The open of no bytes at `at` that is read where a key is expected to
    /// start the `remainder` of an object.
    #[inline]
    pub fn remainder(at: usize) -> Self {
        Tok::punct(Kind::Open, at, 0)
    }

    /// Whether the token is the open of no bytes of [`Tok::remainder`].
    #[inline]
    pub fn is_remainder(&self) -> bool {
        self.kind == Kind::Open && self.next == self.start
    }

    /// Whether the token is the close of no bytes of [`Tok::no_value`].
    #[inline]
    pub fn is_no_value(&self) -> bool {
        self.kind == Kind::Close && self.next == self.start
    }

    #[inline]
    pub fn is_scalar(&self) -> bool {
        matches!(self.kind, Kind::Unquoted | Kind::Quoted)
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.start as usize
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.start as usize + self.len as usize
    }

    #[inline]
    pub fn next(&self) -> usize {
        self.next as usize
    }

    #[inline]
    fn new(kind: Kind, start: usize, end: usize, next: usize) -> Self {
        debug_assert!(u32::try_from(next).is_ok());
        Tok {
            kind,
            op: Operator::Equal,
            start: start as u32,
            len: (end - start) as u32,
            next: next as u32,
        }
    }

    #[inline]
    fn punct(kind: Kind, at: usize, len: usize) -> Self {
        Tok::new(kind, at, at, at + len)
    }

    #[inline]
    pub fn operator(op: Operator, at: usize, len: usize) -> Self {
        Tok {
            op,
            ..Tok::punct(Kind::Operator, at, len)
        }
    }
}

/// The result of scanning for a token.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Scan {
    Token(Tok),

    /// Only whitespace and comments remain, and the data has ended.
    End,

    /// The window ended inside a token that starts at the given offset;
    /// everything before it is whitespace or comments.
    More(usize),

    /// The window ended inside a comment that starts at the given offset.
    Comment(usize),

    /// The data ended inside a quoted scalar or an `@[` expression.
    Unterminated,

    /// A parameter block of script, as in `[[name] effect = x ]`, starts at
    /// the given offset.
    Parameter(usize),

    /// A token that the deserializer doesn't read starts at the given
    /// offset: a scalar in brackets, as in `[GetName]`, or a `!` or `?`
    /// that is not part of an operator.
    Unsupported(usize),
}

/// The result of [`token`], which writes the token, or the offset of the
/// token that needs more data, to its output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Status {
    Token,
    End,
    More,

    /// More data is needed, and the window ended in a comment whose `#` is
    /// at the offset in the output.
    Comment,
    Unterminated,

    /// A parameter block starts at the offset in the output.
    Parameter,

    /// A token that the deserializer doesn't read starts at the offset in
    /// the output.
    Unsupported,
}

/// The result of looking for the next byte that is not whitespace or part of
/// a comment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Peek {
    Byte(usize, u8),
    End,

    /// The window ended in whitespace, or in a comment that starts at the
    /// given offset.
    More(Option<usize>),
}

#[inline]
fn more_or(eof: bool, at: usize, end: Scan) -> Scan {
    if eof { end } else { Scan::More(at) }
}

/// Scan the token at the front of the window when it is one of the frequent
/// kinds that don't need many registers to scan: an unquoted scalar, a
/// brace, or `=`. Returns `None` for anything else, which [`token`] scans.
#[inline(always)]
pub(crate) fn token_fast(window: &[u8]) -> Option<Tok> {
    let word = window.first_chunk::<8>()?;
    if window.len() <= 8 {
        return None;
    }

    // Values in saves are followed by a newline and tabs, which a single
    // word skips.
    let mut i = leading_whitespace(u64::from_le_bytes(*word)) as usize;
    loop {
        let b = window[i];

        // Unquoted scalars are by far the most frequent token (~72% in EU4
        // saves), so they are dispatched first and scanned 8 bytes at a time.
        if matches!(b, b'a'..=b'z' | b'0'..=b'9' | b'A'..=b'Z' | b'-') {
            let mut j = i + 1;
            while let Some(chunk) = window[j..].first_chunk::<8>() {
                for (k, &c) in chunk.iter().enumerate() {
                    if is_boundary(c) {
                        let end = j + k;

                        // A space after a scalar in an array is consumed
                        // with it.
                        let next = end + usize::from(c == b' ');
                        return Some(Tok::new(Kind::Unquoted, i, end, next));
                    }
                }
                j += 8;
            }
            return None;
        } else if b == b'{' {
            return Some(Tok::punct(Kind::Open, i, 1));
        } else if b == b'}' {
            return Some(Tok::punct(Kind::Close, i, 1));
        } else if b == b'=' && window.get(i + 1).is_some_and(|&x| x != b'=') {
            return Some(Tok::operator(Operator::Equal, i, 1));
        } else if matches!(b, b' ' | b'\r') {
            // Game files have a space after an operator and a carriage
            // return at the end of a line.
            i = skip_whitespace(window, i)?;
        } else {
            return None;
        }
    }
}

/// Scan the token that starts after the whitespace at `window[at..]` into
/// `out`. When more data is needed, `out` is an empty token where the
/// unfinished token starts; everything before it is whitespace or comments.
///
/// The token is written to `out` rather than returned, so that the caller's
/// copy is written in place.
#[inline(always)]
pub(crate) fn token(window: &[u8], at: usize, eof: bool, out: &mut Tok) -> Status {
    if let Some(rest) = window.get(at..)
        && rest.len() > 8
        && let Some(word) = rest.first_chunk::<8>()
    {
        // Values in saves are followed by a newline and tabs, which a single
        // word skips.
        let mut i = at + leading_whitespace(u64::from_le_bytes(*word)) as usize;
        loop {
            let b = window[i];

            // Unquoted scalars are by far the most frequent token (~72% in EU4
            // saves), so they are dispatched first and scanned 8 bytes at a time.
            if matches!(b, b'a'..=b'z' | b'0'..=b'9' | b'A'..=b'Z' | b'-') {
                let mut j = i + 1;
                while let Some(chunk) = window[j..].first_chunk::<8>() {
                    for (k, &c) in chunk.iter().enumerate() {
                        if is_boundary(c) {
                            let end = j + k;

                            // A space after a scalar in an array is consumed
                            // with it.
                            let next = end + usize::from(c == b' ');
                            *out = Tok::new(Kind::Unquoted, i, end, next);
                            return Status::Token;
                        }
                    }
                    j += 8;
                }
            } else if b == b'{' {
                *out = Tok::punct(Kind::Open, i, 1);
                return Status::Token;
            } else if b == b'}' {
                *out = Tok::punct(Kind::Close, i, 1);
                return Status::Token;
            } else if b == b'"' {
                let start = i + 1;
                let mut j = start;
                let mut escaped = false;
                while let Some(chunk) = window[j..].first_chunk::<8>() {
                    let data = u64::from_le_bytes(*chunk);
                    escaped |= contains_zero_byte(data ^ repeat_byte(b'\\'));

                    // http://0x80.pl/notesen/2023-03-06-swar-find-any.html#faster-swar-procedure
                    let mask = repeat_byte(0x7f);
                    let lobits = data & mask;
                    let x0 = (lobits ^ repeat_byte(b'\"')) + mask;
                    let t0 = x0 | data;
                    let t1 = t0 & repeat_byte(0x80);
                    let t2 = t1 ^ repeat_byte(0x80);

                    if t2 != 0 {
                        if escaped {
                            break;
                        }
                        let end = j + (t2.trailing_zeros() >> 3) as usize;
                        *out = Tok::new(Kind::Quoted, start, end, end + 1);
                        return Status::Token;
                    }
                    j += 8;
                }
            } else if b == b'=' && window.get(i + 1).is_some_and(|&x| x != b'=') {
                *out = Tok::operator(Operator::Equal, i, 1);
                return Status::Token;
            } else if matches!(b, b' ' | b'\r')
                && let Some(next) = skip_whitespace(window, i)
            {
                // Game files have a space after an operator and a carriage
                // return at the end of a line.
                i = next;
                continue;
            }
            break;
        }
    }

    match token_slow(window, at, eof) {
        Scan::Token(tok) => {
            *out = tok;
            Status::Token
        }
        Scan::End => Status::End,
        Scan::More(start) => {
            *out = Tok::empty(start);
            Status::More
        }
        Scan::Comment(start) => {
            *out = Tok::empty(start);
            Status::Comment
        }
        Scan::Unterminated => Status::Unterminated,
        Scan::Parameter(start) => {
            *out = Tok::empty(start);
            Status::Parameter
        }
        Scan::Unsupported(start) => {
            *out = Tok::empty(start);
            Status::Unsupported
        }
    }
}

#[inline(never)]
fn token_slow(window: &[u8], at: usize, eof: bool) -> Scan {
    let mut i = at;
    loop {
        let Some(&b) = window.get(i) else {
            return more_or(eof, i, Scan::End);
        };

        match b {
            b' ' | b'\t' | b'\n' | b'\r' | b';' | b'\x08' | b'\x0b' | b'\x0c' => i += 1,
            b'#' => match window[i..].iter().position(|&x| x == b'\n') {
                Some(newline) => i += newline + 1,
                None if eof => return Scan::End,
                None => return Scan::Comment(i),
            },
            b'{' => return Scan::Token(Tok::punct(Kind::Open, i, 1)),
            b'}' => return Scan::Token(Tok::punct(Kind::Close, i, 1)),
            b'"' => return quoted(window, i, eof),
            b'[' => match window.get(i + 1) {
                Some(b'[') => return Scan::Parameter(i),
                None if !eof => return Scan::More(i),
                _ => return Scan::Unsupported(i),
            },
            b'@' => match window.get(i + 1) {
                Some(b'[') => {
                    return match window[i + 2..].iter().position(|&x| x == b']') {
                        Some(close) => {
                            let end = i + 2 + close + 1;
                            Scan::Token(Tok::new(Kind::Unquoted, i, end, end))
                        }
                        None => more_or(eof, i, Scan::Unterminated),
                    };
                }
                None if !eof => return Scan::More(i),
                _ => return unquoted(window, i, eof),
            },
            b'=' => return operator(window, i, eof, Operator::Equal, Some(Operator::Exact)),
            b'<' => {
                let alt = Some(Operator::LessThanEqual);
                return operator(window, i, eof, Operator::LessThan, alt);
            }
            b'>' => {
                let alt = Some(Operator::GreaterThanEqual);
                return operator(window, i, eof, Operator::GreaterThan, alt);
            }
            b'!' => return operator(window, i, eof, Operator::NotEqual, None),
            b'?' => return operator(window, i, eof, Operator::Exists, None),
            _ => return unquoted(window, i, eof),
        }
    }
}

/// Scan an operator whose first byte is at `i`. A following `=` makes it the
/// `with_equal` operator, or is part of the operator when there is none, as
/// in `!=` and `?=`, which need it.
#[inline]
fn operator(
    window: &[u8],
    i: usize,
    eof: bool,
    op: Operator,
    with_equal: Option<Operator>,
) -> Scan {
    match window.get(i + 1) {
        None if !eof => Scan::More(i),
        Some(b'=') => Scan::Token(Tok::operator(with_equal.unwrap_or(op), i, 2)),
        _ if with_equal.is_none() => Scan::Unsupported(i),
        _ => Scan::Token(Tok::operator(op, i, 1)),
    }
}

#[inline]
fn unquoted(window: &[u8], i: usize, eof: bool) -> Scan {
    match window[i + 1..].iter().position(|&x| is_boundary(x)) {
        Some(len) => {
            let end = i + 1 + len;
            Scan::Token(Tok::new(Kind::Unquoted, i, end, end))
        }
        None if eof => Scan::Token(Tok::new(Kind::Unquoted, i, window.len(), window.len())),
        None => Scan::More(i),
    }
}

/// Scan a quoted scalar whose opening quote is at `i`. A backslash escapes
/// the byte after it.
#[inline]
fn quoted(window: &[u8], i: usize, eof: bool) -> Scan {
    let start = i + 1;
    let mut j = start;
    loop {
        match window.get(j) {
            None => return more_or(eof, i, Scan::Unterminated),
            Some(b'\\') => j += 2,
            Some(b'"') => return Scan::Token(Tok::new(Kind::Quoted, start, j, j + 1)),
            Some(_) => j += 1,
        }
    }
}

/// Return the offset of the first byte at or after `at` that is not
/// whitespace, skipping 8 bytes at a time, or `None` when the whitespace
/// runs into the last 8 bytes of the window, which [`token_slow`] and
/// [`peek_byte_slow`] scan.
///
/// A newline and tabs, which follow values in saves, are skipped with a
/// word at a time, and a space or a carriage return, which game files have
/// around operators and at the end of lines, costs another word.
#[inline(always)]
fn skip_whitespace(window: &[u8], mut at: usize) -> Option<usize> {
    loop {
        let word = window.get(at..)?.first_chunk::<8>()?;
        let ws = leading_whitespace(u64::from_le_bytes(*word)) as usize;
        at += ws;
        match word.get(ws) {
            Some(b' ' | b'\r' | b';' | b'\x0c') => at += 1,
            Some(_) => return Some(at),
            None => {}
        }
    }
}

/// Find the first byte at or after `at` that is not whitespace or part of a
/// comment.
#[inline(always)]
pub(crate) fn peek_byte(window: &[u8], at: usize, eof: bool) -> Peek {
    if let Some(chunk) = window.get(at..).and_then(<[u8]>::first_chunk::<8>) {
        let ws = leading_whitespace(u64::from_le_bytes(*chunk)) as usize;
        if let Some(&x) = chunk.get(ws)
            && !matches!(x, b' ' | b'\r' | b';' | b'#' | b'\x0c')
        {
            return Peek::Byte(at + ws, x);
        }
    }

    peek_byte_slow(window, at, eof)
}

#[inline(never)]
fn peek_byte_slow(window: &[u8], at: usize, eof: bool) -> Peek {
    let mut i = at;
    loop {
        // Game files have a space after an operator and a carriage return at
        // the end of a line, before the newline and tabs.
        if let Some(next) = skip_whitespace(window, i) {
            i = next;
        }

        match window.get(i) {
            Some(b' ' | b'\t' | b'\n' | b'\r' | b';' | b'\x08' | b'\x0b' | b'\x0c') => i += 1,
            Some(b'#') => match window[i..].iter().position(|&x| x == b'\n') {
                Some(newline) => i += newline + 1,
                None if eof => return Peek::End,
                None => return Peek::More(Some(i)),
            },
            Some(&x) => return Peek::Byte(i, x),
            None if eof => return Peek::End,
            None => return Peek::More(None),
        }
    }
}

/// The bytes of a word that equal a byte, as a word whose bytes have their
/// high bit clear where they match. Combining several with `or` matches any
/// of them.
///
/// <http://0x80.pl/notesen/2023-03-06-swar-find-any.html>
#[derive(Debug, Clone, Copy)]
struct Matches {
    data: u64,
    differ: u64,
}

impl Matches {
    #[inline(always)]
    fn new(data: u64, byte: u8) -> Self {
        // The low 7 bits of a byte differ from `byte` exactly when adding
        // 0x7f to their xor sets the high bit, which doesn't carry.
        let mask = repeat_byte(0x7f);
        let differ = ((data & mask) ^ repeat_byte(byte)).wrapping_add(mask);
        Matches { data, differ }
    }

    #[inline(always)]
    fn or(self, other: Matches) -> Self {
        Matches {
            data: self.data,
            differ: self.differ & other.differ,
        }
    }

    /// The high bit of each matching byte, which is ASCII.
    #[inline(always)]
    fn bits(self) -> u64 {
        !(self.differ | self.data) & repeat_byte(0x80)
    }

    #[inline(always)]
    fn none(self) -> bool {
        self.bits() == 0
    }

    /// The index of the first matching byte, which must exist.
    #[inline(always)]
    fn first(self) -> usize {
        (self.bits().trailing_zeros() / 8) as usize
    }

    #[inline(always)]
    fn count(self) -> i32 {
        ((self.bits() >> 7).wrapping_mul(repeat_byte(1)) >> 56) as i32
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SkipMode {
    None,
    Quote,
    Comment,
}

/// Progress through a block that is skipped, kept across refills.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Skip {
    depth: i32,
    mode: SkipMode,

    /// The byte before the window, which decides whether a quote at the
    /// front of the window starts a string.
    prev: u8,
}

impl Skip {
    /// Skip the block whose open token was consumed.
    pub fn new() -> Self {
        Skip::nested(1)
    }

    /// Skip `depth` blocks from where a token can start.
    pub fn nested(depth: i32) -> Self {
        Skip {
            depth,
            mode: SkipMode::None,
            prev: b'{',
        }
    }

    /// Return that the window ends after its first `processed` bytes, and
    /// keep the byte before the rest.
    #[inline]
    fn end(&mut self, window: &[u8], processed: usize) -> usize {
        if let Some(&x) = processed.checked_sub(1).and_then(|i| window.get(i)) {
            self.prev = x;
        }
        processed
    }

    /// Whether the skip is in a quoted string.
    pub fn in_quote(&self) -> bool {
        self.mode == SkipMode::Quote
    }
}

/// Skip through the block until its close token. Returns the offset after
/// the close token, or the offset up to which the window was processed when
/// it ends first.
#[inline]
pub(crate) fn skip_block(window: &[u8], state: &mut Skip) -> Result<usize, usize> {
    let mut i = 0;
    loop {
        match state.mode {
            SkipMode::None => 'none: loop {
                // Process 8 bytes at a time while they contain no quote or
                // comment, which halved the time spent skipping in EU4 saves.
                // Most words have no brace either, which one test finds.
                while let Some(chunk) = window.get(i..).and_then(<[u8]>::first_chunk::<8>)
                    && window.len() - i > 8
                {
                    let data = u64::from_le_bytes(*chunk);

                    // A quote and a `#` differ only in their lowest bit.
                    let quote_or_comment = Matches::new(data | repeat_byte(1), b'#');
                    let open = Matches::new(data, b'{');
                    let close = Matches::new(data, b'}');
                    let any = quote_or_comment.or(open).or(close);
                    if any.none() {
                        i += 8;
                        continue;
                    }

                    // The bytes before the first match are skipped, and the
                    // rest are processed one at a time, except that a quote
                    // right after an `=`, as in `name="x"`, starts a string.
                    if !quote_or_comment.none() {
                        let at = any.first();
                        let pair = (data >> (8 * at.saturating_sub(1))) as u16;
                        if at > 0 && pair == u16::from_le_bytes(*b"=\"") {
                            i += at + 1;
                            state.mode = SkipMode::Quote;
                            break 'none;
                        }
                        i += at;
                        break;
                    }

                    let depth = state.depth - close.count();
                    if depth < 1 {
                        i += any.first();
                        break;
                    }
                    state.depth = depth + open.count();
                    i += 8;
                }

                let Some(&b) = window.get(i) else {
                    return Err(state.end(window, i));
                };
                i += 1;
                match b {
                    b'{' => state.depth += 1,
                    b'}' => {
                        state.depth -= 1;
                        if state.depth == 0 {
                            return Ok(i);
                        }
                    }
                    // A quote starts a string where a token can start, and
                    // is part of a scalar after its first byte, as the
                    // tokenizer reads it.
                    b'"' if quote_starts(
                        window.get(i.wrapping_sub(2)).map_or(state.prev, |&x| x),
                    ) =>
                    {
                        state.mode = SkipMode::Quote;
                        break;
                    }
                    b'#' => {
                        state.mode = SkipMode::Comment;
                        break;
                    }
                    _ => {}
                }
            },
            SkipMode::Quote => loop {
                // Skip 8 bytes at a time to the first quote or escape.
                while let Some(chunk) = window.get(i..).and_then(<[u8]>::first_chunk::<8>) {
                    let data = u64::from_le_bytes(*chunk);
                    let stop = Matches::new(data, b'"').or(Matches::new(data, b'\\'));
                    if stop.none() {
                        i += 8;
                    } else {
                        i += stop.first();
                        break;
                    }
                }

                match window.get(i) {
                    None => return Err(state.end(window, i)),
                    // An escape at the end of the window is processed once
                    // the escaped byte is read.
                    Some(b'\\') if window.len() - i <= 2 => return Err(state.end(window, i)),
                    Some(b'\\') => i += 2,
                    Some(b'"') => {
                        i += 1;
                        state.mode = SkipMode::None;
                        break;
                    }
                    Some(_) => i += 1,
                }
            },
            SkipMode::Comment => match window[i..].iter().position(|&x| x == b'\n') {
                Some(newline) => {
                    i += newline + 1;
                    state.mode = SkipMode::None;
                }
                None => return Err(state.end(window, window.len())),
            },
        }
    }
}

/// Whether a quote after `prev` starts a string, as it does where a token
/// can start: after a byte that ends an unquoted scalar, or another string.
#[inline]
fn quote_starts(prev: u8) -> bool {
    QUOTE_STARTS[usize::from(prev)]
}

/// The bytes that end an unquoted scalar, as in `is_boundary`, and a quote.
static QUOTE_STARTS: [bool; 256] = {
    let mut table = [false; 256];
    let bytes = b"\t\n\x0b\x0c\r !#;<=>[]{}\"";
    let mut i = 0;
    while i < bytes.len() {
        table[bytes[i] as usize] = true;
        i += 1;
    }
    table
};

#[cfg(test)]
mod tests {
    use super::*;

    fn scan(data: &[u8], at: usize, eof: bool) -> Scan {
        let mut tok = Tok::empty(0);
        match token(data, at, eof, &mut tok) {
            Status::Token => Scan::Token(tok),
            Status::End => Scan::End,
            Status::More => Scan::More(tok.start()),
            Status::Comment => Scan::Comment(tok.start()),
            Status::Unterminated => Scan::Unterminated,
            Status::Parameter => Scan::Parameter(tok.start()),
            Status::Unsupported => Scan::Unsupported(tok.start()),
        }
    }

    fn tokens(data: &[u8]) -> Vec<(Kind, Operator, &[u8])> {
        let mut at = 0;
        let mut out = Vec::new();
        loop {
            match scan(data, at, true) {
                Scan::Token(tok) => {
                    out.push((tok.kind, tok.op, &data[tok.start()..tok.end()]));
                    at = tok.next();
                }
                Scan::End => return out,
                x => panic!("unexpected {x:?}"),
            }
        }
    }

    #[test]
    fn test_tokens() {
        let data = b"a=b c==\"d \\\" e\" { 1 2 }\n\t\t@[x+1] ?= !=x # hi\n<=k";
        assert_eq!(
            tokens(data),
            vec![
                (Kind::Unquoted, Operator::Equal, &b"a"[..]),
                (Kind::Operator, Operator::Equal, b""),
                (Kind::Unquoted, Operator::Equal, b"b"),
                (Kind::Unquoted, Operator::Equal, b"c"),
                (Kind::Operator, Operator::Exact, b""),
                (Kind::Quoted, Operator::Equal, b"d \\\" e"),
                (Kind::Open, Operator::Equal, b""),
                (Kind::Unquoted, Operator::Equal, b"1"),
                (Kind::Unquoted, Operator::Equal, b"2"),
                (Kind::Close, Operator::Equal, b""),
                (Kind::Unquoted, Operator::Equal, b"@[x+1]"),
                (Kind::Operator, Operator::Exists, b""),
                (Kind::Operator, Operator::NotEqual, b""),
                (Kind::Unquoted, Operator::Equal, b"x"),
                (Kind::Operator, Operator::LessThanEqual, b""),
                (Kind::Unquoted, Operator::Equal, b"k"),
            ]
        );
    }

    /// A scalar that starts with a bracket and a `!` or `?` without its `=`
    /// are not tokens that are read.
    #[test]
    fn test_unsupported() {
        assert_eq!(scan(b" [GetName] ", 0, true), Scan::Unsupported(1));
        assert_eq!(scan(b"[", 0, true), Scan::Unsupported(0));
        assert_eq!(scan(b"[", 0, false), Scan::More(0));
        assert_eq!(scan(b" ! x", 0, true), Scan::Unsupported(1));
        assert_eq!(scan(b"?", 0, true), Scan::Unsupported(0));
        assert_eq!(scan(b"?", 0, false), Scan::More(0));
        assert_eq!(
            scan(b"<", 0, true),
            Scan::Token(Tok::operator(Operator::LessThan, 0, 1))
        );
    }

    #[test]
    fn test_more() {
        assert_eq!(scan(b"  abc", 0, false), Scan::More(2));
        assert_eq!(scan(b"  \"abc", 0, false), Scan::More(2));
        assert_eq!(scan(b"  \"abc", 0, true), Scan::Unterminated);
        assert_eq!(scan(b" # abc", 0, false), Scan::Comment(1));
        assert_eq!(scan(b" # abc", 0, true), Scan::End);
        assert_eq!(scan(b"=", 0, false), Scan::More(0));
        assert_eq!(peek_byte(b" \n\t # x\n {", 0, true), Peek::Byte(9, b'{'));
        assert_eq!(peek_byte(b" # x", 0, false), Peek::More(Some(1)));
        assert_eq!(peek_byte(b" \n ", 0, false), Peek::More(None));
    }

    #[test]
    fn test_matches() {
        let data = u64::from_le_bytes(*b"}a}{\"#\xfd}");
        let close = Matches::new(data, b'}');
        assert_eq!(close.count(), 3);
        assert_eq!(close.bits(), 0x8000_0000_0080_0080);
        assert_eq!(Matches::new(data, b'{').count(), 1);
        assert_eq!(
            Matches::new(data, b'"')
                .or(Matches::new(data, b'#'))
                .count(),
            2
        );
        assert!(Matches::new(data, b'=').none());
        assert_eq!(
            Matches::new(data | repeat_byte(1), b'#').bits(),
            0x0000_8080_0000_0000
        );
        assert_eq!(
            Matches::new(data, b'{')
                .or(Matches::new(data, b'#'))
                .first(),
            3
        );
    }

    #[test]
    fn test_skip_block() {
        let data = b" \"}\" # }\n { } } b";
        let mut state = Skip::new();
        assert_eq!(skip_block(data, &mut state), Ok(15));

        for split in 0..15 {
            let mut state = Skip::new();
            let consumed = skip_block(&data[..split], &mut state).unwrap_err();
            let end = skip_block(&data[consumed..], &mut state).unwrap();
            assert_eq!(consumed + end, 15, "split {split}");
        }
    }

    /// A quote after the first byte of an unquoted scalar is part of it, as
    /// the tokenizer reads it, so it doesn't start a string.
    #[test]
    fn test_skip_block_quote_in_scalar() {
        let data = b" a = abc\"d } b = \"}\" } x";
        let mut state = Skip::new();
        assert_eq!(skip_block(data, &mut state), Ok(12));

        for split in 0..12 {
            let mut state = Skip::new();
            let consumed = skip_block(&data[..split], &mut state).unwrap_err();
            let end = skip_block(&data[consumed..], &mut state).unwrap();
            assert_eq!(consumed + end, 12, "split {split}");
        }

        let data = b" a=\"}\"\"}\" } x";
        assert_eq!(skip_block(data, &mut Skip::new()), Ok(11));
    }

    #[test]
    fn test_skip_block_words() {
        let data = b" \"a long } quoted \\\" string\" # } {\n { a=b c={ d } } \"#\" } x { y } b";
        let mut state = Skip::new();
        assert_eq!(skip_block(data, &mut state), Ok(57));

        for split in 0..57 {
            let mut state = Skip::new();
            let consumed = skip_block(&data[..split], &mut state).unwrap_err();
            let end = skip_block(&data[consumed..], &mut state).unwrap();
            assert_eq!(consumed + end, 57, "split {split}");
        }
    }
}
