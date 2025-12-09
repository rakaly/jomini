use super::Rgb;
use crate::{Scalar, util::get_split};
use std::fmt;

/// The ID of current Lexeme
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LexemeId(pub u16);

impl LexemeId {
    /// A binary '{' (open bracket)
    pub const OPEN: LexemeId = LexemeId::new(0x0003);

    /// A binary '}' (close bracket)
    pub const CLOSE: LexemeId = LexemeId::new(0x0004);

    /// A binary '='
    pub const EQUAL: LexemeId = LexemeId::new(0x0001);

    /// A binary 32 bit unsigned integer
    pub const U32: LexemeId = LexemeId::new(0x0014);

    /// A binary 64 bit unsigned integer
    pub const U64: LexemeId = LexemeId::new(0x029c);

    /// A binary 32 bit signed integer
    pub const I32: LexemeId = LexemeId::new(0x000c);

    /// A binary boolean
    pub const BOOL: LexemeId = LexemeId::new(0x000e);

    /// A binary string that is typically quoted
    pub const QUOTED: LexemeId = LexemeId::new(0x000f);

    /// A binary string that is typically without quotes
    pub const UNQUOTED: LexemeId = LexemeId::new(0x0017);

    /// A binary 32 bit floating point
    pub const F32: LexemeId = LexemeId::new(0x000d);

    /// A binary 64 bit floating point
    pub const F64: LexemeId = LexemeId::new(0x0167);

    /// A binary RGB value
    pub const RGB: LexemeId = LexemeId::new(0x0243);

    /// A binary 64 bit signed integer
    pub const I64: LexemeId = LexemeId::new(0x0317);

    /// A binary 8-bit lookup index
    pub const LOOKUP_U8: LexemeId = LexemeId::new(0x0d40);

    /// A binary 24-bit lookup index
    pub const LOOKUP_U24: LexemeId = LexemeId::new(0x0d41);

    /// A binary 16-bit lookup index
    pub const LOOKUP_U16: LexemeId = LexemeId::new(0x0d3e);

    /// Compact 8-bit lookup index (alternative to LOOKUP_U8) (EU5 1.0.10+)
    /// Stores a u8 lookup table index in compact form
    pub const LOOKUP_U8_ALT: LexemeId = LexemeId::new(0x0d43);

    /// Compact 16-bit lookup index (alternative to LOOKUP_U16) (EU5 1.0.10+)
    /// Stores a u16 lookup table index in compact form
    pub const LOOKUP_U16_ALT: LexemeId = LexemeId::new(0x0d44);

    /// Fixed-point zero constant - 0 bytes of data (EU5 1.0.10+)
    /// Space efficient encoding of the value 0.0
    pub const FIXED5_ZERO: LexemeId = LexemeId::new(0x0d47);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 2.55 | Formula: raw / 100,000
    pub const FIXED5_U8: LexemeId = LexemeId::new(0x0d48);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 655.35 | Formula: raw / 100,000
    pub const FIXED5_U16: LexemeId = LexemeId::new(0x0d49);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 167,772.15 | Formula: raw / 100,000
    pub const FIXED5_U24: LexemeId = LexemeId::new(0x0d4a);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 42,949.67295 | Formula: raw / 100,000
    pub const FIXED5_U32: LexemeId = LexemeId::new(0x0d4b);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 10,995,116.27775 | Formula: raw / 100,000
    pub const FIXED5_U40: LexemeId = LexemeId::new(0x0d4c);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 281,474,976,710.65535 | Formula: raw / 100,000
    pub const FIXED5_U48: LexemeId = LexemeId::new(0x0d4d);

    /// Fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: 0.0 to 72,057,594,037,927.935 | Formula: raw / 100,000
    pub const FIXED5_U56: LexemeId = LexemeId::new(0x0d4e);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -2.55 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I8: LexemeId = LexemeId::new(0x0d4f);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -655.35 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I16: LexemeId = LexemeId::new(0x0d50);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -167,772.15 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I24: LexemeId = LexemeId::new(0x0d51);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -42,949.67295 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I32: LexemeId = LexemeId::new(0x0d52);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -281,474,976,710.65535 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I40: LexemeId = LexemeId::new(0x0d53);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -281,474,976,710.65535 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I48: LexemeId = LexemeId::new(0x0d54);

    /// Negative fixed-point decimal with 5 decimal places (EU5 1.0.10+)
    /// Value range: -72,057,594,037,927.935 to 0.0 | Formula: -(raw / 100,000)
    pub const FIXED5_I56: LexemeId = LexemeId::new(0x0d55);

    /// Construct a new [LexemeId] from a 16bit value
    #[inline]
    pub const fn new(x: u16) -> Self {
        LexemeId(x)
    }

    /// Identifies if the given ID does not match of the predefined [LexemeId]
    /// constants, and thus can be considered an ID token.
    ///
    /// ```rust
    /// use jomini::binary::LexemeId;
    /// let lid = LexemeId::new(0x1000);
    /// assert!(lid.is_id());
    /// ```
    #[inline]
    pub const fn is_id(&self) -> bool {
        matches!(self.into_kind(), TokenKind::Id)
    }

    /// Convert the `LexemeId` into a `TokenKind`
    #[inline]
    pub const fn into_kind(self) -> TokenKind {
        match self {
            LexemeId::OPEN => TokenKind::Open,
            LexemeId::CLOSE => TokenKind::Close,
            LexemeId::EQUAL => TokenKind::Equal,
            LexemeId::U32 => TokenKind::U32,
            LexemeId::U64 => TokenKind::U64,
            LexemeId::I32 => TokenKind::I32,
            LexemeId::BOOL => TokenKind::Bool,
            LexemeId::QUOTED => TokenKind::Quoted,
            LexemeId::UNQUOTED => TokenKind::Unquoted,
            LexemeId::F32 => TokenKind::F32,
            LexemeId::F64 => TokenKind::F64,
            LexemeId::RGB => TokenKind::Rgb,
            LexemeId::I64 => TokenKind::I64,
            LexemeId::LOOKUP_U8
            | LexemeId::LOOKUP_U8_ALT
            | LexemeId::LOOKUP_U16
            | LexemeId::LOOKUP_U16_ALT
            | LexemeId::LOOKUP_U24 => TokenKind::Lookup,
            // Fixed5 lexemes are converted to F64 tokens
            LexemeId::FIXED5_ZERO
            | LexemeId::FIXED5_U8
            | LexemeId::FIXED5_U16
            | LexemeId::FIXED5_U24
            | LexemeId::FIXED5_U32
            | LexemeId::FIXED5_U40
            | LexemeId::FIXED5_U48
            | LexemeId::FIXED5_U56
            | LexemeId::FIXED5_I8
            | LexemeId::FIXED5_I16
            | LexemeId::FIXED5_I24
            | LexemeId::FIXED5_I32
            | LexemeId::FIXED5_I40
            | LexemeId::FIXED5_I48
            | LexemeId::FIXED5_I56 => TokenKind::F64,
            _ => TokenKind::Id,
        }
    }
}

/// Kind of token represented by a [LexemeId]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    /// '{' token
    Open,

    /// '}' token
    Close,

    /// '=' token
    Equal,

    /// 32bit unsigned integer
    U32,

    /// 64bit unsigned integer
    U64,

    /// 32bit signed integer
    I32,

    /// boolean
    Bool,

    /// quoted string
    Quoted,

    /// unquoted string
    Unquoted,

    /// 32bits of floating point data
    F32,

    /// 64bits of floating point data
    F64,

    /// RGB value
    Rgb,

    /// 64bit signed integer
    I64,

    /// lookup index
    Lookup,

    /// Identifier token
    Id,
}

impl From<LexemeId> for TokenKind {
    #[inline]
    fn from(id: LexemeId) -> Self {
        id.into_kind()
    }
}

#[inline]
pub(crate) fn read_id(data: &[u8]) -> Result<(LexemeId, &[u8]), LexError> {
    let (head, rest) = get_split::<2>(data).ok_or(LexError::Eof)?;
    Ok((LexemeId::new(u16::from_le_bytes(*head)), rest))
}

#[inline]
pub(crate) fn read_string(data: &[u8]) -> Result<(Scalar<'_>, &[u8]), LexError> {
    let (head, rest) = get_split::<2>(data).ok_or(LexError::Eof)?;
    let text_len = usize::from(u16::from_le_bytes(*head));
    if text_len <= rest.len() {
        let (text, rest) = rest.split_at(text_len);
        Ok((Scalar::new(text), rest))
    } else {
        Err(LexError::Eof)
    }
}

#[inline]
pub(crate) fn read_bool(data: &[u8]) -> Result<(bool, &[u8]), LexError> {
    let (&first, rest) = data.split_first().ok_or(LexError::Eof)?;
    Ok((first != 0, rest))
}

#[inline]
pub(crate) fn read_u32(data: &[u8]) -> Result<(u32, &[u8]), LexError> {
    let (head, rest) = get_split::<4>(data).ok_or(LexError::Eof)?;
    Ok((u32::from_le_bytes(*head), rest))
}

#[inline]
pub(crate) fn read_u64(data: &[u8]) -> Result<(u64, &[u8]), LexError> {
    let (head, rest) = get_split::<8>(data).ok_or(LexError::Eof)?;
    Ok((u64::from_le_bytes(*head), rest))
}

#[inline]
pub(crate) fn read_i64(data: &[u8]) -> Result<(i64, &[u8]), LexError> {
    let (head, rest) = get_split::<8>(data).ok_or(LexError::Eof)?;
    Ok((i64::from_le_bytes(*head), rest))
}

#[inline]
pub(crate) fn read_i32(data: &[u8]) -> Result<(i32, &[u8]), LexError> {
    let (head, rest) = get_split::<4>(data).ok_or(LexError::Eof)?;
    Ok((i32::from_le_bytes(*head), rest))
}

#[inline]
pub(crate) fn read_f32(data: &[u8]) -> Result<(&[u8; 4], &[u8]), LexError> {
    get_split::<4>(data).ok_or(LexError::Eof)
}

#[inline]
pub(crate) fn read_f64(data: &[u8]) -> Result<(&[u8; 8], &[u8]), LexError> {
    get_split::<8>(data).ok_or(LexError::Eof)
}

#[inline]
pub(crate) fn read_lookup_u8(data: &[u8]) -> Result<(u8, &[u8]), LexError> {
    let (&first, rest) = data.split_first().ok_or(LexError::Eof)?;
    Ok((first, rest))
}

#[inline]
pub(crate) fn read_lookup_u16(data: &[u8]) -> Result<(u16, &[u8]), LexError> {
    let (head, rest) = get_split::<2>(data).ok_or(LexError::Eof)?;
    Ok((u16::from_le_bytes(*head), rest))
}

#[inline]
pub(crate) fn read_lookup_u24(data: &[u8]) -> Result<(u32, &[u8]), LexError> {
    let (head, rest) = get_split::<3>(data).ok_or(LexError::Eof)?;
    Ok((u32::from_le_bytes([head[0], head[1], head[2], 0]), rest))
}

#[inline]
pub(crate) fn read_compact_f64(
    lexeme: LexemeId,
    data: &[u8],
) -> Result<([u8; 8], &[u8]), LexError> {
    let offset = lexeme.0 - LexemeId::FIXED5_ZERO.0;
    let is_negative = offset > 7;
    let byte_count = offset - (is_negative as u16 * 7);
    let (data, rest) = data
        .split_at_checked(byte_count as usize)
        .ok_or(LexError::Eof)?;
    let mut buf = [0u8; 8];
    buf[..byte_count as usize].copy_from_slice(data);
    let sign = 1i64 - (is_negative as i64) * 2;
    let i64_bytes = (u64::from_le_bytes(buf) as i64 * sign).to_le_bytes();
    Ok((i64_bytes, rest))
}

#[inline]
pub(crate) fn read_rgb(data: &[u8]) -> Result<(Rgb, &[u8]), LexError> {
    let (start, data) = read_id(data)?;
    let (rtoken, data) = read_id(data)?;
    let (r, data) = read_u32(data)?;
    let (gtoken, data) = read_id(data)?;
    let (g, data) = read_u32(data)?;
    let (btoken, data) = read_id(data)?;
    let (b, data) = read_u32(data)?;
    let (next_tok, data) = read_id(data)?;
    match (start, rtoken, gtoken, btoken, next_tok) {
        (LexemeId::OPEN, LexemeId::U32, LexemeId::U32, LexemeId::U32, LexemeId::CLOSE) => {
            Ok((Rgb { r, g, b, a: None }, data))
        }
        (LexemeId::OPEN, LexemeId::U32, LexemeId::U32, LexemeId::U32, LexemeId::U32) => {
            let (a, data) = read_u32(data)?;
            let (end, data) = read_id(data)?;
            if end == LexemeId::CLOSE {
                let a = Some(a);
                Ok((Rgb { r, g, b, a }, data))
            } else {
                Err(LexError::InvalidRgb)
            }
        }
        _ => Err(LexError::InvalidRgb),
    }
}

/// Binary token containing raw tokens parsed from binary data
///
/// This binary token contains the yielded raw tokens, and won't match open and
/// close tokens together, nor does it make a determination if open and close
/// represents an array, object, or both.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    /// '{'
    Open,

    /// '}'
    Close,

    /// '='
    Equal,

    /// 32bit unsigned integer
    U32(u32),

    /// 64bit unsigned integer
    U64(u64),

    /// 32bit signed integer
    I32(i32),

    /// boolean
    Bool(bool),

    /// quoted text
    Quoted(Scalar<'a>),

    /// text that is not quoted
    Unquoted(Scalar<'a>),

    /// 32bits of floating point data
    F32([u8; 4]),

    /// 64bits of floating point data
    F64([u8; 8]),

    /// Rgb data
    Rgb(Rgb),

    /// 64bit signed integer
    I64(i64),

    /// lookup index
    Lookup(u32),

    /// token id that can be resolved to a string via a
    /// [TokenResolver](crate::binary::TokenResolver)
    Id(u16),
}

impl Token<'_> {
    fn write_u32(mut wtr: impl std::io::Write, num: u32) -> Result<(), std::io::Error> {
        wtr.write_all(&LexemeId::U32.0.to_le_bytes())?;
        wtr.write_all(&num.to_le_bytes())
    }

    /// Write the binary representation of a token to a writer
    ///
    /// ```rust
    /// use jomini::binary::Token;
    /// let out = Vec::new();
    /// let mut cursor = std::io::Cursor::new(out);
    /// for token in &[Token::Id(0x00e1), Token::Equal, Token::U32(10)] {
    ///     token.write(&mut cursor)?;
    /// }
    ///
    /// assert_eq!(&cursor.into_inner(), &[0xe1, 0x00, 0x01, 0x00, 0x14, 0x00, 0x0a, 0x00, 0x00, 0x00]);
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn write(&self, mut wtr: impl std::io::Write) -> Result<(), std::io::Error> {
        match self {
            Token::Open => wtr.write_all(&LexemeId::OPEN.0.to_le_bytes()),
            Token::Close => wtr.write_all(&LexemeId::CLOSE.0.to_le_bytes()),
            Token::Equal => wtr.write_all(&LexemeId::EQUAL.0.to_le_bytes()),
            Token::U32(x) => Token::write_u32(wtr, *x),
            Token::U64(x) => {
                wtr.write_all(&LexemeId::U64.0.to_le_bytes())?;
                wtr.write_all(&x.to_le_bytes())
            }
            Token::I32(x) => {
                wtr.write_all(&LexemeId::I32.0.to_le_bytes())?;
                wtr.write_all(&x.to_le_bytes())
            }
            Token::Bool(x) => {
                wtr.write_all(&LexemeId::BOOL.0.to_le_bytes())?;
                wtr.write_all(&[if *x { 1u8 } else { 0 }])
            }
            Token::Quoted(x) => {
                wtr.write_all(&LexemeId::QUOTED.0.to_le_bytes())?;
                wtr.write_all(&(x.as_bytes().len() as u16).to_le_bytes())?;
                wtr.write_all(x.as_bytes())
            }
            Token::Unquoted(x) => {
                wtr.write_all(&LexemeId::UNQUOTED.0.to_le_bytes())?;
                wtr.write_all(&(x.as_bytes().len() as u16).to_le_bytes())?;
                wtr.write_all(x.as_bytes())
            }
            Token::F32(x) => {
                wtr.write_all(&LexemeId::F32.0.to_le_bytes())?;
                wtr.write_all(x)
            }
            Token::F64(x) => {
                wtr.write_all(&LexemeId::F64.0.to_le_bytes())?;
                wtr.write_all(x)
            }
            Token::Rgb(x) => {
                wtr.write_all(&LexemeId::RGB.0.to_le_bytes())?;
                wtr.write_all(&LexemeId::OPEN.0.to_le_bytes())?;
                Token::write_u32(&mut wtr, x.r)?;
                Token::write_u32(&mut wtr, x.g)?;
                Token::write_u32(&mut wtr, x.b)?;
                if let Some(a) = x.a.as_ref() {
                    Token::write_u32(&mut wtr, *a)?;
                }
                wtr.write_all(&LexemeId::CLOSE.0.to_le_bytes())
            }
            Token::I64(x) => {
                wtr.write_all(&LexemeId::I64.0.to_le_bytes())?;
                wtr.write_all(&x.to_le_bytes())
            }
            Token::Lookup(x) => {
                wtr.write_all(&LexemeId::LOOKUP_U24.0.to_le_bytes())?;
                wtr.write_all(&x.to_le_bytes()[..3])
            }
            Token::Id(x) => wtr.write_all(&x.to_le_bytes()),
        }
    }
}

#[inline]
pub(crate) fn read_token(data: &[u8]) -> Result<(Token<'_>, &[u8]), LexError> {
    let (id, data) = read_id(data)?;
    match id {
        LexemeId::OPEN => Ok((Token::Open, data)),
        LexemeId::CLOSE => Ok((Token::Close, data)),
        LexemeId::EQUAL => Ok((Token::Equal, data)),
        LexemeId::U32 => read_u32(data).map(|(x, d)| (Token::U32(x), d)),
        LexemeId::U64 => read_u64(data).map(|(x, d)| (Token::U64(x), d)),
        LexemeId::I32 => read_i32(data).map(|(x, d)| (Token::I32(x), d)),
        LexemeId::BOOL => read_bool(data).map(|(x, d)| (Token::Bool(x), d)),
        LexemeId::QUOTED => read_string(data).map(|(x, d)| (Token::Quoted(x), d)),
        LexemeId::UNQUOTED => read_string(data).map(|(x, d)| (Token::Unquoted(x), d)),
        LexemeId::F32 => read_f32(data).map(|(x, d)| (Token::F32(*x), d)),
        LexemeId::F64 => read_f64(data).map(|(x, d)| (Token::F64(*x), d)),
        LexemeId::RGB => read_rgb(data).map(|(x, d)| (Token::Rgb(x), d)),
        LexemeId::I64 => read_i64(data).map(|(x, d)| (Token::I64(x), d)),
        LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
            read_lookup_u8(data).map(|(x, d)| (Token::Lookup(x as u32), d))
        }
        LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
            read_lookup_u16(data).map(|(x, d)| (Token::Lookup(x as u32), d))
        }
        LexemeId::LOOKUP_U24 => read_lookup_u24(data).map(|(x, d)| (Token::Lookup(x), d)),
        lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
            read_compact_f64(lexeme, data).map(|(x, d)| (Token::F64(x), d))
        }
        LexemeId(id) => Ok((Token::Id(id), data)),
    }
}

/// Lexical error type without positional information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError {
    /// Data ended too soon
    Eof,

    /// An invalid RGB block encountered
    InvalidRgb,
}

impl std::error::Error for LexError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LexError::Eof => write!(f, "unexpected end of file"),
            LexError::InvalidRgb => write!(f, "invalid rgb data encountered",),
        }
    }
}

impl LexError {
    #[inline]
    #[must_use]
    pub(crate) fn at(self, position: usize) -> LexerError {
        LexerError {
            position,
            kind: self,
        }
    }
}

/// Lexical error type with positional information
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexerError {
    position: usize,
    kind: LexError,
}

impl LexerError {
    /// Return the byte position where the error occurred
    pub fn position(&self) -> usize {
        self.position
    }

    /// Return a reference the error kind
    pub fn kind(&self) -> &LexError {
        &self.kind
    }

    /// Consume self and return the error kind
    #[must_use]
    pub fn into_kind(self) -> LexError {
        self.kind
    }
}

impl std::error::Error for LexerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            LexError::Eof => write!(f, "not enough data to read at {}", self.position),
            LexError::InvalidRgb => write!(f, "invalid rgb data encountered at {}", self.position),
        }
    }
}

/// Zero cost binary data scanner.
///
/// There are two main ways to drive the lexer. To see them in action, imagine
/// we want to count the max amount of nesting.
///
/// ```rust
/// use jomini::binary::{Lexer, Token};
/// let mut lexer = Lexer::new(&[0x2d, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x04, 0x00]);
/// let mut max_depth = 0;
/// let mut current_depth = 0;
/// while let Some(token) = lexer.next_token()? {
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
/// # Ok::<(), jomini::binary::LexerError>(())
/// ```
///
/// The [Lexer::next_token] is an ergonomic way to scan through binary tokens.
/// The functions prefixed with `read_`denote more data is expected, while
/// `next_` allows for the data to finish.
///
/// If it is desired scan through the binary data with zero overhead, one needs
/// to drive the lexer more thoroughly.
///
/// ```rust
/// use jomini::binary::{Lexer, LexemeId};
/// let mut lexer = Lexer::new(&[0x2d, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x04, 0x00]);
/// let mut max_depth = 0;
/// let mut current_depth = 0;
/// while let Some(id) = lexer.next_id()? {
///   match id {
///     LexemeId::OPEN => {
///       current_depth += 1;
///       max_depth = max_depth.max(current_depth);
///     }
///     LexemeId::CLOSE => current_depth -= 1,
///     LexemeId::U32 => { lexer.read_u32()?; }
///     LexemeId::I32 => { lexer.read_i32()?; }
///     LexemeId::BOOL => { lexer.read_bool()?; }
///     LexemeId::QUOTED | LexemeId::UNQUOTED => { lexer.read_string()?; }
///     LexemeId::F32 => { lexer.read_f32()?; }
///     LexemeId::F64 => { lexer.read_f64()?; }
///     LexemeId::I64 => { lexer.read_i64()?; }
///     _ => {}
///   }
/// }
/// assert_eq!(max_depth, 2);
/// # Ok::<(), jomini::binary::LexerError>(())
/// ```
///
/// Only at token boundaries can `token` functions be intertwined with the
/// individual lexeme functions.
///
/// Errors reported will contain positional information.
pub struct Lexer<'a> {
    data: &'a [u8],
    original_length: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a new lexer over the given data
    #[inline]
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            original_length: data.len(),
        }
    }

    /// Returns the remaining data that has not yet been processed.
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexemeId};
    /// let mut lexer = Lexer::new(&[0xd2, 0x28, 0xff]);
    /// assert_eq!(lexer.read_id().unwrap(), LexemeId::new(0x28d2));
    /// assert_eq!(lexer.remainder(), &[0xff]);
    /// ```
    #[inline]
    pub fn remainder(&self) -> &'a [u8] {
        self.data
    }

    /// Returns how many bytes have been processed by the lexer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexemeId};
    /// let mut lexer = Lexer::new(&[0xd2, 0x28, 0xff]);
    /// assert_eq!(lexer.read_id().unwrap(), LexemeId::new(0x28d2));
    /// assert_eq!(lexer.position(), 2);
    /// ```
    #[inline]
    pub fn position(&self) -> usize {
        self.original_length - self.data.len()
    }

    #[inline]
    fn err_position(&self, err: LexError) -> LexerError {
        err.at(self.position())
    }

    /// Advance the lexer through the next lexeme id, and return it
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexemeId, LexError};
    /// let mut lexer = Lexer::new(&[0x2d, 0x28]);
    /// assert_eq!(lexer.read_id(), Ok(LexemeId::new(0x282d)));
    /// assert_eq!(lexer.read_id().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_id(&mut self) -> Result<LexemeId, LexerError> {
        let (result, rest) = read_id(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Attempt to advance through the [LexemeId]
    ///
    /// An EOF error can still be thrown if data is present but not enough
    /// exists to decode the next [LexemeId]
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexemeId, LexError};
    /// let mut lexer = Lexer::new(&[0x2d, 0x28]);
    /// assert_eq!(lexer.next_id(), Ok(Some(LexemeId::new(0x282d))));
    /// assert_eq!(lexer.next_id(), Ok(None));
    ///
    /// let mut lexer = Lexer::new(&[0x2d]);
    /// assert_eq!(lexer.next_id().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn next_id(&mut self) -> Result<Option<LexemeId>, LexerError> {
        match read_id(self.data) {
            Ok((result, rest)) => {
                self.data = rest;
                Ok(Some(result))
            }
            Err(LexError::Eof) if self.remainder().is_empty() => Ok(None),
            Err(e) => Err(self.err_position(e)),
        }
    }

    /// Assume more tokens exist in the data and read the next one.
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError, Token};
    /// let mut lexer = Lexer::new(&[0x2d, 0x28]);
    /// assert_eq!(lexer.read_token(), Ok(Token::Id(0x282d)));
    /// assert_eq!(lexer.read_token().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_token(&mut self) -> Result<Token<'a>, LexerError> {
        let (result, rest) = read_token(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Attempt to advance through the next token or return `None` if no data remains
    ///
    /// An EOF error can still be thrown if data is present but not enough
    /// exists to decode the next token.
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, Token, LexError};
    /// let mut lexer = Lexer::new(&[0x2d, 0x28]);
    /// assert_eq!(lexer.next_token(), Ok(Some(Token::Id(0x282d))));
    /// assert_eq!(lexer.next_token(), Ok(None));
    ///
    /// let mut lexer = Lexer::new(&[0x2d]);
    /// assert_eq!(lexer.next_token().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn next_token(&mut self) -> Result<Option<Token<'a>>, LexerError> {
        match read_token(self.data) {
            Ok((result, rest)) => {
                self.data = rest;
                Ok(Some(result))
            }
            Err(LexError::Eof) if self.remainder().is_empty() => Ok(None),
            Err(e) => Err(self.err_position(e)),
        }
    }

    /// Peek at the next [LexemeId] without advancing the lexer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError, LexemeId};
    /// let mut lexer = Lexer::new(&[0x01, 0x00][..]);
    /// assert_eq!(lexer.peek_id(), Some(LexemeId::EQUAL));
    /// assert_eq!(lexer.read_id(), Ok(LexemeId::EQUAL));
    /// assert_eq!(lexer.peek_id(), None);
    /// ```
    #[inline]
    pub fn peek_id(&self) -> Option<LexemeId> {
        self.data
            .get(..2)
            .map(|head| LexemeId::new(u16::from_le_bytes([head[0], head[1]])))
    }

    /// Peek at the next [Token] without advancing the lexer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError, Token};
    /// let mut lexer = Lexer::new(&[0x01, 0x00][..]);
    /// assert_eq!(lexer.peek_token(), Some(Token::Equal));
    /// assert_eq!(lexer.read_token(), Ok(Token::Equal));
    /// assert_eq!(lexer.peek_token(), None);
    /// ```
    #[inline]
    pub fn peek_token(&self) -> Option<Token<'a>> {
        read_token(self.data).ok().map(|(t, _)| t)
    }

    /// Advance the lexer through a length prefixed string
    ///
    /// ```rust
    /// use jomini::{Scalar, binary::{Lexer, LexError}};
    /// let mut lexer = Lexer::new(&[0x03, 0x00, 0x45, 0x4e, 0x47][..]);
    /// assert_eq!(lexer.read_string(), Ok(Scalar::new(b"ENG")));
    /// assert_eq!(lexer.read_string().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_string(&mut self) -> Result<Scalar<'a>, LexerError> {
        let (result, rest) = read_string(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through a boolean
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(&[0x01, 0x00][..]);
    /// assert_eq!(lexer.read_bool(), Ok(true));
    /// assert_eq!(lexer.read_bool(), Ok(false));
    /// assert_eq!(lexer.read_bool().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_bool(&mut self) -> Result<bool, LexerError> {
        let (result, rest) = read_bool(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through an 8-bit lookup index
    #[inline]
    pub fn read_lookup_u8(&mut self) -> Result<u8, LexerError> {
        let (result, rest) = read_lookup_u8(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through a 16-bit lookup index
    #[inline]
    pub fn read_lookup_u16(&mut self) -> Result<u16, LexerError> {
        let (result, rest) = read_lookup_u16(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through a 24-bit lookup index
    #[inline]
    pub fn read_lookup_u24(&mut self) -> Result<u32, LexerError> {
        let (result, rest) = read_lookup_u24(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through unsigned little endian 32 bit integer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(&[0x59, 0x00, 0x00, 0x00][..]);
    /// assert_eq!(lexer.read_u32(), Ok(89));
    /// assert_eq!(lexer.read_u32().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_u32(&mut self) -> Result<u32, LexerError> {
        let (result, rest) = read_u32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through unsigned little endian 64 bit integer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(&[0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00][..]);
    /// assert_eq!(lexer.read_u64(), Ok(128));
    /// assert_eq!(lexer.read_u64().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_u64(&mut self) -> Result<u64, LexerError> {
        let (result, rest) = read_u64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through signed little endian 64 bit integer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(&[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff][..]);
    /// assert_eq!(lexer.read_i64(), Ok(-1));
    /// assert_eq!(lexer.read_i64().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_i64(&mut self) -> Result<i64, LexerError> {
        let (result, rest) = read_i64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through signed little endian 32 bit integer
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(&[0x59, 0x00, 0x00, 0x00][..]);
    /// assert_eq!(lexer.read_i32(), Ok(89));
    /// assert_eq!(lexer.read_i32().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_i32(&mut self) -> Result<i32, LexerError> {
        let (result, rest) = read_i32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance the lexer through 32 bits of floating point data and return the bytes
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let data = [0x17, 0x00, 0x00, 0x00];
    /// let mut lexer = Lexer::new(&data[..]);
    /// assert_eq!(lexer.read_f32(), Ok(data));
    /// assert_eq!(lexer.read_f32().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_f32(&mut self) -> Result<[u8; 4], LexerError> {
        let (result, rest) = read_f32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(*result)
    }

    /// Advance the lexer through 64 bits of floating point data and return the bytes
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let data = [0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
    /// let mut lexer = Lexer::new(&data[..]);
    /// assert_eq!(lexer.read_f64(), Ok(data));
    /// assert_eq!(lexer.read_f64().unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_f64(&mut self) -> Result<[u8; 8], LexerError> {
        let (result, rest) = read_f64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(*result)
    }

    /// Advance the lexer through an rgb value (with optional alpha channel)
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError, Rgb};
    /// let data = [0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
    ///             0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00,
    ///             0x1b, 0x00, 0x00, 0x00, 0x04, 0x00];
    /// let mut lexer = Lexer::new(&data[..]);
    /// assert_eq!(lexer.read_rgb(), Ok(Rgb { r: 110, g: 27, b: 27, a: None }));
    /// assert_eq!(lexer.read_rgb().unwrap_err().kind(), &LexError::Eof);
    /// ```
    pub fn read_rgb(&mut self) -> Result<Rgb, LexerError> {
        let (result, rest) = read_rgb(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Advance a given number of bytes and return them
    ///
    /// ```rust
    /// use jomini::binary::{Lexer, LexError};
    /// let mut lexer = Lexer::new(b"EU4bin");
    /// assert_eq!(lexer.read_bytes(6), Ok(&b"EU4bin"[..]));
    /// assert_eq!(lexer.read_bytes(1).unwrap_err().kind(), &LexError::Eof);
    /// ```
    #[inline]
    pub fn read_bytes(&mut self, bytes: usize) -> Result<&'a [u8], LexerError> {
        if self.data.len() >= bytes {
            let (head, rest) = self.data.split_at(bytes);
            self.data = rest;
            Ok(head)
        } else {
            Err(self.err_position(LexError::Eof))
        }
    }

    #[inline]
    pub(crate) fn read_compact_f64(&mut self, lexeme: LexemeId) -> Result<[u8; 8], LexerError> {
        let (result, rest) =
            read_compact_f64(lexeme, self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    /// Skip the value denoted by the [LexemeId]. Will skip entire containers.
    #[inline]
    pub fn skip_value(&mut self, id: LexemeId) -> Result<(), LexerError> {
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                self.read_string()?;
                Ok(())
            }
            LexemeId::U32 => {
                self.read_u32()?;
                Ok(())
            }
            LexemeId::I32 => {
                self.read_i32()?;
                Ok(())
            }
            LexemeId::U64 => {
                self.read_u64()?;
                Ok(())
            }
            LexemeId::I64 => {
                self.read_i64()?;
                Ok(())
            }
            LexemeId::BOOL => {
                self.read_bool()?;
                Ok(())
            }
            LexemeId::F32 => {
                self.read_f32()?;
                Ok(())
            }
            LexemeId::F64 => {
                self.read_f64()?;
                Ok(())
            }
            LexemeId::OPEN => self.skip_container(),
            LexemeId::RGB => {
                self.read_rgb()?;
                Ok(())
            }
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                self.read_lookup_u8()?;
                Ok(())
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                self.read_lookup_u16()?;
                Ok(())
            }
            LexemeId::LOOKUP_U24 => {
                self.read_lookup_u24()?;
                Ok(())
            }
            lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
                self.read_compact_f64(lexeme)?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    #[inline]
    fn skip_container(&mut self) -> Result<(), LexerError> {
        let mut depth = 1;
        loop {
            match self.read_id()? {
                LexemeId::QUOTED | LexemeId::UNQUOTED => {
                    self.read_string()?;
                }
                LexemeId::U32 => {
                    self.read_u32()?;
                }
                LexemeId::I32 => {
                    self.read_i32()?;
                }
                LexemeId::U64 => {
                    self.read_u64()?;
                }
                LexemeId::I64 => {
                    self.read_i64()?;
                }
                LexemeId::BOOL => {
                    self.read_bool()?;
                }
                LexemeId::F32 => {
                    self.read_f32()?;
                }
                LexemeId::F64 => {
                    self.read_f64()?;
                }
                LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                    self.read_lookup_u8()?;
                }
                LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                    self.read_lookup_u16()?;
                }
                LexemeId::LOOKUP_U24 => {
                    self.read_lookup_u24()?;
                }
                LexemeId::CLOSE => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
                    self.read_compact_f64(lexeme)?;
                }
                LexemeId::OPEN => depth += 1,
                _ => {}
            }
        }
    }
}
