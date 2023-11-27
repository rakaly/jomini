use super::Rgb;
use crate::{util::get_split, Scalar};
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LexemeId(pub u16);

impl LexemeId {
    pub const OPEN: LexemeId = LexemeId::new(0x0003);
    pub const END: LexemeId = LexemeId::new(0x0004);
    pub const EQUAL: LexemeId = LexemeId::new(0x0001);
    pub const U32: LexemeId = LexemeId::new(0x0014);
    pub const U64: LexemeId = LexemeId::new(0x029c);
    pub const I32: LexemeId = LexemeId::new(0x000c);
    pub const BOOL: LexemeId = LexemeId::new(0x000e);
    pub const QUOTED_STRING: LexemeId = LexemeId::new(0x000f);
    pub const UNQUOTED_STRING: LexemeId = LexemeId::new(0x0017);
    pub const F32: LexemeId = LexemeId::new(0x000d);
    pub const F64: LexemeId = LexemeId::new(0x0167);
    pub const RGB: LexemeId = LexemeId::new(0x0243);
    pub const I64: LexemeId = LexemeId::new(0x0317);

    #[inline]
    pub const fn new(x: u16) -> Self {
        LexemeId(x)
    }

    #[inline]
    pub const fn is_id(&self) -> bool {
        !matches!(
            *self,
            LexemeId::OPEN
                | LexemeId::END
                | LexemeId::EQUAL
                | LexemeId::U32
                | LexemeId::U64
                | LexemeId::I32
                | LexemeId::BOOL
                | LexemeId::QUOTED_STRING
                | LexemeId::UNQUOTED_STRING
                | LexemeId::F32
                | LexemeId::F64
                | LexemeId::RGB
                | LexemeId::I64
        )
    }
}

#[inline]
pub(crate) fn read_id(data: &[u8]) -> Result<(LexemeId, &[u8]), LexError> {
    let (head, rest) = get_split::<2>(data).ok_or(LexError::Eof)?;
    Ok((LexemeId::new(u16::from_le_bytes(head)), rest))
}

#[inline]
pub(crate) fn read_string(data: &[u8]) -> Result<(Scalar, &[u8]), LexError> {
    let (head, rest) = get_split::<2>(data).ok_or(LexError::Eof)?;
    let text_len = usize::from(u16::from_le_bytes(head));
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
    Ok((u32::from_le_bytes(head), rest))
}

#[inline]
pub(crate) fn read_u64(data: &[u8]) -> Result<(u64, &[u8]), LexError> {
    let (head, rest) = get_split::<8>(data).ok_or(LexError::Eof)?;
    Ok((u64::from_le_bytes(head), rest))
}

#[inline]
pub(crate) fn read_i64(data: &[u8]) -> Result<(i64, &[u8]), LexError> {
    let (head, rest) = get_split::<8>(data).ok_or(LexError::Eof)?;
    Ok((i64::from_le_bytes(head), rest))
}

#[inline]
pub(crate) fn read_i32(data: &[u8]) -> Result<(i32, &[u8]), LexError> {
    let (head, rest) = get_split::<4>(data).ok_or(LexError::Eof)?;
    Ok((i32::from_le_bytes(head), rest))
}

#[inline]
pub(crate) fn read_f32(data: &[u8]) -> Result<([u8; 4], &[u8]), LexError> {
    let (head, rest) = get_split::<4>(data).ok_or(LexError::Eof)?;
    Ok((head, rest))
}

#[inline]
pub(crate) fn read_f64(data: &[u8]) -> Result<([u8; 8], &[u8]), LexError> {
    let (head, rest) = get_split::<8>(data).ok_or(LexError::Eof)?;
    Ok((head, rest))
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
        (LexemeId::OPEN, LexemeId::U32, LexemeId::U32, LexemeId::U32, LexemeId::END) => {
            Ok((Rgb { r, g, b, a: None }, data))
        }
        (LexemeId::OPEN, LexemeId::U32, LexemeId::U32, LexemeId::U32, LexemeId::U32) => {
            let (a, data) = read_u32(data)?;
            let (end, data) = read_id(data)?;
            if end == LexemeId::END {
                let a = Some(a);
                Ok((Rgb { r, g, b, a }, data))
            } else {
                Err(LexError::InvalidRgb)
            }
        }
        _ => Err(LexError::InvalidRgb),
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'a> {
    Open,
    End,
    Equal,
    U32(u32),
    U64(u64),
    I32(i32),
    Bool(bool),
    QuotedString(Scalar<'a>),
    UnquotedString(Scalar<'a>),
    F32([u8; 4]),
    F64([u8; 8]),
    RGB(Rgb),
    I64(i64),
    Other(u16),
}

#[inline]
pub(crate) fn read_token(data: &[u8]) -> Result<(Token, &[u8]), LexError> {
    let (id, data) = read_id(data)?;
    match id {
        LexemeId::OPEN => Ok((Token::Open, data)),
        LexemeId::END => Ok((Token::End, data)),
        LexemeId::EQUAL => Ok((Token::Equal, data)),
        LexemeId::U32 => read_u32(data).map(|(x, d)| (Token::U32(x), d)),
        LexemeId::U64 => read_u64(data).map(|(x, d)| (Token::U64(x), d)),
        LexemeId::I32 => read_i32(data).map(|(x, d)| (Token::I32(x), d)),
        LexemeId::BOOL => read_bool(data).map(|(x, d)| (Token::Bool(x), d)),
        LexemeId::QUOTED_STRING => read_string(data).map(|(x, d)| (Token::QuotedString(x), d)),
        LexemeId::UNQUOTED_STRING => read_string(data).map(|(x, d)| (Token::UnquotedString(x), d)),
        LexemeId::F32 => read_f32(data).map(|(x, d)| (Token::F32(x), d)),
        LexemeId::F64 => read_f64(data).map(|(x, d)| (Token::F64(x), d)),
        LexemeId::RGB => read_rgb(data).map(|(x, d)| (Token::RGB(x), d)),
        LexemeId::I64 => read_i64(data).map(|(x, d)| (Token::I64(x), d)),
        LexemeId(id) => Ok((Token::Other(id), data)),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError {
    Eof,
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
    pub fn at(self, position: usize) -> LexerError {
        LexerError {
            position,
            kind: self,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexerError {
    position: usize,
    kind: LexError,
}

impl LexerError {
    pub fn position(&self) -> usize {
        self.position
    }

    pub fn kind(&self) -> &LexError {
        &self.kind
    }

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

pub struct Lexer<'a> {
    data: &'a [u8],
    original_length: usize,
}

impl<'a> Lexer<'a> {
    #[inline]
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            original_length: data.len(),
        }
    }

    #[inline]
    pub fn remainder(&self) -> &'a [u8] {
        self.data
    }

    #[inline]
    pub fn position(&self) -> usize {
        self.original_length - self.data.len()
    }

    #[inline]
    fn err_position(&self, err: LexError) -> LexerError {
        err.at(self.position())
    }

    #[inline]
    pub fn read_id(&mut self) -> Result<LexemeId, LexerError> {
        let (result, rest) = read_id(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn next_token(&mut self) -> Result<Token<'a>, LexerError> {
        let (result, rest) = read_token(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn peek(&mut self) -> Option<LexemeId> {
        self.data
            .get(..2)
            .map(|head| LexemeId::new(u16::from_le_bytes([head[0], head[1]])))
    }

    #[inline]
    pub fn read_string(&mut self) -> Result<Scalar<'a>, LexerError> {
        let (result, rest) = read_string(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_bool(&mut self) -> Result<bool, LexerError> {
        let (result, rest) = read_bool(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_u32(&mut self) -> Result<u32, LexerError> {
        let (result, rest) = read_u32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_u64(&mut self) -> Result<u64, LexerError> {
        let (result, rest) = read_u64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_i64(&mut self) -> Result<i64, LexerError> {
        let (result, rest) = read_i64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_i32(&mut self) -> Result<i32, LexerError> {
        let (result, rest) = read_i32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_f32(&mut self) -> Result<[u8; 4], LexerError> {
        let (result, rest) = read_f32(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn read_f64(&mut self) -> Result<[u8; 8], LexerError> {
        let (result, rest) = read_f64(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }

    #[inline]
    pub fn skip_value(&mut self, id: LexemeId) -> Result<(), LexerError> {
        match id {
            LexemeId::QUOTED_STRING | LexemeId::UNQUOTED_STRING => {
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
            _ => Ok(()),
        }
    }

    #[inline]
    fn skip_container(&mut self) -> Result<(), LexerError> {
        let mut depth = 1;
        loop {
            match self.read_id()? {
                LexemeId::QUOTED_STRING | LexemeId::UNQUOTED_STRING => {
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
                LexemeId::END => {
                    depth -= 1;
                    if depth == 0 {
                        return Ok(());
                    }
                }
                LexemeId::OPEN => depth += 1,
                _ => {}
            }
        }
    }

    pub fn read_rgb(&mut self) -> Result<Rgb, LexerError> {
        let (result, rest) = read_rgb(self.data).map_err(|e| self.err_position(e))?;
        self.data = rest;
        Ok(result)
    }
}
