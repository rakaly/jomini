use std::ops::{Deref, DerefMut};

use crate::binary::{LexemeId, TokenKind};

struct ParserBuf {
    buf: Box<[u8]>,
    start: u16,
    end: u16,
}

impl ParserBuf {
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        unsafe {
            self.buf
                .get_unchecked(self.start as usize..self.end as usize)
        }
    }

    #[inline]
    pub fn spare_capacity_mut(&mut self) -> MutableBuffer<'_> {
        let carry_over = self.end - self.start;
        if carry_over == 0 {
            self.start = 0;
            self.end = 0;
            return MutableBuffer {
                buf: self,
                carry_over: 0,
            };
        }

        self.buf
            .copy_within(self.start as usize..self.end as usize, 0);
        self.start = 0;
        self.end = carry_over;
        MutableBuffer {
            buf: self,
            carry_over,
        }
    }

    fn range(&self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }

    #[inline]
    fn peek_bytes<const N: usize>(&self) -> Option<&'_ [u8; N]> {
        let result = unsafe { self.buf.get_unchecked(self.range()) }.first_chunk::<N>()?;
        Some(result)
    }

    #[inline]
    fn read_bytes<const N: usize>(&mut self) -> Option<&'_ [u8; N]> {
        let result = unsafe { self.buf.get_unchecked(self.range()) }.first_chunk::<N>()?;
        self.end += N as u16;
        Some(result)
    }

    #[inline]
    fn read_slice(&mut self, len: u16) -> Option<&'_ [u8]> {
        let result = unsafe { self.buf.get_unchecked(self.range()) }.get(..len as usize)?;
        self.end += len;
        Some(result)
    }
}

struct MutableBuffer<'a> {
    buf: &'a mut ParserBuf,
    carry_over: u16,
}

impl<'a> MutableBuffer<'a> {
    pub unsafe fn set_len(&mut self, amt: u16) {
        self.buf.end += amt;
    }
}

impl<'a> Deref for MutableBuffer<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe { self.buf.buf.get_unchecked(self.carry_over as usize..) }
    }
}

impl<'a> DerefMut for MutableBuffer<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.buf.buf.get_unchecked_mut(self.carry_over as usize..) }
    }
}

pub struct ParserState {
    buf: ParserBuf,
    storage: [u8; 8],
    // storage_request: u16,
}

impl ParserState {
    #[inline]
    fn read_bytes<const N: usize>(&mut self) -> Option<&[u8; N]> {
        self.buf.read_bytes::<N>()
    }

    #[inline]
    fn store<const N: usize>(&mut self, data: [u8; N]) {
        self.storage[..N].copy_from_slice(&data);
    }

    // #[inline]
    // fn buffer(&mut self, len: u16) {
    //     self.storage_request = len;
    // }
}

enum TokenSignal {
    Kind(TokenKind),
    Eof,
}

enum SkipKind {
    Open,
    Close,
    Other,
}

struct Error;

pub trait BinaryFormat {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error>;
    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        match self.visit(reader, id) {
            Ok(TokenSignal::Kind(TokenKind::Open)) => Some(SkipKind::Open),
            Ok(TokenSignal::Kind(TokenKind::Close)) => Some(SkipKind::Close),
            Ok(TokenSignal::Kind(_)) => Some(SkipKind::Other),
            Ok(TokenSignal::Eof) | Err(_) => None,
        }
    }

    fn resolve_id(&self, token: u16) -> Option<&str> {
        None
    }

    fn resolve_lookup(&self, token: u32) -> Option<&str> {
        None
    }
}

struct Eu4Format;

impl BinaryFormat for Eu4Format {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error> {
        match id {
            LexemeId::OPEN => Ok(TokenSignal::Kind(TokenKind::Open)),
            LexemeId::CLOSE => Ok(TokenSignal::Kind(TokenKind::Close)),
            LexemeId::EQUAL => Ok(TokenSignal::Kind(TokenKind::Equal)),
            LexemeId::U32 | LexemeId::I32 => {
                let Some(data) = reader.read_bytes::<4>().copied() else {
                    return Ok(TokenSignal::Eof);
                };

                reader.store(data);
                if id == LexemeId::I32 {
                    Ok(TokenSignal::Kind(TokenKind::I32))
                } else {
                    Ok(TokenSignal::Kind(TokenKind::U32))
                }
            }
            LexemeId::U64 | LexemeId::I64 => {
                let Some(data) = reader.read_bytes::<8>().copied() else {
                    return Ok(TokenSignal::Eof);
                };

                reader.store(data);
                if id == LexemeId::I64 {
                    Ok(TokenSignal::Kind(TokenKind::I64))
                } else {
                    Ok(TokenSignal::Kind(TokenKind::U64))
                }
            }
            LexemeId::F32 => {
                let Some(data) = reader.read_bytes::<4>().copied() else {
                    return Ok(TokenSignal::Eof);
                };

                let result = eu4_specific_f32_decoding(&data).to_bits();
                reader.store(result.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::F32))
            }
            LexemeId::F64 => {
                let Some(data) = reader.read_bytes::<8>().copied() else {
                    return Ok(TokenSignal::Eof);
                };

                let result = eu4_specific_f64_decoding(&data).to_bits();
                reader.store(result.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::F64))
            }
            LexemeId::BOOL => {
                let Some(data) = reader.read_bytes::<1>().copied() else {
                    return Ok(TokenSignal::Eof);
                };

                reader.store(data);
                Ok(TokenSignal::Kind(TokenKind::Bool))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(len) = reader.read_bytes::<2>().copied().map(u16::from_le_bytes) else {
                    return Ok(TokenSignal::Eof);
                };

                reader.store(len.to_le_bytes());
                // reader.buffer(len);
                if id == LexemeId::QUOTED {
                    Ok(TokenSignal::Kind(TokenKind::Quoted))
                } else {
                    Ok(TokenSignal::Kind(TokenKind::Unquoted))
                }
            }
            id => {
                // Signal to call resolve_id
                Ok(TokenSignal::Kind(TokenKind::Id))
            }
        }
    }

    // Replace TokenResolver
    fn resolve_id(&self, token: u16) -> Option<&str> {
        eu4_specific_token_resolution(token)
    }
}

fn eu4_specific_f32_decoding(data: &[u8; 4]) -> f32 {
    f32::from_le_bytes(*data)
}

fn eu4_specific_f64_decoding(data: &[u8; 8]) -> f64 {
    f64::from_le_bytes(*data)
}

fn eu4_specific_token_resolution(token: u16) -> Option<&'static str> {
    match token {
        0x1234 => Some("example_token"),
        _ => None,
    }
}

pub struct TokenReader<R, F> {
    reader: R,
    format: F,
    state: ParserState,
}

impl<R, F> TokenReader<R, F>
where
    R: std::io::Read,
    F: BinaryFormat,
{
    #[cold]
    fn fill(&mut self) -> Result<usize, Error> {
        let mut spare = self.state.buf.spare_capacity_mut();
        let amt = self.reader.read(&mut spare).unwrap();
        unsafe {
            spare.set_len(amt as u16);
        }
        Ok(amt)
    }

    pub fn buffered_data(&self) -> &[u8] {
        self.state.buf.as_slice()
    }

    pub fn into_remainder(self) -> (R, ParserBuf) {
        (self.reader, self.state.buf)
    }

    #[cold]
    fn next_kind_refill(&mut self) -> Result<Option<TokenKind>, Error> {
        let amt = self.fill()?;
        if amt == 0 {
            return Ok(None);
        }

        let id = self
            .state
            .buf
            .read_bytes::<2>()
            .copied()
            .map(u16::from_le_bytes)
            .map(LexemeId)
            .ok_or_else(|| todo!())?;

        match self.format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::Eof => todo!(),
        }
    }

    #[inline]
    pub fn next_kind(&mut self) -> Result<Option<TokenKind>, Error> {
        let Some(id) = self
            .state
            .buf
            .peek_bytes::<2>()
            .copied()
            .map(u16::from_le_bytes)
            .map(LexemeId)
        else {
            return self.next_kind_refill();
        };

        self.state.buf.end += 2;
        match self.format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::Eof => self.next_kind_refill(),
        }
    }

    /// Return the u64 data associated with [`TokenKind::U64`].
    #[inline]
    pub fn u64_data(&self) -> u64 {
        u64::from_le_bytes(self.state.storage)
    }

    /// Return the i64 data associated with [`TokenKind::I64`].
    #[inline]
    pub fn i64_data(&self) -> i64 {
        i64::from_le_bytes(self.state.storage)
    }

    /// Return the f64 data associated with [`TokenKind::F64`].
    #[inline]
    pub fn f64_data(&self) -> f64 {
        f64::from_bits(self.u64_data())
    }

    /// Return the u32 data associated with [`TokenKind::U32`].
    #[inline]
    pub fn u32_data(&self) -> u32 {
        u32::from_le_bytes([
            self.state.storage[0],
            self.state.storage[1],
            self.state.storage[2],
            self.state.storage[3],
        ])
    }

    /// Return the i32 data associated with [`TokenKind::I32`].
    #[inline]
    pub fn i32_data(&self) -> i32 {
        i32::from_le_bytes([
            self.state.storage[0],
            self.state.storage[1],
            self.state.storage[2],
            self.state.storage[3],
        ])
    }

    /// Return the f32 data associated with [`TokenKind::F32`].
    #[inline]
    pub fn f32_data(&self) -> f32 {
        f32::from_bits(self.u32_data())
    }

    /// Return the bool data associated with [`TokenKind::Bool`].
    #[inline]
    pub fn bool_data(&self) -> bool {
        self.state.storage[0] != 0
    }

    /// Return the 32-bit data associated with [`TokenKind::Lookup`].
    #[inline]
    pub fn lookup_data(&self) -> u32 {
        u32::from_le_bytes([
            self.state.storage[0],
            self.state.storage[1],
            self.state.storage[2],
            0,
        ])
    }

    #[inline]
    pub fn read_buffer(&mut self) -> Result<&[u8], Error> {
        let len = u16::from_le_bytes([self.state.storage[0], self.state.storage[1]]);

        match self.state.buf.read_slice(len) {
            Some(data) => return Ok(data),
            None => {
                let mut spare = self.state.buf.spare_capacity_mut();
                let amt = self.reader.read(&mut spare).unwrap();
                unsafe {
                    spare.set_len(amt as u16);
                }
                self.state.buf.read_slice(len).ok_or_else(|| todo!())
            }
        }
    }
}
