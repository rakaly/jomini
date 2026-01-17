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
    fn read_bytes<const N: usize>(&mut self) -> Option<&'_ [u8; N]> {
        let result = unsafe { self.buf.get_unchecked(self.range()) }.first_chunk::<N>()?;
        self.end += N as u16;
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

struct ParserState<'a> {
    buf: &'a mut ParserBuf,
    storage: [u8; 8],
    storage_request: Option<u16>,
}

impl<'a> ParserState<'a> {
    #[inline]
    fn read_bytes<const N: usize>(&'a mut self) -> Option<&'a [u8; N]> {
        self.buf.read_bytes::<N>()
    }

    #[inline]
    fn store<const N: usize>(&mut self, data: [u8; N]) {
        self.storage[..N].copy_from_slice(&data);
    }

    #[inline]
    fn buffer(&mut self, len: u16) {
        self.storage_request = Some(len);
    }
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

trait BinaryFormat {
    fn visit<'a>(
        &mut self,
        reader: &'a mut ParserState<'a>,
        id: LexemeId,
    ) -> Result<TokenSignal, Error>;
    fn skip<'a>(&mut self, reader: &'a mut ParserState<'a>, id: LexemeId) -> Option<SkipKind> {
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
    fn visit<'a>(&mut self, reader: &'a mut ParserState<'a>, id: LexemeId) -> Result<TokenSignal, Error> {
        match id {
            LexemeId::F32 => {
                let Some(data) = reader.read_bytes::<4>() else {
                    return Ok(TokenSignal::Eof);
                };

                let result = eu4_specific_f32_decoding(data).to_le_bytes();
                reader.store(result);
                Ok(TokenSignal::Kind(TokenKind::F32))
            }
            // LexemeId::QUOTED => {
            //     let Some(len) = reader.read_bytes::<2>().copied().map(u16::from_le_bytes) else {
            //         return Ok(TokenSignal::Eof);
            //     };

            //     reader.store(len.to_le_bytes());
            //     reader.buffer(len);
            //     Ok(TokenSignal::Kind(TokenKind::Quoted))
            // }
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

fn eu4_specific_token_resolution(token: u16) -> Option<&'static str> {
    match token {
        0x1234 => Some("example_token"),
        _ => None,
    }
}

pub struct TokenReader<R, F> {
    reader: R,
    format: F,
    buf: ParserBuf,
    data: [u8; 8],
}

impl<R, F> TokenReader<R, F>
where
    R: std::io::Read,
    F: BinaryFormat,
{
    fn fill(&mut self) -> Result<usize, Error> {
        let mut spare = self.buf.spare_capacity_mut();
        let amt = self.reader.read(&mut spare).unwrap();
        unsafe {
            spare.set_len(amt as u16);
        }
        Ok(amt)
    }

    pub fn buffered_data(&self) -> &[u8] {
        self.buf.as_slice()
    }

    pub fn into_remainder(self) -> (R, ParserBuf) {
        (self.reader, self.buf)
    }

    #[cold]
    fn next_kind_refill(&mut self) -> Result<Option<TokenKind>, Error> {
        let amt = self.fill()?;
        if amt == 0 {
            return Ok(None);
        }

        todo!()
        // self.next_kind()
    }

    pub fn next_kind(&mut self) -> Result<Option<TokenKind>, Error> {
        let Some(id) = self
            .buf
            .read_bytes::<2>()
            .copied()
            .map(u16::from_le_bytes)
            .map(LexemeId)
        else {
            return self.next_kind_refill();
        };

        // match self.format.visit()

        todo!()
    }
}
