#![allow(dead_code)]

use crate::{
    DeserializeError, DeserializeErrorKind, Encoding, Error,
    binary::{FailedResolveStrategy, LexError, LexemeId, Rgb, TokenKind, lexer::read_rgb},
    de::ColorSequence,
};
use serde::{
    Deserialize,
    de::{self, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor},
};
use std::{borrow::Cow, io::Read};

pub struct ParserBuf {
    buf: Box<[u8]>,
    start: *const u8,
    len: usize,
    total_read: usize,
}

impl ParserBuf {
    pub fn from_slice(data: &[u8]) -> Self {
        Self {
            buf: Box::new([]),
            start: data.as_ptr(),
            len: data.len(),
            total_read: 0,
        }
    }

    pub fn new(size: usize) -> Self {
        let buf = vec![0u8; size].into_boxed_slice();
        let start = buf.as_ptr();
        Self {
            buf,
            start,
            len: 0,
            total_read: 0,
        }
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.start, self.len) }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    fn fill(&mut self, mut reader: impl Read) -> Result<usize, std::io::Error> {
        let dst = if self.buf.is_empty() || self.len == 0 {
            &mut self.buf
        } else {
            unsafe {
                self.start.copy_to(self.buf.as_mut_ptr(), self.len);
            }
            self.start = self.buf.as_ptr();
            unsafe { self.buf.get_unchecked_mut(self.len..) }
        };

        let amt = reader.read(dst)?;
        self.len += amt;
        Ok(amt)
    }

    #[inline]
    fn peek_bytes<const N: usize>(&self) -> Option<&'_ [u8; N]> {
        self.as_slice().first_chunk::<N>()
    }

    #[inline]
    fn read_bytes<const N: usize>(&mut self) -> Option<&'_ [u8; N]> {
        let result =
            unsafe { std::slice::from_raw_parts(self.start, self.len) }.first_chunk::<N>()?;
        self.start = unsafe { self.start.add(N) };
        self.len -= N;
        self.total_read += N;
        Some(result)
    }

    #[inline]
    fn read_slice(&mut self, len: u16) -> Option<&'_ [u8]> {
        let result =
            unsafe { std::slice::from_raw_parts(self.start, self.len) }.get(..len as usize)?;
        self.start = unsafe { self.start.add(len as usize) };
        self.len -= len as usize;
        self.total_read += len as usize;
        Some(result)
    }
}

pub struct ParserState {
    buf: ParserBuf,
    storage: [u8; 8],
}

impl ParserState {
    #[inline]
    pub fn read_bytes<const N: usize>(&mut self) -> Option<&[u8; N]> {
        self.buf.read_bytes::<N>()
    }

    #[inline]
    pub fn read_slice(&mut self, len: u16) -> Option<&[u8]> {
        self.buf.read_slice(len)
    }

    #[inline]
    pub fn store<const N: usize>(&mut self, data: [u8; N]) {
        self.storage[..N].copy_from_slice(&data);
    }

    #[inline]
    pub fn peek_bytes<const N: usize>(&self) -> Option<&[u8; N]> {
        self.buf.peek_bytes::<N>()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.buf.is_empty()
    }

    /// Advances the buffer by `amt` bytes.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `amt` does not exceed the current buffer length,
    /// otherwise this will result in undefined behavior by reading beyond the buffer bounds.
    #[inline]
    pub unsafe fn consume(&mut self, amt: usize) {
        self.buf.start = unsafe { self.buf.start.add(amt) };
        self.buf.len -= amt;
        self.buf.total_read += amt;
    }
}

pub enum TokenSignal {
    Kind(TokenKind),
    MoreData,
}

pub enum SkipKind {
    Open,
    Close,
    Other,
}

pub trait PdxVisitor<'de>: Sized {
    type Value;
    fn visit_i32(self, v: i32) -> Result<Self::Value, Error>;
    fn visit_u32(self, v: u32) -> Result<Self::Value, Error>;
    fn visit_i64(self, v: i64) -> Result<Self::Value, Error>;
    fn visit_u64(self, v: u64) -> Result<Self::Value, Error>;
    fn visit_f32(self, v: f32) -> Result<Self::Value, Error>;
    fn visit_f64(self, v: f64) -> Result<Self::Value, Error>;
    fn visit_bool(self, v: bool) -> Result<Self::Value, Error>;
    fn visit_str(self, v: &str) -> Result<Self::Value, Error>;
    fn visit_string(self, v: String) -> Result<Self::Value, Error>;
    fn visit_borrowed_str(self, v: &'de str) -> Result<Self::Value, Error>;
    fn visit_bytes(self, v: &[u8]) -> Result<Self::Value, Error>;
    fn visit_unit(self) -> Result<Self::Value, Error>;
    fn visit_rgb(self, rgb: Rgb) -> Result<Self::Value, Error>;
}

pub enum ParseResult<T, V> {
    Value(T),
    Open(V),
}

struct PdxSerdeVisitor<V>(V);

impl<V> PdxSerdeVisitor<V> {
    fn into_inner(self) -> V {
        self.0
    }
}

impl<'de, V: Visitor<'de>> PdxVisitor<'de> for PdxSerdeVisitor<V> {
    type Value = V::Value;

    #[inline]
    fn visit_i32(self, v: i32) -> Result<Self::Value, Error> {
        self.0.visit_i32(v)
    }
    #[inline]
    fn visit_u32(self, v: u32) -> Result<Self::Value, Error> {
        self.0.visit_u32(v)
    }
    #[inline]
    fn visit_i64(self, v: i64) -> Result<Self::Value, Error> {
        self.0.visit_i64(v)
    }
    #[inline]
    fn visit_u64(self, v: u64) -> Result<Self::Value, Error> {
        self.0.visit_u64(v)
    }
    #[inline]
    fn visit_f32(self, v: f32) -> Result<Self::Value, Error> {
        self.0.visit_f32(v)
    }
    #[inline]
    fn visit_f64(self, v: f64) -> Result<Self::Value, Error> {
        self.0.visit_f64(v)
    }
    #[inline]
    fn visit_bool(self, v: bool) -> Result<Self::Value, Error> {
        self.0.visit_bool(v)
    }
    #[inline]
    fn visit_str(self, v: &str) -> Result<Self::Value, Error> {
        self.0.visit_str(v)
    }
    #[inline]
    fn visit_string(self, v: String) -> Result<Self::Value, Error> {
        self.0.visit_string(v)
    }
    #[inline]
    fn visit_borrowed_str(self, v: &'de str) -> Result<Self::Value, Error> {
        self.0.visit_borrowed_str(v)
    }
    #[inline]
    fn visit_bytes(self, v: &[u8]) -> Result<Self::Value, Error> {
        self.0.visit_bytes(v)
    }
    #[inline]
    fn visit_unit(self) -> Result<Self::Value, Error> {
        self.0.visit_unit()
    }
    #[inline]
    fn visit_rgb(self, rgb: Rgb) -> Result<Self::Value, Error> {
        self.0.visit_seq(ColorSequence::new(rgb))
    }
}

pub trait BinaryFormat {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error>;
    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        match self.visit(reader, id) {
            Ok(TokenSignal::Kind(TokenKind::Open)) => Some(SkipKind::Open),
            Ok(TokenSignal::Kind(TokenKind::Close)) => Some(SkipKind::Close),
            Ok(TokenSignal::Kind(_)) => Some(SkipKind::Other),
            Ok(TokenSignal::MoreData) | Err(_) => None,
        }
    }

    fn deserialize_i32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_u32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_i64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_u64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_f32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_f64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_bool<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        self.deserialize_value_any(reader, visitor, config)
    }

    fn deserialize_value_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error>;
}

macro_rules! forward_deserialize {
    ($method:ident) => {
        #[inline]
        fn $method<'de, V: PdxVisitor<'de>>(
            &mut self,
            reader: &mut ParserState,
            visitor: V,
            config: &BinaryConfig,
        ) -> Result<ParseResult<V::Value, V>, Error> {
            (**self).$method(reader, visitor, config)
        }
    };
}

impl<F: BinaryFormat> BinaryFormat for &'_ mut F {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error> {
        (**self).visit(reader, id)
    }

    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        (**self).skip(reader, id)
    }

    forward_deserialize!(deserialize_i32);
    forward_deserialize!(deserialize_u32);
    forward_deserialize!(deserialize_i64);
    forward_deserialize!(deserialize_u64);
    forward_deserialize!(deserialize_f32);
    forward_deserialize!(deserialize_f64);
    forward_deserialize!(deserialize_bool);
    forward_deserialize!(deserialize_str);
    forward_deserialize!(deserialize_any);
    forward_deserialize!(deserialize_value_any);
}

pub trait TokenResolver2<'str> {
    fn resolve_field(&self, _token: FieldId) -> Option<&'str str> {
        None
    }

    fn resolve_lookup(&self, _index: LookupIndex) -> Option<&'str str> {
        None
    }
}

pub trait BinaryFlavor2<'str>: BinaryFormat + TokenResolver2<'str> + Encoding {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FieldId(u16);

impl FieldId {
    #[inline]
    pub const fn new(index: u16) -> Self {
        FieldId(index)
    }

    #[inline]
    pub const fn value(self) -> u16 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LookupIndex(u32);

impl LookupIndex {
    pub const fn new(index: u32) -> Self {
        LookupIndex(index)
    }

    pub const fn value(self) -> u32 {
        self.0
    }
}

struct StandardFormat;

impl BinaryFormat for StandardFormat {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error> {
        match id {
            LexemeId::OPEN => Ok(TokenSignal::Kind(TokenKind::Open)),
            LexemeId::CLOSE => Ok(TokenSignal::Kind(TokenKind::Close)),
            LexemeId::EQUAL => Ok(TokenSignal::Kind(TokenKind::Equal)),
            LexemeId::U32 | LexemeId::I32 => {
                let Some(data) = reader.read_bytes::<4>().copied() else {
                    return Ok(TokenSignal::MoreData);
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
                    return Ok(TokenSignal::MoreData);
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
                    return Ok(TokenSignal::MoreData);
                };

                let result = f32::from_le_bytes(data).to_bits();
                reader.store(result.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::F32))
            }
            LexemeId::F64 => {
                let Some(data) = reader.read_bytes::<8>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };

                let result = f64::from_le_bytes(data).to_bits();
                reader.store(result.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::F64))
            }
            LexemeId::BOOL => {
                let Some(data) = reader.read_bytes::<1>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };

                reader.store(data);
                Ok(TokenSignal::Kind(TokenKind::Bool))
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let Some(len) = reader.peek_bytes::<2>().copied().map(u16::from_le_bytes) else {
                    return Ok(TokenSignal::MoreData);
                };

                if reader.len() < (len as usize) + 2 {
                    return Ok(TokenSignal::MoreData);
                }

                unsafe {
                    reader.consume((len + 2) as usize);
                }
                reader.store(len.to_le_bytes());
                if id == LexemeId::QUOTED {
                    Ok(TokenSignal::Kind(TokenKind::Quoted))
                } else {
                    Ok(TokenSignal::Kind(TokenKind::Unquoted))
                }
            }
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                let Some(data) = reader.read_bytes::<1>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };
                let mut tmp = [0u8; 8];
                tmp[0] = data[0];
                reader.store(tmp);
                Ok(TokenSignal::Kind(TokenKind::Lookup))
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                let Some(data) = reader.read_bytes::<2>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };
                let mut tmp = [0u8; 8];
                tmp[0..2].copy_from_slice(&data);
                reader.store(tmp);
                Ok(TokenSignal::Kind(TokenKind::Lookup))
            }
            LexemeId::LOOKUP_U24 => {
                let Some(data) = reader.read_bytes::<3>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };
                let mut tmp = [0u8; 8];
                tmp[0..3].copy_from_slice(&data);
                reader.store(tmp);
                Ok(TokenSignal::Kind(TokenKind::Lookup))
            }
            LexemeId::RGB => {
                // RGB has complex nested structure: { OPEN U32 U32 U32 [U32] CLOSE }
                // We need to parse it to calculate total size for later rgb_data() reconstruction

                // Peek ahead to ensure we have enough data
                // Minimum RGB: 2 (OPEN) + 2 (U32) + 4 + 2 (U32) + 4 + 2 (U32) + 4 + 2 (CLOSE) = 22 bytes
                // Maximum RGB: 22 + 2 (U32) + 4 = 28 bytes

                if reader.len() < 28 {
                    // Need to try to parse what we have
                    match read_rgb(reader.buf.as_slice()) {
                        Ok((_, rest)) => {
                            let consumed = reader.buf.as_slice().len() - rest.len();
                            unsafe {
                                reader.consume(consumed);
                            }
                            reader.store([consumed as u8, 0, 0, 0, 0, 0, 0, 0]);
                            Ok(TokenSignal::Kind(TokenKind::Rgb))
                        }
                        Err(_) => Ok(TokenSignal::MoreData),
                    }
                } else {
                    // Fast path: we have enough data
                    let (_, rest) = read_rgb(reader.buf.as_slice())
                        .map_err(|e| Error::from(e.at(reader.buf.total_read)))?;
                    let consumed = reader.buf.as_slice().len() - rest.len();
                    unsafe {
                        reader.consume(consumed);
                    }
                    reader.store([consumed as u8, 0, 0, 0, 0, 0, 0, 0]);
                    Ok(TokenSignal::Kind(TokenKind::Rgb))
                }
            }
            id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                let is_negative = offset > 7;
                let byte_count = offset - (is_negative as u16 * 7);

                if byte_count == 0 {
                    // FIXED5_ZERO: no data to read
                    reader.store([0u8; 8]);
                    return Ok(TokenSignal::Kind(TokenKind::F64));
                }

                let Some(data) = reader.read_slice(byte_count) else {
                    return Ok(TokenSignal::MoreData);
                };

                let mut buf = [0u8; 8];
                buf[..byte_count as usize].copy_from_slice(data);
                let sign = 1i64 - (is_negative as i64) * 2;
                let i64_bytes = (u64::from_le_bytes(buf) as i64 * sign).to_le_bytes();
                reader.store(i64_bytes);
                Ok(TokenSignal::Kind(TokenKind::F64))
            }
            id => {
                reader.store(id.0.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::Id))
            }
        }
    }

    #[inline]
    fn deserialize_i32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<6>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::I32 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = i32::from_le_bytes([data[2], data[3], data[4], data[5]]);
        unsafe { reader.consume(6) };
        Ok(ParseResult::Value(visitor.visit_i32(result)?))
    }

    #[inline]
    fn deserialize_u32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<6>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::U32 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = u32::from_le_bytes([data[2], data[3], data[4], data[5]]);
        unsafe { reader.consume(6) };
        Ok(ParseResult::Value(visitor.visit_u32(result)?))
    }

    #[inline]
    fn deserialize_i64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<10>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::I64 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = i64::from_le_bytes([data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9]]);
        unsafe { reader.consume(10) };
        Ok(ParseResult::Value(visitor.visit_i64(result)?))
    }

    #[inline]
    fn deserialize_u64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<10>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::U64 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = u64::from_le_bytes([data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9]]);
        unsafe { reader.consume(10) };
        Ok(ParseResult::Value(visitor.visit_u64(result)?))
    }

    #[inline]
    fn deserialize_f32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<6>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::F32 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = f32::from_le_bytes([data[2], data[3], data[4], data[5]]);
        unsafe { reader.consume(6) };
        Ok(ParseResult::Value(visitor.visit_f32(result)?))
    }

    #[inline]
    fn deserialize_f64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<10>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::F64 {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = f64::from_le_bytes([data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9]]);
        unsafe { reader.consume(10) };
        Ok(ParseResult::Value(visitor.visit_f64(result)?))
    }

    #[inline]
    fn deserialize_bool<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(data) = reader.peek_bytes::<3>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
        if id != LexemeId::BOOL {
            return self.deserialize_value_any(reader, visitor, config);
        }
        let result = data[2] != 0;
        unsafe { reader.consume(3) };
        Ok(ParseResult::Value(visitor.visit_bool(result)?))
    }

    #[inline]
    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(header) = reader.peek_bytes::<4>() else {
            return self.deserialize_value_any(reader, visitor, config);
        };
        let id = LexemeId::new(u16::from_le_bytes([header[0], header[1]]));
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let len = u16::from_le_bytes([header[2], header[3]]);
                if reader.len() < 4 + len as usize {
                    return self.deserialize_value_any(reader, visitor, config);
                }
                unsafe { reader.consume(4) };
                let data = reader.read_slice(len).ok_or_else(Error::eof)?;
                Ok(ParseResult::Value(visitor.visit_bytes(data)?))
            }
            _ => self.deserialize_value_any(reader, visitor, config),
        }
    }

    fn deserialize_value_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let Some(id_bytes) = reader.read_bytes::<2>().copied() else {
            return Err(Error::eof());
        };
        let id = LexemeId::new(u16::from_le_bytes(id_bytes));
        standard_deserialize_value_any(reader, id, visitor, config)
    }
}

fn standard_deserialize_value_any<'de, V: PdxVisitor<'de>>(
    reader: &mut ParserState,
    id: LexemeId,
    visitor: V,
    _config: &BinaryConfig,
) -> Result<ParseResult<V::Value, V>, Error> {
    match id {
        LexemeId::OPEN => Ok(ParseResult::Open(visitor)),
        LexemeId::I32 => {
            let data = reader.read_bytes::<4>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_i32(i32::from_le_bytes(data))?))
        }
        LexemeId::U32 => {
            let data = reader.read_bytes::<4>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_u32(u32::from_le_bytes(data))?))
        }
        LexemeId::I64 => {
            let data = reader.read_bytes::<8>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_i64(i64::from_le_bytes(data))?))
        }
        LexemeId::U64 => {
            let data = reader.read_bytes::<8>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_u64(u64::from_le_bytes(data))?))
        }
        LexemeId::F32 => {
            let data = reader.read_bytes::<4>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_f32(f32::from_le_bytes(data))?))
        }
        LexemeId::F64 => {
            let data = reader.read_bytes::<8>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_f64(f64::from_le_bytes(data))?))
        }
        LexemeId::BOOL => {
            let data = reader.read_bytes::<1>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_bool(data[0] != 0)?))
        }
        LexemeId::QUOTED | LexemeId::UNQUOTED => {
            let len_bytes = reader.peek_bytes::<2>().copied().ok_or_else(Error::eof)?;
            let len = u16::from_le_bytes(len_bytes);
            if reader.len() < (len as usize) + 2 {
                return Err(Error::eof());
            }
            unsafe { reader.consume((len + 2) as usize) };
            // Store len for read_buffer compatibility
            reader.store(len.to_le_bytes());
            // Read the string data from just before current position
            let data = unsafe {
                std::slice::from_raw_parts(reader.buf.start.byte_sub(len as usize), len as usize)
            };
            Ok(ParseResult::Value(visitor.visit_bytes(data)?))
        }
        LexemeId::RGB => {
            let slice = reader.buf.as_slice();
            let (rgb, rest) = read_rgb(slice)
                .map_err(|e| Error::from(e.at(reader.buf.total_read)))?;
            let consumed = slice.len() - rest.len();
            unsafe { reader.consume(consumed) };
            Ok(ParseResult::Value(visitor.visit_rgb(rgb)?))
        }
        LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
            let data = reader.read_bytes::<1>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_u32(data[0] as u32)?))
        }
        LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
            let data = reader.read_bytes::<2>().copied().ok_or_else(Error::eof)?;
            Ok(ParseResult::Value(visitor.visit_u32(u16::from_le_bytes(data) as u32)?))
        }
        LexemeId::LOOKUP_U24 => {
            let data = reader.read_bytes::<3>().copied().ok_or_else(Error::eof)?;
            let val = data[0] as u32 | (data[1] as u32) << 8 | (data[2] as u32) << 16;
            Ok(ParseResult::Value(visitor.visit_u32(val)?))
        }
        id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
            let offset = id.0 - LexemeId::FIXED5_ZERO.0;
            let is_negative = offset > 7;
            let byte_count = offset - (is_negative as u16 * 7);

            if byte_count == 0 {
                // FIXED5_ZERO: raw i64 value is 0, reported as f64
                return Ok(ParseResult::Value(visitor.visit_f64(f64::from_bits(0))?));
            }

            let data = reader.read_slice(byte_count).ok_or_else(Error::eof)?;
            let mut buf = [0u8; 8];
            buf[..byte_count as usize].copy_from_slice(data);
            let sign = 1i64 - (is_negative as i64) * 2;
            let i64_val = u64::from_le_bytes(buf) as i64 * sign;
            // Store as raw i64 bytes and report as f64 (matches old behavior)
            Ok(ParseResult::Value(visitor.visit_f64(f64::from_bits(u64::from_le_bytes(i64_val.to_le_bytes())))?))
        }
        LexemeId::CLOSE => Err(Error::invalid_syntax("did not expect end", reader.buf.total_read)),
        LexemeId::EQUAL => Err(Error::invalid_syntax("did not expect equal", reader.buf.total_read)),
        id => {
            // Field ID
            Ok(ParseResult::Value(visitor.visit_u32(id.0 as u32)?))
        }
    }
}

pub struct TokenReader<R> {
    reader: R,
    state: ParserState,
}

impl TokenReader<()> {
    pub fn from_slice(data: &[u8]) -> TokenReader<&[u8]> {
        TokenReader {
            reader: data,
            state: ParserState {
                buf: ParserBuf::from_slice(data),
                storage: [0u8; 8],
            },
        }
    }
}

impl<R> TokenReader<R>
where
    R: std::io::Read,
{
    pub fn buffered_data(&self) -> &[u8] {
        self.state.buf.as_slice()
    }

    pub fn into_remainder(self) -> (R, ParserBuf) {
        (self.reader, self.state.buf)
    }

    pub fn position(&self) -> usize {
        self.state.buf.total_read
    }

    #[cold]
    fn fill(&mut self) -> Result<usize, Error> {
        let amt = self.state.buf.fill(&mut self.reader)?;
        Ok(amt)
    }

    #[cold]
    fn next_kind_refill(
        &mut self,
        mut format: impl BinaryFormat,
    ) -> Result<Option<TokenKind>, Error> {
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
            .ok_or_else(Error::eof)?;

        match format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::MoreData => Err(Error::eof()),
        }
    }

    #[inline]
    pub fn next_kind(&mut self, mut format: impl BinaryFormat) -> Result<Option<TokenKind>, Error> {
        let Some(id) = self
            .state
            .buf
            .peek_bytes::<2>()
            .copied()
            .map(u16::from_le_bytes)
            .map(LexemeId)
        else {
            return self.next_kind_refill(format);
        };

        unsafe { self.state.consume(2) };
        match format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::MoreData => self.next_kind_refill(format),
        }
    }

    #[inline]
    pub fn read_kind(&mut self, format: impl BinaryFormat) -> Result<TokenKind, Error> {
        match self.next_kind(format)? {
            Some(kind) => Ok(kind),
            None => Err(LexError::Eof.at(self.position()).into()),
        }
    }

    /// Peeks at the next LexemeId without consuming it.
    /// Returns `Ok(None)` at EOF, `Err` on IO error during refill.
    #[inline]
    pub fn peek_lexeme_id(&mut self) -> Result<Option<LexemeId>, Error> {
        if let Some(bytes) = self.state.buf.peek_bytes::<2>() {
            Ok(Some(LexemeId::new(u16::from_le_bytes([bytes[0], bytes[1]]))))
        } else {
            self.peek_lexeme_id_refill()
        }
    }

    #[cold]
    fn peek_lexeme_id_refill(&mut self) -> Result<Option<LexemeId>, Error> {
        let amt = self.fill()?;
        if amt == 0 {
            return Ok(None);
        }
        match self.state.buf.peek_bytes::<2>() {
            Some(bytes) => Ok(Some(LexemeId::new(u16::from_le_bytes([bytes[0], bytes[1]])))),
            None => Err(Error::eof()),
        }
    }

    /// Consumes the 2-byte LexemeId that was previously peeked.
    ///
    /// # Safety
    ///
    /// The caller must ensure that at least 2 bytes are available (i.e., a successful
    /// `peek_lexeme_id` was called and no data was consumed since).
    #[inline]
    unsafe fn consume_lexeme_id(&mut self) {
        unsafe { self.state.consume(2) };
    }

    /// Deserialize a value using the format's speculative path.
    /// The LexemeId has NOT been consumed yet.
    #[inline]
    fn deserialize_format<'de, V: PdxVisitor<'de>>(
        &mut self,
        format: &mut impl BinaryFormat,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        format.deserialize_any(&mut self.state, visitor, config)
    }

    /// Deserialize a value using the format's speculative path, with refill on MoreData.
    #[cold]
    fn deserialize_format_refill<'de, V: PdxVisitor<'de>>(
        &mut self,
        format: &mut impl BinaryFormat,
        visitor: V,
        config: &BinaryConfig,
    ) -> Result<ParseResult<V::Value, V>, Error> {
        let amt = self.fill()?;
        if amt == 0 {
            return Err(Error::eof());
        }
        format.deserialize_any(&mut self.state, visitor, config)
    }

    #[inline]
    pub fn skip_kind(
        &mut self,
        kind: TokenKind,
        mut format: impl BinaryFormat,
    ) -> Result<(), Error> {
        match kind {
            TokenKind::Open => {
                let mut depth = 1;
                loop {
                    match self.read_kind(&mut format)? {
                        TokenKind::Open => depth += 1,
                        TokenKind::Close => {
                            depth -= 1;
                            if depth == 0 {
                                return Ok(());
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => Ok(()),
        }
    }

    /// Reads the buffered string data.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the length stored in the first two bytes of `state.storage`
    /// is valid and that the buffer pointer has been advanced past this data.
    #[inline]
    pub unsafe fn read_buffer(&mut self) -> &[u8] {
        let len = u16::from_le_bytes([self.state.storage[0], self.state.storage[1]]);
        unsafe {
            std::slice::from_raw_parts(self.state.buf.start.byte_sub(len as usize), len as usize)
        }

        // let len = u16::from_le_bytes([self.state.storage[0], self.state.storage[1]]);

        // // Check if we have enough data without borrowing
        // let available = self.state.buf.len;
        // if available >= (len as usize) {
        //     return Ok(unsafe { self.state.buf.read_slice(len).unwrap_unchecked() });
        // }

        // self.fill()?;
        // self.state.buf.read_slice(len).ok_or_else(Error::eof)
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
    pub fn lookup_id(&self) -> LookupIndex {
        LookupIndex::new(u32::from_le_bytes([
            self.state.storage[0],
            self.state.storage[1],
            self.state.storage[2],
            0,
        ]))
    }

    /// Return the field value associated with [`TokenKind::Id`].
    #[inline]
    pub fn field_id(&self) -> FieldId {
        FieldId::new(u16::from_le_bytes([
            self.state.storage[0],
            self.state.storage[1],
        ]))
    }

    /// Return the RGB data associated with [`TokenKind::Rgb`].
    #[inline]
    pub fn rgb_data(&self) -> Rgb {
        let size = self.state.storage[0] as usize;
        let data = unsafe { std::slice::from_raw_parts(self.state.buf.start.byte_sub(size), size) };
        let (result, _) = read_rgb(data).expect("valid rgb data");
        result
    }
}

pub struct BinaryConfig {
    failed_resolve_strategy: FailedResolveStrategy,
}

pub struct BinaryReaderDeserializer<R, F> {
    reader: TokenReader<R>,
    format: F,
    config: BinaryConfig,
}

impl<'a, R, F> BinaryReaderDeserializer<R, F>
where
    R: Read,
    F: BinaryFlavor2<'a>,
{
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

fn deserialize<T>(
    reader: TokenReader<impl Read>,
    format: impl for<'a> BinaryFlavor2<'a>,
) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    let mut reader = BinaryReaderDeserializer {
        reader,
        format,
        config: BinaryConfig {
            failed_resolve_strategy: FailedResolveStrategy::Error,
        },
    };
    let value: T = serde::de::Deserialize::deserialize(&mut reader).unwrap();

    Ok(value)
}

fn deserialize2<'a, T, F>(reader: TokenReader<impl Read>, format: F) -> Result<T, Error>
where
    T: Deserialize<'a>,
    F: BinaryFlavor2<'a>,
{
    let mut reader = BinaryReaderDeserializer {
        reader,
        format,
        config: BinaryConfig {
            failed_resolve_strategy: FailedResolveStrategy::Error,
        },
    };

    let value: T = serde::de::Deserialize::deserialize(&mut reader).unwrap();

    Ok(value)
}

impl<'de, 'a: 'de, F, R> de::Deserializer<'de> for &'_ mut BinaryReaderDeserializer<R, F>
where
    F: BinaryFlavor2<'a>,
    R: Read,
{
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "root deserializer can only work with key value pairs",
            )),
        }))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(BinaryReaderMap::new::<true>(self))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

struct BinaryReaderMap<'a, const ROOT: bool, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
}

impl<'a, R, F> BinaryReaderMap<'a, false, R, F> {
    fn new<const ROOT: bool>(
        de: &'a mut BinaryReaderDeserializer<R, F>,
    ) -> BinaryReaderMap<'a, ROOT, R, F> {
        BinaryReaderMap::<'a, ROOT, R, F> { de }
    }
}

impl<'de, 'a: 'de, const ROOT: bool, R, F> MapAccess<'de> for BinaryReaderMap<'_, ROOT, R, F>
where
    R: Read,
    F: BinaryFlavor2<'a>,
{
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match self.de.reader.peek_lexeme_id()? {
                None if ROOT => return Ok(None),
                None => return Err(LexError::Eof.at(self.de.reader.position()).into()),
                Some(LexemeId::CLOSE) => {
                    unsafe { self.de.reader.consume_lexeme_id() };
                    return Ok(None);
                }
                Some(LexemeId::OPEN) => {
                    // Ghost object: consume open, skip the key inside
                    unsafe { self.de.reader.consume_lexeme_id() };
                    let _ = self.de.reader.read_kind(&mut self.de.format)?;
                }
                Some(_) => {
                    // Don't consume - BinaryReaderTokenDeserializer will read it
                    return seed
                        .deserialize(BinaryReaderTokenDeserializer { de: self.de })
                        .map(Some);
                }
            }
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        // Peek and skip Equal if present
        if let Some(LexemeId::EQUAL) = self.de.reader.peek_lexeme_id()? {
            unsafe { self.de.reader.consume_lexeme_id() };
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de })
    }
}

struct BinaryReaderTokenDeserializer<'a, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
}

impl<'b, R, F> BinaryReaderTokenDeserializer<'b, R, F>
where
    R: Read,
{
    #[inline]
    fn handle_parse_result<'de, 'a: 'de, V>(
        de: &'b mut BinaryReaderDeserializer<R, F>,
        result: Result<ParseResult<V::Value, PdxSerdeVisitor<V>>, Error>,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
        F: BinaryFlavor2<'a>,
    {
        match result? {
            ParseResult::Value(v) => Ok(v),
            ParseResult::Open(pdx) => {
                let visitor = pdx.into_inner();
                let mut seq = BinaryReaderSeq::new(de);
                let result = visitor.visit_seq(&mut seq)?;
                if !seq.hit_end {
                    if !matches!(
                        de.reader.read_kind(&mut de.format)?,
                        TokenKind::Close
                    ) {
                        return Err(Error::invalid_syntax(
                            "Expected sequence to be terminated with an end token",
                            de.reader.position(),
                        ));
                    }
                }
                Ok(result)
            }
        }
    }
}

macro_rules! deserialize_speculative {
    ($method:ident) => {
        #[inline]
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>,
        {
            let config = &self.de.config as *const BinaryConfig;
            let result = self.de.format.$method(
                &mut self.de.reader.state,
                PdxSerdeVisitor(visitor),
                unsafe { &*config },
            );
            Self::handle_parse_result(self.de, result)
        }
    };
}

impl<'de, 'a: 'de, R: Read, F> de::Deserializer<'de> for BinaryReaderTokenDeserializer<'_, R, F>
where
    F: BinaryFlavor2<'a>,
{
    type Error = Error;

    #[inline]
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let config = &self.de.config as *const BinaryConfig;
        let result = self.de.format.deserialize_any(
            &mut self.de.reader.state,
            PdxSerdeVisitor(visitor),
            unsafe { &*config },
        );
        Self::handle_parse_result(self.de, result)
    }

    deserialize_speculative!(deserialize_i32);
    deserialize_speculative!(deserialize_u32);
    deserialize_speculative!(deserialize_i64);
    deserialize_speculative!(deserialize_u64);
    deserialize_speculative!(deserialize_f32);
    deserialize_speculative!(deserialize_f64);
    deserialize_speculative!(deserialize_bool);

    #[inline]
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    #[inline]
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let config = &self.de.config as *const BinaryConfig;
        let result = self.de.format.deserialize_str(
            &mut self.de.reader.state,
            PdxSerdeVisitor(visitor),
            unsafe { &*config },
        );
        Self::handle_parse_result(self.de, result)
    }

    #[inline]
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    #[inline]
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    #[inline]
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    #[inline]
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    #[inline]
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let config = &self.de.config as *const BinaryConfig;
        let result = self.de.format.deserialize_any(
            &mut self.de.reader.state,
            PdxSerdeVisitor(visitor),
            unsafe { &*config },
        );
        Self::handle_parse_result(self.de, result)
    }

    #[inline]
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let config = &self.de.config as *const BinaryConfig;
        let result = self.de.format.deserialize_any(
            &mut self.de.reader.state,
            PdxSerdeVisitor(visitor),
            unsafe { &*config },
        );
        match result? {
            ParseResult::Value(v) => Ok(v),
            ParseResult::Open(pdx) => {
                pdx.into_inner().visit_map(BinaryReaderMap::new::<false>(self.de))
            }
        }
    }

    #[inline]
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    #[inline]
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        // For enum, we need the token to be read via the old path since
        // BinaryReaderEnum needs the kind for variant_seed
        let kind = self.de.reader.read_kind(&mut self.de.format)?;
        visitor.visit_enum(BinaryReaderEnum::new(self.de, kind))
    }

    #[inline]
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let kind = self.de.reader.read_kind(&mut self.de.format)?;
        self.de.reader.skip_kind(kind, &mut self.de.format)?;
        visitor.visit_unit()
    }

    #[inline]
    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct BinaryReaderSeq<'a, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
    hit_end: bool,
}

impl<'a, R, F> BinaryReaderSeq<'a, R, F> {
    fn new(de: &'a mut BinaryReaderDeserializer<R, F>) -> Self {
        BinaryReaderSeq { de, hit_end: false }
    }
}

impl<'de, 'a: 'de, R: Read, F> SeqAccess<'de> for BinaryReaderSeq<'_, R, F>
where
    F: BinaryFlavor2<'a>,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.de.reader.peek_lexeme_id()? {
            Some(LexemeId::CLOSE) => {
                unsafe { self.de.reader.consume_lexeme_id() };
                self.hit_end = true;
                return Ok(None);
            }
            Some(LexemeId::EQUAL) => {
                // This is a standalone Equal token from object template syntax
                unsafe { self.de.reader.consume_lexeme_id() };
            }
            Some(_) => {}
            None => return Err(Error::eof()),
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de })
            .map(Some)
    }
}

/// Deserializer for a token that has already been consumed via the old `read_kind` path.
/// Used for enum variants and ignored_any where the token kind is already known.
struct BinaryReaderConsumedTokenDeserializer<'a, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
    kind: TokenKind,
}

impl<'de, 'a: 'de, R: Read, F> de::Deserializer<'de>
    for BinaryReaderConsumedTokenDeserializer<'_, R, F>
where
    F: BinaryFlavor2<'a>,
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.kind {
            TokenKind::U32 => visitor.visit_u32(self.de.reader.u32_data()),
            TokenKind::U64 => visitor.visit_u64(self.de.reader.u64_data()),
            TokenKind::I32 => visitor.visit_i32(self.de.reader.i32_data()),
            TokenKind::I64 => visitor.visit_i64(self.de.reader.i64_data()),
            TokenKind::Bool => visitor.visit_bool(self.de.reader.bool_data()),
            TokenKind::F32 => visitor.visit_f32(self.de.reader.f32_data()),
            TokenKind::F64 => visitor.visit_f64(self.de.reader.f64_data()),
            TokenKind::Quoted | TokenKind::Unquoted => {
                match self.de.format.decode(unsafe { self.de.reader.read_buffer() }) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            TokenKind::Id => match self.de.format.resolve_field(self.de.reader.field_id()) {
                Some(id) => visitor.visit_borrowed_str(id),
                None => match self.de.config.failed_resolve_strategy {
                    FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::UnknownToken {
                            token_id: self.de.reader.field_id().value() as u32,
                        },
                    })),
                    FailedResolveStrategy::Stringify => {
                        visitor.visit_string(format!("0x{:x}", self.de.reader.field_id().value()))
                    }
                    FailedResolveStrategy::Ignore => {
                        visitor.visit_borrowed_str("__internal_identifier_ignore")
                    }
                },
            },
            TokenKind::Open => visitor.visit_seq(BinaryReaderSeq::new(self.de)),
            TokenKind::Rgb => visitor.visit_seq(ColorSequence::new(self.de.reader.rgb_data())),
            TokenKind::Lookup => {
                match self.de.format.resolve_lookup(self.de.reader.lookup_id()) {
                    Some(value) => visitor.visit_borrowed_str(value.as_ref()),
                    None => match self.de.config.failed_resolve_strategy {
                        FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                            kind: DeserializeErrorKind::UnknownToken {
                                token_id: self.de.reader.lookup_id().value(),
                            },
                        })),
                        FailedResolveStrategy::Stringify => {
                            visitor.visit_string(format!("{}", self.de.reader.lookup_id().value()))
                        }
                        FailedResolveStrategy::Ignore => {
                            visitor.visit_borrowed_str("__internal_identifier_ignore")
                        }
                    },
                }
            }
            TokenKind::Close => Err(Error::invalid_syntax(
                "did not expect end",
                self.de.reader.position(),
            )),
            TokenKind::Equal => Err(Error::invalid_syntax(
                "did not expect equal",
                self.de.reader.position(),
            )),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

struct BinaryReaderEnum<'a, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
    kind: TokenKind,
}

impl<'a, F, R> BinaryReaderEnum<'a, R, F> {
    fn new(de: &'a mut BinaryReaderDeserializer<R, F>, kind: TokenKind) -> Self {
        BinaryReaderEnum { de, kind }
    }
}

impl<'de, 'a: 'de, R: Read, F> de::EnumAccess<'de> for BinaryReaderEnum<'_, R, F>
where
    F: BinaryFlavor2<'a>,
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(BinaryReaderConsumedTokenDeserializer {
            de: self.de,
            kind: self.kind,
        })?;
        Ok((variant, self))
    }
}

impl<'de, 'a: 'de, R: Read, F> de::VariantAccess<'de> for BinaryReaderEnum<'_, R, F>
where
    F: BinaryFlavor2<'a>,
{
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from(
                "unsupported enum deserialization. Please file issue",
            )),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ErrorKind, Scalar,
        binary::{Rgb, Token},
    };
    use rstest::*;

    #[test]
    fn test_parser_buf_from_slice() {
        let data = [1u8, 2, 3, 4, 5];
        let buf = ParserBuf::from_slice(&data);

        assert_eq!(buf.as_slice(), &[1, 2, 3, 4, 5]);
        assert_eq!(buf.len, 5);
        assert_eq!(buf.total_read, 0);
    }

    #[test]
    fn test_parser_buf_new() {
        let buf = ParserBuf::new(128);

        assert_eq!(buf.as_slice(), &[] as &[u8]);
        assert_eq!(buf.len, 0);
        assert_eq!(buf.total_read, 0);
        assert_eq!(buf.buf.len(), 128);
    }

    #[test]
    fn test_peek_bytes() {
        let data = [1u8, 2, 3, 4, 5];
        let buf = ParserBuf::from_slice(&data);

        let peeked = buf.peek_bytes::<2>();
        assert_eq!(peeked, Some(&[1u8, 2]));

        // Peek should not consume
        assert_eq!(buf.len, 5);
        assert_eq!(buf.total_read, 0);
    }

    #[test]
    fn test_peek_bytes_insufficient_data() {
        let data = [1u8, 2, 3];
        let buf = ParserBuf::from_slice(&data);

        let peeked = buf.peek_bytes::<5>();
        assert_eq!(peeked, None);
    }

    #[test]
    fn test_read_bytes() {
        let data = [1u8, 2, 3, 4, 5];
        let mut buf = ParserBuf::from_slice(&data);

        let read = buf.read_bytes::<2>();
        assert_eq!(read, Some(&[1u8, 2]));
        assert_eq!(buf.len, 3);
        assert_eq!(buf.total_read, 2);

        let read = buf.read_bytes::<2>();
        assert_eq!(read, Some(&[3u8, 4]));
        assert_eq!(buf.len, 1);
        assert_eq!(buf.total_read, 4);
    }

    #[test]
    fn test_read_bytes_insufficient_data() {
        let data = [1u8, 2, 3];
        let mut buf = ParserBuf::from_slice(&data);

        let read = buf.read_bytes::<5>();
        assert_eq!(read, None);

        // Should not have consumed any data
        assert_eq!(buf.len, 3);
        assert_eq!(buf.total_read, 0);
    }

    #[test]
    fn test_read_slice() {
        let data = [1u8, 2, 3, 4, 5, 6, 7, 8];
        let mut buf = ParserBuf::from_slice(&data);

        let slice = buf.read_slice(3);
        assert_eq!(slice, Some(&[1u8, 2, 3][..]));
        assert_eq!(buf.len, 5);
        assert_eq!(buf.total_read, 3);

        let slice = buf.read_slice(2);
        assert_eq!(slice, Some(&[4u8, 5][..]));
        assert_eq!(buf.len, 3);
        assert_eq!(buf.total_read, 5);
    }

    #[test]
    fn test_read_slice_insufficient_data() {
        let data = [1u8, 2, 3];
        let mut buf = ParserBuf::from_slice(&data);

        let slice = buf.read_slice(5);
        assert_eq!(slice, None);

        // Should not have consumed any data
        assert_eq!(buf.len, 3);
        assert_eq!(buf.total_read, 0);
    }

    #[test]
    fn test_read_slice_exact() {
        let data = [1u8, 2, 3];
        let mut buf = ParserBuf::from_slice(&data);

        let slice = buf.read_slice(3);
        assert_eq!(slice, Some(&[1u8, 2, 3][..]));
        assert_eq!(buf.len, 0);
        assert_eq!(buf.total_read, 3);
    }

    #[test]
    fn test_multiple_operations() {
        let data = [1u8, 2, 3, 4, 5, 6, 7, 8];
        let mut buf = ParserBuf::from_slice(&data);

        // Peek doesn't consume
        let peeked = buf.peek_bytes::<2>();
        assert_eq!(peeked, Some(&[1u8, 2]));
        assert_eq!(buf.total_read, 0);

        // Read consumes
        let read = buf.read_bytes::<2>();
        assert_eq!(read, Some(&[1u8, 2]));
        assert_eq!(buf.total_read, 2);

        // Read slice
        let slice = buf.read_slice(3);
        assert_eq!(slice, Some(&[3u8, 4, 5][..]));
        assert_eq!(buf.total_read, 5);

        // Remaining bytes
        assert_eq!(buf.as_slice(), &[6, 7, 8]);
    }

    #[test]
    fn test_standard_format_lookup() {
        // Test LOOKUP_U8, LOOKUP_U16, LOOKUP_U24 variants
        let data = vec![
            0x40, 0x0d, // LOOKUP_U8
            0xFF, // value 255
            0x3e, 0x0d, // LOOKUP_U16
            0xFF, 0xFF, // value 65535
            0x41, 0x0d, // LOOKUP_U24
            0xFF, 0xFF, 0xFF, // value 16777215
        ];

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        // LOOKUP_U8
        assert_eq!(
            reader.next_kind(&mut format).unwrap(),
            Some(TokenKind::Lookup)
        );
        assert_eq!(reader.lookup_id().value(), 255);

        // LOOKUP_U16
        assert_eq!(
            reader.next_kind(&mut format).unwrap(),
            Some(TokenKind::Lookup)
        );
        assert_eq!(reader.lookup_id().value(), 65535);

        // LOOKUP_U24
        assert_eq!(
            reader.next_kind(&mut format).unwrap(),
            Some(TokenKind::Lookup)
        );
        assert_eq!(reader.lookup_id().value(), 16777215);
    }

    #[test]
    fn test_standard_format_fixed5() {
        // Test FIXED5_ZERO
        let data = vec![0x47, 0x0d]; // FIXED5_ZERO
        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;
        assert_eq!(reader.next_kind(&mut format).unwrap(), Some(TokenKind::F64));
        assert_eq!(reader.i64_data(), 0);

        // Test FIXED5_U8 (e.g., raw value 123 = 0.00123)
        let data = vec![0x48, 0x0d, 0x7B]; // FIXED5_U8, value 123
        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;
        assert_eq!(reader.next_kind(&mut format).unwrap(), Some(TokenKind::F64));
        assert_eq!(reader.i64_data(), 123);

        // Test FIXED5_I8 (negative, e.g., raw value 123 = -0.00123)
        let data = vec![0x4f, 0x0d, 0x7B]; // FIXED5_I8, value -123
        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;
        assert_eq!(reader.next_kind(&mut format).unwrap(), Some(TokenKind::F64));
        assert_eq!(reader.i64_data(), -123);
    }

    #[test]
    fn test_standard_format_rgb() {
        // RGB without alpha: { OPEN U32 r U32 g U32 b CLOSE }
        let data = vec![
            0x43, 0x02, // RGB lexeme (0x0243)
            0x03, 0x00, // OPEN (0x0003)
            0x14, 0x00, // U32 (0x0014)
            0x6E, 0x00, 0x00, 0x00, // r = 110
            0x14, 0x00, // U32 (0x0014)
            0x1C, 0x00, 0x00, 0x00, // g = 28
            0x14, 0x00, // U32 (0x0014)
            0x1B, 0x00, 0x00, 0x00, // b = 27
            0x04, 0x00, // CLOSE (0x0004)
        ];

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;
        assert_eq!(reader.next_kind(&mut format).unwrap(), Some(TokenKind::Rgb));
        let rgb = reader.rgb_data();
        assert_eq!(rgb.r, 110);
        assert_eq!(rgb.g, 28);
        assert_eq!(rgb.b, 27);
        assert_eq!(rgb.a, None);
    }

    // --- Roundtrip Tests ---

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

        // Read back using new API
        let mut reader = TokenReader::from_slice(data.as_slice());
        let mut format = StandardFormat;

        for (i, expected) in input.iter().enumerate() {
            let kind = reader.read_kind(&mut format).unwrap();

            match (expected, kind) {
                (Token::Open, TokenKind::Open) => {}
                (Token::Close, TokenKind::Close) => {}
                (Token::Equal, TokenKind::Equal) => {}
                (Token::U32(val), TokenKind::U32) => {
                    assert_eq!(*val, reader.u32_data(), "U32 mismatch at token {}", i);
                }
                (Token::U64(val), TokenKind::U64) => {
                    assert_eq!(*val, reader.u64_data(), "U64 mismatch at token {}", i);
                }
                (Token::I32(val), TokenKind::I32) => {
                    assert_eq!(*val, reader.i32_data(), "I32 mismatch at token {}", i);
                }
                (Token::I64(val), TokenKind::I64) => {
                    assert_eq!(*val, reader.i64_data(), "I64 mismatch at token {}", i);
                }
                (Token::F32(bytes), TokenKind::F32) => {
                    let expected_f32 = f32::from_le_bytes(*bytes);
                    assert_eq!(
                        expected_f32,
                        reader.f32_data(),
                        "F32 mismatch at token {}",
                        i
                    );
                }
                (Token::F64(bytes), TokenKind::F64) => {
                    let expected_f64 = f64::from_le_bytes(*bytes);
                    assert_eq!(
                        expected_f64,
                        reader.f64_data(),
                        "F64 mismatch at token {}",
                        i
                    );
                }
                (Token::Bool(val), TokenKind::Bool) => {
                    assert_eq!(*val, reader.bool_data(), "Bool mismatch at token {}", i);
                }
                (Token::Quoted(val), TokenKind::Quoted) => {
                    let read = unsafe { reader.read_buffer() };
                    assert_eq!(val.as_bytes(), read, "Quoted mismatch at token {}", i);
                }
                (Token::Unquoted(val), TokenKind::Unquoted) => {
                    let read = unsafe { reader.read_buffer() };
                    assert_eq!(val.as_bytes(), read, "Unquoted mismatch at token {}", i);
                }
                (Token::Rgb(rgb), TokenKind::Rgb) => {
                    let read_rgb = reader.rgb_data();
                    assert_eq!(rgb.r, read_rgb.r, "RGB.r mismatch at token {}", i);
                    assert_eq!(rgb.g, read_rgb.g, "RGB.g mismatch at token {}", i);
                    assert_eq!(rgb.b, read_rgb.b, "RGB.b mismatch at token {}", i);
                    assert_eq!(rgb.a, read_rgb.a, "RGB.a mismatch at token {}", i);
                }
                (Token::Lookup(val), TokenKind::Lookup) => {
                    assert_eq!(
                        *val,
                        reader.lookup_id().value(),
                        "Lookup mismatch at token {}",
                        i
                    );
                }
                (Token::Id(val), TokenKind::Id) => {
                    assert_eq!(
                        *val,
                        reader.field_id().value(),
                        "Id mismatch at token {}",
                        i
                    );
                }
                (expected, kind) => {
                    panic!(
                        "Token mismatch at index {}: expected {:?}, got {:?}",
                        i, expected, kind
                    );
                }
            }
        }

        // Verify we've consumed all data
        assert_eq!(reader.position(), data.len());

        // Attempting to read more should error
        let result = reader.next_kind(&mut format);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
    }

    #[test]
    fn test_not_enough_data() {
        let mut reader = TokenReader::from_slice(&[0x43]);
        let mut format = StandardFormat;
        let result = reader.read_kind(&mut format);
        assert!(matches!(result.unwrap_err().kind(), ErrorKind::Eof));
    }

    /// Tests that a truncated payload (lexeme ID present but insufficient bytes for the value)
    #[rstest]
    #[case(&[0x14, 0x00, 0x01, 0x02, 0x03])] // U32 lexeme (0x0014) needs 4 bytes, only 3 provided
    #[case(&[0x9c, 0x02, 0x01, 0x02, 0x03, 0x04])] // U64 lexeme (0x029c) needs 8 bytes, only 4 provided
    #[case(&[0x0e, 0x00])] // Bool lexeme (0x000e) needs 1 byte, none provided
    #[case(&[0x0f, 0x00, 0x05, 0x00, 0x61, 0x62])] // Quoted lexeme (0x000f) with length prefix 5 but only 2 bytes of payload
    #[case(&[0x0f, 0x00, 0x05, 0x00, 0x61, 0x62])]
    fn test_truncated_payload(#[case] data: &[u8]) {
        let mut reader = TokenReader::from_slice(data);
        let mut format = StandardFormat;
        let result = reader.read_kind(&mut format);
        assert!(
            matches!(result.unwrap_err().kind(), ErrorKind::Eof),
            "expected Eof error for truncated payload"
        );
    }

    #[test]
    fn test_bool_values() {
        // Test both true and false
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Bool(true).write(&mut writer).unwrap();
        Token::Bool(false).write(&mut writer).unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Bool);
        assert!(reader.bool_data());

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Bool);
        assert!(!reader.bool_data());
    }

    #[test]
    fn test_position_tracking() {
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Id(0x1234).write(&mut writer).unwrap();
        Token::Equal.write(&mut writer).unwrap();
        Token::U32(42).write(&mut writer).unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.position(), 0);

        reader.read_kind(&mut format).unwrap(); // Id
        assert_eq!(reader.position(), 2); // 2 bytes for Id

        reader.read_kind(&mut format).unwrap(); // Equal
        assert_eq!(reader.position(), 4); // +2 bytes for Equal

        reader.read_kind(&mut format).unwrap(); // U32
        assert_eq!(reader.position(), 10); // +2 for lexeme + 4 for u32 = 6 more
    }

    #[test]
    fn test_buffer_boundaries() {
        // Test very small string (1 byte)
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Quoted(Scalar::new(b"x")).write(&mut writer).unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Quoted);
        let read = unsafe { reader.read_buffer() };
        assert_eq!(read, b"x");

        // Test larger string
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        let long_string = "a".repeat(255);
        Token::Unquoted(Scalar::new(long_string.as_bytes()))
            .write(&mut writer)
            .unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Unquoted);
        let read = unsafe { reader.read_buffer() };
        assert_eq!(read, long_string.as_bytes());
    }

    #[test]
    fn test_rgb_deserialization_integration() {
        // Test RGB without alpha
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Rgb(Rgb {
            r: 110,
            g: 28,
            b: 27,
            a: None,
        })
        .write(&mut writer)
        .unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Rgb);
        let rgb = reader.rgb_data();
        assert_eq!(rgb.r, 110);
        assert_eq!(rgb.g, 28);
        assert_eq!(rgb.b, 27);
        assert_eq!(rgb.a, None);

        // Test RGB with alpha
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Rgb(Rgb {
            r: 110,
            g: 28,
            b: 27,
            a: Some(128),
        })
        .write(&mut writer)
        .unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data);
        let mut format = StandardFormat;

        assert_eq!(reader.read_kind(&mut format).unwrap(), TokenKind::Rgb);
        let rgb = reader.rgb_data();
        assert_eq!(rgb.r, 110);
        assert_eq!(rgb.g, 28);
        assert_eq!(rgb.b, 27);
        assert_eq!(rgb.a, Some(128));
    }
}
