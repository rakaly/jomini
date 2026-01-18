#![allow(dead_code)]

use std::{
    borrow::Cow,
    io::Read,
    ops::{Deref, DerefMut},
};

use serde::{
    Deserialize,
    de::{self, DeserializeOwned, DeserializeSeed, MapAccess, Visitor},
};

use crate::{
    DeserializeError, DeserializeErrorKind, Error, Windows1252Encoding,
    binary::{FailedResolveStrategy, LexError, LexemeId, TokenKind},
};

pub struct ParserBuf {
    buf: Box<[u8]>,
    start: u16,
    end: u16,
    total_read: usize,
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
        self.start += N as u16;
        self.total_read += N;
        Some(result)
    }

    #[inline]
    fn read_slice(&mut self, len: u16) -> Option<&'_ [u8]> {
        let result = unsafe { self.buf.get_unchecked(self.range()) }.get(..len as usize)?;
        self.start += len;
        self.total_read += len as usize;
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
}

pub enum TokenSignal {
    Kind(TokenKind),
    Eof,
}

pub enum SkipKind {
    Open,
    Close,
    Other,
}

pub trait BinaryFormat<'str> {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error>;
    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        match self.visit(reader, id) {
            Ok(TokenSignal::Kind(TokenKind::Open)) => Some(SkipKind::Open),
            Ok(TokenSignal::Kind(TokenKind::Close)) => Some(SkipKind::Close),
            Ok(TokenSignal::Kind(_)) => Some(SkipKind::Other),
            Ok(TokenSignal::Eof) | Err(_) => None,
        }
    }

    fn resolve_field(&self, token: FieldId) -> Option<&'str str> {
        None
    }

    fn resolve_lookup(&self, index: LookupIndex) -> Option<&'str str> {
        None
    }

    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str>;
}

impl<'str, F: BinaryFormat<'str>> BinaryFormat<'str> for &'_ mut F {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error> {
        (**self).visit(reader, id)
    }

    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        (**self).decode(data)
    }

    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        (**self).skip(reader, id)
    }

    fn resolve_field(&self, token: FieldId) -> Option<&'str str> {
        (**self).resolve_field(token)
    }

    fn resolve_lookup(&self, index: LookupIndex) -> Option<&'str str> {
        (**self).resolve_lookup(index)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct FieldId(u16);

impl FieldId {
    pub const fn new(index: u16) -> Self {
        FieldId(index)
    }

    pub const fn value(self) -> u16 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct LookupIndex(u32);

impl LookupIndex {
    pub const fn new(index: u32) -> Self {
        LookupIndex(index)
    }

    pub const fn value(self) -> u32 {
        self.0
    }
}

struct Eu4Format;

impl BinaryFormat<'static> for Eu4Format {
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
                reader.store(id.0.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::Id))
            }
        }
    }

    // Replace TokenResolver
    fn resolve_field(&self, field: FieldId) -> Option<&'static str> {
        eu4_specific_token_resolution(field.value())
    }

    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Windows1252Encoding::decode(data)
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

pub struct TokenReader<R> {
    reader: R,
    state: ParserState,
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
        let mut spare = self.state.buf.spare_capacity_mut();
        let amt = self.reader.read(&mut spare).unwrap();
        unsafe {
            spare.set_len(amt as u16);
        }
        Ok(amt)
    }

    #[cold]
    fn next_kind_refill<F>(&mut self, mut format: F) -> Result<Option<TokenKind>, Error>
    where
        for<'a> F: BinaryFormat<'a>,
    {
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
            .expect("todo");

        match format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::Eof => todo!(),
        }
    }

    #[inline]
    pub fn next_kind<F>(&mut self, mut format: F) -> Result<Option<TokenKind>, Error>
    where
        for<'a> F: BinaryFormat<'a>,
    {
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

        self.state.buf.end += 2;
        match format.visit(&mut self.state, id)? {
            TokenSignal::Kind(kind) => Ok(Some(kind)),
            TokenSignal::Eof => self.next_kind_refill(format),
        }
    }

    #[inline]
    pub fn read_kind<F>(&mut self, format: F) -> Result<TokenKind, Error>
    where
        for<'a> F: BinaryFormat<'a>,
    {
        match self.next_kind(format)? {
            Some(kind) => Ok(kind),
            None => Err(LexError::Eof.at(self.position()).into()),
        }
    }

    #[inline]
    pub fn skip_kind(&mut self, kind: TokenKind) -> Result<(), Error> {
        match kind {
            TokenKind::Open => {
                // skip_container
                todo!()
            }
            TokenKind::Quoted | TokenKind::Unquoted | TokenKind::Rgb => {
                let _ = self.read_buffer()?;
                Ok(())
            }
            _ => Ok(()),
        }
    }

    #[inline]
    pub fn read_buffer(&mut self) -> Result<&[u8], Error> {
        let len = u16::from_le_bytes([self.state.storage[0], self.state.storage[1]]);

        // Check if we have enough data without borrowing
        let available = self.state.buf.end - self.state.buf.start;
        if available >= len {
            return Ok(unsafe { self.state.buf.read_slice(len).unwrap_unchecked() });
        }

        self.fill()?;
        self.state.buf.read_slice(len).ok_or_else(|| todo!())
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
}

pub struct BinaryReaderDeserializer<R, F> {
    reader: TokenReader<R>,
    format: F,
    // config: BinaryConfig<'res, RES, F>,
}

impl<R, F> BinaryReaderDeserializer<R, F>
where
    R: Read,
    for<'a> F: BinaryFormat<'a>,
{
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

fn deserialize<T>(reader: TokenReader<impl Read>, format: impl for<'a> BinaryFormat<'a>) -> Result<T, Error>
where
    T: DeserializeOwned,
{
    let mut reader = BinaryReaderDeserializer { reader, format };
    let value: T = serde::de::Deserialize::deserialize(&mut reader).unwrap();

    Ok(value)
}

// fn deserialize2<'a, T, F, A>(reader: TokenReader<impl Read>, format: F) -> Result<T, Error>
// where
//     T: Deserialize<'a>,
//     A: AsRef<str> + 'a,
//     F: BinaryFormat<'a, Id<'a> = A> + 'a,
// {
//     let mut reader = BinaryReaderDeserializer { reader, format };

//     let value: T = serde::de::Deserialize::deserialize(&mut reader).unwrap();

//     Ok(value)
// }

impl<'de, F, R> de::Deserializer<'de> for &'_ mut BinaryReaderDeserializer<R, F>
where
    F: BinaryFormat<'de>,
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

impl<'de, const ROOT: bool, R, F> MapAccess<'de> for BinaryReaderMap<'_, ROOT, R, F>
where
    R: Read,
    F: BinaryFormat<'de>,
{
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match self.de.reader.next_kind(&mut self.de.format) {
                Ok(Some(TokenKind::Close)) => return Ok(None),
                Ok(Some(TokenKind::Open)) => {
                    // let _ = self.de.reader.read_token();
                    todo!()
                }
                Ok(Some(token)) => {
                    todo!()
                    // return seed
                    //     .deserialize(BinaryReaderTokenDeserializer { de: self.de, token })
                    //     .map(Some);
                }
                Ok(None) if ROOT => return Ok(None),
                Ok(None) => {
                    return Err(LexError::Eof.at(self.de.reader.position()).into());
                }
                Err(e) => return Err(e.into()),
            }
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let mut kind = self.de.reader.read_kind(&mut self.de.format)?;
        if matches!(kind, TokenKind::Equal) {
            kind = self.de.reader.read_kind(&mut self.de.format)?;
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de, kind })
    }
}

struct BinaryReaderTokenDeserializer<'a, R, F> {
    de: &'a mut BinaryReaderDeserializer<R, F>,
    kind: TokenKind,
}

impl<R, F> BinaryReaderTokenDeserializer<'_, R, F>
where
    R: Read,
{
    #[inline]
    fn deser<'de, V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
        F: BinaryFormat<'de>,
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
                match self.de.format.decode(self.de.reader.read_buffer()?) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            TokenKind::Id => match self.de.format.resolve_field(self.de.reader.field_id()) {
                Some(id) => visitor.visit_borrowed_str(id.as_ref()),
                None => todo!(),
                // None => match self.de.config.failed_resolve_strategy {
                //     FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                //         kind: DeserializeErrorKind::UnknownToken {
                //             token_id: self.de.reader.token_id() as u32,
                //         },
                //     })),
                //     FailedResolveStrategy::Stringify => {
                //         visitor.visit_string(format!("0x{:x}", self.de.reader.token_id()))
                //     }
                //     FailedResolveStrategy::Ignore => {
                //         visitor.visit_borrowed_str("__internal_identifier_ignore")
                //     }
                // },
            },
            TokenKind::Close => Err(Error::invalid_syntax(
                "did not expect end",
                self.de.reader.position(),
            )),
            TokenKind::Equal => Err(Error::invalid_syntax(
                "did not expect equal",
                self.de.reader.position(),
            )),
            // TokenKind::Open => visitor.visit_seq(BinaryReaderSeq::new(self.de)),
            TokenKind::Open => todo!(),
            TokenKind::Rgb => todo!(),
            TokenKind::Lookup => match self.de.format.resolve_lookup(self.de.reader.lookup_id()) {
                Some(value) => visitor.visit_borrowed_str(value.as_ref()),
                None => todo!(),
                // None => match self.de.config.failed_resolve_strategy {
                //     FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                //         kind: DeserializeErrorKind::UnknownToken { token_id: index },
                //     })),
                //     FailedResolveStrategy::Stringify => {
                //         visitor.visit_string(format!("{}", index))
                //     }
                //     FailedResolveStrategy::Ignore => {
                //         visitor.visit_borrowed_str("__internal_identifier_ignore")
                //     }
                // },
            },
        }
    }
}

macro_rules! deserialize_scalar {
    ($method:ident) => {
        #[inline]
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            self.deser(visitor)
        }
    };
}

impl<'de, R, F> de::Deserializer<'de> for BinaryReaderTokenDeserializer<'_, R, F>
where
    F: BinaryFormat<'de>,
    R: Read,
{
    type Error = Error;

    deserialize_scalar!(deserialize_any);
    deserialize_scalar!(deserialize_i8);
    deserialize_scalar!(deserialize_i16);
    deserialize_scalar!(deserialize_u8);
    deserialize_scalar!(deserialize_char);
    deserialize_scalar!(deserialize_identifier);

    #[inline]
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.kind {
            TokenKind::Quoted | TokenKind::Unquoted => {
                visitor.visit_bytes(self.de.reader.read_buffer()?)
            }
            _ => self.deser(visitor),
        }
    }

    #[inline]
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    #[inline]
    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::Bool) {
            visitor.visit_bool(self.de.reader.bool_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.kind {
            TokenKind::Id => visitor.visit_u16(self.de.reader.field_id().value()),
            _ => self.deser(visitor),
        }
    }

    #[inline]
    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::I32) {
            visitor.visit_i32(self.de.reader.i32_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.kind {
            TokenKind::U32 => visitor.visit_u32(self.de.reader.u32_data()),
            TokenKind::Lookup => visitor.visit_u32(self.de.reader.lookup_id().value()),
            _ => self.deser(visitor),
        }
    }

    #[inline]
    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::U64) {
            visitor.visit_u64(self.de.reader.u64_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::I64) {
            visitor.visit_i64(self.de.reader.i64_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::F32) {
            visitor.visit_f32(self.de.reader.f32_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.kind, TokenKind::F64) {
            visitor.visit_f64(self.de.reader.f64_data())
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    #[inline]
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.kind {
            TokenKind::Quoted | TokenKind::Unquoted => {
                match self.de.format.decode(self.de.reader.read_buffer()?) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            _ => self.deser(visitor),
        }
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
        match self.kind {
            TokenKind::Open => todo!(),
            // TokenKind::Open => {
            //     let mut seq = BinaryReaderSeq::new(self.de);
            //     let result = visitor.visit_seq(&mut seq)?;
            //     if !seq.hit_end {
            //         // For when we are deserializing an array that doesn't read
            //         // the closing token
            //         if !matches!(self.de.reader.read_token()?, TokenKind::Close) {
            //             return Err(Error::invalid_syntax(
            //                 "Expected sequence to be terminated with an end token",
            //                 self.de.reader.position(),
            //             ));
            //         }
            //     }
            //     Ok(result)
            // }
            // TokenKind::Rgb => visitor.visit_seq(ColorSequence::new(self.de.reader.rgb_data())),
            TokenKind::Rgb => todo!(),
            _ => self.deser(visitor),
        }
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
        if matches!(self.kind, TokenKind::Open) {
            visitor.visit_map(BinaryReaderMap::new::<false>(self.de))
        } else {
            self.deser(visitor)
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
        todo!()
        // visitor.visit_enum(BinaryReaderEnum::new(self.de, self.token))
    }

    #[inline]
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.reader.skip_kind(self.kind)?;
        visitor.visit_unit()
    }
}

// struct BinaryReaderSeq<'a: 'a, 'res, RES: 'a, F, R> {
//     de: &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
//     hit_end: bool,
// }

// impl<'a, 'res, RES: 'a, F, R> BinaryReaderSeq<'a, 'res, RES, F, R> {
//     fn new(de: &'a mut BinaryReaderDeserializer<'res, RES, F, R>) -> Self {
//         BinaryReaderSeq { de, hit_end: false }
//     }
// }

// impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> SeqAccess<'de>
//     for BinaryReaderSeq<'_, 'res, RES, F, R>
// {
//     type Error = Error;

//     fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
//     where
//         T: DeserializeSeed<'de>,
//     {
//         let mut token = self.de.reader.read_token()?;
//         if matches!(token, TokenKind::Close) {
//             self.hit_end = true;
//             return Ok(None);
//         } else if matches!(token, TokenKind::Equal) {
//             // This is a standalone Equal token from object template syntax
//             token = self.de.reader.read_token()?;
//         }

//         seed.deserialize(BinaryReaderTokenDeserializer { de: self.de, token })
//             .map(Some)
//     }
// }

// struct BinaryReaderEnum<'a, 'res, RES: 'a, F, R> {
//     de: &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
//     token: TokenKind,
// }

// impl<'a, 'res, RES: 'a, F, R> BinaryReaderEnum<'a, 'res, RES, F, R> {
//     fn new(de: &'a mut BinaryReaderDeserializer<'res, RES, F, R>, token: TokenKind) -> Self {
//         BinaryReaderEnum { de, token }
//     }
// }

// impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> de::EnumAccess<'de>
//     for BinaryReaderEnum<'_, 'res, RES, F, R>
// {
//     type Error = Error;
//     type Variant = Self;

//     fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
//     where
//         V: de::DeserializeSeed<'de>,
//     {
//         let variant = seed.deserialize(BinaryReaderTokenDeserializer {
//             de: self.de,
//             token: self.token,
//         })?;
//         Ok((variant, self))
//     }
// }

// impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R> de::VariantAccess<'de>
//     for BinaryReaderEnum<'_, 'res, RES, F, R>
// {
//     type Error = Error;

//     fn unit_variant(self) -> Result<(), Self::Error> {
//         Ok(())
//     }

//     fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
//     where
//         T: DeserializeSeed<'de>,
//     {
//         Err(Error::from(DeserializeError {
//             kind: DeserializeErrorKind::Unsupported(String::from(
//                 "unsupported enum deserialization. Please file issue",
//             )),
//         }))
//     }

//     fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
//     where
//         V: Visitor<'de>,
//     {
//         Err(Error::from(DeserializeError {
//             kind: DeserializeErrorKind::Unsupported(String::from(
//                 "unsupported enum deserialization. Please file issue",
//             )),
//         }))
//     }

//     fn struct_variant<V>(
//         self,
//         _fields: &'static [&'static str],
//         _visitor: V,
//     ) -> Result<V::Value, Self::Error>
//     where
//         V: Visitor<'de>,
//     {
//         Err(Error::from(DeserializeError {
//             kind: DeserializeErrorKind::Unsupported(String::from(
//                 "unsupported enum deserialization. Please file issue",
//             )),
//         }))
//     }
// }
