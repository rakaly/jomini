#![allow(dead_code)]

use std::{
    borrow::Cow,
    io::Read,
    ops::{Deref, DerefMut},
};

use serde::{
    Deserialize,
    de::{self, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor},
};

use crate::{
    DeserializeError, DeserializeErrorKind, Encoding, Error,
    binary::{FailedResolveStrategy, LexError, LexemeId, TokenKind},
};

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

struct MutableBuffer<'a> {
    buf: &'a mut ParserBuf,
    carry_over: u16,
}

impl<'a> MutableBuffer<'a> {
    pub unsafe fn set_len(&mut self, amt: u16) {
        self.buf.len += amt as usize;
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
}

impl<F: BinaryFormat> BinaryFormat for &'_ mut F {
    fn visit(&mut self, reader: &mut ParserState, id: LexemeId) -> Result<TokenSignal, Error> {
        (**self).visit(reader, id)
    }

    fn skip(&mut self, reader: &mut ParserState, id: LexemeId) -> Option<SkipKind> {
        (**self).skip(reader, id)
    }
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

struct Eu4Format;

impl BinaryFormat for Eu4Format {
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

                let result = eu4_specific_f32_decoding(&data).to_bits();
                reader.store(result.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::F32))
            }
            LexemeId::F64 => {
                let Some(data) = reader.read_bytes::<8>().copied() else {
                    return Ok(TokenSignal::MoreData);
                };

                let result = eu4_specific_f64_decoding(&data).to_bits();
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
            id => {
                reader.store(id.0.to_le_bytes());
                Ok(TokenSignal::Kind(TokenKind::Id))
            }
        }
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

    #[inline]
    pub fn skip_kind(&mut self, kind: TokenKind) -> Result<(), Error> {
        match kind {
            TokenKind::Open => {
                // skip_container
                todo!()
            }
            // TokenKind::Quoted | TokenKind::Unquoted | TokenKind::Rgb => {
            //     let _ = self.read_buffer()?;
            //     Ok(())
            // }
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
}

struct BinaryConfig {
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
            match self.de.reader.next_kind(&mut self.de.format) {
                Ok(Some(TokenKind::Close)) => return Ok(None),
                Ok(Some(TokenKind::Open)) => {
                    let _ = self.de.reader.read_kind(&mut self.de.format)?;
                }
                Ok(Some(kind)) => {
                    return seed
                        .deserialize(BinaryReaderTokenDeserializer { de: self.de, kind })
                        .map(Some);
                }
                Ok(None) if ROOT => return Ok(None),
                Ok(None) => {
                    return Err(LexError::Eof.at(self.de.reader.position()).into());
                }
                Err(e) => return Err(e),
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
    fn deser<'de, 'a: 'de, V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
        F: BinaryFlavor2<'a>,
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
                match self
                    .de
                    .format
                    .decode(unsafe { self.de.reader.read_buffer() })
                {
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
            TokenKind::Close => Err(Error::invalid_syntax(
                "did not expect end",
                self.de.reader.position(),
            )),
            TokenKind::Equal => Err(Error::invalid_syntax(
                "did not expect equal",
                self.de.reader.position(),
            )),
            TokenKind::Open => visitor.visit_seq(BinaryReaderSeq::new(self.de)),
            TokenKind::Rgb => todo!(),
            TokenKind::Lookup => match self.de.format.resolve_lookup(self.de.reader.lookup_id()) {
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

impl<'de, 'a: 'de, R: Read, F> de::Deserializer<'de> for BinaryReaderTokenDeserializer<'_, R, F>
where
    F: BinaryFlavor2<'a>,
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
                visitor.visit_bytes(unsafe { self.de.reader.read_buffer() })
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
                match self
                    .de
                    .format
                    .decode(unsafe { self.de.reader.read_buffer() })
                {
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
            TokenKind::Open => {
                let mut seq = BinaryReaderSeq::new(self.de);
                let result = visitor.visit_seq(&mut seq)?;
                if !seq.hit_end {
                    // For when we are deserializing an array that doesn't read
                    // the closing token
                    if !matches!(
                        self.de.reader.read_kind(&mut self.de.format)?,
                        TokenKind::Close
                    ) {
                        return Err(Error::invalid_syntax(
                            "Expected sequence to be terminated with an end token",
                            self.de.reader.position(),
                        ));
                    }
                }
                Ok(result)
            }
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
        visitor.visit_enum(BinaryReaderEnum::new(self.de, self.kind))
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
        let mut kind = self.de.reader.read_kind(&mut self.de.format)?;
        if matches!(kind, TokenKind::Close) {
            self.hit_end = true;
            return Ok(None);
        } else if matches!(kind, TokenKind::Equal) {
            // This is a standalone Equal token from object template syntax
            kind = self.de.reader.read_kind(&mut self.de.format)?;
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de, kind })
            .map(Some)
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
        let variant = seed.deserialize(BinaryReaderTokenDeserializer {
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

        assert_eq!(buf.as_slice(), &[]);
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
}
