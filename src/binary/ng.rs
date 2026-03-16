use crate::{
    DeserializeError, DeserializeErrorKind, Error,
    binary::{LexError, LexemeId, Rgb},
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
        let dst = if self.buf.is_empty() {
            &mut self.buf
        } else if self.len == 0 {
            self.start = self.buf.as_ptr();
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
    pub fn read_exact<const N: usize>(&mut self) -> Result<[u8; N], Error> {
        self.read_bytes::<N>().copied().ok_or_else(Error::eof)
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
    pub fn as_slice(&self) -> &[u8] {
        self.buf.as_slice()
    }

    #[inline]
    pub fn peek_bytes<const N: usize>(&self) -> Option<&[u8; N]> {
        self.buf.peek_bytes::<N>()
    }

    #[inline]
    pub fn token_cursor(&mut self) -> TokenCursor<'_> {
        TokenCursor {
            state: &mut self.buf,
            consumed: 0,
        }
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
    pub(crate) unsafe fn consume(&mut self, amt: usize) {
        self.buf.start = unsafe { self.buf.start.add(amt) };
        self.buf.len -= amt;
        self.buf.total_read += amt;
    }
}

#[derive(Clone, Copy)]
pub struct CursorCheckpoint(usize);

pub struct TokenCursor<'a> {
    state: &'a mut ParserBuf,
    consumed: usize,
}

impl<'a> TokenCursor<'a> {
    #[inline]
    pub fn checkpoint(&self) -> CursorCheckpoint {
        CursorCheckpoint(self.consumed)
    }

    #[inline]
    pub fn consumed(&self) -> usize {
        self.consumed
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.state.len - self.consumed
    }

    #[inline]
    pub fn read_bytes<const N: usize>(&mut self) -> Option<&'a [u8; N]> {
        let result =
            unsafe { std::slice::from_raw_parts(self.state.start.add(self.consumed), self.len()) }
                .first_chunk::<N>()?;
        self.consumed += N;
        Some(result)
    }

    #[inline]
    pub fn read_slice(&mut self, len: u16) -> Option<&'a [u8]> {
        let result =
            unsafe { std::slice::from_raw_parts(self.state.start.add(self.consumed), self.len()) }
                .get(..len as usize)?;
        self.consumed += len as usize;
        Some(result)
    }

    #[inline]
    pub fn read_len_prefixed(&mut self) -> Option<&'a [u8]> {
        let slice =
            unsafe { std::slice::from_raw_parts(self.state.start.add(self.consumed), self.len()) };
        let len_bytes = slice.first_chunk::<2>()?;
        let len = u16::from_le_bytes(*len_bytes) as usize;
        let result = slice.get(2..2 + len)?;
        self.consumed += 2 + len;
        Some(result)
    }

    #[inline]
    pub fn read_lexeme(&mut self) -> Option<LexemeId> {
        let data = self.read_bytes::<2>()?;
        Some(LexemeId::new(u16::from_le_bytes(*data)))
    }

    #[inline]
    pub fn consume(self) {
        self.state.start = unsafe { self.state.start.add(self.consumed) };
        self.state.len -= self.consumed;
        self.state.total_read += self.consumed;
    }

    #[inline]
    pub fn consume_to(self, checkpoint: CursorCheckpoint) {
        debug_assert!(checkpoint.0 <= self.consumed);
        self.state.start = unsafe { self.state.start.add(checkpoint.0) };
        self.state.len -= checkpoint.0;
        self.state.total_read += checkpoint.0;
    }
}

pub enum TokenResult<T> {
    Token(T),
    MoreData,
}

pub trait BinaryTokenFormat {
    type Token<'a>;

    fn next_token<'a>(
        &mut self,
        reader: &'a mut ParserState,
    ) -> Result<TokenResult<Self::Token<'a>>, Error>;

    #[inline]
    fn on_open(&mut self) {}

    /// Called by the serde deserializer after it has already peeked and
    /// identified an `=` structural lexeme and before it consumes the 2-byte
    /// lexeme from the stream.
    #[inline]
    fn on_equal(&mut self) {}

    /// Called by the serde deserializer after it has already peeked and
    /// identified a `}` structural lexeme and before it consumes the 2-byte
    /// lexeme from the stream.
    #[inline]
    fn on_close(&mut self) {}

    fn skip_value(
        &mut self,
        state: &mut ParserState,
        fill: &mut impl FnMut(&mut ParserState) -> Result<usize, Error>,
    ) -> Result<(), Error>
    where
        Self: Sized,
    {
        skip_value_slow(self, state, fill)
    }
}

fn skip_value_slow<F: BinaryTokenFormat>(
    format: &mut F,
    state: &mut ParserState,
    fill: &mut impl FnMut(&mut ParserState) -> Result<usize, Error>,
) -> Result<(), Error> {
    let mut depth: i32 = -1;
    loop {
        let lexeme = loop {
            let Some(id_bytes) = state.peek_bytes::<2>().copied() else {
                if fill(state)? == 0 {
                    return Err(Error::eof());
                }
                continue;
            };
            break LexemeId::new(u16::from_le_bytes(id_bytes));
        };

        let state_ptr = std::ptr::addr_of_mut!(*state);
        let result = format.next_token(unsafe { &mut *state_ptr })?;

        match result {
            TokenResult::Token(_) => {}
            TokenResult::MoreData => {
                if fill(state)? == 0 {
                    return Err(Error::eof());
                }
                continue;
            }
        }

        match lexeme {
            LexemeId::OPEN => {
                depth += 1;
            }
            LexemeId::CLOSE => {
                if depth <= 0 {
                    return Ok(());
                }
                depth -= 1;
            }
            _ => {
                if depth < 0 {
                    return Ok(());
                }
            }
        }
    }
}

pub trait BinaryValueFormat {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error>;

    fn deserialize_i32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_u32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_i64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_u64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_f32<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_f64<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_bool<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.deserialize_any(reader, visitor)
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V>;
}

pub trait BinaryFormat: BinaryTokenFormat + BinaryValueFormat {}

impl<T: BinaryTokenFormat + BinaryValueFormat> BinaryFormat for T {}

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
    fn visit_bytes(self, v: &[u8]) -> Result<Self::Value, Error>;
    fn visit_unit(self) -> Result<Self::Value, Error>;
    fn visit_rgb(self, rgb: Rgb) -> Result<Self::Value, Error>;
}

pub enum ValueResult<T, V> {
    Value(T),
    Open(V),
    MoreData(V),
}

pub type VisitResult<'de, V> = Result<ValueResult<<V as PdxVisitor<'de>>::Value, V>, Error>;

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

macro_rules! forward_deserialize {
    ($method:ident) => {
        #[inline]
        fn $method<'de, V: PdxVisitor<'de>>(
            &mut self,
            reader: &mut ParserState,
            visitor: V,
        ) -> VisitResult<'de, V> {
            (**self).$method(reader, visitor)
        }
    };
}

impl<F: BinaryValueFormat> BinaryValueFormat for &'_ mut F {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        (**self).decode_scalar(data)
    }

    forward_deserialize!(deserialize_i32);
    forward_deserialize!(deserialize_u32);
    forward_deserialize!(deserialize_i64);
    forward_deserialize!(deserialize_u64);
    forward_deserialize!(deserialize_f32);
    forward_deserialize!(deserialize_f64);
    forward_deserialize!(deserialize_bool);
    forward_deserialize!(deserialize_str);
    forward_deserialize!(deserialize_identifier);
    forward_deserialize!(deserialize_any);
}

impl<F: BinaryTokenFormat> BinaryTokenFormat for &'_ mut F {
    type Token<'a> = F::Token<'a>;

    fn next_token<'a>(
        &mut self,
        reader: &'a mut ParserState,
    ) -> Result<TokenResult<Self::Token<'a>>, Error> {
        (**self).next_token(reader)
    }

    #[inline]
    fn on_open(&mut self) {
        (**self).on_open()
    }

    #[inline]
    fn on_equal(&mut self) {
        (**self).on_equal()
    }

    #[inline]
    fn on_close(&mut self) {
        (**self).on_close()
    }

    fn skip_value(
        &mut self,
        state: &mut ParserState,
        fill: &mut impl FnMut(&mut ParserState) -> Result<usize, Error>,
    ) -> Result<(), Error> {
        (**self).skip_value(state, fill)
    }
}

pub struct TokenReader<R, F> {
    reader: R,
    state: ParserState,
    format: F,
}

impl<F> TokenReader<(), F> {
    pub fn from_slice(data: &[u8], format: F) -> TokenReader<&[u8], F> {
        TokenReader {
            reader: data,
            state: ParserState {
                buf: ParserBuf::from_slice(data),
                storage: [0u8; 8],
            },
            format,
        }
    }
}

impl<R, F> TokenReader<R, F>
where
    R: std::io::Read,
    F: BinaryTokenFormat,
{
    pub fn new(reader: R, format: F) -> Self {
        TokenReader {
            reader,
            state: ParserState {
                buf: ParserBuf::new(32 * 1024),
                storage: [0u8; 8],
            },
            format,
        }
    }

    pub fn buffered_data(&self) -> &[u8] {
        self.state.buf.as_slice()
    }

    pub fn into_remainder(self) -> (R, ParserBuf, F) {
        (self.reader, self.state.buf, self.format)
    }

    pub fn position(&self) -> usize {
        self.state.buf.total_read
    }

    #[cold]
    fn fill(&mut self) -> Result<usize, Error> {
        let amt = self.state.buf.fill(&mut self.reader)?;
        Ok(amt)
    }

    #[inline]
    pub fn next_token(&mut self) -> Result<Option<F::Token<'_>>, Error> {
        loop {
            let state = std::ptr::addr_of_mut!(self.state);
            match self.format.next_token(unsafe { &mut *state })? {
                TokenResult::Token(token) => return Ok(Some(token)),
                TokenResult::MoreData => {
                    let amt = self.fill()?;
                    if amt == 0 {
                        return if self.state.is_empty() {
                            Ok(None)
                        } else {
                            Err(Error::eof())
                        };
                    }
                }
            }
        }
    }

    #[inline]
    pub fn read_token(&mut self) -> Result<F::Token<'_>, Error> {
        match self.next_token()? {
            Some(token) => Ok(token),
            None => Err(Error::eof()),
        }
    }

    /// Peeks at the next LexemeId without consuming it.
    /// Returns `Ok(None)` at EOF, `Err` on IO error during refill.
    #[inline]
    pub fn peek_lexeme_id(&mut self) -> Result<Option<LexemeId>, Error> {
        if let Some(bytes) = self.state.buf.peek_bytes::<2>() {
            Ok(Some(LexemeId::new(u16::from_le_bytes([
                bytes[0], bytes[1],
            ]))))
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
            Some(bytes) => Ok(Some(LexemeId::new(u16::from_le_bytes([
                bytes[0], bytes[1],
            ])))),
            None => Err(Error::eof()),
        }
    }

    /// Skips the next binary value using the active format's skip logic.
    #[inline]
    pub fn skip_value(&mut self) -> Result<(), Error> {
        let reader = &mut self.reader;
        self.format.skip_value(&mut self.state, &mut |state| {
            Ok(state.buf.fill(&mut *reader)?)
        })
    }
}

pub struct BinaryReaderDeserializer<R, F>
where
    F: BinaryFormat,
{
    reader: TokenReader<R, F>,
}

impl<R, F> BinaryReaderDeserializer<R, F>
where
    R: Read,
    F: BinaryFormat,
{
    /// Create a binary reader deserializer with the provided format
    pub fn from_reader(reader: R, format: F) -> Self {
        BinaryReaderDeserializer {
            reader: TokenReader::new(reader, format),
        }
    }

    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

pub fn from_slice<'de, T, F>(data: &'de [u8], format: F) -> Result<T, Error>
where
    T: Deserialize<'de>,
    F: BinaryFormat,
{
    let mut reader = BinaryReaderDeserializer {
        reader: TokenReader::from_slice(data, format),
    };
    let value = serde::de::Deserialize::deserialize(&mut reader)?;

    Ok(value)
}

pub fn from_reader<T, R, F>(reader: R, format: F) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
    F: BinaryFormat,
{
    let mut reader = BinaryReaderDeserializer::from_reader(reader, format);

    let value = serde::de::Deserialize::deserialize(&mut reader)?;

    Ok(value)
}

impl<'de, F, R> de::Deserializer<'de> for &'_ mut BinaryReaderDeserializer<R, F>
where
    F: BinaryFormat,
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

struct BinaryReaderMap<'a, const ROOT: bool, R, F>
where
    F: BinaryFormat,
{
    de: &'a mut BinaryReaderDeserializer<R, F>,
}

impl<'a, R, F> BinaryReaderMap<'a, false, R, F>
where
    F: BinaryFormat,
{
    fn new<const ROOT: bool>(
        de: &'a mut BinaryReaderDeserializer<R, F>,
    ) -> BinaryReaderMap<'a, ROOT, R, F> {
        BinaryReaderMap::<'a, ROOT, R, F> { de }
    }
}

impl<'de, const ROOT: bool, R, F> MapAccess<'de> for BinaryReaderMap<'_, ROOT, R, F>
where
    R: Read,
    F: BinaryFormat,
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
                    unsafe {
                        self.de.reader.state.consume(2);
                    }
                    self.de.reader.format.on_close();
                    return Ok(None);
                }
                Some(LexemeId::OPEN) => {
                    // Ghost object: consume open, skip the key inside
                    unsafe {
                        self.de.reader.state.consume(2);
                    }
                    self.de.reader.format.on_open();
                    self.de.reader.skip_value()?;
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
            unsafe {
                self.de.reader.state.consume(2);
            }
            self.de.reader.format.on_equal();
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de })
    }
}

struct BinaryReaderTokenDeserializer<'a, R, F>
where
    F: BinaryFormat,
{
    de: &'a mut BinaryReaderDeserializer<R, F>,
}

impl<R, F> BinaryReaderTokenDeserializer<'_, R, F>
where
    R: Read,
    F: BinaryFormat,
{
    #[inline]
    fn parse_with_refill<'de, V>(
        de: &mut BinaryReaderDeserializer<R, F>,
        visitor: V,
        mut parse: impl FnMut(
            &mut F,
            &mut ParserState,
            PdxSerdeVisitor<V>,
        ) -> Result<ValueResult<V::Value, PdxSerdeVisitor<V>>, Error>,
    ) -> Result<ValueResult<V::Value, PdxSerdeVisitor<V>>, Error>
    where
        V: Visitor<'de>,
    {
        let mut visitor = PdxSerdeVisitor(visitor);

        loop {
            let result = {
                let de = &mut *de;
                parse(
                    &mut de.reader.format,
                    &mut de.reader.state,
                    visitor,
                )?
            };

            match result {
                ValueResult::MoreData(pdx) => {
                    visitor = pdx;
                    let amt = de.reader.fill()?;
                    if amt == 0 {
                        return Err(Error::eof());
                    }
                }
                other => return Ok(other),
            }
        }
    }

    #[inline]
    fn handle_parse_result<'de, V>(
        de: &mut BinaryReaderDeserializer<R, F>,
        result: ValueResult<V::Value, PdxSerdeVisitor<V>>,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match result {
            ValueResult::Value(v) => Ok(v),
            ValueResult::Open(pdx) => {
                let visitor = pdx.into_inner();
                let mut seq = BinaryReaderSeq::new(de);
                let result = visitor.visit_seq(&mut seq)?;
                if !seq.hit_end {
                    if de.reader.peek_lexeme_id_refill()? != Some(LexemeId::CLOSE) {
                        return Err(Error::invalid_syntax(
                            "Expected sequence to be terminated with an end token",
                            de.reader.position(),
                        ));
                    }

                    unsafe {
                        de.reader.state.consume(2);
                    }
                    de.reader.format.on_close();
                }
                Ok(result)
            }
            ValueResult::MoreData(_) => unreachable!("parse_with_refill resolves MoreData"),
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
            let result =
                Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                    format.$method(state, visitor)
                })?;
            Self::handle_parse_result(self.de, result)
        }
    };
}

impl<'de, R: Read, F> de::Deserializer<'de> for BinaryReaderTokenDeserializer<'_, R, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    #[inline]
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let result =
            Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                format.deserialize_any(state, visitor)
            })?;
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
        let result =
            Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                format.deserialize_identifier(state, visitor)
            })?;
        Self::handle_parse_result(self.de, result)
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
        let result =
            Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                format.deserialize_str(state, visitor)
            })?;
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
        let result =
            Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                format.deserialize_any(state, visitor)
            })?;
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
        let result =
            Self::parse_with_refill(self.de, visitor, |format, state, visitor| {
                format.deserialize_any(state, visitor)
            })?;
        match result {
            ValueResult::Value(v) => Ok(v),
            ValueResult::Open(pdx) => pdx
                .into_inner()
                .visit_map(BinaryReaderMap::new::<false>(self.de)),
            ValueResult::MoreData(_) => unreachable!("parse_with_refill resolves MoreData"),
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
        visitor.visit_enum(BinaryReaderEnum::new(self.de))
    }

    #[inline]
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.reader.skip_value()?;
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

struct BinaryReaderSeq<'a, R, F>
where
    F: BinaryFormat,
{
    de: &'a mut BinaryReaderDeserializer<R, F>,
    hit_end: bool,
}

impl<'a, R, F> BinaryReaderSeq<'a, R, F>
where
    F: BinaryFormat,
{
    fn new(de: &'a mut BinaryReaderDeserializer<R, F>) -> Self {
        BinaryReaderSeq { de, hit_end: false }
    }
}

impl<'de, R: Read, F> SeqAccess<'de> for BinaryReaderSeq<'_, R, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.de.reader.peek_lexeme_id()? {
            Some(LexemeId::CLOSE) => {
                unsafe {
                    self.de.reader.state.consume(2);
                }
                self.de.reader.format.on_close();
                self.hit_end = true;
                return Ok(None);
            }
            Some(LexemeId::EQUAL) => {
                // This is a standalone Equal token from object template syntax
                unsafe {
                    self.de.reader.state.consume(2);
                }
                self.de.reader.format.on_equal();
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
struct BinaryReaderEnum<'a, R, F>
where
    F: BinaryFormat,
{
    de: &'a mut BinaryReaderDeserializer<R, F>,
}

impl<'a, F, R> BinaryReaderEnum<'a, R, F>
where
    F: BinaryFormat,
{
    fn new(de: &'a mut BinaryReaderDeserializer<R, F>) -> Self {
        BinaryReaderEnum { de }
    }
}

impl<'de, R: Read, F> de::EnumAccess<'de> for BinaryReaderEnum<'_, R, F>
where
    F: BinaryFormat,
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(BinaryReaderTokenDeserializer { de: self.de })?;
        Ok((variant, self))
    }
}

impl<'de, R: Read, F> de::VariantAccess<'de> for BinaryReaderEnum<'_, R, F>
where
    F: BinaryFormat,
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
        DeserializeError, DeserializeErrorKind, ErrorKind, Scalar,
        binary::{FailedResolveStrategy, Rgb, Token, lexer::read_rgb},
    };
    use rstest::*;
    use serde::de::Error as _;
    use standard_support::{StandardFormat, StandardToken};
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    struct FieldId(u16);

    impl FieldId {
        #[inline]
        const fn new(index: u16) -> Self {
            Self(index)
        }

        #[inline]
        const fn value(self) -> u16 {
            self.0
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
    struct LookupIndex(u32);

    impl LookupIndex {
        #[inline]
        const fn new(index: u32) -> Self {
            Self(index)
        }

        #[inline]
        const fn value(self) -> u32 {
            self.0
        }
    }

    mod standard_support {
        use super::*;

        fn visit_field_identifier<'de, V: PdxVisitor<'de>>(
            field: FieldId,
            visitor: V,
            format: &StandardFormat,
        ) -> VisitResult<'de, V> {
            match format.fields.get(&field.value()).copied() {
                Some(name) => Ok(ValueResult::Value(visitor.visit_str(name)?)),
                None => match format.failed_resolve_strategy {
                    FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::UnknownToken {
                            token_id: field.value() as u32,
                        },
                    })),
                    FailedResolveStrategy::Stringify => Ok(ValueResult::Value(
                        visitor.visit_string(format!("0x{:x}", field.value()))?,
                    )),
                    FailedResolveStrategy::Ignore => Ok(ValueResult::Value(
                        visitor.visit_str("__internal_identifier_ignore")?,
                    )),
                },
            }
        }

        #[derive(Debug, Clone, PartialEq)]
        pub enum StandardToken<'a> {
            Open,
            Close,
            Equal,
            U32(u32),
            I32(i32),
            U64(u64),
            I64(i64),
            F32([u8; 4]),
            F64([u8; 8]),
            Fixed5(i64),
            Bool(bool),
            Quoted(&'a [u8]),
            Unquoted(&'a [u8]),
            Lookup(LookupIndex),
            Rgb(Rgb),
            Id(FieldId),
        }

        fn standard_is_field_id(id: LexemeId) -> bool {
            !matches!(
                id,
                LexemeId::OPEN
                    | LexemeId::CLOSE
                    | LexemeId::EQUAL
                    | LexemeId::U32
                    | LexemeId::I32
                    | LexemeId::U64
                    | LexemeId::I64
                    | LexemeId::F32
                    | LexemeId::F64
                    | LexemeId::BOOL
                    | LexemeId::QUOTED
                    | LexemeId::UNQUOTED
                    | LexemeId::LOOKUP_U8
                    | LexemeId::LOOKUP_U8_ALT
                    | LexemeId::LOOKUP_U16
                    | LexemeId::LOOKUP_U16_ALT
                    | LexemeId::LOOKUP_U24
                    | LexemeId::RGB
            ) && !(id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56)
        }

        pub struct StandardFormat {
            pub fields: HashMap<u16, &'static str>,
            pub failed_resolve_strategy: FailedResolveStrategy,
        }

        impl StandardFormat {
            pub fn new(fields: HashMap<u16, &'static str>) -> Self {
                Self {
                    fields,
                    failed_resolve_strategy: FailedResolveStrategy::Error,
                }
            }
        }

        impl Default for StandardFormat {
            fn default() -> Self {
                Self::new(HashMap::new())
            }
        }

        impl BinaryTokenFormat for StandardFormat {
            type Token<'a>
                = StandardToken<'a>
            where
                Self: 'a;

            fn next_token<'a>(
                &mut self,
                reader: &'a mut ParserState,
            ) -> Result<TokenResult<Self::Token<'a>>, Error> {
                let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
                    return Ok(TokenResult::MoreData);
                };
                let id = LexemeId::new(u16::from_le_bytes(id_bytes));

                match id {
                    LexemeId::OPEN => {
                        unsafe { reader.consume(2) };
                        Ok(TokenResult::Token(StandardToken::Open))
                    }
                    LexemeId::CLOSE => {
                        unsafe { reader.consume(2) };
                        Ok(TokenResult::Token(StandardToken::Close))
                    }
                    LexemeId::EQUAL => {
                        unsafe { reader.consume(2) };
                        Ok(TokenResult::Token(StandardToken::Equal))
                    }
                    LexemeId::U32 | LexemeId::I32 => {
                        if reader.len() < 6 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<4>()?;

                        if id == LexemeId::I32 {
                            Ok(TokenResult::Token(StandardToken::I32(i32::from_le_bytes(
                                data,
                            ))))
                        } else {
                            Ok(TokenResult::Token(StandardToken::U32(u32::from_le_bytes(
                                data,
                            ))))
                        }
                    }
                    LexemeId::U64 | LexemeId::I64 => {
                        if reader.len() < 10 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<8>()?;

                        if id == LexemeId::I64 {
                            Ok(TokenResult::Token(StandardToken::I64(i64::from_le_bytes(
                                data,
                            ))))
                        } else {
                            Ok(TokenResult::Token(StandardToken::U64(u64::from_le_bytes(
                                data,
                            ))))
                        }
                    }
                    LexemeId::F32 => {
                        if reader.len() < 6 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<4>()?;
                        Ok(TokenResult::Token(StandardToken::F32(data)))
                    }
                    LexemeId::F64 => {
                        if reader.len() < 10 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<8>()?;
                        Ok(TokenResult::Token(StandardToken::F64(data)))
                    }
                    LexemeId::BOOL => {
                        if reader.len() < 3 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<1>()?;
                        Ok(TokenResult::Token(StandardToken::Bool(data[0] != 0)))
                    }
                    LexemeId::QUOTED | LexemeId::UNQUOTED => {
                        let Some(header) = reader.peek_bytes::<4>().copied() else {
                            return Ok(TokenResult::MoreData);
                        };
                        let len = u16::from_le_bytes([header[2], header[3]]);

                        if reader.len() < (len as usize) + 4 {
                            return Ok(TokenResult::MoreData);
                        }

                        unsafe { reader.consume(4) };
                        let data = reader.read_slice(len).ok_or_else(Error::eof)?;
                        if id == LexemeId::QUOTED {
                            Ok(TokenResult::Token(StandardToken::Quoted(data)))
                        } else {
                            Ok(TokenResult::Token(StandardToken::Unquoted(data)))
                        }
                    }
                    LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                        if reader.len() < 3 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<1>()?;
                        Ok(TokenResult::Token(StandardToken::Lookup(LookupIndex::new(
                            data[0] as u32,
                        ))))
                    }
                    LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                        if reader.len() < 4 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<2>()?;
                        Ok(TokenResult::Token(StandardToken::Lookup(LookupIndex::new(
                            u16::from_le_bytes(data) as u32,
                        ))))
                    }
                    LexemeId::LOOKUP_U24 => {
                        if reader.len() < 5 {
                            return Ok(TokenResult::MoreData);
                        }
                        unsafe { reader.consume(2) };
                        let data = reader.read_exact::<3>()?;
                        let value = data[0] as u32 | (data[1] as u32) << 8 | (data[2] as u32) << 16;
                        Ok(TokenResult::Token(StandardToken::Lookup(LookupIndex::new(
                            value,
                        ))))
                    }
                    LexemeId::RGB => {
                        let slice = &reader.buf.as_slice()[2..];
                        if reader.len() < 30 {
                            match read_rgb(slice) {
                                Ok((_, rest)) => {
                                    let consumed = slice.len() - rest.len();
                                    unsafe {
                                        reader.consume(consumed + 2);
                                    }
                                    let data = unsafe {
                                        std::slice::from_raw_parts(
                                            reader.buf.start.byte_sub(consumed),
                                            consumed,
                                        )
                                    };
                                    let (rgb, _) = read_rgb(data).expect("valid rgb data");
                                    Ok(TokenResult::Token(StandardToken::Rgb(rgb)))
                                }
                                Err(_) => Ok(TokenResult::MoreData),
                            }
                        } else {
                            let (_, rest) = read_rgb(slice)
                                .map_err(|e| Error::from(e.at(reader.buf.total_read)))?;
                            let consumed = slice.len() - rest.len();
                            unsafe {
                                reader.consume(consumed + 2);
                            }
                            let data = unsafe {
                                std::slice::from_raw_parts(
                                    reader.buf.start.byte_sub(consumed),
                                    consumed,
                                )
                            };
                            let (rgb, _) = read_rgb(data).expect("valid rgb data");
                            Ok(TokenResult::Token(StandardToken::Rgb(rgb)))
                        }
                    }
                    id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                        let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                        let is_negative = offset > 7;
                        let byte_count = offset - (is_negative as u16 * 7);

                        if byte_count == 0 {
                            unsafe { reader.consume(2) };
                            return Ok(TokenResult::Token(StandardToken::Fixed5(0)));
                        }

                        if reader.len() < byte_count as usize + 2 {
                            return Ok(TokenResult::MoreData);
                        }

                        unsafe { reader.consume(2) };
                        let data = reader.read_slice(byte_count).ok_or_else(Error::eof)?;

                        let mut buf = [0u8; 8];
                        buf[..byte_count as usize].copy_from_slice(data);
                        let sign = 1i64 - (is_negative as i64) * 2;
                        let raw = u64::from_le_bytes(buf) as i64 * sign;
                        Ok(TokenResult::Token(StandardToken::Fixed5(raw)))
                    }
                    id => {
                        unsafe { reader.consume(2) };
                        Ok(TokenResult::Token(StandardToken::Id(FieldId::new(id.0))))
                    }
                }
            }
        }

        impl BinaryValueFormat for StandardFormat {
            fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
                std::str::from_utf8(data)
                    .map(Cow::Borrowed)
                    .map_err(|err| de::Error::custom(err.to_string()))
            }

            #[inline]
            fn deserialize_i32<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<6>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::I32 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = i32::from_le_bytes([data[2], data[3], data[4], data[5]]);
                unsafe { reader.consume(6) };
                Ok(ValueResult::Value(visitor.visit_i32(result)?))
            }

            #[inline]
            fn deserialize_u32<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<6>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::U32 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = u32::from_le_bytes([data[2], data[3], data[4], data[5]]);
                unsafe { reader.consume(6) };
                Ok(ValueResult::Value(visitor.visit_u32(result)?))
            }

            #[inline]
            fn deserialize_i64<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<10>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::I64 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = i64::from_le_bytes([
                    data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9],
                ]);
                unsafe { reader.consume(10) };
                Ok(ValueResult::Value(visitor.visit_i64(result)?))
            }

            #[inline]
            fn deserialize_u64<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<10>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::U64 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = u64::from_le_bytes([
                    data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9],
                ]);
                unsafe { reader.consume(10) };
                Ok(ValueResult::Value(visitor.visit_u64(result)?))
            }

            #[inline]
            fn deserialize_f32<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<6>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::F32 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = f32::from_le_bytes([data[2], data[3], data[4], data[5]]);
                unsafe { reader.consume(6) };
                Ok(ValueResult::Value(visitor.visit_f32(result)?))
            }

            #[inline]
            fn deserialize_f64<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<10>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::F64 {
                    return self.deserialize_any(reader, visitor);
                }
                let result = f64::from_le_bytes([
                    data[2], data[3], data[4], data[5], data[6], data[7], data[8], data[9],
                ]);
                unsafe { reader.consume(10) };
                Ok(ValueResult::Value(visitor.visit_f64(result)?))
            }

            #[inline]
            fn deserialize_bool<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(data) = reader.peek_bytes::<3>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([data[0], data[1]]));
                if id != LexemeId::BOOL {
                    return self.deserialize_any(reader, visitor);
                }
                let result = data[2] != 0;
                unsafe { reader.consume(3) };
                Ok(ValueResult::Value(visitor.visit_bool(result)?))
            }

            #[inline]
            fn deserialize_str<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(header) = reader.peek_bytes::<4>() else {
                    let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
                        return Ok(ValueResult::MoreData(visitor));
                    };
                    let id = LexemeId::new(u16::from_le_bytes(id_bytes));
                    return if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
                        Ok(ValueResult::MoreData(visitor))
                    } else {
                        self.deserialize_any(reader, visitor)
                    };
                };
                let id = LexemeId::new(u16::from_le_bytes([header[0], header[1]]));
                match id {
                    LexemeId::QUOTED | LexemeId::UNQUOTED => {
                        let len = u16::from_le_bytes([header[2], header[3]]);
                        if reader.len() < 4 + len as usize {
                            return self.deserialize_any(reader, visitor);
                        }
                        unsafe { reader.consume(4) };
                        let data = reader.read_slice(len).ok_or_else(Error::eof)?;
                        let value = match self.decode_scalar(data)? {
                            Cow::Borrowed(x) => visitor.visit_str(x)?,
                            Cow::Owned(x) => visitor.visit_string(x)?,
                        };
                        Ok(ValueResult::Value(value))
                    }
                    _ => self.deserialize_any(reader, visitor),
                }
            }

            #[inline]
            fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                let Some(id_bytes) = reader.peek_bytes::<2>() else {
                    return self.deserialize_any(reader, visitor);
                };
                let id = LexemeId::new(u16::from_le_bytes([id_bytes[0], id_bytes[1]]));

                if standard_is_field_id(id) {
                    unsafe { reader.consume(2) };
                    return visit_field_identifier(FieldId::new(id.0), visitor, self);
                }

                if matches!(id, LexemeId::QUOTED | LexemeId::UNQUOTED) {
                    return self.deserialize_str(reader, visitor);
                }

                self.deserialize_any(reader, visitor)
            }

            fn deserialize_any<'de, V: PdxVisitor<'de>>(
                &mut self,
                reader: &mut ParserState,
                visitor: V,
            ) -> VisitResult<'de, V> {
                standard_deserialize_any(self, reader, visitor)
            }
        }

        fn standard_deserialize_any<'de, V: PdxVisitor<'de>>(
            format: &StandardFormat,
            reader: &mut ParserState,
            visitor: V,
        ) -> VisitResult<'de, V> {
            let Some(id_bytes) = reader.peek_bytes::<2>().copied() else {
                return Ok(ValueResult::MoreData(visitor));
            };
            let id = LexemeId::new(u16::from_le_bytes(id_bytes));

            match id {
                LexemeId::OPEN => {
                    unsafe { reader.consume(2) };
                    Ok(ValueResult::Open(visitor))
                }
                LexemeId::I32 => {
                    if reader.len() < 6 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<4>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_i32(i32::from_le_bytes(data))?,
                    ))
                }
                LexemeId::U32 => {
                    if reader.len() < 6 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<4>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_u32(u32::from_le_bytes(data))?,
                    ))
                }
                LexemeId::I64 => {
                    if reader.len() < 10 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<8>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_i64(i64::from_le_bytes(data))?,
                    ))
                }
                LexemeId::U64 => {
                    if reader.len() < 10 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<8>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_u64(u64::from_le_bytes(data))?,
                    ))
                }
                LexemeId::F32 => {
                    if reader.len() < 6 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<4>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_f32(f32::from_le_bytes(data))?,
                    ))
                }
                LexemeId::F64 => {
                    if reader.len() < 10 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<8>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_f64(f64::from_le_bytes(data))?,
                    ))
                }
                LexemeId::BOOL => {
                    if reader.len() < 3 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<1>()?;
                    Ok(ValueResult::Value(visitor.visit_bool(data[0] != 0)?))
                }
                LexemeId::QUOTED | LexemeId::UNQUOTED => {
                    let Some(header) = reader.peek_bytes::<4>().copied() else {
                        return Ok(ValueResult::MoreData(visitor));
                    };
                    let len = u16::from_le_bytes([header[2], header[3]]);
                    if reader.len() < (len as usize) + 4 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(4) };
                    let data = reader.read_slice(len).ok_or_else(Error::eof)?;
                    let value = match format.decode_scalar(data)? {
                        Cow::Borrowed(x) => visitor.visit_str(x)?,
                        Cow::Owned(x) => visitor.visit_string(x)?,
                    };
                    Ok(ValueResult::Value(value))
                }
                LexemeId::RGB => {
                    let slice = reader.buf.as_slice();
                    if reader.len() < 30 {
                        let Ok((rgb, rest)) = read_rgb(slice) else {
                            return Ok(ValueResult::MoreData(visitor));
                        };
                        let consumed = slice.len() - rest.len();
                        unsafe { reader.consume(consumed) };
                        Ok(ValueResult::Value(visitor.visit_rgb(rgb)?))
                    } else {
                        let (rgb, rest) = read_rgb(slice)
                            .map_err(|e| Error::from(e.at(reader.buf.total_read)))?;
                        let consumed = slice.len() - rest.len();
                        unsafe { reader.consume(consumed) };
                        Ok(ValueResult::Value(visitor.visit_rgb(rgb)?))
                    }
                }
                LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
                    if reader.len() < 3 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<1>()?;
                    Ok(ValueResult::Value(visitor.visit_u32(data[0] as u32)?))
                }
                LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                    if reader.len() < 4 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<2>()?;
                    Ok(ValueResult::Value(
                        visitor.visit_u32(u16::from_le_bytes(data) as u32)?,
                    ))
                }
                LexemeId::LOOKUP_U24 => {
                    if reader.len() < 5 {
                        return Ok(ValueResult::MoreData(visitor));
                    }
                    unsafe { reader.consume(2) };
                    let data = reader.read_exact::<3>()?;
                    let val = data[0] as u32 | (data[1] as u32) << 8 | (data[2] as u32) << 16;
                    Ok(ValueResult::Value(visitor.visit_u32(val)?))
                }
                id if id >= LexemeId::FIXED5_ZERO && id <= LexemeId::FIXED5_I56 => {
                    let offset = id.0 - LexemeId::FIXED5_ZERO.0;
                    let is_negative = offset > 7;
                    let byte_count = offset - (is_negative as u16 * 7);

                    if reader.len() < byte_count as usize + 2 {
                        return Ok(ValueResult::MoreData(visitor));
                    }

                    unsafe { reader.consume(2) };

                    if byte_count == 0 {
                        return Ok(ValueResult::Value(visitor.visit_f64(f64::from_bits(0))?));
                    }

                    let data = reader.read_slice(byte_count).ok_or_else(Error::eof)?;
                    let mut buf = [0u8; 8];
                    buf[..byte_count as usize].copy_from_slice(data);
                    let sign = 1i64 - (is_negative as i64) * 2;
                    let i64_val = u64::from_le_bytes(buf) as i64 * sign;
                    Ok(ValueResult::Value(visitor.visit_f64(f64::from_bits(
                        u64::from_le_bytes(i64_val.to_le_bytes()),
                    ))?))
                }
                LexemeId::CLOSE => Err(Error::invalid_syntax(
                    "did not expect end",
                    reader.buf.total_read,
                )),
                LexemeId::EQUAL => Err(Error::invalid_syntax(
                    "did not expect equal",
                    reader.buf.total_read,
                )),
                id if standard_is_field_id(id) => {
                    unsafe { reader.consume(2) };
                    visit_field_identifier(FieldId::new(id.0), visitor, format)
                }
                id => Err(Error::invalid_syntax(
                    format!("unsupported lexeme 0x{:x}", id.0),
                    reader.buf.total_read,
                )),
            }
        }
    }

    #[derive(Debug, PartialEq, serde::Deserialize)]
    struct NestedSyncData {
        nested: InnerSyncData,
        after: i32,
    }

    #[derive(Debug, PartialEq, serde::Deserialize)]
    struct InnerSyncData {
        inner: i32,
    }

    #[derive(Debug, PartialEq, serde::Deserialize)]
    struct SequenceSyncData {
        values: Vec<i32>,
        after: i32,
    }

    #[derive(Default)]
    struct TrackingState {
        waiting_for_close: bool,
        saw_close: bool,
        seen_lexemes: Vec<LexemeId>,
    }

    struct TrackingStandardFormat {
        inner: StandardFormat,
        state: Rc<RefCell<TrackingState>>,
    }

    impl TrackingStandardFormat {
        fn new(state: Rc<RefCell<TrackingState>>, fields: HashMap<u16, &'static str>) -> Self {
            Self {
                inner: StandardFormat::new(fields),
                state,
            }
        }
    }

    impl BinaryTokenFormat for TrackingStandardFormat {
        type Token<'a> = StandardToken<'a>;

        fn next_token<'a>(
            &mut self,
            reader: &'a mut ParserState,
        ) -> Result<TokenResult<Self::Token<'a>>, Error> {
            if let Some(id_bytes) = reader.peek_bytes::<2>().copied() {
                let id = LexemeId::new(u16::from_le_bytes(id_bytes));
                let mut state = self.state.borrow_mut();
                state.seen_lexemes.push(id);
                if id == LexemeId::CLOSE {
                    state.saw_close = true;
                }
            }

            self.inner.next_token(reader)
        }

        fn on_open(&mut self) {
            let mut state = self.state.borrow_mut();
            state.seen_lexemes.push(LexemeId::OPEN);
        }

        fn on_equal(&mut self) {
            let mut state = self.state.borrow_mut();
            state.seen_lexemes.push(LexemeId::EQUAL);
        }

        fn on_close(&mut self) {
            let mut state = self.state.borrow_mut();
            state.seen_lexemes.push(LexemeId::CLOSE);
            state.saw_close = true;
        }

        fn skip_value(
            &mut self,
            state: &mut ParserState,
            fill: &mut impl FnMut(&mut ParserState) -> Result<usize, Error>,
        ) -> Result<(), Error> {
            self.inner.skip_value(state, fill)
        }
    }

    impl BinaryValueFormat for TrackingStandardFormat {
        fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
            self.inner.decode_scalar(data)
        }

        fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
            &mut self,
            reader: &mut ParserState,
            visitor: V,
        ) -> VisitResult<'de, V> {
            if let Some(id_bytes) = reader.peek_bytes::<2>().copied() {
                let id = u16::from_le_bytes(id_bytes);
                if matches!(id, 0x1002 | 0x1101) {
                    let mut state = self.state.borrow_mut();
                    if state.waiting_for_close && !state.saw_close {
                        return Err(Error::custom("stateful close was not synchronized"));
                    }
                    state.waiting_for_close = false;
                    state.saw_close = false;
                }
            }

            self.inner.deserialize_identifier(reader, visitor)
        }

        fn deserialize_any<'de, V: PdxVisitor<'de>>(
            &mut self,
            reader: &mut ParserState,
            visitor: V,
        ) -> VisitResult<'de, V> {
            if let Some(id_bytes) = reader.peek_bytes::<2>().copied() {
                let id = LexemeId::new(u16::from_le_bytes(id_bytes));
                if id == LexemeId::OPEN {
                    let mut state = self.state.borrow_mut();
                    state.waiting_for_close = true;
                    state.saw_close = false;
                }
            }

            self.inner.deserialize_any(reader, visitor)
        }
    }

    fn tracking_fields() -> HashMap<u16, &'static str> {
        HashMap::from([
            (0x1000, "nested"),
            (0x1001, "inner"),
            (0x1002, "after"),
            (0x1100, "values"),
            (0x1101, "after"),
        ])
    }

    fn encode_tokens(tokens: &[Token]) -> Vec<u8> {
        let mut writer = std::io::Cursor::new(Vec::new());
        for token in tokens {
            token.write(&mut writer).unwrap();
        }
        writer.into_inner()
    }

    fn nested_map_sync_fixture() -> Vec<u8> {
        encode_tokens(&[
            Token::Id(0x1000),
            Token::Equal,
            Token::Open,
            Token::Id(0x1001),
            Token::Equal,
            Token::I32(1),
            Token::Close,
            Token::Id(0x1002),
            Token::Equal,
            Token::I32(2),
        ])
    }

    fn nested_sequence_sync_fixture() -> Vec<u8> {
        encode_tokens(&[
            Token::Id(0x1100),
            Token::Equal,
            Token::Open,
            Token::I32(1),
            Token::I32(2),
            Token::Close,
            Token::Id(0x1101),
            Token::Equal,
            Token::I32(3),
        ])
    }

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
    fn test_read_len_prefixed_is_transactional_on_incomplete_payload() {
        let data = [0x04u8, 0x00, b'a', b'b'];
        let mut state = ParserState {
            buf: ParserBuf::from_slice(&data),
            storage: [0u8; 8],
        };

        let mut cursor = state.token_cursor();
        assert_eq!(cursor.read_len_prefixed(), None);
        cursor.consume();

        assert_eq!(state.as_slice(), &data);
        assert_eq!(state.buf.total_read, 0);
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

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        // LOOKUP_U8
        assert_eq!(
            reader.next_token().unwrap(),
            Some(StandardToken::Lookup(LookupIndex::new(255)))
        );

        // LOOKUP_U16
        assert_eq!(
            reader.next_token().unwrap(),
            Some(StandardToken::Lookup(LookupIndex::new(65535)))
        );

        // LOOKUP_U24
        assert_eq!(
            reader.next_token().unwrap(),
            Some(StandardToken::Lookup(LookupIndex::new(16777215)))
        );
    }

    #[test]
    fn test_standard_format_fixed5() {
        // Test FIXED5_ZERO
        let data = vec![0x47, 0x0d]; // FIXED5_ZERO
        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());
        assert_eq!(reader.next_token().unwrap(), Some(StandardToken::Fixed5(0)));

        // Test FIXED5_U8 (e.g., raw value 123 = 0.00123)
        let data = vec![0x48, 0x0d, 0x7B]; // FIXED5_U8, value 123
        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());
        assert_eq!(
            reader.next_token().unwrap(),
            Some(StandardToken::Fixed5(123))
        );

        // Test FIXED5_I8 (negative, e.g., raw value 123 = -0.00123)
        let data = vec![0x4f, 0x0d, 0x7B]; // FIXED5_I8, value -123
        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());
        assert_eq!(
            reader.next_token().unwrap(),
            Some(StandardToken::Fixed5(-123))
        );
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

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());
        let Some(StandardToken::Rgb(rgb)) = reader.next_token().unwrap() else {
            panic!("expected rgb token");
        };
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
        let mut reader = TokenReader::from_slice(data.as_slice(), StandardFormat::default());

        for (i, expected) in input.iter().enumerate() {
            let token = reader.read_token().unwrap();

            match (expected, token) {
                (Token::Open, StandardToken::Open) => {}
                (Token::Close, StandardToken::Close) => {}
                (Token::Equal, StandardToken::Equal) => {}
                (Token::U32(val), StandardToken::U32(actual)) => {
                    assert_eq!(*val, actual, "U32 mismatch at token {}", i);
                }
                (Token::U64(val), StandardToken::U64(actual)) => {
                    assert_eq!(*val, actual, "U64 mismatch at token {}", i);
                }
                (Token::I32(val), StandardToken::I32(actual)) => {
                    assert_eq!(*val, actual, "I32 mismatch at token {}", i);
                }
                (Token::I64(val), StandardToken::I64(actual)) => {
                    assert_eq!(*val, actual, "I64 mismatch at token {}", i);
                }
                (Token::F32(bytes), StandardToken::F32(actual)) => {
                    assert_eq!(*bytes, actual, "F32 mismatch at token {}", i);
                }
                (Token::F64(bytes), StandardToken::F64(actual)) => {
                    assert_eq!(*bytes, actual, "F64 mismatch at token {}", i);
                }
                (Token::Bool(val), StandardToken::Bool(actual)) => {
                    assert_eq!(*val, actual, "Bool mismatch at token {}", i);
                }
                (Token::Quoted(val), StandardToken::Quoted(read)) => {
                    assert_eq!(val.as_bytes(), read, "Quoted mismatch at token {}", i);
                }
                (Token::Unquoted(val), StandardToken::Unquoted(read)) => {
                    assert_eq!(val.as_bytes(), read, "Unquoted mismatch at token {}", i);
                }
                (Token::Rgb(rgb), StandardToken::Rgb(read_rgb)) => {
                    assert_eq!(rgb.r, read_rgb.r, "RGB.r mismatch at token {}", i);
                    assert_eq!(rgb.g, read_rgb.g, "RGB.g mismatch at token {}", i);
                    assert_eq!(rgb.b, read_rgb.b, "RGB.b mismatch at token {}", i);
                    assert_eq!(rgb.a, read_rgb.a, "RGB.a mismatch at token {}", i);
                }
                (Token::Lookup(val), StandardToken::Lookup(actual)) => {
                    assert_eq!(*val, actual.value(), "Lookup mismatch at token {}", i);
                }
                (Token::Id(val), StandardToken::Id(actual)) => {
                    assert_eq!(*val, actual.value(), "Id mismatch at token {}", i);
                }
                (expected, token) => {
                    panic!(
                        "Token mismatch at index {}: expected {:?}, got {:?}",
                        i, expected, token
                    );
                }
            }
        }

        // Verify we've consumed all data
        assert_eq!(reader.position(), data.len());

        // Attempting to read more should error
        let result = reader.next_token();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), None);
    }

    #[test]
    fn test_not_enough_data() {
        let mut reader = TokenReader::from_slice(&[0x43], StandardFormat::default());
        let result = reader.read_token();
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
        let mut reader = TokenReader::from_slice(data, StandardFormat::default());
        let result = reader.read_token();
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

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        assert_eq!(reader.read_token().unwrap(), StandardToken::Bool(true));

        assert_eq!(reader.read_token().unwrap(), StandardToken::Bool(false));
    }

    #[test]
    fn test_position_tracking() {
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Id(0x1234).write(&mut writer).unwrap();
        Token::Equal.write(&mut writer).unwrap();
        Token::U32(42).write(&mut writer).unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        assert_eq!(reader.position(), 0);

        let _ = reader.read_token().unwrap(); // Id
        assert_eq!(reader.position(), 2); // 2 bytes for Id

        let _ = reader.read_token().unwrap(); // Equal
        assert_eq!(reader.position(), 4); // +2 bytes for Equal

        let _ = reader.read_token().unwrap(); // U32
        assert_eq!(reader.position(), 10); // +2 for lexeme + 4 for u32 = 6 more
    }

    #[test]
    fn test_buffer_boundaries() {
        // Test very small string (1 byte)
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        Token::Quoted(Scalar::new(b"x")).write(&mut writer).unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        assert_eq!(reader.read_token().unwrap(), StandardToken::Quoted(b"x"));

        // Test larger string
        let data = Vec::new();
        let mut writer = std::io::Cursor::new(data);
        let long_string = "a".repeat(255);
        Token::Unquoted(Scalar::new(long_string.as_bytes()))
            .write(&mut writer)
            .unwrap();
        let data = writer.into_inner();

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        assert_eq!(
            reader.read_token().unwrap(),
            StandardToken::Unquoted(long_string.as_bytes())
        );
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

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        let StandardToken::Rgb(rgb) = reader.read_token().unwrap() else {
            panic!("expected rgb token");
        };
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

        let mut reader = TokenReader::from_slice(&data, StandardFormat::default());

        let StandardToken::Rgb(rgb) = reader.read_token().unwrap() else {
            panic!("expected rgb token");
        };
        assert_eq!(rgb.r, 110);
        assert_eq!(rgb.g, 28);
        assert_eq!(rgb.b, 27);
        assert_eq!(rgb.a, Some(128));
    }

    #[test]
    fn map_access_consumes_nested_close_via_next_token() {
        let state = Rc::new(RefCell::new(TrackingState::default()));
        let data = nested_map_sync_fixture();

        let actual: NestedSyncData =
            from_slice(&data, TrackingStandardFormat::new(state.clone(), tracking_fields())).unwrap();

        assert_eq!(
            actual,
            NestedSyncData {
                nested: InnerSyncData { inner: 1 },
                after: 2,
            }
        );
        assert!(state.borrow().seen_lexemes.contains(&LexemeId::CLOSE));
    }

    #[test]
    fn seq_access_consumes_nested_close_via_next_token() {
        let state = Rc::new(RefCell::new(TrackingState::default()));
        let data = nested_sequence_sync_fixture();

        let actual: SequenceSyncData =
            from_slice(&data, TrackingStandardFormat::new(state.clone(), tracking_fields())).unwrap();

        assert_eq!(
            actual,
            SequenceSyncData {
                values: vec![1, 2],
                after: 3,
            }
        );
        assert!(state.borrow().seen_lexemes.contains(&LexemeId::CLOSE));
    }
}
