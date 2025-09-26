use super::{
    lexer::{LexemeId, Lexer},
    LexError, Token, TokenReader, TokenReaderBuilder,
};
use crate::{
    binary::{BinaryFlavor, FailedResolveStrategy, TokenResolver},
    de::ColorSequence,
    DeserializeError, DeserializeErrorKind, Error,
};
use serde::de::{
    self, Deserialize, DeserializeOwned, DeserializeSeed, MapAccess, SeqAccess, Visitor,
};
use std::{borrow::Cow, io::Read};

/// Serde deserializer over a streaming binary reader
pub struct BinaryReaderDeserializer<'res, RES, F, R> {
    reader: TokenReader<R>,
    config: BinaryConfig<'res, RES, F>,
}

impl<RES: TokenResolver, E: BinaryFlavor, R: Read> BinaryReaderDeserializer<'_, RES, E, R> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> de::Deserializer<'de>
    for &'_ mut BinaryReaderDeserializer<'res, RES, F, R>
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
        let me = std::ptr::addr_of!(self);
        visitor.visit_map(BinaryReaderMap::new(me, true))
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

struct BinaryReaderMap<'a: 'a, 'res, RES: 'a, F, R> {
    de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
    root: bool,
}

impl<'a, 'res, RES: 'a, F, R> BinaryReaderMap<'a, 'res, RES, F, R> {
    fn new(de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>, root: bool) -> Self {
        BinaryReaderMap { de, root }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> MapAccess<'de>
    for BinaryReaderMap<'_, 'res, RES, F, R>
{
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match unsafe { self.de.read() }.reader.next() {
                Ok(Some(Token::Close)) => return Ok(None),
                Ok(Some(Token::Open)) => {
                    let _ = unsafe { self.de.read() }.reader.read();
                }
                Ok(Some(token)) => {
                    return seed
                        .deserialize(BinaryReaderTokenDeserializer { de: self.de, token })
                        .map(Some)
                }
                Ok(None) if self.root => return Ok(None),
                Ok(None) => {
                    return Err(LexError::Eof
                        .at(unsafe { self.de.read() }.reader.position())
                        .into())
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
        let mut token = unsafe { self.de.read() }.reader.read()?;
        if matches!(token, Token::Equal) {
            token = unsafe { self.de.read() }.reader.read()?;
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de, token })
    }
}

struct BinaryReaderTokenDeserializer<'a, 'res, RES: 'a, F, R> {
    de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
    token: Token<'a>,
}

impl<'res, RES: TokenResolver, F, R> BinaryReaderTokenDeserializer<'_, 'res, RES, F, R>
where
    F: BinaryFlavor,
    R: Read,
{
    #[inline]
    fn deser<'de, V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
        'res: 'de,
    {
        match self.token {
            Token::U32(x) => visitor.visit_u32(x),
            Token::U64(x) => visitor.visit_u64(x),
            Token::I32(x) => visitor.visit_i32(x),
            Token::Bool(x) => visitor.visit_bool(x),
            Token::Quoted(x) | Token::Unquoted(x) => {
                match unsafe { self.de.read() }.config.flavor.decode(x.as_bytes()) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            Token::F32(x) => {
                visitor.visit_f32(unsafe { self.de.read() }.config.flavor.visit_f32(x))
            }
            Token::F64(x) => {
                visitor.visit_f64(unsafe { self.de.read() }.config.flavor.visit_f64(x))
            }
            Token::Rgb(x) => visitor.visit_seq(ColorSequence::new(x)),
            Token::I64(x) => visitor.visit_i64(x),
            Token::Id(s) => match unsafe { self.de.read() }.config.resolver.resolve(s) {
                Some(id) => visitor.visit_borrowed_str(id),
                None => match unsafe { self.de.read() }.config.failed_resolve_strategy {
                    FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::UnknownToken { token_id: s },
                    })),
                    FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{:x}", s)),
                    FailedResolveStrategy::Ignore => {
                        visitor.visit_borrowed_str("__internal_identifier_ignore")
                    }
                },
            },
            Token::Close => Err(Error::invalid_syntax(
                "did not expect end",
                unsafe { self.de.read() }.reader.position(),
            )),
            Token::Equal => Err(Error::invalid_syntax(
                "did not expect equal",
                unsafe { self.de.read() }.reader.position(),
            )),
            Token::Open => visitor.visit_seq(BinaryReaderSeq::new(self.de)),
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

impl<'a, 'de: 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> de::Deserializer<'de>
    for BinaryReaderTokenDeserializer<'a, 'res, RES, F, R>
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
        match self.token {
            Token::Quoted(x) | Token::Unquoted(x) => visitor.visit_bytes(x.as_bytes()),
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
        if let Token::Bool(x) = &self.token {
            visitor.visit_bool(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::Id(x) = &self.token {
            visitor.visit_u16(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::I32(x) = &self.token {
            visitor.visit_i32(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::U32(x) = &self.token {
            visitor.visit_u32(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::U64(x) = &self.token {
            visitor.visit_u64(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::I64(x) = &self.token {
            visitor.visit_i64(*x)
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::F32(x) = &self.token {
            visitor.visit_f32(unsafe { self.de.read() }.config.flavor.visit_f32(*x))
        } else {
            self.deser(visitor)
        }
    }

    #[inline]
    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Token::F64(x) = &self.token {
            visitor.visit_f64(unsafe { self.de.read() }.config.flavor.visit_f64(*x))
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
        match self.token {
            Token::Quoted(x) | Token::Unquoted(x) => {
                match unsafe { self.de.read() }.config.flavor.decode(x.as_bytes()) {
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
        match self.token {
            Token::Open => {
                let mut seq = BinaryReaderSeq::new(self.de);
                let result = visitor.visit_seq(&mut seq)?;
                if !seq.hit_end {
                    // For when we are deserializing an array that doesn't read
                    // the closing token
                    if !matches!(unsafe { self.de.read() }.reader.read()?, Token::Close) {
                        return Err(Error::invalid_syntax(
                            "Expected sequence to be terminated with an end token",
                            unsafe { self.de.read() }.reader.position(),
                        ));
                    }
                }
                Ok(result)
            }
            Token::Rgb(x) => visitor.visit_seq(ColorSequence::new(x)),
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
        if matches!(self.token, Token::Open) {
            visitor.visit_map(BinaryReaderMap::new(self.de, false))
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
        visitor.visit_enum(BinaryReaderEnum::new(self.de, self.token))
    }

    #[inline]
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if matches!(self.token, Token::Open) {
            unsafe { self.de.read() }.reader.skip_container()?;
        }

        visitor.visit_unit()
    }
}

struct BinaryReaderSeq<'a: 'a, 'res, RES: 'a, F, R> {
    de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
    hit_end: bool,
}

impl<'a, 'res, RES: 'a, F, R> BinaryReaderSeq<'a, 'res, RES, F, R> {
    fn new(de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>) -> Self {
        BinaryReaderSeq { de, hit_end: false }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> SeqAccess<'de>
    for BinaryReaderSeq<'_, 'res, RES, F, R>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let mut token = unsafe { self.de.read() }.reader.read()?;
        if matches!(token, Token::Close) {
            self.hit_end = true;
            return Ok(None);
        } else if matches!(token, Token::Equal) {
            // This is a standalone Equal token from object template syntax
            token = unsafe { self.de.read() }.reader.read()?;
        }

        seed.deserialize(BinaryReaderTokenDeserializer { de: self.de, token })
            .map(Some)
    }
}

struct BinaryReaderEnum<'a, 'res, RES: 'a, F, R> {
    de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>,
    token: Token<'a>,
}

impl<'a, 'res, RES: 'a, F, R> BinaryReaderEnum<'a, 'res, RES, F, R> {
    fn new(de: *const &'a mut BinaryReaderDeserializer<'res, RES, F, R>, token: Token<'a>) -> Self {
        BinaryReaderEnum { de, token }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R: Read> de::EnumAccess<'de>
    for BinaryReaderEnum<'_, 'res, RES, F, R>
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(BinaryReaderTokenDeserializer {
            de: self.de,
            token: self.token,
        })?;
        Ok((variant, self))
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, R> de::VariantAccess<'de>
    for BinaryReaderEnum<'_, 'res, RES, F, R>
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

/// On-demand binary deserializer
pub struct OndemandBinaryDeserializer<'data, 'res: 'data, RES, F> {
    parser: Lexer<'data>,
    config: BinaryConfig<'res, RES, F>,
}

impl<'de, RES: TokenResolver, E: BinaryFlavor> OndemandBinaryDeserializer<'de, '_, RES, E> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: Deserialize<'de>,
    {
        T::deserialize(self)
    }
}

impl<'de, RES: TokenResolver, F: BinaryFlavor> de::Deserializer<'de>
    for &'_ mut OndemandBinaryDeserializer<'de, '_, RES, F>
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
        visitor.visit_map(OndemandMap::new(self, true))
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

struct OndemandMap<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    root: bool,
}

impl<'a, 'de: 'a, 'res: 'de, RES: 'a, F> OndemandMap<'a, 'de, 'res, RES, F> {
    fn new(de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>, root: bool) -> Self {
        OndemandMap { de, root }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> MapAccess<'de>
    for OndemandMap<'_, 'de, 'res, RES, F>
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match self.de.parser.read_id() {
                Ok(LexemeId::CLOSE) => return Ok(None),
                Ok(LexemeId::OPEN) => {
                    let _ = self.de.parser.read_id()?;
                }
                Ok(token) => {
                    return seed
                        .deserialize(OndemandTokenDeserializer {
                            de: &mut *self.de,
                            token,
                        })
                        .map(Some)
                }
                Err(e) => {
                    return match e.kind() {
                        LexError::Eof if self.root => Ok(None),
                        _ => Err(e.into()),
                    }
                }
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let mut token = self.de.parser.read_id()?;
        if token == LexemeId::EQUAL {
            token = self.de.parser.read_id()?;
        }

        seed.deserialize(OndemandTokenDeserializer {
            de: &mut *self.de,
            token,
        })
    }
}

struct OndemandTokenDeserializer<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    token: LexemeId,
}

impl<'a, 'de: 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor>
    OndemandTokenDeserializer<'a, 'de, 'res, RES, F>
{
    fn deser<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        match self.token {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let data = self.de.parser.read_string()?;
                match self.de.config.flavor.decode(data.as_bytes()) {
                    Cow::Borrowed(x) => visitor.visit_borrowed_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            LexemeId::U32 => visitor.visit_u32(self.de.parser.read_u32()?),
            LexemeId::I32 => visitor.visit_i32(self.de.parser.read_i32()?),
            LexemeId::U64 => visitor.visit_u64(self.de.parser.read_u64()?),
            LexemeId::I64 => visitor.visit_i64(self.de.parser.read_i64()?),
            LexemeId::BOOL => visitor.visit_bool(self.de.parser.read_bool()?),
            LexemeId::F32 => {
                visitor.visit_f32(self.de.config.flavor.visit_f32(self.de.parser.read_f32()?))
            }
            LexemeId::F64 => {
                visitor.visit_f64(self.de.config.flavor.visit_f64(self.de.parser.read_f64()?))
            }
            LexemeId::OPEN => visitor.visit_seq(OndemandSeq::new(self.de)),
            LexemeId::CLOSE | LexemeId::EQUAL => Err(Error::invalid_syntax(
                "unexpected token encountered",
                self.de.parser.position(),
            )),
            LexemeId(s) => match self.de.config.resolver.resolve(s) {
                Some(id) => visitor.visit_borrowed_str(id),
                None => match self.de.config.failed_resolve_strategy {
                    FailedResolveStrategy::Error => Err(Error::from(DeserializeError {
                        kind: DeserializeErrorKind::UnknownToken { token_id: s },
                    })),
                    FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{:x}", s)),
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
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: de::Visitor<'de>,
        {
            self.deser(visitor)
        }
    };
}

impl<'a, 'de: 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> de::Deserializer<'de>
    for OndemandTokenDeserializer<'a, 'de, 'res, RES, F>
{
    type Error = Error;

    deserialize_scalar!(deserialize_any);
    deserialize_scalar!(deserialize_i8);
    deserialize_scalar!(deserialize_i16);
    deserialize_scalar!(deserialize_u8);
    deserialize_scalar!(deserialize_char);
    deserialize_scalar!(deserialize_identifier);

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.token {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let data = self.de.parser.read_string()?;
                visitor.visit_borrowed_bytes(data.as_bytes())
            }
            _ => self.deser(visitor),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::BOOL {
            visitor.visit_bool(self.de.parser.read_bool()?)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token.is_id() {
            let LexemeId(x) = self.token;
            visitor.visit_u16(x)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::I32 {
            visitor.visit_i32(self.de.parser.read_i32()?)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::U32 {
            visitor.visit_u32(self.de.parser.read_u32()?)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::U64 {
            visitor.visit_u64(self.de.parser.read_u64()?)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::I64 {
            visitor.visit_i64(self.de.parser.read_i64()?)
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::F32 {
            visitor.visit_f32(self.de.config.flavor.visit_f32(self.de.parser.read_f32()?))
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::F64 {
            visitor.visit_f64(self.de.config.flavor.visit_f64(self.de.parser.read_f64()?))
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_string(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::QUOTED || self.token == LexemeId::UNQUOTED {
            let data = self.de.parser.read_string()?;
            match self.de.config.flavor.decode(data.as_bytes()) {
                Cow::Borrowed(x) => visitor.visit_borrowed_str(x),
                Cow::Owned(x) => visitor.visit_string(x),
            }
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

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

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::OPEN {
            let mut seq = OndemandSeq::new(self.de);
            let result = visitor.visit_seq(&mut seq)?;
            if !seq.hit_end {
                // For when we are deserializing an array that doesn't read
                // the closing token
                let ender = self.de.parser.read_id()?;
                if ender != LexemeId::CLOSE {
                    return Err(Error::invalid_syntax(
                        "Expected sequence to be terminated with an end token",
                        self.de.parser.position(),
                    ));
                }
            }
            Ok(result)
        } else if self.token == LexemeId::RGB {
            let rgb = self.de.parser.read_rgb()?;
            visitor.visit_seq(ColorSequence::new(rgb))
        } else {
            self.deser(visitor)
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

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

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == LexemeId::OPEN {
            visitor.visit_map(OndemandMap::new(self.de, false))
        } else {
            self.deser(visitor)
        }
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

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(OndemandEnum::new(self.de, self.token))
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.parser.skip_value(self.token)?;
        visitor.visit_unit()
    }
}

struct OndemandSeq<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    hit_end: bool,
}

impl<'a, 'de: 'a, 'res: 'de, RES: 'a, F> OndemandSeq<'a, 'de, 'res, RES, F> {
    fn new(de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>) -> Self {
        OndemandSeq { de, hit_end: false }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> SeqAccess<'de>
    for OndemandSeq<'_, 'de, 'res, RES, F>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let mut token = self.de.parser.read_id()?;
        if matches!(token, LexemeId::CLOSE) {
            self.hit_end = true;
            return Ok(None);
        } else if matches!(token, LexemeId::EQUAL) {
            token = self.de.parser.read_id()?;
        }

        seed.deserialize(OndemandTokenDeserializer {
            de: &mut *self.de,
            token,
        })
        .map(Some)
    }
}

struct OndemandEnum<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    token: LexemeId,
}

impl<'a, 'de: 'a, 'res: 'de, RES: 'a, F> OndemandEnum<'a, 'de, 'res, RES, F> {
    fn new(de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>, token: LexemeId) -> Self {
        OndemandEnum { de, token }
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> de::EnumAccess<'de>
    for OndemandEnum<'_, 'de, 'res, RES, F>
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(OndemandTokenDeserializer {
            de: self.de,
            token: self.token,
        })?;
        Ok((variant, self))
    }
}

impl<'de, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> de::VariantAccess<'de>
    for OndemandEnum<'_, 'de, 'res, RES, F>
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

/// A structure to deserialize binary data into Rust values.
///
/// By default, if a token is unable to be resolved then it will be ignored by
/// the default. Construct a custom instance through the `builder` method to
/// tweak this behavior.
///
/// The example below demonstrates how to deserialize data
///
/// ```
/// # #[cfg(feature = "derive")] {
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use jomini::{Encoding, JominiDeserialize, Windows1252Encoding, BinaryDeserializer};
/// use serde::Deserialize;
/// use std::{borrow::Cow, collections::HashMap};
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructA {
///   field1: String,
///   field2: i32
/// }
///
/// #[derive(Debug, Default)]
/// pub struct BinaryTestFlavor;
///
/// impl jomini::binary::BinaryFlavor for BinaryTestFlavor {
///     fn visit_f32(&self, data: [u8; 4]) -> f32 {
///         f32::from_le_bytes(data)
///     }
///
///     fn visit_f64(&self, data: [u8; 8]) -> f64 {
///         f64::from_le_bytes(data)
///     }
/// }
///
/// impl Encoding for BinaryTestFlavor {
///     fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
///         Windows1252Encoding::decode(data)
///     }
/// }
///
/// let data = [
///    0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
///    0x83, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x59, 0x00, 0x00, 0x00,
/// ];
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, String::from("field1"));
/// map.insert(0x2d83, String::from("field2"));
///
/// let builder = BinaryDeserializer::builder_flavor(BinaryTestFlavor);
/// let mut deserializer = builder.from_slice(&data[..], &map);
/// let a: StructA = deserializer.deserialize()?;
/// assert_eq!(a, StructA {
///   field1: "ENG".to_string(),
///   field2: 89,
/// });
///
/// # Ok(())
/// # }
/// # }
/// ```
///
/// Build a tweaked binary deserializer
#[derive(Debug)]
pub struct BinaryDeserializerBuilder<F> {
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
    reader_config: TokenReaderBuilder,
}

/// Type alias for the binary deserializer builder for backward compatibility
pub type BinaryDeserializer = BinaryDeserializerBuilder<()>;

impl BinaryDeserializer {
    /// A convenience method to create a binary deserializer with the given flavor
    pub fn builder_flavor<F>(flavor: F) -> BinaryDeserializerBuilder<F>
    where
        F: BinaryFlavor,
    {
        BinaryDeserializerBuilder::with_flavor(flavor)
    }
}

impl<F> BinaryDeserializerBuilder<F>
where
    F: BinaryFlavor,
{
    /// Create a new builder instance
    pub fn with_flavor(flavor: F) -> Self {
        BinaryDeserializerBuilder {
            failed_resolve_strategy: FailedResolveStrategy::Ignore,
            flavor,
            reader_config: TokenReaderBuilder::default(),
        }
    }

    /// Set the behavior when a unknown token is encountered
    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.failed_resolve_strategy = strategy;
        self
    }

    /// Set the reader buffer config (unused for slice deserializations)
    pub fn reader_config(&mut self, val: TokenReaderBuilder) -> &mut Self {
        self.reader_config = val;
        self
    }

    /// Create binary deserializer from reader
    pub fn from_reader<RES, R>(
        self,
        reader: R,
        resolver: &RES,
    ) -> BinaryReaderDeserializer<'_, RES, F, R>
    where
        RES: TokenResolver,
    {
        let reader = self.reader_config.build(reader);
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
            flavor: self.flavor,
        };

        BinaryReaderDeserializer { reader, config }
    }

    /// Deserialize value from reader
    pub fn deserialize_reader<RES, T, R: Read>(self, reader: R, resolver: &RES) -> Result<T, Error>
    where
        T: DeserializeOwned,
        RES: TokenResolver,
    {
        self.from_reader(reader, resolver).deserialize()
    }

    /// Create a binary deserializer from a slice
    pub fn from_slice<'a, 'res: 'a, RES>(
        self,
        data: &'a [u8],
        resolver: &'res RES,
    ) -> OndemandBinaryDeserializer<'a, 'res, RES, F>
    where
        RES: TokenResolver,
    {
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
            flavor: self.flavor,
        };

        OndemandBinaryDeserializer {
            parser: Lexer::new(data),
            config,
        }
    }

    /// Deserialize value from slice
    pub fn deserialize_slice<'data, 'res: 'data, RES, T>(
        self,
        data: &'data [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'data>,
        RES: TokenResolver,
    {
        self.from_slice(data, resolver).deserialize()
    }
}

struct BinaryConfig<'res, RES, F> {
    resolver: &'res RES,
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
}

#[cfg(all(test, feature = "derive"))]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour};
    use crate::{Encoding, Scalar, Windows1252Encoding};
    use jomini_derive::JominiDeserialize;
    use serde::{de::Deserializer, Deserialize};
    use std::{collections::HashMap, fmt, marker::PhantomData};

    /// The eu4 binary flavor
    #[derive(Debug, Default)]
    pub struct Eu4Flavor(Windows1252Encoding);

    impl Eu4Flavor {
        /// Creates a new eu4 flavor
        pub fn new() -> Self {
            Eu4Flavor(Windows1252Encoding::new())
        }
    }

    impl Encoding for Eu4Flavor {
        fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
            self.0.decode(data)
        }
    }

    impl BinaryFlavor for Eu4Flavor {
        fn visit_f32(&self, data: [u8; 4]) -> f32 {
            // First encoding is an i32 that has a fixed point offset of 3 decimal digits
            i32::from_le_bytes(data) as f32 / 1000.0
        }

        fn visit_f64(&self, data: [u8; 8]) -> f64 {
            // Second encoding is Q49.15 with 5 fractional digits
            // https://en.wikipedia.org/wiki/Q_(number_format)
            let val = i64::from_le_bytes(data) as f64 / 32768.0;
            (val * 10_0000.0).round() / 10_0000.0
        }
    }

    fn eu4_builder() -> BinaryDeserializerBuilder<Eu4Flavor> {
        BinaryDeserializerBuilder::with_flavor(Eu4Flavor::new())
    }

    fn from_slice<'a, 'res: 'a, RES, T>(data: &'a [u8], resolver: &'res RES) -> Result<T, Error>
    where
        T: Deserialize<'a> + PartialEq + std::fmt::Debug,
        RES: TokenResolver,
    {
        eu4_builder().deserialize_slice(data, resolver)
    }

    fn from_owned<'a, 'res: 'a, RES, T>(data: &'a [u8], resolver: &'res RES) -> Result<T, Error>
    where
        T: DeserializeOwned + PartialEq + std::fmt::Debug,
        RES: TokenResolver,
    {
        let res = from_slice(data, resolver).unwrap();
        let reader: T = eu4_builder().deserialize_reader(data, resolver).unwrap();
        assert_eq!(reader, res);
        Ok(res)
    }

    #[test]
    fn test_single_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "ENG".to_string()
            }
        );
    }

    #[test]
    fn test_borrowed_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: &'a str,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: "ENG" });
    }

    #[test]
    fn test_cow_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: Cow<'a, str>,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Cow::Borrowed("ENG"),
            }
        );
    }

    #[test]
    fn test_false_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));
        map.insert(0x284c, String::from("no"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "no".to_string()
            }
        );
    }

    #[test]
    fn test_i32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x59, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 89 });
    }

    #[test]
    fn test_u32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 89 });
    }

    #[test]
    fn test_u64_event() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let mut map = HashMap::new();
        map.insert(0x326b, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 128 });
    }

    #[test]
    fn test_i64_event() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x17, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
        }

        let mut map = HashMap::new();
        map.insert(0x326b, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: -1 });
    }

    #[test]
    fn test_f32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0d, 0x00, 0x17, 0x00, 0x00, 0x00];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 0.023 });
    }

    #[test]
    fn test_q16_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x67, 0x01, 0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 1.78732 });
    }

    #[test]
    fn test_string1_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_string2_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_date_field() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0xe0, 0x47, 0x5c, 0x03];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Date,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Date::from_ymd(1436, 1, 1)
            }
        );
    }

    #[test]
    fn test_datehour_field() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x4b, 0x1d, 0x9f, 0x03];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: DateHour,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: DateHour::from_ymdh(1936, 1, 1, 12)
            }
        );
    }

    #[test]
    fn test_token_visit() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(token = 0x2d82)]
            field1: String,
        }

        struct NullResolver;
        impl TokenResolver for NullResolver {
            fn resolve(&self, _token: u16) -> Option<&str> {
                None
            }
        }

        let actual: MyStruct = from_owned(&data[..], &NullResolver).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_multiple_top_level_events() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: String,
            field2: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, String::from("field1"));
        map.insert(0x284d, String::from("field2"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: String::from("no"),
                field2: String::from("yes"),
            }
        );
    }

    #[test]
    fn test_string_array() {
        let data = [
            0xe1, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x0a, 0x00, 0x41, 0x72, 0x74, 0x20,
            0x6f, 0x66, 0x20, 0x57, 0x61, 0x72, 0x0f, 0x00, 0x14, 0x00, 0x43, 0x6f, 0x6e, 0x71,
            0x75, 0x65, 0x73, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x50, 0x61, 0x72, 0x61, 0x64, 0x69,
            0x73, 0x65, 0x0f, 0x00, 0x0b, 0x00, 0x52, 0x65, 0x73, 0x20, 0x50, 0x75, 0x62, 0x6c,
            0x69, 0x63, 0x61, 0x0f, 0x00, 0x11, 0x00, 0x57, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x20,
            0x6f, 0x66, 0x20, 0x4e, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            dlc_enabled: Vec<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x2ee1, String::from("dlc_enabled"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                dlc_enabled: vec![
                    String::from("Art of War"),
                    String::from("Conquest of Paradise"),
                    String::from("Res Publica"),
                    String::from("Wealth of Nations"),
                ],
            }
        );
    }

    #[test]
    fn test_array_deserialization() {
        let data = [
            0xe1, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x0a, 0x00, 0x41, 0x72, 0x74, 0x20,
            0x6f, 0x66, 0x20, 0x57, 0x61, 0x72, 0x0f, 0x00, 0x14, 0x00, 0x43, 0x6f, 0x6e, 0x71,
            0x75, 0x65, 0x73, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x50, 0x61, 0x72, 0x61, 0x64, 0x69,
            0x73, 0x65, 0x0f, 0x00, 0x0b, 0x00, 0x52, 0x65, 0x73, 0x20, 0x50, 0x75, 0x62, 0x6c,
            0x69, 0x63, 0x61, 0x0f, 0x00, 0x11, 0x00, 0x57, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x20,
            0x6f, 0x66, 0x20, 0x4e, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x04, 0x00, 0x82, 0x2d,
            0x01, 0x00, 0x0e, 0x00, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            dlc_enabled: [String; 4],
            field1: bool,
        }

        let mut map = HashMap::new();
        map.insert(0x2ee1, String::from("dlc_enabled"));
        map.insert(0x2d82, String::from("field1"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                dlc_enabled: [
                    String::from("Art of War"),
                    String::from("Conquest of Paradise"),
                    String::from("Res Publica"),
                    String::from("Wealth of Nations"),
                ],
                field1: false,
            }
        );
    }

    #[test]
    fn test_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00, 0x00, 0x00, 0xc7, 0x2e,
            0x01, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x00, 0x00, 0xc8, 0x2e, 0x01, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
            second: i32,
            third: i32,
            fourth: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2ec9, String::from("savegame_version"));
        map.insert(0x28e2, String::from("first"));
        map.insert(0x28e3, String::from("second"));
        map.insert(0x2ec7, String::from("third"));
        map.insert(0x2ec8, String::from("fourth"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version {
                    first: 1,
                    second: 11,
                    third: 4,
                    fourth: 0,
                }
            }
        );
    }

    #[test]
    fn test_empty_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x03, 0x00, 0x04, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00,
            0x00, 0x00, 0xc7, 0x2e, 0x01, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x00, 0x00, 0xc8, 0x2e,
            0x01, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
            second: i32,
            third: i32,
            fourth: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2ec9, String::from("savegame_version"));
        map.insert(0x28e2, String::from("first"));
        map.insert(0x28e3, String::from("second"));
        map.insert(0x2ec7, String::from("third"));
        map.insert(0x2ec8, String::from("fourth"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version {
                    first: 1,
                    second: 11,
                    third: 4,
                    fourth: 0,
                }
            }
        );
    }

    #[test]
    fn test_trailing_empty_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x03, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2ec9, String::from("savegame_version"));
        map.insert(0x28e2, String::from("first"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version { first: 1 }
            }
        );
    }

    #[test]
    fn test_numerical_identifiers() {
        let data = [
            0x0c, 0x00, 0x59, 0x00, 0x00, 0x00, 0x01, 0x00, 0x0c, 0x00, 0x1e, 0x00, 0x00, 0x00,
        ];

        let map: HashMap<u16, String> = HashMap::new();
        let actual: HashMap<i32, i32> = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual.len(), 1);
        assert_eq!(actual.get(&89), Some(&30));
    }

    #[test]
    fn test_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x0b, 0x00]);
        data.extend_from_slice(b"1444.11.11\n");
        data.extend_from_slice(&0x0004u16.to_le_bytes());

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            flags: HashMap<String, String>,
        }

        let mut map = HashMap::new();
        map.insert(0x29cc, String::from("flags"));

        let mut expected_map = HashMap::new();
        expected_map.insert(
            String::from("schools_initiated"),
            String::from("1444.11.11"),
        );

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                flags: expected_map
            }
        )
    }

    #[test]
    fn test_escaped_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x16, 0x00]);
        data.extend_from_slice(br#"Joe \"Captain\" Rogers"#);
        data.extend_from_slice(&0x0004u16.to_le_bytes());

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            flags: HashMap<String, String>,
        }

        let mut map = HashMap::new();
        map.insert(0x29cc, String::from("flags"));

        let mut expected_map = HashMap::new();
        expected_map.insert(
            String::from("schools_initiated"),
            String::from(r#"Joe "Captain" Rogers"#),
        );

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                flags: expected_map
            }
        )
    }

    #[test]
    fn test_no_equal_object() {
        let data = [
            0xf1, 0x36, 0x03, 0x00, 0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            map_area_data: MapData,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MapData {
            #[serde(alias = "type")]
            _type: String,
        }

        let mut map = HashMap::new();
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x00e1, String::from("type"));
        map.insert(0x28be, String::from("general"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                map_area_data: MapData {
                    _type: String::from("general"),
                }
            }
        );
    }

    #[test]
    fn test_empty_array() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(alias = "type")]
            _type: Vec<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x00e1, String::from("type"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { _type: vec![] });
    }

    #[test]
    fn test_array_of_objects() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4c, 0x28, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            technology_group: Vec<Data>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Data {
            map_area_data: String,
        }

        let mut map = HashMap::new();
        map.insert(0x2863, String::from("technology_group"));
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                technology_group: vec![
                    Data {
                        map_area_data: String::from("no")
                    },
                    Data {
                        map_area_data: String::from("yes")
                    }
                ]
            }
        );
    }

    #[test]
    fn test_skip_unwanted() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28, 0x04, 0x00, 0x04, 0x00,
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x326b, String::from("field1"));
        map.insert(0x2863, String::from("technology_group"));
        map.insert(0x36f1, String::from("map_area_data"));
        map.insert(0x284c, String::from("yes"));
        map.insert(0x284b, String::from("no"));

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { field1: 128 });
    }

    #[test]
    fn test_consecutive_fields() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x14, 0x00, 0x5a, 0x00, 0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00,
            0x00, 0x00,
        ];

        #[derive(JominiDeserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            field1: Vec<u32>,
            second: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x28e3, "second");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: vec![89, 90],
                second: 11
            }
        );
    }

    #[test]
    fn test_error_unresolved_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let map: HashMap<u16, String> = HashMap::new();
        let mut builder = eu4_builder();
        builder.on_failed_resolve(FailedResolveStrategy::Error);
        let actual: Result<MyStruct, _> = builder.from_slice(&data[..], &map).deserialize();
        assert!(actual.is_err());
    }

    #[test]
    fn test_stringify_unresolved_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let map: HashMap<u16, String> = HashMap::new();
        let mut builder = eu4_builder();
        builder.on_failed_resolve(FailedResolveStrategy::Stringify);

        let actual: HashMap<String, &str> = builder.deserialize_slice(&data[..], &map).unwrap();
        let mut expected = HashMap::new();
        expected.insert(String::from("0x2d82"), "ENG");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_optional_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Some("ENG".to_string())
            }
        );
    }

    #[test]
    fn test_enum_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x07, 0x00, 0x67, 0x65, 0x6e, 0x65, 0x72, 0x61,
            0x6c,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: MyEnum,
        }

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        #[serde(rename_all = "camelCase")]
        enum MyEnum {
            General,
            Admiral,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: MyEnum::General,
            }
        );
    }

    #[test]
    fn test_deserialize_untagged_enum() {
        let data = [
            0x5e, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0c, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x00,
            0x65, 0x01, 0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x03, 0x00, 0x1b, 0x00,
            0x01, 0x00, 0x0f, 0x00, 0x04, 0x00, 0x64, 0x79, 0x6e, 0x6e, 0x0e, 0x28, 0x01, 0x00,
            0x0c, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            dynasty_house: HashMap<i32, MaybeObject<DynastyHouse>>,
        }

        #[derive(Debug, Clone, PartialEq)]
        enum MaybeObject<T> {
            Text(String),
            Object(T),
        }

        impl<'de, T> Deserialize<'de> for MaybeObject<T>
        where
            T: Deserialize<'de>,
        {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct MaybeObjectVisitor<T1> {
                    marker: PhantomData<T1>,
                }

                impl<'de, T1> de::Visitor<'de> for MaybeObjectVisitor<T1>
                where
                    T1: Deserialize<'de>,
                {
                    type Value = MaybeObject<T1>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("an object or string")
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(MaybeObject::Text(String::from(v)))
                    }

                    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::MapAccess<'de>,
                    {
                        let mvd = de::value::MapAccessDeserializer::new(map);
                        let result = T1::deserialize(mvd)?;
                        Ok(MaybeObject::Object(result))
                    }
                }

                deserializer.deserialize_map(MaybeObjectVisitor {
                    marker: PhantomData,
                })
            }
        }

        #[derive(Debug, Deserialize, Clone, PartialEq)]
        struct DynastyHouse {
            name: String,
            dynasty: i32,
        }

        let expected = HashMap::from([
            (1, MaybeObject::Text(String::from("none"))),
            (
                2,
                MaybeObject::Object(DynastyHouse {
                    name: String::from("dynn"),
                    dynasty: 1,
                }),
            ),
        ]);

        let mut map = HashMap::new();
        map.insert(0x2e5e, "dynasty_house");
        map.insert(0x280e, "dynasty");
        map.insert(0x1b, "name");
        map.insert(0x165, "none");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                dynasty_house: expected
            }
        );
    }

    #[test]
    fn test_tuple_struct_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47, 0x0f,
            0x00, 0x03, 0x00, 0x4f, 0x4e, 0x47, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: MyFlags,
        }

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyFlags(String, String);

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: MyFlags("ENG".to_string(), "ONG".to_string())
            }
        );
    }

    #[test]
    fn test_tuple_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47, 0x0f,
            0x00, 0x03, 0x00, 0x4f, 0x4e, 0x47, 0x04, 0x00,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: (String, String),
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: ("ENG".to_string(), "ONG".to_string())
            }
        );
    }

    #[test]
    fn test_newtype_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: MyString,
        }

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyString(String);

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: MyString("ENG".to_string())
            }
        );
    }

    #[test]
    fn test_consecutive_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0x04, 0x00, 0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00,
            0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00,
            0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            savegame_version: Vec<Version>,
            field1: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x2ec9, "savegame_version");
        map.insert(0x28e2, "first");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: vec![Version { first: 1 }, Version { first: 2 }],
                field1: String::from("ENG"),
            }
        );
    }

    #[test]
    fn test_consecutive_nested_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d,
            0x01, 0x00, 0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00, 0xc9, 0x2e,
            0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x0c, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            savegame_version: Vec<Version>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct Version {
            #[jomini(duplicated)]
            field: Vec<MyField>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyField {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "first");
        map.insert(0x2ec9, "savegame_version");
        map.insert(0x28e2, "field");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: vec![
                    Version {
                        field: vec![MyField { first: 2 }]
                    },
                    Version {
                        field: vec![MyField { first: 3 }]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_consecutive_empty_nested_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00, 0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00,
            0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x03, 0x00,
            0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            savegame_version: Vec<Version>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct Version {
            #[jomini(duplicated)]
            field: Vec<MyField>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyField {
            first: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "first");
        map.insert(0x2ec9, "savegame_version");
        map.insert(0x28e2, "field");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: vec![
                    Version { field: vec![] },
                    Version {
                        field: vec![MyField { first: 3 }]
                    }
                ]
            }
        );
    }

    #[test]
    fn test_non_consecutive_fields() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x14, 0x00, 0x5a, 0x00, 0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00,
            0x00, 0x00, 0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x5b, 0x00, 0x00, 0x00,
        ];

        #[derive(JominiDeserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            field1: Vec<u32>,
            second: i32,
        }

        let mut map = HashMap::new();
        map.insert(0x2d82, "field1");
        map.insert(0x28e3, "second");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: vec![89, 90, 91],
                second: 11
            }
        );
    }

    #[test]
    fn test_meta() {
        let data = &include_bytes!("../../tests/fixtures/meta.bin")["EU4bin".len()..];

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct Meta {
            campaign_id: String,
        }

        let mut map = HashMap::new();
        map.insert(0x337f, "campaign_id");

        let actual: Meta = from_owned(data, &map).unwrap();
        assert_eq!(
            actual,
            Meta {
                campaign_id: String::from("72ce90e3-eff3-4be4-9395-f1c3d33fd1c7"),
            }
        );
    }

    #[test]
    fn test_deserialize_rgb() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x053a, "color");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: Color {
                    red: 110,
                    blue: 27,
                    green: 27
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: Color,
        }

        #[derive(Debug, PartialEq)]
        struct Color {
            red: u8,
            blue: u8,
            green: u8,
        }

        impl<'de> Deserialize<'de> for Color {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let [red, green, blue]: [u8; 3] = Deserialize::deserialize(deserializer)?;
                Ok(Color { red, green, blue })
            }
        }
    }

    #[test]
    fn test_deserialize_rgba() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00,
            0x1c, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x053a, "color");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: (110, 27, 27, 28)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: (u8, u8, u8, u8),
        }
    }

    #[test]
    fn test_deserialize_skip_rgb() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut map = HashMap::new();
        map.insert(0x053a, "color");

        let actual: MyStruct = from_owned(&data[..], &map).unwrap();
        assert_eq!(actual, MyStruct { something: None });

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            something: Option<i32>,
        }
    }

    #[test]
    fn test_object_template_support() {
        // The pattern `{ key=value }=result` becomes an array: [object, value, object, value, ...]
        let mut resolver_map = std::collections::HashMap::new();
        resolver_map.insert(0x2000, "obj".to_string());
        resolver_map.insert(0x2001, "id".to_string());
        resolver_map.insert(0x2002, "type".to_string());
        resolver_map.insert(0x2003, "admin".to_string());
        resolver_map.insert(0x2004, "diplo".to_string());

        let token_map = resolver_map
            .iter()
            .map(|(k, v)| (v.clone(), *k))
            .collect::<std::collections::HashMap<_, _>>();

        let data = text_to_binary(
            b"obj={ { id=31 type=admin }=16 { id=32 type=diplo }=18 }",
            &token_map,
        )
        .unwrap();

        #[derive(Deserialize, PartialEq, Debug)]
        struct TemplateKey {
            id: i32,
            #[serde(rename = "type")]
            key_type: String,
        }

        #[derive(PartialEq, Debug)]
        struct ObjectTemplateList(Vec<(TemplateKey, i32)>);

        impl<'de> Deserialize<'de> for ObjectTemplateList {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                use serde::de::{SeqAccess, Visitor};

                struct ObjectTemplateVisitor;

                impl<'de> Visitor<'de> for ObjectTemplateVisitor {
                    type Value = ObjectTemplateList;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("a sequence of alternating objects and values")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let mut entries = Vec::new();

                        while let Some(key) = seq.next_element::<TemplateKey>()? {
                            if let Some(value) = seq.next_element::<i32>()? {
                                entries.push((key, value));
                            } else {
                                return Err(serde::de::Error::custom("Expected value after key"));
                            }
                        }

                        Ok(ObjectTemplateList(entries))
                    }
                }

                deserializer.deserialize_seq(ObjectTemplateVisitor)
            }
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: ObjectTemplateList,
        }

        let actual: MyStruct = from_owned(&data[..], &resolver_map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: ObjectTemplateList(vec![
                    (
                        TemplateKey {
                            id: 31,
                            key_type: "admin".to_string()
                        },
                        16
                    ),
                    (
                        TemplateKey {
                            id: 32,
                            key_type: "diplo".to_string()
                        },
                        18
                    ),
                ])
            }
        );
    }

    #[test]
    fn test_deserialize_bytes() {
        #[derive(PartialEq, Debug)]
        struct ByteField(Vec<u8>);

        impl<'de> Deserialize<'de> for ByteField {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct ByteVisitor;

                impl<'de> Visitor<'de> for ByteVisitor {
                    type Value = ByteField;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("bytes")
                    }

                    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v.to_vec()))
                    }

                    fn visit_borrowed_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v.to_vec()))
                    }

                    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v))
                    }
                }

                deserializer.deserialize_bytes(ByteVisitor)
            }
        }

        #[derive(JominiDeserialize, Debug, PartialEq)]
        struct MyStruct {
            #[jomini(token = 0x1234)]
            text_field: ByteField,
            #[jomini(token = 0x5678)]
            text_field2: String,
        }

        let out = Vec::new();
        let mut writer = std::io::Cursor::new(out);
        for token in [
            Token::Id(0x1234),
            Token::Equal,
            Token::Quoted(Scalar::new(br#"Joe \"Captain\" Rogers "#)),
            Token::Id(0x5678),
            Token::Equal,
            Token::Quoted(Scalar::new(br#"Joe \"Captain\" Rogers "#)),
        ] {
            token.write(&mut writer).unwrap();
        }

        let binary_data = writer.into_inner();
        let hash = HashMap::<u16, &str>::new();
        let actual: MyStruct = from_slice(&binary_data, &hash).unwrap();

        assert_eq!(
            actual,
            MyStruct {
                text_field: ByteField(b"Joe \\\"Captain\\\" Rogers ".to_vec()),
                text_field2: String::from("Joe \"Captain\" Rogers")
            }
        );
    }

    /// Utility function for unit tests: converts text format to binary format
    fn text_to_binary(
        text: &[u8],
        token_map: &std::collections::HashMap<String, u16>,
    ) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        use crate::binary::Token as BinaryToken;
        use crate::text::Token as TextToken;
        use crate::text::TokenReader as TextTokenReader;
        use std::convert::TryFrom;
        use std::io::Write;

        let mut result = Vec::new();
        let mut reader = TextTokenReader::new(text);

        while let Ok(Some(token)) = reader.next() {
            match token {
                TextToken::Open => {
                    BinaryToken::Open.write(&mut result)?;
                }
                TextToken::Close => {
                    BinaryToken::Close.write(&mut result)?;
                }
                TextToken::Operator(_) => {
                    BinaryToken::Equal.write(&mut result)?;
                }
                TextToken::Quoted(scalar) => {
                    BinaryToken::Quoted(scalar).write(&mut result)?;
                }
                TextToken::Unquoted(scalar) => {
                    // Try to parse as boolean first (using Scalar method)
                    if let Ok(bool_val) = scalar.to_bool() {
                        BinaryToken::Bool(bool_val).write(&mut result)?;
                    }
                    // Try to parse as signed integer (using Scalar method)
                    else if let Ok(i64_val) = scalar.to_i64() {
                        if let Ok(i32_val) = i32::try_from(i64_val) {
                            BinaryToken::I32(i32_val).write(&mut result)?;
                        } else {
                            BinaryToken::I64(i64_val).write(&mut result)?;
                        }
                    } else if let Ok(u64_val) = scalar.to_u64() {
                        if let Ok(u32_val) = u32::try_from(u64_val) {
                            BinaryToken::U32(u32_val).write(&mut result)?;
                        } else {
                            BinaryToken::U64(u64_val).write(&mut result)?;
                        }
                    } else {
                        let text = std::str::from_utf8(scalar.as_bytes())?;

                        if let Some(&token_id) = token_map.get(text) {
                            result.write_all(&token_id.to_le_bytes())?;
                        } else {
                            BinaryToken::Unquoted(scalar).write(&mut result)?;
                        }
                    }
                }
            }
        }

        Ok(result)
    }
}
