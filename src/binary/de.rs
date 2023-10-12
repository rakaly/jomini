use super::{tokens::*, Rgb};
use crate::{
    binary::{BinaryFlavor, FailedResolveStrategy, TokenResolver},
    de::ColorSequence,
    util::get_split,
    BinaryTape, BinaryToken, DeserializeError, DeserializeErrorKind, Error, ErrorKind,
};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::borrow::Cow;

#[derive(Debug)]
struct OndemandParser<'data> {
    data: &'data [u8],
    original_length: usize,
}

impl<'data> OndemandParser<'data> {
    #[inline]
    pub fn peek(&mut self) -> Option<u16> {
        self.data
            .get(..2)
            .map(|head| u16::from_le_bytes([head[0], head[1]]))
    }

    #[inline]
    pub fn next(&mut self) -> Option<u16> {
        let (data, token) =
            get_split::<2>(self.data).map(|(head, rest)| (rest, u16::from_le_bytes(head)))?;
        self.data = data;
        Some(token)
    }

    #[inline]
    pub fn read(&mut self) -> Result<u16, Error> {
        self.next().ok_or_else(Error::eof)
    }

    #[inline]
    pub fn read_string(&mut self) -> Result<&'data [u8], Error> {
        let (head, rest) = get_split::<2>(self.data).ok_or_else(Error::eof)?;
        let text_len = usize::from(u16::from_le_bytes(head));
        if text_len <= rest.len() {
            let (text, rest) = rest.split_at(text_len);
            self.data = rest;
            Ok(text)
        } else {
            Err(Error::eof())
        }
    }

    #[inline]
    pub fn read_bool(&mut self) -> Result<bool, Error> {
        let (&first, rest) = self.data.split_first().ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(first != 0)
    }

    #[inline]
    fn read_u32(&mut self) -> Result<u32, Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(u32::from_le_bytes(head))
    }

    #[inline]
    fn read_u64(&mut self) -> Result<u64, Error> {
        let (head, rest) = get_split::<8>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(u64::from_le_bytes(head))
    }

    #[inline]
    fn read_i64(&mut self) -> Result<i64, Error> {
        let (head, rest) = get_split::<8>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(i64::from_le_bytes(head))
    }

    #[inline]
    fn read_i32(&mut self) -> Result<i32, Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(i32::from_le_bytes(head))
    }

    #[inline]
    fn read_f32(&mut self) -> Result<[u8; 4], Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(head)
    }

    #[inline]
    fn read_f64(&mut self) -> Result<[u8; 8], Error> {
        let (head, rest) = get_split::<8>(self.data).ok_or_else(Error::eof)?;
        self.data = rest;
        Ok(head)
    }

    #[inline]
    fn skip_value(&mut self, init: u16) -> Result<(), Error> {
        match init {
            QUOTED_STRING | UNQUOTED_STRING => {
                self.read_string()?;
                Ok(())
            }
            U32 => {
                self.read_u32()?;
                Ok(())
            }
            I32 => {
                self.read_i32()?;
                Ok(())
            }
            U64 => {
                self.read_u64()?;
                Ok(())
            }
            I64 => {
                self.read_i64()?;
                Ok(())
            }
            BOOL => {
                self.read_bool()?;
                Ok(())
            }
            F32 => {
                self.read_f32()?;
                Ok(())
            }
            F64 => {
                self.read_f64()?;
                Ok(())
            }
            OPEN => self.skip_container(),
            _ => Ok(()),
        }
    }

    #[inline]
    fn skip_container(&mut self) -> Result<(), Error> {
        let mut depth = 1;
        while depth != 0 {
            match self.read()? {
                QUOTED_STRING | UNQUOTED_STRING => {
                    self.read_string()?;
                }
                U32 => {
                    self.read_u32()?;
                }
                I32 => {
                    self.read_i32()?;
                }
                U64 => {
                    self.read_u64()?;
                }
                I64 => {
                    self.read_i64()?;
                }
                BOOL => {
                    self.read_bool()?;
                }
                F32 => {
                    self.read_f32()?;
                }
                F64 => {
                    self.read_f64()?;
                }
                END => depth -= 1,
                OPEN => depth += 1,
                _ => {}
            }
        }

        Ok(())
    }

    fn read_rgb(&mut self) -> Result<Rgb, Error> {
        let start = self.read()?;
        let rtoken = self.read()?;
        let r = self.read_u32()?;
        let gtoken = self.read()?;
        let g = self.read_u32()?;
        let btoken = self.read()?;
        let b = self.read_u32()?;
        let end = self.read()?;
        match (start, rtoken, gtoken, btoken, end) {
            (OPEN, U32, U32, U32, END) => Ok(Rgb { r, g, b }),
            _ => Err(self.invalid_syntax("invalid rgb value")),
        }
    }

    #[cold]
    #[inline(never)]
    fn invalid_syntax<T: Into<String>>(&self, msg: T) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: msg.into(),
            offset: self.original_length - self.data.len(),
        })
    }
}

/// On-demand binary deserializer
pub struct OndemandBinaryDeserializer<'data, 'res: 'data, RES, F> {
    parser: OndemandParser<'data>,
    config: BinaryConfig<'res, RES, F>,
}

impl OndemandBinaryDeserializer<'_, '_, (), ()> {
    /// Constructs a OndemandBinaryDeserializerBuilder
    pub fn builder_flavor<F: BinaryFlavor>(flavor: F) -> OndemandBinaryDeserializerBuilder<F> {
        OndemandBinaryDeserializerBuilder::with_flavor(flavor)
    }
}

/// Build a tweaked on-deman binary deserializer
#[derive(Debug)]
pub struct OndemandBinaryDeserializerBuilder<F> {
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
}

impl<F> OndemandBinaryDeserializerBuilder<F>
where
    F: BinaryFlavor,
{
    /// Create a new builder instance
    pub fn with_flavor(flavor: F) -> Self {
        OndemandBinaryDeserializerBuilder {
            failed_resolve_strategy: FailedResolveStrategy::Ignore,
            flavor,
        }
    }

    /// Set the behavior when a unknown token is encountered
    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.failed_resolve_strategy = strategy;
        self
    }

    /// Convenience method for parsing and building a deserializer
    pub fn from_slice<'data, 'res: 'data, RES>(
        self,
        data: &'data [u8],
        resolver: &'res RES,
    ) -> OndemandBinaryDeserializer<'data, 'res, RES, F>
    where
        RES: TokenResolver,
    {
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
            flavor: self.flavor,
        };

        OndemandBinaryDeserializer {
            parser: OndemandParser {
                data,
                original_length: data.len(),
            },
            config,
        }
    }

    /// Convenience method for parsing and deserializing binary data
    pub fn deserialize_slice<'b, 'data, 'res: 'data, RES, T>(
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

impl<'de, 'res, RES: TokenResolver, E: BinaryFlavor> OndemandBinaryDeserializer<'de, 'res, RES, E> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: Deserialize<'de>,
    {
        T::deserialize(self)
    }
}

impl<'a, 'de, 'res, RES: TokenResolver, F: BinaryFlavor> de::Deserializer<'de>
    for &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>
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

impl<'de, 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> MapAccess<'de>
    for OndemandMap<'a, 'de, 'res, RES, F>
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        let token = self.de.parser.next();
        match token {
            Some(END) => Ok(None),
            None if self.root => Ok(None),
            None => Err(Error::eof()),
            Some(token) => seed
                .deserialize(OndemandTokenDeserializer {
                    de: &mut *self.de,
                    token,
                })
                .map(Some),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let mut token = self.de.parser.read()?;
        if token == EQUAL {
            token = self.de.parser.read()?;
        }

        seed.deserialize(OndemandTokenDeserializer {
            de: &mut *self.de,
            token,
        })
    }
}

struct OndemandTokenDeserializer<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    token: u16,
}

impl<'a, 'de: 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor>
    OndemandTokenDeserializer<'a, 'de, 'res, RES, F>
where
    F: BinaryFlavor,
{
    fn deser<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: de::Visitor<'de>,
    {
        let mut tok = self.token;

        // Skip empty objects masquerading as keys
        while tok == OPEN && matches!(self.de.parser.peek(), Some(END)) {
            self.de.parser.read()?;
            tok = self.de.parser.read()?;
        }

        match tok {
            QUOTED_STRING | UNQUOTED_STRING => {
                let data = self.de.parser.read_string()?;
                match self.de.config.flavor.decode(data) {
                    Cow::Borrowed(x) => visitor.visit_borrowed_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            U32 => visitor.visit_u32(self.de.parser.read_u32()?),
            I32 => visitor.visit_i32(self.de.parser.read_i32()?),
            U64 => visitor.visit_u64(self.de.parser.read_u64()?),
            I64 => visitor.visit_i64(self.de.parser.read_i64()?),
            BOOL => visitor.visit_bool(self.de.parser.read_bool()?),
            F32 => visitor.visit_f32(self.de.config.flavor.visit_f32(self.de.parser.read_f32()?)),
            F64 => visitor.visit_f64(self.de.config.flavor.visit_f64(self.de.parser.read_f64()?)),
            OPEN => visitor.visit_seq(OndemandSeq::new(self.de)),
            END | EQUAL => Err(self
                .de
                .parser
                .invalid_syntax("unexpected token encountered")),
            s => match self.de.config.resolver.resolve(s) {
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
    deserialize_scalar!(deserialize_u16);
    deserialize_scalar!(deserialize_char);
    deserialize_scalar!(deserialize_identifier);
    deserialize_scalar!(deserialize_bytes);
    deserialize_scalar!(deserialize_byte_buf);

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == BOOL {
            visitor.visit_bool(self.de.parser.read_bool()?)
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == I32 {
            visitor.visit_i32(self.de.parser.read_i32()?)
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == U32 {
            visitor.visit_u32(self.de.parser.read_u32()?)
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == U64 {
            visitor.visit_u64(self.de.parser.read_u64()?)
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == I64 {
            visitor.visit_i64(self.de.parser.read_i64()?)
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == F32 {
            visitor.visit_f32(self.de.config.flavor.visit_f32(self.de.parser.read_f32()?))
        } else {
            Ok(self.deser(visitor)?)
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.token == F64 {
            visitor.visit_f64(self.de.config.flavor.visit_f64(self.de.parser.read_f64()?))
        } else {
            Ok(self.deser(visitor)?)
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
        if self.token == QUOTED_STRING || self.token == UNQUOTED_STRING {
            let data = self.de.parser.read_string()?;
            match self.de.config.flavor.decode(data) {
                Cow::Borrowed(x) => visitor.visit_borrowed_str(x),
                Cow::Owned(x) => visitor.visit_string(x),
            }
        } else {
            Ok(self.deser(visitor)?)
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
        if self.token == OPEN {
            let mut seq = OndemandSeq::new(self.de);
            let result = visitor.visit_seq(&mut seq)?;
            if !seq.hit_end {
                // For when we are deserializing an array that doesn't read
                // the closing token
                let ender = self.de.parser.read()?;
                if ender != END {
                    return Err(self
                        .de
                        .parser
                        .invalid_syntax("Expected sequence to be terminated with an end token"));
                }
            }
            Ok(result)
        } else if self.token == RGB {
            let rgb = self.de.parser.read_rgb()?;
            visitor.visit_seq(ColorSequence::new(rgb))
        } else {
            Ok(self.deser(visitor)?)
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
        if self.token == OPEN {
            visitor.visit_map(OndemandMap::new(self.de, false))
        } else {
            Ok(self.deser(visitor)?)
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

impl<'de, 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> SeqAccess<'de>
    for OndemandSeq<'a, 'de, 'res, RES, F>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        let token = self.de.parser.read()?;
        if token == END {
            self.hit_end = true;
            Ok(None)
        } else {
            seed.deserialize(OndemandTokenDeserializer {
                de: &mut *self.de,
                token,
            })
            .map(Some)
        }
    }
}

struct OndemandEnum<'a, 'de: 'a, 'res: 'de, RES: 'a, F> {
    de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>,
    token: u16,
}

impl<'a, 'de: 'a, 'res: 'de, RES: 'a, F> OndemandEnum<'a, 'de, 'res, RES, F> {
    fn new(de: &'a mut OndemandBinaryDeserializer<'de, 'res, RES, F>, token: u16) -> Self {
        OndemandEnum { de, token }
    }
}

impl<'de, 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> de::EnumAccess<'de>
    for OndemandEnum<'a, 'de, 'res, RES, F>
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

impl<'de, 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> de::VariantAccess<'de>
    for OndemandEnum<'a, 'de, 'res, RES, F>
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
/// use jomini::{BinaryDeserializer, Encoding, JominiDeserialize, Windows1252Encoding};
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
/// let mut deserializer = builder.from_slice(&data[..], &map)?;
/// let a: StructA = deserializer.deserialize()?;
/// assert_eq!(a, StructA {
///   field1: "ENG".to_string(),
///   field2: 89,
/// });
///
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// It is not recommended to use the [`flatten` serde attribute][0], as then it
/// would be difficult to reuse the same struct for binary and text
/// deserialization. See [TextDeserializer](crate::TextDeserializer) for more
/// info
///
/// [0]: https://serde.rs/attr-flatten.html
pub struct BinaryDeserializer<'b, 'data: 'b, 'res: 'data, RES, F> {
    tape: BinaryDeserializerKind<'data, 'b>,
    config: BinaryConfig<'res, RES, F>,
}

enum BinaryDeserializerKind<'data, 'b> {
    Owned(BinaryTape<'data>),
    Borrowed(&'b BinaryTape<'data>),
}

/// Build a tweaked binary deserializer
#[derive(Debug)]
pub struct BinaryDeserializerBuilder<F> {
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
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
        }
    }

    /// Set the behavior when a unknown token is encountered
    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.failed_resolve_strategy = strategy;
        self
    }

    /// Convenience method for parsing and building a deserializer
    pub fn from_slice<'b, 'a, 'res: 'a, RES>(
        self,
        data: &'a [u8],
        resolver: &'res RES,
    ) -> Result<BinaryDeserializer<'b, 'a, 'res, RES, F>, Error>
    where
        RES: TokenResolver,
    {
        let tape = BinaryTape::from_slice(data)?;
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
            flavor: self.flavor,
        };

        Ok(BinaryDeserializer {
            tape: BinaryDeserializerKind::Owned(tape),
            config,
        })
    }

    /// Convenience method for parsing and deserializing binary data
    pub fn deserialize_slice<'b, 'data, 'res: 'data, RES, T>(
        self,
        data: &'data [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: Deserialize<'data>,
        RES: TokenResolver,
    {
        let deser = self.from_slice(data, resolver)?;
        deser.deserialize()
    }

    /// Deserialize the given binary tape
    pub fn from_tape<'data, 'b, 'res: 'data, RES>(
        self,
        tape: &'b BinaryTape<'data>,
        resolver: &'res RES,
    ) -> BinaryDeserializer<'b, 'data, 'res, RES, F>
    where
        RES: TokenResolver,
    {
        let config = BinaryConfig {
            resolver,
            failed_resolve_strategy: self.failed_resolve_strategy,
            flavor: self.flavor,
        };

        BinaryDeserializer {
            tape: BinaryDeserializerKind::Borrowed(tape),
            config,
        }
    }
}

impl<'b, 'de, 'res, RES: TokenResolver, E: BinaryFlavor> BinaryDeserializer<'b, 'de, 'res, RES, E> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&self) -> Result<T, Error>
    where
        T: Deserialize<'de>,
    {
        T::deserialize(self)
    }
}

impl BinaryDeserializer<'_, '_, '_, (), ()> {
    /// Constructs a BinaryDeserializerBuilder
    pub fn builder_flavor<F: BinaryFlavor>(flavor: F) -> BinaryDeserializerBuilder<F> {
        BinaryDeserializerBuilder::with_flavor(flavor)
    }
}

impl<RES, E> BinaryDeserializer<'_, '_, '_, RES, E> {
    /// Update how the deserializer handles failed token resolution
    pub fn on_failed_resolve(&mut self, strategy: FailedResolveStrategy) -> &mut Self {
        self.config.failed_resolve_strategy = strategy;
        self
    }
}

struct BinaryConfig<'res, RES, F> {
    resolver: &'res RES,
    failed_resolve_strategy: FailedResolveStrategy,
    flavor: F,
}

impl<'a, 'b, 'de, 'res, RES: TokenResolver, F: BinaryFlavor> de::Deserializer<'de>
    for &'a BinaryDeserializer<'b, 'de, 'res, RES, F>
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
        match &self.tape {
            BinaryDeserializerKind::Owned(x) | &BinaryDeserializerKind::Borrowed(x) => visitor
                .visit_map(BinaryMap::new(
                    &self.config,
                    x.tokens(),
                    0,
                    x.tokens().len(),
                )),
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

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

struct BinaryMap<'c, 'a: 'c, 'de: 'a, 'res: 'de, RES: 'a, E> {
    config: &'a BinaryConfig<'res, RES, E>,
    tokens: &'c [BinaryToken<'de>],
    tape_idx: usize,
    end_idx: usize,
    value_ind: usize,
}

impl<'c, 'a, 'de, 'res: 'de, RES, E> BinaryMap<'c, 'a, 'de, 'res, RES, E> {
    fn new(
        config: &'a BinaryConfig<'res, RES, E>,
        tokens: &'c [BinaryToken<'de>],
        tape_idx: usize,
        end_idx: usize,
    ) -> Self {
        BinaryMap {
            config,
            tokens,
            tape_idx,
            end_idx,
            value_ind: 0,
        }
    }
}

impl<'c, 'de, 'a, 'res: 'de, RES: TokenResolver, F: BinaryFlavor> MapAccess<'de>
    for BinaryMap<'c, 'a, 'de, 'res, RES, F>
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if self.tape_idx < self.end_idx {
            let current_idx = self.tape_idx;

            self.value_ind = self.tape_idx + 1;
            let next_key = match self.tokens[self.value_ind] {
                BinaryToken::Array(x) | BinaryToken::Object(x) => x,
                _ => self.value_ind,
            };

            self.tape_idx = next_key + 1;
            seed.deserialize(KeyDeserializer {
                tape_idx: current_idx,
                tokens: self.tokens,
                config: self.config,
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            value_ind: self.value_ind,
            tokens: self.tokens,
            config: self.config,
        })
    }

    fn size_hint(&self) -> Option<usize> {
        Some(object_len(self.tokens, self.tape_idx))
    }
}

struct KeyDeserializer<'b, 'de: 'b, 'res: 'de, RES, F> {
    config: &'b BinaryConfig<'res, RES, F>,
    tokens: &'b [BinaryToken<'de>],
    tape_idx: usize,
}

fn visit_key<'b, 'de: 'b, 'res: 'de, RES: TokenResolver, F: BinaryFlavor, V: Visitor<'de>>(
    tape_idx: usize,
    tokens: &'b [BinaryToken<'de>],
    config: &'b BinaryConfig<'res, RES, F>,
    visitor: V,
) -> Result<V::Value, Error> {
    match tokens[tape_idx] {
        BinaryToken::Object(_)
        | BinaryToken::Array(_)
        | BinaryToken::End(_)
        | BinaryToken::Rgb(_) => Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(String::from("unable to deserialize key type")),
        })),
        BinaryToken::MixedContainer | BinaryToken::Equal => visitor.visit_unit(),
        BinaryToken::Bool(x) => visitor.visit_bool(x),
        BinaryToken::U32(x) => visitor.visit_u32(x),
        BinaryToken::U64(x) => visitor.visit_u64(x),
        BinaryToken::I64(x) => visitor.visit_i64(x),
        BinaryToken::I32(x) => visitor.visit_i32(x),
        BinaryToken::Quoted(x) | BinaryToken::Unquoted(x) => {
            match config.flavor.decode(x.as_bytes()) {
                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            }
        }
        BinaryToken::F32(x) => visitor.visit_f32(config.flavor.visit_f32(x)),
        BinaryToken::F64(x) => visitor.visit_f64(config.flavor.visit_f64(x)),
        BinaryToken::Token(s) => match config.resolver.resolve(s) {
            Some(id) => visitor.visit_borrowed_str(id),
            None => match config.failed_resolve_strategy {
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

impl<'b, 'de, 'res: 'de, RES: TokenResolver, E: BinaryFlavor> de::Deserializer<'de>
    for KeyDeserializer<'b, 'de, 'res, RES, E>
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visit_key(self.tape_idx, self.tokens, self.config, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum ignored_any identifier struct
    }
}

struct ValueDeserializer<'c, 'b: 'c, 'de: 'b, 'res: 'de, RES, E> {
    config: &'b BinaryConfig<'res, RES, E>,
    value_ind: usize,
    tokens: &'c [BinaryToken<'de>],
}

impl<'c, 'b, 'de, 'res: 'de, RES: TokenResolver, E: BinaryFlavor> de::Deserializer<'de>
    for ValueDeserializer<'c, 'b, 'de, 'res, RES, E>
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Array(x) => visitor.visit_seq(BinarySequence {
                config: self.config,
                tokens: self.tokens,
                idx: idx + 1,
                end_idx: *x,
            }),
            BinaryToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(*x)),
            BinaryToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.config, self.tokens, idx + 1, *x))
            }
            BinaryToken::End(_x) => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from(
                    "encountered end when trying to deserialize",
                )),
            })),
            _ => visit_key(idx, self.tokens, self.config, visitor),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Array(x) => visitor.visit_seq(BinarySequence {
                config: self.config,
                tokens: self.tokens,
                idx: idx + 1,
                end_idx: *x,
            }),
            BinaryToken::Rgb(x) => visitor.visit_seq(ColorSequence::new(*x)),
            _ => visit_key(idx, self.tokens, self.config, visitor),
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
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

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
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

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_ind;
        match &self.tokens[idx] {
            BinaryToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.config, self.tokens, idx + 1, *x))
            }

            // An array is supported if it is empty
            BinaryToken::Array(x) => {
                visitor.visit_map(BinaryMap::new(self.config, self.tokens, idx + 1, *x))
            }
            _ => visit_key(idx, self.tokens, self.config, visitor),
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_enum(EnumAccess {
            config: self.config,
            tokens: self.tokens,
            idx: self.value_ind,
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct
        identifier
    }
}

struct EnumAccess<'b, 'de: 'b, 'res: 'de, RES, E> {
    config: &'b BinaryConfig<'res, RES, E>,
    tokens: &'b [BinaryToken<'de>],
    idx: usize,
}

impl<'b, 'de, 'tokens, RES, E> de::EnumAccess<'de> for EnumAccess<'b, 'de, 'tokens, RES, E>
where
    RES: TokenResolver,
    E: BinaryFlavor,
{
    type Error = Error;
    type Variant = VariantDeserializer;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = ValueDeserializer {
            value_ind: self.idx,
            tokens: self.tokens,
            config: self.config,
        };

        let visitor = VariantDeserializer;
        seed.deserialize(variant).map(|v| (v, visitor))
    }
}

struct VariantDeserializer;

impl<'de> de::VariantAccess<'de> for VariantDeserializer {
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

struct BinarySequence<'b, 'de: 'b, 'res: 'de, RES, E> {
    config: &'b BinaryConfig<'res, RES, E>,
    tokens: &'b [BinaryToken<'de>],
    idx: usize,
    end_idx: usize,
}

impl<'b, 'de, 'res: 'de, RES: TokenResolver, E: BinaryFlavor> SeqAccess<'de>
    for BinarySequence<'b, 'de, 'res, RES, E>
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.end_idx {
            Ok(None)
        } else {
            let next_key = match self.tokens[self.idx] {
                BinaryToken::Array(x) | BinaryToken::Object(x) => x,
                _ => self.idx,
            };

            let value_ind = self.idx;
            self.idx = next_key + 1;
            seed.deserialize(ValueDeserializer {
                config: self.config,
                tokens: self.tokens,
                value_ind,
            })
            .map(Some)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(array_len(self.tokens, self.idx))
    }
}

/// Returns the number of fields left in an object
fn object_len(tokens: &[BinaryToken], mut key_idx: usize) -> usize {
    let mut count = 0;

    while let Some(key) = tokens.get(key_idx) {
        if let BinaryToken::End(_) = key {
            return count;
        }

        let val_ind = key_idx + 1;
        key_idx = match tokens.get(val_ind) {
            Some(BinaryToken::Array(x)) | Some(BinaryToken::Object(x)) => x + 1,
            _ => val_ind + 1,
        };

        count += 1;
    }

    count
}

/// Returns the number of values left in an array
fn array_len(tokens: &[BinaryToken], mut val_ind: usize) -> usize {
    let mut count = 0;

    while let Some(val) = tokens.get(val_ind) {
        val_ind = match val {
            BinaryToken::Array(x) | BinaryToken::Object(x) => x + 1,
            BinaryToken::End(_) => return count,
            _ => val_ind + 1,
        };

        count += 1;
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour};
    use crate::{Encoding, Windows1252Encoding};
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
        BinaryDeserializer::builder_flavor(Eu4Flavor::new())
    }

    fn from_slice<'a, 'res: 'a, RES, T>(data: &'a [u8], resolver: &'res RES) -> Result<T, Error>
    where
        T: Deserialize<'a> + PartialEq + std::fmt::Debug,
        RES: TokenResolver,
    {
        let result = eu4_builder().from_slice(data, resolver)?.deserialize()?;
        let ondemand = OndemandBinaryDeserializerBuilder::with_flavor(Eu4Flavor::new())
            .deserialize_slice(data, resolver)?;
        assert_eq!(result, ondemand);
        Ok(result)
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: DateHour::from_ymdh(1936, 1, 1, 12)
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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
    fn test_numerical_identifiers() {
        let data = [
            0x0c, 0x00, 0x59, 0x00, 0x00, 0x00, 0x01, 0x00, 0x0c, 0x00, 0x1e, 0x00, 0x00, 0x00,
        ];

        let map: HashMap<u16, String> = HashMap::new();
        let actual: HashMap<i32, i32> = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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
        let actual: Result<MyStruct, _> =
            builder.from_slice(&data[..], &map).unwrap().deserialize();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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

        let actual: Meta = from_slice(&data[..], &map).unwrap();
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

        let actual: MyStruct = from_slice(&data[..], &map).unwrap();
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
                struct ColorVisitor;

                impl<'de> Visitor<'de> for ColorVisitor {
                    type Value = Color;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a color")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::SeqAccess<'de>,
                    {
                        let ty = seq.next_element::<&str>()?.expect("value type");
                        match ty {
                            "rgb" => {
                                let (red, green, blue) =
                                    seq.next_element::<(u8, u8, u8)>()?.expect("rgb channels");
                                Ok(Color { red, green, blue })
                            }
                            _ => panic!("unexpected color type"),
                        }
                    }
                }

                deserializer.deserialize_seq(ColorVisitor)
            }
        }
    }

    #[test]
    fn test_object_len() {
        let tokens = vec![
            BinaryToken::Token(0x0000),
            BinaryToken::Token(0x0001),
            BinaryToken::Token(0x0002),
            BinaryToken::Token(0x0003),
        ];

        assert_eq!(object_len(&tokens, 0), 2);
        assert_eq!(object_len(&tokens, 2), 1);
        assert_eq!(object_len(&tokens, 4), 0);
    }

    #[test]
    fn test_object_len2() {
        let tokens = vec![
            BinaryToken::Token(0x0000),
            BinaryToken::Object(6),
            BinaryToken::Token(0x0001),
            BinaryToken::Token(0x0002),
            BinaryToken::Token(0x0003),
            BinaryToken::Token(0x0004),
            BinaryToken::End(1),
            BinaryToken::Token(0x0005),
            BinaryToken::Token(0x0006),
        ];

        assert_eq!(object_len(&tokens, 0), 2);
        assert_eq!(object_len(&tokens, 2), 2);
        assert_eq!(object_len(&tokens, 4), 1);
        assert_eq!(object_len(&tokens, 6), 0);
        assert_eq!(object_len(&tokens, 7), 1);
        assert_eq!(object_len(&tokens, 9), 0);
    }

    #[test]
    fn test_array_len() {
        let tokens = vec![
            BinaryToken::Token(0x0000),
            BinaryToken::Array(4),
            BinaryToken::Token(0x0001),
            BinaryToken::Token(0x0002),
            BinaryToken::End(1),
        ];

        assert_eq!(array_len(&tokens, 2), 2);
        assert_eq!(array_len(&tokens, 3), 1);
        assert_eq!(array_len(&tokens, 4), 0);
    }

    #[test]
    fn test_array_len2() {
        let tokens = vec![
            BinaryToken::Token(0x0000),
            BinaryToken::Array(8),
            BinaryToken::Object(7),
            BinaryToken::Token(0x0001),
            BinaryToken::Token(0x0002),
            BinaryToken::Token(0x0003),
            BinaryToken::Token(0x0004),
            BinaryToken::End(1),
            BinaryToken::End(1),
        ];

        assert_eq!(array_len(&tokens, 2), 1);
    }
}
