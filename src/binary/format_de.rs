use super::format::BinaryFormat;
use crate::{BinarySourceExt, Error, ParserSource, binary::LexemeId};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::io::Read;

/// A binary deserializer generic over a [`BinaryFormat`].
///
/// Owns a [`ParserSource`] and a game-specific format. The deserializer handles
/// map, sequence, struct, enum, option, and structural delimiter traversal; the
/// format handles scalar and identifier decoding.
pub struct BinaryFormatDeserializer<'de, F> {
    source: ParserSource<'de>,
    format: F,
}

/// Context passed to [`BinaryFormat`] implementations while decoding values.
///
/// The context exposes the parser source and format state without exposing the
/// deserializer's internal access types. Implementations that consume an
/// opening delimiter should advance the source past `OPEN`, then call
/// [`BinaryFormatContext::visit_open_seq`] to run the format hook and hand the
/// container to the visitor.
pub struct BinaryFormatContext<'a, 'de, F> {
    de: &'a mut BinaryFormatDeserializer<'de, F>,
}

impl<'a, 'de, F> BinaryFormatContext<'a, 'de, F> {
    #[inline]
    pub(crate) fn new(de: &'a mut BinaryFormatDeserializer<'de, F>) -> Self {
        Self { de }
    }

    /// Return the underlying parser source.
    #[inline]
    pub fn source(&mut self) -> &mut ParserSource<'de> {
        &mut self.de.source
    }

    /// Return shared access to the format state.
    #[inline]
    pub fn format(&self) -> &F {
        &self.de.format
    }

    /// Return mutable access to the format state.
    #[inline]
    pub fn format_mut(&mut self) -> &mut F {
        &mut self.de.format
    }

    /// Return mutable access to both the format state and parser source.
    #[inline]
    pub fn parts(&mut self) -> (&mut F, &mut ParserSource<'de>) {
        (&mut self.de.format, &mut self.de.source)
    }
}

impl<'a, 'de, F> BinaryFormatContext<'a, 'de, F>
where
    F: BinaryFormat,
{
    /// Visit a sequence after the caller has consumed the `OPEN` delimiter.
    ///
    /// This method calls [`BinaryFormat::on_open`] before visiting the
    /// sequence, and consumes the closing delimiter when the visitor stops
    /// before the end of the container.
    #[inline]
    pub fn visit_open_seq<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.de.format.on_open();
        let mut seq = BinarySeqAccess::new(self.de);
        let result = visitor.visit_seq(&mut seq)?;
        seq.finish()?;
        Ok(result)
    }
}

impl<'de, F> BinaryFormatDeserializer<'de, F> {
    /// Construct a deserializer from an existing parser source and format.
    pub fn from_source(source: ParserSource<'de>, format: F) -> Self {
        Self { source, format }
    }

    /// Construct a deserializer over an in-memory slice.
    pub fn from_slice(data: &'de [u8], format: F) -> Self {
        Self::from_source(ParserSource::from_slice(data), format)
    }

    /// Construct a deserializer over a streaming reader.
    pub fn from_reader<R: Read + 'de>(reader: R, format: F) -> Self {
        Self::from_source(ParserSource::from_reader(reader), format)
    }

    /// Construct a deserializer over a streaming reader with a caller-provided
    /// buffer.
    pub fn from_reader_with_buf<R: Read + 'de>(reader: R, format: F, buffer: Vec<u8>) -> Self {
        Self::from_source(ParserSource::from_reader_with_buf(reader, buffer), format)
    }
}

impl<'de, F: BinaryFormat> BinaryFormatDeserializer<'de, F> {
    /// Deserialize a value from this deserializer.
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: Deserialize<'de>,
    {
        T::deserialize(self)
    }

    #[inline]
    fn peek_lexeme_id(&mut self) -> Result<Option<LexemeId>, Error> {
        Ok(self.source.peek_lexeme_id()?)
    }

    fn maybe_open_container(&mut self) -> Result<bool, Error> {
        match self.peek_lexeme_id()? {
            Some(LexemeId::OPEN) => {
                self.source.take::<2>()?;
                self.format.on_open();
                Ok(true)
            }
            Some(_) => Ok(false),
            None => Err(Error::eof()),
        }
    }

    fn skip_value(&mut self) -> Result<(), Error> {
        let mut cx = BinaryFormatContext::new(self);
        F::skip_value(&mut cx)
    }
}

impl<'de, F> de::Deserializer<'de> for &mut BinaryFormatDeserializer<'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::deserialize_msg(
            "root deserializer can only work with key value pairs",
        ))
    }

    #[inline]
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(BinaryMapAccess::new(self, true))
    }

    #[inline]
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
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

struct BinaryValueDeserializer<'a, 'de, F> {
    de: &'a mut BinaryFormatDeserializer<'de, F>,
}

impl<'de, F> BinaryValueDeserializer<'_, 'de, F>
where
    F: BinaryFormat,
{
    fn visit_container_seq<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let mut seq = BinarySeqAccess::new(self.de);
        let result = visitor.visit_seq(&mut seq)?;
        seq.finish()?;
        Ok(result)
    }
}

macro_rules! value_method {
    ($name:ident, $format_method:ident) => {
        #[inline]
        fn $name<V>(self, visitor: V) -> Result<V::Value, Error>
        where
            V: Visitor<'de>,
        {
            let mut cx = BinaryFormatContext::new(self.de);
            F::$format_method(&mut cx, visitor)
        }
    };
}

impl<'de, F> de::Deserializer<'de> for BinaryValueDeserializer<'_, 'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    value_method!(deserialize_any, deserialize_any);
    value_method!(deserialize_i32, deserialize_i32);
    value_method!(deserialize_u32, deserialize_u32);
    value_method!(deserialize_i64, deserialize_i64);
    value_method!(deserialize_u64, deserialize_u64);
    value_method!(deserialize_f32, deserialize_f32);
    value_method!(deserialize_f64, deserialize_f64);
    value_method!(deserialize_bool, deserialize_bool);
    value_method!(deserialize_str, deserialize_str);
    value_method!(deserialize_identifier, deserialize_identifier);

    #[inline]
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    #[inline]
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let mut cx = BinaryFormatContext::new(self.de);
        F::deserialize_bytes(&mut cx, visitor)
    }

    #[inline]
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    /// A bare identifier token decodes to its raw 16-bit value, enabling the
    /// derive macro's `#[jomini(token = ...)]` fast match.
    #[inline]
    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        if let Some(id) = self.de.peek_lexeme_id()?
            && id.is_id()
        {
            self.de.source.take::<2>()?;
            return visitor.visit_u16(id.0);
        }
        let mut cx = BinaryFormatContext::new(self.de);
        F::deserialize_any(&mut cx, visitor)
    }

    #[inline]
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        if self.de.maybe_open_container()? {
            visitor.visit_map(BinaryMapAccess::new(self.de, false))
        } else {
            let mut cx = BinaryFormatContext::new(self.de);
            F::deserialize_any(&mut cx, visitor)
        }
    }

    #[inline]
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error>
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
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(BinaryEnumAccess::new(self.de))
    }

    #[inline]
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        if self.de.maybe_open_container()? {
            self.visit_container_seq(visitor)
        } else {
            let mut cx = BinaryFormatContext::new(self.de);
            F::deserialize_any(&mut cx, visitor)
        }
    }

    #[inline]
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Error>
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
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    #[inline]
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    #[inline]
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.de.skip_value()?;
        visitor.visit_unit()
    }

    #[inline]
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    #[inline]
    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        i8 i16 i128 u8 u128 char
    }
}

struct BinaryMapAccess<'a, 'de, F> {
    de: &'a mut BinaryFormatDeserializer<'de, F>,
    root: bool,
}

impl<'a, 'de, F> BinaryMapAccess<'a, 'de, F> {
    fn new(de: &'a mut BinaryFormatDeserializer<'de, F>, root: bool) -> Self {
        Self { de, root }
    }
}

impl<'de, F> MapAccess<'de> for BinaryMapAccess<'_, 'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            let Some(id) = self.de.peek_lexeme_id()? else {
                return if self.root {
                    Ok(None)
                } else {
                    Err(Error::eof())
                };
            };

            match id {
                LexemeId::CLOSE => {
                    self.de.source.take::<2>()?;
                    self.de.format.on_close();
                    return Ok(None);
                }
                LexemeId::OPEN => {
                    // Ghost object: consume open, skip the contained value
                    self.de.source.take::<2>()?;
                    self.de.format.on_open();
                    self.de.skip_value()?;
                }
                _ => {
                    return seed
                        .deserialize(BinaryValueDeserializer { de: self.de })
                        .map(Some);
                }
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Error>
    where
        V: DeserializeSeed<'de>,
    {
        if matches!(self.de.peek_lexeme_id()?, Some(LexemeId::EQUAL)) {
            self.de.source.take::<2>()?;
            self.de.format.on_equal();
        }
        seed.deserialize(BinaryValueDeserializer { de: self.de })
    }
}

struct BinarySeqAccess<'a, 'de, F> {
    de: &'a mut BinaryFormatDeserializer<'de, F>,
    hit_end: bool,
    elements_read: usize,
}

impl<'a, 'de, F> BinarySeqAccess<'a, 'de, F> {
    fn new(de: &'a mut BinaryFormatDeserializer<'de, F>) -> Self {
        Self {
            de,
            hit_end: false,
            elements_read: 0,
        }
    }

    fn finish(self) -> Result<(), Error>
    where
        F: BinaryFormat,
    {
        if self.hit_end {
            return Ok(());
        }

        match self.de.peek_lexeme_id()? {
            Some(LexemeId::CLOSE) => {
                self.de.source.take::<2>()?;
                self.de.format.on_close();
                Ok(())
            }
            Some(_) => Err(<Error as de::Error>::invalid_length(
                self.elements_read,
                &"end of sequence",
            )),
            None => Err(Error::eof()),
        }
    }
}

impl<'de, F> SeqAccess<'de> for BinarySeqAccess<'_, 'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DeserializeSeed<'de>,
    {
        let Some(id) = self.de.peek_lexeme_id()? else {
            return Err(Error::eof());
        };

        if id == LexemeId::CLOSE {
            self.de.source.take::<2>()?;
            self.de.format.on_close();
            self.hit_end = true;
            return Ok(None);
        }

        if id == LexemeId::EQUAL {
            self.de.source.take::<2>()?;
            self.de.format.on_equal();
        }

        let value = seed.deserialize(BinaryValueDeserializer { de: self.de })?;
        self.elements_read += 1;

        Ok(Some(value))
    }
}

struct BinaryEnumAccess<'a, 'de, F> {
    de: &'a mut BinaryFormatDeserializer<'de, F>,
}

impl<'a, 'de, F> BinaryEnumAccess<'a, 'de, F> {
    fn new(de: &'a mut BinaryFormatDeserializer<'de, F>) -> Self {
        Self { de }
    }
}

impl<'de, F> de::EnumAccess<'de> for BinaryEnumAccess<'_, 'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Error>
    where
        V: DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(BinaryValueDeserializer { de: self.de })?;
        Ok((variant, self))
    }
}

impl<'de, F> de::VariantAccess<'de> for BinaryEnumAccess<'_, 'de, F>
where
    F: BinaryFormat,
{
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Error>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::deserialize_msg(
            "unsupported enum deserialization. Please file issue",
        ))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::deserialize_msg(
            "unsupported enum deserialization. Please file issue",
        ))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::deserialize_msg(
            "unsupported enum deserialization. Please file issue",
        ))
    }
}
