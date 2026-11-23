use crate::{
    BinarySourceExt, DeserializeErrorKind, Error, ParserSource,
    binary::de::BinaryFormatContext,
    binary::{BinaryFlavor, FailedResolveStrategy, LexemeId, Rgb, TokenResolver},
    de::ColorSequence,
};
use serde::de::Visitor;
use std::borrow::Cow;

/// Extension of Serde's [`Visitor`] trait with Jomini-specific binary values.
///
/// The default `visit_rgb` gives the color to `visit_seq` as a sequence of
/// its channels, so existing Serde visitors receive RGB values with no change.
pub trait PdxVisitor<'de>: Visitor<'de> {
    /// Visit an RGB color value. Defaults to a 3- or 4-element sequence.
    #[inline]
    fn visit_rgb(self, rgb: Rgb) -> Result<Self::Value, Error>
    where
        Self: Sized,
    {
        self.visit_seq(ColorSequence::new(rgb))
    }
}

impl<'de, V> PdxVisitor<'de> for V where V: Visitor<'de> {}

/// Semantic decoding for a game-specific binary format.
///
/// A [`BinaryFormat`] is the extension point for deserializing the binary
/// formats of individual games and patches. The
/// [`BinaryFormatDeserializer`](crate::binary::BinaryFormatDeserializer) handles
/// structural traversal (maps, sequences, enums, and the `{`, `}`, `=`
/// delimiters) while the format decodes the value and identifier lexemes.
///
/// Implementations receive a [`BinaryFormatContext`], which exposes source
/// access and the format state while keeping deserializer traversal internals
/// private.
///
/// Structural delimiters (`{`, `}`, `=`) are coordinated with the
/// deserializer. Formats that consume `OPEN` should call
/// [`BinaryFormatContext::visit_open_seq`] after advancing the source so the
/// deserializer can drive container traversal. `EQUAL` and `CLOSE` remain part
/// of map and sequence traversal.
pub trait BinaryFormat: Sized {
    /// Decode raw scalar bytes into text.
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Cow<'a, str>;

    /// Reports a map key's lexeme id, letting a stateful format remember the
    /// previous field so later values can be decoded accordingly.
    ///
    /// Called before the key is consumed, so do not advance the source. The id
    /// is the raw token for token keys, or `QUOTED`/`UNQUOTED` for string keys.
    #[inline]
    fn on_key(&mut self, _id: LexemeId) {}

    /// Called after the deserializer consumes a structural opening delimiter.
    #[inline]
    fn on_open(&mut self) {}

    /// Called after the deserializer consumes a structural `=`.
    #[inline]
    fn on_equal(&mut self) {}

    /// Called after the deserializer consumes a structural closing delimiter.
    #[inline]
    fn on_close(&mut self) {}

    /// Skip the next semantic value without decoding it.
    fn skip_value(cx: &mut BinaryFormatContext<'_, '_, Self>) -> Result<(), Error>;

    /// Attempt to deserialize the next value as `i32`.
    #[inline]
    fn deserialize_i32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `u32`.
    #[inline]
    fn deserialize_u32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `i64`.
    #[inline]
    fn deserialize_i64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `u64`.
    #[inline]
    fn deserialize_u64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `f32`.
    #[inline]
    fn deserialize_f32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `f64`.
    #[inline]
    fn deserialize_f64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as `bool`.
    #[inline]
    fn deserialize_bool<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as a string-like scalar.
    fn deserialize_str<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as raw bytes.
    #[inline]
    fn deserialize_bytes<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Attempt to deserialize the next value as an identifier.
    #[inline]
    fn deserialize_identifier<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        Self::deserialize_any(cx, visitor)
    }

    /// Deserialize the next value without a type hint.
    fn deserialize_any<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>;
}

// --- ParserSource value readers shared by the built-in format ---------------

#[inline]
fn read_u32(source: &mut ParserSource<'_>) -> Result<u32, Error> {
    Ok(u32::from_le_bytes(*source.take::<4>()?))
}

#[inline]
fn read_i32(source: &mut ParserSource<'_>) -> Result<i32, Error> {
    Ok(i32::from_le_bytes(*source.take::<4>()?))
}

#[inline]
fn read_u64(source: &mut ParserSource<'_>) -> Result<u64, Error> {
    Ok(u64::from_le_bytes(*source.take::<8>()?))
}

#[inline]
fn read_i64(source: &mut ParserSource<'_>) -> Result<i64, Error> {
    Ok(i64::from_le_bytes(*source.take::<8>()?))
}

#[inline]
fn read_bool(source: &mut ParserSource<'_>) -> Result<bool, Error> {
    Ok(source.take::<1>()?[0] != 0)
}

#[inline]
fn read_f32_bytes(source: &mut ParserSource<'_>) -> Result<[u8; 4], Error> {
    Ok(*source.take::<4>()?)
}

#[inline]
fn read_f64_bytes(source: &mut ParserSource<'_>) -> Result<[u8; 8], Error> {
    Ok(*source.take::<8>()?)
}

/// Decode a variable-length compact f64 (`FIXED5_*`) into its i64 little-endian
/// byte representation, mirroring [`crate::binary::lexer::read_compact_f64`].
#[inline]
fn read_compact_f64(source: &mut ParserSource<'_>, lexeme: LexemeId) -> Result<[u8; 8], Error> {
    let offset = lexeme.0 - LexemeId::FIXED5_ZERO.0;
    let is_negative = offset > 7;
    let byte_count = (offset - (is_negative as u16 * 7)) as usize;
    let mut buf = [0u8; 8];
    buf[..byte_count].copy_from_slice(source.take_bytes(byte_count)?);
    let sign = 1i64 - (is_negative as i64) * 2;
    Ok((u64::from_le_bytes(buf) as i64 * sign).to_le_bytes())
}

// --- Built-in format bridging BinaryFlavor + TokenResolver ------------------

/// The crate's built-in [`BinaryFormat`], decoding the standard Paradox binary
/// lexemes using a [`BinaryFlavor`] for floats and text and a separate
/// [`TokenResolver`] for identifier and lookup resolution.
pub(crate) struct FlavorFormat<F, R> {
    pub(crate) flavor: F,
    pub(crate) resolver: R,
    pub(crate) failed_resolve_strategy: FailedResolveStrategy,
}

impl<F, R> FlavorFormat<F, R>
where
    F: BinaryFlavor,
    R: TokenResolver,
{
    fn resolve_token<'de, V>(&self, token: u16, visitor: V) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        match self.resolver.resolve(token) {
            Some(id) => visitor.visit_str(id),
            None => match self.failed_resolve_strategy {
                FailedResolveStrategy::Error => {
                    Err(Error::deserialize(DeserializeErrorKind::UnknownToken {
                        token_id: token as u32,
                    }))
                }
                FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{:x}", token)),
                FailedResolveStrategy::Ignore => {
                    visitor.visit_borrowed_str("__internal_identifier_ignore")
                }
            },
        }
    }

    fn resolve_lookup<'de, V>(&self, index: u32, visitor: V) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        match self.resolver.lookup(index) {
            Some(value) => visitor.visit_str(value),
            None => match self.failed_resolve_strategy {
                FailedResolveStrategy::Error => {
                    Err(Error::deserialize(DeserializeErrorKind::UnknownToken {
                        token_id: index,
                    }))
                }
                FailedResolveStrategy::Stringify => visitor.visit_string(format!("{}", index)),
                FailedResolveStrategy::Ignore => {
                    visitor.visit_borrowed_str("__internal_identifier_ignore")
                }
            },
        }
    }

    fn read_lookup(&self, source: &mut ParserSource<'_>, id: LexemeId) -> Result<u32, Error> {
        match id {
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT | LexemeId::LOOKUP_U8_SAV03 => {
                Ok(source.take::<1>()?[0] as u32)
            }
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT | LexemeId::LOOKUP_U16_SAV03 => {
                Ok(u16::from_le_bytes(*source.take::<2>()?) as u32)
            }
            LexemeId::LOOKUP_U24 | LexemeId::LOOKUP_U24_ALT | LexemeId::LOOKUP_U24_SAV03 => {
                let b = source.take::<3>()?;
                Ok(u32::from_le_bytes([b[0], b[1], b[2], 0]))
            }
            LexemeId::LOOKUP_U32 | LexemeId::LOOKUP_U32_ALT | LexemeId::LOOKUP_U32_SAV03 => {
                Ok(u32::from_le_bytes(*source.take::<4>()?))
            }
            _ => unreachable!("read_lookup called with non-lookup lexeme"),
        }
    }

    /// Decode the value identified by `id`, reading its payload from `source`.
    #[inline]
    fn dispatch<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        id: LexemeId,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED | LexemeId::STR_SAV03 => {
                let (format, source) = cx.parts();
                let data = source.read_bstr()?;
                match format.flavor.decode(data) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            LexemeId::EMPTY_STRING => visitor.visit_borrowed_str(""),
            LexemeId::U32 => visitor.visit_u32(read_u32(cx.source())?),
            LexemeId::I32 => visitor.visit_i32(read_i32(cx.source())?),
            LexemeId::U64 => visitor.visit_u64(read_u64(cx.source())?),
            LexemeId::I64 => visitor.visit_i64(read_i64(cx.source())?),
            LexemeId::BOOL => visitor.visit_bool(read_bool(cx.source())?),
            LexemeId::F32 => {
                let (format, source) = cx.parts();
                visitor.visit_f32(format.flavor.visit_f32(read_f32_bytes(source)?))
            }
            LexemeId::F64 => {
                let (format, source) = cx.parts();
                visitor.visit_f64(format.flavor.visit_f64(read_f64_bytes(source)?))
            }
            LexemeId::RGB => visitor.visit_rgb(cx.source().read_rgb()?),
            LexemeId::LOOKUP_U8
            | LexemeId::LOOKUP_U8_ALT
            | LexemeId::LOOKUP_U16
            | LexemeId::LOOKUP_U16_ALT
            | LexemeId::LOOKUP_U24
            | LexemeId::LOOKUP_U24_ALT
            | LexemeId::LOOKUP_U32
            | LexemeId::LOOKUP_U32_ALT
            | LexemeId::LOOKUP_U8_SAV03
            | LexemeId::LOOKUP_U16_SAV03
            | LexemeId::LOOKUP_U24_SAV03
            | LexemeId::LOOKUP_U32_SAV03 => {
                let (format, source) = cx.parts();
                let index = format.read_lookup(source, id)?;
                format.resolve_lookup(index, visitor)
            }
            lexeme if (LexemeId::FIXED5_ZERO..=LexemeId::FIXED5_I56).contains(&lexeme) => {
                let (format, source) = cx.parts();
                visitor.visit_f64(format.flavor.visit_f64(read_compact_f64(source, lexeme)?))
            }
            LexemeId::OPEN => cx.visit_open_seq(visitor),
            LexemeId::CLOSE | LexemeId::EQUAL => Err(Error::invalid_syntax(
                "unexpected structural token",
                cx.source().position(),
            )),
            LexemeId(token) => cx.format().resolve_token(token, visitor),
        }
    }
}

impl<F, R> BinaryFormat for FlavorFormat<F, R>
where
    F: BinaryFlavor,
    R: TokenResolver,
{
    #[inline]
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        self.flavor.decode(data)
    }

    fn skip_value(cx: &mut BinaryFormatContext<'_, '_, Self>) -> Result<(), Error> {
        skip_standard(cx.source())
    }

    #[inline]
    fn deserialize_i32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        if let Some(d) = source.window().first_chunk::<6>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::I32
        {
            let value = i32::from_le_bytes([d[2], d[3], d[4], d[5]]);
            source.advance(6);
            return visitor.visit_i32(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_u32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let (format, source) = cx.parts();
        if let Some(d) = source.window().first_chunk::<6>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::U32
        {
            let value = u32::from_le_bytes([d[2], d[3], d[4], d[5]]);
            source.advance(6);
            return visitor.visit_u32(value);
        }

        let id = source.read_lexeme_id()?;
        match id {
            LexemeId::LOOKUP_U8
            | LexemeId::LOOKUP_U8_ALT
            | LexemeId::LOOKUP_U16
            | LexemeId::LOOKUP_U16_ALT
            | LexemeId::LOOKUP_U24
            | LexemeId::LOOKUP_U24_ALT
            | LexemeId::LOOKUP_U32
            | LexemeId::LOOKUP_U32_ALT
            | LexemeId::LOOKUP_U8_SAV03
            | LexemeId::LOOKUP_U16_SAV03
            | LexemeId::LOOKUP_U24_SAV03
            | LexemeId::LOOKUP_U32_SAV03 => visitor.visit_u32(format.read_lookup(source, id)?),
            _ => Self::dispatch(cx, id, visitor),
        }
    }

    #[inline]
    fn deserialize_i64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        if let Some(d) = source.window().first_chunk::<10>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::I64
        {
            let value = i64::from_le_bytes([d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9]]);
            source.advance(10);
            return visitor.visit_i64(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_u64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        if let Some(d) = source.window().first_chunk::<10>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::U64
        {
            let value = u64::from_le_bytes([d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9]]);
            source.advance(10);
            return visitor.visit_u64(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_f32<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let (format, source) = cx.parts();
        if let Some(d) = source.window().first_chunk::<6>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::F32
        {
            let value = format.flavor.visit_f32([d[2], d[3], d[4], d[5]]);
            source.advance(6);
            return visitor.visit_f32(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_f64<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let (format, source) = cx.parts();
        if let Some(d) = source.window().first_chunk::<10>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::F64
        {
            let value = format
                .flavor
                .visit_f64([d[2], d[3], d[4], d[5], d[6], d[7], d[8], d[9]]);
            source.advance(10);
            return visitor.visit_f64(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_bool<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        if let Some(d) = source.window().first_chunk::<3>()
            && LexemeId::new(u16::from_le_bytes([d[0], d[1]])) == LexemeId::BOOL
        {
            let value = d[2] != 0;
            source.advance(3);
            return visitor.visit_bool(value);
        }

        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }

    #[inline]
    fn deserialize_str<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let (format, source) = cx.parts();
        let id = source.read_lexeme_id()?;
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED | LexemeId::STR_SAV03 => {
                let data = source.read_bstr()?;
                match format.flavor.decode(data) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            _ => Self::dispatch(cx, id, visitor),
        }
    }

    #[inline]
    fn deserialize_bytes<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        let id = source.read_lexeme_id()?;
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED | LexemeId::STR_SAV03 => {
                visitor.visit_bytes(source.read_bstr()?)
            }
            _ => Self::dispatch(cx, id, visitor),
        }
    }

    #[inline]
    fn deserialize_any<'de, V>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: PdxVisitor<'de>,
    {
        let source = cx.source();
        let id = source.read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }
}

fn skip_standard(source: &mut ParserSource<'_>) -> Result<(), Error> {
    let id = source.read_lexeme_id()?;
    Ok(source.skip_value(id)?)
}
