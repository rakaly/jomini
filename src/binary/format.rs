use crate::{
    BinarySourceExt, DeserializeErrorKind, Error, ParserSource,
    binary::format_de::BinaryFormatContext,
    binary::{BinaryFlavor, FailedResolveStrategy, LexemeId, Rgb, TokenResolver},
    de::ColorSequence,
};
use serde::de::Visitor;
use std::borrow::Cow;

/// Extension of Serde's [`Visitor`] trait with Jomini-specific binary values.
///
/// The default `visit_rgb` forwards to `visit_seq` via [`ColorSequence`], so
/// existing Serde visitors automatically receive RGB support.
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

/// Read an RGB value following an `RGB` lexeme, mirroring
/// [`crate::binary::lexer::read_rgb`].
fn read_rgb(source: &mut ParserSource<'_>) -> Result<Rgb, Error> {
    let invalid =
        |source: &ParserSource<'_>| Error::invalid_syntax("invalid rgb", source.position());
    if source.read_lexeme_id()? != LexemeId::OPEN {
        return Err(invalid(source));
    }
    let channel = |source: &mut ParserSource<'_>| -> Result<u32, Error> {
        if source.read_lexeme_id()? != LexemeId::U32 {
            return Err(invalid(source));
        }
        read_u32(source)
    };
    let r = channel(source)?;
    let g = channel(source)?;
    let b = channel(source)?;

    match source.read_lexeme_id()? {
        LexemeId::CLOSE => Ok(Rgb { r, g, b, a: None }),
        LexemeId::U32 => {
            let a = read_u32(source)?;
            if source.read_lexeme_id()? == LexemeId::CLOSE {
                Ok(Rgb {
                    r,
                    g,
                    b,
                    a: Some(a),
                })
            } else {
                Err(invalid(source))
            }
        }
        _ => Err(invalid(source)),
    }
}

/// Skip the payload of a single non-structural lexeme.
fn skip_payload(source: &mut ParserSource<'_>, id: LexemeId) -> Result<(), Error> {
    match id {
        LexemeId::QUOTED | LexemeId::UNQUOTED => {
            source.read_bstr()?;
        }
        LexemeId::U32 | LexemeId::I32 | LexemeId::F32 => {
            source.take::<4>()?;
        }
        LexemeId::U64 | LexemeId::I64 | LexemeId::F64 => {
            source.take::<8>()?;
        }
        LexemeId::BOOL => {
            source.take::<1>()?;
        }
        LexemeId::RGB => {
            read_rgb(source)?;
        }
        LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => {
            source.take::<1>()?;
        }
        LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
            source.take::<2>()?;
        }
        LexemeId::LOOKUP_U24 => {
            source.take::<3>()?;
        }
        lexeme if (LexemeId::FIXED5_ZERO..=LexemeId::FIXED5_I56).contains(&lexeme) => {
            read_compact_f64(source, lexeme)?;
        }
        // Bare identifier tokens and equals carry no payload.
        _ => {}
    }
    Ok(())
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
            LexemeId::LOOKUP_U8 | LexemeId::LOOKUP_U8_ALT => Ok(source.take::<1>()?[0] as u32),
            LexemeId::LOOKUP_U16 | LexemeId::LOOKUP_U16_ALT => {
                Ok(u16::from_le_bytes(*source.take::<2>()?) as u32)
            }
            LexemeId::LOOKUP_U24 => {
                let b = source.take::<3>()?;
                Ok(u32::from_le_bytes([b[0], b[1], b[2], 0]))
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
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let (format, source) = cx.parts();
                let data = source.read_bstr()?;
                match format.flavor.decode(data) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
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
            LexemeId::RGB => visitor.visit_rgb(read_rgb(cx.source())?),
            LexemeId::LOOKUP_U8
            | LexemeId::LOOKUP_U8_ALT
            | LexemeId::LOOKUP_U16
            | LexemeId::LOOKUP_U16_ALT
            | LexemeId::LOOKUP_U24 => {
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
            | LexemeId::LOOKUP_U24 => visitor.visit_u32(format.read_lookup(source, id)?),
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
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
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
            LexemeId::QUOTED | LexemeId::UNQUOTED => visitor.visit_bytes(source.read_bstr()?),
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
    static PAYLOAD_SIZES: [u8; (LexemeId::FIXED5_I56.0 as usize) + 1] = {
        let mut t = [0u8; (LexemeId::FIXED5_I56.0 as usize) + 1];
        t[LexemeId::U32.0 as usize] = 4;
        t[LexemeId::I32.0 as usize] = 4;
        t[LexemeId::F32.0 as usize] = 4;
        t[LexemeId::BOOL.0 as usize] = 1;
        t[LexemeId::U64.0 as usize] = 8;
        t[LexemeId::I64.0 as usize] = 8;
        t[LexemeId::F64.0 as usize] = 8;
        t[LexemeId::LOOKUP_U8.0 as usize] = 1;
        t[LexemeId::LOOKUP_U8_ALT.0 as usize] = 1;
        t[LexemeId::LOOKUP_U16.0 as usize] = 2;
        t[LexemeId::LOOKUP_U16_ALT.0 as usize] = 2;
        t[LexemeId::LOOKUP_U24.0 as usize] = 3;
        t[LexemeId::FIXED5_ZERO.0 as usize] = 0;
        t[LexemeId::FIXED5_U8.0 as usize] = 1;
        t[LexemeId::FIXED5_U16.0 as usize] = 2;
        t[LexemeId::FIXED5_U24.0 as usize] = 3;
        t[LexemeId::FIXED5_U32.0 as usize] = 4;
        t[LexemeId::FIXED5_U40.0 as usize] = 5;
        t[LexemeId::FIXED5_U48.0 as usize] = 6;
        t[LexemeId::FIXED5_U56.0 as usize] = 7;
        t[LexemeId::FIXED5_I8.0 as usize] = 1;
        t[LexemeId::FIXED5_I16.0 as usize] = 2;
        t[LexemeId::FIXED5_I24.0 as usize] = 3;
        t[LexemeId::FIXED5_I32.0 as usize] = 4;
        t[LexemeId::FIXED5_I40.0 as usize] = 5;
        t[LexemeId::FIXED5_I48.0 as usize] = 6;
        t[LexemeId::FIXED5_I56.0 as usize] = 7;
        t
    };

    #[cold]
    fn single_step(source: &mut ParserSource<'_>, depth: &mut u32) -> Result<(), Error> {
        let id = source.read_lexeme_id()?;
        match id {
            LexemeId::OPEN => {
                *depth += 1;
                Ok(())
            }
            LexemeId::CLOSE => {
                *depth -= 1;
                Ok(())
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => Ok(source.read_bstr().map(|_| ())?),
            LexemeId::RGB => Ok(read_rgb(source)?).map(|_| ()),
            other => {
                let entry = PAYLOAD_SIZES.get(other.0 as usize).copied().unwrap_or(0);
                match entry {
                    0 => Ok(()),
                    1 => Ok(source.take::<1>().map(|_| ())?),
                    2 => Ok(source.take::<2>().map(|_| ())?),
                    3 => Ok(source.take::<3>().map(|_| ())?),
                    4 => Ok(source.take::<4>().map(|_| ())?),
                    5 => Ok(source.take::<5>().map(|_| ())?),
                    6 => Ok(source.take::<6>().map(|_| ())?),
                    7 => Ok(source.take::<7>().map(|_| ())?),
                    8 => Ok(source.take::<8>().map(|_| ())?),
                    _ => unreachable!(), // table only holds 0..=8 now
                }
            }
        }
    }

    let id = source.read_lexeme_id()?;
    if id == LexemeId::OPEN {
        // Save games contain large subtrees we don't care about (even in
        // pdx.tools EU4, skipping subtrees account for 30% of all IR), so this
        // path has been optimized.
        //
        // The fast loop walks the current window with a *local* cursor so that
        // skipping a token is pure register arithmetic. Round-tripping the
        // source pointer through memory on every token (load `ptr`/`end`, store
        // the advanced `ptr`) dominated this loop's instruction count.
        let mut depth = 1u32;
        loop {
            // Raw window pointer; the borrow ends with this statement so the
            // `source` can be mutated again below. We never mutate `source`
            // while reading through `base`, so the pointer stays valid.
            let base = source.window().as_ptr();
            let len = source.window_len();
            let mut i = 0usize;

            // A 10-byte tail margin guarantees that reading a lexeme id (2
            // bytes) plus its largest fixed payload (8 bytes) stays in bounds,
            // so the inner loop needs no per-token bounds checks.
            while i + 10 <= len {
                // SAFETY: `i + 2 <= len` from the loop condition.
                let id = LexemeId::new(u16::from_le_bytes(unsafe {
                    [*base.add(i), *base.add(i + 1)]
                }));
                match id {
                    LexemeId::OPEN => {
                        i += 2;
                        depth += 1;
                    }
                    LexemeId::CLOSE => {
                        i += 2;
                        depth -= 1;
                        if depth == 0 {
                            unsafe { source.advance_unchecked(i) };
                            return Ok(());
                        }
                    }
                    LexemeId::QUOTED | LexemeId::UNQUOTED => {
                        // Layout: [id:2][len:2][bytes:len]. The length is in
                        // bounds via the margin; the bytes may not be, in which
                        // case we defer to the slow path. Skipping strings
                        // inline (rather than breaking) is what keeps this loop
                        // out of a pathological refill: breaking mid-window
                        // forces `refill` to memmove the entire remaining
                        // window for every string in string-heavy subtrees.
                        let str_len =
                            u16::from_le_bytes(unsafe { [*base.add(i + 2), *base.add(i + 3)] })
                                as usize;
                        let total = 4 + str_len;
                        if i + total > len {
                            break;
                        }
                        i += total;
                    }
                    // Rare and variable-width; let the slow path handle it.
                    LexemeId::RGB => break,
                    _ => {
                        let payload = PAYLOAD_SIZES.get(id.0 as usize).copied().unwrap_or(0);
                        i += 2 + payload as usize;
                    }
                }
            }

            // Commit the bytes consumed by the fast loop, then take one careful
            // step across the window boundary (refilling as needed).
            unsafe { source.advance_unchecked(i) };
            source.refill()?;
            single_step(source, &mut depth)?;
            if depth == 0 {
                return Ok(());
            }
        }
    } else {
        skip_payload(source, id)
    }
}
