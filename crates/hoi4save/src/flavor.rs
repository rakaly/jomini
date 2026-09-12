use jomini::{
    binary::{
        BinaryFlavor, BinaryFormat, BinaryFormatContext, FailedResolveStrategy, LexemeId,
        PdxVisitor, TokenResolver,
    },
    BinarySourceExt, Encoding, Error, Utf8Encoding,
};
use serde::de::Error as _;
use std::borrow::Cow;

const SAVE_VERSION_ID: u16 = 0x349d;

pub struct Hoi4Flavor;

impl Hoi4Flavor {
    pub(crate) fn decode_legacy_f32(raw: [u8; 4]) -> f32 {
        i32::from_le_bytes(raw) as f32 / 1_000.0
    }
    pub(crate) fn decode_modern_f64(raw: [u8; 8]) -> f64 {
        i64::from_le_bytes(raw) as f64 / 100_000.0
    }
    pub(crate) fn decode_f64(raw: [u8; 8]) -> f64 {
        let val = i64::from_le_bytes(raw) as f64 / 32768.0;
        (val * 100_000.0).floor() / 100_000.0
    }
}

impl Encoding for Hoi4Flavor {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}
impl BinaryFlavor for Hoi4Flavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        Self::decode_legacy_f32(data)
    }
    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        Self::decode_f64(data)
    }
}

/// Binary format shared by HOI4 deserialization and melting.
///
/// HOI4 changed token `F32` from a four-byte fixed-point value to an
/// eight-byte fixed-point value in save format version 30. This stateful
/// format observes `save_version` and transparently accepts either encoding.
pub struct Hoi4BinaryFormat<R> {
    resolver: R,
    failed_resolve_strategy: FailedResolveStrategy,
    pending_save_version: bool,
    save_version: i32,
}

impl<R: TokenResolver> Hoi4BinaryFormat<R> {
    pub fn new(resolver: R) -> Self {
        Self {
            resolver,
            failed_resolve_strategy: FailedResolveStrategy::Error,
            pending_save_version: false,
            save_version: 0,
        }
    }
    pub fn with_failed_resolve_strategy(mut self, strategy: FailedResolveStrategy) -> Self {
        self.failed_resolve_strategy = strategy;
        self
    }
    pub(crate) fn modern_f32(&self) -> bool {
        self.save_version >= 30
    }
    pub(crate) fn observe_key(&mut self, id: u16) {
        self.pending_save_version = id == SAVE_VERSION_ID;
    }
    pub(crate) fn observe_i32(&mut self, value: i32) {
        if self.pending_save_version {
            self.save_version = value;
            self.pending_save_version = false;
        }
    }

    fn resolve<'de, V: PdxVisitor<'de>>(&self, id: u16, visitor: V) -> Result<V::Value, Error> {
        match self.resolver.resolve(id) {
            Some(value) => visitor.visit_str(value),
            None => match self.failed_resolve_strategy {
                FailedResolveStrategy::Error => {
                    Err(Error::custom(format!("unknown field token 0x{id:x}")))
                }
                FailedResolveStrategy::Stringify => visitor.visit_string(format!("0x{id:x}")),
                FailedResolveStrategy::Ignore => {
                    visitor.visit_borrowed_str("__internal_identifier_ignore")
                }
            },
        }
    }

    fn dispatch<'de, V: PdxVisitor<'de>>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        id: LexemeId,
        visitor: V,
    ) -> Result<V::Value, Error> {
        match id {
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                let (format, source) = cx.parts();
                let data = source.read_bstr()?;
                match format.decode_scalar(data) {
                    Cow::Borrowed(x) => visitor.visit_str(x),
                    Cow::Owned(x) => visitor.visit_string(x),
                }
            }
            LexemeId::U32 => visitor.visit_u32(u32::from_le_bytes(*cx.source().take::<4>()?)),
            LexemeId::U64 => visitor.visit_u64(u64::from_le_bytes(*cx.source().take::<8>()?)),
            LexemeId::I32 => {
                let x = i32::from_le_bytes(*cx.source().take::<4>()?);
                cx.format_mut().observe_i32(x);
                visitor.visit_i32(x)
            }
            LexemeId::I64 => visitor.visit_i64(i64::from_le_bytes(*cx.source().take::<8>()?)),
            LexemeId::BOOL => visitor.visit_bool(cx.source().take::<1>()?[0] != 0),
            LexemeId::F32 => {
                if cx.format().modern_f32() {
                    visitor.visit_f64(Hoi4Flavor::decode_modern_f64(*cx.source().take::<8>()?))
                } else {
                    visitor.visit_f32(Hoi4Flavor::decode_legacy_f32(*cx.source().take::<4>()?))
                }
            }
            LexemeId::F64 => visitor.visit_f64(Hoi4Flavor::decode_f64(*cx.source().take::<8>()?)),
            LexemeId::OPEN => cx.visit_open_seq(visitor),
            LexemeId::CLOSE | LexemeId::EQUAL => Err(Error::custom("unexpected structural token")),
            LexemeId(id) => cx.format().resolve(id, visitor),
        }
    }
}

impl<R: TokenResolver> BinaryFormat for Hoi4BinaryFormat<R> {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
    fn on_key(&mut self, id: LexemeId) {
        self.observe_key(id.0);
    }
    fn skip_value(cx: &mut BinaryFormatContext<'_, '_, Self>) -> Result<(), Error> {
        let id = cx.source().read_lexeme_id()?;
        match id {
            LexemeId::OPEN => {
                let mut depth = 1;
                while depth != 0 {
                    match cx.source().read_lexeme_id()? {
                        LexemeId::OPEN => depth += 1,
                        LexemeId::CLOSE => depth -= 1,
                        LexemeId::QUOTED | LexemeId::UNQUOTED => {
                            cx.source().read_bstr()?;
                        }
                        LexemeId::I32 | LexemeId::U32 => {
                            cx.source().take::<4>()?;
                        }
                        LexemeId::F32 => {
                            if cx.format().modern_f32() {
                                cx.source().take::<8>()?;
                            } else {
                                cx.source().take::<4>()?;
                            }
                        }
                        LexemeId::I64 | LexemeId::U64 | LexemeId::F64 => {
                            cx.source().take::<8>()?;
                        }
                        LexemeId::BOOL => {
                            cx.source().take::<1>()?;
                        }
                        _ => {}
                    }
                }
            }
            LexemeId::QUOTED | LexemeId::UNQUOTED => {
                cx.source().read_bstr()?;
            }
            LexemeId::I32 => {
                let value = i32::from_le_bytes(*cx.source().take::<4>()?);
                cx.format_mut().observe_i32(value);
            }
            LexemeId::U32 => {
                cx.source().take::<4>()?;
            }
            LexemeId::F32 => {
                if cx.format().modern_f32() {
                    cx.source().take::<8>()?;
                } else {
                    cx.source().take::<4>()?;
                }
            }
            LexemeId::I64 | LexemeId::U64 | LexemeId::F64 => {
                cx.source().take::<8>()?;
            }
            LexemeId::BOOL => {
                cx.source().take::<1>()?;
            }
            _ => {}
        }
        Ok(())
    }
    fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error> {
        let id = cx.source().read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }
    fn deserialize_str<'de, V: PdxVisitor<'de>>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error> {
        let id = cx.source().read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }
    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        cx: &mut BinaryFormatContext<'_, 'de, Self>,
        visitor: V,
    ) -> Result<V::Value, Error> {
        let id = cx.source().read_lexeme_id()?;
        Self::dispatch(cx, id, visitor)
    }
}
