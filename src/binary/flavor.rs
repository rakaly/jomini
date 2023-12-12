#[cfg(feature = "derive")]
use crate::{
    binary::{de::BinaryDeserializerBuilder, TokenResolver},
    BinaryDeserializer, Error,
};

/// Trait customizing decoding values from binary data
///
/// How binary data is encoded differs between games and even
/// between patches!
pub trait BinaryFlavor: crate::Encoding {
    /// Decode a f32 from 4 bytes of data
    fn visit_f32(&self, data: [u8; 4]) -> f32;

    /// Decode a f64 from 8 bytes of data
    fn visit_f64(&self, data: [u8; 8]) -> f64;

    #[cfg(feature = "derive")]
    fn deserializer(&self) -> BinaryDeserializerBuilder<&Self> {
        BinaryDeserializer::builder_flavor(self)
    }

    #[cfg(feature = "derive")]
    fn deserialize_slice<'de, 'res: 'de, T, RES>(
        &self,
        data: &'de [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: serde::de::Deserialize<'de>,
        RES: TokenResolver,
    {
        self.deserializer().deserialize_slice(data, resolver)
    }
}

impl<T: BinaryFlavor + ?Sized> BinaryFlavor for &'_ T {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        (**self).visit_f32(data)
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        (**self).visit_f64(data)
    }
}

impl<T: BinaryFlavor + ?Sized> BinaryFlavor for Box<T> {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        (**self).visit_f32(data)
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        (**self).visit_f64(data)
    }
}
