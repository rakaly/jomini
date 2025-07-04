#[cfg(feature = "derive")]
use crate::{
    binary::{de::BinaryDeserializerBuilder, TokenResolver},
    Error,
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

    /// Create binary deserializer from this binary flavor
    #[cfg(feature = "derive")]
    fn deserializer(&self) -> BinaryDeserializerBuilder<&Self>
    where
        Self: Sized,
    {
        BinaryDeserializerBuilder::with_flavor(self)
    }

    /// Deserialize value from slice of data with this binary flavor
    #[cfg(feature = "derive")]
    fn deserialize_slice<'de, 'res: 'de, T, RES>(
        &self,
        data: &'de [u8],
        resolver: &'res RES,
    ) -> Result<T, Error>
    where
        T: serde::de::Deserialize<'de>,
        RES: TokenResolver,
        Self: Sized,
    {
        self.deserializer().deserialize_slice(data, resolver)
    }

    /// Deserialize value from stream of data with this binary flavor
    #[cfg(feature = "derive")]
    fn deserialize_reader<T, RES, R>(&self, reader: R, resolver: &RES) -> Result<T, Error>
    where
        T: serde::de::DeserializeOwned,
        RES: TokenResolver,
        R: std::io::Read,
        Self: Sized,
    {
        self.deserializer().deserialize_reader(reader, resolver)
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
