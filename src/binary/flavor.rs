use crate::BinaryDeserializer;

use super::de::BinaryDeserializerBuilder;

/// Trait customizing decoding values from binary data
///
/// How binary data is encoded differs between games and even
/// between patches!
pub trait BinaryFlavor: crate::Encoding {
    /// Decode a f32 from 4 bytes of data
    fn visit_f32(&self, data: [u8; 4]) -> f32;

    /// Decode a f64 from 8 bytes of data
    fn visit_f64(&self, data: [u8; 8]) -> f64;

    fn deserializer(self) -> BinaryDeserializerBuilder<Self> where Self: Sized {
        BinaryDeserializer::builder_flavor(self)
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
