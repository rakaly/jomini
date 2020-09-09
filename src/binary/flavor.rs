use crate::{util::le_f32, util::le_i32, Encoding, Utf8Encoding, Windows1252Encoding};

/// Trait customizing decoding values from binary data
pub trait BinaryFlavor: Sized + Encoding {
    /// Decode a f32 from 4 bytes of data
    fn visit_f32_1(&self, data: &[u8]) -> f32;

    /// Decode a f32 from 8 bytes of data
    fn visit_f32_2(&self, data: &[u8]) -> f32;
}

impl<T: BinaryFlavor> BinaryFlavor for &'_ T {
    fn visit_f32_1(&self, data: &[u8]) -> f32 {
        (**self).visit_f32_1(data)
    }

    fn visit_f32_2(&self, data: &[u8]) -> f32 {
        (**self).visit_f32_2(data)
    }
}

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
    fn visit_f32_1(&self, data: &[u8]) -> f32 {
        // First encoding is an i32 that has a fixed point offset of 3 decimal digits
        (le_i32(data) as f32) / 1000.0
    }

    fn visit_f32_2(&self, data: &[u8]) -> f32 {
        // Second encoding is Q17.15 with 5 fractional digits
        // https://en.wikipedia.org/wiki/Q_(number_format)
        let val = le_i32(data) as f32 / 32768.0;
        (val * 10_0000.0).floor() / 10_0000.0
    }
}

/// The ck3 binary flavor
#[derive(Debug, Default)]
pub struct Ck3Flavor(Utf8Encoding);

impl Ck3Flavor {
    /// Creates a new ck3 flavor
    pub fn new() -> Self {
        Ck3Flavor(Utf8Encoding::new())
    }
}

impl Encoding for Ck3Flavor {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        self.0.decode(data)
    }
}

impl BinaryFlavor for Ck3Flavor {
    fn visit_f32_1(&self, data: &[u8]) -> f32 {
        le_f32(data)
    }

    fn visit_f32_2(&self, data: &[u8]) -> f32 {
        (le_i32(data) as f32) / 1000.0
    }
}
