use jomini::{binary::BinaryFlavor, Encoding, Utf8Encoding};

#[derive(Debug, Default, Clone, Copy)]
pub struct ImperatorFlavor;

impl ImperatorFlavor {
    pub fn new() -> Self {
        ImperatorFlavor
    }
}

impl Encoding for ImperatorFlavor {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

impl BinaryFlavor for ImperatorFlavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_bits(u32::from_le_bytes(data))
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        i64::from_le_bytes(data) as f64 / 100000.0
    }
}
