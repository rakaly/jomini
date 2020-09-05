use crate::util::le_i32;

pub trait BinaryFlavor: Sized {
    fn visit_f32(&self, data: &[u8]) -> f32;
    fn visit_q16(&self, data: &[u8]) -> f32;
}

#[derive(Debug)]
pub struct DefaultFlavor;

impl BinaryFlavor for DefaultFlavor {
    fn visit_f32(&self, data: &[u8]) -> f32 {
        (le_i32(data) as f32) / 1000.0
    }

    fn visit_q16(&self, data: &[u8]) -> f32 {
        let mut val = le_i32(data) as f32;
        val = val * 2.0 / 65536.0 * 100_000.0;
        val.floor() / 100_000.0
    }
}