use crate::util::le_i32;

pub trait BinaryFlavor: Sized {
    fn visit_f32(&self, data: &[u8]) -> f32;
}

#[derive(Debug)]
pub struct DefaultFlavor;

impl BinaryFlavor for DefaultFlavor {
    fn visit_f32(&self, data: &[u8]) -> f32 {
        (le_i32(data) as f32) / 1000.0
    }
}