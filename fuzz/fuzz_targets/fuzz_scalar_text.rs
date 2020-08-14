#![no_main]
use jomini::Scalar;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let scalar = Scalar::new(data);
    let _ = scalar.to_utf8();
    let _ = scalar.to_f64();
    let _ = scalar.to_u64();
    let _ = scalar.to_i64();
    let _ = scalar.is_ascii();
    let _ = scalar.to_bool();
});
