#![no_main]
use jomini::Scalar;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let scalar = Scalar::new(data);
    let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(data);
    assert_eq!(scalar.to_utf8(), cow);
    let _ = scalar.to_f64();
    let _ = scalar.to_u64();
    let _ = scalar.to_i64();
    let _ = scalar.is_ascii();
    let _ = scalar.to_bool();
});
