#![no_main]
use libfuzzer_sys::fuzz_target;
use pfars::Scalar;

fuzz_target!(|data: &[u8]| {
    let scalar = Scalar::new(data);
    let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(data);
    assert_eq!(scalar.to_utf8(), cow);
});
