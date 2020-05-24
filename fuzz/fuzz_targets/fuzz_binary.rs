#![no_main]
use libfuzzer_sys::fuzz_target;
use jomini::binary::BinTape;

fuzz_target!(|data: &[u8]| {
    let _ = BinTape::from_slice(data);
});
