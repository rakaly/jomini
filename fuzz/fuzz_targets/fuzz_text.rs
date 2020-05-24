#![no_main]
use libfuzzer_sys::fuzz_target;
use jomini::text::TextTape;

fuzz_target!(|data: &[u8]| {
    let _ = TextTape::from_slice(data);
});
