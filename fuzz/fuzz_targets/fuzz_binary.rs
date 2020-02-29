#![no_main]
use libfuzzer_sys::fuzz_target;
use pfars::BinaryParser;

fuzz_target!(|data: &[u8]| {
    let mut parser = BinaryParser::new();
    for event in parser.events(data) {
        if event.is_err() {
            return;
        }
    }
});
