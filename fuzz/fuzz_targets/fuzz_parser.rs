#![no_main]
use libfuzzer_sys::fuzz_target;
use pfars::TextParser;

fuzz_target!(|data: &[u8]| {
    let parser = TextParser::new(data);
    let mut count = 0;
    for _event in parser {
        count += 1;
    }

    assert!(count >= 0);
});
