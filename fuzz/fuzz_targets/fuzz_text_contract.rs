#![no_main]
use libfuzzer_sys::fuzz_target;

#[path = "../../crates/jomini/tests/contract/mod.rs"]
mod contract;

// `TextDeserializer` gives the value of the syntax parser, or an error that
// names it, on documents that the fuzzer's bytes choose the structure of,
// and without excuses on documents without the constructs of the
// allow-list.
fuzz_target!(|data: &[u8]| {
    let doc = contract::generate(&mut contract::Choices::new(data));
    for (reader, verdict) in contract::check(&doc) {
        if let contract::Verdict::Broken { streaming, syntax }
        | contract::Verdict::Unhinted { streaming, syntax } = verdict
        {
            panic!(
                "{reader}: {:?}\n  TextDeserializer: {streaming}\n  syntax parser:    {syntax}",
                String::from_utf8_lossy(&doc)
            );
        }
    }

    let doc = contract::generate(&mut contract::Choices::strict(data));
    if contract::known_difference(&doc).is_some() {
        return;
    }
    for (reader, verdict) in contract::check(&doc) {
        assert!(
            matches!(
                verdict,
                contract::Verdict::Same | contract::Verdict::Hinted | contract::Verdict::SyntaxError
            ),
            "{reader}: {:?}: {verdict:?}",
            String::from_utf8_lossy(&doc)
        );
    }
});
