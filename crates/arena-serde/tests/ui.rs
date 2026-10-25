// trybuild runs cargo, which Miri and wasm cannot do.
#[test]
#[cfg_attr(any(miri, target_family = "wasm"), ignore)]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs");
}
