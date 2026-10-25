// trybuild runs cargo, which Miri and wasm cannot do.
#[test]
#[cfg_attr(any(miri, target_family = "wasm"), ignore)]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-parse.rs");
    t.compile_fail("tests/compile-fail/*.rs");
}
