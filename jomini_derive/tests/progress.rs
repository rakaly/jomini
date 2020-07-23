#[test]
fn tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/01-parse.rs");
    t.pass("tests/02-create-deserializer.rs");
    t.pass("tests/03-duplicated.rs");
    t.pass("tests/04-default.rs");
    t.pass("tests/05-default-fn.rs");
    t.pass("tests/06-alias.rs");
    t.pass("tests/07-multiple.rs");
    t.pass("tests/08-duplicated-numbers.rs");
    t.pass("tests/09-deserialize-with.rs");
    t.pass("tests/10-options.rs");
}
