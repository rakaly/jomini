#![cfg(all(feature = "syntax", feature = "derive"))]

//! The wrapper types and owned values through `TextDeserializer` give what
//! the syntax parser gives, including in the remainder of an object, and
//! through a `TextTapeDeserializer` where it reads the data the same.

mod contract;

use contract::{AnyValue, Verdict};
use jomini::text::de::TextTapeDeserializer;
use jomini::text::syntax::{self, OwnedBlock, OwnedValue};
use jomini::text::{Headered, MaybeQuoted, Operator, Property};
use jomini::{TextDeserializer, Windows1252Encoding};
use serde::Deserialize;
use serde::de::DeserializeOwned;
use std::fmt;

/// Inputs of the wrapper types, from a review of them.
const CASES: &[&[u8]] = &[
    b"a = { 1 2 3 }",
    b"a = rgb { 1 2 3 }",
    b"a = rgb { }",
    b"a = rgb # c\n { 1 }",
    b"a > { 1 2 }",
    b"a > rgb { 1 2 }",
    b"a = { b > rgb { 1 } }",
    b"a = { x = 1 c d e = f }",
    b"a = { x = 1 c d e < f }",
    b"a = { x = 1 c rgb { 1 } \"q\" }",
    b"a = { x = 1 c d e > { f } }",
    b"a = \"q x\"",
    b"a = \"q \\\" x\"",
    b"a = \"q } x\" b = 2",
    b"a = @v",
    b"a = @[ 1 + 2 ]",
    b"a = { @[ 1 + 2 ] 4 }",
    b"@v = 3 a = @v",
    b"\n= 1\ns = 2",
    b"a = {\n= 1\ns = 2 }",
    b"a = { {} 1 2 }",
    b"a = { { } { a = 1 } }",
    b"a = { a = 1 } }",
    b"a = { 1 2 3",
    b"income_taxes={ 80 0=278.75 1=433.88 }",
    b"a = { 80 0=278.75 1=433.88 }",
    b"a = rgb { 1 2 } b = hsv { 0.1 0.2 0.3 }",
    b"a = x { a = 1 } b = 2",
    b"a = { a = { a = { a = { a = { 1 } } } } }",
    b"a=rgb{1 2 3}b=2",
    b"\xef\xbb\xbfa = { 1 }",
    b"a = { 1 } b c",
    b"a = rgb { a = 1 }",
    b"t = { pattern = list \"faction_patterns_list\" color1 = red }",
    b"x = { 1 c d e = f } y = 2",
    b"c d e = f",
];

#[test]
fn wrapper_cases() {
    for data in CASES {
        for (reader, verdict) in contract::check(data) {
            assert!(
                !matches!(verdict, Verdict::Broken { .. } | Verdict::Unhinted { .. }),
                "{reader}: {:?}: {verdict:?}",
                String::from_utf8_lossy(data)
            );
        }
    }
}

fn show<T: fmt::Debug, E: fmt::Display>(result: &Result<T, E>) -> String {
    match result {
        Ok(x) => format!("Ok {x:?}"),
        Err(e) => format!(
            "Err {}",
            e.to_string().split(" (offset").next().unwrap_or("")
        ),
    }
}

/// The slice, the readers whose buffer holds the data, and the tree give
/// the same value.
fn same<T: DeserializeOwned + fmt::Debug>(data: &[u8]) -> T {
    let encoding = Windows1252Encoding::new();
    let tree = syntax::parse(data).deserialize::<T, _>(encoding).unwrap();
    let slice = TextDeserializer::from_slice(data, encoding).deserialize::<T>();
    assert_eq!(show(&slice), show::<_, String>(&Ok(&tree)));
    for size in [data.len() + 1, 64, 4096] {
        let reader = TextDeserializer::from_reader_with_buf(data, encoding, vec![0; size])
            .deserialize::<T>();
        assert_eq!(show(&reader), show(&slice), "buffer size {size}");
    }
    tree
}

#[derive(Debug, Deserialize)]
struct RemainderOwned {
    #[allow(dead_code)]
    x: i32,
    remainder: OwnedValue,
}

#[derive(Debug, Deserialize)]
struct RemainderBlock {
    #[allow(dead_code)]
    x: i32,
    remainder: OwnedBlock,
}

#[derive(Debug, Deserialize)]
struct RemainderQuoted {
    #[allow(dead_code)]
    x: i32,
    remainder: Vec<MaybeQuoted<String>>,
}

#[derive(Debug, Deserialize)]
struct Outer<T> {
    a: T,
}

/// The remainder of an object as an owned value is the block of its parts,
/// with the fields in it, as the syntax parser gives it.
#[test]
fn remainder_owned_value() {
    let data = b"a = { x = 1 c d e = f }";
    let value = same::<Outer<RemainderOwned>>(data).a.remainder;
    let OwnedValue::Block(block) = value else {
        panic!("{value:?}");
    };
    let mut expected = OwnedBlock::new();
    expected.push(OwnedValue::unquoted("c"));
    expected.push(OwnedValue::unquoted("d"));
    expected.push(syntax::OwnedField::new(
        "e",
        Some(Operator::Equal),
        OwnedValue::unquoted("f"),
    ));
    assert_eq!(block, expected);

    let block = same::<Outer<RemainderBlock>>(b"a = { x = 1 c d e > { f } }").a;
    assert_eq!(block.remainder.items().len(), 3);
    same::<Outer<RemainderOwned>>(b"a = { x = 1 c rgb { 1 } \"q\" } b = 2");
}

/// The operators of a remainder read into `MaybeQuoted` are unquoted
/// scalars, as the syntax parser gives them.
#[test]
fn remainder_operator_wrappers() {
    let parts = same::<Outer<RemainderQuoted>>(b"a = { x = 1 c d e = f }")
        .a
        .remainder;
    let unquoted = |x: &str| MaybeQuoted::Unquoted(String::from(x));
    assert_eq!(parts, ["c", "d", "e", "=", "f"].map(unquoted).to_vec());
}

/// A `TextTapeDeserializer` keeps the operator of a field for the block of
/// a header, as the tree and `TextDeserializer` do.
#[test]
fn tape_headered_property() {
    type Value = Headered<Property<Vec<u8>>>;
    let data = b"a > rgb { 1 2 }";
    let expected: Outer<Value> = same(data);
    let tape = TextTapeDeserializer::from_windows1252_slice(data)
        .unwrap()
        .deserialize::<Outer<Value>>()
        .unwrap();
    assert_eq!(tape.a, expected.a);
    assert_eq!(tape.a.value().operator(), Operator::GreaterThan);
}

/// Generic values through every wrapper, as in the coats of arms of CK3.
#[test]
fn wrapped_any() {
    let data = b"t = { pattern = list \"faction_patterns_list\" color1 = red }";
    same::<contract::Fields<contract::Wrapped>>(data);
    same::<contract::Fields<MaybeQuoted<Headered<AnyValue>>>>(data);
}
