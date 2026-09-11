use jomini::{Scalar, TextTape, TextToken, text::Operator};

#[test]
fn test_greater_than_operator() {
    let data = b"age > 16";
    let tape = TextTape::from_slice(&data[..]).unwrap();
    assert_eq!(
        tape.tokens(),
        vec![
            TextToken::Unquoted(Scalar::new(b"age")),
            TextToken::Operator(Operator::GreaterThan),
            TextToken::Unquoted(Scalar::new(b"16")),
        ]
    );
}
