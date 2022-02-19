use jomini::{BinaryTape, Operator, Scalar, TextTape, TextToken};

#[test]
fn reject_bin_obj_in_hidden_obj() {
    let data = include_bytes!("./fixtures/nested-hidden-obj.bin");
    assert!(BinaryTape::from_slice(&data[..]).is_err());
}

#[test]
fn reject_txt_obj_in_hidden_obj() {
    let data = include_bytes!("./fixtures/nested-hidden-obj.txt");
    assert!(TextTape::from_slice(&data[..]).is_err());
}

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
