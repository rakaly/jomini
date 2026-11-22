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

/// The examples of the "Differences from `from_slice`" section of the
/// `parse_tape` docs. Each gives the JSON of `from_slice` (`None` when it
/// rejects the source), the JSON of `parse_tape`, and its error kinds.
#[cfg(all(feature = "syntax", feature = "json"))]
#[test]
fn parse_tape_differences_from_from_slice() {
    use jomini::text::syntax::{SyntaxErrorKind::*, parse_tape};

    let cases = [
        // No syntax error.
        ("OPERATOR = <=", None, r#"{"OPERATOR":"<="}"#, vec![]),
        ("a = 1 c", None, r#"{"a":1,"remainder":["c"]}"#, vec![]),
        (
            "@:define m = { x = 1 }",
            Some(r#"{"remainder":["@:define",{"m":{"x":1}}]}"#),
            "{}",
            vec![],
        ),
        ("a = @:insert m", None, r#"{"a":"@:insert m"}"#, vec![]),
        (
            "blockoverride = \"tab_glow\" { visible = no }",
            None,
            r#"{"blockoverride":{"tab_glow":{"visible":false}}}"#,
            vec![],
        ),
        // Errors of game files that keep their data.
        (
            "a = { b = 1 } } c = 2",
            Some(r#"{"a":{"b":1},"c":2}"#),
            r#"{"a":{"b":1},"c":2}"#,
            vec![UnmatchedCloseBrace],
        ),
        (
            "a = { b = 1",
            Some(r#"{"a":{"b":1}}"#),
            r#"{"a":{"b":1}}"#,
            vec![UnclosedBlock],
        ),
    ];
    for (source, from_slice, tape_json, tape_errors) in cases {
        let actual = TextTape::from_slice(source.as_bytes())
            .ok()
            .map(|tape| tape.windows1252_reader().json().to_string());
        assert_eq!(actual.as_deref(), from_slice, "from_slice of {source}");

        let (tape, errors) = parse_tape(source.as_bytes()).into_parts();
        let json = tape.windows1252_reader().json().to_string();
        assert_eq!(json, tape_json, "parse_tape of {source}");
        let kinds: Vec<_> = errors.iter().map(|e| e.kind).collect();
        assert_eq!(kinds, tape_errors, "parse_tape errors of {source}");
    }

    // The quoted name is the header of the block.
    let tape = parse_tape(b"blockoverride = \"tab_glow\" { visible = no }")
        .into_result()
        .unwrap();
    assert_eq!(
        tape.tokens()[1],
        TextToken::QuotedHeader(Scalar::new(b"tab_glow"))
    );
}
