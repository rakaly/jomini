#![no_main]
use jomini::{
    TextToken,
    text::{ObjectReader, ValueReader, lossless},
};
use libfuzzer_sys::fuzz_target;

fn read_value(value: ValueReader<jomini::Windows1252Encoding>) {
    match value.token() {
        TextToken::Object { .. } => read_object(value.read_object().unwrap()),
        TextToken::Array { .. } => {
            for value in value.read_array().unwrap().values() {
                read_value(value);
            }
        }
        TextToken::End(_) => panic!("end!?"),
        TextToken::Operator(_) | TextToken::MixedContainer => {}
        _ => {
            let _ = value.read_str().unwrap();
        }
    }
}

fn read_object(reader: ObjectReader<jomini::Windows1252Encoding>) {
    for (key, _op, value) in reader.fields() {
        let _ = key.read_str();
        read_value(value);
    }
}

fn collect_text(node: lossless::SyntaxNode, out: &mut Vec<u8>) {
    for element in node.children() {
        match element {
            lossless::SyntaxElement::Token(token) => out.extend_from_slice(token.text()),
            lossless::SyntaxElement::Node(node) => collect_text(node, out),
        }
    }
}

fn read_items<'t, 'a>(items: impl Iterator<Item = lossless::Item<'t, 'a>>) {
    for item in items {
        let lossless::Item::Field(field) = item else {
            continue;
        };
        let _ = field.key().as_scalar();
        let _ = field.op();
        let Some(value) = field.value() else {
            continue;
        };
        let _ = (value.to_f64(), value.to_i64(), value.to_bool());
        if let Some(block) = value.as_block() {
            read_items(block.items());
        }
    }
}

fn significant(source: &[u8], flavor: lossless::Flavor) -> Vec<Vec<u8>> {
    lossless::parse_with(source, flavor)
        .significant_tokens()
        .filter(|t| !t.kind().is_missing())
        .map(|t| t.text().to_vec())
        .collect()
}

fn check_flavor(data: &[u8], flavor: lossless::Flavor) {
    // The tree keeps every byte of the source.
    let tree = lossless::parse_with(data, flavor);
    let mut text = Vec::with_capacity(data.len());
    collect_text(tree.root(), &mut text);
    assert_eq!(text, data);

    // The parent of each token covers the token, and the leaf at each token
    // start is the token.
    for token in tree.tokens() {
        if let Some(parent) = token.parent() {
            assert!(parent.text_range().contains_range(token.text_range()));
        }
        if !token.text_range().is_empty() {
            let at = tree.token_at_offset(token.text_range().start());
            assert_eq!(at.right_biased(), Some(token));
            assert_eq!(tree.covering_element(token.text_range()).text_range(), token.text_range());
        }
    }

    // Leaf steps walk the same leaves as `tokens()`.
    let root = tree.root();
    let forward = std::iter::successors(root.first_token(), |t| t.next_token());
    assert!(forward.eq(tree.tokens()));

    // A pre-order walk enters each node once, in the order of `descendants()`.
    let entered = root.preorder().filter_map(|event| match event {
        lossless::WalkEvent::Enter(node) => Some(node),
        lossless::WalkEvent::Leave(_) => None,
    });
    assert!(entered.eq(root.descendants()));

    read_items(tree.ast().items());

    let index = lossless::LineIndex::new(data);
    for error in tree.errors() {
        let at = index.line_col(error.range.start());
        assert_eq!(index.offset(at), Some(error.range.start()));
        if let Some(delimiter) = &error.recovery {
            let _ = tree.repair_applicability(delimiter);
        }
    }
    let _ = tree.repaired_source();

    // The formatter keeps the significant tokens, and a second pass does not
    // change the output.
    let formatted = tree.format(&lossless::FormatOptions::default());
    assert_eq!(significant(data, flavor), significant(&formatted, flavor));
    let again = lossless::parse_with(&formatted[..], flavor).format(&Default::default());
    assert_eq!(formatted, again);
}

fuzz_target!(|data: &[u8]| {
    check_flavor(data, lossless::Flavor::default());
    check_flavor(data, lossless::Flavor::hoi4());

    // The lossless parser does not have to write the same tape as `TextTape`
    // for malformed input, but the tape must always be valid.
    let tape = lossless::parse_tape(data);
    let tokens = tape.tokens();
    for (i, token) in tokens.iter().enumerate() {
        match token {
            TextToken::Array { end, .. } | TextToken::Object { end, .. } => {
                assert!(*end > i, "container ends before it starts");
                assert_eq!(tokens[*end], TextToken::End(i));
            }
            TextToken::End(start) => assert!(*start < i, "end before its container"),
            _ => {}
        }
    }

    read_object(tape.windows1252_reader());
});
