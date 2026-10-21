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

fuzz_target!(|data: &[u8]| {
    // The tree keeps every byte of the source.
    let tree = lossless::parse(data);
    let mut text = Vec::with_capacity(data.len());
    collect_text(tree.root(), &mut text);
    assert_eq!(text, data);

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
