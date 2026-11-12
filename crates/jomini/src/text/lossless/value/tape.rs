//! Makes an owned value from the tokens of a [`TextTape`](crate::TextTape).
//!
//! The tape keeps the quotes, the operators that are not `=`, the headers,
//! and the containers that have both fields and bare values. It does not
//! keep the difference between `key = { ... }` and `key { ... }`, so each
//! field of the tape has an operator.

use super::{OwnedBlock, OwnedField, OwnedItem, OwnedParameter, OwnedScalar, OwnedValue};
use crate::{Encoding, TextToken, text::Operator};

/// The value at `index`.
pub(crate) fn value<E: Encoding>(tokens: &[TextToken], index: usize, encoding: &E) -> OwnedValue {
    let text = |s: &crate::Scalar| encoding.decode(s.as_bytes()).into_owned();
    match &tokens[index] {
        TextToken::Array { end, .. } => {
            OwnedValue::Block(block(tokens, index + 1, *end, false, encoding))
        }
        TextToken::Object { end, .. } => {
            OwnedValue::Block(block(tokens, index + 1, *end, true, encoding))
        }
        TextToken::Header(s) => {
            let header = unquoted(text(s));
            match value(tokens, index + 1, encoding) {
                OwnedValue::Block(body) => OwnedValue::HeaderedBlock(header, body),
                _ => OwnedValue::Scalar(header),
            }
        }
        TextToken::Quoted(s) => OwnedValue::Scalar(OwnedScalar::Quoted(text(s))),
        TextToken::Unquoted(s) | TextToken::Parameter(s) | TextToken::UndefinedParameter(s) => {
            let text = text(s);
            if text.starts_with("@[") {
                OwnedValue::Calc(text)
            } else {
                OwnedValue::Scalar(unquoted(text))
            }
        }
        TextToken::Operator(op) => OwnedValue::unquoted(op.symbol()),
        TextToken::MixedContainer | TextToken::End(_) => OwnedValue::Block(OwnedBlock::new()),
    }
}

/// The scalar of a key.
pub(crate) fn key<E: Encoding>(token: &TextToken, encoding: &E) -> OwnedScalar {
    match token {
        TextToken::Quoted(s) => OwnedScalar::Quoted(encoding.decode(s.as_bytes()).into_owned()),
        token => match token.as_scalar() {
            Some(s) => unquoted(encoding.decode(s.as_bytes()).into_owned()),
            None => OwnedScalar::Unquoted(String::new()),
        },
    }
}

/// An unquoted text, or a variable when the text is `@name`.
pub(crate) fn unquoted(text: String) -> OwnedScalar {
    let variable = text
        .as_bytes()
        .get(1)
        .is_some_and(|&b| b.is_ascii_alphanumeric() || b == b'_');
    if text.starts_with('@') && variable {
        OwnedScalar::Variable(text)
    } else {
        OwnedScalar::Unquoted(text)
    }
}

/// The index after the value at `index`.
fn next(tokens: &[TextToken], index: usize) -> usize {
    match tokens[index] {
        TextToken::Array { end, .. } | TextToken::Object { end, .. } => end + 1,
        TextToken::Header(_) if index + 1 < tokens.len() => next(tokens, index + 1),
        _ => index + 1,
    }
}

/// The entries from `start` to `end`.
///
/// In an object, each entry is a key, an optional operator, and a value.
/// In an array, each entry is a value. After a `MixedContainer` token,
/// an entry is a field when an operator follows its first token, and each
/// operator is in the tape, also `=`.
pub(crate) fn block<E: Encoding>(
    tokens: &[TextToken],
    start: usize,
    end: usize,
    object: bool,
    encoding: &E,
) -> OwnedBlock {
    let mut items = Vec::new();
    let mut flat = false;
    let mut index = start;
    while index < end {
        let token = &tokens[index];
        match token {
            TextToken::MixedContainer => {
                flat = true;
                index += 1;
            }
            TextToken::End(_) => index += 1,
            TextToken::Parameter(s) | TextToken::UndefinedParameter(s) if index + 1 < end => {
                let body = match &tokens[index + 1] {
                    TextToken::Array { end, .. } | TextToken::Object { end, .. } => {
                        block(tokens, index + 2, *end, true, encoding)
                    }
                    _ => {
                        OwnedBlock::from(vec![OwnedItem::Value(value(tokens, index + 1, encoding))])
                    }
                };
                let name = encoding.decode(s.as_bytes()).into_owned();
                let undefined = matches!(token, TextToken::UndefinedParameter(_));
                items.push(OwnedItem::Parameter(OwnedParameter::new(
                    name, undefined, body,
                )));
                index = next(tokens, index + 1);
            }
            _ => {
                let op = match tokens.get(index + 1) {
                    Some(TextToken::Operator(op)) if index + 2 < end => Some((*op, index + 2)),
                    _ if object && !flat && index + 1 < end => Some((Operator::Equal, index + 1)),
                    _ => None,
                };
                match op {
                    Some((op, at)) => {
                        let value = value(tokens, at, encoding);
                        let field = OwnedField::new(key(token, encoding), Some(op), value);
                        items.push(OwnedItem::Field(field));
                        index = next(tokens, at);
                    }
                    None => {
                        items.push(OwnedItem::Value(value(tokens, index, encoding)));
                        index = next(tokens, index);
                    }
                }
            }
        }
    }
    OwnedBlock::from(items)
}
