//! Makes an owned value from the tokens of a [`TextTape`](crate::TextTape).
//!
//! The tape keeps the quotes, the operators that are not `=`, the headers,
//! and the containers that have both fields and bare values. It does not
//! keep the difference between `key = { ... }` and `key { ... }`, so each
//! field of the tape has an operator.
//!
//! The builder keeps the open blocks in a list, not on the stack, so a block
//! can nest as deep as the tape allows.

use super::{
    Becomes, OwnedBlock, OwnedField, OwnedItem, OwnedParameter, OwnedScalar, OwnedValue,
    block_value, owned_scalar,
};
use crate::{Encoding, TextToken, text::Operator};

/// The value at `index`.
pub(crate) fn value<E: Encoding>(tokens: &[TextToken], index: usize, encoding: &E) -> OwnedValue {
    match start(tokens, index, encoding) {
        Ok(value) => value,
        Err((header, block)) => {
            let block = build(tokens, block.frame(Becomes::Result), encoding);
            block_value(header, block)
        }
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
    let block = Open { start, end, object };
    build(tokens, block.frame(Becomes::Result), encoding)
}

/// The scalar of a key.
pub(crate) fn key<E: Encoding>(token: &TextToken, encoding: &E) -> OwnedScalar {
    match token.as_scalar() {
        Some(s) => owned_scalar(
            s.as_bytes(),
            matches!(token, TextToken::Quoted(_)),
            encoding,
        ),
        None => OwnedScalar::Unquoted(String::new()),
    }
}

/// The entries of a block: the tokens from `start` to `end`.
struct Open {
    start: usize,
    end: usize,
    object: bool,
}

impl Open {
    /// The block that the token at `index` opens.
    fn at(token: &TextToken, index: usize) -> Option<Open> {
        let (end, object) = match *token {
            TextToken::Array { end, .. } => (end, false),
            TextToken::Object { end, .. } => (end, true),
            _ => return None,
        };
        Some(Open {
            start: index + 1,
            end,
            object,
        })
    }

    fn frame(self, into: Becomes) -> Frame {
        Frame {
            items: Vec::new(),
            index: self.start,
            end: self.end,
            object: self.object,
            flat: false,
            into,
        }
    }
}

/// The value at `index` when it is not a block. Else the block that it
/// opens, with the header of a headered block.
fn start<E: Encoding>(
    tokens: &[TextToken],
    index: usize,
    encoding: &E,
) -> Result<OwnedValue, (Option<OwnedScalar>, Open)> {
    let token = &tokens[index];
    if let Some(block) = Open::at(token, index) {
        return Err((None, block));
    }
    Ok(match token {
        TextToken::Header(s) | TextToken::QuotedHeader(s) => {
            let quoted = matches!(token, TextToken::QuotedHeader(_));
            let header = owned_scalar(s.as_bytes(), quoted, encoding);
            let body = tokens
                .get(index + 1)
                .and_then(|next| Open::at(next, index + 1));
            match body {
                Some(block) => return Err((Some(header), block)),
                None => OwnedValue::Scalar(header),
            }
        }
        TextToken::Quoted(s) => OwnedValue::Scalar(owned_scalar(s.as_bytes(), true, encoding)),
        TextToken::Unquoted(s) | TextToken::Parameter(s) | TextToken::UndefinedParameter(s) => {
            if s.as_bytes().starts_with(b"@[") {
                OwnedValue::Calc(encoding.decode(s.as_bytes()).into_owned())
            } else {
                OwnedValue::Scalar(owned_scalar(s.as_bytes(), false, encoding))
            }
        }
        TextToken::Operator(op) => OwnedValue::unquoted(op.symbol()),
        TextToken::Array { .. }
        | TextToken::Object { .. }
        | TextToken::MixedContainer
        | TextToken::End(_) => OwnedValue::Block(OwnedBlock::new()),
    })
}

/// The index after the value at `index`.
fn next(tokens: &[TextToken], mut index: usize) -> usize {
    loop {
        match tokens[index] {
            TextToken::Array { end, .. } | TextToken::Object { end, .. } => return end + 1,
            TextToken::Header(_) | TextToken::QuotedHeader(_) if index + 1 < tokens.len() => {
                index += 1
            }
            _ => return index + 1,
        }
    }
}

/// A block that the builder has opened, and the items it has made so far.
struct Frame {
    items: Vec<OwnedItem>,
    /// The token of the next entry.
    index: usize,
    end: usize,
    object: bool,
    /// The block has had a `MixedContainer` token.
    flat: bool,
    into: Becomes,
}

/// Build the block of `root`.
fn build<E: Encoding>(tokens: &[TextToken], root: Frame, encoding: &E) -> OwnedBlock {
    let mut open = vec![root];
    loop {
        let top = open.last_mut().expect("the result block is open");
        let index = top.index;
        if index >= top.end {
            let done = open.pop().expect("the result block is open");
            let block = OwnedBlock::from(done.items);
            let item = match done.into {
                Becomes::Result => return block,
                Becomes::Field { key, op, header } => {
                    OwnedItem::Field(OwnedField::new(key, op, block_value(header, block)))
                }
                Becomes::Value { header } => OwnedItem::Value(block_value(header, block)),
                Becomes::Parameter { name, undefined } => {
                    OwnedItem::Parameter(OwnedParameter::new(name, undefined, block))
                }
            };
            open.last_mut()
                .expect("an inner block has an enclosing block")
                .items
                .push(item);
            continue;
        }

        let token = &tokens[index];
        match token {
            TextToken::MixedContainer => {
                top.flat = true;
                top.index += 1;
            }
            TextToken::End(_) => top.index += 1,
            TextToken::Parameter(s) | TextToken::UndefinedParameter(s) if index + 1 < top.end => {
                top.index = next(tokens, index + 1);
                let into = Becomes::Parameter {
                    name: encoding.decode(s.as_bytes()).into_owned(),
                    undefined: matches!(token, TextToken::UndefinedParameter(_)),
                };
                if let Some(body) = Open::at(&tokens[index + 1], index + 1) {
                    let body = Open {
                        object: true,
                        ..body
                    };
                    open.push(body.frame(into));
                    continue;
                }

                // The body is one value. The body has no tokens of its own,
                // so it is complete after the value.
                let mut body = Open {
                    start: 0,
                    end: 0,
                    object: true,
                }
                .frame(into);
                match start(tokens, index + 1, encoding) {
                    Ok(value) => {
                        body.items.push(OwnedItem::Value(value));
                        open.push(body);
                    }
                    Err((header, block)) => {
                        open.push(body);
                        open.push(block.frame(Becomes::Value { header }));
                    }
                }
            }
            _ => {
                let op = match tokens.get(index + 1) {
                    Some(TextToken::Operator(op)) if index + 2 < top.end => Some((*op, index + 2)),
                    _ if top.object && !top.flat && index + 1 < top.end => {
                        Some((Operator::Equal, index + 1))
                    }
                    _ => None,
                };
                let (at, key) = match op {
                    Some((op, at)) => (at, Some((key(token, encoding), Some(op)))),
                    None => (index, None),
                };
                top.index = next(tokens, at);
                match (start(tokens, at, encoding), key) {
                    (Ok(value), Some((key, op))) => {
                        top.items
                            .push(OwnedItem::Field(OwnedField::new(key, op, value)));
                    }
                    (Ok(value), None) => top.items.push(OwnedItem::Value(value)),
                    (Err((header, block)), Some((key, op))) => {
                        open.push(block.frame(Becomes::Field { key, op, header }));
                    }
                    (Err((header, block)), None) => {
                        open.push(block.frame(Becomes::Value { header }));
                    }
                }
            }
        }
    }
}
