//! Read ordinary fields and blocks directly into the tape. Stop at bracket
//! constructs and recovery cases. The full grammar then continues from the
//! last checkpoint. Both paths use the same scalar and trivia scans.

use super::{Flavor, Leaf, NO_OPEN, OnEmpty, Shape};
use crate::{Scalar, TextToken};
use fearless_simd::Simd;

use super::super::{
    BOM, CLASS, CLASS_BANG, CLASS_CLOSE_BRACE, CLASS_CLOSE_BRACKET, CLASS_COMMENT,
    CLASS_OPEN_BRACE, CLASS_OPEN_BRACKET, CLASS_OPERATOR, CLASS_QUOTE, MAX_DEPTH, find_line_end,
    is_ws, scan_quoted, scan_unquoted, skip_whitespace, strip_quotes,
};

/// The state between two root items. The full grammar can continue from this
/// state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Checkpoint {
    /// The byte offset after the last complete root item.
    pub(super) pos: usize,
    /// The tape length at `pos`. The tape before this length is complete.
    pub(super) len: usize,
    /// The root is mixed.
    pub(super) mixed: bool,
    /// The root has no entries.
    pub(super) empty: bool,
}

/// Write the tape of `source` into `tape`, which must be empty. On `Err`, the
/// tape has a partial result. Truncate it to the checkpoint length and give it
/// to the full grammar.
#[inline(always)]
pub(super) fn parse<'a, S: Simd>(
    simd: S,
    source: &'a [u8],
    flavor: Flavor,
    tape: Vec<TextToken<'a>>,
) -> Result<Vec<TextToken<'a>>, (Vec<TextToken<'a>>, Checkpoint)> {
    let mut p = Parser::new(simd, source, flavor, tape);
    match p.items() {
        Some(()) => Ok(p.tape),
        None => Err((p.tape, p.checkpoint)),
    }
}

/// Give each checkpoint that the fast path records for `source`, and the
/// tape that it writes.
#[cfg(test)]
pub(super) fn checkpoints(source: &[u8], flavor: Flavor) -> (Vec<Checkpoint>, Vec<TextToken<'_>>) {
    let mut p = Parser::new(fearless_simd::Fallback::new(), source, flavor, Vec::new());
    p.log.push(p.checkpoint);
    let _ = p.items();
    (p.log, p.tape)
}

/// The fast-path form of [`super::Container`].
struct Container {
    shape: Shape,
    /// After a [`TextToken::MixedContainer`], each operator is a token.
    mixed: bool,
    empty: bool,
    /// The first entry starts with a scalar. When this block closes in a
    /// mixed parent, the parent stays mixed.
    first_scalar: bool,
    /// The mixed flag that `TextTape` keeps in the open token. When a child
    /// block closes, the mixed mode comes from this flag again.
    flag: bool,
    /// The index of the open token, or `NO_OPEN` for the root.
    open: usize,
    on_empty: OnEmpty,
}

impl Container {
    fn new(shape: Shape, open: usize, on_empty: OnEmpty) -> Self {
        Self {
            shape,
            mixed: false,
            empty: true,
            first_scalar: false,
            flag: false,
            open,
            on_empty,
        }
    }

    #[inline(always)]
    fn entry(&mut self, scalar: bool) {
        if self.empty {
            self.first_scalar = scalar;
            self.empty = false;
        }
    }
}

struct Parser<'a, S> {
    simd: S,
    source: &'a [u8],
    flavor: Flavor,
    pos: usize,
    tape: Vec<TextToken<'a>>,
    stack: Vec<Container>,
    /// The state after the last `}` at the root, or at the start.
    checkpoint: Checkpoint,
    #[cfg(test)]
    log: Vec<Checkpoint>,
}

impl<'a, S: Simd> Parser<'a, S> {
    fn new(simd: S, source: &'a [u8], flavor: Flavor, tape: Vec<TextToken<'a>>) -> Self {
        let pos = if source.starts_with(BOM) { 3 } else { 0 };
        Parser {
            simd,
            source,
            flavor,
            pos,
            tape,
            stack: Vec::new(),
            checkpoint: Checkpoint {
                pos,
                len: 0,
                mixed: false,
                empty: true,
            },
            #[cfg(test)]
            log: Vec::new(),
        }
    }

    #[inline(always)]
    fn trivia(&mut self) -> Option<u8> {
        loop {
            let b = *self.source.get(self.pos)?;
            if is_ws(b) {
                self.pos = skip_whitespace(self.simd, self.source, self.pos + 1);
            } else if b == b'#' {
                self.pos = find_line_end(self.simd, self.source, self.pos + 1);
            } else {
                return Some(b);
            }
        }
    }

    #[inline(always)]
    fn scalar(&mut self, b: u8, key: bool) -> Option<Leaf<'a>> {
        let start = self.pos;
        if b == b'"' {
            self.pos = scan_quoted(
                self.simd,
                self.source,
                start + 1,
                self.flavor.newline_terminated_strings,
            );
            return Some(Leaf::Quoted(Scalar::new(strip_quotes(
                &self.source[start..self.pos],
            ))));
        }
        match CLASS[b as usize] {
            CLASS_CLOSE_BRACKET if key => {
                // A closing bracket can be a key in a save file.
                self.pos += 1;
                if self.trivia() != Some(b'=') {
                    return None;
                }
                return Some(Leaf::Unquoted(Scalar::new(&self.source[start..start + 1])));
            }
            CLASS_OPEN_BRACE | CLASS_CLOSE_BRACE | CLASS_OPEN_BRACKET | CLASS_OPERATOR
            | CLASS_BANG | CLASS_COMMENT | CLASS_QUOTE | CLASS_CLOSE_BRACKET => return None,
            _ => {}
        }
        if (b == b'@' && self.flavor.variables && self.source.get(start + 1) == Some(&b'['))
            || (b == b'?' && self.source.get(start + 1) == Some(&b'='))
        {
            return None;
        }
        let (mut end, _) = scan_unquoted(self.simd, self.source, start);
        if self.source[end - 1] == b'!' && end - start > 1 && self.source.get(end) == Some(&b'=') {
            end -= 1;
        }
        self.pos = end;
        let text = &self.source[start..end];
        Some(Leaf::Unquoted(Scalar::new(text)))
    }

    #[inline(always)]
    fn operator(&mut self) -> Option<crate::text::Operator> {
        let start = self.pos;
        let b = *self.source.get(start)?;
        if !matches!(b, b'=' | b'<' | b'>' | b'!' | b'?') {
            return None;
        }
        let len = 1 + usize::from(self.source.get(start + 1) == Some(&b'='));
        let op = super::operator(&self.source[start..start + len])?;
        self.pos += len;
        Some(op)
    }

    /// Read an operator in item position, as in `{ a=b }={ c }`. The tape
    /// has no token for it, but a block that has it is not empty. Return
    /// `None` when no operator is at the position.
    #[cold]
    fn stray_operator(&mut self, current: &mut Container) -> Option<()> {
        self.operator()?;
        if current.open != NO_OPEN {
            current.empty = false;
        }
        Some(())
    }

    #[inline(always)]
    fn block(&mut self, current: &mut Container, on_empty: OnEmpty) -> Option<()> {
        if self.stack.len() + 1 >= MAX_DEPTH as usize {
            return None;
        }
        self.pos += 1;
        let open = self.tape.len();
        self.tape.push(TextToken::Array {
            end: 0,
            mixed: false,
        });
        self.stack.push(std::mem::replace(
            current,
            Container::new(Shape::Open, open, on_empty),
        ));
        Some(())
    }

    #[inline(always)]
    fn close(&mut self, current: &mut Container) {
        let Some(parent) = self.stack.pop() else {
            return;
        };
        let child = std::mem::replace(current, parent);
        if child.empty {
            match child.on_empty {
                OnEmpty::Keep => {}
                OnEmpty::Drop {
                    len,
                    shape,
                    mixed,
                    first,
                } => {
                    self.tape.truncate(len);
                    current.shape = shape;
                    current.mixed = mixed;
                    if first {
                        current.empty = true;
                        current.first_scalar = false;
                    }
                    return;
                }
                OnEmpty::Header { len } => {
                    let TextToken::Header(header) = self.tape[len] else {
                        unreachable!()
                    };
                    self.tape.truncate(len);
                    self.tape.push(TextToken::Unquoted(header));
                    return;
                }
            }
        }
        let end = self.tape.len();
        self.tape[child.open] = if child.shape == Shape::Object {
            TextToken::Object {
                end,
                mixed: child.mixed,
            }
        } else {
            TextToken::Array {
                end,
                mixed: child.mixed,
            }
        };
        self.tape.push(TextToken::End(child.open));
        current.flag |= child.first_scalar && current.mixed && current.open != NO_OPEN;
        current.mixed = current.flag;
    }

    #[inline(always)]
    fn items(&mut self) -> Option<()> {
        let mut current = Container::new(Shape::Object, NO_OPEN, OnEmpty::Keep);
        let mut next = self.trivia();
        loop {
            let Some(b) = next else {
                if self.stack.is_empty() {
                    return Some(());
                }
                self.close(&mut current);
                continue;
            };
            if b == b'}' {
                self.pos += 1;
                self.close(&mut current);
                if self.stack.is_empty() {
                    // A root item ends here. The grammar does not look back
                    // past a `}`, thus it can continue from this position.
                    self.checkpoint = Checkpoint {
                        pos: self.pos,
                        len: self.tape.len(),
                        mixed: current.mixed,
                        empty: current.empty,
                    };
                    #[cfg(test)]
                    self.log.push(self.checkpoint);
                }
                next = self.trivia();
                continue;
            }
            if b == b'{' {
                let first = current.empty;
                current.entry(false);
                let len = self.tape.len();
                let drop = OnEmpty::Drop {
                    len,
                    shape: current.shape,
                    mixed: current.mixed,
                    first,
                };
                let on_empty = match current.shape {
                    Shape::Open => {
                        current.shape = Shape::Array;
                        drop
                    }
                    Shape::Object if !current.mixed => {
                        self.tape.push(TextToken::MixedContainer);
                        current.mixed = true;
                        drop
                    }
                    _ => OnEmpty::Keep,
                };
                self.block(&mut current, on_empty)?;
                next = self.trivia();
                continue;
            }
            let start = self.pos;
            let Some(key) = self.scalar(b, true) else {
                self.pos = start;
                self.stray_operator(&mut current)?;
                next = self.trivia();
                continue;
            };
            current.entry(true);
            next = self.trivia();
            let op = self.operator();
            if op.is_some() || next == Some(b'{') {
                match current.shape {
                    Shape::Open => {
                        current.shape = if op.is_some() {
                            Shape::Object
                        } else {
                            Shape::Array
                        }
                    }
                    Shape::Array if op.is_some() && !current.mixed => {
                        self.tape.push(TextToken::MixedContainer);
                        current.mixed = true;
                    }
                    _ => {}
                }
                let header_ok = current.shape == Shape::Object && !current.mixed;
                self.tape.push(key.token());
                if let Some(op) = op
                    && (current.mixed || op != crate::text::Operator::Equal)
                {
                    self.tape.push(TextToken::Operator(op));
                }
                if op.is_some() {
                    next = self.trivia();
                }
                let b = next?;
                if b == b'{' {
                    self.block(&mut current, OnEmpty::Keep)?;
                    next = self.trivia();
                } else {
                    let value = self.scalar(b, false)?;
                    next = self.trivia();
                    if next == Some(b'[') {
                        return None;
                    }
                    if next == Some(b'{') && matches!(value, Leaf::Unquoted(_)) {
                        // The grammar reads an unquoted scalar, a macro
                        // parameter, and a variable before `{` as a header.
                        // After a quoted value, the block is the next item.
                        let len = self.tape.len();
                        self.tape.push(if header_ok {
                            TextToken::Header(value.scalar())
                        } else {
                            value.token()
                        });
                        let on_empty = if header_ok {
                            OnEmpty::Header { len }
                        } else {
                            OnEmpty::Keep
                        };
                        self.block(&mut current, on_empty)?;
                        next = self.trivia();
                    } else {
                        self.tape.push(value.token());
                    }
                }
            } else {
                if next == Some(b'[') {
                    return None;
                }
                if current.shape == Shape::Open {
                    current.shape = Shape::Array;
                } else if current.shape == Shape::Object && !current.mixed {
                    self.tape.push(TextToken::MixedContainer);
                    current.mixed = true;
                }
                self.tape.push(key.token());
            }
        }
    }
}
