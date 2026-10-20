//! Read ordinary fields and blocks directly into the tape. Return `None` for
//! bracket constructs and recovery cases. The full grammar then reads the
//! source again. Both paths use the same scalar and trivia scans.

use super::{Flavor, Leaf, Shape};
use crate::{Scalar, TextToken};
use fearless_simd::Simd;

use super::super::{
    BOM, CLASS, CLASS_BANG, CLASS_CLOSE_BRACE, CLASS_CLOSE_BRACKET, CLASS_COMMENT,
    CLASS_OPEN_BRACE, CLASS_OPEN_BRACKET, CLASS_OPERATOR, CLASS_QUOTE, MAX_DEPTH, classify_run,
    find_line_end, is_ws, scan_quoted, scan_unquoted, skip_whitespace, strip_quotes,
};

#[inline(always)]
pub(super) fn parse<S: Simd>(simd: S, source: &[u8], flavor: Flavor) -> Option<Vec<TextToken<'_>>> {
    let mut p = Parser {
        simd,
        source,
        flavor,
        pos: if source.starts_with(BOM) { 3 } else { 0 },
        tape: Vec::with_capacity(source.len() / 5),
    };
    p.items(Shape::Object, 0)?;
    Some(p.tape)
}

struct Container {
    shape: Shape,
    mixed: bool,
    empty: bool,
    first_scalar: bool,
}

struct Parser<'a, S> {
    simd: S,
    source: &'a [u8],
    flavor: Flavor,
    pos: usize,
    tape: Vec<TextToken<'a>>,
}

impl<'a, S: Simd> Parser<'a, S> {
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

    #[inline(always)]
    fn block(&mut self, depth: u32) -> Option<Container> {
        if depth + 1 >= MAX_DEPTH {
            return None;
        }
        self.pos += 1;
        let open = self.tape.len();
        self.tape.push(TextToken::Array {
            end: 0,
            mixed: false,
        });
        let container = self.items(Shape::Open, depth + 1)?;
        let end = self.tape.len();
        self.tape[open] = if container.shape == Shape::Object {
            TextToken::Object {
                end,
                mixed: container.mixed,
            }
        } else {
            TextToken::Array {
                end,
                mixed: container.mixed,
            }
        };
        self.tape.push(TextToken::End(open));
        Some(container)
    }

    fn items(&mut self, shape: Shape, depth: u32) -> Option<Container> {
        // Each recursive call must enable the SIMD level again. Keep the
        // closure inline so vector operations do not call separate helpers.
        self.simd.vectorize(
            #[inline(always)]
            || self.items_simd(shape, depth),
        )
    }

    #[inline(always)]
    fn items_simd(&mut self, mut shape: Shape, depth: u32) -> Option<Container> {
        let mut empty = true;
        let mut first_scalar = false;
        let mut mixed = false;
        // A child block can set the mixed flag in the parent's open token.
        // When a child closes, the parent uses that flag again.
        let mut flag = false;
        while let Some(b) = self.trivia() {
            if b == b'}' {
                self.pos += 1;
                if depth != 0 {
                    break;
                }
                continue;
            }
            if b == b'{' {
                let len = self.tape.len();
                let old_shape = shape;
                let old_mixed = mixed;
                if shape == Shape::Open {
                    shape = Shape::Array;
                } else if shape == Shape::Object && !mixed {
                    self.tape.push(TextToken::MixedContainer);
                    mixed = true;
                }
                let child = self.block(depth)?;
                if child.empty
                    && (old_shape == Shape::Open || (old_shape == Shape::Object && !old_mixed))
                {
                    self.tape.truncate(len);
                    shape = old_shape;
                    mixed = old_mixed;
                } else {
                    flag |= child.first_scalar && mixed && depth != 0;
                    mixed = flag;
                }
                empty = false;
                continue;
            }
            let key = self.scalar(b, true)?;
            if empty {
                first_scalar = true;
            }
            let next = self.trivia();
            let op = self.operator();
            if op.is_some() || next == Some(b'{') {
                match shape {
                    Shape::Open => {
                        shape = if op.is_some() {
                            Shape::Object
                        } else {
                            Shape::Array
                        }
                    }
                    Shape::Array if op.is_some() && !mixed => {
                        self.tape.push(TextToken::MixedContainer);
                        mixed = true;
                    }
                    _ => {}
                }
                let header_ok = shape == Shape::Object && !mixed;
                self.tape.push(key.token());
                if let Some(op) = op
                    && (mixed || op != crate::text::Operator::Equal)
                {
                    self.tape.push(TextToken::Operator(op));
                }
                let b = self.trivia()?;
                if b == b'{' {
                    let child = self.block(depth)?;
                    flag |= child.first_scalar && mixed && depth != 0;
                    mixed = flag;
                } else {
                    let value = self.scalar(b, false)?;
                    let next = self.trivia();
                    if next == Some(b'[') {
                        return None;
                    }
                    if next == Some(b'{') {
                        let Leaf::Unquoted(scalar) = value else {
                            return None;
                        };
                        let text = scalar.as_bytes();
                        if classify_run(text, text.contains(&b'$'), self.flavor)
                            != super::SyntaxKind::Unquoted
                        {
                            return None;
                        }
                        let len = self.tape.len();
                        self.tape.push(if header_ok {
                            TextToken::Header(value.scalar())
                        } else {
                            value.token()
                        });
                        let child = self.block(depth)?;
                        if child.empty && header_ok {
                            self.tape.truncate(len);
                            self.tape.push(value.token());
                        } else {
                            flag |= child.first_scalar && mixed && depth != 0;
                            mixed = flag;
                        }
                    } else {
                        self.tape.push(value.token());
                    }
                }
            } else {
                if next == Some(b'[') {
                    return None;
                }
                if shape == Shape::Open {
                    shape = Shape::Array;
                } else if shape == Shape::Object && !mixed {
                    self.tape.push(TextToken::MixedContainer);
                    mixed = true;
                }
                self.tape.push(key.token());
            }
            empty = false;
        }
        Some(Container {
            shape,
            mixed,
            empty,
            first_scalar,
        })
    }
}
