//! The parser of the expressions inside a calc.

use super::SyntaxKind;
use super::lexer::Token;
use super::parser::MAX_DEPTH;

/// A node in the temporary calc tree (see [`parse_calc_interior`]).
pub(super) enum CalcElem {
    /// A leaf that refers to a lexed token by its index.
    Leaf(u32),
    /// A zero-width missing token, appended at EOF when emitted.
    Missing(SyntaxKind),
    /// An interior expression node with children in source order.
    Node {
        kind: SyntaxKind,
        children: Vec<CalcElem>,
        depth: u32,
    },
}

impl CalcElem {
    fn depth(&self) -> u32 {
        match self {
            Self::Node { depth, .. } => *depth,
            _ => 0,
        }
    }
}

/// The result of [`parse_calc_interior`].
pub(super) struct CalcInterior {
    /// The expression, then any unconsumed (malformed) tokens.
    pub(super) elems: Vec<CalcElem>,
    /// `true` if the [`MAX_DEPTH`] guard tripped.
    pub(super) overflowed: bool,
    /// The token indexes of each `(` that has no `)` because the interior
    /// ends inside it, innermost first.
    pub(super) missing_parens: Vec<u32>,
    /// The token indexes of each `(` that has no `)` because its expression
    /// stops at a different token, as in `(1 2`, innermost first.
    pub(super) unclosed_parens: Vec<u32>,
}

/// Infix binding powers; `* /` bind tighter than `+ -`. The left power being
/// below the right power makes equal-precedence chains left-associative.
fn infix_bp(kind: SyntaxKind) -> Option<(u8, u8)> {
    match kind {
        SyntaxKind::Plus | SyntaxKind::Minus => Some((1, 2)),
        SyntaxKind::Star | SyntaxKind::Slash => Some((3, 4)),
        _ => None,
    }
}

/// Parse the interior tokens of a calc (everything between `@[` and `]`) into a
/// flat list of [`CalcElem`]s: the expression, then the tokens that the
/// expression does not use, as in `@[1 2]`. `base` is the index of the first
/// token of `toks` in the token buffer.
///
/// The grammar is:
///
/// ```text
/// operand : Number | CalcIdent | `(` expr `)` | (`-` | `+`) operand
/// expr    : operand ((`+` | `-` | `*` | `/`) operand)*
/// ```
///
/// `*` and `/` bind more tightly than `+` and `-`, and each operator is
/// left-associative. The interior whitespace stays in the gaps between the
/// tokens, so the caller emits only the tokens and the nodes.
///
/// When `insert_missing_parens` is set, each `(` in
/// [`CalcInterior::missing_parens`] gets a zero-width
/// [`SyntaxKind::MissingCloseParen`]. When the [`MAX_DEPTH`] guard
/// trips, the result is each token as a flat leaf.
pub(super) fn parse_calc_interior(
    toks: &[Token],
    base: u32,
    insert_missing_parens: bool,
) -> CalcInterior {
    let mut c = CalcCursor {
        toks,
        base,
        pos: 0,
        depth: 0,
        overflowed: false,
        missing_parens: Vec::new(),
        unclosed_parens: Vec::new(),
        insert_missing_parens,
    };
    let mut elems = Vec::new();
    if c.has_more() {
        elems.push(c.parse_expr(0));
    }
    // For malformed input like `@[1 2]`, any leftover tokens the expression
    // grammar did not consume are kept as leaves so the region still tiles
    // its source.
    while c.has_more() {
        elems.push(c.leaf());
    }
    if c.overflowed {
        elems = (0..toks.len())
            .map(|i| CalcElem::Leaf(base + i as u32))
            .collect();
        c.missing_parens.clear();
        c.unclosed_parens.clear();
    }
    CalcInterior {
        elems,
        overflowed: c.overflowed,
        missing_parens: c.missing_parens,
        unclosed_parens: c.unclosed_parens,
    }
}

/// A cursor over a calc's interior tokens used by [`parse_calc_interior`].
struct CalcCursor<'t> {
    toks: &'t [Token],
    base: u32,
    pos: usize,
    /// Current operand-nesting depth, compared against [`MAX_DEPTH`].
    depth: u32,
    /// Set once the depth guard trips (see [`CalcCursor::parse_operand`]).
    overflowed: bool,
    missing_parens: Vec<u32>,
    unclosed_parens: Vec<u32>,
    insert_missing_parens: bool,
}

impl CalcCursor<'_> {
    fn has_more(&self) -> bool {
        self.pos < self.toks.len()
    }

    fn peek(&self) -> Option<SyntaxKind> {
        self.toks.get(self.pos).map(|t| t.kind)
    }

    /// Consume one token as a leaf, advancing the cursor.
    fn leaf(&mut self) -> CalcElem {
        let index = self.base + self.pos as u32;
        self.pos += 1;
        CalcElem::Leaf(index)
    }

    /// Parse an operand: a unary expression, a parenthesized expression, a
    /// number/identifier leaf, or (for malformed input) whatever leaf is here.
    fn parse_operand(&mut self) -> CalcElem {
        // Limit parser recursion through parentheses and unary operators. The
        // outermost operand is at depth 1, so `MAX_DEPTH` parentheses fit.
        if self.overflowed || self.depth > MAX_DEPTH {
            self.overflowed = true;
            return self.leaf();
        }
        self.depth += 1;
        let elem = self.parse_operand_inner();
        self.depth -= 1;
        elem
    }

    fn parse_operand_inner(&mut self) -> CalcElem {
        match self.peek() {
            Some(SyntaxKind::Minus) | Some(SyntaxKind::Plus) => {
                let mut children = vec![self.leaf()]; // unary operator
                if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                    children.push(self.parse_operand());
                }
                self.node(SyntaxKind::UnaryExpr, children)
            }
            Some(SyntaxKind::OpenParen) => {
                let open = self.base + self.pos as u32;
                let mut children = vec![self.leaf()]; // `(`
                if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                    children.push(self.parse_expr(0));
                }
                if self.peek() == Some(SyntaxKind::CloseParen) {
                    children.push(self.leaf()); // `)`
                } else if self.has_more() {
                    // The `)` is missing before the next token, not at the end
                    // of the interior.
                    self.unclosed_parens.push(open);
                } else {
                    if self.insert_missing_parens {
                        children.push(CalcElem::Missing(SyntaxKind::MissingCloseParen));
                    }
                    self.missing_parens.push(open);
                }
                self.node(SyntaxKind::ParenExpr, children)
            }
            // Number, CalcIdent, or — in malformed input — a stray operator.
            Some(_) => self.leaf(),
            None => unreachable!("parse_operand called at end of interior"),
        }
    }

    fn node(&mut self, kind: SyntaxKind, children: Vec<CalcElem>) -> CalcElem {
        let depth = 1 + children.iter().map(CalcElem::depth).max().unwrap_or(0);
        if depth > MAX_DEPTH {
            self.overflowed = true;
            // Each child is bounded. Discard this partial expression safely.
            return CalcElem::Leaf(self.base);
        }
        CalcElem::Node {
            kind,
            children,
            depth,
        }
    }

    /// Precedence-climbing parse of a (sub)expression with binding power floor
    /// `min_bp`.
    fn parse_expr(&mut self, min_bp: u8) -> CalcElem {
        let mut lhs = self.parse_operand();
        while !self.overflowed {
            match self.peek().and_then(infix_bp) {
                Some((l_bp, r_bp)) if l_bp >= min_bp => {
                    let mut children = vec![lhs, self.leaf()];
                    if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                        children.push(self.parse_expr(r_bp));
                    }
                    lhs = self.node(SyntaxKind::BinaryExpr, children);
                }
                _ => break,
            }
        }
        lhs
    }
}
