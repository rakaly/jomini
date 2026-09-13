//! **Experimental** lossless syntax tree for the Clausewitz text format.
//!
//! This is the foundation for tooling — formatting, linting, highlighting, and
//! transformations — and (later) ergonomic deserialization. Unlike [`TextTape`],
//! which is a fast, lossy tape that discards trivia and elides the `=` operator,
//! this tree preserves **every byte** of the source: comments, whitespace, the
//! `=`, even bytes that match no rule. The defining invariant is:
//!
//! ```text
//! concat(text of every leaf token, in order) == source
//! ```
//!
//! # Representation
//!
//! The tree is stored in a single flat arena ([`GreenTree::tape`]) in pre-order,
//! mirroring jomini's tape philosophy for cache locality. Following rowan's
//! green/red split, the stored data is **position-independent**: each node and
//! token records only its *relative width* in bytes, never an absolute offset.
//! Absolute offsets and parent links are derived once and the lightweight
//! [`SyntaxNode`]/[`SyntaxToken`] cursors ("red layer") read them on demand.
//! Keeping widths relative is what makes future incremental reparse (reusing
//! untouched subtrees) tractable.
//!
//! # Games (one superset parser)
//!
//! A single permissive parser handles every PDS title. The lexer accepts the
//! *union* of all games' syntax (`@vars`, `@[calc]`, `$macros$`, `[[params]]`,
//! broad identifier bytes); game-appropriateness is a concern for a later lint
//! layer, not the parser. The handful of genuine lexer forks are toggled by
//! [`Flavor`] — currently just HoI4's newline-terminated strings.
//!
//! On top of the green tree sit lightweight [`SyntaxNode`]/[`SyntaxToken`]
//! cursors and an ungrammar-style **typed AST** ([`AstNode`], [`Field`],
//! [`Block`], [`HeaderedBlock`], [`Calc`], …) with value coercions
//! ([`Value::to_f64`]). A [`format`](fn@format) pass reprints the tree in a
//! normalized house style, preserving every comment and significant token.
//!
//! This is a Phase-1 module: `[[param]]` blocks are not yet given dedicated
//! structure (they round-trip losslessly as loose tokens, surfaced as
//! [`Item::Other`]). The internals of `@[calc]` *are* parsed into a real
//! expression subtree ([`SyntaxKind::Calc`] / [`SyntaxKind::BinaryExpr`] /
//! [`SyntaxKind::UnaryExpr`] / [`SyntaxKind::ParenExpr`]). A depth guard caps
//! recursion on pathological nesting (see [`SyntaxError`]).
//!
//! [`TextTape`]: crate::TextTape
#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::{Scalar, text::Operator};

/// The kind of every node (interior) and token (leaf) in a [`GreenTree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // ===== tokens (leaves) =====
    /// A leading UTF-8 byte-order-mark (`EF BB BF`).
    Bom,
    /// A run of spaces, tabs, newlines, carriage returns, or semicolons.
    Whitespace,
    /// A `# ...` comment running to end of line.
    Comment,
    /// A bare scalar: identifier, number, date, `yes`/`no`, etc.
    Unquoted,
    /// A `"..."` quoted scalar (raw text, including the quotes).
    Quoted,
    /// A `@name` reader-variable reference.
    Variable,
    /// A `$NAME$` macro parameter.
    MacroParam,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `!`, the undefined-parameter marker in `[[!name] ...]`.
    Bang,
    /// An operator: `=` `==` `?=` `!=` `<` `<=` `>` `>=`.
    Operator,
    /// Reserved for explicit error tokens (the current lexer classifies every
    /// byte, worst case as [`SyntaxKind::Unquoted`], so it is not emitted yet).
    Error,

    // ===== calc tokens (interior of `@[ ... ]`) =====
    /// `@[`, opening a parse-time calculation.
    CalcOpen,
    /// `]`, closing a parse-time calculation.
    CalcClose,
    /// `+` inside a calc.
    Plus,
    /// `-` inside a calc.
    Minus,
    /// `*` inside a calc.
    Star,
    /// `/` inside a calc.
    Slash,
    /// `(` inside a calc.
    OpenParen,
    /// `)` inside a calc.
    CloseParen,
    /// A numeric literal inside a calc (e.g. `1`, `10.0`, `10.0f`).
    Number,
    /// An operand identifier inside a calc (e.g. `tier`, `leopard_x`, `@var`).
    CalcIdent,

    // ===== nodes (interior) =====
    /// The whole document.
    Root,
    /// A `key <op> value` field.
    Field,
    /// A `{ ... }` block.
    Block,
    /// A tagged block such as `rgb { 1 2 3 }` (a header scalar plus a block).
    HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation wrapping an arithmetic expression.
    Calc,
    /// A binary arithmetic expression `lhs <op> rhs` inside a [`SyntaxKind::Calc`].
    BinaryExpr,
    /// A prefix `-`/`+` expression inside a [`SyntaxKind::Calc`].
    UnaryExpr,
    /// A parenthesized expression `( ... )` inside a [`SyntaxKind::Calc`].
    ParenExpr,
    /// An error-recovery wrapper around tokens that could not be placed.
    Bogus,
}

impl SyntaxKind {
    /// Whitespace, comments, and the BOM — insignificant to structure but
    /// preserved for losslessness.
    pub fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace | SyntaxKind::Comment | SyntaxKind::Bom
        )
    }

    /// A token that can stand as a scalar value (or an object key). This is the
    /// single definition of "scalar" shared by key detection, [`Value::Scalar`],
    /// and the formatter: `$macro$` parameters count, since in template files
    /// they appear in key, value, and array-element position alike.
    pub fn is_scalar(self) -> bool {
        matches!(
            self,
            SyntaxKind::Unquoted
                | SyntaxKind::Quoted
                | SyntaxKind::Variable
                | SyntaxKind::MacroParam
        )
    }

    /// Whether this kind labels an interior node (rather than a leaf token).
    pub fn is_node(self) -> bool {
        matches!(
            self,
            SyntaxKind::Root
                | SyntaxKind::Field
                | SyntaxKind::Block
                | SyntaxKind::HeaderedBlock
                | SyntaxKind::Calc
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::Bogus
        )
    }
}

// ---------------------------------------------------------------------------
// Lexing: a hand-rolled, error-tolerant, BOM-preserving scanner. It classifies
// every byte (worst case as Unquoted) so the tokens always tile the input with
// no gaps or overlaps, and every branch advances by at least one byte.
// ---------------------------------------------------------------------------

/// Game-specific lexer configuration.
///
/// [`Flavor::default`] is the most permissive superset and is correct for every
/// game except where a genuine lexer fork exists. [`Flavor::hoi4`] enables
/// HoI4's newline-terminated strings and treats `@`/`$` as ordinary identifier
/// bytes (HoI4 has no reader variables or macros).
#[derive(Debug, Clone, Copy)]
pub struct Flavor {
    /// A quoted string ends at the first newline if it has no closing quote.
    pub newline_terminated_strings: bool,
    /// Recognize `@name` reader variables and `@[ ... ]` calculations.
    pub variables: bool,
    /// Recognize `$NAME$` macro parameters.
    pub macros: bool,
}

impl Default for Flavor {
    fn default() -> Self {
        Flavor {
            newline_terminated_strings: false,
            variables: true,
            macros: true,
        }
    }
}

impl Flavor {
    /// The permissive cross-game superset (same as [`Flavor::default`]).
    pub fn superset() -> Self {
        Flavor::default()
    }

    /// Hearts of Iron IV: newline-terminated strings, no `@vars`/`$macros$`.
    pub fn hoi4() -> Self {
        Flavor {
            newline_terminated_strings: true,
            variables: false,
            macros: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Tok {
    kind: SyntaxKind,
    start: u32,
    len: u32,
}

#[inline]
fn is_ws(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\r' | b'\n' | 0x0b | 0x0c | b';')
}

/// Bytes that terminate an unquoted run. These either start their own token
/// (braces, brackets, operators, quote, comment) or are whitespace. Notably
/// `@`, `$`, `!`, `?`, `:`, `.`, `-`, `/`, `|` and high bytes are *not* here, so
/// they are absorbed mid-identifier and only dispatch specially as a first byte.
#[inline]
fn is_stop(b: u8) -> bool {
    is_ws(b)
        || matches!(
            b,
            b'{' | b'}' | b'[' | b']' | b'=' | b'<' | b'>' | b'"' | b'#'
        )
}

/// Find the closing `$` of a macro parameter starting just after the opening
/// `$`, or `None` if a terminator is hit first (so it isn't really a macro).
fn macro_close(source: &[u8], from: usize) -> Option<usize> {
    let mut j = from;
    while j < source.len() {
        match source[j] {
            b'$' => return Some(j),
            b if is_stop(b) => return None,
            _ => j += 1,
        }
    }
    None
}

fn lex(source: &[u8], flavor: Flavor) -> Vec<Tok> {
    let mut out: Vec<Tok> = Vec::new();
    let n = source.len();

    let mut i = 0usize;
    if source.starts_with(&[0xEF, 0xBB, 0xBF]) {
        out.push(Tok {
            kind: SyntaxKind::Bom,
            start: 0,
            len: 3,
        });
        i = 3;
    }

    while i < n {
        let b = source[i];

        // `@[ ... ]` calc: emit structured interior tokens (CalcOpen, operands,
        // operators, interior whitespace as trivia, CalcClose) rather than one
        // opaque span, so the arithmetic expression can be parsed into a subtree.
        if flavor.variables && b == b'@' && i + 1 < n && source[i + 1] == b'[' {
            i = lex_calc(source, i, &mut out);
            continue;
        }

        let start = i;
        let kind;

        if is_ws(b) {
            i += 1;
            while i < n && is_ws(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::Whitespace;
        } else if b == b'#' {
            i += 1;
            while i < n && source[i] != b'\n' && source[i] != b'\r' {
                i += 1;
            }
            kind = SyntaxKind::Comment;
        } else if b == b'"' {
            i += 1;
            loop {
                if i >= n {
                    break;
                }
                match source[i] {
                    b'\\' => i = (i + 2).min(n),
                    b'"' => {
                        i += 1;
                        break;
                    }
                    b'\n' | b'\r' if flavor.newline_terminated_strings => break,
                    _ => i += 1,
                }
            }
            kind = SyntaxKind::Quoted;
        } else if b == b'{' {
            i += 1;
            kind = SyntaxKind::OpenBrace;
        } else if b == b'}' {
            i += 1;
            kind = SyntaxKind::CloseBrace;
        } else if b == b'[' {
            i += 1;
            kind = SyntaxKind::OpenBracket;
        } else if b == b']' {
            i += 1;
            kind = SyntaxKind::CloseBracket;
        } else if matches!(b, b'=' | b'<' | b'>') {
            i += 1;
            if i < n && source[i] == b'=' {
                i += 1; // ==, <=, >=
            }
            kind = SyntaxKind::Operator;
        } else if matches!(b, b'!' | b'?') && i + 1 < n && source[i + 1] == b'=' {
            i += 2; // != or ?=
            kind = SyntaxKind::Operator;
        } else if b == b'!' {
            i += 1;
            kind = SyntaxKind::Bang;
        } else if b == b'@'
            && flavor.variables
            && i + 1 < n
            && !is_stop(source[i + 1])
            && source[i + 1] != b'@'
        {
            // @name reader variable
            i += 1;
            while i < n && !is_stop(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::Variable;
        } else if b == b'$' && flavor.macros && macro_close(source, i + 1).is_some() {
            let close = macro_close(source, i + 1).unwrap();
            i = close + 1;
            kind = SyntaxKind::MacroParam;
        } else {
            // Ordinary unquoted run: consume up to the next terminator. This
            // catch-all is what keeps the lexer total (every byte classified).
            i += 1;
            while i < n && !is_stop(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::Unquoted;
        }

        out.push(Tok {
            kind,
            start: start as u32,
            len: (i - start) as u32,
        });
    }

    out
}

/// A byte that ends a calc operand identifier or number: whitespace, an
/// arithmetic operator, a parenthesis, or the closing `]`. A bare `[` is *not* a
/// stop, so a stray one (only seen in malformed input) is absorbed rather than
/// stalling the lexer; a real calc never contains one.
#[inline]
fn is_calc_stop(b: u8) -> bool {
    is_ws(b) || matches!(b, b'(' | b')' | b'+' | b'-' | b'*' | b'/' | b']')
}

/// Scan a calc numeric literal beginning at `i` (`source[i]` is a digit or a `.`
/// directly followed by a digit). Consumes digits, an optional fractional part,
/// and an optional `f` suffix (e.g. `1`, `10.0`, `.5`, `10.0f`).
fn scan_number(source: &[u8], mut i: usize) -> usize {
    let n = source.len();
    while i < n && source[i].is_ascii_digit() {
        i += 1;
    }
    if i < n && source[i] == b'.' {
        i += 1;
        while i < n && source[i].is_ascii_digit() {
            i += 1;
        }
    }
    if i < n && source[i] == b'f' {
        i += 1;
    }
    i
}

/// Lex a `@[ ... ]` calc region. `i` points at the leading `@`. Emits a
/// `CalcOpen` token, then structured interior tokens (operands, operators, and
/// interior whitespace as trivia) until the matching `]` (emitted as
/// `CalcClose`) or end of input, and returns the index just past what it
/// consumed. Every byte is classified, so the calc region tiles the input.
fn lex_calc(source: &[u8], mut i: usize, out: &mut Vec<Tok>) -> usize {
    let n = source.len();
    out.push(Tok {
        kind: SyntaxKind::CalcOpen,
        start: i as u32,
        len: 2,
    });
    i += 2; // past `@[`

    while i < n {
        let b = source[i];
        let start = i;
        let kind;

        if b == b']' {
            i += 1;
            out.push(Tok {
                kind: SyntaxKind::CalcClose,
                start: start as u32,
                len: 1,
            });
            return i; // leave calc mode
        } else if is_ws(b) {
            i += 1;
            while i < n && is_ws(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::Whitespace;
        } else if b == b'(' {
            i += 1;
            kind = SyntaxKind::OpenParen;
        } else if b == b')' {
            i += 1;
            kind = SyntaxKind::CloseParen;
        } else if b == b'+' {
            i += 1;
            kind = SyntaxKind::Plus;
        } else if b == b'-' {
            i += 1;
            kind = SyntaxKind::Minus;
        } else if b == b'*' {
            i += 1;
            kind = SyntaxKind::Star;
        } else if b == b'/' {
            i += 1;
            kind = SyntaxKind::Slash;
        } else if b.is_ascii_digit() || (b == b'.' && i + 1 < n && source[i + 1].is_ascii_digit()) {
            i = scan_number(source, i);
            kind = SyntaxKind::Number;
        } else {
            // An operand identifier (`tier`, `leopard_x`, `@var`) — or, in
            // malformed input, any other non-stop byte. Always consumes >= 1.
            i += 1;
            while i < n && !is_calc_stop(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::CalcIdent;
        }

        out.push(Tok {
            kind,
            start: start as u32,
            len: (i - start) as u32,
        });
    }

    i // reached EOF without a closing `]` (unterminated calc)
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// A non-fatal problem found while parsing. Parsing never fails — the tree is
/// always lossless — but recoverable issues are collected here for tooling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    /// Human-readable description.
    pub message: String,
    /// The half-open byte range the problem covers.
    pub range: (u32, u32),
}

// ---------------------------------------------------------------------------
// Green tree: flat pre-order arena. Nodes store a relative `len` (width in
// bytes) and an `end` index delimiting their subtree; tokens store only `len`.
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
enum Green {
    /// A leaf. Its bytes are `source[offset .. offset + len]`, where `offset`
    /// is derived by the tree (never stored on the node itself).
    Token { kind: SyntaxKind, len: u32 },
    /// An interior node. Its children are the elements in `[self_index+1, end)`.
    Node {
        kind: SyntaxKind,
        len: u32,
        end: u32,
    },
}

impl Green {
    fn kind(&self) -> SyntaxKind {
        match *self {
            Green::Token { kind, .. } | Green::Node { kind, .. } => kind,
        }
    }

    fn len(&self) -> u32 {
        match *self {
            Green::Token { len, .. } | Green::Node { len, .. } => len,
        }
    }
}

/// Precomputed per-subtree summary bits, one byte per element, filled in a
/// single reverse pass of [`Builder::finish`]. An element's flags are the union
/// of its own kind's bits and *every* descendant's, so a consumer can skip a
/// whole subtree with one O(1) test — "does this block contain a comment? a
/// syntax error? a calc? a macro parameter?" — instead of walking it. This
/// mirrors swift-syntax's `RecursiveRawSyntaxFlags` and Carbon's per-node
/// `has_error` bit, and is the cheap accelerator a linter or a future
/// incremental engine leans on. Stored in a side `Vec` (not on [`Green`]) so the
/// hot build loop is untouched — consistent with the offsets/parents design.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct NodeFlags(u8);

impl NodeFlags {
    /// The subtree contains at least one [`SyntaxKind::Comment`].
    pub const HAS_COMMENT: NodeFlags = NodeFlags(1 << 0);
    /// The subtree contains an error-recovery [`SyntaxKind::Bogus`] node or an
    /// explicit [`SyntaxKind::Error`] token.
    pub const HAS_ERROR: NodeFlags = NodeFlags(1 << 1);
    /// The subtree contains a `@[ ... ]` [`SyntaxKind::Calc`].
    pub const HAS_CALC: NodeFlags = NodeFlags(1 << 2);
    /// The subtree contains a `$macro$` [`SyntaxKind::MacroParam`].
    pub const HAS_MACRO: NodeFlags = NodeFlags(1 << 3);

    /// Whether every bit set in `other` is also set in `self`.
    pub fn contains(self, other: NodeFlags) -> bool {
        self.0 & other.0 == other.0
    }

    fn insert(&mut self, other: NodeFlags) {
        self.0 |= other.0;
    }
}

/// The flag bits a single element contributes on its own, before the upward
/// union of its descendants' bits.
fn own_flag_bits(kind: SyntaxKind) -> NodeFlags {
    match kind {
        SyntaxKind::Comment => NodeFlags::HAS_COMMENT,
        SyntaxKind::Error | SyntaxKind::Bogus => NodeFlags::HAS_ERROR,
        // The `Calc` node covers a well-formed calc; `CalcOpen` covers the rare
        // flattened case where the depth guard tripped before the node formed.
        SyntaxKind::Calc | SyntaxKind::CalcOpen => NodeFlags::HAS_CALC,
        SyntaxKind::MacroParam => NodeFlags::HAS_MACRO,
        _ => NodeFlags(0),
    }
}

/// A lossless syntax tree borrowing its source bytes.
pub struct GreenTree<'a> {
    source: &'a [u8],
    tape: Vec<Green>,
    /// Absolute byte offset of each element, derived from leaf widths.
    offsets: Vec<u32>,
    /// Parent element index per element (`u32::MAX` for the root).
    parents: Vec<u32>,
    /// Per-subtree [`NodeFlags`] for each element (see [`NodeFlags`]).
    flags: Vec<NodeFlags>,
    errors: Vec<SyntaxError>,
}

/// Parse `source` into a lossless [`GreenTree`] using the permissive superset.
pub fn parse(source: &[u8]) -> GreenTree<'_> {
    parse_with(source, Flavor::default())
}

/// Parse `source` into a lossless [`GreenTree`] with a specific [`Flavor`].
pub fn parse_with(source: &[u8], flavor: Flavor) -> GreenTree<'_> {
    let tokens = lex(source, flavor);
    let builder = Builder::with_capacity(tokens.len());
    let mut p = Parser {
        tokens: &tokens,
        pos: 0,
        builder,
        errors: Vec::new(),
        depth: 0,
    };
    p.builder.start_node(SyntaxKind::Root);
    p.parse_items(false);
    p.builder.finish_node();
    let Parser {
        builder, errors, ..
    } = p;
    builder.finish(source, errors)
}

impl<'a> GreenTree<'a> {
    /// The full source the tree was parsed from.
    pub fn source(&self) -> &'a [u8] {
        self.source
    }

    /// The root [`SyntaxNode`] (always [`SyntaxKind::Root`]).
    pub fn root(&self) -> SyntaxNode<'_, 'a> {
        SyntaxNode { tree: self, idx: 0 }
    }

    /// Recoverable problems found during parsing (empty for clean input).
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    /// Every leaf [`SyntaxToken`] in document order.
    ///
    /// Because the green tape is stored in pre-order, the leaves are *already*
    /// sequenced — this merely skips the interior nodes, so it is O(n) with no
    /// tree walking. Concatenating [`SyntaxToken::text`] over this iterator
    /// reproduces [`source`](GreenTree::source): the lossless invariant in
    /// iterator form. Ideal for highlighters and other leaf-oriented tooling.
    pub fn tokens(&self) -> impl Iterator<Item = SyntaxToken<'_, 'a>> + '_ {
        (0..self.tape.len() as u32).filter_map(move |i| match self.tape[i as usize] {
            Green::Token { .. } => Some(SyntaxToken { tree: self, idx: i }),
            Green::Node { .. } => None,
        })
    }

    /// Reconstruct the source by walking the tree's leaves via the cursor API.
    ///
    /// Equals [`GreenTree::source`] for any tree; the lossless invariant.
    pub fn reconstruct(&self) -> Vec<u8> {
        fn collect(node: SyntaxNode<'_, '_>, out: &mut Vec<u8>) {
            for el in node.children() {
                match el {
                    SyntaxElement::Token(t) => out.extend_from_slice(t.text()),
                    SyntaxElement::Node(n) => collect(n, out),
                }
            }
        }
        let mut out = Vec::with_capacity(self.source.len());
        collect(self.root(), &mut out);
        out
    }

    /// An indented S-expression dump of the tree, for debugging and tests.
    pub fn debug_tree(&self) -> String {
        fn go(el: SyntaxElement<'_, '_>, depth: usize, out: &mut String) {
            for _ in 0..depth {
                out.push_str("  ");
            }
            match el {
                SyntaxElement::Node(n) => {
                    out.push_str(&format!("{:?}\n", n.kind()));
                    for c in n.children() {
                        go(c, depth + 1, out);
                    }
                }
                SyntaxElement::Token(t) => {
                    out.push_str(&format!(
                        "{:?} {:?}\n",
                        t.kind(),
                        String::from_utf8_lossy(t.text())
                    ));
                }
            }
        }
        let mut out = String::new();
        go(SyntaxElement::Node(self.root()), 0, &mut out);
        out
    }

    fn element(&self, idx: u32) -> SyntaxElement<'_, 'a> {
        match self.tape[idx as usize] {
            Green::Node { .. } => SyntaxElement::Node(SyntaxNode { tree: self, idx }),
            Green::Token { .. } => SyntaxElement::Token(SyntaxToken { tree: self, idx }),
        }
    }
}

/// Builds the flat arena. The hot path pushes only to a single `tape` vector;
/// the derived `offsets`/`parents` caches are computed in two tight linear
/// passes in [`Builder::finish`]. (Filling those caches incrementally during
/// the build measured ~6% slower — three interleaved vector pushes hurt the
/// hot loop more than two cache-friendly sequential passes cost.)
struct Builder {
    tape: Vec<Green>,
    stack: Vec<usize>,
    len_stack: Vec<u32>,
}

impl Builder {
    fn with_capacity(tokens: usize) -> Self {
        // Elements = tokens + interior nodes; nodes are a fraction of tokens.
        Builder {
            tape: Vec::with_capacity(tokens + tokens / 2),
            stack: Vec::new(),
            len_stack: Vec::new(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        let idx = self.tape.len();
        self.tape.push(Green::Node {
            kind,
            len: 0,
            end: 0,
        });
        self.stack.push(idx);
        self.len_stack.push(0);
    }

    fn token(&mut self, kind: SyntaxKind, len: u32) {
        self.tape.push(Green::Token { kind, len });
        if let Some(top) = self.len_stack.last_mut() {
            *top += len;
        }
    }

    fn finish_node(&mut self) {
        let idx = self.stack.pop().expect("finish_node without start_node");
        let len = self.len_stack.pop().unwrap();
        let end = self.tape.len() as u32;
        if let Green::Node { len: l, end: e, .. } = &mut self.tape[idx] {
            *l = len;
            *e = end;
        }
        if let Some(top) = self.len_stack.last_mut() {
            *top += len;
        }
    }

    fn finish(self, source: &[u8], errors: Vec<SyntaxError>) -> GreenTree<'_> {
        debug_assert!(self.stack.is_empty(), "unfinished nodes remain");
        let tape = self.tape;

        // Absolute offset of each element = sum of leaf widths preceding it.
        let mut offsets = vec![0u32; tape.len()];
        let mut acc = 0u32;
        for (i, g) in tape.iter().enumerate() {
            offsets[i] = acc;
            if let Green::Token { len, .. } = g {
                acc += *len;
            }
        }
        debug_assert_eq!(
            acc as usize,
            source.len(),
            "leaf widths do not cover the source"
        );

        // Nearest-enclosing-node parent links via an end-delimited stack.
        let mut parents = vec![u32::MAX; tape.len()];
        let mut stack: Vec<(u32, u32)> = Vec::new(); // (node idx, end)
        for (i, g) in tape.iter().enumerate() {
            while let Some(&(_, end)) = stack.last() {
                if end as usize <= i {
                    stack.pop();
                } else {
                    break;
                }
            }
            if let Some(&(p, _)) = stack.last() {
                parents[i] = p;
            }
            if let Green::Node { end, .. } = g {
                stack.push((i as u32, *end));
            }
        }

        // Per-subtree flag bits. Seed each element with its own bits, then fold
        // each element's flags into its parent. Pre-order means every descendant
        // has a higher index than its ancestors, so iterating high→low finalizes
        // a node's flags (all descendants already folded in) before it folds into
        // its own parent — one linear pass, no extra tree walk. (The root at
        // index 0 has no parent, so the range starts at 1.)
        let mut flags: Vec<NodeFlags> = tape.iter().map(|g| own_flag_bits(g.kind())).collect();
        for i in (1..tape.len()).rev() {
            let p = parents[i];
            if p != u32::MAX {
                let child = flags[i];
                flags[p as usize].insert(child);
            }
        }

        GreenTree {
            source,
            tape,
            offsets,
            parents,
            flags,
            errors,
        }
    }
}

/// Maximum nesting depth before the parser stops recursing, flattens the
/// remainder into flat leaf tokens, and records a diagnostic. This guards
/// against a stack overflow on pathological input like `{{{{…}}}}` or
/// `@[((((…))))]` thousands deep, while sitting far above any real game file's
/// nesting. It applies independently to block nesting and to calc-expression
/// nesting (each has its own recursion).
const MAX_DEPTH: u32 = 256;

struct Parser<'t> {
    tokens: &'t [Tok],
    pos: usize,
    builder: Builder,
    errors: Vec<SyntaxError>,
    /// Current block-nesting depth, compared against [`MAX_DEPTH`].
    depth: u32,
}

impl Parser<'_> {
    fn peek(&self) -> Option<SyntaxKind> {
        self.tokens.get(self.pos).map(|t| t.kind)
    }

    fn bump(&mut self) {
        let t = self.tokens[self.pos];
        self.builder.token(t.kind, t.len);
        self.pos += 1;
    }

    fn bump_trivia(&mut self) {
        while matches!(self.peek(), Some(k) if k.is_trivia()) {
            self.bump();
        }
    }

    /// Kind of the first non-trivia token at or after `from`.
    fn next_significant(&self, from: usize) -> Option<SyntaxKind> {
        self.tokens[from..]
            .iter()
            .map(|t| t.kind)
            .find(|k| !k.is_trivia())
    }

    fn parse_items(&mut self, in_block: bool) {
        loop {
            match self.peek() {
                None => break,
                Some(SyntaxKind::CloseBrace) if in_block => break, // caller eats `}`
                Some(SyntaxKind::CloseBrace) => {
                    // Unmatched `}` at the top level: record and wrap in Bogus.
                    let t = self.tokens[self.pos];
                    self.errors.push(SyntaxError {
                        message: "unmatched '}'".into(),
                        range: (t.start, t.start + t.len),
                    });
                    self.builder.start_node(SyntaxKind::Bogus);
                    self.bump();
                    self.builder.finish_node();
                }
                Some(k) if k.is_trivia() => self.bump(),
                Some(_) => self.parse_item(),
            }
        }
    }

    fn parse_item(&mut self) {
        match self.peek() {
            Some(k) if k.is_scalar() => {
                if self.next_significant(self.pos + 1) == Some(SyntaxKind::Operator) {
                    // key <op> value
                    self.builder.start_node(SyntaxKind::Field);
                    self.bump(); // key
                    self.bump_trivia();
                    self.bump(); // operator
                    self.bump_trivia();
                    self.parse_value();
                    self.builder.finish_node();
                } else {
                    // bare scalar (array element / loose value)
                    self.bump();
                }
            }
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            // Operators, brackets, bang, macros in item position: keep verbatim.
            Some(_) => self.bump(),
            None => {}
        }
    }

    fn parse_value(&mut self) {
        match self.peek() {
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            Some(SyntaxKind::Unquoted)
                if self.next_significant(self.pos + 1) == Some(SyntaxKind::OpenBrace) =>
            {
                // headered block: `rgb { ... }`, `hsv { ... }`, tag { ... }
                self.builder.start_node(SyntaxKind::HeaderedBlock);
                self.bump(); // header scalar
                self.bump_trivia();
                self.parse_block();
                self.builder.finish_node();
            }
            Some(k) if !k.is_trivia() && k != SyntaxKind::CloseBrace => self.bump(),
            // missing value (e.g. `a =` at EOF, or `a = }`): emit nothing.
            _ => {}
        }
    }

    fn parse_block(&mut self) {
        let open = self.tokens[self.pos];
        self.builder.start_node(SyntaxKind::Block);
        self.bump(); // `{`
        self.depth += 1;
        if self.depth >= MAX_DEPTH {
            // Pathologically deep nesting. Rather than recurse (and risk a
            // stack overflow), consume the rest of this block — including
            // everything nested inside it — as flat leaf tokens. The tree
            // stays lossless; it just loses structure past this point.
            self.errors.push(SyntaxError {
                message: "maximum nesting depth exceeded; structure flattened".into(),
                range: (open.start, open.start + open.len),
            });
            self.flatten_to_block_close();
        } else {
            self.parse_items(true);
            if self.peek() == Some(SyntaxKind::CloseBrace) {
                self.bump(); // `}`
            } else {
                self.errors.push(SyntaxError {
                    message: "unclosed '{'".into(),
                    range: (open.start, open.start + open.len),
                });
            }
        }
        self.depth -= 1;
        self.builder.finish_node();
    }

    /// Consume the remainder of the current block — including any nested braces
    /// — as flat leaf tokens, stopping just after the matching `}` (or at EOF).
    /// The opening `{` has already been consumed, so brace depth starts at 1.
    /// Used by the depth guard to bound recursion without sacrificing
    /// losslessness (every token is still emitted, just unstructured).
    fn flatten_to_block_close(&mut self) {
        let mut balance = 1u32;
        while let Some(k) = self.peek() {
            match k {
                SyntaxKind::OpenBrace => balance += 1,
                SyntaxKind::CloseBrace => balance -= 1,
                _ => {}
            }
            self.bump();
            if balance == 0 {
                return; // emitted the matching `}`
            }
        }
        // EOF before the block closed — the flatten diagnostic already covers
        // it, so no separate "unclosed" diagnostic is added here.
    }

    /// Parse a `@[ ... ]` calc. The current token is [`SyntaxKind::CalcOpen`].
    ///
    /// The lexer has already split the interior into calc tokens, so the whole
    /// region is a contiguous run `CalcOpen, <interior...>, [CalcClose]`. The
    /// interior is parsed (precedence-climbing) into a temporary element tree by
    /// [`parse_calc_interior`], then emitted into the builder in source order —
    /// no checkpoints needed, since a lossless tree always emits leaves in order
    /// and only adds node boundaries.
    fn parse_calc(&mut self) {
        let open = self.tokens[self.pos];
        self.builder.start_node(SyntaxKind::Calc);
        self.bump(); // `@[`

        // The interior is everything up to the matching `]` (or EOF). The lexer
        // only ever emits calc tokens between a CalcOpen and its CalcClose, so
        // no calc token can leak past this slice into the outer parser.
        let from = self.pos;
        while !matches!(self.peek(), Some(SyntaxKind::CalcClose) | None) {
            self.pos += 1;
        }
        let (elems, overflowed) = parse_calc_interior(&self.tokens[from..self.pos]);
        for el in &elems {
            emit_calc(&mut self.builder, el);
        }
        if overflowed {
            self.errors.push(SyntaxError {
                message: "maximum calc nesting depth exceeded; structure flattened".into(),
                range: (open.start, open.start + open.len),
            });
        }

        if self.peek() == Some(SyntaxKind::CalcClose) {
            self.bump(); // `]`
        } else {
            self.errors.push(SyntaxError {
                message: "unclosed '@['".into(),
                range: (open.start, open.start + open.len),
            });
        }
        self.builder.finish_node();
    }
}

// ---------------------------------------------------------------------------
// Calc expression grammar (interior of `@[ ... ]`).
//
// operands : Number | CalcIdent | `(` expr `)` | (`-`|`+`) operand
// binary   : `+` `-` (looser) and `*` `/` (tighter), left-associative
//
// Parsed precedence-climbing into a temporary `CalcElem` tree, which is then
// walked in pre-order to emit green tokens/nodes. Interior whitespace rides
// along as trivia leaves, attached to whichever node encloses it, so the region
// round-trips byte-for-byte.
// ---------------------------------------------------------------------------

/// A node in the temporary calc tree (see [`parse_calc_interior`]).
enum CalcElem {
    /// A leaf carrying a lexer token's kind and width.
    Leaf { kind: SyntaxKind, len: u32 },
    /// An interior expression node with children in source order.
    Node {
        kind: SyntaxKind,
        children: Vec<CalcElem>,
    },
}

/// Walk a [`CalcElem`] in pre-order, emitting its tokens/nodes into `b`.
fn emit_calc(b: &mut Builder, el: &CalcElem) {
    match el {
        CalcElem::Leaf { kind, len } => b.token(*kind, *len),
        CalcElem::Node { kind, children } => {
            b.start_node(*kind);
            for c in children {
                emit_calc(b, c);
            }
            b.finish_node();
        }
    }
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
/// flat list of [`CalcElem`]s: leading trivia, the expression, then any trailing
/// trivia / unconsumed (malformed) tokens. Total over any token slice. The
/// returned bool is `true` if the [`MAX_DEPTH`] guard tripped (pathologically
/// deep parens/unary), in which case parsing stopped descending but every token
/// is still emitted as a flat leaf.
fn parse_calc_interior(toks: &[Tok]) -> (Vec<CalcElem>, bool) {
    let mut c = CalcCursor {
        toks,
        pos: 0,
        depth: 0,
        overflowed: false,
    };
    let mut out = Vec::new();
    c.eat_trivia(&mut out);
    if c.has_more() {
        out.push(c.parse_expr(0));
    }
    // Trailing trivia plus, for malformed input like `@[1 2]`, any leftover
    // tokens the expression grammar did not consume — kept as leaves so the
    // region still tiles its source.
    while c.has_more() {
        out.push(c.leaf());
    }
    (out, c.overflowed)
}

/// A cursor over a calc's interior tokens used by [`parse_calc_interior`].
struct CalcCursor<'t> {
    toks: &'t [Tok],
    pos: usize,
    /// Current operand-nesting depth, compared against [`MAX_DEPTH`].
    depth: u32,
    /// Set once the depth guard trips (see [`CalcCursor::parse_operand`]).
    overflowed: bool,
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
        let t = self.toks[self.pos];
        self.pos += 1;
        CalcElem::Leaf {
            kind: t.kind,
            len: t.len,
        }
    }

    /// Push any run of trivia tokens at the cursor onto `out`.
    fn eat_trivia(&mut self, out: &mut Vec<CalcElem>) {
        while matches!(self.peek(), Some(k) if k.is_trivia()) {
            out.push(self.leaf());
        }
    }

    /// Parse an operand: a unary expression, a parenthesized expression, a
    /// number/identifier leaf, or (for malformed input) whatever leaf is here.
    fn parse_operand(&mut self) -> CalcElem {
        // Depth guard: `@[((((…))))]` and `@[----…x]` recurse through here.
        // Past the limit, stop descending and hand the current token back as a
        // leaf; the remaining interior tokens then fall through to flat leaves
        // in `parse_calc_interior`. Every operand frame increments `depth`, so
        // this bounds the whole calc recursion while staying lossless. (Flat
        // chains like `1+1+1` do not nest — `parse_expr` handles those in its
        // loop — so they never approach the limit.)
        if self.depth >= MAX_DEPTH {
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
                self.eat_trivia(&mut children);
                if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                    children.push(self.parse_operand());
                }
                CalcElem::Node {
                    kind: SyntaxKind::UnaryExpr,
                    children,
                }
            }
            Some(SyntaxKind::OpenParen) => {
                let mut children = vec![self.leaf()]; // `(`
                self.eat_trivia(&mut children);
                if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                    children.push(self.parse_expr(0));
                }
                self.eat_trivia(&mut children);
                if self.peek() == Some(SyntaxKind::CloseParen) {
                    children.push(self.leaf()); // `)`
                }
                CalcElem::Node {
                    kind: SyntaxKind::ParenExpr,
                    children,
                }
            }
            // Number, CalcIdent, or — in malformed input — a stray operator.
            Some(_) => self.leaf(),
            None => unreachable!("parse_operand called at end of interior"),
        }
    }

    /// Precedence-climbing parse of a (sub)expression with binding power floor
    /// `min_bp`. Trivia before a candidate operator is held speculatively and
    /// either folded into the binary node or rewound to the enclosing context.
    fn parse_expr(&mut self, min_bp: u8) -> CalcElem {
        let mut lhs = self.parse_operand();
        // Length of the left-associative chain folded in this call. Unlike
        // parens/unary, a flat chain (`@[1+1+1+…]`) is folded *iteratively*
        // here, so `parse_operand`'s depth counter never sees it — yet it still
        // builds a `BinaryExpr` tree nested `folds` deep, which `emit_calc` and
        // the tree's recursive `Drop` would later walk. Cap it the same way so
        // no calc shape can overflow the stack; the tail falls to flat leaves.
        let mut folds = 0u32;
        loop {
            // Peek past trivia for the next operator; rewind if it isn't one
            // (or binds too loosely) so that trivia stays with the outer node.
            let save = self.pos;
            let mut trivia = Vec::new();
            self.eat_trivia(&mut trivia);
            let bp = self.peek().and_then(infix_bp);
            match bp {
                Some((l_bp, r_bp)) if l_bp >= min_bp && folds < MAX_DEPTH => {
                    folds += 1;
                    let mut children = vec![lhs];
                    children.append(&mut trivia);
                    children.push(self.leaf()); // operator
                    self.eat_trivia(&mut children);
                    if self.has_more() && self.peek() != Some(SyntaxKind::CloseParen) {
                        children.push(self.parse_expr(r_bp));
                    }
                    lhs = CalcElem::Node {
                        kind: SyntaxKind::BinaryExpr,
                        children,
                    };
                }
                _ => {
                    // If a foldable operator is present and we are stopping only
                    // because the fold cap was hit, flag the overflow (the tail
                    // becomes flat leaves in `parse_calc_interior`).
                    if matches!(bp, Some((l_bp, _)) if l_bp >= min_bp) {
                        self.overflowed = true;
                    }
                    self.pos = save;
                    break;
                }
            }
        }
        lhs
    }
}

// ---------------------------------------------------------------------------
// Cursor ("red layer"): lightweight handles into the tree that compute
// positions on demand. `'t` borrows the tree, `'a` is the source lifetime.
// ---------------------------------------------------------------------------

/// A handle to an interior node in a [`GreenTree`].
#[derive(Clone, Copy)]
pub struct SyntaxNode<'t, 'a> {
    tree: &'t GreenTree<'a>,
    idx: u32,
}

/// A handle to a leaf token in a [`GreenTree`].
#[derive(Clone, Copy)]
pub struct SyntaxToken<'t, 'a> {
    tree: &'t GreenTree<'a>,
    idx: u32,
}

/// Either a [`SyntaxNode`] or a [`SyntaxToken`].
#[derive(Clone, Copy)]
pub enum SyntaxElement<'t, 'a> {
    Node(SyntaxNode<'t, 'a>),
    Token(SyntaxToken<'t, 'a>),
}

impl<'t, 'a> SyntaxNode<'t, 'a> {
    /// This node's kind.
    pub fn kind(&self) -> SyntaxKind {
        self.tree.tape[self.idx as usize].kind()
    }

    /// The half-open byte range this node spans in the source.
    pub fn text_range(&self) -> (u32, u32) {
        let start = self.tree.offsets[self.idx as usize];
        (start, start + self.tree.tape[self.idx as usize].len())
    }

    /// The source bytes this node spans (a lossless slice of the subtree).
    pub fn text(&self) -> &'a [u8] {
        let (s, e) = self.text_range();
        &self.tree.source[s as usize..e as usize]
    }

    /// The parent node, or `None` for the root.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = self.tree.parents[self.idx as usize];
        (p != u32::MAX).then_some(SyntaxNode {
            tree: self.tree,
            idx: p,
        })
    }

    /// The precomputed [`NodeFlags`] summarizing this node's whole subtree (see
    /// [`NodeFlags`]). An O(1) lookup — no subtree walk.
    pub fn flags(&self) -> NodeFlags {
        self.tree.flags[self.idx as usize]
    }

    /// Whether this subtree contains a [`SyntaxKind::Bogus`]/[`SyntaxKind::Error`]
    /// recovery element. Lets a linter cheaply skip — or downgrade — semantic
    /// checks over a region the parser already flagged as malformed.
    pub fn has_error(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_ERROR)
    }

    /// Whether this subtree contains a comment (O(1); see [`NodeFlags`]).
    pub fn has_comment(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_COMMENT)
    }

    /// Whether this subtree contains a `@[ ... ]` calc (O(1); see [`NodeFlags`]).
    pub fn contains_calc(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_CALC)
    }

    /// Whether this subtree contains a `$macro$` parameter (O(1); see [`NodeFlags`]).
    pub fn contains_macro(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_MACRO)
    }

    /// All direct children, nodes and tokens, in source order.
    pub fn children(&self) -> Children<'t, 'a> {
        let end = match self.tree.tape[self.idx as usize] {
            Green::Node { end, .. } => end,
            Green::Token { .. } => self.idx + 1,
        };
        Children {
            tree: self.tree,
            next: self.idx + 1,
            end,
        }
    }

    /// Direct child nodes only.
    pub fn child_nodes(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + 't {
        self.children().filter_map(SyntaxElement::into_node)
    }

    /// Direct child tokens only.
    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + 't {
        self.children().filter_map(SyntaxElement::into_token)
    }
}

impl<'t, 'a> SyntaxToken<'t, 'a> {
    /// This token's kind.
    pub fn kind(&self) -> SyntaxKind {
        self.tree.tape[self.idx as usize].kind()
    }

    /// The half-open byte range this token spans in the source.
    pub fn text_range(&self) -> (u32, u32) {
        let start = self.tree.offsets[self.idx as usize];
        (start, start + self.tree.tape[self.idx as usize].len())
    }

    /// The raw source bytes of this token.
    pub fn text(&self) -> &'a [u8] {
        let (s, e) = self.text_range();
        &self.tree.source[s as usize..e as usize]
    }

    /// The parent node.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = self.tree.parents[self.idx as usize];
        (p != u32::MAX).then_some(SyntaxNode {
            tree: self.tree,
            idx: p,
        })
    }

    /// If this is an [`SyntaxKind::Operator`] token, the concrete [`Operator`].
    pub fn operator(&self) -> Option<Operator> {
        if self.kind() != SyntaxKind::Operator {
            return None;
        }
        Some(match self.text() {
            b"<" => Operator::LessThan,
            b"<=" => Operator::LessThanEqual,
            b">" => Operator::GreaterThan,
            b">=" => Operator::GreaterThanEqual,
            b"==" => Operator::Exact,
            b"=" => Operator::Equal,
            b"!=" => Operator::NotEqual,
            b"?=" => Operator::Exists,
            _ => return None,
        })
    }
}

impl<'t, 'a> SyntaxElement<'t, 'a> {
    /// This element's kind.
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxElement::Node(n) => n.kind(),
            SyntaxElement::Token(t) => t.kind(),
        }
    }

    /// The source bytes this element spans (its subtree, for a node).
    pub fn text(&self) -> &'a [u8] {
        match self {
            SyntaxElement::Node(n) => n.text(),
            SyntaxElement::Token(t) => t.text(),
        }
    }

    /// Unwrap to a node, or `None` if this is a token.
    pub fn into_node(self) -> Option<SyntaxNode<'t, 'a>> {
        match self {
            SyntaxElement::Node(n) => Some(n),
            SyntaxElement::Token(_) => None,
        }
    }

    /// Unwrap to a token, or `None` if this is a node.
    pub fn into_token(self) -> Option<SyntaxToken<'t, 'a>> {
        match self {
            SyntaxElement::Token(t) => Some(t),
            SyntaxElement::Node(_) => None,
        }
    }
}

/// Iterator over a node's direct children (see [`SyntaxNode::children`]).
pub struct Children<'t, 'a> {
    tree: &'t GreenTree<'a>,
    next: u32,
    end: u32,
}

impl<'t, 'a> Iterator for Children<'t, 'a> {
    type Item = SyntaxElement<'t, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.end {
            return None;
        }
        let idx = self.next;
        // Skip past this child's whole subtree to reach the next sibling.
        self.next = match self.tree.tape[idx as usize] {
            Green::Node { end, .. } => end,
            Green::Token { .. } => idx + 1,
        };
        Some(self.tree.element(idx))
    }
}

// ---------------------------------------------------------------------------
// Typed AST: "ungrammar-style" strongly-typed views over the red layer. Each
// wrapper is a thin newtype around a `SyntaxNode` of a fixed `SyntaxKind`, with
// accessors that locate children by kind/position. They are zero-copy `Copy`
// views computed on demand, so constructing one is free; `syntax()` always
// recovers the untyped node for ranges, text, or raw traversal.
// ---------------------------------------------------------------------------

/// A typed view over a [`SyntaxNode`] of a particular [`SyntaxKind`].
///
/// Mirrors rust-analyzer's generated AST: [`AstNode::cast`] succeeds only when
/// the node's kind matches, and [`AstNode::syntax`] recovers the untyped node.
pub trait AstNode<'t, 'a>: Sized {
    /// Wrap `node` if its kind matches this type, otherwise `None`.
    fn cast(node: SyntaxNode<'t, 'a>) -> Option<Self>;
    /// The underlying untyped node.
    fn syntax(&self) -> SyntaxNode<'t, 'a>;
}

macro_rules! ast_nodes {
    ($($(#[$m:meta])* $name:ident => $kind:ident),+ $(,)?) => {$(
        $(#[$m])*
        #[derive(Clone, Copy)]
        pub struct $name<'t, 'a>(SyntaxNode<'t, 'a>);

        impl<'t, 'a> AstNode<'t, 'a> for $name<'t, 'a> {
            fn cast(node: SyntaxNode<'t, 'a>) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some($name(node))
                } else {
                    None
                }
            }
            fn syntax(&self) -> SyntaxNode<'t, 'a> {
                self.0
            }
        }
    )+};
}

ast_nodes! {
    /// The whole document ([`SyntaxKind::Root`]).
    Root => Root,
    /// A `key <op> value` field ([`SyntaxKind::Field`]).
    Field => Field,
    /// A `{ ... }` block ([`SyntaxKind::Block`]).
    Block => Block,
    /// A tagged block such as `rgb { 1 2 3 }` ([`SyntaxKind::HeaderedBlock`]).
    HeaderedBlock => HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation ([`SyntaxKind::Calc`]).
    Calc => Calc,
    /// A binary arithmetic expression inside a [`Calc`] ([`SyntaxKind::BinaryExpr`]).
    BinaryExpr => BinaryExpr,
    /// A prefix `-`/`+` expression inside a [`Calc`] ([`SyntaxKind::UnaryExpr`]).
    UnaryExpr => UnaryExpr,
    /// A parenthesized expression inside a [`Calc`] ([`SyntaxKind::ParenExpr`]).
    ParenExpr => ParenExpr,
}

/// An entry within a [`Root`] or [`Block`].
#[derive(Clone, Copy)]
pub enum Item<'t, 'a> {
    /// A `key <op> value` field.
    Field(Field<'t, 'a>),
    /// A bare value: an array element or a loose value.
    Value(Value<'t, 'a>),
    /// A significant element that is neither a field nor a value — e.g. a stray
    /// operator/bracket, a [`SyntaxKind::Bogus`] node, or the still-ungrouped
    /// tokens of a `[[param]]` block. Surfaced so the typed view drops nothing.
    Other(SyntaxElement<'t, 'a>),
}

/// A value in value position (the RHS of a [`Field`] or a bare array element).
#[derive(Clone, Copy)]
pub enum Value<'t, 'a> {
    /// A scalar token: [`SyntaxKind::Unquoted`], [`SyntaxKind::Quoted`],
    /// [`SyntaxKind::Variable`], or [`SyntaxKind::MacroParam`].
    Scalar(SyntaxToken<'t, 'a>),
    /// A `{ ... }` block.
    Block(Block<'t, 'a>),
    /// A tagged block such as `rgb { 1 2 3 }`.
    Headered(HeaderedBlock<'t, 'a>),
    /// A `@[ ... ]` calculation.
    Calc(Calc<'t, 'a>),
}

/// An arithmetic expression inside a [`Calc`].
#[derive(Clone, Copy)]
pub enum Expr<'t, 'a> {
    /// `lhs <op> rhs`.
    Binary(BinaryExpr<'t, 'a>),
    /// `-operand` / `+operand`.
    Unary(UnaryExpr<'t, 'a>),
    /// `( inner )`.
    Paren(ParenExpr<'t, 'a>),
    /// A numeric literal ([`SyntaxKind::Number`]).
    Number(SyntaxToken<'t, 'a>),
    /// An operand identifier ([`SyntaxKind::CalcIdent`]).
    Ident(SyntaxToken<'t, 'a>),
}

/// Strip a leading and/or trailing `"` from a quoted token's raw bytes. Handles
/// the unterminated case (only a leading quote) and the degenerate `"`/`""`.
fn strip_quotes(b: &[u8]) -> &[u8] {
    let b = b.strip_prefix(b"\"").unwrap_or(b);
    b.strip_suffix(b"\"").unwrap_or(b)
}

impl<'t, 'a> SyntaxToken<'t, 'a> {
    /// The semantic scalar value of this token: its raw bytes, but with the
    /// surrounding quotes stripped for a [`SyntaxKind::Quoted`] token (escapes
    /// inside the quotes are left as-is). This is the right input for the
    /// numeric/boolean coercions on [`Scalar`].
    pub fn as_scalar(&self) -> Scalar<'a> {
        let text = self.text();
        let bytes = if self.kind() == SyntaxKind::Quoted {
            strip_quotes(text)
        } else {
            text
        };
        Scalar::new(bytes)
    }
}

/// First significant child element of `node` after skipping leading trivia and
/// (for blocks) the delimiting braces — used to classify entries.
fn child_items<'t, 'a>(node: SyntaxNode<'t, 'a>) -> impl Iterator<Item = Item<'t, 'a>> + 't {
    node.children().filter_map(|el| {
        let kind = el.kind();
        if kind.is_trivia() || matches!(kind, SyntaxKind::OpenBrace | SyntaxKind::CloseBrace) {
            return None;
        }
        match el {
            SyntaxElement::Node(n) if kind == SyntaxKind::Field => Field::cast(n).map(Item::Field),
            _ => Some(match Value::cast_element(el) {
                Some(v) => Item::Value(v),
                None => Item::Other(el),
            }),
        }
    })
}

impl<'t, 'a> Root<'t, 'a> {
    /// Every top-level entry, in source order (trivia skipped).
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + 't {
        child_items(self.syntax())
    }

    /// The top-level `key = value` fields.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + 't {
        self.items().filter_map(Item::into_field)
    }
}

impl<'t, 'a> Field<'t, 'a> {
    /// The key scalar token (the left-hand side).
    pub fn key(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax().child_tokens().find(|t| t.kind().is_scalar())
    }

    /// The operator token (`=`, `==`, `?=`, `<`, …).
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::Operator)
    }

    /// The concrete [`Operator`], re-derived from the operator token's text.
    pub fn op(&self) -> Option<Operator> {
        self.op_token().and_then(|t| t.operator())
    }

    /// The value (the right-hand side), or `None` if it is missing.
    pub fn value(&self) -> Option<Value<'t, 'a>> {
        let mut after_op = false;
        for el in self.syntax().children() {
            if el.kind().is_trivia() {
                continue;
            }
            if !after_op {
                after_op = el.kind() == SyntaxKind::Operator;
                continue;
            }
            return Value::cast_element(el);
        }
        None
    }
}

impl<'t, 'a> Block<'t, 'a> {
    /// Every entry between the braces, in source order (trivia skipped).
    pub fn entries(&self) -> impl Iterator<Item = Item<'t, 'a>> + 't {
        child_items(self.syntax())
    }

    /// The `key = value` fields directly inside this block.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_field)
    }

    /// The bare values (array elements) directly inside this block.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_value)
    }

    /// Whether the block has no entries (only braces, whitespace, comments).
    pub fn is_empty(&self) -> bool {
        self.entries().next().is_none()
    }
}

impl<'t, 'a> HeaderedBlock<'t, 'a> {
    /// The header scalar (e.g. `rgb`, `hsv`, a tag).
    pub fn header(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax().child_tokens().find(|t| t.kind().is_scalar())
    }

    /// The block that follows the header.
    pub fn block(&self) -> Option<Block<'t, 'a>> {
        self.syntax().child_nodes().find_map(Block::cast)
    }
}

impl<'t, 'a> Calc<'t, 'a> {
    /// The wrapped arithmetic expression (between `@[` and `]`).
    pub fn expr(&self) -> Option<Expr<'t, 'a>> {
        self.syntax().children().find_map(Expr::cast_element)
    }
}

impl<'t, 'a> BinaryExpr<'t, 'a> {
    /// The left operand.
    pub fn lhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax().children().find_map(Expr::cast_element)
    }

    /// The operator token (`+`, `-`, `*`, `/`).
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax().child_tokens().find(|t| {
            matches!(
                t.kind(),
                SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash
            )
        })
    }

    /// The right operand.
    pub fn rhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .children()
            .filter_map(Expr::cast_element)
            .nth(1)
    }
}

impl<'t, 'a> UnaryExpr<'t, 'a> {
    /// The prefix operator token (`-` or `+`).
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .child_tokens()
            .find(|t| matches!(t.kind(), SyntaxKind::Plus | SyntaxKind::Minus))
    }

    /// The operand the prefix applies to.
    pub fn operand(&self) -> Option<Expr<'t, 'a>> {
        self.syntax().children().find_map(Expr::cast_element)
    }
}

impl<'t, 'a> ParenExpr<'t, 'a> {
    /// The expression between the parentheses.
    pub fn inner(&self) -> Option<Expr<'t, 'a>> {
        self.syntax().children().find_map(Expr::cast_element)
    }
}

impl<'t, 'a> Item<'t, 'a> {
    /// The [`Field`] if this entry is one.
    pub fn into_field(self) -> Option<Field<'t, 'a>> {
        match self {
            Item::Field(f) => Some(f),
            _ => None,
        }
    }

    /// The [`Value`] if this entry is a bare value.
    pub fn into_value(self) -> Option<Value<'t, 'a>> {
        match self {
            Item::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl<'t, 'a> Value<'t, 'a> {
    /// Classify a child element as a value, or `None` if it cannot be one.
    fn cast_element(el: SyntaxElement<'t, 'a>) -> Option<Self> {
        match el {
            SyntaxElement::Token(t) if t.kind().is_scalar() => Some(Value::Scalar(t)),
            SyntaxElement::Token(_) => None,
            SyntaxElement::Node(n) => match n.kind() {
                SyntaxKind::Block => Block::cast(n).map(Value::Block),
                SyntaxKind::HeaderedBlock => HeaderedBlock::cast(n).map(Value::Headered),
                SyntaxKind::Calc => Calc::cast(n).map(Value::Calc),
                _ => None,
            },
        }
    }

    /// The scalar token, if this is [`Value::Scalar`].
    pub fn as_scalar(&self) -> Option<Scalar<'a>> {
        match self {
            Value::Scalar(t) => Some(t.as_scalar()),
            _ => None,
        }
    }

    /// The block, if this is [`Value::Block`].
    pub fn as_block(&self) -> Option<Block<'t, 'a>> {
        match self {
            Value::Block(b) => Some(*b),
            _ => None,
        }
    }

    /// The headered block, if this is [`Value::Headered`].
    pub fn as_headered(&self) -> Option<HeaderedBlock<'t, 'a>> {
        match self {
            Value::Headered(h) => Some(*h),
            _ => None,
        }
    }

    /// The calc, if this is [`Value::Calc`].
    pub fn as_calc(&self) -> Option<Calc<'t, 'a>> {
        match self {
            Value::Calc(c) => Some(*c),
            _ => None,
        }
    }

    /// Coerce a scalar value to `f64` (e.g. `1.000`, `-5.7`, `10.0f`).
    pub fn to_f64(&self) -> Option<f64> {
        self.as_scalar()?.to_f64().ok()
    }

    /// Coerce a scalar value to `i64`.
    pub fn to_i64(&self) -> Option<i64> {
        self.as_scalar()?.to_i64().ok()
    }

    /// Coerce a scalar value to `u64`.
    pub fn to_u64(&self) -> Option<u64> {
        self.as_scalar()?.to_u64().ok()
    }

    /// Coerce a scalar value to `bool` (`yes`/`no`).
    pub fn to_bool(&self) -> Option<bool> {
        self.as_scalar()?.to_bool().ok()
    }
}

impl<'t, 'a> Expr<'t, 'a> {
    /// Classify a child element as a calc expression, or `None`.
    fn cast_element(el: SyntaxElement<'t, 'a>) -> Option<Self> {
        match el {
            SyntaxElement::Node(n) => match n.kind() {
                SyntaxKind::BinaryExpr => BinaryExpr::cast(n).map(Expr::Binary),
                SyntaxKind::UnaryExpr => UnaryExpr::cast(n).map(Expr::Unary),
                SyntaxKind::ParenExpr => ParenExpr::cast(n).map(Expr::Paren),
                _ => None,
            },
            SyntaxElement::Token(t) => match t.kind() {
                SyntaxKind::Number => Some(Expr::Number(t)),
                SyntaxKind::CalcIdent => Some(Expr::Ident(t)),
                _ => None,
            },
        }
    }
}

impl<'a> GreenTree<'a> {
    /// The typed [`Root`] of the document.
    pub fn ast(&self) -> Root<'_, 'a> {
        Root::cast(self.root()).expect("the root node is always SyntaxKind::Root")
    }
}

// ---------------------------------------------------------------------------
// Formatter: walk the tree and re-emit it with normalized whitespace. Only
// whitespace is regenerated — every significant token, comment, and the BOM is
// reproduced byte-for-byte — so reparsing the output yields the same tree of
// significant tokens, and formatting is idempotent.
//
// House style:
//   * one entry per line, indented by `FormatOptions::indent` per nesting level;
//   * `key = value`, a single space around the operator;
//   * a block of only bare scalars stays inline (`{ 1 2 3 }`); any block with
//     fields, nested blocks, or comments breaks onto multiple lines; an empty
//     block collapses to `{}`;
//   * comments are preserved and always end their line, so a comment can never
//     swallow a following token; an author's blank line between entries is kept
//     (collapsed to a single blank line);
//   * the document ends in exactly one newline.
//
// Constructs the parser does not yet structure (`[[param]]` blocks, `Bogus`
// recovery nodes, calc internals) are reflowed conservatively: calc and Bogus
// nodes are reprinted verbatim, and loose tokens land one per line. Content is
// always preserved; only the layout of those rare constructs is rough.
// ---------------------------------------------------------------------------

/// Configuration for [`GreenTree::format`].
#[derive(Debug, Clone)]
pub struct FormatOptions {
    /// The string emitted once per nesting level of indentation. Defaults to a
    /// single tab (the Paradox convention); set it to spaces if preferred.
    pub indent: String,
}

impl Default for FormatOptions {
    fn default() -> Self {
        FormatOptions {
            indent: String::from("\t"),
        }
    }
}

/// Parse `source` and reformat it with the default [`FormatOptions`].
///
/// The result reparses to the same significant tokens as `source` and is
/// idempotent (`format(format(x)) == format(x)`).
pub fn format(source: &[u8]) -> Vec<u8> {
    parse(source).format(&FormatOptions::default())
}

impl<'a> GreenTree<'a> {
    /// Reformat the tree with the given [`FormatOptions`]. See the module-level
    /// formatter notes for the house style.
    pub fn format(&self, opts: &FormatOptions) -> Vec<u8> {
        let mut f = Fmt {
            out: Vec::with_capacity(self.source.len()),
            opts,
            comment_open: false,
            quote_open: false,
        };
        f.fmt_root(self.root());
        f.finish()
    }
}

/// Number of newline bytes in `bytes` (used to detect line breaks and the
/// author's blank lines within a whitespace run).
fn count_newlines(bytes: &[u8]) -> usize {
    bytes.iter().filter(|&&b| b == b'\n').count()
}

/// Whether a quoted token's bytes (`text` begins with `"`) are properly closed
/// by a `"` before running out — mirroring the lexer's escape handling. An
/// *un*closed quote (it ran to end of input) would absorb any byte the
/// formatter emits after it, so the formatter must not follow it with a newline.
fn quote_is_closed(text: &[u8]) -> bool {
    if text.first() != Some(&b'"') {
        return true; // not an opening quote; nothing to absorb
    }
    let mut i = 1;
    while i < text.len() {
        match text[i] {
            b'\\' => i += 2,
            b'"' => return true,
            _ => i += 1,
        }
    }
    false
}

/// Whether `el` is a bare scalar token eligible to keep its block inline.
fn is_inline_scalar(el: SyntaxElement<'_, '_>) -> bool {
    el.kind().is_scalar()
}

/// Collect the comments that sit *inline* within a field/headered-block line —
/// directly inside the node, or inside a nested headered block (a field's value
/// can be `rgb # c { … }`). Comments inside a [`SyntaxKind::Block`] are *not*
/// gathered: those belong to the block's own layout. Such inline comments are
/// hoisted ahead of the line so they can never sit mid-line or fall inside a
/// multi-line block value (which would break idempotence).
fn collect_inline_comments<'t, 'a>(node: SyntaxNode<'t, 'a>, out: &mut Vec<SyntaxToken<'t, 'a>>) {
    for el in node.children() {
        match el {
            SyntaxElement::Token(t) if t.kind() == SyntaxKind::Comment => out.push(t),
            SyntaxElement::Node(n)
                if matches!(n.kind(), SyntaxKind::Field | SyntaxKind::HeaderedBlock) =>
            {
                collect_inline_comments(n, out);
            }
            _ => {}
        }
    }
}

struct Fmt<'o> {
    out: Vec<u8>,
    opts: &'o FormatOptions,
    /// True when the current output line ends in a comment, so the next thing
    /// emitted must start on a new line (a comment runs to end of line).
    comment_open: bool,
    /// True when the last token emitted is an unterminated quoted string, which
    /// would absorb a following newline; suppresses the trailing newline.
    quote_open: bool,
}

impl Fmt<'_> {
    fn finish(mut self) -> Vec<u8> {
        // Exactly one trailing newline when there is content — unless the last
        // token is an unterminated quote, which would swallow it.
        if !self.out.is_empty() && self.out.last() != Some(&b'\n') && !self.quote_open {
            self.out.push(b'\n');
        }
        self.out
    }

    /// Emit raw layout bytes (braces, spaces, indentation). These reset both
    /// "open" flags: the last thing on the line is now ordinary content.
    fn push(&mut self, bytes: &[u8]) {
        self.out.extend_from_slice(bytes);
        self.comment_open = false;
        self.quote_open = false;
    }

    fn push_byte(&mut self, b: u8) {
        self.out.push(b);
        self.comment_open = false;
        self.quote_open = false;
    }

    /// Emit a token verbatim, tracking whether it leaves a quote open.
    fn emit_text(&mut self, text: &[u8]) {
        self.push(text);
        self.quote_open = !quote_is_closed(text);
    }

    /// Emit a comment verbatim, marking the line as comment-closed.
    fn emit_comment(&mut self, text: &[u8]) {
        self.push(text);
        self.comment_open = true;
    }

    /// Emit a newline (two for a preserved blank line), reopening the line.
    fn line_break(&mut self, blank: bool) {
        self.out.push(b'\n');
        if blank {
            self.out.push(b'\n');
        }
        self.comment_open = false;
        self.quote_open = false;
    }

    fn write_indent(&mut self, level: usize) {
        for _ in 0..level {
            self.out.extend_from_slice(self.opts.indent.as_bytes());
        }
    }

    fn fmt_root(&mut self, root: SyntaxNode<'_, '_>) {
        let children: Vec<SyntaxElement<'_, '_>> = root.children().collect();
        // A leading BOM is reproduced verbatim at the very top of the file.
        let start = match children.first() {
            Some(el) if el.kind() == SyntaxKind::Bom => {
                self.push(el.text());
                1
            }
            _ => 0,
        };
        self.fmt_items(&children[start..], 0, true);
    }

    /// Format a run of container children (significant items interleaved with
    /// trivia), one significant item per line at `indent`. `suppress_first`
    /// omits the break before the first item — true for the document root
    /// (no leading blank line), false for a block (the break follows its `{`).
    fn fmt_items(&mut self, items: &[SyntaxElement<'_, '_>], indent: usize, suppress_first: bool) {
        let mut first = true;
        let mut pending_blank = false;
        let mut ws_had_newline = true; // a fresh container behaves like a new line

        for &el in items {
            match el.kind() {
                SyntaxKind::Whitespace => {
                    let nls = count_newlines(el.text());
                    ws_had_newline = nls >= 1;
                    if nls >= 2 && !first {
                        pending_blank = true;
                    }
                }
                // A stray BOM mid-stream cannot occur (the lexer only emits it
                // first), but reproduce it verbatim if it ever does.
                SyntaxKind::Bom => self.push(el.text()),
                SyntaxKind::Comment => {
                    // Keep it trailing only if the previous item is on this very
                    // line and that line is not already closed by a comment.
                    let trailing = !first && !ws_had_newline && !self.comment_open;
                    if trailing {
                        self.push_byte(b' ');
                    } else {
                        if !(first && suppress_first) {
                            self.line_break(pending_blank);
                        }
                        self.write_indent(indent);
                    }
                    self.emit_comment(el.text());
                    pending_blank = false;
                    ws_had_newline = true;
                    first = false;
                }
                _ => {
                    if !(first && suppress_first) {
                        self.line_break(pending_blank);
                    }
                    self.write_indent(indent);
                    self.hoist_inline_comments(el, indent);
                    self.fmt_item(el, indent);
                    pending_blank = false;
                    ws_had_newline = false;
                    first = false;
                }
            }
        }
    }

    /// Emit any inline comments of a field/headered-block item as their own
    /// leading lines (see [`collect_inline_comments`]); a no-op otherwise. The
    /// caller has already written this line's indent.
    fn hoist_inline_comments(&mut self, el: SyntaxElement<'_, '_>, indent: usize) {
        let node = match el {
            SyntaxElement::Node(n)
                if matches!(n.kind(), SyntaxKind::Field | SyntaxKind::HeaderedBlock) =>
            {
                n
            }
            _ => return,
        };
        let mut leading = Vec::new();
        collect_inline_comments(node, &mut leading);
        for c in leading {
            self.emit_comment(c.text());
            self.line_break(false);
            self.write_indent(indent);
        }
    }

    /// Format one significant element (already positioned at the line start).
    fn fmt_item(&mut self, el: SyntaxElement<'_, '_>, indent: usize) {
        match el {
            SyntaxElement::Node(n) => match n.kind() {
                SyntaxKind::Field | SyntaxKind::HeaderedBlock => self.fmt_spaced(n, indent),
                SyntaxKind::Block => self.fmt_block(n, indent),
                // A calc is a self-contained expression and a Bogus node wraps
                // unstructured bytes — reprint either verbatim.
                _ => self.emit_text(n.text()),
            },
            // A loose scalar / operator / bracket / bang token: verbatim.
            SyntaxElement::Token(t) => self.emit_text(t.text()),
        }
    }

    /// Format a [`SyntaxKind::Field`] or [`SyntaxKind::HeaderedBlock`]: their
    /// significant children joined by single spaces (`key = value`,
    /// `rgb { … }`). Interior comments are *not* emitted here — the caller has
    /// already hoisted them ahead of the line via [`Fmt::hoist_inline_comments`]
    /// — so this only lays out significant tokens.
    fn fmt_spaced(&mut self, node: SyntaxNode<'_, '_>, indent: usize) {
        let mut first = true;
        for el in node.children() {
            match el.kind() {
                SyntaxKind::Whitespace | SyntaxKind::Bom | SyntaxKind::Comment => {}
                _ => {
                    if !first {
                        self.push_byte(b' ');
                    }
                    first = false;
                    self.fmt_item(el, indent);
                }
            }
        }
    }

    /// Format a [`SyntaxKind::Block`]. Inline when it holds only bare scalars;
    /// `{}` when empty; multiline otherwise. The closing `}` is emitted only if
    /// the block actually has one (an unclosed block keeps its token count).
    fn fmt_block(&mut self, block: SyntaxNode<'_, '_>, indent: usize) {
        self.push_byte(b'{');

        let children: Vec<SyntaxElement<'_, '_>> = block.children().collect();
        let has_close = matches!(children.last(), Some(el) if el.kind() == SyntaxKind::CloseBrace);
        // children[0] is always the opening `{`; drop it and the closing `}`.
        let inner_end = if has_close {
            children.len() - 1
        } else {
            children.len()
        };
        let inner = &children[1..inner_end];

        let has_comment = inner.iter().any(|el| el.kind() == SyntaxKind::Comment);
        let sig: Vec<SyntaxElement<'_, '_>> = inner
            .iter()
            .copied()
            .filter(|el| !el.kind().is_trivia())
            .collect();

        if sig.is_empty() && !has_comment {
            if has_close {
                self.push_byte(b'}');
            }
            return;
        }

        if has_close && !has_comment && sig.iter().all(|el| is_inline_scalar(*el)) {
            self.push_byte(b' ');
            for (i, el) in sig.iter().enumerate() {
                if i > 0 {
                    self.push_byte(b' ');
                }
                self.emit_text(el.text());
            }
            self.push(b" }");
            return;
        }

        self.fmt_items(inner, indent + 1, false);
        if has_close {
            self.line_break(false);
            self.write_indent(indent);
            self.push_byte(b'}');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    /// Assert the lexer tiles the input: contiguous spans covering `[0, len)`.
    fn assert_tiles(data: &[u8], flavor: Flavor) {
        let toks = lex(data, flavor);
        let mut at = 0u32;
        for t in &toks {
            assert_eq!(t.start, at, "gap/overlap at {} in {:?}", at, data);
            at += t.len;
        }
        assert_eq!(at as usize, data.len(), "tokens do not reach end of input");
    }

    /// Parse, then assert byte-exact round-trip via the cursor traversal.
    fn rt(data: &[u8]) {
        assert_tiles(data, Flavor::default());
        assert_tiles(data, Flavor::hoi4());
        let tree = parse(data);
        assert_eq!(
            tree.reconstruct(),
            data,
            "round-trip mismatch\n--- tree ---\n{}",
            tree.debug_tree()
        );
        assert_eq!(parse_with(data, Flavor::hoi4()).reconstruct(), data);
    }

    #[test]
    fn round_trip_comprehensive() {
        let cases: &[&[u8]] = &[
            b"",
            b"   ",
            b"\n\t ;; ",
            b"foo = bar",
            b"foo=bar",
            b"open={1 2}",
            b"field1=-100.535",
            br#""foo"="bar" "3"="1444.11.11""#,
            br#"custom_name="THE !@#$%^&*( '\"LEGION\"')""#,
            b"foo{bar=qux}",
            b"foo=abc#def\nbar=qux",
            b"flavor_tur.8=yes",
            b"dashed-identifier=yes",
            b"province_id = event_target:agenda_province",
            b"mult = value:job_weights_research_modifier|JOB|head_researcher|",
            b"@planet_standard_scale = 11",
            b"window_name = @default_window_name",
            b"value=\"win\"; a=b",
            b"foo = 0.3;",
            b"a = 1; b = 2;; c = 3;",
            b";;;key = value;;;",
            b"age > 16",
            b"a==b c<=d e>=f g!=h i?=j k<l m>n",
            b"position = { @[1-leopard_x] @leopard_y }",
            b"my_calc = @[(-half-half)*half]",
            b"a = @[ tier + 1 ]",
            b"a = @[1+2*3]",
            b"a = @[ (1 - leopard_x) ]",
            b"a = @[10.0f / 2.5]",
            b"a = @[ -x ]",
            b"a = @[((@var))]",
            b"a = @[]",
            b"a = @[   ]",
            b"a = @[1 2 3]", // malformed: loose operands, still lossless
            b"a = @[1 +",    // unterminated calc
            b"a = @[)*/]",   // malformed operators, still lossless
            b"[[scaled_skill] code here ] [[!var_name] other code ]",
            b"stats={{id=0 type=general} {id=1 type=admiral}}",
            b"868416617618464 = { 11777 4108 { 5632 4187=1089 } 0=1089 }",
            b"weird = $MACRO$ ( ) * +",
            b"name = $TIER|capital$",
            b"color = rgb { 255 0 255 }",
            b"hsv_color = hsv360 { 180 10 60 }",
            b"\xef\xbb\xbf# BOM\nfoo = bar",
            b"name = \"J\xc3\xa5hk\xc3\xa5m\xc3\xa5hkke\"",
            b"\xa7GRichard\xa7",
            b"a =",
            b"a = }",
            b"} stray",
            b"{{{{}}}}",
            b"a = { b = c",
        ];
        for c in cases {
            rt(c);
        }
    }

    #[test]
    fn round_trip_fixtures() {
        let fixtures: &[&[u8]] = &[
            include_bytes!("../../tests/fixtures/meta.txt"),
            include_bytes!("../../tests/fixtures/ck3-header.txt"),
            include_bytes!("../../tests/fixtures/campaign_stats.txt"),
            include_bytes!("../../tests/fixtures/string-array.txt"),
            include_bytes!("../../tests/fixtures/nested-hidden-obj.txt"),
        ];
        for f in fixtures {
            rt(f);
        }
    }

    #[test]
    fn structure_field() {
        let tree = parse(b"a = b");
        let root = tree.root();
        assert_eq!(root.kind(), SyntaxKind::Root);

        let fields: Vec<_> = root.child_nodes().collect();
        assert_eq!(fields.len(), 1);
        let field = fields[0];
        assert_eq!(field.kind(), SyntaxKind::Field);

        let kinds: Vec<_> = field.child_tokens().map(|t| t.kind()).collect();
        assert_eq!(
            kinds,
            [
                SyntaxKind::Unquoted,
                SyntaxKind::Whitespace,
                SyntaxKind::Operator,
                SyntaxKind::Whitespace,
                SyntaxKind::Unquoted
            ]
        );

        let toks: Vec<_> = field.child_tokens().collect();
        assert_eq!(toks[0].text(), b"a");
        assert_eq!(toks[2].operator(), Some(Operator::Equal));
        assert_eq!(toks[4].text(), b"b");
        assert_eq!(field.text(), b"a = b");
        assert!(tree.errors().is_empty());
    }

    #[test]
    fn structure_block_value() {
        let tree = parse(b"a = { b c }");
        let field = tree.root().child_nodes().next().unwrap();
        assert_eq!(field.kind(), SyntaxKind::Field);

        let block = field.child_nodes().next().unwrap();
        assert_eq!(block.kind(), SyntaxKind::Block);
        assert_eq!(block.text(), b"{ b c }");

        let scalars: Vec<_> = block
            .child_tokens()
            .filter(|t| t.kind() == SyntaxKind::Unquoted)
            .map(|t| t.text())
            .collect();
        assert_eq!(scalars, [b"b", b"c"]);
    }

    #[test]
    fn headered_color_block() {
        let tree = parse(b"color = rgb { 1 2 3 }");
        let field = tree.root().child_nodes().next().unwrap();
        let hb = field.child_nodes().next().unwrap();
        assert_eq!(hb.kind(), SyntaxKind::HeaderedBlock);
        assert_eq!(hb.text(), b"rgb { 1 2 3 }");

        let header = hb
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::Unquoted)
            .unwrap();
        assert_eq!(header.text(), b"rgb");

        let block = hb.child_nodes().next().unwrap();
        assert_eq!(block.kind(), SyntaxKind::Block);
    }

    #[test]
    fn macro_param_token() {
        let tree = parse(b"x = $TIER|capital$");
        let field = tree.root().child_nodes().next().unwrap();
        let m = field
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::MacroParam)
            .unwrap();
        assert_eq!(m.text(), b"$TIER|capital$");
    }

    #[test]
    fn non_equal_operator_preserved() {
        let tree = parse(b"age > 16");
        let field = tree.root().child_nodes().next().unwrap();
        let op = field
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::Operator)
            .unwrap();
        assert_eq!(op.operator(), Some(Operator::GreaterThan));
    }

    #[test]
    fn flavor_newline_terminated_strings() {
        let src = b"x=\"ab\ncd\"";

        // Default: the newline is inside the string; one Quoted token.
        let toks = lex(src, Flavor::default());
        let quoted: Vec<_> = toks
            .iter()
            .filter(|t| t.kind == SyntaxKind::Quoted)
            .collect();
        assert_eq!(quoted.len(), 1);
        assert_eq!(quoted[0].len as usize, src.len() - 2); // `"ab\ncd"`

        // HoI4: the string ends at the newline.
        let toks = lex(src, Flavor::hoi4());
        let first = toks.iter().find(|t| t.kind == SyntaxKind::Quoted).unwrap();
        let text = &src[first.start as usize..(first.start + first.len) as usize];
        assert_eq!(text, b"\"ab");

        // Both round-trip.
        assert_eq!(parse_with(src, Flavor::default()).reconstruct(), src);
        assert_eq!(parse_with(src, Flavor::hoi4()).reconstruct(), src);
    }

    #[test]
    fn flavor_hoi4_treats_at_as_identifier() {
        // With HoI4, `@` is an ordinary identifier byte, not a variable marker.
        let toks = lex(b"@foo", Flavor::hoi4());
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].kind, SyntaxKind::Unquoted);

        let toks = lex(b"@foo", Flavor::default());
        assert_eq!(toks[0].kind, SyntaxKind::Variable);
    }

    #[test]
    fn diagnostics_unclosed_block() {
        let tree = parse(b"a = { b");
        assert!(tree.errors().iter().any(|e| e.message.contains("unclosed")));
        assert_eq!(tree.reconstruct(), b"a = { b"); // still lossless
    }

    #[test]
    fn diagnostics_unmatched_brace() {
        let tree = parse(b"x } y");
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("unmatched"))
        );
        assert_eq!(tree.reconstruct(), b"x } y");
        // The stray brace lands in a Bogus node.
        assert!(
            tree.root()
                .child_nodes()
                .any(|n| n.kind() == SyntaxKind::Bogus)
        );
    }

    #[test]
    fn clean_input_has_no_errors() {
        assert!(parse(b"a = { b = c }").errors().is_empty());
    }

    #[test]
    #[cfg_attr(miri, ignore)] // the deep input is slow under miri
    fn deeply_nested_blocks_do_not_overflow() {
        // Analogous to TextTape's `test_too_heavily_nested`: tens of thousands
        // of open braces would blow a naive recursive descent's stack. The
        // depth guard caps recursion, records a diagnostic, and the tree still
        // round-trips byte-for-byte.
        let mut data = Vec::new();
        data.extend_from_slice(b"foo=");
        data.resize(data.len() + 100_000, b'{');
        let tree = parse(&data);
        assert_eq!(tree.reconstruct(), data);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("maximum nesting depth"))
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn deeply_nested_balanced_blocks_round_trip() {
        // Balanced this time, so flattening must still pair every brace.
        let mut data = vec![b'{'; 50_000];
        data.resize(100_000, b'}');
        let tree = parse(&data);
        assert_eq!(tree.reconstruct(), data);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn deeply_nested_calc_parens_do_not_overflow() {
        // Calc expressions recurse through `parse_operand`/`parse_expr`; the
        // same guard covers `@[((((…))))]`.
        let mut data = Vec::new();
        data.extend_from_slice(b"x = @[");
        data.resize(data.len() + 100_000, b'(');
        data.push(b'1');
        data.resize(data.len() + 100_000, b')');
        data.push(b']');
        let tree = parse(&data);
        assert_eq!(tree.reconstruct(), data);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("calc nesting"))
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn deeply_nested_calc_unary_do_not_overflow() {
        // Unary `-` also recurses through `parse_operand`.
        let mut data = Vec::new();
        data.extend_from_slice(b"x = @[");
        data.resize(data.len() + 100_000, b'-');
        data.extend_from_slice(b"x]");
        let tree = parse(&data);
        assert_eq!(tree.reconstruct(), data);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("calc nesting"))
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn deeply_nested_calc_binary_chain_does_not_overflow() {
        // A flat chain `@[1+1+1+…]` is folded iteratively, but yields a
        // left-nested BinaryExpr tree of depth = chain length that emit_calc and
        // the tree's recursive Drop walk — so the fold cap must bound it too.
        let mut data = Vec::new();
        data.extend_from_slice(b"x = @[1");
        for _ in 0..100_000 {
            data.extend_from_slice(b"+1");
        }
        data.push(b']');
        let tree = parse(&data);
        assert_eq!(tree.reconstruct(), data);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("calc nesting"))
        );
        // A pure `*` chain folds the same way.
        let mut data = Vec::new();
        data.extend_from_slice(b"x = @[1");
        for _ in 0..100_000 {
            data.extend_from_slice(b"*1");
        }
        data.push(b']');
        assert_eq!(parse(&data).reconstruct(), data);
    }

    #[test]
    fn macro_param_is_a_scalar_key_and_value() {
        // `$P$` counts as a scalar everywhere (template files use it as a key,
        // value, and array element), so `$P$ = $Q$` is a Field.
        let tree = parse(b"$P$ = $Q$");
        let field = tree.ast().fields().next().expect("a Field");
        assert_eq!(field.key().unwrap().text(), b"$P$");
        assert_eq!(
            field.value().unwrap().as_scalar().unwrap().as_bytes(),
            b"$Q$"
        );
    }

    // ---- typed AST ----

    #[test]
    fn ast_field_key_op_value() {
        let tree = parse(b"name = \"Ragusa\"");
        let field = tree.ast().fields().next().unwrap();
        assert_eq!(field.key().unwrap().text(), b"name");
        assert_eq!(field.op(), Some(Operator::Equal));
        let value = field.value().unwrap();
        // Quoted value: as_scalar() strips the surrounding quotes.
        assert_eq!(value.as_scalar().unwrap().as_bytes(), b"Ragusa");
    }

    #[test]
    fn ast_value_coercions() {
        let tree = parse(b"a = 1.5 b = -3 c = 42 d = yes");
        let vals: Vec<_> = tree.ast().fields().map(|f| f.value().unwrap()).collect();
        assert_eq!(vals[0].to_f64(), Some(1.5));
        assert_eq!(vals[1].to_i64(), Some(-3));
        assert_eq!(vals[2].to_u64(), Some(42));
        assert_eq!(vals[3].to_bool(), Some(true));
    }

    #[test]
    fn ast_block_entries_fields_and_values() {
        let tree = parse(b"obj = { a = 1 b = 2 } arr = { x y z }");
        let mut fields = tree.ast().fields();
        let obj = fields.next().unwrap().value().unwrap().as_block().unwrap();
        assert_eq!(obj.fields().count(), 2);
        assert!(obj.values().next().is_none());
        assert!(!obj.is_empty());

        let arr = fields.next().unwrap().value().unwrap().as_block().unwrap();
        let elems: Vec<_> = arr
            .values()
            .filter_map(|v| v.as_scalar())
            .map(|s| s.as_bytes().to_vec())
            .collect();
        assert_eq!(elems, [b"x", b"y", b"z"]);
        assert!(arr.fields().next().is_none());
    }

    #[test]
    fn ast_empty_block_is_empty() {
        let tree = parse(b"a = {}");
        let block = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        assert!(block.is_empty());
    }

    #[test]
    fn ast_headered_block_is_generic() {
        let tree = parse(b"color = rgb { 1 0.5 0 } custom = tag { x = y }");
        let mut fields = tree.ast().fields();

        let rgb = fields
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_headered()
            .unwrap();
        assert_eq!(rgb.header().unwrap().text(), b"rgb");
        let channels: Vec<_> = rgb
            .block()
            .unwrap()
            .values()
            .map(|value| value.to_f64())
            .collect();
        assert_eq!(channels, [Some(1.0), Some(0.5), Some(0.0)]);

        let custom = fields
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_headered()
            .unwrap();
        assert_eq!(custom.header().unwrap().text(), b"tag");
        assert_eq!(custom.block().unwrap().fields().count(), 1);
    }

    #[test]
    fn ast_reader_variable_definition_with_calc_value() {
        let tree = parse(b"@half = @[1 / 2]");
        let field = tree.ast().fields().next().unwrap();

        let key = field.key().unwrap();
        assert_eq!(key.kind(), SyntaxKind::Variable);
        assert_eq!(key.text(), b"@half");
        assert_eq!(field.op(), Some(Operator::Equal));

        let calc = field.value().unwrap().as_calc().unwrap();
        let Some(Expr::Binary(div)) = calc.expr() else {
            panic!("expected a binary calculation");
        };
        assert_eq!(div.op_token().unwrap().kind(), SyntaxKind::Slash);
        assert!(matches!(div.lhs(), Some(Expr::Number(_))));
        assert!(matches!(div.rhs(), Some(Expr::Number(_))));
    }

    #[test]
    fn ast_headered_block_with_variables_and_calcs() {
        let tree = parse(b"color = rgb { @red @[green / 2] 0 }");
        let headered = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_headered()
            .unwrap();
        assert_eq!(headered.header().unwrap().text(), b"rgb");

        let values: Vec<_> = headered.block().unwrap().values().collect();
        let Value::Scalar(red) = values[0] else {
            panic!("expected a variable scalar");
        };
        assert_eq!(red.kind(), SyntaxKind::Variable);
        assert_eq!(red.text(), b"@red");

        let Value::Calc(calc) = values[1] else {
            panic!("expected a calculation");
        };
        let Some(Expr::Binary(div)) = calc.expr() else {
            panic!("expected a binary calculation");
        };
        assert!(matches!(div.lhs(), Some(Expr::Ident(_))));
        assert!(matches!(div.rhs(), Some(Expr::Number(_))));

        let Value::Scalar(zero) = values[2] else {
            panic!("expected a scalar channel");
        };
        assert_eq!(zero.text(), b"0");
    }

    #[test]
    fn ast_calc_expr_structure() {
        let tree = parse(b"x = @[1 + tier * 2]");
        let calc = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_calc()
            .unwrap();
        // 1 + (tier * 2): top is a binary `+`.
        let Some(Expr::Binary(add)) = calc.expr() else {
            panic!("expected a binary expression");
        };
        assert_eq!(add.op_token().unwrap().text(), b"+");
        assert!(matches!(add.lhs(), Some(Expr::Number(_))));
        // rhs is the tighter `tier * 2`.
        let Some(Expr::Binary(mul)) = add.rhs() else {
            panic!("expected a nested binary expression");
        };
        assert_eq!(mul.op_token().unwrap().text(), b"*");
        assert!(matches!(mul.lhs(), Some(Expr::Ident(_))));
        assert!(matches!(mul.rhs(), Some(Expr::Number(_))));
    }

    #[test]
    fn ast_calc_unary_and_paren() {
        let tree = parse(b"x = @[ -(a) ]");
        let calc = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_calc()
            .unwrap();
        let Some(Expr::Unary(neg)) = calc.expr() else {
            panic!("expected a unary expression");
        };
        assert_eq!(neg.op_token().unwrap().text(), b"-");
        let Some(Expr::Paren(paren)) = neg.operand() else {
            panic!("expected a parenthesized operand");
        };
        assert!(matches!(paren.inner(), Some(Expr::Ident(_))));
    }

    #[test]
    fn ast_item_other_preserves_loose_tokens() {
        // `[[param]]` tokens are not yet grouped, so they surface as Item::Other
        // rather than being silently dropped from the typed entry view.
        let tree = parse(b"[[scaled_skill] body ]");
        let others = tree
            .ast()
            .items()
            .filter(|i| matches!(i, Item::Other(_)))
            .count();
        assert!(
            others >= 1,
            "loose bracket tokens should appear as Item::Other"
        );
    }

    // ---- formatter ----

    fn fmt(src: &[u8]) -> String {
        String::from_utf8(format(src)).unwrap()
    }

    /// The significant-token stream (everything but whitespace and comments) as
    /// `(kind, text)` pairs. The formatter must preserve this exactly, in order.
    fn significant(src: &[u8]) -> Vec<(SyntaxKind, Vec<u8>)> {
        parse(src)
            .tokens()
            .filter(|t| !matches!(t.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
            .map(|t| (t.kind(), t.text().to_vec()))
            .collect()
    }

    /// The multiset of comment texts (comments may be relocated, never dropped,
    /// merged, or altered).
    fn comment_texts(src: &[u8]) -> Vec<Vec<u8>> {
        let mut v: Vec<Vec<u8>> = parse(src)
            .tokens()
            .filter(|t| t.kind() == SyntaxKind::Comment)
            .map(|t| t.text().to_vec())
            .collect();
        v.sort();
        v
    }

    #[test]
    fn fmt_normalizes_field_spacing() {
        assert_eq!(fmt(b"a=b"), "a = b\n");
        assert_eq!(fmt(b"a    =\tb"), "a = b\n");
        assert_eq!(fmt(b"a == b"), "a == b\n"); // operator text kept verbatim
        assert_eq!(fmt(b"age>16"), "age > 16\n");
    }

    #[test]
    fn fmt_inline_scalar_blocks() {
        assert_eq!(fmt(b"color=rgb{255 0 128}"), "color = rgb { 255 0 128 }\n");
        assert_eq!(fmt(b"arr = {1    2 3}"), "arr = { 1 2 3 }\n");
    }

    #[test]
    fn fmt_empty_block_collapses() {
        assert_eq!(fmt(b"a = {   }"), "a = {}\n");
        assert_eq!(fmt(b"a={\n\n}"), "a = {}\n");
    }

    #[test]
    fn fmt_nested_blocks_indent_with_tabs() {
        assert_eq!(fmt(b"a={b={c=d}}"), "a = {\n\tb = {\n\t\tc = d\n\t}\n}\n");
    }

    #[test]
    fn fmt_block_with_fields_is_multiline() {
        assert_eq!(fmt(b"o={x=1 y=2}"), "o = {\n\tx = 1\n\ty = 2\n}\n");
    }

    #[test]
    fn fmt_indent_option_spaces() {
        let opts = FormatOptions {
            indent: "  ".into(),
        };
        let out = String::from_utf8(parse(b"a={b=c}").format(&opts)).unwrap();
        assert_eq!(out, "a = {\n  b = c\n}\n");
    }

    #[test]
    fn fmt_comments_leading_trailing_and_in_block() {
        assert_eq!(fmt(b"a = b # note"), "a = b # note\n");
        assert_eq!(fmt(b"# header\na = b"), "# header\na = b\n");
        assert_eq!(
            fmt(b"o = {\n# inner\nx = 1\n}"),
            "o = {\n\t# inner\n\tx = 1\n}\n"
        );
    }

    #[test]
    fn fmt_relocated_interior_comment_is_safe() {
        // A comment between `=` and the value is hoisted to its own line before
        // the field so it cannot comment out the value; both tokens survive.
        let out = fmt(b"a = # oops\n b");
        assert_eq!(out, "# oops\na = b\n");
        assert_eq!(significant(b"a = # oops\n b"), significant(out.as_bytes()));
    }

    #[test]
    fn fmt_preserves_single_blank_line() {
        assert_eq!(fmt(b"a = 1\n\n\n\nb = 2"), "a = 1\n\nb = 2\n");
    }

    #[test]
    fn fmt_calc_is_verbatim() {
        assert_eq!(fmt(b"x=@[1+2]"), "x = @[1+2]\n");
        assert_eq!(fmt(b"x = @[ a + b ]"), "x = @[ a + b ]\n");
    }

    #[test]
    fn fmt_unterminated_quote_keeps_no_trailing_newline() {
        // The trailing newline would be absorbed into the open string, so it is
        // suppressed and the (significant) token stream is preserved.
        assert_eq!(format(b"a = \"abc"), b"a = \"abc");
        assert_eq!(
            significant(b"a = \"abc"),
            significant(&format(b"a = \"abc"))
        );
    }

    #[test]
    fn fmt_bom_preserved_at_top() {
        assert_eq!(format(b"\xef\xbb\xbfa=b"), b"\xef\xbb\xbfa = b\n");
    }

    #[test]
    fn fmt_empty_input() {
        assert_eq!(format(b""), b"");
        assert_eq!(format(b"   \n\t "), b"");
    }

    #[test]
    fn fmt_unclosed_block_keeps_brace_count() {
        // No phantom `}` is invented for an unclosed block.
        let out = fmt(b"a = { b = c");
        assert_eq!(significant(b"a = { b = c"), significant(out.as_bytes()));
        assert_eq!(out.matches('}').count(), 0);
    }

    #[quickcheck]
    fn prop_format_idempotent(data: Vec<u8>) -> bool {
        let once = format(&data);
        let twice = format(&once);
        once == twice
    }

    #[quickcheck]
    fn prop_format_preserves_significant_and_comments(data: Vec<u8>) -> bool {
        let f = format(&data);
        significant(&data) == significant(&f) && comment_texts(&data) == comment_texts(&f)
    }

    /// Stress the formatter on a dense alphabet of structural bytes (random
    /// input almost never forms blocks/calcs/comments otherwise).
    #[quickcheck]
    fn prop_format_alphabet(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"{}[]()=<># \n\t\"@$.0123456789abc-+*/";
        let m: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        let f = format(&m);
        significant(&m) == significant(&f)
            && comment_texts(&m) == comment_texts(&f)
            && format(&f) == f
    }

    #[test]
    fn tokens_iter_is_ordered_leaves_and_lossless() {
        let src = b"a = { b = @[1+2] }  # c";
        let tree = parse(src);
        // Only leaves, never nodes.
        assert!(tree.tokens().all(|t| !t.kind().is_node()));
        // In document order, contiguous, covering the whole source.
        let mut at = 0u32;
        let mut joined = Vec::new();
        for t in tree.tokens() {
            assert_eq!(t.text_range().0, at);
            at = t.text_range().1;
            joined.extend_from_slice(t.text());
        }
        assert_eq!(joined, src); // the lossless invariant, via tokens()
    }

    #[test]
    fn node_flags_summarize_subtrees() {
        // `outer` holds a comment + a macro; the nested `inner` block is clean;
        // the calc lives only under sibling `c`. Each node's flags reflect its
        // *whole* subtree, derived in finish() with no per-query walk.
        let tree = parse(b"outer = { # hi\n a = $P$ inner = { b = c } } c = @[1+2]");
        let root = tree.root();
        assert!(root.has_comment() && root.contains_macro() && root.contains_calc());
        assert!(!root.has_error());

        let mut fields = tree.ast().fields();
        let outer_block = fields
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap()
            .syntax();
        assert!(outer_block.has_comment());
        assert!(outer_block.contains_macro());
        assert!(
            !outer_block.contains_calc(),
            "the calc is in a sibling, not here"
        );

        // The genuinely nested `inner = { b = c }` block carries none of the bits.
        let inner = outer_block
            .child_nodes()
            .filter_map(|f| Field::cast(f)?.value()?.as_block())
            .map(|b| b.syntax())
            .next()
            .expect("the `inner` block");
        assert!(!inner.has_comment() && !inner.contains_macro() && !inner.contains_calc());

        // The calc sibling carries HAS_CALC and nothing spurious.
        let calc_block = fields.next().unwrap().syntax();
        assert!(calc_block.contains_calc() && !calc_block.has_comment());

        // A `Bogus` recovery node sets HAS_ERROR all the way to the root.
        // (An unclosed `{`, by contrast, is only a diagnostic — it creates no
        // error element — so it deliberately does not set the bit.)
        let stray = parse(b"x } y");
        assert!(stray.root().has_error(), "the Bogus node sets HAS_ERROR");
        assert!(!parse(b"a = { b").root().has_error());
    }

    #[test]
    fn parent_links_reach_root() {
        let tree = parse(b"a = { b = c }");
        let field = tree.root().child_nodes().next().unwrap();
        let block = field.child_nodes().next().unwrap();
        let inner = block.child_nodes().next().unwrap();
        assert_eq!(inner.kind(), SyntaxKind::Field);
        let c = inner
            .child_tokens()
            .filter(|t| t.kind() == SyntaxKind::Unquoted)
            .last()
            .unwrap();
        assert_eq!(c.text(), b"c");

        let mut n = c.parent().unwrap();
        let mut depth = 0;
        while let Some(p) = n.parent() {
            n = p;
            depth += 1;
        }
        assert_eq!(n.kind(), SyntaxKind::Root);
        assert!(depth >= 2, "expected Field -> Block -> Root nesting");
    }

    /// Compact structural rendering of a subtree: interior nodes as
    /// `Kind(child ...)`, significant tokens as their bare `Kind`, trivia
    /// omitted. Terse enough to assert precedence and associativity directly.
    fn shape(el: SyntaxElement<'_, '_>) -> String {
        match el {
            SyntaxElement::Node(n) => {
                let kids: Vec<String> = n
                    .children()
                    .filter(|c| !c.kind().is_trivia())
                    .map(shape)
                    .collect();
                format!("{:?}({})", n.kind(), kids.join(" "))
            }
            SyntaxElement::Token(t) => format!("{:?}", t.kind()),
        }
    }

    /// Parse `key = <calc>` and return the structural shape of the `Calc` node.
    fn calc_shape(src: &[u8]) -> String {
        let tree = parse(src);
        let field = tree.root().child_nodes().next().unwrap();
        let calc = field
            .child_nodes()
            .find(|n| n.kind() == SyntaxKind::Calc)
            .expect("a Calc node");
        shape(SyntaxElement::Node(calc))
    }

    #[test]
    fn calc_precedence_mul_binds_tighter() {
        // `*` binds tighter than `+`: 1 + (2 * 3).
        assert_eq!(
            calc_shape(b"x = @[1+2*3]"),
            "Calc(CalcOpen BinaryExpr(Number Plus BinaryExpr(Number Star Number)) CalcClose)"
        );
        // Mirror: (1 * 2) + 3.
        assert_eq!(
            calc_shape(b"x = @[1*2+3]"),
            "Calc(CalcOpen BinaryExpr(BinaryExpr(Number Star Number) Plus Number) CalcClose)"
        );
    }

    #[test]
    fn calc_addition_is_left_associative() {
        // 1 - 2 - 3 parses as (1 - 2) - 3, not 1 - (2 - 3).
        assert_eq!(
            calc_shape(b"x = @[1-2-3]"),
            "Calc(CalcOpen BinaryExpr(BinaryExpr(Number Minus Number) Minus Number) CalcClose)"
        );
    }

    #[test]
    fn calc_unary_minus() {
        assert_eq!(
            calc_shape(b"x = @[ -x ]"),
            "Calc(CalcOpen UnaryExpr(Minus CalcIdent) CalcClose)"
        );
        // Unary binds tighter than binary: (-half) - half.
        assert_eq!(
            calc_shape(b"x = @[-half-half]"),
            "Calc(CalcOpen BinaryExpr(UnaryExpr(Minus CalcIdent) Minus CalcIdent) CalcClose)"
        );
    }

    #[test]
    fn calc_nested_parens() {
        assert_eq!(
            calc_shape(b"x = @[((1))]"),
            "Calc(CalcOpen ParenExpr(OpenParen ParenExpr(OpenParen Number CloseParen) \
             CloseParen) CalcClose)"
        );
        // A parenthesized sub-expression lowers `*` below it: (a + b) * c.
        assert_eq!(
            calc_shape(b"x = @[(a+b)*c]"),
            "Calc(CalcOpen BinaryExpr(ParenExpr(OpenParen BinaryExpr(CalcIdent Plus CalcIdent) \
             CloseParen) Star CalcIdent) CalcClose)"
        );
    }

    #[test]
    fn calc_float_with_f_suffix_is_one_number() {
        let tree = parse(b"x = @[10.0f / tier]");
        let field = tree.root().child_nodes().next().unwrap();
        let calc = field.child_nodes().next().unwrap();
        let bin = calc.child_nodes().next().unwrap();
        assert_eq!(bin.kind(), SyntaxKind::BinaryExpr);
        let nums: Vec<_> = bin
            .child_tokens()
            .filter(|t| t.kind() == SyntaxKind::Number)
            .map(|t| t.text())
            .collect();
        assert_eq!(nums, [b"10.0f"]);
        let op = bin
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::Slash)
            .unwrap();
        assert_eq!(op.text(), b"/");
    }

    #[test]
    fn calc_at_variable_operand() {
        let tree = parse(b"x = @[@var + 1]");
        let field = tree.root().child_nodes().next().unwrap();
        let bin = field
            .child_nodes()
            .next()
            .unwrap()
            .child_nodes()
            .next()
            .unwrap();
        let ident = bin
            .child_tokens()
            .find(|t| t.kind() == SyntaxKind::CalcIdent)
            .unwrap();
        assert_eq!(ident.text(), b"@var");
    }

    #[test]
    fn calc_in_array_position() {
        // A bare calc as an array element (no `=`) is still grouped as a Calc.
        let tree = parse(b"position = { @[1-leopard_x] @leopard_y }");
        let block = tree
            .root()
            .child_nodes()
            .next()
            .unwrap()
            .child_nodes()
            .next()
            .unwrap();
        assert_eq!(block.kind(), SyntaxKind::Block);
        let calc = block
            .child_nodes()
            .find(|n| n.kind() == SyntaxKind::Calc)
            .unwrap();
        assert_eq!(calc.text(), b"@[1-leopard_x]");
        // The following `@leopard_y` is a Variable token, not folded into the calc.
        assert!(
            block
                .child_tokens()
                .any(|t| t.kind() == SyntaxKind::Variable)
        );
    }

    #[test]
    fn calc_empty() {
        assert_eq!(calc_shape(b"x = @[]"), "Calc(CalcOpen CalcClose)");
        assert!(parse(b"x = @[]").errors().is_empty());
    }

    #[test]
    fn calc_unterminated_diagnostic() {
        let tree = parse(b"x = @[1 + 2");
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message.contains("unclosed '@['")),
            "expected an unclosed-calc diagnostic, got {:?}",
            tree.errors()
        );
        assert_eq!(tree.reconstruct(), b"x = @[1 + 2"); // still lossless
        // No CalcClose token was emitted, but the expression still parsed.
        let calc = tree
            .root()
            .child_nodes()
            .next()
            .unwrap()
            .child_nodes()
            .find(|n| n.kind() == SyntaxKind::Calc)
            .unwrap();
        assert!(
            calc.child_tokens()
                .all(|t| t.kind() != SyntaxKind::CalcClose)
        );
        assert!(
            calc.child_nodes()
                .any(|n| n.kind() == SyntaxKind::BinaryExpr)
        );
    }

    #[test]
    fn calc_hoi4_flavor_has_no_calc() {
        // HoI4 has no reader variables, so `@[` is ordinary identifier bytes and
        // no calc machinery (no `Calc`/`CalcOpen`/...) ever appears in the tree.
        let tree = parse_with(b"x = @[1+2]", Flavor::hoi4());
        assert_eq!(tree.reconstruct(), b"x = @[1+2]");
        assert!(!tree.debug_tree().contains("Calc"));
    }

    #[quickcheck]
    fn prop_round_trip(data: Vec<u8>) -> bool {
        parse(&data).reconstruct() == data
    }

    #[quickcheck]
    fn prop_round_trip_hoi4(data: Vec<u8>) -> bool {
        parse_with(&data, Flavor::hoi4()).reconstruct() == data
    }

    /// Round-trip over a calc-heavy alphabet: random bytes almost never form a
    /// `@[ ... ]`, so map each byte onto the small set of calc-relevant tokens
    /// to actually exercise the calc lexer/parser under quickcheck.
    #[quickcheck]
    fn prop_round_trip_calc_alphabet(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"@[]()+-*/ .0123456789fxy_";
        let mapped: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        parse(&mapped).reconstruct() == mapped
    }
}
