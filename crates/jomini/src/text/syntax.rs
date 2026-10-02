//! **Experimental** lossless syntax tree for the Clausewitz text format.
//!
//! This tree is the base for tooling: formatting, linting, highlighting, and
//! transformations. Later, it will also be the base for deserialization. Unlike
//! [`TextTape`], which discards trivia and the `=` operator, this tree keeps
//! **every byte** of the source: comments, whitespace, the `=`, and bytes that
//! match no rule. The defining invariant is:
//!
//! ```text
//! concat(text of every leaf token, in order) == source
//! ```
//!
//! # Representation
//!
//! The tree has two flat arrays:
//!
//! - A **token buffer** that holds only the *significant* tokens. Each token
//!   records its kind and its absolute byte range.
//! - A **node tape** in pre-order. Each node records its kind, the range of
//!   tokens it covers, and the index after its last descendant node.
//!
//! Trivia (whitespace, comments, and the BOM) is not stored. It is the bytes
//! *between* two significant tokens. A cursor lexes such a gap again when it
//! must show trivia. The gap between two tokens belongs to their nearest
//! common ancestor node, so the cursors give a complete lossless tree. This
//! layout is the same as the one that Carbon uses. Trivia is approximately 40%
//! of all leaves in a save file, so the tree does not pay to store it.
//!
//! Offsets are absolute. Parent links are calculated only when a cursor asks
//! for them. The [`SyntaxNode`]/[`SyntaxToken`] cursors are small `Copy` handles.
//!
//! # Parsing
//!
//! The lexer classifies bytes with a lookup table. It scans long runs (unquoted
//! scalars, whitespace, quoted strings, and comments) 16 bytes at a time with
//! [`fearless_simd`]. The parser is a recursive descent parser. It sends its
//! events (start node, token, finish node) to an internal sink. The sink builds
//! the tree. Thus the grammar is independent of the storage.
//!
//! # Games (one superset parser)
//!
//! A single permissive parser handles every PDS title. The lexer accepts the
//! *union* of all games' syntax (`@vars`, `@[calc]`, `$macros$`, `[[params]]`,
//! broad identifier bytes). A lint layer, not the parser, decides if syntax is
//! correct for a game. [`Flavor`] toggles the few genuine lexer forks.
//!
//! In item position, a scalar followed directly by a block (`key { ... }`) is a
//! [`Field`] that has no operator. The parser does not try to decide if the
//! scalar is a key or a header such as `rgb`. The semantic layer makes that
//! decision. In value position (`color = rgb { ... }`), the scalar and the block
//! are a [`HeaderedBlock`], so the field has exactly one value.
//!
//! On top of the tree sit the cursors and an ungrammar-style **typed AST**
//! ([`AstNode`], [`Field`], [`Block`], [`HeaderedBlock`], [`Calc`], …) with value
//! coercions ([`Value::to_f64`]). A [`format`](fn@format) pass prints the tree
//! again in a normalized house style. It keeps every comment and every
//! significant token.
//!
//! The parser makes dedicated nodes for parameter blocks. It parses the inner
//! part of `@[calc]` into an expression subtree ([`SyntaxKind::Calc`] /
//! [`SyntaxKind::BinaryExpr`] / [`SyntaxKind::UnaryExpr`] /
//! [`SyntaxKind::ParenExpr`]). A depth guard limits recursion on pathological
//! nesting (see [`SyntaxError`]).
//!
//! [`TextTape`]: crate::TextTape

#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::{Encoding, Scalar, text::Operator};
use fearless_simd::{Level, Simd, dispatch, mask8x16, prelude::*, u8x16};
use std::borrow::Cow;
use std::sync::OnceLock;

mod tape;
pub use tape::{parse_tape, parse_tape_with};

/// The kind of every node (interior) and token (leaf) in a [`SyntaxTree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum SyntaxKind {
    // ===== trivia (made from the gaps between stored tokens) =====
    /// A leading UTF-8 byte-order-mark (`EF BB BF`).
    Bom,
    /// A run of spaces, tabs, newlines, carriage returns, or semicolons.
    Whitespace,
    /// A `# ...` comment running to end of line.
    Comment,

    // ===== tokens (leaves) =====
    /// A bare scalar: identifier, number, date, `yes`/`no`, etc.
    Unquoted,
    /// A `"..."` quoted scalar (raw text, including the quotes).
    Quoted,
    /// A `@name` reader-variable reference.
    Variable,
    /// An unquoted scalar that contains one or more `$NAME$` macro
    /// parameters, such as `$NAME$` or `$key$_modifier`.
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
    /// A synthetic, zero-width `}` inserted after an unclosed block at EOF.
    MissingCloseBrace,
    /// A synthetic, zero-width `]` inserted after an unclosed bracket construct.
    /// Code payloads use two consecutive tokens because their `]]` delimiter
    /// consists of two source tokens.
    MissingCloseBracket,

    // ===== calc tokens (interior of `@[ ... ]`) =====
    /// `@[`, opening a parse-time calculation.
    CalcOpen,
    /// `]`, closing a parse-time calculation.
    CalcClose,
    /// A synthetic, zero-width `]` inserted after an unclosed calculation.
    MissingCalcClose,
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
    /// A synthetic, zero-width `)` inserted after an unclosed calc expression.
    MissingCloseParen,
    /// A numeric literal inside a calc (e.g. `1`, `10.0`, `10.0f`).
    Number,
    /// An operand identifier inside a calc (e.g. `tier`, `leopard_x`, `@var`).
    CalcIdent,

    // ===== nodes (interior) =====
    /// The whole document.
    Root,
    /// A `key <op> value` field, or a `key { ... }` field with no operator.
    Field,
    /// A `{ ... }` block.
    Block,
    /// A tagged block in value position, such as `rgb { 1 2 3 }` in
    /// `color = rgb { 1 2 3 }` (a header scalar plus a block). This node
    /// gives the field exactly one value. In item position, `rgb { 1 2 3 }`
    /// is a [`SyntaxKind::Field`] with no operator.
    HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation wrapping an arithmetic expression.
    Calc,
    /// An EU4 conditional parameter block, `[[name] ... ]`.
    Parameter,
    /// An EU4 undefined conditional parameter block, `[[!name] ... ]`.
    UndefinedParameter,
    /// An EU5 code payload, `code [[ ... ]]`.
    Code,
    /// A single-bracket interpolation, such as `[ROOT.GetName]`.
    Interpolation,
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
    /// Return `true` for a synthetic recovery token.
    pub fn is_missing(self) -> bool {
        matches!(
            self,
            Self::MissingCloseBrace
                | Self::MissingCloseBracket
                | Self::MissingCalcClose
                | Self::MissingCloseParen
        )
    }

    /// Return the physical token that a missing token represents.
    pub fn expected_kind(self) -> Option<SyntaxKind> {
        Some(match self {
            Self::MissingCloseBrace => Self::CloseBrace,
            Self::MissingCloseBracket => Self::CloseBracket,
            Self::MissingCalcClose => Self::CalcClose,
            Self::MissingCloseParen => Self::CloseParen,
            _ => return None,
        })
    }

    /// Return the source spelling for a missing token.
    pub fn missing_text(self) -> Option<&'static str> {
        Some(match self {
            Self::MissingCloseBrace => "}",
            Self::MissingCloseBracket | Self::MissingCalcClose => "]",
            Self::MissingCloseParen => ")",
            _ => return None,
        })
    }

    /// Return `true` when this kind is a physical closing delimiter.
    pub fn is_physical_close(self) -> bool {
        matches!(
            self,
            Self::CloseBrace | Self::CloseBracket | Self::CalcClose | Self::CloseParen
        )
    }
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
                | SyntaxKind::Parameter
                | SyntaxKind::UndefinedParameter
                | SyntaxKind::Code
                | SyntaxKind::Interpolation
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::Bogus
        )
    }
}

// ---------------------------------------------------------------------------
// Text ranges
// ---------------------------------------------------------------------------

/// A half-open byte range `start..end` in the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct TextRange {
    start: u32,
    end: u32,
}

impl TextRange {
    /// Make a range. `start` must not be greater than `end`.
    #[inline]
    pub const fn new(start: u32, end: u32) -> Self {
        assert!(start <= end, "range start is greater than range end");
        TextRange { start, end }
    }

    /// Make a zero-width range at `offset`.
    #[inline]
    pub const fn empty(offset: u32) -> Self {
        TextRange {
            start: offset,
            end: offset,
        }
    }

    /// The first byte offset of the range.
    #[inline]
    pub const fn start(self) -> u32 {
        self.start
    }

    /// The byte offset after the last byte of the range.
    #[inline]
    pub const fn end(self) -> u32 {
        self.end
    }

    /// The number of bytes in the range.
    #[inline]
    pub const fn len(self) -> u32 {
        self.end - self.start
    }

    /// Return `true` when the range has no bytes.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Return `true` when `offset` is in `start..end`.
    #[inline]
    pub const fn contains(self, offset: u32) -> bool {
        self.start <= offset && offset < self.end
    }

    /// Return `true` when `offset` is in `start..=end`.
    #[inline]
    pub const fn contains_inclusive(self, offset: u32) -> bool {
        self.start <= offset && offset <= self.end
    }

    /// Return `true` when `other` is fully inside this range.
    #[inline]
    pub const fn contains_range(self, other: TextRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// The range as a `usize` range, to index the source.
    #[inline]
    pub const fn to_usize(self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl From<TextRange> for (u32, u32) {
    #[inline]
    fn from(range: TextRange) -> Self {
        (range.start, range.end)
    }
}

impl From<TextRange> for std::ops::Range<usize> {
    #[inline]
    fn from(range: TextRange) -> Self {
        range.to_usize()
    }
}

impl std::fmt::Display for TextRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

// ---------------------------------------------------------------------------
// Lexing: a table-driven, error-tolerant, BOM-preserving scanner. It keeps
// only significant tokens. The bytes between two tokens are always trivia.
// Every branch advances by at least one byte.
// ---------------------------------------------------------------------------

/// Game-specific lexer configuration.
///
/// [`Flavor::default`] is the most permissive superset and is correct for every
/// game except where a genuine lexer fork exists. [`Flavor::hoi4`] enables
/// HoI4's newline-terminated strings and treats `@`/`$` as ordinary identifier
/// bytes (HoI4 has no reader variables or macros).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// A significant token in the token buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Token {
    start: u32,
    len: u32,
    kind: SyntaxKind,
    flags: u8,
}

/// The gap before this token contains a comment.
const TOKEN_COMMENT_BEFORE: u8 = 1 << 0;
/// A diagnostic starts at this token.
const TOKEN_ERROR: u8 = 1 << 1;

impl Token {
    #[inline]
    fn end(self) -> u32 {
        self.start + self.len
    }

    #[inline]
    fn range(self) -> TextRange {
        TextRange::new(self.start, self.end())
    }
}

const CLASS_OTHER: u8 = 0;
const CLASS_WS: u8 = 1;
const CLASS_COMMENT: u8 = 2;
const CLASS_QUOTE: u8 = 3;
const CLASS_OPEN_BRACE: u8 = 4;
const CLASS_CLOSE_BRACE: u8 = 5;
const CLASS_OPEN_BRACKET: u8 = 6;
const CLASS_CLOSE_BRACKET: u8 = 7;
const CLASS_OPERATOR: u8 = 8;
const CLASS_BANG: u8 = 9;
const CLASS_QUESTION: u8 = 10;
const CLASS_AT: u8 = 11;

/// The lexer class of each byte.
static CLASS: [u8; 256] = {
    let mut table = [CLASS_OTHER; 256];
    table[b' ' as usize] = CLASS_WS;
    table[b'\t' as usize] = CLASS_WS;
    table[b'\r' as usize] = CLASS_WS;
    table[b'\n' as usize] = CLASS_WS;
    table[0x0b] = CLASS_WS;
    table[0x0c] = CLASS_WS;
    table[b';' as usize] = CLASS_WS;
    table[b'#' as usize] = CLASS_COMMENT;
    table[b'"' as usize] = CLASS_QUOTE;
    table[b'{' as usize] = CLASS_OPEN_BRACE;
    table[b'}' as usize] = CLASS_CLOSE_BRACE;
    table[b'[' as usize] = CLASS_OPEN_BRACKET;
    table[b']' as usize] = CLASS_CLOSE_BRACKET;
    table[b'=' as usize] = CLASS_OPERATOR;
    table[b'<' as usize] = CLASS_OPERATOR;
    table[b'>' as usize] = CLASS_OPERATOR;
    table[b'!' as usize] = CLASS_BANG;
    table[b'?' as usize] = CLASS_QUESTION;
    table[b'@' as usize] = CLASS_AT;
    table
};

/// Bytes that terminate an unquoted run. These either start their own token
/// (braces, brackets, operators, quote, comment) or are whitespace. Notably
/// `@`, `$`, `!`, `?`, `:`, `.`, `-`, `/`, `|` and high bytes are *not* here, so
/// they are absorbed mid-identifier and only dispatch specially as a first byte.
static STOP: [bool; 256] = {
    let mut table = [false; 256];
    let mut i = 0;
    while i < 256 {
        table[i] = matches!(
            CLASS[i],
            CLASS_WS
                | CLASS_COMMENT
                | CLASS_QUOTE
                | CLASS_OPEN_BRACE
                | CLASS_CLOSE_BRACE
                | CLASS_OPEN_BRACKET
                | CLASS_CLOSE_BRACKET
                | CLASS_OPERATOR
        );
        i += 1;
    }
    table
};

/// What an unquoted run does at each byte: continue, stop, or note a `$`.
const RUN_CONTINUE: u8 = 0;
const RUN_STOP: u8 = 1;
const RUN_DOLLAR: u8 = 2;

static RUN: [u8; 256] = {
    let mut table = [RUN_CONTINUE; 256];
    let mut i = 0;
    while i < 256 {
        if STOP[i] {
            table[i] = RUN_STOP;
        }
        i += 1;
    }
    table[b'$' as usize] = RUN_DOLLAR;
    table
};

#[inline]
fn is_ws(b: u8) -> bool {
    CLASS[b as usize] == CLASS_WS
}

/// The output of the lexer.
struct Lexed {
    tokens: Vec<Token>,
    /// The gap after the last token contains a comment.
    trailing_comment: bool,
}

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

fn lex(source: &[u8], flavor: Flavor) -> Lexed {
    // Save files average approximately five bytes per significant token.
    let mut tokens = Vec::with_capacity(source.len() / 5 + 16);
    let level = Level::new();
    let trailing_comment = dispatch!(level, simd => lex_simd(simd, source, flavor, &mut tokens));
    Lexed {
        tokens,
        trailing_comment,
    }
}

/// Lex `source` into `out`. Return `true` when a comment follows the last token.
#[inline(always)]
fn lex_simd<S: Simd>(simd: S, source: &[u8], flavor: Flavor, out: &mut Vec<Token>) -> bool {
    let n = source.len();
    let mut i = if source.starts_with(BOM) { 3 } else { 0 };
    let mut flags = 0u8;

    while i < n {
        let b = source[i];
        let start = i;
        let kind = match CLASS[b as usize] {
            CLASS_WS => {
                i = skip_whitespace(simd, source, i + 1);
                continue;
            }
            CLASS_COMMENT => {
                i = find_line_end(simd, source, i + 1);
                flags |= TOKEN_COMMENT_BEFORE;
                continue;
            }
            CLASS_QUOTE => {
                i = scan_quoted(simd, source, i + 1, flavor.newline_terminated_strings);
                SyntaxKind::Quoted
            }
            CLASS_OPEN_BRACE => {
                i += 1;
                SyntaxKind::OpenBrace
            }
            CLASS_CLOSE_BRACE => {
                i += 1;
                SyntaxKind::CloseBrace
            }
            CLASS_OPEN_BRACKET => {
                i += 1;
                SyntaxKind::OpenBracket
            }
            CLASS_CLOSE_BRACKET => {
                i += 1;
                SyntaxKind::CloseBracket
            }
            CLASS_OPERATOR => {
                i += 1;
                if i < n && source[i] == b'=' {
                    i += 1; // ==, <=, >=
                }
                SyntaxKind::Operator
            }
            CLASS_BANG | CLASS_QUESTION if i + 1 < n && source[i + 1] == b'=' => {
                i += 2; // != or ?=
                SyntaxKind::Operator
            }
            CLASS_BANG => {
                i += 1;
                SyntaxKind::Bang
            }
            CLASS_AT if flavor.variables && i + 1 < n && source[i + 1] == b'[' => {
                // `@[ ... ]` calc: emit structured interior tokens so the
                // arithmetic expression can be parsed into a subtree.
                i = lex_calc(simd, source, i, flags, out);
                flags = 0;
                continue;
            }
            _ => {
                // Ordinary unquoted run: consume up to the next terminator. This
                // catch-all is what keeps the lexer total (every byte classified).
                let (mut end, dollar) = scan_unquoted(simd, source, i + 1);
                if end < n && source[end] == b'=' && end - start > 1 && source[end - 1] == b'!' {
                    // `a!=b` is `a`, `!=`, `b`, as in `TextTape`.
                    end -= 1;
                }
                i = end;
                classify_run(&source[start..i], b == b'$' || dollar, flavor)
            }
        };

        out.push(Token {
            start: start as u32,
            len: (i - start) as u32,
            kind,
            flags,
        });
        flags = 0;
    }

    flags & TOKEN_COMMENT_BEFORE != 0
}

/// Classify an unquoted run. `dollar` is `true` when the run contains a `$`.
#[inline(always)]
fn classify_run(run: &[u8], dollar: bool, flavor: Flavor) -> SyntaxKind {
    if flavor.variables && run.len() > 1 && run[0] == b'@' && run[1] != b'@' {
        SyntaxKind::Variable
    } else if flavor.macros && dollar && has_macro(run) {
        SyntaxKind::MacroParam
    } else {
        SyntaxKind::Unquoted
    }
}

/// Return `true` when `run` contains a `$...$` pair. An unquoted run has no
/// stop bytes, so any two `$` bytes enclose a macro name.
#[cold]
fn has_macro(run: &[u8]) -> bool {
    run.iter().filter(|&&b| b == b'$').nth(1).is_some()
}

/// A lane mask for `v <= k` with unsigned bytes.
#[inline(always)]
fn lanes_le<S: Simd>(v: u8x16<S>, k: u8) -> mask8x16<S> {
    v.saturating_sub(k).simd_eq(0u8)
}

/// A lane mask for whitespace bytes: `\t` `\n` `\v` `\f` `\r` ` ` `;`.
#[inline(always)]
fn ws_lanes<S: Simd>(v: u8x16<S>) -> mask8x16<S> {
    lanes_le(v - 9u8, 4) | v.simd_eq(b' ') | v.simd_eq(b';')
}

/// A lane mask for the bytes in [`STOP`]. The masks are exact:
///
/// - `v | 0x20` is `{` for `{` and `[`, and `}` for `}` and `]`.
/// - `v - '<' <= 2` is `<`, `=`, and `>`.
/// - `v - '"' <= 1` is `"` and `#`.
#[inline(always)]
fn stop_lanes<S: Simd>(v: u8x16<S>) -> mask8x16<S> {
    let folded = v | 0x20u8;
    ws_lanes(v)
        | folded.simd_eq(b'{')
        | folded.simd_eq(b'}')
        | lanes_le(v - b'<', 2)
        | lanes_le(v - b'"', 1)
}

#[inline(always)]
fn load<S: Simd>(simd: S, source: &[u8], i: usize) -> u8x16<S> {
    u8x16::from_slice(simd, &source[i..i + 16])
}

/// Return the end of an unquoted run that continues at `i`, and whether the
/// scanned bytes contain a `$`.
#[inline(always)]
fn scan_unquoted<S: Simd>(simd: S, source: &[u8], mut i: usize) -> (usize, bool) {
    let mut dollar = false;
    // Most tokens are short (numbers, dates, `yes`). Check the first bytes one
    // at a time, and use the vector scan only for a longer run.
    let short_end = (i + 8).min(source.len());
    while i < short_end {
        match RUN[source[i] as usize] {
            RUN_CONTINUE => {}
            RUN_STOP => return (i, dollar),
            _ => dollar = true,
        }
        i += 1;
    }
    while i + 16 <= source.len() {
        let v = load(simd, source, i);
        let stops = stop_lanes(v).to_bitmask();
        let dollars = v.simd_eq(b'$').to_bitmask();
        if stops != 0 {
            let at = stops.trailing_zeros();
            dollar |= dollars & ((1u64 << at) - 1) != 0;
            return (i + at as usize, dollar);
        }
        dollar |= dollars != 0;
        i += 16;
    }
    while i < source.len() {
        match RUN[source[i] as usize] {
            RUN_CONTINUE => {}
            RUN_STOP => break,
            _ => dollar = true,
        }
        i += 1;
    }
    (i, dollar)
}

/// Return the index of the first non-whitespace byte at or after `i`.
#[inline(always)]
fn skip_whitespace<S: Simd>(simd: S, source: &[u8], mut i: usize) -> usize {
    // Most whitespace runs are a newline and some indentation. Check the
    // first bytes one at a time, and use the vector scan only for a longer run.
    let short_end = (i + 8).min(source.len());
    while i < short_end {
        if !is_ws(source[i]) {
            return i;
        }
        i += 1;
    }
    while i + 16 <= source.len() {
        let other = !ws_lanes(load(simd, source, i)).to_bitmask() & 0xFFFF;
        if other != 0 {
            return i + other.trailing_zeros() as usize;
        }
        i += 16;
    }
    while i < source.len() && is_ws(source[i]) {
        i += 1;
    }
    i
}

/// Return the index of the first `\n` or `\r` at or after `i`, or the end of
/// the source.
#[inline(always)]
fn find_line_end<S: Simd>(simd: S, source: &[u8], mut i: usize) -> usize {
    while i + 16 <= source.len() {
        let v = load(simd, source, i);
        let hits = (v.simd_eq(b'\n') | v.simd_eq(b'\r')).to_bitmask();
        if hits != 0 {
            return i + hits.trailing_zeros() as usize;
        }
        i += 16;
    }
    while i < source.len() && source[i] != b'\n' && source[i] != b'\r' {
        i += 1;
    }
    i
}

/// Return the end of a quoted string whose body starts at `i`. The end is
/// after the closing quote. An unclosed string ends at the end of the source,
/// or at a newline when `newline_terminated` is set.
#[inline(always)]
fn scan_quoted<S: Simd>(simd: S, source: &[u8], mut i: usize, newline_terminated: bool) -> usize {
    let n = source.len();
    loop {
        // Find the next quote, backslash, or (optionally) line break.
        while i + 16 <= n {
            let v = load(simd, source, i);
            let mut hits = v.simd_eq(b'"') | v.simd_eq(b'\\');
            if newline_terminated {
                hits = hits | v.simd_eq(b'\n') | v.simd_eq(b'\r');
            }
            let hits = hits.to_bitmask();
            if hits != 0 {
                i += hits.trailing_zeros() as usize;
                break;
            }
            i += 16;
        }
        while i < n
            && !matches!(source[i], b'"' | b'\\')
            && !(newline_terminated && matches!(source[i], b'\n' | b'\r'))
        {
            i += 1;
        }
        if i >= n {
            return n;
        }
        match source[i] {
            b'\\' => i = (i + 2).min(n),
            b'"' => return i + 1,
            _ => return i, // a line break ends the string
        }
    }
}

/// A byte that ends a calc operand identifier or number.
#[inline]
fn is_calc_stop(b: u8) -> bool {
    is_ws(b)
        || matches!(
            b,
            b'(' | b')' | b'+' | b'-' | b'*' | b'/' | b'[' | b']' | b'{' | b'}'
        )
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
/// `CalcOpen` token, then structured interior tokens (operands and operators)
/// until the matching `]` (emitted as `CalcClose`) or end of input, and returns
/// the index just past what it consumed. Interior whitespace stays in the gaps.
#[inline(always)]
fn lex_calc<S: Simd>(
    simd: S,
    source: &[u8],
    mut i: usize,
    flags: u8,
    out: &mut Vec<Token>,
) -> usize {
    let n = source.len();
    out.push(Token {
        start: i as u32,
        len: 2,
        kind: SyntaxKind::CalcOpen,
        flags,
    });
    i += 2; // past `@[`

    while i < n {
        let b = source[i];
        let start = i;
        let kind;

        if b == b']' {
            out.push(Token {
                start: start as u32,
                len: 1,
                kind: SyntaxKind::CalcClose,
                flags: 0,
            });
            return i + 1; // leave calc mode
        } else if matches!(b, b'{' | b'}') {
            // A brace belongs to the surrounding Clausewitz construct. Keep it
            // outside the calc so a mismatched brace cannot receive a fix.
            return i;
        } else if is_ws(b) {
            i = skip_whitespace(simd, source, i + 1);
            continue;
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

        out.push(Token {
            start: start as u32,
            len: (i - start) as u32,
            kind,
            flags: 0,
        });
    }

    i // reached EOF without a closing `]` (unterminated calc)
}

/// Lex one trivia piece of a gap. A gap holds only whitespace, comments, and a
/// leading BOM, because the lexer emits every other byte as a token.
fn lex_trivia(source: &[u8], start: u32, end: u32) -> (SyntaxKind, u32) {
    let (s, e) = (start as usize, end as usize);
    let bytes = &source[s..e];
    if s == 0 && bytes.starts_with(BOM) {
        return (SyntaxKind::Bom, 3);
    }
    if bytes[0] == b'#' {
        let len = bytes
            .iter()
            .position(|&b| b == b'\n' || b == b'\r')
            .unwrap_or(bytes.len());
        return (SyntaxKind::Comment, len as u32);
    }
    debug_assert!(is_ws(bytes[0]), "a gap contains a significant byte");
    let len = bytes
        .iter()
        .position(|&b| !is_ws(b))
        .unwrap_or(bytes.len())
        .max(1);
    (SyntaxKind::Whitespace, len as u32)
}

// ---------------------------------------------------------------------------
// Diagnostics
// ---------------------------------------------------------------------------

/// The kind of a [`SyntaxError`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum SyntaxErrorKind {
    UnmatchedCloseBrace,
    UnclosedBlock,
    UnclosedInterpolation,
    UnclosedParameterHeader,
    UnclosedParameter,
    UnclosedCodePayload,
    CodePayloadMissingFirstBracket,
    CodePayloadMissingSecondBracket,
    UnclosedCalc,
    UnclosedCalcParen,
    BlockDepthExceeded,
    ParameterDepthExceeded,
    CodeDepthExceeded,
    CalcDepthExceeded,
    /// A `[` that starts no parameter, code payload, or interpolation.
    UnexpectedOpenBracket,
}

impl SyntaxErrorKind {
    /// A human-readable description.
    pub fn message(self) -> &'static str {
        match self {
            Self::UnmatchedCloseBrace => "unmatched '}'",
            Self::UnclosedBlock => "unclosed '{'",
            Self::UnclosedInterpolation => "unclosed bracket interpolation",
            Self::UnclosedParameterHeader => "unclosed parameter header",
            Self::UnclosedParameter => "unclosed parameter block",
            Self::UnclosedCodePayload => "unclosed code payload",
            Self::CodePayloadMissingFirstBracket => "unclosed code payload (missing first ']')",
            Self::CodePayloadMissingSecondBracket => "unclosed code payload (missing second ']')",
            Self::UnclosedCalc => "unclosed '@['",
            Self::UnclosedCalcParen => "unclosed parenthesized calculation",
            Self::BlockDepthExceeded => "maximum nesting depth exceeded; structure flattened",
            Self::ParameterDepthExceeded => {
                "maximum parameter nesting depth exceeded; structure flattened"
            }
            Self::CodeDepthExceeded => "maximum code nesting depth exceeded; structure flattened",
            Self::CalcDepthExceeded => "maximum calc nesting depth exceeded; structure flattened",
            Self::UnexpectedOpenBracket => "'[' starts no construct",
        }
    }
}

impl std::fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message())
    }
}

/// A non-fatal problem found while parsing. Parsing never fails — the tree is
/// always lossless — but recoverable issues are collected here for tooling.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    /// What is wrong.
    pub kind: SyntaxErrorKind,
    /// The half-open byte range the problem covers.
    pub range: TextRange,
    /// Structured information for a missing trailing delimiter.
    pub recovery: Option<MissingDelimiter>,
}

impl SyntaxError {
    /// A human-readable description.
    pub fn message(&self) -> &'static str {
        self.kind.message()
    }
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.message(), self.range)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryConstruct {
    Block,
    Interpolation,
    Parameter,
    Calculation,
    CodePayload,
    ParenthesizedCalculation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Applicability {
    MachineApplicable,
    Unsafe,
}

impl Applicability {
    /// Return `true` when a client may apply the repair automatically.
    pub fn is_machine_applicable(self) -> bool {
        matches!(self, Self::MachineApplicable)
    }
}

/// A missing trailing delimiter that the parser inserted as a zero-width token.
///
/// Use [`SyntaxTree::repair_applicability`] to know if a client can apply the
/// repair automatically. That check parses the repaired source again, so the
/// tree does it only when a client asks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingDelimiter {
    pub expected: SyntaxKind,
    pub missing: SyntaxKind,
    pub construct: RecoveryConstruct,
    pub opening_range: TextRange,
    pub insertion_range: TextRange,
    pub repair_text: String,
    pub order: u32,
    /// The parser's local decision, before the reparse check.
    local_applicability: Applicability,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Repair {
    pub range: TextRange,
    pub replacement: String,
}

// ---------------------------------------------------------------------------
// Tree storage: a buffer of significant tokens and a pre-order node tape.
// ---------------------------------------------------------------------------

/// Precomputed per-subtree summary bits, one byte per node, filled in while the
/// tree is built. A node's flags are the union of its own kind's bits and
/// *every* descendant's, so a consumer can skip a whole subtree with one O(1)
/// test — "does this block contain a comment? a syntax error? a calc? a macro
/// parameter?" — instead of walking it. This mirrors swift-syntax's
/// `RecursiveRawSyntaxFlags` and Carbon's per-node `has_error` bit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct NodeFlags(u8);

impl NodeFlags {
    /// The subtree contains at least one [`SyntaxKind::Comment`].
    pub const HAS_COMMENT: NodeFlags = NodeFlags(1 << 0);
    /// The subtree contains a syntax error or recovery element.
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
/// union of its descendants' bits. The match has no guards, so the compiler
/// can make it a lookup table.
#[inline]
fn own_flag_bits(kind: SyntaxKind) -> NodeFlags {
    match kind {
        SyntaxKind::Comment => NodeFlags::HAS_COMMENT,
        SyntaxKind::Error
        | SyntaxKind::Bogus
        | SyntaxKind::MissingCloseBrace
        | SyntaxKind::MissingCloseBracket
        | SyntaxKind::MissingCalcClose
        | SyntaxKind::MissingCloseParen => NodeFlags::HAS_ERROR,
        // The `Calc` node covers a well-formed calc; `CalcOpen` covers the rare
        // flattened case where the depth guard tripped before the node formed.
        SyntaxKind::Calc | SyntaxKind::CalcOpen => NodeFlags::HAS_CALC,
        SyntaxKind::MacroParam => NodeFlags::HAS_MACRO,
        _ => NodeFlags(0),
    }
}

/// An interior node. It covers the tokens `first_token..token_end`. Its
/// descendant nodes are `self_index + 1..subtree_end`.
#[derive(Debug, Clone, Copy)]
struct Node {
    kind: SyntaxKind,
    flags: NodeFlags,
    first_token: u32,
    token_end: u32,
    subtree_end: u32,
}

/// The parent of each node and each token. `NO_PARENT` marks the root.
struct Parents {
    nodes: Vec<u32>,
    tokens: Vec<u32>,
}

const NO_PARENT: u32 = u32::MAX;

/// A lossless syntax tree borrowing its source bytes.
pub struct SyntaxTree<'a> {
    source: &'a [u8],
    flavor: Flavor,
    /// The significant and missing tokens in sink emission order.
    tokens: Vec<Token>,
    nodes: Vec<Node>,
    errors: Vec<SyntaxError>,
    /// Calculated on the first parent query.
    parents: OnceLock<Parents>,
    /// Calculated on the first repair query: `true` when the repaired source
    /// parses without a missing-delimiter diagnostic.
    repairs_valid: OnceLock<bool>,
}

/// Parse `source` into a lossless [`SyntaxTree`] using the permissive superset.
pub fn parse(source: &[u8]) -> SyntaxTree<'_> {
    parse_with(source, Flavor::default())
}

/// Parse `source` into a lossless [`SyntaxTree`] with a specific [`Flavor`].
pub fn parse_with(source: &[u8], flavor: Flavor) -> SyntaxTree<'_> {
    let p = run(source, flavor, |lexed| {
        Builder::with_capacity(lexed.tokens.len())
    });
    let Parser {
        sink,
        mut tokens,
        lexed,
        errors,
        trailing_comment,
        ..
    } = p;
    let nodes = sink.finish(trailing_comment, &mut tokens, lexed);
    SyntaxTree {
        source,
        flavor,
        tokens,
        nodes,
        errors,
        parents: OnceLock::new(),
        repairs_valid: OnceLock::new(),
    }
}

/// Lex `source`, then send the events of the grammar to the sink that
/// `make_sink` makes.
fn run<S: Sink>(
    source: &[u8],
    flavor: Flavor,
    make_sink: impl FnOnce(&Lexed) -> S,
) -> Parser<'_, S> {
    assert!(
        u32::try_from(source.len()).is_ok(),
        "source is larger than 4 GiB"
    );
    let lexed = lex(source, flavor);
    let sink = make_sink(&lexed);
    let mut p = Parser::new(source, lexed, sink);
    p.sink.start_node(SyntaxKind::Root);
    p.parse_items(false);
    p.sink.finish_node();
    p
}

impl<'a> SyntaxTree<'a> {
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

    /// Return if a client can apply the repair of `delimiter` automatically.
    ///
    /// A repair is machine-applicable only when the parser found it safe, and
    /// when the source with all such repairs parses without a missing
    /// delimiter. The first call does that reparse.
    pub fn repair_applicability(&self, delimiter: &MissingDelimiter) -> Applicability {
        if delimiter.local_applicability.is_machine_applicable() && self.repairs_valid() {
            Applicability::MachineApplicable
        } else {
            Applicability::Unsafe
        }
    }

    /// Coalesced, machine-applicable delimiter insertions in source order.
    pub fn repair_fixes(&self) -> Vec<Repair> {
        if !self.repairs_valid() {
            return Vec::new();
        }
        self.local_fixes()
    }

    /// Apply all safe virtual delimiter repairs without changing other bytes.
    pub fn repair(&self) -> Vec<u8> {
        apply_repairs(self.source, self.repair_fixes())
    }

    fn repairs_valid(&self) -> bool {
        *self.repairs_valid.get_or_init(|| {
            let candidate = apply_repairs(self.source, self.local_fixes());
            candidate == self.source
                || !parse_with(&candidate, self.flavor)
                    .errors
                    .iter()
                    .any(|error| error.recovery.is_some())
        })
    }

    fn local_fixes(&self) -> Vec<Repair> {
        let mut records: Vec<&MissingDelimiter> = self
            .errors
            .iter()
            .filter_map(|e| e.recovery.as_ref())
            .filter(|r| r.local_applicability.is_machine_applicable())
            .collect();
        records.sort_by_key(|r| (r.insertion_range.start(), r.order));
        let mut fixes: Vec<Repair> = Vec::new();
        for record in records {
            if let Some(last) = fixes
                .last_mut()
                .filter(|f| f.range == record.insertion_range)
            {
                last.replacement.push_str(&record.repair_text);
            } else {
                fixes.push(Repair {
                    range: record.insertion_range,
                    replacement: record.repair_text.clone(),
                });
            }
        }
        fixes
    }

    /// Every leaf [`SyntaxToken`] in document order, trivia included.
    ///
    /// Concatenating [`SyntaxToken::text`] over this iterator reproduces
    /// [`source`](SyntaxTree::source): the lossless invariant in iterator form.
    /// Ideal for highlighters and other leaf-oriented tooling.
    pub fn tokens(&self) -> Tokens<'_, 'a> {
        Tokens {
            tree: self,
            next: 0,
            prev_end: 0,
            gap: Gap::EMPTY,
            pending: None,
            trailing_done: false,
        }
    }

    /// Every significant (non-trivia) leaf [`SyntaxToken`] in document order.
    pub fn significant_tokens(&self) -> impl Iterator<Item = SyntaxToken<'_, 'a>> + '_ {
        (0..self.tokens.len() as u32).map(move |i| self.token(i))
    }

    /// Reconstruct the source by walking the tree's leaves via the cursor API.
    ///
    /// Equals [`SyntaxTree::source`] for any tree; the lossless invariant.
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

    fn token(&self, idx: u32) -> SyntaxToken<'_, 'a> {
        let token = self.tokens[idx as usize];
        SyntaxToken {
            tree: self,
            kind: token.kind,
            range: token.range(),
            idx,
            trivia: false,
        }
    }

    fn trivia(&self, kind: SyntaxKind, start: u32, len: u32, next: u32) -> SyntaxToken<'_, 'a> {
        SyntaxToken {
            tree: self,
            kind,
            range: TextRange::new(start, start + len),
            idx: next,
            trivia: true,
        }
    }

    fn parents(&self) -> &Parents {
        self.parents.get_or_init(|| {
            let mut nodes = vec![NO_PARENT; self.nodes.len()];
            let mut tokens = vec![NO_PARENT; self.tokens.len()];
            let mut stack: Vec<u32> = Vec::new();
            let mut t = 0u32;
            let assign = |stack: &mut Vec<u32>, tokens: &mut [u32], t: u32| {
                while let Some(&top) = stack.last() {
                    if t < self.nodes[top as usize].token_end {
                        break;
                    }
                    stack.pop();
                }
                tokens[t as usize] = stack.last().copied().unwrap_or(NO_PARENT);
            };
            for (i, node) in self.nodes.iter().enumerate() {
                // Pre-order means `first_token` does not decrease, so every
                // token before this node belongs to a node already visited.
                while t < node.first_token {
                    assign(&mut stack, &mut tokens, t);
                    t += 1;
                }
                while let Some(&top) = stack.last() {
                    if (i as u32) < self.nodes[top as usize].subtree_end {
                        break;
                    }
                    stack.pop();
                }
                nodes[i] = stack.last().copied().unwrap_or(NO_PARENT);
                stack.push(i as u32);
            }
            while (t as usize) < self.tokens.len() {
                assign(&mut stack, &mut tokens, t);
                t += 1;
            }
            Parents { nodes, tokens }
        })
    }

    /// The node that owns the gap before token `next`: the nearest common
    /// ancestor of the tokens on the two sides of the gap.
    fn gap_owner(&self, next: u32) -> u32 {
        if next == 0 || next as usize >= self.tokens.len() {
            return 0;
        }
        let parents = self.parents();
        let mut left = Vec::new();
        let mut node = parents.tokens[next as usize - 1];
        while node != NO_PARENT {
            left.push(node);
            node = parents.nodes[node as usize];
        }
        let mut node = parents.tokens[next as usize];
        while node != NO_PARENT {
            if left.contains(&node) {
                return node;
            }
            node = parents.nodes[node as usize];
        }
        0
    }
}

fn apply_repairs(source: &[u8], fixes: Vec<Repair>) -> Vec<u8> {
    let mut out = source.to_vec();
    for fix in fixes.into_iter().rev() {
        out.splice(fix.range.to_usize(), fix.replacement.bytes());
    }
    out
}

/// Receives the parser's events in source order. The grammar knows only this
/// interface, so a different storage can reuse the same grammar.
trait Sink {
    /// Open a node. Its first token is the next token.
    fn start_node(&mut self, kind: SyntaxKind);
    /// Add the next token to the open node.
    fn token(&mut self, token: Token);
    /// Close the most recently opened node.
    fn finish_node(&mut self);
    /// Record that the open node contains a diagnostic.
    fn error(&mut self);

    /// Add a field node whose key, operator, and value are single tokens. A
    /// sink can do this faster than the four separate events.
    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        self.start_node(SyntaxKind::Field);
        self.token(key);
        self.token(op);
        self.token(value);
        self.finish_node();
    }
}

/// Build the node tape and record the positions of missing tokens.
struct Builder {
    nodes: Vec<Node>,
    /// The open nodes and the flags collected for each.
    stack: Vec<(u32, NodeFlags)>,
    /// The index of the next token.
    next_token: u32,
    missing_tokens: Vec<(u32, Token)>,
}

impl Builder {
    fn with_capacity(tokens: usize) -> Self {
        // Nodes are a fraction of tokens: approximately one per field.
        Builder {
            nodes: Vec::with_capacity(tokens / 2 + 1),
            stack: Vec::new(),
            next_token: 0,
            missing_tokens: Vec::new(),
        }
    }

    fn finish(
        mut self,
        trailing_comment: bool,
        tokens: &mut Vec<Token>,
        lexed: usize,
    ) -> Vec<Node> {
        if !self.missing_tokens.is_empty() {
            let mut ordered = Vec::with_capacity(tokens.len());
            let mut significant = tokens[..lexed].iter().copied().peekable();
            for (index, mut token) in self.missing_tokens {
                while ordered.len() < index as usize {
                    ordered.push(significant.next().expect("missing significant token"));
                }
                if let Some(next) = significant.peek() {
                    token.start = next.start;
                }
                ordered.push(token);
            }
            ordered.extend(significant);
            *tokens = ordered;
        }
        debug_assert!(self.stack.is_empty(), "unfinished nodes remain");
        if trailing_comment {
            self.nodes[0].flags.insert(NodeFlags::HAS_COMMENT);
        }
        self.nodes
    }
}

impl Sink for Builder {
    #[inline]
    fn start_node(&mut self, kind: SyntaxKind) {
        let idx = self.nodes.len() as u32;
        self.nodes.push(Node {
            kind,
            flags: NodeFlags(0),
            first_token: self.next_token,
            token_end: 0,
            subtree_end: 0,
        });
        self.stack.push((idx, own_flag_bits(kind)));
    }

    #[inline]
    fn token(&mut self, token: Token) {
        let index = self.next_token;
        self.next_token += 1;
        if token.kind.is_missing() {
            self.missing_tokens.push((index, token));
        }
        if let Some((_, flags)) = self.stack.last_mut() {
            flags.insert(own_flag_bits(token.kind));
        }
        if token.flags & TOKEN_COMMENT_BEFORE != 0 {
            // The comment belongs to the deepest open node that also holds
            // the previous token. The root holds a leading comment.
            let nodes = &self.nodes;
            let owner = self
                .stack
                .iter_mut()
                .rev()
                .find(|(node, _)| nodes[*node as usize].first_token < index);
            if let Some((_, flags)) = owner {
                flags.insert(NodeFlags::HAS_COMMENT);
            } else if let Some((_, flags)) = self.stack.first_mut() {
                flags.insert(NodeFlags::HAS_COMMENT);
            }
        }
    }

    #[inline]
    fn finish_node(&mut self) {
        let (idx, flags) = self.stack.pop().expect("finish_node without start_node");
        let subtree_end = self.nodes.len() as u32;
        let node = &mut self.nodes[idx as usize];
        node.flags = flags;
        node.token_end = self.next_token;
        node.subtree_end = subtree_end;
        if let Some((_, parent)) = self.stack.last_mut() {
            parent.insert(flags);
        }
    }

    fn error(&mut self) {
        if let Some((_, flags)) = self.stack.last_mut() {
            flags.insert(NodeFlags::HAS_ERROR);
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

struct Parser<'t, S: Sink> {
    source: &'t [u8],
    /// The lexed tokens. The parser appends missing tokens after them.
    tokens: Vec<Token>,
    /// The number of lexed tokens. A position at or after it is EOF.
    lexed: usize,
    pos: usize,
    sink: S,
    errors: Vec<SyntaxError>,
    /// The gap after the last lexed token contains a comment.
    trailing_comment: bool,
    /// Current block-nesting depth, compared against [`MAX_DEPTH`].
    depth: u32,
    /// Current EU4 parameter nesting depth.
    parameter_depth: u32,
    /// Unclosed ordinary square brackets inside the current parameter body.
    bracket_depth: u32,
    /// Current EU5 code-payload nesting depth.
    code_depth: u32,
    /// Number of non-recoverable interruptions seen below this parser.
    recovery_barrier: u32,
    /// Number of missing delimiters inserted at EOF.
    eof_repairs: u32,
    /// Sorted indexes of the `[` tokens that start a closed interpolation.
    /// Calculated when the parser first needs it.
    interpolation_starts: Option<Vec<u32>>,
}

impl<'t, S: Sink> Parser<'t, S> {
    fn new(source: &'t [u8], lexed: Lexed, sink: S) -> Self {
        let count = lexed.tokens.len();
        Parser {
            source,
            tokens: lexed.tokens,
            lexed: count,
            pos: 0,
            sink,
            errors: Vec::new(),
            trailing_comment: lexed.trailing_comment,
            depth: 0,
            parameter_depth: 0,
            bracket_depth: 0,
            code_depth: 0,
            recovery_barrier: 0,
            eof_repairs: 0,
            interpolation_starts: None,
        }
    }

    /// Insert a zero-width `missing` token at EOF and record its repair.
    fn missing(
        &mut self,
        open: usize,
        missing: SyntaxKind,
        construct: RecoveryConstruct,
        kind: SyntaxErrorKind,
    ) {
        let at = self.source.len() as u32;
        let order = self.eof_repairs;
        self.eof_repairs += 1;
        let mut repair_text = missing.missing_text().unwrap().to_owned();
        // A delimiter appended to an unterminated line comment remains comment
        // text. Start a new line and preserve the file's established ending.
        if order == 0
            && self.trailing_comment
            && !self.source.ends_with(b"\n")
            && !self.source.ends_with(b"\r")
        {
            let newline = if self.source.windows(2).any(|w| w == b"\r\n") {
                "\r\n"
            } else {
                "\n"
            };
            repair_text.insert_str(0, newline);
        }
        let unsafe_quote = self.lexed > 0 && {
            let last = self.tokens[self.lexed - 1];
            last.kind == SyntaxKind::Quoted
                && last.end() == at
                && !quote_is_closed(self.token_text(self.lexed - 1))
        };
        self.push_missing(missing);
        self.errors.push(SyntaxError {
            kind,
            range: TextRange::empty(at),
            recovery: Some(MissingDelimiter {
                expected: missing.expected_kind().unwrap(),
                missing,
                construct,
                opening_range: self.tokens[open].range(),
                insertion_range: TextRange::empty(at),
                repair_text,
                order,
                local_applicability: if unsafe_quote {
                    Applicability::Unsafe
                } else {
                    Applicability::MachineApplicable
                },
            }),
        });
    }

    /// Append a zero-width token at EOF and send it to the sink.
    fn push_missing(&mut self, kind: SyntaxKind) {
        let token = Token {
            start: self.source.len() as u32,
            len: 0,
            kind,
            flags: 0,
        };
        self.tokens.push(token);
        self.sink.token(token);
    }

    /// Record a diagnostic that starts at the emitted token `index`. Call it
    /// while the nodes that hold the token are still open.
    fn error_at(&mut self, index: usize, kind: SyntaxErrorKind) {
        self.tokens[index].flags |= TOKEN_ERROR;
        self.sink.error();
        self.errors.push(SyntaxError {
            kind,
            range: self.tokens[index].range(),
            recovery: None,
        });
    }

    #[inline]
    fn kind_at(&self, index: usize) -> Option<SyntaxKind> {
        self.tokens[..self.lexed].get(index).map(|token| token.kind)
    }

    #[inline]
    fn peek(&self) -> Option<SyntaxKind> {
        self.kind_at(self.pos)
    }

    #[inline]
    fn bump(&mut self) {
        let t = self.tokens[self.pos];
        self.sink.token(t);
        self.pos += 1;
    }

    /// Return `true` when the field at the current position has a scalar
    /// value that is a single token. The key and the operator are known.
    /// A value that a brace or a bracket follows can be a header or a code
    /// payload, so it goes on the general path.
    #[inline]
    fn is_scalar_field(&self) -> bool {
        self.kind_at(self.pos + 2)
            .is_some_and(SyntaxKind::is_scalar)
            && !matches!(
                self.kind_at(self.pos + 3),
                Some(SyntaxKind::OpenBrace | SyntaxKind::OpenBracket)
            )
    }

    /// Return `true` when the `]` at the current position is the key of a
    /// field, as in `active_idea_groups = { ]=0 }`.
    fn is_bracket_key(&self) -> bool {
        self.parameter_depth == 0
            && self.code_depth == 0
            && self.kind_at(self.pos + 1) == Some(SyntaxKind::Operator)
    }

    fn is_code_close(&self, index: usize) -> bool {
        self.kind_at(index) == Some(SyntaxKind::CloseBracket)
            && self.kind_at(index + 1) == Some(SyntaxKind::CloseBracket)
    }

    fn parse_items(&mut self, in_block: bool) {
        loop {
            match self.peek() {
                None => break,
                Some(SyntaxKind::CloseBracket)
                    if self.parameter_depth > 0 && self.bracket_depth == 0 =>
                {
                    break;
                }
                Some(SyntaxKind::CloseBracket) if self.is_bracket_key() => {
                    // EU4 saves contain `]=0` as a field. The `]` is a scalar.
                    self.tokens[self.pos].kind = SyntaxKind::Unquoted;
                    self.parse_item();
                }
                Some(SyntaxKind::CloseBracket) if in_block && self.parameter_depth == 0 => {
                    // A raw `]` interrupts an ordinary block. Leave it for the
                    // enclosing parser and refuse a virtual `}` for this block.
                    break;
                }
                Some(SyntaxKind::CloseBracket)
                    if self.code_depth > 0 && self.is_code_close(self.pos) =>
                {
                    break;
                }
                Some(SyntaxKind::CloseBrace)
                    if in_block && self.parameter_depth > 0 && self.bracket_depth > 0 =>
                {
                    self.parse_item()
                }
                Some(SyntaxKind::CloseBrace) if in_block => break, // caller eats `}`
                Some(SyntaxKind::CloseBrace) => {
                    // Unmatched `}` at the top level: wrap in Bogus and record.
                    let index = self.pos;
                    self.sink.start_node(SyntaxKind::Bogus);
                    self.bump();
                    self.error_at(index, SyntaxErrorKind::UnmatchedCloseBrace);
                    self.sink.finish_node();
                }
                Some(_) => self.parse_item(),
            }
        }
    }

    fn parse_item(&mut self) {
        match self.peek() {
            Some(k) if k.is_scalar() => {
                if self.is_code_statement() {
                    self.parse_code_statement();
                    return;
                }
                match self.kind_at(self.pos + 1) {
                    Some(SyntaxKind::Operator) if self.is_scalar_field() => {
                        let (key, op, value) = (
                            self.tokens[self.pos],
                            self.tokens[self.pos + 1],
                            self.tokens[self.pos + 2],
                        );
                        self.sink.scalar_field(key, op, value);
                        self.pos += 3;
                    }
                    Some(SyntaxKind::Operator) => {
                        // key <op> value
                        self.sink.start_node(SyntaxKind::Field);
                        self.bump(); // key
                        self.bump(); // operator
                        self.parse_value();
                        self.sink.finish_node();
                    }
                    Some(SyntaxKind::OpenBrace) => {
                        // key { ... }: a field with no operator. The semantic
                        // layer decides if the scalar is a key or a header.
                        self.sink.start_node(SyntaxKind::Field);
                        self.bump(); // key
                        self.parse_block();
                        self.sink.finish_node();
                    }
                    // bare scalar (array element / loose value)
                    _ => self.bump(),
                }
            }
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            Some(SyntaxKind::OpenBracket) => self.parse_open_bracket(),
            Some(SyntaxKind::CloseBracket) if self.parameter_depth > 0 => {
                self.bump();
                self.bracket_depth = self.bracket_depth.saturating_sub(1);
            }
            // Operators, bang, and macros in item position: keep verbatim.
            Some(_) => self.bump(),
            None => {}
        }
    }

    fn parse_value(&mut self) {
        match self.peek() {
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            Some(SyntaxKind::OpenBracket) => self.parse_open_bracket(),
            Some(SyntaxKind::Unquoted) if self.is_code_statement() => self.parse_code_statement(),
            Some(SyntaxKind::Unquoted)
                if self.kind_at(self.pos + 1) == Some(SyntaxKind::OpenBrace) =>
            {
                // headered block: `rgb { ... }`, `hsv { ... }`, tag { ... }
                self.sink.start_node(SyntaxKind::HeaderedBlock);
                self.bump(); // header scalar
                self.parse_block();
                self.sink.finish_node();
            }
            Some(k) if k != SyntaxKind::CloseBrace => self.bump(),
            // missing value (e.g. `a =` at EOF, or `a = }`): emit nothing.
            _ => {}
        }
    }

    /// Return the EU4 parameter node kind at the current token position.
    ///
    /// EU5 uses `code [[ ... ]]` for a different payload format. The EU4 form
    /// has a compact header with a name immediately before its first `]`.
    fn parameter_kind(&self) -> Option<SyntaxKind> {
        let open = self.pos;
        if self.code_depth > 0 {
            return None;
        }
        if self.kind_at(open)? != SyntaxKind::OpenBracket
            || self.kind_at(open + 1)? != SyntaxKind::OpenBracket
        {
            return None;
        }

        let mut name = open + 2;
        let kind = if self.kind_at(name)? == SyntaxKind::Bang {
            name += 1;
            SyntaxKind::UndefinedParameter
        } else {
            SyntaxKind::Parameter
        };

        if self.kind_at(name)? != SyntaxKind::Unquoted {
            return None;
        }

        // The header must close directly after the name, or the input must
        // end there (an incomplete header).
        let header_close = self.kind_at(name + 1);
        if header_close.is_some_and(|kind| kind != SyntaxKind::CloseBracket) {
            return None;
        }

        // Keep the EU5 `code [[...]]` payload out of the EU4 node grammar even
        // when a compact payload happens to resemble a parameter header.
        if self.preceded_by_code(open) {
            return None;
        }

        Some(kind)
    }

    fn code_payload_start(&self, open: usize) -> bool {
        self.kind_at(open) == Some(SyntaxKind::OpenBracket)
            && self.kind_at(open + 1) == Some(SyntaxKind::OpenBracket)
            && self.preceded_by_code(open)
    }

    #[inline]
    fn is_code_keyword(&self, index: usize) -> bool {
        self.kind_at(index) == Some(SyntaxKind::Unquoted)
            && self.tokens[index].len == 4
            && self.token_text(index) == b"code"
    }

    #[inline]
    fn is_code_statement(&self) -> bool {
        self.is_code_keyword(self.pos) && self.code_payload_start(self.pos + 1)
    }

    fn code_statement_open(&self, index: usize) -> Option<usize> {
        if !self.is_code_keyword(index) {
            return None;
        }

        let mut next = index + 1;
        if self.kind_at(next) == Some(SyntaxKind::Operator) && self.token_text(next) == b"=" {
            next += 1;
        }
        self.code_payload_start(next).then_some(next)
    }

    fn token_text(&self, index: usize) -> &'t [u8] {
        &self.source[self.tokens[index].range().to_usize()]
    }

    fn preceded_by_code(&self, open: usize) -> bool {
        let Some(previous) = open.checked_sub(1) else {
            return false;
        };
        if self.is_code_keyword(previous) {
            return true;
        }
        if self.tokens[previous].kind != SyntaxKind::Operator || self.token_text(previous) != b"=" {
            return false;
        }
        previous
            .checked_sub(1)
            .is_some_and(|code| self.is_code_keyword(code))
    }

    fn parse_open_bracket(&mut self) {
        if self.code_payload_start(self.pos) {
            self.parse_code_payload();
        } else if let Some(kind) = self.parameter_kind() {
            if self.parameter_depth >= MAX_DEPTH {
                let index = self.pos;
                self.recovery_barrier += 1;
                self.bump();
                self.error_at(index, SyntaxErrorKind::ParameterDepthExceeded);
                self.bracket_depth += 1;
            } else {
                self.parse_parameter(kind);
            }
        } else if self.is_interpolation_start() {
            self.parse_interpolation();
        } else {
            let index = self.pos;
            self.bump();
            if self.parameter_depth > 0 {
                self.bracket_depth += 1;
            } else {
                // A bare `[` has no clear construct at this position. Report
                // it, but do not let an outer EOF repair hide that ambiguity.
                self.recovery_barrier += 1;
                self.error_at(index, SyntaxErrorKind::UnexpectedOpenBracket);
            }
        }
    }

    fn is_interpolation_start(&mut self) -> bool {
        if self.peek() != Some(SyntaxKind::OpenBracket)
            || self.kind_at(self.pos + 1) == Some(SyntaxKind::OpenBracket)
        {
            return false;
        }
        let pos = self.pos as u32;
        self.interpolation_starts
            .get_or_insert_with(|| interpolation_starts(&self.tokens[..self.lexed]))
            .binary_search(&pos)
            .is_ok()
    }

    fn parse_interpolation(&mut self) {
        let open = self.pos;
        self.sink.start_node(SyntaxKind::Interpolation);
        self.bump(); // `[`

        let mut bracket_depth = 1u32;
        let mut closed = false;
        loop {
            match self.peek() {
                Some(SyntaxKind::OpenBracket) => {
                    bracket_depth += 1;
                    self.bump();
                }
                Some(SyntaxKind::CloseBracket) => {
                    bracket_depth -= 1;
                    self.bump();
                    if bracket_depth == 0 {
                        closed = true;
                        break;
                    }
                }
                Some(SyntaxKind::CloseBrace) if bracket_depth == 1 => {
                    self.recovery_barrier += 1;
                    break;
                }
                None => break,
                Some(_) => self.bump(),
            }
        }

        if !closed {
            if self.peek().is_none() {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBracket,
                    RecoveryConstruct::Interpolation,
                    SyntaxErrorKind::UnclosedInterpolation,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedInterpolation);
            }
        }
        self.sink.finish_node();
    }

    fn parse_code_statement(&mut self) {
        self.sink.start_node(SyntaxKind::Code);
        self.bump(); // `code`
        self.parse_code_payload_body();
        self.sink.finish_node();
    }

    fn parse_code_payload(&mut self) {
        self.sink.start_node(SyntaxKind::Code);
        self.parse_code_payload_body();
        self.sink.finish_node();
    }

    fn parse_code_payload_body(&mut self) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.bump(); // first `[`
        self.bump(); // second `[`

        self.code_depth += 1;
        if self.code_depth >= MAX_DEPTH {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::CodeDepthExceeded);
            self.flatten_to_code_close();
            self.code_depth -= 1;
            // The depth diagnostic owns this flattened region.
            return;
        }

        let mut closed = false;
        loop {
            match self.peek() {
                Some(SyntaxKind::CloseBracket) if self.is_code_close(self.pos) => {
                    self.bump();
                    self.bump();
                    closed = true;
                    break;
                }
                Some(SyntaxKind::CloseBrace) => {
                    // Keep an outer block delimiter available when the payload
                    // is incomplete. A brace directly before `]]` is payload.
                    if !self.is_code_close(self.pos + 1) {
                        if self.pos + 1 < self.lexed {
                            self.recovery_barrier += 1;
                        }
                        break;
                    }
                    self.bump();
                }
                None => break,
                Some(_) => self.parse_item(),
            }
        }
        self.code_depth -= 1;

        if !closed {
            if self.peek().is_none() && self.recovery_barrier == barrier {
                let missing_count = if self.pos.checked_sub(1).is_some_and(|index| {
                    let token = self.tokens[index];
                    token.kind == SyntaxKind::CloseBracket
                        && token.end() == self.source.len() as u32
                }) {
                    1
                } else {
                    2
                };
                for index in 0..missing_count {
                    let kind = if missing_count == 1 || index == 1 {
                        SyntaxErrorKind::CodePayloadMissingSecondBracket
                    } else {
                        SyntaxErrorKind::CodePayloadMissingFirstBracket
                    };
                    self.missing(
                        open,
                        SyntaxKind::MissingCloseBracket,
                        RecoveryConstruct::CodePayload,
                        kind,
                    );
                }
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedCodePayload);
            }
        }
    }

    /// Consume a code payload without recursing into nested code statements.
    fn flatten_to_code_close(&mut self) -> bool {
        let mut payload_depth = 1u32;
        while let Some(kind) = self.peek() {
            if self.is_code_close(self.pos) {
                self.bump();
                self.bump();
                payload_depth -= 1;
                if payload_depth == 0 {
                    return true;
                }
                continue;
            }

            if kind == SyntaxKind::Unquoted && self.code_statement_open(self.pos).is_some() {
                payload_depth += 1;
            }

            if kind == SyntaxKind::CloseBrace && !self.is_code_close(self.pos + 1) {
                break;
            }
            self.bump();
        }
        false
    }

    /// Parse an EU4 conditional parameter and its complete body.
    fn parse_parameter(&mut self, kind: SyntaxKind) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.sink.start_node(kind);
        self.bump(); // first `[`
        self.bump(); // second `[`
        if kind == SyntaxKind::UndefinedParameter {
            self.bump(); // `!`
        }
        self.bump(); // parameter name
        if self.peek() == Some(SyntaxKind::CloseBracket) {
            self.bump(); // header `]`
        } else {
            self.missing(
                open,
                SyntaxKind::MissingCloseBracket,
                RecoveryConstruct::Parameter,
                SyntaxErrorKind::UnclosedParameterHeader,
            );
        }

        self.parameter_depth += 1;
        let outer_bracket_depth = self.bracket_depth;
        self.bracket_depth = 0;
        let mut closed = false;
        loop {
            match self.peek() {
                Some(SyntaxKind::CloseBracket) if self.bracket_depth == 0 => {
                    self.bump();
                    closed = true;
                    break;
                }
                Some(SyntaxKind::CloseBrace) if self.bracket_depth == 0 => {
                    // EU4 parameter bodies can carry a brace across two
                    // parameter blocks, as in `... = { ]` followed by
                    // `[[name] } ]`. Keep that brace in the body when its
                    // parameter close follows it.
                    if self.kind_at(self.pos + 1) == Some(SyntaxKind::CloseBracket) {
                        self.bump();
                        continue;
                    }
                    self.recovery_barrier += 1;
                    break;
                }
                None => break,
                Some(_) => self.parse_item(),
            }
        }
        self.parameter_depth -= 1;
        self.bracket_depth = outer_bracket_depth;

        if !closed {
            if self.peek().is_none() && self.recovery_barrier == barrier {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBracket,
                    RecoveryConstruct::Parameter,
                    SyntaxErrorKind::UnclosedParameter,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedParameter);
            }
        }
        self.sink.finish_node();
    }

    fn parse_block(&mut self) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.sink.start_node(SyntaxKind::Block);
        self.bump(); // `{`
        self.depth += 1;
        if self.depth >= MAX_DEPTH {
            self.recovery_barrier += 1;
            // Pathologically deep nesting. Rather than recurse (and risk a
            // stack overflow), consume the rest of this block — including
            // everything nested inside it — as flat leaf tokens. The tree
            // stays lossless; it just loses structure past this point.
            self.error_at(open, SyntaxErrorKind::BlockDepthExceeded);
            self.flatten_to_block_close();
        } else {
            self.parse_items(true);
            if self.peek() == Some(SyntaxKind::CloseBrace) {
                self.bump(); // `}`
            } else if self.peek() == Some(SyntaxKind::CloseBracket)
                && self.parameter_depth > 0
                && self.bracket_depth == 0
            {
                // The parameter delimiter can close a block body that is
                // intentionally continued by a later parameter block.
            } else if self.peek() == Some(SyntaxKind::CloseBracket) {
                self.recovery_barrier += 1;
                self.error_at(open, SyntaxErrorKind::UnclosedBlock);
            } else if self.peek().is_none() && self.recovery_barrier == barrier {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBrace,
                    RecoveryConstruct::Block,
                    SyntaxErrorKind::UnclosedBlock,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedBlock);
            }
        }
        self.depth -= 1;
        self.sink.finish_node();
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
    /// [`parse_calc_interior`], then emitted to the sink in source order.
    fn parse_calc(&mut self) {
        let open = self.pos;
        self.sink.start_node(SyntaxKind::Calc);
        self.bump(); // `@[`

        // The interior is everything up to the matching `]` (or EOF). The lexer
        // only ever emits calc tokens between a CalcOpen and its CalcClose, so
        // no calc token can leak past this slice into the outer parser.
        let from = self.pos;
        while !matches!(
            self.peek(),
            Some(SyntaxKind::CalcClose | SyntaxKind::OpenBrace | SyntaxKind::CloseBrace) | None
        ) {
            self.pos += 1;
        }
        let at_eof = self.peek().is_none();
        let interior = parse_calc_interior(&self.tokens[from..self.pos], from as u32, at_eof);
        for el in &interior.elems {
            self.emit_calc(
                el,
                if at_eof {
                    &[]
                } else {
                    &interior.missing_parens
                },
            );
        }
        if !at_eof && !interior.missing_parens.is_empty() {
            self.recovery_barrier += 1;
        }
        if at_eof && !interior.overflowed {
            for paren_open in interior.missing_parens {
                // The zero-width leaf was emitted inside its ParenExpr above.
                let at = self.source.len() as u32;
                let order = self.eof_repairs;
                self.eof_repairs += 1;
                self.errors.push(SyntaxError {
                    kind: SyntaxErrorKind::UnclosedCalcParen,
                    range: TextRange::empty(at),
                    recovery: Some(MissingDelimiter {
                        expected: SyntaxKind::CloseParen,
                        missing: SyntaxKind::MissingCloseParen,
                        construct: RecoveryConstruct::ParenthesizedCalculation,
                        opening_range: self.tokens[paren_open as usize].range(),
                        insertion_range: TextRange::empty(at),
                        repair_text: ")".into(),
                        order,
                        local_applicability: Applicability::MachineApplicable,
                    }),
                });
            }
        }
        if interior.overflowed {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::CalcDepthExceeded);
        }

        if self.peek() == Some(SyntaxKind::CalcClose) {
            self.bump(); // `]`
        } else if at_eof && !interior.overflowed {
            self.missing(
                open,
                SyntaxKind::MissingCalcClose,
                RecoveryConstruct::Calculation,
                SyntaxErrorKind::UnclosedCalc,
            );
        } else if !at_eof {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::UnclosedCalc);
        }
        self.sink.finish_node();
    }

    /// Walk a [`CalcElem`] in pre-order and send its tokens and nodes to the sink.
    fn emit_calc(&mut self, el: &CalcElem, missing_parens: &[u32]) {
        match el {
            CalcElem::Leaf(index) => {
                let token = self.tokens[*index as usize];
                self.sink.token(token);
            }
            CalcElem::Missing(kind) => self.push_missing(*kind),
            CalcElem::Node { kind, children, .. } => {
                self.sink.start_node(*kind);
                if *kind == SyntaxKind::ParenExpr
                    && let Some(CalcElem::Leaf(open)) = children.first()
                    && missing_parens.contains(open)
                {
                    self.error_at(*open as usize, SyntaxErrorKind::UnclosedCalcParen);
                }
                for c in children {
                    self.emit_calc(c, missing_parens);
                }
                self.sink.finish_node();
            }
        }
    }
}

/// Find each `[` that starts a closed single-bracket interpolation, in one pass.
///
/// A `[` starts an interpolation when its matching `]` comes before a `}` that
/// is directly inside it. At EOF, a still-open `[` that is followed by more
/// tokens is a trailing interpolation. A bare final `[` is not.
fn interpolation_starts(tokens: &[Token]) -> Vec<u32> {
    // Open brackets and whether a `}` interrupted each one.
    let mut stack: Vec<(u32, bool)> = Vec::new();
    let mut starts = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        match token.kind {
            SyntaxKind::OpenBracket => stack.push((index as u32, false)),
            SyntaxKind::CloseBracket => {
                if let Some((open, false)) = stack.pop() {
                    starts.push(open);
                }
            }
            SyntaxKind::CloseBrace => {
                if let Some(top) = stack.last_mut() {
                    top.1 = true;
                }
            }
            _ => {}
        }
    }
    let count = tokens.len() as u32;
    starts.extend(
        stack
            .into_iter()
            .filter(|&(open, interrupted)| !interrupted && open + 1 < count)
            .map(|(open, _)| open),
    );
    starts.sort_unstable();
    starts
}

// ---------------------------------------------------------------------------
// Calc expression grammar (interior of `@[ ... ]`).
//
// operands : Number | CalcIdent | `(` expr `)` | (`-`|`+`) operand
// binary   : `+` `-` (looser) and `*` `/` (tighter), left-associative
//
// Parsed precedence-climbing into a temporary `CalcElem` tree, which is then
// walked in pre-order to emit tokens/nodes. Interior whitespace stays in the
// gaps between tokens, so the region round-trips byte-for-byte.
// ---------------------------------------------------------------------------

/// A node in the temporary calc tree (see [`parse_calc_interior`]).
enum CalcElem {
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
struct CalcInterior {
    /// The expression, then any unconsumed (malformed) tokens.
    elems: Vec<CalcElem>,
    /// `true` if the [`MAX_DEPTH`] guard tripped.
    overflowed: bool,
    /// The token indexes of `(` that have no `)`, innermost first.
    missing_parens: Vec<u32>,
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
/// flat list of [`CalcElem`]s: the expression, then any unconsumed (malformed)
/// tokens. Total over any token slice. `base` is the index of the first token
/// of `toks` in the token buffer. When the [`MAX_DEPTH`] guard trips, parsing
/// stops descending but every token is still emitted as a flat leaf.
fn parse_calc_interior(toks: &[Token], base: u32, insert_missing_parens: bool) -> CalcInterior {
    let mut c = CalcCursor {
        toks,
        base,
        pos: 0,
        depth: 0,
        overflowed: false,
        missing_parens: Vec::new(),
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
    }
    CalcInterior {
        elems,
        overflowed: c.overflowed,
        missing_parens: c.missing_parens,
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
        // Limit parser recursion through parentheses and unary operators.
        if self.overflowed || self.depth >= MAX_DEPTH {
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

// ---------------------------------------------------------------------------
// Cursors: lightweight handles into the tree. `'t` borrows the tree, `'a` is
// the source lifetime. Trivia tokens are made on demand from the gaps.
// ---------------------------------------------------------------------------

/// A handle to an interior node in a [`SyntaxTree`].
#[derive(Clone, Copy)]
pub struct SyntaxNode<'t, 'a> {
    tree: &'t SyntaxTree<'a>,
    idx: u32,
}

/// A handle to a leaf token in a [`SyntaxTree`]. It is a significant token from
/// the token buffer, or a trivia token made from a gap.
#[derive(Clone, Copy)]
pub struct SyntaxToken<'t, 'a> {
    tree: &'t SyntaxTree<'a>,
    kind: SyntaxKind,
    range: TextRange,
    /// For a significant token, its index in the token buffer. For trivia, the
    /// index of the significant token after the gap.
    idx: u32,
    trivia: bool,
}

/// Either a [`SyntaxNode`] or a [`SyntaxToken`].
#[derive(Clone, Copy)]
pub enum SyntaxElement<'t, 'a> {
    Node(SyntaxNode<'t, 'a>),
    Token(SyntaxToken<'t, 'a>),
}

impl<'t, 'a> SyntaxNode<'t, 'a> {
    #[inline]
    fn data(&self) -> &'t Node {
        &self.tree.nodes[self.idx as usize]
    }

    /// This node's kind.
    pub fn kind(&self) -> SyntaxKind {
        self.data().kind
    }

    /// The half-open byte range this node spans in the source. The root spans
    /// the whole source. Other nodes span from their first token to their last.
    pub fn text_range(&self) -> TextRange {
        let node = self.data();
        if self.idx == 0 {
            return TextRange::new(0, self.tree.source.len() as u32);
        }
        let tokens = &self.tree.tokens;
        TextRange::new(
            tokens[node.first_token as usize].start,
            tokens[node.token_end as usize - 1].end(),
        )
    }

    /// The source bytes this node spans (a lossless slice of the subtree).
    pub fn text(&self) -> &'a [u8] {
        &self.tree.source[self.text_range().to_usize()]
    }

    /// The parent node, or `None` for the root.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = self.tree.parents().nodes[self.idx as usize];
        (p != NO_PARENT).then_some(SyntaxNode {
            tree: self.tree,
            idx: p,
        })
    }

    /// The precomputed [`NodeFlags`] summarizing this node's whole subtree (see
    /// [`NodeFlags`]). An O(1) lookup — no subtree walk.
    pub fn flags(&self) -> NodeFlags {
        self.data().flags
    }

    /// Whether this subtree contains a syntax error or recovery element. Lets
    /// a linter cheaply skip or downgrade checks over malformed input.
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

    /// All direct children, nodes and tokens, in source order. This includes
    /// the trivia tokens that this node owns.
    pub fn children(&self) -> Children<'t, 'a> {
        self.children_impl(true)
    }

    /// The direct children without trivia. This is faster than
    /// [`children`](Self::children) because it does not lex the gaps.
    pub fn significant_children(&self) -> Children<'t, 'a> {
        self.children_impl(false)
    }

    fn children_impl(&self, trivia: bool) -> Children<'t, 'a> {
        let node = self.data();
        Children {
            tree: self.tree,
            next_token: node.first_token,
            token_end: node.token_end,
            next_child: self.idx + 1,
            child_end: node.subtree_end,
            trivia,
            is_root: self.idx == 0,
            started: false,
            trailing_done: false,
            prev_end: 0,
            gap: Gap::EMPTY,
            pending: None,
        }
    }

    /// Direct child nodes only.
    pub fn child_nodes(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + 't {
        self.significant_children()
            .filter_map(SyntaxElement::into_node)
    }

    /// Direct child tokens only, trivia included.
    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + 't {
        self.children().filter_map(SyntaxElement::into_token)
    }

    /// Direct significant child tokens only.
    pub fn significant_child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + 't {
        self.significant_children()
            .filter_map(SyntaxElement::into_token)
    }
}

impl<'t, 'a> SyntaxToken<'t, 'a> {
    /// This token's kind.
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Whether this is a zero-width parser recovery token.
    pub fn is_missing(&self) -> bool {
        self.kind.is_missing()
    }

    /// Whether this is a trivia token (whitespace, comment, or BOM).
    pub fn is_trivia(&self) -> bool {
        self.trivia
    }

    /// Return the flags for this token.
    pub fn flags(&self) -> NodeFlags {
        let mut flags = own_flag_bits(self.kind);
        if !self.trivia && self.tree.tokens[self.idx as usize].flags & TOKEN_ERROR != 0 {
            flags.insert(NodeFlags::HAS_ERROR);
        }
        flags
    }

    /// Return `true` when this token has a syntax error.
    pub fn has_error(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_ERROR)
    }

    /// The half-open byte range this token spans in the source.
    pub fn text_range(&self) -> TextRange {
        self.range
    }

    /// The raw source bytes of this token.
    pub fn text(&self) -> &'a [u8] {
        &self.tree.source[self.range.to_usize()]
    }

    /// The parent node. A trivia token's parent is the node that owns its gap.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = if self.trivia {
            self.tree.gap_owner(self.idx)
        } else {
            self.tree.parents().tokens[self.idx as usize]
        };
        (p != NO_PARENT).then_some(SyntaxNode {
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

    /// Return `true` for a synthetic recovery token.
    pub fn is_missing(&self) -> bool {
        matches!(self, Self::Token(token) if token.is_missing())
    }

    /// The half-open byte range this element spans in the source.
    pub fn text_range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(n) => n.text_range(),
            SyntaxElement::Token(t) => t.text_range(),
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

/// The part of a gap that is not yet lexed into trivia tokens.
#[derive(Clone, Copy)]
struct Gap {
    start: u32,
    end: u32,
    /// The index of the significant token after the gap.
    next: u32,
}

impl Gap {
    const EMPTY: Gap = Gap {
        start: 0,
        end: 0,
        next: 0,
    };

    #[inline]
    fn next_token<'t, 'a>(&mut self, tree: &'t SyntaxTree<'a>) -> Option<SyntaxToken<'t, 'a>> {
        if self.start >= self.end {
            return None;
        }
        let (kind, len) = lex_trivia(tree.source, self.start, self.end);
        let token = tree.trivia(kind, self.start, len, self.next);
        self.start += len;
        Some(token)
    }
}

/// Iterator over a node's direct children (see [`SyntaxNode::children`]).
pub struct Children<'t, 'a> {
    tree: &'t SyntaxTree<'a>,
    next_token: u32,
    token_end: u32,
    next_child: u32,
    child_end: u32,
    trivia: bool,
    is_root: bool,
    started: bool,
    trailing_done: bool,
    prev_end: u32,
    gap: Gap,
    pending: Option<SyntaxElement<'t, 'a>>,
}

impl<'t, 'a> Iterator for Children<'t, 'a> {
    type Item = SyntaxElement<'t, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.gap.next_token(self.tree) {
                return Some(SyntaxElement::Token(token));
            }
            if let Some(el) = self.pending.take() {
                return Some(el);
            }
            if self.next_token < self.token_end {
                let tree = self.tree;
                let first = self.next_token;
                let el = match tree.nodes.get(self.next_child as usize) {
                    Some(child)
                        if self.next_child < self.child_end && child.first_token == first =>
                    {
                        let el = SyntaxElement::Node(SyntaxNode {
                            tree,
                            idx: self.next_child,
                        });
                        self.next_token = child.token_end;
                        self.next_child = child.subtree_end;
                        el
                    }
                    _ => {
                        self.next_token += 1;
                        SyntaxElement::Token(tree.token(first))
                    }
                };
                if !self.trivia {
                    return Some(el);
                }
                // The gap before a child belongs to this node, unless the
                // child is the first one. The root also owns its leading gap.
                let start = tree.tokens[first as usize].start;
                if self.started || self.is_root {
                    self.gap = Gap {
                        start: self.prev_end,
                        end: start,
                        next: first,
                    };
                }
                self.started = true;
                self.prev_end = tree.tokens[self.next_token as usize - 1].end();
                self.pending = Some(el);
                continue;
            }
            if self.trivia && self.is_root && !self.trailing_done {
                // The root owns the gap after the last token.
                self.trailing_done = true;
                self.gap = Gap {
                    start: self.prev_end,
                    end: self.tree.source.len() as u32,
                    next: self.tree.tokens.len() as u32,
                };
                continue;
            }
            return None;
        }
    }
}

/// Iterator over every leaf token of a tree, trivia included (see
/// [`SyntaxTree::tokens`]).
pub struct Tokens<'t, 'a> {
    tree: &'t SyntaxTree<'a>,
    next: u32,
    prev_end: u32,
    gap: Gap,
    pending: Option<SyntaxToken<'t, 'a>>,
    trailing_done: bool,
}

impl<'t, 'a> Iterator for Tokens<'t, 'a> {
    type Item = SyntaxToken<'t, 'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.gap.next_token(self.tree) {
                return Some(token);
            }
            if let Some(token) = self.pending.take() {
                return Some(token);
            }
            let tree = self.tree;
            if (self.next as usize) < tree.tokens.len() {
                let index = self.next;
                self.next += 1;
                let token = tree.tokens[index as usize];
                self.gap = Gap {
                    start: self.prev_end,
                    end: token.start,
                    next: index,
                };
                self.prev_end = token.end();
                self.pending = Some(tree.token(index));
                continue;
            }
            if !self.trailing_done {
                self.trailing_done = true;
                self.gap = Gap {
                    start: self.prev_end,
                    end: tree.source.len() as u32,
                    next: self.next,
                };
                continue;
            }
            return None;
        }
    }
}

// ---------------------------------------------------------------------------
// Typed AST: "ungrammar-style" strongly-typed views over the cursors. Each
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
    /// A tagged block in value position, such as `rgb { 1 2 3 }` in
    /// `color = rgb { 1 2 3 }` ([`SyntaxKind::HeaderedBlock`]). In item
    /// position, the same text is a [`Field`] with no operator.
    HeaderedBlock => HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation ([`SyntaxKind::Calc`]).
    Calc => Calc,
    /// An EU4 conditional parameter block ([`SyntaxKind::Parameter`]).
    Parameter => Parameter,
    /// An EU4 undefined conditional parameter block
    /// ([`SyntaxKind::UndefinedParameter`]).
    UndefinedParameter => UndefinedParameter,
    /// An EU5 code payload ([`SyntaxKind::Code`]).
    Code => Code,
    /// A single-bracket interpolation ([`SyntaxKind::Interpolation`]).
    Interpolation => Interpolation,
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
    /// An EU4 conditional parameter block.
    Parameter(Parameter<'t, 'a>),
    /// An EU4 undefined conditional parameter block.
    UndefinedParameter(UndefinedParameter<'t, 'a>),
    /// An EU5 code payload.
    Code(Code<'t, 'a>),
    /// A single-bracket interpolation.
    Interpolation(Interpolation<'t, 'a>),
    /// A bare value: an array element or a loose value.
    Value(Value<'t, 'a>),
    /// A significant element that is neither a field nor a value — e.g. a stray
    /// operator/bracket or a [`SyntaxKind::Bogus`] node. Surfaced so the typed
    /// view drops nothing.
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
    /// An EU4 conditional parameter block.
    Parameter(Parameter<'t, 'a>),
    /// An EU4 undefined conditional parameter block.
    UndefinedParameter(UndefinedParameter<'t, 'a>),
    /// An EU5 code payload.
    Code(Code<'t, 'a>),
    /// A single-bracket interpolation.
    Interpolation(Interpolation<'t, 'a>),
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

    /// Decode the scalar value of this token into a string with `encoding`.
    /// The encoding removes the escape characters of a quoted string.
    ///
    /// ```
    /// use jomini::{Windows1252Encoding, text::syntax::parse};
    ///
    /// let tree = parse(b"name = \"Kr\xe4ftig \\\"Leo\\\"\"");
    /// let field = tree.ast().fields().next().unwrap();
    /// let value = field.value().unwrap();
    /// let decoded = value.decode(Windows1252Encoding::new()).unwrap();
    /// assert_eq!(decoded, "Kr\u{e4}ftig \"Leo\"");
    /// ```
    pub fn decode<E: Encoding>(&self, encoding: E) -> Cow<'a, str> {
        encoding.decode(self.as_scalar().as_bytes())
    }
}

/// First significant child element of `node` after skipping leading trivia and
/// (for blocks) the delimiting braces — used to classify entries.
fn child_items<'t, 'a>(node: SyntaxNode<'t, 'a>) -> impl Iterator<Item = Item<'t, 'a>> + 't {
    node.significant_children().filter_map(item_from_element)
}

fn item_from_element<'t, 'a>(el: SyntaxElement<'t, 'a>) -> Option<Item<'t, 'a>> {
    let kind = el.kind();
    if kind.is_trivia()
        || kind.is_missing()
        || matches!(kind, SyntaxKind::OpenBrace | SyntaxKind::CloseBrace)
    {
        return None;
    }
    match el {
        SyntaxElement::Node(n) => match n.kind() {
            SyntaxKind::Field => Field::cast(n).map(Item::Field),
            SyntaxKind::Parameter => Parameter::cast(n).map(Item::Parameter),
            SyntaxKind::UndefinedParameter => {
                UndefinedParameter::cast(n).map(Item::UndefinedParameter)
            }
            SyntaxKind::Code => Code::cast(n).map(Item::Code),
            SyntaxKind::Interpolation => Interpolation::cast(n).map(Item::Interpolation),
            _ => Value::cast_element(el)
                .map(Item::Value)
                .or(Some(Item::Other(el))),
        },
        _ => Value::cast_element(el)
            .map(Item::Value)
            .or(Some(Item::Other(el))),
    }
}

fn parameter_name<'t, 'a>(node: SyntaxNode<'t, 'a>) -> Option<SyntaxToken<'t, 'a>> {
    let mut open_brackets = 0;
    node.significant_children().find_map(|el| match el {
        SyntaxElement::Token(token) if token.kind() == SyntaxKind::OpenBracket => {
            open_brackets += 1;
            None
        }
        SyntaxElement::Token(token) if open_brackets >= 2 && token.kind().is_scalar() => {
            Some(token)
        }
        _ => None,
    })
}

fn parameter_items<'t, 'a>(node: SyntaxNode<'t, 'a>) -> impl Iterator<Item = Item<'t, 'a>> + 't {
    let mut header_done = false;
    let mut body_done = false;
    node.significant_children().filter_map(move |el| {
        if body_done {
            return None;
        }
        if !header_done {
            if el.kind() == SyntaxKind::CloseBracket {
                header_done = true;
            }
            return None;
        }
        if matches!(
            el.kind(),
            SyntaxKind::CloseBracket | SyntaxKind::MissingCloseBracket
        ) {
            body_done = true;
            return None;
        }
        parameter_body_item(el)
    })
}

fn parameter_body_item<'t, 'a>(el: SyntaxElement<'t, 'a>) -> Option<Item<'t, 'a>> {
    if el.kind().is_trivia() || el.kind().is_missing() || el.kind() == SyntaxKind::OpenBrace {
        return None;
    }
    match el {
        SyntaxElement::Node(n) => match n.kind() {
            SyntaxKind::Field => Field::cast(n).map(Item::Field),
            SyntaxKind::Parameter => Parameter::cast(n).map(Item::Parameter),
            SyntaxKind::UndefinedParameter => {
                UndefinedParameter::cast(n).map(Item::UndefinedParameter)
            }
            SyntaxKind::Code => Code::cast(n).map(Item::Code),
            SyntaxKind::Interpolation => Interpolation::cast(n).map(Item::Interpolation),
            _ => Value::cast_element(el)
                .map(Item::Value)
                .or(Some(Item::Other(el))),
        },
        _ => Value::cast_element(el)
            .map(Item::Value)
            .or(Some(Item::Other(el))),
    }
}

fn code_items<'t, 'a>(node: SyntaxNode<'t, 'a>) -> impl Iterator<Item = Item<'t, 'a>> + 't {
    let children: Vec<_> = node.significant_children().collect();
    let payload_start = children
        .windows(2)
        .position(|pair| {
            pair[0].kind() == SyntaxKind::OpenBracket && pair[1].kind() == SyntaxKind::OpenBracket
        })
        .map(|index| index + 2)
        .unwrap_or(children.len());
    let payload_end = children[payload_start..]
        .windows(2)
        .position(|pair| {
            pair[0].kind() == SyntaxKind::CloseBracket && pair[1].kind() == SyntaxKind::CloseBracket
        })
        .map(|index| payload_start + index)
        .unwrap_or(children.len());

    children
        .into_iter()
        .skip(payload_start)
        .take(payload_end.saturating_sub(payload_start))
        .filter_map(parameter_body_item)
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
        self.syntax()
            .significant_child_tokens()
            .find(|t| t.kind().is_scalar())
    }

    /// The operator token (`=`, `==`, `?=`, `<`, …), or `None` for a
    /// `key { ... }` field that has no operator.
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|t| t.kind() == SyntaxKind::Operator)
    }

    /// The concrete [`Operator`], re-derived from the operator token's text.
    pub fn op(&self) -> Option<Operator> {
        self.op_token().and_then(|t| t.operator())
    }

    /// The value (the right-hand side), or `None` if it is missing.
    ///
    /// A field with no operator (`key { ... }`) has its block as the value.
    pub fn value(&self) -> Option<Value<'t, 'a>> {
        let mut children = self.syntax().significant_children();
        children.next()?; // key
        let mut value = children.next()?;
        if value.kind() == SyntaxKind::Operator {
            value = children.next()?;
        }
        Value::cast_element(value)
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

    /// Return the physical closing brace, if the source contains one.
    pub fn close_brace(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|token| token.kind() == SyntaxKind::CloseBrace)
    }

    /// Return `true` when the block has a physical closing brace.
    pub fn is_closed_in_source(&self) -> bool {
        self.close_brace().is_some()
    }
}

impl<'t, 'a> Parameter<'t, 'a> {
    /// The parameter name in `[[name] ... ]`.
    pub fn name(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_name(self.syntax())
    }

    /// The entries in the parameter body.
    pub fn entries(&self) -> impl Iterator<Item = Item<'t, 'a>> + 't {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_field)
    }

    /// The bare values directly inside the parameter body.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_value)
    }

    /// Return the physical body closing bracket, if the source contains one.
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .filter(|token| token.kind() == SyntaxKind::CloseBracket)
            .skip(1)
            .last()
    }

    /// Return `true` when the parameter body has a physical closing bracket.
    pub fn is_closed_in_source(&self) -> bool {
        self.close_bracket().is_some()
    }
}

impl<'t, 'a> UndefinedParameter<'t, 'a> {
    /// The parameter name in `[[!name] ... ]`.
    pub fn name(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_name(self.syntax())
    }

    /// The entries in the parameter body.
    pub fn entries(&self) -> impl Iterator<Item = Item<'t, 'a>> + 't {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_field)
    }

    /// The bare values directly inside the parameter body.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_value)
    }

    /// Return the physical body closing bracket, if the source contains one.
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .filter(|token| token.kind() == SyntaxKind::CloseBracket)
            .skip(1)
            .last()
    }

    /// Return `true` when the parameter body has a physical closing bracket.
    pub fn is_closed_in_source(&self) -> bool {
        self.close_bracket().is_some()
    }
}

impl<'t, 'a> Code<'t, 'a> {
    /// The `code` keyword, when this node includes the no-equals form.
    pub fn keyword(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|token| token.kind() == SyntaxKind::Unquoted && token.text() == b"code")
    }

    /// The optional operator in `code = [[ ... ]]`.
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|token| token.kind() == SyntaxKind::Operator)
    }

    /// The entries in the code payload.
    pub fn entries(&self) -> impl Iterator<Item = Item<'t, 'a>> + 't {
        code_items(self.syntax())
    }

    /// The fields directly inside the code payload.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_field)
    }

    /// The bare values directly inside the code payload.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + 't {
        self.entries().filter_map(Item::into_value)
    }

    /// Return `true` when the payload has two physical closing brackets.
    pub fn is_closed_in_source(&self) -> bool {
        let closes: Vec<_> = self
            .syntax()
            .significant_child_tokens()
            .filter(|token| token.kind() == SyntaxKind::CloseBracket)
            .collect();
        closes
            .windows(2)
            .any(|pair| pair[0].text_range().end() == pair[1].text_range().start())
    }
}

impl<'t, 'a> Interpolation<'t, 'a> {
    /// The elements between the interpolation brackets.
    pub fn body(&self) -> impl Iterator<Item = SyntaxElement<'t, 'a>> + 't {
        self.syntax().children().filter(|element| {
            !matches!(
                element.kind(),
                SyntaxKind::OpenBracket
                    | SyntaxKind::CloseBracket
                    | SyntaxKind::MissingCloseBracket
            )
        })
    }

    /// Return `true` when the interpolation has a physical closing bracket.
    pub fn is_closed_in_source(&self) -> bool {
        self.syntax()
            .significant_child_tokens()
            .any(|token| token.kind() == SyntaxKind::CloseBracket)
    }
}

impl<'t, 'a> HeaderedBlock<'t, 'a> {
    /// The header scalar (e.g. `rgb`, `hsv`, a tag).
    pub fn header(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|t| t.kind().is_scalar())
    }

    /// The block that follows the header.
    pub fn block(&self) -> Option<Block<'t, 'a>> {
        self.syntax().child_nodes().find_map(Block::cast)
    }
}

impl<'t, 'a> Calc<'t, 'a> {
    /// The wrapped arithmetic expression (between `@[` and `]`).
    pub fn expr(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .find_map(Expr::cast_element)
    }
}

impl<'t, 'a> BinaryExpr<'t, 'a> {
    /// The left operand.
    pub fn lhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .find_map(Expr::cast_element)
    }

    /// The operator token (`+`, `-`, `*`, `/`).
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax().significant_child_tokens().find(|t| {
            matches!(
                t.kind(),
                SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::Star | SyntaxKind::Slash
            )
        })
    }

    /// The right operand.
    pub fn rhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .filter_map(Expr::cast_element)
            .nth(1)
    }
}

impl<'t, 'a> UnaryExpr<'t, 'a> {
    /// The prefix operator token (`-` or `+`).
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|t| matches!(t.kind(), SyntaxKind::Plus | SyntaxKind::Minus))
    }

    /// The operand the prefix applies to.
    pub fn operand(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .find_map(Expr::cast_element)
    }
}

impl<'t, 'a> ParenExpr<'t, 'a> {
    /// The expression between the parentheses.
    pub fn inner(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .find_map(Expr::cast_element)
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

    /// The [`Parameter`] if this entry is one.
    pub fn into_parameter(self) -> Option<Parameter<'t, 'a>> {
        match self {
            Item::Parameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`UndefinedParameter`] if this entry is one.
    pub fn into_undefined_parameter(self) -> Option<UndefinedParameter<'t, 'a>> {
        match self {
            Item::UndefinedParameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`Code`] if this entry is one.
    pub fn into_code(self) -> Option<Code<'t, 'a>> {
        match self {
            Item::Code(code) => Some(code),
            _ => None,
        }
    }

    /// The [`Interpolation`] if this entry is one.
    pub fn into_interpolation(self) -> Option<Interpolation<'t, 'a>> {
        match self {
            Item::Interpolation(interpolation) => Some(interpolation),
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
                SyntaxKind::Parameter => Parameter::cast(n).map(Value::Parameter),
                SyntaxKind::UndefinedParameter => {
                    UndefinedParameter::cast(n).map(Value::UndefinedParameter)
                }
                SyntaxKind::Code => Code::cast(n).map(Value::Code),
                SyntaxKind::Interpolation => Interpolation::cast(n).map(Value::Interpolation),
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

    /// The parameter, if this is [`Value::Parameter`].
    pub fn as_parameter(&self) -> Option<Parameter<'t, 'a>> {
        match self {
            Value::Parameter(parameter) => Some(*parameter),
            _ => None,
        }
    }

    /// The undefined parameter, if this is [`Value::UndefinedParameter`].
    pub fn as_undefined_parameter(&self) -> Option<UndefinedParameter<'t, 'a>> {
        match self {
            Value::UndefinedParameter(parameter) => Some(*parameter),
            _ => None,
        }
    }

    /// The code payload, if this is [`Value::Code`].
    pub fn as_code(&self) -> Option<Code<'t, 'a>> {
        match self {
            Value::Code(code) => Some(*code),
            _ => None,
        }
    }

    /// The interpolation, if this is [`Value::Interpolation`].
    pub fn as_interpolation(&self) -> Option<Interpolation<'t, 'a>> {
        match self {
            Value::Interpolation(interpolation) => Some(*interpolation),
            _ => None,
        }
    }

    /// Decode a scalar value into a string with `encoding` (see
    /// [`SyntaxToken::decode`]).
    pub fn decode<E: Encoding>(&self, encoding: E) -> Option<Cow<'a, str>> {
        match self {
            Value::Scalar(t) => Some(t.decode(encoding)),
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

impl<'a> SyntaxTree<'a> {
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
// Constructs the parser does not yet structure (`Bogus` recovery nodes and
// some calc internals) are reflowed conservatively: parameter, code, calc, and
// Bogus nodes are reprinted verbatim, and loose tokens land one per line.
// Content is always preserved; only the layout of those rare constructs is
// rough.
// ---------------------------------------------------------------------------

/// Configuration for [`SyntaxTree::format`].
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

impl<'a> SyntaxTree<'a> {
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

    /// Emit a node verbatim. Then set the "open" flags from the end of the
    /// node: an unclosed quote, or a comment in the trailing gap of a node that
    /// missing tokens close at EOF.
    fn emit_node_text(&mut self, node: SyntaxNode<'_, '_>) {
        self.push(node.text());
        let tree = node.tree;
        let data = node.data();
        let tokens = &tree.tokens[data.first_token as usize..data.token_end as usize];
        let Some(last) = tokens.iter().rev().find(|t| !t.kind.is_missing()) else {
            return;
        };
        let mut gap = Gap {
            start: last.end(),
            end: node.text_range().end(),
            next: 0,
        };
        let mut last_trivia = None;
        while let Some(token) = gap.next_token(tree) {
            last_trivia = Some(token.kind());
        }
        match last_trivia {
            Some(kind) => self.comment_open = kind == SyntaxKind::Comment,
            None => {
                self.quote_open = last.kind == SyntaxKind::Quoted
                    && !quote_is_closed(&tree.source[last.range().to_usize()]);
            }
        }
    }

    /// Emit a newline (two for a preserved blank line), reopening the line.
    fn line_break(&mut self, blank: bool) {
        if self.out.last() != Some(&b'\n') {
            self.out.push(b'\n');
        }
        if blank && !self.out.ends_with(b"\n\n") {
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
                kind if kind.is_missing() => {}
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
                _ => self.emit_node_text(n),
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
                kind if kind.is_missing() => {}
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
            .filter(|el| !el.kind().is_trivia() && !el.kind().is_missing())
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

    /// Assert the lexer output and the leaf iterator tile the input: the
    /// significant tokens are in order and do not overlap, every gap is only
    /// trivia, and the leaves are contiguous spans covering `[0, len)`.
    fn assert_tiles(data: &[u8], flavor: Flavor) {
        let lexed = lex(data, flavor);
        let mut at = 0u32;
        for t in &lexed.tokens {
            assert!(t.start >= at, "overlap at {} in {:?}", at, data);
            let mut gap = at;
            while gap < t.start {
                let (kind, len) = lex_trivia(data, gap, t.start);
                assert!(kind.is_trivia());
                gap += len;
            }
            at = t.end();
        }
        let tree = parse_with(data, flavor);
        let mut at = 0u32;
        for t in tree.tokens() {
            assert_eq!(
                t.text_range().start(),
                at,
                "gap/overlap at {} in {:?}",
                at,
                data
            );
            at = t.text_range().end();
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
        let toks = lex(src, Flavor::default()).tokens;
        let quoted: Vec<_> = toks
            .iter()
            .filter(|t| t.kind == SyntaxKind::Quoted)
            .collect();
        assert_eq!(quoted.len(), 1);
        assert_eq!(quoted[0].len as usize, src.len() - 2); // `"ab\ncd"`

        // HoI4: the string ends at the newline.
        let toks = lex(src, Flavor::hoi4()).tokens;
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
        let toks = lex(b"@foo", Flavor::hoi4()).tokens;
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].kind, SyntaxKind::Unquoted);

        let toks = lex(b"@foo", Flavor::default()).tokens;
        assert_eq!(toks[0].kind, SyntaxKind::Variable);
    }

    #[test]
    fn diagnostics_unclosed_block() {
        let tree = parse(b"a = { b");
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message().contains("unclosed"))
        );
        assert_eq!(tree.reconstruct(), b"a = { b"); // still lossless
    }

    #[test]
    fn missing_block_is_zero_width_and_repairable() {
        let source = b"a = { b";
        let tree = parse(source);
        let error = tree
            .errors()
            .iter()
            .find(|error| error.recovery.is_some())
            .unwrap();
        let recovery = error.recovery.as_ref().unwrap();
        assert_eq!(recovery.expected, SyntaxKind::CloseBrace);
        assert_eq!(recovery.missing, SyntaxKind::MissingCloseBrace);
        assert_eq!(recovery.opening_range, TextRange::new(4, 5));
        assert_eq!(
            recovery.insertion_range,
            TextRange::empty(source.len() as u32)
        );

        let missing = tree.tokens().find(|token| token.is_missing()).unwrap();
        assert_eq!(missing.kind(), SyntaxKind::MissingCloseBrace);
        assert_eq!(missing.text(), b"");
        assert_eq!(missing.text_range(), TextRange::empty(source.len() as u32));
        assert!(missing.has_error());
        assert_eq!(missing.parent().unwrap().kind(), SyntaxKind::Block);
        assert!(missing.parent().unwrap().has_error());
        assert!(tree.root().has_error());

        let block = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        assert!(!block.is_closed_in_source());
        assert!(block.close_brace().is_none());
        assert_eq!(
            tree.repair_fixes(),
            [Repair {
                range: TextRange::empty(7),
                replacement: "}".into()
            }]
        );
        assert_eq!(tree.repair(), b"a = { b}");
        assert!(parse(&tree.repair()).errors().is_empty());
        assert_eq!(tree.reconstruct(), source);
        assert!(!format(source).contains(&b'}'));
    }

    #[test]
    fn nested_repairs_are_inside_out_and_keep_siblings_clean() {
        let source = b"outer = { clean = {} bad = { value";
        let tree = parse(source);
        let missing: Vec<_> = tree
            .tokens()
            .filter(|token| token.is_missing())
            .map(|token| token.kind())
            .collect();
        assert_eq!(
            missing,
            [SyntaxKind::MissingCloseBrace, SyntaxKind::MissingCloseBrace]
        );
        assert_eq!(tree.repair_fixes()[0].replacement, "}}".to_string());
        assert_eq!(tree.repair(), b"outer = { clean = {} bad = { value}}");
        assert!(parse(&tree.repair()).errors().is_empty());

        let outer = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        let blocks: Vec<_> = outer
            .syntax()
            .child_nodes()
            .filter(|node| node.kind() == SyntaxKind::Field)
            .filter_map(|field| Field::cast(field)?.value()?.as_block())
            .collect();
        assert_eq!(blocks.len(), 2);
        assert!(!blocks[0].syntax().has_error());
        assert!(blocks[1].syntax().has_error());
    }

    #[test]
    fn recovery_refuses_mismatches_and_interrupted_constructs() {
        for source in [b"a = { b ]".as_slice(), b"a = { b = @[ (1]".as_slice()] {
            let tree = parse(source);
            assert!(
                tree.errors()
                    .iter()
                    .any(|error| error.message().contains("unclosed"))
            );
            assert!(
                tree.repair_fixes().is_empty(),
                "unexpected fix for {source:?}"
            );
            assert!(tree.errors().iter().all(|error| {
                error.recovery.as_ref().is_none_or(|recovery| {
                    !tree.repair_applicability(recovery).is_machine_applicable()
                })
            }));
        }

        let mismatched_block = parse(b"a = { b ]");
        let block = mismatched_block
            .ast()
            .fields()
            .next()
            .and_then(|field| field.value())
            .and_then(|value| value.as_block())
            .unwrap();
        assert!(block.syntax().has_error());

        let interrupted_calc = parse(b"a = { b = @[1 }");
        let calc = interrupted_calc
            .ast()
            .fields()
            .next()
            .and_then(|field| field.value())
            .and_then(|value| value.as_block())
            .and_then(|block| block.fields().next())
            .and_then(|field| field.value())
            .and_then(|value| value.as_calc())
            .unwrap();
        assert!(calc.syntax().has_error());
        assert!(
            interrupted_calc
                .errors()
                .iter()
                .any(|error| error.message().contains("unclosed '@['"))
        );
        assert!(interrupted_calc.repair_fixes().is_empty());

        let ambiguous_bracket = parse(b"a = { [");
        assert!(ambiguous_bracket.repair_fixes().is_empty());
    }

    #[test]
    fn trailing_comments_and_line_endings_get_safe_repairs() {
        let cases = [
            (b"a = { x # tail".as_slice(), b"\n}".as_slice()),
            (b"a = { x # tail\n".as_slice(), b"}".as_slice()),
            (b"a = {\r\n x # tail".as_slice(), b"\r\n}".as_slice()),
            (b"a = { x\r\n".as_slice(), b"}".as_slice()),
        ];
        for (source, replacement) in cases {
            let tree = parse(source);
            assert_eq!(tree.repair_fixes()[0].replacement.as_bytes(), replacement);
            assert!(
                parse(&tree.repair()).errors().is_empty(),
                "source={source:?}"
            );
        }

        for source in [
            b"a = { \"unterminated".as_slice(),
            b"a = { \"escaped\\\"".as_slice(),
        ] {
            let tree = parse(source);
            assert!(tree.repair_fixes().is_empty());
            assert!(tree.errors().iter().any(|error| {
                error.recovery.as_ref().is_some_and(|recovery| {
                    tree.repair_applicability(recovery) == Applicability::Unsafe
                })
            }));
        }
    }

    #[test]
    fn incomplete_parameters_and_code_payloads_preserve_typed_content() {
        let parameter = parse(b"[[name] value");
        let parameter_node = parameter
            .ast()
            .items()
            .find_map(|item| match item {
                Item::Parameter(value) => Some(value),
                _ => None,
            })
            .unwrap();
        assert_eq!(parameter_node.values().count(), 1);
        assert!(parameter_node.close_bracket().is_none());
        assert_eq!(parameter.repair_fixes()[0].replacement, "]");
        assert!(parse(&parameter.repair()).errors().is_empty());

        let header = parse(b"[[name");
        assert!(
            header
                .ast()
                .items()
                .any(|item| matches!(item, Item::Parameter(_)))
        );
        assert_eq!(header.repair_fixes()[0].replacement, "]]".to_string());
        assert!(parse(&header.repair()).errors().is_empty());

        let one = parse(b"code [[value]");
        assert_eq!(one.repair_fixes()[0].replacement, "]");
        assert_eq!(one.tokens().filter(|token| token.is_missing()).count(), 1);
        assert!(parse(&one.repair()).errors().is_empty());

        let both = parse(b"code [[value");
        assert_eq!(both.repair_fixes()[0].replacement, "]]".to_string());
        assert_eq!(both.tokens().filter(|token| token.is_missing()).count(), 2);
        assert!(parse(&both.repair()).errors().is_empty());
    }

    #[test]
    fn nested_calc_repairs_include_parentheses_and_calc_close() {
        let source = b"a = { x = @[ (1";
        let tree = parse(source);
        let kinds: Vec<_> = tree
            .tokens()
            .filter(|token| token.is_missing())
            .map(|token| token.kind())
            .collect();
        assert_eq!(
            kinds,
            [
                SyntaxKind::MissingCloseParen,
                SyntaxKind::MissingCalcClose,
                SyntaxKind::MissingCloseBrace
            ]
        );
        assert_eq!(tree.repair_fixes()[0].replacement, ")]}");
        assert!(parse(&tree.repair()).errors().is_empty());
        assert_eq!(tree.reconstruct(), source);
    }

    #[test]
    fn interpolation_recovery_is_typed_and_lossless() {
        let source = b"value = [ROOT.GetName";
        let tree = parse(source);
        let interpolation = tree
            .ast()
            .fields()
            .next()
            .and_then(|field| field.value())
            .and_then(|value| value.as_interpolation())
            .unwrap();

        assert!(interpolation.syntax().has_error());
        assert!(!interpolation.is_closed_in_source());
        assert_eq!(interpolation.syntax().text(), b"[ROOT.GetName");
        assert!(
            interpolation
                .body()
                .any(|element| element.kind() == SyntaxKind::Unquoted)
        );
        assert_eq!(tree.repair_fixes()[0].replacement, "]");
        assert_eq!(tree.repair(), b"value = [ROOT.GetName]");
        assert!(parse(&tree.repair()).errors().is_empty());
    }

    #[test]
    fn diagnostics_unmatched_brace() {
        let tree = parse(b"x } y");
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.message().contains("unmatched"))
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
    fn bang_equal_splits_from_a_scalar() {
        let tree = parse(b"a!=1");
        let field = tree.ast().fields().next().unwrap();
        assert_eq!(field.key().unwrap().text(), b"a");
        assert_eq!(field.op(), Some(Operator::NotEqual));
        assert_eq!(tree.reconstruct(), b"a!=1");
    }

    #[test]
    fn lone_open_bracket_has_a_diagnostic() {
        for source in [&b"desc = ["[..], b"desc = [ ", b"a = { desc = [ }"] {
            let tree = parse(source);
            let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
            assert_eq!(kinds, vec![SyntaxErrorKind::UnexpectedOpenBracket]);
            assert!(tree.errors()[0].recovery.is_none());
            assert!(tree.repair_fixes().is_empty());
            assert_eq!(tree.reconstruct(), source);
        }

        // A `[` in a parameter body is an ordinary bracket.
        assert!(parse(b"[[x] a = [ ] ]").errors().is_empty());
    }

    #[test]
    fn close_bracket_key_is_a_field() {
        let source = b"active_idea_groups = { ]=0 defensive_ideas=2 }";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        let field = tree.ast().fields().next().unwrap();
        let block = field.value().unwrap().as_block().unwrap();
        let keys: Vec<_> = block
            .fields()
            .map(|f| f.key().unwrap().text().to_vec())
            .collect();
        assert_eq!(keys, vec![b"]".to_vec(), b"defensive_ideas".to_vec()]);
        assert_eq!(tree.reconstruct(), source);
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
                .any(|e| e.message().contains("maximum nesting depth"))
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
                .any(|e| e.message().contains("calc nesting"))
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
                .any(|e| e.message().contains("calc nesting"))
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
                .any(|e| e.message().contains("calc nesting"))
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
    fn ast_parameter_nodes_preserve_body() {
        let source = b"generate_advisor = { [[scaled_skill] a = { b = c } ] }";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let block = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        let entries = block.entries().collect::<Vec<_>>();
        let [Item::Parameter(parameter)] = entries.as_slice() else {
            panic!("expected one parameter entry");
        };
        assert_eq!(parameter.syntax().kind(), SyntaxKind::Parameter);
        assert_eq!(
            parameter.syntax().text(),
            b"[[scaled_skill] a = { b = c } ]"
        );
        assert_eq!(parameter.name().unwrap().text(), b"scaled_skill");
        assert_eq!(parameter.fields().count(), 1);
        assert_eq!(
            parameter.fields().next().unwrap().key().unwrap().text(),
            b"a"
        );
    }

    #[test]
    fn ast_undefined_parameter_node_is_distinct() {
        let source = b"[[!scaled_skill] a = b ]";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let entries = tree.ast().items().collect::<Vec<_>>();
        let [Item::UndefinedParameter(parameter)] = entries.as_slice() else {
            panic!("expected one undefined parameter entry");
        };
        assert_eq!(parameter.syntax().kind(), SyntaxKind::UndefinedParameter);
        assert_eq!(parameter.name().unwrap().text(), b"scaled_skill");
        assert_eq!(
            parameter.fields().next().unwrap().key().unwrap().text(),
            b"a"
        );
    }

    #[test]
    fn ast_empty_parameter_node_is_valid() {
        let source = b"[[dip_reward]\n]";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let entries = tree.ast().items().collect::<Vec<_>>();
        let [Item::Parameter(parameter)] = entries.as_slice() else {
            panic!("expected one empty parameter entry");
        };
        assert_eq!(parameter.name().unwrap().text(), b"dip_reward");
        assert_eq!(parameter.entries().count(), 0);
    }

    #[test]
    fn interpolation_nodes_are_distinct_from_parameters() {
        let source = b"text = [PdxAccount.GetLastLinkErrorLocalized]\nname = [ROOT.GetName]\n";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let fields = tree.ast().fields().collect::<Vec<_>>();
        let first = fields[0].value().unwrap().as_interpolation().unwrap();
        assert_eq!(first.syntax().kind(), SyntaxKind::Interpolation);
        assert_eq!(
            first.syntax().text(),
            b"[PdxAccount.GetLastLinkErrorLocalized]"
        );
        assert_eq!(first.body().count(), 1);

        let second = fields[1].value().unwrap().as_interpolation().unwrap();
        assert_eq!(second.syntax().text(), b"[ROOT.GetName]");
    }

    #[test]
    fn eu5_code_payload_is_not_a_parameter() {
        let source = b"template example { code [[\n\tvalue = { name = wall }\n]] }";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let block = tree
            .ast()
            .items()
            .find_map(|item| match item {
                // `example { ... }` is a field with no operator.
                Item::Field(field) => field.value()?.as_block(),
                _ => None,
            })
            .unwrap();
        let entries = block.entries().collect::<Vec<_>>();
        let [Item::Code(code)] = entries.as_slice() else {
            panic!("expected one code payload");
        };
        assert_eq!(code.syntax().kind(), SyntaxKind::Code);
        assert_eq!(code.keyword().unwrap().text(), b"code");
        assert_eq!(
            code.syntax().text(),
            b"code [[\n\tvalue = { name = wall }\n]]"
        );
        assert_eq!(
            code.fields().next().unwrap().key().unwrap().text(),
            b"value"
        );
        assert!(
            code.fields()
                .next()
                .unwrap()
                .value()
                .unwrap()
                .as_block()
                .is_some()
        );

        let compact = parse(b"template example { code = [[name] body ]] }");
        assert!(compact.errors().is_empty());
        let compact_block = compact
            .ast()
            .items()
            .find_map(|item| match item {
                // `example { ... }` is a field with no operator.
                Item::Field(field) => field.value()?.as_block(),
                _ => None,
            })
            .unwrap();
        let code = compact_block.fields().next().unwrap();
        assert_eq!(code.key().unwrap().text(), b"code");
        assert_eq!(code.op(), Some(Operator::Equal));
        let payload = code.value().unwrap().as_code().unwrap();
        assert!(payload.keyword().is_none());
        assert!(payload.op_token().is_none());
        assert_eq!(payload.fields().count(), 0);
    }

    #[test]
    fn eu5_code_payload_allows_empty_bodies() {
        for source in [b"code [[]]".as_slice(), b"code [[ ]]".as_slice()] {
            let tree = parse(source);
            assert!(tree.errors().is_empty());
            assert_eq!(tree.reconstruct(), source);
            let entries = tree.ast().items().collect::<Vec<_>>();
            let [Item::Code(code)] = entries.as_slice() else {
                panic!("expected a code payload");
            };
            assert_eq!(code.entries().count(), 0);
        }

        for source in [b"code = [[]]".as_slice(), b"code = [[ ]]".as_slice()] {
            let tree = parse(source);
            assert!(tree.errors().is_empty());
            assert_eq!(tree.reconstruct(), source);
            let field = tree.ast().fields().next().unwrap();
            assert_eq!(
                field.value().unwrap().as_code().unwrap().entries().count(),
                0
            );
        }
    }

    #[test]
    fn eu5_code_payload_handles_nested_braces_and_brackets() {
        let source = b"code [[ value = { nested = { link = [ROOT.GetName] } } ]]";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);

        let entries = tree.ast().items().collect::<Vec<_>>();
        let [Item::Code(code)] = entries.as_slice() else {
            panic!("expected a code payload");
        };
        let nested = code
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        let interpolation = nested
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_interpolation()
            .unwrap();
        assert_eq!(interpolation.syntax().text(), b"[ROOT.GetName]");
    }

    #[test]
    fn eu5_code_payload_recovery_keeps_following_block_delimiter() {
        let source = b"outer = { code [[value = yes }";
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        assert!(
            tree.errors()
                .iter()
                .any(|error| error.message().contains("unclosed code payload"))
        );
        let outer = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        assert!(
            outer
                .syntax()
                .child_tokens()
                .any(|token| token.kind() == SyntaxKind::CloseBrace)
        );
    }

    #[test]
    fn eu5_code_payload_recovery_handles_eof() {
        let sources = [
            b"code [[value = yes".as_slice(),
            b"code = [[value = yes".as_slice(),
        ];
        for source in sources {
            let tree = parse(source);
            assert_eq!(tree.reconstruct(), source);
            assert!(
                tree.errors()
                    .iter()
                    .any(|error| error.message().contains("unclosed code payload"))
            );
        }
    }

    #[test]
    fn deeply_nested_code_payloads_do_not_overflow() {
        let depth = MAX_DEPTH as usize + 16;
        let mut source = Vec::new();
        for _ in 0..depth {
            source.extend_from_slice(b"code [[");
        }
        for _ in 0..depth {
            source.extend_from_slice(b"]]");
        }

        let tree = parse(&source);
        assert_eq!(tree.reconstruct(), source);
        assert!(tree.errors().iter().any(|error| {
            error
                .message()
                .contains("maximum code nesting depth exceeded")
        }));
    }

    #[test]
    fn parameter_recovery_keeps_following_block_delimiter() {
        let source = b"outer = { [[name] value = yes }";
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        assert!(
            tree.errors()
                .iter()
                .any(|error| error.message().contains("unclosed parameter block"))
        );
        let outer = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        assert!(
            outer
                .syntax()
                .child_tokens()
                .any(|token| token.kind() == SyntaxKind::CloseBrace)
        );
    }

    #[test]
    fn parameter_body_can_continue_a_brace_across_parameters() {
        let source = b"outer = { [[tooltip] tooltip = { ] [[tooltip] } ] }";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);
        let outer = tree
            .ast()
            .fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .unwrap();
        assert_eq!(
            outer
                .entries()
                .filter(|entry| matches!(entry, Item::Parameter(_)))
                .count(),
            2
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
            .filter(|t| {
                !t.kind().is_missing()
                    && !matches!(t.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment)
            })
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

    #[test]
    fn fmt_interrupted_calc_keeps_physical_tokens() {
        let source = b"@[{";
        let formatted = format(source);
        assert_eq!(significant(source), significant(&formatted));
        assert_eq!(format(&formatted), formatted);
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
            assert_eq!(t.text_range().start(), at);
            at = t.text_range().end();
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
        // Virtual missing delimiters also set HAS_ERROR through their ancestors.
        let stray = parse(b"x } y");
        assert!(stray.root().has_error(), "the Bogus node sets HAS_ERROR");
        assert!(parse(b"a = { b").root().has_error());
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
                .any(|e| e.message().contains("unclosed '@['")),
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
        assert_eq!(tree.repair_fixes()[0].replacement, "]");
        assert!(calc.has_error());
        assert!(parse(&tree.repair()).errors().is_empty());
    }

    #[test]
    fn calc_combined_tree_depth_is_bounded() {
        let mut nested = String::from("a=@[");
        nested.push_str(&"(".repeat(200));
        nested.push('1');
        for _ in 0..200 {
            nested.push_str(&"+1".repeat(200));
            nested.push(')');
        }
        nested.push(']');
        for source in [nested.as_bytes(), &nested.as_bytes()[..nested.len() - 1]] {
            let tree = parse(source);
            assert!(
                tree.errors()
                    .iter()
                    .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
            );
            assert!(tree.root().has_error());
            assert_eq!(tree.reconstruct(), source);
            assert!(tree.repair_fixes().is_empty());
            let mut stack = vec![(tree.root(), 0)];
            while let Some((node, depth)) = stack.pop() {
                assert!(depth <= MAX_DEPTH + 3);
                stack.extend(node.child_nodes().map(|child| (child, depth + 1)));
            }
        }

        let source = format!("a=@[{}1{}]", "(".repeat(100), "+1)".repeat(100));
        let tree = parse(source.as_bytes());
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source.as_bytes());
    }

    #[test]
    fn calc_missing_parens_before_close_have_diagnostics() {
        for source in [b"a=@[(1]".as_slice(), b"a=@[((1]", b"a={b=@[(1}"] {
            let tree = parse(source);
            assert!(
                tree.errors()
                    .iter()
                    .any(|e| e.kind == SyntaxErrorKind::UnclosedCalcParen)
            );
            assert!(tree.errors().iter().all(|e| e.recovery.is_none()));
            assert!(tree.root().has_error());
            assert!(tree.repair_fixes().is_empty());
            assert_eq!(tree.reconstruct(), source);
            let mut stack = vec![tree.root()];
            while let Some(node) = stack.pop() {
                if node.kind() == SyntaxKind::ParenExpr {
                    assert!(node.has_error());
                }
                stack.extend(node.child_nodes());
            }
        }
    }

    #[test]
    fn calc_recovery_tokens_keep_their_emitted_parents() {
        for source in [b"a=@[(1 2".as_slice(), b"a=@[((1 2", b"a={b=@[(1 2 "] {
            let tree = parse(source);
            assert_eq!(tree.reconstruct(), source);
            let mut previous_end = 0;
            for token in tree.significant_tokens() {
                assert!(token.text_range().start() >= previous_end);
                previous_end = token.text_range().end();
                if token.kind() == SyntaxKind::MissingCloseParen {
                    let parent = token.parent().unwrap();
                    assert_eq!(parent.kind(), SyntaxKind::ParenExpr);
                    assert!(parent.has_error());
                    assert!(parent.child_tokens().any(|child| child.idx == token.idx));
                }
                if token.text() == b"2" {
                    assert_eq!(token.parent().unwrap().kind(), SyntaxKind::Calc);
                }
            }
            let calc = if source.starts_with(b"a={") {
                tree.ast()
                    .fields()
                    .next()
                    .unwrap()
                    .value()
                    .unwrap()
                    .as_block()
                    .unwrap()
                    .fields()
                    .next()
                    .unwrap()
                    .value()
                    .unwrap()
                    .as_calc()
                    .unwrap()
            } else {
                tree.ast()
                    .fields()
                    .next()
                    .unwrap()
                    .value()
                    .unwrap()
                    .as_calc()
                    .unwrap()
            };
            let Some(Expr::Paren(paren)) = calc.expr() else {
                panic!("expected a parenthesized expression");
            };
            assert!(
                paren
                    .syntax()
                    .significant_child_tokens()
                    .any(|token| token.kind() == SyntaxKind::MissingCloseParen)
            );
        }
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

    /// A byte-at-a-time lexer with the same rules as [`lex`]. It checks the
    /// SIMD scans, which handle 16 bytes at a time.
    fn reference_lex(source: &[u8], flavor: Flavor) -> Vec<(SyntaxKind, u32, u32, u8)> {
        let n = source.len();
        let mut i = if source.starts_with(BOM) { 3 } else { 0 };
        let mut flags = 0u8;
        let mut out = Vec::new();
        let push = |out: &mut Vec<_>, kind, start: usize, end: usize, flags: &mut u8| {
            out.push((kind, start as u32, end as u32, *flags));
            *flags = 0;
        };
        while i < n {
            let b = source[i];
            let start = i;
            if is_ws(b) {
                while i < n && is_ws(source[i]) {
                    i += 1;
                }
                continue;
            }
            if b == b'#' {
                while i < n && source[i] != b'\n' && source[i] != b'\r' {
                    i += 1;
                }
                flags = TOKEN_COMMENT_BEFORE;
                continue;
            }
            if flavor.variables && b == b'@' && source.get(i + 1) == Some(&b'[') {
                push(&mut out, SyntaxKind::CalcOpen, i, i + 2, &mut flags);
                i += 2;
                while i < n {
                    let c = source[i];
                    let start = i;
                    let kind = match c {
                        b']' => {
                            i += 1;
                            push(&mut out, SyntaxKind::CalcClose, start, i, &mut flags);
                            break;
                        }
                        b'{' | b'}' => break,
                        c if is_ws(c) => {
                            i += 1;
                            continue;
                        }
                        b'(' => SyntaxKind::OpenParen,
                        b')' => SyntaxKind::CloseParen,
                        b'+' => SyntaxKind::Plus,
                        b'-' => SyntaxKind::Minus,
                        b'*' => SyntaxKind::Star,
                        b'/' => SyntaxKind::Slash,
                        c if c.is_ascii_digit()
                            || (c == b'.' && source.get(i + 1).is_some_and(u8::is_ascii_digit)) =>
                        {
                            i = scan_number(source, i);
                            push(&mut out, SyntaxKind::Number, start, i, &mut flags);
                            continue;
                        }
                        _ => {
                            i += 1;
                            while i < n && !is_calc_stop(source[i]) {
                                i += 1;
                            }
                            push(&mut out, SyntaxKind::CalcIdent, start, i, &mut flags);
                            continue;
                        }
                    };
                    i += 1;
                    push(&mut out, kind, start, i, &mut flags);
                }
                continue;
            }
            let kind = match b {
                b'"' => {
                    i += 1;
                    while i < n {
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
                    SyntaxKind::Quoted
                }
                b'{' | b'}' | b'[' | b']' => {
                    i += 1;
                    match b {
                        b'{' => SyntaxKind::OpenBrace,
                        b'}' => SyntaxKind::CloseBrace,
                        b'[' => SyntaxKind::OpenBracket,
                        _ => SyntaxKind::CloseBracket,
                    }
                }
                b'=' | b'<' | b'>' => {
                    i += 1;
                    if source.get(i) == Some(&b'=') {
                        i += 1;
                    }
                    SyntaxKind::Operator
                }
                b'!' | b'?' if source.get(i + 1) == Some(&b'=') => {
                    i += 2;
                    SyntaxKind::Operator
                }
                b'!' => {
                    i += 1;
                    SyntaxKind::Bang
                }
                _ => {
                    i += 1;
                    while i < n && !STOP[source[i] as usize] {
                        i += 1;
                    }
                    if source.get(i) == Some(&b'=') && i - start > 1 && source[i - 1] == b'!' {
                        i -= 1;
                    }
                    let run = &source[start..i];
                    if flavor.variables && run.len() > 1 && run[0] == b'@' && run[1] != b'@' {
                        SyntaxKind::Variable
                    } else if flavor.macros && run.iter().filter(|&&b| b == b'$').count() >= 2 {
                        SyntaxKind::MacroParam
                    } else {
                        SyntaxKind::Unquoted
                    }
                }
            };
            push(&mut out, kind, start, i, &mut flags);
        }
        out
    }

    /// Compare the dispatched lexer and the portable fallback lexer (the path
    /// for targets without SIMD) with [`reference_lex`].
    fn lex_matches_reference(source: &[u8]) -> bool {
        let simplify = |tokens: &[Token]| -> Vec<_> {
            tokens
                .iter()
                .map(|t| (t.kind, t.start, t.end(), t.flags))
                .collect()
        };
        [Flavor::default(), Flavor::hoi4()]
            .into_iter()
            .all(|flavor| {
                let expected = reference_lex(source, flavor);
                let lexed = lex(source, flavor);
                let mut fallback = Vec::new();
                let fallback_comment = lex_simd(
                    fearless_simd::Fallback::new(),
                    source,
                    flavor,
                    &mut fallback,
                );
                simplify(&lexed.tokens) == expected
                    && simplify(&fallback) == expected
                    && fallback_comment == lexed.trailing_comment
            })
    }

    #[quickcheck]
    fn prop_lex_matches_reference(data: Vec<u8>) -> bool {
        lex_matches_reference(&data)
    }

    /// Map random bytes onto the bytes that the lexer treats specially, so
    /// that the SIMD masks see every class at every lane position.
    #[quickcheck]
    fn prop_lex_matches_reference_alphabet(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"ab01._-$@#\"\\ \t\r\n\x0b\x0c;{}[]=<>!?()+*/|:\x80\xff";
        let mapped: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        lex_matches_reference(&mapped)
    }

    #[test]
    fn lex_scans_across_vector_boundaries() {
        for len in 0..48 {
            let word = "x".repeat(len);
            let cases = [
                format!("{word}={word}"),
                format!("\"{word}\\\"{word}\""),
                format!("#{word}\n{word}"),
                format!("{}{word}", " ".repeat(len)),
                format!("$a$_{word}"),
                format!("{word}$a$ {word}"),
                format!("\"{word}\n{word}"),
            ];
            for case in &cases {
                assert!(lex_matches_reference(case.as_bytes()), "{case:?}");
                rt(case.as_bytes());
            }
        }
    }

    /// The scan that [`interpolation_starts`] replaces: from one `[`, count
    /// brackets until the match, and stop at a `}` directly inside it.
    fn naive_interpolation_start(tokens: &[Token], pos: usize) -> bool {
        let mut depth = 1u32;
        for token in &tokens[pos + 1..] {
            match token.kind {
                SyntaxKind::OpenBracket => depth += 1,
                SyntaxKind::CloseBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return true;
                    }
                }
                SyntaxKind::CloseBrace if depth == 1 => return false,
                _ => {}
            }
        }
        pos + 1 < tokens.len()
    }

    #[quickcheck]
    fn prop_interpolation_starts_match_naive_scan(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"[]{} a";
        let mapped: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        let tokens = lex(&mapped, Flavor::default()).tokens;
        let starts = interpolation_starts(&tokens);
        tokens.iter().enumerate().all(|(pos, token)| {
            token.kind != SyntaxKind::OpenBracket
                || starts.binary_search(&(pos as u32)).is_ok()
                    == naive_interpolation_start(&tokens, pos)
        })
    }

    #[test]
    fn scalar_followed_by_block_is_a_field_without_operator() {
        let tree = parse(b"foo{bar=qux} color = { rgb { 1 2 3 } }");
        assert!(tree.errors().is_empty());
        let fields: Vec<_> = tree.ast().fields().collect();
        assert_eq!(fields.len(), 2);

        assert_eq!(fields[0].key().unwrap().text(), b"foo");
        assert!(fields[0].op_token().is_none());
        assert!(fields[0].op().is_none());
        let block = fields[0].value().unwrap().as_block().unwrap();
        assert_eq!(block.fields().next().unwrap().key().unwrap().text(), b"bar");

        // In item position, a header such as `rgb` is a field too. The
        // semantic layer decides that it is a header.
        let inner = fields[1].value().unwrap().as_block().unwrap();
        let rgb = inner.fields().next().unwrap();
        assert_eq!(rgb.key().unwrap().text(), b"rgb");
        let values = rgb.value().unwrap().as_block().unwrap().values().count();
        assert_eq!(values, 3);

        // In value position, a header is still a headered block.
        let tree = parse(b"color = rgb { 1 2 3 }");
        let value = tree.ast().fields().next().unwrap().value().unwrap();
        assert!(value.as_headered().is_some());
    }

    #[test]
    fn macro_parameters_glue_to_the_surrounding_scalar() {
        for (source, kind) in [
            (b"$a$_b".as_slice(), SyntaxKind::MacroParam),
            (b"foo_$a$", SyntaxKind::MacroParam),
            (b"$a$_$b$_c", SyntaxKind::MacroParam),
            (b"$$", SyntaxKind::MacroParam),
            (b"$5", SyntaxKind::Unquoted),
            (b"a$b", SyntaxKind::Unquoted),
        ] {
            let tokens = lex(source, Flavor::default()).tokens;
            assert_eq!(tokens.len(), 1, "{source:?}");
            assert_eq!(tokens[0].kind, kind, "{source:?}");
        }

        let tree = parse(b"k = $a$_b");
        let field = tree.ast().fields().next().unwrap();
        let value = field.value().unwrap().as_scalar().unwrap();
        assert_eq!(value.as_bytes(), b"$a$_b");
        assert!(tree.root().contains_macro());
        assert_eq!(tree.root().child_nodes().count(), 1);

        // HoI4 has no macros.
        let tokens = lex(b"$a$_b", Flavor::hoi4()).tokens;
        assert_eq!(tokens[0].kind, SyntaxKind::Unquoted);
    }

    #[test]
    fn trivia_belongs_to_the_nearest_common_ancestor() {
        let source = b"# lead\na # inside\n= { b }  # tail\n";
        let tree = parse(source);
        let comments: Vec<_> = tree
            .tokens()
            .filter(|t| t.kind() == SyntaxKind::Comment)
            .collect();
        assert_eq!(comments.len(), 3);
        assert!(comments.iter().all(|c| c.is_trivia()));
        assert_eq!(comments[0].parent().unwrap().kind(), SyntaxKind::Root);
        assert_eq!(comments[1].parent().unwrap().kind(), SyntaxKind::Field);
        assert_eq!(comments[2].parent().unwrap().kind(), SyntaxKind::Root);

        // The space after `{` belongs to the block.
        let space = tree
            .tokens()
            .find(|t| t.kind() == SyntaxKind::Whitespace && t.text_range().start() == 21)
            .unwrap();
        assert_eq!(space.parent().unwrap().kind(), SyntaxKind::Block);

        let field = tree.root().child_nodes().next().unwrap();
        assert!(field.has_comment());
        assert!(tree.root().has_comment());
        let block = field.child_nodes().next().unwrap();
        assert!(!block.has_comment());

        // The node's children hold the same trivia as the cursor parents.
        let field_comments = field
            .child_tokens()
            .filter(|t| t.kind() == SyntaxKind::Comment)
            .count();
        assert_eq!(field_comments, 1);
        assert_eq!(field.text(), b"a # inside\n= { b }");
        assert_eq!(field.significant_children().count(), 3);
    }

    #[test]
    fn every_leaf_parent_holds_the_leaf() {
        let source = b"# c\na = { b = @[ (1 + x) * 2 ] [[p] c = d ] } e { f } g\n";
        let tree = parse(source);
        fn walk(node: SyntaxNode<'_, '_>) {
            for child in node.children() {
                match child {
                    SyntaxElement::Token(token) => {
                        assert_eq!(token.parent().unwrap().text_range(), node.text_range());
                        assert_eq!(token.parent().unwrap().kind(), node.kind());
                    }
                    SyntaxElement::Node(child) => {
                        assert_eq!(child.parent().unwrap().text_range(), node.text_range());
                        walk(child);
                    }
                }
            }
        }
        walk(tree.root());
        assert_eq!(tree.reconstruct(), source);
    }

    #[test]
    fn decode_removes_quotes_and_escapes() {
        let tree = parse(b"a = \"x \\\"y\\\"\" b = \xe4");
        let values: Vec<_> = tree
            .ast()
            .fields()
            .map(|f| {
                f.value()
                    .unwrap()
                    .decode(crate::Windows1252Encoding::new())
                    .unwrap()
            })
            .collect();
        assert_eq!(values, ["x \"y\"", "\u{e4}"]);
    }

    #[test]
    fn repair_validation_is_lazy_and_cached() {
        let tree = parse(b"a = { b");
        assert!(tree.repairs_valid.get().is_none());
        let recovery = tree.errors()[0].recovery.as_ref().unwrap();
        assert!(tree.repair_applicability(recovery).is_machine_applicable());
        assert_eq!(tree.repairs_valid.get(), Some(&true));
    }
}
