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
//! layout is similar to the one that Carbon uses, but Carbon keeps its nodes in
//! post-order. Trivia is approximately 40% of all leaves in a save file, so the
//! tree does not pay to store it.
//!
//! Offsets are absolute [`TextSize`] values, and ranges are [`TextRange`]
//! values. Parent links are calculated only when a cursor asks for them. The
//! [`SyntaxNode`]/[`SyntaxToken`] cursors are small `Copy` handles. Because the
//! nodes of a subtree are adjacent in pre-order, [`SyntaxNode::descendants`],
//! [`SyntaxNode::preorder`], and the sibling queries do not allocate.
//! [`SyntaxTree::token_at_offset`] and [`SyntaxTree::covering_element`] find
//! the element at a position with a binary search.
//!
//! The tree holds only offsets, not the source. A [`Parse`] holds the tree
//! and the source, and the parser does not copy the source. [`parse`] borrows
//! the bytes, so the text of each token has the lifetime of the source and can
//! outlive the `Parse`. [`Parse::owned`] keeps an [`OwnedSource`] such as a
//! `Vec<u8>` or an `Arc<[u8]>`. The [`SyntaxTree`] view and the cursors have
//! two lifetimes: `'t` for the tree and `'a` for the source.
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
//! # Examples
//!
//! Read the values of a file with the typed AST:
//!
//! ```
//! use jomini::text::{Operator, lossless::parse};
//!
//! let source = b"
//! # Brittany
//! capital = 169
//! is_city = yes
//! discovered_by = { western eastern }
//! trigger = { num_of_cities >= 5 }
//! ";
//! let parse = parse(source);
//! assert!(parse.errors().is_empty());
//!
//! let root = parse.ast();
//! let field = |key: &[u8]| {
//!     root.fields()
//!         .find(|f| f.key().as_scalar().as_bytes() == key)
//!         .unwrap()
//! };
//!
//! assert_eq!(field(b"capital").value().unwrap().to_u64(), Some(169));
//! assert_eq!(field(b"is_city").value().unwrap().to_bool(), Some(true));
//!
//! let discovered_by = field(b"discovered_by").value().unwrap().as_block().unwrap();
//! assert_eq!(discovered_by.values().count(), 2);
//!
//! let trigger = field(b"trigger").value().unwrap().as_block().unwrap();
//! let condition = trigger.fields().next().unwrap();
//! assert_eq!(condition.op(), Some(Operator::GreaterThanEqual));
//! ```
//!
//! Change one value and keep all of the other bytes. The comments, the
//! whitespace, and the order of the fields do not change, because each token
//! knows its byte range in the source:
//!
//! ```
//! use jomini::text::lossless::{parse, Value};
//!
//! let source = b"# Set by the history file\nbase_tax = 3 # was 2\nbase_production = 2\n";
//! let parse = parse(source);
//! let field = parse
//!     .ast()
//!     .fields()
//!     .find(|f| f.key().as_scalar().as_bytes() == b"base_tax")
//!     .unwrap();
//! let Some(Value::Scalar(token)) = field.value() else {
//!     panic!("expected a scalar");
//! };
//!
//! let mut edited = source.to_vec();
//! edited.splice(token.text_range().to_range(), *b"5");
//! assert_eq!(
//!     edited,
//!     b"# Set by the history file\nbase_tax = 5 # was 2\nbase_production = 2\n"
//! );
//! ```
//!
//! # Debugging
//!
//! [`SyntaxTree::debug_tree`] shows the tree with the kind and the byte range
//! of each node and token. The `lossless` example prints it for a file or for
//! stdin, and it can also print the tokens, the formatted file, and the
//! syntax errors at their line and column:
//!
//! ```sh
//! printf 'building = { cost = 100 } # wood\n' | cargo run --features lossless --example lossless -- --tree --no-trivia
//! cargo run --features lossless --example lossless -- --tokens --no-trivia FILE
//! cargo run --features lossless --example lossless -- --help
//! ```
//!
//! [`TextTape`]: crate::TextTape

#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::{Encoding, Scalar, text::Operator};
use fearless_simd::{Level, Simd, dispatch, mask8x16, prelude::*, u8x16};
use std::borrow::Cow;
use std::fmt;
use std::sync::OnceLock;

mod line_index;
mod tape;
pub use line_index::{LineCol, LineIndex};
pub use tape::{parse_tape, parse_tape_with};

/// The kind of every node (interior) and token (leaf) in a [`SyntaxTree`].
///
/// The tree does not store the trivia kinds ([`Bom`](Self::Bom),
/// [`Whitespace`](Self::Whitespace), and [`Comment`](Self::Comment)). The
/// cursors lex them again from the gaps between the stored tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
#[non_exhaustive]
pub enum SyntaxKind {
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
    /// A synthetic, zero-width `}` inserted after an unclosed block at EOF.
    MissingCloseBrace,
    /// A synthetic, zero-width `]` inserted after an unclosed bracket construct.
    /// Code payloads use two consecutive tokens because their `]]` delimiter
    /// consists of two source tokens.
    MissingCloseBracket,

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

    /// Return `true` for whitespace, a comment, or the BOM. These tokens have
    /// no effect on the structure, but the tree keeps their bytes.
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

/// A byte offset in the source.
///
/// The offset is a `u32`, so a source must be smaller than 4 GiB. Use
/// [`TextSize::to_usize`] or `usize::from` to index the source.
///
/// ```
/// use jomini::text::lossless::TextSize;
///
/// let start = TextSize::new(4);
/// let end = start + TextSize::of(b"name");
/// assert_eq!(u32::from(end), 8);
/// assert_eq!(end.to_string(), "8");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct TextSize(u32);

impl TextSize {
    /// Make an offset from a raw `u32`.
    #[inline]
    pub const fn new(offset: u32) -> Self {
        TextSize(offset)
    }

    /// The length of `bytes` as an offset.
    ///
    /// # Panics
    ///
    /// Panics if `bytes` is 4 GiB or larger.
    #[inline]
    pub fn of(bytes: &[u8]) -> Self {
        match u32::try_from(bytes.len()) {
            Ok(len) => TextSize(len),
            Err(_) => panic!("text is larger than 4 GiB"),
        }
    }

    /// The offset as a `u32`.
    #[inline]
    pub const fn to_u32(self) -> u32 {
        self.0
    }

    /// The offset as a `usize`, to index the source.
    #[inline]
    pub const fn to_usize(self) -> usize {
        self.0 as usize
    }

    /// Add `other`, or return `None` on overflow.
    #[inline]
    pub const fn checked_add(self, other: TextSize) -> Option<TextSize> {
        match self.0.checked_add(other.0) {
            Some(sum) => Some(TextSize(sum)),
            None => None,
        }
    }

    /// Subtract `other`, or return `None` when `other` is larger.
    #[inline]
    pub const fn checked_sub(self, other: TextSize) -> Option<TextSize> {
        match self.0.checked_sub(other.0) {
            Some(diff) => Some(TextSize(diff)),
            None => None,
        }
    }
}

impl fmt::Display for TextSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<u32> for TextSize {
    #[inline]
    fn from(offset: u32) -> Self {
        TextSize(offset)
    }
}

impl From<TextSize> for u32 {
    #[inline]
    fn from(size: TextSize) -> Self {
        size.0
    }
}

impl From<TextSize> for usize {
    #[inline]
    fn from(size: TextSize) -> Self {
        size.to_usize()
    }
}

impl TryFrom<usize> for TextSize {
    type Error = std::num::TryFromIntError;

    #[inline]
    fn try_from(offset: usize) -> Result<Self, Self::Error> {
        u32::try_from(offset).map(TextSize)
    }
}

impl std::ops::Add for TextSize {
    type Output = TextSize;

    /// # Panics
    ///
    /// Panics on overflow when debug assertions are on, the same as `u32`.
    #[inline]
    fn add(self, rhs: TextSize) -> TextSize {
        TextSize(self.0 + rhs.0)
    }
}

impl std::ops::Sub for TextSize {
    type Output = TextSize;

    #[inline]
    fn sub(self, rhs: TextSize) -> TextSize {
        TextSize(self.0 - rhs.0)
    }
}

impl std::ops::AddAssign for TextSize {
    #[inline]
    fn add_assign(&mut self, rhs: TextSize) {
        self.0 += rhs.0;
    }
}

impl std::ops::SubAssign for TextSize {
    #[inline]
    fn sub_assign(&mut self, rhs: TextSize) {
        self.0 -= rhs.0;
    }
}

/// A half-open byte range `start..end` in the source.
///
/// Index the source with the range to get its bytes:
///
/// ```
/// use jomini::text::lossless::{parse, TextRange, TextSize};
///
/// let source = b"name = Jean";
/// let parse = parse(source);
/// let key = parse.ast().fields().next().unwrap().key();
/// let range = key.text_range();
/// assert_eq!(range, TextRange::new(TextSize::new(0), TextSize::new(4)));
/// assert_eq!(&source[range], b"name");
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
pub struct TextRange {
    start: u32,
    end: u32,
}

impl TextRange {
    /// Make a range.
    ///
    /// # Panics
    ///
    /// Panics if `start` is greater than `end`.
    #[inline]
    pub const fn new(start: TextSize, end: TextSize) -> Self {
        assert!(start.0 <= end.0, "range start is greater than range end");
        TextRange {
            start: start.0,
            end: end.0,
        }
    }

    /// Make a range of `len` bytes at `offset`.
    #[inline]
    pub const fn at(offset: TextSize, len: TextSize) -> Self {
        TextRange::new(offset, TextSize(offset.0 + len.0))
    }

    /// Make a zero-width range at `offset`.
    #[inline]
    pub const fn empty(offset: TextSize) -> Self {
        TextRange {
            start: offset.0,
            end: offset.0,
        }
    }

    /// Make a range from raw offsets. The caller makes sure that
    /// `start <= end`.
    #[inline]
    pub(crate) const fn span(start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        TextRange { start, end }
    }

    /// The first byte offset of the range.
    #[inline]
    pub const fn start(self) -> TextSize {
        TextSize(self.start)
    }

    /// The byte offset after the last byte of the range.
    #[inline]
    pub const fn end(self) -> TextSize {
        TextSize(self.end)
    }

    /// The number of bytes in the range.
    #[inline]
    pub const fn len(self) -> TextSize {
        TextSize(self.end - self.start)
    }

    /// Return `true` when the range has no bytes.
    #[inline]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Return `true` when `offset` is in `start..end`.
    #[inline]
    pub const fn contains(self, offset: TextSize) -> bool {
        self.start <= offset.0 && offset.0 < self.end
    }

    /// Return `true` when `offset` is in `start..=end`.
    #[inline]
    pub const fn contains_inclusive(self, offset: TextSize) -> bool {
        self.start <= offset.0 && offset.0 <= self.end
    }

    /// Return `true` when `other` is fully inside this range.
    #[inline]
    pub const fn contains_range(self, other: TextRange) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    /// The smallest range that contains this range and `other`.
    ///
    /// ```
    /// use jomini::text::lossless::{TextRange, TextSize};
    ///
    /// let a = TextRange::at(TextSize::new(2), TextSize::new(3));
    /// let b = TextRange::empty(TextSize::new(9));
    /// assert_eq!(a.cover(b).to_string(), "2..9");
    /// ```
    #[inline]
    pub const fn cover(self, other: TextRange) -> TextRange {
        let start = if self.start < other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end > other.end {
            self.end
        } else {
            other.end
        };
        TextRange { start, end }
    }

    /// The bytes that are in both ranges, or `None` when the ranges have no
    /// common offset. Two ranges that only touch give an empty range.
    #[inline]
    pub const fn intersect(self, other: TextRange) -> Option<TextRange> {
        let start = if self.start > other.start {
            self.start
        } else {
            other.start
        };
        let end = if self.end < other.end {
            self.end
        } else {
            other.end
        };
        if start <= end {
            Some(TextRange { start, end })
        } else {
            None
        }
    }

    /// The range as a `usize` range, to index the source.
    #[inline]
    pub const fn to_range(self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}

impl From<TextRange> for std::ops::Range<usize> {
    #[inline]
    fn from(range: TextRange) -> Self {
        range.to_range()
    }
}

impl std::ops::Index<TextRange> for [u8] {
    type Output = [u8];

    #[inline]
    fn index(&self, range: TextRange) -> &[u8] {
        &self[range.to_range()]
    }
}

impl std::ops::Index<TextRange> for str {
    type Output = str;

    #[inline]
    fn index(&self, range: TextRange) -> &str {
        &self[range.to_range()]
    }
}

/// Write the range as `start..end`.
impl fmt::Display for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// Write the range as `start..end`, the same as [`Display`](fmt::Display).
impl fmt::Debug for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Game-specific lexer configuration.
///
/// [`Flavor::default`] is the most permissive superset and is correct for every
/// game except where a genuine lexer fork exists. [`Flavor::hoi4`] enables
/// HoI4's newline-terminated strings and treats `@`/`$` as ordinary identifier
/// bytes (HoI4 has no reader variables or macros).
///
/// In HoI4, a newline closes a quoted string that has no closing quote. The
/// default flavor reads such a string to the end of the source:
///
/// ```
/// use jomini::text::lossless::{parse, parse_with, Flavor};
///
/// let source = b"name = \"Paris\nowner = FRA";
///
/// let hoi4 = parse_with(&source[..], Flavor::hoi4());
/// assert!(hoi4.errors().is_empty());
/// assert_eq!(hoi4.ast().fields().count(), 2);
///
/// let default = parse(&source[..]);
/// assert_eq!(default.errors().len(), 1);
/// assert_eq!(default.ast().fields().count(), 1);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
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
        Flavor::DEFAULT
    }
}

impl Flavor {
    /// The permissive superset. This is the same as [`Flavor::default`].
    pub const DEFAULT: Flavor = Flavor {
        newline_terminated_strings: false,
        variables: true,
        macros: true,
    };

    /// Hearts of Iron IV: newline-terminated strings, no `@vars`/`$macros$`.
    pub const fn hoi4() -> Self {
        Flavor {
            newline_terminated_strings: true,
            variables: false,
            macros: false,
        }
    }

    /// Set [`newline_terminated_strings`](Flavor::newline_terminated_strings).
    ///
    /// The struct is `#[non_exhaustive]`, so other crates cannot use the
    /// struct update syntax. Use these methods to change one field:
    ///
    /// ```
    /// use jomini::text::lossless::Flavor;
    ///
    /// const FLAVOR: Flavor = Flavor::DEFAULT.with_macros(false);
    /// assert!(!FLAVOR.macros);
    /// assert!(FLAVOR.variables);
    /// ```
    #[must_use]
    pub const fn with_newline_terminated_strings(mut self, value: bool) -> Self {
        self.newline_terminated_strings = value;
        self
    }

    /// Set [`variables`](Flavor::variables).
    #[must_use]
    pub const fn with_variables(mut self, value: bool) -> Self {
        self.variables = value;
        self
    }

    /// Set [`macros`](Flavor::macros).
    #[must_use]
    pub const fn with_macros(mut self, value: bool) -> Self {
        self.macros = value;
        self
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
        TextRange::span(self.start, self.end())
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
/// (braces, brackets, operators, comment) or are whitespace. Notably `"`, `@`,
/// `$`, `!`, `?`, `:`, `.`, `-`, `/`, `|` and high bytes are *not* here, so they
/// are absorbed mid-identifier and only dispatch specially as a first byte.
///
/// A `"` in a run is part of the scalar, as in `TextTape`. Game files contain
/// typos such as `{ name" }`. If such a quote started a string, the string
/// would contain the `}` and change the structure of the remainder of the file.
static STOP: [bool; 256] = {
    let mut table = [false; 256];
    let mut i = 0;
    while i < 256 {
        table[i] = matches!(
            CLASS[i],
            CLASS_WS
                | CLASS_COMMENT
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

/// A resumable lexer. Each [`Lexer::fill`] call appends the next tokens, so
/// the parser can read a small window of tokens instead of a full buffer.
struct Lexer {
    level: Level,
    flavor: Flavor,
    /// The byte offset where lexing continues.
    offset: usize,
    /// The flags for the next token.
    flags: u8,
    /// All the source is lexed.
    done: bool,
    /// The opening quote of a quoted string that has no closing quote.
    unclosed_quote: Option<TextRange>,
}

const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

impl Lexer {
    fn new(source: &[u8], flavor: Flavor) -> Self {
        Lexer {
            level: Level::new(),
            flavor,
            offset: if source.starts_with(BOM) { 3 } else { 0 },
            flags: 0,
            done: source.is_empty(),
            unclosed_quote: None,
        }
    }

    /// Make a lexer that starts at the byte `offset`. A token or the trivia
    /// before a token must start at `offset`.
    fn at(source: &[u8], flavor: Flavor, offset: usize) -> Self {
        Lexer {
            level: Level::new(),
            flavor,
            offset,
            flags: 0,
            done: offset >= source.len(),
            unclosed_quote: None,
        }
    }

    /// Append at least `min` tokens to `out`, or all the remaining tokens.
    #[inline(never)]
    fn fill(&mut self, source: &[u8], out: &mut Vec<Token>, min: usize) {
        if self.done {
            return;
        }
        let limit = out.len().saturating_add(min);
        let (offset, flags) = dispatch!(self.level, simd => lex_simd(
            simd,
            source,
            self.flavor,
            (self.offset, self.flags),
            limit,
            out,
        ));
        self.offset = offset;
        self.flags = flags;
        self.done = offset >= source.len();

        // Only the last token can be a quoted string without a closing quote,
        // because such a string continues to the end of the source.
        if self.done
            && let Some(t) = out.last()
            && t.kind == SyntaxKind::Quoted
            && t.flags & TOKEN_ERROR != 0
        {
            self.unclosed_quote = Some(TextRange::span(t.start, t.start + 1));
        }
    }

    /// Return `true` when the gap after the last token contains a comment.
    /// The value is correct only when the lexer is done.
    fn trailing_comment(&self) -> bool {
        self.flags & TOKEN_COMMENT_BEFORE != 0
    }
}

/// All the tokens of a source.
#[cfg(test)]
struct Lexed {
    tokens: Vec<Token>,
    /// The gap after the last token contains a comment.
    trailing_comment: bool,
}

#[cfg(test)]
fn lex(source: &[u8], flavor: Flavor) -> Lexed {
    let mut lexer = Lexer::new(source, flavor);
    let mut tokens = Vec::new();
    lexer.fill(source, &mut tokens, usize::MAX);
    Lexed {
        tokens,
        trailing_comment: lexer.trailing_comment(),
    }
}

/// Lex `source` from the state `(offset, flags)` into `out` until `out` has
/// `limit` tokens or the source ends. Return the state after the last token.
///
/// The lexer pushes only significant tokens, so the bytes between two tokens
/// are always trivia. It accepts all input: each byte that matches no other
/// rule starts an unquoted run, and each branch moves forward one or more
/// bytes.
#[inline(always)]
fn lex_simd<S: Simd>(
    simd: S,
    source: &[u8],
    flavor: Flavor,
    (mut i, mut flags): (usize, u8),
    limit: usize,
    out: &mut Vec<Token>,
) -> (usize, u8) {
    let n = source.len();

    while i < n && out.len() < limit {
        let mut b = source[i];
        if is_ws(b) {
            // Most tokens follow whitespace. Skip it here, so that it does
            // not need a separate dispatch.
            i = skip_whitespace(simd, source, i + 1);
            if i >= n {
                break;
            }
            b = source[i];
        }
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
                if i == n && !quote_is_closed(&source[start..]) {
                    flags |= TOKEN_ERROR;
                }
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
                // The first byte is not a stop byte, so the scan can include
                // it in the `$` check.
                let (mut end, dollar) = scan_unquoted(simd, source, i);
                if source[end - 1] == b'!' && end - start > 1 && end < n && source[end] == b'=' {
                    // `a!=b` is `a`, `!=`, `b`, as in `TextTape`.
                    end -= 1;
                }
                i = end;
                let kind = classify_run(&source[start..i], dollar, flavor);
                // Most keys have the form `key = `. Lex the operator here,
                // because a branch is easier to predict than the jump table.
                if let Some(&[b' ', b'=', after]) = source.get(i..i + 3)
                    && is_ws(after)
                {
                    out.push(Token {
                        start: start as u32,
                        len: (i - start) as u32,
                        kind,
                        flags,
                    });
                    out.push(Token {
                        start: i as u32 + 1,
                        len: 1,
                        kind: SyntaxKind::Operator,
                        flags: 0,
                    });
                    flags = 0;
                    i += 2;
                    continue;
                }
                kind
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

    (i, flags)
}

/// Classify an unquoted run. `dollar` is `true` when the run contains a `$`.
#[inline(always)]
fn classify_run(run: &[u8], dollar: bool, flavor: Flavor) -> SyntaxKind {
    if let [b'@', second, ..] = run
        && *second != b'@'
        && flavor.variables
    {
        SyntaxKind::Variable
    } else if dollar && flavor.macros && has_macro(run) {
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

/// The run classes of a byte come from two tables: one for the low nibble and
/// one for the high nibble. The class of a byte is the bitwise AND of the two
/// values. Each low bit is one group of [`STOP`] bytes:
///
/// - `0x01`: `\t` `\n` `\v` `\f` `\r` (high nibble 0, low nibble 9 to D).
/// - `0x02`: ` ` `#` (high nibble 2, low nibble 0 or 3).
/// - `0x04`: `;` `<` `=` `>` (high nibble 3, low nibble B to E).
/// - `0x08`: `[` `]` `{` `}` (high nibble 5 or 7, low nibble B or D).
///
/// The class of `$` is [`RUN_CLASS_DOLLAR`] alone.
const RUN_CLASS_STOP: u8 = 0x0F;
const RUN_CLASS_DOLLAR: u8 = 0x80;

static RUN_LOW_NIBBLE: [u8; 16] = [
    0x02, 0, 0, 0x02, 0x80, 0, 0, 0, 0, 0x01, 0x01, 0x0D, 0x05, 0x0D, 0x04, 0,
];

static RUN_HIGH_NIBBLE: [u8; 16] = [
    0x01, 0, 0x82, 0x04, 0, 0x08, 0, 0x08, 0, 0, 0, 0, 0, 0, 0, 0,
];

/// Return the lane bits of the bytes in [`STOP`] and of the `$` bytes.
#[inline(always)]
fn run_lanes<S: Simd>(simd: S, v: u8x16<S>) -> (u64, u64) {
    let low = u8x16::from_slice(simd, &RUN_LOW_NIBBLE).swizzle_dyn(v & 0x0Fu8);
    let high = u8x16::from_slice(simd, &RUN_HIGH_NIBBLE).swizzle_dyn(v >> 4);
    let class = low & high;
    let stops = !(class & RUN_CLASS_STOP).simd_eq(0u8).to_bitmask() & 0xFFFF;
    let dollars = class.simd_eq(RUN_CLASS_DOLLAR).to_bitmask();
    (stops, dollars)
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
    while i + 16 <= source.len() {
        let (stops, dollars) = run_lanes(simd, load(simd, source, i));
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
    // Most whitespace runs are one byte. Check that byte first.
    if i < source.len() && !is_ws(source[i]) {
        return i;
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
    'quoted: loop {
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
                match source[i] {
                    b'\\' => {
                        i = (i + 2).min(n);
                        continue 'quoted;
                    }
                    b'"' => return i + 1,
                    _ => return i,
                }
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
            // An operand name, as in `tier`, `leopard_x` or `@var`. In
            // incorrect input, this is any other byte that is not a stop
            // byte. This branch always consumes one or more bytes.
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

/// The kind of a [`SyntaxError`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum SyntaxErrorKind {
    UnmatchedCloseBrace,
    /// A `]` that closes no parameter, code payload, or interpolation.
    UnmatchedCloseBracket,
    UnclosedBlock,
    UnclosedInterpolation,
    UnclosedParameterHeader,
    UnclosedParameter,
    UnclosedCode,
    CodeMissingFirstBracket,
    CodeMissingSecondBracket,
    UnclosedCalc,
    UnclosedCalcParen,
    BlockDepthExceeded,
    ParameterDepthExceeded,
    CodeDepthExceeded,
    CalcDepthExceeded,
    /// A `[` that starts no parameter, code payload, or interpolation.
    UnexpectedOpenBracket,
    /// A quoted string that has no closing quote. The string continues to
    /// the end of the source.
    UnclosedQuote,
}

impl SyntaxErrorKind {
    /// A human-readable description.
    pub fn message(self) -> &'static str {
        match self {
            Self::UnmatchedCloseBrace => "unmatched '}'",
            Self::UnmatchedCloseBracket => "unmatched ']'",
            Self::UnclosedBlock => "unclosed '{'",
            Self::UnclosedInterpolation => "unclosed bracket interpolation",
            Self::UnclosedParameterHeader => "unclosed parameter header",
            Self::UnclosedParameter => "unclosed parameter block",
            Self::UnclosedCode => "unclosed code payload",
            Self::CodeMissingFirstBracket => "unclosed code payload (missing first ']')",
            Self::CodeMissingSecondBracket => "unclosed code payload (missing second ']')",
            Self::UnclosedCalc => "unclosed '@['",
            Self::UnclosedCalcParen => "unclosed parenthesized calculation",
            Self::BlockDepthExceeded => "maximum nesting depth exceeded; structure flattened",
            Self::ParameterDepthExceeded => {
                "maximum parameter nesting depth exceeded; structure flattened"
            }
            Self::CodeDepthExceeded => "maximum code nesting depth exceeded; structure flattened",
            Self::CalcDepthExceeded => "maximum calc nesting depth exceeded; structure flattened",
            Self::UnexpectedOpenBracket => "'[' starts no construct",
            Self::UnclosedQuote => "unclosed '\"'",
        }
    }
}

impl fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

/// A non-fatal problem found while parsing. Parsing never fails — the tree is
/// always lossless — but recoverable issues are collected here for tooling.
///
/// The parser continues after an error, so one parse finds all of the
/// problems in a file, and the fields after an error are still available:
///
/// ```
/// use jomini::text::lossless::{parse, SyntaxErrorKind};
///
/// let parse = parse(b"a = { b = 1 } } c = 2");
/// let error = &parse.errors()[0];
/// assert_eq!(error.kind, SyntaxErrorKind::UnmatchedCloseBrace);
/// assert_eq!(error.to_string(), "unmatched '}' at 14..15");
/// assert_eq!(parse.ast().fields().count(), 2);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
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

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message(), self.range)
    }
}

impl std::error::Error for SyntaxError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum RecoveryConstruct {
    Block,
    Interpolation,
    Parameter,
    Calc,
    Code,
    CalcParen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Applicability {
    MachineApplicable,
    MaybeIncorrect,
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
    /// The position of this repair among the repairs at the same offset.
    /// [`SyntaxTree::repairs`] joins them into the repair of position 0.
    pub order: u32,
    /// The parser's local decision, before the reparse check.
    local_applicability: Applicability,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct Repair {
    pub range: TextRange,
    pub replacement: String,
}

/// Summary bits for a subtree, one byte for each node. The parser sets the
/// bits when it builds the tree.
///
/// The flags of a node contain the bits of its kind and the bits of *all* its
/// descendants. Thus one O(1) test tells if a block has a comment, a syntax
/// error, a calc, or a macro parameter. You do not have to walk the subtree.
/// swift-syntax (`RecursiveRawSyntaxFlags`) and Carbon (`has_error`) use the
/// same method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct NodeFlags(u8);

impl NodeFlags {
    /// No bits.
    pub const EMPTY: NodeFlags = NodeFlags(0);
    /// The subtree contains at least one [`SyntaxKind::Comment`].
    pub const HAS_COMMENT: NodeFlags = NodeFlags(1 << 0);
    /// The subtree contains a syntax error or recovery element.
    pub const HAS_ERROR: NodeFlags = NodeFlags(1 << 1);
    /// The subtree contains a `@[ ... ]` [`SyntaxKind::Calc`].
    pub const HAS_CALC: NodeFlags = NodeFlags(1 << 2);
    /// The subtree contains a `$macro$` [`SyntaxKind::MacroParam`].
    pub const HAS_MACRO: NodeFlags = NodeFlags(1 << 3);

    /// Whether every bit set in `other` is also set in `self`.
    pub const fn contains(self, other: NodeFlags) -> bool {
        self.0 & other.0 == other.0
    }

    /// Whether at least one bit set in `other` is also set in `self`.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, NodeFlags};
    ///
    /// let parse = parse(b"a = { b = $B$ }");
    /// let flags = parse.root().flags();
    /// assert!(flags.intersects(NodeFlags::HAS_CALC | NodeFlags::HAS_MACRO));
    /// assert!(!flags.contains(NodeFlags::HAS_CALC | NodeFlags::HAS_MACRO));
    /// ```
    pub const fn intersects(self, other: NodeFlags) -> bool {
        self.0 & other.0 != 0
    }

    /// Whether no bit is set.
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    fn insert(&mut self, other: NodeFlags) {
        self.0 |= other.0;
    }
}

impl std::ops::BitOr for NodeFlags {
    type Output = NodeFlags;

    fn bitor(self, rhs: NodeFlags) -> NodeFlags {
        NodeFlags(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for NodeFlags {
    fn bitor_assign(&mut self, rhs: NodeFlags) {
        self.insert(rhs);
    }
}

/// The flag bits a single element contributes on its own, before the upward
/// union of its descendants' bits. The match has no guards, so the compiler
/// can make it a lookup table.
#[inline]
fn own_flag_bits(kind: SyntaxKind) -> NodeFlags {
    match kind {
        SyntaxKind::Comment => NodeFlags::HAS_COMMENT,
        SyntaxKind::Bogus
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
#[derive(Clone)]
struct Parents {
    nodes: Vec<u32>,
    tokens: Vec<u32>,
}

const NO_PARENT: u32 = u32::MAX;

/// The parse result without the source. It holds only byte offsets, so it
/// has no lifetime.
#[derive(Clone)]
struct TreeData {
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

/// A source and its lossless syntax tree.
///
/// The source type `S` decides who owns the bytes, and the parser does not
/// copy them:
///
/// - [`parse`] borrows the bytes and gives a `Parse<&[u8]>`. The text of its
///   tokens has the lifetime of the source, so the text can outlive the
///   `Parse`.
/// - [`Parse::owned`] takes an [`OwnedSource`], such as a `Vec<u8>`, an
///   `Arc<[u8]>`, or a different buffer type in [`Owned`]. You can keep such
///   a `Parse` in a struct, a cache, or a map,
///   or send it to a different thread. The text of its tokens borrows the
///   `Parse`.
///
/// Use [`tree`](Parse::tree) to get the [`SyntaxTree`] view. The methods of
/// the view are also available on the `Parse`.
///
/// ```
/// use jomini::text::lossless::{parse, Parse};
/// use std::sync::Arc;
///
/// // The keys borrow the source, not the parse.
/// fn keys(source: &[u8]) -> Vec<&[u8]> {
///     let parse = parse(source);
///     parse.ast().fields().map(|f| f.key().text()).collect()
/// }
/// assert_eq!(keys(b"a = 1 b = 2"), [&b"a"[..], &b"b"[..]]);
///
/// let source: Arc<[u8]> = Arc::from(&b"a = 1"[..]);
/// let parse: Parse<Arc<[u8]>> = Parse::owned(source);
/// assert_eq!(parse.root().children().count(), 1);
/// assert!(parse.errors().is_empty());
/// ```
#[derive(Clone)]
pub struct Parse<S> {
    source: S,
    data: TreeData,
}

/// A source that a [`Parse`] owns.
///
/// The bytes from [`AsRef::as_ref`] must be the same for each call. If they
/// change, the tree does not match them: the cursors can then give wrong text
/// or panic, but they do not cause undefined behavior.
///
/// To parse a memory map or a different buffer type, put it in [`Owned`], or
/// parse a borrowed slice with [`parse`].
pub trait OwnedSource: AsRef<[u8]> {}

impl OwnedSource for Vec<u8> {}
impl OwnedSource for Box<[u8]> {}
impl OwnedSource for String {}
impl OwnedSource for std::sync::Arc<[u8]> {}
impl OwnedSource for std::rc::Rc<[u8]> {}
impl OwnedSource for Cow<'static, [u8]> {}
impl<T: AsRef<[u8]>> OwnedSource for Owned<T> {}

/// An [`OwnedSource`] for any buffer type that gives bytes, such as a memory
/// map.
///
/// `OwnedSource` does not have an implementation for each `AsRef<[u8]>` type,
/// because a borrowed `&[u8]` is also such a type, and [`parse`] gives the
/// borrowed tree. This wrapper selects the owned tree for a different type.
///
/// ```
/// use jomini::text::lossless::{Owned, Parse};
///
/// // A buffer type from a different crate, such as `memmap2::Mmap`.
/// struct Buffer(Vec<u8>);
/// impl AsRef<[u8]> for Buffer {
///     fn as_ref(&self) -> &[u8] {
///         &self.0
///     }
/// }
///
/// let parse: Parse<Owned<Buffer>> = Parse::owned(Owned(Buffer(b"a = 1".to_vec())));
/// assert_eq!(parse.ast().fields().count(), 1);
/// let Owned(buffer) = parse.into_source();
/// assert_eq!(buffer.0, b"a = 1");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Owned<T>(pub T);

impl<T> Owned<T> {
    /// Return the buffer.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T: AsRef<[u8]>> AsRef<[u8]> for Owned<T> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

/// A view of a lossless syntax tree and its source bytes.
///
/// The view is two references, so it is cheap to copy. The text of its
/// tokens has the lifetime `'a` of the source, not the lifetime `'t` of the
/// tree.
#[derive(Clone, Copy)]
pub struct SyntaxTree<'t, 'a> {
    data: &'t TreeData,
    source: &'a [u8],
}

/// Parse `source` into a lossless syntax tree using the permissive superset.
///
/// The tree borrows `source`. Use [`Parse::owned`] to give the source to the
/// tree.
///
/// ```
/// use jomini::text::lossless::parse;
///
/// let parse = parse(b"owner = FRA controller = FRA");
/// let keys: Vec<&[u8]> = parse
///     .ast()
///     .fields()
///     .map(|f| f.key().text())
///     .collect();
/// assert_eq!(keys, [&b"owner"[..], &b"controller"[..]]);
/// ```
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
#[must_use]
pub fn parse(source: &[u8]) -> Parse<&[u8]> {
    parse_with(source, Flavor::default())
}

/// Parse `source` into a lossless syntax tree with a specific [`Flavor`].
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
#[must_use]
pub fn parse_with(source: &[u8], flavor: Flavor) -> Parse<&[u8]> {
    let data = parse_data(source, flavor);
    Parse { source, data }
}

impl<S: OwnedSource> Parse<S> {
    /// Parse `source` and keep it in the `Parse`, with the permissive
    /// superset.
    ///
    /// # Panics
    ///
    /// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
    #[must_use]
    pub fn owned(source: S) -> Self {
        Parse::owned_with(source, Flavor::default())
    }

    /// Parse `source` and keep it in the `Parse`, with a specific [`Flavor`].
    ///
    /// # Panics
    ///
    /// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
    #[must_use]
    pub fn owned_with(source: S, flavor: Flavor) -> Self {
        let data = parse_data(source.as_ref(), flavor);
        Parse { source, data }
    }
}

/// The methods that give cursors. A borrowed source gives cursors whose text
/// has the lifetime `$a` of the source. An owned source gives cursors that
/// borrow the `Parse`.
macro_rules! parse_cursors {
    ($a:lifetime) => {
        /// See [`SyntaxTree::root`].
        pub fn root(&self) -> SyntaxNode<'_, $a> {
            self.tree().root()
        }

        /// See [`SyntaxTree::ast`].
        pub fn ast(&self) -> Root<'_, $a> {
            self.tree().ast()
        }

        /// See [`SyntaxTree::tokens`].
        pub fn tokens(&self) -> Tokens<'_, $a> {
            self.tree().tokens()
        }

        /// See [`SyntaxTree::significant_tokens`].
        pub fn significant_tokens(&self) -> impl Iterator<Item = SyntaxToken<'_, $a>> {
            self.tree().significant_tokens()
        }

        /// See [`SyntaxTree::token_at_offset`].
        pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken<'_, $a>> {
            self.tree().token_at_offset(offset)
        }

        /// See [`SyntaxTree::covering_element`].
        pub fn covering_element(&self, range: TextRange) -> SyntaxElement<'_, $a> {
            self.tree().covering_element(range)
        }
    };
}

impl<'a> Parse<&'a [u8]> {
    /// The [`SyntaxTree`] view of the tree and the source. The text of the
    /// view has the lifetime `'a` of the source.
    pub fn tree(&self) -> SyntaxTree<'_, 'a> {
        SyntaxTree {
            data: &self.data,
            source: self.source,
        }
    }

    parse_cursors!('a);
}

impl<S: OwnedSource> Parse<S> {
    /// The [`SyntaxTree`] view of the tree and the source. The view borrows
    /// the `Parse`.
    pub fn tree(&self) -> SyntaxTree<'_, '_> {
        self.view()
    }

    parse_cursors!('_);
}

impl<S: AsRef<[u8]>> Parse<S> {
    /// The view for the methods that do not give cursors.
    fn view(&self) -> SyntaxTree<'_, '_> {
        SyntaxTree {
            data: &self.data,
            source: self.source.as_ref(),
        }
    }

    /// The source.
    pub fn source(&self) -> &S {
        &self.source
    }

    /// Discard the tree and return the source.
    pub fn into_source(self) -> S {
        self.source
    }

    /// See [`SyntaxTree::errors`].
    pub fn errors(&self) -> &[SyntaxError] {
        &self.data.errors
    }

    /// See [`SyntaxTree::repair_applicability`].
    pub fn repair_applicability(&self, delimiter: &MissingDelimiter) -> Applicability {
        self.view().repair_applicability(delimiter)
    }

    /// See [`SyntaxTree::repairs`].
    pub fn repairs(&self) -> Vec<Repair> {
        self.view().repairs()
    }

    /// See [`SyntaxTree::repaired_source`].
    #[must_use]
    pub fn repaired_source(&self) -> Vec<u8> {
        self.view().repaired_source()
    }

    /// See [`SyntaxTree::reconstruct`].
    #[cfg(test)]
    pub fn reconstruct(&self) -> Vec<u8> {
        self.view().reconstruct()
    }

    /// See [`SyntaxTree::debug_tree`].
    pub fn debug_tree(&self) -> String {
        self.view().debug_tree()
    }

    /// See [`SyntaxTree::format`].
    pub fn format(&self, opts: &FormatOptions) -> Vec<u8> {
        self.view().format(opts)
    }
}

fn parse_data(source: &[u8], flavor: Flavor) -> TreeData {
    let p = run(source, flavor, Builder::with_capacity(source.len() / 5));
    let trailing_comment = p.lexer.trailing_comment();
    let Parser {
        sink,
        mut tokens,
        base,
        lexed,
        errors,
        ..
    } = p;
    debug_assert_eq!(base, 0, "the tree keeps every token");
    let nodes = sink.finish(trailing_comment, &mut tokens, lexed);
    TreeData {
        flavor,
        tokens,
        nodes,
        errors,
        parents: OnceLock::new(),
        repairs_valid: OnceLock::new(),
    }
}

/// Send the events of the grammar for `source` to `sink`. The parser lexes
/// the source while it parses.
fn run<S: Sink>(source: &[u8], flavor: Flavor, sink: S) -> Parser<'_, S> {
    assert!(
        u32::try_from(source.len()).is_ok(),
        "source is larger than 4 GiB"
    );
    let mut p = Parser::new(source, Lexer::new(source, flavor), sink);
    p.sink.start_node(SyntaxKind::Root);
    p.parse_items(false);
    if S::KEEP_TOKENS
        && let Some(range) = p.lexer.unclosed_quote
    {
        p.errors.push(SyntaxError {
            kind: SyntaxErrorKind::UnclosedQuote,
            range,
            recovery: None,
        });
    }
    p.sink.finish_node();
    p
}

/// Send the events of the grammar for the root items that start at the byte
/// `offset` to `sink`. The sink must have an open root node, with the state
/// that it has at `offset` in a full parse. The parser does not have the
/// tokens before `offset`, thus the grammar must not look back at them. This
/// is true after a `}` token. The diagnostics are not complete.
fn run_items<S: Sink>(source: &[u8], flavor: Flavor, offset: usize, sink: S) -> Parser<'_, S> {
    let mut p = Parser::new(source, Lexer::at(source, flavor, offset), sink);
    p.parse_items(false);
    p.sink.finish_node();
    p
}

impl<'t, 'a> SyntaxTree<'t, 'a> {
    /// The full source the tree was parsed from.
    pub fn source(&self) -> &'a [u8] {
        self.source
    }

    /// The root [`SyntaxNode`] (always [`SyntaxKind::Root`]).
    pub fn root(&self) -> SyntaxNode<'t, 'a> {
        SyntaxNode {
            tree: *self,
            idx: 0,
        }
    }

    /// Recoverable problems found during parsing (empty for clean input).
    pub fn errors(&self) -> &[SyntaxError] {
        &self.data.errors
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
            Applicability::MaybeIncorrect
        }
    }

    /// Coalesced, machine-applicable delimiter insertions in source order.
    ///
    /// An editor can show each repair as a quick fix:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, TextSize};
    ///
    /// let parse = parse(b"country = { tag = FRA");
    /// let repairs = parse.repairs();
    /// assert_eq!(repairs.len(), 1);
    /// assert_eq!(repairs[0].range.start(), TextSize::new(21));
    /// assert_eq!(repairs[0].replacement, "}");
    /// ```
    pub fn repairs(&self) -> Vec<Repair> {
        if !self.repairs_valid() {
            return Vec::new();
        }
        self.local_fixes()
    }

    /// Apply all safe virtual delimiter repairs without changing other bytes.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"country = { tag = FRA # unclosed");
    /// assert_eq!(parse.repaired_source(), b"country = { tag = FRA # unclosed\n}");
    /// ```
    #[must_use]
    pub fn repaired_source(&self) -> Vec<u8> {
        apply_repairs(self.source, self.repairs())
    }

    fn repairs_valid(&self) -> bool {
        *self.data.repairs_valid.get_or_init(|| {
            let candidate = apply_repairs(self.source, self.local_fixes());
            // The parser cannot read a source of 4 GiB or more.
            if u32::try_from(candidate.len()).is_err() {
                return false;
            }
            if candidate == self.source {
                return true;
            }
            // The repairs must remove every missing delimiter and must not
            // add a different error.
            let errors = &self.data.errors;
            let kept = errors.iter().filter(|e| e.recovery.is_none()).count();
            let reparsed = parse_data(&candidate, self.data.flavor).errors;
            reparsed.len() <= kept && reparsed.iter().all(|e| e.recovery.is_none())
        })
    }

    fn local_fixes(&self) -> Vec<Repair> {
        let mut records: Vec<&MissingDelimiter> = self
            .data
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
    /// [`source`](SyntaxTree::source).
    ///
    /// ```
    /// use jomini::text::lossless::{parse, SyntaxKind};
    ///
    /// let source = b"size = 3 # small";
    /// let parse = parse(source);
    ///
    /// // Give each token a highlight class.
    /// let classes: Vec<(&str, &[u8])> = parse
    ///     .tokens()
    ///     .filter(|t| t.kind() != SyntaxKind::Whitespace)
    ///     .map(|t| {
    ///         let class = match t.kind() {
    ///             SyntaxKind::Comment => "comment",
    ///             SyntaxKind::Operator => "operator",
    ///             _ if t.as_scalar().to_f64().is_ok() => "number",
    ///             _ => "identifier",
    ///         };
    ///         (class, t.text())
    ///     })
    ///     .collect();
    /// assert_eq!(classes, [
    ///     ("identifier", &b"size"[..]),
    ///     ("operator", b"="),
    ///     ("number", b"3"),
    ///     ("comment", b"# small"),
    /// ]);
    ///
    /// // The tokens give back the source.
    /// let text: Vec<u8> = parse.tokens().flat_map(|t| t.text()).copied().collect();
    /// assert_eq!(text, source);
    /// ```
    pub fn tokens(&self) -> Tokens<'t, 'a> {
        Tokens {
            tree: *self,
            next: 0,
            end: self.data.tokens.len() as u32,
            prev_end: 0,
            gap: Gap::EMPTY,
            pending: None,
            trailing_done: false,
        }
    }

    /// Every leaf [`SyntaxToken`] that is not trivia, in document order. The
    /// zero-width tokens that the parser inserts for a missing delimiter are
    /// included.
    pub fn significant_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + use<'t, 'a> {
        let tree = *self;
        (0..self.data.tokens.len() as u32).map(move |i| tree.token(i))
    }

    /// Reconstruct the source by walking the tree's leaves via the cursor API.
    ///
    /// Equals [`SyntaxTree::source`] for any tree; the lossless invariant.
    #[cfg(test)]
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

    /// An indented dump of the tree, for debugging and tests.
    ///
    /// Each line shows the kind and the byte range of one node or token, in
    /// the same form as rust-analyzer. A token also shows its text. The dump
    /// includes the trivia and the zero-width recovery tokens.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"building = { cost = 100 } # wood\n");
    /// assert_eq!(parse.debug_tree(), r##"Root@0..33
    ///   Field@0..25
    ///     Unquoted@0..8 "building"
    ///     Whitespace@8..9 " "
    ///     Operator@9..10 "="
    ///     Whitespace@10..11 " "
    ///     Block@11..25
    ///       OpenBrace@11..12 "{"
    ///       Whitespace@12..13 " "
    ///       Field@13..23
    ///         Unquoted@13..17 "cost"
    ///         Whitespace@17..18 " "
    ///         Operator@18..19 "="
    ///         Whitespace@19..20 " "
    ///         Unquoted@20..23 "100"
    ///       Whitespace@23..24 " "
    ///       CloseBrace@24..25 "}"
    ///   Whitespace@25..26 " "
    ///   Comment@26..32 "# wood"
    ///   Whitespace@32..33 "\n"
    /// "##);
    /// ```
    pub fn debug_tree(&self) -> String {
        use fmt::Write;

        fn go(el: SyntaxElement<'_, '_>, depth: usize, out: &mut String) {
            for _ in 0..depth {
                out.push_str("  ");
            }
            match el {
                SyntaxElement::Node(n) => {
                    let _ = writeln!(out, "{n:?}");
                    for c in n.children() {
                        go(c, depth + 1, out);
                    }
                }
                SyntaxElement::Token(t) => {
                    let _ = writeln!(out, "{t:?}");
                }
            }
        }
        let mut out = String::new();
        go(SyntaxElement::Node(self.root()), 0, &mut out);
        out
    }

    fn token(&self, idx: u32) -> SyntaxToken<'t, 'a> {
        let token = self.data.tokens[idx as usize];
        SyntaxToken {
            tree: *self,
            kind: token.kind,
            range: token.range(),
            idx,
            trivia: false,
        }
    }

    fn trivia(&self, kind: SyntaxKind, start: u32, len: u32, next: u32) -> SyntaxToken<'t, 'a> {
        SyntaxToken {
            tree: *self,
            kind,
            range: TextRange::span(start, start + len),
            idx: next,
            trivia: true,
        }
    }

    fn parents(&self) -> &Parents {
        self.data.parents.get_or_init(|| {
            let mut nodes = vec![NO_PARENT; self.data.nodes.len()];
            let mut tokens = vec![NO_PARENT; self.data.tokens.len()];
            let mut stack: Vec<u32> = Vec::new();
            let mut t = 0u32;
            let assign = |stack: &mut Vec<u32>, tokens: &mut [u32], t: u32| {
                while let Some(&top) = stack.last() {
                    if t < self.data.nodes[top as usize].token_end {
                        break;
                    }
                    stack.pop();
                }
                tokens[t as usize] = stack.last().copied().unwrap_or(NO_PARENT);
            };
            for (i, node) in self.data.nodes.iter().enumerate() {
                // Pre-order means `first_token` does not decrease, so every
                // token before this node belongs to a node already visited.
                while t < node.first_token {
                    assign(&mut stack, &mut tokens, t);
                    t += 1;
                }
                while let Some(&top) = stack.last() {
                    if (i as u32) < self.data.nodes[top as usize].subtree_end {
                        break;
                    }
                    stack.pop();
                }
                nodes[i] = stack.last().copied().unwrap_or(NO_PARENT);
                stack.push(i as u32);
            }
            while (t as usize) < self.data.tokens.len() {
                assign(&mut stack, &mut tokens, t);
                t += 1;
            }
            Parents { nodes, tokens }
        })
    }

    /// The byte range of the gap before significant token `next`. For
    /// `next == tokens.len()`, it is the gap after the last token.
    fn gap_before(&self, next: u32) -> Gap {
        let tokens = &self.data.tokens;
        let start = match next.checked_sub(1) {
            Some(prev) => tokens[prev as usize].end(),
            None => 0,
        };
        let end = tokens
            .get(next as usize)
            .map_or(self.source.len() as u32, |t| t.start);
        Gap { start, end, next }
    }

    /// The leaf that contains the byte at `offset`, or `None` when `offset`
    /// is not before the end of the source. A zero-width token contains no
    /// byte, so this function does not give one.
    fn leaf_at(&self, offset: u32) -> Option<SyntaxToken<'t, 'a>> {
        if offset as usize >= self.source.len() {
            return None;
        }
        let tokens = &self.data.tokens;
        let next = tokens.partition_point(|t| t.end() <= offset);
        if let Some(token) = tokens.get(next)
            && token.start <= offset
        {
            return Some(self.token(next as u32));
        }
        // Lex the gap from its start, because a comment can begin before
        // `offset`.
        let mut gap = self.gap_before(next as u32);
        while let Some(token) = gap.next_token(*self) {
            if token.range.end > offset {
                return Some(token);
            }
        }
        None
    }

    /// The leaves at `offset`. When `offset` is between two leaves, the
    /// result has both.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, SyntaxKind, TextSize, TokenAtOffset};
    ///
    /// let parse = parse(b"a=b # c");
    /// let tree = parse.tree();
    /// let TokenAtOffset::Between(left, right) = tree.token_at_offset(TextSize::new(1)) else {
    ///     panic!("expected two tokens");
    /// };
    /// assert_eq!((left.text(), right.text()), (&b"a"[..], &b"="[..]));
    ///
    /// let comment = tree.token_at_offset(TextSize::new(6)).right_biased().unwrap();
    /// assert_eq!(comment.kind(), SyntaxKind::Comment);
    /// ```
    pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken<'t, 'a>> {
        let offset = offset.0;
        if offset as usize > self.source.len() {
            return TokenAtOffset::None;
        }
        let right = self.leaf_at(offset);
        let left = offset.checked_sub(1).and_then(|o| self.leaf_at(o));
        match (left, right) {
            (Some(left), Some(right)) if left != right => TokenAtOffset::Between(left, right),
            (Some(token), _) | (None, Some(token)) => TokenAtOffset::Single(token),
            (None, None) => TokenAtOffset::None,
        }
    }

    /// The smallest node or token that contains all of `range`. For an empty
    /// range, the search starts at the token after the offset. When `range`
    /// is not inside the source, the result is the root.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, SyntaxKind, TextRange, TextSize};
    ///
    /// let parse = parse(b"a = { b = c }");
    /// let tree = parse.tree();
    /// let token = tree.covering_element(TextRange::at(TextSize::new(6), TextSize::new(1)));
    /// assert_eq!(token.kind(), SyntaxKind::Unquoted);
    /// let field = tree.covering_element(TextRange::new(TextSize::new(6), TextSize::new(11)));
    /// assert_eq!(field.kind(), SyntaxKind::Field);
    /// assert_eq!(field.text(), b"b = c");
    /// ```
    pub fn covering_element(&self, range: TextRange) -> SyntaxElement<'t, 'a> {
        let root = SyntaxElement::Node(self.root());
        if range.end as usize > self.source.len() {
            return root;
        }
        let leaf = self
            .token_at_offset(range.start())
            .right_biased()
            .or_else(|| self.token_at_offset(range.start()).left_biased());
        let Some(leaf) = leaf else {
            return root;
        };
        if leaf.range.contains_range(range) {
            return SyntaxElement::Token(leaf);
        }
        leaf.ancestors()
            .find(|node| node.text_range().contains_range(range))
            .map_or(root, SyntaxElement::Node)
    }

    /// The node that owns the gap before token `next`: the nearest common
    /// ancestor of the tokens on the two sides of the gap.
    fn gap_owner(&self, next: u32) -> u32 {
        if next == 0 || next as usize >= self.data.tokens.len() {
            return 0;
        }
        let parents = self.parents();
        let mut left = parents.tokens[next as usize - 1];
        let mut right = parents.tokens[next as usize];
        // The nodes are in pre-order, so an ancestor has a smaller index than
        // its descendants. The node with the larger index cannot be the
        // common ancestor, thus move it to its parent.
        while left != right {
            if left == NO_PARENT || right == NO_PARENT {
                return 0;
            }
            if left > right {
                left = parents.nodes[left as usize];
            } else {
                right = parents.nodes[right as usize];
            }
        }
        if left == NO_PARENT { 0 } else { left }
    }
}

fn apply_repairs(source: &[u8], fixes: Vec<Repair>) -> Vec<u8> {
    let mut out = source.to_vec();
    for fix in fixes.into_iter().rev() {
        out.splice(fix.range.to_range(), fix.replacement.bytes());
    }
    out
}

/// Receives the parser's events in source order. The grammar knows only this
/// interface, so a different storage can reuse the same grammar.
trait Sink {
    /// The sink needs every token and every diagnostic after the parse. When
    /// this is `false`, the parser keeps only a small window of tokens and
    /// does not record diagnostics.
    const KEEP_TOKENS: bool;

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

    /// Add a `{` or a `}` token of a block. A sink that does not use the
    /// braces can do this faster than [`Sink::token`].
    #[inline]
    fn brace(&mut self, token: Token) {
        self.token(token);
    }

    /// Open a field node and add its key and operator. The value follows.
    #[inline]
    fn field_head(&mut self, key: Token, op: Token) {
        self.start_node(SyntaxKind::Field);
        self.token(key);
        self.token(op);
    }
}

/// Build the node tape and record the positions of missing tokens.
struct Builder {
    nodes: Vec<Node>,
    /// The open nodes. Each entry holds the index of a node and the flags
    /// of its parent. The flags of the innermost open node are in `flags`,
    /// because each token changes them.
    stack: Vec<(u32, NodeFlags)>,
    /// The flags collected for the innermost open node.
    flags: NodeFlags,
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
            flags: NodeFlags(0),
            next_token: 0,
            missing_tokens: Vec::new(),
        }
    }

    /// Record the diagnostic and the comment of a token.
    #[cold]
    #[inline(never)]
    fn token_flags(&mut self, token: Token, index: u32) {
        if token.flags & TOKEN_ERROR != 0 {
            self.flags.insert(NodeFlags::HAS_ERROR);
        }
        if token.flags & TOKEN_COMMENT_BEFORE != 0 {
            // The comment belongs to the deepest open node that also holds
            // the previous token. The root holds a leading comment.
            let nodes = &self.nodes;
            let owner = self
                .stack
                .iter()
                .rposition(|(node, _)| nodes[*node as usize].first_token < index)
                .unwrap_or(0);
            if !self.stack.is_empty() {
                self.open_flags(owner).insert(NodeFlags::HAS_COMMENT);
            }
        }
    }

    /// The flags of the open node at `depth` in the stack.
    fn open_flags(&mut self, depth: usize) -> &mut NodeFlags {
        match self.stack.get_mut(depth + 1) {
            Some((_, parent)) => parent,
            None => &mut self.flags,
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
    const KEEP_TOKENS: bool = true;

    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        if (key.flags | op.flags | value.flags) & TOKEN_COMMENT_BEFORE != 0 {
            self.start_node(SyntaxKind::Field);
            self.token(key);
            self.token(op);
            self.token(value);
            self.finish_node();
            return;
        }
        let mut flags = own_flag_bits(key.kind);
        flags.insert(own_flag_bits(value.kind));
        if (key.flags | op.flags | value.flags) & TOKEN_ERROR != 0 {
            flags.insert(NodeFlags::HAS_ERROR);
        }
        let first_token = self.next_token;
        self.next_token += 3;
        let subtree_end = self.nodes.len() as u32 + 1;
        self.nodes.push(Node {
            kind: SyntaxKind::Field,
            flags,
            first_token,
            token_end: self.next_token,
            subtree_end,
        });
        self.flags.insert(flags);
    }

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
        self.stack.push((idx, self.flags));
        self.flags = own_flag_bits(kind);
    }

    #[inline]
    fn token(&mut self, token: Token) {
        let index = self.next_token;
        self.next_token += 1;
        if token.kind.is_missing() {
            self.missing_tokens.push((index, token));
        }
        self.flags.insert(own_flag_bits(token.kind));
        if token.flags & (TOKEN_ERROR | TOKEN_COMMENT_BEFORE) != 0 {
            self.token_flags(token, index);
        }
    }

    #[inline]
    fn finish_node(&mut self) {
        let (idx, parent) = self.stack.pop().expect("finish_node without start_node");
        let flags = self.flags;
        let subtree_end = self.nodes.len() as u32;
        let node = &mut self.nodes[idx as usize];
        node.flags = flags;
        node.token_end = self.next_token;
        node.subtree_end = subtree_end;
        self.flags = parent;
        self.flags.insert(flags);
    }

    fn error(&mut self) {
        if !self.stack.is_empty() {
            self.flags.insert(NodeFlags::HAS_ERROR);
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

/// The number of tokens that the parser can always read after its position
/// with [`Parser::kind_at`]. The grammar looks at most five tokens ahead.
const LOOKAHEAD: usize = 8;

/// The number of tokens that the lexer adds to the window at a time. Tests
/// use a small value so that they exercise the window logic.
const CHUNK: usize = if cfg!(test) { 3 } else { 1024 };

/// The number of tokens before the parser position that stay in the window.
/// The grammar looks at most two tokens back.
const LOOKBEHIND: usize = 4;

struct Parser<'t, S: Sink> {
    source: &'t [u8],
    lexer: Lexer,
    /// A window of the lexed tokens: `tokens[0]` has the index `base`. When
    /// the sink keeps tokens, the window holds all the tokens and `base` is
    /// zero.
    tokens: Vec<Token>,
    base: usize,
    /// The number of lexed tokens. When the lexer is done, a position at or
    /// after it is EOF.
    lexed: usize,
    /// The window keeps the tokens at and after this index.
    pin: usize,
    pos: usize,
    sink: S,
    errors: Vec<SyntaxError>,
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
    /// A missing delimiter was emitted at EOF.
    missing_at_eof: bool,
    /// The sorted indexes of the `[` tokens that start an interpolation,
    /// for the `[` tokens before `resolved_until`.
    interpolations: Vec<u32>,
    resolved_until: usize,
}

impl<'t, S: Sink> Parser<'t, S> {
    fn new(source: &'t [u8], lexer: Lexer, sink: S) -> Self {
        // Save files average approximately five bytes per significant token.
        let capacity = if S::KEEP_TOKENS {
            source.len() / 5 + 16
        } else {
            2 * CHUNK + LOOKAHEAD
        };
        let mut parser = Parser {
            source,
            lexer,
            tokens: Vec::with_capacity(capacity),
            base: 0,
            lexed: 0,
            pin: usize::MAX,
            pos: 0,
            sink,
            errors: Vec::new(),
            depth: 0,
            parameter_depth: 0,
            bracket_depth: 0,
            code_depth: 0,
            recovery_barrier: 0,
            eof_repairs: 0,
            missing_at_eof: false,
            interpolations: Vec::new(),
            resolved_until: 0,
        };
        parser.refill();
        parser
    }

    /// Lex more tokens into the window. When the sink does not keep tokens,
    /// first remove the tokens that the parser cannot read again.
    #[inline(never)]
    fn fill(&mut self) {
        if !S::KEEP_TOKENS {
            let keep = self.pos.saturating_sub(LOOKBEHIND).min(self.pin);
            if keep >= self.base + CHUNK {
                self.tokens.drain(..keep - self.base);
                self.base = keep;
            }
        }
        self.lexer.fill(self.source, &mut self.tokens, CHUNK);
        self.lexed = self.base + self.tokens.len();
    }

    /// Lex until [`LOOKAHEAD`] tokens follow the position, or the source ends.
    #[inline]
    fn refill(&mut self) {
        while self.pos + LOOKAHEAD > self.lexed && !self.lexer.done {
            self.fill();
        }
    }

    /// Move the position `n` tokens forward.
    #[inline]
    fn advance(&mut self, n: usize) {
        self.pos += n;
        self.refill();
    }

    /// The token at `index`. It must be in the window.
    #[inline]
    fn tok(&self, index: usize) -> Token {
        self.tokens[index - self.base]
    }

    #[inline]
    fn tok_mut(&mut self, index: usize) -> &mut Token {
        &mut self.tokens[index - self.base]
    }

    /// Insert a zero-width `missing` token at EOF and record its repair.
    fn missing(
        &mut self,
        open: usize,
        missing: SyntaxKind,
        construct: RecoveryConstruct,
        kind: SyntaxErrorKind,
    ) {
        self.push_missing(missing);
        if !S::KEEP_TOKENS {
            return;
        }
        let at = self.source.len() as u32;
        let order = self.eof_repairs;
        self.eof_repairs += 1;
        let mut repair_text = missing.missing_text().unwrap().to_owned();
        // A delimiter appended to an unterminated line comment remains comment
        // text. Start a new line and preserve the file's established ending.
        if order == 0
            && self.lexer.trailing_comment()
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
        // The lexer is done at EOF, so it already knows if the last token is
        // a quoted string without a closing quote. Do not scan it again for
        // each missing delimiter.
        let unsafe_quote = self.lexer.unclosed_quote.is_some();
        self.errors.push(SyntaxError {
            kind,
            range: TextRange::empty(TextSize(at)),
            recovery: Some(MissingDelimiter {
                expected: missing.expected_kind().unwrap(),
                missing,
                construct,
                opening_range: self.tok(open).range(),
                insertion_range: TextRange::empty(TextSize(at)),
                repair_text,
                order,
                local_applicability: if unsafe_quote {
                    Applicability::MaybeIncorrect
                } else {
                    Applicability::MachineApplicable
                },
            }),
        });
    }

    /// Send a zero-width token at EOF to the sink. The window does not hold
    /// it: the sink records where it goes.
    fn push_missing(&mut self, kind: SyntaxKind) {
        // A comment at EOF comes before the first missing delimiter. Mark it
        // there, so that the nodes that own the comment get its flag.
        let flags = if !self.missing_at_eof && self.lexer.trailing_comment() {
            TOKEN_COMMENT_BEFORE
        } else {
            0
        };
        self.missing_at_eof = true;
        let token = Token {
            start: self.source.len() as u32,
            len: 0,
            kind,
            flags,
        };
        self.sink.token(token);
    }

    /// Record a diagnostic that starts at the emitted token `index`. Call it
    /// while the nodes that hold the token are still open.
    fn error_at(&mut self, index: usize, kind: SyntaxErrorKind) {
        if !S::KEEP_TOKENS {
            return;
        }
        self.tok_mut(index).flags |= TOKEN_ERROR;
        self.sink.error();
        self.errors.push(SyntaxError {
            kind,
            range: self.tok(index).range(),
            recovery: None,
        });
    }

    #[inline]
    fn kind_at(&self, index: usize) -> Option<SyntaxKind> {
        debug_assert!(index >= self.base, "lookbehind before the token window");
        let token = self.tokens.get(index.wrapping_sub(self.base));
        debug_assert!(
            token.is_some() || self.lexer.done,
            "lookahead past the token window"
        );
        token.map(|token| token.kind)
    }

    /// Return the kind at `index` like [`Parser::kind_at`], but lex as far as
    /// necessary.
    fn kind_far(&mut self, index: usize) -> Option<SyntaxKind> {
        while index >= self.lexed && !self.lexer.done {
            self.fill();
        }
        self.kind_at(index)
    }

    #[inline]
    fn peek(&self) -> Option<SyntaxKind> {
        self.kind_at(self.pos)
    }

    #[inline]
    fn bump(&mut self) {
        let t = self.tok(self.pos);
        self.sink.token(t);
        self.advance(1);
    }

    /// Add the brace at the position to the sink, and move to the next token.
    #[inline]
    fn bump_brace(&mut self) {
        let t = self.tok(self.pos);
        self.sink.brace(t);
        self.advance(1);
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
                    self.tok_mut(self.pos).kind = SyntaxKind::Unquoted;
                    self.parse_item();
                }
                Some(SyntaxKind::CloseBracket)
                    if self.code_depth > 0 && self.is_code_close(self.pos) =>
                {
                    break;
                }
                Some(SyntaxKind::CloseBracket) if self.parameter_depth == 0 => {
                    // A stray `]`, as in `{ 0.5 0.9] }`. Keep it in a bogus
                    // node and continue, so that the enclosing blocks keep
                    // their structure. The `]` makes the intent of the author
                    // unclear, thus refuse a virtual `}` at EOF.
                    let index = self.pos;
                    self.recovery_barrier += 1;
                    self.sink.start_node(SyntaxKind::Bogus);
                    self.bump();
                    self.error_at(index, SyntaxErrorKind::UnmatchedCloseBracket);
                    self.sink.finish_node();
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

    #[inline(always)]
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
                            self.tok(self.pos),
                            self.tok(self.pos + 1),
                            self.tok(self.pos + 2),
                        );
                        self.sink.scalar_field(key, op, value);
                        self.advance(3);
                    }
                    Some(SyntaxKind::Operator) => {
                        // key <op> value
                        let (key, op) = (self.tok(self.pos), self.tok(self.pos + 1));
                        self.sink.field_head(key, op);
                        self.advance(2);
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
            Some(SyntaxKind::Unquoted | SyntaxKind::MacroParam | SyntaxKind::Variable)
                if self.kind_at(self.pos + 1) == Some(SyntaxKind::OpenBrace) =>
            {
                // headered block: `rgb { ... }`, `hsv { ... }`, tag { ... }.
                // TextTape also accepts `$NAME$ { ... }` and `@name { ... }`.
                self.sink.start_node(SyntaxKind::HeaderedBlock);
                self.bump(); // header scalar
                self.parse_block();
                self.sink.finish_node();
            }
            // A `]` that closes a parameter body or a code payload is not a
            // value, as in `[[p] a = ]`.
            Some(SyntaxKind::CloseBracket)
                if (self.parameter_depth > 0 && self.bracket_depth == 0)
                    || (self.code_depth > 0 && self.is_code_close(self.pos)) => {}
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
            && self.tok(index).len == 4
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
        &self.source[self.tok(index).range().to_range()]
    }

    fn preceded_by_code(&self, open: usize) -> bool {
        let Some(previous) = open.checked_sub(1) else {
            return false;
        };
        if self.is_code_keyword(previous) {
            return true;
        }
        if self.tok(previous).kind != SyntaxKind::Operator || self.token_text(previous) != b"=" {
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
        if self.pos >= self.resolved_until {
            let mut starts = std::mem::take(&mut self.interpolations);
            self.resolved_until =
                resolve_interpolations(self.pos, |index| self.kind_far(index), &mut starts);
            self.interpolations = starts;
        }
        self.interpolations
            .binary_search(&(self.pos as u32))
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
        // The index of the last lone `]` that the payload took directly.
        let mut lone_close = usize::MAX;
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
                        if self.kind_at(self.pos + 1).is_some() {
                            self.recovery_barrier += 1;
                        }
                        break;
                    }
                    self.bump();
                }
                None => break,
                Some(kind) => {
                    if kind == SyntaxKind::CloseBracket {
                        lone_close = self.pos;
                    }
                    self.parse_item();
                }
            }
        }
        self.code_depth -= 1;

        if !closed {
            if self.peek().is_none() && self.recovery_barrier == barrier {
                // Only a lone `]` of the payload is the first half of `]]`.
                // A `]` that closes an interpolation is not, as in `a[b]`.
                let missing_count = if self.pos.checked_sub(1) == Some(lone_close)
                    && self.tok(lone_close).end() == self.source.len() as u32
                {
                    1
                } else {
                    2
                };
                for index in 0..missing_count {
                    let kind = if missing_count == 1 || index == 1 {
                        SyntaxErrorKind::CodeMissingSecondBracket
                    } else {
                        SyntaxErrorKind::CodeMissingFirstBracket
                    };
                    self.missing(
                        open,
                        SyntaxKind::MissingCloseBracket,
                        RecoveryConstruct::Code,
                        kind,
                    );
                }
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedCode);
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
        self.bump_brace(); // `{`
        self.depth += 1;
        if self.depth >= MAX_DEPTH {
            self.recovery_barrier += 1;
            // The nesting is too deep. Do not recurse, because recursion can
            // cause a stack overflow. Consume the remaining tokens of this
            // block as flat tokens. The nested blocks are also flat. The tree
            // keeps all bytes, but it has no structure after this point.
            self.error_at(open, SyntaxErrorKind::BlockDepthExceeded);
            self.flatten_to_block_close();
        } else {
            self.parse_items(true);
            if self.peek() == Some(SyntaxKind::CloseBrace) {
                self.bump_brace(); // `}`
            } else if self.peek() == Some(SyntaxKind::CloseBracket)
                && self.parameter_depth > 0
                && self.bracket_depth == 0
            {
                // The `]` of a parameter can close a block body. A later
                // parameter block continues that block body.
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

    /// Consume the remaining tokens of the current block as flat tokens. The
    /// nested braces are also flat. Stop after the matching `}` or at EOF.
    /// The caller consumed the `{`, thus the brace depth starts at 1. The
    /// depth guard uses this function to limit recursion. The parser emits
    /// all tokens, thus the tree keeps all bytes.
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
        // The block did not close before EOF. The depth diagnostic covers
        // this block, thus do not add an "unclosed" diagnostic.
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
        self.pin = from;
        while !matches!(
            self.peek(),
            Some(SyntaxKind::CalcClose | SyntaxKind::OpenBrace | SyntaxKind::CloseBrace) | None
        ) {
            self.advance(1);
        }
        let at_eof = self.peek().is_none();
        let interior = parse_calc_interior(
            &self.tokens[from - self.base..self.pos - self.base],
            from as u32,
            at_eof,
        );
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
        if at_eof && !interior.overflowed && S::KEEP_TOKENS {
            for paren_open in interior.missing_parens {
                // The zero-width leaf was emitted inside its ParenExpr above.
                let at = self.source.len() as u32;
                let order = self.eof_repairs;
                self.eof_repairs += 1;
                self.errors.push(SyntaxError {
                    kind: SyntaxErrorKind::UnclosedCalcParen,
                    range: TextRange::empty(TextSize(at)),
                    recovery: Some(MissingDelimiter {
                        expected: SyntaxKind::CloseParen,
                        missing: SyntaxKind::MissingCloseParen,
                        construct: RecoveryConstruct::CalcParen,
                        opening_range: self.tok(paren_open as usize).range(),
                        insertion_range: TextRange::empty(TextSize(at)),
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
                RecoveryConstruct::Calc,
                SyntaxErrorKind::UnclosedCalc,
            );
        } else if !at_eof {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::UnclosedCalc);
        }
        self.pin = usize::MAX;
        self.sink.finish_node();
    }

    /// Walk a [`CalcElem`] in pre-order and send its tokens and nodes to the sink.
    fn emit_calc(&mut self, el: &CalcElem, missing_parens: &[u32]) {
        match el {
            CalcElem::Leaf(index) => {
                let token = self.tok(*index as usize);
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

/// Find each `[` that starts a single-bracket interpolation, from the `[` at
/// `from` to the returned end index. Push their indexes to `starts` in sorted
/// order. `kind_at` gives the kind of a token, or `None` at EOF.
///
/// A `[` starts an interpolation when its matching `]` comes before a `}` that
/// is directly inside it. At EOF, a still-open `[` that is followed by more
/// tokens is a trailing interpolation. A bare final `[` is not.
///
/// The result for a `[` depends only on the tokens after it. The scan stops
/// when the `[` at `from` is resolved. At that point, each `[` inside the
/// scanned region is also resolved, so the total work is linear.
fn resolve_interpolations(
    from: usize,
    mut kind_at: impl FnMut(usize) -> Option<SyntaxKind>,
    starts: &mut Vec<u32>,
) -> usize {
    starts.clear();
    // Open brackets and whether a `}` interrupted each one.
    let mut stack: Vec<(u32, bool)> = vec![(from as u32, false)];
    let mut index = from + 1;
    let end = loop {
        match kind_at(index) {
            Some(SyntaxKind::OpenBracket) => stack.push((index as u32, false)),
            Some(SyntaxKind::CloseBracket) => {
                if let Some((open, false)) = stack.pop() {
                    starts.push(open);
                }
                if stack.is_empty() {
                    break index + 1;
                }
            }
            Some(SyntaxKind::CloseBrace) => {
                if stack.len() == 1 {
                    // A `}` interrupts the `[` at `from`.
                    break index;
                }
                if let Some(top) = stack.last_mut() {
                    top.1 = true;
                }
            }
            Some(_) => {}
            None => {
                starts.extend(
                    stack
                        .iter()
                        .filter(|&&(open, interrupted)| !interrupted && open as usize + 1 < index)
                        .map(|&(open, _)| open),
                );
                break index;
            }
        }
        index += 1;
    };
    starts.sort_unstable();
    end
}

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
/// When `insert_missing_parens` is set, each `(` that has no `)` gets a
/// zero-width [`SyntaxKind::MissingCloseParen`]. When the [`MAX_DEPTH`] guard
/// trips, the result is each token as a flat leaf.
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

/// A handle to an interior node in a [`SyntaxTree`].
///
/// The untyped cursors see every byte, the trivia too. The flags of a node
/// tell in constant time if its subtree has a comment, an error, a calc, or a
/// macro, so a tool can skip a subtree that it does not have to examine:
///
/// ```
/// use jomini::text::lossless::{parse, SyntaxKind};
///
/// let parse = parse(b"a = { 1 2 } b = { # keep\n 3 }");
/// let fields: Vec<_> = parse.root().child_nodes().collect();
/// assert!(!fields[0].has_comment());
/// assert!(fields[1].has_comment());
///
/// let comment = fields[1]
///     .child_nodes()
///     .flat_map(|block| block.child_tokens())
///     .find(|t| t.kind() == SyntaxKind::Comment)
///     .unwrap();
/// assert_eq!(comment.text(), b"# keep");
/// assert_eq!(comment.parent().unwrap().kind(), SyntaxKind::Block);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxNode<'t, 'a> {
    tree: SyntaxTree<'t, 'a>,
    idx: u32,
}

/// A handle to a leaf token in a [`SyntaxTree`]. It is a significant token from
/// the token buffer, or a trivia token made from a gap.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxToken<'t, 'a> {
    tree: SyntaxTree<'t, 'a>,
    kind: SyntaxKind,
    range: TextRange,
    /// For a significant token, its index in the token buffer. For trivia, the
    /// index of the significant token after the gap.
    idx: u32,
    trivia: bool,
}

/// Either a [`SyntaxNode`] or a [`SyntaxToken`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElement<'t, 'a> {
    Node(SyntaxNode<'t, 'a>),
    Token(SyntaxToken<'t, 'a>),
}

/// Two views are equal when they refer to the same tree and the same source.
/// The comparison does not look at the contents.
impl PartialEq for SyntaxTree<'_, '_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.data, other.data) && std::ptr::eq(self.source, other.source)
    }
}

impl Eq for SyntaxTree<'_, '_> {}

impl std::hash::Hash for SyntaxTree<'_, '_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self.data, state);
        std::ptr::hash(self.source.as_ptr(), state);
    }
}

impl fmt::Debug for SyntaxTree<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxTree")
            .field("root", &self.root())
            .field("errors", &self.errors().len())
            .finish_non_exhaustive()
    }
}

/// Write the [`SyntaxTree`] view of the parse.
impl<S: AsRef<[u8]>> fmt::Debug for Parse<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.view().fmt(f)
    }
}

/// Write the node as `Kind@start..end`, the same as [`SyntaxTree::debug_tree`].
impl fmt::Debug for SyntaxNode<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{}", self.kind(), self.text_range())
    }
}

/// Write the token as `Kind@start..end "text"`, the same as
/// [`SyntaxTree::debug_tree`].
impl fmt::Debug for SyntaxToken<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}@{} \"{}\"",
            self.kind(),
            self.text_range(),
            self.text().escape_ascii()
        )
    }
}

impl<'t, 'a> SyntaxNode<'t, 'a> {
    #[inline]
    fn data(&self) -> &'t Node {
        &self.tree.data.nodes[self.idx as usize]
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
            return TextRange::span(0, self.tree.source.len() as u32);
        }
        let tokens = &self.tree.data.tokens;
        TextRange::span(
            tokens[node.first_token as usize].start,
            tokens[node.token_end as usize - 1].end(),
        )
    }

    /// The source bytes this node spans (a lossless slice of the subtree).
    pub fn text(&self) -> &'a [u8] {
        &self.tree.source[self.text_range().to_range()]
    }

    /// The parent node, or `None` for the root.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = self.tree.parents().nodes[self.idx as usize];
        (p != NO_PARENT).then_some(SyntaxNode {
            tree: self.tree,
            idx: p,
        })
    }

    /// The [`NodeFlags`] of this node and its whole subtree. The parser
    /// calculates them, so this call does not walk the subtree.
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
    pub fn has_calc(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_CALC)
    }

    /// Whether this subtree contains a `$macro$` parameter (O(1); see [`NodeFlags`]).
    pub fn has_macro(&self) -> bool {
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
    pub fn child_nodes(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        self.significant_children().filter_map(|el| el.as_node())
    }

    /// Direct child tokens only, trivia included.
    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + use<'t, 'a> {
        self.children().filter_map(|el| el.as_token())
    }

    /// Direct significant child tokens only.
    pub fn significant_child_tokens(
        &self,
    ) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + use<'t, 'a> {
        self.significant_children().filter_map(|el| el.as_token())
    }

    #[inline]
    fn node(&self, idx: u32) -> SyntaxNode<'t, 'a> {
        SyntaxNode {
            tree: self.tree,
            idx,
        }
    }

    /// This node and all the nodes in its subtree, in pre-order.
    ///
    /// The nodes of a subtree are adjacent in the tree, so this iterator does
    /// not allocate. To get one type of node, cast each node:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, AstNode, Field};
    ///
    /// let parse = parse(b"a = { b = 1 c = { d = 2 } }");
    /// let keys: Vec<&[u8]> = parse
    ///     .root()
    ///     .descendants()
    ///     .filter_map(Field::cast)
    ///     .map(|f| f.key().text())
    ///     .collect();
    /// assert_eq!(keys, [&b"a"[..], b"b", b"c", b"d"]);
    /// ```
    pub fn descendants(
        &self,
    ) -> impl DoubleEndedIterator<Item = SyntaxNode<'t, 'a>> + ExactSizeIterator + use<'t, 'a> {
        let node = *self;
        (self.idx..self.data().subtree_end).map(move |idx| node.node(idx))
    }

    /// An [`Enter`](WalkEvent::Enter) and a [`Leave`](WalkEvent::Leave) event
    /// for each node of the subtree, in pre-order. Use the events to keep a
    /// depth or a stack of scopes.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, WalkEvent};
    ///
    /// let parse = parse(b"a = { b = 1 }");
    /// let mut depth = 0;
    /// let mut max = 0;
    /// for event in parse.root().preorder() {
    ///     match event {
    ///         WalkEvent::Enter(_) => depth += 1,
    ///         WalkEvent::Leave(_) => depth -= 1,
    ///     }
    ///     max = max.max(depth);
    /// }
    /// assert_eq!((depth, max), (0, 4)); // Root, Field, Block, Field
    /// ```
    pub fn preorder(&self) -> Preorder<'t, 'a> {
        Preorder {
            node: *self,
            next: Some(WalkEvent::Enter(self.idx)),
        }
    }

    /// This node and its ancestors, from this node to the root.
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        std::iter::successors(Some(*self), |node| node.parent())
    }

    /// The first child node.
    pub fn first_child(&self) -> Option<SyntaxNode<'t, 'a>> {
        let child = self.idx + 1;
        (child < self.data().subtree_end).then(|| self.node(child))
    }

    /// The last child node.
    pub fn last_child(&self) -> Option<SyntaxNode<'t, 'a>> {
        let mut child = self.first_child()?;
        while let Some(next) = child.next_sibling() {
            child = next;
        }
        Some(child)
    }

    /// The next node that has the same parent.
    pub fn next_sibling(&self) -> Option<SyntaxNode<'t, 'a>> {
        let parent = self.parent()?;
        let next = self.data().subtree_end;
        (next < parent.data().subtree_end).then(|| self.node(next))
    }

    /// The previous node that has the same parent. This walks the siblings
    /// before this node, so it takes linear time.
    pub fn prev_sibling(&self) -> Option<SyntaxNode<'t, 'a>> {
        let mut child = self.parent()?.first_child()?;
        let mut prev = None;
        while child.idx != self.idx {
            prev = Some(child);
            child = self.node(child.data().subtree_end);
        }
        prev
    }

    /// The first leaf of the subtree. Only the root can start with trivia.
    pub fn first_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        if self.idx == 0 {
            return self.tree.tokens().next();
        }
        Some(self.tree.token(self.data().first_token))
    }

    /// The last leaf of the subtree. Only the root can end with trivia.
    pub fn last_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        let tree = self.tree;
        if self.idx == 0 {
            let mut gap = tree.gap_before(tree.data.tokens.len() as u32);
            let mut last = None;
            while let Some(token) = gap.next_token(tree) {
                last = Some(token);
            }
            return last.or_else(|| {
                tree.data
                    .tokens
                    .len()
                    .checked_sub(1)
                    .map(|i| tree.token(i as u32))
            });
        }
        Some(tree.token(self.data().token_end - 1))
    }

    /// Every leaf of the subtree in document order, trivia included.
    pub fn tokens(&self) -> Tokens<'t, 'a> {
        if self.idx == 0 {
            return self.tree.tokens();
        }
        let node = self.data();
        Tokens {
            tree: self.tree,
            next: node.first_token,
            end: node.token_end,
            // A node does not own the gaps before and after it.
            prev_end: self.tree.data.tokens[node.first_token as usize].start,
            gap: Gap::EMPTY,
            pending: None,
            trailing_done: true,
        }
    }
}

/// An event of a [`SyntaxNode::preorder`] walk.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WalkEvent<T> {
    /// The walk goes into the node, before its children.
    Enter(T),
    /// The walk goes out of the node, after its children.
    Leave(T),
}

impl<T> WalkEvent<T> {
    /// Change the value of the event.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WalkEvent<U> {
        match self {
            WalkEvent::Enter(value) => WalkEvent::Enter(f(value)),
            WalkEvent::Leave(value) => WalkEvent::Leave(f(value)),
        }
    }
}

/// Iterator over the [`WalkEvent`]s of a subtree (see
/// [`SyntaxNode::preorder`]).
///
/// The walk uses the parent table of the tree, so it does not allocate after
/// the first parent query of the tree.
#[derive(Debug, Clone)]
pub struct Preorder<'t, 'a> {
    /// The root of the walk.
    node: SyntaxNode<'t, 'a>,
    next: Option<WalkEvent<u32>>,
}

impl<'t, 'a> Iterator for Preorder<'t, 'a> {
    type Item = WalkEvent<SyntaxNode<'t, 'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        let event = self.next.take()?;
        let nodes = &self.node.tree.data.nodes;
        self.next = match event {
            WalkEvent::Enter(idx) => {
                // In pre-order, the first child is the next node.
                if idx + 1 < nodes[idx as usize].subtree_end {
                    Some(WalkEvent::Enter(idx + 1))
                } else {
                    Some(WalkEvent::Leave(idx))
                }
            }
            WalkEvent::Leave(idx) if idx == self.node.idx => None,
            WalkEvent::Leave(idx) => {
                let parent = self.node.tree.parents().nodes[idx as usize];
                let sibling = nodes[idx as usize].subtree_end;
                if sibling < nodes[parent as usize].subtree_end {
                    Some(WalkEvent::Enter(sibling))
                } else {
                    Some(WalkEvent::Leave(parent))
                }
            }
        };
        Some(event.map(|idx| self.node.node(idx)))
    }
}

impl std::iter::FusedIterator for Preorder<'_, '_> {}

/// The leaves at an offset (see [`SyntaxTree::token_at_offset`]).
///
/// The value is also an iterator over the leaves, from left to right.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenAtOffset<T> {
    /// The offset is after the end of the source, or the source is empty.
    None,
    /// The offset is inside one leaf, or at the start or the end of the
    /// source.
    Single(T),
    /// The offset is between two leaves: the leaf that ends at the offset and
    /// the leaf that starts at it.
    Between(T, T),
}

impl<T> TokenAtOffset<T> {
    /// The leaf that ends at the offset, when there are two.
    pub fn left_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(value) | TokenAtOffset::Between(value, _) => Some(value),
        }
    }

    /// The leaf that starts at the offset, when there are two.
    pub fn right_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(value) | TokenAtOffset::Between(_, value) => Some(value),
        }
    }

    /// Change each value.
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> TokenAtOffset<U> {
        match self {
            TokenAtOffset::None => TokenAtOffset::None,
            TokenAtOffset::Single(value) => TokenAtOffset::Single(f(value)),
            TokenAtOffset::Between(left, right) => TokenAtOffset::Between(f(left), f(right)),
        }
    }
}

impl<T> Iterator for TokenAtOffset<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match std::mem::replace(self, TokenAtOffset::None) {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(value) => Some(value),
            TokenAtOffset::Between(left, right) => {
                *self = TokenAtOffset::Single(right);
                Some(left)
            }
        }
    }
}

impl<T> std::iter::FusedIterator for TokenAtOffset<T> {}

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
        if !self.trivia && self.tree.data.tokens[self.idx as usize].flags & TOKEN_ERROR != 0 {
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
        &self.tree.source[self.range.to_range()]
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

    /// The ancestors of this token, from its parent to the root.
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        std::iter::successors(self.parent(), |node| node.parent())
    }

    /// The next leaf in document order, trivia included.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"a = b");
    /// let first = parse.root().first_token().unwrap();
    /// let texts: Vec<&[u8]> = std::iter::successors(Some(first), |t| t.next_token())
    ///     .map(|t| t.text())
    ///     .collect();
    /// assert_eq!(texts, [&b"a"[..], b" ", b"=", b" ", b"b"]);
    /// ```
    pub fn next_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        let tree = self.tree;
        // For a significant token, the next gap is before the next token. For
        // trivia, it is the rest of the gap of this token.
        let next = if self.trivia { self.idx } else { self.idx + 1 };
        let mut gap = tree.gap_before(next);
        gap.start = self.range.end;
        gap.next_token(tree)
            .or_else(|| ((next as usize) < tree.data.tokens.len()).then(|| tree.token(next)))
    }

    /// The previous leaf in document order, trivia included.
    pub fn prev_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        let tree = self.tree;
        // The gap before this token: all of it for a significant token, the
        // part before this token for trivia.
        let mut gap = tree.gap_before(self.idx);
        gap.end = self.range.start;
        let mut last = None;
        while let Some(token) = gap.next_token(tree) {
            last = Some(token);
        }
        last.or_else(|| self.idx.checked_sub(1).map(|prev| tree.token(prev)))
    }

    /// If this is an [`SyntaxKind::Operator`] token, the concrete [`Operator`].
    pub fn op(&self) -> Option<Operator> {
        if self.kind() != SyntaxKind::Operator {
            return None;
        }
        operator(self.text())
    }
}

/// The operator that `text` spells.
#[inline]
fn operator(text: &[u8]) -> Option<Operator> {
    Some(match *text {
        [b'='] => Operator::Equal,
        [b'<'] => Operator::LessThan,
        [b'>'] => Operator::GreaterThan,
        [b'<', b'='] => Operator::LessThanEqual,
        [b'>', b'='] => Operator::GreaterThanEqual,
        [b'=', b'='] => Operator::Exact,
        [b'!', b'='] => Operator::NotEqual,
        [b'?', b'='] => Operator::Exists,
        _ => return None,
    })
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

    /// The parent node, or `None` for the root.
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        match self {
            SyntaxElement::Node(n) => n.parent(),
            SyntaxElement::Token(t) => t.parent(),
        }
    }

    /// The ancestors of this element, from its parent to the root.
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        std::iter::successors(self.parent(), |node| node.parent())
    }

    /// Unwrap to a node, or `None` if this is a token.
    pub fn as_node(&self) -> Option<SyntaxNode<'t, 'a>> {
        match *self {
            SyntaxElement::Node(n) => Some(n),
            SyntaxElement::Token(_) => None,
        }
    }

    /// Unwrap to a token, or `None` if this is a node.
    pub fn as_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        match *self {
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
    fn next_token<'t, 'a>(&mut self, tree: SyntaxTree<'t, 'a>) -> Option<SyntaxToken<'t, 'a>> {
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
    tree: SyntaxTree<'t, 'a>,
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
                let el = match tree.data.nodes.get(self.next_child as usize) {
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
                let start = tree.data.tokens[first as usize].start;
                if self.started || self.is_root {
                    self.gap = Gap {
                        start: self.prev_end,
                        end: start,
                        next: first,
                    };
                }
                self.started = true;
                self.prev_end = tree.data.tokens[self.next_token as usize - 1].end();
                self.pending = Some(el);
                continue;
            }
            if self.trivia && self.is_root && !self.trailing_done {
                // The root owns the gap after the last token.
                self.trailing_done = true;
                self.gap = Gap {
                    start: self.prev_end,
                    end: self.tree.source.len() as u32,
                    next: self.tree.data.tokens.len() as u32,
                };
                continue;
            }
            return None;
        }
    }
}

impl std::iter::FusedIterator for Children<'_, '_> {}

/// Iterator over every leaf token of a tree, trivia included (see
/// [`SyntaxTree::tokens`]).
pub struct Tokens<'t, 'a> {
    tree: SyntaxTree<'t, 'a>,
    next: u32,
    /// The index after the last significant token to give.
    end: u32,
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
            if self.next < self.end {
                let index = self.next;
                self.next += 1;
                let token = tree.data.tokens[index as usize];
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

impl std::iter::FusedIterator for Tokens<'_, '_> {}

/// A typed view over a [`SyntaxNode`] of a particular [`SyntaxKind`].
///
/// Mirrors rust-analyzer's generated AST: [`AstNode::cast`] succeeds only when
/// the node's kind matches, and [`AstNode::syntax`] recovers the untyped node.
///
/// Use `cast` to go from the untyped cursors to the typed view, and `syntax`
/// to go back:
///
/// ```
/// use jomini::text::lossless::{parse, AstNode, Field, SyntaxKind};
///
/// let parse = parse(b"a = 1");
/// let node = parse.root().child_nodes().next().unwrap();
/// let field = Field::cast(node).unwrap();
/// assert_eq!(field.key().text(), b"a");
/// assert_eq!(field.syntax().kind(), SyntaxKind::Field);
/// assert!(Field::cast(parse.root()).is_none());
/// ```
pub trait AstNode<'t, 'a>: Sized {
    /// Wrap `node` if its kind matches this type, otherwise `None`.
    fn cast(node: SyntaxNode<'t, 'a>) -> Option<Self>;
    /// The underlying untyped node.
    fn syntax(&self) -> SyntaxNode<'t, 'a>;
}

macro_rules! ast_nodes {
    ($($(#[$m:meta])* $name:ident => $kind:ident),+ $(,)?) => {$(
        $(#[$m])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    ///
    /// The field keeps its operator, so a tool can tell `>=` from `=`. A field
    /// can also have no operator, as in `key { ... }`:
    ///
    /// ```
    /// use jomini::text::{Operator, lossless::parse};
    ///
    /// let parse = parse(b"num_of_cities >= 5 modifier { tax = 1 }");
    /// let fields: Vec<_> = parse.ast().fields().collect();
    ///
    /// assert_eq!(fields[0].op(), Some(Operator::GreaterThanEqual));
    /// assert_eq!(fields[0].op_token().unwrap().text(), b">=");
    /// assert_eq!(fields[0].value().unwrap().to_i64(), Some(5));
    ///
    /// assert_eq!(fields[1].key().text(), b"modifier");
    /// assert_eq!(fields[1].op(), None);
    /// assert!(fields[1].value().unwrap().as_block().is_some());
    /// ```
    Field => Field,
    /// A `{ ... }` block ([`SyntaxKind::Block`]).
    ///
    /// A block can hold fields, bare values, or both:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, Item};
    ///
    /// let parse = parse(b"area = { color = { 1 2 3 } 169 170 }");
    /// let field = parse.ast().fields().next().unwrap();
    /// let block = field.value().unwrap().as_block().unwrap();
    ///
    /// let provinces: Vec<u64> = block.values().filter_map(|v| v.to_u64()).collect();
    /// assert_eq!(provinces, [169, 170]);
    ///
    /// let kinds: Vec<&str> = block
    ///     .items()
    ///     .map(|item| match item {
    ///         Item::Field(_) => "field",
    ///         Item::Value(_) => "value",
    ///         _ => "other",
    ///     })
    ///     .collect();
    /// assert_eq!(kinds, ["field", "value", "value"]);
    /// ```
    Block => Block,
    /// A tagged block in value position, such as `rgb { 1 2 3 }` in
    /// `color = rgb { 1 2 3 }` ([`SyntaxKind::HeaderedBlock`]). In item
    /// position, the same text is a [`Field`] with no operator.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"color = hsv { 0.5 0.2 0.8 }");
    /// let value = parse.ast().fields().next().unwrap().value().unwrap();
    /// let color = value.as_headered_block().unwrap();
    /// assert_eq!(color.header().unwrap().text(), b"hsv");
    ///
    /// let hsv: Vec<f64> = color.block().unwrap().values().filter_map(|v| v.to_f64()).collect();
    /// assert_eq!(hsv, [0.5, 0.2, 0.8]);
    /// ```
    HeaderedBlock => HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation ([`SyntaxKind::Calc`]).
    ///
    /// The parser makes an expression tree from the calculation, so a tool can
    /// examine or evaluate it:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, Expr};
    ///
    /// let parse = parse(b"cost = @[base * 2]");
    /// let value = parse.ast().fields().next().unwrap().value().unwrap();
    /// let calc = value.as_calc().unwrap();
    ///
    /// let Some(Expr::Binary(product)) = calc.expr() else {
    ///     panic!("expected a binary expression");
    /// };
    /// let Some(Expr::Ident(lhs)) = product.lhs() else { panic!() };
    /// let Some(Expr::Number(rhs)) = product.rhs() else { panic!() };
    /// assert_eq!(lhs.text(), b"base");
    /// assert_eq!(product.op_token().unwrap().text(), b"*");
    /// assert_eq!(rhs.text(), b"2");
    /// ```
    Calc => Calc,
    /// An EU4 conditional parameter block ([`SyntaxKind::Parameter`]).
    ///
    /// ```
    /// use jomini::text::lossless::{parse, AstNode};
    ///
    /// let parse = parse(b"[[scaled] add_prestige = $scaled$ ]");
    /// let item = parse.ast().items().next().unwrap();
    /// let parameter = item.as_parameter().unwrap();
    /// assert_eq!(parameter.name().unwrap().text(), b"scaled");
    ///
    /// let field = parameter.fields().next().unwrap();
    /// assert_eq!(field.key().text(), b"add_prestige");
    /// assert!(field.syntax().has_macro());
    /// ```
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

/// An item in a [`Root`], [`Block`], [`Parameter`], [`UndefinedParameter`], or [`Code`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
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
///
/// The coercions read a scalar value as a number or a boolean. They give
/// `None` for a value that is not a scalar, or that has a different type. To
/// know why a coercion failed, use the [`Scalar`] from
/// [`as_scalar`](Value::as_scalar), which gives a [`ScalarError`]. Use
/// [`syntax`](Value::syntax) or [`text_range`](Value::text_range) to find any
/// value in the source:
///
/// ```
/// use jomini::{Windows1252Encoding, text::lossless::parse};
///
/// let parse = parse(b"tax = 1.5 count = -3 core = yes name = \"Bretagne\" list = { 1 }");
/// let values: Vec<_> = parse.ast().fields().map(|f| f.value().unwrap()).collect();
///
/// assert_eq!(values[0].to_f64(), Some(1.5));
/// assert_eq!(values[1].to_i64(), Some(-3));
/// assert_eq!(values[1].to_u64(), None);
/// assert_eq!(values[2].to_bool(), Some(true));
/// assert_eq!(values[3].decode(Windows1252Encoding::new()).unwrap(), "Bretagne");
/// assert_eq!(values[4].to_f64(), None);
/// assert!(values[4].as_scalar().is_none());
/// assert!(values[1].as_scalar().unwrap().to_u64().is_err());
/// assert_eq!(values[4].syntax().text(), b"{ 1 }");
/// ```
///
/// [`ScalarError`]: crate::ScalarError
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Value<'t, 'a> {
    /// A scalar token: [`SyntaxKind::Unquoted`], [`SyntaxKind::Quoted`],
    /// [`SyntaxKind::Variable`], or [`SyntaxKind::MacroParam`].
    Scalar(SyntaxToken<'t, 'a>),
    /// A `{ ... }` block.
    Block(Block<'t, 'a>),
    /// A tagged block such as `rgb { 1 2 3 }`.
    HeaderedBlock(HeaderedBlock<'t, 'a>),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
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
    ///
    /// The function does not examine the kind of the token. Thus for a token
    /// that is not a scalar, such as `{`, the result holds the bytes of the
    /// token. Use [`SyntaxKind::is_scalar`] to examine the kind first.
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
    /// use jomini::{Windows1252Encoding, text::lossless::parse};
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

/// The items of a root or a block: the significant children without the
/// braces.
fn child_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
    node.significant_children().filter_map(item_from_element)
}

fn item_from_element<'t, 'a>(el: SyntaxElement<'t, 'a>) -> Option<Item<'t, 'a>> {
    if el.kind() == SyntaxKind::CloseBrace {
        return None;
    }
    parameter_body_item(el)
}

/// The closing bracket of the parameter body. The body closes with the last
/// bracket of the node. When that bracket is a missing token, the body is not
/// closed, and a `]` before it closes an inner bracket.
fn parameter_close_bracket<'t, 'a>(node: SyntaxNode<'t, 'a>) -> Option<SyntaxToken<'t, 'a>> {
    node.significant_child_tokens()
        .filter(|token| {
            matches!(
                token.kind(),
                SyntaxKind::CloseBracket | SyntaxKind::MissingCloseBracket
            )
        })
        .skip(1)
        .last()
        .filter(|token| token.kind() == SyntaxKind::CloseBracket)
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

fn parameter_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
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

/// Convert an element to an item. A `}` is an item, because a parameter body
/// can close a brace that an earlier parameter opens.
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

fn code_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
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
    /// Every top-level item, in source order (trivia skipped).
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        child_items(self.syntax())
    }

    /// The top-level `key = value` fields.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }
}

impl<'t, 'a> Field<'t, 'a> {
    /// The key scalar token (the left-hand side). The parser opens a field
    /// only at a scalar token, so each field has a key.
    ///
    /// The text of a quoted key includes the quotes. Use
    /// [`SyntaxToken::as_scalar`] to compare a key without them.
    pub fn key(&self) -> SyntaxToken<'t, 'a> {
        self.syntax()
            .significant_child_tokens()
            .next()
            .expect("a field starts with its key token")
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
        self.op_token().and_then(|t| t.op())
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
    /// Every item between the braces, in source order (trivia skipped).
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        child_items(self.syntax())
    }

    /// The `key = value` fields directly inside this block.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values (array elements) directly inside this block.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Whether the block has no entries (only braces, whitespace, comments).
    pub fn is_empty(&self) -> bool {
        self.items().next().is_none()
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

    /// The items in the parameter body.
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the parameter body.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Return the physical body closing bracket, if the source contains one.
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_close_bracket(self.syntax())
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

    /// The items in the parameter body.
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the parameter body.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Return the physical body closing bracket, if the source contains one.
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_close_bracket(self.syntax())
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

    /// The items in the code payload.
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        code_items(self.syntax())
    }

    /// The fields directly inside the code payload.
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the code payload.
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
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
    pub fn body(&self) -> impl Iterator<Item = SyntaxElement<'t, 'a>> + use<'t, 'a> {
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
    /// The untyped element of this item.
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Item::Field(field) => SyntaxElement::Node(field.syntax()),
            Item::Parameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Item::UndefinedParameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Item::Code(code) => SyntaxElement::Node(code.syntax()),
            Item::Interpolation(interpolation) => SyntaxElement::Node(interpolation.syntax()),
            Item::Value(value) => value.syntax(),
            Item::Other(el) => el,
        }
    }

    /// The byte range of this item in the source.
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// The [`Field`] if this item is one.
    pub fn as_field(&self) -> Option<Field<'t, 'a>> {
        match *self {
            Item::Field(f) => Some(f),
            _ => None,
        }
    }

    /// The [`Parameter`] if this item is one.
    pub fn as_parameter(&self) -> Option<Parameter<'t, 'a>> {
        match *self {
            Item::Parameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`UndefinedParameter`] if this item is one.
    pub fn as_undefined_parameter(&self) -> Option<UndefinedParameter<'t, 'a>> {
        match *self {
            Item::UndefinedParameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`Code`] if this item is one.
    pub fn as_code(&self) -> Option<Code<'t, 'a>> {
        match *self {
            Item::Code(code) => Some(code),
            _ => None,
        }
    }

    /// The [`Interpolation`] if this item is one.
    pub fn as_interpolation(&self) -> Option<Interpolation<'t, 'a>> {
        match *self {
            Item::Interpolation(interpolation) => Some(interpolation),
            _ => None,
        }
    }

    /// The [`Value`] if this item is a bare value.
    pub fn as_value(&self) -> Option<Value<'t, 'a>> {
        match *self {
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
                SyntaxKind::HeaderedBlock => HeaderedBlock::cast(n).map(Value::HeaderedBlock),
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

    /// The untyped element of this value.
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Value::Scalar(token) => SyntaxElement::Token(token),
            Value::Block(block) => SyntaxElement::Node(block.syntax()),
            Value::HeaderedBlock(block) => SyntaxElement::Node(block.syntax()),
            Value::Calc(calc) => SyntaxElement::Node(calc.syntax()),
            Value::Parameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Value::UndefinedParameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Value::Code(code) => SyntaxElement::Node(code.syntax()),
            Value::Interpolation(interpolation) => SyntaxElement::Node(interpolation.syntax()),
        }
    }

    /// The byte range of this value in the source.
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// The scalar token, if this is [`Value::Scalar`].
    pub fn as_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        match *self {
            Value::Scalar(token) => Some(token),
            _ => None,
        }
    }

    /// The scalar value of the token, if this is [`Value::Scalar`]. The
    /// quotes of a quoted string are removed (see [`SyntaxToken::as_scalar`]).
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

    /// The headered block, if this is [`Value::HeaderedBlock`].
    pub fn as_headered_block(&self) -> Option<HeaderedBlock<'t, 'a>> {
        match self {
            Value::HeaderedBlock(h) => Some(*h),
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
    /// The untyped element of this expression.
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Expr::Binary(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Unary(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Paren(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Number(token) | Expr::Ident(token) => SyntaxElement::Token(token),
        }
    }

    /// The byte range of this expression in the source.
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

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

impl<'t, 'a> SyntaxTree<'t, 'a> {
    /// The typed [`Root`] of the document.
    pub fn ast(&self) -> Root<'t, 'a> {
        Root::cast(self.root()).expect("the root node is always SyntaxKind::Root")
    }
}

/// Configuration for [`SyntaxTree::format`].
///
/// ```
/// use jomini::text::lossless::{parse, FormatOptions};
///
/// let mut opts = FormatOptions::default();
/// opts.indent = String::from("    ");
/// let parse = parse(b"a = { b = 1 }");
/// assert_eq!(parse.format(&opts), b"a = {\n    b = 1\n}\n");
/// ```
#[derive(Debug, Clone)]
#[non_exhaustive]
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

/// Parse `source` and reformat it with the default [`FormatOptions`]. See
/// [`SyntaxTree::format`] for the style.
///
/// ```
/// use jomini::text::lossless::format;
///
/// let source = b"building={cost=100 # gold\nsize={1 2}}";
/// let expected = "building = {\n\tcost = 100 # gold\n\tsize = { 1 2 }\n}\n";
/// assert_eq!(String::from_utf8(format(source)).unwrap(), expected);
/// ```
#[must_use]
pub fn format(source: &[u8]) -> Vec<u8> {
    parse(source).format(&FormatOptions::default())
}

impl<'t, 'a> SyntaxTree<'t, 'a> {
    /// Reformat the tree with the given [`FormatOptions`].
    ///
    /// The formatter changes only the whitespace. It writes each significant
    /// token, each comment, and the BOM with the same bytes. Thus a parse of
    /// the output gives the same significant tokens, and a second format
    /// makes no change.
    ///
    /// The style is:
    ///
    /// - Each entry is on its own line. Each nesting level adds one
    ///   [`indent`](FormatOptions::indent).
    /// - A field is `key = value`, with one space on each side of the
    ///   operator.
    /// - A block that has only scalars stays on one line (`{ 1 2 3 }`). A
    ///   block with fields, blocks, or comments has one entry on each line. An
    ///   empty block becomes `{}`.
    /// - A comment always ends its line, so it cannot hide the token after it.
    ///   A comment inside a field moves to a line before the field. A quoted
    ///   string that a newline closes also ends its line.
    /// - One blank line between entries stays. More blank lines become one.
    /// - The document ends with one newline, unless the last token is a
    ///   quoted string with no closing quote.
    ///
    /// The formatter writes parameter, code, calc, and `Bogus` nodes with
    /// their source bytes, and each loose token on its own line.
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

/// Count the line breaks in `bytes`. A line break is `\n`, `\r\n`, or a `\r`
/// alone, the same as for the lexer.
fn count_newlines(bytes: &[u8]) -> usize {
    let lf = bytes.iter().filter(|&&b| b == b'\n').count();
    let lone_cr = bytes
        .iter()
        .enumerate()
        .filter(|&(i, &b)| b == b'\r' && bytes.get(i + 1) != Some(&b'\n'))
        .count();
    lf + lone_cr
}

/// Return `false` when `text` starts with a `"` that has no closing `"`. A
/// backslash escapes the next byte, as in the lexer.
///
/// The lexer uses this to find a string that continues to the end of the
/// source. The formatter uses it because such a string, or a string that a
/// newline closes, takes the bytes that follow it.
fn quote_is_closed(text: &[u8]) -> bool {
    if text.first() != Some(&b'"') {
        return true;
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
    // A quoted string that a newline closes (`Flavor::newline_terminated_strings`)
    // must stay at the end of its line.
    el.kind().is_scalar() && quote_is_closed(el.text())
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
        let tokens = &tree.data.tokens[data.first_token as usize..data.token_end as usize];
        let Some(last) = tokens.iter().rev().find(|t| !t.kind.is_missing()) else {
            return;
        };
        let mut gap = Gap {
            start: last.end(),
            end: node.text_range().end,
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
                    && !quote_is_closed(&tree.source[last.range().to_range()]);
            }
        }
    }

    /// Emit a newline (two for a preserved blank line), reopening the line.
    fn line_break(&mut self, blank: bool) {
        // An open quoted string can end with an escaped `\n`. That byte is
        // part of the string, so it does not end the line.
        if self.quote_open || self.out.last() != Some(&b'\n') {
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
            _ => {
                // The formatter removes leading whitespace. If the first token
                // starts with the bytes of a BOM, the lexer would then read
                // them as a BOM. Keep one space before the token.
                let first = root.tree.significant_tokens().next();
                if first.is_some_and(|t| t.text().starts_with(BOM)) {
                    self.push_byte(b' ');
                }
                0
            }
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
                    let trailing =
                        !first && !ws_had_newline && !self.comment_open && !self.quote_open;
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
                        if self.quote_open {
                            // A newline closes this quoted string. A space
                            // would put the next token inside it.
                            self.line_break(false);
                            self.write_indent(indent);
                        } else {
                            self.push_byte(b' ');
                        }
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

    fn error_kinds<S: AsRef<[u8]>>(parse: &Parse<S>) -> Vec<SyntaxErrorKind> {
        parse.errors().iter().map(|e| e.kind).collect()
    }

    /// The fast [`Sink::scalar_field`] of the builder must make the same
    /// nodes and flags as the four separate events.
    #[quickcheck]
    fn scalar_field_matches_events(kinds: (u8, u8), flags: (u8, u8, u8), nested: bool) -> bool {
        const KINDS: &[SyntaxKind] = &[
            SyntaxKind::Unquoted,
            SyntaxKind::Quoted,
            SyntaxKind::Variable,
            SyntaxKind::MacroParam,
        ];
        let token = |kind, flags| Token {
            start: 0,
            len: 1,
            kind,
            flags: flags & 3,
        };
        let key = token(KINDS[kinds.0 as usize % KINDS.len()], flags.0);
        let op = token(SyntaxKind::Operator, flags.1);
        let value = token(KINDS[kinds.1 as usize % KINDS.len()], flags.2);
        let mut actual = Builder::with_capacity(16);
        let mut expected = Builder::with_capacity(16);
        for builder in [&mut actual, &mut expected] {
            builder.start_node(SyntaxKind::Root);
            if nested {
                builder.start_node(SyntaxKind::Block);
                builder.token(token(SyntaxKind::OpenBrace, 0));
            }
        }
        actual.scalar_field(key, op, value);
        expected.start_node(SyntaxKind::Field);
        expected.token(key);
        expected.token(op);
        expected.token(value);
        expected.finish_node();
        for builder in [&mut actual, &mut expected] {
            while !builder.stack.is_empty() {
                builder.finish_node();
            }
        }
        let nodes = |builder: Builder| {
            builder
                .nodes
                .into_iter()
                .map(|node| {
                    (
                        node.kind,
                        node.flags,
                        node.first_token,
                        node.token_end,
                        node.subtree_end,
                    )
                })
                .collect::<Vec<_>>()
        };
        nodes(actual) == nodes(expected)
    }

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
        let mut at = TextSize::new(0);
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
        assert_eq!(
            at.to_usize(),
            data.len(),
            "tokens do not reach end of input"
        );
    }

    /// Check the navigation API against the iterators: each query must agree
    /// with a walk over `tokens()`, `children()`, and the node tape.
    fn assert_navigation(data: &[u8], flavor: Flavor) {
        let parse = parse_with(data, flavor);
        let tree = parse.tree();
        let root = tree.root();
        let leaves: Vec<SyntaxToken<'_, '_>> = tree.tokens().collect();

        // `next_token` and `prev_token` walk the same leaves as `tokens()`.
        let forward: Vec<_> =
            std::iter::successors(root.first_token(), |t| t.next_token()).collect();
        assert_eq!(forward, leaves, "next_token in {data:?}");
        let mut backward: Vec<_> =
            std::iter::successors(root.last_token(), |t| t.prev_token()).collect();
        backward.reverse();
        assert_eq!(backward, leaves, "prev_token in {data:?}");

        // Each byte belongs to exactly one leaf.
        for leaf in leaves.iter().filter(|t| !t.text_range().is_empty()) {
            let range = leaf.text_range();
            for offset in range.start().to_u32()..range.end().to_u32() {
                let at = tree.token_at_offset(TextSize::new(offset));
                assert_eq!(
                    at.right_biased(),
                    Some(*leaf),
                    "offset {offset} in {data:?}"
                );
                if offset > range.start().to_u32() {
                    assert_eq!(at, TokenAtOffset::Single(*leaf));
                }
            }
            assert_eq!(tree.covering_element(range), SyntaxElement::Token(*leaf));
        }
        assert_eq!(
            tree.token_at_offset(TextSize::of(data) + TextSize::new(1)),
            TokenAtOffset::None
        );

        // `descendants` and the `Enter` events of `preorder` give every node in
        // pre-order, and each `Enter` has a `Leave` in nested order.
        let nodes: Vec<_> = root.descendants().collect();
        assert_eq!(nodes.len(), tree.data.nodes.len());
        let mut stack = Vec::new();
        let mut entered = Vec::new();
        for event in root.preorder() {
            match event {
                WalkEvent::Enter(node) => {
                    if let Some(&parent) = stack.last() {
                        assert_eq!(node.parent(), Some(parent));
                    }
                    stack.push(node);
                    entered.push(node);
                }
                WalkEvent::Leave(node) => assert_eq!(stack.pop(), Some(node)),
            }
        }
        assert!(stack.is_empty());
        assert_eq!(entered, nodes);

        for node in nodes {
            let children: Vec<_> = node.child_nodes().collect();
            assert_eq!(node.first_child(), children.first().copied());
            assert_eq!(node.last_child(), children.last().copied());
            for pair in children.windows(2) {
                assert_eq!(pair[0].next_sibling(), Some(pair[1]));
                assert_eq!(pair[1].prev_sibling(), Some(pair[0]));
            }
            if let (Some(first), Some(last)) = (children.first(), children.last()) {
                assert_eq!(first.prev_sibling(), None);
                assert_eq!(last.next_sibling(), None);
            }
            assert_eq!(node.ancestors().next(), Some(node));
            assert_eq!(node.ancestors().last(), Some(root));

            // The leaves of a subtree are the leaves inside its range.
            let own: Vec<_> = node.tokens().collect();
            let inside: Vec<_> = leaves
                .iter()
                .copied()
                .filter(|t| node.text_range().contains_range(t.text_range()))
                .filter(|t| node == root || t.ancestors().any(|a| a == node))
                .collect();
            assert_eq!(own, inside, "tokens of {node:?} in {data:?}");
            assert_eq!(node.first_token(), own.first().copied());
            assert_eq!(node.last_token(), own.last().copied());
            let text: Vec<u8> = own.iter().flat_map(|t| t.text()).copied().collect();
            assert_eq!(text, node.text());

            // No child covers the range of the element that covers a node.
            let range = node.text_range();
            let cover = tree.covering_element(range);
            assert!(cover.text_range().contains_range(range));
            if let SyntaxElement::Node(cover) = cover {
                assert!(
                    cover
                        .children()
                        .all(|c| !c.text_range().contains_range(range) || range.is_empty()),
                    "covering_element of {range:?} in {data:?}"
                );
            }
        }
    }

    #[quickcheck]
    fn prop_navigation(data: Vec<u8>) -> bool {
        assert_navigation(&data, Flavor::default());
        assert_navigation(&data, Flavor::hoi4());
        true
    }

    #[quickcheck]
    fn prop_navigation_alphabet(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"ab{}[]()=<>#\"@$ \t\n\r";
        let data: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        assert_navigation(&data, Flavor::default());
        assert_navigation(&data, Flavor::hoi4());
        true
    }

    #[test]
    fn navigation_examples() {
        for source in [
            &b""[..],
            b"   ",
            b"\xef\xbb\xbf# c\n",
            b"a = { b = 1 c = { d = 2 } } # end",
            b"x = { \"unclosed",
            b"a = { [[x] b = $X$ ] @[1 + 2] }",
        ] {
            assert_navigation(source, Flavor::default());
        }
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
    fn owned_parse() {
        fn assert_send_sync<T: Send + Sync + 'static>(_: &T) {}

        let parse = Parse::owned(std::sync::Arc::<[u8]>::from(&b"a = { b = 1 }"[..]));
        assert_send_sync(&parse);
        let count = std::thread::spawn(move || parse.root().children().count())
            .join()
            .unwrap();
        assert_eq!(count, 1);

        let parse = Parse::owned(b"a = 1".to_vec());
        assert_eq!(parse.reconstruct(), b"a = 1");
        assert_eq!(parse.into_source(), b"a = 1");
    }

    #[test]
    fn unclosed_quote() {
        let source = b"a = \"x\nb = 2";
        let tree = parse(source);
        let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
        assert_eq!(kinds, vec![SyntaxErrorKind::UnclosedQuote]);
        assert_eq!(tree.errors()[0].range, TextRange::span(4, 5));
        assert!(tree.root().has_error());
        assert_eq!(tree.reconstruct(), source);

        let quoted = tree
            .tokens()
            .find(|t| t.kind() == SyntaxKind::Quoted)
            .unwrap();
        assert!(quoted.has_error());

        // An escaped quote or a trailing backslash does not close the string.
        for source in [&b"a = \"x\\\""[..], b"a = \"x\\"] {
            let kinds: Vec<_> = parse(source).errors().iter().map(|e| e.kind).collect();
            assert_eq!(kinds, vec![SyntaxErrorKind::UnclosedQuote]);
        }

        // The error marks each node that holds the string.
        let tree = parse(b"a = { b = \"x");
        let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
        assert!(kinds.contains(&SyntaxErrorKind::UnclosedQuote));
        let block = tree
            .root()
            .child_nodes()
            .find_map(|n| n.child_nodes().find(|n| n.kind() == SyntaxKind::Block))
            .unwrap();
        assert!(block.has_error());

        // Closed strings have no error.
        assert!(parse(b"a = \"x\"").errors().is_empty());
        assert!(parse(b"a = \"x\\\"y\"").errors().is_empty());

        // With HoI4, a line break ends a string, so only a quote at the end of
        // the source is unclosed.
        assert!(
            parse_with(b"a = \"x\nb = 2", Flavor::hoi4())
                .errors()
                .is_empty()
        );
        let kinds: Vec<_> = parse_with(b"a = \"x", Flavor::hoi4())
            .errors()
            .iter()
            .map(|e| e.kind)
            .collect();
        assert_eq!(kinds, vec![SyntaxErrorKind::UnclosedQuote]);
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
        assert_eq!(recovery.opening_range, TextRange::span(4, 5));
        assert_eq!(
            recovery.insertion_range,
            TextRange::empty(TextSize::of(source))
        );

        let missing = tree.tokens().find(|token| token.is_missing()).unwrap();
        assert_eq!(missing.kind(), SyntaxKind::MissingCloseBrace);
        assert_eq!(missing.text(), b"");
        assert_eq!(missing.text_range(), TextRange::empty(TextSize::of(source)));
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
            tree.repairs(),
            [Repair {
                range: TextRange::empty(TextSize::new(7)),
                replacement: "}".into()
            }]
        );
        assert_eq!(tree.repaired_source(), b"a = { b}");
        assert!(parse(&tree.repaired_source()).errors().is_empty());
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
        assert_eq!(tree.repairs()[0].replacement, "}}".to_string());
        assert_eq!(
            tree.repaired_source(),
            b"outer = { clean = {} bad = { value}}"
        );
        assert!(parse(&tree.repaired_source()).errors().is_empty());

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
        for (source, kinds) in [
            (
                b"a = { b ]".as_slice(),
                [
                    SyntaxErrorKind::UnmatchedCloseBracket,
                    SyntaxErrorKind::UnclosedBlock,
                ],
            ),
            (
                b"a = { b = @[ (1]".as_slice(),
                [
                    SyntaxErrorKind::UnclosedCalcParen,
                    SyntaxErrorKind::UnclosedBlock,
                ],
            ),
        ] {
            let tree = parse(source);
            assert_eq!(error_kinds(&tree), kinds, "{source:?}");
            assert!(tree.repairs().is_empty(), "unexpected fix for {source:?}");
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
        assert_eq!(
            error_kinds(&interrupted_calc),
            [SyntaxErrorKind::UnclosedCalc]
        );
        assert!(interrupted_calc.repairs().is_empty());

        let ambiguous_bracket = parse(b"a = { [");
        assert!(ambiguous_bracket.repairs().is_empty());
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
            assert_eq!(tree.repairs()[0].replacement.as_bytes(), replacement);
            assert!(
                parse(&tree.repaired_source()).errors().is_empty(),
                "source={source:?}"
            );
        }

        for source in [
            b"a = { \"unterminated".as_slice(),
            b"a = { \"escaped\\\"".as_slice(),
        ] {
            let tree = parse(source);
            assert!(tree.repairs().is_empty());
            assert!(tree.errors().iter().any(|error| {
                error.recovery.as_ref().is_some_and(|recovery| {
                    tree.repair_applicability(recovery) == Applicability::MaybeIncorrect
                })
            }));
        }
    }

    #[test]
    fn unclosed_parameter_with_inner_bracket_is_not_closed() {
        let tree = parse(b"[[x] [ } ]");
        let parameter = tree
            .ast()
            .items()
            .find_map(|item| item.as_parameter())
            .unwrap();
        assert!(!parameter.is_closed_in_source(), "{}", tree.debug_tree());

        let tree = parse(b"[[!x] [ } ]");
        let parameter = tree
            .ast()
            .items()
            .find_map(|item| match item {
                Item::UndefinedParameter(value) => Some(value),
                _ => None,
            })
            .unwrap();
        assert!(!parameter.is_closed_in_source(), "{}", tree.debug_tree());

        let tree = parse(b"[[x] a = b ]");
        let parameter = tree
            .ast()
            .items()
            .find_map(|item| item.as_parameter())
            .unwrap();
        assert!(parameter.is_closed_in_source());
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
        assert_eq!(parameter.repairs()[0].replacement, "]");
        assert!(parse(&parameter.repaired_source()).errors().is_empty());

        let header = parse(b"[[name");
        assert!(
            header
                .ast()
                .items()
                .any(|item| matches!(item, Item::Parameter(_)))
        );
        assert_eq!(header.repairs()[0].replacement, "]]".to_string());
        assert!(parse(&header.repaired_source()).errors().is_empty());

        let one = parse(b"code [[value]");
        assert_eq!(one.repairs()[0].replacement, "]");
        assert_eq!(one.tokens().filter(|token| token.is_missing()).count(), 1);
        assert!(parse(&one.repaired_source()).errors().is_empty());

        let both = parse(b"code [[value");
        assert_eq!(both.repairs()[0].replacement, "]]".to_string());
        assert_eq!(both.tokens().filter(|token| token.is_missing()).count(), 2);
        assert!(parse(&both.repaired_source()).errors().is_empty());
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
        assert_eq!(tree.repairs()[0].replacement, ")]}");
        assert!(parse(&tree.repaired_source()).errors().is_empty());
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
        assert_eq!(tree.repairs()[0].replacement, "]");
        assert_eq!(tree.repaired_source(), b"value = [ROOT.GetName]");
        assert!(parse(&tree.repaired_source()).errors().is_empty());
    }

    #[test]
    fn unmatched_close_brace_is_a_bogus_node() {
        let tree = parse(b"x } y");
        assert_eq!(error_kinds(&tree), [SyntaxErrorKind::UnmatchedCloseBrace]);
        let bogus: Vec<_> = tree
            .root()
            .child_nodes()
            .filter(|n| n.kind() == SyntaxKind::Bogus)
            .collect();
        assert_eq!(bogus.len(), 1);
        assert_eq!(bogus[0].text(), b"}");
        assert!(bogus[0].has_error());
        assert!(tree.root().has_error());
    }

    #[test]
    fn bang_equal_splits_from_a_scalar() {
        let tree = parse(b"a!=1");
        let field = tree.ast().fields().next().unwrap();
        assert_eq!(field.key().text(), b"a");
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
            assert!(tree.repairs().is_empty());
            assert_eq!(tree.reconstruct(), source);
        }

        // A `[` in a parameter body is an ordinary bracket.
        assert!(parse(b"[[x] a = [ ] ]").errors().is_empty());
    }

    #[test]
    fn stray_close_bracket_keeps_block_structure() {
        let source = b"a = { pos = { 0.5 0.9] } scale = { 1 } } b = c";
        let tree = parse(source);
        let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
        assert_eq!(kinds, vec![SyntaxErrorKind::UnmatchedCloseBracket]);
        let fields: Vec<_> = tree.ast().fields().collect();
        assert_eq!(fields.len(), 2);
        let block = fields[0].value().unwrap().as_block().unwrap();
        assert!(block.is_closed_in_source());
        assert_eq!(block.fields().count(), 2);
        assert_eq!(tree.reconstruct(), source);

        // The `]` refuses the virtual `}` at EOF.
        let tree = parse(b"a = { b ] c");
        assert!(tree.repairs().is_empty());
    }

    #[test]
    fn close_bracket_key_is_a_field() {
        let source = b"active_idea_groups = { ]=0 defensive_ideas=2 }";
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        let field = tree.ast().fields().next().unwrap();
        let block = field.value().unwrap().as_block().unwrap();
        let keys: Vec<_> = block.fields().map(|f| f.key().text().to_vec()).collect();
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
                .any(|e| e.kind == SyntaxErrorKind::BlockDepthExceeded)
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
                .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
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
                .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
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
                .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
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
        assert_eq!(field.key().text(), b"$P$");
        assert_eq!(
            field.value().unwrap().as_scalar().unwrap().as_bytes(),
            b"$Q$"
        );
    }

    #[test]
    fn ast_field_key_op_value() {
        let tree = parse(b"name = \"Ragusa\"");
        let field = tree.ast().fields().next().unwrap();
        assert_eq!(field.key().text(), b"name");
        assert_eq!(field.op(), Some(Operator::Equal));
        let value = field.value().unwrap();
        // Quoted value: as_scalar() strips the surrounding quotes.
        assert_eq!(value.as_scalar().unwrap().as_bytes(), b"Ragusa");
    }

    #[test]
    fn ast_block_items_fields_and_values() {
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
    fn ast_block_with_only_trivia_is_empty() {
        let tree = parse(b"a = {} b = { # note\n } c = { = }");
        let blocks: Vec<_> = tree
            .ast()
            .fields()
            .map(|f| f.value().unwrap().as_block().unwrap())
            .collect();
        assert!(blocks[0].is_empty());
        assert!(blocks[1].is_empty());
        // A stray operator is an `Item::Other`, so the block is not empty.
        assert!(!blocks[2].is_empty());
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
            .as_headered_block()
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
            .as_headered_block()
            .unwrap();
        assert_eq!(custom.header().unwrap().text(), b"tag");
        assert_eq!(custom.block().unwrap().fields().count(), 1);
    }

    #[test]
    fn ast_reader_variable_definition_with_calc_value() {
        let tree = parse(b"@half = @[1 / 2]");
        let field = tree.ast().fields().next().unwrap();

        let key = field.key();
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
            .as_headered_block()
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
        let entries = block.items().collect::<Vec<_>>();
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
        assert_eq!(parameter.fields().next().unwrap().key().text(), b"a");
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
        assert_eq!(parameter.fields().next().unwrap().key().text(), b"a");
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
        assert_eq!(parameter.items().count(), 0);
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
        let entries = block.items().collect::<Vec<_>>();
        let [Item::Code(code)] = entries.as_slice() else {
            panic!("expected one code payload");
        };
        assert_eq!(code.syntax().kind(), SyntaxKind::Code);
        assert_eq!(code.keyword().unwrap().text(), b"code");
        assert_eq!(
            code.syntax().text(),
            b"code [[\n\tvalue = { name = wall }\n]]"
        );
        assert_eq!(code.fields().next().unwrap().key().text(), b"value");
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
        assert_eq!(code.key().text(), b"code");
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
            assert_eq!(code.items().count(), 0);
        }

        for source in [b"code = [[]]".as_slice(), b"code = [[ ]]".as_slice()] {
            let tree = parse(source);
            assert!(tree.errors().is_empty());
            assert_eq!(tree.reconstruct(), source);
            let field = tree.ast().fields().next().unwrap();
            assert_eq!(field.value().unwrap().as_code().unwrap().items().count(), 0);
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
        assert_eq!(error_kinds(&tree), [SyntaxErrorKind::UnclosedCode]);
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
            assert_eq!(
                error_kinds(&tree),
                [
                    SyntaxErrorKind::CodeMissingFirstBracket,
                    SyntaxErrorKind::CodeMissingSecondBracket,
                ]
            );
            assert_eq!(tree.repairs()[0].replacement, "]]");
        }
    }

    #[test]
    fn close_bracket_is_not_a_field_value() {
        for source in [b"[[p] a = ]\nb = 1".as_slice(), b"x = { code [[ a = ]] }"] {
            let tree = parse(source);
            assert_eq!(tree.reconstruct(), source);
            assert!(tree.errors().is_empty(), "{source:?}");
        }

        let tree = parse(b"[[p] a = ]\nb = 1");
        let parameter = tree
            .root()
            .child_nodes()
            .find(|node| node.kind() == SyntaxKind::Parameter)
            .unwrap();
        assert_eq!(parameter.text(), b"[[p] a = ]");
    }

    #[test]
    fn interpolation_close_is_not_half_of_code_close() {
        let tree = parse(b"x = { code [[ a[b]");
        let repaired = tree.repaired_source();
        assert_eq!(repaired, b"x = { code [[ a[b]]]}");
        assert!(parse(&repaired).errors().is_empty());
    }

    #[test]
    fn trailing_comment_flags_the_unclosed_block() {
        let tree = parse(b"a = { b = 1 # c");
        let field = tree.root().child_nodes().next().unwrap();
        let block = field
            .child_nodes()
            .find(|node| node.kind() == SyntaxKind::Block)
            .unwrap();
        assert!(field.has_comment());
        assert!(block.has_comment());
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
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.kind == SyntaxErrorKind::CodeDepthExceeded)
        );
    }

    #[test]
    fn parameter_recovery_keeps_following_block_delimiter() {
        let source = b"outer = { [[name] value = yes }";
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        assert_eq!(error_kinds(&tree), [SyntaxErrorKind::UnclosedParameter]);
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
                .items()
                .filter(|entry| matches!(entry, Item::Parameter(_)))
                .count(),
            2
        );
    }

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
    fn fmt_newline_terminated_string_ends_its_line() {
        let tokens = |src: &[u8]| -> Vec<(SyntaxKind, Vec<u8>)> {
            parse_with(src, Flavor::hoi4())
                .tokens()
                .filter(|t| !t.kind().is_missing() && !t.kind().is_trivia())
                .map(|t| (t.kind(), t.text().to_vec()))
                .collect()
        };
        for src in [
            &b"x = { \"abc\n 1 }"[..],
            b"x = { \"abc\n}",
            b"\"a\n= b",
            b"\"x\r#c",
            b"{ \"a\\\n\n}",
        ] {
            let out = parse_with(src, Flavor::hoi4()).format(&FormatOptions::default());
            assert_eq!(
                tokens(&out),
                tokens(src),
                "{}",
                String::from_utf8_lossy(&out)
            );
        }
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
    fn fmt_bom_bytes_in_first_token_stay_a_token() {
        let source = b" \xef\xbb\xbf = 1";
        let out = format(source);
        assert_eq!(significant(source), significant(&out));
        assert_eq!(format(&out), out);
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
    fn node_flags_summarize_subtrees() {
        // The `outer` block has a comment and a macro. Its `inner` block has
        // neither. Only the sibling field `c` has a calc.
        let tree = parse(b"outer = { # hi\n a = $P$ inner = { b = c } } c = @[1+2]");
        let root = tree.root();
        assert!(root.has_comment() && root.has_macro() && root.has_calc());
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
        assert!(outer_block.has_macro());
        assert!(!outer_block.has_calc());

        let inner = outer_block
            .child_nodes()
            .filter_map(|f| Field::cast(f)?.value()?.as_block())
            .map(|b| b.syntax())
            .next()
            .expect("the `inner` block");
        assert!(!inner.has_comment() && !inner.has_macro() && !inner.has_calc());

        let calc_field = fields.next().unwrap().syntax();
        assert!(calc_field.has_calc() && !calc_field.has_comment() && !calc_field.has_macro());
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
    fn calc_operands_are_single_tokens() {
        // `10.0f`, `.5`, `@var`, and `leopard_x` are each one operand token.
        assert_eq!(
            calc_shape(b"x = @[10.0f / @var]"),
            "Calc(CalcOpen BinaryExpr(Number Slash CalcIdent) CalcClose)"
        );
        assert_eq!(
            calc_shape(b"x = @[.5*leopard_x]"),
            "Calc(CalcOpen BinaryExpr(Number Star CalcIdent) CalcClose)"
        );
        // A `-` ends an identifier, so it is an operator and not a name byte.
        assert_eq!(
            calc_shape(b"x = @[a-b]"),
            "Calc(CalcOpen BinaryExpr(CalcIdent Minus CalcIdent) CalcClose)"
        );
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
        let source = b"x = @[1 + 2";
        let tree = parse(source);
        assert_eq!(error_kinds(&tree), [SyntaxErrorKind::UnclosedCalc]);
        // The expression is complete. Only the `]` is missing.
        assert_eq!(
            calc_shape(source),
            "Calc(CalcOpen BinaryExpr(Number Plus Number) MissingCalcClose)"
        );
        assert_eq!(tree.repairs()[0].replacement, "]");
        assert!(parse(&tree.repaired_source()).errors().is_empty());
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
            assert!(tree.repairs().is_empty());
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
            assert!(tree.repairs().is_empty());
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
            let mut previous_end = TextSize::new(0);
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
                    let mut closed = false;
                    while i < n {
                        match source[i] {
                            b'\\' => i = (i + 2).min(n),
                            b'"' => {
                                i += 1;
                                closed = true;
                                break;
                            }
                            b'\n' | b'\r' if flavor.newline_terminated_strings => break,
                            _ => i += 1,
                        }
                    }
                    if !closed && i >= n {
                        flags |= TOKEN_ERROR;
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
                let start = if source.starts_with(BOM) { 3 } else { 0 };
                let (_, flags) = lex_simd(
                    fearless_simd::Fallback::new(),
                    source,
                    flavor,
                    (start, 0),
                    usize::MAX,
                    &mut fallback,
                );
                let fallback_comment = flags & TOKEN_COMMENT_BEFORE != 0;
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

    /// Check the nibble tables of [`run_lanes`] against [`STOP`] for every
    /// byte, in the fallback and in the dispatched SIMD level.
    #[test]
    fn run_lanes_match_stop_table() {
        fn check<S: Simd>(simd: S) {
            for b in 0..=255u8 {
                let mut bytes = [b'a'; 16];
                bytes[b as usize % 16] = b;
                let (stops, dollars) = run_lanes(simd, u8x16::from_slice(simd, &bytes));
                let lane = 1u64 << (b as usize % 16);
                assert_eq!(stops == lane, STOP[b as usize], "stop byte {b:#04x}");
                assert_eq!(stops & !lane, 0, "stop byte {b:#04x}");
                assert_eq!(dollars, if b == b'$' { lane } else { 0 }, "byte {b:#04x}");
            }
        }
        check(fearless_simd::Fallback::new());
        dispatch!(Level::new(), simd => check(simd));
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

    /// The scan that [`resolve_interpolations`] replaces: from one `[`, count
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
        let mut starts = Vec::new();
        let mut resolved_until = 0;
        tokens.iter().enumerate().all(|(pos, token)| {
            if token.kind != SyntaxKind::OpenBracket {
                return true;
            }
            if pos >= resolved_until {
                resolved_until = resolve_interpolations(
                    pos,
                    |index| tokens.get(index).map(|t| t.kind),
                    &mut starts,
                );
            }
            starts.binary_search(&(pos as u32)).is_ok() == naive_interpolation_start(&tokens, pos)
        })
    }

    #[test]
    fn scalar_followed_by_block_is_a_field_without_operator() {
        let tree = parse(b"foo{bar=qux} color = { rgb { 1 2 3 } }");
        assert!(tree.errors().is_empty());
        let fields: Vec<_> = tree.ast().fields().collect();
        assert_eq!(fields.len(), 2);

        assert_eq!(fields[0].key().text(), b"foo");
        assert!(fields[0].op_token().is_none());
        assert!(fields[0].op().is_none());
        let block = fields[0].value().unwrap().as_block().unwrap();
        assert_eq!(block.fields().next().unwrap().key().text(), b"bar");

        // In item position, a header such as `rgb` is a field too. The
        // semantic layer decides that it is a header.
        let inner = fields[1].value().unwrap().as_block().unwrap();
        let rgb = inner.fields().next().unwrap();
        assert_eq!(rgb.key().text(), b"rgb");
        let values = rgb.value().unwrap().as_block().unwrap().values().count();
        assert_eq!(values, 3);

        // In value position, a header is still a headered block.
        let tree = parse(b"color = rgb { 1 2 3 }");
        let value = tree.ast().fields().next().unwrap().value().unwrap();
        assert!(value.as_headered_block().is_some());
    }

    #[test]
    fn macro_parameters_glue_to_the_surrounding_scalar() {
        for (source, kind) in [
            (b"$a$_b".as_slice(), SyntaxKind::MacroParam),
            (b"foo_$a$", SyntaxKind::MacroParam),
            (b"$a$_$b$_c", SyntaxKind::MacroParam),
            (b"$TIER|capital$", SyntaxKind::MacroParam),
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
        assert!(tree.root().has_macro());
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
            .find(|t| {
                t.kind() == SyntaxKind::Whitespace && t.text_range().start() == TextSize::new(21)
            })
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

    /// The parent of each leaf is the node whose children hold the leaf, on
    /// random input of structural bytes.
    #[quickcheck]
    fn prop_leaf_parent_holds_the_leaf(data: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"{}[]()=@# \n\"ab1+";
        let source: Vec<u8> = data
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        fn walk(node: SyntaxNode<'_, '_>) -> bool {
            node.children().all(|child| match child {
                SyntaxElement::Token(token) => token.parent() == Some(node),
                SyntaxElement::Node(child) => child.parent() == Some(node) && walk(child),
            })
        }
        walk(parse(&source[..]).root())
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
        assert!(tree.data.repairs_valid.get().is_none());
        let recovery = tree.errors()[0].recovery.as_ref().unwrap();
        assert!(tree.repair_applicability(recovery).is_machine_applicable());
        assert_eq!(tree.data.repairs_valid.get(), Some(&true));
    }
}
