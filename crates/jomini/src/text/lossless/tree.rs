//! The storage of a parsed tree and the entry points that parse a source.
//!
//! The tree has two flat arrays:
//!
//! - A token buffer holds only the significant tokens. Each token has a
//!   kind and an absolute byte range.
//! - A node tape holds the nodes in pre-order. Each node has a kind, the
//!   range of tokens it covers, and the index after its last descendant.
//!
//! The tree does not store trivia. Trivia is the bytes between two
//! significant tokens, and the gap between two tokens belongs to their
//! nearest common ancestor node. A cursor lexes a gap again when it must
//! show trivia. The parent links are calculated on the first query for a
//! parent.

use super::cursor::Gap;
use super::lexer::{Lexer, Token};
use super::parser::{Builder, Parser, Sink};
use super::{
    Flavor, FormatOptions, MissingDelimiter, NodeId, Root, SyntaxElement, SyntaxError,
    SyntaxErrorKind, SyntaxKind, SyntaxNode, SyntaxToken, TextEdit, TextRange, TextSize,
    TokenAtOffset, TokenId, Tokens,
};
use std::fmt;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

/// Summary bits for a subtree, one byte for each node. The parser sets the
/// bits when it builds the tree.
///
/// The flags of a node contain the bits of its kind and the bits of *all* its
/// descendants. Thus one O(1) test tells if a block has a comment, a syntax
/// error, a calc, or a macro parameter. You do not have to walk the subtree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct NodeFlags(pub(super) u8);

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

    pub(super) fn insert(&mut self, other: NodeFlags) {
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
pub(super) fn own_flag_bits(kind: SyntaxKind) -> NodeFlags {
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
pub(super) struct Node {
    pub(super) kind: SyntaxKind,
    pub(super) flags: NodeFlags,
    pub(super) first_token: u32,
    pub(super) token_end: u32,
    pub(super) subtree_end: u32,
}

/// The parent of each node and each token. `NO_PARENT` marks the root.
#[derive(Clone)]
pub(super) struct Parents {
    pub(super) nodes: Vec<u32>,
    pub(super) tokens: Vec<u32>,
}

pub(super) const NO_PARENT: u32 = u32::MAX;

/// The parse result without the source. It holds only byte offsets, so it
/// has no lifetime.
#[derive(Clone)]
pub(super) struct TreeData {
    /// The identity of the parse. Each parse gets a different number, and
    /// a clone keeps it. A resolution compares it to find its tree.
    pub(super) parse_id: u64,
    pub(super) flavor: Flavor,
    /// The significant and missing tokens in sink emission order.
    pub(super) tokens: Vec<Token>,
    pub(super) nodes: Vec<Node>,
    errors: Vec<SyntaxError>,
    /// Calculated on the first parent query.
    parents: OnceLock<Parents>,
    /// Calculated on the first call of [`SyntaxTree::closing_edit`].
    pub(super) closing_edit: OnceLock<Option<TextEdit>>,
}

/// A borrowed source and its lossless syntax tree, from [`parse`].
///
/// The parser does not copy the source. The text of the tokens has the
/// lifetime `'a` of the source, so the text can outlive the `Parse`. To keep
/// the tree and the source together, use [`parse_owned`]. The [`OwnedParse`]
/// that it gives has the same methods. Only the lifetime of the text is
/// different.
///
/// Use [`tree`](Parse::tree) to get the [`SyntaxTree`] view. The methods of
/// the view are also available on the `Parse`.
///
/// ```
/// use jomini::text::lossless::parse;
///
/// // The keys borrow the source, not the parse.
/// fn keys(source: &[u8]) -> Vec<&[u8]> {
///     let parse = parse(source);
///     parse.ast().fields().map(|f| f.key().text()).collect()
/// }
/// assert_eq!(keys(b"a = 1 b = 2"), [&b"a"[..], &b"b"[..]]);
/// ```
#[derive(Clone)]
pub struct Parse<'a> {
    source: &'a [u8],
    pub(super) data: TreeData,
}

/// A source and its lossless syntax tree, from [`parse_owned`].
///
/// The `OwnedParse` keeps the source, so you can keep it in a struct, a
/// cache, or a map, or send it to a different thread. The text of its tokens
/// borrows the `OwnedParse`. It has the same methods as [`Parse`].
///
/// ```
/// use jomini::text::lossless::{parse_owned, OwnedParse};
/// use std::sync::Arc;
///
/// let source: Arc<[u8]> = Arc::from(&b"a = 1"[..]);
/// let parse: OwnedParse<Arc<[u8]>> = parse_owned(source);
/// assert_eq!(parse.root().children().count(), 1);
/// assert_eq!(parse.errors(), []);
/// ```
#[derive(Clone)]
pub struct OwnedParse<S> {
    source: S,
    pub(super) data: TreeData,
}

/// A view of a lossless syntax tree and its source bytes.
///
/// The view is two references, so it is cheap to copy. The text of its
/// tokens has the lifetime `'a` of the source, not the lifetime `'t` of the
/// tree.
#[derive(Clone, Copy)]
pub struct SyntaxTree<'t, 'a> {
    pub(super) data: &'t TreeData,
    pub(super) source: &'a [u8],
}

/// Parse `source` into a lossless syntax tree with the default [`Flavor`].
/// For HoI4 files, use [`parse_with`] and [`Flavor::hoi4`].
///
/// The tree borrows `source`. Use [`parse_owned`] to give the source to the
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
pub fn parse(source: &[u8]) -> Parse<'_> {
    parse_with(source, Flavor::default())
}

/// Parse `source` into a lossless syntax tree with a specific [`Flavor`].
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
#[must_use]
pub fn parse_with(source: &[u8], flavor: Flavor) -> Parse<'_> {
    let data = parse_data(source, flavor);
    Parse { source, data }
}

/// Parse `source` with the default [`Flavor`], and keep the source with the
/// tree.
///
/// `source` can be any type that gives bytes, such as a `Vec<u8>`, a
/// `String`, an `Arc<[u8]>`, or a memory map. [`AsRef::as_ref`] must give the
/// same bytes for each call. If the bytes change, the tree does not match
/// them: the cursors can then give wrong text or panic, but they do not cause
/// undefined behavior.
///
/// A borrowed `&[u8]` also works, but its text then borrows the
/// [`OwnedParse`]. Use [`parse`] to get text with the lifetime of the source.
///
/// ```
/// use jomini::text::lossless::parse_owned;
///
/// let parse = parse_owned(String::from("a = 1"));
/// assert_eq!(parse.ast().fields().count(), 1);
/// assert_eq!(parse.into_source(), "a = 1");
/// ```
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
#[must_use]
pub fn parse_owned<S: AsRef<[u8]>>(source: S) -> OwnedParse<S> {
    parse_owned_with(source, Flavor::default())
}

/// Parse `source` and keep it with the tree, with a specific [`Flavor`].
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
#[must_use]
pub fn parse_owned_with<S: AsRef<[u8]>>(source: S, flavor: Flavor) -> OwnedParse<S> {
    let data = parse_data(source.as_ref(), flavor);
    OwnedParse { source, data }
}

/// The methods that [`Parse`] and [`OwnedParse`] share. Each one calls the
/// method of the [`SyntaxTree`] view. The cursors of a `Parse` have the
/// lifetime `$a` of the source. The cursors of an `OwnedParse` borrow it.
macro_rules! parse_methods {
    ($a:lifetime) => {
        /// The root [`SyntaxNode`], of kind [`SyntaxKind::Root`]. See
        /// [`SyntaxTree::root`].
        pub fn root(&self) -> SyntaxNode<'_, $a> {
            self.tree().root()
        }

        /// The typed [`Root`] of the document. Start here to read fields and
        /// values. See [`SyntaxTree::ast`].
        pub fn ast(&self) -> Root<'_, $a> {
            self.tree().ast()
        }

        /// Every leaf token in source order, trivia included. Their text
        /// reproduces the source. See [`SyntaxTree::tokens`].
        pub fn tokens(&self) -> Tokens<'_, $a> {
            self.tree().tokens()
        }

        /// Every leaf token that is not trivia, in source order. See
        /// [`SyntaxTree::significant_tokens`].
        pub fn significant_tokens(
            &self,
        ) -> impl ExactSizeIterator<Item = SyntaxToken<'_, $a>>
        + DoubleEndedIterator
        + std::iter::FusedIterator {
            self.tree().significant_tokens()
        }

        /// The leaf tokens at a byte offset, such as the position of an editor
        /// cursor. See [`SyntaxTree::token_at_offset`].
        pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken<'_, $a>> {
            self.tree().token_at_offset(offset)
        }

        /// The smallest node or token that contains a byte range. See
        /// [`SyntaxTree::covering_element`].
        pub fn covering_element(&self, range: TextRange) -> SyntaxElement<'_, $a> {
            self.tree().covering_element(range)
        }

        /// The syntax errors that the parser recovered from. See
        /// [`SyntaxTree::errors`].
        pub fn errors(&self) -> &[SyntaxError] {
            &self.data.errors
        }

        /// The edit that closes each open construct at the end of the
        /// source. See [`SyntaxTree::closing_edit`].
        pub fn closing_edit(&self) -> Option<&TextEdit> {
            self.tree().closing_edit()
        }

        /// See [`SyntaxTree::reconstruct`].
        #[cfg(test)]
        pub fn reconstruct(&self) -> Vec<u8> {
            self.tree().reconstruct()
        }

        /// An indented dump of the tree: the kind and the byte range of each
        /// node and token. See [`SyntaxTree::debug_tree`].
        pub fn debug_tree(&self) -> String {
            self.tree().debug_tree()
        }

        /// The source written again in a normalized style, with each comment
        /// kept. See [`SyntaxTree::format`].
        pub fn format(&self, opts: &FormatOptions) -> Vec<u8> {
            self.tree().format(opts)
        }
    };
}

impl<'a> Parse<'a> {
    /// The [`SyntaxTree`] view of the tree and the source. The text of the
    /// view has the lifetime `'a` of the source.
    pub fn tree(&self) -> SyntaxTree<'_, 'a> {
        SyntaxTree {
            data: &self.data,
            source: self.source,
        }
    }

    /// The source.
    #[inline]
    pub fn source(&self) -> &'a [u8] {
        self.source
    }

    parse_methods!('a);
}

impl<S: AsRef<[u8]>> OwnedParse<S> {
    /// The [`SyntaxTree`] view of the tree and the source. The view borrows
    /// the `OwnedParse`.
    pub fn tree(&self) -> SyntaxTree<'_, '_> {
        SyntaxTree {
            data: &self.data,
            source: self.source.as_ref(),
        }
    }

    /// The source.
    #[inline]
    pub fn source(&self) -> &S {
        &self.source
    }

    /// Discard the tree and return the source.
    pub fn into_source(self) -> S {
        self.source
    }

    parse_methods!('_);
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
        parse_id: next_parse_id(),
        flavor,
        tokens,
        nodes,
        errors,
        parents: OnceLock::new(),
        closing_edit: OnceLock::new(),
    }
}

/// A number that no other parse has.
fn next_parse_id() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Send the events of the grammar for `source` to `sink`. The parser lexes
/// the source while it parses.
pub(super) fn run<S: Sink>(source: &[u8], flavor: Flavor, sink: S) -> Parser<'_, S> {
    assert!(
        u32::try_from(source.len()).is_ok(),
        "source is larger than 4 GiB"
    );
    let mut p = Parser::new(source, Lexer::new(source, flavor), sink);
    p.sink.start_node(SyntaxKind::Root);
    p.parse_items(false);
    if S::KEEP_TOKENS {
        for &start in &p.lexer.hiding_quotes {
            p.errors.push(SyntaxError {
                kind: SyntaxErrorKind::ProbablyUnclosedQuote,
                range: TextRange::span(start, start + 1),
                missing_delimiter: None,
            });
        }
    }
    if S::KEEP_TOKENS
        && let Some(range) = p.lexer.unclosed_quote
    {
        p.errors.push(SyntaxError {
            kind: SyntaxErrorKind::UnclosedQuote,
            range,
            missing_delimiter: None,
        });
    }
    p.sink.finish_node();
    p
}

/// Send the events of the grammar for the root items that start at the byte
/// `offset` to `sink`. The sink must have an open root node, with the state
/// that it has at `offset` in a full parse. The parser does not have the
/// tokens before `offset`, thus the grammar must not look back at them. This
/// is true after a `}` token that no operator follows. The diagnostics are not
/// complete.
pub(super) fn run_items<S: Sink>(
    source: &[u8],
    flavor: Flavor,
    offset: usize,
    sink: S,
) -> Parser<'_, S> {
    let mut p = Parser::new(source, Lexer::at(source, flavor, offset), sink);
    p.parse_items(false);
    p.sink.finish_node();
    p
}

impl<'t, 'a> SyntaxTree<'t, 'a> {
    /// The full source the tree was parsed from.
    #[inline]
    pub fn source(&self) -> &'a [u8] {
        self.source
    }

    /// The root [`SyntaxNode`] (always [`SyntaxKind::Root`]).
    #[inline]
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

    /// The number of nodes. See [`NodeId`].
    #[inline]
    pub fn node_count(&self) -> usize {
        self.data.nodes.len()
    }

    /// The number of significant tokens. See [`TokenId`].
    #[inline]
    pub fn token_count(&self) -> usize {
        self.data.tokens.len()
    }

    /// The node that has the identity `id`. Return `None` when this tree
    /// has no such node.
    #[inline]
    pub fn node_by_id(&self, id: NodeId) -> Option<SyntaxNode<'t, 'a>> {
        (id.index() < self.data.nodes.len()).then_some(SyntaxNode {
            tree: *self,
            idx: id.0,
        })
    }

    /// The significant token that has the identity `id`. Return `None`
    /// when this tree has no such token.
    #[inline]
    pub fn token_by_id(&self, id: TokenId) -> Option<SyntaxToken<'t, 'a>> {
        (id.index() < self.data.tokens.len()).then(|| self.token(id.0))
    }

    /// The edit that closes each open construct at the end of the source: the
    /// missing `}`, `]`, `)`, and `]]`, from the inside out. Each error that
    /// has a [`missing_delimiter`](SyntaxError::missing_delimiter) shares
    /// this one edit, so a client shows it once, for all of these errors:
    ///
    /// ```
    /// use jomini::text::lossless::{apply_edits, parse};
    ///
    /// let parse = parse(b"a = { b = { c = 1 # note");
    /// assert_eq!(parse.errors().len(), 2);
    /// let edit = parse.closing_edit().unwrap();
    /// assert_eq!(edit.new_text, b"\n}}");
    ///
    /// let fixed = apply_edits(parse.source(), parse.closing_edit())?;
    /// assert_eq!(fixed, b"a = { b = { c = 1 # note\n}}");
    /// # Ok::<(), jomini::text::lossless::EditError>(())
    /// ```
    ///
    /// The edit starts with a line break when the source ends in a comment,
    /// because a delimiter after the comment is a part of the comment.
    ///
    /// The result is `None` when no construct is open, or when the edited
    /// source does not parse cleanly: it still has a missing delimiter, or
    /// its errors are not the other errors of this source, as after an
    /// unclosed quote. The first call parses the edited source to check it,
    /// and later calls give the same edit with no cost.
    ///
    /// The edit keeps the meaning of the file, because the parser and the
    /// game also close each open construct at the end of the source. Thus a
    /// tool can apply it, for example in a `--fix` mode. It cannot restore
    /// a `}` that was lost earlier in the file: the fields after that point
    /// stay in the block.
    pub fn closing_edit(&self) -> Option<&'t TextEdit> {
        self.data
            .closing_edit
            .get_or_init(|| self.check_closing_edit())
            .as_ref()
    }

    fn check_closing_edit(&self) -> Option<TextEdit> {
        let errors = &self.data.errors;
        let mut delimiters: Vec<&MissingDelimiter> = errors
            .iter()
            .filter_map(|e| e.missing_delimiter.as_ref())
            .collect();
        if delimiters.is_empty() || delimiters.iter().any(|d| !d.safe) {
            return None;
        }
        delimiters.sort_by_key(|d| d.order);
        let text: String = delimiters.iter().map(|d| d.text).collect();
        let candidate = [self.source, text.as_bytes()].concat();
        // The parser cannot read a source of 4 GiB or more.
        u32::try_from(candidate.len()).ok()?;
        // The edited source must keep each other error and add none.
        let reparsed = parse_data(&candidate, self.data.flavor).errors;
        let kept = errors.iter().filter(|e| e.missing_delimiter.is_none());
        if !reparsed.iter().eq(kept) {
            return None;
        }
        let end = TextSize::new(self.source.len() as u32);
        Some(TextEdit::new(TextRange::empty(end), text))
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
    ///             _ if t.text().first().is_some_and(u8::is_ascii_digit) => "number",
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
    pub fn significant_tokens(
        &self,
    ) -> impl ExactSizeIterator<Item = SyntaxToken<'t, 'a>>
    + DoubleEndedIterator
    + std::iter::FusedIterator
    + use<'t, 'a> {
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

    #[inline]
    pub(super) fn token(&self, idx: u32) -> SyntaxToken<'t, 'a> {
        let token = self.data.tokens[idx as usize];
        SyntaxToken {
            tree: *self,
            kind: token.kind,
            range: token.range(),
            idx,
            trivia: false,
        }
    }

    #[inline]
    pub(super) fn trivia(
        &self,
        kind: SyntaxKind,
        start: u32,
        len: u32,
        next: u32,
    ) -> SyntaxToken<'t, 'a> {
        SyntaxToken {
            tree: *self,
            kind,
            range: TextRange::span(start, start + len),
            idx: next,
            trivia: true,
        }
    }

    pub(super) fn parents(&self) -> &Parents {
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
    #[inline]
    pub(super) fn gap_before(&self, next: u32) -> Gap {
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
    pub(super) fn gap_owner(&self, next: u32) -> u32 {
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
