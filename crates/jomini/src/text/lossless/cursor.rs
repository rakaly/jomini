//! The cursors that walk the nodes and tokens of a [`SyntaxTree`].

use super::lexer::{TOKEN_ERROR, lex_trivia};
use super::tree::{NO_PARENT, Node, own_flag_bits};
use super::{NodeFlags, OwnedParse, Parse, SyntaxKind, SyntaxTree, TextRange};
use crate::text::Operator;
use std::fmt;

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
    pub(super) tree: SyntaxTree<'t, 'a>,
    pub(super) idx: u32,
}

/// A handle to a leaf token in a [`SyntaxTree`]. It is a significant token from
/// the token buffer, or a trivia token made from a gap.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxToken<'t, 'a> {
    pub(super) tree: SyntaxTree<'t, 'a>,
    pub(super) kind: SyntaxKind,
    pub(super) range: TextRange,
    /// For a significant token, its index in the token buffer. For trivia, the
    /// index of the significant token after the gap.
    pub(super) idx: u32,
    pub(super) trivia: bool,
}

/// The identity of a node in its tree.
///
/// [`index`](NodeId::index) is the position of the node in pre-order,
/// starting at 0 for the root and always less than
/// [`SyntaxTree::node_count`]. Thus a `Vec` or a bit set can hold data for
/// each node, and a pass over the tree does not need a hash map. An
/// identity is valid only for the tree that gave it.
///
/// ```
/// use jomini::text::lossless::{parse, AstNode, Field};
///
/// let parse = parse(b"a = { b = 1 } c = 2");
/// let tree = parse.tree();
///
/// // Record the depth of each node in a side table.
/// let mut depth = vec![0u32; tree.node_count()];
/// for node in tree.root().descendants().skip(1) {
///     let parent = node.parent().unwrap();
///     depth[node.id().index()] = depth[parent.id().index()] + 1;
/// }
///
/// let b = tree.root().descendants().filter_map(Field::cast).nth(1).unwrap();
/// assert_eq!(depth[b.syntax().id().index()], 3);
/// assert_eq!(tree.node_by_id(b.syntax().id()), Some(b.syntax()));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub(super) u32);

impl NodeId {
    /// The position of the node in pre-order.
    #[inline]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// The identity of a significant token in its tree.
///
/// [`index`](TokenId::index) is the position of the token in source order,
/// starting at 0 and always less than [`SyntaxTree::token_count`]. A trivia
/// token has no identity. An identity is valid only for the tree that gave it.
///
/// ```
/// use jomini::text::lossless::parse;
///
/// let parse = parse(b"a = 1 # one");
/// let tree = parse.tree();
/// assert_eq!(tree.token_count(), 3);
///
/// let value = tree.significant_tokens().last().unwrap();
/// let id = value.id().unwrap();
/// assert_eq!(id.index(), 2);
/// assert_eq!(tree.token_by_id(id), Some(value));
///
/// let comment = tree.tokens().last().unwrap();
/// assert_eq!(comment.id(), None);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenId(pub(super) u32);

impl TokenId {
    /// The position of the token among the significant tokens.
    #[inline]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// Either a [`SyntaxNode`] or a [`SyntaxToken`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElement<'t, 'a> {
    /// An interior node, such as a field or a block.
    Node(SyntaxNode<'t, 'a>),
    /// A leaf token, such as a scalar, an operator, or trivia.
    Token(SyntaxToken<'t, 'a>),
}

/// Two views are equal when they refer to the same tree and the same source.
/// The comparison does not look at the contents.
impl PartialEq for SyntaxTree<'_, '_> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.data, other.data) && std::ptr::eq(self.source, other.source)
    }
}

impl Eq for SyntaxTree<'_, '_> {}

impl std::hash::Hash for SyntaxTree<'_, '_> {
    #[inline]
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
impl fmt::Debug for Parse<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.tree().fmt(f)
    }
}

/// Write the [`SyntaxTree`] view of the parse.
impl<S: AsRef<[u8]>> fmt::Debug for OwnedParse<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.tree().fmt(f)
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
    pub(super) fn data(&self) -> &'t Node {
        &self.tree.data.nodes[self.idx as usize]
    }

    /// The identity of this node in its tree. See [`NodeId`].
    #[inline]
    pub fn id(&self) -> NodeId {
        NodeId(self.idx)
    }

    /// This node's kind.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data().kind
    }

    /// The half-open byte range this node spans in the source. The root spans
    /// the whole source. Other nodes span from their first token to their last.
    #[inline]
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
    #[inline]
    pub fn text(&self) -> &'a [u8] {
        &self.tree.source[self.text_range().to_range()]
    }

    /// The parent node, or `None` for the root.
    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        let p = self.tree.parents().nodes[self.idx as usize];
        (p != NO_PARENT).then_some(SyntaxNode {
            tree: self.tree,
            idx: p,
        })
    }

    /// The [`NodeFlags`] of this node and its whole subtree. The parser
    /// calculates them, so this call does not walk the subtree.
    #[inline]
    pub fn flags(&self) -> NodeFlags {
        self.data().flags
    }

    /// Return `true` when this subtree contains a syntax error or a recovery
    /// element. A linter can use it to skip checks on incorrect input.
    #[inline]
    pub fn has_error(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_ERROR)
    }

    /// Whether this subtree contains a comment (O(1); see [`NodeFlags`]).
    #[inline]
    pub fn has_comment(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_COMMENT)
    }

    /// Whether this subtree contains a `@[ ... ]` calc (O(1); see [`NodeFlags`]).
    #[inline]
    pub fn has_calc(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_CALC)
    }

    /// Whether this subtree contains a `$macro$` parameter (O(1); see [`NodeFlags`]).
    #[inline]
    pub fn has_macro(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_MACRO)
    }

    /// All direct children, nodes and tokens, in source order. This includes
    /// the trivia tokens that this node owns.
    #[inline]
    pub fn children(&self) -> Children<'t, 'a> {
        Children {
            inner: self.significant_children(),
            is_root: self.idx == 0,
            started: false,
            trailing_done: false,
            prev_end: 0,
            gap: Gap::EMPTY,
            pending: None,
        }
    }

    /// The direct children without trivia. This is faster than
    /// [`children`](Self::children) because it does not lex the gaps.
    #[inline]
    pub fn significant_children(&self) -> SignificantChildren<'t, 'a> {
        let node = self.data();
        SignificantChildren {
            tree: self.tree,
            next_token: node.first_token,
            token_end: node.token_end,
            next_child: self.idx + 1,
            child_end: node.subtree_end,
        }
    }

    /// Direct child nodes only.
    #[inline]
    pub fn child_nodes(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        self.significant_children().filter_map(|el| el.as_node())
    }

    /// Direct child tokens only, trivia included.
    #[inline]
    pub fn child_tokens(&self) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + use<'t, 'a> {
        self.children().filter_map(|el| el.as_token())
    }

    /// Direct significant child tokens only.
    #[inline]
    pub fn significant_child_tokens(
        &self,
    ) -> impl Iterator<Item = SyntaxToken<'t, 'a>> + use<'t, 'a> {
        self.significant_children().filter_map(|el| el.as_token())
    }

    #[inline]
    pub(super) fn node(&self, idx: u32) -> SyntaxNode<'t, 'a> {
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
    #[inline]
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
    #[inline]
    pub fn preorder(&self) -> Preorder<'t, 'a> {
        Preorder {
            node: *self,
            next: Some(WalkEvent::Enter(self.idx)),
        }
    }

    /// This node and its ancestors, from this node to the root.
    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        std::iter::successors(Some(*self), |node| node.parent())
    }

    /// The first child node.
    #[inline]
    pub fn first_child(&self) -> Option<SyntaxNode<'t, 'a>> {
        let child = self.idx + 1;
        (child < self.data().subtree_end).then(|| self.node(child))
    }

    /// The last child node.
    #[inline]
    pub fn last_child(&self) -> Option<SyntaxNode<'t, 'a>> {
        let mut child = self.first_child()?;
        while let Some(next) = child.next_sibling() {
            child = next;
        }
        Some(child)
    }

    /// The next node that has the same parent.
    #[inline]
    pub fn next_sibling(&self) -> Option<SyntaxNode<'t, 'a>> {
        let parent = self.parent()?;
        let next = self.data().subtree_end;
        (next < parent.data().subtree_end).then(|| self.node(next))
    }

    /// The previous node that has the same parent. This walks the siblings
    /// before this node, so it takes linear time.
    #[inline]
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
    #[inline]
    pub fn first_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        if self.idx == 0 {
            return self.tree.tokens().next();
        }
        Some(self.tree.token(self.data().first_token))
    }

    /// The last leaf of the subtree. Only the root can end with trivia.
    #[inline]
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
    #[inline]
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
    #[inline]
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

    #[inline]
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
    #[inline]
    pub fn left_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(value) | TokenAtOffset::Between(value, _) => Some(value),
        }
    }

    /// The leaf that starts at the offset, when there are two.
    #[inline]
    pub fn right_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(value) | TokenAtOffset::Between(_, value) => Some(value),
        }
    }

    /// Change each value.
    #[inline]
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

    #[inline]
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
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Whether this is a zero-width parser recovery token.
    #[inline]
    pub fn is_missing(&self) -> bool {
        self.kind.is_missing()
    }

    /// Whether this is a trivia token (whitespace, comment, or BOM).
    #[inline]
    pub fn is_trivia(&self) -> bool {
        self.trivia
    }

    /// Return the flags for this token.
    #[inline]
    pub fn flags(&self) -> NodeFlags {
        let mut flags = own_flag_bits(self.kind);
        if !self.trivia && self.tree.data.tokens[self.idx as usize].flags & TOKEN_ERROR != 0 {
            flags.insert(NodeFlags::HAS_ERROR);
        }
        flags
    }

    /// Return `true` when this token has a syntax error.
    #[inline]
    pub fn has_error(&self) -> bool {
        self.flags().contains(NodeFlags::HAS_ERROR)
    }

    /// The half-open byte range this token spans in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.range
    }

    /// The raw source bytes of this token.
    #[inline]
    pub fn text(&self) -> &'a [u8] {
        &self.tree.source[self.range.to_range()]
    }

    /// The parent node. A trivia token's parent is the node that owns its gap.
    #[inline]
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
    #[inline]
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
    #[inline]
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
    ///
    /// The tree does not store trivia, so each call lexes the trivia before
    /// this token again. A backward walk through a run of N comments is thus
    /// O(N²). To walk backward, use
    /// [`significant_tokens().rev()`](SyntaxTree::significant_tokens), or
    /// collect [`tokens`](SyntaxTree::tokens) once.
    #[inline]
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
    #[inline]
    pub fn op(&self) -> Option<Operator> {
        if self.kind() != SyntaxKind::Operator {
            return None;
        }
        operator(self.text())
    }
}

/// The operator that `text` spells.
#[inline]
pub(super) fn operator(text: &[u8]) -> Option<Operator> {
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
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxElement::Node(n) => n.kind(),
            SyntaxElement::Token(t) => t.kind(),
        }
    }

    /// Return `true` for a synthetic recovery token.
    #[inline]
    pub fn is_missing(&self) -> bool {
        matches!(self, Self::Token(token) if token.is_missing())
    }

    /// The half-open byte range this element spans in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(n) => n.text_range(),
            SyntaxElement::Token(t) => t.text_range(),
        }
    }

    /// The source bytes this element spans (its subtree, for a node).
    #[inline]
    pub fn text(&self) -> &'a [u8] {
        match self {
            SyntaxElement::Node(n) => n.text(),
            SyntaxElement::Token(t) => t.text(),
        }
    }

    /// The parent node, or `None` for the root.
    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode<'t, 'a>> {
        match self {
            SyntaxElement::Node(n) => n.parent(),
            SyntaxElement::Token(t) => t.parent(),
        }
    }

    /// The ancestors of this element, from its parent to the root.
    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<'t, 'a>> + use<'t, 'a> {
        std::iter::successors(self.parent(), |node| node.parent())
    }

    /// Unwrap to a node, or `None` if this is a token.
    #[inline]
    pub fn as_node(&self) -> Option<SyntaxNode<'t, 'a>> {
        match *self {
            SyntaxElement::Node(n) => Some(n),
            SyntaxElement::Token(_) => None,
        }
    }

    /// Unwrap to a token, or `None` if this is a node.
    #[inline]
    pub fn as_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        match *self {
            SyntaxElement::Token(t) => Some(t),
            SyntaxElement::Node(_) => None,
        }
    }
}

/// The part of a gap that is not yet lexed into trivia tokens.
#[derive(Debug, Clone, Copy)]
pub(super) struct Gap {
    pub(super) start: u32,
    pub(super) end: u32,
    /// The index of the significant token after the gap.
    pub(super) next: u32,
}

impl Gap {
    pub(super) const EMPTY: Gap = Gap {
        start: 0,
        end: 0,
        next: 0,
    };

    #[inline]
    pub(super) fn next_token<'t, 'a>(
        &mut self,
        tree: SyntaxTree<'t, 'a>,
    ) -> Option<SyntaxToken<'t, 'a>> {
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
#[derive(Debug, Clone)]
pub struct Children<'t, 'a> {
    inner: SignificantChildren<'t, 'a>,
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
            let tree = self.inner.tree;
            if let Some(token) = self.gap.next_token(tree) {
                return Some(SyntaxElement::Token(token));
            }
            if let Some(el) = self.pending.take() {
                return Some(el);
            }
            let first = self.inner.next_token;
            if let Some(el) = self.inner.next() {
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
                self.prev_end = tree.data.tokens[self.inner.next_token as usize - 1].end();
                self.pending = Some(el);
                continue;
            }
            if self.is_root && !self.trailing_done {
                // The root owns the gap after the last token.
                self.trailing_done = true;
                self.gap = Gap {
                    start: self.prev_end,
                    end: tree.source.len() as u32,
                    next: tree.data.tokens.len() as u32,
                };
                continue;
            }
            return None;
        }
    }
}

impl std::iter::FusedIterator for Children<'_, '_> {}

/// Iterator over the direct children of a node without trivia (see
/// [`SyntaxNode::significant_children`]).
///
/// The iterator reads only the token buffer and the node tape. It does not
/// read the source.
#[derive(Debug, Clone)]
pub struct SignificantChildren<'t, 'a> {
    tree: SyntaxTree<'t, 'a>,
    next_token: u32,
    token_end: u32,
    next_child: u32,
    child_end: u32,
}

impl<'t, 'a> Iterator for SignificantChildren<'t, 'a> {
    type Item = SyntaxElement<'t, 'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        let first = self.next_token;
        if first >= self.token_end {
            return None;
        }
        let tree = self.tree;
        if self.next_child < self.child_end {
            let child = &tree.data.nodes[self.next_child as usize];
            if child.first_token == first {
                let el = SyntaxElement::Node(SyntaxNode {
                    tree,
                    idx: self.next_child,
                });
                self.next_token = child.token_end;
                self.next_child = child.subtree_end;
                return Some(el);
            }
        }
        self.next_token = first + 1;
        Some(SyntaxElement::Token(tree.token(first)))
    }

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let tokens = self.token_end.saturating_sub(self.next_token) as usize;
        (usize::from(tokens != 0), Some(tokens))
    }
}

impl std::iter::FusedIterator for SignificantChildren<'_, '_> {}

/// Iterator over every leaf token of a tree, trivia included (see
/// [`SyntaxTree::tokens`]).
#[derive(Debug, Clone)]
pub struct Tokens<'t, 'a> {
    pub(super) tree: SyntaxTree<'t, 'a>,
    pub(super) next: u32,
    /// The index after the last significant token to give.
    pub(super) end: u32,
    pub(super) prev_end: u32,
    pub(super) gap: Gap,
    pub(super) pending: Option<SyntaxToken<'t, 'a>>,
    pub(super) trailing_done: bool,
}

impl<'t, 'a> Iterator for Tokens<'t, 'a> {
    type Item = SyntaxToken<'t, 'a>;

    #[inline]
    fn size_hint(&self) -> (usize, Option<usize>) {
        // Each significant token that is left comes out. The number of trivia
        // tokens is not known before the gaps are lexed.
        let left = self.end.saturating_sub(self.next) as usize;
        (left + usize::from(self.pending.is_some()), None)
    }

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
