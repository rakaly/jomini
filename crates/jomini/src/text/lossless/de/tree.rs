//! The model of a [`SyntaxTree`] for the shared serde rules.
//!
//! The deserializer visits each entry that it reads, so this model is the
//! hot path. It reads the token buffer and the node tape of the tree
//! directly: the steps are the same as those of
//! [`SyntaxNode::significant_children`], but no cursor object is made and no
//! trivia state is kept.

use super::engine::{Doc, DocDe, Item, View};
use super::error::Error;
use crate::{
    Encoding, Scalar,
    text::{
        Operator,
        lossless::{
            AstNode, Node, Parameter, SyntaxKind, SyntaxNode, SyntaxTree, TextRange, Token,
            UndefinedParameter,
        },
    },
};
use serde::de::Visitor;
use std::borrow::Cow;

/// A [`SyntaxTree`] and the encoding of its text.
pub(crate) struct TreeCtx<'c, 'a, E> {
    tokens: &'c [Token],
    nodes: &'c [Node],
    source: &'a [u8],
    tree: SyntaxTree<'c, 'a>,
    encoding: E,
}

impl<'c, 'a, E> TreeCtx<'c, 'a, E> {
    pub(crate) fn new(tree: SyntaxTree<'c, 'a>, encoding: E) -> Self {
        TreeCtx {
            tokens: &tree.data.tokens,
            nodes: &tree.data.nodes,
            source: tree.source,
            tree,
            encoding,
        }
    }

    pub(crate) fn tree(&self) -> SyntaxTree<'c, 'a> {
        self.tree
    }

    pub(crate) fn encoding(&self) -> &E {
        &self.encoding
    }
}

/// The model of the tree. It is one pointer, so the deserializer can copy
/// it at no cost.
pub(crate) struct TreeDoc<'c, 'a, E> {
    ctx: &'c TreeCtx<'c, 'a, E>,
}

impl<E> Clone for TreeDoc<'_, '_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for TreeDoc<'_, '_, E> {}

/// A value of the tree: a token or a node, by index.
#[derive(Clone, Copy)]
pub(crate) enum TreeNode {
    Token(u32),
    Node(u32),
}

/// A scalar of the tree: a token, or a node that acts as a scalar (a
/// calculation or an interpolation), by index. The deserializer reads the
/// bytes only when it needs them.
#[derive(Clone, Copy)]
pub(crate) struct TreeScalar {
    index: u32,
    node: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ContainerKind {
    Root,
    Block,
    Parameter,
}

/// The root, a block, or the body of a parameter.
#[derive(Clone, Copy)]
pub(crate) struct TreeContainer {
    node: u32,
    kind: ContainerKind,
}

impl<'c, 'a, E: Encoding> TreeDoc<'c, 'a, E> {
    pub(crate) fn new(ctx: &'c TreeCtx<'c, 'a, E>) -> Self {
        TreeDoc { ctx }
    }

    /// The root of the tree as a container.
    pub(crate) fn root(&self) -> TreeContainer {
        TreeContainer {
            node: 0,
            kind: ContainerKind::Root,
        }
    }

    #[inline]
    fn token_scalar(self, index: u32) -> TreeScalar {
        TreeScalar { index, node: false }
    }

    fn node_scalar(self, index: u32) -> TreeScalar {
        TreeScalar { index, node: true }
    }

    /// The bytes of a scalar. The bytes of a quoted token have no quotes.
    #[inline]
    pub(super) fn bytes(self, scalar: TreeScalar) -> &'a [u8] {
        if scalar.node {
            return &self.ctx.source[self.node(scalar.index).text_range().to_range()];
        }
        let token = self.ctx.tokens[scalar.index as usize];
        let text = &self.ctx.source[token.start as usize..token.end() as usize];
        if token.kind == SyntaxKind::Quoted {
            let text = text.strip_prefix(b"\"").unwrap_or(text);
            text.strip_suffix(b"\"").unwrap_or(text)
        } else {
            text
        }
    }

    #[inline]
    fn is_quoted(self, scalar: TreeScalar) -> bool {
        !scalar.node && self.ctx.tokens[scalar.index as usize].kind == SyntaxKind::Quoted
    }

    #[inline]
    fn range(self, scalar: TreeScalar) -> TextRange {
        if scalar.node {
            self.node(scalar.index).text_range()
        } else {
            self.ctx.tokens[scalar.index as usize].range()
        }
    }

    /// The keys of the fields that hold `node`, from the inner field to the
    /// outer field.
    #[cold]
    pub(super) fn keys_of(self, node: TreeNode) -> Vec<String> {
        let mut parent = match node {
            TreeNode::Token(index) => self.ctx.tree.token(index).parent(),
            TreeNode::Node(index) => self.node(index).parent(),
        };
        let mut keys = Vec::new();
        while let Some(node) = parent {
            if node.kind() == SyntaxKind::Field {
                let first = self.ctx.nodes[node.idx as usize].first_token;
                keys.push(self.string_of(self.token_scalar(first)));
            }
            parent = node.parent();
        }
        keys
    }

    fn string_of(self, scalar: TreeScalar) -> String {
        self.ctx.encoding.decode(self.bytes(scalar)).into_owned()
    }

    pub(super) fn node(self, index: u32) -> SyntaxNode<'c, 'a> {
        SyntaxNode {
            tree: self.ctx.tree,
            idx: index,
        }
    }

    #[inline]
    fn cursor(self, index: u32) -> Cursor {
        let node = self.ctx.nodes[index as usize];
        Cursor {
            token: node.first_token,
            token_end: node.token_end,
            child: index + 1,
            child_end: node.subtree_end,
        }
    }

    /// A parameter block as an entry.
    #[cold]
    #[inline(never)]
    fn parameter(self, index: u32) -> Option<Item<Self>> {
        let node = self.node(index);
        let undefined = node.kind() == SyntaxKind::UndefinedParameter;
        let name = match Parameter::cast(node) {
            Some(parameter) => parameter.name(),
            None => UndefinedParameter::cast(node).and_then(|p| p.name()),
        }?;
        Some(Item::Parameter {
            name: TreeScalar {
                index: name.idx,
                node: false,
            },
            undefined,
            body: TreeContainer {
                node: index,
                kind: ContainerKind::Parameter,
            },
        })
    }

    /// The parts of a field: the key, the operator, and the value. Return
    /// `None` for a field that has no value, as `TextTape` ignores it.
    #[inline]
    fn field(self, index: u32) -> Option<Item<Self>> {
        // The usual shapes are `key = token`, `key = { ... }`, and
        // `key { ... }`. The general code examines the other shapes.
        let ctx = self.ctx;
        let node = ctx.nodes[index as usize];
        let first = node.first_token;
        let tokens = ctx.tokens;
        let key_token = tokens.get(first as usize)?;
        if !key_token.kind.is_scalar() {
            return None;
        }
        let key = self.token_scalar(first);
        let child = index + 1;
        if child < node.subtree_end {
            let value = ctx.nodes[child as usize];
            if value.kind == SyntaxKind::Bogus {
                return None;
            }
            if value.first_token == first + 1 {
                let value = TreeNode::Node(child);
                return Some(Item::Field {
                    key,
                    op: None,
                    value,
                });
            }
            if value.first_token == first + 2
                && let Some(op) = tokens.get(first as usize + 1)
                && op.kind == SyntaxKind::Operator
            {
                let op = Some(self.operator(op.start, op.len));
                let value = TreeNode::Node(child);
                return Some(Item::Field { key, op, value });
            }
        } else if node.token_end == first + 3
            && let Some(op) = tokens.get(first as usize + 1)
            && let Some(value) = tokens.get(first as usize + 2)
            && op.kind == SyntaxKind::Operator
            && (value.kind.is_scalar() || value.kind == SyntaxKind::Operator)
        {
            let op = Some(self.operator(op.start, op.len));
            let value = TreeNode::Token(first + 2);
            return Some(Item::Field { key, op, value });
        }
        self.field_general(index)
    }

    /// The parts of a field of an unusual shape.
    #[cold]
    #[inline(never)]
    fn field_general(self, index: u32) -> Option<Item<Self>> {
        let ctx = self.ctx;
        let node = ctx.nodes[index as usize];
        let first = node.first_token;
        let tokens = ctx
            .tokens
            .get(first as usize..node.token_end as usize)
            .unwrap_or_default();
        let [key, rest @ ..] = tokens else {
            return None;
        };
        if !key.kind.is_scalar() {
            return None;
        }

        // The key and the operator are tokens, so a child node is the value.
        let child = index + 1;
        let child_start = if child < node.subtree_end {
            ctx.nodes[child as usize].first_token
        } else {
            u32::MAX
        };

        let mut position = first + 1;
        let mut op = None;
        match rest.first() {
            None => return None,
            Some(token) if token.kind == SyntaxKind::Operator && child_start != position => {
                op = Some(self.operator(token.start, token.len));
                position += 1;
            }
            Some(_) => {}
        }

        let value = if child_start == position {
            if ctx.nodes[child as usize].kind == SyntaxKind::Bogus {
                return None;
            }
            TreeNode::Node(child)
        } else {
            let token = rest.get((position - first - 1) as usize)?;
            if !token.kind.is_scalar() && token.kind != SyntaxKind::Operator {
                return None;
            }
            TreeNode::Token(position)
        };

        Some(Item::Field {
            key: self.token_scalar(first),
            op,
            value,
        })
    }

    /// The operator of the token at `start` with `len` bytes.
    #[inline]
    fn operator(self, start: u32, len: u32) -> Operator {
        let source = self.ctx.source;
        let start = start as usize;
        match (source.get(start), len) {
            (Some(b'='), 1) => Operator::Equal,
            _ => match source.get(start..start + len as usize).unwrap_or_default() {
                b"<" => Operator::LessThan,
                b"<=" => Operator::LessThanEqual,
                b">" => Operator::GreaterThan,
                b">=" => Operator::GreaterThanEqual,
                b"==" => Operator::Exact,
                b"!=" => Operator::NotEqual,
                b"?=" => Operator::Exists,
                _ => Operator::Equal,
            },
        }
    }
}

/// A cursor over the significant children of a node: a token or a node,
/// by index.
#[derive(Clone, Copy)]
struct Cursor {
    token: u32,
    token_end: u32,
    child: u32,
    child_end: u32,
}

impl Cursor {
    #[inline]
    fn next(&mut self, nodes: &[Node]) -> Option<TreeNode> {
        if self.token >= self.token_end {
            return None;
        }
        let first = self.token;
        if self.child < self.child_end {
            let child = nodes[self.child as usize];
            if child.first_token == first {
                let index = self.child;
                self.token = child.token_end;
                self.child = child.subtree_end;
                return Some(TreeNode::Node(index));
            }
        }
        self.token += 1;
        Some(TreeNode::Token(first))
    }
}

/// The scalar tokens of a block without child nodes. The braces and the
/// other loose tokens are not values, as [`TreeItems`] skips them.
#[derive(Clone)]
pub(crate) struct TreeScalars<'c> {
    tokens: &'c [Token],
    index: u32,
    end: u32,
}

impl Iterator for TreeScalars<'_> {
    type Item = TreeNode;

    #[inline]
    fn next(&mut self) -> Option<TreeNode> {
        while self.index < self.end {
            let index = self.index;
            self.index += 1;
            if self.tokens[index as usize].kind.is_scalar() {
                return Some(TreeNode::Token(index));
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // At most the two braces are not values.
        let len = (self.end - self.index) as usize;
        (len.saturating_sub(2), Some(len))
    }
}

/// The entries of a container of the tree.
pub(crate) struct TreeItems<'c, 'a, E> {
    doc: TreeDoc<'c, 'a, E>,
    cursor: Cursor,
    /// For a parameter: the header (`[[name]`) is done.
    header_done: bool,
    parameter: bool,
}

impl<E> Clone for TreeItems<'_, '_, E> {
    fn clone(&self) -> Self {
        TreeItems {
            doc: self.doc,
            cursor: self.cursor,
            header_done: self.header_done,
            parameter: self.parameter,
        }
    }
}

impl<'c, 'a, E> Iterator for TreeItems<'c, 'a, E>
where
    E: Encoding,
{
    type Item = Item<TreeDoc<'c, 'a, E>>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let ctx = self.doc.ctx;
        loop {
            match self.cursor.next(ctx.nodes)? {
                TreeNode::Token(index) => {
                    let kind = ctx.tokens[index as usize].kind;
                    if !self.header_done {
                        self.header_done = kind == SyntaxKind::CloseBracket;
                        continue;
                    }
                    if kind.is_scalar() {
                        return Some(Item::Value(TreeNode::Token(index)));
                    }
                    if self.parameter
                        && matches!(
                            kind,
                            SyntaxKind::CloseBracket | SyntaxKind::MissingCloseBracket
                        )
                    {
                        self.cursor.token = self.cursor.token_end;
                        return None;
                    }
                }
                TreeNode::Node(index) => match ctx.nodes[index as usize].kind {
                    SyntaxKind::Field => {
                        if let Some(item) = self.doc.field(index) {
                            return Some(item);
                        }
                    }
                    SyntaxKind::Block
                    | SyntaxKind::HeaderedBlock
                    | SyntaxKind::Calc
                    | SyntaxKind::Interpolation
                    | SyntaxKind::Code => return Some(Item::Value(TreeNode::Node(index))),
                    SyntaxKind::Parameter | SyntaxKind::UndefinedParameter => {
                        if let Some(item) = self.doc.parameter(index) {
                            return Some(item);
                        }
                    }
                    _ => {}
                },
            }
        }
    }
}

impl<'c, 'a, E> Doc for TreeDoc<'c, 'a, E>
where
    E: Encoding,
{
    type Node = TreeNode;
    type Scalar = TreeScalar;
    type Container = TreeContainer;
    type Items = TreeItems<'c, 'a, E>;
    type Scalars = TreeScalars<'c>;

    #[inline]
    fn view(self, node: Self::Node) -> View<Self> {
        let index = match node {
            TreeNode::Token(index) => return View::Scalar(self.token_scalar(index)),
            TreeNode::Node(index) => index,
        };
        let block = |node| TreeContainer {
            node,
            kind: ContainerKind::Block,
        };
        let ctx = self.ctx;
        match ctx.nodes[index as usize].kind {
            SyntaxKind::Block => View::Block(block(index)),
            SyntaxKind::HeaderedBlock => {
                let mut cursor = self.cursor(index);
                match (cursor.next(ctx.nodes), cursor.next(ctx.nodes)) {
                    (Some(TreeNode::Token(header)), Some(TreeNode::Node(body)))
                        if ctx.tokens[header as usize].kind.is_scalar()
                            && ctx.nodes[body as usize].kind == SyntaxKind::Block =>
                    {
                        View::Headered(self.token_scalar(header), block(body))
                    }
                    _ => View::Scalar(self.node_scalar(index)),
                }
            }
            SyntaxKind::Calc => View::Calc(self.node_scalar(index)),
            _ => View::Scalar(self.node_scalar(index)),
        }
    }

    #[inline]
    fn scalar_of(self, node: Self::Node) -> Option<Self::Scalar> {
        match node {
            // A token value is always a scalar or an operator.
            TreeNode::Token(index) => Some(self.token_scalar(index)),
            TreeNode::Node(_) => match self.view(node) {
                View::Scalar(s) | View::Calc(s) | View::Headered(s, _) => Some(s),
                View::Block(_) => None,
            },
        }
    }

    #[inline]
    fn items(self, container: Self::Container) -> Self::Items {
        let parameter = container.kind == ContainerKind::Parameter;
        TreeItems {
            doc: self,
            cursor: self.cursor(container.node),
            header_done: !parameter,
            parameter,
        }
    }

    #[inline]
    fn always_object(self, container: Self::Container) -> bool {
        container.kind != ContainerKind::Block
    }

    #[inline]
    fn is_root(self, container: Self::Container) -> bool {
        container.kind == ContainerKind::Root
    }

    #[inline]
    fn quoted(self, scalar: Self::Scalar) -> bool {
        self.is_quoted(scalar)
    }

    #[inline]
    fn with_scalar<R>(self, scalar: Self::Scalar, f: impl FnOnce(Scalar<'_>) -> R) -> R {
        f(Scalar::new(self.bytes(scalar)))
    }

    fn string(self, scalar: Self::Scalar) -> String {
        self.ctx.encoding.decode(self.bytes(scalar)).into_owned()
    }

    fn node_range(self, node: Self::Node) -> Option<TextRange> {
        Some(match node {
            TreeNode::Token(index) => self.ctx.tokens[index as usize].range(),
            TreeNode::Node(index) => self.node(index).text_range(),
        })
    }

    fn scalar_range(self, scalar: Self::Scalar) -> Option<TextRange> {
        Some(self.range(scalar))
    }

    fn container_range(self, container: Self::Container) -> Option<TextRange> {
        Some(self.node(container.node).text_range())
    }

    #[inline]
    fn scalars(self, container: Self::Container) -> Option<Self::Scalars> {
        // A field, a block, a headered block, and a calculation are nodes, so
        // a block without child nodes has only scalars and loose tokens.
        let node = self.ctx.nodes[container.node as usize];
        let leaf = container.kind == ContainerKind::Block && node.subtree_end == container.node + 1;
        leaf.then_some(TreeScalars {
            tokens: self.ctx.tokens,
            index: node.first_token,
            end: node.token_end,
        })
    }

    fn len_hint(self, container: Self::Container) -> usize {
        let mut cursor = self.cursor(container.node);
        // Each child node or token outside a child node is at most one entry.
        let children = if cursor.child == cursor.child_end {
            (cursor.token_end - cursor.token) as usize
        } else {
            let mut count = 0;
            while cursor.next(self.ctx.nodes).is_some() {
                count += 1;
            }
            count
        };
        match container.kind {
            // The braces are not entries.
            ContainerKind::Block => children.saturating_sub(2),
            ContainerKind::Root | ContainerKind::Parameter => children,
        }
    }
}

impl<'a, E> DocDe<'a> for TreeDoc<'_, 'a, E>
where
    E: Encoding,
{
    #[inline]
    fn visit_str<V: Visitor<'a>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        match self.ctx.encoding.decode(self.bytes(scalar)) {
            Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
            Cow::Owned(s) => visitor.visit_string(s),
        }
    }

    #[inline]
    fn visit_bytes<V: Visitor<'a>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_borrowed_bytes(self.bytes(scalar))
    }
}
