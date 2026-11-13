//! The model of a [`SyntaxTree`] for the shared serde rules.
//!
//! The deserializer visits each entry that it reads, so this model is the
//! hot path. It uses only the public interface of the tree: the typed AST
//! gives the parts of each entry, and a [`NodeId`] or a [`TokenId`] is the
//! handle that the deserializer keeps. A handle is four bytes, so the
//! deserializer copies it at no cost.

use super::engine::{Doc, DocDe, Elems, Item, Val, View, classify};
use super::error::{Error, Segment};
use crate::{
    Encoding, Scalar, ScalarError,
    text::lossless::{
        AstNode, Block, DirectiveKind, Field, HeaderedBlock, NodeId, Parameter, ReaderDirective,
        ScalarToken, SignificantChildren, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken,
        SyntaxTree, TextRange, TextSize, TokenId, UndefinedParameter, Value,
        resolve::{Resolution, ResolvedScalar},
    },
};
use serde::de::Visitor;
use std::borrow::Cow;
use std::sync::Arc;

/// A [`SyntaxTree`] and the encoding of its text.
pub(crate) struct TreeCtx<'c, 'a, E> {
    tree: SyntaxTree<'c, 'a>,
    encoding: E,
    /// The values of the variables and the calculations. When it is set, a
    /// resolved scalar gives its value, and a variable definition is not an
    /// entry.
    resolution: Option<Arc<Resolution>>,
}

impl<'c, 'a, E> TreeCtx<'c, 'a, E> {
    pub(crate) fn new(tree: SyntaxTree<'c, 'a>, encoding: E) -> Self {
        TreeCtx {
            tree,
            encoding,
            resolution: None,
        }
    }

    /// Set the resolution. The caller checks that it is of this tree.
    pub(crate) fn set_resolution(&mut self, resolution: Arc<Resolution>) {
        self.resolution = Some(resolution);
    }

    /// Return `true` when the context has a resolution.
    pub(crate) fn has_resolution(&self) -> bool {
        self.resolution.is_some()
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
///
/// When `RESOLVED` is `true`, the model gives the values of the resolution of the
/// context. A model without a resolution has no cost for it, because each
/// test of `RESOLVED` is a constant.
pub(crate) struct TreeDoc<'c, 'a, E, const RESOLVED: bool = false> {
    ctx: &'c TreeCtx<'c, 'a, E>,
}

impl<E, const RESOLVED: bool> Clone for TreeDoc<'_, '_, E, RESOLVED> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E, const RESOLVED: bool> Copy for TreeDoc<'_, '_, E, RESOLVED> {}

/// A value of the tree: a token or a node, by identity.
#[derive(Clone, Copy)]
pub(crate) enum TreeNode {
    Token(TokenId),
    Node(NodeId),
}

/// A scalar of the tree: a token, or a node that acts as a scalar (a
/// calculation or an interpolation), by identity. The deserializer reads the
/// bytes only when it needs them.
#[derive(Clone, Copy)]
pub(crate) enum TreeScalar {
    Token(TokenId),
    Node(NodeId),
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
    node: NodeId,
    kind: ContainerKind,
}

/// The handle of a value of the typed AST.
#[inline]
pub(super) fn value_handle(value: Value<'_, '_>) -> Option<TreeNode> {
    Some(match value {
        Value::Scalar(token) => TreeNode::Token(token.id()),
        Value::Operator(token) => TreeNode::Token(token.id()?),
        Value::Block(block) => TreeNode::Node(block.syntax().id()),
        Value::HeaderedBlock(block) => TreeNode::Node(block.syntax().id()),
        value => TreeNode::Node(value.syntax().as_node()?.id()),
    })
}

impl<'c, 'a, E: Encoding> TreeDoc<'c, 'a, E> {
    pub(crate) fn new(ctx: &'c TreeCtx<'c, 'a, E>) -> Self {
        TreeDoc { ctx }
    }
}

impl<'c, 'a, E: Encoding, const RESOLVED: bool> TreeDoc<'c, 'a, E, RESOLVED> {
    /// A model of the tree of `ctx`. When `RESOLVED` is `true`, the context must
    /// have a resolution.
    pub(crate) fn of(ctx: &'c TreeCtx<'c, 'a, E>) -> Self {
        debug_assert!(!RESOLVED || ctx.resolution.is_some());
        TreeDoc { ctx }
    }

    /// The value of a variable or a calculation from the resolution.
    #[inline]
    pub(super) fn resolved(self, scalar: TreeScalar) -> Option<&'c ResolvedScalar> {
        if !RESOLVED {
            return None;
        }
        match &self.ctx.resolution {
            None => None,
            Some(resolution) => self.resolved_in(resolution, scalar),
        }
    }

    /// Most deserializers have no resolution, so keep this lookup out of
    /// the hot path.
    #[cold]
    #[inline(never)]
    fn resolved_in(
        self,
        resolution: &'c Resolution,
        scalar: TreeScalar,
    ) -> Option<&'c ResolvedScalar> {
        match scalar {
            TreeScalar::Node(id) => resolution.node(id),
            TreeScalar::Token(id) => {
                let variable = matches!(
                    self.token(id).kind(),
                    SyntaxKind::Variable | SyntaxKind::VariableKey
                );
                variable.then(|| resolution.token(id)).flatten()
            }
        }
    }

    /// The root of the tree as a container.
    pub(crate) fn root(&self) -> TreeContainer {
        TreeContainer {
            node: self.ctx.tree.root().id(),
            kind: ContainerKind::Root,
        }
    }

    /// The node of a handle. Each handle comes from this tree.
    #[inline]
    pub(super) fn node(self, id: NodeId) -> SyntaxNode<'c, 'a> {
        self.ctx
            .tree
            .node_by_id(id)
            .expect("the handle is of this tree")
    }

    /// The token of a handle. Each handle comes from this tree.
    #[inline]
    fn token(self, id: TokenId) -> SyntaxToken<'c, 'a> {
        self.ctx
            .tree
            .token_by_id(id)
            .expect("the handle is of this tree")
    }

    /// The decoded text of a scalar. The encoding removes the escapes of a
    /// token. A node, such as a code payload, is source text, so its text
    /// keeps its escapes.
    pub(super) fn decode(self, scalar: TreeScalar) -> Cow<'a, str> {
        let bytes = self.bytes(scalar);
        match scalar {
            TreeScalar::Token(_) => self.ctx.encoding.decode(bytes),
            TreeScalar::Node(_) => decode_source(&self.ctx.encoding, bytes),
        }
    }

    /// The bytes of a scalar. The bytes of a quoted token have no quotes.
    #[inline]
    pub(super) fn bytes(self, scalar: TreeScalar) -> &'a [u8] {
        match scalar {
            TreeScalar::Node(id) => self.node(id).text(),
            TreeScalar::Token(id) => {
                let token = self.token(id);
                // An operator in value position is a scalar for serde, but
                // it is not a scalar token.
                match ScalarToken::cast(token) {
                    Some(scalar) => scalar.as_bytes(),
                    None => token.text(),
                }
            }
        }
    }

    #[inline]
    fn is_quoted(self, scalar: TreeScalar) -> bool {
        matches!(scalar, TreeScalar::Token(id) if self.token(id).kind() == SyntaxKind::Quoted)
    }

    #[inline]
    fn is_variable(self, scalar: TreeScalar) -> bool {
        matches!(scalar, TreeScalar::Token(id) if self.token(id).kind() == SyntaxKind::Variable)
    }

    #[inline]
    fn range(self, scalar: TreeScalar) -> TextRange {
        match scalar {
            TreeScalar::Node(id) => self.node(id).text_range(),
            TreeScalar::Token(id) => self.token(id).text_range(),
        }
    }

    /// The path to `node`, from the inner part to the outer part: the key
    /// of each field and the name of each parameter that holds `node`, and
    /// the index of each value in an array. When `node` is a field, its key
    /// is the first key. The path is the path that a read of the whole
    /// document gives.
    ///
    /// `index` gives the index of an element in a block, as
    /// [`ArrayIndex`] does.
    #[cold]
    pub(super) fn path_of(
        self,
        node: TreeNode,
        mut index: impl FnMut(Block<'c, 'a>, SyntaxElement<'c, 'a>) -> Option<usize>,
    ) -> Vec<Segment> {
        let encoding = &self.ctx.encoding;
        let mut element = match node {
            TreeNode::Token(id) => SyntaxElement::Token(self.token(id)),
            TreeNode::Node(id) => SyntaxElement::Node(self.node(id)),
        };
        let mut path = Vec::new();
        loop {
            if let Some(node) = element.as_node() {
                let key = match node.kind() {
                    SyntaxKind::Field => Field::cast(node).map(|f| f.key()),
                    SyntaxKind::Parameter => Parameter::cast(node).and_then(|p| p.name()),
                    SyntaxKind::UndefinedParameter => {
                        UndefinedParameter::cast(node).and_then(|p| p.name())
                    }
                    _ => None,
                };
                if let Some(key) = key {
                    path.push(Segment::Key(key.decode(encoding).into_owned()));
                }
            }
            let Some(parent) = element.parent() else {
                break;
            };
            if let Some(i) = Block::cast(parent).and_then(|block| index(block, element)) {
                path.push(Segment::Index(i));
            }
            element = SyntaxElement::Node(parent);
        }
        path
    }

    /// A parameter block as an entry.
    #[cold]
    #[inline(never)]
    fn parameter(self, node: SyntaxNode<'c, 'a>) -> Option<Item<Self>> {
        let undefined = node.kind() == SyntaxKind::UndefinedParameter;
        let name = match Parameter::cast(node) {
            Some(parameter) => parameter.name(),
            None => UndefinedParameter::cast(node).and_then(|p| p.name()),
        }?;
        Some(Item::Parameter {
            name: TreeScalar::Token(name.id()),
            undefined,
            body: TreeContainer {
                node: node.id(),
                kind: ContainerKind::Parameter,
            },
        })
    }

    /// The parts of a field: the key, the operator, and the value. Return
    /// `None` for a field that has no value, as `TextTape` ignores it.
    #[inline]
    fn field(self, field: Field<'c, 'a>) -> Option<Item<Self>> {
        // A variable definition is not data.
        if RESOLVED && field.is_variable_definition() {
            return None;
        }
        let value = value_handle(field.value()?)?;
        Some(Item::Field {
            key: TreeScalar::Token(field.key().id()),
            op: field.op(),
            value,
        })
    }
}

/// The scalar tokens of a block without child nodes. The braces and the
/// other loose tokens are not values, as [`TreeItems`] skips them.
#[derive(Clone)]
pub(crate) struct TreeScalars<'c, 'a> {
    children: SignificantChildren<'c, 'a>,
    /// The number of scalars that are left.
    remaining: usize,
}

impl Iterator for TreeScalars<'_, '_> {
    type Item = TreeNode;

    #[inline]
    fn next(&mut self) -> Option<TreeNode> {
        let token = self
            .children
            .find_map(|el| ScalarToken::cast(el.as_token()?))?;
        self.remaining -= 1;
        Some(TreeNode::Token(token.id()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for TreeScalars<'_, '_> {}

/// The index of each element in a sequence over a block, when the block is
/// an array: the elements that the deserializer gives, from the same
/// iterator. An object gives its entries in a map, with keys and not
/// indices.
pub(super) struct ArrayIndex {
    /// The start of each element and its index, in source order.
    starts: Vec<(TextSize, usize)>,
}

impl ArrayIndex {
    /// Index `block` with one pass over the elements of a sequence over it.
    /// Return `None` when the block is an object.
    pub(super) fn new<E: Encoding, const RESOLVED: bool>(
        doc: TreeDoc<'_, '_, E, RESOLVED>,
        block: Block<'_, '_>,
    ) -> Option<Self> {
        let container = TreeContainer {
            node: block.syntax().id(),
            kind: ContainerKind::Block,
        };
        if classify(doc, container).0 {
            return None;
        }
        let starts = Elems::of(doc, container)
            .enumerate()
            .filter_map(|(index, val)| match val {
                Val::Node(node) => Some((doc.node_range(node)?.start(), index)),
                _ => None,
            })
            .collect();
        Some(ArrayIndex { starts })
    }

    /// The index of `element`, if it is an element of the block.
    pub(super) fn get(&self, element: SyntaxElement<'_, '_>) -> Option<usize> {
        let start = element.text_range().start();
        let at = self.starts.binary_search_by_key(&start, |&(s, _)| s).ok()?;
        Some(self.starts[at].1)
    }
}

/// Decode source text with its escapes. The encoding removes each backslash
/// and the whitespace at the end of the text. Thus decode each part between
/// two backslashes with one more byte, `x`, that keeps the whitespace at the
/// end of the part. Then remove the `x` and put the backslashes back.
fn decode_source<'a, E: Encoding>(encoding: &E, text: &'a [u8]) -> Cow<'a, str> {
    if !text.contains(&b'\\') {
        return encoding.decode(text);
    }
    let mut out = String::with_capacity(text.len());
    let mut part_x = Vec::new();
    let mut parts = text.split(|&byte| byte == b'\\').peekable();
    while let Some(part) = parts.next() {
        if parts.peek().is_none() {
            out.push_str(&encoding.decode(part));
            break;
        }
        part_x.clear();
        part_x.extend_from_slice(part);
        part_x.push(b'x');
        let decoded = encoding.decode(&part_x);
        out.push_str(decoded.strip_suffix('x').unwrap_or(&decoded));
        out.push('\\');
    }
    Cow::Owned(out)
}

/// The float of a resolved value. See [`TreeDoc::to_f64`].
fn resolved_f64(value: &ResolvedScalar) -> Result<f64, ScalarError> {
    let text = value.as_bytes();
    if !is_decimal(text) {
        return value.as_scalar().to_f64();
    }
    // A decimal can be too large for an f64, which gives infinity.
    let float: f64 = std::str::from_utf8(text)
        .ok()
        .and_then(|text| text.parse().ok())
        .ok_or(ScalarError::AllDigits)?;
    if float.is_finite() {
        Ok(float)
    } else {
        Err(ScalarError::Overflow)
    }
}

/// Return `true` for a decimal number: an optional sign, digits, and an
/// optional `.` and more digits. An exponent, `inf`, and `NaN` are not.
fn is_decimal(text: &[u8]) -> bool {
    let text = text
        .strip_prefix(b"-")
        .or_else(|| text.strip_prefix(b"+"))
        .unwrap_or(text);
    let (whole, fraction) = match text.iter().position(|&b| b == b'.') {
        Some(dot) => (&text[..dot], &text[dot + 1..]),
        None => (text, &b""[..]),
    };
    (!whole.is_empty() || !fraction.is_empty())
        && whole.iter().chain(fraction).all(u8::is_ascii_digit)
}

/// Return `true` for an element that [`TreeScalars`] gives.
fn is_scalar(el: SyntaxElement<'_, '_>) -> bool {
    el.as_token()
        .is_some_and(|token| ScalarToken::cast(token).is_some())
}

/// The entries of a container of the tree.
pub(crate) struct TreeItems<'c, 'a, E, const RESOLVED: bool> {
    doc: TreeDoc<'c, 'a, E, RESOLVED>,
    children: SignificantChildren<'c, 'a>,
    /// For a parameter: the header (`[[name]`) is done.
    header_done: bool,
    parameter: bool,
    /// For a parameter: the bracket that closes the body is done.
    done: bool,
    /// For a parameter: the `[` tokens of the body that are open. A body
    /// `]` closes one of them before it closes the body, as in the parser.
    open_brackets: u32,
}

impl<E, const RESOLVED: bool> Clone for TreeItems<'_, '_, E, RESOLVED> {
    fn clone(&self) -> Self {
        TreeItems {
            doc: self.doc,
            children: self.children.clone(),
            header_done: self.header_done,
            parameter: self.parameter,
            done: self.done,
            open_brackets: self.open_brackets,
        }
    }
}

impl<'c, 'a, E, const RESOLVED: bool> Iterator for TreeItems<'c, 'a, E, RESOLVED>
where
    E: Encoding,
{
    type Item = Item<TreeDoc<'c, 'a, E, RESOLVED>>;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        loop {
            match self.children.next()? {
                SyntaxElement::Token(token) => {
                    let kind = token.kind();
                    if !self.header_done {
                        self.header_done = kind == SyntaxKind::CloseBracket;
                        continue;
                    }
                    if let Some(scalar) = ScalarToken::cast(token) {
                        return Some(Item::Value(TreeNode::Token(scalar.id())));
                    }
                    if self.parameter {
                        match kind {
                            SyntaxKind::OpenBracket => self.open_brackets += 1,
                            SyntaxKind::CloseBracket if self.open_brackets > 0 => {
                                self.open_brackets -= 1;
                            }
                            SyntaxKind::CloseBracket | SyntaxKind::MissingCloseBracket => {
                                self.done = true;
                                return None;
                            }
                            _ => {}
                        }
                    }
                }
                SyntaxElement::Node(node) => match node.kind() {
                    SyntaxKind::Field => {
                        let field = Field::cast(node).and_then(|field| self.doc.field(field));
                        if let Some(item) = field {
                            return Some(item);
                        }
                    }
                    SyntaxKind::Block
                    | SyntaxKind::HeaderedBlock
                    | SyntaxKind::Calc
                    | SyntaxKind::Interpolation
                    | SyntaxKind::Code => return Some(Item::Value(TreeNode::Node(node.id()))),
                    // With a resolution, `@:load_variable name` is a value, as
                    // `@name` is. Each other directive is not an entry.
                    SyntaxKind::ReaderDirective
                        if RESOLVED
                            && ReaderDirective::cast(node)
                                .is_some_and(|d| d.kind() == DirectiveKind::LoadVariable) =>
                    {
                        return Some(Item::Value(TreeNode::Node(node.id())));
                    }
                    SyntaxKind::Parameter | SyntaxKind::UndefinedParameter => {
                        if let Some(item) = self.doc.parameter(node) {
                            return Some(item);
                        }
                    }
                    _ => {}
                },
            }
        }
    }
}

impl<'c, 'a, E, const RESOLVED: bool> Doc for TreeDoc<'c, 'a, E, RESOLVED>
where
    E: Encoding,
{
    type Node = TreeNode;
    type Scalar = TreeScalar;
    type Container = TreeContainer;
    type Items = TreeItems<'c, 'a, E, RESOLVED>;
    type Scalars = TreeScalars<'c, 'a>;

    #[inline]
    fn view(self, node: Self::Node) -> View<Self> {
        let id = match node {
            TreeNode::Token(id) => return View::Scalar(TreeScalar::Token(id)),
            TreeNode::Node(id) => id,
        };
        let block = |node| TreeContainer {
            node,
            kind: ContainerKind::Block,
        };
        let node = self.node(id);
        match node.kind() {
            SyntaxKind::Block => View::Block(block(id)),
            SyntaxKind::HeaderedBlock => match HeaderedBlock::cast(node) {
                Some(headered) => View::Headered(
                    TreeScalar::Token(headered.header().id()),
                    block(headered.block().syntax().id()),
                ),
                None => View::Scalar(TreeScalar::Node(id)),
            },
            SyntaxKind::Calc => {
                let scalar = TreeScalar::Node(id);
                match self.resolved(scalar) {
                    Some(_) => View::Scalar(scalar),
                    None => View::Calc(scalar),
                }
            }
            _ => View::Scalar(TreeScalar::Node(id)),
        }
    }

    #[inline]
    fn scalar_of(self, node: Self::Node) -> Option<Self::Scalar> {
        match node {
            // A token value is always a scalar or an operator.
            TreeNode::Token(id) => Some(TreeScalar::Token(id)),
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
            children: self.node(container.node).significant_children(),
            header_done: !parameter,
            parameter,
            done: false,
            open_brackets: 0,
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
        match self.resolved(scalar) {
            Some(value) => value.is_quoted(),
            None => self.is_quoted(scalar),
        }
    }

    #[inline]
    fn variable(self, scalar: Self::Scalar) -> bool {
        self.is_variable(scalar) && self.resolved(scalar).is_none()
    }

    #[inline]
    fn with_scalar<R>(self, scalar: Self::Scalar, f: impl FnOnce(Scalar<'_>) -> R) -> R {
        match self.resolved(scalar) {
            Some(value) => f(value.as_scalar()),
            None => f(Scalar::new(self.bytes(scalar))),
        }
    }

    /// A resolved value is text that the resolver made or took from a
    /// different file. Its decimal number can have more digits than
    /// [`Scalar::to_f64`] reads exactly, so such text uses the parser of
    /// the standard library, which rounds correctly. Other text keeps the
    /// rules of `TextDeserializer`.
    #[inline]
    fn to_f64(self, scalar: Self::Scalar) -> Result<f64, ScalarError> {
        match self.resolved(scalar) {
            Some(value) => resolved_f64(value),
            None => self.with_scalar(scalar, |s| s.to_f64()),
        }
    }

    fn string(self, scalar: Self::Scalar) -> String {
        match self.resolved(scalar) {
            Some(value) => value.decode(&self.ctx.encoding).into_owned(),
            None => self.decode(scalar).into_owned(),
        }
    }

    fn node_range(self, node: Self::Node) -> Option<TextRange> {
        Some(match node {
            TreeNode::Token(id) => self.token(id).text_range(),
            TreeNode::Node(id) => self.node(id).text_range(),
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
        let node = self.node(container.node);
        let leaf = container.kind == ContainerKind::Block && node.first_child().is_none();
        leaf.then(|| {
            let children = node.significant_children();
            let remaining = children.clone().filter(|el| is_scalar(*el)).count();
            TreeScalars {
                children,
                remaining,
            }
        })
    }

    /// A container with child nodes has no hint. To count its entries, the
    /// deserializer must read all of its children one more time. That costs
    /// more than the growth of a collection: about 8% of the time to
    /// deserialize an EU4 save. A block without child nodes uses
    /// [`TreeScalars`], which counts its scalars.
    fn len_hint(self, _container: Self::Container) -> Option<usize> {
        None
    }

    fn capacity_hint(self, container: Self::Container) -> usize {
        let node = self.node(container.node);
        if node.first_child().is_some() {
            return 0;
        }
        // Each token is at most one entry.
        let children = node.significant_children().size_hint().1.unwrap_or(0);
        match container.kind {
            // The braces are not entries.
            ContainerKind::Block => children.saturating_sub(2),
            ContainerKind::Root | ContainerKind::Parameter => children,
        }
    }
}

impl<'a, E, const RESOLVED: bool> DocDe<'a> for TreeDoc<'_, 'a, E, RESOLVED>
where
    E: Encoding,
{
    #[inline]
    fn visit_str<V: Visitor<'a>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        if let Some(value) = self.resolved(scalar) {
            return match value.decode(&self.ctx.encoding) {
                Cow::Borrowed(s) => visitor.visit_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            };
        }
        match self.decode(scalar) {
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
        match self.resolved(scalar) {
            Some(value) => visitor.visit_bytes(value.as_bytes()),
            None => visitor.visit_borrowed_bytes(self.bytes(scalar)),
        }
    }
}
