//! The typed view. Each type wraps a [`SyntaxNode`] of one kind.

use super::{
    SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, SyntaxTree, TextRange, TextSize, TokenId,
};
use crate::{Encoding, Scalar, text::Operator};
use std::borrow::Cow;

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
            #[inline]
            fn cast(node: SyntaxNode<'t, 'a>) -> Option<Self> {
                if node.kind() == SyntaxKind::$kind {
                    Some($name(node))
                } else {
                    None
                }
            }
            #[inline]
            fn syntax(&self) -> SyntaxNode<'t, 'a> {
                self.0
            }
        }
    )+};
}

ast_nodes! {
    /// The whole document ([`SyntaxKind::Root`]) and the entry point of the
    /// typed view. [`Parse::ast`](super::Parse::ast) returns the root.
    ///
    /// - [`get`](Root::get) finds the first field with a key, and
    ///   [`get_all`](Root::get_all) finds each field with a key.
    /// - [`fields`](Root::fields) gives each field, including a field with no
    ///   operator, such as `key { ... }`.
    ///   [`items`](Root::items) gives every item, including bare values and
    ///   other constructs, such as an EU4 parameter block.
    ///
    /// A [`Field`] has a key and an optional operator. It also has a
    /// [`Value`], unless the source has no value after the operator. A value
    /// is a scalar, a [`Block`], or a different construct, such as a
    /// [`HeaderedBlock`] or a [`Calc`]. Use [`Value::as_block`] to go down one
    /// level; the block has the same methods as the root.
    ///
    /// The typed view keeps every significant element: an element that has no
    /// typed form, such as a stray operator, is an [`Item::Other`]. The typed
    /// view does not show the trivia. [`AstNode::syntax`] gives the
    /// underlying [`SyntaxNode`], which shows the comments and the
    /// whitespace:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, AstNode};
    ///
    /// let parse = parse(b"capital = 169 # Rennes\nadd_core = BRE\nadd_core = FRA\n");
    /// let root = parse.ast();
    ///
    /// let capital = root.get(b"capital").and_then(|f| f.value());
    /// assert_eq!(capital.and_then(|v| v.to_u64()), Some(169));
    /// assert_eq!(root.get_all(b"add_core").count(), 2);
    /// assert_eq!(root.items().count(), 3);
    /// assert!(root.syntax().has_comment());
    /// ```
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
    /// assert_eq!(color.header().text(), b"hsv");
    ///
    /// let hsv: Vec<f64> = color.block().values().filter_map(|v| v.to_f64()).collect();
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
    /// assert_eq!(product.op_token().text(), b"*");
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
    /// A CK3 reader directive ([`SyntaxKind::ReaderDirective`]).
    ///
    /// The game runs a directive while it reads the file, before the file
    /// has a structure. The tree keeps the directive as one node, and does
    /// not expand it:
    ///
    /// ```
    /// use jomini::text::lossless::{parse, DirectiveKind};
    ///
    /// let parse = parse(b"@:insert cool_block = { trait = lunatic }");
    /// let item = parse.ast().items().next().unwrap();
    /// let directive = item.as_reader_directive().unwrap();
    /// assert_eq!(directive.kind(), DirectiveKind::Insert);
    /// assert_eq!(directive.argument().unwrap().text(), b"cool_block");
    /// let block = directive.value().unwrap().as_block().unwrap();
    /// assert_eq!(block.fields().count(), 1);
    /// ```
    ReaderDirective => ReaderDirective,
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
    /// A CK3 reader directive, such as `@:insert name`.
    ReaderDirective(ReaderDirective<'t, 'a>),
    /// A bare value: an array element or a loose value.
    Value(Value<'t, 'a>),
    /// A significant element that is not a field or a value, for example an
    /// operator or a bracket out of place, or a [`SyntaxKind::Bogus`] node.
    /// Thus the typed view does not lose an element.
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
    Scalar(ScalarToken<'t, 'a>),
    /// An operator that is the value of a field, as in `OPERATOR = <=`. A
    /// bare value in a block is never an operator.
    Operator(SyntaxToken<'t, 'a>),
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
    /// A CK3 reader directive in value position, such as
    /// `@:load_variable name`.
    ReaderDirective(ReaderDirective<'t, 'a>),
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

/// Remove the quotes from the bytes of a quoted token. A string that has no
/// closing quote keeps its last byte. A `"` after an odd number of
/// backslashes is escaped, so it is not a closing quote.
#[inline]
pub(super) fn strip_quotes(b: &[u8]) -> &[u8] {
    let Some(inner) = b.strip_prefix(b"\"") else {
        return b;
    };
    match inner.strip_suffix(b"\"") {
        Some(body) if body.iter().rev().take_while(|&&c| c == b'\\').count() % 2 == 0 => body,
        _ => inner,
    }
}

impl<'t, 'a> SyntaxToken<'t, 'a> {
    /// The identity of a significant token in its tree. Return `None` for
    /// a trivia token, because the tree does not store trivia. See
    /// [`TokenId`].
    #[inline]
    pub fn id(&self) -> Option<TokenId> {
        (!self.trivia).then_some(TokenId(self.idx))
    }
}

/// A token that is a scalar: a key, a value, or a header.
///
/// The kind of the token is [`SyntaxKind::Unquoted`], [`SyntaxKind::Quoted`],
/// [`SyntaxKind::Variable`], [`SyntaxKind::VariableKey`], or
/// [`SyntaxKind::MacroParam`]. Only such a token can become a `ScalarToken`,
/// so the scalar accessors are not available for a token such as `{`.
///
/// [`text`](ScalarToken::text) gives the bytes of the source, with the
/// quotes of a quoted string. [`as_scalar`](ScalarToken::as_scalar) and
/// [`as_bytes`](ScalarToken::as_bytes) give the value without the quotes.
///
/// ```
/// use jomini::text::lossless::{parse, ScalarToken};
///
/// let parse = parse(b"name = \"Jean\" age = 30");
/// let fields: Vec<_> = parse.ast().fields().collect();
///
/// let name = fields[0].value().unwrap().as_scalar_token().unwrap();
/// assert!(name.is_quoted());
/// assert_eq!(name.text(), b"\"Jean\"");
/// assert_eq!(name.as_bytes(), b"Jean");
///
/// let age = fields[1].value().unwrap().as_scalar_token().unwrap();
/// assert_eq!(age.as_scalar().to_u64().unwrap(), 30);
///
/// let operator = fields[1].op_token().unwrap();
/// assert!(ScalarToken::cast(operator).is_none());
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScalarToken<'t, 'a>(pub(super) SyntaxToken<'t, 'a>);

impl<'t, 'a> ScalarToken<'t, 'a> {
    /// Wrap `token` if its kind is a scalar kind (see
    /// [`SyntaxKind::is_scalar`]), otherwise `None`.
    #[inline]
    pub fn cast(token: SyntaxToken<'t, 'a>) -> Option<Self> {
        token.kind().is_scalar().then_some(ScalarToken(token))
    }

    /// The untyped token.
    #[inline]
    pub fn syntax(&self) -> SyntaxToken<'t, 'a> {
        self.0
    }

    /// The kind of the token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.0.kind
    }

    /// The identity of the token in its tree. A scalar token is always a
    /// significant token, so it always has an identity. See [`TokenId`].
    #[inline]
    pub fn id(&self) -> TokenId {
        TokenId(self.0.idx)
    }

    /// The bytes of the token in the source. A quoted string has its quotes.
    #[inline]
    pub fn text(&self) -> &'a [u8] {
        self.0.text()
    }

    /// The byte range of the token in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.0.range
    }

    /// Return `true` for a [`SyntaxKind::Quoted`] token.
    #[inline]
    pub fn is_quoted(&self) -> bool {
        self.0.kind == SyntaxKind::Quoted
    }

    /// The value of the token: the bytes of the source without the quotes
    /// of a quoted string. The escape characters in a quoted string stay.
    #[inline]
    pub fn as_bytes(&self) -> &'a [u8] {
        let text = self.0.text();
        if self.is_quoted() {
            strip_quotes(text)
        } else {
            text
        }
    }

    /// The value of the token as a [`Scalar`], which converts it to a number
    /// or a boolean.
    #[inline]
    pub fn as_scalar(&self) -> Scalar<'a> {
        Scalar::new(self.as_bytes())
    }

    /// Decode the value of the token into a string with `encoding`. The
    /// encoding removes the escape characters of a quoted string.
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
    #[inline]
    pub fn decode<E: Encoding>(&self, encoding: E) -> Cow<'a, str> {
        encoding.decode(self.as_bytes())
    }

    /// The name of a reader variable without its `@` or `@@` prefix. Return
    /// `None` when the token is not a [`SyntaxKind::Variable`] or a
    /// [`SyntaxKind::VariableKey`].
    ///
    /// ```
    /// use jomini::text::lossless::{parse, ScalarToken};
    ///
    /// let parse = parse(b"@size = 10 width = @size");
    /// let names: Vec<_> = parse
    ///     .significant_tokens()
    ///     .filter_map(ScalarToken::cast)
    ///     .filter_map(|t| t.variable_name())
    ///     .collect();
    /// assert_eq!(names, [&b"size"[..], b"size"]);
    /// ```
    #[inline]
    pub fn variable_name(&self) -> Option<&'a [u8]> {
        match self.0.kind {
            SyntaxKind::Variable => self.text().strip_prefix(b"@"),
            SyntaxKind::VariableKey => self.text().strip_prefix(b"@@"),
            _ => None,
        }
    }
}

/// The items of a root or a block: the significant children without the
/// braces.
#[inline]
fn child_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
    node.significant_children().filter_map(item_from_element)
}

#[inline]
fn item_from_element<'t, 'a>(el: SyntaxElement<'t, 'a>) -> Option<Item<'t, 'a>> {
    if el.kind() == SyntaxKind::CloseBrace {
        return None;
    }
    parameter_body_item(el)
}

/// The closing bracket of the parameter body. The body closes with the last
/// bracket of the node. When that bracket is a missing token, the body is not
/// closed, and a `]` before it closes an inner bracket.
#[inline]
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

#[inline]
fn parameter_name<'t, 'a>(node: SyntaxNode<'t, 'a>) -> Option<ScalarToken<'t, 'a>> {
    let mut open_brackets = 0;
    node.significant_children().find_map(|el| match el {
        SyntaxElement::Token(token) if token.kind() == SyntaxKind::OpenBracket => {
            open_brackets += 1;
            None
        }
        SyntaxElement::Token(token) if open_brackets >= 2 => ScalarToken::cast(token),
        _ => None,
    })
}

#[inline]
fn parameter_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
    let mut header_done = false;
    let mut body_done = false;
    // The `[` tokens of the body that are open. A body `]` closes one of
    // them before it closes the body, as in the parser.
    let mut open_brackets = 0u32;
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
        match el.kind() {
            SyntaxKind::OpenBracket => open_brackets += 1,
            SyntaxKind::CloseBracket if open_brackets > 0 => open_brackets -= 1,
            SyntaxKind::CloseBracket | SyntaxKind::MissingCloseBracket => {
                body_done = true;
                return None;
            }
            _ => {}
        }
        parameter_body_item(el)
    })
}

/// Convert an element to an item. A `}` is an item, because a parameter body
/// can close a brace that an earlier parameter opens.
#[inline]
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
            SyntaxKind::ReaderDirective => ReaderDirective::cast(n).map(Item::ReaderDirective),
            _ => Value::cast_element(el)
                .map(Item::Value)
                .or(Some(Item::Other(el))),
        },
        _ => Value::cast_element(el)
            .map(Item::Value)
            .or(Some(Item::Other(el))),
    }
}

/// The items between the `[[` and the `]]` of a code node, with no
/// allocation.
#[inline]
fn code_items<'t, 'a>(
    node: SyntaxNode<'t, 'a>,
) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
    let mut children = node.significant_children().peekable();
    // Skip the `code` keyword and the `[[`.
    let mut after_open = false;
    for child in children.by_ref() {
        let open = child.kind() == SyntaxKind::OpenBracket;
        if open && after_open {
            break;
        }
        after_open = open;
    }
    std::iter::from_fn(move || {
        let child = children.next()?;
        let closes = child.kind() == SyntaxKind::CloseBracket
            && children
                .peek()
                .is_some_and(|next| next.kind() == SyntaxKind::CloseBracket);
        (!closes).then_some(child)
    })
    .fuse()
    .filter_map(parameter_body_item)
}

impl<'t, 'a> Root<'t, 'a> {
    /// Every top-level item, in source order (trivia skipped).
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        child_items(self.syntax())
    }

    /// The top-level fields, including a field with no operator, such as
    /// `key { ... }`.
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The first field whose key is `key`. See [`get_all`](Self::get_all).
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"capital = 169 trade = { goods = wine }");
    /// let root = parse.ast();
    /// assert_eq!(root.get(b"capital").unwrap().value().unwrap().to_u64(), Some(169));
    /// let trade = root.get(b"trade").unwrap().value().unwrap().as_block().unwrap();
    /// assert_eq!(trade.get(b"goods").unwrap().value().unwrap().syntax().text(), b"wine");
    /// assert!(root.get(b"owner").is_none());
    /// ```
    #[inline]
    pub fn get(&self, key: &[u8]) -> Option<Field<'t, 'a>> {
        self.get_all(key).next()
    }

    /// Each field whose key is `key`, in source order. A game file can use
    /// a key more than once.
    ///
    /// The comparison uses the bytes of the key without the quotes, and it
    /// is case-sensitive. The search is linear.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"add_core = FRA owner = FRA add_core = BUR");
    /// let cores: Vec<&[u8]> = parse
    ///     .ast()
    ///     .get_all(b"add_core")
    ///     .filter_map(|f| f.value())
    ///     .map(|v| v.syntax().text())
    ///     .collect();
    /// assert_eq!(cores, [&b"FRA"[..], b"BUR"]);
    /// ```
    #[inline]
    pub fn get_all<'k>(
        &self,
        key: &'k [u8],
    ) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a, 'k> {
        fields_named(self.syntax(), key)
    }
}

/// The fields of `node` whose key is `key`.
#[inline]
fn fields_named<'t, 'a, 'k>(
    node: SyntaxNode<'t, 'a>,
    key: &'k [u8],
) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a, 'k> {
    node.significant_children()
        .filter_map(|el| el.as_node().and_then(Field::cast))
        .filter(move |field| field.key_is(key))
}

impl<'t, 'a> Field<'t, 'a> {
    /// Return `true` when the key of the field is `key`. The comparison uses
    /// the bytes of the key without the quotes, and it is case-sensitive.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"\"owner\" = FRA");
    /// let field = parse.ast().fields().next().unwrap();
    /// assert!(field.key_is(b"owner"));
    /// assert!(!field.key_is(b"Owner"));
    /// ```
    #[inline]
    pub fn key_is(&self, key: &[u8]) -> bool {
        self.key().as_bytes() == key
    }

    /// Return `true` when the field defines a reader variable, as in
    /// `@size = 10`.
    #[inline]
    pub fn is_variable_definition(&self) -> bool {
        self.key().kind() == SyntaxKind::Variable
    }

    /// The key scalar token (the left-hand side). The parser opens a field
    /// only at a scalar token, so each field has a key.
    ///
    /// The text of a quoted key includes the quotes. Use
    /// [`key_is`](Field::key_is) or [`ScalarToken::as_bytes`] to compare a
    /// key without them.
    #[inline]
    pub fn key(&self) -> ScalarToken<'t, 'a> {
        let node = self.0;
        ScalarToken(node.tree.token(node.data().first_token))
    }

    /// The index of the operator token, when the field has one. The
    /// operator is the token after the key.
    #[inline]
    fn op_index(&self) -> Option<u32> {
        let node = self.0;
        let data = node.data();
        let index = data.first_token + 1;
        let tokens = &node.tree.data.tokens;
        (index < data.token_end && tokens[index as usize].kind == SyntaxKind::Operator)
            .then_some(index)
    }

    /// The operator token (`=`, `==`, `?=`, `<`, …), or `None` for a
    /// `key { ... }` field that has no operator.
    #[inline]
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.op_index().map(|index| self.0.tree.token(index))
    }

    /// The concrete [`Operator`], re-derived from the operator token's text.
    #[inline]
    pub fn op(&self) -> Option<Operator> {
        self.op_token().and_then(|t| t.op())
    }

    /// The element after the key and the operator.
    #[inline]
    fn value_element(&self) -> Option<SyntaxElement<'t, 'a>> {
        let node = self.0;
        let data = node.data();
        let index = match self.op_index() {
            Some(op) => op + 1,
            None => data.first_token + 1,
        };
        if index >= data.token_end {
            return None;
        }
        // The nodes are in pre-order, so a child node that starts at the
        // token is the node directly after the field.
        let child = node.idx + 1;
        if child < data.subtree_end && node.tree.data.nodes[child as usize].first_token == index {
            return Some(SyntaxElement::Node(node.node(child)));
        }
        Some(SyntaxElement::Token(node.tree.token(index)))
    }

    /// The value (the right-hand side), or `None` if it is missing.
    ///
    /// A field with no operator (`key { ... }`) has its block as the value.
    #[inline]
    pub fn value(&self) -> Option<Value<'t, 'a>> {
        Value::after_operator(self.value_element()?)
    }
}

impl<'t, 'a> Block<'t, 'a> {
    /// Every item between the braces, in source order (trivia skipped).
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        child_items(self.syntax())
    }

    /// The fields directly inside this block, including a field with no
    /// operator, such as `key { ... }`.
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values (array elements) directly inside this block.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// The first field whose key is `key`. See [`get_all`](Self::get_all).
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"trade = { goods = wine }");
    /// let trade = parse.ast().get(b"trade").unwrap().value().unwrap().as_block().unwrap();
    /// assert_eq!(trade.get(b"goods").unwrap().value().unwrap().syntax().text(), b"wine");
    /// assert!(trade.get(b"owner").is_none());
    /// ```
    #[inline]
    pub fn get(&self, key: &[u8]) -> Option<Field<'t, 'a>> {
        self.get_all(key).next()
    }

    /// Each field whose key is `key`, in source order. A game file can use
    /// a key more than once.
    ///
    /// The comparison uses the bytes of the key without the quotes, and it
    /// is case-sensitive. The search is linear.
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"history = { add_core = FRA owner = FRA add_core = BUR }");
    /// let history = parse.ast().get(b"history").unwrap().value().unwrap().as_block().unwrap();
    /// let cores: Vec<&[u8]> = history
    ///     .get_all(b"add_core")
    ///     .filter_map(|f| f.value())
    ///     .map(|v| v.syntax().text())
    ///     .collect();
    /// assert_eq!(cores, [&b"FRA"[..], b"BUR"]);
    /// ```
    #[inline]
    pub fn get_all<'k>(
        &self,
        key: &'k [u8],
    ) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a, 'k> {
        fields_named(self.syntax(), key)
    }

    /// Whether the block has no entries (only braces, whitespace, comments).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.items().next().is_none()
    }

    /// Return the physical closing brace, if the source contains one.
    #[inline]
    pub fn close_brace(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .find(|token| token.kind() == SyntaxKind::CloseBrace)
    }

    /// Return `true` when the block has a physical closing brace.
    #[inline]
    pub fn is_closed_in_source(&self) -> bool {
        self.close_brace().is_some()
    }
}

impl<'t, 'a> Parameter<'t, 'a> {
    /// The parameter name in `[[name] ... ]`.
    #[inline]
    pub fn name(&self) -> Option<ScalarToken<'t, 'a>> {
        parameter_name(self.syntax())
    }

    /// The items in the parameter body.
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the parameter body.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Return the physical body closing bracket, if the source contains one.
    #[inline]
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_close_bracket(self.syntax())
    }

    /// Return `true` when the parameter body has a physical closing bracket.
    #[inline]
    pub fn is_closed_in_source(&self) -> bool {
        self.close_bracket().is_some()
    }
}

impl<'t, 'a> UndefinedParameter<'t, 'a> {
    /// The parameter name in `[[!name] ... ]`.
    #[inline]
    pub fn name(&self) -> Option<ScalarToken<'t, 'a>> {
        parameter_name(self.syntax())
    }

    /// The items in the parameter body.
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        parameter_items(self.syntax())
    }

    /// The fields directly inside the parameter body.
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the parameter body.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Return the physical body closing bracket, if the source contains one.
    #[inline]
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        parameter_close_bracket(self.syntax())
    }

    /// Return `true` when the parameter body has a physical closing bracket.
    #[inline]
    pub fn is_closed_in_source(&self) -> bool {
        self.close_bracket().is_some()
    }
}

impl<'t, 'a> Code<'t, 'a> {
    /// The `code` keyword of the statement form `code [[ ... ]]`. In the form
    /// `code = [[ ... ]]`, the keyword and the operator belong to the
    /// enclosing [`Field`], and this node is only the payload.
    #[inline]
    pub fn keyword(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .next()
            .filter(|token| token.kind() == SyntaxKind::Unquoted)
    }

    /// The items in the code payload.
    #[inline]
    pub fn items(&self) -> impl Iterator<Item = Item<'t, 'a>> + use<'t, 'a> {
        code_items(self.syntax())
    }

    /// The fields directly inside the code payload.
    #[inline]
    pub fn fields(&self) -> impl Iterator<Item = Field<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_field())
    }

    /// The bare values directly inside the code payload.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = Value<'t, 'a>> + use<'t, 'a> {
        self.items().filter_map(|el| el.as_value())
    }

    /// Return `true` when the source has the two `]` that close the payload.
    /// Trivia can be between them, as for the parser.
    #[inline]
    pub fn is_closed_in_source(&self) -> bool {
        let mut last = [None, None];
        for child in self.syntax().significant_children() {
            last = [last[1], Some(child.kind())];
        }
        last == [Some(SyntaxKind::CloseBracket); 2]
    }
}

/// The kind of a CK3 [`ReaderDirective`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DirectiveKind {
    /// `@:define name = { tokens }` defines a block of tokens to insert.
    Define,
    /// `@:insert name` or `@:insert name = { arguments }` inserts a block.
    Insert,
    /// `@:register_variable name = value` defines a variable, as
    /// `@name = value` does.
    RegisterVariable,
    /// `@:load_variable name` reads a variable, as `@name` does.
    LoadVariable,
    /// `@:log "text"` writes the text to the debug log.
    Log,
    /// `@:assert` stops the game in a debug build.
    Assert,
    /// A directive that the game does not know.
    Unknown,
}

/// The arguments that a directive takes.
#[derive(Debug, Clone, Copy)]
pub(super) struct DirectiveForm {
    /// A name or a text follows the directive.
    pub(super) argument: bool,
    /// `= value` can follow the argument.
    pub(super) value: bool,
}

impl DirectiveKind {
    /// The kind of a directive token, such as `@:insert`.
    #[inline]
    pub(super) fn from_token(text: &[u8]) -> Self {
        match text.strip_prefix(b"@:").unwrap_or(text) {
            b"define" => DirectiveKind::Define,
            b"insert" => DirectiveKind::Insert,
            b"register_variable" => DirectiveKind::RegisterVariable,
            b"load_variable" => DirectiveKind::LoadVariable,
            b"log" => DirectiveKind::Log,
            b"assert" => DirectiveKind::Assert,
            _ => DirectiveKind::Unknown,
        }
    }

    pub(super) const fn form(self) -> DirectiveForm {
        let (argument, value) = match self {
            DirectiveKind::Define | DirectiveKind::Insert | DirectiveKind::RegisterVariable => {
                (true, true)
            }
            DirectiveKind::LoadVariable | DirectiveKind::Log => (true, false),
            DirectiveKind::Assert | DirectiveKind::Unknown => (false, false),
        };
        DirectiveForm { argument, value }
    }
}

impl<'t, 'a> ReaderDirective<'t, 'a> {
    /// The directive token, such as `@:insert`.
    #[inline]
    pub fn directive(&self) -> SyntaxToken<'t, 'a> {
        self.syntax()
            .significant_child_tokens()
            .next()
            .expect("a reader directive starts with its directive token")
    }

    /// The kind of the directive.
    #[inline]
    pub fn kind(&self) -> DirectiveKind {
        DirectiveKind::from_token(self.directive().text())
    }

    /// The name or the text after the directive: the variable name of
    /// `@:register_variable` and `@:load_variable`, the block name of
    /// `@:define` and `@:insert`, and the text of `@:log`.
    #[inline]
    pub fn argument(&self) -> Option<ScalarToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .nth(1)
            .and_then(ScalarToken::cast)
    }

    /// The operator after the argument, if any.
    #[inline]
    pub fn op_token(&self) -> Option<SyntaxToken<'t, 'a>> {
        self.syntax()
            .significant_child_tokens()
            .nth(2)
            .filter(|token| token.kind() == SyntaxKind::Operator)
    }

    /// The value after the operator: the token block of `@:define`, the
    /// arguments of `@:insert`, and the value of `@:register_variable`.
    #[inline]
    pub fn value(&self) -> Option<Value<'t, 'a>> {
        self.op_token()?;
        Value::after_operator(self.syntax().significant_children().nth(3)?)
    }
}

impl<'t, 'a> Interpolation<'t, 'a> {
    /// The elements between the outer brackets. A nested bracket, as in
    /// `[outer[inner]]`, is part of the body.
    #[inline]
    pub fn body(&self) -> impl Iterator<Item = SyntaxElement<'t, 'a>> + use<'t, 'a> {
        let node = self.syntax();
        let open = TextRange::at(node.text_range().start(), TextSize::new(1));
        let close = self.close_bracket().map(|token| token.text_range());
        node.children().filter(move |element| {
            let range = element.text_range();
            element.kind() != SyntaxKind::MissingCloseBracket
                && range != open
                && Some(range) != close
        })
    }

    /// The `]` that closes the outer `[`, if the source has it.
    #[inline]
    pub fn close_bracket(&self) -> Option<SyntaxToken<'t, 'a>> {
        // The parser stops at the `]` that closes the outer `[`. Thus the
        // brackets of the node balance only when the source has that `]`.
        let mut depth = 0i32;
        let mut last = None;
        for token in self.syntax().significant_child_tokens() {
            match token.kind() {
                SyntaxKind::OpenBracket => depth += 1,
                SyntaxKind::CloseBracket => {
                    depth -= 1;
                    last = Some(token);
                }
                _ => {}
            }
        }
        last.filter(|_| depth == 0)
    }

    /// Return `true` when the source has the `]` that closes the outer `[`.
    #[inline]
    pub fn is_closed_in_source(&self) -> bool {
        self.close_bracket().is_some()
    }
}

impl<'t, 'a> HeaderedBlock<'t, 'a> {
    /// The header scalar (e.g. `rgb`, `hsv`, a tag). It is the first token
    /// of the node.
    #[inline]
    pub fn header(&self) -> ScalarToken<'t, 'a> {
        let node = self.0;
        ScalarToken(node.tree.token(node.data().first_token))
    }

    /// The block that follows the header. The parser opens a headered block
    /// only before a `{`, so the block is the first child node.
    #[inline]
    pub fn block(&self) -> Block<'t, 'a> {
        let block = self.0.node(self.0.idx + 1);
        debug_assert_eq!(block.kind(), SyntaxKind::Block);
        Block(block)
    }
}

impl<'t, 'a> Calc<'t, 'a> {
    /// The arithmetic expression between `@[` and `]`.
    ///
    /// Return `None` when the calculation is empty or when the parser kept
    /// tokens that the expression does not use, as in `@[1 2]` or
    /// `@[min(1, 2)]`. The closing `]` is not examined: the parser records a
    /// missing `]` as a [`SyntaxError`](super::SyntaxError).
    ///
    /// ```
    /// use jomini::text::lossless::parse;
    ///
    /// let parse = parse(b"a = @[1 + 2] b = @[1 2]");
    /// let calcs: Vec<_> = parse
    ///     .ast()
    ///     .fields()
    ///     .map(|f| f.value().unwrap().as_calc().unwrap().expr().is_some())
    ///     .collect();
    /// assert_eq!(calcs, [true, false]);
    /// ```
    #[inline]
    pub fn expr(&self) -> Option<Expr<'t, 'a>> {
        let mut parts = self.syntax().significant_children().skip(1);
        let expr = Expr::cast_element(parts.next()?)?;
        match parts.next() {
            None => Some(expr),
            Some(close) if is_close(close.kind()) => Some(expr),
            Some(_) => None,
        }
    }
}

/// Return `true` for a closing delimiter of a calculation or of a group.
#[inline]
fn is_close(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::CalcClose
            | SyntaxKind::MissingCalcClose
            | SyntaxKind::CloseParen
            | SyntaxKind::MissingCloseParen
    )
}

/// The operator of a [`BinaryExpr`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
}

impl BinOp {
    #[inline]
    const fn from_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::Plus => Some(Self::Add),
            SyntaxKind::Minus => Some(Self::Sub),
            SyntaxKind::Star => Some(Self::Mul),
            SyntaxKind::Slash => Some(Self::Div),
            _ => None,
        }
    }

    /// The text of the operator.
    pub const fn symbol(self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
        }
    }
}

/// The operator of a [`UnaryExpr`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `-`
    Neg,
    /// `+`
    Plus,
}

impl UnaryOp {
    /// The text of the operator.
    pub const fn symbol(self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Plus => "+",
        }
    }
}

impl<'t, 'a> BinaryExpr<'t, 'a> {
    /// The left operand. Return `None` when the parser put a token that is
    /// not an operand there, as the first `*` in `@[* * 2]`.
    #[inline]
    pub fn lhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .next()
            .and_then(Expr::cast_element)
    }

    /// The operator token: `+`, `-`, `*` or `/`.
    #[inline]
    pub fn op_token(&self) -> SyntaxToken<'t, 'a> {
        // The parser makes a binary expression only from its operator, so
        // the operator is always the second part.
        self.syntax()
            .significant_children()
            .nth(1)
            .and_then(|el| el.as_token())
            .expect("a binary expression has an operator")
    }

    /// The operator.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, BinOp, Expr};
    ///
    /// let parse = parse(b"cost = @[base * 2]");
    /// let calc = parse.ast().get(b"cost").unwrap().value().unwrap().as_calc().unwrap();
    /// let Some(Expr::Binary(product)) = calc.expr() else { panic!() };
    /// assert_eq!(product.op(), BinOp::Mul);
    /// ```
    #[inline]
    pub fn op(&self) -> BinOp {
        BinOp::from_kind(self.op_token().kind()).expect("the operator is arithmetic")
    }

    /// The right operand. Return `None` when it is missing, as in `@[1 +]`,
    /// or when the parser put a token that is not an operand there.
    #[inline]
    pub fn rhs(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .nth(2)
            .and_then(Expr::cast_element)
    }
}

impl<'t, 'a> UnaryExpr<'t, 'a> {
    /// The prefix operator token: `-` or `+`.
    #[inline]
    pub fn op_token(&self) -> SyntaxToken<'t, 'a> {
        // The parser makes a unary expression only from its operator, so the
        // operator is always the first part.
        self.syntax()
            .significant_children()
            .next()
            .and_then(|el| el.as_token())
            .expect("a unary expression has an operator")
    }

    /// The prefix operator.
    #[inline]
    pub fn op(&self) -> UnaryOp {
        match self.op_token().kind() {
            SyntaxKind::Minus => UnaryOp::Neg,
            SyntaxKind::Plus => UnaryOp::Plus,
            kind => unreachable!("{kind:?} is not a prefix operator"),
        }
    }

    /// The operand the prefix applies to. Return `None` when it is missing,
    /// as in `@[-]`, or when the parser put a token that is not an operand
    /// there.
    #[inline]
    pub fn operand(&self) -> Option<Expr<'t, 'a>> {
        self.syntax()
            .significant_children()
            .nth(1)
            .and_then(Expr::cast_element)
    }
}

impl<'t, 'a> ParenExpr<'t, 'a> {
    /// The expression between the parentheses. Return `None` when the group
    /// is empty, or when the parser put a token that is not an operand
    /// there. The closing `)` is not examined: the parser records a missing
    /// `)` as a [`SyntaxError`](super::SyntaxError).
    #[inline]
    pub fn inner(&self) -> Option<Expr<'t, 'a>> {
        let mut parts = self.syntax().significant_children().skip(1);
        let inner = Expr::cast_element(parts.next()?)?;
        match parts.next() {
            None => Some(inner),
            Some(close) if is_close(close.kind()) => Some(inner),
            Some(_) => None,
        }
    }
}

impl<'t, 'a> Item<'t, 'a> {
    /// The untyped element of this item.
    #[inline]
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Item::Field(field) => SyntaxElement::Node(field.syntax()),
            Item::Parameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Item::UndefinedParameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Item::Code(code) => SyntaxElement::Node(code.syntax()),
            Item::Interpolation(interpolation) => SyntaxElement::Node(interpolation.syntax()),
            Item::ReaderDirective(directive) => SyntaxElement::Node(directive.syntax()),
            Item::Value(value) => value.syntax(),
            Item::Other(el) => el,
        }
    }

    /// The byte range of this item in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// The [`Field`] if this item is one.
    #[inline]
    pub fn as_field(&self) -> Option<Field<'t, 'a>> {
        match *self {
            Item::Field(f) => Some(f),
            _ => None,
        }
    }

    /// The [`Parameter`] if this item is one.
    #[inline]
    pub fn as_parameter(&self) -> Option<Parameter<'t, 'a>> {
        match *self {
            Item::Parameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`UndefinedParameter`] if this item is one.
    #[inline]
    pub fn as_undefined_parameter(&self) -> Option<UndefinedParameter<'t, 'a>> {
        match *self {
            Item::UndefinedParameter(parameter) => Some(parameter),
            _ => None,
        }
    }

    /// The [`Code`] if this item is one.
    #[inline]
    pub fn as_code(&self) -> Option<Code<'t, 'a>> {
        match *self {
            Item::Code(code) => Some(code),
            _ => None,
        }
    }

    /// The [`Interpolation`] if this item is one.
    #[inline]
    pub fn as_interpolation(&self) -> Option<Interpolation<'t, 'a>> {
        match *self {
            Item::Interpolation(interpolation) => Some(interpolation),
            _ => None,
        }
    }

    /// The [`ReaderDirective`] if this item is one.
    #[inline]
    pub fn as_reader_directive(&self) -> Option<ReaderDirective<'t, 'a>> {
        match *self {
            Item::ReaderDirective(directive) => Some(directive),
            _ => None,
        }
    }

    /// The [`Value`] if this item is a bare value.
    #[inline]
    pub fn as_value(&self) -> Option<Value<'t, 'a>> {
        match *self {
            Item::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl<'t, 'a> Value<'t, 'a> {
    /// Classify the element after an operator as a value. An operator is a
    /// value there, as in `a = <=`.
    #[inline]
    fn after_operator(el: SyntaxElement<'t, 'a>) -> Option<Self> {
        match el {
            SyntaxElement::Token(token) if token.kind == SyntaxKind::Operator => {
                Some(Value::Operator(token))
            }
            el => Value::cast_element(el),
        }
    }

    /// Classify a child element as a value, or `None` if it cannot be one.
    #[inline]
    fn cast_element(el: SyntaxElement<'t, 'a>) -> Option<Self> {
        match el {
            SyntaxElement::Token(t) => ScalarToken::cast(t).map(Value::Scalar),
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
                SyntaxKind::ReaderDirective => ReaderDirective::cast(n).map(Value::ReaderDirective),
                _ => None,
            },
        }
    }

    /// The untyped element of this value.
    #[inline]
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Value::Scalar(token) => SyntaxElement::Token(token.syntax()),
            Value::Operator(token) => SyntaxElement::Token(token),
            Value::Block(block) => SyntaxElement::Node(block.syntax()),
            Value::HeaderedBlock(block) => SyntaxElement::Node(block.syntax()),
            Value::Calc(calc) => SyntaxElement::Node(calc.syntax()),
            Value::Parameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Value::UndefinedParameter(parameter) => SyntaxElement::Node(parameter.syntax()),
            Value::Code(code) => SyntaxElement::Node(code.syntax()),
            Value::Interpolation(interpolation) => SyntaxElement::Node(interpolation.syntax()),
            Value::ReaderDirective(directive) => SyntaxElement::Node(directive.syntax()),
        }
    }

    /// The byte range of this value in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// The scalar token, if this is [`Value::Scalar`].
    #[inline]
    pub fn as_scalar_token(&self) -> Option<ScalarToken<'t, 'a>> {
        match *self {
            Value::Scalar(token) => Some(token),
            _ => None,
        }
    }

    /// The scalar value of the token, if this is [`Value::Scalar`]. The
    /// quotes of a quoted string are removed (see [`ScalarToken::as_scalar`]).
    #[inline]
    pub fn as_scalar(&self) -> Option<Scalar<'a>> {
        match self {
            Value::Scalar(t) => Some(t.as_scalar()),
            _ => None,
        }
    }

    /// The block, if this is [`Value::Block`].
    #[inline]
    pub fn as_block(&self) -> Option<Block<'t, 'a>> {
        match self {
            Value::Block(b) => Some(*b),
            _ => None,
        }
    }

    /// The headered block, if this is [`Value::HeaderedBlock`].
    #[inline]
    pub fn as_headered_block(&self) -> Option<HeaderedBlock<'t, 'a>> {
        match self {
            Value::HeaderedBlock(h) => Some(*h),
            _ => None,
        }
    }

    /// The calc, if this is [`Value::Calc`].
    #[inline]
    pub fn as_calc(&self) -> Option<Calc<'t, 'a>> {
        match self {
            Value::Calc(c) => Some(*c),
            _ => None,
        }
    }

    /// The parameter, if this is [`Value::Parameter`].
    #[inline]
    pub fn as_parameter(&self) -> Option<Parameter<'t, 'a>> {
        match self {
            Value::Parameter(parameter) => Some(*parameter),
            _ => None,
        }
    }

    /// The undefined parameter, if this is [`Value::UndefinedParameter`].
    #[inline]
    pub fn as_undefined_parameter(&self) -> Option<UndefinedParameter<'t, 'a>> {
        match self {
            Value::UndefinedParameter(parameter) => Some(*parameter),
            _ => None,
        }
    }

    /// The code payload, if this is [`Value::Code`].
    #[inline]
    pub fn as_code(&self) -> Option<Code<'t, 'a>> {
        match self {
            Value::Code(code) => Some(*code),
            _ => None,
        }
    }

    /// The interpolation, if this is [`Value::Interpolation`].
    #[inline]
    pub fn as_interpolation(&self) -> Option<Interpolation<'t, 'a>> {
        match self {
            Value::Interpolation(interpolation) => Some(*interpolation),
            _ => None,
        }
    }

    /// Decode a scalar value into a string with `encoding` (see
    /// [`ScalarToken::decode`]).
    #[inline]
    pub fn decode<E: Encoding>(&self, encoding: E) -> Option<Cow<'a, str>> {
        match self {
            Value::Scalar(t) => Some(t.decode(encoding)),
            _ => None,
        }
    }

    /// Coerce a scalar value to `f64` (e.g. `1.000`, `-5.7`, `10.0f`).
    #[inline]
    pub fn to_f64(&self) -> Option<f64> {
        self.as_scalar()?.to_f64().ok()
    }

    /// Coerce a scalar value to `i64`.
    #[inline]
    pub fn to_i64(&self) -> Option<i64> {
        self.as_scalar()?.to_i64().ok()
    }

    /// Coerce a scalar value to `u64`.
    #[inline]
    pub fn to_u64(&self) -> Option<u64> {
        self.as_scalar()?.to_u64().ok()
    }

    /// Coerce a scalar value to `bool` (`yes`/`no`).
    #[inline]
    pub fn to_bool(&self) -> Option<bool> {
        self.as_scalar()?.to_bool().ok()
    }
}

impl<'t, 'a> Expr<'t, 'a> {
    /// The untyped element of this expression.
    #[inline]
    pub fn syntax(&self) -> SyntaxElement<'t, 'a> {
        match *self {
            Expr::Binary(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Unary(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Paren(expr) => SyntaxElement::Node(expr.syntax()),
            Expr::Number(token) | Expr::Ident(token) => SyntaxElement::Token(token),
        }
    }

    /// The byte range of this expression in the source.
    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// Classify a child element as a calc expression, or `None`.
    #[inline]
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
    /// The typed [`Root`] of the document. Start here to read fields and
    /// values. See [`Root`] for an overview of the typed view.
    #[inline]
    pub fn ast(&self) -> Root<'t, 'a> {
        Root::cast(self.root()).expect("the root node is always SyntaxKind::Root")
    }
}
