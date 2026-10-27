//! **Experimental** values of `@name` variables and `@[ ... ]` calculations.
//!
//! Some game files define variables and use them in other values:
//!
//! ```text
//! @size = 0.5
//! emblem = { scale = { @size @size } position = { @[1 - size] 0.5 } }
//! ```
//!
//! The parser keeps `@size` and `@[1 - size]` as they are in the source, so a
//! file can go back to its source text. [`resolve`] finds the value of each
//! variable and each calculation of a tree, and keeps the values in a
//! [`Resolution`]. A [`Deserializer`](super::de::Deserializer) with a
//! resolution then gives these values in place of the source text:
//!
//! ```
//! use jomini::{Utf8Encoding, text::lossless::{parse, de::Deserializer, resolve::resolve}};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize)]
//! struct Emblem {
//!     scale: [f32; 2],
//!     position: [f32; 2],
//! }
//!
//! #[derive(Deserialize)]
//! struct File {
//!     emblem: Emblem,
//! }
//!
//! let source = b"@size = 0.5\nemblem = { scale = { @size @size } position = { @[1 - size * 2] 0.5 } }";
//! let parse = parse(source);
//! let resolution = resolve(parse.tree(), &());
//! assert!(resolution.errors().is_empty());
//!
//! let file: File = Deserializer::new(parse.tree(), Utf8Encoding::new())
//!     .with_resolution(resolution)
//!     .deserialize()?;
//! assert_eq!(file.emblem.scale, [0.5, 0.5]);
//! assert_eq!(file.emblem.position, [0.0, 0.5]);
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! # Rules
//!
//! - A field with a variable key, such as `@size = 0.5`, defines the
//!   variable. The value is a scalar, a different variable, or a
//!   calculation. A definition applies to the parts of the file after it,
//!   and a new definition of the same name replaces it.
//! - A variable that the file does not define before it is used comes from
//!   the [`Scope`] that you give. Use `&()` for no scope, or [`Variables`]
//!   for the variables of a different file.
//! - A calculation has numbers, variable names (with or without `@`), `+`,
//!   `-`, `*`, `/`, and parentheses. `*` and `/` bind more tightly than `+`
//!   and `-`.
//! - The resolver does not examine a code payload (`code [[ ... ]]`), such as
//!   the body of an EU5 `template`, or a conditional parameter block
//!   (`[[name] ... ]`). Their contents apply only where a caller gives the
//!   parameters, so a `@name` in them keeps its source text.
//! - When the resolver cannot find a value, it records a [`ResolveError`]
//!   and keeps the source text of that variable or calculation.

use super::{AstNode, Calc, Field, SyntaxElement, SyntaxKind, SyntaxNode, SyntaxTree, TextRange};
use std::collections::{HashMap, HashSet};
use std::fmt;

/// The variables that a file does not define itself, such as the variables
/// of a different file.
///
/// `()` is the empty scope.
pub trait Scope {
    /// The value of the variable `name`. The name has no `@`.
    fn lookup(&self, name: &[u8]) -> Option<&[u8]>;
}

impl Scope for () {
    fn lookup(&self, _name: &[u8]) -> Option<&[u8]> {
        None
    }
}

/// A set of variables and their values. The names have no `@`.
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{Variables, resolve}};
///
/// let mut shared = Variables::new();
/// shared.insert("half", "0.5");
///
/// let parse = parse(b"@quarter = @[half / 2]\nx = @half");
/// let resolution = resolve(parse.tree(), &shared);
/// assert!(resolution.errors().is_empty());
/// assert_eq!(resolution.variables().get("quarter"), Some(&b"0.25"[..]));
/// assert_eq!(resolution.variables().get("half"), None);
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Variables {
    values: HashMap<Box<[u8]>, Box<[u8]>>,
}

impl Variables {
    /// An empty set.
    pub fn new() -> Self {
        Variables::default()
    }

    /// Set the value of the variable `name`.
    pub fn insert(&mut self, name: impl AsRef<[u8]>, value: impl AsRef<[u8]>) {
        self.values
            .insert(name.as_ref().into(), value.as_ref().into());
    }

    /// The value of the variable `name`.
    pub fn get(&self, name: impl AsRef<[u8]>) -> Option<&[u8]> {
        self.values.get(name.as_ref()).map(|value| &**value)
    }

    /// The number of variables.
    pub fn len(&self) -> usize {
        self.values.len()
    }

    /// Return `true` when the set has no variables.
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    /// The names and the values, in no specified order.
    pub fn iter(&self) -> impl Iterator<Item = (&[u8], &[u8])> {
        self.values.iter().map(|(name, value)| (&**name, &**value))
    }

    fn remove(&mut self, name: &[u8]) {
        self.values.remove(name);
    }
}

impl Scope for Variables {
    fn lookup(&self, name: &[u8]) -> Option<&[u8]> {
        self.get(name)
    }
}

/// The type of a [`ResolveError`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ResolveErrorKind {
    /// The file does not define the variable before it is used, and the
    /// scope does not have it.
    UndefinedVariable,
    /// A calculation uses a variable that is not a number.
    NotANumber,
    /// The result of a calculation is not a finite number, as in `@[1 / 0]`.
    NotFinite,
    /// The resolver does not know the form of the calculation, such as the
    /// function in `@[min(1, 2)]`, or the calculation is not complete, as in
    /// `@[1 +]`.
    UnsupportedCalculation,
    /// The value of a variable definition is a block.
    UnsupportedDefinition,
}

impl ResolveErrorKind {
    /// A description of the error.
    pub fn message(&self) -> &'static str {
        match self {
            Self::UndefinedVariable => "undefined variable",
            Self::NotANumber => "variable is not a number",
            Self::NotFinite => "result is not a finite number",
            Self::UnsupportedCalculation => "unsupported calculation",
            Self::UnsupportedDefinition => "variable value is not a scalar or a calculation",
        }
    }
}

impl fmt::Display for ResolveErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

/// A variable or a calculation that has no value.
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{ResolveErrorKind, resolve}};
///
/// let parse = parse(b"x = @[1 / 0]\ny = @size");
/// let resolution = resolve(parse.tree(), &());
/// let kinds: Vec<_> = resolution.errors().iter().map(|e| e.kind).collect();
/// assert_eq!(kinds, [ResolveErrorKind::NotFinite, ResolveErrorKind::UndefinedVariable]);
/// assert_eq!(resolution.errors()[1].to_string(), "undefined variable at 17..22");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct ResolveError {
    /// What is wrong.
    pub kind: ResolveErrorKind,
    /// The byte range of the variable or of the part of the calculation.
    pub range: TextRange,
}

impl ResolveError {
    fn new(kind: ResolveErrorKind, range: TextRange) -> Self {
        ResolveError { kind, range }
    }

    /// A description of the error.
    pub fn message(&self) -> &'static str {
        self.kind.message()
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message(), self.range)
    }
}

impl std::error::Error for ResolveError {}

/// The values of the variables and the calculations of a tree. Make one
/// with [`resolve`].
#[derive(Debug, Clone, Default)]
pub struct Resolution {
    /// The value of each variable token, by token index.
    pub(crate) tokens: HashMap<u32, Box<[u8]>>,
    /// The value of each calculation node, by node index.
    pub(crate) nodes: HashMap<u32, Box<[u8]>>,
    variables: Variables,
    errors: Vec<ResolveError>,
    /// The size of the tree, to find a resolution of a different tree.
    #[cfg_attr(not(feature = "serde"), allow(dead_code))]
    pub(crate) shape: (usize, usize),
}

impl Resolution {
    /// The variables and calculations that have no value.
    pub fn errors(&self) -> &[ResolveError] {
        &self.errors
    }

    /// The variables that the file defines, with the values at the end of
    /// the file.
    pub fn variables(&self) -> &Variables {
        &self.variables
    }

    /// The value of a variable or of a calculation of the tree, as text.
    /// The value of a calculation is a decimal number.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, resolve::resolve};
    ///
    /// let parse = parse(b"@size = 2\nx = @size\ny = @[size * 3]");
    /// let resolution = resolve(parse.tree(), &());
    /// let values: Vec<_> = parse
    ///     .ast()
    ///     .fields()
    ///     .filter_map(|field| resolution.get(field.value()?))
    ///     .collect();
    /// assert_eq!(values, [b"2", b"6"]);
    /// ```
    pub fn get(&self, value: super::Value<'_, '_>) -> Option<&[u8]> {
        let value = match value {
            super::Value::Scalar(token) => self.tokens.get(&token.idx),
            super::Value::Calc(calc) => self.nodes.get(&calc.syntax().idx),
            _ => None,
        };
        value.map(|value| &**value)
    }
}

/// Find the value of each variable and each calculation of `tree`.
///
/// A variable that the tree does not define comes from `scope`. Use `&()`
/// for no scope.
pub fn resolve(tree: SyntaxTree<'_, '_>, scope: &dyn Scope) -> Resolution {
    let tokens = &tree.data.tokens;
    let nodes = &tree.data.nodes;
    let mut resolver = Resolver {
        tree,
        scope,
        failed: HashSet::new(),
        out: Resolution {
            shape: (tokens.len(), nodes.len()),
            ..Resolution::default()
        },
    };

    // Walk the tokens in source order. A node that starts at a token comes
    // before the token. A definition and a calculation take their subtree.
    // A code payload, such as the body of an EU5 `template`, and a
    // conditional parameter block are not a part of the file scope: they
    // apply only where a caller gives the parameters.
    let mut token = 0;
    let mut node = 0;
    'tokens: while token < tokens.len() as u32 {
        while let Some(data) = nodes.get(node as usize)
            && data.first_token <= token
        {
            let key = tokens.get(data.first_token as usize).map(|t| t.kind);
            match data.kind {
                SyntaxKind::Field if key == Some(SyntaxKind::Variable) => resolver.define(node),
                SyntaxKind::Calc => resolver.calc(node),
                SyntaxKind::Code | SyntaxKind::Parameter | SyntaxKind::UndefinedParameter => {}
                _ => {
                    node += 1;
                    continue;
                }
            }
            token = data.token_end.max(token + 1);
            node = data.subtree_end;
            continue 'tokens;
        }
        if tokens[token as usize].kind == SyntaxKind::Variable {
            resolver.reference(token);
        }
        token += 1;
    }
    resolver.out
}

/// Find the value of a calculation. A variable comes from `scope`.
///
/// ```
/// use jomini::text::lossless::{Calc, AstNode, parse, resolve::{Variables, evaluate}};
///
/// let parse = parse(b"x = @[(1 + width) * 2]");
/// let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
/// let mut scope = Variables::new();
/// scope.insert("width", "0.5");
/// assert_eq!(evaluate(calc, &scope), Ok(3.0));
/// ```
pub fn evaluate(calc: Calc<'_, '_>, scope: &dyn Scope) -> Result<f64, ResolveError> {
    let lookup = |name: &[u8]| match scope.lookup(name) {
        Some(value) => Lookup::Found(value),
        None => Lookup::Missing,
    };
    // Only a failed definition of the file gives `None`, and this scope has
    // no definitions.
    eval_calc(calc.syntax(), &lookup).map_err(|e| e.expect("an error has a cause"))
}

enum Lookup<'s> {
    Found(&'s [u8]),
    /// The definition of the variable has an error, which the resolver
    /// records one time.
    Failed,
    Missing,
}

/// An error of a calculation, or `None` when the cause is a definition that
/// has an error.
type EvalResult = Result<f64, Option<ResolveError>>;

struct Resolver<'t, 'a, 's> {
    tree: SyntaxTree<'t, 'a>,
    scope: &'s dyn Scope,
    /// The variables whose last definition has an error.
    failed: HashSet<Box<[u8]>>,
    out: Resolution,
}

impl Resolver<'_, '_, '_> {
    fn lookup(&self, name: &[u8]) -> Lookup<'_> {
        if let Some(value) = self.out.variables.get(name) {
            Lookup::Found(value)
        } else if self.failed.contains(name) {
            Lookup::Failed
        } else if let Some(value) = self.scope.lookup(name) {
            Lookup::Found(value)
        } else {
            Lookup::Missing
        }
    }

    /// The value of a variable token, or `None` when it has no value.
    fn variable(&mut self, token: u32) -> Option<Box<[u8]>> {
        let token = self.tree.token(token);
        let name = &token.text()[1..];
        match self.lookup(name) {
            Lookup::Found(value) => Some(value.into()),
            Lookup::Failed => None,
            Lookup::Missing => {
                let error = ResolveError::new(ResolveErrorKind::UndefinedVariable, token.range);
                self.out.errors.push(error);
                None
            }
        }
    }

    fn reference(&mut self, token: u32) {
        if let Some(value) = self.variable(token) {
            self.out.tokens.insert(token, value);
        }
    }

    fn calc(&mut self, node: u32) {
        let node = SyntaxNode {
            tree: self.tree,
            idx: node,
        };
        let value = eval_calc(node, &|name| self.lookup(name));
        match value {
            Ok(value) => {
                self.out.nodes.insert(node.idx, format_number(value));
            }
            Err(Some(error)) => self.out.errors.push(error),
            Err(None) => {}
        }
    }

    fn define(&mut self, node: u32) {
        let node = SyntaxNode {
            tree: self.tree,
            idx: node,
        };
        let Some(field) = Field::cast(node) else {
            return;
        };
        let name: Box<[u8]> = field.key().text()[1..].into();
        let value = match field.value() {
            Some(super::Value::Scalar(token)) => match token.kind() {
                SyntaxKind::Variable => self.variable(token.idx),
                SyntaxKind::Quoted => Some(strip_quotes(token.text()).into()),
                _ => Some(token.text().into()),
            },
            Some(super::Value::Calc(calc)) => {
                let value = eval_calc(calc.syntax(), &|name| self.lookup(name));
                match value {
                    Ok(value) => Some(format_number(value)),
                    Err(error) => {
                        self.out.errors.extend(error);
                        None
                    }
                }
            }
            Some(value) => {
                let range = value.syntax().text_range();
                let error = ResolveError::new(ResolveErrorKind::UnsupportedDefinition, range);
                self.out.errors.push(error);
                None
            }
            // The parser gives a syntax error for a field with no value.
            None => None,
        };
        match value {
            Some(value) => {
                self.failed.remove(&name);
                self.out.variables.insert(name, value);
            }
            None => {
                self.out.variables.remove(&name);
                self.failed.insert(name);
            }
        }
    }
}

fn strip_quotes(text: &[u8]) -> &[u8] {
    let text = text.strip_prefix(b"\"").unwrap_or(text);
    text.strip_suffix(b"\"").unwrap_or(text)
}

/// The shortest decimal text that gives `value` again. It has no exponent,
/// so each game can read it.
fn format_number(value: f64) -> Box<[u8]> {
    // Add zero to change `-0` into `0`.
    (value + 0.0).to_string().into_bytes().into_boxed_slice()
}

fn unsupported(range: TextRange) -> Option<ResolveError> {
    Some(ResolveError::new(
        ResolveErrorKind::UnsupportedCalculation,
        range,
    ))
}

fn finite(value: f64, range: TextRange) -> EvalResult {
    if value.is_finite() {
        Ok(value)
    } else {
        Err(Some(ResolveError::new(ResolveErrorKind::NotFinite, range)))
    }
}

/// Evaluate a `Calc` node. The parser accepts tokens that are not a part of
/// the expression, as in `@[1 2]`, so each node must have the expected
/// parts and no others.
fn eval_calc<'s>(node: SyntaxNode<'_, '_>, lookup: &dyn Fn(&[u8]) -> Lookup<'s>) -> EvalResult {
    let mut parts = node.significant_children();
    let (Some(open), Some(expr), close, None) =
        (parts.next(), parts.next(), parts.next(), parts.next())
    else {
        return Err(unsupported(node.text_range()));
    };
    let closed = match close {
        None => true,
        Some(close) => matches!(
            close.kind(),
            SyntaxKind::CalcClose | SyntaxKind::MissingCalcClose
        ),
    };
    if open.kind() != SyntaxKind::CalcOpen || !closed {
        return Err(unsupported(node.text_range()));
    }
    let value = eval(expr, lookup)?;
    finite(value, node.text_range())
}

fn eval<'s>(element: SyntaxElement<'_, '_>, lookup: &dyn Fn(&[u8]) -> Lookup<'s>) -> EvalResult {
    let range = element.text_range();
    let node = match element {
        SyntaxElement::Token(token) => {
            let text = token.text();
            return match token.kind() {
                SyntaxKind::Number => parse_number(text).ok_or_else(|| unsupported(range)),
                SyntaxKind::CalcIdent => {
                    let name = text.strip_prefix(b"@").unwrap_or(text);
                    match lookup(name) {
                        Lookup::Found(value) => parse_number(value).ok_or_else(|| {
                            Some(ResolveError::new(ResolveErrorKind::NotANumber, range))
                        }),
                        Lookup::Failed => Err(None),
                        Lookup::Missing => Err(Some(ResolveError::new(
                            ResolveErrorKind::UndefinedVariable,
                            range,
                        ))),
                    }
                }
                _ => Err(unsupported(range)),
            };
        }
        SyntaxElement::Node(node) => node,
    };

    let mut parts = node.significant_children();
    let parts = [parts.next(), parts.next(), parts.next(), parts.next()];
    let kinds = parts.map(|part| part.map(|p| p.kind()));
    match (node.kind(), parts, kinds) {
        (SyntaxKind::BinaryExpr, [Some(lhs), _, Some(rhs), None], [_, Some(op), _, _]) => {
            let (lhs, rhs) = (eval(lhs, lookup)?, eval(rhs, lookup)?);
            let value = match op {
                SyntaxKind::Plus => lhs + rhs,
                SyntaxKind::Minus => lhs - rhs,
                SyntaxKind::Star => lhs * rhs,
                SyntaxKind::Slash => lhs / rhs,
                _ => return Err(unsupported(range)),
            };
            finite(value, range)
        }
        (SyntaxKind::UnaryExpr, [_, Some(operand), None, None], [Some(op), ..]) => {
            let value = eval(operand, lookup)?;
            match op {
                SyntaxKind::Minus => Ok(-value),
                SyntaxKind::Plus => Ok(value),
                _ => Err(unsupported(range)),
            }
        }
        (
            SyntaxKind::ParenExpr,
            [_, Some(inner), _, None],
            [
                Some(SyntaxKind::OpenParen),
                _,
                Some(SyntaxKind::CloseParen | SyntaxKind::MissingCloseParen),
                _,
            ],
        ) => eval(inner, lookup),
        _ => Err(unsupported(range)),
    }
}

fn parse_number(text: &[u8]) -> Option<f64> {
    std::str::from_utf8(text).ok()?.parse().ok()
}
