//! **Experimental.** Find the values of `@name` variables and `@[ ... ]`
//! calculations.
//!
//! Some game files define variables and use them in other places:
//!
//! ```text
//! @size = 0.5
//! emblem = { scale = { @size @size } position = { @[1 - size] 0.5 } }
//! ```
//!
//! The parser keeps the source text, so that a tool can write the file back
//! unchanged. As a result, code that reads the tree sees `@size`, not `0.5`.
//! [`resolve`] finds the value of each variable and each calculation, and
//! returns the values in a [`Resolution`]. The tree does not change: you give
//! the resolution to the code that reads the tree.
//!
//! # Deserialize with the values
//!
//! Pass the resolution to
//! [`Document::with_resolution`](super::de::Document::with_resolution). The
//! document then reads each variable and each calculation as its value. The
//! document skips a definition such as `@size = 0.5`, so your type needs no
//! field for it, even with `#[serde(deny_unknown_fields)]`:
//!
//! ```
//! use jomini::{Utf8Encoding, text::lossless::{parse, de::Document, resolve::resolve}};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize)]
//! struct Emblem {
//!     scale: [f32; 2],
//!     position: [f32; 2],
//! }
//!
//! #[derive(Deserialize)]
//! #[serde(deny_unknown_fields)]
//! struct File {
//!     emblem: Emblem,
//! }
//!
//! let source = b"@size = 0.5\nemblem = { scale = { @size @size } position = { @[1 - size] 0.5 } }";
//! let parse = parse(source);
//! let resolution = resolve(parse.tree());
//! assert_eq!(resolution.errors(), []);
//!
//! let file: File = Document::new(parse.tree(), Utf8Encoding::new())
//!     .with_resolution(resolution)?
//!     .deserialize()?;
//! assert_eq!(file.emblem.scale, [0.5, 0.5]);
//! assert_eq!(file.emblem.position, [0.5, 0.5]);
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```
//!
//! # Read one value without serde
//!
//! Find the [`Value`] node of a variable or a calculation in the tree, and
//! pass the node to [`Resolution::get`]. It returns a [`ResolvedScalar`]: the
//! text of the value, and whether the definition was quoted. Here,
//! `ruler = @name` gives the quoted text `Jean II`, because the definition of
//! `@name` has quotes. The calculation gives an unquoted number:
//!
//! ```
//! use jomini::text::lossless::{parse, resolve::{ResolvedScalar, resolve}};
//!
//! let parse = parse(b"@name = \"Jean II\"\n@size = 2\nruler = @name\nwidth = @[size * 3]");
//! let resolution = resolve(parse.tree());
//! let resolved = |key: &[u8]| resolution.get(parse.ast().get(key)?.value()?);
//! assert_eq!(resolved(b"ruler"), Some(&ResolvedScalar::quoted("Jean II")));
//! assert_eq!(resolved(b"width"), Some(&ResolvedScalar::unquoted("6")));
//! ```
//!
//! # Use variables from other files
//!
//! A file can use a variable that a different file defines. Give the
//! variables of the other files to [`resolve_with`] as one [`Variables`]
//! set. This set is the *scope* of the file. To resolve a file with a scope:
//!
//! 1. Resolve each shared file with [`resolve`].
//! 2. Get the variables of each shared file with [`Resolution::variables`].
//! 3. Combine them into one set with [`Variables::extend_from`].
//! 4. Resolve the file that uses them with [`resolve_with`] and the scope.
//!
//! ```
//! use jomini::text::lossless::{parse, resolve::{ResolvedScalar, Variables, resolve, resolve_with}};
//!
//! let colors = parse(b"@red = 4");
//! let sizes = parse(b"@half = 2\n@width = @[half * red]");
//!
//! let mut scope = Variables::new();
//! scope.extend_from(resolve(colors.tree()).variables());
//! let resolution = resolve_with(sizes.tree(), &scope);
//! assert_eq!(resolution.errors(), []);
//! assert_eq!(resolution.variables().get("width"), Some(&ResolvedScalar::unquoted("8")));
//! ```
//!
//! # Resolve a file again after an edit
//!
//! An editor resolves a file again after each change. The scope for the file
//! must not hold the variables of the file itself. When a combined scope
//! still holds the variables of the file from before the edit, each
//! definition in the file is a [`ResolveErrorKind::DefinedInScope`] error,
//! and each reference gets the value from before the edit.
//!
//! To prevent this, keep one [`Variables`] set for each file. Make the scope
//! for a file from the sets of the other files only:
//!
//! ```
//! use jomini::text::lossless::{parse, Parse, resolve::{
//!     Resolution, ResolveErrorKind, ResolvedScalar, Variables, resolve, resolve_with,
//! }};
//!
//! let width = |parse: &Parse, resolution: &Resolution| {
//!     resolution.get(parse.ast().get(b"width")?.value()?).cloned()
//! };
//!
//! // One set of variables for each file.
//! let colors = parse(b"@red = 4");
//! let colors_variables = resolve(colors.tree()).variables().clone();
//! let sizes = parse(b"@half = 2\nwidth = @[half * red]");
//! let mut scope = Variables::new();
//! scope.extend_from(&colors_variables);
//! let sizes_variables = resolve_with(sizes.tree(), &scope).variables().clone();
//!
//! // The user changes `@half` to 3. The scope has only the variables of the
//! // colors file.
//! let edited = parse(b"@half = 3\nwidth = @[half * red]");
//! let resolution = resolve_with(edited.tree(), &scope);
//! assert_eq!(resolution.errors(), []);
//! assert_eq!(width(&edited, &resolution), Some(ResolvedScalar::unquoted("12")));
//!
//! // Wrong: a scope that also holds the variables of the sizes file from
//! // before the edit.
//! let mut stale_scope = scope.clone();
//! stale_scope.extend_from(&sizes_variables);
//! let stale = resolve_with(edited.tree(), &stale_scope);
//! assert_eq!(stale.errors()[0].kind, ResolveErrorKind::DefinedInScope);
//! assert_eq!(width(&edited, &stale), Some(ResolvedScalar::unquoted("8")));
//! ```
//!
//! A clone of a [`Variables`] set copies the names but shares the values, so
//! a set for each file uses little memory. Alternatively, keep one combined
//! scope, and take the variables of a file out of it with
//! [`Variables::remove`] before you resolve the file again. Read the caveat
//! on `remove` first.
//!
//! # Find variables that have no value
//!
//! When the resolver cannot find the value of a variable or a calculation, it
//! records a [`ResolveError`] in [`Resolution::errors`]. The
//! [`range`](ResolveError::range) of the error is a byte range of the
//! source. [`LineIndex`](super::LineIndex) converts its start to a line and a
//! column.
//!
//! A [`Document`](super::de::Document) reads a variable or a calculation
//! that has no value as its source text. A number field then fails with an
//! error such as `invalid type: string "@size", expected f32`, which does not
//! tell the cause. Report the errors of the resolution before you
//! deserialize:
//!
//! ```
//! use jomini::text::lossless::{LineIndex, parse, resolve::resolve};
//!
//! // The field uses `@size` before its definition.
//! let source = b"scale = @size\n@size = 0.5";
//! let parse = parse(source);
//! let resolution = resolve(parse.tree());
//!
//! let lines = LineIndex::new(source);
//! let messages: Vec<String> = resolution
//!     .errors()
//!     .iter()
//!     .map(|error| format!("{}: {}", lines.line_col(error.range.start()), error.message()))
//!     .collect();
//! assert_eq!(messages, ["1:9: undefined variable"]);
//! ```
//!
//! The rules that the resolver follows are in the
//! [`resolve_with`](resolve_with#rules) documentation.

use super::{
    AstNode, BinOp, Calc, DirectiveKind, Expr, Field, NodeId, ReaderDirective, ScalarToken,
    SyntaxElement, SyntaxKind, SyntaxNode, SyntaxTree, TextRange, TokenId, UnaryOp, Value,
};
use crate::{Encoding, Scalar};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::Arc;

/// The value of a variable or a calculation: its text, and whether the text
/// was quoted.
///
/// The value of a variable is quoted when the right side of its definition
/// is quoted. After `@name = "a b"`, a document reads `@name` as the quoted
/// text `a b`, as if the source had `"a b"` in its place. A number and other
/// unquoted text give an unquoted value.
///
/// A calculation gives an unquoted number. Its text is the shortest decimal
/// text that gives the same `f64`, with no exponent. For example,
/// `@[0.1 + 0.2]` is `0.30000000000000004`, and a target of type `f32`
/// reads it as `0.3`.
///
/// The text is shared. A clone of a `ResolvedScalar` does not copy the
/// text, and a [`Resolution`] and a [`Variables`] set hold one copy of each
/// value.
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{ResolvedScalar, resolve}};
///
/// let parse = parse(b"@name = \"a b\"\n@size = 2\n@sum = @[0.1 + 0.2]");
/// let resolution = resolve(parse.tree());
/// let variables = resolution.variables();
/// assert_eq!(variables.get("name"), Some(&ResolvedScalar::quoted("a b")));
/// assert_eq!(variables.get("size"), Some(&ResolvedScalar::unquoted("2")));
/// assert_eq!(variables.get("sum"), Some(&ResolvedScalar::unquoted("0.30000000000000004")));
/// ```
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct ResolvedScalar {
    text: Arc<[u8]>,
    quoted: bool,
}

impl ResolvedScalar {
    /// An unquoted value, such as a number or a name.
    pub fn unquoted(text: impl AsRef<[u8]>) -> Self {
        ResolvedScalar {
            text: text.as_ref().into(),
            quoted: false,
        }
    }

    /// A quoted value. `text` is the text between the quotes, with its
    /// escape characters, as in the source.
    pub fn quoted(text: impl AsRef<[u8]>) -> Self {
        ResolvedScalar {
            text: text.as_ref().into(),
            quoted: true,
        }
    }

    /// The value of a scalar token of the source.
    fn of_token(token: ScalarToken<'_, '_>) -> Self {
        ResolvedScalar {
            text: token.as_bytes().into(),
            quoted: token.is_quoted(),
        }
    }

    /// Return `true` for a quoted value.
    pub fn is_quoted(&self) -> bool {
        self.quoted
    }

    /// The bytes of the value without the quotes. The escape characters of
    /// a quoted value stay, as with
    /// [`ScalarToken::as_bytes`](super::ScalarToken::as_bytes).
    pub fn as_bytes(&self) -> &[u8] {
        &self.text
    }

    /// The value as a [`Scalar`]. Use the `Scalar` to convert the value to a
    /// number or a boolean.
    pub fn as_scalar(&self) -> Scalar<'_> {
        Scalar::new(&self.text)
    }

    /// Decode the value into a string with `encoding`. The encoding removes
    /// the escape characters of a quoted value.
    pub fn decode<E: Encoding>(&self, encoding: E) -> Cow<'_, str> {
        encoding.decode(&self.text)
    }
}

impl fmt::Debug for ResolvedScalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = if self.quoted { "Quoted" } else { "Unquoted" };
        f.debug_tuple(name)
            .field(&String::from_utf8_lossy(&self.text))
            .finish()
    }
}

/// An unquoted value.
impl From<&str> for ResolvedScalar {
    fn from(text: &str) -> Self {
        ResolvedScalar::unquoted(text)
    }
}

/// An unquoted value.
impl From<String> for ResolvedScalar {
    fn from(text: String) -> Self {
        ResolvedScalar::unquoted(text)
    }
}

/// An unquoted value.
impl From<&[u8]> for ResolvedScalar {
    fn from(text: &[u8]) -> Self {
        ResolvedScalar::unquoted(text)
    }
}

/// An unquoted value.
impl<const N: usize> From<&[u8; N]> for ResolvedScalar {
    fn from(text: &[u8; N]) -> Self {
        ResolvedScalar::unquoted(text)
    }
}

/// An unquoted value.
impl From<Vec<u8>> for ResolvedScalar {
    fn from(text: Vec<u8>) -> Self {
        ResolvedScalar::unquoted(text)
    }
}

/// A set of variables and their values. The names have no `@`.
///
/// The names and the values are bytes of the source. A set does not know
/// their encoding, so the files that define the variables and the files that
/// use them must use the same encoding. When the encodings differ, a value
/// from a Windows-1252 file decodes to wrong text in a UTF-8 document, and
/// no error occurs.
///
/// A set is the scope that [`resolve_with`] reads, and
/// [`Resolution::variables`] gives the variables that one file defines. That
/// set does not include the variables of the scope:
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{ResolvedScalar, Variables, resolve_with}};
///
/// let mut scope = Variables::new();
/// scope.insert("half", "0.5");
///
/// let parse = parse(b"@quarter = @[half / 2]\nx = @half");
/// let resolution = resolve_with(parse.tree(), &scope);
/// assert_eq!(resolution.errors(), []);
/// assert_eq!(resolution.variables().get("quarter"), Some(&ResolvedScalar::unquoted("0.25")));
/// assert_eq!(resolution.variables().get("half"), None);
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Variables {
    /// Each value is shared with each reference of a [`Resolution`] that
    /// gives it, so a long value is stored one time.
    values: HashMap<Box<[u8]>, ResolvedScalar>,
}

impl Variables {
    /// An empty set.
    pub fn new() -> Self {
        Variables::default()
    }

    /// Add the variable `name` with `value`, and return its previous value,
    /// if any.
    ///
    /// A `&str`, a `String`, or bytes, such as `"0.5"`, give an unquoted
    /// value. For a quoted value, pass a [`ResolvedScalar::quoted`].
    ///
    /// ```
    /// use jomini::text::lossless::resolve::{ResolvedScalar, Variables};
    ///
    /// let mut scope = Variables::new();
    /// assert_eq!(scope.insert("half", "0.5"), None);
    /// assert_eq!(scope.insert("half", "0.4"), Some(ResolvedScalar::unquoted("0.5")));
    /// scope.insert("name", ResolvedScalar::quoted("Jean II"));
    /// assert_eq!(scope.get("name"), Some(&ResolvedScalar::quoted("Jean II")));
    /// ```
    pub fn insert(
        &mut self,
        name: impl AsRef<[u8]>,
        value: impl Into<ResolvedScalar>,
    ) -> Option<ResolvedScalar> {
        self.values.insert(name.as_ref().into(), value.into())
    }

    /// The value of the variable `name`.
    pub fn get(&self, name: impl AsRef<[u8]>) -> Option<&ResolvedScalar> {
        self.values.get(name.as_ref())
    }

    /// Return `true` when the set has the variable `name`.
    pub fn contains(&self, name: impl AsRef<[u8]>) -> bool {
        self.values.contains_key(name.as_ref())
    }

    /// Remove the variable `name`, and return its value, if any.
    ///
    /// A set has one value for each name. When two files define the same
    /// name and you combine their variables in one set, the removal of the
    /// variables of one file also removes the definition of the other file.
    ///
    /// ```
    /// use jomini::text::lossless::resolve::{ResolvedScalar, Variables};
    ///
    /// let mut scope: Variables = [("red", "4"), ("half", "2")].into_iter().collect();
    /// assert_eq!(scope.remove("half"), Some(ResolvedScalar::unquoted("2")));
    /// assert_eq!(scope.remove("half"), None);
    /// assert_eq!(scope.len(), 1);
    /// ```
    pub fn remove(&mut self, name: impl AsRef<[u8]>) -> Option<ResolvedScalar> {
        self.values.remove(name.as_ref())
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
    pub fn iter(&self) -> Iter<'_> {
        Iter(self.values.iter())
    }

    /// Add each variable of `other` to this set, for example to combine the
    /// variables of several files. When both sets have a name, the value of
    /// `other` replaces the value of this set. The sets share the values, so
    /// the function copies only the names.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, resolve::{ResolvedScalar, Variables, resolve, resolve_with}};
    ///
    /// let first = parse(b"@half = 0.5");
    /// let second = parse(b"@quarter = @[half / 2]");
    /// let mut scope = Variables::new();
    /// scope.extend_from(resolve(first.tree()).variables());
    /// scope.extend_from(resolve_with(second.tree(), &scope).variables());
    /// assert_eq!(scope.get("half"), Some(&ResolvedScalar::unquoted("0.5")));
    /// assert_eq!(scope.get("quarter"), Some(&ResolvedScalar::unquoted("0.25")));
    /// ```
    pub fn extend_from(&mut self, other: &Variables) {
        for (name, value) in &other.values {
            self.values.insert(name.clone(), value.clone());
        }
    }
}

/// Add variables from names and values. When the set already has a name,
/// the new value replaces the old value. To add the variables of a different
/// set, use [`Variables::extend_from`], which copies only the names.
impl<K: AsRef<[u8]>, V: Into<ResolvedScalar>> Extend<(K, V)> for Variables {
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for (name, value) in iter {
            self.insert(name, value);
        }
    }
}

impl<K: AsRef<[u8]>, V: Into<ResolvedScalar>> FromIterator<(K, V)> for Variables {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut variables = Variables::new();
        variables.extend(iter);
        variables
    }
}

impl<'v> IntoIterator for &'v Variables {
    type Item = (&'v [u8], &'v ResolvedScalar);
    type IntoIter = Iter<'v>;

    fn into_iter(self) -> Iter<'v> {
        self.iter()
    }
}

/// The names and the values of a set of [`Variables`], from
/// [`Variables::iter`].
#[derive(Debug, Clone)]
pub struct Iter<'v>(std::collections::hash_map::Iter<'v, Box<[u8]>, ResolvedScalar>);

impl<'v> Iterator for Iter<'v> {
    type Item = (&'v [u8], &'v ResolvedScalar);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(name, value)| (&**name, value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl ExactSizeIterator for Iter<'_> {}

impl std::iter::FusedIterator for Iter<'_> {}

/// The cause of a [`ResolveError`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ResolveErrorKind {
    /// The file does not define the variable before its use, and the scope
    /// of [`resolve_with`] does not have it.
    UndefinedVariable,
    /// A calculation uses a variable whose value is not a number, as in
    /// `@[x + 1]` after `@x = text`.
    NotNumeric,
    /// The result of a calculation is not a finite number, as in `@[1 / 0]`.
    NotFinite,
    /// The resolver does not know the form of the calculation, such as the
    /// function in `@[min(1, 2)]`, or the calculation is not complete, as in
    /// `@[1 +]`.
    UnsupportedCalculation,
    /// The right side of a variable definition is not a scalar, a variable,
    /// or a calculation. For example, it is a block, as in `@x = { 1 2 }`.
    UnsupportedDefinition,
    /// The file defines the variable a second time. The variable keeps the
    /// value of the first definition.
    Redefined,
    /// The file defines a variable that the scope of [`resolve_with`]
    /// already has, such as a CK3 variable of `reader_export`. The variable
    /// keeps the value of the scope.
    ///
    /// This error often means that the scope holds the variables of the file
    /// itself. See [Resolve a file again after an
    /// edit](crate::text::lossless::resolve#resolve-a-file-again-after-an-edit).
    DefinedInScope,
}

impl ResolveErrorKind {
    /// A description of the error.
    pub fn message(self) -> &'static str {
        match self {
            Self::UndefinedVariable => "undefined variable",
            Self::NotNumeric => "variable is not a number",
            Self::NotFinite => "result is not a finite number",
            Self::UnsupportedCalculation => "unsupported calculation",
            Self::UnsupportedDefinition => "variable value is not a scalar or a calculation",
            Self::Redefined => "variable is already defined",
            Self::DefinedInScope => "variable is already defined in the scope",
        }
    }
}

impl fmt::Display for ResolveErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

/// The error for a variable or a calculation that has no value.
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{ResolveErrorKind, resolve}};
///
/// let parse = parse(b"x = @[1 / 0]\ny = @size");
/// let resolution = resolve(parse.tree());
/// let kinds: Vec<_> = resolution.errors().iter().map(|e| e.kind).collect();
/// assert_eq!(kinds, [ResolveErrorKind::NotFinite, ResolveErrorKind::UndefinedVariable]);
/// assert_eq!(resolution.errors()[1].to_string(), "undefined variable at 17..22");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct ResolveError {
    /// What is wrong.
    pub kind: ResolveErrorKind,
    /// The byte range of the variable, or of the part of the calculation that
    /// has the error. For example, the range of `@[1 / 0]` is the range of
    /// `1 / 0`, and the range of an undefined variable in a calculation is
    /// the range of its name.
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
/// with [`resolve`] or [`resolve_with`].
///
/// A resolution owns its data, so a cache can keep it with an owned parse.
///
/// A resolution is for one parse and its clones. See
/// [`is_for`](Resolution::is_for).
#[derive(Debug, Clone)]
pub struct Resolution {
    /// The value of each variable token, sorted by token.
    tokens: Vec<(TokenId, ResolvedScalar)>,
    /// The value of each calculation and each `@:load_variable` node,
    /// sorted by node.
    nodes: Vec<(NodeId, ResolvedScalar)>,
    variables: Variables,
    errors: Vec<ResolveError>,
    /// The identity of the parse of the tree.
    parse_id: u64,
}

impl Resolution {
    /// An error for each variable and each calculation that has no value.
    pub fn errors(&self) -> &[ResolveError] {
        &self.errors
    }

    /// The variables that the file defines, each with the value it has after
    /// the whole file is read. This set does not include the variables of the
    /// scope.
    pub fn variables(&self) -> &Variables {
        &self.variables
    }

    /// The value of a variable token.
    pub(crate) fn token(&self, id: TokenId) -> Option<&ResolvedScalar> {
        let index = self.tokens.binary_search_by_key(&id, |(id, _)| *id).ok()?;
        Some(&self.tokens[index].1)
    }

    /// The value of a calculation or a `@:load_variable` node.
    pub(crate) fn node(&self, id: NodeId) -> Option<&ResolvedScalar> {
        let index = self.nodes.binary_search_by_key(&id, |(id, _)| *id).ok()?;
        Some(&self.nodes[index].1)
    }

    /// Return `true` when this is the resolution of `tree`: the tree is of
    /// the parse that [`resolve`] read, or of a clone of that parse. A
    /// second parse of the same source is a different parse. The check
    /// compares one number.
    ///
    /// ```
    /// use jomini::text::lossless::{parse, resolve::resolve};
    ///
    /// let first = parse(b"@x = 1 a = @x");
    /// let edited = parse(b"@x = 2 a = @x");
    /// let resolution = resolve(first.tree());
    /// assert!(resolution.is_for(first.tree()));
    /// assert!(resolution.is_for(first.clone().tree()));
    /// assert!(!resolution.is_for(edited.tree()));
    /// ```
    pub fn is_for(&self, tree: SyntaxTree<'_, '_>) -> bool {
        self.parse_id == tree.data.parse_id
    }

    /// The value of a variable, a calculation, or a `@:load_variable`
    /// directive in the [`Value`] node `value`.
    ///
    /// Returns `None` when:
    ///
    /// - `value` is a scalar that is not a variable, or a block.
    /// - The resolver recorded an error for `value`. See
    ///   [`errors`](Resolution::errors).
    /// - `value` is from a different parse. See
    ///   [`is_for`](Resolution::is_for).
    ///
    /// ```
    /// use jomini::text::lossless::{parse, resolve::{ResolvedScalar, resolve}};
    ///
    /// let parse = parse(b"@size = 2\nx = @size\ny = @[size * 3]\nz = 4\nw = @missing");
    /// let resolution = resolve(parse.tree());
    /// let resolved = |key: &[u8]| resolution.get(parse.ast().get(key)?.value()?);
    /// assert_eq!(resolved(b"x"), Some(&ResolvedScalar::unquoted("2")));
    /// assert_eq!(resolved(b"y"), Some(&ResolvedScalar::unquoted("6")));
    /// assert_eq!(resolved(b"z"), None);
    /// assert_eq!(resolved(b"w"), None);
    /// ```
    pub fn get(&self, value: Value<'_, '_>) -> Option<&ResolvedScalar> {
        let tree = match value.syntax() {
            SyntaxElement::Node(node) => node.tree,
            SyntaxElement::Token(token) => token.tree,
        };
        if !self.is_for(tree) {
            return None;
        }
        match value {
            Value::Scalar(token) => self.token(token.id()),
            Value::Calc(calc) => self.node(calc.syntax().id()),
            Value::ReaderDirective(directive) => self.node(directive.syntax().id()),
            _ => None,
        }
    }
}

/// Find the value of each variable and each calculation of `tree`.
///
/// A variable that the tree does not define is a
/// [`UndefinedVariable`](ResolveErrorKind::UndefinedVariable) error. To use
/// the variables of other files, use [`resolve_with`]. The resolver follows
/// the [rules](resolve_with#rules) of `resolve_with`.
pub fn resolve(tree: SyntaxTree<'_, '_>) -> Resolution {
    resolve_with(tree, &Variables::new())
}

/// Find the value of each variable and each calculation of `tree`. A
/// variable that the tree does not define comes from `scope`, the variables
/// of other files.
///
/// ```
/// use jomini::text::lossless::{parse, resolve::{ResolvedScalar, Variables, resolve_with}};
///
/// let scope: Variables = [("width", "2")].into_iter().collect();
/// let parse = parse(b"x = @[width * 3]");
/// let resolution = resolve_with(parse.tree(), &scope);
/// let x = parse.ast().get(b"x").and_then(|field| field.value()).unwrap();
/// assert_eq!(resolution.get(x), Some(&ResolvedScalar::unquoted("6")));
/// ```
///
/// # Rules
///
/// These rules follow the CK3 file `game/reader_export/_reader_export.info`
/// and the behavior of the game files of EU5, CK3, Vic3, HoI4, and Imperator.
///
/// - A field with a variable key, such as `@size = 0.5`, defines the
///   variable. The right side is a scalar, a different variable, or a
///   calculation. The CK3 directive `@:register_variable size = 0.5` also
///   defines a variable.
/// - A definition applies from its position to the end of the file. A block
///   does not limit it: a definition in a block also applies after the
///   block, because the game replaces the variables before it parses the
///   blocks. A use before the definition is an
///   [`UndefinedVariable`](ResolveErrorKind::UndefinedVariable) error.
/// - In value position, `@:load_variable size` is the same as `@size`. In
///   key position, `@@size` puts the value of `size` in place of the key:
///   after `@size = width`, the field `@@size = 3` reads as `width = 3`.
/// - A calculation has numbers, variable names (with or without `@`), `+`,
///   `-`, `*`, `/`, and parentheses. `*` and `/` bind more tightly than `+`
///   and `-`. Operators of the same level apply from left to right, so
///   `@[8 / 4 / 2]` is `1`. A `-` before an operand changes its sign, and a
///   `+` before an operand has no effect.
/// - The resolver does not examine the tokens of `@:define` or the
///   arguments of `@:insert`. The game applies them where it inserts the
///   block.
/// - The resolver does not examine a code payload (`code [[ ... ]]`), such
///   as the body of an EU5 `template`, or a conditional parameter block
///   (`[[name] ... ]`). The game applies their contents only where a script
///   gives the parameters. [`Resolution::get`] returns `None` for a `@name`
///   in them, and a document reads its source text.
/// - When the resolver cannot find a value, it records a [`ResolveError`].
///   [`Resolution::get`] then returns `None`, and a document reads the
///   source text. The variants of [`ResolveErrorKind`] give the other rules,
///   such as what a second definition of a name does.
pub fn resolve_with(tree: SyntaxTree<'_, '_>, scope: &Variables) -> Resolution {
    let mut resolver = Resolver {
        scope,
        failed: HashSet::new(),
        out: Resolution {
            tokens: Vec::new(),
            nodes: Vec::new(),
            variables: Variables::new(),
            errors: Vec::new(),
            parse_id: tree.data.parse_id,
        },
    };
    resolver.walk(tree.root());
    let mut out = resolver.out;
    // The walk is in source order, so the lists are almost always sorted
    // already.
    out.tokens.sort_unstable_by_key(|(id, _)| *id);
    out.nodes.sort_unstable_by_key(|(id, _)| *id);
    out
}

/// Find the value of a calculation that has only numbers. To use
/// variables, use [`evaluate_with`].
///
/// ```
/// use jomini::text::lossless::{Calc, AstNode, parse, resolve::evaluate};
///
/// let parse = parse(b"x = @[(1 + 0.5) * 2]");
/// let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
/// assert_eq!(evaluate(calc), Ok(3.0));
/// ```
///
/// # Errors
///
/// Returns each error of the calculation in source order, as
/// [`resolve`] records them. The list is not empty.
pub fn evaluate(calc: Calc<'_, '_>) -> Result<f64, Vec<ResolveError>> {
    evaluate_with(calc, &Variables::new())
}

/// Find the value of a calculation. A variable comes from `scope`.
///
/// ```
/// use jomini::text::lossless::{Calc, AstNode, parse, resolve::{Variables, evaluate_with}};
///
/// let parse = parse(b"x = @[(1 + width) * 2]");
/// let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
/// let mut scope = Variables::new();
/// scope.insert("width", "0.5");
/// assert_eq!(evaluate_with(calc, &scope), Ok(3.0));
/// ```
///
/// # Errors
///
/// Returns each error of the calculation in source order, as
/// [`resolve_with`] records them. The list is not empty.
pub fn evaluate_with(calc: Calc<'_, '_>, scope: &Variables) -> Result<f64, Vec<ResolveError>> {
    let lookup = |name: &[u8]| match scope.get(name) {
        Some(value) => Lookup::Found(value.as_bytes()),
        None => Lookup::Missing,
    };
    // Only a failed definition of the file gives an empty list, and the
    // scope has no failed definitions.
    eval_calc(calc, &lookup)
}

enum Lookup<'s> {
    Found(&'s [u8]),
    /// The definition of the variable has an error, which the resolver
    /// records one time.
    Failed,
    Missing,
}

/// The errors of a calculation. The list is empty when the cause is a
/// definition that has an error, which the resolver records one time.
type EvalResult = Result<f64, Vec<ResolveError>>;

struct Resolver<'s> {
    scope: &'s Variables,
    /// The variables whose last definition has an error.
    failed: HashSet<Box<[u8]>>,
    out: Resolution,
}

impl Resolver<'_> {
    /// Walk the children of `node` in source order. A definition and a
    /// calculation take their subtree. A code payload, such as the body of
    /// an EU5 `template`, and a conditional parameter block are not a part
    /// of the file scope: they apply only where a caller gives the
    /// parameters.
    ///
    /// The parser limits the depth of the tree, so the recursion is safe.
    fn walk(&mut self, node: SyntaxNode<'_, '_>) {
        for element in node.significant_children() {
            let child = match element {
                SyntaxElement::Token(token) => {
                    if let Some(token) = ScalarToken::cast(token)
                        && token.variable_name().is_some()
                    {
                        self.reference(token);
                    }
                    continue;
                }
                SyntaxElement::Node(child) => child,
            };
            match child.kind() {
                SyntaxKind::Field => match Field::cast(child) {
                    Some(field) if field.is_variable_definition() => self.define(field),
                    _ => self.walk(child),
                },
                SyntaxKind::Calc => {
                    if let Some(calc) = Calc::cast(child) {
                        self.calc(calc);
                    }
                }
                SyntaxKind::ReaderDirective => self.directive(child),
                SyntaxKind::Code | SyntaxKind::Parameter | SyntaxKind::UndefinedParameter => {}
                _ => self.walk(child),
            }
        }
    }

    fn lookup(&self, name: &[u8]) -> Lookup<'_> {
        if let Some(value) = self.out.variables.get(name) {
            Lookup::Found(value.as_bytes())
        } else if self.failed.contains(name) {
            Lookup::Failed
        } else if let Some(value) = self.scope.get(name) {
            Lookup::Found(value.as_bytes())
        } else {
            Lookup::Missing
        }
    }

    /// The value of a variable token, which it also records, or `None`
    /// when it has no value.
    fn variable(&mut self, token: ScalarToken<'_, '_>) -> Option<ResolvedScalar> {
        let name = token.variable_name().unwrap_or(token.text());
        let value = self.variable_named(name, token.text_range())?;
        self.out.tokens.push((token.id(), value.clone()));
        Some(value)
    }

    /// The value of the variable `name`, or `None` when it has no value.
    /// An error is at `range`.
    fn variable_named(&mut self, name: &[u8], range: TextRange) -> Option<ResolvedScalar> {
        if let Some(value) = self.out.variables.get(name) {
            return Some(value.clone());
        }
        if self.failed.contains(name) {
            return None;
        }
        if let Some(value) = self.scope.get(name) {
            return Some(value.clone());
        }
        let error = ResolveError::new(ResolveErrorKind::UndefinedVariable, range);
        self.out.errors.push(error);
        None
    }

    /// The value of a `@:load_variable` directive, which it also records.
    fn load_variable(&mut self, directive: ReaderDirective<'_, '_>) -> Option<ResolvedScalar> {
        let argument = directive.argument()?;
        let value = self.variable_named(argument.text(), argument.text_range())?;
        self.out
            .nodes
            .push((directive.syntax().id(), value.clone()));
        Some(value)
    }

    /// A CK3 reader directive. `@:register_variable` defines a variable,
    /// and `@:load_variable` in value position gives its value. The tokens
    /// of `@:define` and the arguments of `@:insert` apply where the block
    /// is inserted, so the resolver does not examine them.
    fn directive(&mut self, node: SyntaxNode<'_, '_>) {
        let Some(directive) = ReaderDirective::cast(node) else {
            return;
        };
        match directive.kind() {
            DirectiveKind::RegisterVariable => {
                if let Some(argument) = directive.argument() {
                    self.define_value(argument, argument.text(), directive.value());
                }
            }
            DirectiveKind::LoadVariable => {
                self.load_variable(directive);
            }
            _ => {}
        }
    }

    fn reference(&mut self, token: ScalarToken<'_, '_>) {
        self.variable(token);
    }

    fn calc(&mut self, calc: Calc<'_, '_>) {
        let value = eval_calc(calc, &|name| self.lookup(name));
        match value {
            Ok(value) => {
                self.out
                    .nodes
                    .push((calc.syntax().id(), format_number(value)));
            }
            Err(errors) => self.out.errors.extend(errors),
        }
    }

    fn define(&mut self, field: Field<'_, '_>) {
        let key = field.key();
        let name = key.variable_name().unwrap_or(key.text());
        self.define_value(key, name, field.value());
    }

    /// Define the variable `name` with `value`. The name is at `key`.
    fn define_value(
        &mut self,
        key: ScalarToken<'_, '_>,
        name: &[u8],
        value: Option<Value<'_, '_>>,
    ) {
        if self.out.variables.get(name).is_some() || self.failed.contains(name) {
            let error = ResolveError::new(ResolveErrorKind::Redefined, key.text_range());
            self.out.errors.push(error);
            return;
        }
        if self.scope.get(name).is_some() {
            let error = ResolveError::new(ResolveErrorKind::DefinedInScope, key.text_range());
            self.out.errors.push(error);
            return;
        }
        let name: Box<[u8]> = name.into();
        let value = match value {
            Some(Value::Scalar(token)) => match token.kind() {
                SyntaxKind::Variable => self.variable(token),
                _ => Some(ResolvedScalar::of_token(token)),
            },
            Some(Value::ReaderDirective(directive))
                if directive.kind() == DirectiveKind::LoadVariable =>
            {
                self.load_variable(directive)
            }
            Some(Value::Calc(calc)) => {
                let value = eval_calc(calc, &|name| self.lookup(name));
                match value {
                    Ok(value) => {
                        let value = format_number(value);
                        self.out.nodes.push((calc.syntax().id(), value.clone()));
                        Some(value)
                    }
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
                self.out.variables.values.insert(name, value);
            }
            None => {
                self.failed.insert(name);
            }
        }
    }
}

/// The shortest decimal text that gives `value` again. It has no exponent,
/// so each game can read it.
fn format_number(value: f64) -> ResolvedScalar {
    // Add zero to change `-0` into `0`.
    ResolvedScalar::unquoted((value + 0.0).to_string())
}

fn unsupported(range: TextRange) -> Vec<ResolveError> {
    vec![ResolveError::new(
        ResolveErrorKind::UnsupportedCalculation,
        range,
    )]
}

fn finite(value: f64, range: TextRange) -> EvalResult {
    if value.is_finite() {
        Ok(value)
    } else {
        Err(vec![ResolveError::new(ResolveErrorKind::NotFinite, range)])
    }
}

/// Evaluate a calculation. The parser accepts tokens that are not a part of
/// the expression, as in `@[1 2]`. The accessors of the expression do not
/// give such a part, so the calculation is not supported.
fn eval_calc<'s>(calc: Calc<'_, '_>, lookup: &dyn Fn(&[u8]) -> Lookup<'s>) -> EvalResult {
    let range = calc.syntax().text_range();
    let expr = calc.expr().ok_or_else(|| unsupported(range))?;
    let value = eval(expr, lookup)?;
    finite(value, range)
}

fn eval<'s>(expr: Expr<'_, '_>, lookup: &dyn Fn(&[u8]) -> Lookup<'s>) -> EvalResult {
    let range = expr.text_range();
    match expr {
        // A number that is too large for an f64 parses as infinity, and a
        // division can hide it, so each operand must be finite.
        Expr::Number(token) => {
            let value = parse_number(token.text()).ok_or_else(|| unsupported(range))?;
            finite(value, range)
        }
        Expr::Ident(token) => {
            let text = token.text();
            let name = text.strip_prefix(b"@").unwrap_or(text);
            match lookup(name) {
                Lookup::Found(value) => {
                    let value = parse_number(value).ok_or_else(|| {
                        vec![ResolveError::new(ResolveErrorKind::NotNumeric, range)]
                    })?;
                    finite(value, range)
                }
                Lookup::Failed => Err(Vec::new()),
                Lookup::Missing => Err(vec![ResolveError::new(
                    ResolveErrorKind::UndefinedVariable,
                    range,
                )]),
            }
        }
        Expr::Binary(binary) => {
            let (Some(lhs), Some(rhs)) = (binary.lhs(), binary.rhs()) else {
                return Err(unsupported(range));
            };
            // Evaluate both operands, so each undefined variable of the
            // calculation has an error, also after the first one.
            let (lhs, rhs) = match (eval(lhs, lookup), eval(rhs, lookup)) {
                (Ok(lhs), Ok(rhs)) => (lhs, rhs),
                (lhs, rhs) => {
                    let mut errors = lhs.err().unwrap_or_default();
                    errors.extend(rhs.err().unwrap_or_default());
                    return Err(errors);
                }
            };
            let value = match binary.op() {
                BinOp::Add => lhs + rhs,
                BinOp::Sub => lhs - rhs,
                BinOp::Mul => lhs * rhs,
                BinOp::Div => lhs / rhs,
            };
            finite(value, range)
        }
        Expr::Unary(unary) => {
            let operand = unary.operand().ok_or_else(|| unsupported(range))?;
            let value = eval(operand, lookup)?;
            Ok(match unary.op() {
                UnaryOp::Neg => -value,
                UnaryOp::Plus => value,
            })
        }
        Expr::Paren(paren) => eval(paren.inner().ok_or_else(|| unsupported(range))?, lookup),
    }
}

/// Parse a number of a calculation or of a variable. The lexer accepts a
/// number with an `f` at the end, as in `10.0f`.
fn parse_number(text: &[u8]) -> Option<f64> {
    let text = text.strip_suffix(b"f").unwrap_or(text);
    std::str::from_utf8(text).ok()?.parse().ok()
}
