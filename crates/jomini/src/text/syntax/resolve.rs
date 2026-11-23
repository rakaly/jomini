//! Resolve `@name` variables and `@[ ... ]` calculations in game and mod files.
//! Use a syntax tree for resolution and `TextDeserializer` for saves that
//! only need streaming deserialization.
//!
//! [`resolve`] returns a [`Resolution`] alongside the tree, preserving its
//! source text for tools that write the file back.
//!
//! # Deserialize with the values
//!
//! Pass the resolution to
//! `Document::with_resolution` with the `serde` feature. The
//! document then reads each variable and each calculation as its value. The
//! document skips a definition such as `@size = 0.5`, so your type needs no
//! field for it, even with `#[serde(deny_unknown_fields)]`:
//!
//! ```
//! # #[cfg(feature = "serde")] {
//! use jomini::{Utf8Encoding, text::syntax::{parse, resolve::resolve}};
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
//! let file: File = parse.document(Utf8Encoding::new())
//!     .with_resolution(resolution)?
//!     .deserialize()?;
//! assert_eq!(file.emblem.scale, [0.5, 0.5]);
//! assert_eq!(file.emblem.position, [0.5, 0.5]);
//! # }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Read one value without serde
//!
//! Find the [`Value`] node of a variable or a calculation in the tree, and
//! pass the node to [`Resolution::get`]. It returns a [`ResolvedScalar`]: the
//! text of the value, and whether the definition was quoted. Here,
//! `ruler = @name` gives the quoted text `Jean II`, because the definition of
//! `@name` has quotes. The calculation gives an unquoted number. For a
//! variable that is not a `Value` node, such as the `@@name` key of
//! `@@name = 1`, pass its [`ScalarToken`]:
//!
//! ```
//! use jomini::text::syntax::{parse, resolve::{ResolvedScalar, resolve}};
//!
//! let parse = parse(b"@name = \"Jean II\"\n@size = 2\nruler = @name\nwidth = @[size * 3]\n@@name = 1");
//! let resolution = resolve(parse.tree());
//! let resolved = |key: &[u8]| resolution.get(parse.ast().get(key)?.value()?);
//! assert_eq!(resolved(b"ruler"), Some(&ResolvedScalar::quoted("Jean II")));
//! assert_eq!(resolved(b"width"), Some(&ResolvedScalar::unquoted("6")));
//! let key = parse.ast().fields().last().unwrap().key();
//! assert_eq!(resolution.get(key), Some(&ResolvedScalar::quoted("Jean II")));
//! ```
//!
//! # Use variables from other files
//!
//! A file can use a variable that a shared file defines, such as a CK3 file
//! of `reader_export` or a Vic3 file of `gui/shared`. Give the variables of
//! the shared files to [`resolve_with`] as one [`Variables`] set. This set
//! is the *scope* of the file. The scope holds the variables of the files
//! that share them, not of every file. To resolve a file with a scope:
//!
//! 1. Resolve the shared files in the order that the game loads them, each
//!    with [`resolve_with`] and the variables of the shared files before it.
//! 2. Add the [`Resolution::variables`] of each shared file to the scope
//!    with [`Variables::extend_from`], in the same order.
//! 3. Resolve each file that uses them with [`resolve_with`] and the scope.
//!
//! [`Resolution::variables`] of a file holds each name that the file
//! defines, also a name that its scope has. So when shared files define the
//! same name, the scope depends on their order: the value of the last file
//! wins. A name whose last definition in a file has an error is not in that
//! file's variables, so a value from an earlier file stays in the combined
//! scope; the error is in that file's errors.
//!
//! ```
//! use jomini::text::syntax::{parse, resolve::{ResolvedScalar, Variables, resolve, resolve_with}};
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
//! A definition applies from its position until the next definition of the
//! name, and the scope counts as definitions before the file starts. So a
//! file can define a name that the scope has, as many Vic3 GUI files
//! redefine a variable of `gui/shared/layout.gui`, and the right side of
//! each definition reads the value before it:
//!
//! ```
//! use jomini::text::syntax::{parse, resolve::{ResolvedScalar, Variables, resolve_with}};
//!
//! let scope: Variables = [("width", "1")].into_iter().collect();
//! let parse = parse(b"a = @width\n@width = @[width + 1]\nb = @width");
//! let resolution = resolve_with(parse.tree(), &scope);
//! let resolved = |key: &[u8]| resolution.get(parse.ast().get(key)?.value()?);
//! assert_eq!(resolution.errors(), []);
//! assert_eq!(resolved(b"a"), Some(&ResolvedScalar::unquoted("1")));
//! assert_eq!(resolved(b"b"), Some(&ResolvedScalar::unquoted("2")));
//! assert_eq!(resolution.variables().get("width"), Some(&ResolvedScalar::unquoted("2")));
//! ```
//!
//! To deserialize with a shared scope, pass its resolution to
//! `Document::with_resolution` as above.
//!
//! # Resolve a file again after an edit
//!
//! An editor resolves a file again after each change. The scope for the file
//! must not hold the variables of the file itself. A definition of the file
//! replaces the value of the scope only from its position, so when a
//! combined scope still holds the variables of the file from before the
//! edit, a use before the definition gets the value from before the edit
//! and has no error. Here, `a` should be an undefined variable, but the
//! stale scope gives it the old value `1` with no error:
//!
//! ```
//! use jomini::text::syntax::{parse, resolve::{ResolvedScalar, resolve, resolve_with}};
//!
//! let old = parse(b"@x = 1");
//! let stale = resolve(old.tree()).variables().clone();
//!
//! let edited = parse(b"a = @x\n@x = 2\nb = @x");
//! let resolution = resolve_with(edited.tree(), &stale);
//! let resolved = |key: &[u8]| resolution.get(edited.ast().get(key)?.value()?);
//! assert_eq!(resolution.errors(), []);
//! assert_eq!(resolved(b"a"), Some(&ResolvedScalar::unquoted("1")));
//! assert_eq!(resolved(b"b"), Some(&ResolvedScalar::unquoted("2")));
//! ```
//!
//! Keep one [`Variables`] set per shared file, in load order. After an edit
//! of a shared file, resolve it again with the sets of the shared files
//! before it, then resolve each later shared file again. A clone of a set
//! shares the names and the values. If you use one combined scope, see the
//! caveat in [`Variables::remove`] before removing a file's old definitions.
//!
//! # Find variables that have no value
//!
//! When the resolver cannot find the value of a variable or a calculation, it
//! records a [`ResolveError`] in [`Resolution::errors`]. When a definition
//! has an error, each use of the name until the next definition has no
//! value, and the error is at the definition. The
//! [`range`](ResolveError::range) of the error is a byte
//! range of the source. [`LineIndex`](super::LineIndex) converts its start
//! to a line and a column.
//!
//! A `Document` reads a variable or a calculation
//! that has no value as its source text. A number field then fails with an
//! error such as `invalid type: string "@size", expected f32`, which does not
//! tell the cause. Report the errors of the resolution before you
//! deserialize:
//!
//! ```
//! use jomini::text::syntax::{LineIndex, parse, resolve::resolve};
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
//! # Find each definition
//!
//! A second definition of a name is not an error. To find one, as a linter
//! does, or to find the definition that a use reads, as an editor does for
//! "go to definition", use [`Resolution::definitions`] and
//! [`Resolution::definition`].
//!
//! The rules that the resolver follows are in the
//! [`resolve_with`](resolve_with#rules) documentation.

use super::{
    AstNode, BinOp, Calc, DirectiveKind, Expr, Field, NodeId, ReaderDirective, ScalarToken,
    SyntaxElement, SyntaxKind, SyntaxNode, SyntaxTree, TextRange, TokenId, UnaryOp, Value,
};
use crate::{Encoding, Scalar, text::decode_quoted};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::num::NonZeroU32;
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
/// use jomini::text::syntax::{parse, resolve::{ResolvedScalar, resolve}};
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

    /// Decode the value into a string with `encoding`. A quoted value is
    /// first unescaped, as with
    /// [`ScalarToken::decode`](super::ScalarToken::decode).
    pub fn decode<E: Encoding>(&self, encoding: E) -> Cow<'_, str> {
        if self.quoted {
            decode_quoted(&encoding, &self.text)
        } else {
            encoding.decode(&self.text)
        }
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
/// set does not include the variables of the scope that the file only uses.
/// A variable of the scope that the file defines has the value of the file:
///
/// ```
/// use jomini::text::syntax::{parse, resolve::{ResolvedScalar, Variables, resolve_with}};
///
/// let mut scope = Variables::new();
/// scope.insert("half", "0.5");
/// scope.insert("width", "1");
///
/// let parse = parse(b"@quarter = @[half / 2]\nx = @half\n@width = 2");
/// let resolution = resolve_with(parse.tree(), &scope);
/// assert_eq!(resolution.errors(), []);
/// let variables = resolution.variables();
/// assert_eq!(variables.get("quarter"), Some(&ResolvedScalar::unquoted("0.25")));
/// assert_eq!(variables.get("half"), None);
/// assert_eq!(variables.get("width"), Some(&ResolvedScalar::unquoted("2")));
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Variables {
    /// Each value is shared with each reference of a [`Resolution`] that
    /// gives it, so a long value is stored one time.
    values: HashMap<Arc<[u8]>, ResolvedScalar>,
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
    /// use jomini::text::syntax::resolve::{ResolvedScalar, Variables};
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
    /// use jomini::text::syntax::resolve::{ResolvedScalar, Variables};
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
    /// variables of several shared files into a scope. When both sets have a
    /// name, the value of `other` replaces the value of this set. The sets
    /// share the names and the values, so the function copies no text.
    ///
    /// ```
    /// use jomini::text::syntax::{parse, resolve::{ResolvedScalar, Variables, resolve, resolve_with}};
    ///
    /// // Two shared files, where the second uses a variable of the first.
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
/// set, use [`Variables::extend_from`], which copies no text.
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
pub struct Iter<'v>(std::collections::hash_map::Iter<'v, Arc<[u8]>, ResolvedScalar>);

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
    /// a `@:load_variable` directive, or a calculation. For example, it is a
    /// block, as in `@x = { 1 2 }`.
    UnsupportedDefinition,
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
/// use jomini::text::syntax::{parse, resolve::{ResolveErrorKind, resolve}};
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
    /// the range of its name. For an
    /// [`UnsupportedDefinition`](ResolveErrorKind::UnsupportedDefinition), it
    /// is the right side of the definition, or the whole directive for a
    /// `@:register_variable` with no value.
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
    /// What each variable token reads, sorted by token.
    tokens: Vec<Recorded<TokenId>>,
    /// The value of each calculation and what each `@:load_variable` node
    /// reads, sorted by node.
    nodes: Vec<Recorded<NodeId>>,
    /// Each definition of the file, in source order.
    definitions: Vec<DefinitionData>,
    variables: Variables,
    errors: Vec<ResolveError>,
    /// The identity of the parse of the tree.
    parse_id: u64,
}

impl Resolution {
    /// The errors of the variables and the calculations that have no value.
    /// A use of a variable whose definition has an error has no error of its
    /// own: the error is at the definition.
    pub fn errors(&self) -> &[ResolveError] {
        &self.errors
    }

    /// The variables that the file defines, each with the value of its last
    /// definition. This set includes a variable of the scope that the file
    /// defines, with the value of the file, and does not include the
    /// variables of the scope that the file only uses. A name whose last
    /// definition has an error is not in the set.
    pub fn variables(&self) -> &Variables {
        &self.variables
    }

    /// Each definition of a variable in the file, in source order. A
    /// definition that has an error is also in the list, with no value.
    ///
    /// Use the definitions to find what the file defines more than one time,
    /// or to show which definition a use reads, with
    /// [`definition`](Resolution::definition). Here, a linter reports each
    /// definition that replaces a definition of the file:
    ///
    /// ```
    /// use jomini::text::syntax::{LineIndex, parse, resolve::{Definition, Replaced, resolve}};
    ///
    /// let source = b"@a = 1\n@b = 2\n@a = @[a + 1]";
    /// let parse = parse(source);
    /// let resolution = resolve(parse.tree());
    /// assert_eq!(resolution.errors(), []);
    ///
    /// let lines = LineIndex::new(source);
    /// let at = |definition: Definition| lines.line_col(definition.key_range().start());
    /// let messages: Vec<String> = resolution
    ///     .definitions()
    ///     .filter_map(|definition| match definition.replaces()? {
    ///         Replaced::Definition(previous) => Some(format!(
    ///             "{}: @{} is already defined at {}",
    ///             at(definition),
    ///             String::from_utf8_lossy(definition.name()),
    ///             at(previous),
    ///         )),
    ///         Replaced::Scope => None,
    ///     })
    ///     .collect();
    /// assert_eq!(messages, ["3:1: @a is already defined at 1:1"]);
    /// ```
    pub fn definitions(&self) -> Definitions<'_> {
        Definitions {
            resolution: self,
            indices: 0..self.definitions.len(),
        }
    }

    /// The definition of the file that the variable or the
    /// `@:load_variable` directive at `at` reads. See [`Resolvable`] for
    /// what `at` can be.
    ///
    /// Returns `None` in the cases of [`get`](Resolution::get), for a
    /// calculation, which can read several definitions, and for a use that
    /// reads the scope or an undefined variable. A use of a failed
    /// definition gives the definition, whose [`value`](Definition::value)
    /// is `None`.
    ///
    /// ```
    /// use jomini::text::syntax::{AstNode, ReaderDirective, parse, resolve::{ResolvedScalar, Variables, resolve_with}};
    ///
    /// let scope: Variables = [("w", "1")].into_iter().collect();
    /// let parse = parse(b"a = @w\n@w = 2\nb = @w\n@w = 3\nc = @w\n@@w = 4\nd = @:load_variable w");
    /// let resolution = resolve_with(parse.tree(), &scope);
    /// let read = |key: &[u8]| resolution.definition(parse.ast().get(key)?.value()?);
    /// assert_eq!(read(b"a"), None);
    /// assert_eq!(read(b"b").map(|d| d.key_range().to_range()), Some(7..9));
    /// let c = read(b"c").unwrap();
    /// assert_eq!(c.key_range().to_range(), 21..23);
    /// assert_eq!(c.value(), Some(&ResolvedScalar::unquoted("3")));
    ///
    /// // The `@@w` key and the directive read the same definition.
    /// let key = parse.ast().fields().nth(5).unwrap().key();
    /// assert_eq!(resolution.definition(key), Some(c));
    /// let directive = parse.root().descendants().find_map(ReaderDirective::cast).unwrap();
    /// assert_eq!(resolution.definition(directive), Some(c));
    /// ```
    pub fn definition(&self, at: impl Resolvable) -> Option<Definition<'_>> {
        Some(Definition {
            resolution: self,
            index: at.reads(self)?.definition?,
        })
    }

    /// What the resolver recorded for a variable token.
    fn token_reads(&self, token: ScalarToken<'_, '_>) -> Option<Reads<'_>> {
        if !self.is_for(token.syntax().tree) {
            return None;
        }
        find(&self.tokens, token.id()).map(Recorded::reads)
    }

    /// What the resolver recorded for a calculation or a `@:load_variable`
    /// node.
    fn node_reads(&self, node: SyntaxNode<'_, '_>) -> Option<Reads<'_>> {
        if !self.is_for(node.tree) {
            return None;
        }
        find(&self.nodes, node.id()).map(Recorded::reads)
    }

    /// The value of a variable token.
    pub(crate) fn token(&self, id: TokenId) -> Option<&ResolvedScalar> {
        find(&self.tokens, id)?.value.as_ref()
    }

    /// The value of a calculation or a `@:load_variable` node.
    pub(crate) fn node(&self, id: NodeId) -> Option<&ResolvedScalar> {
        find(&self.nodes, id)?.value.as_ref()
    }

    /// Return `true` when this is the resolution of `tree`: the tree is of
    /// the parse that [`resolve`] read, or of a clone of that parse. A
    /// second parse of the same source is a different parse. The check
    /// compares one number.
    ///
    /// ```
    /// use jomini::text::syntax::{parse, resolve::resolve};
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

    /// The value of the variable, the calculation, or the `@:load_variable`
    /// directive at `at`. See [`Resolvable`] for what `at` can be.
    ///
    /// Returns `None` when `at` is not a use, such as a block or the key of
    /// a definition; when it has no value, as an undefined variable (see
    /// [`errors`](Resolution::errors)); when it is in a part that the
    /// resolver skips: a code payload, a parameter block, a `@:define` or
    /// `@:insert` body, or the right side of an unsupported definition; and
    /// when it is from a different parse.
    ///
    /// ```
    /// use jomini::text::syntax::{AstNode, Calc, parse, resolve::{ResolvedScalar, resolve}};
    ///
    /// let parse = parse(b"@size = 2\nx = @size\ny = @[size * 3]\nz = 4\nw = @missing\n@@size = 5");
    /// let resolution = resolve(parse.tree());
    /// let resolved = |key: &[u8]| resolution.get(parse.ast().get(key)?.value()?);
    /// assert_eq!(resolved(b"x"), Some(&ResolvedScalar::unquoted("2")));
    /// assert_eq!(resolved(b"y"), Some(&ResolvedScalar::unquoted("6")));
    /// assert_eq!(resolved(b"z"), None);
    /// assert_eq!(resolved(b"w"), None);
    ///
    /// // `@@size = 5` reads as `2 = 5`.
    /// let key = parse.ast().fields().last().unwrap().key();
    /// assert_eq!(resolution.get(key), Some(&ResolvedScalar::unquoted("2")));
    ///
    /// let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    /// assert_eq!(resolution.get(calc), Some(&ResolvedScalar::unquoted("6")));
    /// ```
    pub fn get(&self, at: impl Resolvable) -> Option<&ResolvedScalar> {
        at.reads(self)?.value
    }
}

/// A part of a tree that [`Resolution::get`] and [`Resolution::definition`]
/// can look up: a [`Value`] node, a [`ScalarToken`], a [`Calc`], or a
/// [`ReaderDirective`]. A token reaches the variables that are not a
/// [`Value`] node, such as the `@@name` key of `@@name = 3` or the header
/// of `x = @name { 1 }`. A [`Value::Scalar`] and its token give the same
/// result.
///
/// The trait is sealed: other crates cannot implement it.
///
/// ```
/// use jomini::text::syntax::{parse, resolve::{ResolvedScalar, resolve}};
///
/// let parse = parse(b"@a = 1\nx = @a { 2 }\ny = @a");
/// let resolution = resolve(parse.tree());
/// let one = Some(&ResolvedScalar::unquoted("1"));
/// let x = parse.ast().get(b"x").and_then(|field| field.value()).unwrap();
/// let header = x.as_headered_block().unwrap().header();
/// assert_eq!(resolution.get(header), one);
///
/// let y = parse.ast().get(b"y").and_then(|field| field.value()).unwrap();
/// assert_eq!(resolution.get(y), one);
/// assert_eq!(resolution.get(y.as_scalar_token().unwrap()), one);
/// ```
pub trait Resolvable: Copy + sealed::Sealed {}

use sealed::Reads;

mod sealed {
    use super::{AstNode, Calc, ReaderDirective, Resolution, ResolvedScalar, ScalarToken, Value};

    /// What the resolver recorded for a use: the index of the definition
    /// that it reads, and its value.
    pub struct Reads<'r> {
        pub(super) definition: Option<usize>,
        pub(super) value: Option<&'r ResolvedScalar>,
    }

    pub trait Sealed {
        /// What the resolver recorded for `self`, or `None` when `self` is
        /// of a different parse or the resolver recorded nothing.
        fn reads(self, resolution: &Resolution) -> Option<Reads<'_>>;
    }

    impl Sealed for Value<'_, '_> {
        fn reads(self, resolution: &Resolution) -> Option<Reads<'_>> {
            match self {
                Value::Scalar(token) => token.reads(resolution),
                Value::Calc(calc) => calc.reads(resolution),
                Value::ReaderDirective(directive) => directive.reads(resolution),
                _ => None,
            }
        }
    }

    impl Sealed for ScalarToken<'_, '_> {
        fn reads(self, resolution: &Resolution) -> Option<Reads<'_>> {
            resolution.token_reads(self)
        }
    }

    impl Sealed for Calc<'_, '_> {
        fn reads(self, resolution: &Resolution) -> Option<Reads<'_>> {
            resolution.node_reads(self.syntax())
        }
    }

    impl Sealed for ReaderDirective<'_, '_> {
        fn reads(self, resolution: &Resolution) -> Option<Reads<'_>> {
            resolution.node_reads(self.syntax())
        }
    }
}

impl Resolvable for Value<'_, '_> {}
impl Resolvable for ScalarToken<'_, '_> {}
impl Resolvable for Calc<'_, '_> {}
impl Resolvable for ReaderDirective<'_, '_> {}

fn find<I: Ord + Copy>(records: &[Recorded<I>], id: I) -> Option<&Recorded<I>> {
    let index = records.binary_search_by_key(&id, |record| record.id).ok()?;
    Some(&records[index])
}

/// What a variable token, a calculation, or a `@:load_variable` node gives.
#[derive(Debug, Clone)]
struct Recorded<I> {
    id: I,
    /// The definition of the file that a use reads. `None` for a use of the
    /// scope and for a calculation.
    definition: Option<DefinitionIndex>,
    /// `None` for a use of a definition that has an error.
    value: Option<ResolvedScalar>,
}

impl<I> Recorded<I> {
    fn reads(&self) -> Reads<'_> {
        Reads {
            definition: self.definition.map(DefinitionIndex::get),
            value: self.value.as_ref(),
        }
    }
}

impl Recorded<()> {
    fn with_id<I>(self, id: I) -> Recorded<I> {
        Recorded {
            id,
            definition: self.definition,
            value: self.value,
        }
    }
}

/// An index of `Resolution::definitions` plus one. An `Option` of it takes
/// four bytes, so on a 64-bit target a `Recorded<TokenId>` takes no more
/// space than a token id and a value: the index fits in their padding.
#[derive(Debug, Clone, Copy)]
struct DefinitionIndex(NonZeroU32);

impl DefinitionIndex {
    fn new(index: u32) -> Option<Self> {
        index.checked_add(1).and_then(NonZeroU32::new).map(Self)
    }

    fn get(self) -> usize {
        self.0.get() as usize - 1
    }
}

#[derive(Debug, Clone)]
struct DefinitionData {
    name: Arc<[u8]>,
    key: TextRange,
    value: Option<ResolvedScalar>,
    replaces: Replaces,
}

#[derive(Debug, Clone, Copy)]
enum Replaces {
    Nothing,
    Scope,
    /// The index of the previous definition of the name.
    Definition(u32),
}

/// A definition of a variable in a file, such as `@size = 0.5` or
/// `@:register_variable size = 0.5`. Get the definitions from
/// [`Resolution::definitions`] and [`Resolution::definition`].
///
/// Two definitions are equal when they are the same definition of the same
/// resolution.
#[derive(Clone, Copy)]
pub struct Definition<'r> {
    resolution: &'r Resolution,
    index: usize,
}

impl<'r> Definition<'r> {
    fn data(&self) -> &'r DefinitionData {
        &self.resolution.definitions[self.index]
    }

    /// The name of the variable, with no `@`.
    pub fn name(&self) -> &'r [u8] {
        &self.data().name
    }

    /// The byte range of the name in the source: the key `@size` of
    /// `@size = 0.5`, or `size` of `@:register_variable size = 0.5`.
    pub fn key_range(&self) -> TextRange {
        self.data().key
    }

    /// The value of the definition, or `None` when the definition has no
    /// value: its right side has an error, reads a variable that has no
    /// value, or is missing. The error is in [`Resolution::errors`], at the
    /// definition or at the earlier definition that has no value, or, for a
    /// definition with `=` and no value such as `@size =`, in the parse
    /// errors.
    pub fn value(&self) -> Option<&'r ResolvedScalar> {
        self.data().value.as_ref()
    }

    /// What the definition replaces: the previous definition of the name in
    /// the file, or else a variable of the scope. `None` for the first
    /// definition of a name that the scope does not have.
    pub fn replaces(&self) -> Option<Replaced<'r>> {
        match self.data().replaces {
            Replaces::Nothing => None,
            Replaces::Scope => Some(Replaced::Scope),
            Replaces::Definition(index) => Some(Replaced::Definition(Definition {
                resolution: self.resolution,
                index: index as usize,
            })),
        }
    }
}

impl PartialEq for Definition<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.resolution, other.resolution) && self.index == other.index
    }
}

impl Eq for Definition<'_> {}

impl std::hash::Hash for Definition<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::from_ref(self.resolution).hash(state);
        self.index.hash(state);
    }
}

impl fmt::Debug for Definition<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Definition")
            .field("name", &String::from_utf8_lossy(self.name()))
            .field("key_range", &self.key_range())
            .field("value", &self.value())
            .finish()
    }
}

/// What a [`Definition`] replaces, from [`Definition::replaces`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Replaced<'r> {
    /// A variable of the scope of [`resolve_with`].
    Scope,
    /// The previous definition of the name in the file.
    Definition(Definition<'r>),
}

/// The definitions of a file in source order, from
/// [`Resolution::definitions`].
#[derive(Clone)]
pub struct Definitions<'r> {
    resolution: &'r Resolution,
    indices: std::ops::Range<usize>,
}

/// The definitions that are left.
impl fmt::Debug for Definitions<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

impl<'r> Iterator for Definitions<'r> {
    type Item = Definition<'r>;

    fn next(&mut self) -> Option<Definition<'r>> {
        let index = self.indices.next()?;
        Some(Definition {
            resolution: self.resolution,
            index,
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.indices.size_hint()
    }
}

impl DoubleEndedIterator for Definitions<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let index = self.indices.next_back()?;
        Some(Definition {
            resolution: self.resolution,
            index,
        })
    }
}

impl ExactSizeIterator for Definitions<'_> {}

impl std::iter::FusedIterator for Definitions<'_> {}

/// Find the value of each variable and each calculation of `tree`.
///
/// A use of a variable that the tree does not define before the use is a
/// [`UndefinedVariable`](ResolveErrorKind::UndefinedVariable) error. A use
/// of a definition that has an error has no value and no second error.
/// To use the variables of other files, use [`resolve_with`]. The resolver
/// follows the [rules](resolve_with#rules) of `resolve_with`.
pub fn resolve(tree: SyntaxTree<'_, '_>) -> Resolution {
    resolve_with(tree, &Variables::new())
}

/// Find the value of each variable and each calculation of `tree`. A
/// variable that the tree does not define yet comes from `scope`, the
/// variables of other files.
///
/// ```
/// use jomini::text::syntax::{parse, resolve::{ResolvedScalar, Variables, resolve_with}};
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
/// - A definition applies from its position until the next definition of
///   the name. The scope counts as definitions before the file starts. A
///   block does not limit a definition: a definition in a block also
///   applies after the block, because the game replaces the variables
///   before it parses the blocks. A use before the first definition of the
///   name gets the value of the scope, and is an
///   [`UndefinedVariable`](ResolveErrorKind::UndefinedVariable) error when
///   the scope does not have the name.
/// - The right side of a definition reads the variables before it, so
///   `@x = @[x + 1]` adds one to the previous value of `x`, of the file or
///   of the scope. A second definition of a name is not an error. To find
///   one, use [`Resolution::definitions`].
/// - When a definition has an error, each use of the name until the next
///   definition has no value, also when the scope has the name. The error
///   is at the definition, and the uses have no errors. A
///   `@:register_variable size` with no `= value` is an
///   [`UnsupportedDefinition`](ResolveErrorKind::UnsupportedDefinition)
///   error; a definition with `=` and no value, such as `@size =`, has only
///   a parse error.
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
/// - When the resolver cannot find a value, it records a [`ResolveError`],
///   except for the cases above. [`Resolution::get`] then returns `None`,
///   and a document reads the source text. The variants of
///   [`ResolveErrorKind`] give the other rules.
pub fn resolve_with(tree: SyntaxTree<'_, '_>, scope: &Variables) -> Resolution {
    let mut resolver = Resolver {
        scope,
        latest: HashMap::new(),
        out: Resolution {
            tokens: Vec::new(),
            nodes: Vec::new(),
            definitions: Vec::new(),
            variables: Variables::new(),
            errors: Vec::new(),
            parse_id: tree.data.parse_id,
        },
    };
    resolver.walk(tree.root());
    let Resolver {
        latest, mut out, ..
    } = resolver;
    out.variables.values.reserve(latest.len());
    for (name, index) in latest {
        if let Some(value) = &out.definitions[index as usize].value {
            out.variables.values.insert(name, value.clone());
        }
    }
    // The walk is in source order, so the lists are almost always sorted
    // already.
    out.tokens.sort_unstable_by_key(|record| record.id);
    out.nodes.sort_unstable_by_key(|record| record.id);
    // A cache can keep the resolution for a long time.
    out.tokens.shrink_to_fit();
    out.nodes.shrink_to_fit();
    out.definitions.shrink_to_fit();
    out.errors.shrink_to_fit();
    out
}

/// Find the value of a calculation that has only numbers. To use
/// variables, use [`evaluate_with`].
///
/// ```
/// use jomini::text::syntax::{Calc, AstNode, parse, resolve::evaluate};
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
/// use jomini::text::syntax::{Calc, AstNode, parse, resolve::{Variables, evaluate_with}};
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
    /// The last definition of the variable has an error, which the
    /// resolver records one time.
    Failed,
    Missing,
}

/// The errors of a calculation. The list is empty when the cause is a
/// definition that has an error, which the resolver records one time.
type EvalResult = Result<f64, Vec<ResolveError>>;

/// What a use of a variable reads.
enum Read<'r> {
    /// The definition of the file at this index, and its value, if any.
    File(u32, Option<&'r ResolvedScalar>),
    Scope(&'r ResolvedScalar),
    Missing,
}

struct Resolver<'s> {
    scope: &'s Variables,
    /// The last definition of each name so far, as an index of
    /// `out.definitions`. The name is shared with the definitions.
    latest: HashMap<Arc<[u8]>, u32>,
    out: Resolution,
}

impl Resolver<'_> {
    /// Walk the elements under `node` in source order. A definition and a
    /// calculation take their subtree. A code payload, such as the body of
    /// an EU5 `template`, and a conditional parameter block are not a part
    /// of the file scope: they apply only where a caller gives the
    /// parameters.
    ///
    /// Blocks nest without limit, as in an EU4 save, so the walk keeps the
    /// nodes that it is in on a list, not on the stack.
    fn walk(&mut self, node: SyntaxNode<'_, '_>) {
        let mut open = vec![node.significant_children()];
        while let Some(children) = open.last_mut() {
            let Some(element) = children.next() else {
                open.pop();
                continue;
            };
            let child = match element {
                SyntaxElement::Token(token) => {
                    if let Some(token) = ScalarToken::cast(token)
                        && token.variable_name().is_some()
                    {
                        self.variable(token);
                    }
                    continue;
                }
                SyntaxElement::Node(child) => child,
            };
            match child.kind() {
                SyntaxKind::Field => match Field::cast(child) {
                    Some(field) if field.is_variable_definition() => self.define(field),
                    _ => open.push(child.significant_children()),
                },
                SyntaxKind::Calc => {
                    if let Some(calc) = Calc::cast(child) {
                        self.calc(calc);
                    }
                }
                SyntaxKind::ReaderDirective => self.directive(child),
                SyntaxKind::Code | SyntaxKind::Parameter | SyntaxKind::UndefinedParameter => {}
                _ => open.push(child.significant_children()),
            }
        }
    }

    /// The last definition of `name` in the file so far, or else the scope.
    fn read(&self, name: &[u8]) -> Read<'_> {
        if let Some(&index) = self.latest.get(name) {
            let value = self.out.definitions[index as usize].value.as_ref();
            Read::File(index, value)
        } else if let Some(value) = self.scope.get(name) {
            Read::Scope(value)
        } else {
            Read::Missing
        }
    }

    fn lookup(&self, name: &[u8]) -> Lookup<'_> {
        match self.read(name) {
            Read::File(_, Some(value)) | Read::Scope(value) => Lookup::Found(value.as_bytes()),
            Read::File(_, None) => Lookup::Failed,
            Read::Missing => Lookup::Missing,
        }
    }

    /// What a use of `name` reads. A missing variable is an error at
    /// `range` and gives `None`.
    fn read_named(&mut self, name: &[u8], range: TextRange) -> Option<Recorded<()>> {
        let (definition, value) = match self.read(name) {
            Read::File(index, value) => (DefinitionIndex::new(index), value.cloned()),
            Read::Scope(value) => (None, Some(value.clone())),
            Read::Missing => {
                let error = ResolveError::new(ResolveErrorKind::UndefinedVariable, range);
                self.out.errors.push(error);
                return None;
            }
        };
        Some(Recorded {
            id: (),
            definition,
            value,
        })
    }

    /// The value of a variable token, which it also records, or `None`
    /// when it has no value.
    fn variable(&mut self, token: ScalarToken<'_, '_>) -> Option<ResolvedScalar> {
        let name = token.variable_name().unwrap_or(token.text());
        let read = self.read_named(name, token.text_range())?;
        let value = read.value.clone();
        self.out.tokens.push(read.with_id(token.id()));
        value
    }

    /// The value of a `@:load_variable` directive, which it also records.
    fn load_variable(&mut self, directive: ReaderDirective<'_, '_>) -> Option<ResolvedScalar> {
        let argument = directive.argument()?;
        let read = self.read_named(argument.text(), argument.text_range())?;
        let value = read.value.clone();
        self.out.nodes.push(read.with_id(directive.syntax().id()));
        value
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
                    let value = directive.value();
                    // With `=` and no value, the parser has an error.
                    if value.is_none() && directive.op_token().is_none() {
                        let range = directive.syntax().text_range();
                        let error =
                            ResolveError::new(ResolveErrorKind::UnsupportedDefinition, range);
                        self.out.errors.push(error);
                    }
                    self.define_value(argument, argument.text(), value);
                }
            }
            DirectiveKind::LoadVariable => {
                self.load_variable(directive);
            }
            _ => {}
        }
    }

    /// The value of a calculation, which it also records, or `None` when
    /// it has an error.
    fn calc(&mut self, calc: Calc<'_, '_>) -> Option<ResolvedScalar> {
        match eval_calc(calc, &|name| self.lookup(name)) {
            Ok(value) => {
                let value = format_number(value);
                self.out.nodes.push(Recorded {
                    id: calc.syntax().id(),
                    definition: None,
                    value: Some(value.clone()),
                });
                Some(value)
            }
            Err(errors) => {
                self.out.errors.extend(errors);
                None
            }
        }
    }

    fn define(&mut self, field: Field<'_, '_>) {
        let key = field.key();
        let name = key.variable_name().unwrap_or(key.text());
        self.define_value(key, name, field.value());
    }

    /// Define the variable `name` with `value`. The name is at `key`. The
    /// right side reads the variables before the definition.
    fn define_value(
        &mut self,
        key: ScalarToken<'_, '_>,
        name: &[u8],
        value: Option<Value<'_, '_>>,
    ) {
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
            Some(Value::Calc(calc)) => self.calc(calc),
            Some(value) => {
                let range = value.syntax().text_range();
                let error = ResolveError::new(ResolveErrorKind::UnsupportedDefinition, range);
                self.out.errors.push(error);
                None
            }
            // The parser gives a syntax error for a definition with `=` and
            // no value, and `directive` records an error for a
            // `@:register_variable` with no `=`.
            None => None,
        };
        let index = self.out.definitions.len() as u32;
        let (name, replaces) = match self.latest.get_mut(name) {
            Some(latest) => {
                let previous = std::mem::replace(latest, index);
                let name = self.out.definitions[previous as usize].name.clone();
                (name, Replaces::Definition(previous))
            }
            None => {
                let replaces = if self.scope.contains(name) {
                    Replaces::Scope
                } else {
                    Replaces::Nothing
                };
                let name: Arc<[u8]> = name.into();
                self.latest.insert(name.clone(), index);
                (name, replaces)
            }
        };
        self.out.definitions.push(DefinitionData {
            name,
            key: key.text_range(),
            value,
            replaces,
        });
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

#[cfg(test)]
mod tests {
    use super::*;

    /// A 32-bit target, such as wasm32, has no padding for the index.
    #[test]
    #[cfg(target_pointer_width = "64")]
    fn a_use_records_its_definition_in_no_more_space() {
        let plain = std::mem::size_of::<(TokenId, ResolvedScalar)>();
        assert_eq!(std::mem::size_of::<Recorded<TokenId>>(), plain);
        assert_eq!(std::mem::size_of::<Recorded<NodeId>>(), plain);
    }
}
