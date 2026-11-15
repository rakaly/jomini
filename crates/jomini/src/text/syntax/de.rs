//! **Experimental** serde deserialization that reads the syntax tree.
//!
//! [`Parse::deserialize`] and [`Parse::document`] read the
//! [`SyntaxTree`] of a parse with the rules of
//! [`TextDeserializer`](crate::TextDeserializer), so a type that derives
//! [`Deserialize`] needs no change. Because the tree knows the position of
//! each value, an [`Error`] tells the line, the column, and the path of keys.
//!
//! This deserializer is for the game and mod files that a tool also checks or
//! changes, and for the syntax of game files that `TextDeserializer` does not
//! read, such as `@variables` and parameter blocks. To only deserialize a
//! save file, use `TextDeserializer`: it streams the file, so it is faster
//! and uses less memory.
//!
//! # Deserialize a file
//!
//! ```
//! use jomini::{Windows1252Encoding, text::syntax::parse};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Area {
//!     color: Vec<u8>,
//!     remainder: Vec<u32>,
//! }
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Areas {
//!     brittany_area: Area,
//! }
//!
//! let encoding = Windows1252Encoding::new();
//! let source = b"brittany_area = { color = { 118 99 151 } 169 170 171 }";
//! let areas: Areas = parse(source).deserialize(encoding)?;
//! assert_eq!(areas.brittany_area.color, vec![118, 99, 151]);
//! assert_eq!(areas.brittany_area.remainder, vec![169, 170, 171]);
//!
//! let source = b"brittany_area = {\n  color = blue\n}";
//! let error = parse(source).deserialize::<Areas, _>(encoding).unwrap_err();
//! assert_eq!(
//!     error.to_string(),
//!     r#"2:11: brittany_area.color[0]: invalid type: string "blue", expected u8"#
//! );
//! # Ok::<(), jomini::text::syntax::de::Error>(())
//! ```
//!
//! [`Parse::deserialize`] returns the first syntax error, except an extra
//! closing brace (`}`), which `TextTape` ignores too.
//!
//! # Read a file that has syntax errors
//!
//! The parser recovers from syntax errors, so an editor can read the data
//! before and after an error. [`Parse::document`] gives a [`Document`],
//! which reads a tree and does not examine its syntax errors. It fails only
//! when the data does not match the type. To find the syntax errors, read
//! [`Parse::errors`](super::Parse::errors), or call [`Document::check`],
//! which gives the first error as an [`Error`].
//!
//! # Read part of a file
//!
//! To read only some values, find them with the typed AST of the tree
//! ([`Root`](super::Root), [`Block`](super::Block), [`Field`](super::Field),
//! [`Value`](super::Value)). Then give each value to the [`Document`]. It
//! decodes the value or deserializes it into a serde type. Its errors also
//! tell the file name:
//!
//! ```
//! use jomini::{Utf8Encoding, text::syntax::parse};
//!
//! let parse = parse(b"state = { id = 1 provinces = { 3838 9851 } }");
//! let doc = parse.document(Utf8Encoding::new()).with_file("1-France.txt");
//! let state = doc.block(doc.require(doc.root(), b"state")?)?;
//!
//! let provinces: Vec<u32> = doc.deserialize_value(doc.require(state, b"provinces")?)?;
//! assert_eq!(provinces, [3838, 9851]);
//!
//! let error = doc.require(state, b"capital").unwrap_err();
//! assert_eq!(error.to_string(), "1-France.txt:1:9: state: missing key `capital`");
//! # Ok::<(), jomini::text::syntax::de::Error>(())
//! ```
//!
//! Code that holds only a [`SyntaxTree`] makes a document with
//! [`Document::new`].
//!
//! # Read variables as their values
//!
//! By default, the deserializer reads a `@name` variable or a `@[ ... ]`
//! calculation as its source text. To read their values, make a
//! [`Resolution`](super::resolve::Resolution) with
//! [`resolve`](super::resolve::resolve()), and pass it to
//! [`Document::with_resolution`] of the document of
//! [`Parse::document`]. The [`resolve`](mod@super::resolve) module has
//! examples.
//!
//! # Keep a document
//!
//! An [`OwnedBlock`](super::OwnedBlock) holds a whole document as data. It
//! owns its text, so you can keep it after the source is gone. It keeps the
//! quotes, the variables, the operators, the headers, duplicate keys, and
//! the order of the items. It does not keep the comments and the
//! whitespace. For them, keep the tree.
//!
//! A block deserializes as the root of a document. Thus it gives the same
//! result as the tree:
//!
//! ```
//! use jomini::{Utf8Encoding, text::syntax::{parse, OwnedBlock}};
//! use serde::Deserialize;
//!
//! #[derive(Deserialize, Debug, PartialEq)]
//! struct Areas {
//!     brittany: Vec<u32>,
//! }
//!
//! let parse = parse(b"brittany { 169 170 }");
//! let root = OwnedBlock::from_tree(parse.tree(), Utf8Encoding::new());
//! drop(parse);
//!
//! assert_eq!(Areas::deserialize(&root)?, Areas { brittany: vec![169, 170] });
//! # Ok::<(), jomini::text::syntax::de::Error>(())
//! ```
//!
//! # Keep what a serde type loses
//!
//! A plain serde type does not keep the quotes, the operator, the header
//! (`rgb` in `rgb { 1 2 3 }`), or the position of a value. These wrapper
//! types keep one of them:
//!
//! - [`MaybeQuoted`](crate::text::MaybeQuoted) keeps the quotes.
//! - [`Property`](crate::text::Property) keeps the operator.
//! - [`Headered`](crate::text::Headered) keeps the header.
//! - [`Spanned`] keeps the byte range. Only the tree knows the position of
//!   a value.
//!
//! The wrapper types also work with `TextDeserializer`. An
//! [`OwnedBlock`](super::OwnedBlock) keeps all of these, except the
//! position.
//!
//! In `#[serde(flatten)]` or `#[serde(untagged)]` containers, serde first
//! copies the data into its own model, which has no quotes and no headers.
//! Then `MaybeQuoted` and `Headered` return an error, and an owned value
//! is unquoted text with `=` operators.
//!
//! [`IgnoredAny`](serde::de::IgnoredAny) skips a value of any size in
//! constant time.
//!
//! # Rules
//!
//! The deserializer uses the rules of `TextDeserializer`:
//!
//! - Duplicate keys are separate map entries in source order. Use
//!   `#[jomini(duplicated)]` of [`JominiDeserialize`](crate::JominiDeserialize)
//!   to collect them.
//! - A container is an object when its first entry is a field with an
//!   operator. Else it is an array. A map over an object gives the fields up
//!   to the first bare value, then the key `remainder` with the rest.
//! - An empty block (`{}`) is an element of a sequence, and it is not a
//!   value in an object.
//! - A sequence over a block with fields and values (a mixed container)
//!   starts at the first bare value.
//! - A sequence over a headered block (`rgb { 1 2 3 }`) gives the header and
//!   then the block.
//! - A field with no operator (`key { ... }`) is a field.
//! - A tuple or an array reads the elements it needs, and the rest of the
//!   sequence has no effect. Thus a `[u8; 3]` reads `{ 1 2 3 255 }`, which
//!   `TextDeserializer` rejects. To reject more elements, read a `Vec`.
//! - A CK3 reader directive (`@:insert name`) is not an entry. In the value
//!   of a field, as in `x = @:load_variable name`, it is the text of the
//!   directive. The deserializer does not expand a directive.

/// Bind `$doc` to the model of the tree of `$ctx` and run `$body`. With a
/// resolution, the model gives the values of the resolution. The two models
/// are two types, so the hot path of each one tests a constant.
macro_rules! with_doc {
    ($ctx:expr, |$doc:ident| $body:expr) => {
        if $ctx.has_resolution() {
            let $doc = $crate::text::syntax::de::tree::TreeDoc::<_, true>::of($ctx);
            $body
        } else {
            let $doc = $crate::text::syntax::de::tree::TreeDoc::<_, false>::of($ctx);
            $body
        }
    };
}

/// Bind `$doc` to the model of the tree of `$ctx` without a resolution and
/// run `$body`. A caller that never has a resolution uses this, so it does
/// not compile the deserializer of the model with a resolution.
macro_rules! without_resolution {
    ($ctx:expr, |$doc:ident| $body:expr) => {{
        let $doc = $crate::text::syntax::de::tree::TreeDoc::<_, false>::of($ctx);
        $body
    }};
}

mod document;
mod engine;
mod error;
mod tree;
mod wrappers;

pub use self::document::Document;
pub use self::error::{Error, ErrorKind, PathSegment};
pub use self::wrappers::Spanned;

pub(crate) use self::engine::{Doc, DocDe, Item, Rest, VALUE, ValueDe, View};

use self::tree::{TreeCtx, TreeDoc};
use super::{OwnedParse, Parse, SyntaxErrorKind, SyntaxTree};
use crate::Encoding;
use serde::de::Deserialize;

impl<'a> Parse<'a> {
    /// Deserialize the document into `T` with the encoding of its scalars.
    /// The data of `T` can borrow from the source.
    ///
    /// The first syntax error is the error, except an extra closing brace
    /// (`}`), which `TextTape` ignores too. To read a document that has
    /// syntax errors, use [`document`](Parse::document).
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::syntax::parse};
    /// use std::collections::HashMap;
    ///
    /// let map: HashMap<&str, u32> = parse(b"a = 1 b = 2").deserialize(Utf8Encoding::new())?;
    /// assert_eq!(map["b"], 2);
    ///
    /// let error = parse(b"a = { b = 2").deserialize::<HashMap<&str, u32>, _>(Utf8Encoding::new());
    /// assert_eq!(error.unwrap_err().to_string(), "1:12: unclosed '{'");
    /// # Ok::<(), jomini::text::syntax::de::Error>(())
    /// ```
    pub fn deserialize<T, E>(&self, encoding: E) -> Result<T, Error>
    where
        T: Deserialize<'a>,
        E: Encoding,
    {
        check_errors(&self.tree())?;
        Document::new(self.tree(), encoding).deserialize_unresolved()
    }

    /// A [`Document`] of the tree with the encoding of its scalars: it
    /// deserializes the whole document or part of it, resolves `@variables`,
    /// and adds a file name to its errors. It does not examine the syntax
    /// errors of the tree.
    ///
    /// The parser recovers from syntax errors, so the data before and after
    /// an error is available:
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::syntax::parse};
    /// use serde::Deserialize;
    ///
    /// #[derive(Deserialize)]
    /// struct Modifier {
    ///     tax: u32,
    /// }
    ///
    /// #[derive(Deserialize)]
    /// struct Building {
    ///     cost: u32,
    ///     modifier: Modifier,
    /// }
    ///
    /// // The block has no closing brace.
    /// let parse = parse(b"cost = 100\nmodifier = { tax = 1");
    /// assert!(parse.deserialize::<Building, _>(Utf8Encoding::new()).is_err());
    ///
    /// let doc = parse.document(Utf8Encoding::new());
    /// let building: Building = doc.deserialize()?;
    /// assert_eq!((building.cost, building.modifier.tax), (100, 1));
    /// assert_eq!(doc.check().unwrap_err().to_string(), "2:21: unclosed '{'");
    /// # Ok::<(), jomini::text::syntax::de::Error>(())
    /// ```
    pub fn document<E: Encoding>(&self, encoding: E) -> Document<'_, 'a, E> {
        Document::new(self.tree(), encoding)
    }
}

impl<S: AsRef<[u8]>> OwnedParse<S> {
    /// Deserialize the document into `T`, which can borrow from the
    /// `OwnedParse`. See [`Parse::deserialize`].
    pub fn deserialize<'s, T, E>(&'s self, encoding: E) -> Result<T, Error>
    where
        T: Deserialize<'s>,
        E: Encoding,
    {
        check_errors(&self.tree())?;
        Document::new(self.tree(), encoding).deserialize_unresolved()
    }

    /// A [`Document`] of the tree. See [`Parse::document`].
    pub fn document<E: Encoding>(&self, encoding: E) -> Document<'_, '_, E> {
        Document::new(self.tree(), encoding)
    }
}

/// Read the root of a tree into an owned block.
pub(crate) fn tree_block<E: Encoding>(tree: SyntaxTree<'_, '_>, encoding: E) -> super::OwnedBlock {
    let ctx = TreeCtx::new(tree, encoding);
    let doc = TreeDoc::new(&ctx);
    super::value::ValueBuilder::new(doc).block(doc.root())
}

/// The owned value of the first field of `source`, as `x=<value>`, which
/// [`TextDeserializer`](crate::TextDeserializer) copies from its data.
pub(crate) fn field_value<E: Encoding>(source: &[u8], encoding: &E) -> super::OwnedValue {
    let parse = super::parse(source);
    let tree = parse.tree();
    let ctx = TreeCtx::new(tree, encoding);
    let doc = TreeDoc::new(&ctx);
    let value = tree.ast().fields().next().and_then(|field| field.value());
    match value.and_then(self::tree::value_handle) {
        Some(node) => super::value::ValueBuilder::new(doc).value(node),
        None => super::OwnedValue::Block(super::OwnedBlock::new()),
    }
}

/// Add the file name to an error that has none.
pub(crate) fn with_file(error: Error, file: Option<&str>) -> Error {
    match file {
        Some(file) if error.file().is_none() => error.with_file(file),
        _ => error,
    }
}

/// Return the first syntax error that `TextTape` does not ignore.
fn check_errors(tree: &SyntaxTree<'_, '_>) -> Result<(), Error> {
    let error = tree
        .errors()
        .iter()
        .find(|e| e.kind != SyntaxErrorKind::UnmatchedCloseBrace);
    match error {
        Some(error) => Err(Error::syntax(error).locate(tree.source())),
        None => Ok(()),
    }
}
