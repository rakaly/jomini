//! Keyed access to a document with located errors.

use super::engine::{Doc, Item, ValueDe, View};
use super::error::Error;
use super::tree::{TreeContainer, TreeCtx, TreeDoc, TreeNode, TreeScalar};
use crate::{
    Encoding,
    text::{
        Operator,
        lossless::{LineCol, LineIndex, SyntaxTree, TextRange, value::Value},
    },
};
use serde::de::{Deserialize, Error as _};
use std::{borrow::Cow, sync::OnceLock};

/// A syntax tree, its encoding, and its file name, for keyed access.
///
/// A [`Document`] gives the items of the tree without a serde type. Each
/// lookup that can fail returns an [`Error`] that tells the file, the line,
/// the column, and the path of keys. The scalars are decoded with the
/// encoding of the document, and they borrow from the source when they can.
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::{parse, de::Document}};
///
/// let source = b"state = {\n  id = 1\n  name = \"STATE_1\"\n  provinces = { 3838 9851 }\n}";
/// let tree = parse(&source[..]);
/// let doc = Document::new(tree.tree(), Utf8Encoding::new()).with_file("1-France.txt");
///
/// let state = doc.root().require("state")?.block()?;
/// let id: u32 = state.require("id")?.deserialize()?;
/// let name = state.get("name").map(|v| v.str()).transpose()?;
/// let provinces: Vec<u32> = state.require("provinces")?.deserialize()?;
/// assert_eq!((id, name.as_deref(), provinces.len()), (1, Some("STATE_1"), 2));
///
/// let error = state.require("capital").unwrap_err();
/// assert_eq!(error.to_string(), "1-France.txt:1:9: state: missing key `capital`");
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
pub struct Document<'t, 'a, E> {
    ctx: TreeCtx<'t, 'a, E>,
    file: Option<String>,
    lines: OnceLock<LineIndex>,
}

impl<'t, 'a, E: Encoding> Document<'t, 'a, E> {
    /// Make a document for `tree`. The encoding decodes the scalars.
    pub fn new(tree: SyntaxTree<'t, 'a>, encoding: E) -> Self {
        Document {
            ctx: TreeCtx::new(tree, encoding),
            file: None,
            lines: OnceLock::new(),
        }
    }

    /// Set the file name that the errors show.
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// The file name, when you give one.
    pub fn file(&self) -> Option<&str> {
        self.file.as_deref()
    }

    /// The tree of the document.
    pub fn tree(&self) -> SyntaxTree<'t, 'a> {
        self.ctx.tree()
    }

    /// The first syntax error of the tree, as an [`Error`]. An extra closing
    /// brace is not an error, as with `TextTape`.
    pub fn check(&self) -> Result<(), Error> {
        super::check_errors(&self.tree()).map_err(|e| self.finish(e))
    }

    /// The line and column of a byte offset. The first call makes the line
    /// index.
    pub fn line_col(&self, offset: u32) -> LineCol {
        self.lines
            .get_or_init(|| LineIndex::new(self.tree().source()))
            .line_col(offset)
    }

    /// The items at the top of the document.
    pub fn root(&self) -> BlockRef<'_, 'a, E> {
        let doc = self.doc();
        BlockRef {
            document: self.shorten(),
            container: doc.root(),
            node: None,
        }
    }

    /// Deserialize the whole document into `T`.
    pub fn deserialize<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        let doc = self.doc();
        T::deserialize(ValueDe::root(doc, doc.root())).map_err(|e| self.finish(e))
    }

    /// Read the whole document into an owned [`Value`].
    pub fn to_value(&self) -> Value {
        Value::from_tree(self.tree(), self.ctx.encoding())
    }

    fn doc(&self) -> TreeDoc<'_, 'a, E> {
        TreeDoc::new(self.shorten_ctx())
    }

    fn shorten_ctx(&self) -> &TreeCtx<'_, 'a, E> {
        &self.ctx
    }

    fn shorten(&self) -> &Document<'_, 'a, E> {
        self
    }

    #[cold]
    fn finish(&self, error: Error) -> Error {
        super::finish(error, self.tree().source(), self.file.as_deref())
    }
}

/// The items of the root, of a block, or of the block of a headered block.
pub struct BlockRef<'d, 'a, E> {
    document: &'d Document<'d, 'a, E>,
    container: TreeContainer,
    /// The value that holds the block. The root has none.
    node: Option<TreeNode>,
}

impl<E> Clone for BlockRef<'_, '_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for BlockRef<'_, '_, E> {}

/// A field of a [`BlockRef`]: a key, an operator, and a value.
pub struct FieldRef<'d, 'a, E> {
    document: &'d Document<'d, 'a, E>,
    key: TreeScalar,
    op: Option<Operator>,
    value: TreeNode,
}

impl<E> Clone for FieldRef<'_, '_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for FieldRef<'_, '_, E> {}

/// A value of a [`Document`]: a scalar, a block, a headered block, or a
/// calculation.
pub struct ValueRef<'d, 'a, E> {
    document: &'d Document<'d, 'a, E>,
    node: TreeNode,
}

impl<E> Clone for ValueRef<'_, '_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for ValueRef<'_, '_, E> {}

impl<'d, 'a, E: Encoding> BlockRef<'d, 'a, E> {
    fn doc(&self) -> TreeDoc<'d, 'a, E> {
        TreeDoc::new(&self.document.ctx)
    }

    /// The fields in source order. Bare values and parameter blocks are not
    /// fields.
    pub fn fields(&self) -> impl Iterator<Item = FieldRef<'d, 'a, E>> + use<'d, 'a, E> {
        let document = self.document;
        self.doc()
            .items(self.container)
            .filter_map(move |item| match item {
                Item::Field { key, op, value } => Some(FieldRef {
                    document,
                    key,
                    op,
                    value,
                }),
                _ => None,
            })
    }

    /// The bare values in source order, such as the elements of an array.
    pub fn values(&self) -> impl Iterator<Item = ValueRef<'d, 'a, E>> + use<'d, 'a, E> {
        let document = self.document;
        self.doc()
            .items(self.container)
            .filter_map(move |item| match item {
                Item::Value(node) => Some(ValueRef { document, node }),
                _ => None,
            })
    }

    /// The value of the first field with `key`.
    pub fn get(&self, key: &str) -> Option<ValueRef<'d, 'a, E>> {
        self.get_all(key).next()
    }

    /// The values of all of the fields with `key`, in source order.
    pub fn get_all<'k>(
        &self,
        key: &'k str,
    ) -> impl Iterator<Item = ValueRef<'d, 'a, E>> + use<'d, 'a, 'k, E> {
        self.fields()
            .filter(move |field| field.key_is(key))
            .map(|field| field.value())
    }

    /// The value of the first field with `key`, or an error that tells where
    /// the object is.
    pub fn require(&self, key: &str) -> Result<ValueRef<'d, 'a, E>, Error> {
        self.get(key)
            .ok_or_else(|| self.error(Error::custom(format_args!("missing key `{key}`"))))
    }

    /// The keys of the fields in source order, decoded.
    pub fn keys(&self) -> impl Iterator<Item = Cow<'a, str>> + use<'d, 'a, E> {
        self.fields().map(|field| field.key())
    }

    /// Deserialize the items into `T`, with the same rules as the
    /// deserializer of the tree.
    pub fn deserialize<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        let doc = self.doc();
        T::deserialize(ValueDe::root(doc, self.container)).map_err(|e| self.error(e))
    }

    /// The byte range of the object in the source.
    pub fn range(&self) -> TextRange {
        self.doc()
            .container_range(self.container)
            .unwrap_or_default()
    }

    #[cold]
    fn error(&self, error: Error) -> Error {
        let doc = self.doc();
        let error = error.or_range(Some(self.range()));
        let error = match self.node {
            Some(node) => error.in_keys(doc.keys_of(node)),
            None => error,
        };
        self.document.finish(error)
    }
}

impl<'d, 'a, E: Encoding> FieldRef<'d, 'a, E> {
    fn doc(&self) -> TreeDoc<'d, 'a, E> {
        TreeDoc::new(&self.document.ctx)
    }

    /// The decoded key.
    pub fn key(&self) -> Cow<'a, str> {
        self.document
            .ctx
            .encoding()
            .decode(self.doc().bytes(self.key))
    }

    /// Return `true` when the decoded key is `key`.
    pub fn key_is(&self, key: &str) -> bool {
        let bytes = self.doc().bytes(self.key);
        bytes == key.as_bytes() || (!bytes.is_ascii() && self.key() == key)
    }

    /// The operator, or `None` for a `key { ... }` field that has no
    /// operator.
    pub fn op(&self) -> Option<Operator> {
        self.op
    }

    /// The value.
    pub fn value(&self) -> ValueRef<'d, 'a, E> {
        ValueRef {
            document: self.document,
            node: self.value,
        }
    }
}

impl<'d, 'a, E: Encoding> ValueRef<'d, 'a, E> {
    fn doc(&self) -> TreeDoc<'d, 'a, E> {
        TreeDoc::new(&self.document.ctx)
    }

    /// The decoded text of a scalar, or an error for a block.
    pub fn str(&self) -> Result<Cow<'a, str>, Error> {
        let doc = self.doc();
        match doc.view(self.node) {
            View::Scalar(scalar) | View::Calc(scalar) => {
                Ok(self.document.ctx.encoding().decode(doc.bytes(scalar)))
            }
            _ => Err(self.error(Error::custom("expected a scalar, found a block"))),
        }
    }

    /// Return `true` for a scalar that has quotes in the source.
    pub fn is_quoted(&self) -> bool {
        match self.doc().view(self.node) {
            View::Scalar(scalar) => self.doc().quoted(scalar),
            _ => false,
        }
    }

    /// The header of a headered block, such as `rgb` in `rgb { 1 2 3 }`.
    pub fn header(&self) -> Option<Cow<'a, str>> {
        match self.doc().view(self.node) {
            View::Headered(header, _) => Some(
                self.document
                    .ctx
                    .encoding()
                    .decode(self.doc().bytes(header)),
            ),
            _ => None,
        }
    }

    /// The items of a block, or of the block of a headered block.
    pub fn block(&self) -> Result<BlockRef<'d, 'a, E>, Error> {
        match self.doc().view(self.node) {
            View::Block(container) | View::Headered(_, container) => Ok(BlockRef {
                document: self.document,
                container,
                node: Some(self.node),
            }),
            _ => Err(self.error(Error::custom("expected a block, found a scalar"))),
        }
    }

    /// Deserialize the value into `T`, such as a number, a `Vec`, or a
    /// struct. The rules are the same as those of the deserializer of the
    /// tree.
    pub fn deserialize<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        T::deserialize(ValueDe::node(self.doc(), self.node)).map_err(|e| self.error(e))
    }

    /// Copy the value into an owned [`Value`].
    pub fn to_value(&self) -> Value {
        self.deserialize::<Value>()
            .expect("a tree always gives a value")
    }

    /// The byte range of the value in the source.
    pub fn range(&self) -> TextRange {
        self.doc().node_range(self.node).unwrap_or_default()
    }

    /// The line and column of the start of the value.
    pub fn line_col(&self) -> LineCol {
        self.document.line_col(self.range().start())
    }

    #[cold]
    fn error(&self, error: Error) -> Error {
        let doc = self.doc();
        let error = error
            .or_range(Some(self.range()))
            .in_keys(doc.keys_of(self.node));
        self.document.finish(error)
    }
}

impl<E: Encoding> std::fmt::Debug for BlockRef<'_, '_, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BlockRef")
            .field("range", &self.range())
            .finish()
    }
}

impl<E: Encoding> std::fmt::Debug for FieldRef<'_, '_, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FieldRef")
            .field("key", &self.key())
            .field("op", &self.op)
            .field("value", &self.value())
            .finish()
    }
}

impl<E: Encoding> std::fmt::Debug for ValueRef<'_, '_, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let source = self.document.tree().source();
        let text = String::from_utf8_lossy(&source[self.range().to_range()]);
        f.debug_struct("ValueRef")
            .field("range", &self.range())
            .field("text", &text)
            .finish()
    }
}
