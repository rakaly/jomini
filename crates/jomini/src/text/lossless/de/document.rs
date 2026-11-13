//! Serde reads and located errors for the typed AST.

use super::engine::{Doc, ValueDe, View};
use super::error::Error;
use super::tree::{ArrayIndex, TreeCtx, TreeDoc, TreeNode, value_handle};
use crate::{
    Encoding,
    text::lossless::{
        AstNode, Block, Field, LineCol, LineIndex, NodeId, OwnedValue, Root, SyntaxElement,
        SyntaxKind, SyntaxTree, TextSize, Value, resolve::Resolution, value::ValueBuilder,
    },
};
use serde::de::{self, Deserialize, DeserializeSeed, Error as _, Visitor};
use std::{
    borrow::Cow,
    collections::HashMap,
    marker::PhantomData,
    sync::{Arc, Mutex, OnceLock, PoisonError},
};

/// A syntax tree, its encoding, and its file name.
///
/// `&Document` is the serde deserializer of the whole tree, and
/// [`deserialize`](Document::deserialize) deserializes the tree into a type.
/// The document also reads the parts of the tree. Each method that takes an
/// item of the typed AST returns an error when the item is from a different
/// tree.
///
/// Use the typed AST ([`Root`], [`Block`], [`Field`], [`Value`]) to find
/// the items of the tree. A [`Document`] adds the operations that need the
/// encoding or that can fail: it decodes a value, deserializes a value into
/// a serde type, and finds a key that must be present. Each error tells the
/// file, the line, the column, and the path of keys. The decoded text
/// borrows from the source when it can.
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::{parse, de::Document}};
///
/// let source = b"state = {\n  id = 1\n  name = \"STATE_1\"\n  provinces = { 3838 9851 }\n}";
/// let tree = parse(&source[..]);
/// let doc = Document::new(tree.tree(), Utf8Encoding::new()).with_file("1-France.txt");
///
/// let state = doc.block(doc.require(doc.root(), b"state")?)?;
/// let id: u32 = doc.deserialize_value(doc.require(state, b"id")?)?;
/// let name = state.get(b"name").and_then(|f| f.value()).map(|v| doc.str(v)).transpose()?;
/// let provinces: Vec<u32> = doc.deserialize_value(doc.require(state, b"provinces")?)?;
/// assert_eq!((id, name.as_deref(), provinces.len()), (1, Some("STATE_1"), 2));
///
/// let error = doc.require(state, b"capital").unwrap_err();
/// assert_eq!(error.to_string(), "1-France.txt:1:9: state: missing key `capital`");
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
///
/// A tool can read the parts of a field that a serde type does not model,
/// such as the operator, the quotes, and the header:
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::{parse, Value, de::Document}};
///
/// let tree = parse(b"trigger = { num_of_cities >= 5 is_at_war = no }\ncolor = rgb { 10 20 30 }");
/// let doc = Document::new(tree.tree(), Utf8Encoding::new());
/// let trigger = doc.block(doc.require(doc.root(), b"trigger")?)?;
///
/// let conditions: Vec<String> = trigger
///     .fields()
///     .map(|f| {
///         let key = f.key().decode(doc.encoding());
///         let op = f.op().map_or("", |op| op.symbol());
///         Ok(format!("{key} {op} {}", doc.str(doc.value(f)?)?))
///     })
///     .collect::<Result<_, jomini::text::lossless::de::Error>>()?;
/// assert_eq!(conditions, ["num_of_cities >= 5", "is_at_war = no"]);
///
/// let color = doc.require(doc.root(), b"color")?;
/// let header = color.as_headered_block().unwrap().header();
/// assert_eq!(header.decode(doc.encoding()), "rgb");
/// let rgb = Value::Block(doc.block(color)?);
/// assert_eq!(doc.deserialize_value::<Vec<u8>>(rgb)?, [10, 20, 30]);
/// assert_eq!(doc.line_col(color.text_range().start()).to_string(), "2:9");
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
pub struct Document<'t, 'a, E> {
    ctx: TreeCtx<'t, 'a, E>,
    file: Option<String>,
    lines: OnceLock<LineIndex>,
    /// The index of the elements of each array that an error path goes
    /// through, so that the path of each error costs only its ancestors.
    arrays: Mutex<HashMap<NodeId, Option<ArrayIndex>>>,
}

impl<'t, 'a, E: Encoding> Document<'t, 'a, E> {
    /// Make a document for `tree`. The encoding decodes the scalars.
    pub fn new(tree: SyntaxTree<'t, 'a>, encoding: E) -> Self {
        Document {
            ctx: TreeCtx::new(tree, encoding),
            file: None,
            lines: OnceLock::new(),
            arrays: Mutex::new(HashMap::new()),
        }
    }

    /// Make the document read each variable and each calculation as its
    /// value from `resolution`. See
    /// [`resolve`](crate::text::lossless::resolve).
    ///
    /// `resolution` can be a [`Resolution`] or an `Arc<Resolution>`, so
    /// several documents can share one resolution.
    ///
    /// The document skips each variable definition, such as `@size = 0.5`:
    /// a definition is not an entry of its block or of the root. These reads
    /// use the resolution, also when the target is an [`OwnedValue`]:
    ///
    /// - [`deserialize`](Document::deserialize)
    /// - [`deserialize_value`](Document::deserialize_value) and
    ///   [`deserialize_value_seed`](Document::deserialize_value_seed)
    /// - [`deserialize_field`](Document::deserialize_field) and
    ///   [`deserialize_field_seed`](Document::deserialize_field_seed)
    /// - [`str`](Document::str)
    ///
    /// Only [`to_owned_value`](Document::to_owned_value) ignores the
    /// resolution and copies the source text.
    ///
    /// A variable or a calculation that has no value reads as its source
    /// text. See [Find variables that have no
    /// value](crate::text::lossless::resolve#find-variables-that-have-no-value).
    ///
    /// A resolved value is not a part of the source, so a target that
    /// borrows from the source, such as `&'a str`, cannot take it. Use
    /// `String` or `Cow<str>`:
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{parse, de::Document, resolve::resolve}};
    ///
    /// let parse = parse(b"@name = \"Jean II\"\nruler = @name");
    /// let doc = Document::new(parse.tree(), Utf8Encoding::new())
    ///     .with_resolution(resolve(parse.tree()))?;
    /// let ruler = doc.require(doc.root(), b"ruler")?;
    /// assert_eq!(doc.deserialize_value::<String>(ruler)?, "Jean II");
    ///
    /// let error = doc.deserialize_value::<&str>(ruler).unwrap_err();
    /// assert_eq!(
    ///     error.to_string(),
    ///     r#"2:9: ruler: invalid type: string "Jean II", expected a borrowed string"#
    /// );
    /// # Ok::<(), jomini::text::lossless::de::Error>(())
    /// ```
    ///
    /// `with_resolution` returns the same `Document` type, so code that has a
    /// resolution for only some files needs one code path:
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{parse, de::Document, resolve::resolve}};
    /// use std::collections::HashMap;
    ///
    /// let parse = parse(b"@size = 2\nwidth = @size");
    /// for (resolution, width) in [(None, "@size"), (Some(resolve(parse.tree())), "2")] {
    ///     let mut doc = Document::new(parse.tree(), Utf8Encoding::new());
    ///     if let Some(resolution) = resolution {
    ///         doc = doc.with_resolution(resolution)?;
    ///     }
    ///     let map: HashMap<String, String> = doc.deserialize()?;
    ///     assert_eq!(map["width"], width);
    /// }
    /// # Ok::<(), jomini::text::lossless::de::Error>(())
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an error when the resolution is not for the tree of the
    /// document. See [`Resolution::is_for`].
    pub fn with_resolution(
        mut self,
        resolution: impl Into<Arc<Resolution>>,
    ) -> Result<Self, Error> {
        let resolution = resolution.into();
        if !resolution.is_for(self.tree()) {
            let error = Error::custom("the resolution is of a different tree");
            return Err(self.finish(error));
        }
        self.ctx.set_resolution(resolution);
        // A resolution changes the entries of a container: a variable
        // definition is not an entry.
        self.arrays
            .get_mut()
            .unwrap_or_else(PoisonError::into_inner)
            .clear();
        Ok(self)
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

    /// The root of the typed AST.
    pub fn root(&self) -> Root<'t, 'a> {
        self.ctx.tree().ast()
    }

    /// The encoding of the document. Give it to
    /// [`ScalarToken::decode`](crate::text::lossless::ScalarToken::decode)
    /// to decode a key or a header.
    pub fn encoding(&self) -> &E {
        self.ctx.encoding()
    }

    /// The first syntax error of the tree, as an [`Error`]. An extra closing
    /// brace is not an error, as with `TextTape`.
    pub fn check(&self) -> Result<(), Error> {
        super::check_errors(&self.tree()).map_err(|e| self.finish(e))
    }

    /// The line and column of a byte offset. The first call makes the line
    /// index.
    pub fn line_col(&self, offset: TextSize) -> LineCol {
        self.lines
            .get_or_init(|| LineIndex::new(self.tree().source()))
            .line_col(offset)
    }

    /// Deserialize the whole document into `T`. Some errors have no position
    /// of their own, such as a missing field of the root or an error of a
    /// `#[serde(try_from)]` conversion. These errors get the position of the
    /// root.
    ///
    /// ```
    /// use jomini::{Windows1252Encoding, text::lossless::{parse, de::Document}};
    /// use serde::Deserialize;
    ///
    /// #[derive(Deserialize, Debug)]
    /// struct Meta {
    ///     date: String,
    ///     version: u32,
    /// }
    ///
    /// let tree = parse(b"date = 1444.11.11");
    /// let doc = Document::new(tree.tree(), Windows1252Encoding::new()).with_file("meta.txt");
    /// let error = doc.deserialize::<Meta>().unwrap_err();
    /// assert_eq!(error.to_string(), "meta.txt:1:1: missing field `version`");
    /// ```
    pub fn deserialize<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        // A conversion such as `#[serde(try_from)]` makes its error after
        // the deserializer returns, so finish the error here too.
        T::deserialize(self).map_err(|e| self.finish_root(e))
    }

    /// Deserialize the whole document into `T`, as
    /// [`deserialize`](Document::deserialize) does, for a document that has
    /// no resolution.
    pub(super) fn deserialize_unresolved<T: Deserialize<'a>>(&self) -> Result<T, Error> {
        debug_assert!(!self.ctx.has_resolution());
        T::deserialize(Unresolved(self)).map_err(|e| self.finish_root(e))
    }

    /// Finish an error of the root. An error with no position of its own
    /// gets the position of the root.
    #[cold]
    fn finish_root(&self, error: Error) -> Error {
        let root = Some(self.tree().root().text_range());
        self.finish(error.or_range(root))
    }

    /// The value of the first field of `parent` that has the key `key` and
    /// a value. `parent` is usually a [`Root`] or a [`Block`].
    ///
    /// The comparison uses the bytes of the key without the quotes, as
    /// [`Block::get`] does. When no field matches, the error tells where
    /// `parent` is.
    pub fn require<N: AstNode<'t, 'a>>(
        &self,
        parent: N,
        key: &[u8],
    ) -> Result<Value<'t, 'a>, Error> {
        let node = parent.syntax();
        self.check_tree(SyntaxElement::Node(node))?;
        node.significant_children()
            .filter_map(|el| el.as_node().and_then(Field::cast))
            .filter(|field| field.key_is(key))
            .find_map(|field| field.value())
            .ok_or_else(|| {
                let key = self.ctx.encoding().decode(key);
                let error = Error::custom(format_args!("missing key `{key}`"));
                self.error(TreeNode::Node(node.id()), error)
            })
    }

    /// The value of `field`, or an error that tells where the field is when
    /// it has no value, as in `a =` at the end of a file.
    pub fn value(&self, field: Field<'t, 'a>) -> Result<Value<'t, 'a>, Error> {
        self.check_tree(SyntaxElement::Node(field.syntax()))?;
        field.value().ok_or_else(|| {
            let error = Error::custom("expected a value, found none");
            self.error(TreeNode::Node(field.syntax().id()), error)
        })
    }

    /// The block of a block, or the block of a headered block, such as
    /// `{ 1 2 3 }` in `rgb { 1 2 3 }`. Else an error.
    pub fn block(&self, value: Value<'t, 'a>) -> Result<Block<'t, 'a>, Error> {
        let node = self.handle(value)?;
        match value {
            Value::Block(block) => Ok(block),
            Value::HeaderedBlock(headered) => Ok(headered.block()),
            _ => Err(self.error(node, Error::custom("expected a block, found a scalar"))),
        }
    }

    /// The decoded text of a scalar, an operator, a calculation, or an
    /// interpolation. For a block, it returns an error.
    ///
    /// With a [resolution](Document::with_resolution), a variable or a
    /// calculation that has a value always gives a copy of the value
    /// ([`Cow::Owned`]). Other text borrows from the source
    /// ([`Cow::Borrowed`]) when decoding does not change it.
    pub fn str(&self, value: Value<'t, 'a>) -> Result<Cow<'a, str>, Error> {
        let node = self.handle(value)?;
        with_doc!(&self.ctx, |doc| match doc.view(node) {
            View::Scalar(scalar) | View::Calc(scalar) => Ok(match doc.resolved(scalar) {
                Some(value) => Cow::Owned(value.decode(self.ctx.encoding()).into_owned()),
                None => doc.decode(scalar),
            }),
            _ => Err(self.error(node, Error::custom("expected a scalar, found a block"))),
        })
    }

    /// Deserialize `value` into `T`, such as a number, a `Vec`, or a
    /// struct. The rules are the same as those of the deserializer of the
    /// tree. A headered block is a sequence of its header and its block.
    pub fn deserialize_value<T: Deserialize<'a>>(&self, value: Value<'t, 'a>) -> Result<T, Error> {
        self.deserialize_value_seed(value, PhantomData)
    }

    /// Deserialize `value` with a [`DeserializeSeed`], with the rules of
    /// [`deserialize_value`](Document::deserialize_value). A seed can use
    /// state of the caller, such as a collection to add to, or a table that
    /// gives an identifier for a name.
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{parse, de::Document}};
    /// use serde::de::{Deserialize, DeserializeSeed, Deserializer};
    ///
    /// /// Add the numbers of a sequence to a vector.
    /// struct Extend<'v>(&'v mut Vec<u32>);
    ///
    /// impl<'de> DeserializeSeed<'de> for Extend<'_> {
    ///     type Value = ();
    ///     fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<(), D::Error> {
    ///         self.0.extend(Vec::<u32>::deserialize(d)?);
    ///         Ok(())
    ///     }
    /// }
    ///
    /// let tree = parse(b"a = { 1 2 } b = { 3 }");
    /// let doc = Document::new(tree.tree(), Utf8Encoding::new());
    /// let mut all = Vec::new();
    /// for key in [b"a", b"b"] {
    ///     doc.deserialize_value_seed(doc.require(doc.root(), key)?, Extend(&mut all))?;
    /// }
    /// assert_eq!(all, [1, 2, 3]);
    /// # Ok::<(), jomini::text::lossless::de::Error>(())
    /// ```
    pub fn deserialize_value_seed<S: DeserializeSeed<'a>>(
        &self,
        value: Value<'t, 'a>,
        seed: S,
    ) -> Result<S::Value, Error> {
        let node = self.handle(value)?;
        with_doc!(&self.ctx, |doc| {
            seed.deserialize(ValueDe::node(doc, node))
                .map_err(|e| self.error(node, e))
        })
    }

    /// Deserialize the value of `field` into `T`, with the operator of the
    /// field, as a field of a derived struct is. Thus a
    /// [`Property`](crate::text::Property) gets the operator, which
    /// [`deserialize_value`](Document::deserialize_value) does not know.
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::{Operator, Property, lossless::{parse, de::Document}}};
    ///
    /// let tree = parse(b"age >= 16");
    /// let doc = Document::new(tree.tree(), Utf8Encoding::new());
    /// let field = doc.root().get(b"age").unwrap();
    /// let age: Property<u32> = doc.deserialize_field(field)?;
    /// assert_eq!(age, Property::new(Operator::GreaterThanEqual, 16));
    /// # Ok::<(), jomini::text::lossless::de::Error>(())
    /// ```
    pub fn deserialize_field<T: Deserialize<'a>>(&self, field: Field<'t, 'a>) -> Result<T, Error> {
        self.deserialize_field_seed(field, PhantomData)
    }

    /// Deserialize the value of `field` with a [`DeserializeSeed`], with the
    /// rules of [`deserialize_field`](Document::deserialize_field).
    pub fn deserialize_field_seed<S: DeserializeSeed<'a>>(
        &self,
        field: Field<'t, 'a>,
        seed: S,
    ) -> Result<S::Value, Error> {
        let node = self.handle(self.value(field)?)?;
        with_doc!(&self.ctx, |doc| {
            seed.deserialize(ValueDe::field(doc, node, field.op()))
                .map_err(|e| self.error(node, e))
        })
    }

    /// Copy `value` into an [`OwnedValue`]. The copy keeps the source text,
    /// also when the document has a resolution: a variable stays a
    /// variable, and a variable definition stays an entry. To get the
    /// resolved values, deserialize the value into an `OwnedValue` with
    /// [`deserialize_value`](Document::deserialize_value).
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{parse, OwnedValue, de::Document, resolve::resolve}};
    ///
    /// let tree = parse(b"@x = 1\nm = { @x }");
    /// let doc = Document::new(tree.tree(), Utf8Encoding::new())
    ///     .with_resolution(resolve(tree.tree()))?;
    /// let m = doc.require(doc.root(), b"m")?;
    /// let source = doc.to_owned_value(m)?;
    /// let resolved: OwnedValue = doc.deserialize_value(m)?;
    /// assert_eq!(source.as_block().unwrap().values().next().unwrap().as_str(), Some("@x"));
    /// assert_eq!(resolved.as_block().unwrap().values().next().unwrap().as_str(), Some("1"));
    /// # Ok::<(), jomini::text::lossless::de::Error>(())
    /// ```
    pub fn to_owned_value(&self, value: Value<'t, 'a>) -> Result<OwnedValue, Error> {
        let node = self.handle(value)?;
        Ok(ValueBuilder::new(self.doc()).value(node))
    }

    fn doc(&self) -> TreeDoc<'_, 'a, E> {
        TreeDoc::new(&self.ctx)
    }

    /// The handle of `value` in the tree of the document. A caller can make
    /// a [`Value::Operator`] from each token, so its token must be a
    /// significant operator.
    fn handle(&self, value: Value<'t, 'a>) -> Result<TreeNode, Error> {
        self.check_tree(value.syntax())?;
        if let Value::Operator(token) = value
            && token.kind() != SyntaxKind::Operator
        {
            let error = Error::custom(format_args!(
                "expected an operator, found {:?}",
                token.kind()
            ));
            return Err(self.finish(error.or_range(Some(token.text_range()))));
        }
        value_handle(value)
            .ok_or_else(|| self.finish(Error::custom("the value has no identity in the tree")))
    }

    /// Return an error when `element` is from a different tree. The handle
    /// of such an element is an index into the different tree.
    fn check_tree(&self, element: SyntaxElement<'t, 'a>) -> Result<(), Error> {
        let tree = match element {
            SyntaxElement::Node(node) => node.tree,
            SyntaxElement::Token(token) => token.tree,
        };
        let ours = self.tree();
        if std::ptr::eq(tree.data, ours.data) && std::ptr::eq(tree.source, ours.source) {
            Ok(())
        } else {
            Err(self.finish(Error::custom("the item is from a different tree")))
        }
    }

    /// Add the range of `node`, the path of keys to `node`, the line, the
    /// column, and the file name to an error.
    #[cold]
    fn error(&self, node: TreeNode, error: Error) -> Error {
        let doc = self.doc();
        let error = error
            .or_range(doc.node_range(node))
            .in_path(doc.path_of(node, |block, element| self.array_index(block, element)));
        self.finish(error)
    }

    /// Add the line, the column, and the file name to an error. The line
    /// index of the document is made one time for all of the errors.
    /// The index of `element` in the array `block`, from the cache.
    fn array_index(&self, block: Block<'t, 'a>, element: SyntaxElement<'t, 'a>) -> Option<usize> {
        let mut arrays = self.arrays.lock().unwrap_or_else(PoisonError::into_inner);
        let array = arrays
            .entry(block.syntax().id())
            .or_insert_with(|| with_doc!(&self.ctx, |doc| ArrayIndex::new(doc, block)));
        array.as_ref()?.get(element)
    }

    #[cold]
    fn finish(&self, error: Error) -> Error {
        let error = error.locate_with(|offset| self.line_col(offset));
        super::with_file(error, self.file.as_deref())
    }
}

/// The methods of a deserializer of the root of the document
/// `$document`, which bind the model with the macro `$model`.
macro_rules! forward_root {
    (|$this:ident| $document:expr, $model:ident) => {
        forward_root! {
            @methods [$this, $document] $model
            deserialize_any()
            deserialize_bool()
            deserialize_i8()
            deserialize_i16()
            deserialize_i32()
            deserialize_i64()
            deserialize_i128()
            deserialize_u8()
            deserialize_u16()
            deserialize_u32()
            deserialize_u64()
            deserialize_u128()
            deserialize_f32()
            deserialize_f64()
            deserialize_char()
            deserialize_str()
            deserialize_string()
            deserialize_bytes()
            deserialize_byte_buf()
            deserialize_option()
            deserialize_unit()
            deserialize_unit_struct(name: &'static str)
            deserialize_newtype_struct(name: &'static str)
            deserialize_seq()
            deserialize_tuple(len: usize)
            deserialize_tuple_struct(name: &'static str, len: usize)
            deserialize_map()
            deserialize_struct(name: &'static str, fields: &'static [&'static str])
            deserialize_enum(name: &'static str, variants: &'static [&'static str])
            deserialize_identifier()
            deserialize_ignored_any()
        }
    };
    (@methods [$this:ident, $document:expr] $model:ident
        $($method:ident ( $($arg:ident : $ty:ty),* ))*) => {$(
        fn $method<V: Visitor<'a>>(self, $($arg: $ty,)* visitor: V) -> Result<V::Value, Error> {
            let $this = self;
            let document: &Document<'_, 'a, E> = $document;
            $model!(&document.ctx, |doc| {
                let root = ValueDe::root(doc, doc.root());
                de::Deserializer::$method(root, $($arg,)* visitor)
                    .map_err(|e| document.finish_root(e))
            })
        }
    )*};
}

/// Deserialize the whole tree. The data of the type can borrow from the
/// source.
impl<'a, E: Encoding> de::Deserializer<'a> for &Document<'_, 'a, E> {
    type Error = Error;

    forward_root!(|this| this, with_doc);
}

/// The deserializer of the whole tree of a document that has no
/// resolution. It does not compile the model with a resolution.
struct Unresolved<'d, 't, 'a, E>(&'d Document<'t, 'a, E>);

impl<'a, E: Encoding> de::Deserializer<'a> for Unresolved<'_, '_, 'a, E> {
    type Error = Error;

    forward_root!(|this| this.0, without_resolution);
}
