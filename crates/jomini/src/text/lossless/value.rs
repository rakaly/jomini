//! **Experimental** owned value that keeps all of a document.
//!
//! A [`Value`] is like `serde_json::Value`, but for the Clausewitz format.
//! It keeps the data that a serde data model does not: quotes, operators,
//! headers, duplicate keys, the order of the items, and containers that
//! have both fields and bare values. It owns its data (`Value: 'static`),
//! so it can live longer than the source.
//!
//! You can make a value in two ways:
//!
//! - [`Value::from_tree`] reads a [`SyntaxTree`].
//! - `Value` implements [`Deserialize`](serde::Deserialize). The tree
//!   deserializer gives all of the data. A different deserializer gives
//!   less: no quotes, no operators, and no headers.
//!
//! A value is also a serde deserializer, so you can deserialize it into a
//! type later.
//!
//! ```
//! use jomini::text::{Operator, lossless::{de::from_utf8_slice, value::Value}};
//! use serde::Deserialize;
//!
//! let source = br#"
//!     unquoted = 1234
//!     quoted = "567"
//!     operator > 0.5
//!     color = hsv { 0.3 0.4 0.5 }
//!     sequence = { 1 2 3 4 }
//!     map = { a = 1 b = 2 }
//! "#;
//! let root: Value = from_utf8_slice(source)?;
//! let root = root.as_block().unwrap();
//!
//! let unquoted = root.get("unquoted").unwrap();
//! assert_eq!(f32::deserialize(unquoted)?, 1234.0);
//! assert!(root.get("quoted").unwrap().is_quoted());
//!
//! let field = root.fields().find(|f| f.key == "operator").unwrap();
//! assert_eq!(field.op, Some(Operator::GreaterThan));
//!
//! let (header, body) = root.get("color").unwrap().as_headered_block().unwrap();
//! assert_eq!(header, "hsv");
//! assert_eq!(body.len(), 3);
//!
//! assert!(root.get("sequence").unwrap().as_block().unwrap().is_array());
//! assert_eq!(root.get("map").unwrap().get("b").unwrap().to_i64(), Some(2));
//! # Ok::<(), jomini::text::lossless::de::Error>(())
//! ```

mod de;

use super::SyntaxTree;
use super::de::{Doc, Item as DocItem, View};
use crate::{Encoding, Scalar, text::Operator};

/// An owned value of a document.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Value {
    /// A scalar with no quotes, such as `yes`, `1.5`, `1444.11.11`, or
    /// `@variable`. An interpolation (`[Root.GetName]`) and a code payload
    /// are also unquoted scalars that hold their source text.
    Unquoted(String),
    /// A scalar that had quotes. The text has no quotes and no escape
    /// characters.
    Quoted(String),
    /// A `@[ ... ]` calculation. The text is the source text, `@[` and `]`
    /// included.
    Calc(String),
    /// A `{ ... }` block: an object, an array, a mixed container, or an
    /// empty block. The root of a document is also a block.
    Block(Block),
    /// A header and its block, such as `rgb { 1 2 3 }`.
    HeaderedBlock(String, Block),
}

/// The items of a block in source order.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Block {
    items: Vec<Item>,
}

/// An item of a [`Block`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Item {
    /// A `key <op> value` field.
    Field(Field),
    /// A bare value, such as an element of an array.
    Value(Value),
    /// A conditional parameter block, such as `[[name] ... ]` in EU4.
    Parameter(Parameter),
}

/// A `key <op> value` field.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Field {
    /// The key. The text has no quotes.
    pub key: String,
    /// The key had quotes.
    pub quoted_key: bool,
    /// The operator, or `None` for a `key { ... }` field that has no
    /// operator.
    pub op: Option<Operator>,
    /// The value.
    pub value: Value,
}

/// A conditional parameter block, `[[name] ... ]` or `[[!name] ... ]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct Parameter {
    /// The name of the parameter.
    pub name: String,
    /// The block applies when the parameter is not defined (`[[!name]`).
    pub undefined: bool,
    /// The items of the block.
    pub body: Block,
}

impl Value {
    /// Read a whole tree into a block. The encoding decodes the scalars.
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{parse, value::Value}};
    ///
    /// let tree = parse(b"a = 1 b > 2");
    /// let value = Value::from_tree(tree.tree(), Utf8Encoding::new());
    /// assert_eq!(value.get("b").unwrap().as_str(), Some("2"));
    /// ```
    pub fn from_tree<E: Encoding>(tree: SyntaxTree<'_, '_>, encoding: E) -> Value {
        super::de::tree_value(tree, encoding)
    }

    /// Make an unquoted scalar.
    pub fn unquoted(text: impl Into<String>) -> Value {
        Value::Unquoted(text.into())
    }

    /// Make a quoted scalar.
    pub fn quoted(text: impl Into<String>) -> Value {
        Value::Quoted(text.into())
    }

    /// The text of a scalar, quoted or unquoted.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::Unquoted(s) | Value::Quoted(s) => Some(s),
            _ => None,
        }
    }

    /// Return `true` for a scalar.
    pub fn is_scalar(&self) -> bool {
        matches!(self, Value::Unquoted(_) | Value::Quoted(_))
    }

    /// Return `true` for a scalar that had quotes.
    pub fn is_quoted(&self) -> bool {
        matches!(self, Value::Quoted(_))
    }

    /// The block of a [`Value::Block`].
    pub fn as_block(&self) -> Option<&Block> {
        match self {
            Value::Block(b) => Some(b),
            _ => None,
        }
    }

    /// The block of a [`Value::Block`], mutable.
    pub fn as_block_mut(&mut self) -> Option<&mut Block> {
        match self {
            Value::Block(b) => Some(b),
            _ => None,
        }
    }

    /// The header and the block of a [`Value::HeaderedBlock`].
    pub fn as_headered_block(&self) -> Option<(&str, &Block)> {
        match self {
            Value::HeaderedBlock(h, b) => Some((h, b)),
            _ => None,
        }
    }

    /// The header of a [`Value::HeaderedBlock`].
    pub fn header(&self) -> Option<&str> {
        self.as_headered_block().map(|(h, _)| h)
    }

    /// The source text of a [`Value::Calc`].
    pub fn as_calc(&self) -> Option<&str> {
        match self {
            Value::Calc(s) => Some(s),
            _ => None,
        }
    }

    /// The first value of `key` in a block. See [`Block::get`].
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.as_block()?.get(key)
    }

    fn scalar(&self) -> Option<Scalar<'_>> {
        self.as_str().map(|s| Scalar::new(s.as_bytes()))
    }

    /// Parse a scalar as a `bool` (`yes` or `no`).
    pub fn to_bool(&self) -> Option<bool> {
        self.scalar()?.to_bool().ok()
    }

    /// Parse a scalar as an `i64`.
    pub fn to_i64(&self) -> Option<i64> {
        self.scalar()?.to_i64().ok()
    }

    /// Parse a scalar as a `u64`.
    pub fn to_u64(&self) -> Option<u64> {
        self.scalar()?.to_u64().ok()
    }

    /// Parse a scalar as an `f64`.
    pub fn to_f64(&self) -> Option<f64> {
        self.scalar()?.to_f64().ok()
    }
}

impl From<Block> for Value {
    fn from(block: Block) -> Self {
        Value::Block(block)
    }
}

impl Block {
    /// Make an empty block.
    pub fn new() -> Self {
        Block::default()
    }

    /// The items in source order.
    pub fn items(&self) -> &[Item] {
        &self.items
    }

    /// The items, mutable.
    pub fn items_mut(&mut self) -> &mut Vec<Item> {
        &mut self.items
    }

    /// Discard the block and return the items.
    pub fn into_items(self) -> Vec<Item> {
        self.items
    }

    /// The number of items.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Return `true` when the block has no items.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// The fields in source order.
    pub fn fields(&self) -> impl DoubleEndedIterator<Item = &Field> {
        self.items.iter().filter_map(|e| match e {
            Item::Field(f) => Some(f),
            _ => None,
        })
    }

    /// The bare values in source order.
    pub fn values(&self) -> impl DoubleEndedIterator<Item = &Value> {
        self.items.iter().filter_map(|e| match e {
            Item::Value(v) => Some(v),
            _ => None,
        })
    }

    /// The value of the first field with `key`.
    ///
    /// A key can occur more than one time. Use [`get_all`](Block::get_all)
    /// to get all of the values, or [`get_last`](Block::get_last) for the
    /// last one.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.fields().find(|f| f.key == key).map(|f| &f.value)
    }

    /// The value of the last field with `key`.
    pub fn get_last(&self, key: &str) -> Option<&Value> {
        self.fields().rfind(|f| f.key == key).map(|f| &f.value)
    }

    /// The values of all of the fields with `key`, in source order.
    pub fn get_all<'b>(&'b self, key: &'b str) -> impl Iterator<Item = &'b Value> + 'b {
        self.fields()
            .filter(move |f| f.key == key)
            .map(|f| &f.value)
    }

    /// Return `true` when the block has items and each item is a field
    /// or a parameter.
    pub fn is_object(&self) -> bool {
        !self.is_empty() && self.items.iter().all(|e| !matches!(e, Item::Value(_)))
    }

    /// Return `true` when the block has items and each item is a bare
    /// value.
    pub fn is_array(&self) -> bool {
        !self.is_empty() && self.items.iter().all(|e| matches!(e, Item::Value(_)))
    }

    /// Return `true` when the block has both bare values and fields.
    pub fn is_mixed(&self) -> bool {
        !self.is_empty() && !self.is_object() && !self.is_array()
    }

    /// Add an item at the end.
    pub fn push(&mut self, item: impl Into<Item>) {
        self.items.push(item.into());
    }
}

impl From<Vec<Item>> for Block {
    fn from(items: Vec<Item>) -> Self {
        Block { items }
    }
}

impl FromIterator<Item> for Block {
    fn from_iter<I: IntoIterator<Item = Item>>(iter: I) -> Self {
        Block {
            items: iter.into_iter().collect(),
        }
    }
}

impl<'b> IntoIterator for &'b Block {
    type Item = &'b Item;
    type IntoIter = std::slice::Iter<'b, Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl From<Field> for Item {
    fn from(field: Field) -> Self {
        Item::Field(field)
    }
}

impl From<Value> for Item {
    fn from(value: Value) -> Self {
        Item::Value(value)
    }
}

impl From<Parameter> for Item {
    fn from(parameter: Parameter) -> Self {
        Item::Parameter(parameter)
    }
}

impl Item {
    /// The field, if the item is one.
    pub fn as_field(&self) -> Option<&Field> {
        match self {
            Item::Field(f) => Some(f),
            _ => None,
        }
    }

    /// The bare value, if the item is one.
    pub fn as_value(&self) -> Option<&Value> {
        match self {
            Item::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl Field {
    /// Make a field with an unquoted key.
    pub fn new(key: impl Into<String>, op: Option<Operator>, value: Value) -> Self {
        Field {
            key: key.into(),
            quoted_key: false,
            op,
            value,
        }
    }
}

impl Parameter {
    /// Make a parameter block.
    pub fn new(name: impl Into<String>, undefined: bool, body: Block) -> Self {
        Parameter {
            name: name.into(),
            undefined,
            body,
        }
    }
}

/// Makes values from a document model. It gives the same values as the
/// deserializer of [`Value`], but it does not use serde. Each block
/// allocates its items one time, with the size hint of the model.
pub(crate) struct ValueBuilder<D> {
    doc: D,
}

impl<D: Doc> ValueBuilder<D> {
    pub(crate) fn new(doc: D) -> Self {
        ValueBuilder { doc }
    }

    /// The items of a container as a block.
    pub(crate) fn block(&mut self, container: D::Container) -> Block {
        let mut items = Vec::with_capacity(self.doc.len_hint(container));
        for item in self.doc.items(container) {
            items.push(self.item(item));
        }
        Block { items }
    }

    pub(crate) fn value(&mut self, node: D::Node) -> Value {
        let doc = self.doc;
        match doc.view(node) {
            View::Scalar(s) if doc.quoted(s) => Value::Quoted(doc.string(s)),
            View::Scalar(s) => Value::Unquoted(doc.string(s)),
            View::Calc(s) => Value::Calc(doc.string(s)),
            View::Block(c) => Value::Block(self.block(c)),
            View::Headered(h, c) => Value::HeaderedBlock(doc.string(h), self.block(c)),
        }
    }

    fn item(&mut self, item: DocItem<D>) -> Item {
        let doc = self.doc;
        match item {
            DocItem::Field { key, op, value } => Item::Field(Field {
                key: doc.string(key),
                quoted_key: doc.quoted(key),
                op,
                value: self.value(value),
            }),
            DocItem::Value(node) => Item::Value(self.value(node)),
            DocItem::Parameter {
                name,
                undefined,
                body,
            } => Item::Parameter(Parameter {
                name: doc.string(name),
                undefined,
                body: self.block(body),
            }),
        }
    }
}
