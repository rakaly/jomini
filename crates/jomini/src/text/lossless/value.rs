//! **Experimental** owned value for the Clausewitz format.

mod de;
pub(crate) mod tape;

use super::SyntaxTree;
use super::de::{Doc, Item as DocItem, View};
use crate::{Encoding, Scalar, text::Operator};

/// **Experimental.** An owned value of a document.
///
/// An owned value is like `serde_json::Value`. It also keeps the data that
/// the serde data model does not: quotes, variables, operators, headers,
/// duplicate keys, the order of the items, and containers that have both
/// fields and bare values. It owns its data (`OwnedValue: 'static`), so it
/// can live longer than the source.
///
/// It does not keep the comments and the whitespace. It also does not keep a
/// CK3 reader directive (`@:insert name`), except in the value of a field.
/// There, the directive is its text.
///
/// For the value of a whole document, use [`OwnedBlock`]. It deserializes
/// with the rules of the root.
///
/// # Deserialize into a value
///
/// `OwnedValue` and `OwnedBlock` implement
/// [`Deserialize`](serde::Deserialize). The data that they keep depends on
/// the deserializer:
///
/// - The tree deserializer gives all of the data.
/// - [`TextDeserializer`](crate::TextDeserializer) gives all of the data of
///   its tape. The tape does not keep the difference between `key = { }`
///   and `key { }`, and it drops some empty blocks.
/// - The streaming reader cannot make a value. It returns an error.
/// - A different deserializer gives no quotes, no variables, no operators,
///   and no headers.
/// - A field of a `#[serde(flatten)]` or `#[serde(untagged)]` container
///   also gives less, because serde first copies the data into its own
///   model. To keep all of the data, deserialize the whole block.
///
/// # Deserialize from a value
///
/// A value and a block are also serde deserializers, so you can deserialize
/// them into a type later. A scalar holds decoded text. Thus a `&[u8]` or a
/// byte buffer target gets the UTF-8 bytes of the text, with no escape
/// characters. The tree and the tape give the bytes of the source instead:
/// they are in the encoding of the source and keep the escape characters.
///
/// ```
/// use jomini::text::{Operator, lossless::{OwnedBlock, de::from_utf8_slice}};
/// use serde::Deserialize;
///
/// let source = br#"
///     unquoted = 1234
///     quoted = "567"
///     operator > 0.5
///     color = hsv { 0.3 0.4 0.5 }
///     sequence = { 1 2 3 4 }
///     map = { a = 1 b = 2 }
/// "#;
/// let root: OwnedBlock = from_utf8_slice(source)?;
///
/// let unquoted = root.get("unquoted").unwrap();
/// assert_eq!(f32::deserialize(unquoted)?, 1234.0);
/// assert!(root.get("quoted").unwrap().as_scalar().unwrap().is_quoted());
///
/// let field = root.fields().find(|f| f.key == "operator").unwrap();
/// assert_eq!(field.op, Some(Operator::GreaterThan));
///
/// let (header, body) = root.get("color").unwrap().as_headered_block().unwrap();
/// assert_eq!(header, "hsv");
/// assert_eq!(body.len(), 3);
///
/// assert!(root.get("sequence").unwrap().as_block().unwrap().is_array());
/// assert_eq!(root.get("map").unwrap().get("b").unwrap().to_i64(), Some(2));
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum OwnedValue {
    /// A scalar, such as `yes`, `"text"`, or `@name`.
    Scalar(OwnedScalar),
    /// A `@[ ... ]` calculation. The text is the source text, `@[` and `]`
    /// included.
    Calc(String),
    /// A `{ ... }` block: an object, an array, a mixed container, or an
    /// empty block.
    ///
    /// As a deserializer, the value reads the block as a block in a
    /// document: the first item tells if it is an object or an array. To
    /// read a block as the root of a document, deserialize the
    /// [`OwnedBlock`].
    Block(OwnedBlock),
    /// A header and its block, such as `rgb { 1 2 3 }`. The header is an
    /// unquoted scalar or a variable, as in `@space { 1 2 3 }`.
    HeaderedBlock(OwnedScalar, OwnedBlock),
}

/// **Experimental.** An owned scalar: a scalar value, the key of a field, or
/// the header of a block.
///
/// The variant tells how the source writes the scalar, so a program can
/// expand a variable and keep quoted text as it is:
///
/// ```
/// use jomini::text::lossless::{OwnedBlock, OwnedScalar, de::from_utf8_slice};
///
/// let root: OwnedBlock = from_utf8_slice(b"@size = 2 a = @size b = \"@size\" c = rgb { 1 2 3 }")?;
/// assert_eq!(root.fields().next().unwrap().key, OwnedScalar::Variable("@size".into()));
/// assert_eq!(root.get("a").unwrap().as_scalar(), Some(&OwnedScalar::Variable("@size".into())));
/// assert_eq!(root.get("b").unwrap().as_scalar(), Some(&OwnedScalar::Quoted("@size".into())));
/// assert_eq!(root.get("c").unwrap().header(), Some(&OwnedScalar::Unquoted("rgb".into())));
/// assert_eq!(root.get("b").unwrap().as_str(), Some("@size"));
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum OwnedScalar {
    /// A scalar with no quotes, such as `yes`, `1.5`, or `1444.11.11`. An
    /// interpolation (`[Root.GetName]`) and a code payload are also unquoted
    /// scalars that hold their source text.
    Unquoted(String),
    /// A scalar that had quotes. The text has no quotes and no escape
    /// characters.
    Quoted(String),
    /// A `@name` variable. The text is the source text, `@` included.
    ///
    /// The [`Flavor`](super::Flavor) of the parser must recognize variables.
    /// Else `@name` is [`OwnedScalar::Unquoted`].
    Variable(String),
}

impl OwnedScalar {
    /// The text of the scalar.
    pub fn as_str(&self) -> &str {
        match self {
            OwnedScalar::Unquoted(s) | OwnedScalar::Quoted(s) | OwnedScalar::Variable(s) => s,
        }
    }

    /// Discard the kind and return the text.
    pub fn into_string(self) -> String {
        match self {
            OwnedScalar::Unquoted(s) | OwnedScalar::Quoted(s) | OwnedScalar::Variable(s) => s,
        }
    }

    /// Return `true` for a scalar that had quotes.
    pub fn is_quoted(&self) -> bool {
        matches!(self, OwnedScalar::Quoted(_))
    }

    /// Return `true` for a `@name` variable.
    pub fn is_variable(&self) -> bool {
        matches!(self, OwnedScalar::Variable(_))
    }
}

/// An unquoted scalar.
impl From<String> for OwnedScalar {
    fn from(text: String) -> Self {
        OwnedScalar::Unquoted(text)
    }
}

/// An unquoted scalar.
impl From<&str> for OwnedScalar {
    fn from(text: &str) -> Self {
        OwnedScalar::Unquoted(text.to_string())
    }
}

impl AsRef<str> for OwnedScalar {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl std::fmt::Display for OwnedScalar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Compare the text. The kind of the scalar has no effect.
impl PartialEq<str> for OwnedScalar {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

/// Compare the text. The kind of the scalar has no effect.
impl PartialEq<&str> for OwnedScalar {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

/// **Experimental.** The items of a block in source order.
///
/// A block keeps each field, also when a key occurs more than one time:
///
/// ```
/// use jomini::text::lossless::{OwnedBlock, de::from_utf8_slice};
///
/// let root: OwnedBlock = from_utf8_slice(b"owner = FRA owner = BRI add_core = FRA")?;
/// assert_eq!(root.get("owner").unwrap().as_str(), Some("FRA"));
/// assert_eq!(root.get_last("owner").unwrap().as_str(), Some("BRI"));
/// assert_eq!(root.get_all("owner").count(), 2);
/// assert!(root.is_object());
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
///
/// A block is the root of a document, so the first item does not make the
/// root an array:
///
/// ```
/// use jomini::{Utf8Encoding, text::lossless::{OwnedBlock, OwnedValue, parse}};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Areas {
///     brittany: Vec<u32>,
/// }
///
/// let tree = parse(b"brittany { 169 170 }");
/// let root = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
/// assert_eq!(Areas::deserialize(&root)?.brittany, [169, 170]);
///
/// // The same block in a value is an array, as it is in a document.
/// let value = OwnedValue::from(root);
/// assert!(Areas::deserialize(&value).is_err());
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
///
/// You can also make a block in code, and then deserialize it into a type:
///
/// ```
/// use jomini::text::{Operator, lossless::{OwnedBlock, OwnedField, OwnedValue}};
/// use serde::Deserialize;
///
/// #[derive(Deserialize)]
/// struct Province {
///     owner: String,
///     base_tax: u32,
/// }
///
/// let mut block = OwnedBlock::new();
/// block.push(OwnedField::new("owner", Some(Operator::Equal), OwnedValue::unquoted("FRA")));
/// block.push(OwnedField::new("base_tax", Some(Operator::Equal), OwnedValue::unquoted("3")));
///
/// let province = Province::deserialize(block)?;
/// assert_eq!((province.owner.as_str(), province.base_tax), ("FRA", 3));
/// # Ok::<(), jomini::text::lossless::de::Error>(())
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct OwnedBlock {
    items: Vec<OwnedItem>,
}

/// **Experimental.** An item of an [`OwnedBlock`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum OwnedItem {
    /// A `key <op> value` field.
    Field(OwnedField),
    /// A bare value, such as an element of an array.
    Value(OwnedValue),
    /// A conditional parameter block, such as `[[name] ... ]` in EU4.
    Parameter(OwnedParameter),
}

/// **Experimental.** A `key <op> value` field.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct OwnedField {
    /// The key. A [`OwnedScalar::Variable`] key defines the variable, as in
    /// `@size = 0.5`.
    pub key: OwnedScalar,
    /// The operator, or `None` for a `key { ... }` field that has no
    /// operator.
    pub op: Option<Operator>,
    /// The value.
    pub value: OwnedValue,
}

/// **Experimental.** A conditional parameter block, `[[name] ... ]` or
/// `[[!name] ... ]`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct OwnedParameter {
    /// The name of the parameter.
    pub name: String,
    /// The block applies when the parameter is not defined (`[[!name]`).
    pub undefined: bool,
    /// The items of the block.
    pub body: OwnedBlock,
}

impl OwnedValue {
    /// Make an unquoted scalar.
    pub fn unquoted(text: impl Into<String>) -> OwnedValue {
        OwnedValue::Scalar(OwnedScalar::Unquoted(text.into()))
    }

    /// Make a quoted scalar.
    pub fn quoted(text: impl Into<String>) -> OwnedValue {
        OwnedValue::Scalar(OwnedScalar::Quoted(text.into()))
    }

    /// The scalar of a [`OwnedValue::Scalar`].
    pub fn as_scalar(&self) -> Option<&OwnedScalar> {
        match self {
            OwnedValue::Scalar(s) => Some(s),
            _ => None,
        }
    }

    /// The text of a scalar of any kind, or the source text of a
    /// calculation. This is the text that a `String` target gets.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            OwnedValue::Scalar(s) => Some(s.as_str()),
            OwnedValue::Calc(s) => Some(s),
            _ => None,
        }
    }

    /// The block of a [`OwnedValue::Block`].
    pub fn as_block(&self) -> Option<&OwnedBlock> {
        match self {
            OwnedValue::Block(b) => Some(b),
            _ => None,
        }
    }

    /// The block of a [`OwnedValue::Block`], mutable.
    pub fn as_block_mut(&mut self) -> Option<&mut OwnedBlock> {
        match self {
            OwnedValue::Block(b) => Some(b),
            _ => None,
        }
    }

    /// The header and the block of a [`OwnedValue::HeaderedBlock`].
    pub fn as_headered_block(&self) -> Option<(&OwnedScalar, &OwnedBlock)> {
        match self {
            OwnedValue::HeaderedBlock(h, b) => Some((h, b)),
            _ => None,
        }
    }

    /// The header of a [`OwnedValue::HeaderedBlock`].
    pub fn header(&self) -> Option<&OwnedScalar> {
        self.as_headered_block().map(|(h, _)| h)
    }

    /// The source text of a [`OwnedValue::Calc`].
    pub fn as_calc(&self) -> Option<&str> {
        match self {
            OwnedValue::Calc(s) => Some(s),
            _ => None,
        }
    }

    /// The first value of `key` in a block. See [`OwnedBlock::get`].
    pub fn get(&self, key: &str) -> Option<&OwnedValue> {
        self.as_block()?.get(key)
    }

    fn scalar(&self) -> Option<Scalar<'_>> {
        self.as_scalar().map(|s| Scalar::new(s.as_str().as_bytes()))
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

impl From<OwnedBlock> for OwnedValue {
    fn from(block: OwnedBlock) -> Self {
        OwnedValue::Block(block)
    }
}

impl From<OwnedScalar> for OwnedValue {
    fn from(scalar: OwnedScalar) -> Self {
        OwnedValue::Scalar(scalar)
    }
}

impl OwnedBlock {
    /// Read the root of a tree into a block. The encoding decodes the
    /// scalars. This function does not examine the syntax errors of the
    /// tree.
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::lossless::{OwnedBlock, parse}};
    ///
    /// let tree = parse(b"a = 1 b > 2");
    /// let root = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    /// assert_eq!(root.get("b").unwrap().as_str(), Some("2"));
    /// ```
    pub fn from_tree<E: Encoding>(tree: SyntaxTree<'_, '_>, encoding: E) -> OwnedBlock {
        super::de::tree_block(tree, encoding)
    }

    /// Make an empty block.
    pub fn new() -> Self {
        OwnedBlock::default()
    }

    /// The items in source order.
    pub fn items(&self) -> &[OwnedItem] {
        &self.items
    }

    /// The items, mutable.
    pub fn items_mut(&mut self) -> &mut Vec<OwnedItem> {
        &mut self.items
    }

    /// Discard the block and return the items.
    pub fn into_items(self) -> Vec<OwnedItem> {
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
    pub fn fields(&self) -> impl DoubleEndedIterator<Item = &OwnedField> {
        self.items.iter().filter_map(|e| match e {
            OwnedItem::Field(f) => Some(f),
            _ => None,
        })
    }

    /// The bare values in source order.
    pub fn values(&self) -> impl DoubleEndedIterator<Item = &OwnedValue> {
        self.items.iter().filter_map(|e| match e {
            OwnedItem::Value(v) => Some(v),
            _ => None,
        })
    }

    /// The value of the first field with `key`. The comparison uses the
    /// text of the key, so `"key"` and `key` are equal.
    ///
    /// A key can occur more than one time. Use
    /// [`get_all`](OwnedBlock::get_all) to get all of the values, or
    /// [`get_last`](OwnedBlock::get_last) for the last one.
    pub fn get(&self, key: &str) -> Option<&OwnedValue> {
        self.fields().find(|f| f.key == key).map(|f| &f.value)
    }

    /// The value of the last field with `key`.
    pub fn get_last(&self, key: &str) -> Option<&OwnedValue> {
        self.fields().rfind(|f| f.key == key).map(|f| &f.value)
    }

    /// The values of all of the fields with `key`, in source order.
    pub fn get_all<'v, 'k>(
        &'v self,
        key: &'k str,
    ) -> impl Iterator<Item = &'v OwnedValue> + use<'v, 'k> {
        self.fields()
            .filter(move |f| f.key == key)
            .map(|f| &f.value)
    }

    /// Return `true` when the block has items and each item is a field
    /// or a parameter.
    pub fn is_object(&self) -> bool {
        !self.is_empty() && self.items.iter().all(|e| !matches!(e, OwnedItem::Value(_)))
    }

    /// Return `true` when the block has items and each item is a bare
    /// value.
    pub fn is_array(&self) -> bool {
        !self.is_empty() && self.items.iter().all(|e| matches!(e, OwnedItem::Value(_)))
    }

    /// Return `true` when the block has both bare values and fields.
    pub fn is_mixed(&self) -> bool {
        !self.is_empty() && !self.is_object() && !self.is_array()
    }

    /// Add an item at the end.
    pub fn push(&mut self, item: impl Into<OwnedItem>) {
        self.items.push(item.into());
    }
}

impl From<Vec<OwnedItem>> for OwnedBlock {
    fn from(items: Vec<OwnedItem>) -> Self {
        OwnedBlock { items }
    }
}

impl FromIterator<OwnedItem> for OwnedBlock {
    fn from_iter<I: IntoIterator<Item = OwnedItem>>(iter: I) -> Self {
        OwnedBlock {
            items: iter.into_iter().collect(),
        }
    }
}

impl<'b> IntoIterator for &'b OwnedBlock {
    type Item = &'b OwnedItem;
    type IntoIter = std::slice::Iter<'b, OwnedItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}

impl From<OwnedField> for OwnedItem {
    fn from(field: OwnedField) -> Self {
        OwnedItem::Field(field)
    }
}

impl From<OwnedValue> for OwnedItem {
    fn from(value: OwnedValue) -> Self {
        OwnedItem::Value(value)
    }
}

impl From<OwnedParameter> for OwnedItem {
    fn from(parameter: OwnedParameter) -> Self {
        OwnedItem::Parameter(parameter)
    }
}

impl OwnedItem {
    /// The field, if the item is one.
    pub fn as_field(&self) -> Option<&OwnedField> {
        match self {
            OwnedItem::Field(f) => Some(f),
            _ => None,
        }
    }

    /// The bare value, if the item is one.
    pub fn as_value(&self) -> Option<&OwnedValue> {
        match self {
            OwnedItem::Value(v) => Some(v),
            _ => None,
        }
    }
}

impl OwnedField {
    /// Make a field. A text key is an unquoted scalar.
    pub fn new(key: impl Into<OwnedScalar>, op: Option<Operator>, value: OwnedValue) -> Self {
        OwnedField {
            key: key.into(),
            op,
            value,
        }
    }
}

impl OwnedParameter {
    /// Make a parameter block.
    pub fn new(name: impl Into<String>, undefined: bool, body: OwnedBlock) -> Self {
        OwnedParameter {
            name: name.into(),
            undefined,
            body,
        }
    }
}

/// Makes values from a document model. It gives the same values as the
/// deserializer of [`OwnedValue`], but it does not use serde. Each block
/// allocates its items one time, with the size hint of the model.
pub(crate) struct ValueBuilder<D> {
    doc: D,
}

impl<D: Doc> ValueBuilder<D> {
    pub(crate) fn new(doc: D) -> Self {
        ValueBuilder { doc }
    }

    /// The items of a container as a block.
    pub(crate) fn block(&mut self, container: D::Container) -> OwnedBlock {
        let mut items = Vec::with_capacity(self.doc.capacity_hint(container));
        for item in self.doc.items(container) {
            items.push(self.item(item));
        }
        OwnedBlock { items }
    }

    fn scalar(&self, s: D::Scalar) -> OwnedScalar {
        let doc = self.doc;
        if doc.quoted(s) {
            OwnedScalar::Quoted(doc.string(s))
        } else if doc.variable(s) {
            OwnedScalar::Variable(doc.string(s))
        } else {
            OwnedScalar::Unquoted(doc.string(s))
        }
    }

    pub(crate) fn value(&mut self, node: D::Node) -> OwnedValue {
        match self.doc.view(node) {
            View::Scalar(s) => OwnedValue::Scalar(self.scalar(s)),
            View::Calc(s) => OwnedValue::Calc(self.doc.string(s)),
            View::Block(c) => OwnedValue::Block(self.block(c)),
            View::Headered(h, c) => OwnedValue::HeaderedBlock(self.scalar(h), self.block(c)),
        }
    }

    fn item(&mut self, item: DocItem<D>) -> OwnedItem {
        match item {
            DocItem::Field { key, op, value } => OwnedItem::Field(OwnedField {
                key: self.scalar(key),
                op,
                value: self.value(value),
            }),
            DocItem::Value(node) => OwnedItem::Value(self.value(node)),
            DocItem::Parameter {
                name,
                undefined,
                body,
            } => OwnedItem::Parameter(OwnedParameter {
                name: self.doc.string(name),
                undefined,
                body: self.block(body),
            }),
        }
    }
}
