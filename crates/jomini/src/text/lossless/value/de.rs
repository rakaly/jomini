//! Serde support for [`OwnedValue`]: `Deserialize` and a deserializer.

use super::{OwnedBlock, OwnedField, OwnedItem, OwnedParameter, OwnedScalar, OwnedValue};
use crate::{
    Scalar,
    text::{
        Operator,
        lossless::{
            TextRange,
            de::{
                Doc, DocDe, Error, Item as DocItem, TAG_BLOCK, TAG_CALC, TAG_FIELD, TAG_HEADERED,
                TAG_PARAMETER, TAG_QUOTED, TAG_UNQUOTED, TAG_VALUE, TAG_VARIABLE, VALUE, ValueDe,
                View,
            },
        },
    },
};
use serde::de::{
    self, Deserialize, DeserializeSeed, Deserializer, EnumAccess, IntoDeserializer, MapAccess,
    SeqAccess, VariantAccess, Visitor,
};
use std::{fmt, marker::PhantomData};

impl<'de> Deserialize<'de> for OwnedValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_newtype_struct(VALUE, ValueVisitor)
    }
}

/// The kinds of the private protocol of the tree deserializer.
enum Tag {
    Unquoted,
    Quoted,
    Variable,
    Calc,
    Block,
    Headered,
    Field,
    Value,
    Parameter,
}

impl<'de> Deserialize<'de> for Tag {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct TagVisitor;

        impl Visitor<'_> for TagVisitor {
            type Value = Tag;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a jomini value tag")
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<Tag, E> {
                Ok(match v {
                    TAG_UNQUOTED => Tag::Unquoted,
                    TAG_QUOTED => Tag::Quoted,
                    TAG_VARIABLE => Tag::Variable,
                    TAG_CALC => Tag::Calc,
                    TAG_BLOCK => Tag::Block,
                    TAG_HEADERED => Tag::Headered,
                    TAG_FIELD => Tag::Field,
                    TAG_VALUE => Tag::Value,
                    TAG_PARAMETER => Tag::Parameter,
                    _ => return Err(E::custom("unknown jomini value tag")),
                })
            }
        }

        deserializer.deserialize_identifier(TagVisitor)
    }
}

struct ValueVisitor;

impl<'de> Visitor<'de> for ValueVisitor {
    type Value = OwnedValue;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<OwnedValue, A::Error> {
        let (tag, variant) = data.variant::<Tag>()?;
        let scalar = |s| Ok(OwnedValue::Scalar(s));
        match tag {
            Tag::Unquoted => scalar(OwnedScalar::Unquoted(variant.newtype_variant()?)),
            Tag::Quoted => scalar(OwnedScalar::Quoted(variant.newtype_variant()?)),
            Tag::Variable => scalar(OwnedScalar::Variable(variant.newtype_variant()?)),
            Tag::Calc => variant.newtype_variant().map(OwnedValue::Calc),
            Tag::Block => variant
                .newtype_variant_seed(BlockSeed)
                .map(OwnedValue::Block),
            Tag::Headered => variant.tuple_variant(2, HeaderedVisitor),
            _ => Err(de::Error::custom("unexpected jomini value tag")),
        }
    }

    fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<OwnedValue, D::Error> {
        d.deserialize_any(AnyVisitor)
    }

    // A deserializer that does not know the private protocol can call the
    // visitor directly.

    fn visit_bool<E: de::Error>(self, v: bool) -> Result<OwnedValue, E> {
        AnyVisitor.visit_bool(v)
    }

    fn visit_i64<E: de::Error>(self, v: i64) -> Result<OwnedValue, E> {
        AnyVisitor.visit_i64(v)
    }

    fn visit_u64<E: de::Error>(self, v: u64) -> Result<OwnedValue, E> {
        AnyVisitor.visit_u64(v)
    }

    fn visit_f64<E: de::Error>(self, v: f64) -> Result<OwnedValue, E> {
        AnyVisitor.visit_f64(v)
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<OwnedValue, E> {
        AnyVisitor.visit_str(v)
    }

    fn visit_string<E: de::Error>(self, v: String) -> Result<OwnedValue, E> {
        AnyVisitor.visit_string(v)
    }

    fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<OwnedValue, E> {
        AnyVisitor.visit_bytes(v)
    }

    fn visit_none<E: de::Error>(self) -> Result<OwnedValue, E> {
        AnyVisitor.visit_none()
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<OwnedValue, D::Error> {
        AnyVisitor.visit_some(d)
    }

    fn visit_unit<E: de::Error>(self) -> Result<OwnedValue, E> {
        AnyVisitor.visit_unit()
    }

    fn visit_seq<A: SeqAccess<'de>>(self, seq: A) -> Result<OwnedValue, A::Error> {
        AnyVisitor.visit_seq(seq)
    }

    fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<OwnedValue, A::Error> {
        AnyVisitor.visit_map(map)
    }
}

struct HeaderedVisitor;

impl<'de> Visitor<'de> for HeaderedVisitor {
    type Value = OwnedValue;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a headered block")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<OwnedValue, A::Error> {
        // The header is a tagged scalar, so it keeps its kind.
        let header = match next(&mut seq, 0)? {
            OwnedValue::Scalar(s) => s,
            _ => return Err(de::Error::custom("a header is a scalar")),
        };
        let block = seq
            .next_element_seed(BlockSeed)?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        Ok(OwnedValue::HeaderedBlock(header, block))
    }
}

fn next<'de, T: Deserialize<'de>, A: SeqAccess<'de>>(
    seq: &mut A,
    index: usize,
) -> Result<T, A::Error> {
    seq.next_element()?
        .ok_or_else(|| de::Error::invalid_length(index, &"the parts of a jomini item"))
}

/// Reads the items of a block from the private protocol.
struct BlockSeed;

impl<'de> DeserializeSeed<'de> for BlockSeed {
    type Value = OwnedBlock;

    fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<OwnedBlock, D::Error> {
        d.deserialize_seq(self)
    }
}

impl<'de> Visitor<'de> for BlockSeed {
    type Value = OwnedBlock;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("the items of a block")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<OwnedBlock, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(item) = seq.next_element_seed(ItemSeed)? {
            items.push(item);
        }
        Ok(OwnedBlock { items })
    }
}

/// Reads one item of a block from the private protocol.
struct ItemSeed;

impl<'de> DeserializeSeed<'de> for ItemSeed {
    type Value = OwnedItem;

    fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<OwnedItem, D::Error> {
        d.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for ItemSeed {
    type Value = OwnedItem;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("an item of a block")
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<OwnedItem, A::Error> {
        let (tag, variant) = data.variant::<Tag>()?;
        match tag {
            Tag::Field => variant.tuple_variant(5, FieldVisitor),
            Tag::Value => variant.newtype_variant().map(OwnedItem::Value),
            Tag::Parameter => variant.tuple_variant(3, ParameterVisitor),
            _ => Err(de::Error::custom("unexpected jomini item tag")),
        }
    }
}

struct FieldVisitor;

impl<'de> Visitor<'de> for FieldVisitor {
    type Value = OwnedItem;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a field")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<OwnedItem, A::Error> {
        let text: String = next(&mut seq, 0)?;
        let quoted: bool = next(&mut seq, 1)?;
        let variable: bool = next(&mut seq, 2)?;
        let key = if quoted {
            OwnedScalar::Quoted(text)
        } else if variable {
            OwnedScalar::Variable(text)
        } else {
            OwnedScalar::Unquoted(text)
        };
        Ok(OwnedItem::Field(OwnedField {
            key,
            op: next(&mut seq, 3)?,
            value: next(&mut seq, 4)?,
        }))
    }
}

struct ParameterVisitor;

impl<'de> Visitor<'de> for ParameterVisitor {
    type Value = OwnedItem;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a parameter")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<OwnedItem, A::Error> {
        let name = next(&mut seq, 0)?;
        let undefined = next(&mut seq, 1)?;
        let body = seq
            .next_element_seed(BlockSeed)?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;
        Ok(OwnedItem::Parameter(OwnedParameter {
            name,
            undefined,
            body,
        }))
    }
}

/// Makes a value from a deserializer that does not know the private
/// protocol. Such a deserializer does not tell about quotes, operators, or
/// headers, so each scalar is unquoted and each field has `=`.
struct AnyVisitor;

impl<'de> Visitor<'de> for AnyVisitor {
    type Value = OwnedValue;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_bool<E: de::Error>(self, v: bool) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(if v { "yes" } else { "no" }))
    }

    fn visit_i64<E: de::Error>(self, v: i64) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(v.to_string()))
    }

    fn visit_u64<E: de::Error>(self, v: u64) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(v.to_string()))
    }

    fn visit_f64<E: de::Error>(self, v: f64) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(v.to_string()))
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(v))
    }

    fn visit_string<E: de::Error>(self, v: String) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(v))
    }

    fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<OwnedValue, E> {
        Ok(OwnedValue::unquoted(String::from_utf8_lossy(v)))
    }

    fn visit_none<E: de::Error>(self) -> Result<OwnedValue, E> {
        Ok(OwnedValue::Block(OwnedBlock::new()))
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<OwnedValue, D::Error> {
        OwnedValue::deserialize(d)
    }

    fn visit_unit<E: de::Error>(self) -> Result<OwnedValue, E> {
        Ok(OwnedValue::Block(OwnedBlock::new()))
    }

    fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<OwnedValue, D::Error> {
        OwnedValue::deserialize(d)
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<OwnedValue, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0).min(4096));
        while let Some(value) = seq.next_element::<OwnedValue>()? {
            items.push(OwnedItem::Value(value));
        }
        Ok(OwnedValue::Block(OwnedBlock { items }))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<OwnedValue, A::Error> {
        let mut items = Vec::with_capacity(map.size_hint().unwrap_or(0).min(4096));
        while let Some(key) = map.next_key::<String>()? {
            let value = map.next_value::<OwnedValue>()?;
            items.push(OwnedItem::Field(OwnedField::new(
                key,
                Some(Operator::Equal),
                value,
            )));
        }
        Ok(OwnedValue::Block(OwnedBlock { items }))
    }
}

/// The scalars of a value can be borrowed for the deserializer.
#[derive(Clone, Copy)]
pub(crate) struct Borrowed;

/// The value is a local copy, so the scalars are copied.
#[derive(Clone, Copy)]
pub(crate) struct Copied;

/// The lifetime of the value and the mode, with no data.
type Mode<'v, B> = PhantomData<(&'v (), fn() -> B)>;

/// The [`Doc`] model of an [`OwnedValue`]. A value then deserializes with the same
/// rules as the syntax tree.
pub(crate) struct ValueDoc<'v, B> {
    mode: Mode<'v, B>,
}

impl<B> Clone for ValueDoc<'_, B> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<B> Copy for ValueDoc<'_, B> {}

impl<B> ValueDoc<'_, B> {
    fn new() -> Self {
        ValueDoc { mode: PhantomData }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct ValueScalar<'v> {
    text: &'v str,
    quoted: bool,
    variable: bool,
}

impl<'v> ValueScalar<'v> {
    fn new(scalar: &'v OwnedScalar) -> Self {
        ValueScalar {
            text: scalar.as_str(),
            quoted: scalar.is_quoted(),
            variable: scalar.is_variable(),
        }
    }

    /// A text that is not a scalar of the source, such as a calculation.
    fn text(text: &'v str) -> Self {
        ValueScalar {
            text,
            quoted: false,
            variable: false,
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct ValueContainer<'v> {
    items: &'v [OwnedItem],
    /// The root and the body of a parameter are always objects.
    root: bool,
    /// The block is the root of the document.
    top: bool,
}

pub(crate) struct ValueItems<'v, B> {
    iter: std::slice::Iter<'v, OwnedItem>,
    mode: PhantomData<B>,
}

impl<B> Clone for ValueItems<'_, B> {
    fn clone(&self) -> Self {
        ValueItems {
            iter: self.iter.clone(),
            mode: PhantomData,
        }
    }
}

impl<'v, B> Iterator for ValueItems<'v, B> {
    type Item = DocItem<ValueDoc<'v, B>>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.iter.next()? {
            OwnedItem::Field(f) => DocItem::Field {
                key: ValueScalar::new(&f.key),
                op: f.op,
                value: &f.value,
            },
            OwnedItem::Value(v) => DocItem::Value(v),
            OwnedItem::Parameter(p) => DocItem::Parameter {
                name: ValueScalar::text(&p.name),
                undefined: p.undefined,
                body: ValueContainer {
                    items: &p.body.items,
                    root: true,
                    top: false,
                },
            },
        })
    }
}

impl<'v, B> Doc for ValueDoc<'v, B> {
    type Node = &'v OwnedValue;
    type Scalar = ValueScalar<'v>;
    type Container = ValueContainer<'v>;
    type Items = ValueItems<'v, B>;
    type Scalars = std::iter::Empty<&'v OwnedValue>;

    fn view(self, node: Self::Node) -> View<Self> {
        let block = |b: &'v OwnedBlock| ValueContainer {
            items: &b.items,
            root: false,
            top: false,
        };
        match node {
            OwnedValue::Scalar(s) => View::Scalar(ValueScalar::new(s)),
            OwnedValue::Calc(s) => View::Calc(ValueScalar::text(s)),
            OwnedValue::Block(b) => View::Block(block(b)),
            OwnedValue::HeaderedBlock(h, b) => View::Headered(ValueScalar::new(h), block(b)),
        }
    }

    fn items(self, container: Self::Container) -> Self::Items {
        ValueItems {
            iter: container.items.iter(),
            mode: PhantomData,
        }
    }

    fn always_object(self, container: Self::Container) -> bool {
        container.root
    }

    fn is_root(self, container: Self::Container) -> bool {
        container.top
    }

    fn quoted(self, scalar: Self::Scalar) -> bool {
        scalar.quoted
    }

    fn variable(self, scalar: Self::Scalar) -> bool {
        scalar.variable
    }

    fn with_scalar<R>(self, scalar: Self::Scalar, f: impl FnOnce(Scalar<'_>) -> R) -> R {
        f(Scalar::new(scalar.text.as_bytes()))
    }

    fn string(self, scalar: Self::Scalar) -> String {
        scalar.text.to_string()
    }

    fn node_range(self, _node: Self::Node) -> Option<TextRange> {
        None
    }

    fn scalar_range(self, _scalar: Self::Scalar) -> Option<TextRange> {
        None
    }

    fn container_range(self, _container: Self::Container) -> Option<TextRange> {
        None
    }

    /// A bare value of an array is one element. A field gives its key, its
    /// operator, and its value. A headered block gives its header and its
    /// block. A first empty block is not an element. Thus give the
    /// number of items only when each item is a bare value that is not such
    /// a block. The check reads the items and not their contents.
    fn len_hint(self, container: Self::Container) -> Option<usize> {
        let simple = container.items.iter().all(|item| match item {
            OwnedItem::Value(OwnedValue::HeaderedBlock(..)) => false,
            OwnedItem::Value(OwnedValue::Block(block)) => !block.items.is_empty(),
            OwnedItem::Value(_) => true,
            OwnedItem::Field(_) | OwnedItem::Parameter(_) => false,
        });
        simple.then_some(container.items.len())
    }

    fn capacity_hint(self, container: Self::Container) -> usize {
        container.items.len()
    }

    fn scalars(self, _container: Self::Container) -> Option<Self::Scalars> {
        None
    }
}

impl<'de, 'v: 'de> DocDe<'de> for ValueDoc<'v, Borrowed> {
    fn visit_str<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_borrowed_str(scalar.text)
    }

    fn visit_bytes<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_borrowed_bytes(scalar.text.as_bytes())
    }
}

impl<'de> DocDe<'de> for ValueDoc<'_, Copied> {
    fn visit_str<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_str(scalar.text)
    }

    fn visit_bytes<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_bytes(scalar.text.as_bytes())
    }
}

/// The deserializer for `value`. A block is an object or an array by its
/// first item, as a block in a document is.
fn value_de<B>(value: &OwnedValue) -> ValueDe<ValueDoc<'_, B>> {
    ValueDe::node(ValueDoc::new(), value)
}

/// The deserializer for `block` as the root of a document: the block is
/// always an object.
fn block_de<B>(block: &OwnedBlock) -> ValueDe<ValueDoc<'_, B>> {
    let root = ValueContainer {
        items: &block.items,
        root: true,
        top: true,
    };
    ValueDe::root(ValueDoc::new(), root)
}

impl<'de> Deserialize<'de> for OwnedBlock {
    /// Read a block or the root of a document. A different value is an
    /// error.
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        match OwnedValue::deserialize(deserializer)? {
            OwnedValue::Block(block) => Ok(block),
            _ => Err(de::Error::custom("expected a block")),
        }
    }
}

macro_rules! forward_value {
    ($lt:lifetime, $de:expr, $($method:ident ( $($arg:ident : $ty:ty),* ))*) => {$(
        fn $method<V: Visitor<$lt>>(self, $($arg: $ty,)* visitor: V) -> Result<V::Value, Error> {
            Deserializer::$method($de(&self), $($arg,)* visitor)
        }
    )*};
}

macro_rules! value_deserializer {
    ($lt:lifetime, $de:expr) => {
        forward_value! {
            $lt, $de,
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
}

/// Deserialize a value into a type. The data of the type can borrow from
/// the value.
impl<'de> Deserializer<'de> for &'de OwnedValue {
    type Error = Error;

    value_deserializer!('de, value_de::<Borrowed>);
}

/// Deserialize a value into a type. The data of the type cannot borrow
/// from the value.
impl<'de> Deserializer<'de> for OwnedValue {
    type Error = Error;

    value_deserializer!('de, value_de::<Copied>);
}

/// Deserialize a block as the root of a document. The data of the type
/// can borrow from the block.
impl<'de> Deserializer<'de> for &'de OwnedBlock {
    type Error = Error;

    value_deserializer!('de, block_de::<Borrowed>);
}

/// Deserialize a block as the root of a document. The data of the type
/// cannot borrow from the block.
impl<'de> Deserializer<'de> for OwnedBlock {
    type Error = Error;

    value_deserializer!('de, block_de::<Copied>);
}

impl<'de> IntoDeserializer<'de, Error> for &'de OwnedValue {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}

impl<'de> IntoDeserializer<'de, Error> for OwnedValue {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}

impl<'de> IntoDeserializer<'de, Error> for &'de OwnedBlock {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}

impl<'de> IntoDeserializer<'de, Error> for OwnedBlock {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}
