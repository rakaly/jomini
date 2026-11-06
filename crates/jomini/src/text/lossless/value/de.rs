//! Serde support for [`Value`]: `Deserialize` and a deserializer.

use super::{Block, Field, Item, Parameter, Value};
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

impl<'de> Deserialize<'de> for Value {
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
    type Value = Value;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<Value, A::Error> {
        let (tag, variant) = data.variant::<Tag>()?;
        match tag {
            Tag::Unquoted => variant.newtype_variant().map(Value::Unquoted),
            Tag::Quoted => variant.newtype_variant().map(Value::Quoted),
            Tag::Variable => variant.newtype_variant().map(Value::Variable),
            Tag::Calc => variant.newtype_variant().map(Value::Calc),
            Tag::Block => variant.newtype_variant_seed(BlockSeed).map(Value::Block),
            Tag::Headered => variant.tuple_variant(2, HeaderedVisitor),
            _ => Err(de::Error::custom("unexpected jomini value tag")),
        }
    }

    fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<Value, D::Error> {
        d.deserialize_any(AnyVisitor)
    }

    // A deserializer that does not know the private protocol can call the
    // visitor directly.

    fn visit_bool<E: de::Error>(self, v: bool) -> Result<Value, E> {
        AnyVisitor.visit_bool(v)
    }

    fn visit_i64<E: de::Error>(self, v: i64) -> Result<Value, E> {
        AnyVisitor.visit_i64(v)
    }

    fn visit_u64<E: de::Error>(self, v: u64) -> Result<Value, E> {
        AnyVisitor.visit_u64(v)
    }

    fn visit_f64<E: de::Error>(self, v: f64) -> Result<Value, E> {
        AnyVisitor.visit_f64(v)
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Value, E> {
        AnyVisitor.visit_str(v)
    }

    fn visit_string<E: de::Error>(self, v: String) -> Result<Value, E> {
        AnyVisitor.visit_string(v)
    }

    fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<Value, E> {
        AnyVisitor.visit_bytes(v)
    }

    fn visit_none<E: de::Error>(self) -> Result<Value, E> {
        AnyVisitor.visit_none()
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<Value, D::Error> {
        AnyVisitor.visit_some(d)
    }

    fn visit_unit<E: de::Error>(self) -> Result<Value, E> {
        AnyVisitor.visit_unit()
    }

    fn visit_seq<A: SeqAccess<'de>>(self, seq: A) -> Result<Value, A::Error> {
        AnyVisitor.visit_seq(seq)
    }

    fn visit_map<A: MapAccess<'de>>(self, map: A) -> Result<Value, A::Error> {
        AnyVisitor.visit_map(map)
    }
}

struct HeaderedVisitor;

impl<'de> Visitor<'de> for HeaderedVisitor {
    type Value = Value;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a headered block")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Value, A::Error> {
        let header: String = next(&mut seq, 0)?;
        let block = seq
            .next_element_seed(BlockSeed)?
            .ok_or_else(|| de::Error::invalid_length(1, &self))?;
        Ok(Value::HeaderedBlock(header, block))
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
    type Value = Block;

    fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<Block, D::Error> {
        d.deserialize_seq(self)
    }
}

impl<'de> Visitor<'de> for BlockSeed {
    type Value = Block;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("the items of a block")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Block, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(item) = seq.next_element_seed(ItemSeed)? {
            items.push(item);
        }
        Ok(Block { items })
    }
}

/// Reads one item of a block from the private protocol.
struct ItemSeed;

impl<'de> DeserializeSeed<'de> for ItemSeed {
    type Value = Item;

    fn deserialize<D: Deserializer<'de>>(self, d: D) -> Result<Item, D::Error> {
        d.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for ItemSeed {
    type Value = Item;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("an item of a block")
    }

    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<Item, A::Error> {
        let (tag, variant) = data.variant::<Tag>()?;
        match tag {
            Tag::Field => variant.tuple_variant(5, FieldVisitor),
            Tag::Value => variant.newtype_variant().map(Item::Value),
            Tag::Parameter => variant.tuple_variant(3, ParameterVisitor),
            _ => Err(de::Error::custom("unexpected jomini item tag")),
        }
    }
}

struct FieldVisitor;

impl<'de> Visitor<'de> for FieldVisitor {
    type Value = Item;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a field")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Item, A::Error> {
        Ok(Item::Field(Field {
            key: next(&mut seq, 0)?,
            quoted_key: next(&mut seq, 1)?,
            variable_key: next(&mut seq, 2)?,
            op: next(&mut seq, 3)?,
            value: next(&mut seq, 4)?,
        }))
    }
}

struct ParameterVisitor;

impl<'de> Visitor<'de> for ParameterVisitor {
    type Value = Item;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("a parameter")
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Item, A::Error> {
        let name = next(&mut seq, 0)?;
        let undefined = next(&mut seq, 1)?;
        let body = seq
            .next_element_seed(BlockSeed)?
            .ok_or_else(|| de::Error::invalid_length(2, &self))?;
        Ok(Item::Parameter(Parameter {
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
    type Value = Value;

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_bool<E: de::Error>(self, v: bool) -> Result<Value, E> {
        Ok(Value::unquoted(if v { "yes" } else { "no" }))
    }

    fn visit_i64<E: de::Error>(self, v: i64) -> Result<Value, E> {
        Ok(Value::Unquoted(v.to_string()))
    }

    fn visit_u64<E: de::Error>(self, v: u64) -> Result<Value, E> {
        Ok(Value::Unquoted(v.to_string()))
    }

    fn visit_f64<E: de::Error>(self, v: f64) -> Result<Value, E> {
        Ok(Value::Unquoted(v.to_string()))
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Value, E> {
        Ok(Value::unquoted(v))
    }

    fn visit_string<E: de::Error>(self, v: String) -> Result<Value, E> {
        Ok(Value::Unquoted(v))
    }

    fn visit_bytes<E: de::Error>(self, v: &[u8]) -> Result<Value, E> {
        Ok(Value::Unquoted(String::from_utf8_lossy(v).into_owned()))
    }

    fn visit_none<E: de::Error>(self) -> Result<Value, E> {
        Ok(Value::Block(Block::new()))
    }

    fn visit_some<D: Deserializer<'de>>(self, d: D) -> Result<Value, D::Error> {
        Value::deserialize(d)
    }

    fn visit_unit<E: de::Error>(self) -> Result<Value, E> {
        Ok(Value::Block(Block::new()))
    }

    fn visit_newtype_struct<D: Deserializer<'de>>(self, d: D) -> Result<Value, D::Error> {
        Value::deserialize(d)
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Value, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0).min(4096));
        while let Some(value) = seq.next_element::<Value>()? {
            items.push(Item::Value(value));
        }
        Ok(Value::Block(Block { items }))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Value, A::Error> {
        let mut items = Vec::with_capacity(map.size_hint().unwrap_or(0).min(4096));
        while let Some(key) = map.next_key::<String>()? {
            let value = map.next_value::<Value>()?;
            items.push(Item::Field(Field::new(key, Some(Operator::Equal), value)));
        }
        Ok(Value::Block(Block { items }))
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

/// The [`Doc`] model of a [`Value`]. A value then deserializes with the same
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

#[derive(Clone, Copy)]
pub(crate) struct ValueContainer<'v> {
    items: &'v [Item],
    /// The root and the body of a parameter are always objects.
    root: bool,
    /// The block is the root of the document.
    top: bool,
}

pub(crate) struct ValueItems<'v, B> {
    iter: std::slice::Iter<'v, Item>,
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
            Item::Field(f) => DocItem::Field {
                key: ValueScalar {
                    text: &f.key,
                    quoted: f.quoted_key,
                    variable: f.variable_key,
                },
                op: f.op,
                value: &f.value,
            },
            Item::Value(v) => DocItem::Value(v),
            Item::Parameter(p) => DocItem::Parameter {
                name: ValueScalar {
                    text: &p.name,
                    quoted: false,
                    variable: false,
                },
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
    type Node = &'v Value;
    type Scalar = ValueScalar<'v>;
    type Container = ValueContainer<'v>;
    type Items = ValueItems<'v, B>;
    type Scalars = std::iter::Empty<&'v Value>;

    fn view(self, node: Self::Node) -> View<Self> {
        let scalar = |text: &'v str, quoted, variable| ValueScalar {
            text,
            quoted,
            variable,
        };
        let block = |b: &'v Block| ValueContainer {
            items: &b.items,
            root: false,
            top: false,
        };
        match node {
            Value::Unquoted(s) => View::Scalar(scalar(s, false, false)),
            Value::Quoted(s) => View::Scalar(scalar(s, true, false)),
            Value::Variable(s) => View::Scalar(scalar(s, false, true)),
            Value::Calc(s) => View::Calc(scalar(s, false, false)),
            Value::Block(b) => View::Block(block(b)),
            Value::HeaderedBlock(h, b) => View::Headered(scalar(h, false, false), block(b)),
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

    fn len_hint(self, container: Self::Container) -> usize {
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

/// The deserializer for `value`. A block that you deserialize directly
/// acts as the root of a document, so it is always an object.
fn value_de<B>(value: &Value) -> ValueDe<ValueDoc<'_, B>> {
    let doc = ValueDoc::new();
    match value {
        Value::Block(b) => ValueDe::root(
            doc,
            ValueContainer {
                items: &b.items,
                root: true,
                top: true,
            },
        ),
        _ => ValueDe::node(doc, value),
    }
}

macro_rules! forward_value {
    ($lt:lifetime, $mode:ident, $($method:ident ( $($arg:ident : $ty:ty),* ))*) => {$(
        fn $method<V: Visitor<$lt>>(self, $($arg: $ty,)* visitor: V) -> Result<V::Value, Error> {
            Deserializer::$method(value_de::<$mode>(&self), $($arg,)* visitor)
        }
    )*};
}

macro_rules! value_deserializer {
    ($lt:lifetime, $mode:ident) => {
        forward_value! {
            $lt, $mode,
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
impl<'de> Deserializer<'de> for &'de Value {
    type Error = Error;

    value_deserializer!('de, Borrowed);
}

/// Deserialize a value into a type. The data of the type cannot borrow
/// from the value.
impl<'de> Deserializer<'de> for Value {
    type Error = Error;

    value_deserializer!('de, Copied);
}

impl<'de> IntoDeserializer<'de, Error> for &'de Value {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}

impl<'de> IntoDeserializer<'de, Error> for Value {
    type Deserializer = Self;

    fn into_deserializer(self) -> Self {
        self
    }
}
