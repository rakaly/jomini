//! Serde support for [`OwnedValue`]: `Deserialize` and a deserializer.

use super::{OwnedBlock, OwnedField, OwnedItem, OwnedScalar, OwnedValue};
use crate::{
    Scalar,
    text::{
        Operator,
        syntax::{
            TextRange,
            de::{Doc, DocDe, Error, Item as DocItem, VALUE, ValueDe, View},
        },
    },
};
use serde::de::{
    self, Deserialize, DeserializeSeed, Deserializer, EnumAccess, IntoDeserializer, MapAccess,
    SeqAccess, Unexpected, VariantAccess, Visitor, value::BorrowedStrDeserializer,
};
use std::{cell::Cell, fmt, marker::PhantomData};

impl<'de> Deserialize<'de> for OwnedValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_newtype_struct(VALUE, ValueVisitor)
    }
}

/// The variant by which a deserializer of the crate hands a value to
/// [`ValueVisitor`].
const HANDOFF: &str = "$__jomini_private_Handoff";

thread_local! {
    /// The value that a deserializer of the crate hands to [`ValueVisitor`].
    static SLOT: Cell<Slot> = const { Cell::new(Slot::Empty) };
}

/// The state of [`SLOT`].
enum Slot {
    Empty,
    /// [`ValueVisitor`] is about to read the variant of a [`Handoff`]. Only
    /// then does the handoff make a value.
    Waiting,
    /// The value of the handoff, until [`ValueVisitor`] takes it.
    Full(OwnedValue),
}

/// Give the value that `build` makes to the visitor of an [`OwnedValue`].
///
/// A deserializer of the crate calls this when a visitor asks for
/// [`VALUE`]. A block can nest thousands of levels, so the value does not go
/// through serde one level at a time. Instead `build` makes the whole
/// value without recursion, and the value moves to the visitor through a
/// thread-local slot. Only [`ValueVisitor`] takes the value. A different
/// visitor sees an enum variant that it does not know, and reading the
/// variant makes no value.
pub(crate) fn visit_owned<'de, V, E, F>(build: F, visitor: V) -> Result<V::Value, E>
where
    V: Visitor<'de>,
    E: de::Error,
    F: FnOnce() -> OwnedValue,
{
    // A panic in a visitor can leave the slot waiting.
    SLOT.set(Slot::Empty);
    visitor.visit_enum(Handoff {
        build,
        error: PhantomData,
    })
}

/// The enum that hands a value to [`ValueVisitor`]. It has one unit variant,
/// [`HANDOFF`]. The value is made when the visitor reads the variant.
struct Handoff<F, E> {
    build: F,
    error: PhantomData<E>,
}

impl<'de, F: FnOnce() -> OwnedValue, E: de::Error> EnumAccess<'de> for Handoff<F, E> {
    type Error = E;
    type Variant = Self;

    fn variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<(T::Value, Self), E> {
        let tag = seed.deserialize(BorrowedStrDeserializer::new(HANDOFF))?;
        Ok((tag, self))
    }
}

impl<'de, F: FnOnce() -> OwnedValue, E: de::Error> VariantAccess<'de> for Handoff<F, E> {
    type Error = E;

    fn unit_variant(self) -> Result<(), E> {
        if let Slot::Waiting = SLOT.replace(Slot::Empty) {
            SLOT.set(Slot::Full((self.build)()));
        }
        Ok(())
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, _seed: T) -> Result<T::Value, E> {
        Err(de::Error::invalid_type(
            Unexpected::UnitVariant,
            &"a newtype variant",
        ))
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, _visitor: V) -> Result<V::Value, E> {
        Err(de::Error::invalid_type(
            Unexpected::UnitVariant,
            &"a tuple variant",
        ))
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, E> {
        Err(de::Error::invalid_type(
            Unexpected::UnitVariant,
            &"a struct variant",
        ))
    }
}

/// The name of the variant of a [`Handoff`].
struct HandoffTag;

impl<'de> Deserialize<'de> for HandoffTag {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct TagVisitor;

        impl Visitor<'_> for TagVisitor {
            type Value = HandoffTag;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a jomini value")
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<HandoffTag, E> {
                match v {
                    HANDOFF => Ok(HandoffTag),
                    _ => Err(E::unknown_variant(v, &[])),
                }
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

    /// Take the value of a [`Handoff`]. The slot waits for the value only
    /// while the variant is read, and it is empty after, so the value is the
    /// one that this deserializer made.
    fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<OwnedValue, A::Error> {
        let (HandoffTag, variant) = data.variant()?;
        SLOT.set(Slot::Waiting);
        let result = variant.unit_variant();
        let slot = SLOT.replace(Slot::Empty);
        result?;
        match slot {
            Slot::Full(value) => Ok(value),
            Slot::Empty | Slot::Waiting => Err(de::Error::custom(
                "the deserializer did not hand off a value",
            )),
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

/// The model of a value, for the JSON serializer.
#[cfg(feature = "json")]
pub(crate) fn json_doc<'v>() -> ValueDoc<'v, Borrowed> {
    ValueDoc::new()
}

/// A block as the root of a document, for the JSON serializer.
#[cfg(feature = "json")]
pub(crate) fn json_root(block: &OwnedBlock) -> ValueContainer<'_> {
    ValueContainer {
        items: &block.items,
        root: true,
        top: true,
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

    #[cfg(feature = "json")]
    fn with_str<R>(self, scalar: Self::Scalar, f: impl FnOnce(&str) -> R) -> R {
        f(scalar.text)
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        TextDeserializer, Utf8Encoding,
        text::{de::TextTapeDeserializer, syntax},
    };

    /// A value that a visitor other than [`ValueVisitor`] reads when it asks
    /// for [`VALUE`]: the name of a unit variant.
    #[derive(Debug, PartialEq)]
    struct Foreign(String);

    impl<'de> Deserialize<'de> for Foreign {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct ForeignVisitor;

            impl<'de> Visitor<'de> for ForeignVisitor {
                type Value = Foreign;

                fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str("a unit variant")
                }

                fn visit_enum<A: EnumAccess<'de>>(self, data: A) -> Result<Foreign, A::Error> {
                    let (name, variant) = data.variant::<String>()?;
                    variant.unit_variant()?;
                    Ok(Foreign(name))
                }
            }

            deserializer.deserialize_newtype_struct(VALUE, ForeignVisitor)
        }
    }

    fn slot_is_empty() -> bool {
        matches!(SLOT.replace(Slot::Empty), Slot::Empty)
    }

    #[test]
    fn foreign_visitor_sees_an_unknown_unit_variant() {
        let expected = Foreign(String::from(HANDOFF));

        let tree: Foreign = syntax::parse(b"a=1")
            .deserialize(Utf8Encoding::new())
            .unwrap();
        assert_eq!(tree, expected);
        assert!(slot_is_empty());

        let tape: Foreign = TextTapeDeserializer::from_utf8_slice(b"a=1")
            .unwrap()
            .deserialize()
            .unwrap();
        assert_eq!(tape, expected);
        assert!(slot_is_empty());

        let slice: Foreign = TextDeserializer::from_slice(b"a=1", Utf8Encoding::new())
            .deserialize()
            .unwrap();
        assert_eq!(slice, expected);
        assert!(slot_is_empty());

        let reader: Foreign = TextDeserializer::from_reader(&b"a=1"[..], Utf8Encoding::new())
            .deserialize()
            .unwrap();
        assert_eq!(reader, expected);
        assert!(slot_is_empty());

        let value = Foreign::deserialize(&OwnedValue::unquoted("1")).unwrap();
        assert_eq!(value, expected);
        assert!(slot_is_empty());
    }
}
