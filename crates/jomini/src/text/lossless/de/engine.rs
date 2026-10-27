//! The serde rules that the tree deserializer and the [`Value`] deserializer
//! share.
//!
//! A [`Doc`] gives a model of the document: values, scalars, containers, and
//! the entries of a container. The tree and the owned [`Value`] both give
//! this model, so the two deserializers have the same behavior.
//!
//! The rules are those of `TextDeserializer` on a `TextTape`:
//!
//! - A container is an object when its first entry is a field with an
//!   operator, or a parameter. Else the container is an array. The root and
//!   the body of a parameter are always objects.
//! - A map over an object gives the fields in source order, duplicate keys
//!   too. At the first bare value, the map gives one more key, `remainder`,
//!   with the rest of the entries as a sequence.
//! - A sequence over a container gives each part of each entry: a key, an
//!   operator, a value, and the header of a headered block. A sequence over
//!   an object that has bare values starts at the first bare value.
//! - A sequence over a headered block gives the header and then the block.
//! - An empty block is not a value when it is the first entry of a
//!   container, or when it is in an object.
//!
//! [`Value`]: crate::text::lossless::value::Value

use super::error::Error;
use crate::{
    Scalar,
    text::{Operator, lossless::TextRange},
};
use serde::de::{self, DeserializeSeed, IntoDeserializer, Visitor};

/// The serde name that asks for a [`Value`](crate::text::lossless::value::Value).
pub(crate) const VALUE: &str = "$__jomini_private_Value";
/// The serde name of [`Spanned`](super::Spanned).
pub(crate) const SPANNED: &str = "$__jomini_private_Spanned";
/// The serde name of [`MaybeQuoted`](super::MaybeQuoted).
pub(crate) const QUOTED: &str = "$__jomini_private_MaybeQuoted";
/// The serde name of [`WithOp`](super::WithOp).
pub(crate) const WITH_OP: &str = "$__jomini_private_WithOp";
/// The serde name of [`Headered`](super::Headered).
pub(crate) const HEADERED: &str = "$__jomini_private_Headered";
/// The serde name of [`Property`](crate::text::Property).
pub(crate) const PROPERTY: &str = "_internal_jomini_property";

/// A model of a document.
pub(crate) trait Doc: Copy {
    /// A value: a scalar, a block, a headered block, or a calculation.
    type Node: Copy;
    /// A scalar: a key, a header, or a scalar value.
    type Scalar: Copy;
    /// A list of entries: the root, a block, or the body of a parameter.
    type Container: Copy;
    /// The entries of a container in source order.
    type Items: Iterator<Item = Item<Self>> + Clone;
    /// The values of a container that has only scalars. See
    /// [`Doc::scalars`].
    type Scalars: Iterator<Item = Self::Node> + Clone;

    fn view(self, node: Self::Node) -> View<Self>;
    fn items(self, container: Self::Container) -> Self::Items;
    /// Return `true` for the root and the body of a parameter.
    fn always_object(self, container: Self::Container) -> bool;
    /// Return `true` for the root of the document.
    fn is_root(self, container: Self::Container) -> bool;
    fn quoted(self, scalar: Self::Scalar) -> bool;
    /// Return `true` for a `@name` variable.
    fn variable(self, scalar: Self::Scalar) -> bool;
    fn with_scalar<R>(self, scalar: Self::Scalar, f: impl FnOnce(Scalar<'_>) -> R) -> R;
    /// The decoded text of a scalar.
    fn string(self, scalar: Self::Scalar) -> String;
    fn node_range(self, node: Self::Node) -> Option<TextRange>;
    fn scalar_range(self, scalar: Self::Scalar) -> Option<TextRange>;
    fn container_range(self, container: Self::Container) -> Option<TextRange>;
    /// The number of entries of a container, or a number a little larger.
    /// A map or a sequence gives it to serde as a size hint, so that a
    /// collection can allocate one time.
    fn len_hint(self, container: Self::Container) -> usize;
    /// The values of a block that can have only scalar values, such as
    /// `{ 1 2 3 }`. Each entry of such a block is a part of its sequence, so
    /// a sequence can skip the general rules. Return `None` for a container
    /// that can have different entries.
    fn scalars(self, container: Self::Container) -> Option<Self::Scalars>;

    fn is_empty(self, container: Self::Container) -> bool {
        self.items(container).next().is_none()
    }

    /// The text of a value that acts as a scalar: a scalar, a calculation,
    /// or the header of a headered block.
    #[inline]
    fn scalar_of(self, node: Self::Node) -> Option<Self::Scalar> {
        match self.view(node) {
            View::Scalar(s) | View::Calc(s) | View::Headered(s, _) => Some(s),
            View::Block(_) => None,
        }
    }
}

/// A [`Doc`] that can give its scalars to a visitor.
pub(crate) trait DocDe<'de>: Doc {
    fn visit_str<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error>;
    fn visit_bytes<V: Visitor<'de>>(
        self,
        scalar: Self::Scalar,
        visitor: V,
    ) -> Result<V::Value, Error>;
}

/// What a value is.
#[derive(Clone, Copy)]
pub(crate) enum View<D: Doc> {
    Scalar(D::Scalar),
    /// A `@[ ... ]` calculation. The scalar is its source text.
    Calc(D::Scalar),
    Block(D::Container),
    Headered(D::Scalar, D::Container),
}

/// An entry of a container.
#[derive(Clone, Copy)]
pub(crate) enum Item<D: Doc> {
    Field {
        key: D::Scalar,
        op: Option<Operator>,
        value: D::Node,
    },
    Value(D::Node),
    Parameter {
        name: D::Scalar,
        undefined: bool,
        body: D::Container,
    },
}

/// The entries of a container from an entry: an optional first entry,
/// then the rest.
pub(crate) struct Rest<D: Doc> {
    head: Option<Item<D>>,
    items: D::Items,
}

impl<D: Doc> Clone for Rest<D> {
    fn clone(&self) -> Self {
        Rest {
            head: self.head,
            items: self.items.clone(),
        }
    }
}

impl<D: Doc> Rest<D> {
    fn new(items: D::Items) -> Self {
        Rest { head: None, items }
    }
}

impl<D: Doc> Iterator for Rest<D> {
    type Item = Item<D>;

    #[inline]
    fn next(&mut self) -> Option<Item<D>> {
        match self.head.take() {
            Some(item) => Some(item),
            None => self.items.next(),
        }
    }
}

/// The thing that a [`ValueDe`] reads.
#[derive(Clone)]
pub(crate) enum Val<D: Doc> {
    Node(D::Node),
    /// A key or a header that is an element of a sequence.
    Scalar(D::Scalar),
    /// A fixed text: an operator in a sequence, or an empty parameter.
    Static(&'static str),
    /// The block of a headered block, or the root.
    Container(D::Container),
    /// The entries of a mixed container after the fields.
    Remainder(Box<Flatten<D>>),
}

/// A scalar or a fixed text.
#[derive(Clone, Copy)]
pub(crate) enum Text<D: Doc> {
    Scalar(D::Scalar),
    Static(&'static str),
}

impl<D: Doc> Text<D> {
    fn visit<'de, V: Visitor<'de>>(self, doc: D, visitor: V) -> Result<V::Value, Error>
    where
        D: DocDe<'de>,
    {
        match self {
            Text::Scalar(s) => doc.visit_str(s, visitor),
            Text::Static(s) => visitor.visit_borrowed_str(s),
        }
    }

    fn with_scalar<R>(self, doc: D, f: impl FnOnce(Scalar<'_>) -> R) -> R {
        match self {
            Text::Scalar(s) => doc.with_scalar(s, f),
            Text::Static(s) => f(Scalar::new(s.as_bytes())),
        }
    }
}

fn is_empty_block<D: Doc>(doc: D, item: &Item<D>) -> bool {
    match item {
        Item::Value(node) => matches!(doc.view(*node), View::Block(c) if doc.is_empty(c)),
        _ => false,
    }
}

/// Return `true` when the container is an object.
///
/// Also return the entries from the first entry that is not an empty block.
/// A map and a sequence drop the leading empty blocks, so they can start
/// there.
#[inline]
fn classify<D: Doc>(doc: D, container: D::Container) -> (bool, Rest<D>) {
    let always = doc.always_object(container);
    let mut items = doc.items(container);
    while let Some(item) = items.next() {
        let object = match item {
            Item::Field { op: Some(_), .. } | Item::Parameter { .. } => true,
            ref x if is_empty_block(doc, x) => continue,
            _ => always,
        };
        let rest = Rest {
            head: Some(item),
            items,
        };
        return (object, rest);
    }
    (always, Rest::new(items))
}

/// The value of a parameter: its body, a single scalar, or an empty text.
fn param_val<D: Doc>(doc: D, body: D::Container) -> Val<D> {
    let mut items = doc.items(body);
    match (items.next(), items.next()) {
        (None, _) => Val::Static(""),
        (Some(Item::Value(node)), None) if matches!(doc.view(node), View::Scalar(_)) => {
            Val::Node(node)
        }
        _ => Val::Container(body),
    }
}

/// The parts of the entries of a container, as a sequence gives them.
pub(crate) struct Flatten<D: Doc> {
    doc: D,
    container: D::Container,
    /// The entries from the start of the sequence.
    start: Rest<D>,
    items: Rest<D>,
    /// The container is an object, so an `=` is not a part.
    object: bool,
    /// The container is the root of the document.
    root: bool,
    /// After a bare value in an object, or a field in an array, each
    /// operator is a part.
    mixed: bool,
    /// The mixed mode stays after a child block. `TextTape` keeps this flag
    /// in the open token of the container: a child block that starts in
    /// mixed mode with a scalar sets it. When a child block closes, the
    /// mixed mode comes from this flag again.
    flag: bool,
    /// No entry is a part yet.
    first: bool,
    pending: [Option<Val<D>>; 3],
    next: usize,
}

impl<D: Doc> Clone for Flatten<D> {
    fn clone(&self) -> Self {
        Flatten {
            doc: self.doc,
            container: self.container,
            start: self.start.clone(),
            items: self.items.clone(),
            object: self.object,
            root: self.root,
            mixed: self.mixed,
            flag: self.flag,
            first: self.first,
            pending: self.pending.clone(),
            next: self.next,
        }
    }
}

impl<D: Doc> Flatten<D> {
    fn new(doc: D, container: D::Container, items: Rest<D>, object: bool, mixed: bool) -> Self {
        Flatten {
            doc,
            container,
            start: items.clone(),
            items,
            object,
            root: doc.is_root(container),
            mixed,
            flag: false,
            first: !mixed,
            pending: [None, None, None],
            next: 0,
        }
    }

    /// The sequence of the entries of `container`.
    fn of(doc: D, container: D::Container) -> Self {
        let (object, items) = classify(doc, container);
        Flatten::classified(doc, container, object, items)
    }

    fn classified(doc: D, container: D::Container, object: bool, items: Rest<D>) -> Self {
        if !object {
            return Flatten::new(doc, container, items, false, false);
        }

        // In an object with bare values, the sequence starts at the first
        // bare value.
        let mut scan = items.clone();
        loop {
            let before = scan.clone();
            match scan.next() {
                None => return Flatten::new(doc, container, items, true, false),
                Some(ref item @ Item::Value(_)) if !is_empty_block(doc, item) => {
                    return Flatten::new(doc, container, before, true, true);
                }
                Some(_) => {}
            }
        }
    }

    /// The entries of `container` from `items`, after the fields of an
    /// object.
    fn remainder(doc: D, container: D::Container, items: Rest<D>) -> Self {
        Flatten::new(doc, container, items, true, true)
    }

    /// The size hint of the sequence. Only a sequence over an array has
    /// one, because each entry of an array is one part.
    fn size_hint(&self) -> Option<usize> {
        (!self.object && !self.mixed).then(|| self.doc.len_hint(self.container))
    }

    /// Return `true` when the sequence has no parts.
    fn is_empty(&self) -> bool {
        self.clone().next().is_none()
    }

    fn push(&mut self, val: Val<D>) {
        if let Some(slot) = self.pending.iter_mut().find(|x| x.is_none()) {
            *slot = Some(val);
        }
    }

    /// A child block closes: set the mixed mode again.
    fn close_block(&mut self, block: D::Container) {
        if self.mixed && !self.root {
            let first = self.doc.items(block).next();
            let scalar = match first {
                Some(Item::Field { .. }) => true,
                Some(Item::Value(node)) => {
                    matches!(self.doc.view(node), View::Scalar(_) | View::Calc(_))
                }
                _ => false,
            };
            self.flag |= scalar;
        }
        self.mixed = self.flag;
    }

    /// Push the parts of the value of a field.
    fn push_value(&mut self, node: D::Node) {
        match self.doc.view(node) {
            View::Headered(header, body) => {
                self.push(Val::Scalar(header));
                if self.mixed {
                    self.push(Val::Container(body));
                    self.close_block(body);
                } else if !self.doc.is_empty(body) {
                    self.push(Val::Container(body));
                }
            }
            View::Block(block) => {
                self.push(Val::Node(node));
                if self.mixed {
                    self.close_block(block);
                }
            }
            _ => self.push(Val::Node(node)),
        }
    }
}

impl<D: Doc> Iterator for Flatten<D> {
    type Item = Val<D>;

    fn next(&mut self) -> Option<Val<D>> {
        if self.next < self.pending.len()
            && let Some(val) = self.pending[self.next].take()
        {
            self.next += 1;
            return Some(val);
        }
        self.next = 0;

        loop {
            let item = self.items.next()?;
            match item {
                Item::Value(node) => {
                    let droppable = self.first || (self.object && !self.mixed);
                    if droppable && is_empty_block(self.doc, &item) {
                        continue;
                    }
                    self.first = false;
                    if self.object {
                        self.mixed = true;
                    }
                    // Only the mixed mode can change when a child block closes.
                    if self.mixed
                        && let View::Block(block) = self.doc.view(node)
                    {
                        self.close_block(block);
                    }
                    return Some(Val::Node(node));
                }
                Item::Field { key, op, value } => {
                    if !self.object && op.is_some() {
                        self.mixed = true;
                    }
                    self.first = false;
                    if let Some(op) = op
                        && (self.mixed || op != Operator::Equal)
                    {
                        self.push(Val::Static(op.symbol()));
                    }
                    self.push_value(value);
                    return Some(Val::Scalar(key));
                }
                Item::Parameter { name, body, .. } => {
                    self.first = false;
                    let value = param_val(self.doc, body);
                    if matches!(value, Val::Container(_)) {
                        self.mixed = self.flag;
                    }
                    self.push(value);
                    return Some(Val::Scalar(name));
                }
            }
        }
    }
}

/// The elements of a sequence.
pub(crate) enum Elems<D: Doc> {
    /// The values of a block that has only scalars.
    Scalars(D::Scalars),
    Flatten(Flatten<D>),
    /// A header and its block.
    Pair(std::array::IntoIter<Val<D>, 2>),
}

impl<D: Doc> Elems<D> {
    /// The elements of a block.
    fn of(doc: D, container: D::Container) -> Self {
        match doc.scalars(container) {
            Some(scalars) => Elems::Scalars(scalars),
            None => Elems::Flatten(Flatten::of(doc, container)),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self {
            Elems::Scalars(x) => Some(x.size_hint().0),
            Elems::Flatten(x) => x.size_hint(),
            Elems::Pair(x) => Some(x.len()),
        }
    }
}

impl<D: Doc> Iterator for Elems<D> {
    type Item = Val<D>;

    fn next(&mut self) -> Option<Val<D>> {
        match self {
            Elems::Scalars(x) => x.next().map(Val::Node),
            Elems::Flatten(x) => x.next(),
            Elems::Pair(x) => x.next(),
        }
    }
}

/// A serde deserializer for one value.
pub(crate) struct ValueDe<D: Doc> {
    doc: D,
    val: Val<D>,
    /// The operator of the field that holds the value.
    op: Option<Operator>,
    /// The value is the value of a field.
    field: bool,
}

impl<D: Doc> ValueDe<D> {
    pub(crate) fn root(doc: D, container: D::Container) -> Self {
        ValueDe {
            doc,
            val: Val::Container(container),
            op: None,
            field: false,
        }
    }

    pub(crate) fn node(doc: D, node: D::Node) -> Self {
        ValueDe::new(doc, Val::Node(node))
    }

    fn new(doc: D, val: Val<D>) -> Self {
        ValueDe {
            doc,
            val,
            op: None,
            field: false,
        }
    }

    fn range(&self) -> Option<TextRange> {
        val_range(self.doc, &self.val)
    }

    /// The text of the value, when it acts as a scalar.
    fn text(&self) -> Option<Text<D>> {
        match &self.val {
            Val::Node(node) => self.doc.scalar_of(*node).map(Text::Scalar),
            Val::Scalar(s) => Some(Text::Scalar(*s)),
            Val::Static(s) => Some(Text::Static(s)),
            Val::Container(_) | Val::Remainder(_) => None,
        }
    }

    /// The container of the value for a map or a sequence.
    fn container(&self) -> Option<D::Container> {
        match &self.val {
            Val::Node(node) => match self.doc.view(*node) {
                View::Block(c) => Some(c),
                _ => None,
            },
            Val::Container(c) => Some(*c),
            _ => None,
        }
    }

    /// The elements of the value as a sequence.
    fn elems(&self) -> Option<Elems<D>> {
        match &self.val {
            Val::Node(node) => match self.doc.view(*node) {
                View::Block(c) => Some(Elems::of(self.doc, c)),
                View::Headered(h, c) => {
                    Some(Elems::Pair([Val::Scalar(h), Val::Container(c)].into_iter()))
                }
                _ => None,
            },
            Val::Container(c) => Some(Elems::of(self.doc, *c)),
            Val::Remainder(f) => Some(Elems::Flatten((**f).clone())),
            _ => None,
        }
    }

    fn quoted(&self) -> bool {
        match &self.val {
            Val::Node(node) => match self.doc.view(*node) {
                View::Scalar(s) => self.doc.quoted(s),
                _ => false,
            },
            Val::Scalar(s) => self.doc.quoted(*s),
            _ => false,
        }
    }

    fn container_any<'de, V: Visitor<'de>>(
        self,
        container: D::Container,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        D: DocDe<'de>,
    {
        if let Some(scalars) = self.doc.scalars(container) {
            return visitor.visit_seq(SeqDe::new(self.doc, Elems::Scalars(scalars)));
        }
        let (object, items) = classify(self.doc, container);
        if object {
            visitor.visit_map(MapDe::classified(self.doc, container, object, items))
        } else {
            let flatten = Flatten::classified(self.doc, container, object, items);
            visitor.visit_seq(SeqDe::new(self.doc, Elems::Flatten(flatten)))
        }
    }

    fn with_val(&self, val: Val<D>) -> Self {
        ValueDe {
            doc: self.doc,
            val,
            op: self.op,
            field: self.field,
        }
    }
}

/// The range of a value for an error. The range of a node is calculated
/// only when an error occurs.
enum RangeOf<D: Doc> {
    Node(D::Node),
    Known(Option<TextRange>),
}

impl<D: Doc> RangeOf<D> {
    #[inline]
    fn new(de: &ValueDe<D>) -> Self {
        match &de.val {
            Val::Node(node) => RangeOf::Node(*node),
            _ => RangeOf::Known(de.range()),
        }
    }

    #[cold]
    fn get(self, doc: D) -> Option<TextRange> {
        match self {
            RangeOf::Node(node) => doc.node_range(node),
            RangeOf::Known(range) => range,
        }
    }
}

fn val_range<D: Doc>(doc: D, val: &Val<D>) -> Option<TextRange> {
    match val {
        Val::Node(node) => doc.node_range(*node),
        Val::Scalar(s) => doc.scalar_range(*s),
        Val::Static(_) => None,
        Val::Container(c) => doc.container_range(*c),
        Val::Remainder(f) => {
            let mut items = f.start.clone();
            let first = items.next()?;
            let last = items.last().unwrap_or(first);
            let start = item_range(doc, &first)?;
            let end = item_range(doc, &last)?;
            Some(TextRange::new(start.start(), end.end()))
        }
    }
}

fn item_range<D: Doc>(doc: D, item: &Item<D>) -> Option<TextRange> {
    match item {
        Item::Field { key, value, .. } => {
            let start = doc.scalar_range(*key)?;
            let end = doc.node_range(*value)?;
            Some(TextRange::new(start.start(), end.end()))
        }
        Item::Value(node) => doc.node_range(*node),
        Item::Parameter { body, .. } => doc.container_range(*body),
    }
}

macro_rules! parse_scalar {
    ($self:ident, $visitor:ident, $method:ident, $visit:ident) => {{
        let doc = $self.doc;
        if let Some(text) = $self.text()
            && let Ok(x) = text.with_scalar(doc, |s| s.$method())
        {
            return $visitor.$visit(x);
        }
        $self.deserialize_any($visitor)
    }};
}

impl<'de, D: DocDe<'de>> de::Deserializer<'de> for ValueDe<D> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match &self.val {
            Val::Node(node) => match self.doc.view(*node) {
                View::Scalar(s) | View::Calc(s) => self.doc.visit_str(s, visitor),
                View::Block(c) => self.container_any(c, visitor),
                View::Headered(h, c) => {
                    if self.doc.is_empty(c) {
                        self.doc.visit_str(h, visitor)
                    } else {
                        self.container_any(c, visitor)
                    }
                }
            },
            Val::Scalar(s) => self.doc.visit_str(*s, visitor),
            Val::Static(s) => visitor.visit_borrowed_str(s),
            Val::Container(c) => {
                let c = *c;
                self.container_any(c, visitor)
            }
            Val::Remainder(f) => {
                let elems = Elems::Flatten((**f).clone());
                visitor.visit_seq(SeqDe::new(self.doc, elems))
            }
        }
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        parse_scalar!(self, visitor, to_bool, visit_bool)
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        parse_scalar!(self, visitor, to_i64, visit_i64)
    }

    fn deserialize_i128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_i64(visitor)
    }

    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        parse_scalar!(self, visitor, to_u64, visit_u64)
    }

    fn deserialize_u128<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_u64(visitor)
    }

    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        parse_scalar!(self, visitor, to_f64, visit_f64)
    }

    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.text() {
            Some(text) => text.visit(self.doc, visitor),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.text() {
            Some(Text::Scalar(s)) => self.doc.visit_bytes(s, visitor),
            Some(Text::Static(s)) => visitor.visit_borrowed_bytes(s.as_bytes()),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error> {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error> {
        let doc = self.doc;
        match name {
            VALUE => visitor.visit_enum(ValueEnum { doc, val: self.val }),
            SPANNED => match self.range() {
                Some(range) => visitor.visit_seq(PartsSeq::new(
                    doc,
                    [
                        Part::U32(range.start().to_u32()),
                        Part::U32(range.end().to_u32()),
                        Part::Value(self),
                    ],
                )),
                None => visitor.visit_newtype_struct(self),
            },
            QUOTED => {
                let quoted = self.quoted();
                visitor.visit_seq(PartsSeq::new(doc, [Part::Bool(quoted), Part::Value(self)]))
            }
            WITH_OP => {
                let op = Part::op(self.op);
                visitor.visit_seq(PartsSeq::new(doc, [op, Part::Value(self)]))
            }
            HEADERED => {
                let headered = match &self.val {
                    Val::Node(node) => match self.doc.view(*node) {
                        View::Headered(h, c) => Some((h, c)),
                        _ => None,
                    },
                    _ => None,
                };
                let (header, value) = match headered {
                    Some((h, c)) => (Some(Text::Scalar(h)), self.with_val(Val::Container(c))),
                    None => (None, self),
                };
                visitor.visit_seq(PartsSeq::new(
                    doc,
                    [Part::OptText(header), Part::Value(value)],
                ))
            }
            _ => visitor.visit_newtype_struct(self),
        }
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.elems() {
            Some(elems) => visitor.visit_seq(SeqDe::new(self.doc, elems)),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.container() {
            Some(c) => visitor.visit_map(MapDe::new(self.doc, c)),
            None => self.deserialize_any(visitor),
        }
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error> {
        if name == PROPERTY && self.field {
            let op = self.op.unwrap_or(Operator::Equal);
            let value = ValueDe::new(self.doc, self.val);
            return visitor.visit_map(PropertyMap {
                op,
                value: Some(value),
                state: 0,
            });
        }
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error> {
        match self.elems() {
            Some(mut elems) => {
                let Some(variant) = elems.next() else {
                    return Err(de::Error::custom("expected an enum variant"));
                };
                visitor.visit_enum(EnumDe {
                    doc: self.doc,
                    variant,
                    rest: Some(elems),
                })
            }
            None => visitor.visit_enum(EnumDe {
                doc: self.doc,
                variant: self.val,
                rest: None,
            }),
        }
    }

    fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_unit()
    }
}

/// The fields of a container as a map.
struct MapDe<D: Doc> {
    doc: D,
    container: D::Container,
    items: Rest<D>,
    object: bool,
    done: bool,
    pending: Pending<D>,
}

/// The value for the key that the map gave last.
#[derive(Clone)]
enum Pending<D: Doc> {
    None,
    Field {
        key: D::Scalar,
        op: Option<Operator>,
        value: D::Node,
    },
    /// The value of a parameter, or the remainder.
    Other {
        key: Text<D>,
        val: Val<D>,
    },
}

impl<D: Doc> MapDe<D> {
    fn new(doc: D, container: D::Container) -> Self {
        let (object, items) = classify(doc, container);
        MapDe::classified(doc, container, object, items)
    }

    fn classified(doc: D, container: D::Container, object: bool, items: Rest<D>) -> Self {
        MapDe {
            doc,
            container,
            items,
            object,
            done: false,
            pending: Pending::None,
        }
    }

    fn remainder<'de, K: DeserializeSeed<'de>>(
        &mut self,
        flatten: Flatten<D>,
        seed: K,
    ) -> Result<Option<K::Value>, Error>
    where
        D: DocDe<'de>,
    {
        self.done = true;
        if flatten.is_empty() {
            return Ok(None);
        }
        self.pending = Pending::Other {
            key: Text::Static("remainder"),
            val: Val::Remainder(Box::new(flatten)),
        };
        seed.deserialize(StaticDe("remainder")).map(Some)
    }

    #[cold]
    fn value_error(&self, error: Error, key: Text<D>, range: RangeOf<D>) -> Error {
        let doc = self.doc;
        let key = match key {
            Text::Scalar(s) => doc.string(s),
            Text::Static(s) => s.to_string(),
        };
        error.or_range(range.get(doc)).in_key(key)
    }
}

impl<'de, D: DocDe<'de>> de::MapAccess<'de> for MapDe<D> {
    type Error = Error;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Error> {
        if self.done {
            return Ok(None);
        }

        if !self.object {
            let items = self.items.clone();
            let flatten = Flatten::new(self.doc, self.container, items, false, false);
            return self.remainder(flatten, seed);
        }

        loop {
            let Some(item) = self.items.next() else {
                self.done = true;
                return Ok(None);
            };
            match item {
                Item::Field { key, op, value } => {
                    self.pending = Pending::Field { key, op, value };
                    let de = ValueDe::new(self.doc, Val::Scalar(key));
                    return seed.deserialize(de).map(Some);
                }
                Item::Parameter { name, body, .. } => {
                    self.pending = Pending::Other {
                        key: Text::Scalar(name),
                        val: param_val(self.doc, body),
                    };
                    let de = ValueDe::new(self.doc, Val::Scalar(name));
                    return seed.deserialize(de).map(Some);
                }
                Item::Value(_) if is_empty_block(self.doc, &item) => continue,
                Item::Value(_) => {
                    let mut items = self.items.clone();
                    items.head = Some(item);
                    let flatten = Flatten::remainder(self.doc, self.container, items);
                    return self.remainder(flatten, seed);
                }
            }
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, Error> {
        // A visitor can ask for the value again, as with `TextDeserializer`.
        match self.pending.clone() {
            Pending::Field { key, op, value } => {
                let de = ValueDe {
                    doc: self.doc,
                    val: Val::Node(value),
                    op,
                    field: true,
                };
                seed.deserialize(de)
                    .map_err(|e| self.value_error(e, Text::Scalar(key), RangeOf::Node(value)))
            }
            Pending::Other { key, val } => {
                let de = ValueDe {
                    doc: self.doc,
                    val,
                    op: None,
                    field: true,
                };
                let range = RangeOf::new(&de);
                seed.deserialize(de)
                    .map_err(|e| self.value_error(e, key, range))
            }
            Pending::None => Err(de::Error::custom("a map value without a key")),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.pending {
            Pending::None if self.object && !self.done => Some(self.doc.len_hint(self.container)),
            _ => None,
        }
    }
}

/// The elements of a sequence.
struct SeqDe<D: Doc> {
    doc: D,
    elems: Elems<D>,
    index: usize,
}

impl<D: Doc> SeqDe<D> {
    fn new(doc: D, elems: Elems<D>) -> Self {
        SeqDe {
            doc,
            elems,
            index: 0,
        }
    }
}

impl<'de, D: DocDe<'de>> de::SeqAccess<'de> for SeqDe<D> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Error> {
        let Some(val) = self.elems.next() else {
            return Ok(None);
        };
        let index = self.index;
        self.index += 1;
        let de = ValueDe::new(self.doc, val);
        let range = RangeOf::new(&de);
        let doc = self.doc;
        seed.deserialize(de)
            .map(Some)
            .map_err(|e| e.or_range(range.get(doc)).in_index(index))
    }

    fn size_hint(&self) -> Option<usize> {
        if self.index == 0 {
            self.elems.size_hint()
        } else {
            None
        }
    }
}

/// The map that a [`Property`](crate::text::Property) reads.
struct PropertyMap<D: Doc> {
    op: Operator,
    value: Option<ValueDe<D>>,
    state: u8,
}

impl<'de, D: DocDe<'de>> de::MapAccess<'de> for PropertyMap<D> {
    type Error = Error;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Error> {
        match self.state {
            0 => seed.deserialize(StaticDe("operator")).map(Some),
            1 => seed.deserialize(StaticDe("value")).map(Some),
            _ => Ok(None),
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, Error> {
        self.state += 1;
        if self.state == 1 {
            seed.deserialize(StaticDe(self.op.symbol()))
        } else {
            match self.value.take() {
                Some(value) => seed.deserialize(value),
                None => Err(de::Error::custom("a property has one value")),
            }
        }
    }
}

struct EnumDe<D: Doc> {
    doc: D,
    variant: Val<D>,
    rest: Option<Elems<D>>,
}

impl<'de, D: DocDe<'de>> de::EnumAccess<'de> for EnumDe<D> {
    type Error = Error;
    type Variant = VariantDe<D>;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Error> {
        let variant = seed.deserialize(ValueDe::new(self.doc, self.variant))?;
        Ok((
            variant,
            VariantDe {
                doc: self.doc,
                rest: self.rest,
            },
        ))
    }
}

struct VariantDe<D: Doc> {
    doc: D,
    rest: Option<Elems<D>>,
}

impl<D: Doc> VariantDe<D> {
    fn next(&mut self) -> Result<ValueDe<D>, Error> {
        match self.rest.as_mut().and_then(|x| x.next()) {
            Some(val) => Ok(ValueDe::new(self.doc, val)),
            None => Err(de::Error::custom("expected a value for the enum variant")),
        }
    }
}

impl<'de, D: DocDe<'de>> de::VariantAccess<'de> for VariantDe<D> {
    type Error = Error;

    fn unit_variant(mut self) -> Result<(), Error> {
        if self.rest.is_none() {
            Ok(())
        } else {
            de::Deserialize::deserialize(self.next()?)
        }
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(mut self, seed: T) -> Result<T::Value, Error> {
        seed.deserialize(self.next()?)
    }

    fn tuple_variant<V: Visitor<'de>>(
        mut self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Error> {
        de::Deserializer::deserialize_seq(self.next()?, visitor)
    }

    fn struct_variant<V: Visitor<'de>>(
        mut self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Error> {
        de::Deserializer::deserialize_struct(self.next()?, "", fields, visitor)
    }
}

/// A deserializer for a fixed text.
pub(crate) struct StaticDe(pub(crate) &'static str);

impl<'de> de::Deserializer<'de> for StaticDe {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_borrowed_str(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// A deserializer for the text of a scalar.
struct TextDe<D: Doc> {
    doc: D,
    text: Text<D>,
}

impl<'de, D: DocDe<'de>> de::Deserializer<'de> for TextDe<D> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.text.visit(self.doc, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// A deserializer for an optional text, such as an operator or a header.
struct OptionDe<D: Doc> {
    doc: D,
    text: Option<Text<D>>,
}

impl<'de, D: DocDe<'de>> de::Deserializer<'de> for OptionDe<D> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        self.deserialize_option(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        match self.text {
            None => visitor.visit_none(),
            Some(text) => visitor.visit_some(TextDe {
                doc: self.doc,
                text,
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// One part of the private sequence that a wrapper type or a value reads.
enum Part<D: Doc> {
    U32(u32),
    Bool(bool),
    OptText(Option<Text<D>>),
    Text(Text<D>),
    Entries(Rest<D>),
    Value(ValueDe<D>),
}

impl<D: Doc> Part<D> {
    fn op(op: Option<Operator>) -> Self {
        Part::OptText(op.map(|op| Text::Static(op.symbol())))
    }
}

/// A private sequence of parts.
struct PartsSeq<D: Doc> {
    doc: D,
    parts: std::array::IntoIter<Option<Part<D>>, 5>,
}

impl<D: Doc> PartsSeq<D> {
    fn new<const N: usize>(doc: D, parts: [Part<D>; N]) -> Self {
        let mut all = [None, None, None, None, None];
        for (slot, part) in all.iter_mut().zip(parts) {
            *slot = Some(part);
        }
        PartsSeq {
            doc,
            parts: all.into_iter(),
        }
    }
}

impl<'de, D: DocDe<'de>> de::SeqAccess<'de> for PartsSeq<D> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Error> {
        let Some(Some(part)) = self.parts.next() else {
            return Ok(None);
        };
        let doc = self.doc;
        let value = match part {
            Part::U32(x) => seed.deserialize(IntoDeserializer::<Error>::into_deserializer(x)),
            Part::Bool(x) => seed.deserialize(IntoDeserializer::<Error>::into_deserializer(x)),
            Part::OptText(text) => seed.deserialize(OptionDe { doc, text }),
            Part::Text(text) => seed.deserialize(TextDe { doc, text }),
            Part::Entries(items) => seed.deserialize(EntriesDe { doc, items }),
            Part::Value(value) => seed.deserialize(value),
        };
        value.map(Some)
    }
}

/// The private enum that a [`Value`](crate::text::lossless::value::Value)
/// reads.
struct ValueEnum<D: Doc> {
    doc: D,
    val: Val<D>,
}

pub(crate) const TAG_UNQUOTED: &str = "unquoted";
pub(crate) const TAG_QUOTED: &str = "quoted";
pub(crate) const TAG_VARIABLE: &str = "variable";
pub(crate) const TAG_CALC: &str = "calc";
pub(crate) const TAG_BLOCK: &str = "block";
pub(crate) const TAG_HEADERED: &str = "headered";
pub(crate) const TAG_FIELD: &str = "field";
pub(crate) const TAG_VALUE: &str = "value";
pub(crate) const TAG_PARAMETER: &str = "parameter";

impl<'de, D: DocDe<'de>> de::EnumAccess<'de> for ValueEnum<D> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Error> {
        let doc = self.doc;
        let scalar_tag = |s| {
            if doc.quoted(s) {
                TAG_QUOTED
            } else if doc.variable(s) {
                TAG_VARIABLE
            } else {
                TAG_UNQUOTED
            }
        };
        let tag = match &self.val {
            Val::Node(node) => match doc.view(*node) {
                View::Scalar(s) => scalar_tag(s),
                View::Calc(_) => TAG_CALC,
                View::Block(_) => TAG_BLOCK,
                View::Headered(..) => TAG_HEADERED,
            },
            Val::Scalar(s) => scalar_tag(*s),
            Val::Static(_) => TAG_UNQUOTED,
            Val::Container(_) | Val::Remainder(_) => TAG_BLOCK,
        };
        let tag = seed.deserialize(StaticDe(tag))?;
        Ok((tag, self))
    }
}

impl<'de, D: DocDe<'de>> de::VariantAccess<'de> for ValueEnum<D> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Err(de::Error::custom("a value is not a unit variant"))
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, Error> {
        let doc = self.doc;
        match self.val {
            Val::Node(node) => match doc.view(node) {
                View::Scalar(s) | View::Calc(s) => seed.deserialize(TextDe {
                    doc,
                    text: Text::Scalar(s),
                }),
                View::Block(c) => seed.deserialize(EntriesDe {
                    doc,
                    items: Rest::new(doc.items(c)),
                }),
                View::Headered(..) => Err(de::Error::custom("a headered block has two parts")),
            },
            Val::Scalar(s) => seed.deserialize(TextDe {
                doc,
                text: Text::Scalar(s),
            }),
            Val::Static(s) => seed.deserialize(StaticDe(s)),
            Val::Container(c) => seed.deserialize(EntriesDe {
                doc,
                items: Rest::new(doc.items(c)),
            }),
            Val::Remainder(f) => seed.deserialize(EntriesDe {
                doc,
                items: f.start.clone(),
            }),
        }
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, Error> {
        let doc = self.doc;
        match self.val {
            Val::Node(node) => match doc.view(node) {
                View::Headered(h, c) => visitor.visit_seq(PartsSeq::new(
                    doc,
                    [
                        Part::Text(Text::Scalar(h)),
                        Part::Entries(Rest::new(doc.items(c))),
                    ],
                )),
                _ => Err(de::Error::custom("only a headered block has two parts")),
            },
            _ => Err(de::Error::custom("only a headered block has two parts")),
        }
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error> {
        Err(de::Error::custom("a value is not a struct variant"))
    }
}

/// The entries of a container for a value.
struct EntriesDe<D: Doc> {
    doc: D,
    items: Rest<D>,
}

impl<'de, D: DocDe<'de>> de::Deserializer<'de> for EntriesDe<D> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

impl<'de, D: DocDe<'de>> de::SeqAccess<'de> for EntriesDe<D> {
    type Error = Error;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Error> {
        match self.items.next() {
            Some(item) => seed
                .deserialize(EntryDe {
                    doc: self.doc,
                    item,
                })
                .map(Some),
            None => Ok(None),
        }
    }
}

/// One entry of a container for a value.
struct EntryDe<D: Doc> {
    doc: D,
    item: Item<D>,
}

impl<'de, D: DocDe<'de>> de::Deserializer<'de> for EntryDe<D> {
    type Error = Error;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Error> {
        visitor.visit_enum(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

impl<'de, D: DocDe<'de>> de::EnumAccess<'de> for EntryDe<D> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, Self::Variant), Error> {
        let tag = match self.item {
            Item::Field { .. } => TAG_FIELD,
            Item::Value(_) => TAG_VALUE,
            Item::Parameter { .. } => TAG_PARAMETER,
        };
        let tag = seed.deserialize(StaticDe(tag))?;
        Ok((tag, self))
    }
}

impl<'de, D: DocDe<'de>> de::VariantAccess<'de> for EntryDe<D> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Error> {
        Err(de::Error::custom("an entry is not a unit variant"))
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(self, seed: T) -> Result<T::Value, Error> {
        match self.item {
            Item::Value(node) => seed.deserialize(ValueDe::new(self.doc, Val::Node(node))),
            _ => Err(de::Error::custom("this entry has more than one part")),
        }
    }

    fn tuple_variant<V: Visitor<'de>>(self, _len: usize, visitor: V) -> Result<V::Value, Error> {
        let doc = self.doc;
        match self.item {
            Item::Field { key, op, value } => {
                let value = ValueDe {
                    doc,
                    val: Val::Node(value),
                    op,
                    field: true,
                };
                let range = value.range();
                let parts = [
                    Part::Text(Text::Scalar(key)),
                    Part::Bool(doc.quoted(key)),
                    Part::Bool(doc.variable(key)),
                    Part::op(op),
                    Part::Value(value),
                ];
                visitor
                    .visit_seq(PartsSeq::new(doc, parts))
                    .map_err(|e| e.or_range(range).in_key(doc.string(key)))
            }
            Item::Parameter {
                name,
                undefined,
                body,
            } => visitor.visit_seq(PartsSeq::new(
                doc,
                [
                    Part::Text(Text::Scalar(name)),
                    Part::Bool(undefined),
                    Part::Entries(Rest::new(doc.items(body))),
                ],
            )),
            Item::Value(_) => Err(de::Error::custom("a bare value has one part")),
        }
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Error> {
        Err(de::Error::custom("an entry is not a struct variant"))
    }
}
