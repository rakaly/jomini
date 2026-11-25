//! JSON from the document models of [`crate::text::syntax`].

use super::{DuplicateKeyMode, JsonOptions, TypeNarrowing};
use crate::{
    Encoding, Utf8Encoding,
    text::{
        Operator,
        syntax::{
            OwnedBlock, OwnedValue,
            de::{Doc, Document, Item, Rest, TreeNode, View, classify, is_decimal, is_empty_block},
            value::{Borrowed, ValueDoc, json_doc, json_root},
        },
    },
};
use std::{collections::HashMap, fmt, io};

/// The largest integer `n` such that a 64-bit float has each integer up to
/// `n + 1`.
const MAX_SAFE_INTEGER: u64 = (1 << 53) - 1;

/// The number of bytes in the buffer above which they go to the writer.
/// The check is after each entry, so the buffer also holds the escaped text of
/// one entry: its key, its header, and its scalar.
const CHUNK: usize = 16 * 1024;

/// The deepest level that pretty printing indents. With no limit, the size of
/// the output is quadratic in the depth.
const MAX_INDENT: usize = 128;

/// The number of groups above which a map finds the group of a key.
const SCAN_GROUPS: usize = 16;

/// Creates JSON from a [`Document`], an [`OwnedBlock`], or an [`OwnedValue`].
///
/// ```
/// use jomini::{
///     Utf8Encoding,
///     json::{DuplicateKeyMode, JsonOptions},
///     text::syntax::{OwnedBlock, parse},
/// };
///
/// let mut block: OwnedBlock = parse(b"core = FRA core = BRI tax > 3")
///     .deserialize(Utf8Encoding::new())?;
/// block.items_mut().pop();
///
/// let options = JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group);
/// let json = block.json().with_options(options).to_string();
/// assert_eq!(json, r#"{"core":["FRA","BRI"]}"#);
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// `E` is the encoding of the document. The builder of an owned block or value
/// has the default, because its text is decoded. The builder is [`Send`] when
/// the encoding is [`Sync`].
///
/// # Rules
///
/// - The root is an object, and a different empty block is `[]`.
/// - A block is an object when its first entry is a field with an operator,
///   or a parameter. Else it is an array, and a field in it is an object of
///   one key. These are the rules of [`de`](crate::text::syntax::de#rules).
/// - In an object, the entries from the first bare value are an array with
///   the key `remainder`.
/// - A headered block, `rgb { 1 2 3 }`, is `{"rgb":[1,2,3]}`. When the block is
///   empty, as in `rank = 2 { }`, it is the scalar.
/// - An operator that is not `=` wraps its value, as in
///   `{"GREATER_THAN":1}`.
/// - The key of a parameter is `[name]` or `[!name]`. Its value is its only
///   scalar, or an empty string when it has no entries, or an object.
/// - A whole number in the range of the safe integers of JavaScript, 2^53 - 1
///   and below, is a number. A larger one is a string. A decimal is the
///   nearest 64-bit float, or a string when it is too large for one. `1.5f` and
///   `1444.11.11` are strings.
///
/// With [`DuplicateKeyMode::KeyValuePairs`], each container and each
/// construct that JSON does not have is an object with a `type`:
///
/// | Source | JSON |
/// |---|---|
/// | an object | `{"type":"obj","val":[["key",value], ...]}` |
/// | an array | `{"type":"array","val":[value, ...]}` |
/// | a field of an array | `{"type":"field","key":"key","val":value}` |
/// | `key > value` | `["key",{"type":"op","op":"GREATER_THAN","val":value}]` |
/// | `rgb { 1 2 3 }` | `{"type":"header","name":"rgb","val":array}` |
/// | `[[!name] ... ]` | `{"type":"parameter","name":"name","undefined":true,"val":object}` |
/// | the remainder | `{"type":"remainder","val":[value, ...]}` |
///
/// The parameters and the remainder of an object are elements of `val`
/// beside the pairs.
///
/// # What the JSON does not keep
///
/// - Comments, whitespace, quotes (except for the narrowing), an absent
///   operator, and the empty blocks of an object before its remainder. With
///   [`DuplicateKeyMode::KeyValuePairs`], a header with an empty block is kept.
/// - A CK3 reader directive that is not the value of a field. With a
///   resolution, `@:load_variable name` is a bare value: the value of the
///   variable, or the text of the directive when the variable has none.
/// - In the other modes, data can have the same JSON: an operator and a
///   header with its name, a field and a parameter or a remainder with its
///   key, and, with [`DuplicateKeyMode::Group`], a repeated key and an array.
///   [`DuplicateKeyMode::Preserve`] can repeat a key, and many JSON readers
///   keep only the last one.
/// - The syntax errors. See [`Document::check`].
///
/// A block can nest as deep as the parser allows. Some JSON readers have a
/// lower limit: `serde_json` reads 127 levels by default. Pretty printing
/// indents at most 128 levels.
///
/// [`Document`]: crate::text::syntax::de::Document
/// [`Document::check`]: crate::text::syntax::de::Document::check
#[must_use]
pub struct JsonBuilder<'v, E = Utf8Encoding> {
    source: Source<'v, E>,
    options: JsonOptions,
}

enum Source<'v, E> {
    Block(&'v OwnedBlock),
    Value(&'v OwnedValue),
    /// The root, or one value.
    Document(&'v Document<'v, 'v, E>, Option<TreeNode>),
}

// A derive would need an encoding that is `Copy`.
impl<E> Clone for JsonBuilder<'_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for JsonBuilder<'_, E> {}

impl<E> Clone for Source<'_, E> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<E> Copy for Source<'_, E> {}

/// The source can be large and can nest deep, so it is not in the output.
impl<E> fmt::Debug for JsonBuilder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("JsonBuilder")
            .field("options", &self.options)
            .finish_non_exhaustive()
    }
}

impl<'v, E> JsonBuilder<'v, E> {
    fn new(source: Source<'v, E>) -> Self {
        JsonBuilder {
            source,
            options: JsonOptions::default(),
        }
    }

    pub(crate) fn document(document: &'v Document<'v, 'v, E>, node: Option<TreeNode>) -> Self {
        JsonBuilder::new(Source::Document(document, node))
    }

    /// Output JSON with the set of options
    pub fn with_options(mut self, options: JsonOptions) -> Self {
        self.options = options;
        self
    }
}

impl<E: Encoding> JsonBuilder<'_, E> {
    /// Output JSON to the given writer. The output is buffered. An error is
    /// always an error of the writer, which can already have a part of the
    /// JSON.
    pub fn to_writer<W: io::Write>(&self, mut writer: W) -> io::Result<()> {
        self.write(Some(&mut writer)).map(|_| ())
    }

    /// Output JSON to vec that contains UTF-8 data
    pub fn to_vec(&self) -> Vec<u8> {
        self.write(None).expect("a vector has no write errors")
    }

    fn write(&self, sink: Option<&mut dyn io::Write>) -> io::Result<Vec<u8>> {
        let mut out = Out {
            buf: Vec::new(),
            sink,
            pretty: self.options.pretty,
            indent: 0,
            has_value: false,
        };
        match self.source {
            Source::Block(block) => {
                write_owned(Start::Container(json_root(block)), self.options, &mut out)?;
            }
            Source::Value(value) => write_owned(Start::Node(value), self.options, &mut out)?,
            Source::Document(document, node) => {
                document.write_json(node, self.options, &mut out)?;
            }
        }
        out.drain()?;
        Ok(out.buf)
    }
}

/// Write the JSON, also for `to_string`. The alternate form, `{:#}`, is pretty
/// printed with any options.
///
/// ```
/// use jomini::{Utf8Encoding, text::syntax::{OwnedBlock, parse}};
///
/// let block: OwnedBlock = parse(b"tax = 3").deserialize(Utf8Encoding::new())?;
/// assert_eq!(format!("block: {}", block.json()), r#"block: {"tax":3}"#);
/// assert_eq!(format!("{:#}", block.json()), "{\n  \"tax\": 3\n}");
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
impl<E: Encoding> fmt::Display for JsonBuilder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut options = self.options;
        options.pretty |= f.alternate();
        self.with_options(options)
            .to_writer(FormatterWriter(f))
            .map_err(|_| fmt::Error)
    }
}

/// Gives the output to a formatter. Each write must have whole characters, as
/// each write of [`Out`] has.
struct FormatterWriter<'f, 'a>(&'f mut fmt::Formatter<'a>);

impl io::Write for FormatterWriter<'_, '_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let text = std::str::from_utf8(buf).map_err(io::Error::other)?;
        self.0.write_str(text).map_err(io::Error::other)?;
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl OwnedBlock {
    /// Convert the block to JSON as the root of a document, as it
    /// deserializes. For the JSON of an inner block, use
    /// [`OwnedValue::json`] of the value that has it.
    ///
    /// ```
    /// use jomini::{Utf8Encoding, text::syntax::{OwnedBlock, parse}};
    ///
    /// let root: OwnedBlock = parse(b"a = { 1 2 }").deserialize(Utf8Encoding::new())?;
    /// assert_eq!(root.json().to_string(), r#"{"a":[1,2]}"#);
    /// assert_eq!(root.get("a").unwrap().json().to_string(), "[1,2]");
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn json(&self) -> JsonBuilder<'_> {
        JsonBuilder::new(Source::Block(self))
    }
}

impl OwnedValue {
    /// Convert the value to JSON. See [`JsonBuilder`].
    pub fn json(&self) -> JsonBuilder<'_> {
        JsonBuilder::new(Source::Value(self))
    }
}

/// The JSON text. It does not depend on the document model or the type of
/// the writer.
pub(crate) struct Out<'w> {
    buf: Vec<u8>,
    sink: Option<&'w mut dyn io::Write>,
    pretty: bool,
    indent: usize,
    /// The open container has an element.
    has_value: bool,
}

impl Out<'_> {
    /// Write the buffer to the sink.
    fn drain(&mut self) -> io::Result<()> {
        if let Some(sink) = &mut self.sink {
            sink.write_all(&self.buf)?;
            self.buf.clear();
        }
        Ok(())
    }

    /// The buffer must go to the sink.
    fn is_full(&self) -> bool {
        self.buf.len() >= CHUNK && self.sink.is_some()
    }

    fn drain_chunk(&mut self) -> io::Result<()> {
        if self.is_full() {
            self.drain()?;
        }
        Ok(())
    }

    fn begin(&mut self, open: u8) {
        self.buf.push(open);
        self.indent += 1;
        self.has_value = false;
    }

    fn end(&mut self, close: u8) {
        self.indent -= 1;
        if self.has_value {
            self.newline();
        }
        self.buf.push(close);
        self.has_value = true;
    }

    /// Start an element of a container.
    fn element(&mut self) {
        if self.has_value {
            self.buf.push(b',');
        }
        self.newline();
    }

    fn newline(&mut self) {
        if self.pretty {
            self.buf.push(b'\n');
            self.buf
                .resize(self.buf.len() + 2 * self.indent.min(MAX_INDENT), b' ');
        }
    }

    fn key(&mut self, key: &str) {
        self.element();
        self.string(key);
        self.colon();
    }

    fn colon(&mut self) {
        self.buf
            .extend_from_slice(if self.pretty { b": " } else { b":" });
    }

    fn string(&mut self, text: &str) {
        self.scalar(text);
    }

    fn scalar<T: serde::Serialize + ?Sized>(&mut self, value: &T) {
        serde_json::to_writer(&mut self.buf, value).expect("a vector has no write errors");
        self.has_value = true;
    }

    /// Begin an object that has a `type`.
    fn typed(&mut self, name: &str) {
        self.begin(b'{');
        self.key("type");
        self.string(name);
    }
}

/// The thing that becomes the whole JSON.
pub(crate) enum Start<D: Doc> {
    Node(D::Node),
    Container(D::Container),
}

/// Write the JSON of `start` with no recursion, as a block can nest as deep
/// as the parser allows.
pub(crate) fn write_json<D: Doc>(
    doc: D,
    start: Start<D>,
    options: JsonOptions,
    out: &mut Out<'_>,
) -> io::Result<()> {
    let mut writer = Writer {
        doc,
        options,
        out,
        open: Vec::new(),
    };
    match start {
        Start::Node(node) => writer.node(node),
        Start::Container(container) => writer.container(container),
    }
    writer.finish()
}

/// Write the JSON of an owned value. It does not depend on the encoding, so it
/// is compiled in this crate.
fn write_owned(
    start: Start<ValueDoc<'_, Borrowed>>,
    options: JsonOptions,
    out: &mut Out<'_>,
) -> io::Result<()> {
    write_json(json_doc(), start, options, out)
}

struct Writer<'o, 'w, D: Doc> {
    doc: D,
    options: JsonOptions,
    out: &'o mut Out<'w>,
    /// The rest of the output. The last one is next.
    open: Vec<Open<D>>,
}

/// A container with output that remains.
enum Open<D: Doc> {
    /// Entries of an object, with [`DuplicateKeyMode::Preserve`].
    Fields(Rest<D>),
    /// Entries of an object, with [`DuplicateKeyMode::Group`].
    Groups(std::vec::IntoIter<Group<D>>),
    /// Values of one group.
    Values(std::vec::IntoIter<Part<D>>),
    /// Entries of an object, with [`DuplicateKeyMode::KeyValuePairs`].
    Pairs(Rest<D>),
    /// Entries of an array.
    Elements(Rest<D>),
    /// The delimiter that closes a container.
    End(u8),
}

/// The value of an entry of an object.
enum Part<D: Doc> {
    Node(D::Node),
    Operator(Operator, D::Node),
    Parameter(D::Container),
    Remainder(Rest<D>),
}

/// The values of a key.
struct Group<D: Doc> {
    key: String,
    first: Part<D>,
    rest: Vec<Part<D>>,
}

fn field_part<D: Doc>(op: Option<Operator>, value: D::Node) -> Part<D> {
    match op {
        Some(op) if op != Operator::Equal => Part::Operator(op, value),
        _ => Part::Node(value),
    }
}

fn parameter_key(name: &str, undefined: bool) -> String {
    let not = if undefined { "!" } else { "" };
    format!("[{not}{name}]")
}

impl<D: Doc> Writer<'_, '_, D> {
    fn finish(mut self) -> io::Result<()> {
        while let Some(open) = self.open.pop() {
            match open {
                Open::Fields(items) => self.fields(items),
                Open::Groups(mut groups) => {
                    if let Some(group) = groups.next() {
                        self.open.push(Open::Groups(groups));
                        self.group(group);
                    }
                }
                Open::Values(mut values) => {
                    if let Some(value) = values.next() {
                        self.open.push(Open::Values(values));
                        self.out.element();
                        self.part(value);
                    }
                }
                Open::Pairs(items) => self.pairs(items),
                Open::Elements(items) => self.elements(items),
                Open::End(close) => self.out.end(close),
            }
            self.out.drain_chunk()?;
        }
        Ok(())
    }

    /// Return `true` with [`DuplicateKeyMode::KeyValuePairs`].
    fn tagged(&self) -> bool {
        self.options.duplicate_keys == DuplicateKeyMode::KeyValuePairs
    }

    /// Begin a container. Its end is after the output that the caller then
    /// opens.
    fn begin(&mut self, open: u8, close: u8) {
        self.out.begin(open);
        self.open.push(Open::End(close));
    }

    /// Begin a tagged object.
    fn begin_typed(&mut self, name: &str) {
        self.out.typed(name);
        self.open.push(Open::End(b'}'));
    }

    fn key(&mut self, key: D::Scalar) {
        self.out.element();
        self.text(key);
        self.out.colon();
    }

    fn text(&mut self, scalar: D::Scalar) {
        self.doc.with_str(scalar, |s| self.out.string(s));
    }

    fn scalar(&mut self, scalar: D::Scalar) {
        let narrow = match self.options.type_narrowing {
            TypeNarrowing::All => true,
            TypeNarrowing::Unquoted => !self.doc.quoted(scalar),
            TypeNarrowing::None => false,
        };
        self.doc.with_str(scalar, |text| {
            match narrow.then(|| narrow_text(text)).flatten() {
                Some(Narrowed::Bool(x)) => self.out.scalar(&x),
                Some(Narrowed::Integer(x)) => self.out.scalar(&x),
                Some(Narrowed::Float(x)) => self.out.scalar(&x),
                None => self.out.string(text),
            }
        });
    }

    fn node(&mut self, node: D::Node) {
        match self.doc.view(node) {
            View::Scalar(s) => self.scalar(s),
            View::Calc(s) => self.text(s),
            View::Block(container) => self.container(container),
            View::Headered(header, container) if self.tagged() => {
                self.begin_typed("header");
                self.out.key("name");
                self.text(header);
                self.out.key("val");
                self.container(container);
            }
            // This is the scalar for the deserializers too.
            View::Headered(header, container) if self.doc.is_empty(container) => {
                self.scalar(header);
            }
            View::Headered(header, container) => {
                self.begin(b'{', b'}');
                self.key(header);
                self.container(container);
            }
        }
    }

    fn container(&mut self, container: D::Container) {
        match classify(self.doc, container) {
            (true, items) => self.object(items),
            (false, items) => self.array(items),
        }
    }

    fn object(&mut self, items: Rest<D>) {
        match self.options.duplicate_keys {
            DuplicateKeyMode::Preserve => {
                self.begin(b'{', b'}');
                self.open.push(Open::Fields(items));
            }
            DuplicateKeyMode::Group => {
                let groups = self.groups(items);
                self.begin(b'{', b'}');
                self.open.push(Open::Groups(groups.into_iter()));
            }
            DuplicateKeyMode::KeyValuePairs => {
                self.begin_typed("obj");
                self.out.key("val");
                self.begin(b'[', b']');
                self.open.push(Open::Pairs(items));
            }
        }
    }

    fn array(&mut self, items: Rest<D>) {
        if self.tagged() {
            self.begin_typed("array");
            self.out.key("val");
        }
        self.sequence(items);
    }

    /// The entries as an array with no tag.
    fn sequence(&mut self, items: Rest<D>) {
        self.begin(b'[', b']');
        self.open.push(Open::Elements(items));
    }

    fn part(&mut self, part: Part<D>) {
        match part {
            Part::Node(node) => self.node(node),
            Part::Operator(op, node) if self.tagged() => {
                self.begin_typed("op");
                self.out.key("op");
                self.out.string(op.name());
                self.out.key("val");
                self.node(node);
            }
            Part::Operator(op, node) => {
                self.begin(b'{', b'}');
                self.out.key(op.name());
                self.node(node);
            }
            Part::Parameter(body) => self.parameter(body),
            Part::Remainder(items) => self.sequence(items),
        }
    }

    /// The value of a parameter: an empty text, its only scalar, or its body.
    fn parameter(&mut self, body: D::Container) {
        let mut items = self.doc.items(body);
        match (items.next(), items.next()) {
            (None, _) => self.out.string(""),
            (Some(Item::Value(node)), None)
                if matches!(self.doc.view(node), View::Scalar(_) | View::Calc(_)) =>
            {
                self.node(node);
            }
            _ => self.container(body),
        }
    }

    /// Write entries of an object up to one that opens a container, or until
    /// the buffer is full.
    fn fields(&mut self, mut items: Rest<D>) {
        while let Some(item) = items.next() {
            match item {
                Item::Field { key, op, value } => {
                    self.key(key);
                    if matches!(op, None | Some(Operator::Equal))
                        && let View::Scalar(scalar) = self.doc.view(value)
                    {
                        self.scalar(scalar);
                        if self.out.is_full() {
                            return self.open.push(Open::Fields(items));
                        }
                        continue;
                    }
                    self.open.push(Open::Fields(items));
                    return self.part(field_part(op, value));
                }
                Item::Parameter {
                    name,
                    undefined,
                    body,
                } => {
                    let key = self.doc.with_str(name, |s| parameter_key(s, undefined));
                    self.out.key(&key);
                    self.open.push(Open::Fields(items));
                    return self.parameter(body);
                }
                // An empty block is not a value in an object.
                item if is_empty_block(self.doc, &item) => {}
                item => {
                    items.put_back(item);
                    self.out.key("remainder");
                    return self.sequence(items);
                }
            }
        }
    }

    fn groups(&self, mut items: Rest<D>) -> Vec<Group<D>> {
        let mut groups: Vec<Group<D>> = Vec::new();
        let mut index: HashMap<String, usize> = HashMap::new();
        let mut add = |key: String, part: Part<D>| {
            let found = if groups.len() <= SCAN_GROUPS {
                groups.iter().position(|group| group.key == key)
            } else {
                if index.is_empty() {
                    let keys = groups.iter().map(|group| group.key.clone());
                    index.extend(keys.zip(0..));
                }
                index.get(&key).copied()
            };
            match found {
                Some(at) => groups[at].rest.push(part),
                None => {
                    if !index.is_empty() {
                        index.insert(key.clone(), groups.len());
                    }
                    groups.push(Group {
                        key,
                        first: part,
                        rest: Vec::new(),
                    });
                }
            }
        };
        while let Some(item) = items.next() {
            match item {
                Item::Field { key, op, value } => {
                    add(self.doc.string(key), field_part(op, value));
                }
                Item::Parameter {
                    name,
                    undefined,
                    body,
                } => {
                    let key = self.doc.with_str(name, |s| parameter_key(s, undefined));
                    add(key, Part::Parameter(body));
                }
                item if is_empty_block(self.doc, &item) => {}
                item => {
                    items.put_back(item);
                    add("remainder".to_string(), Part::Remainder(items));
                    break;
                }
            }
        }
        groups
    }

    fn group(&mut self, group: Group<D>) {
        self.out.key(&group.key);
        if group.rest.is_empty() {
            return self.part(group.first);
        }
        self.begin(b'[', b']');
        self.open.push(Open::Values(group.rest.into_iter()));
        self.out.element();
        self.part(group.first);
    }

    /// Write one entry of an object in its tagged form.
    fn pairs(&mut self, mut items: Rest<D>) {
        while let Some(item) = items.next() {
            match item {
                Item::Field { key, op, value } => {
                    self.open.push(Open::Pairs(items));
                    self.out.element();
                    self.begin(b'[', b']');
                    self.out.element();
                    self.text(key);
                    self.out.element();
                    return self.part(field_part(op, value));
                }
                Item::Parameter { .. } => {
                    self.open.push(Open::Pairs(items));
                    self.out.element();
                    return self.single(item);
                }
                item if is_empty_block(self.doc, &item) => {}
                item => {
                    items.put_back(item);
                    self.out.element();
                    self.begin_typed("remainder");
                    self.out.key("val");
                    return self.sequence(items);
                }
            }
        }
    }

    /// Write entries of an array up to one that opens a container, or until
    /// the buffer is full.
    fn elements(&mut self, mut items: Rest<D>) {
        while let Some(item) = items.next() {
            self.out.element();
            if let Item::Value(node) = item
                && let View::Scalar(scalar) = self.doc.view(node)
            {
                self.scalar(scalar);
                if self.out.is_full() {
                    return self.open.push(Open::Elements(items));
                }
                continue;
            }
            self.open.push(Open::Elements(items));
            return self.single(item);
        }
    }

    /// An entry that is not in an object.
    fn single(&mut self, item: Item<D>) {
        match item {
            Item::Value(node) => self.node(node),
            Item::Field { key, op, value } => {
                if self.tagged() {
                    self.begin_typed("field");
                    self.out.key("key");
                    self.text(key);
                    self.out.key("val");
                } else {
                    self.begin(b'{', b'}');
                    self.key(key);
                }
                self.part(field_part(op, value));
            }
            Item::Parameter {
                name,
                undefined,
                body,
            } => {
                if self.tagged() {
                    self.begin_typed("parameter");
                    self.out.key("name");
                    self.text(name);
                    self.out.key("undefined");
                    self.out.scalar(&undefined);
                    self.out.key("val");
                    self.container(body);
                } else {
                    let key = self.doc.with_str(name, |s| parameter_key(s, undefined));
                    self.begin(b'{', b'}');
                    self.out.key(&key);
                    self.parameter(body);
                }
            }
        }
    }
}

enum Narrowed {
    Bool(bool),
    Integer(i64),
    Float(f64),
}

/// Narrow the decoded text of a scalar. Each document model has the same
/// decoded text, so it gives the same JSON.
fn narrow_text(text: &str) -> Option<Narrowed> {
    match text {
        "yes" => return Some(Narrowed::Bool(true)),
        "no" => return Some(Narrowed::Bool(false)),
        _ => {}
    }
    if !is_decimal(text.as_bytes()) {
        return None;
    }
    if text.contains('.') {
        let float: f64 = text.parse().ok()?;
        return float.is_finite().then_some(Narrowed::Float(float));
    }
    let integer: i64 = text.parse().ok()?;
    (integer.unsigned_abs() <= MAX_SAFE_INTEGER).then_some(Narrowed::Integer(integer))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Utf8Encoding, Windows1252Encoding,
        text::syntax::{parse, resolve::resolve},
    };
    use rstest::rstest;

    /// A scalar that a header is without its block has the JSON of the value
    /// that the deserializers read.
    #[rstest]
    #[case(b"a = 2 { }")]
    #[case(b"a = 2\n{ }\n{ }\n")]
    #[case(b"a = 2")]
    fn json_of_a_scalar_is_its_deserialized_value(#[case] source: &[u8]) {
        let parsed = parse(source);
        let map: HashMap<String, i32> = parsed.deserialize(Utf8Encoding::new()).unwrap();
        let json = parsed.document(Utf8Encoding::new()).json().to_string();
        assert_eq!(json, serde_json::to_string(&map).unwrap());

        let stream: HashMap<String, i32> =
            crate::TextDeserializer::from_slice(source, Utf8Encoding::new())
                .deserialize()
                .unwrap();
        assert_eq!(stream, map);
    }

    /// The JSON of the document, which must be that of the owned block.
    fn json_with(source: &[u8], options: JsonOptions, resolved: bool) -> String {
        let parse = parse(source);
        let mut doc = parse.document(Utf8Encoding::new());
        if resolved {
            doc = doc.with_resolution(resolve(parse.tree())).unwrap();
        }
        let direct = doc.json().with_options(options).to_string();
        let block: OwnedBlock = doc.deserialize().unwrap();
        let owned = block.json().with_options(options).to_string();
        assert_eq!(direct, owned);

        for field in doc.root().fields().filter(|f| !f.is_variable_definition()) {
            let value = doc.value(field).unwrap();
            let owned: OwnedValue = doc.deserialize_value(value).unwrap();
            assert_eq!(
                doc.value_json(value)
                    .unwrap()
                    .with_options(options)
                    .to_string(),
                owned.json().with_options(options).to_string()
            );
        }
        direct
    }

    fn mode(mode: DuplicateKeyMode) -> JsonOptions {
        JsonOptions::new().with_duplicate_keys(mode)
    }

    #[rstest]
    #[case("", "{}")]
    #[case("a = 2 { } b = 1", r#"{"a":2,"b":1}"#)]
    #[case("a = 2\n{ }\n{ }\nb = 1", r#"{"a":2,"b":1}"#)]
    #[case("a = rgb { }", r#"{"a":"rgb"}"#)]
    #[case("a = \"x y\" { }", r#"{"a":"x y"}"#)]
    #[case("a = {}", r#"{"a":[]}"#)]
    #[case("a = { {} }", r#"{"a":[[]]}"#)]
    #[case("a = { {} 1 }", r#"{"a":[[],1]}"#)]
    #[case("a = { b = {1} {} c = 2 }", r#"{"a":{"b":[1],"c":2}}"#)]
    #[case("a = 1 a = 2", r#"{"a":1,"a":2}"#)]
    #[case("a { 169 170 }", r#"{"a":[169,170]}"#)]
    #[case("1 2", r#"{"remainder":[1,2]}"#)]
    #[case(
        "a = { b = 1 10 c = d 20 }",
        r#"{"a":{"b":1,"remainder":[10,{"c":"d"},20]}}"#
    )]
    #[case(
        "a = { 10 0 = 2 1 > 3 }",
        r#"{"a":[10,{"0":2},{"1":{"GREATER_THAN":3}}]}"#
    )]
    #[case("c = rgb { 1 2 }", r#"{"c":{"rgb":[1,2]}}"#)]
    #[case("c = \"x\" { v = no }", r#"{"c":{"x":{"v":false}}}"#)]
    #[case(
        "a ?= 1 b >= 2 c == 3",
        r#"{"a":{"EXISTS":1},"b":{"GREATER_THAN_EQUAL":2},"c":{"EXACT":3}}"#
    )]
    #[case(
        "f = { [[x] a = b ] [[!y] $z$ ] }",
        r#"{"f":{"[x]":{"a":"b"},"[!y]":"$z$"}}"#
    )]
    #[case("\"k\" = \"a \\\"b\\\"\"", r#"{"k":"a \"b\""}"#)]
    #[case(
        "d = 1444.11.11 n = -5 s = 76561197960287930",
        r#"{"d":"1444.11.11","n":-5,"s":"76561197960287930"}"#
    )]
    #[case("@x = 2 a = @x b = @[x + 1]", r#"{"@x":2,"a":"@x","b":"@[x + 1]"}"#)]
    fn rules(#[case] source: &str, #[case] expected: &str) {
        assert_eq!(
            json_with(source.as_bytes(), JsonOptions::new(), false),
            expected
        );
    }

    #[rstest]
    #[case("@x = 2 a = @x b = @[x * 1.5] m = { @x }", r#"{"a":2,"b":3,"m":[2]}"#)]
    #[case("@x = 0.1 a = @[x + 0.2]", r#"{"a":0.30000000000000004}"#)]
    #[case("a = @none", r#"{"a":"@none"}"#)]
    fn resolution(#[case] source: &str, #[case] expected: &str) {
        assert_eq!(
            json_with(source.as_bytes(), JsonOptions::new(), true),
            expected
        );
    }

    #[test]
    fn narrowing_uses_the_resolved_quotes() {
        let options = JsonOptions::new().with_type_narrowing(TypeNarrowing::Unquoted);
        let source = b"@q = \"1\" @u = 1 a = @q b = @u c = \"yes\"";
        assert_eq!(
            json_with(source, options, true),
            r#"{"a":"1","b":1,"c":"yes"}"#
        );
        let options = options.with_type_narrowing(TypeNarrowing::None);
        assert_eq!(
            json_with(source, options, true),
            r#"{"a":"1","b":"1","c":"yes"}"#
        );
    }

    #[rstest]
    #[case("a = 1 b = 2 \"a\" > 3", r#"{"a":[1,{"GREATER_THAN":3}],"b":2}"#)]
    #[case("a = { x = 1 x = 2 3 }", r#"{"a":{"x":[1,2],"remainder":[3]}}"#)]
    #[case("remainder = 1 2", r#"{"remainder":[1,[2]]}"#)]
    #[case(
        "f = { [[x] a = 1 ] [[x] b = 2 ] }",
        r#"{"f":{"[x]":[{"a":1},{"b":2}]}}"#
    )]
    fn group(#[case] source: &str, #[case] expected: &str) {
        let options = mode(DuplicateKeyMode::Group);
        assert_eq!(json_with(source.as_bytes(), options, false), expected);
    }

    #[rstest]
    #[case("", r#"{"type":"obj","val":[]}"#)]
    #[case(
        "a = 2 { }",
        r#"{"type":"obj","val":[["a",{"type":"header","name":"2","val":{"type":"array","val":[]}}]]}"#
    )]
    #[case("a = { x = 1 2 }", r#"{"type":"obj","val":[["a",{"type":"obj","val":[["x",1],{"type":"remainder","val":[2]}]}]]}"#)]
    #[case(
        "a = { 1 b = 2 }",
        r#"{"type":"obj","val":[["a",{"type":"array","val":[1,{"type":"field","key":"b","val":2}]}]]}"#
    )]
    fn pairs(#[case] source: &str, #[case] expected: &str) {
        let options = mode(DuplicateKeyMode::KeyValuePairs);
        assert_eq!(json_with(source.as_bytes(), options, false), expected);
    }

    #[test]
    fn encoding_and_pretty() {
        let parse = parse(b"a = \"J\xe5hk\"");
        let doc = parse.document(Windows1252Encoding::new());
        let options = JsonOptions::new().with_prettyprint(true);
        let json = doc.json().with_options(options).to_string();
        assert_eq!(json, "{\n  \"a\": \"Jåhk\"\n}");
    }

    #[test]
    fn value_of_a_different_tree() {
        let (a, b) = (parse(b"a = 1"), parse(b"a = 1"));
        let value = b.ast().get(b"a").unwrap().value().unwrap();
        assert!(a.document(Utf8Encoding::new()).value_json(value).is_err());
    }

    /// The tagged form keeps constructs apart that other modes do not.
    #[rstest]
    #[case("a > { 1 }", "a = GREATER_THAN { 1 }")]
    #[case("remainder = { 1 2 }", "1 2")]
    #[case("a = { [[x] a = 1 ] }", "a = { \"[x]\" = { a = 1 } }")]
    #[case("a = { [[x] ] }", "a = { [[x] \"\" ] }")]
    #[case("a = { 1 b = 2 }", "a = { 1 { b = 2 } }")]
    #[case("a = type { 1 }", "a = { 1 }")]
    #[case("x = { 1 2 }", "x = 1 x = 2")]
    fn pairs_are_distinct(#[case] a: &str, #[case] b: &str) {
        let options = mode(DuplicateKeyMode::KeyValuePairs);
        let a = json_with(a.as_bytes(), options, false);
        assert_ne!(a, json_with(b.as_bytes(), options, false));
    }

    #[rstest]
    #[case("a > 1", r#"["a",{"type":"op","op":"GREATER_THAN","val":1}]"#)]
    #[case(
        "a = rgb { 1 }",
        r#"["a",{"type":"header","name":"rgb","val":{"type":"array","val":[1]}}]"#
    )]
    #[case("[[!x] 1 ]", r#"{"type":"parameter","name":"x","undefined":true,"val":{"type":"obj","val":[{"type":"remainder","val":[1]}]}}"#)]
    fn tags(#[case] source: &str, #[case] expected: &str) {
        let options = mode(DuplicateKeyMode::KeyValuePairs);
        let expected = format!(r#"{{"type":"obj","val":[{expected}]}}"#);
        assert_eq!(json_with(source.as_bytes(), options, false), expected);
    }

    #[rstest]
    #[case("\"1 \"", "1")]
    #[case("\"yes \"", "true")]
    #[case("-", "\"-\"")]
    #[case("+", "\"+\"")]
    #[case(".", "\".\"")]
    #[case("1.5f", "\"1.5f\"")]
    #[case("18446744073709551617.0f", "\"18446744073709551617.0f\"")]
    #[case("9007199254740991", "9007199254740991")]
    #[case("9007199254740992", "\"9007199254740992\"")]
    #[case("-9007199254740991", "-9007199254740991")]
    #[case("1e5", "\"1e5\"")]
    #[case("007", "7")]
    #[case("-.5", "-0.5")]
    fn scalars(#[case] scalar: &str, #[case] expected: &str) {
        let expected = format!("{{\"a\":{expected}}}");
        let source = format!("a = {scalar}");
        assert_eq!(
            json_with(source.as_bytes(), JsonOptions::new(), false),
            expected
        );
        let source = format!("@q = {scalar} a = @q");
        assert_eq!(
            json_with(source.as_bytes(), JsonOptions::new(), true),
            expected
        );
    }

    #[test]
    fn many_groups() {
        let source: String = (0..60).map(|i| format!("k{} = {i} ", i % 40)).collect();
        let json = json_with(source.as_bytes(), mode(DuplicateKeyMode::Group), false);
        assert!(json.starts_with(r#"{"k0":[0,40],"k1":[1,41],"#));
        assert!(json.ends_with(r#""k20":20,"k21":21,"k22":22,"k23":23,"k24":24,"k25":25,"k26":26,"k27":27,"k28":28,"k29":29,"k30":30,"k31":31,"k32":32,"k33":33,"k34":34,"k35":35,"k36":36,"k37":37,"k38":38,"k39":39}"#));
    }

    const MODES: [DuplicateKeyMode; 3] = [
        DuplicateKeyMode::Preserve,
        DuplicateKeyMode::Group,
        DuplicateKeyMode::KeyValuePairs,
    ];

    /// The reader skips containers with no recursion, so it has no limit.
    fn is_json(json: &str) -> bool {
        let mut reader = serde_json::Deserializer::from_str(json);
        let read: Result<serde::de::IgnoredAny, _> = serde::Deserialize::deserialize(&mut reader);
        read.is_ok() && reader.end().is_ok()
    }

    /// Any encoding has JSON, and the builder is `Send` when the encoding is
    /// `Sync`.
    #[test]
    fn encodings() {
        fn check<T: Send + Sync>(_: &T) {}
        fn json<E: Encoding>(doc: &Document<'_, '_, E>) -> String {
            doc.json().to_string()
        }
        fn same<'v, E: Encoding>(doc: &'v Document<'_, '_, E>) -> JsonBuilder<'v, E> {
            doc.json()
        }
        fn any<E: Encoding>(json: JsonBuilder<'_, E>) -> Vec<u8> {
            json.to_vec()
        }

        let parse = parse(b"a = \"\xe5\"");
        let encoding: Box<dyn Encoding> = Box::new(Windows1252Encoding::new());
        let boxed = parse.document(encoding);
        assert_eq!(json(&boxed), "{\"a\":\"å\"}");

        let doc = parse.document(Windows1252Encoding::new());
        check(&same(&doc));
        let block: OwnedBlock = doc.deserialize().unwrap();
        check(&block.json());
        assert_eq!(any(doc.json()), any(block.json()));
        assert_eq!(any(boxed.json()), any(block.json()));
    }

    #[test]
    fn directives() {
        let source = b"@x = 2 @:load_variable x";
        assert_eq!(json_with(source, JsonOptions::new(), false), r#"{"@x":2}"#);
        assert_eq!(
            json_with(source, JsonOptions::new(), true),
            r#"{"remainder":[2]}"#
        );
        let source = b"@:load_variable none";
        let expected = r#"{"remainder":["@:load_variable none"]}"#;
        assert_eq!(json_with(source, JsonOptions::new(), true), expected);
    }

    /// Deep blocks use no stack, and pretty printing is linear.
    #[rstest]
    #[case("{ ", "} ")]
    #[case("{ a = ", "} ")]
    #[case("{ a > h { ", "} } ")]
    #[case("{ 1 a = ", "} ")]
    fn deep(#[case] open: &'static str, #[case] close: &'static str) {
        let check = move || {
            let source = format!("a = {} 1 {}", open.repeat(5_000), close.repeat(5_000));
            let parse = parse(source.as_bytes());
            let doc = parse.document(Utf8Encoding::new());
            let block = OwnedBlock::from_tree(parse.tree(), Utf8Encoding::new());
            for options in MODES.map(mode) {
                let pretty = options.with_prettyprint(true);
                let pretty = doc.json().with_options(pretty).to_string();
                assert!(is_json(&pretty));
                // At most 8 lines of at most 256 spaces for each level.
                assert!(pretty.len() < source.len() * 2_000);
                let direct = doc.json().with_options(options).to_string();
                assert!(is_json(&direct));
                let direct = direct.into_bytes();
                assert_eq!(direct, block.json().with_options(options).to_vec());
            }
            assert!(format!("{:?}", block.json()).contains("options"));
        };
        // Wasm has no threads.
        #[cfg(target_family = "wasm")]
        check();
        #[cfg(not(target_family = "wasm"))]
        {
            let thread = std::thread::Builder::new().stack_size(512 * 1024);
            thread.spawn(check).unwrap().join().unwrap();
        }
    }

    /// The pretty output has the layout of `serde_json`.
    #[test]
    fn pretty_layout() {
        let source = b"a = { } b = { 1 { x = {} } } c > rgb { 1 } e = { [[p] q = 1 ] }";
        for options in MODES.map(mode) {
            let compact = json_with(source, options, false);
            let value: serde_json::Value = serde_json::from_str(&compact).unwrap();
            let pretty = json_with(source, options.with_prettyprint(true), false);
            let parsed: serde_json::Value = serde_json::from_str(&pretty).unwrap();
            assert_eq!(value, parsed);
        }
        let json = json_with(
            b"b = { {} 1 }",
            JsonOptions::new().with_prettyprint(true),
            false,
        );
        assert_eq!(json, "{\n  \"b\": [\n    [],\n    1\n  ]\n}");
    }

    #[test]
    fn writer_gets_the_output_and_errors() {
        let source = "key = value ".repeat(5_000);
        let parse = parse(source.as_bytes());
        let doc = parse.document(Utf8Encoding::new());
        let mut out = Vec::new();
        doc.json().to_writer(&mut out).unwrap();
        assert_eq!(out, doc.json().to_vec());

        let mut small = [0; 100];
        let error = doc.json().to_writer(&mut small[..]).unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::WriteZero);
    }

    /// The buffer goes to the writer after the entry that fills it.
    #[rstest]
    #[case("a=bcde ".repeat(5_000))]
    #[case(format!("a={{ {} }}", "abcdefg ".repeat(5_000)))]
    fn writer_gets_chunks(#[case] source: String) {
        struct Lengths(Vec<usize>);
        impl io::Write for Lengths {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                self.0.push(buf.len());
                Ok(buf.len())
            }
            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }
        let parse = parse(source.as_bytes());
        let doc = parse.document(Utf8Encoding::new());
        let mut out = Lengths(Vec::new());
        doc.json().to_writer(&mut out).unwrap();
        let (last, chunks) = out.0.split_last().unwrap();
        assert!(chunks.iter().all(|n| (CHUNK..CHUNK + 16).contains(n)));
        assert_eq!(chunks.len(), doc.json().to_vec().len() / CHUNK);
        assert!(*last < CHUNK + 16);
    }

    /// The formatter gets the output in chunks, which have whole characters.
    #[test]
    fn display() {
        use std::fmt::Write as _;

        struct Full;
        impl fmt::Write for Full {
            fn write_str(&mut self, _: &str) -> fmt::Result {
                Err(fmt::Error)
            }
        }

        let source = "\u{e9} = \"\u{e9}t\u{e9}\" ".repeat(5_000);
        let parse = parse(source.as_bytes());
        let doc = parse.document(Utf8Encoding::new());
        assert_eq!(doc.json().to_string().into_bytes(), doc.json().to_vec());
        assert_eq!(write!(Full, "{}", doc.json()), Err(fmt::Error));

        let pretty = JsonOptions::new().with_prettyprint(true);
        assert_eq!(
            format!("{:#}", doc.json()),
            doc.json().with_options(pretty).to_string()
        );
    }

    /// Each source, also with errors, gives valid JSON, and the document and
    /// the owned block give the same one.
    #[quickcheck_macros::quickcheck]
    fn valid_json(picks: Vec<u8>) -> bool {
        const PIECES: [&str; 24] = [
            "a",
            "b",
            "=",
            ">",
            "?=",
            "{",
            "}",
            "{",
            "}",
            "1",
            "-2.5",
            "\"q \"",
            "\"\\\"\"",
            "yes",
            "@x",
            "@x = 3",
            "@[x+1]",
            "[[p]",
            "]",
            "[[!p]",
            "rgb",
            "{}",
            "@:insert foo",
            "[Get]",
        ];
        let source: Vec<_> = picks
            .iter()
            .map(|&i| PIECES[i as usize % PIECES.len()])
            .collect();
        let source = source.join(" ");
        let narrowing = [
            TypeNarrowing::All,
            TypeNarrowing::Unquoted,
            TypeNarrowing::None,
        ];
        let all = MODES
            .into_iter()
            .flat_map(|keys| narrowing.map(|narrowing| mode(keys).with_type_narrowing(narrowing)));
        let mut all = all.flat_map(|o| [o, o.with_prettyprint(true)]);
        all.all(|options| {
            [false, true].into_iter().all(|resolved| {
                let parse = parse(source.as_bytes());
                let mut doc = parse.document(Utf8Encoding::new());
                if resolved {
                    doc = doc.with_resolution(resolve(parse.tree())).unwrap();
                }
                let json = doc.json().with_options(options).to_string();
                let block: OwnedBlock = doc.deserialize().unwrap();
                assert_eq!(
                    json,
                    block.json().with_options(options).to_string(),
                    "{source}"
                );
                is_json(&json)
            })
        })
    }
}
