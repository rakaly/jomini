//! Write a [`TextTape`] from the events of the lossless grammar.
//!
//! The deserializer and the DOM readers ([`ObjectReader`] and the other
//! readers) need the token layout of [`TextTape`], not the full tree. This sink
//! writes that layout directly, so the readers, the serde deserializer, and the
//! JSON conversion work without a change. The grammar and the error recovery
//! are the same as for [`parse`](super::parse).
//!
//! The sink uses the same rules as the [`TextTape`] parser:
//!
//! - A container is an object when its first entry is a field or a parameter.
//!   It is an array when its first entry is a value.
//! - A value in an object, or a field in an array, starts a mixed container.
//!   After the [`TextToken::MixedContainer`] token, each operator is a token,
//!   `=` also. When a child container closes, the mixed mode stops, unless
//!   the first entry of a child block that starts in mixed mode is a scalar.
//!   This is how `TextTape` keeps the mode in the open token. The root has no
//!   open token, thus its mixed mode always stops.
//! - A `key { ... }` field with no operator is a field in an object. When it is
//!   the first entry of a container, the key and the block are two values of
//!   an array.
//! - A header (`rgb { ... }`) is a [`TextToken::Header`] only in the value of a
//!   field in an object that is not mixed.
//! - An empty block is removed when it is not a value: when it is the first
//!   entry of a container, when it is in an object, and when it follows a
//!   header.
//! - `@[calc]`, `[interpolation]`, and `code [[ ... ]]` are one unquoted scalar
//!   that holds the source text. An operator in the value of a field, as in
//!   `OPERATOR = <=`, is also an unquoted scalar.
//! - Tokens that have no place in the tape (a stray operator or bracket, and a
//!   [`SyntaxKind::Bogus`] node) are ignored. A field that has no value is
//!   ignored. An unclosed block closes at EOF.
//!
//! [`ObjectReader`]: crate::text::ObjectReader

use super::{Flavor, Sink, SyntaxKind, Token, run, strip_quotes};
use crate::{Scalar, TextTape, TextToken, text::Operator};

/// Parse `source` with the lossless grammar into a [`TextTape`].
///
/// The result has the same layout as [`TextTape::from_slice`], so the
/// [`ObjectReader`](crate::text::ObjectReader) and the deserializer can read
/// it. This parse does not fail: it recovers from syntax errors in the same
/// way as [`parse`](super::parse). Use [`parse`](super::parse) to get the
/// diagnostics.
///
/// ```
/// use jomini::text::syntax::parse_tape;
///
/// let tape = parse_tape(b"name = Jean core = { FRA BUR }");
/// let reader = tape.windows1252_reader();
/// let mut fields = reader.fields();
/// let (key, _op, value) = fields.next().unwrap();
/// assert_eq!(key.read_str(), "name");
/// assert_eq!(value.read_str().unwrap(), "Jean");
/// let (_key, _op, value) = fields.next().unwrap();
/// assert_eq!(value.read_array().unwrap().len(), 2);
/// ```
pub fn parse_tape(source: &[u8]) -> TextTape<'_> {
    parse_tape_with(source, Flavor::default())
}

/// Parse `source` into a [`TextTape`] with a specific [`Flavor`].
pub fn parse_tape_with(source: &[u8], flavor: Flavor) -> TextTape<'_> {
    let p = run(source, flavor, |lexed| {
        TapeSink::new(source, lexed.tokens.len())
    });
    let bom = source.starts_with(super::BOM);
    TextTape::from_parts(p.sink.tape, bom)
}

/// A scalar for the tape.
#[derive(Debug, Clone, Copy)]
enum Leaf<'a> {
    Unquoted(Scalar<'a>),
    Quoted(Scalar<'a>),
}

impl<'a> Leaf<'a> {
    #[inline]
    fn token(self) -> TextToken<'a> {
        match self {
            Leaf::Unquoted(s) => TextToken::Unquoted(s),
            Leaf::Quoted(s) => TextToken::Quoted(s),
        }
    }

    #[inline]
    fn scalar(self) -> Scalar<'a> {
        match self {
            Leaf::Unquoted(s) | Leaf::Quoted(s) => s,
        }
    }
}

/// The form of a container, which the first entry sets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Shape {
    /// No entry sets the form yet.
    Open,
    Object,
    Array,
}

/// What to do when a block closes with no entries.
#[derive(Debug, Clone, Copy)]
enum OnEmpty {
    /// Keep an empty array.
    Keep,
    /// Remove the block. Truncate the tape to `len` and set the state of the
    /// parent container again.
    Drop {
        len: usize,
        shape: Shape,
        mixed: bool,
    },
    /// Remove the block and change the header at `len` to a scalar.
    Header { len: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ContainerKind {
    Root,
    Block,
    /// A parameter before its name.
    ParamHeader {
        undefined: bool,
    },
    /// The body of a parameter.
    ParamBody,
}

/// The root, a block, or the body of a parameter.
#[derive(Debug, Clone, Copy)]
struct Container {
    kind: ContainerKind,
    /// The index of the open token, or `NO_OPEN` when there is no token.
    open: usize,
    shape: Shape,
    /// After a [`TextToken::MixedContainer`], each operator is a token.
    mixed: bool,
    /// The mixed flag that `TextTape` keeps in the open token. When a child
    /// container closes, the mixed mode comes from this flag again.
    flag: bool,
    entries: u32,
    on_empty: OnEmpty,
}

impl Container {
    fn new(kind: ContainerKind, open: usize, shape: Shape, on_empty: OnEmpty) -> Self {
        Container {
            kind,
            open,
            shape,
            mixed: false,
            flag: false,
            entries: 0,
            on_empty,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Frame<'a> {
    Container(Container),
    /// A field. The key goes on the tape when the operator or the block that
    /// follows the key gives the form of the field.
    Field {
        key: Option<Leaf<'a>>,
        resolved: bool,
        has_value: bool,
        /// The value is in an object that is not mixed.
        header_ok: bool,
        /// The state to restore when the field has no value.
        len: usize,
        shape: Shape,
        mixed: bool,
    },
    /// A header and a block in the value of a field.
    Headered {
        header: Option<Scalar<'a>>,
        header_ok: bool,
    },
    /// A node that becomes one scalar, or nothing for a bogus node.
    Opaque {
        start: u32,
        end: u32,
        bogus: bool,
    },
}

const NO_OPEN: usize = usize::MAX;

struct TapeSink<'a> {
    source: &'a [u8],
    tape: Vec<TextToken<'a>>,
    frames: Vec<Frame<'a>>,
    /// The number of open nodes inside the top opaque frame.
    skip: u32,
}

impl<'a> TapeSink<'a> {
    fn new(source: &'a [u8], tokens: usize) -> Self {
        TapeSink {
            source,
            tape: Vec::with_capacity(tokens),
            frames: Vec::with_capacity(16),
            skip: 0,
        }
    }

    #[inline]
    fn text(&self, token: Token) -> &'a [u8] {
        &self.source[token.start as usize..token.end() as usize]
    }

    #[inline]
    fn leaf(&self, token: Token) -> Leaf<'a> {
        let text = self.text(token);
        if token.kind == SyntaxKind::Quoted {
            Leaf::Quoted(Scalar::new(strip_quotes(text)))
        } else {
            Leaf::Unquoted(Scalar::new(text))
        }
    }

    /// The nearest container in `frames[..end]`.
    #[inline]
    fn container_below(&mut self, end: usize) -> Option<&mut Container> {
        self.frames[..end]
            .iter_mut()
            .rev()
            .find_map(|frame| match frame {
                Frame::Container(c) => Some(c),
                _ => None,
            })
    }

    #[inline]
    fn top_container(&mut self) -> Option<&mut Container> {
        match self.frames.last_mut() {
            Some(Frame::Container(c)) => Some(c),
            _ => None,
        }
    }

    /// Count a new entry in the top container. When the first entry of a
    /// block starts with a scalar, `TextTape` copies the mixed mode of the
    /// enclosing container into the open token of that container.
    fn begin_entry(&mut self, starts_with_scalar: bool) {
        let len = self.frames.len();
        let Some(c) = self.top_container() else {
            return;
        };
        let first_in_block = c.entries == 0 && c.kind == ContainerKind::Block;
        c.entries += 1;
        if first_in_block
            && starts_with_scalar
            && let Some(parent) = self.container_below(len - 1)
            && parent.open != NO_OPEN
            && parent.mixed
        {
            parent.flag = true;
        }
    }

    /// Put a value in the top frame.
    fn value(&mut self, leaf: Leaf<'a>) {
        match self.frames.last_mut() {
            Some(Frame::Container(c)) => {
                if let ContainerKind::ParamHeader { undefined } = c.kind {
                    // The parameter name. The body is an object.
                    let name = leaf.scalar();
                    self.tape.push(if undefined {
                        TextToken::UndefinedParameter(name)
                    } else {
                        TextToken::Parameter(name)
                    });
                    c.kind = ContainerKind::ParamBody;
                    c.open = self.tape.len();
                    self.tape.push(TextToken::Object {
                        end: 0,
                        mixed: false,
                    });
                    return;
                }
                self.begin_entry(true);
                let Some(c) = self.top_container() else {
                    return;
                };
                let push_mixed = match c.shape {
                    Shape::Open => {
                        c.shape = Shape::Array;
                        false
                    }
                    Shape::Object => !std::mem::replace(&mut c.mixed, true),
                    Shape::Array => false,
                };
                if push_mixed {
                    self.tape.push(TextToken::MixedContainer);
                }
                self.tape.push(leaf.token());
            }
            Some(Frame::Field {
                key,
                resolved,
                has_value,
                ..
            }) => {
                if key.is_none() {
                    *key = Some(leaf);
                } else if *resolved && !*has_value {
                    *has_value = true;
                    self.tape.push(leaf.token());
                }
            }
            Some(Frame::Headered { header, .. }) => {
                if header.is_none() {
                    *header = Some(leaf.scalar());
                }
            }
            Some(Frame::Opaque { .. }) | None => {}
        }
    }

    /// Put the key of the top field on the tape. `op` is `None` for a field
    /// with no operator.
    fn resolve_field(&mut self, op: Option<Operator>) {
        let len = self.frames.len();
        let Some(Frame::Field { key, .. }) = self.frames.last().copied() else {
            return;
        };
        let Some(c) = self.container_below(len - 1) else {
            return;
        };
        let mut push_mixed = false;
        match (c.shape, op.is_some()) {
            (Shape::Open, true) => c.shape = Shape::Object,
            (Shape::Open, false) => c.shape = Shape::Array,
            (Shape::Array, true) if !c.mixed => {
                c.mixed = true;
                push_mixed = true;
            }
            _ => {}
        }
        let (shape, mixed) = (c.shape, c.mixed);
        if push_mixed {
            self.tape.push(TextToken::MixedContainer);
        }
        if let Some(key) = key {
            self.tape.push(key.token());
        }
        if let Some(op) = op
            && (mixed || op != Operator::Equal)
        {
            self.tape.push(TextToken::Operator(op));
        }
        if let Some(Frame::Field {
            resolved,
            header_ok,
            ..
        }) = self.frames.last_mut()
        {
            *resolved = true;
            *header_ok = shape == Shape::Object && !mixed;
        }
    }

    /// Start a block. The top frame is its parent.
    fn start_block(&mut self) {
        let on_empty = match self.frames.last().copied() {
            Some(Frame::Field { resolved, .. }) => {
                if !resolved {
                    self.resolve_field(None);
                }
                if let Some(Frame::Field { has_value, .. }) = self.frames.last_mut() {
                    *has_value = true;
                }
                OnEmpty::Keep
            }
            Some(Frame::Headered { header, header_ok }) => {
                let header = header.unwrap_or(Scalar::new(b""));
                if header_ok {
                    let len = self.tape.len();
                    self.tape.push(TextToken::Header(header));
                    OnEmpty::Header { len }
                } else {
                    self.tape.push(TextToken::Unquoted(header));
                    OnEmpty::Keep
                }
            }
            Some(Frame::Container(old)) => {
                self.begin_entry(false);
                let len = self.tape.len();
                let drop = OnEmpty::Drop {
                    len,
                    shape: old.shape,
                    mixed: old.mixed,
                };
                let Some(c) = self.top_container() else {
                    return;
                };
                match (c.shape, c.mixed) {
                    (Shape::Open, _) => {
                        c.shape = Shape::Array;
                        drop
                    }
                    (Shape::Object, false) => {
                        c.mixed = true;
                        self.tape.push(TextToken::MixedContainer);
                        drop
                    }
                    _ => OnEmpty::Keep,
                }
            }
            Some(Frame::Opaque { .. }) | None => OnEmpty::Keep,
        };
        let open = self.tape.len();
        self.tape.push(TextToken::Array {
            end: 0,
            mixed: false,
        });
        self.frames.push(Frame::Container(Container::new(
            ContainerKind::Block,
            open,
            Shape::Open,
            on_empty,
        )));
    }

    fn finish_container(&mut self, c: Container) {
        match c.kind {
            ContainerKind::Root | ContainerKind::ParamHeader { .. } => return,
            ContainerKind::ParamBody => {
                // `[[name] value ]` is a parameter with a scalar value. In
                // the body object, the value follows a mixed container token.
                let body = &self.tape[c.open + 1..];
                let value = match body {
                    [] => Some(TextToken::Unquoted(Scalar::new(b""))),
                    [
                        TextToken::MixedContainer,
                        value @ (TextToken::Unquoted(_) | TextToken::Quoted(_)),
                    ] => Some(value.clone()),
                    _ => None,
                };
                if let Some(value) = value {
                    self.tape.truncate(c.open);
                    self.tape.push(value);
                    return;
                }
            }
            ContainerKind::Block if c.entries == 0 => match c.on_empty {
                OnEmpty::Keep => {}
                OnEmpty::Drop { len, shape, mixed } => {
                    self.tape.truncate(len);
                    if let Some(parent) = self.top_container() {
                        parent.shape = shape;
                        parent.mixed = mixed;
                    }
                    return;
                }
                OnEmpty::Header { len } => {
                    if let Some(TextToken::Header(header)) = self.tape.get(len).cloned() {
                        self.tape.truncate(len);
                        self.tape.push(TextToken::Unquoted(header));
                    }
                    return;
                }
            },
            ContainerKind::Block => {}
        }

        let end = self.tape.len();
        self.tape[c.open] = if c.shape == Shape::Object {
            TextToken::Object {
                end,
                mixed: c.mixed,
            }
        } else {
            TextToken::Array {
                end,
                mixed: c.mixed,
            }
        };
        self.tape.push(TextToken::End(c.open));

        let len = self.frames.len();
        if let Some(parent) = self.container_below(len) {
            parent.mixed = parent.flag;
        }
    }
}

fn operator(text: &[u8]) -> Option<Operator> {
    Some(match text {
        b"=" => Operator::Equal,
        b"<" => Operator::LessThan,
        b"<=" => Operator::LessThanEqual,
        b">" => Operator::GreaterThan,
        b">=" => Operator::GreaterThanEqual,
        b"==" => Operator::Exact,
        b"!=" => Operator::NotEqual,
        b"?=" => Operator::Exists,
        _ => return None,
    })
}

impl Sink for TapeSink<'_> {
    #[inline]
    fn start_node(&mut self, kind: SyntaxKind) {
        if self.skip > 0 || matches!(self.frames.last(), Some(Frame::Opaque { .. })) {
            self.skip += 1;
            return;
        }
        match kind {
            SyntaxKind::Root => self.frames.push(Frame::Container(Container::new(
                ContainerKind::Root,
                NO_OPEN,
                Shape::Object,
                OnEmpty::Keep,
            ))),
            SyntaxKind::Block => self.start_block(),
            SyntaxKind::Field => {
                self.begin_entry(true);
                let (shape, mixed) = match self.top_container() {
                    Some(c) => (c.shape, c.mixed),
                    None => (Shape::Open, false),
                };
                self.frames.push(Frame::Field {
                    key: None,
                    resolved: false,
                    has_value: false,
                    header_ok: false,
                    len: self.tape.len(),
                    shape,
                    mixed,
                });
            }
            SyntaxKind::HeaderedBlock => {
                let mut header_ok = false;
                if let Some(Frame::Field {
                    has_value,
                    header_ok: ok,
                    ..
                }) = self.frames.last_mut()
                {
                    *has_value = true;
                    header_ok = *ok;
                }
                self.frames.push(Frame::Headered {
                    header: None,
                    header_ok,
                });
            }
            SyntaxKind::Parameter | SyntaxKind::UndefinedParameter
                if self.top_container().is_some() =>
            {
                self.begin_entry(false);
                if let Some(c) = self.top_container()
                    && c.shape == Shape::Open
                {
                    c.shape = Shape::Object;
                }
                self.frames.push(Frame::Container(Container::new(
                    ContainerKind::ParamHeader {
                        undefined: kind == SyntaxKind::UndefinedParameter,
                    },
                    NO_OPEN,
                    Shape::Object,
                    OnEmpty::Keep,
                )));
            }
            _ => self.frames.push(Frame::Opaque {
                start: u32::MAX,
                end: 0,
                bogus: kind == SyntaxKind::Bogus,
            }),
        }
    }

    #[inline]
    fn token(&mut self, token: Token) {
        // The most frequent token in a save: a scalar in an array. The
        // array has an entry, so the first-entry rules do not apply.
        if token.kind.is_scalar()
            && let Some(Frame::Container(c)) = self.frames.last_mut()
            && c.shape == Shape::Array
            && c.kind == ContainerKind::Block
        {
            c.entries += 1;
            let leaf = self.leaf(token);
            self.tape.push(leaf.token());
            return;
        }

        match self.frames.last_mut() {
            Some(Frame::Opaque { start, end, .. }) => {
                if *start == u32::MAX {
                    *start = token.start;
                }
                *end = token.end();
            }
            Some(Frame::Field {
                key,
                resolved,
                has_value,
                ..
            }) if token.kind == SyntaxKind::Operator && key.is_some() => {
                if !*resolved {
                    let op = operator(self.text(token));
                    self.resolve_field(Some(op.unwrap_or(Operator::Equal)));
                } else if !*has_value {
                    // An operator as a value, as in the macro argument
                    // `OPERATOR = <=`.
                    *has_value = true;
                    let text = self.text(token);
                    self.tape.push(TextToken::Unquoted(Scalar::new(text)));
                }
            }
            _ => {
                if token.kind.is_scalar() {
                    self.value(self.leaf(token));
                } else if !matches!(
                    token.kind,
                    SyntaxKind::OpenBrace | SyntaxKind::CloseBrace | SyntaxKind::MissingCloseBrace
                ) && let Some(c) = self.top_container()
                    && c.kind == ContainerKind::Block
                    && c.entries == 0
                {
                    // A stray token makes a block not empty, as in `{ = }`.
                    c.entries = 1;
                }
            }
        }
    }

    #[inline]
    fn finish_node(&mut self) {
        if self.skip > 0 {
            self.skip -= 1;
            return;
        }
        match self.frames.pop() {
            Some(Frame::Container(c)) => self.finish_container(c),
            Some(Frame::Field {
                has_value,
                len,
                shape,
                mixed,
                ..
            }) => {
                if !has_value {
                    // A field with no value, as in `a = }`.
                    self.tape.truncate(len);
                    if let Some(c) = self.top_container() {
                        c.shape = shape;
                        c.mixed = mixed;
                    }
                }
            }
            Some(Frame::Headered { .. }) | None => {}
            Some(Frame::Opaque { start, end, bogus }) => {
                if !bogus && start < end {
                    let text = &self.source[start as usize..end as usize];
                    self.value(Leaf::Unquoted(Scalar::new(text)));
                }
            }
        }
    }

    #[inline]
    fn error(&mut self) {}

    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        if self.skip > 0 || !matches!(self.frames.last(), Some(Frame::Container(_))) {
            self.start_node(SyntaxKind::Field);
            self.token(key);
            self.token(op);
            self.token(value);
            self.finish_node();
            return;
        }
        let op = operator(self.text(op)).unwrap_or(Operator::Equal);
        let (key, value) = (self.leaf(key), self.leaf(value));
        self.begin_entry(true);
        let Some(c) = self.top_container() else {
            return;
        };
        let mut push_mixed = false;
        match c.shape {
            Shape::Open => c.shape = Shape::Object,
            Shape::Array if !c.mixed => {
                c.mixed = true;
                push_mixed = true;
            }
            _ => {}
        }
        let mixed = c.mixed;
        if push_mixed {
            self.tape.push(TextToken::MixedContainer);
        }
        self.tape.push(key.token());
        if mixed || op != Operator::Equal {
            self.tape.push(TextToken::Operator(op));
        }
        self.tape.push(value.token());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Assert that the lossless tape is the same as the `TextTape`.
    #[track_caller]
    fn same(data: &[u8]) {
        let expected = TextTape::from_slice(data).unwrap();
        let actual = parse_tape(data);
        assert_eq!(
            actual.tokens(),
            expected.tokens(),
            "input: {}",
            String::from_utf8_lossy(data)
        );
        assert_eq!(actual.utf8_bom(), expected.utf8_bom());
    }

    #[test]
    fn simple_fields() {
        same(b"foo=bar");
        same(b"foo = bar baz = \"qux\"");
        same(b"\xef\xbb\xbffoo=bar");
        same(b"");
        same(b"# only a comment");
    }

    #[test]
    fn containers() {
        same(b"foo={bar=qux}");
        same(b"foo={1 2 3}");
        same(b"foo={}");
        same(b"foo={{a=b} {c=d}}");
        same(b"foo={{1 2} {3 4}}");
        same(b"foo{bar=qux}");
        same(b"foo={bar=val {}} me=you");
        same(b"foo={bar=val { } a=b} me=you");
        same(b"foo={{} {a=b}}");
        same(b"foo={1 {} 2}");
        same(b"foo={a {b=c}}");
        same(b"foo={a=b c {d=e}}");
        same(b"army={name=abc} army={name=def}");
    }

    #[test]
    fn operators() {
        same(b"a < 1 b <= 2 c > 3 d >= 4 e != 5 f == 6 g ?= 7");
        same(b"foo = { a > 1 b = 2 }");
        same(b"foo = { a ?= 1 }");
    }

    #[test]
    fn headers() {
        same(b"color = rgb { 100 200 150 }");
        same(b"color = hsv { 0.5 0.2 0.8 } a = b");
        same(b"color = rgb { }");
        same(b"foo = { color = rgb { 1 2 3 } }");
        same(b"foo = { rgb { 1 2 3 } }");
        same(b"foo = LIST { \"a\" \"b\" }");
    }

    #[test]
    fn mixed_containers() {
        same(b"foo = { 10 0=1 1=2 }");
        same(b"foo = { a=b c d=e }");
        same(b"foo = { a=b c }");
        same(b"foo = { a b=c d=e }");
        same(b"foo = { a=b c d={x=y} e=f }");
        same(b"foo = { 1 a = rgb { 1 2 } }");
        same(b"foo = { a=b \"c\" \"d\" }");
    }

    #[test]
    fn parameters() {
        same(b"generate_advisor = { [[scaled_skill] if = { limit = { a = b } } ] }");
        same(b"foo = { [[skill] $skill$ ] }");
        same(b"foo = { [[!skill] a = b ] c = d }");
        same(b"[[x] a = b ]");
    }

    #[test]
    fn scalars() {
        same(b"a = @var b = @[1 + 2] c = $MACRO$");
        same(b"date=1444.11.11 name=\"a \\\"b\\\" c\"");
        same(b"foo = { @[a * 2] }");
    }

    #[test]
    fn object_template() {
        same(b"obj = { { a = b } = { c = d } }");
    }

    #[test]
    fn recovery() {
        // `TextTape` closes one unclosed block at EOF as an object.
        same(b"foo = { a = b");
        // A stray `}` at the top level is ignored.
        same(b"a = b } c = d");

        // An operator as a value is a scalar.
        let tape = parse_tape(b"OPERATOR = <=");
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"OPERATOR")),
                TextToken::Unquoted(Scalar::new(b"<=")),
            ]
        );

        // A field with no value is ignored.
        let tape = parse_tape(b"a = } b = c");
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"c")),
            ]
        );

        // Each unclosed block closes at EOF.
        let tape = parse_tape(b"a = { b = { c = d");
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Object {
                    end: 7,
                    mixed: false
                },
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Object {
                    end: 6,
                    mixed: false
                },
                TextToken::Unquoted(Scalar::new(b"c")),
                TextToken::Unquoted(Scalar::new(b"d")),
                TextToken::End(3),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn opaque_nodes() {
        let tape = parse_tape(b"a = [GetName] b = code [[ x = 1 ]] c = { 1 } } d");
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Unquoted(Scalar::new(b"[GetName]")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"code [[ x = 1 ]]")),
                TextToken::Unquoted(Scalar::new(b"c")),
                TextToken::Array {
                    end: 7,
                    mixed: false
                },
                TextToken::Unquoted(Scalar::new(b"1")),
                TextToken::End(5),
                TextToken::MixedContainer,
                TextToken::Unquoted(Scalar::new(b"d")),
            ]
        );
    }
}
