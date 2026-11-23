//! Write a [`TextTape`] with the lossless grammar.
//!
//! The deserializer and the DOM readers ([`ObjectReader`] and the other
//! readers) need the token layout of [`TextTape`], not the full tree. This sink
//! writes that layout directly, so the readers, the serde deserializer, and the
//! JSON conversion work without a change. The grammar and the error recovery
//! are the same as for [`parse`](super::parse).
//!
//! The sink uses the same rules as the [`TextTape`] parser:
//!
//! - A container is an object when its first entry is a field or a
//!   parameter. It is an array when its first entry is a value.
//! - A value in an object, or a field in an array, starts a mixed container.
//!   After the [`TextToken::MixedContainer`] token, each operator, including
//!   `=`, is a token. When a child container closes, mixed mode stops,
//!   unless a child that began in mixed mode had a scalar as its first token
//!   (a value or the key of a field). Once such a child appears, mixed mode
//!   stays on after each child closes. The root has no open token, so its
//!   mixed mode always stops.
//! - In an object, `key { ... }` is a field, like `key = { ... }`. When the
//!   field is the first entry of a container, its key and its block are two
//!   values of an array.
//! - A header (`rgb { ... }`) is a [`TextToken::Header`] only in the value of
//!   a field in an object that is not mixed. A quoted header, as in
//!   `blockoverride = "tab_glow" { ... }`, is a [`TextToken::QuotedHeader`].
//! - An empty block is removed when it is not a value: when it is the first
//!   entry of a container, when it is in an object, and when it follows a
//!   header.
//! - A CK3 reader directive is ignored, except in the value of a field,
//!   where it is one unquoted scalar. A field that has no value is ignored.
//!
//! [`ObjectReader`]: crate::text::ObjectReader

mod fast;

use super::{LexFlavor, Sink, SyntaxError, SyntaxKind, Token, operator, run_items, strip_quotes};
use crate::{
    Scalar, TextTape, TextToken,
    text::{Operator, TextFlavor},
};

/// The result of [`parse_tape`]: a [`TextTape`] and the syntax errors of the
/// source.
///
/// The parser recovers from each syntax error, so the tape holds the data
/// after an error. The caller decides which errors are acceptable.
/// The game ignores a `}` that closes no block, and a tool that reads game
/// files can do the same:
///
/// ```
/// use jomini::text::syntax::{parse_tape, SyntaxErrorKind};
///
/// let parsed = parse_tape(b"a = { 10 } } b = yes");
/// let fatal = parsed
///     .errors()
///     .iter()
///     .find(|error| error.kind != SyntaxErrorKind::UnmatchedCloseBrace);
/// assert_eq!(fatal, None);
///
/// let tape = parsed.into_tape();
/// let reader = tape.utf8_reader();
/// let keys: Vec<_> = reader.fields().map(|(key, _, _)| key.read_string()).collect();
/// assert_eq!(keys, ["a", "b"]);
/// ```
#[derive(Debug)]
#[must_use]
pub struct TapeParse<'a> {
    tape: TextTape<'a>,
    errors: Vec<SyntaxError>,
}

impl<'a> TapeParse<'a> {
    /// The tape. When [`TapeParse::errors`] is not empty, this is the tape
    /// after the recovery.
    #[inline]
    pub fn tape(&self) -> &TextTape<'a> {
        &self.tape
    }

    /// The syntax errors of the source. They are the same errors that
    /// [`Parse::errors`](super::Parse::errors) gives.
    #[inline]
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    /// Discard the errors and return the tape.
    pub fn into_tape(self) -> TextTape<'a> {
        self.tape
    }

    /// Return the tape and the errors.
    pub fn into_parts(self) -> (TextTape<'a>, Vec<SyntaxError>) {
        (self.tape, self.errors)
    }

    /// Return the tape only when the source has no syntax error.
    ///
    /// # Errors
    ///
    /// The syntax error that has the smallest start offset.
    ///
    /// ```
    /// use jomini::text::syntax::{parse_tape, SyntaxErrorKind};
    ///
    /// let error = parse_tape(b"core = { FRA BUR").into_result().unwrap_err();
    /// assert_eq!(error.kind, SyntaxErrorKind::UnclosedBlock);
    /// ```
    pub fn into_result(self) -> Result<TextTape<'a>, SyntaxError> {
        match self.errors.into_iter().min_by_key(|e| e.range.start()) {
            None => Ok(self.tape),
            Some(error) => Err(error),
        }
    }
}

/// Parse `source` with the lossless grammar into a [`TextTape`].
///
/// The tape has the same layout as [`TextTape::from_slice`], so the
/// [`ObjectReader`](crate::text::ObjectReader) and the deserializer can read
/// it. This function does not fail. Like [`parse`](super::parse), it recovers
/// from each syntax error and gives all of the errors with the tape. Use
/// [`TapeParse::into_result`] to refuse a source that has an error.
///
/// ```
/// use jomini::text::syntax::parse_tape;
///
/// let tape = parse_tape(b"name = Jean core = { FRA BUR }").into_result()?;
/// let reader = tape.windows1252_reader();
/// let mut fields = reader.fields();
/// let (key, _op, value) = fields.next().unwrap();
/// assert_eq!(key.read_str(), "name");
/// assert_eq!(value.read_str().unwrap(), "Jean");
/// let (_key, _op, value) = fields.next().unwrap();
/// assert_eq!(value.read_array().unwrap().len(), 2);
/// # Ok::<(), jomini::text::syntax::SyntaxError>(())
/// ```
///
/// The source is parsed once, even when it has an error.
///
/// # Differences from `from_slice`
///
/// A few cases differ from `from_slice`:
///
/// - `from_slice` accepts an extra `}` and an unclosed block with no error.
///   `parse_tape` reports them, so [`into_result`](TapeParse::into_result)
///   rejects them. Some game files contain an extra `}` or end before a
///   block closes; to accept them, filter the errors as the [`TapeParse`]
///   example shows.
/// - In a block, `from_slice` accepts an operator that has no key, as in
///   `x = { = }`, and a second operator in a field, as in `x = { a = 1 = 2 }`.
///   `parse_tape` reports them.
/// - Each `@[calc]`, `[interpolation]`, or `code [[ ... ]]` becomes one
///   unquoted scalar that holds its source text. So does an operator used as
///   a field value, as in `OPERATOR = <=`. `from_slice` rejects or splits
///   some of these.
/// - Tokens that have no place in the tape, such as a stray bracket, are
///   ignored.
/// - A quoted scalar before a block in a field value, as in
///   `blockoverride = "tab_glow" { visible = no }`, is a
///   [`TextToken::QuotedHeader`]. `from_slice` rejects it.
///
/// For HoI4 files, use [`parse_tape_with`] with [`Hoi4Flavor`](super::Hoi4Flavor).
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
pub fn parse_tape(source: &[u8]) -> TapeParse<'_> {
    parse_tape_lex(source, LexFlavor::DEFAULT)
}

/// Parse `source` into a [`TextTape`] with the syntax of a game's
/// [`TextFlavor`]. See [`parse_tape`].
///
/// # Panics
///
/// Panics if `source` is 4 GiB or larger, because the offsets are `u32`.
pub fn parse_tape_with<'a, F: TextFlavor + ?Sized>(source: &'a [u8], flavor: &F) -> TapeParse<'a> {
    parse_tape_lex(source, LexFlavor::of(flavor))
}

fn parse_tape_lex(source: &[u8], flavor: LexFlavor) -> TapeParse<'_> {
    assert!(
        u32::try_from(source.len()).is_ok(),
        "source is larger than 4 GiB"
    );
    let bom = source.starts_with(super::BOM);
    let tape = Vec::with_capacity(source.len() / 5);
    let level = fearless_simd::Level::new();
    let fast = fearless_simd::dispatch!(level, simd => fast::parse(simd, source, flavor, tape));
    let (mut tape, at) = match fast {
        Ok(tape) => {
            return TapeParse {
                tape: TextTape::from_parts(tape, bom),
                errors: Vec::new(),
            };
        }
        Err(stop) => stop,
    };
    // The fast path stopped. Keep the tape of the root items before the
    // checkpoint, and continue with the full grammar from there. The fast
    // path accepts only root items with no error, so the grammar finds all
    // of them.
    tape.truncate(at.len);
    let sink = TapeSink::resume(source, tape, at);
    let p = run_items(source, flavor, at.pos, sink);
    TapeParse {
        tape: TextTape::from_parts(p.sink.tape, bom),
        errors: p.errors,
    }
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

    /// The token of the scalar as the header of a block.
    #[inline]
    fn header(self) -> TextToken<'a> {
        match self {
            Leaf::Unquoted(s) => TextToken::Header(s),
            Leaf::Quoted(s) => TextToken::QuotedHeader(s),
        }
    }
}

/// The scalar of the header at `token`, for a header whose block is empty.
#[inline]
fn header_value<'a>(token: &TextToken<'a>) -> Option<TextToken<'a>> {
    match *token {
        TextToken::Header(s) => Some(TextToken::Unquoted(s)),
        TextToken::QuotedHeader(s) => Some(TextToken::Quoted(s)),
        _ => None,
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
    /// parent container again. When `first` is set, the block was the first
    /// entry of the parent, so the parent has no entries again.
    Drop {
        len: usize,
        shape: Shape,
        mixed: bool,
        first: bool,
    },
    /// Remove the block and change the header at `len` to its scalar.
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
    /// An entry was removed, so the block is not empty in the source.
    /// `TextTape` removes only a block that is empty in the source.
    removed: bool,
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
            removed: false,
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
        header: Option<Leaf<'a>>,
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
    fn new(source: &'a [u8], tape: Vec<TextToken<'a>>) -> Self {
        TapeSink {
            source,
            tape,
            frames: Vec::with_capacity(16),
            skip: 0,
        }
    }

    /// Make a sink that has an open root node with the state of `at`. The
    /// tape must hold the tokens before `at`.
    fn resume(source: &'a [u8], tape: Vec<TextToken<'a>>, at: fast::Checkpoint) -> Self {
        let mut sink = TapeSink::new(source, tape);
        let mut root = Container::new(ContainerKind::Root, NO_OPEN, Shape::Object, OnEmpty::Keep);
        root.mixed = at.mixed;
        root.entries = u32::from(!at.empty);
        sink.frames.push(Frame::Container(root));
        sink
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
                    *header = Some(leaf);
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
                let header = header.unwrap_or(Leaf::Unquoted(Scalar::new(b"")));
                if header_ok {
                    let len = self.tape.len();
                    self.tape.push(header.header());
                    OnEmpty::Header { len }
                } else {
                    self.tape.push(header.token());
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
                    first: old.entries == 0,
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
            ContainerKind::Block if c.entries == 0 && !c.removed => match c.on_empty {
                OnEmpty::Keep => {}
                OnEmpty::Drop {
                    len,
                    shape,
                    mixed,
                    first,
                } => {
                    self.tape.truncate(len);
                    if let Some(parent) = self.top_container() {
                        parent.shape = shape;
                        parent.mixed = mixed;
                        parent.removed = true;
                        if first {
                            parent.entries = 0;
                        }
                    }
                    return;
                }
                OnEmpty::Header { len } => {
                    if let Some(value) = self.tape.get(len).and_then(header_value) {
                        self.tape.truncate(len);
                        self.tape.push(value);
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

/// Return `true` when `c` is an object that is not mixed and has an entry. A
/// new field in such a container changes only the entry count.
#[inline]
fn is_plain_object(c: &Container) -> bool {
    c.entries > 0 && c.shape == Shape::Object && !c.mixed
}

impl Sink for TapeSink<'_> {
    const KEEP_TOKENS: bool = false;

    #[inline]
    fn start_node(&mut self, kind: SyntaxKind) {
        // When `skip` is not zero, the top frame is opaque.
        if matches!(self.frames.last(), Some(Frame::Opaque { .. })) {
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
            _ => {
                // A reader directive changes the text before the game reads
                // it, so it has no place in the tape. In value position, as in
                // `x = @:load_variable y`, it is one unquoted scalar.
                let ignore = kind == SyntaxKind::Bogus
                    || (kind == SyntaxKind::ReaderDirective
                        && !matches!(self.frames.last(), Some(Frame::Field { .. })));
                self.frames.push(Frame::Opaque {
                    start: u32::MAX,
                    end: 0,
                    bogus: ignore,
                });
            }
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

    /// The tape has no place for a diagnostic. The parser keeps them.
    #[inline]
    fn error(&mut self) {}

    #[inline]
    fn brace(&mut self, token: Token) {
        // Only an opaque node uses the braces: they are part of its text.
        if matches!(self.frames.last(), Some(Frame::Opaque { .. })) {
            self.token(token);
        }
    }

    #[inline]
    fn field_head(&mut self, key: Token, op: Token) {
        let (key_token, op_token) = (key, op);
        let op = operator(self.text(op)).unwrap_or(Operator::Equal);
        let key = self.leaf(key);
        let Some(Frame::Container(c)) = self.frames.last_mut() else {
            self.start_node(SyntaxKind::Field);
            self.token(key_token);
            self.token(op_token);
            return;
        };
        if is_plain_object(c) {
            // The most frequent case: a field after the first entry of an
            // object. The first-entry rules do not apply.
            c.entries += 1;
            let len = self.tape.len();
            self.tape.push(key.token());
            if op != Operator::Equal {
                self.tape.push(TextToken::Operator(op));
            }
            self.frames.push(Frame::Field {
                key: Some(key),
                resolved: true,
                has_value: false,
                header_ok: true,
                len,
                shape: Shape::Object,
                mixed: false,
            });
            return;
        }
        self.begin_entry(true);
        let len = self.tape.len();
        let Some(c) = self.top_container() else {
            return;
        };
        let (old_shape, old_mixed) = (c.shape, c.mixed);
        let mut push_mixed = false;
        match c.shape {
            Shape::Open => c.shape = Shape::Object,
            Shape::Array if !c.mixed => {
                c.mixed = true;
                push_mixed = true;
            }
            _ => {}
        }
        let (shape, mixed) = (c.shape, c.mixed);
        if push_mixed {
            self.tape.push(TextToken::MixedContainer);
        }
        self.tape.push(key.token());
        if mixed || op != Operator::Equal {
            self.tape.push(TextToken::Operator(op));
        }
        self.frames.push(Frame::Field {
            key: Some(key),
            resolved: true,
            has_value: false,
            header_ok: shape == Shape::Object && !mixed,
            len,
            shape: old_shape,
            mixed: old_mixed,
        });
    }

    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        let tokens = (key, op, value);
        let op = operator(self.text(op)).unwrap_or(Operator::Equal);
        let (key, value) = (self.leaf(key), self.leaf(value));
        let Some(Frame::Container(c)) = self.frames.last_mut() else {
            self.start_node(SyntaxKind::Field);
            self.token(tokens.0);
            self.token(tokens.1);
            self.token(tokens.2);
            self.finish_node();
            return;
        };
        if is_plain_object(c) {
            c.entries += 1;
            self.tape.push(key.token());
            if op != Operator::Equal {
                self.tape.push(TextToken::Operator(op));
            }
            self.tape.push(value.token());
            return;
        }
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
    use super::super::{SyntaxErrorKind, run};
    use super::*;
    use quickcheck_macros::quickcheck;

    /// The tape parse gives the tape of the full grammar and the errors of
    /// the tree, with the same contents and in the same order.
    fn matches_grammar(source: &[u8], flavor: LexFlavor) -> bool {
        let tree = super::super::parse_lex(source, flavor);
        let expected = run(source, flavor, TapeSink::new(source, Vec::new()));
        let (tape, errors) = parse_tape_lex(source, flavor).into_parts();
        errors == tree.errors()
            && expected.errors == tree.errors()
            && tape.tokens() == expected.sink.tape
            && fast::parse(fearless_simd::Fallback::new(), source, flavor, Vec::new())
                .ok()
                .is_none_or(|tape| tape == expected.sink.tape)
            && resumes_match_grammar(source, flavor, &expected.sink.tape, tree.errors())
    }

    /// Continue the full grammar from each checkpoint of the fast path. The
    /// result must be the `expected` tape and the `errors` of a full parse.
    fn resumes_match_grammar(
        source: &[u8],
        flavor: LexFlavor,
        expected: &[TextToken],
        errors: &[SyntaxError],
    ) -> bool {
        let (checkpoints, tape) = fast::checkpoints(source, flavor);
        checkpoints.into_iter().all(|at| {
            let prefix = &expected[..at.len.min(expected.len())];
            if tape.get(..at.len) != Some(prefix) {
                return false;
            }
            let sink = TapeSink::resume(source, prefix.to_vec(), at);
            let p = run_items(source, flavor, at.pos, sink);
            p.sink.tape == expected && p.errors == errors
        })
    }

    /// The grammar reads `@@a` as a key, not as the header of the block, and
    /// the fast path agrees.
    #[test]
    fn variable_key_before_block_is_not_a_header() {
        assert!(matches_grammar(b"a<@@a{a}", LexFlavor::DEFAULT));
    }

    #[test]
    fn resume_after_root_block() {
        let source = b"a={1} b={c=d} e=f [[x] g=h ] i={j}";
        let (checkpoints, _) = fast::checkpoints(source, LexFlavor::DEFAULT);
        assert_eq!(checkpoints.len(), 3);
        assert!(matches_grammar(source, LexFlavor::DEFAULT));
        let source = b"\xef\xbb\xbfa b {1} c=d [[x] e=f ]";
        assert!(matches_grammar(source, LexFlavor::DEFAULT));
    }

    /// The fast path reads these constructs from real saves. It does not
    /// give them to the full grammar.
    #[test]
    fn fast_path_constructs() {
        let sources: &[&[u8]] = &[
            b"x={ y=\"hanafi_school\"\n{\n}\n1398.1.1={ a=b } }",
            b"x={ y=\"a\" { 1 } z=w }",
            b"needed={ { demand=pop masonry=0.1 }={ 1 2 3 } { a=b }={ 4 } }",
            b"{ a=b }={ 1 } c=d",
            b"a = $A$ { b } c = @v { d } e = $E$ { }",
        ];
        for source in sources {
            for flavor in [LexFlavor::DEFAULT, LexFlavor::HOI4] {
                let fast = fast::parse(fearless_simd::Fallback::new(), source, flavor, Vec::new());
                assert!(fast.is_ok(), "input: {}", String::from_utf8_lossy(source));
                assert!(matches_grammar(source, flavor));
            }
        }
    }

    #[quickcheck]
    fn fused_matches_grammar(source: Vec<u8>) -> bool {
        matches_grammar(&source, LexFlavor::DEFAULT) && matches_grammar(&source, LexFlavor::HOI4)
    }

    #[quickcheck]
    fn fused_matches_grammar_alphabet(source: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"abc\"\\{}[]<>!?= \t\n;#@$:";
        let source: Vec<u8> = source
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        matches_grammar(&source, LexFlavor::DEFAULT) && matches_grammar(&source, LexFlavor::HOI4)
    }

    /// The fast path and the full grammar give the same tape for a source
    /// that starts with a BOM.
    #[quickcheck]
    fn fused_matches_grammar_bom(source: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"abc\"\\{}[]<>!?= \t\r\n;#@$:";
        let source: Vec<u8> = super::super::BOM
            .iter()
            .copied()
            .chain(
                source
                    .iter()
                    .map(|b| ALPHABET[*b as usize % ALPHABET.len()]),
            )
            .collect();
        matches_grammar(&source, LexFlavor::DEFAULT) && matches_grammar(&source, LexFlavor::HOI4)
    }

    /// Calculations and their parentheses, which the fast path gives to the
    /// grammar.
    #[quickcheck]
    fn fused_matches_grammar_calc(source: Vec<u8>) -> bool {
        const ALPHABET: &[u8] = b"a1@[]()+*{}= \n";
        let source: Vec<u8> = source
            .iter()
            .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
            .collect();
        matches_grammar(&source, LexFlavor::DEFAULT) && matches_grammar(&source, LexFlavor::HOI4)
    }

    #[quickcheck]
    fn fused_matches_grammar_entries(entries: Vec<u8>) -> bool {
        const ENTRIES: &[&[u8]] = &[
            b"a=b",
            b"a!=b",
            b"a",
            b"{ }",
            b"{{}}",
            b"{a=b}",
            b"{a b=c}",
            b"x={1 a=b c={2} d=e}",
            b"x=rgb {}",
            b"x=rgb {1 2}",
            b"x {a=b}",
            b"x=$v$ {1}",
            b"x=@v {1}",
            b"a=\"b\"",
            b"x={a=b c d={1} e=f}",
            b"[[x] a=b ]",
            b"a=[Name]",
            b"code [[a=b]]",
            b"a=}",
            b"a=]=b",
            b"x={a=b",
            b"x={1 c={{} d=3} e=4}",
            b"a=\"b\" {}",
            b"a=\"b\" {1}",
            b"x={a=\"b\" {c=d} e}",
            b"{a=b}={1}",
            b"x={{a=b}={1 2}}",
            b"x={ = }",
            b"a=b=c",
            b"x=$v$ {}",
            b"x={1 a=@v {2}}",
            b"}",
            b"@:insert m = { a=b }",
            b"@:define m = { $x$ }",
            b"x=@:load_variable v",
            b"@:assert",
            b"@:log \"t\"",
            b"@@k=1",
            b"a<@@a{a}",
            b"x={1 @:insert m}",
            b"a=\"b\" {c",
            b"a=\"b\"\n{c=d}}",
            b"x={a=\"b\" {=} c=\"d\" {}}",
        ];
        let mut source = Vec::new();
        for entry in entries {
            source.extend_from_slice(ENTRIES[entry as usize % ENTRIES.len()]);
            source.push(b' ');
        }
        matches_grammar(&source, LexFlavor::DEFAULT) && matches_grammar(&source, LexFlavor::HOI4)
    }

    /// Assert that the lossless tape is the same as the `TextTape`.
    #[track_caller]
    fn same(data: &[u8]) {
        let expected = TextTape::from_slice(data).unwrap();
        let actual = parse_tape(data).into_tape();
        assert_eq!(
            actual.tokens(),
            expected.tokens(),
            "input: {}",
            String::from_utf8_lossy(data)
        );
        assert_eq!(actual.utf8_bom(), expected.utf8_bom());
    }

    /// In `a=]=b`, the `]` is the value of `a`, not the key of `]=b`. It is not
    /// a scalar, so the tape drops the field and the `=` is a stray operator.
    #[test]
    fn bracket_value_before_operator() {
        assert!(matches_grammar(b"a=]=b", LexFlavor::DEFAULT));
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
        // A block that holds only empty blocks is not empty in the source.
        same(b"a={{{}}}");
        same(b"a={{{}} 1}");
        same(b"a={ {{}} b=c }");
        same(b"a = rgb { {} }");
        same(b"a = LIST { {} {} }");
        same(b"a = { b = rgb { {} } }");
        same(b"a = { 1 c = { {} d = 3 } e = 4 }");
        same(b"a = { 1 c = { {} {} d = 3 } e = 4 }");
        same(b"a = { 1 c = { {} 2 } e = 4 }");
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
        same(b"a = $A$ { b }");
        same(b"a = @v { b }");
    }

    /// A quoted scalar before a block in a field value is a header, as an
    /// unquoted one is, also before a block on the next line. With an empty
    /// block, it is the quoted scalar. `TextTape::from_slice` rejects it.
    #[test]
    fn quoted_headers() {
        fn tokens(data: &[u8]) -> Vec<TextToken<'_>> {
            assert!(matches_grammar(data, LexFlavor::DEFAULT));
            parse_tape(data).into_result().unwrap().tokens().to_vec()
        }
        fn header(data: &[u8]) -> [TextToken<'_>; 6] {
            [
                TextToken::Unquoted(Scalar::new(b"block")),
                TextToken::QuotedHeader(Scalar::new(data)),
                TextToken::Object {
                    end: 5,
                    mixed: false,
                },
                TextToken::Unquoted(Scalar::new(b"visible")),
                TextToken::Unquoted(Scalar::new(b"no")),
                TextToken::End(2),
            ]
        }
        assert_eq!(
            tokens(b"block = \"tab_glow\" { visible = no }"),
            header(b"tab_glow")
        );
        assert_eq!(
            tokens(b"block = \"tab_glow\"\n{ visible = no }"),
            header(b"tab_glow")
        );
        assert_eq!(
            tokens(b"block = \"a\\\"b\" { visible = no }"),
            header(b"a\\\"b")
        );
        assert_eq!(
            tokens(b"a = \"x\" { } b = c"),
            [
                TextToken::Unquoted(Scalar::new(b"a")),
                TextToken::Quoted(Scalar::new(b"x")),
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"c")),
            ]
        );
        assert!(matches_grammar(
            b"foo = { 1 a = \"x\" { 1 2 } }",
            LexFlavor::DEFAULT
        ));
        assert!(matches_grammar(
            b"foo = { a = \"x\" { 1 } \"y\" { 2 } }",
            LexFlavor::DEFAULT
        ));
        // `TextTape::from_slice` rejects a quoted header, except when the
        // `jomini_syntax_tape` cfg builds it with the syntax parser.
        if !cfg!(jomini_syntax_tape) {
            assert!(TextTape::from_slice(b"block = \"tab_glow\" { visible = no }").is_err());
        }
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
        // A quote in an unquoted scalar does not start a string.
        same(b"names = { 10 = { a b\" } 11 = { c } }");
        same(b"foo = { blockoverride\"name\" { a = b } }");
    }

    #[test]
    fn object_template() {
        same(b"obj = { { a = b } = { c = d } }");
        same(b"needed = { { demand = pop } = { 1 2 } { a = b } = { 3 } }");
    }

    #[test]
    fn quoted_value_before_block() {
        // The form in an EU4 save.
        same(b"x={\n\ty=\"hanafi_school\"\n{\n\t}\n\t1398.1.1={\n\t\ta=b\n\t}\n}");
    }

    #[test]
    fn tape_parse_gives_the_tape_and_the_errors() {
        let parsed = parse_tape(b"a = b c = { d }");
        assert!(parsed.errors().is_empty());
        assert_eq!(parsed.tape().tokens().len(), 6);
        assert_eq!(parsed.into_result().unwrap().tokens().len(), 6);

        // The tape of a source with errors holds the fields after each error.
        let source = b"foo = { bar } } line = } last = 1";
        let tree = super::super::parse(source);
        let expected_errors = tree.errors();
        let expected_tokens = [
            TextToken::Unquoted(Scalar::new(b"foo")),
            TextToken::Array {
                end: 3,
                mixed: false,
            },
            TextToken::Unquoted(Scalar::new(b"bar")),
            TextToken::End(1),
            TextToken::Unquoted(Scalar::new(b"last")),
            TextToken::Unquoted(Scalar::new(b"1")),
        ];
        let parsed = parse_tape(source);
        assert_eq!(parsed.errors(), expected_errors);
        let kinds: Vec<_> = parsed.errors().iter().map(|e| e.kind).collect();
        assert_eq!(
            kinds,
            [
                SyntaxErrorKind::UnmatchedCloseBrace,
                SyntaxErrorKind::MissingValue,
                SyntaxErrorKind::UnmatchedCloseBrace,
            ]
        );
        assert_eq!(parsed.tape().tokens(), &expected_tokens);
        let (tape, errors) = parse_tape(source).into_parts();
        assert_eq!(tape.tokens(), &expected_tokens);
        assert_eq!(errors, expected_errors);

        // The result is the error that comes first in the source, also when
        // the parser recorded it last.
        let source = b"a = { b = \"c";
        let parsed = parse_tape(source);
        let kinds: Vec<_> = parsed.errors().iter().map(|e| e.kind).collect();
        assert_eq!(
            kinds,
            [
                SyntaxErrorKind::UnclosedBlock,
                SyntaxErrorKind::UnclosedQuote
            ]
        );
        let error = parsed.into_result().unwrap_err();
        assert_eq!(error.kind, SyntaxErrorKind::UnclosedQuote);
        assert_eq!(error.range.to_range(), 10..11);
    }

    /// The token window of the tape parse no longer holds the token that
    /// opens a long construct when the construct ends. The error still has
    /// the range of that token.
    #[test]
    fn errors_of_long_constructs() {
        let source = b"a = { b = c d = e f = g h = i j = k";
        let errors = parse_tape(source).errors().to_vec();
        assert_eq!(errors, super::super::parse(source).errors());
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, SyntaxErrorKind::UnclosedBlock);
        assert_eq!(errors[0].range.to_range(), 35..35);
        let missing = errors[0].missing_delimiter.as_ref().unwrap();
        assert_eq!(missing.opening_range.to_range(), 4..5);

        let source = b"x = { [[p] a = b c = d e = f } }";
        let errors = parse_tape(source).errors().to_vec();
        assert_eq!(errors, super::super::parse(source).errors());
        let found: Vec<_> = errors
            .iter()
            .map(|e| (e.kind, e.range.to_range()))
            .collect();
        assert_eq!(
            found,
            [
                (SyntaxErrorKind::UnclosedParameter, 6..7),
                (SyntaxErrorKind::UnmatchedCloseBrace, 31..32),
            ]
        );

        let source = b"a = @[(1 + (2 * 3) + 4 + 5 + 6";
        let errors = parse_tape(source).errors().to_vec();
        assert_eq!(errors, super::super::parse(source).errors());
        let found: Vec<_> = errors
            .iter()
            .map(|e| {
                let missing = e.missing_delimiter.as_ref().unwrap();
                (e.kind, missing.opening_range.to_range())
            })
            .collect();
        assert_eq!(
            found,
            [
                (SyntaxErrorKind::UnclosedCalcParen, 6..7),
                (SyntaxErrorKind::UnclosedCalc, 4..6),
            ]
        );
    }

    #[test]
    fn recovery() {
        // `TextTape` closes one unclosed block at EOF as an object.
        same(b"foo = { a = b");
        // A stray `}` at the top level is ignored.
        same(b"a = b } c = d");

        // An operator as a value is a scalar.
        let tape = parse_tape(b"OPERATOR = <=").into_tape();
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"OPERATOR")),
                TextToken::Unquoted(Scalar::new(b"<=")),
            ]
        );

        // A field with no value is ignored.
        let tape = parse_tape(b"a = } b = c").into_tape();
        assert_eq!(
            tape.tokens(),
            &[
                TextToken::Unquoted(Scalar::new(b"b")),
                TextToken::Unquoted(Scalar::new(b"c")),
            ]
        );

        // Each unclosed block closes at EOF.
        let tape = parse_tape(b"a = { b = { c = d").into_tape();
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
        let tape = parse_tape(b"a = [GetName] b = code [[ x = 1 ]] c = { 1 } } d").into_tape();
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
