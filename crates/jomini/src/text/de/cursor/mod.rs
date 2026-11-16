//! The engine of [`TextDeserializer`].
//!
//! The deserializer owns the [`Source`] of bytes and the state of the
//! tokenizer. Map, sequence, and value accessors hold a mutable borrow of the
//! deserializer, never a token's bytes. A token is its kind and offsets into
//! the source's window of unconsumed bytes, and its bytes are resolved only
//! inside the call that hands them to a visitor, so the borrow checker
//! proves that no bytes are held while the source refills.
//!
//! Reading a token does not consume it: its bytes are consumed when the
//! next token is scanned. A refill keeps the unconsumed bytes, so the
//! offsets of a read token, and of tokens looked ahead to, stay valid while
//! `deserialize_any` looks past them.
//!
//! The same engine reads a slice, whose scalars are visited as borrowed
//! strings, and a reader, whose scalars are visited as transient strings.

#![deny(unsafe_code)]

mod scan;
mod source;
mod wrappers;

pub use self::source::{ReaderSource, SliceSource};

use self::scan::{Kind, Peek, Skip, Status, Tok};
use self::source::{Reference, Source};
use self::wrappers::{AnyRemainder, OperatorDeserializer};
use crate::{
    Encoding, Error, ErrorKind, Scalar,
    text::{Operator, decode_quoted},
};
use serde::de::{self, Deserialize, DeserializeOwned, DeserializeSeed, Visitor};
use std::{borrow::Cow, io::Read};

/// Return true when the byte starts an operator.
#[inline]
fn is_operator_start(x: u8) -> bool {
    matches!(x, b'=' | b'<' | b'>' | b'!' | b'?')
}

/// A serde deserializer for text data that streams from a slice or a reader.
///
/// The data is read as it is deserialized, so a reader's data isn't buffered
/// in full: only a window of it is kept, which must hold the longest token.
/// From a slice, strings are borrowed from the data where the encoding
/// allows, as in `&'a str` fields. The source `S` is a [`SliceSource`] from
/// [`from_slice`](Self::from_slice) or a [`ReaderSource`] from
/// [`from_reader`](Self::from_reader). Nothing can borrow from a reader's
/// buffer, so its `deserialize` takes a `DeserializeOwned` type.
///
/// The root of the data is an object. Its fields can be deserialized into a
/// struct or a map, and `deserialize_any` reads the data as a self-describing
/// format: a block whose first item is a field is a map, and any other block
/// is a sequence, whose empty blocks are elements. A `}` at the root, which
/// closes no block, is skipped, and the end of the data closes the blocks
/// that are open, as the games do.
///
/// An enum variant is a scalar. A variant with data is a field value with a
/// header, as in `color = rgb { 1 2 3 }`, whose block is the data.
///
/// A value without a key in an object, as the `80` of `{ 80 0=1 1=2 }`,
/// starts the remainder of the object: a struct reads every part that is
/// left, keys, operators, and values, as a sequence in its `remainder`
/// field.
///
/// It reads what the lossless parser, `jomini::text::syntax::parse`, reads,
/// or gives an error that names it: for a parameter or code block of
/// script, as in `[[scope] set_variable = x ]`, a scalar that starts with a
/// bracket, as in `[GetName]`, a `!` or `?` that is not part of an operator,
/// a value without a key in a map that is not a struct, an object read as a
/// sequence or used as a key, an operator as a value, a header before a map, an enum written as a
/// block or whose header has no block, a sequence longer than its tuple,
/// and a value that is read again after its read failed.
///
/// For game and mod files, `jomini::text::syntax::parse(data)` and its
/// `deserialize(encoding)` or `document(encoding)` give errors with a line,
/// a column, and the path of keys, resolve `@variables`, and read files
/// with syntax errors, at the cost of a syntax tree in memory.
///
/// [`MaybeQuoted`](crate::text::MaybeQuoted) and
/// [`Headered`](crate::text::Headered) keep the quotes and the header of a
/// value, and with the `syntax` feature an owned value of the syntax module
/// keeps all of a value. An owned value is parsed as
/// `jomini::text::syntax::parse` parses it, with the default flavor, since
/// `E` is only an encoding, so for a flavor that reads data differently, as
/// `Hoi4Flavor` reads `$NAME$`, it can differ from the value of `parse_with`
/// and that flavor.
///
/// An error carries the byte offset of the token that caused it, if the
/// error kind has no offset of its own. An unexpected end of the data, in a
/// quoted string or where a value is expected, is at the length of the
/// data.
///
/// ```
/// # #[cfg(feature = "derive")] {
/// use jomini::{Windows1252Encoding, text::de::TextDeserializer};
/// use serde::Deserialize;
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Save<'a> {
///     player: &'a str,
///     date: jomini::common::Date,
///     prestige: f64,
/// }
///
/// let data = b"player=\"ENG\"\ndate=1444.11.11\nprestige=12.5";
/// let encoding = Windows1252Encoding::new();
/// let save: Save = TextDeserializer::from_slice(data, encoding).deserialize()?;
/// assert_eq!(save.player, "ENG");
/// assert_eq!(save.prestige, 12.5);
///
/// // A reader is read in a fixed size buffer.
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Owned {
///     player: String,
/// }
///
/// let save: Owned = TextDeserializer::from_reader(&data[..], encoding).deserialize()?;
/// assert_eq!(save.player, "ENG");
///
/// // A field whose value can't be deserialized reports where it starts.
/// let err = TextDeserializer::from_slice(b"prestige=high", encoding)
///     .deserialize::<Save>()
///     .unwrap_err();
/// assert_eq!(err.offset(), Some(9));
/// # }
/// # Ok::<(), jomini::Error>(())
/// ```
pub struct TextDeserializer<S, E> {
    source: S,
    encoding: E,

    /// The bytes at the front of the window that belong to tokens that were
    /// read. They are consumed when the next token is scanned, so the span
    /// of a read token stays valid until then.
    pending: usize,

    /// The token that was scanned last. The scanner writes it in place, and
    /// callers get copies.
    tok: Tok,

    /// Whether `tok` was scanned but not read.
    peeked: bool,

    /// The operator of the field with an empty key that is being read. The
    /// key consumed the operator, so the value does not read one.
    empty_key_op: Option<Operator>,

    /// How far a map has read the remainder of an object that a value
    /// without a key started.
    remainder: Remainder,

    /// The offset after a value whose read failed, so that reading the
    /// value again is an error rather than a read of what follows it, and
    /// the offset of the value.
    failed_at: Option<(usize, usize)>,
}

/// The remainder of an object, from the first value without a key, as the
/// `80` of `{ 80 0=1 1=2 }`, or the `c` of `{ a=b c d }`, to the end of the
/// object, which a map yields as a sequence under the key `remainder`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Remainder {
    None,

    /// The value without a key, the token, was read as a key, and its
    /// value, which it doesn't have, is read next.
    KeyValue(Tok),

    /// The key `remainder` is read next.
    Key(Tok),

    /// The sequence is read next, from the value without a key, whose token
    /// is still in the window, as nothing was scanned since it was read.
    Value {
        root: bool,
        first: Tok,
    },

    /// The value without a key, the token, was read as a key of a map that
    /// is not a struct, which has no field for the remainder, so its value
    /// is an error.
    MapKeyValue(Tok),
}

/// What follows a block where a key is expected.
enum AfterOpen {
    Key,
    End,
    Remainder,
}

impl<'de, E> TextDeserializer<SliceSource<'de>, E> {
    /// Deserialize the bytes of `data`, which strings can borrow from.
    pub fn from_slice(data: &'de [u8], encoding: E) -> Self {
        Self::new(SliceSource::new(data), encoding)
    }

    /// Deserialize the data into `T`, which can borrow strings from it.
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: Deserialize<'de>,
        E: Encoding,
    {
        T::deserialize(self)
    }
}

impl<R: Read, E> TextDeserializer<ReaderSource<R>, E> {
    /// Deserialize the data into `T`. A reader's data is not kept, so `T`
    /// can't borrow from it.
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
        E: Encoding,
    {
        T::deserialize(self)
    }

    /// Deserialize data from `reader` with a 32 KiB buffer.
    pub fn from_reader(reader: R, encoding: E) -> Self {
        Self::new(ReaderSource::new(reader), encoding)
    }

    /// Deserialize data from `reader` read into `buffer`, whose length is the
    /// capacity. The longest token and the byte after it must fit in the
    /// buffer, or the error is
    /// [`ErrorKind::BufferTooSmall`](crate::ErrorKind::BufferTooSmall).
    /// `deserialize_any` holds a scalar while it looks past it for an
    /// operator or the block of a header, so there the scalar, the open token
    /// of a header, and the byte after each must fit together.
    pub fn from_reader_with_buf(reader: R, encoding: E, buffer: Vec<u8>) -> Self {
        Self::new(ReaderSource::with_buf(reader, buffer), encoding)
    }
}

impl<S, E> TextDeserializer<S, E> {
    fn new(source: S, encoding: E) -> Self {
        TextDeserializer {
            source,
            encoding,
            pending: 0,
            tok: Tok::empty(0),
            peeked: false,
            empty_key_op: None,
            remainder: Remainder::None,
            failed_at: None,
        }
    }
}

impl<'de, S: Source<'de>, E> TextDeserializer<S, E> {
    /// An unexpected end of the data, whose offset is the length of the
    /// data.
    #[cold]
    #[inline(never)]
    fn eof(&self) -> Error {
        let end = self.source.offset(self.source.window().len());
        Error::eof().or_offset(end)
    }

    /// The offset in the data of the token that was scanned last, or of the
    /// opening quote of a quoted scalar.
    #[cold]
    #[inline(never)]
    fn tok_offset(&self) -> usize {
        let quote = usize::from(self.tok.kind == Kind::Quoted);
        self.source.offset(self.tok.start() - quote)
    }

    /// Give an error without an offset the offset of the token that was
    /// scanned last, which caused it.
    #[cold]
    #[inline(never)]
    fn locate(&self, err: Error) -> Error {
        let offset = self.tok_offset();
        err.or_offset(offset)
    }

    /// A token that isn't read, at the token that was scanned last: a
    /// scalar that starts with a bracket, as in `[GetName]`, or a `!` or `?`
    /// that is not part of an operator.
    #[cold]
    #[inline(never)]
    fn unsupported_token(&self) -> Error {
        let what = match self.source.window().get(self.tok.start()) {
            Some(b'[') => "a scalar that starts with a bracket, as in `picture = [GetName]`",
            _ => "a `!` or `?` that is not part of an operator, as in `a ! b`",
        };
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read {what}, which {LOSSLESS} reads"
        ))
        .or_offset(self.tok_offset())
    }

    /// A parameter block of script, as in `[[name] effect = x ]`, at the
    /// token that was scanned last.
    #[cold]
    #[inline(never)]
    fn parameter_block(&self) -> Error {
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read a parameter block or a code block, as in `[[name] effect = x ]` or `code [[ x ]]`, which {LOSSLESS} reads"
        ))
        .or_offset(self.tok_offset())
    }

    /// An error from a sequence, which names the lossless parser when the
    /// token that failed is a field of an object: an operator, or a scalar
    /// followed by one. The error takes the offset of the token, which is
    /// then consumed, as the next scan would, to make room to look past it.
    #[cold]
    #[inline(never)]
    fn seq_error(&mut self, err: Error) -> Error {
        let err = err.or_offset(self.tok_offset());
        let field = match self.tok.kind {
            Kind::Operator => true,
            Kind::Unquoted | Kind::Quoted if !self.peeked => {
                self.source.consume(self.pending);
                self.pending = 0;
                self.tok = Tok::empty(0);
                matches!(self.peek_byte(0, 0), Ok(Some((_, x))) if is_operator_start(x))
            }
            _ => false,
        };
        if field {
            lossless_hint(err, SEQUENCE_FIELDS)
        } else {
            err
        }
    }

    /// An error from the variant of an enum, which is a block that was
    /// rejected and skipped when the enum is written as a block, as in
    /// `a = { rgb = { 1 2 3 } }`.
    #[cold]
    #[inline(never)]
    fn variant_error(&self, err: Error) -> Error {
        if self.tok.kind == Kind::Close
            && !self.tok.is_no_value()
            && matches!(err.kind(), ErrorKind::Deserialize(_))
        {
            lossless_hint(err, BLOCK_ENUM)
        } else {
            err
        }
    }

    /// Skip a utf8 byte order mark at the start of the data.
    fn skip_bom(&mut self) -> Result<(), Error> {
        while self.source.window().len() < 3 && self.source.refill()? {}
        if self.source.window().starts_with(b"\xef\xbb\xbf") {
            self.pending = 3;
        }
        Ok(())
    }

    /// Make the next token `tok` without reading it, or return `false`
    /// at the end of the data.
    ///
    /// Hot paths use `tok` after this rather than a returned `Tok`: a
    /// token wrapped in an `Option` or `Result` is checked for the variant
    /// encoded in its kind, which is loaded from memory, so the check isn't
    /// optimized out.
    #[inline(always)]
    fn fill(&mut self) -> Result<bool, Error> {
        if !self.peeked {
            self.scan()?;
            return Ok(self.peeked);
        }
        Ok(true)
    }

    /// Return the next token without reading it.
    #[inline(always)]
    fn peek(&mut self) -> Result<Option<Tok>, Error> {
        Ok(if self.fill()? { Some(self.tok) } else { None })
    }

    /// Read the peeked token. Its span stays valid until the next peek.
    #[inline(always)]
    fn take(&mut self, tok: Tok) {
        self.peeked = false;
        self.pending = tok.next();
    }

    /// Read the next token into `tok`, or return `false` at the end of the
    /// data.
    #[inline(always)]
    fn advance(&mut self) -> Result<bool, Error> {
        if !self.peeked {
            self.scan()?;
            if !self.peeked {
                return Ok(false);
            }
        }
        self.peeked = false;
        self.pending = self.tok.next();
        Ok(true)
    }

    /// Read the next token, which must exist, into `tok`.
    #[inline(always)]
    fn read(&mut self) -> Result<(), Error> {
        if self.advance()? {
            Ok(())
        } else {
            Err(self.scan_failed(self.eof()))
        }
    }

    /// Consume the pending bytes and scan the token at the front of the
    /// window into `tok`, which is peeked unless the data ended.
    ///
    /// The result is only the error, which fits in a register.
    #[inline(never)]
    fn scan(&mut self) -> Result<(), Error> {
        self.source.consume(self.pending);
        self.pending = 0;

        // The frequent tokens are scanned without the registers that the
        // other tokens need, so this path saves none.
        if let Some(tok) = scan::token_fast(self.source.window()) {
            self.tok = tok;
            self.peeked = true;
            return Ok(());
        }
        self.scan_slow()
    }

    #[inline(never)]
    fn scan_slow(&mut self) -> Result<(), Error> {
        match scan::token(self.source.window(), 0, self.source.eof(), &mut self.tok) {
            Status::Token => {
                self.peeked = true;
                Ok(())
            }
            Status::End => {
                self.end_tok();
                Ok(())
            }
            _ => self.scan_refill(),
        }
    }

    #[cold]
    #[inline(never)]
    fn scan_refill(&mut self) -> Result<(), Error> {
        loop {
            match scan::token(self.source.window(), 0, self.source.eof(), &mut self.tok) {
                Status::Token => {
                    self.peeked = true;
                    return Ok(());
                }
                Status::End => {
                    self.end_tok();
                    return Ok(());
                }
                Status::Unterminated => return Err(self.scan_failed(self.eof())),
                Status::Parameter => return Err(self.scan_failed(self.parameter_block())),
                Status::Unsupported => return Err(self.scan_failed(self.unsupported_token())),
                Status::More => {
                    // Only whitespace precedes the token, so it is dropped
                    // to leave room for the token in the buffer.
                    self.source.consume(self.tok.start());
                    self.tok = Tok::empty(0);
                    if let Err(err) = self.source.refill() {
                        return Err(self.scan_failed(err));
                    }
                }
                Status::Comment => {
                    // Only the `#` of the comment is kept, so the rescan
                    // knows it is in a comment.
                    let len = self.source.window().len();
                    self.source.remove(self.tok.start() + 1, len);
                    self.source.consume(self.tok.start());
                    self.tok = Tok::empty(0);
                    if let Err(err) = self.source.refill() {
                        return Err(self.scan_failed(err));
                    }
                }
            }
        }
    }

    /// Keep where the token whose scan failed is, as `failed` does for a
    /// value, so that a read of the value again, as a retry in another type,
    /// is an error rather than a read from a token that wasn't scanned.
    #[cold]
    #[inline(never)]
    fn scan_failed(&mut self, err: Error) -> Error {
        let at = err.offset().unwrap_or_else(|| self.tok_offset());
        self.failed_at = Some((self.source.offset(self.pending), at));
        err
    }

    /// An operator that was read where a value is expected, as after another
    /// operator or in an object read as a sequence.
    #[cold]
    #[inline(never)]
    fn operator_value(&self) -> Error {
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which {LOSSLESS} reads"
        ))
        .or_offset(self.tok_offset())
    }

    /// A sequence with more elements than the visitor read, as a tuple.
    #[cold]
    #[inline(never)]
    fn long_sequence(&self) -> Error {
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read a sequence with more elements than a tuple has, which {LOSSLESS} reads"
        ))
        .or_offset(self.tok_offset())
    }

    /// Read the peeked operator that is an element of a sequence, as in the
    /// remainder of an object, which a value follows, as after any operator.
    #[cold]
    #[inline(never)]
    fn operator_element(&mut self, tok: Tok) -> Result<(), Error> {
        self.take(tok);
        match self.peek()? {
            None => Err(self.eof()),
            Some(next) if next.kind == Kind::Close => Err(self.unexpected_close()),
            Some(_) => Ok(()),
        }
    }

    /// A close that was read where a value is expected.
    #[cold]
    #[inline(never)]
    fn unexpected_close(&self) -> Error {
        Error::invalid_syntax("did not expect end", self.tok_offset())
    }

    /// Make `tok` an empty token at the end of the data, which the scan
    /// found, so that an error raised at the end is located there.
    #[cold]
    fn end_tok(&mut self) {
        self.tok = Tok::empty(self.source.window().len());
    }

    /// Return the offset and value of the first byte at or after `at` that
    /// is not whitespace or a comment, refilling without consuming. The
    /// bytes in `window[keep..at]` were already looked past and aren't
    /// needed.
    ///
    /// To make room in the buffer, the bytes after `keep` that were looked
    /// past are removed from the window before a refill, so offsets before
    /// `keep` stay valid, and the returned offset is into the new window.
    #[inline]
    fn peek_byte(&mut self, keep: usize, mut at: usize) -> Result<Option<(usize, u8)>, Error> {
        loop {
            match scan::peek_byte(self.source.window(), at, self.source.eof()) {
                Peek::Byte(at, x) => return Ok(Some((at, x))),
                Peek::End => return Ok(None),
                Peek::More(comment) => {
                    let len = self.source.window().len();
                    match comment {
                        // Keep the `#` so the rescan knows it is in a comment.
                        Some(start) => {
                            self.source.remove(start + 1, len);
                            self.source.remove(keep, start);
                        }
                        None => self.source.remove(keep, len),
                    }
                    at = keep;
                    self.source.refill()?;
                }
            }
        }
    }

    /// Read the operator that comes next, or return `None` and leave the
    /// next token peeked.
    #[inline]
    fn read_operator(&mut self) -> Result<Option<Operator>, Error> {
        if !self.peeked
            && let Some([b'=', next]) = self.source.window().get(self.pending..self.pending + 2)
            && *next != b'='
        {
            self.pending += 1;
            return Ok(Some(Operator::Equal));
        }

        self.read_operator_slow()
    }

    /// Read the operator after a key that is not right after it. A key that
    /// no operator or block follows is a value without a key, which starts
    /// the remainder of the object, and the value of the key `remainder`
    /// has no operator.
    #[inline(never)]
    fn read_operator_slow(&mut self) -> Result<Option<Operator>, Error> {
        // The key of a field with an empty key consumed its operator, and
        // what follows is the value, an operator too.
        if let Some(op) = self.empty_key_op.take() {
            return Ok(Some(op));
        }

        // A retry after `failed` peeked an `=` reads the `=` peeked.
        if self.failed_at.is_some() && (!self.peeked || self.tok.kind == Kind::Operator) {
            self.read_again()?;
        }

        if !self.peeked {
            if matches!(self.remainder, Remainder::Value { .. }) {
                return Ok(None);
            }

            // The key may be the first part of the remainder, so it is kept
            // in the window, and the bytes before it are consumed to make
            // room to look past it.
            let start = self.tok.start() - usize::from(self.tok.kind == Kind::Quoted);
            if start > self.pending {
                // The token is not a key that was read, as after a value
                // whose read failed, so this is a read of the value again.
                return Err(self.value_read_again(self.source.offset(self.pending)));
            }
            self.source.consume(start);
            self.pending -= start;
            self.tok = self.tok.shifted(start);
            match self.peek_byte(self.pending, self.pending)? {
                Some((_, x)) if is_operator_start(x) || x == b'{' => {}
                _ => {
                    self.remainder = Remainder::KeyValue(self.tok);
                    return Ok(None);
                }
            }
        }

        match self.peek()? {
            Some(
                tok @ Tok {
                    kind: Kind::Operator,
                    op,
                    ..
                },
            ) => {
                self.take(tok);
                Ok(Some(op))
            }
            _ => Ok(None),
        }
    }

    /// Skip through the block whose open token was read.
    fn skip_block(&mut self) -> Result<(), Error> {
        self.skip_blocks(Skip::new())
    }

    /// Skip from the pending bytes to the end of the blocks that `state`
    /// is in.
    fn skip_blocks(&mut self, mut state: Skip) -> Result<(), Error> {
        self.source.consume(self.pending);
        self.pending = 0;
        match scan::skip_block(self.source.window(), &mut state) {
            Ok(end) => {
                self.skipped(end);
                Ok(())
            }
            Err(processed) => self.skip_block_refill(state, processed),
        }
    }

    /// Continue skipping a block that runs past the window, whose first
    /// `processed` bytes were skipped. The end of the data closes the block,
    /// unless it ends in a quoted string.
    #[cold]
    #[inline(never)]
    fn skip_block_refill(&mut self, mut state: Skip, mut processed: usize) -> Result<(), Error> {
        loop {
            self.source.consume(processed);
            if !self.source.refill()? {
                if state.in_quote() {
                    return Err(self.eof());
                }

                self.pending = self.source.window().len();
                self.end_tok();
                return Ok(());
            }

            match scan::skip_block(self.source.window(), &mut state) {
                Ok(end) => {
                    self.skipped(end);
                    return Ok(());
                }
                Err(x) => processed = x,
            }
        }
    }

    /// Read the close token of a skipped block, which ends at `end`. The
    /// window was consumed up to the block, so the token that was read
    /// before is replaced.
    #[inline]
    fn skipped(&mut self, end: usize) {
        self.pending = end;
        self.tok = Tok::close(end - 1);
    }

    /// Start a field with an empty key, as in `=1825.6.28` in an EU4 save,
    /// whose operator was read: the key is an empty scalar. An `=` right
    /// after the operator, which the value would read as its operator, is
    /// an operator as the value, so it is read as the key instead, which is
    /// the same error. The operator is consumed, and the window refilled
    /// when it ends at the operator, so a reader sees the `=` that a slice
    /// does.
    #[cold]
    fn empty_key(&mut self, op: Operator) -> Result<(), Error> {
        self.empty_key_op = Some(op);
        self.source.consume(self.pending);
        self.pending = 0;
        while self.source.window().is_empty() && self.source.refill()? {}
        self.tok = if self.source.window().first() == Some(&b'=') {
            Tok::operator(Operator::Equal, 0, 1)
        } else {
            Tok::empty(0)
        };
        self.peeked = true;
        Ok(())
    }

    /// Return the operator of a field whose value does not follow an
    /// operator: the operator of the empty key, or `=` for `key { ... }` and
    /// `key value`. A key that the end of its block or of the data follows,
    /// as the trailing `c` of `{ a = b c }`, has no value: a close of no
    /// bytes is peeked, which reads as a unit, and the close of the block
    /// stays to be read.
    #[cold]
    #[inline(never)]
    fn missing_operator(&mut self, is_struct: bool) -> Operator {
        if let Some(op) = self.empty_key_op.take() {
            return op;
        }

        match self.remainder {
            Remainder::KeyValue(first) if !is_struct => {
                self.remainder = Remainder::MapKeyValue(first);
                self.tok = Tok::no_value(self.pending);
                self.peeked = true;
                return Operator::Equal;
            }
            Remainder::KeyValue(_) | Remainder::Value { .. } => {
                self.tok = Tok::no_value(self.pending);
                self.peeked = true;
                return Operator::Equal;
            }
            _ => {}
        }

        if !self.peeked || self.tok.kind == Kind::Close {
            let at = if self.peeked {
                self.tok.start()
            } else {
                self.source.window().len()
            };
            self.tok = Tok::no_value(at);
            self.peeked = true;
        }
        Operator::Equal
    }

    /// Accept the close that was read where a value is expected when it is
    /// the close of no bytes of a key that has no value, or give an error,
    /// as for an operator there.
    #[cold]
    #[inline(never)]
    fn close_as_value(&mut self) -> Result<(), Error>
    where
        E: Encoding,
    {
        if self.tok.kind == Kind::Operator {
            return Err(self.operator_value());
        }

        if !self.tok.is_no_value() {
            return Err(self.unexpected_close());
        }

        match self.remainder {
            Remainder::KeyValue(first) => self.remainder_key(first),
            Remainder::Value { root, first } => {
                let mut seq = self.remainder_seq(root, first);
                while de::SeqAccess::next_element::<de::IgnoredAny>(&mut seq)?.is_some() {}
            }
            Remainder::MapKeyValue(first) => return Err(self.map_key_value(first)),
            _ => {}
        }
        Ok(())
    }

    /// The error for a value without a key in a map that is not a struct,
    /// which has no field for the remainder, at the value.
    #[cold]
    #[inline(never)]
    fn map_key_value(&mut self, first: Tok) -> Error {
        self.remainder = Remainder::None;
        let quote = usize::from(first.kind == Kind::Quoted);
        let offset = self.source.offset(first.start() - quote);
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read a value without a key, as the `80` of `{{ 80 0=1 }}`, except into the `remainder` field of a struct, which {LOSSLESS} reads"
        ))
        .or_offset(offset)
    }

    /// The error for a value that is read again after its read failed, as
    /// a retry in another type does, at the value.
    #[cold]
    #[inline(never)]
    fn read_again(&mut self) -> Result<(), Error> {
        let at = self.source.offset(self.pending);
        match self.failed_at.take() {
            Some((end, offset)) if end == at => Err(self.value_read_again(offset)),
            _ => Ok(()),
        }
    }

    /// The error of a value read again after its read failed, at `offset`.
    #[cold]
    #[inline(never)]
    fn value_read_again(&self, offset: usize) -> Error {
        Error::deserialize_msg(format!(
            "TextDeserializer doesn't read a value again after its read failed, which {LOSSLESS} does"
        ))
        .or_offset(offset)
    }

    /// Make a block at the start of the data that is not empty, a value
    /// without a key, start the remainder of the root, as in a file of
    /// blocks without keys. The map reads the key `remainder` first, since
    /// nothing was read before it.
    #[cold]
    #[inline(never)]
    fn block_at_root(&mut self) -> Result<(), Error> {
        if let Some(tok) = self.peek()?
            && tok.kind == Kind::Open
            && !matches!(self.peek_byte(tok.next(), tok.next())?, Some((_, b'}')))
        {
            self.remainder_key(tok);
        }
        Ok(())
    }

    /// Make the key `remainder` come next, after the value of the value
    /// without a key `first`.
    #[cold]
    fn remainder_key(&mut self, first: Tok) {
        self.tok = Tok::remainder(self.pending);
        self.peeked = true;
        self.remainder = Remainder::Key(first);
    }

    /// The sequence of the remainder, whose first part, the value without a
    /// key, is peeked again.
    #[cold]
    fn remainder_seq(&mut self, root: bool, first: Tok) -> RemainderSeq<'_, S, E> {
        self.remainder = Remainder::None;
        self.tok = first;
        self.peeked = true;
        RemainderSeq { de: self, root }
    }

    /// Read what follows the block whose open token was read where a key is
    /// expected: the key `remainder`, which a value without a key started, or
    /// the next key, after the block and the field it is the key of.
    #[cold]
    #[inline(never)]
    fn key_after_open(&mut self, root: bool) -> Result<AfterOpen, Error> {
        if self.tok.is_remainder()
            && let Remainder::Key(first) = self.remainder
        {
            self.remainder = Remainder::Value { root, first };
            return Ok(AfterOpen::Remainder);
        }

        if self.key_after_block(root)? {
            Ok(AfterOpen::Key)
        } else {
            Ok(AfterOpen::End)
        }
    }

    /// Skip the peeked close at the root, which has no block to close, as a
    /// `TextTape` does.
    #[cold]
    #[inline(never)]
    fn skip_root_close(&mut self) {
        self.take(self.tok);
    }

    /// Skip the block that was read where a key is expected, and skip the
    /// field when the block is its key, as in `{ a = b }={ 1 2 3 }`. Return
    /// whether a key was peeked, or `false` at the end of the object or of
    /// the data. A close at the root is skipped.
    #[cold]
    #[inline(never)]
    fn key_after_block(&mut self, root: bool) -> Result<bool, Error> {
        let mut after_block = true;
        self.skip_block()?;
        loop {
            let Some(tok) = self.peek()? else {
                return Ok(false);
            };

            match tok.kind {
                Kind::Close if root => self.take(tok),
                Kind::Close => {
                    self.take(tok);
                    return Ok(false);
                }
                Kind::Open => {
                    self.take(tok);
                    self.skip_block()?;
                    after_block = true;
                }
                Kind::Operator if after_block => {
                    self.take(tok);
                    self.read()?;
                    match self.tok.kind {
                        Kind::Open => self.skip_block()?,
                        Kind::Close if !root => return Ok(false),
                        _ => {}
                    }
                    after_block = false;
                }
                Kind::Operator => {
                    self.take(tok);
                    self.empty_key(tok.op)?;
                    return Ok(true);
                }
                Kind::Unquoted | Kind::Quoted => return Ok(true),
            }
        }
    }

    /// Look past the enum variant that was read for the block of its data,
    /// as in `color = rgb { 1 2 3 }`, and leave the open token to be scanned
    /// next. The variant was visited, so it is consumed with the bytes looked
    /// past, and only the open token is kept while looking past it. An empty
    /// block, which a `TextTape` drops, is not the variant's block, nor is an
    /// open token that the end of the data closes, and a quoted variant has
    /// no block. The error is at the variant.
    #[cold]
    #[inline(never)]
    fn header_block(&mut self) -> Result<(), Error> {
        let offset = self.tok_offset();
        let no_block = || no_variant_block().or_offset(offset);

        // A quoted scalar is not a header.
        if self.peeked || self.tok.kind == Kind::Quoted {
            return Err(no_block());
        }

        self.source.consume(self.pending);
        self.pending = 0;
        let Some((open, b'{')) = self.peek_byte(0, 0)? else {
            return Err(no_block());
        };

        self.source.consume(open);
        self.tok = Tok::empty(0);
        if matches!(self.peek_byte(1, 1)?, Some((_, b'}')) | None) {
            return Err(no_block());
        }

        Ok(())
    }

    #[inline]
    fn scalar(&self, tok: Tok) -> Option<Scalar<'_>> {
        if tok.is_scalar() {
            Some(Scalar::new(&self.source.window()[tok.start()..tok.end()]))
        } else {
            None
        }
    }
}

impl<'de, S: Source<'de>, E: Encoding> TextDeserializer<S, E> {
    /// Visit the text of a scalar that was read: borrowed from the input
    /// when the source and encoding allow.
    #[inline(always)]
    fn visit_scalar<V>(&self, tok: Tok, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let quoted = tok.kind == Kind::Quoted;
        match self.source.resolve(tok.start(), tok.end()) {
            Reference::Borrowed(data) => match self.decode(data, quoted) {
                Cow::Borrowed(x) => visitor.visit_borrowed_str(x),
                Cow::Owned(x) => visitor.visit_string(x),
            },
            Reference::Copied(data) => match self.decode(data, quoted) {
                Cow::Borrowed(x) => visitor.visit_str(x),
                Cow::Owned(x) => visitor.visit_string(x),
            },
        }
    }

    #[inline]
    fn decode<'b>(&self, data: &'b [u8], quoted: bool) -> Cow<'b, str> {
        if quoted {
            decode_quoted(&self.encoding, data)
        } else {
            self.encoding.decode(data)
        }
    }

    /// Visit a token that was read without looking ahead: a scalar as its
    /// text, an operator as its symbol, and a block as a sequence. Typed
    /// deserialization falls back to this when the token does not match the
    /// requested type, so the visitor can report the mismatch.
    fn visit_token<V>(&mut self, tok: Tok, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match tok.kind {
            Kind::Open => self.visit_block_seq(visitor),
            Kind::Close => self.visit_close(visitor),
            Kind::Operator => Err(self.operator_value()),
            Kind::Unquoted | Kind::Quoted => self.visit_scalar(tok, visitor),
        }
    }

    /// Visit the close that was read where a value is expected: a unit for
    /// the close of no bytes of a key that has no value, as a scalar that
    /// trails the fields of an object, or an error.
    #[cold]
    #[inline(never)]
    fn visit_close<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let Some(root) = self.close_value()? else {
            return visitor
                .visit_unit()
                .map_err(|err| lossless_hint(err, TRAILING_SCALARS));
        };

        match visitor.visit_seq(RemainderSeq { de: self, root }) {
            Err(err) => Err(self.remainder_rejected(err, root)),
            result => result,
        }
    }

    /// The part of `visit_close` that doesn't depend on the visitor, so that
    /// it isn't compiled again for each type: `Some(root)` when the close
    /// starts the remainder, which is set up to be read as a sequence, and
    /// `None` when it is a unit.
    #[cold]
    #[inline(never)]
    fn close_value(&mut self) -> Result<Option<bool>, Error> {
        if !self.tok.is_no_value() {
            return Err(self.unexpected_close());
        }

        match self.remainder {
            Remainder::KeyValue(first) => {
                let quote = usize::from(first.kind == Kind::Quoted);
                let offset = self.source.offset(first.start() - quote);
                self.remainder_key(first);
                Err(Error::deserialize_msg(format!(
                    "TextDeserializer doesn't read a value without a key, as the `80` of `{{ 80 0=1 }}`, except into the `remainder` field of a struct, which {LOSSLESS} reads"
                ))
                .or_offset(offset))
            }
            Remainder::MapKeyValue(first) => Err(self.map_key_value(first)),
            Remainder::Value { root, first } => {
                self.remainder_seq(root, first);
                Ok(Some(root))
            }
            _ => Ok(None),
        }
    }

    /// Skip the rest of the remainder that a visitor rejected.
    #[cold]
    #[inline(never)]
    fn remainder_rejected(&mut self, err: Error, root: bool) -> Error {
        let err = err.or_offset(self.tok_offset());
        let mut seq = RemainderSeq { de: self, root };
        let rest = loop {
            match de::SeqAccess::next_element::<de::IgnoredAny>(&mut seq) {
                Ok(Some(_)) => {}
                Ok(None) => break Ok(()),
                Err(e) => break Err(e),
            }
        };
        recovered(err, rest)
    }

    /// Visit the token that comes next, which is not a block, as a sequence
    /// of the token and, when it is a header, its block.
    #[inline(never)]
    fn visit_header_seq<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        if self.peeked && self.tok.is_no_value() {
            self.take(self.tok);
            return self.visit_close(visitor);
        }

        visitor.visit_seq(HeaderSeq { de: self, state: 0 })
    }

    /// Visit the block whose open token was read last as a sequence. A
    /// block that the visitor rejects without reading it, as a number, is
    /// skipped, so that a caller that recovers from the error, as with a
    /// default on error, continues after the block. The error is at the open
    /// token.
    #[inline(never)]
    fn visit_block_seq<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let tok = self.tok;
        let result = visitor.visit_seq(Seq::new(self));
        match result {
            Err(err) if !self.peeked && self.pending == tok.next() => Err(self.block_rejected(err)),
            result => result,
        }
    }

    /// Skip the block that a visitor rejected without reading it.
    #[cold]
    #[inline(never)]
    fn block_rejected(&mut self, err: Error) -> Error {
        let err = err.or_offset(self.tok_offset());
        let skip = self.skip_block();
        recovered(err, skip)
    }

    /// Visit the token that was read last, as `visit_token` does. Typed
    /// deserialization falls back to this out of line, so that it does not
    /// keep the token in registers while it converts the scalar.
    #[cold]
    #[inline(never)]
    fn visit_read<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let tok = self.tok;
        let result = match tok.kind {
            Kind::Open => self.visit_block_seq(visitor),
            Kind::Close => self.visit_close(visitor),
            Kind::Operator => Err(self.operator_value()),
            Kind::Unquoted | Kind::Quoted => self.scalar_text(tok).visit(visitor),
        };
        match result {
            Ok(value) => Ok(value),
            Err(err) => Err(self.failed(err)),
        }
    }

    /// The text of a scalar, as `visit_scalar` reads it, out of line for
    /// the paths that are not hot, so that each visitor doesn't get a copy
    /// of the decoding.
    #[cold]
    #[inline(never)]
    fn scalar_text(&self, tok: Tok) -> ScalarText<'de, '_> {
        let quoted = tok.kind == Kind::Quoted;
        match self.source.resolve(tok.start(), tok.end()) {
            Reference::Borrowed(data) => match self.decode(data, quoted) {
                Cow::Borrowed(x) => ScalarText::Borrowed(x),
                Cow::Owned(x) => ScalarText::Owned(x),
            },
            Reference::Copied(data) => match self.decode(data, quoted) {
                Cow::Borrowed(x) => ScalarText::Transient(x),
                Cow::Owned(x) => ScalarText::Owned(x),
            },
        }
    }

    /// Keep where the value whose read failed ends, and where its error is,
    /// or else where it starts.
    #[cold]
    #[inline(never)]
    fn failed(&mut self, err: Error) -> Error {
        if self.peeked {
            return err;
        }

        let at = err.offset().unwrap_or_else(|| self.tok_offset());
        self.failed_at = Some((self.source.offset(self.pending), at));

        // The fast path of `read_operator` reads an `=` right after the
        // value without a look at `failed_at`, so a retry would read the
        // field after the value. The `=` is scanned now, as the next scan
        // would scan it, which sends a retry to the slow path. A reader is
        // filled to the bytes that a slice has, so that the token after the
        // value is the same, and the bytes before the value are dropped to
        // make room for them.
        let mut dropped = false;
        while self.source.window().len() < self.pending + 2 {
            match self.source.refill() {
                Ok(true) => {}
                Err(e) if !dropped && matches!(e.kind(), ErrorKind::BufferTooSmall) => {
                    let start = self.tok.start() - usize::from(self.tok.kind == Kind::Quoted);
                    self.source.consume(start);
                    self.pending -= start;
                    self.tok = self.tok.shifted(start);
                    dropped = true;
                }
                _ => break,
            }
        }
        if let Some([b'=', next]) = self.source.window().get(self.pending..self.pending + 2)
            && *next != b'='
        {
            self.source.consume(self.pending);
            self.pending = 0;
            self.tok = Tok::operator(Operator::Equal, 0, 1);
            self.peeked = true;
        }
        err.or_offset(at)
    }

    /// Visit the token that was read where a map is expected, which is not
    /// an open token. A header before a map, as in `o = rgb { s = 1 }`, is
    /// an error that names the lossless parser, which reads the block as the
    /// map, even for a visitor that would take the scalar.
    #[cold]
    #[inline(never)]
    fn visit_not_map<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.header_before_map()?;

        // As `visit_read`, without its block arm: the caller read a token
        // that is not an open token, and a struct visitor would otherwise
        // get a copy of its sequence code that is never run.
        let tok = self.tok;
        debug_assert_ne!(tok.kind, Kind::Open);
        let result = match tok.kind {
            Kind::Close => self.visit_close(visitor),
            Kind::Unquoted | Kind::Quoted => self.scalar_text(tok).visit(visitor),
            Kind::Open | Kind::Operator => Err(self.operator_value()),
        };
        match result {
            Ok(value) => Ok(value),
            Err(err) => Err(self.failed(err)),
        }
    }

    /// The error for a header before a map, from `visit_not_map`, which
    /// doesn't depend on the visitor.
    #[cold]
    #[inline(never)]
    fn header_before_map(&mut self) -> Result<(), Error> {
        if self.tok.kind == Kind::Unquoted
            && !self.peeked
            && let Some((open, b'{')) = self.peek_byte(self.pending, self.pending)?
            && !matches!(self.peek_byte(open + 1, open + 1)?, Some((_, b'}')) | None)
        {
            return Err(Error::deserialize_msg(format!(
                "TextDeserializer doesn't read {HEADER_MAP}, which {LOSSLESS} reads"
            ))
            .or_offset(self.tok_offset()));
        }
        Ok(())
    }

    /// Visit the scalar of a field value that was read, or the block after
    /// it when the scalar is a header, as in `color = rgb { 1 2 3 }`. An
    /// empty block after the scalar is skipped and the scalar is visited, as
    /// a `TextTape` does, and so is an open token that the end of the data
    /// closes.
    #[inline(never)]
    fn visit_value<V>(&mut self, tok: Tok, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        if let Some((open, b'{')) = self.peek_byte(self.pending, self.pending)? {
            // The open token may be removed from the window to look past
            // it, so the bytes up to the next byte are consumed instead.
            match self.peek_byte(self.pending, open + 1)? {
                Some((close, b'}')) => self.pending = close + 1,
                Some((next, _)) => {
                    self.pending = next;
                    return self.visit_block(visitor);
                }
                None => self.pending = self.source.window().len(),
            }
        }

        self.visit_scalar(tok, visitor)
    }

    /// Visit the block whose open token was read. The first item that is
    /// not an empty block decides its kind like a [`TextTape`](crate::TextTape)
    /// does: a field (a scalar followed by an operator, or an operator with an
    /// empty key) makes it a map, and anything else, including an empty
    /// block, makes it a sequence. The first token stays peeked for the map
    /// or sequence. The empty blocks before the first item are elements of a
    /// sequence, as in a typed sequence, and are not values in a map. The end
    /// of the data closes the block. A block that the visitor rejects before
    /// it reads the first item is skipped.
    #[inline(never)]
    fn visit_block<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let mut empty = 0;
        let first = loop {
            let Some(tok) = self.peek()? else {
                let mut seq = AnySeq::new(self, false);
                seq.done = true;
                seq.empty = empty;
                return visitor.visit_seq(seq);
            };
            if tok.kind == Kind::Open
                && matches!(self.peek_byte(tok.next(), tok.next())?, Some((_, b'}')))
            {
                self.take(tok);
                self.read()?;
                empty += 1;
                continue;
            }
            break tok;
        };

        let is_map = match first.kind {
            Kind::Close => {
                self.take(first);
                let mut seq = AnySeq::new(self, false);
                seq.done = true;
                seq.empty = empty;
                return visitor.visit_seq(seq);
            }
            Kind::Operator => true,
            Kind::Open => false,
            Kind::Unquoted | Kind::Quoted => {
                matches!(self.peek_byte(first.next(), first.next())?, Some((_, x)) if is_operator_start(x))
            }
        };

        let result = if is_map {
            visitor.visit_map(AnyMap::new(self, false, true))
        } else {
            let mut seq = AnySeq::new(self, false);
            seq.empty = empty;
            visitor.visit_seq(seq)
        };

        match result {
            Err(err) if self.peeked && self.tok == first => Err(self.skip_rejected(err, first)),
            result => result,
        }
    }

    /// Skip the rest of the block that a visitor rejected before it read
    /// `first`, its first item, which is peeked, as `visit_block_seq` does.
    /// The error is at that item, and stays the error when the skip fails,
    /// as when a token doesn't fit in a reader's buffer, so a reader gives
    /// the error that a slice gives.
    #[cold]
    #[inline(never)]
    fn skip_rejected(&mut self, err: Error, first: Tok) -> Error {
        let err = err.or_offset(self.tok_offset());
        // The skip starts after `first`, where a token can start, since the
        // look past it may have removed the whitespace after it from the
        // window, which would join a scalar and a quote after it.
        self.pending = first.next();
        self.peeked = false;
        let depth = if first.kind == Kind::Open { 2 } else { 1 };
        let skip = self.skip_blocks(Skip::nested(depth));
        recovered(err, skip)
    }
}

/// The decoded text of a scalar: borrowed from the input, borrowed from the
/// source's buffer, or decoded into a new string.
enum ScalarText<'de, 'a> {
    Borrowed(&'de str),
    Transient(&'a str),
    Owned(String),
}

impl<'de> ScalarText<'de, '_> {
    #[inline]
    fn visit<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match self {
            ScalarText::Borrowed(x) => visitor.visit_borrowed_str(x),
            ScalarText::Transient(x) => visitor.visit_str(x),
            ScalarText::Owned(x) => visitor.visit_string(x),
        }
    }
}

impl<'de, S: Source<'de>, E: Encoding> de::Deserializer<'de> for &'_ mut TextDeserializer<S, E> {
    type Error = Error;

    /// The root is an object, so it is visited as a map.
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.skip_bom()?;
        visitor
            .visit_map(AnyMap::new(&mut *self, true, false))
            .map_err(|e| self.locate(e))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.skip_bom()?;
        self.block_at_root()?;
        let map = Map::<_, _, true, false> { de: &mut *self };
        visitor.visit_map(map).map_err(|e| self.locate(e))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.skip_bom()?;
        self.block_at_root()?;
        let map = Map::<_, _, true, true> { de: &mut *self };
        visitor.visit_map(map).map_err(|e| self.locate(e))
    }

    /// An owned value of the root is the block of the whole data. Any other
    /// newtype struct reads the root as `deserialize_any` does.
    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        #[cfg(feature = "syntax")]
        if name == crate::text::syntax::de::VALUE {
            return self.visit_root_owned(visitor);
        }
        let _ = name;
        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

/// A map over the fields of an object, which is the root when `ROOT` is
/// true.
///
/// An operator where a key is expected starts a field whose key is the
/// empty string. An operator after a block in key position makes the block
/// the key, so the map skips the field. A close at the root, which has no
/// block to close, is skipped, as a `TextTape` does.
struct Map<'a, S, E, const ROOT: bool, const STRUCT: bool> {
    de: &'a mut TextDeserializer<S, E>,
}

impl<'de, S: Source<'de>, E: Encoding, const ROOT: bool, const STRUCT: bool> de::MapAccess<'de>
    for Map<'_, S, E, ROOT, STRUCT>
{
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        let de = &mut *self.de;
        loop {
            // The end of the data closes the object.
            if !de.fill()? {
                return Ok(None);
            }

            match de.tok.kind {
                Kind::Unquoted | Kind::Quoted => break,
                Kind::Close if ROOT => de.skip_root_close(),
                Kind::Close => {
                    de.take(de.tok);
                    return Ok(None);
                }
                Kind::Open => {
                    de.take(de.tok);
                    match de.key_after_open(ROOT)? {
                        AfterOpen::Key => break,
                        AfterOpen::End => return Ok(None),
                        AfterOpen::Remainder => {
                            return seed.deserialize(StaticDeserializer("remainder")).map(Some);
                        }
                    }
                }
                Kind::Operator => {
                    de.take(de.tok);
                    de.empty_key(de.tok.op)?;
                    break;
                }
            }
        }

        seed.deserialize(ValueDeserializer::key(de)).map(Some)
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let op = match self.de.read_operator()? {
            Some(op) => op,
            None => self.de.missing_operator(STRUCT),
        };

        seed.deserialize(ValueDeserializer {
            de: &mut *self.de,
            op,
            header: true,
        })
    }
}

/// Deserializes the next token.
struct ValueDeserializer<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,

    /// The operator before a field value, for a [`Property`](crate::text::Property).
    op: Operator,

    /// Whether `deserialize_any` visits the block after a scalar, which is
    /// a header only in a field value.
    header: bool,
}

impl<'a, S, E> ValueDeserializer<'a, S, E> {
    #[inline]
    fn key(de: &'a mut TextDeserializer<S, E>) -> Self {
        ValueDeserializer {
            de,
            op: Operator::Equal,
            header: false,
        }
    }
}

impl<'de, S: Source<'de>, E: Encoding> de::Deserializer<'de> for ValueDeserializer<'_, S, E> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        match tok.kind {
            Kind::Open => self.de.visit_block(visitor),
            Kind::Unquoted if self.header => self.de.visit_value(tok, visitor),
            _ => self.de.visit_token(tok, visitor),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        match self.de.scalar(tok).and_then(|x| x.to_bool().ok()) {
            Some(x) => visitor.visit_bool(x),
            None => self.de.visit_read(visitor),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        match self.de.scalar(tok).and_then(|x| x.to_i64().ok()) {
            Some(x) => visitor.visit_i64(x),
            None => self.de.visit_read(visitor),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        match self.de.scalar(tok).and_then(|x| x.to_u64().ok()) {
            Some(x) => visitor.visit_u64(x),
            None => self.de.visit_read(visitor),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        match self.de.scalar(tok).and_then(|x| x.to_f64().ok()) {
            Some(x) => visitor.visit_f64(x),
            None => self.de.visit_read(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        self.de.visit_read(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        if tok.is_scalar() {
            self.de.visit_scalar(tok, visitor)
        } else {
            self.de.visit_read(visitor)
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        let tok = self.de.tok;
        if !tok.is_scalar() {
            return self.de.visit_read(visitor);
        }

        match self.de.source.resolve(tok.start(), tok.end()) {
            Reference::Borrowed(x) => visitor.visit_borrowed_bytes(x),
            Reference::Copied(x) => visitor.visit_bytes(x),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_ignored_any(visitor)
    }

    /// A newtype struct whose name starts with `$` may be a wrapper type of
    /// the crate, as [`MaybeQuoted`](crate::text::MaybeQuoted), which no
    /// save model has, so the names are compared out of line.
    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name.starts_with('$') {
            return self.visit_wrapper(name, visitor);
        }
        visitor.visit_newtype_struct(self)
    }

    /// Visit the block that comes next, or a scalar as a sequence of the
    /// scalar and, when it is a header, as in `color = rgb { 1 2 3 }`, its
    /// block.
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let tok = self.de.peek()?.ok_or_else(|| self.de.eof())?;
        if tok.kind != Kind::Open {
            return self.de.visit_header_seq(visitor);
        }

        self.de.take(tok);
        let mut seq = Seq::new(self.de);
        let result = match visitor.visit_seq(&mut seq) {
            Ok(result) => result,
            Err(err) => return Err(seq.de.seq_error(err)),
        };

        // Close the sequence for a visitor that stopped before its end,
        // unless the end of the data closes it.
        if !seq.hit_end && seq.de.advance()? && seq.de.tok.kind != Kind::Close {
            return Err(seq.de.long_sequence());
        }

        Ok(result)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        if self.de.tok.kind == Kind::Open {
            visitor.visit_map(Map::<_, _, false, false> { de: self.de })
        } else {
            self.de.visit_not_map(visitor)
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if name == "_internal_jomini_property" {
            visitor.visit_map(PropertyMap {
                de: self.de,
                op: self.op,
                header: self.header,
                state: 0,
            })
        } else {
            self.de.read()?;
            if self.de.tok.kind == Kind::Open {
                visitor.visit_map(Map::<_, _, false, true> { de: self.de })
            } else {
                self.de.visit_not_map(visitor)
            }
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(Enum {
            de: self.de,
            header: self.header,
        })
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.read()?;
        if !self.de.tok.is_scalar() {
            if self.de.tok.kind == Kind::Open {
                self.de.skip_block()?;
            } else {
                // A close or an operator is not a value, so it is an error,
                // as for other types.
                self.de.close_as_value()?;
            }
        }
        visitor.visit_unit()
    }
}

/// A sequence over the elements of a block whose open token was read.
struct Seq<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    hit_end: bool,
}

impl<'a, S, E> Seq<'a, S, E> {
    fn new(de: &'a mut TextDeserializer<S, E>) -> Self {
        Seq { de, hit_end: false }
    }
}

impl<'de, S: Source<'de>, E: Encoding> de::SeqAccess<'de> for Seq<'_, S, E> {
    type Error = Error;

    #[inline]
    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.hit_end {
            return Ok(None);
        }

        // The end of the data closes the block.
        if !self.de.fill()? {
            self.hit_end = true;
            return Ok(None);
        }

        if self.de.tok.kind == Kind::Close {
            self.de.take(self.de.tok);
            self.hit_end = true;
            return Ok(None);
        }

        seed.deserialize(ValueDeserializer::key(&mut *self.de))
            .map(Some)
    }
}

/// The remainder of an object, from the value without a key that started
/// it: each part of each entry, keys, the symbols of operators, and values,
/// to the end of the object, whose close is left for the map. A close at the
/// root, which closes no block, is skipped.
struct RemainderSeq<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    root: bool,
}

impl<'de, S: Source<'de>, E: Encoding> de::SeqAccess<'de> for RemainderSeq<'_, S, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        loop {
            match self.de.peek()? {
                None => return Ok(None),
                Some(tok) if tok.kind == Kind::Close => {
                    if !self.root {
                        return Ok(None);
                    }
                    self.de.skip_root_close();
                }
                Some(tok) if tok.kind == Kind::Operator => {
                    self.de.operator_element(tok)?;
                    return seed
                        .deserialize(OperatorDeserializer(tok.op.symbol()))
                        .map(Some);
                }
                Some(_) => break,
            }
        }

        seed.deserialize(ValueDeserializer::key(&mut *self.de))
            .map(Some)
    }
}

/// A token that is not a block read as a sequence: the token, and the block
/// after it when the token is a header, as in `color = rgb { 1 2 3 }`. An
/// empty block after it is dropped, as a `TextTape` does.
struct HeaderSeq<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    state: u8,
}

impl<'de, S: Source<'de>, E: Encoding> de::SeqAccess<'de> for HeaderSeq<'_, S, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        self.state += 1;
        match self.state {
            1 => {}
            2 => {
                let de = &mut *self.de;
                let Some(tok) = de.peek()? else {
                    return Ok(None);
                };
                if tok.kind != Kind::Open {
                    return Ok(None);
                }

                match de.peek_byte(tok.next(), tok.next())? {
                    Some((_, b'}')) => {
                        de.take(tok);
                        de.read()?;
                        return Ok(None);
                    }
                    None => {
                        de.take(tok);
                        return Ok(None);
                    }
                    Some(_) => {}
                }
            }
            _ => return Ok(None),
        }

        let value = ValueDeserializer::key(&mut *self.de);
        if self.state == 1 {
            seed.deserialize(HeaderToken(value)).map(Some)
        } else {
            seed.deserialize(value).map(Some)
        }
    }
}

/// The token of a [`HeaderSeq`], which is read as any value is, except that
/// as a sequence it is the token alone, as the syntax parser reads it: the
/// block after it is the next element of the [`HeaderSeq`].
struct HeaderToken<'a, S, E>(ValueDeserializer<'a, S, E>);

macro_rules! forward_to_value {
    ($($method:ident)*) => {
        $(
            fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: Visitor<'de>,
            {
                self.0.$method(visitor)
            }
        )*
    };
}

impl<'de, S: Source<'de>, E: Encoding> de::Deserializer<'de> for HeaderToken<'_, S, E> {
    type Error = Error;

    forward_to_value! {
        deserialize_any deserialize_bool deserialize_i8 deserialize_i16 deserialize_i32
        deserialize_i64 deserialize_i128 deserialize_u8 deserialize_u16 deserialize_u32
        deserialize_u64 deserialize_u128 deserialize_f32 deserialize_f64 deserialize_char
        deserialize_str deserialize_string deserialize_bytes deserialize_byte_buf
        deserialize_option deserialize_unit deserialize_map deserialize_identifier
        deserialize_ignored_any
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_unit_struct(name, visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_newtype_struct(name, visitor)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(TokenSeq(Some(self.0)))
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_struct(name, fields, visitor)
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.0.deserialize_enum(name, variants, visitor)
    }
}

/// A sequence of one token.
struct TokenSeq<'a, S, E>(Option<ValueDeserializer<'a, S, E>>);

impl<'de, S: Source<'de>, E: Encoding> de::SeqAccess<'de> for TokenSeq<'_, S, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.0.take() {
            Some(value) => seed.deserialize(value).map(Some),
            None => Ok(None),
        }
    }
}

/// An empty block that was read, `{}`: an empty sequence, or an empty map
/// for a map or a struct.
struct EmptyBlock;

impl<'de> de::Deserializer<'de> for EmptyBlock {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(self)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(self)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    /// An empty block is unquoted, has no header, and is an empty owned
    /// block.
    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.visit_wrapper(name, visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf unit unit_struct seq tuple
        tuple_struct enum identifier
    }
}

impl<'de> de::SeqAccess<'de> for EmptyBlock {
    type Error = Error;

    fn next_element_seed<T>(&mut self, _seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        Ok(None)
    }
}

impl<'de> de::MapAccess<'de> for EmptyBlock {
    type Error = Error;

    fn next_key_seed<K>(&mut self, _seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        Ok(None)
    }

    fn next_value_seed<V>(&mut self, _seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        Err(Error::deserialize_msg("an empty block has no values"))
    }
}

/// A map over a block that `deserialize_any` found to be an object, or over
/// the root.
///
/// Like a [`TextTape`](crate::TextTape) object, scalars that trail the
/// fields, as in `{ a = b c d }`, are yielded as a sequence under the key
/// `remainder`. The sequence runs to the end of the block.
struct AnyMap<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    root: bool,

    /// The first key is peeked and known to be followed by an operator.
    first: bool,

    /// The peeked scalar trails the fields, and the `remainder` key was
    /// yielded.
    remainder: bool,
    done: bool,
}

impl<'a, S, E> AnyMap<'a, S, E> {
    fn new(de: &'a mut TextDeserializer<S, E>, root: bool, first: bool) -> Self {
        AnyMap {
            de,
            root,
            first,
            remainder: false,
            done: false,
        }
    }
}

impl<'de, S: Source<'de>, E: Encoding> de::MapAccess<'de> for AnyMap<'_, S, E> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if self.done {
            return Ok(None);
        }

        let first = std::mem::take(&mut self.first);
        let de = &mut *self.de;
        loop {
            // The end of the data closes the object.
            let Some(tok) = de.peek()? else {
                return Ok(None);
            };

            match tok.kind {
                Kind::Close if self.root => de.skip_root_close(),
                Kind::Close => {
                    de.take(tok);
                    return Ok(None);
                }
                // A block where a key is expected, which a header's block
                // never is here, is a value without a key that starts the
                // remainder, and an empty block is skipped.
                Kind::Open => {
                    if !matches!(de.peek_byte(tok.next(), tok.next())?, Some((_, b'}'))) {
                        self.remainder = true;
                        return seed.deserialize(StaticDeserializer("remainder")).map(Some);
                    }
                    de.take(tok);
                    if !de.key_after_block(self.root)? {
                        return Ok(None);
                    }
                    // An empty key is read as the key, and the scalar that is
                    // peeked may be a value without a key.
                    if de.empty_key_op.is_some() {
                        break;
                    }
                    continue;
                }
                // A field with an empty key, whose operator stays peeked.
                Kind::Operator => return seed.deserialize(StaticDeserializer("")).map(Some),
                Kind::Unquoted | Kind::Quoted if first => break,
                Kind::Unquoted | Kind::Quoted => match de.peek_byte(tok.next(), tok.next())? {
                    Some((_, x)) if is_operator_start(x) || x == b'{' => break,
                    _ => {
                        self.remainder = true;
                        return seed.deserialize(StaticDeserializer("remainder")).map(Some);
                    }
                },
            }
        }

        seed.deserialize(ValueDeserializer::key(de)).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let de = &mut *self.de;
        if self.remainder {
            self.done = true;
            let root = self.root;
            return seed.deserialize(AnyRemainder { de, root });
        }

        // Unlike the typed map, this reads the operator as a token, as the
        // lookahead may have skipped the whitespace before an `==`.
        let tok = de.peek()?.ok_or_else(|| de.eof())?;
        match tok.kind {
            Kind::Operator => {
                de.take(tok);
                seed.deserialize(ValueDeserializer {
                    de,
                    op: tok.op,
                    header: true,
                })
            }
            _ => {
                let op = de.missing_operator(true);
                seed.deserialize(ValueDeserializer {
                    de,
                    op,
                    header: false,
                })
            }
        }
    }
}

/// A sequence over a block that `deserialize_any` found to be an array, or
/// over the scalars that trail the fields of an object.
struct AnySeq<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    root: bool,
    done: bool,

    /// The number of empty blocks that were read before the first element,
    /// which are the first elements.
    empty: u32,
}

impl<'a, S, E> AnySeq<'a, S, E> {
    fn new(de: &'a mut TextDeserializer<S, E>, root: bool) -> Self {
        AnySeq {
            de,
            root,
            done: false,
            empty: 0,
        }
    }
}

impl<'de, S: Source<'de>, E: Encoding> de::SeqAccess<'de> for AnySeq<'_, S, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.empty > 0 {
            self.empty -= 1;
            return seed.deserialize(EmptyBlock).map(Some);
        }

        if self.done {
            return Ok(None);
        }

        loop {
            match self.de.peek()? {
                Some(tok) => match tok.kind {
                    Kind::Close if self.root => self.de.skip_root_close(),
                    Kind::Close => {
                        self.de.take(tok);
                        self.done = true;
                        return Ok(None);
                    }
                    Kind::Operator => {
                        self.de.operator_element(tok)?;
                        return seed
                            .deserialize(OperatorDeserializer(tok.op.symbol()))
                            .map(Some);
                    }
                    _ => break,
                },
                // The end of the data closes the block.
                None => {
                    self.done = true;
                    return Ok(None);
                }
            }
        }

        seed.deserialize(ValueDeserializer::key(&mut *self.de))
            .map(Some)
    }
}

/// An enum whose variant is the next scalar. A unit variant is the scalar
/// alone. A variant with data is a field value that is a header, as in
/// `color = rgb { 1 2 3 }`, whose block is the data: a sequence for a
/// newtype or tuple variant, and a map for a struct variant. The block is
/// read as any sequence or map is, so where a `TextTape` drops the elements
/// after those of a tuple variant and empty blocks before them, they are an
/// error.
struct Enum<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,

    /// Whether the variant is a field value, which can be a header.
    header: bool,
}

impl<'de, S: Source<'de>, E: Encoding> de::EnumAccess<'de> for Enum<'_, S, E> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match seed.deserialize(ValueDeserializer::key(&mut *self.de)) {
            Ok(variant) => Ok((variant, self)),
            Err(err) => Err(self.de.variant_error(err)),
        }
    }
}

impl<'a, S, E> Enum<'a, S, E> {
    /// The deserializer of the block after a variant with data, whose open
    /// token comes next.
    #[cold]
    #[inline(never)]
    fn data<'de>(self) -> Result<ValueDeserializer<'a, S, E>, Error>
    where
        S: Source<'de>,
    {
        if !self.header {
            return Err(no_variant_block());
        }

        self.de.header_block()?;
        Ok(ValueDeserializer::key(self.de))
    }
}

#[cold]
fn no_variant_block() -> Error {
    Error::deserialize_msg(format!(
        "TextDeserializer reads an enum variant with data only from a field value followed by a block that is not empty, as in `color = rgb {{ 1 2 3 }}`; {LOSSLESS} reads the others"
    ))
}

impl<'de, S: Source<'de>, E: Encoding> de::VariantAccess<'de> for Enum<'_, S, E> {
    type Error = Error;

    /// A block after a unit variant isn't read, so the map that holds the
    /// field skips it, as a `TextTape` does.
    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self.data()?)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.data()?, visitor)
    }

    fn struct_variant<V>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.data()?, "", fields, visitor)
    }
}

/// The fields of a [`Property`](crate::text::Property): the operator before
/// the value, and the value, which is read as the field value would be.
struct PropertyMap<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    op: Operator,
    header: bool,
    state: u8,
}

impl<'de, S: Source<'de>, E: Encoding> de::MapAccess<'de> for PropertyMap<'_, S, E> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        match self.state {
            0 => seed.deserialize(StaticDeserializer("operator")).map(Some),
            1 => seed.deserialize(StaticDeserializer("value")).map(Some),
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.state += 1;
        if self.state == 1 {
            seed.deserialize(StaticDeserializer(self.op.symbol()))
        } else {
            seed.deserialize(ValueDeserializer {
                de: &mut *self.de,
                op: Operator::Equal,
                header: self.header,
            })
        }
    }
}

/// Deserializes a static string.
struct StaticDeserializer(&'static str);

impl<'de> de::Deserializer<'de> for StaticDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// The parser that reads what [`TextDeserializer`] does not.
const LOSSLESS: &str = "jomini::text::syntax::parse";

const TRAILING_SCALARS: &str =
    "the scalars that trail the fields of an object, as in `{ a = b 1 2 }`";
const SEQUENCE_FIELDS: &str =
    "an object as a sequence, as the scalars that trail its fields or an object used as a key";
const BLOCK_ENUM: &str = "an enum written as a block, as in `a = { rgb = { 1 2 3 } }`";
const HEADER_MAP: &str = "a header before a map, as in `o = rgb { s = 1 }`";

/// The error of a value whose rest was skipped to recover from it, as a
/// visitor that rejected a block, or the error of the skip when it couldn't
/// read the data, an I/O error or a token that doesn't fit in the buffer,
/// since a caller that recovers from the value's error would read on from
/// where the skip stopped. A skip that runs into the end of the data keeps
/// the value's error, which a slice gives too.
#[cold]
#[inline(never)]
fn recovered(err: Error, skip: Result<(), Error>) -> Error {
    match skip {
        Err(skip) if matches!(skip.kind(), ErrorKind::Io(_) | ErrorKind::BufferTooSmall) => skip,
        _ => err,
    }
}

/// Add to a deserialization error that TextDeserializer doesn't read
/// `what`, which the lossless parser reads.
#[cold]
#[inline(never)]
fn lossless_hint(err: Error, what: &str) -> Error {
    let ErrorKind::Deserialize(inner) = err.kind() else {
        return err;
    };

    // An error from a nested value may have the hint already.
    if inner.to_string().contains(LOSSLESS) {
        return err;
    }

    let hinted = Error::deserialize_msg(format!(
        "{inner}; TextDeserializer doesn't read {what}, which {LOSSLESS} reads"
    ));
    match err.offset() {
        Some(offset) => hinted.or_offset(offset),
        None => hinted,
    }
}

#[cfg(all(test, feature = "derive"))]
mod tests;
