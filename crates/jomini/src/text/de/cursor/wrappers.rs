//! The wrapper types of the crate through [`TextDeserializer`]:
//! [`MaybeQuoted`](crate::text::MaybeQuoted),
//! [`Headered`](crate::text::Headered), and, with the `syntax` feature, an
//! owned value.
//!
//! Each wrapper asks for a newtype struct with a private name, so these
//! paths run only for a newtype struct whose name starts with `$`, which no
//! save model has.

#[cfg(feature = "syntax")]
use super::Remainder;
use super::scan::{Kind, Tok};
#[cfg(feature = "syntax")]
use super::scan::{Skip, skip_block};
#[cfg(feature = "syntax")]
use super::source::Reference;
use super::source::Source;
use super::{AnySeq, EmptyBlock, TextDeserializer, ValueDeserializer};
use crate::text::Operator;
use crate::text::wrappers::{
    HEADERED, KEY_HEADER, KEY_QUOTED, KEY_VALUE, OptionDe, QUOTED, WrapperMap,
};
use crate::{Encoding, Error};
use serde::de::{self, DeserializeSeed, IntoDeserializer, Visitor, value::BorrowedStrDeserializer};

impl<'de, S: Source<'de>, E: Encoding> ValueDeserializer<'_, S, E> {
    /// Visit the newtype struct of a wrapper type, or any other newtype
    /// struct whose name starts with `$`.
    #[cold]
    #[inline(never)]
    pub(super) fn visit_wrapper<V>(self, name: &'static str, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => self.visit_quoted(visitor),
            HEADERED => self.visit_headered(visitor),
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => self.visit_owned(visitor),
            _ => visitor.visit_newtype_struct(self),
        }
    }

    /// Visit a [`MaybeQuoted`](crate::text::MaybeQuoted): whether the value
    /// is a quoted scalar, and the value, which is read as it would be
    /// without the wrapper, so a scalar of a field value is still a header
    /// when a block follows it. A headered block is not a scalar, so it is
    /// unquoted, even with a quoted header.
    fn visit_quoted<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let quoted = match self.de.peek()? {
            Some(tok) if tok.kind == Kind::Quoted => {
                !self.header || self.de.header_open(tok)?.is_none()
            }
            _ => false,
        };
        visitor.visit_map(WrapperMap::new(
            KEY_QUOTED,
            quoted.into_deserializer(),
            self,
        ))
    }

    /// Visit a [`Headered`](crate::text::Headered): the scalar of a field
    /// value that a block that is not empty follows is the header of
    /// the block, which keeps the operator of the field. Anything else is
    /// the value without a header, as a scalar that an empty block follows,
    /// and so is any value that is not the value of a field.
    fn visit_headered<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let header = match self.de.peek()? {
            Some(tok) if self.header && tok.is_scalar() => {
                self.de.header_open(tok)?.map(|open| (tok, open))
            }
            _ => None,
        };

        let Some((tok, open)) = header else {
            let header = OptionDe::<HeaderScalar<'_, S, E>>(None);
            return visitor.visit_map(WrapperMap::new(KEY_HEADER, header, self));
        };

        // The header is visited before the block is scanned, which consumes
        // the bytes before the open token.
        self.de.take(tok);
        self.de.pending = open;
        visitor.visit_map(HeaderedMap {
            de: self.de,
            tok,
            op: self.op,
            state: 0,
        })
    }

    /// Visit an owned value: the bytes of the value, which the syntax parser
    /// reads with its default flavor, so the value is the one that `parse`
    /// gives, whatever the encoding `E` is. The remainder of an
    /// object is the block of its bytes. Any other value, as an operator, is
    /// visited as with `deserialize_any`.
    #[cfg(feature = "syntax")]
    fn visit_owned<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let Some(tok) = self.de.peek()? else {
            return Err(self.de.eof());
        };

        let mut source = b"x=".to_vec();
        match (tok.kind, self.de.remainder) {
            (Kind::Open | Kind::Unquoted | Kind::Quoted, _) => {
                self.de.capture_value(tok, self.header, &mut source)?;
            }
            (Kind::Close, Remainder::Value { root, first }) if tok.is_no_value() => {
                source.push(b'{');
                self.de.capture_remainder(root, first, true, &mut source)?;
                source.push(b'}');
            }
            _ => return de::Deserializer::deserialize_any(self, visitor),
        }
        let encoding = &self.de.encoding;
        crate::text::syntax::value::visit_owned(
            || crate::text::syntax::de::field_value(&source, encoding),
            visitor,
        )
    }
}

/// The private map of a [`Headered`](crate::text::Headered) with a header:
/// the scalar `tok`, and then the block, whose open token is scanned next.
struct HeaderedMap<'a, S, E> {
    de: &'a mut TextDeserializer<S, E>,
    tok: Tok,
    op: Operator,
    state: u8,
}

impl<'de, S: Source<'de>, E: Encoding> de::MapAccess<'de> for HeaderedMap<'_, S, E> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        let key = match self.state {
            0 => KEY_HEADER,
            1 => KEY_VALUE,
            _ => return Ok(None),
        };
        seed.deserialize(BorrowedStrDeserializer::new(key))
            .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        self.state += 1;
        match self.state {
            1 => seed.deserialize(OptionDe(Some(HeaderScalar {
                de: &*self.de,
                tok: self.tok,
            }))),
            2 => seed.deserialize(ValueDeserializer {
                de: &mut *self.de,
                op: self.op,
                header: true,
            }),
            _ => Err(de::Error::custom("a jomini wrapper type has two parts")),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(2usize.saturating_sub(usize::from(self.state)))
    }
}

/// The scalar of a header, visited as text, as a
/// [`MaybeQuoted`](crate::text::MaybeQuoted) that tells if it is quoted, or
/// as the owned value that the syntax parser reads from its bytes.
struct HeaderScalar<'a, S, E> {
    de: &'a TextDeserializer<S, E>,
    tok: Tok,
}

impl<'de, S: Source<'de>, E: Encoding> de::Deserializer<'de> for HeaderScalar<'_, S, E> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.visit_scalar(self.tok, visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => {
                let quoted = (self.tok.kind == Kind::Quoted).into_deserializer();
                visitor.visit_map(WrapperMap::new(KEY_QUOTED, quoted, self))
            }
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => {
                use crate::text::syntax::{OwnedValue, value};
                // `header_open` looked past the header to its `{` with the
                // byte after the header, after the closing quote of a quoted
                // one, as the `keep` of `peek_byte`, which removes
                // from the window only bytes after `keep`, and nothing
                // consumes the window until the block is scanned after this
                // part. So the bytes of the header are still in the window,
                // as for `deserialize_any`.
                let quoted = self.tok.kind == Kind::Quoted;
                let bytes = match self.de.source.resolve(self.tok.start(), self.tok.end()) {
                    Reference::Borrowed(x) => x,
                    Reference::Copied(x) => x,
                };
                let scalar = value::owned_scalar(bytes, quoted, &self.de.encoding);
                value::visit_owned(|| OwnedValue::Scalar(scalar), visitor)
            }
            _ => visitor.visit_newtype_struct(self),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

impl<'de, S: Source<'de>, E> TextDeserializer<S, E> {
    /// Return the offset of the open token after the scalar `tok`, which is
    /// peeked, when a block that is not empty follows it, as in
    /// `rgb { 1 2 3 }`. The scalar and the open token stay in the window, so
    /// a buffer must hold both and the byte after each, as for a header
    /// that `deserialize_any` reads.
    fn header_open(&mut self, tok: Tok) -> Result<Option<usize>, Error> {
        let Some((open, b'{')) = self.peek_byte(tok.next(), tok.next())? else {
            return Ok(None);
        };

        match self.peek_byte(open + 1, open + 1)? {
            Some((_, b'}')) | None => Ok(None),
            Some(_) => Ok(Some(open)),
        }
    }
}

#[cfg(feature = "syntax")]
impl<'de, S: Source<'de>, E> TextDeserializer<S, E> {
    /// Read the value whose first token, `tok`, is peeked, and append its
    /// bytes to `out`: a scalar, with its quotes, a block, or, in a field
    /// value, a scalar and the block after it, which the syntax parser reads
    /// as a header and its block.
    fn capture_value(&mut self, tok: Tok, header: bool, out: &mut Vec<u8>) -> Result<(), Error> {
        let quote = usize::from(tok.kind == Kind::Quoted);
        if tok.kind == Kind::Open {
            self.take(tok);
            out.push(b'{');
            return self.capture_block(out);
        }

        out.extend_from_slice(&self.source.window()[tok.start() - quote..tok.next()]);
        let open = match tok.kind {
            Kind::Unquoted | Kind::Quoted if header => self.peek_byte(tok.next(), tok.next())?,
            _ => None,
        };
        self.take(tok);
        if let Some((open, b'{')) = open {
            out.push(b' ');
            self.pending = open + 1;
            out.push(b'{');
            self.capture_block(out)?;
        }
        Ok(())
    }

    /// Append the remainder of an object, from the value without a key
    /// `first` to the end of the object, to `out`. The close of the object
    /// is read, or stays to be read with `keep_close`, as after the sequence
    /// of the remainder.
    fn capture_remainder(
        &mut self,
        root: bool,
        first: Tok,
        keep_close: bool,
        out: &mut Vec<u8>,
    ) -> Result<(), Error> {
        self.remainder = Remainder::None;
        let quote = usize::from(first.kind == Kind::Quoted);
        out.extend_from_slice(&self.source.window()[first.start() - quote..first.next()]);

        // The look past `first` may have removed the whitespace after it
        // from the window, which would join it to what follows.
        out.push(b' ');
        self.pending = first.next();
        self.peeked = false;
        if root {
            return self.capture_rest(out);
        }

        let depth = if first.kind == Kind::Open { 2 } else { 1 };
        self.capture_blocks(Skip::nested(depth), out)?;
        if self.tok.kind == Kind::Close && out.last() == Some(&b'}') {
            out.pop();
            if keep_close {
                self.pending -= 1;
            }
        }
        Ok(())
    }

    /// Skip the block whose open token was read, as `skip_block` does, and
    /// append its bytes to `out`, up to and with its close. The end of the
    /// data closes the block.
    fn capture_block(&mut self, out: &mut Vec<u8>) -> Result<(), Error> {
        self.capture_blocks(Skip::new(), out)
    }

    /// Skip to the end of the blocks that `state` is in, as `capture_block`
    /// does for one.
    fn capture_blocks(&mut self, mut state: Skip, out: &mut Vec<u8>) -> Result<(), Error> {
        self.source.consume(self.pending);
        self.pending = 0;
        loop {
            match skip_block(self.source.window(), &mut state) {
                Ok(end) => {
                    out.extend_from_slice(&self.source.window()[..end]);
                    self.skipped(end);
                    return Ok(());
                }
                Err(processed) => {
                    out.extend_from_slice(&self.source.window()[..processed]);
                    self.source.consume(processed);
                    if !self.source.refill()? {
                        if state.in_quote() {
                            return Err(self.eof());
                        }
                        out.extend_from_slice(self.source.window());
                        self.pending = self.source.window().len();
                        self.end_tok();
                        return Ok(());
                    }
                }
            }
        }
    }

    /// Read the rest of the data and append it to `out`.
    fn capture_rest(&mut self, out: &mut Vec<u8>) -> Result<(), Error> {
        self.source.consume(self.pending);
        self.pending = 0;
        self.peeked = false;
        loop {
            let window = self.source.window();
            out.extend_from_slice(window);
            let len = window.len();
            self.source.consume(len);
            if !self.source.refill()? {
                self.end_tok();
                return Ok(());
            }
        }
    }
}

#[cfg(feature = "syntax")]
impl<'de, S: Source<'de>, E: Encoding> TextDeserializer<S, E> {
    /// Visit the owned value of the root: the block of the rest of the
    /// data, which the syntax parser reads.
    pub(super) fn visit_root_owned<V>(&mut self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        self.skip_bom()?;
        let mut source = Vec::new();
        self.capture_rest(&mut source)?;
        let encoding = &self.encoding;
        crate::text::syntax::value::visit_owned(
            || {
                let parse = crate::text::syntax::parse(&source);
                let block = crate::text::syntax::de::tree_block(parse.tree(), encoding);
                crate::text::syntax::OwnedValue::Block(block)
            },
            visitor,
        )
    }
}

impl EmptyBlock {
    /// Visit the newtype struct of a wrapper type for an empty block, which
    /// is unquoted, has no header, and is an empty owned block.
    #[cold]
    #[inline(never)]
    pub(super) fn visit_wrapper<'de, V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => {
                visitor.visit_map(WrapperMap::new(KEY_QUOTED, false.into_deserializer(), self))
            }
            HEADERED => {
                let header = OptionDe::<EmptyBlock>(None);
                visitor.visit_map(WrapperMap::new(KEY_HEADER, header, self))
            }
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => crate::text::syntax::value::visit_owned(
                || crate::text::syntax::OwnedValue::Block(crate::text::syntax::OwnedBlock::new()),
                visitor,
            ),
            _ => visitor.visit_newtype_struct(self),
        }
    }
}

/// An operator that is a part of the remainder of an object, or of an
/// object read as a sequence, as the `=` of `{ a = 1 c d = e }`: its symbol,
/// an unquoted scalar without a header, as the syntax parser reads it.
pub(super) struct OperatorDeserializer(pub(super) &'static str);

impl<'de> de::Deserializer<'de> for OperatorDeserializer {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0)
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => {
                visitor.visit_map(WrapperMap::new(KEY_QUOTED, false.into_deserializer(), self))
            }
            HEADERED => visitor.visit_map(WrapperMap::new(
                KEY_HEADER,
                OptionDe::<OperatorDeserializer>(None),
                self,
            )),
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => {
                let symbol = self.0;
                crate::text::syntax::value::visit_owned(
                    || crate::text::syntax::OwnedValue::unquoted(symbol),
                    visitor,
                )
            }
            _ => visitor.visit_newtype_struct(self),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

/// The remainder of an object that `deserialize_any` reads under the key
/// `remainder`, whose first part is peeked: a sequence of its parts, an
/// unquoted value without a header, and, as an owned value, the block of
/// its bytes, as the syntax parser reads it.
pub(super) struct AnyRemainder<'a, S, E> {
    pub(super) de: &'a mut TextDeserializer<S, E>,
    pub(super) root: bool,
}

impl<'de, S: Source<'de>, E: Encoding> de::Deserializer<'de> for AnyRemainder<'_, S, E> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(AnySeq::new(self.de, self.root))
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match name {
            QUOTED => {
                visitor.visit_map(WrapperMap::new(KEY_QUOTED, false.into_deserializer(), self))
            }
            HEADERED => visitor.visit_map(WrapperMap::new(
                KEY_HEADER,
                OptionDe::<OperatorDeserializer>(None),
                self,
            )),
            #[cfg(feature = "syntax")]
            crate::text::syntax::de::VALUE => {
                let first = self.de.tok;
                let mut source = b"x={".to_vec();
                self.de
                    .capture_remainder(self.root, first, false, &mut source)?;
                source.push(b'}');
                let encoding = &self.de.encoding;
                crate::text::syntax::value::visit_owned(
                    || crate::text::syntax::de::field_value(&source, encoding),
                    visitor,
                )
            }
            _ => visitor.visit_newtype_struct(self),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}
