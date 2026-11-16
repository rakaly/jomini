use super::{
    TokenReader, decode_quoted,
    dom::ValuesIter,
    reader::{Token, is_operator_start},
};
use crate::{
    DeserializeError, DeserializeErrorKind, Encoding, Error, Scalar, TextTape, TextToken,
    Utf8Encoding, Windows1252Encoding,
    text::{ArrayReader, FieldsIter, ObjectReader, Operator, Reader, ScalarReader, ValueReader},
};
use serde::de::{self, Deserialize, DeserializeOwned, DeserializeSeed, Visitor};
use std::{
    borrow::Cow,
    fmt::{self, Debug},
    io::Read,
};

/// Represents the field value that contains an operator
///
/// A `Property` is a specially marked type that signals to the deserializer to
/// preserve operators.
///
/// ```
/// # #[cfg(feature = "derive")] {
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// use jomini::{
///     text::{Operator, Property},
///     JominiDeserialize, TextDeserializer,
/// };
/// use serde::Deserialize;
///
/// let data = b"modifier = { abc=yes factor=2 num > 2 }";
/// let actual: MyStruct = jomini::text::de::from_windows1252_slice(&data[..])?;
/// assert_eq!(
///     actual,
///     MyStruct {
///         modifier: Modifier {
///             abc: true,
///             factor: Property::new(Operator::Equal, 2),
///             num: Property::new(Operator::GreaterThan, 2)
///         }
///     }
/// );
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct Modifier {
///     abc: bool,
///     factor: Property<u8>,
///     num: Property<u8>,
/// }
///
/// #[derive(Deserialize, Debug, PartialEq)]
/// struct MyStruct {
///     modifier: Modifier,
/// }
/// # Ok(())
/// # }
/// # }
/// ```
#[derive(serde::Deserialize)]
#[serde(rename = "_internal_jomini_property")]
pub struct Property<T> {
    operator: Operator,
    value: T,
}

impl<T> fmt::Debug for Property<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Property")
            .field("operator", &self.operator)
            .field("value", &self.value)
            .finish()
    }
}

impl<T> PartialEq for Property<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.operator == other.operator && self.value == other.value
    }
}

impl<T> Eq for Property<T> where T: Eq {}

impl<T> Clone for Property<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            operator: self.operator,
            value: self.value.clone(),
        }
    }
}

impl<T> Copy for Property<T> where T: Copy {}

impl<T> Property<T> {
    /// Creates a new property
    pub fn new(operator: Operator, value: T) -> Self {
        Property { operator, value }
    }

    /// Return the present operator in the property
    pub fn operator(&self) -> Operator {
        self.operator
    }

    /// Return a reference to the value
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Convert the property into the value
    pub fn into_value(self) -> T {
        self.value
    }
}

/// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
pub fn from_windows1252_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_windows1252_slice(data)?.deserialize()
}

/// Create a Windows1252 text value from a reader.
///
/// This uses the streaming parser described by [`TokenReader`].
pub fn from_windows1252_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_windows1252_reader(TokenReader::new(reader)).deserialize()
}

/// Convenience method for deserializing streaming utf8 data into a Rust value
///
/// This uses the streaming parser described by [`TokenReader`].
pub fn from_utf8_reader<T, R>(reader: R) -> Result<T, Error>
where
    T: DeserializeOwned,
    R: Read,
{
    TextDeserializer::from_utf8_reader(TokenReader::new(reader)).deserialize()
}

/// Convenience method for parsing the given text data and deserializing as utf8 encoded.
pub fn from_utf8_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
where
    T: Deserialize<'a>,
{
    TextDeserializer::from_utf8_slice(data)?.deserialize()
}

/// A [`Token`] whose scalar is held by a raw pointer.
///
/// A reference to a scalar asserts that the reader's buffer is not written
/// to while the reference is live, and serde passes accessors by value,
/// which keeps references in their fields live for the whole visit. A refill
/// moves the buffer, so tokens that an accessor holds while the reader
/// advances are stored raw, and turned back into a [`Token`] only while the
/// scalar is unchanged.
#[derive(Clone, Copy)]
enum RawToken {
    Open,
    Close,
    Operator(Operator),
    Unquoted(*const [u8]),
    Quoted(*const [u8]),
}

impl RawToken {
    #[inline]
    fn new(token: Token<'_>) -> Self {
        match token {
            Token::Open => RawToken::Open,
            Token::Close => RawToken::Close,
            Token::Operator(op) => RawToken::Operator(op),
            Token::Unquoted(s) => RawToken::Unquoted(s.as_bytes()),
            Token::Quoted(s) => RawToken::Quoted(s.as_bytes()),
        }
    }

    /// Return the token.
    ///
    /// # Safety
    ///
    /// The bytes of a scalar must not have been moved or overwritten since
    /// the token was read.
    #[inline]
    unsafe fn get<'a>(self) -> Token<'a> {
        match self {
            RawToken::Open => Token::Open,
            RawToken::Close => Token::Close,
            RawToken::Operator(op) => Token::Operator(op),
            RawToken::Unquoted(s) => Token::Unquoted(Scalar::new(unsafe { &*s })),
            RawToken::Quoted(s) => Token::Quoted(Scalar::new(unsafe { &*s })),
        }
    }
}

/// A serde deserializer over streaming data
pub struct TextReaderDeserializer<'r, E> {
    reader: TokenReader<'r>,
    encoding: E,

    /// The operator of the field with an empty key that is being read. The
    /// key consumed the operator, so the value does not read one.
    empty_key_op: Option<Operator>,

    /// Holds a scalar that `peek_after` read past while the reader refilled
    /// its buffer.
    scratch: Vec<u8>,
}

impl<'r, E: Encoding> TextReaderDeserializer<'r, E> {
    /// Deserialize into provided type
    pub fn deserialize<T>(&mut self) -> Result<T, Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl<'r, E> TextReaderDeserializer<'r, E> {
    /// Return the first byte after the whitespace and comments that follow
    /// `token` without consuming it, or `None` at the end of the data.
    ///
    /// Reading more data moves the buffer that a scalar token points into,
    /// so before refilling, the scalar is copied into the scratch buffer and
    /// `token` is rewritten to point to it. The copy stays valid until the
    /// next refill in this function copies another scalar, so the token must
    /// be consumed before the next lookahead.
    #[inline]
    fn peek_after(&mut self, token: &mut RawToken) -> Result<Option<u8>, Error> {
        match self.reader.peek_buffered() {
            Some(x) => Ok(Some(x)),
            None => self.peek_after_refill(token),
        }
    }

    #[cold]
    #[inline(never)]
    fn peek_after_refill(&mut self, token: &mut RawToken) -> Result<Option<u8>, Error> {
        if let RawToken::Quoted(scalar) | RawToken::Unquoted(scalar) = token
            && !std::ptr::eq(scalar.cast::<u8>(), self.scratch.as_ptr())
        {
            // SAFETY: the scalar was read since the last refill, so its bytes
            // are unchanged.
            self.scratch.clear();
            self.scratch.extend_from_slice(unsafe { &**scalar });
            *scalar = std::ptr::slice_from_raw_parts(self.scratch.as_ptr(), self.scratch.len());
        }

        while self.reader.fill_buffer()? {
            if let Some(x) = self.reader.peek_buffered() {
                return Ok(Some(x));
            }
        }

        Ok(None)
    }
}

impl<'de, 'r, E: Encoding> de::Deserializer<'de> for &'_ mut TextReaderDeserializer<'r, E> {
    type Error = Error;

    /// The root is an object, so it is visited as a map.
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let me = std::ptr::addr_of!(self);
        visitor.visit_map(TextReaderAnyMap::new(me, true, None))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let me = std::ptr::addr_of!(self);
        visitor.visit_map(TextReaderMap::new(me, true))
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
        self.deserialize_map(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

/// A map over the fields of an object.
///
/// An operator where a key is expected, as in `=1825.6.28` in an EU4 save,
/// starts a field whose key is the empty string. An operator after a block
/// in key position, as in `{ a = b }={ 1 2 3 }`, makes the block the key,
/// so the map skips the field.
struct TextReaderMap<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    root: bool,
}

impl<'a, 'r, E> TextReaderMap<'a, 'r, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<'r, E>, root: bool) -> Self {
        TextReaderMap { de, root }
    }
}

impl<'a, 'r, E: Encoding> TextReaderMap<'a, 'r, E> {
    /// Return the key of a field that starts with the operator.
    #[cold]
    fn empty_key(&mut self, op: Operator) -> Token<'static> {
        unsafe { self.de.read() }.empty_key_op = Some(op);
        Token::Unquoted(Scalar::new(b""))
    }

    /// Return the operator of a field whose value does not follow an
    /// operator: the operator of the empty key, or `=` for `key { ... }`.
    #[cold]
    fn missing_operator(&mut self) -> Operator {
        let de = unsafe { self.de.read() };
        de.empty_key_op.take().unwrap_or(Operator::Equal)
    }

    /// Skip the block that the reader read where a key is expected, and
    /// skip the field when the block is its key. Return the next key, or
    /// `None` at the end of the object.
    #[cold]
    fn key_after_block(&mut self) -> Result<Option<Token<'a>>, Error> {
        let mut after_block = true;
        unsafe { self.de.read() }.reader.skip_container()?;
        loop {
            match unsafe { self.de.read() }.reader.next()? {
                Some(Token::Close) => return Ok(None),
                Some(Token::Open) => {
                    unsafe { self.de.read() }.reader.skip_container()?;
                    after_block = true;
                }
                Some(Token::Operator(_)) if after_block => {
                    let reader = &mut unsafe { self.de.read() }.reader;
                    match reader.read()? {
                        Token::Open => reader.skip_container()?,
                        Token::Close => return Ok(None),
                        _ => {}
                    }
                    after_block = false;
                }
                Some(Token::Operator(op)) => return Ok(Some(self.empty_key(op))),
                Some(token) => return Ok(Some(token)),
                None if self.root => return Ok(None),
                None => return Err(unsafe { self.de.read() }.reader.eof_error().into()),
            }
        }
    }
}

impl<'de, 'r, E: Encoding> de::MapAccess<'de> for TextReaderMap<'_, 'r, E> {
    type Error = Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        let token = match unsafe { self.de.read() }.reader.next() {
            Ok(Some(Token::Close)) => return Ok(None),
            Ok(Some(Token::Open)) => match self.key_after_block()? {
                Some(token) => token,
                None => return Ok(None),
            },
            Ok(Some(Token::Operator(op))) => self.empty_key(op),
            Ok(Some(token)) => token,
            Ok(None) if self.root => return Ok(None),
            Ok(None) => return Err(unsafe { self.de.read() }.reader.eof_error().into()),
            Err(e) => return Err(e.into()),
        };

        seed.deserialize(TextReaderTokenDeserializer::new(self.de, token))
            .map(Some)
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        let op = match unsafe { self.de.read() }.reader.read_operator()? {
            Some(op) => op,
            None => self.missing_operator(),
        };

        seed.deserialize(TextReaderValueDeserializer { de: self.de, op })
    }
}

/// Deserializes a token that was read.
///
/// Serde passes deserializers by value, so a scalar token stays borrowed for
/// the whole visit, and the reader must not refill its buffer meanwhile.
/// Field values that may look ahead are deserialized by
/// [`TextReaderValueDeserializer`] instead.
struct TextReaderTokenDeserializer<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    token: Token<'a>,
    op: Operator,
}

impl<'a, 'r, E> TextReaderTokenDeserializer<'a, 'r, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<'r, E>, token: Token<'a>) -> Self {
        Self {
            de,
            token,
            op: Operator::Equal,
        }
    }
}

impl<'a, 'de: 'a, 'r, E: Encoding> TextReaderTokenDeserializer<'a, 'r, E> {
    /// Visit the block whose open token was just read. The first item
    /// decides its kind like a [`TextTape`] does: a field (a scalar followed
    /// by an operator, or an operator with an empty key) makes it a map, and
    /// anything else, including an empty block, makes it a sequence. The
    /// first token is replayed to the map or sequence. Empty blocks before
    /// the first item are skipped, as a `TextTape` does.
    #[inline(never)]
    fn visit_block<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        let mut first = RawToken::new(unsafe { self.de.read() }.reader.read()?);
        while matches!(first, RawToken::Open)
            && unsafe { self.de.read() }.peek_after(&mut first)? == Some(b'}')
        {
            let reader = &mut unsafe { self.de.read() }.reader;
            reader.read()?;
            first = RawToken::new(reader.read()?);
        }

        let is_map = match first {
            RawToken::Close => {
                return visitor.visit_seq(TextReaderAnySeq::new(self.de, false, None));
            }
            RawToken::Operator(_) => true,
            RawToken::Open => false,
            RawToken::Quoted(_) | RawToken::Unquoted(_) => {
                let next = unsafe { self.de.read() }.peek_after(&mut first)?;
                next.is_some_and(is_operator_start)
            }
        };

        if is_map {
            visitor.visit_map(TextReaderAnyMap::new(self.de, false, Some(first)))
        } else {
            visitor.visit_seq(TextReaderAnySeq::new(self.de, false, Some(first)))
        }
    }

    /// Visit the token without looking ahead: a scalar as its text, an
    /// operator as its symbol, and a block as a sequence. Typed
    /// deserialization falls back to this when the token does not match the
    /// requested type, so the visitor can report the mismatch, and typed
    /// visitors are not instantiated with the maps of `deserialize_any`.
    fn visit_token<V>(self, visitor: V) -> Result<V::Value, Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Token::Open => visitor.visit_seq(TextReaderSeq::new(self.de)),
            Token::Close => Err(Error::invalid_syntax(
                "did not expect end",
                unsafe { self.de.read() }.reader.position(),
            )),
            Token::Operator(x) => visitor.visit_str(x.symbol()),
            Token::Unquoted(s) => visit_text(self.encoding().decode(s.as_bytes()), visitor),
            Token::Quoted(s) => visit_text(decode_quoted(self.encoding(), s.as_bytes()), visitor),
        }
    }

    fn encoding(&self) -> &'a E {
        &unsafe { self.de.read() }.encoding
    }
}

#[inline]
fn visit_text<'de, V: Visitor<'de>>(text: Cow<'_, str>, visitor: V) -> Result<V::Value, Error> {
    match text {
        Cow::Borrowed(x) => visitor.visit_str(x),
        Cow::Owned(x) => visitor.visit_string(x),
    }
}

impl<'a, 'de: 'a, 'r, E: Encoding> de::Deserializer<'de>
    for TextReaderTokenDeserializer<'a, 'r, E>
{
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token {
            Token::Open => self.visit_block(visitor),
            _ => self.visit_token(visitor),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.token.as_scalar().and_then(|x| x.to_bool().ok()) {
            Some(x) => visitor.visit_bool(x),
            None => self.visit_token(visitor),
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
        match self.token.as_scalar().and_then(|x| x.to_i64().ok()) {
            Some(x) => visitor.visit_i64(x),
            None => self.visit_token(visitor),
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
        match self.token.as_scalar().and_then(|x| x.to_u64().ok()) {
            Some(x) => visitor.visit_u64(x),
            None => self.visit_token(visitor),
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
        match self.token.as_scalar().and_then(|x| x.to_f64().ok()) {
            Some(x) => visitor.visit_f64(x),
            None => self.visit_token(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.visit_token(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let encoding = &unsafe { self.de.read() }.encoding;
        let decoded = match self.token {
            Token::Unquoted(s) => encoding.decode(s.as_bytes()),
            Token::Quoted(s) => decode_quoted(encoding, s.as_bytes()),
            _ => return self.visit_token(visitor),
        };

        match decoded {
            Cow::Borrowed(x) => visitor.visit_str(x),
            Cow::Owned(x) => visitor.visit_string(x),
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
        match self.token.as_scalar() {
            Some(s) => visitor.visit_bytes(s.as_bytes()),
            None => self.visit_token(visitor),
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

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visit_seq_after(self.de, RawToken::new(self.token), visitor)
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
        if matches!(self.token, Token::Open) {
            visitor.visit_map(TextReaderMap::new(self.de, false))
        } else {
            self.visit_token(visitor)
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
            visitor.visit_map(PropertyReaderMap {
                de: self.de,
                token: self.token,
                op: self.op,
                state: 0,
            })
        } else {
            self.deserialize_map(visitor)
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
        visitor.visit_enum(TextReaderEnum::new(self.de, self.token))
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
        if matches!(self.token, Token::Open) {
            unsafe { self.de.read() }.reader.skip_container()?;
        }
        visitor.visit_unit()
    }
}

/// Implement deserialize methods that read the token of a field value and
/// deserialize it with a [`TextReaderTokenDeserializer`].
macro_rules! forward_to_token {
    ($($method:ident)*) => {$(
        fn $method<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where
            V: Visitor<'de>,
        {
            self.read()?.$method(visitor)
        }
    )*};
}

/// Deserializes a field value, reading its token when a deserialize method
/// is called.
///
/// Holding no token lets `deserialize_any` look past a scalar for a header
/// block, as in `color = rgb { 1 2 3 }`, even when that refills the reader's
/// buffer: the scalar is then held only by a local [`RawToken`], which
/// `peek_after` moves to the scratch buffer before refilling, and not by a
/// deserializer that serde passed by value.
struct TextReaderValueDeserializer<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    op: Operator,
}

impl<'a, 'r, E> TextReaderValueDeserializer<'a, 'r, E> {
    #[inline]
    fn read(self) -> Result<TextReaderTokenDeserializer<'a, 'r, E>, Error> {
        let token = unsafe { self.de.read() }.reader.read()?;
        let mut deser = TextReaderTokenDeserializer::new(self.de, token);
        deser.op = self.op;
        Ok(deser)
    }
}

impl<'a, 'de: 'a, 'r, E: Encoding> de::Deserializer<'de>
    for TextReaderValueDeserializer<'a, 'r, E>
{
    type Error = Error;

    /// Visit a scalar, or the block after it when the scalar is a header. An
    /// empty block after the scalar is skipped and the scalar is visited, as
    /// a `TextTape` does.
    #[inline(never)]
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let de = unsafe { self.de.read() };
        let mut token = RawToken::new(de.reader.read()?);
        if matches!(token, RawToken::Unquoted(_) | RawToken::Quoted(_))
            && de.peek_after(&mut token)? == Some(b'{')
        {
            de.reader.read()?;
            if de.peek_after(&mut token)? != Some(b'}') {
                let block = TextReaderTokenDeserializer::new(self.de, Token::Open);
                return block.visit_block(visitor);
            }
            de.reader.read()?;
        }

        // SAFETY: a scalar that was read past a refill was moved to the
        // scratch buffer.
        let mut deser = TextReaderTokenDeserializer::new(self.de, unsafe { token.get() });
        deser.op = self.op;
        deser.deserialize_any(visitor)
    }

    forward_to_token! {
        deserialize_bool deserialize_i8 deserialize_i16 deserialize_i32 deserialize_i64
        deserialize_i128 deserialize_u8 deserialize_u16 deserialize_u32 deserialize_u64
        deserialize_u128 deserialize_f32 deserialize_f64 deserialize_char deserialize_str
        deserialize_string deserialize_bytes deserialize_byte_buf deserialize_unit
        deserialize_map deserialize_identifier deserialize_ignored_any
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let token = unsafe { self.de.read() }.reader.read()?;
        visit_seq_after(self.de, RawToken::new(token), visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.read()?.deserialize_unit_struct(name, visitor)
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
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
        self.read()?.deserialize_struct(name, fields, visitor)
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
        self.read()?.deserialize_enum(name, variants, visitor)
    }
}

/// Visit the sequence that `token` starts: the block it opens, or a header
/// and its block, as in `color = rgb { 1 2 3 }`. The token is raw, as the
/// reader refills while the sequence is read.
#[inline]
fn visit_seq_after<'a, 'de: 'a, 'r, E: Encoding, V: Visitor<'de>>(
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    token: RawToken,
    visitor: V,
) -> Result<V::Value, Error> {
    if matches!(token, RawToken::Open) {
        let mut seq = TextReaderSeq::new(de);
        let result = visitor.visit_seq(&mut seq)?;
        if !seq.hit_end {
            // For when we are deserializing an array that doesn't read
            // the closing token, we close it for them
            if !matches!(unsafe { de.read() }.reader.read()?, Token::Close) {
                return Err(Error::invalid_syntax(
                    "Expected sequence to be terminated with an end token",
                    unsafe { de.read() }.reader.position(),
                ));
            }
        }

        Ok(result)
    } else {
        // SAFETY: the header was just read. The sequence holds it in a
        // local, not in a deserializer passed by value.
        let mut seq = TextReaderInitSeq::new(de, unsafe { token.get() });
        visitor.visit_seq(&mut seq)
    }
}

struct TextReaderSeq<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    hit_end: bool,
}

impl<'a, 'r, E> TextReaderSeq<'a, 'r, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<'r, E>) -> Self {
        TextReaderSeq { de, hit_end: false }
    }
}

impl<'de, 'r, E: Encoding> de::SeqAccess<'de> for TextReaderSeq<'_, 'r, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match unsafe { self.de.read() }.reader.read()? {
            Token::Close => {
                self.hit_end = true;
                Ok(None)
            }
            token => seed
                .deserialize(TextReaderTokenDeserializer::new(self.de, token))
                .map(Some),
        }
    }
}

/// For deserializing a sequence where there is an initial token that is not
/// Token::Open. Useful for deserializing header containers:
///
/// ```plain
/// color = rgb { 100 10 200 }
/// ```
struct TextReaderInitSeq<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    token: Option<Token<'a>>,
}

impl<'a, 'r, E> TextReaderInitSeq<'a, 'r, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<'r, E>, token: Token<'a>) -> Self {
        TextReaderInitSeq {
            de,
            token: Some(token),
        }
    }
}

impl<'de, 'r, E: Encoding> de::SeqAccess<'de> for TextReaderInitSeq<'_, 'r, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(token) = self.token.take() {
            return seed
                .deserialize(TextReaderTokenDeserializer::new(self.de, token))
                .map(Some);
        }

        match unsafe { self.de.read() }.reader.read()? {
            Token::Close => Ok(None),
            token => seed
                .deserialize(TextReaderTokenDeserializer::new(self.de, token))
                .map(Some),
        }
    }
}

/// A map over a block that `deserialize_any` found to be an object, or over
/// the root.
///
/// Like a [`TextTape`] object, scalars that trail the fields, as in
/// `{ a = b c d }`, are yielded as a sequence under the key `remainder`. The
/// sequence runs to the end of the block, so a field after the trailing
/// scalars (`{ a = b c d = e }`, which a `TextTape` rejects) is read as the
/// elements `d`, `=`, and `e`.
struct TextReaderAnyMap<'a, 'r, E> {
    map: TextReaderMap<'a, 'r, E>,

    /// The first key, read while deciding the kind of block.
    first: Option<RawToken>,

    /// The first trailing scalar, once the `remainder` key is yielded.
    remainder: Option<RawToken>,
    done: bool,
}

impl<'a, 'r, E> TextReaderAnyMap<'a, 'r, E> {
    fn new(
        de: *const &'a mut TextReaderDeserializer<'r, E>,
        root: bool,
        first: Option<RawToken>,
    ) -> Self {
        TextReaderAnyMap {
            map: TextReaderMap::new(de, root),
            first,
            remainder: None,
            done: false,
        }
    }
}

impl<'de, 'r, E: Encoding> de::MapAccess<'de> for TextReaderAnyMap<'_, 'r, E> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if self.done {
            return Ok(None);
        }

        let de = self.map.de;
        let token = match self.first.take() {
            Some(token) => match unsafe { token.get() } {
                Token::Operator(op) => self.map.empty_key(op),
                token => token,
            },
            None => loop {
                match unsafe { de.read() }.reader.next()? {
                    // A `TextTape` skips an unbalanced close at the root
                    Some(Token::Close) if self.map.root => {}
                    Some(Token::Close) => return Ok(None),
                    Some(Token::Open) => match self.map.key_after_block()? {
                        Some(token) => break token,
                        None => return Ok(None),
                    },
                    Some(Token::Operator(op)) => break self.map.empty_key(op),
                    Some(token) => {
                        let mut token = RawToken::new(token);
                        match unsafe { de.read() }.peek_after(&mut token)? {
                            Some(x) if is_operator_start(x) || x == b'{' => {
                                break unsafe { token.get() };
                            }
                            _ => {
                                self.remainder = Some(token);
                                return seed.deserialize(StaticDeserializer("remainder")).map(Some);
                            }
                        }
                    }
                    None if self.map.root => return Ok(None),
                    None => return Err(unsafe { de.read() }.reader.eof_error().into()),
                }
            },
        };

        seed.deserialize(TextReaderTokenDeserializer::new(de, token))
            .map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match self.remainder.take() {
            Some(token) => {
                self.done = true;
                let mut seq = TextReaderAnySeq::new(self.map.de, self.map.root, Some(token));
                seq.skip_operators = true;
                seed.deserialize(de::value::SeqAccessDeserializer::new(seq))
            }
            // Unlike the typed map, this reads the operator as a token, as
            // the lookahead may have skipped the whitespace before an `==`.
            None => {
                let de = self.map.de;
                let reader = &mut unsafe { de.read() }.reader;
                match reader.read()? {
                    Token::Operator(op) => seed.deserialize(TextReaderValueDeserializer { de, op }),
                    token => {
                        let mut deser = TextReaderTokenDeserializer::new(de, token);
                        deser.op = self.map.missing_operator();
                        seed.deserialize(deser)
                    }
                }
            }
        }
    }
}

/// A sequence over a block that `deserialize_any` found to be an array, or
/// over the scalars that trail the fields of an object.
struct TextReaderAnySeq<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    root: bool,

    /// The first element, read while deciding the kind of block.
    first: Option<RawToken>,
    done: bool,

    /// Operators are not elements, as in the remainder of an object, where
    /// a `TextTape` drops them.
    skip_operators: bool,
}

impl<'a, 'r, E> TextReaderAnySeq<'a, 'r, E> {
    /// Create a sequence whose first element is `first`, or that is empty
    /// when the block's close token was read in its place.
    fn new(
        de: *const &'a mut TextReaderDeserializer<'r, E>,
        root: bool,
        first: Option<RawToken>,
    ) -> Self {
        TextReaderAnySeq {
            de,
            root,
            done: first.is_none(),
            first,
            skip_operators: false,
        }
    }
}

impl<'de, 'r, E: Encoding> de::SeqAccess<'de> for TextReaderAnySeq<'_, 'r, E> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.done {
            return Ok(None);
        }

        let token = match self.first.take() {
            Some(token) => unsafe { token.get() },
            None => loop {
                match unsafe { self.de.read() }.reader.next()? {
                    Some(Token::Close) => {
                        self.done = true;
                        return Ok(None);
                    }
                    Some(Token::Operator(_)) if self.skip_operators => {}
                    Some(token) => break token,
                    None if self.root => {
                        self.done = true;
                        return Ok(None);
                    }
                    None => return Err(unsafe { self.de.read() }.reader.eof_error().into()),
                }
            },
        };

        seed.deserialize(TextReaderTokenDeserializer::new(self.de, token))
            .map(Some)
    }
}

struct TextReaderEnum<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    token: Token<'a>,
}

impl<'a, 'r, E> TextReaderEnum<'a, 'r, E> {
    fn new(de: *const &'a mut TextReaderDeserializer<'r, E>, token: Token<'a>) -> Self {
        TextReaderEnum { de, token }
    }
}

impl<'de, 'r, E: Encoding> de::EnumAccess<'de> for TextReaderEnum<'_, 'r, E> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(TextReaderTokenDeserializer::new(self.de, self.token))?;
        Ok((variant, self))
    }
}

impl<'de, 'r, E: Encoding> de::VariantAccess<'de> for TextReaderEnum<'_, 'r, E> {
    type Error = Error;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, _seed: T) -> Result<T::Value, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(
                "unsupported enum deserialization. Please file issue",
            ),
        }))
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(
                "unsupported enum deserialization. Please file issue",
            ),
        }))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(
                "unsupported enum deserialization. Please file issue",
            ),
        }))
    }
}

struct PropertyReaderMap<'a, 'r, E> {
    de: *const &'a mut TextReaderDeserializer<'r, E>,
    op: Operator,
    token: Token<'a>,
    state: usize,
}

impl<'de, 'r, E: Encoding> de::MapAccess<'de> for PropertyReaderMap<'_, 'r, E> {
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
            seed.deserialize(OperatorDeserializer(self.op))
        } else {
            seed.deserialize(TextReaderTokenDeserializer::new(self.de, self.token))
        }
    }
}

/// A structure to deserialize text data into Rust values.
///
/// The example below demonstrates how to deserialize data
///
/// ```
/// use jomini::{TextDeserializer, TextTape};
/// use serde::Deserialize;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructA {
///   field1: String,
///   field2: i32,
/// }
///
/// let data = b"field1=ENG field2=2";
/// let a: StructA = TextDeserializer::from_windows1252_slice(&data[..])?.deserialize()?;
/// assert_eq!(a, StructA {
///   field1: "ENG".to_string(),
///   field2: 2,
/// });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// Since the Clausewitz text format is not self describing, one should not use
/// the [`flatten` serde attribute][0], as [flattened structs won't type
/// cast][1], and instead prefer to deserialize in two steps:
///
/// ```
/// use jomini::{TextDeserializer, TextTape};
/// use serde::Deserialize;
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructB {
///   field1: String,
/// }
///
/// #[derive(Debug, Clone, Deserialize, PartialEq)]
/// pub struct StructC {
///   field2: i32,
/// }
///
/// let data = b"field1=ENG field2=2";
/// let tape = TextTape::from_slice(&data[..])?;
/// let b: StructB = TextDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// let c: StructC = TextDeserializer::from_windows1252_tape(&tape).deserialize()?;
/// assert_eq!(b, StructB { field1: "ENG".to_string() });
/// assert_eq!(c, StructC { field2: 2 });
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// [0]: https://serde.rs/attr-flatten.html
/// [1]: https://github.com/serde-rs/serde/issues/1529
pub struct TextDeserializer<'a, 'b, E> {
    kind: TextDeserializerKind<'a, 'b, E>,
}

enum TextDeserializerKind<'a, 'b, E> {
    Owned { tape: TextTape<'a>, encoding: E },
    Borrowed { tape: &'b TextTape<'a>, encoding: E },
    Reader { reader: &'b ObjectReader<'a, 'b, E> },
}

impl TextDeserializer<'_, '_, Windows1252Encoding> {
    /// Create a Windows1252 text deserializer over a reader.
    ///
    /// This uses the streaming parser described by [`TokenReader`].
    pub fn from_windows1252_reader<'r>(
        reader: TokenReader<'r>,
    ) -> TextReaderDeserializer<'r, Windows1252Encoding> {
        TextDeserializer::from_encoded_reader(reader, Windows1252Encoding)
    }
}

impl TextDeserializer<'_, '_, Utf8Encoding> {
    /// Create a UTF8 text deserializer over a reader
    ///
    /// This uses the streaming parser described by [`TokenReader`].
    pub fn from_utf8_reader<'r>(
        reader: TokenReader<'r>,
    ) -> TextReaderDeserializer<'r, Utf8Encoding> {
        TextDeserializer::from_encoded_reader(reader, Utf8Encoding)
    }
}

impl<E: Encoding> TextDeserializer<'_, '_, E> {
    /// Create a text deserializer over a reader.
    ///
    /// The deserializer decodes quoted strings with the given encoding.
    /// It uses the streaming parser described by [`TokenReader`].
    pub fn from_encoded_reader<'r>(
        reader: TokenReader<'r>,
        encoding: E,
    ) -> TextReaderDeserializer<'r, E> {
        TextReaderDeserializer {
            reader,
            encoding,
            empty_key_op: None,
            scratch: Vec::new(),
        }
    }
}

impl<'a, 'b> TextDeserializer<'a, 'b, Windows1252Encoding> {
    /// Convenience method for parsing the given text data and deserializing as windows1252 encoded.
    pub fn from_windows1252_slice(
        data: &'a [u8],
    ) -> Result<TextDeserializer<'a, 'b, Windows1252Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are windows1252 encoded.
    pub fn from_windows1252_tape(
        tape: &'b TextTape<'a>,
    ) -> TextDeserializer<'a, 'b, Windows1252Encoding> {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Windows1252Encoding::new(),
            },
        }
    }
}

impl<'a, 'b> TextDeserializer<'a, 'b, Utf8Encoding> {
    /// Convenience method for parsing the given text data and deserializing as utf8 encoded.
    pub fn from_utf8_slice(
        data: &'a [u8],
    ) -> Result<TextDeserializer<'a, 'b, Utf8Encoding>, Error> {
        let tape = TextTape::from_slice(data)?;
        Ok(TextDeserializer {
            kind: TextDeserializerKind::Owned {
                tape,
                encoding: Utf8Encoding::new(),
            },
        })
    }

    /// Deserialize the given text tape assuming quoted strings are utf8 encoded.
    pub fn from_utf8_tape(tape: &'b TextTape<'a>) -> TextDeserializer<'a, 'b, Utf8Encoding> {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed {
                tape,
                encoding: Utf8Encoding::new(),
            },
        }
    }
}

impl<'a, 'b, E> TextDeserializer<'a, 'b, E>
where
    E: Encoding + Clone,
{
    /// Deserialize the given text tape assuming quoted strings can be decoded
    /// according to the given encoder
    pub fn from_encoded_tape(tape: &'b TextTape<'a>, encoding: E) -> TextDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextDeserializer {
            kind: TextDeserializerKind::Borrowed { tape, encoding },
        }
    }

    /// Create text deserialization from an object reader
    ///
    /// See example at [ObjectReader::deserialize]
    pub fn from_reader(reader: &'b ObjectReader<'a, 'b, E>) -> TextDeserializer<'a, 'b, E>
    where
        E: Encoding + Clone,
    {
        TextDeserializer {
            kind: TextDeserializerKind::Reader { reader },
        }
    }

    /// Deserialize into provided type
    pub fn deserialize<T>(&self) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        T::deserialize(self)
    }
}

impl<'de, E> de::Deserializer<'de> for &'_ TextDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(Error::from(DeserializeError {
            kind: DeserializeErrorKind::Unsupported(
                "root deserializer can only work with key value pairs",
            ),
        }))
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.kind {
            TextDeserializerKind::Owned { tape, encoding } => {
                let reader = ObjectReader::new(tape, encoding.clone());
                visitor.visit_map(MapAccess::new(reader, DeserializationConfig))
            }
            TextDeserializerKind::Borrowed { tape, encoding } => {
                let reader = ObjectReader::new(tape, encoding.clone());
                visitor.visit_map(MapAccess::new(reader, DeserializationConfig))
            }
            TextDeserializerKind::Reader { reader } => {
                let access = MapAccess {
                    fields: reader.fields(),
                    config: DeserializationConfig,
                    value: None,
                    at_remainder: false,
                };
                visitor.visit_map(access)
            }
        }
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
        self.deserialize_map(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct enum ignored_any identifier
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct DeserializationConfig;

macro_rules! visit_str {
    ($data: expr, $visitor: expr) => {
        match $data {
            Cow::Borrowed(s) => $visitor.visit_borrowed_str(s),
            Cow::Owned(s) => $visitor.visit_string(s),
        }
    };
}

struct MapAccess<'de, 'tokens, E> {
    fields: FieldsIter<'de, 'tokens, E>,
    config: DeserializationConfig,
    value: Option<OperatorValue<'de, 'tokens, E>>,
    at_remainder: bool,
}

impl<'de, 'tokens, E> MapAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: ObjectReader<'de, 'tokens, E>, config: DeserializationConfig) -> Self {
        MapAccess {
            fields: reader.fields(),
            config,
            value: None,
            at_remainder: false,
        }
    }
}

impl<'de, E> de::MapAccess<'de> for MapAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        if let Some((key, op, value)) = self.fields.next() {
            self.value = Some(OperatorValue {
                operator: op.unwrap_or(Operator::Equal),
                value,
            });

            seed.deserialize(ValueDeserializer {
                kind: ValueKind::Scalar(key),
                config: self.config,
            })
            .map(Some)
        } else if !self.at_remainder && !self.fields.remainder().is_empty() {
            self.at_remainder = true;
            self.value = None;
            seed.deserialize(StaticDeserializer("remainder")).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        match &self.value {
            Some(x) => seed.deserialize(ValueDeserializer {
                kind: ValueKind::OperatorValue(x.clone()),
                config: self.config,
            }),
            None => seed.deserialize(ValueDeserializer {
                kind: ValueKind::Array(self.fields.remainder()),
                config: self.config,
            }),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (lower, upper) = self.fields.size_hint();
        Some(upper.unwrap_or(lower))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct OperatorValue<'data, 'tokens, E> {
    operator: Operator,
    value: ValueReader<'data, 'tokens, E>,
}

#[derive(Debug, Clone)]
enum ValueKind<'data, 'tokens, E> {
    OperatorValue(OperatorValue<'data, 'tokens, E>),
    Value(ValueReader<'data, 'tokens, E>),
    Scalar(ScalarReader<'data, E>),
    Array(ArrayReader<'data, 'tokens, E>),
}

struct ValueDeserializer<'de, 'tokens, E> {
    kind: ValueKind<'de, 'tokens, E>,
    config: DeserializationConfig,
}

impl<'de, 'tokens, E> ValueDeserializer<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn reader(&self) -> Reader<'de, 'tokens, E> {
        match self.kind.clone() {
            ValueKind::OperatorValue(x) => Reader::Value(x.value),
            ValueKind::Value(x) => Reader::Value(x),
            ValueKind::Scalar(x) => Reader::Scalar(x),
            ValueKind::Array(x) => Reader::Array(x),
        }
    }

    fn value_reader(&self) -> Option<ValueReader<'de, 'tokens, E>> {
        match self.kind.clone() {
            ValueKind::OperatorValue(x) => Some(x.value),
            ValueKind::Value(x) => Some(x),
            _ => None,
        }
    }

    fn read_scalar(&self) -> Result<crate::Scalar<'de>, Error> {
        self.reader().read_scalar().map_err(Error::from)
    }
}

macro_rules! deserialize_any_value {
    ($self: expr, $x: expr, $visitor: expr) => {
        match $x.token() {
            TextToken::Quoted(_) | TextToken::Unquoted(_) => {
                visit_str!($x.read_str().map_err(Error::from)?, $visitor)
            }
            TextToken::Header(_) => {
                // This is a bit of a hack to avoid overflows on flattened
                // structs, which already have poor deserialization support,
                // but this at least allows for forward progress in the
                // cases where flattened structs can be used.
                if matches!($x.next(), Some(TextToken::Object { .. })) {
                    $self.deserialize_map($visitor)
                } else {
                    $self.deserialize_seq($visitor)
                }
            }
            TextToken::Array { .. } => $self.deserialize_seq($visitor),
            TextToken::Object { .. } => $self.deserialize_map($visitor),
            _ => Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported("unsupported value reader token"),
            })),
        }
    };
}

impl<'de, E> de::Deserializer<'de> for ValueDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn deserialize_any<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &mut self.kind {
            ValueKind::OperatorValue(x) => deserialize_any_value!(self, x.value, visitor),
            ValueKind::Value(x) => deserialize_any_value!(self, x, visitor),
            ValueKind::Scalar(x) => visit_str!(x.read_str(), visitor),
            ValueKind::Array(_) => self.deserialize_seq(visitor),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Ok(x) = self.reader().read_str() {
            visit_str!(x, visitor)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Ok(scalar) = self.read_scalar() {
            visitor.visit_borrowed_bytes(scalar.as_bytes())
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.read_scalar().map(|x| x.as_bytes().to_vec());
        if let Ok(x) = val {
            visitor.visit_byte_buf(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Ok(x) = self.reader().read_string() {
            visitor.visit_string(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_bool().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_bool(x)
        } else {
            self.deserialize_any(visitor)
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
        let val = self
            .read_scalar()
            .and_then(|x| x.to_i64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_i64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_i64(visitor)
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
        let val = self
            .read_scalar()
            .and_then(|x| x.to_u64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_u64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_u64(visitor)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self
            .read_scalar()
            .and_then(|x| x.to_f64().map_err(Error::from));

        if let Ok(x) = val {
            visitor.visit_f64(x)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_f64(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let val = self.value_reader().and_then(|x| x.read_object().ok());
        if let Some(x) = val {
            let map = MapAccess::new(x, self.config);
            visitor.visit_map(map)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let arr = match self.reader() {
            Reader::Array(x) => Some(x),
            Reader::Value(x) => x.read_array().ok(),
            _ => None,
        };

        if let Some(x) = arr {
            let map = SeqAccess {
                config: self.config,
                values: x.values(),
            };
            visitor.visit_seq(map)
        } else {
            self.deserialize_any(visitor)
        }
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
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

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
    }

    #[inline]
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        let err = || {
            Err(Error::from(DeserializeError {
                kind: DeserializeErrorKind::Unsupported("unexpected reader for enum"),
            }))
        };

        let value_reader = match self.reader() {
            Reader::Value(x) => x,
            _ => return err(),
        };

        if let Ok(arr) = value_reader.read_array() {
            let mut values = arr.values();
            let variant = if let Some(variant) = values.next() {
                variant
            } else {
                return err();
            };

            visitor.visit_enum(EnumAccess {
                variant,
                values: Some(values),
                config: self.config,
            })
        } else {
            visitor.visit_enum(EnumAccess {
                variant: value_reader,
                values: None,
                config: self.config,
            })
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
        if name == "_internal_jomini_property"
            && let ValueKind::OperatorValue(x) = self.kind
        {
            return visitor.visit_map(PropertyMap {
                value: x.clone(),
                config: self.config,
                state: 0,
            });
        }

        self.deserialize_map(visitor)
    }
}

struct PropertyMap<'de, 'tokens, E> {
    value: OperatorValue<'de, 'tokens, E>,
    config: DeserializationConfig,
    state: usize,
}

impl<'de, E> de::MapAccess<'de> for PropertyMap<'de, '_, E>
where
    E: Encoding + Clone,
{
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
        let old_state = self.state;
        self.state += 1;
        if old_state == 0 {
            seed.deserialize(OperatorDeserializer(self.value.operator))
        } else {
            seed.deserialize(ValueDeserializer {
                kind: ValueKind::Value(self.value.value.clone()),
                config: self.config,
            })
        }
    }
}

struct SeqAccess<'de, 'tokens, E> {
    config: DeserializationConfig,
    values: ValuesIter<'de, 'tokens, E>,
}

impl<'de, E> de::SeqAccess<'de> for SeqAccess<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if let Some(x) = self.values.next() {
            seed.deserialize(ValueDeserializer {
                config: self.config,
                kind: ValueKind::Value(x),
            })
            .map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let (lower, upper) = self.values.size_hint();
        Some(upper.unwrap_or(lower))
    }
}

struct EnumAccess<'de, 'tokens, E> {
    config: DeserializationConfig,
    variant: ValueReader<'de, 'tokens, E>,
    values: Option<ValuesIter<'de, 'tokens, E>>,
}

impl<'de, 'tokens, E> de::EnumAccess<'de> for EnumAccess<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Error = Error;
    type Variant = VariantDeserializer<'de, 'tokens, E>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = ValueDeserializer {
            config: self.config,
            kind: ValueKind::Value(self.variant),
        };

        let visitor = VariantDeserializer {
            config: self.config,
            values: self.values,
        };

        seed.deserialize(variant).map(|v| (v, visitor))
    }
}

struct VariantDeserializer<'de, 'tokens, E> {
    config: DeserializationConfig,
    values: Option<ValuesIter<'de, 'tokens, E>>,
}

impl<'de, 'tokens, E> VariantDeserializer<'de, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn next_value(&mut self) -> Result<ValueKind<'de, 'tokens, E>, DeserializeError> {
        let val = self
            .values
            .as_mut()
            .and_then(|x| x.next())
            .ok_or(DeserializeError {
                kind: DeserializeErrorKind::Unsupported("unexpected none for enum variant seed"),
            })?;

        Ok(ValueKind::Value(val))
    }

    fn next_deserializer(
        &mut self,
    ) -> Result<ValueDeserializer<'de, 'tokens, E>, DeserializeError> {
        let value = self.next_value()?;
        Ok(ValueDeserializer {
            kind: value,
            config: self.config,
        })
    }
}

impl<'de, E> de::VariantAccess<'de> for VariantDeserializer<'de, '_, E>
where
    E: Encoding + Clone,
{
    type Error = Error;

    fn unit_variant(mut self) -> Result<(), Self::Error> {
        if self.values.is_none() {
            Ok(())
        } else {
            de::Deserialize::deserialize(self.next_deserializer()?)
        }
    }

    fn newtype_variant_seed<T>(mut self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        seed.deserialize(self.next_deserializer()?)
    }

    fn tuple_variant<V>(mut self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.next_deserializer()?, visitor)
    }

    fn struct_variant<V>(
        mut self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.next_deserializer()?, "", fields, visitor)
    }
}

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

struct OperatorDeserializer(Operator);

impl<'de> de::Deserializer<'de> for OperatorDeserializer {
    type Error = Error;
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.0.symbol())
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

#[cfg(all(test, feature = "derive"))]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour, UniformDate};
    use jomini_derive::JominiDeserialize;
    use rstest::rstest;
    use serde::{
        Deserialize,
        de::{self, DeserializeOwned, Deserializer},
    };
    use std::fmt;
    use std::{collections::HashMap, marker::PhantomData};

    fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, Error>
    where
        T: Deserialize<'a>,
    {
        super::from_windows1252_slice(data)
    }

    fn from_owned<T>(data: &[u8]) -> T
    where
        T: DeserializeOwned + PartialEq + Debug,
    {
        let tape = TextTape::from_slice(data).unwrap();
        let x1: T = TextDeserializer::from_windows1252_tape(&tape)
            .deserialize()
            .unwrap();
        let reader = TokenReader::new(data);
        let mut des = TextDeserializer::from_windows1252_reader(reader);
        let x2 = T::deserialize(&mut des).unwrap();
        assert_eq!(x1, x2);
        x1
    }

    #[test]
    fn test_single_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                field1: "ENG".to_string()
            }
        );
    }

    #[test]
    fn test_borrowed_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: &'a str,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual, MyStruct { field1: "ENG" });
    }

    #[test]
    fn test_cow_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct<'a> {
            field1: Cow<'a, str>,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: Cow::Borrowed("ENG")
            }
        );
    }

    #[test]
    fn test_escaped_field() {
        let data = br#"name = "Joe \"Captain\" Rogers""#;

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            name: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                name: r#"Joe "Captain" Rogers"#.to_string(),
            }
        );
    }

    #[test]
    fn test_escaped_backslashes() {
        let data = br#"
            path = "C:\\Users\\me"
            model = "gfx\models\ships\cog.mesh"
            mixed = "gfx\\interface\English_flag.tga"
            old_tooltip = "Traditions: +50%\n"
            new_tooltip = "Traditions: +50%\\n"
            backslash = "\\"
            nested = "division_template = \"Anarchist Militia\""
            unquoted = gfx\\icon.dds
        "#;

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            path: String,
            model: String,
            mixed: String,
            old_tooltip: String,
            new_tooltip: String,
            backslash: String,
            nested: String,
            unquoted: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                path: r"C:\Users\me".to_string(),
                model: r"gfx\models\ships\cog.mesh".to_string(),
                mixed: r"gfx\interface\English_flag.tga".to_string(),
                old_tooltip: r"Traditions: +50%\n".to_string(),
                new_tooltip: r"Traditions: +50%\n".to_string(),
                backslash: r"\".to_string(),
                nested: r#"division_template = "Anarchist Militia""#.to_string(),
                unquoted: r"gfx\\icon.dds".to_string(),
            }
        );
    }

    #[test]
    fn test_escaped_keys_and_any_values() {
        let data = br#""C:\\Users" = "say \"hi\"" plain = "a\\b""#;
        let actual: HashMap<String, String> = from_owned(&data[..]);
        let expected = HashMap::from([
            (r"C:\Users".to_string(), r#"say "hi""#.to_string()),
            ("plain".to_string(), r"a\b".to_string()),
        ]);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_written_backslashes_round_trip() {
        let path = r#"C:\Users\me "and" \\server\"#;
        let mut out = Vec::new();
        let mut writer = crate::TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"path").unwrap();
        writer.write_quoted(path.as_bytes()).unwrap();
        assert_eq!(out, br#"path="C:\\Users\\me \"and\" \\\\server\\""#);

        let actual: HashMap<String, String> = from_owned(&out);
        assert_eq!(
            actual,
            HashMap::from([("path".to_string(), path.to_string())])
        );
    }

    #[test]
    fn test_false_field() {
        let data = b"field1=no";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: bool,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: false });
    }

    #[test]
    fn test_true_field() {
        let data = b"field1=yes";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: bool,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: true });
    }

    #[test]
    fn test_u64_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u64,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u32_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_u8_field() {
        let data = b"field1=100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u8,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 100 });
    }

    #[test]
    fn test_u16_field() {
        let data = b"field1=1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: u16,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: 1000 });
    }

    #[test]
    fn test_i8_field() {
        let data = b"field1=-100";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i8,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -100 });
    }

    #[test]
    fn test_i16_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i16,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i32_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_i64_field() {
        let data = b"field1=-1000";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -1000 });
    }

    #[test]
    fn test_f32_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -100.535 });
    }

    #[test]
    fn test_f64_field() {
        let data = b"field1=-100.535";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: f64,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { field1: -100.535 });
    }

    #[test]
    fn test_multiple_to_level_events() {
        let data = b"field1=yes\r\nfield2=no";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: bool,
            field2: bool,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                field1: true,
                field2: false,
            }
        );
    }

    #[test]
    fn test_string_array() {
        let data = include_bytes!("../../tests/fixtures/string-array.txt");
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            dlc_enabled: Vec<String>,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                dlc_enabled: vec![
                    String::from("Conquest of Paradise"),
                    String::from("Wealth of Nations"),
                    String::from("Res Publica"),
                    String::from("Art of War"),
                    String::from("El Dorado"),
                ]
            }
        );
    }

    #[test]
    fn test_nested_object() {
        let data = include_bytes!("../../tests/fixtures/savegame.txt");

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            savegame_version: Version,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Version {
            first: u32,
            second: u32,
            third: u32,
            forth: u32,
            name: Option<String>,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                savegame_version: Version {
                    first: 1,
                    second: 29,
                    third: 5,
                    forth: 0,
                    name: Some(String::from("Manchu")),
                }
            }
        );
    }

    #[test]
    fn test_empty_array() {
        let data = b"discovered_by = {}";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            discovered_by: Vec<String>,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                discovered_by: Vec::new(),
            }
        );
    }

    #[test]
    fn test_array_of_objects() {
        let data = include_bytes!("../../tests/fixtures/campaign_stats.txt");

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            campaign_stats: Vec<Stat>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Stat {
            id: u32,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                campaign_stats: vec![Stat { id: 0 }, Stat { id: 1 },]
            }
        );
    }

    #[test]
    fn test_skip_unwanted() {
        let data = b"color = rgb { 10 20 30 }\r\nc = d\r\ne = f";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            c: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                c: String::from("d"),
            }
        );
    }

    #[test]
    fn test_skip_unwanted2() {
        let data = b"a={ \"hello\" \"goodbye\" } \r\nc = d\r\ne = f";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            c: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                c: String::from("d"),
            }
        );
    }

    #[test]
    fn test_consecutive_fields() {
        let data = b"a = b\r\nc = d1\r\nc = d2\r\ne = f";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            a: String,
            #[jomini(duplicated)]
            c: Vec<String>,
            e: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                a: String::from("b"),
                c: vec![String::from("d1"), String::from("d2")],
                e: String::from("f"),
            }
        );
    }

    #[test]
    fn test_non_consecutive_fields() {
        let data = b"c = d1\r\na = b\r\nc = d2\r\ne = f";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            a: String,
            #[jomini(duplicated)]
            c: Vec<String>,
            e: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                a: String::from("b"),
                c: vec![String::from("d1"), String::from("d2")],
                e: String::from("f"),
            }
        );
    }

    #[test]
    fn test_empty_consecutive_fields() {
        let data = b"data = { }";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            data: MyData,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyData {
            #[jomini(duplicated)]
            c: Vec<String>,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                data: MyData { c: Vec::new() }
            }
        );
    }

    #[test]
    fn test_optional_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: Option<String>,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                field1: Some(String::from("ENG")),
            }
        );
    }

    #[test]
    fn test_deserialize_hashmap() {
        let data = b"-1=a\r\n-2=b";

        let actual: HashMap<i32, String> = from_owned(&data[..]);
        let mut expected = HashMap::new();
        expected.insert(-1, String::from("a"));
        expected.insert(-2, String::from("b"));
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_deserialize_nested_hashmap() {
        let data = b"provinces={ -1={ name=\"abc\" } }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            provinces: HashMap<i32, Province>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Province {
            name: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        let mut expected = HashMap::new();
        expected.insert(
            -1,
            Province {
                name: String::from("abc"),
            },
        );
        assert_eq!(
            actual,
            MyStruct {
                provinces: expected
            }
        );
    }

    #[test]
    fn test_empty_objects() {
        let data = b"a={foo={bar=val} {} { } me=you}";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            a: MyNest,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyNest {
            foo: MyFoo,
            me: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyFoo {
            bar: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                a: MyNest {
                    foo: MyFoo {
                        bar: String::from("val"),
                    },
                    me: String::from("you"),
                }
            }
        );
    }

    #[test]
    fn test_empty_objects2() {
        let data = b"foo={bar=val {}} me=you";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            foo: MyFoo,
            me: String,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyFoo {
            bar: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                foo: MyFoo {
                    bar: String::from("val"),
                },
                me: String::from("you"),
            }
        );
    }

    #[test]
    fn test_spanning_objects() {
        let data = b"army={name=abc} army={name=def}";

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct MyStruct {
            #[jomini(duplicated)]
            army: Vec<Army>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Army {
            name: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                army: vec![
                    Army {
                        name: String::from("abc")
                    },
                    Army {
                        name: String::from("def")
                    },
                ]
            }
        );
    }

    #[rstest]
    #[case(b"val < 3 a = b")]
    #[case(b"val <= 3 a = b")]
    #[case(b"val > 3 a = b")]
    #[case(b"val >= 3 a = b")]
    #[case(b"val == 3 a = b")]
    #[case(b"val != 3 a = b")]
    #[case(b"val ?= 3 a = b")]
    fn test_deserialize_ignore_operator(#[case] data: &[u8]) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            val: i32,
            a: String,
        }

        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                val: 3,
                a: String::from("b"),
            }
        );
    }

    #[test]
    fn test_deserialize_enum() {
        let data = b"color = rgb { 10 11 12 }";

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: MyColor,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        enum MyColor {
            #[serde(rename = "rgb")]
            Rgb(u8, u8, u8),
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                color: MyColor::Rgb(10, 11, 12),
            },
        );
    }

    #[test]
    fn test_deserialize_enum_scalar() {
        let data = b"kind = infantry";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                kind: UnitKind::Infantry
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            kind: UnitKind,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        #[serde(rename_all = "camelCase")]
        enum UnitKind {
            Infantry,
            Cavalry,
        }
    }

    #[test]
    fn test_deserialize_mixed_object_array() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(actual.brittany_area, vec![169, 170, 171, 172, 4384]);

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            brittany_area: Vec<u16>,
        }
    }

    #[test]
    fn test_deserialize_mixed_object_into_object() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }
        another_area = { 100 }"#;

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual.brittany_area,
            MyArea {
                color: vec![118, 99, 151],
                remainder: vec![169, 170, 171, 172, 4384],
            }
        );

        assert_eq!(
            actual.another_area,
            MyArea {
                color: Vec::new(),
                remainder: vec![100],
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            brittany_area: MyArea,
            another_area: MyArea,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyArea {
            #[serde(default)]
            color: Vec<u8>,
            remainder: Vec<u16>,
        }
    }

    #[test]
    fn test_deserialize_color() {
        let data = b"color = rgb { 100 200 150 } color2 = hsv { 0.3 0.2 0.8 }";

        #[derive(Debug, PartialEq)]
        enum MyColor {
            Rgb {
                red: u8,
                green: u8,
                blue: u8,
            },
            Hsv {
                hue: f32,
                saturation: f32,
                value: f32,
            },
        }

        #[derive(Debug, PartialEq, Deserialize)]
        #[serde(rename_all = "lowercase")]
        enum ColorHeader {
            Rgb,
            Hsv,
        }

        let actual: MyStruct = from_owned(&data[..]);

        assert_eq!(
            actual,
            MyStruct {
                color: MyColor::Rgb {
                    red: 100,
                    green: 200,
                    blue: 150
                },
                color2: MyColor::Hsv {
                    hue: 0.3,
                    saturation: 0.2,
                    value: 0.8,
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            color: MyColor,
            color2: MyColor,
        }

        impl<'de> Deserialize<'de> for MyColor {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: de::Deserializer<'de>,
            {
                struct ColorVisitor;

                impl<'de> de::Visitor<'de> for ColorVisitor {
                    type Value = MyColor;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("a color sequence")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::SeqAccess<'de>,
                    {
                        let ty = seq.next_element::<ColorHeader>()?.expect("color type");
                        match ty {
                            ColorHeader::Rgb => {
                                let (red, green, blue) =
                                    seq.next_element::<(u8, u8, u8)>()?.unwrap();
                                Ok(MyColor::Rgb { red, green, blue })
                            }
                            ColorHeader::Hsv => {
                                let (hue, saturation, value) = seq
                                    .next_element::<(f32, f32, f32)>()?
                                    .expect("hsv channels");
                                Ok(MyColor::Hsv {
                                    hue,
                                    saturation,
                                    value,
                                })
                            }
                        }
                    }
                }

                deserializer.deserialize_seq(ColorVisitor)
            }
        }
    }

    pub(crate) fn deserialize_vec_pair<'de, D, K, V>(
        deserializer: D,
    ) -> Result<Vec<(K, V)>, D::Error>
    where
        D: Deserializer<'de>,
        K: Deserialize<'de>,
        V: Deserialize<'de>,
    {
        struct VecPairVisitor<K1, V1> {
            marker: PhantomData<Vec<(K1, V1)>>,
        }

        impl<'de, K1, V1> de::Visitor<'de> for VecPairVisitor<K1, V1>
        where
            K1: Deserialize<'de>,
            V1: Deserialize<'de>,
        {
            type Value = Vec<(K1, V1)>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map containing key value tuples")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut values = if let Some(size) = map.size_hint() {
                    Vec::with_capacity(size)
                } else {
                    Vec::new()
                };

                while let Some((key, value)) = map.next_entry()? {
                    values.push((key, value));
                }

                Ok(values)
            }
        }

        deserializer.deserialize_map(VecPairVisitor {
            marker: PhantomData,
        })
    }

    #[rstest]
    #[case(b"active_idea_groups = { a = 10 }", vec![(String::from("a"), 10)])]
    #[case(b"active_idea_groups = { }", vec![])]
    #[case(b"active_idea_groups = { ]=0 defensive_ideas=2 }", vec![(String::from("]"), 0), (String::from("defensive_ideas"), 2)])]
    #[test]
    fn test_deserialize_vec_pair(#[case] input: &[u8], #[case] expected: Vec<(String, u8)>) {
        let actual: MyStruct = from_owned(input);
        assert_eq!(
            actual,
            MyStruct {
                active_idea_groups: expected
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            #[serde(default, deserialize_with = "deserialize_vec_pair")]
            active_idea_groups: Vec<(String, u8)>,
        }
    }

    #[test]
    fn test_deserialize_date_string() {
        let data = b"date=\"1444.11.11\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: Date::from_ymd(1444, 11, 11)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: Date,
        }
    }

    #[test]
    fn test_deserialize_datehour_string() {
        let data = b"date=\"1936.1.1.24\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: DateHour::from_ymdh(1936, 1, 1, 24)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: DateHour,
        }
    }

    #[test]
    fn test_deserialize_uniform_date() {
        let data = b"date=\"2200.2.30\"";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                date: UniformDate::from_ymd(2200, 2, 30),
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            date: UniformDate,
        }
    }

    #[test]
    fn test_deserialize_positive_num() {
        let data = b"pop_happiness = +0.10";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual, MyStruct { pop_happiness: 0.1 });

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            pop_happiness: f64,
        }
    }

    #[test]
    fn test_deserialize_operator() {
        let data = b"num_cities < 0.10";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                num_cities: Property::new(Operator::LessThan, 0.1)
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            num_cities: Property<f64>,
        }
    }

    #[test]
    fn test_deserialize_operator2() {
        let data = b"modifier = { factor = 2 num_communications > 2 }";

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                modifier: Modifier {
                    factor: 2,
                    num_communications: Property::new(Operator::GreaterThan, 2)
                }
            }
        );

        #[derive(Deserialize, Debug, PartialEq)]
        struct Modifier {
            factor: i32,
            num_communications: Property<u8>,
        }

        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            modifier: Modifier,
        }
    }

    #[test]
    fn test_deserialize_i32_hint() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            field1: MaybeI32,
            field2: MaybeI32,
        }

        #[derive(Debug, PartialEq)]
        enum MaybeI32 {
            Val(i32),
            Str(String),
        }

        struct MaybeI32Visitor;
        impl de::Visitor<'_> for MaybeI32Visitor {
            type Value = MaybeI32;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("test case")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(MaybeI32::Val(v as i32))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(MaybeI32::Str(String::from(v)))
            }
        }

        impl<'de> Deserialize<'de> for MaybeI32 {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                deserializer.deserialize_i32(MaybeI32Visitor)
            }
        }

        let data = br#"field1=1 field2=invalid"#;
        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                field1: MaybeI32::Val(1),
                field2: MaybeI32::Str(String::from("invalid")),
            }
        );
    }

    #[test]
    fn test_deserialize_untagged() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct MyStruct {
            dynasty_house: HashMap<String, MaybeObject<DynastyHouse>>,
        }

        #[derive(Debug, Clone, PartialEq)]
        enum MaybeObject<T> {
            Text(String),
            Object(T),
        }

        impl<'de, T> Deserialize<'de> for MaybeObject<T>
        where
            T: Deserialize<'de>,
        {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                struct MaybeObjectVisitor<T1> {
                    marker: PhantomData<T1>,
                }

                impl<'de, T1> de::Visitor<'de> for MaybeObjectVisitor<T1>
                where
                    T1: Deserialize<'de>,
                {
                    type Value = MaybeObject<T1>;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("an object or string")
                    }

                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(MaybeObject::Text(String::from(v)))
                    }

                    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
                    where
                        A: de::MapAccess<'de>,
                    {
                        let mvd = de::value::MapAccessDeserializer::new(map);
                        let result = T1::deserialize(mvd)?;
                        Ok(MaybeObject::Object(result))
                    }
                }

                deserializer.deserialize_map(MaybeObjectVisitor {
                    marker: PhantomData,
                })
            }
        }

        #[derive(Debug, Deserialize, Clone, PartialEq)]
        struct DynastyHouse {
            name: String,
            dynasty: u64,
        }

        let data = br#"dynasty_house={1=none 2={name="dynn" dynasty=1}}"#;
        let expected = HashMap::from([
            (String::from("1"), MaybeObject::Text(String::from("none"))),
            (
                String::from("2"),
                MaybeObject::Object(DynastyHouse {
                    name: String::from("dynn"),
                    dynasty: 1,
                }),
            ),
        ]);

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(
            actual,
            MyStruct {
                dynasty_house: expected
            }
        );
    }

    // https://github.com/rakaly/jomini/issues/137
    #[test]
    fn test_double_next_value_no_panic() {
        #[derive(Deserialize, Debug)]
        struct Color(#[allow(unused)] (u8, u8, u8));

        #[derive(Deserialize, Debug)]
        enum ColorName {
            Red,
            Green,
            Blue,
        }

        struct Container;

        impl<'de> Deserialize<'de> for Container {
            fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
                struct TVisitor;
                impl<'de> serde::de::Visitor<'de> for TVisitor {
                    type Value = Container;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("{r g b} or name")
                    }

                    fn visit_map<A: serde::de::MapAccess<'de>>(
                        self,
                        mut map: A,
                    ) -> Result<Self::Value, A::Error> {
                        while map.next_key::<String>()?.is_some() {
                            if map.next_value::<Color>().is_err() {
                                let _ = map.next_value::<ColorName>()?;
                            }
                        }
                        Ok(Container)
                    }
                }
                deserializer.deserialize_map(TVisitor)
            }
        }

        let data = r#"
        color1 = Red
        color2 = { 255 0 0 }
        "#;
        let _: Container = from_slice(data.as_bytes()).unwrap();
    }

    #[test]
    fn test_object_template_deserialize_basic() {
        let data = br"obj={ { a = b }={ 1 2 3 } }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct ObjectKey {
            a: String,
        }

        // Test with tuple deserialization - treats array elements as tuple fields
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (ObjectKey, Vec<i32>),
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (ObjectKey { a: "b".to_string() }, vec![1, 2, 3])
            }
        );
    }

    #[test]
    fn test_object_template_deserialize_array_key() {
        let data = br"obj={ { foo }={ 1000 } }";

        // Test with tuple deserialization
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (Vec<String>, Vec<i32>),
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (vec!["foo".to_string()], vec![1000])
            }
        );
    }

    #[test]
    fn test_object_template_deserialize_scalar_value() {
        let data = br"obj={ { a = b }=16 }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct ObjectKey {
            a: String,
        }

        // Test with tuple deserialization
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: (ObjectKey, i32),
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: (ObjectKey { a: "b".to_string() }, 16)
            }
        );
    }

    #[test]
    fn test_object_template_deserialize_multiple_entries() {
        let data = br"obj={ { id = 31 type = admin }=16 { id = 32 type = diplo }=18 }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct TemplateKey {
            id: i32,
            #[serde(rename = "type")]
            key_type: String,
        }

        #[derive(PartialEq, Debug)]
        struct ObjectTemplateList(Vec<(TemplateKey, i32)>);

        impl<'de> Deserialize<'de> for ObjectTemplateList {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                use serde::de::{SeqAccess, Visitor};

                struct ObjectTemplateVisitor;

                impl<'de> Visitor<'de> for ObjectTemplateVisitor {
                    type Value = ObjectTemplateList;

                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("a sequence of alternating objects and values")
                    }

                    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                    where
                        A: SeqAccess<'de>,
                    {
                        let mut entries = Vec::new();

                        while let Some(key) = seq.next_element::<TemplateKey>()? {
                            if let Some(value) = seq.next_element::<i32>()? {
                                entries.push((key, value));
                            } else {
                                return Err(serde::de::Error::custom("Expected value after key"));
                            }
                        }

                        Ok(ObjectTemplateList(entries))
                    }
                }

                deserializer.deserialize_seq(ObjectTemplateVisitor)
            }
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: ObjectTemplateList,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
        assert_eq!(
            actual,
            MyStruct {
                obj: ObjectTemplateList(vec![
                    (
                        TemplateKey {
                            id: 31,
                            key_type: "admin".to_string()
                        },
                        16
                    ),
                    (
                        TemplateKey {
                            id: 32,
                            key_type: "diplo".to_string()
                        },
                        18
                    ),
                ])
            }
        );
    }

    #[test]
    fn test_deserialize_bytes() {
        #[derive(PartialEq, Debug)]
        struct ByteField(Vec<u8>);

        impl<'de> Deserialize<'de> for ByteField {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct ByteVisitor;

                impl<'de> Visitor<'de> for ByteVisitor {
                    type Value = ByteField;

                    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                        formatter.write_str("bytes")
                    }

                    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v.to_vec()))
                    }

                    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
                    where
                        E: de::Error,
                    {
                        Ok(ByteField(v))
                    }
                }

                deserializer.deserialize_bytes(ByteVisitor)
            }
        }

        // Test data with escape sequences that would normally be processed
        let data = br#"raw_field="Joe \"Captain\" Rogers " string_field="Joe \"Captain\" Rogers ""#;

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            raw_field: ByteField,
            string_field: String,
        }

        let actual: MyStruct = from_owned(&data[..]);
        assert_eq!(actual.raw_field.0, b"Joe \\\"Captain\\\" Rogers ");
        assert_eq!(actual.string_field, "Joe \"Captain\" Rogers");
    }

    const EMPTY_KEY_FIRST: &[u8] =
        b"cooldowns={\n\t=1825.1.11\n\ttrade_favors_for_men=1826.6.22\n}";
    const EMPTY_KEY_LAST: &[u8] = b"cooldowns={\n\ttrade_favors_for_men=1826.6.22\n\t=1825.1.11\n}";
    const EMPTY_KEY_ONLY: &[u8] = b"cooldowns={\n\t=1825.6.28\n}";
    const EMPTY_KEY_EXACT: &[u8] =
        b"idea_dates={\n\t==-5000.1.1\n\tnoble_knights=1461.11.17\n\tmilitary_traditions=1473.5.13\n}";

    #[rstest]
    #[case(EMPTY_KEY_FIRST, Some("1826.6.22"))]
    #[case(EMPTY_KEY_LAST, Some("1826.6.22"))]
    #[case(EMPTY_KEY_ONLY, None)]
    fn test_empty_key_struct(#[case] data: &[u8], #[case] expected: Option<&str>) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            cooldowns: Cooldowns,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            trade_favors_for_men: Option<String>,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct JominiStruct {
            cooldowns: JominiCooldowns,
        }

        #[derive(JominiDeserialize, PartialEq, Debug)]
        struct JominiCooldowns {
            trade_favors_for_men: Option<String>,
        }

        let expected = expected.map(String::from);
        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                cooldowns: Cooldowns {
                    trade_favors_for_men: expected.clone()
                }
            }
        );

        let actual: JominiStruct = from_owned(data);
        assert_eq!(
            actual,
            JominiStruct {
                cooldowns: JominiCooldowns {
                    trade_favors_for_men: expected
                }
            }
        );
    }

    #[rstest]
    #[case(EMPTY_KEY_FIRST, &[("", "1825.1.11"), ("trade_favors_for_men", "1826.6.22")])]
    #[case(EMPTY_KEY_LAST, &[("trade_favors_for_men", "1826.6.22"), ("", "1825.1.11")])]
    #[case(EMPTY_KEY_ONLY, &[("", "1825.6.28")])]
    fn test_empty_key_map(#[case] data: &[u8], #[case] expected: &[(&str, &str)]) {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyMap {
            cooldowns: HashMap<String, String>,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyPairs {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            cooldowns: Vec<(String, String)>,
        }

        let expected: Vec<(String, String)> = expected
            .iter()
            .map(|(k, v)| (String::from(*k), String::from(*v)))
            .collect();

        let actual: MyMap = from_owned(data);
        assert_eq!(
            actual,
            MyMap {
                cooldowns: expected.iter().cloned().collect()
            }
        );

        let actual: MyPairs = from_owned(data);
        assert_eq!(
            actual,
            MyPairs {
                cooldowns: expected
            }
        );
    }

    #[test]
    fn test_empty_key_property() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            idea_dates: Vec<(String, Property<String>)>,
        }

        let actual: MyStruct = from_owned(EMPTY_KEY_EXACT);
        assert_eq!(
            actual,
            MyStruct {
                idea_dates: vec![
                    (
                        String::from(""),
                        Property::new(Operator::Exact, String::from("-5000.1.1"))
                    ),
                    (
                        String::from("noble_knights"),
                        Property::new(Operator::Equal, String::from("1461.11.17"))
                    ),
                    (
                        String::from("military_traditions"),
                        Property::new(Operator::Equal, String::from("1473.5.13"))
                    ),
                ]
            }
        );

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            cooldowns: Vec<(String, Property<String>)>,
        }

        let actual: Cooldowns = from_owned(EMPTY_KEY_LAST);
        assert_eq!(
            actual,
            Cooldowns {
                cooldowns: vec![
                    (
                        String::from("trade_favors_for_men"),
                        Property::new(Operator::Equal, String::from("1826.6.22"))
                    ),
                    (
                        String::from(""),
                        Property::new(Operator::Equal, String::from("1825.1.11"))
                    ),
                ]
            }
        );
    }

    #[derive(Debug, PartialEq)]
    enum AnyValue {
        Str(String),
        Seq(Vec<AnyValue>),
        Map(Vec<(String, AnyValue)>),
    }

    struct AnyVisitor;

    impl<'de> de::Visitor<'de> for AnyVisitor {
        type Value = AnyValue;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("any value")
        }

        fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
            Ok(AnyValue::Str(String::from(v)))
        }

        fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            let mut values = Vec::new();
            while let Some(value) = seq.next_element()? {
                values.push(value);
            }
            Ok(AnyValue::Seq(values))
        }

        fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
            let mut entries = Vec::new();
            while let Some(entry) = map.next_entry()? {
                entries.push(entry);
            }
            Ok(AnyValue::Map(entries))
        }
    }

    impl<'de> Deserialize<'de> for AnyValue {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            deserializer.deserialize_any(AnyVisitor)
        }
    }

    /// Writes `{key=value}` for maps and `[a b]` for sequences.
    impl fmt::Display for AnyValue {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                AnyValue::Str(x) => f.write_str(x),
                AnyValue::Seq(values) => {
                    f.write_str("[")?;
                    for (i, value) in values.iter().enumerate() {
                        let sep = if i == 0 { "" } else { " " };
                        write!(f, "{sep}{value}")?;
                    }
                    f.write_str("]")
                }
                AnyValue::Map(entries) => {
                    f.write_str("{")?;
                    for (i, (key, value)) in entries.iter().enumerate() {
                        let sep = if i == 0 { "" } else { " " };
                        write!(f, "{sep}{key}={value}")?;
                    }
                    f.write_str("}")
                }
            }
        }
    }

    #[test]
    fn test_empty_key_deserialize_any() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            #[serde(deserialize_with = "deserialize_vec_pair")]
            idea_dates: Vec<(AnyValue, AnyValue)>,
        }

        let actual: MyStruct = from_owned(EMPTY_KEY_EXACT);
        assert_eq!(
            actual,
            MyStruct {
                idea_dates: vec![
                    (
                        AnyValue::Str(String::from("")),
                        AnyValue::Str(String::from("-5000.1.1"))
                    ),
                    (
                        AnyValue::Str(String::from("noble_knights")),
                        AnyValue::Str(String::from("1461.11.17"))
                    ),
                    (
                        AnyValue::Str(String::from("military_traditions")),
                        AnyValue::Str(String::from("1473.5.13"))
                    ),
                ]
            }
        );

        #[derive(Deserialize, PartialEq, Debug)]
        struct Cooldowns {
            cooldowns: AnyValue,
        }

        let actual: Cooldowns = from_owned(EMPTY_KEY_FIRST);
        assert_eq!(
            actual,
            Cooldowns {
                cooldowns: AnyValue::Map(vec![
                    (String::from(""), AnyValue::Str(String::from("1825.1.11"))),
                    (
                        String::from("trade_favors_for_men"),
                        AnyValue::Str(String::from("1826.6.22"))
                    ),
                ])
            }
        );
    }

    /// The root through `deserialize_map` on a tape, which has no
    /// `deserialize_any` for the root.
    struct TapeRoot(AnyValue);

    impl<'de> Deserialize<'de> for TapeRoot {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            deserializer.deserialize_map(AnyVisitor).map(TapeRoot)
        }
    }

    fn reader_any(reader: TokenReader) -> String {
        let mut deserializer = TextDeserializer::from_windows1252_reader(reader);
        AnyValue::deserialize(&mut deserializer)
            .unwrap()
            .to_string()
    }

    /// `deserialize_any` through the streaming reader over a slice, and over
    /// buffers small enough that refills land inside the lookahead and inside
    /// blocks whose first token was replayed. `tape` is whether a tape gives
    /// the same value; it rejects fields after trailing scalars and
    /// operators in arrays.
    #[rstest]
    #[case::block_kind(
        b"object={ a=b c<2 } array={ 1 2 } empty={} objects={ { a=b } { c=d } } leading_empty={ {} {} 3 } empty_key={ =b }",
        "{object={a=b c=2} array=[1 2] empty=[] objects=[{a=b} {c=d}] leading_empty=[3] empty_key={=b}}",
        true
    )]
    // A `key { }` field is a field after the first, but as the first item a
    // scalar followed by a block makes the block an array.
    #[case::key_block_field(
        b"later={ a=b c { d=e } } first={ c { d=e } }",
        "{later={a=b c={d=e}} first=[c {d=e}]}",
        true
    )]
    // A header is only recognized for a field value.
    #[case::header(
        b"color = rgb { 1 2 3 } object = obj { a = b } empty = x {} list = { rgb { 1 } }",
        "{color=[1 2 3] object={a=b} empty=x list=[rgb [1]]}",
        true
    )]
    #[case::comment(
        b"object={ a # note\n = b }\ncolor = rgb # note\n { 1 }",
        "{object={a=b} color=[1]}",
        true
    )]
    #[case::exact_operator(b"object={ a == b c ==d }", "{object={a=b c=d}}", true)]
    #[case::remainder(b"object={ a=b c d }", "{object={a=b remainder=[c d]}}", true)]
    #[case::field_after_remainder(
        b"object={ a=b c d=e }",
        "{object={a=b remainder=[c d e]}}",
        false
    )]
    #[case::operator_in_array(b"array={ a b=c }", "{array=[a b = c]}", false)]
    #[case::root(b"a=b }\nc={ d=e } f=rgb { 1 }", "{a=b c={d=e} f=[1]}", true)]
    #[case::root_remainder(b"a=b c d", "{a=b remainder=[c d]}", false)]
    #[case::empty(b"", "{}", true)]
    #[case::refill_in_lookahead(
        b"first={ a_long_key\n\n\t\t= value }\nsecond={ alpha # comment\n  beta }\ncolor = a_header_scalar  \n\n  { 1 2 }\nlast=z",
        "{first={a_long_key=value} second=[alpha beta] color=[1 2] last=z}",
        true
    )]
    #[case::refill_after_replay(
        b"list={ alpha beta gamma delta epsilon zeta eta theta }\nobj={ k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6 }\nrem={ a=b c d e f g h i j k l }\ncolor=hdr { 1 2 3 4 5 6 7 8 9 10 11 12 }",
        "{list=[alpha beta gamma delta epsilon zeta eta theta] obj={k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6} rem={a=b remainder=[c d e f g h i j k l]} color=[1 2 3 4 5 6 7 8 9 10 11 12]}",
        true
    )]
    fn test_any(#[case] data: &[u8], #[case] expected: &str, #[case] tape: bool) {
        assert_eq!(reader_any(TokenReader::from_slice(data)), expected);
        for size in 20..=40 {
            let reader = TokenReader::from_reader_with_buf(data, vec![0; size]);
            assert_eq!(reader_any(reader), expected, "buffer size {size}");
        }

        let tape_value = from_slice::<TapeRoot>(data).map(|x| x.0.to_string());
        assert_eq!(tape_value.ok().as_deref() == Some(expected), tape);
    }

    #[cfg(feature = "json")]
    #[test]
    fn test_any_json_value() {
        let data = b"a={ b=1 c={ 1 2 } } d=rgb { 3 4 } e={}";
        let actual: serde_json::Value = from_windows1252_reader(&data[..]).unwrap();
        assert_eq!(
            actual,
            serde_json::json!({
                "a": { "b": "1", "c": ["1", "2"] },
                "d": ["3", "4"],
                "e": [],
            })
        );
    }

    #[test]
    fn test_empty_key_sequence() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            cooldowns: Vec<String>,
        }

        // The tape gives the key and the value of each field.
        let actual: MyStruct = from_slice(EMPTY_KEY_FIRST).unwrap();
        assert_eq!(
            actual.cooldowns,
            vec!["", "1825.1.11", "trade_favors_for_men", "1826.6.22"]
        );

        // The streaming reader gives each token of the block, operators too.
        let actual: MyStruct = from_windows1252_reader(EMPTY_KEY_FIRST).unwrap();
        assert_eq!(
            actual.cooldowns,
            vec!["=", "1825.1.11", "trade_favors_for_men", "=", "1826.6.22"]
        );
    }

    #[test]
    fn test_operator_on_next_line_after_key() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            name: String,
            #[serde(deserialize_with = "deserialize_vec_pair")]
            block: Vec<(String, String)>,
        }

        let data = b"name\n= \"x\"\nblock={\n\ta = b\n\t= c\n}";
        let actual: MyStruct = from_owned(data);
        assert_eq!(
            actual,
            MyStruct {
                name: String::from("x"),
                block: vec![
                    (String::from("a"), String::from("b")),
                    (String::from(""), String::from("c")),
                ]
            }
        );
    }

    #[test]
    fn test_reader_skips_field_with_block_key() {
        let data = b"obj={ { a = b }={ 1 2 3 } { c = d }=16 e=f }";

        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            obj: HashMap<String, String>,
        }

        let actual: MyStruct = from_windows1252_reader(&data[..]).unwrap();
        let expected = HashMap::from([(String::from("e"), String::from("f"))]);
        assert_eq!(actual, MyStruct { obj: expected });
    }
    #[test]
    fn test_header_seq_streaming_refill() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            color: (String, Vec<String>),
            next: String,
        }

        let data = b"color = rgb { 10 20 30 40 50 60 70 80 90 }\nnext = x";
        for size in 16..=24 {
            let reader = TokenReader::from_reader_with_buf(&data[..], vec![0; size]);
            let actual: MyStruct = TextDeserializer::from_windows1252_reader(reader)
                .deserialize()
                .unwrap();
            let numbers = ["10", "20", "30", "40", "50", "60", "70", "80", "90"];
            assert_eq!(
                actual,
                MyStruct {
                    color: (
                        String::from("rgb"),
                        numbers.iter().map(|x| String::from(*x)).collect()
                    ),
                    next: String::from("x"),
                },
                "buffer size {size}"
            );
        }
    }
}
