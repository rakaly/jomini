use crate::{
    DeserializeError, DeserializeErrorKind, Encoding, Operator, Scalar, TextTape, TextToken,
};
use std::borrow::Cow;

pub type KeyValue<'data, 'tokens, E> = (
    ScalarReader<'data, E>,
    Option<Operator>,
    ValueReader<'data, 'tokens, E>,
);

pub type KeyValues<'data, 'tokens, E> = (
    ScalarReader<'data, E>,
    Vec<(Option<Operator>, ValueReader<'data, 'tokens, E>)>,
);

/// Calculate what index the next value is. This assumes that a header + value
/// are two separate values
#[inline]
fn next_idx_header(tokens: &[TextToken], idx: usize) -> usize {
    match tokens[idx] {
        TextToken::Array(x) | TextToken::Object(x) | TextToken::HiddenObject(x) => x + 1,
        TextToken::Operator(_) => idx + 2,
        _ => idx + 1,
    }
}

/// Calculate what index the next value is. This assumes that a header + value
/// is one value
#[inline]
fn next_idx(tokens: &[TextToken], idx: usize) -> usize {
    match tokens[idx] {
        TextToken::Array(x) | TextToken::Object(x) | TextToken::HiddenObject(x) => x + 1,
        TextToken::Operator(_) => next_idx(tokens, idx + 1),
        TextToken::Header(_) => next_idx_header(tokens, idx + 1),
        _ => idx + 1,
    }
}

/// All possible text reader variants
#[derive(Debug, Clone)]
pub enum Reader<'data, 'tokens, E> {
    /// object reader
    Object(ObjectReader<'data, 'tokens, E>),

    /// array reader
    Array(ArrayReader<'data, 'tokens, E>),

    /// scalar reader
    Scalar(ScalarReader<'data, E>),

    /// value reader
    Value(ValueReader<'data, 'tokens, E>),
}

impl<'data, 'tokens, E> Reader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Interpret value as a string
    #[inline]
    pub fn read_str(&self) -> Result<Cow<'data, str>, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_str()),
            Reader::Value(x) => x.read_str(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }

    /// Interpret value as a string
    #[inline]
    pub fn read_string(&self) -> Result<String, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_string()),
            Reader::Value(x) => x.read_string(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }

    /// Interpret value as a scalar
    #[inline]
    pub fn read_scalar(&self) -> Result<Scalar<'data>, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_scalar()),
            Reader::Value(x) => x.read_scalar(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }
}

/// A reader that will advance through an object
#[derive(Debug, Clone)]
pub struct ObjectReader<'data, 'tokens, E> {
    token_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
    val_ind: usize,
    seen: Vec<bool>,
}

impl<'data, 'tokens, E> ObjectReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Create a new object reader from parsed data with encoded strings
    pub fn new(tape: &'tokens TextTape<'data>, encoding: E) -> Self {
        let tokens = tape.tokens();
        ObjectReader {
            tokens,
            end_ind: tokens.len(),
            token_ind: 0,
            val_ind: 0,
            encoding,
            seen: Vec::new(),
        }
    }

    /// Return the number of key value pairs that the object contains
    pub fn fields_len(&self) -> usize {
        let mut ind = self.token_ind;
        let mut count = 0;
        while ind < self.end_ind {
            let key_ind = ind;
            let value_ind = match self.tokens[key_ind + 1] {
                TextToken::Operator(_) => key_ind + 2,
                _ => key_ind + 1,
            };
            ind = next_idx(self.tokens, value_ind);
            count += 1;
        }

        count
    }

    /// Advance the reader and return the next field
    #[inline]
    pub fn next_field(&mut self) -> Option<KeyValue<'data, 'tokens, E>> {
        if self.token_ind < self.end_ind {
            let key_ind = self.token_ind;
            let key_scalar = match self.tokens[key_ind] {
                TextToken::Quoted(x) | TextToken::Unquoted(x) => x,
                _ => {
                    // this is a broken invariant, so we safely recover by saying the object
                    // has no more fields
                    debug_assert!(false, "All keys should be scalars");
                    return None;
                }
            };

            let key_reader = self.new_scalar_reader(key_scalar);

            let (op, value_ind) = match self.tokens[key_ind + 1] {
                TextToken::Operator(x) => (Some(x), key_ind + 2),
                _ => (None, key_ind + 1),
            };

            // When reading an mixed object (a = { b = { c } 10 10 10 })
            // there is an uneven number of keys and values so we drop the last "field"
            if value_ind >= self.end_ind {
                return None;
            }

            let value_reader = self.new_value_reader(value_ind);
            self.token_ind = next_idx(self.tokens, value_ind);
            Some((key_reader, op, value_reader))
        } else {
            None
        }
    }

    /// Advance the reader and return all fields that share the same key in the object
    #[inline]
    pub fn next_fields(&mut self) -> Option<KeyValues<'data, 'tokens, E>> {
        if self.val_ind == 0 {
            self.seen = vec![false; self.fields_len()];
        }

        let mut values = Vec::new();
        while self.token_ind < self.end_ind {
            if !self.seen[self.val_ind] {
                let key_ind = self.token_ind;
                let key = &self.tokens[self.token_ind];
                self.seen[self.val_ind] = true;
                let key_scalar = match self.tokens[key_ind] {
                    TextToken::Quoted(x) | TextToken::Unquoted(x) => x,
                    _ => {
                        // this is a broken invariant, so we safely recover by saying the object
                        // has no more fields
                        debug_assert!(false, "All keys should be scalars");
                        return None;
                    }
                };

                let key_reader = self.new_scalar_reader(key_scalar);
                let (op, value_ind) = match self.tokens[key_ind + 1] {
                    TextToken::Operator(x) => (Some(x), key_ind + 2),
                    _ => (None, key_ind + 1),
                };

                // When reading an mixed object (a = { b = { c } 10 10 10 })
                // there is an uneven number of keys and values so we drop the last "field"
                if value_ind >= self.end_ind {
                    return None;
                }

                let value_reader = self.new_value_reader(value_ind);
                self.token_ind = next_idx(self.tokens, value_ind);
                values.push((op, value_reader));

                let mut future = self.token_ind;
                let mut future_ind = self.val_ind + 1;
                while future < self.end_ind {
                    if !self.seen[future_ind] && self.tokens[future] == *key {
                        let (op, value_ind) = match self.tokens[future + 1] {
                            TextToken::Operator(x) => (Some(x), future + 2),
                            _ => (None, future + 1),
                        };
                        self.seen[future_ind] = true;
                        if value_ind < self.end_ind {
                            let value_reader = self.new_value_reader(value_ind);
                            values.push((op, value_reader));
                        }
                    }
                    future_ind += 1;
                    future = next_idx(self.tokens, future + 1);
                }

                self.val_ind += 1;
                return Some((key_reader, values));
            } else {
                self.val_ind += 1;
                self.token_ind = next_idx(self.tokens, self.token_ind + 1);
            }
        }

        None
    }

    #[inline]
    fn new_scalar_reader(&self, scalar: Scalar<'data>) -> ScalarReader<'data, E> {
        ScalarReader {
            scalar,
            encoding: self.encoding.clone(),
        }
    }

    #[inline]
    fn new_value_reader(&self, value_ind: usize) -> ValueReader<'data, 'tokens, E> {
        ValueReader {
            value_ind,
            tokens: self.tokens,
            encoding: self.encoding.clone(),
        }
    }
}

/// A text reader that wraps an underlying scalar value
#[derive(Debug, Clone)]
pub struct ScalarReader<'data, E> {
    scalar: Scalar<'data>,
    encoding: E,
}

impl<'data, E> ScalarReader<'data, E>
where
    E: Encoding,
{
    /// Decode the data with a given string encoding
    #[inline]
    pub fn read_str(&self) -> Cow<'data, str> {
        self.encoding.decode(self.scalar.view_data())
    }

    /// Decode the data with a given string encoding
    #[inline]
    pub fn read_string(&self) -> String {
        self.encoding.decode(self.scalar.view_data()).into_owned()
    }

    /// Return the underlying scalar
    #[inline]
    pub fn read_scalar(&self) -> Scalar<'data> {
        self.scalar
    }
}

/// A text reader for a text value
#[derive(Debug, Clone)]
pub struct ValueReader<'data, 'tokens, E> {
    value_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ValueReader<'data, 'tokens, E> {
    /// Return the token that the reader is abstracting
    #[inline]
    pub fn token(&self) -> &TextToken<'data> {
        &self.tokens[self.value_ind]
    }
}

impl<'data, 'tokens, E> Encoding for ValueReader<'data, 'tokens, E>
where
    E: Encoding,
{
    #[inline]
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        self.encoding.decode(data)
    }
}

impl<'data, 'tokens, E> ValueReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Interpret the current value as string
    #[inline]
    pub fn read_str(&self) -> Result<Cow<'data, str>, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .map(|x| self.encoding.decode(x.view_data()))
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as string
    #[inline]
    pub fn read_string(&self) -> Result<String, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .map(|x| self.encoding.decode(x.view_data()).into_owned())
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as a scalar
    #[inline]
    pub fn read_scalar(&self) -> Result<Scalar<'data>, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as an object
    #[inline]
    pub fn read_object(&self) -> Result<ObjectReader<'data, 'tokens, E>, DeserializeError> {
        match self.tokens[self.value_ind] {
            TextToken::Object(ind) | TextToken::HiddenObject(ind) => Ok(ObjectReader {
                tokens: self.tokens,
                token_ind: self.value_ind + 1,
                val_ind: 0,
                end_ind: ind,
                seen: Vec::new(),
                encoding: self.encoding.clone(),
            }),

            // An array can be an object if it is empty
            TextToken::Array(ind) if ind == self.value_ind + 1 => Ok(ObjectReader {
                tokens: self.tokens,
                token_ind: self.value_ind + 1,
                val_ind: 0,
                end_ind: ind,
                encoding: self.encoding.clone(),
                seen: Vec::new(),
            }),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not an object")),
            }),
        }
    }

    /// Interpret the current value as an array
    #[inline]
    pub fn read_array(&self) -> Result<ArrayReader<'data, 'tokens, E>, DeserializeError> {
        match self.tokens[self.value_ind] {
            TextToken::Array(ind) => Ok(ArrayReader {
                tokens: self.tokens,
                token_ind: self.value_ind + 1,
                end_ind: ind,
                encoding: self.encoding.clone(),
            }),

            // An object can be considered an array of alternating keys and values
            TextToken::Object(ind) => Ok(ArrayReader {
                tokens: self.tokens,
                token_ind: self.value_ind + 1,
                end_ind: ind,
                encoding: self.encoding.clone(),
            }),

            // A header can be seen as a two element array
            TextToken::Header(_) => Ok(ArrayReader {
                tokens: self.tokens,
                token_ind: self.value_ind,
                end_ind: next_idx(self.tokens, self.value_ind + 1),
                encoding: self.encoding.clone(),
            }),

            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not an array")),
            }),
        }
    }
}

/// A text reader that advances through a sequence of values
#[derive(Debug, Clone)]
pub struct ArrayReader<'data, 'tokens, E> {
    token_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ArrayReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Return the number of values in the array
    #[inline]
    pub fn values_len(&self) -> usize {
        let mut count = 0;
        let mut ind = self.token_ind;
        while ind < self.end_ind {
            ind = next_idx_header(self.tokens, ind);
            count += 1;
        }

        count
    }

    /// Advance the array and return the next value
    #[inline]
    pub fn next_value(&mut self) -> Option<ValueReader<'data, 'tokens, E>> {
        if self.token_ind < self.end_ind {
            let value_ind = self.token_ind;
            self.token_ind = next_idx_header(self.tokens, self.token_ind);
            Some(ValueReader {
                value_ind,
                tokens: self.tokens,
                encoding: self.encoding.clone(),
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_value<E>(value: ValueReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        match value.token() {
            TextToken::Object(_) | TextToken::HiddenObject(_) => {
                iterate_object(value.read_object().unwrap());
            }
            TextToken::Array(_) => {
                iterate_array(value.read_array().unwrap());
            }
            TextToken::End(_) => panic!("end!?"),
            TextToken::Operator(_) => panic!("end!?"),
            TextToken::Unquoted(_) | TextToken::Quoted(_) | TextToken::Header(_) => {
                let _ = value.read_str().unwrap();
            }
        }
    }

    fn iterate_array<E>(mut reader: ArrayReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        while let Some(value) = reader.next_value() {
            read_value(value)
        }
    }

    fn iterate_object<E>(mut reader: ObjectReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        let mut fields_reader = reader.clone();
        while let Some((_key, entries)) = fields_reader.next_fields() {
            for (_op, value) in entries {
                read_value(value);
            }
        }

        while let Some((key, _op, value)) = reader.next_field() {
            let _ = key.read_str();
            read_value(value);
        }
    }

    #[test]
    fn simple_text_reader_text() {
        let data = b"foo=bar";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();
        assert_eq!(reader.fields_len(), 1);

        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));
        assert_eq!(value.read_string().unwrap(), String::from("bar"));

        assert!(reader.next_field().is_none());
    }

    #[test]
    fn simple_text_reader_obj() {
        let data = b"foo={bar=qux}";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();

        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));

        let mut nested = value.read_object().unwrap();
        let (key2, _op, value2) = nested.next_field().unwrap();
        assert_eq!(key2.read_string(), String::from("bar"));
        assert_eq!(value2.read_string().unwrap(), String::from("qux"));
        assert!(nested.next_field().is_none());
        assert!(reader.next_field().is_none());
    }

    #[test]
    fn simple_text_reader_array() {
        let data = b"foo={bar qux}";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();

        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));

        let mut nested = value.read_array().unwrap();
        assert_eq!(nested.values_len(), 2);
        let value1 = nested.next_value().unwrap().read_string().unwrap();
        let value2 = nested.next_value().unwrap().read_string().unwrap();

        assert!(nested.next_value().is_none());
        assert_eq!(value1, String::from("bar"));
        assert_eq!(value2, String::from("qux"));
    }

    #[test]
    fn text_reader_hidden_object() {
        let data = b"levels={10 0=1 0=2}";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();

        assert_eq!(reader.fields_len(), 1);
        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("levels"));

        let mut nested = value.read_array().unwrap();
        assert_eq!(nested.values_len(), 2);

        let value1 = nested.next_value().unwrap().read_string().unwrap();
        assert_eq!(value1, String::from("10"));

        let mut hidden = nested.next_value().unwrap().read_object().unwrap();
        assert_eq!(hidden.fields_len(), 2);
        let (key, _op, value) = hidden.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("0"));
        assert_eq!(value.read_string().unwrap(), String::from("1"));

        let (key, _op, value) = hidden.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("0"));
        assert_eq!(value.read_string().unwrap(), String::from("2"));

        assert!(hidden.next_field().is_none());
        assert!(nested.next_value().is_none());
    }

    #[test]
    fn text_reader_read_fields() {
        let data = b"name=aaa name=bbb core=123 core=456 name=ccc name=ddd";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();

        let (key, values) = reader.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(values.len(), 4);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("aaa"));
        assert_eq!(values[1].1.read_string().unwrap(), String::from("bbb"));
        assert_eq!(values[2].1.read_string().unwrap(), String::from("ccc"));
        assert_eq!(values[3].1.read_string().unwrap(), String::from("ddd"));

        let (key, values) = reader.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("core"));
        assert_eq!(values.len(), 2);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("123"));
        assert_eq!(values[1].1.read_string().unwrap(), String::from("456"));
    }

    #[test]
    fn text_reader_read_fields_nested() {
        let data =
            b"army={name=aaa unit={name=bbb} unit={name=ccc}} army={name=ddd unit={name=eee}}";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();

        let (key, army_values) = reader.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("army"));
        assert_eq!(army_values.len(), 2);

        let mut aaa = army_values[0].1.read_object().unwrap();
        assert_eq!(aaa.fields_len(), 3);

        let (key, values) = aaa.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(values.len(), 1);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("aaa"));

        let (key, values) = aaa.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("unit"));
        assert_eq!(values.len(), 2);

        let mut bbb = values[0].1.read_object().unwrap();
        let (key, _, value) = bbb.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("bbb"));

        let mut ccc = values[1].1.read_object().unwrap();
        let (key, _, value) = ccc.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("ccc"));

        let mut ddd = army_values[1].1.read_object().unwrap();
        assert_eq!(ddd.fields_len(), 2);

        let (key, values) = ddd.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(values.len(), 1);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("ddd"));

        let (key, values) = ddd.next_fields().unwrap();
        assert_eq!(key.read_string(), String::from("unit"));
        assert_eq!(values.len(), 1);

        let mut eee = values[0].1.read_object().unwrap();
        let (key, _, value) = eee.next_field().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("eee"));
    }

    #[test]
    fn text_reader_read_fields_consume() {
        let data = b"name=aaa name=bbb core=123 name=ccc name=ddd";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();
        let mut count = 0;
        while let Some((_key, mut entries)) = reader.next_fields() {
            for (_i, (_op, value)) in entries.drain(..).enumerate() {
                count += value.read_scalar().map(|_| 1).unwrap_or(0);
            }
        }

        assert_eq!(count, 5);
    }

    #[test]
    fn text_reader_mixed_object() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();
        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_str(), "brittany_area");

        let mut keys = vec![];
        let mut brittany = value.read_object().unwrap();
        while let Some((key, _op, _value)) = brittany.next_field() {
            keys.push(key.read_str());
        }

        assert_eq!(
            keys,
            vec![
                String::from("color"),
                String::from("169"),
                String::from("171")
            ]
        );
    }

    #[test]
    fn text_reader_mixed_object_array() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();
        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_str(), "brittany_area");

        let mut values = vec![];
        let mut brittany = value.read_array().unwrap();
        while let Some(value) = brittany.next_value() {
            let nv = value.token();
            values.push((*nv).clone());
        }

        assert_eq!(
            values,
            vec![
                TextToken::Unquoted(Scalar::new(b"color")),
                TextToken::Array(7),
                TextToken::Unquoted(Scalar::new(b"169")),
                TextToken::Unquoted(Scalar::new(b"170")),
                TextToken::Unquoted(Scalar::new(b"171")),
                TextToken::Unquoted(Scalar::new(b"172")),
                TextToken::Unquoted(Scalar::new(b"4384")),
            ]
        );
    }

    #[test]
    fn text_reader_header() {
        let data = b"color = rgb { 10 20 30 }";
        let tape = TextTape::from_slice(data).unwrap();
        let mut reader = tape.windows1252_reader();
        let (key, _op, value) = reader.next_field().unwrap();
        assert_eq!(key.read_str(), "color");

        let mut header_array = value.read_array().unwrap();
        let rgb = header_array.next_value().unwrap();
        assert_eq!(rgb.read_str().unwrap(), "rgb");

        let vals = header_array.next_value().unwrap();
        let mut s = vals.read_array().unwrap();

        let r = s
            .next_value()
            .unwrap()
            .read_scalar()
            .unwrap()
            .to_u64()
            .unwrap();
        let g = s
            .next_value()
            .unwrap()
            .read_scalar()
            .unwrap()
            .to_u64()
            .unwrap();
        let b = s
            .next_value()
            .unwrap()
            .read_scalar()
            .unwrap()
            .to_u64()
            .unwrap();

        assert_eq!(r, 10);
        assert_eq!(g, 20);
        assert_eq!(b, 30);
    }

    #[test]
    fn reader_crash1() {
        let data = b"a=r{}";
        let tape = TextTape::from_slice(data).unwrap();
        iterate_object(tape.windows1252_reader());
    }

    #[test]
    fn text_reader_object_fields() {
        let data = b"a{b=}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_op() {
        let data = b"a{c=d b>}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_op2() {
        let data = b"a{}b>{}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_dupe() {
        let data = b"a{b=c d=E d}";
        if let Ok(tape) = TextTape::from_slice(data) {
            dbg!(&tape);
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_header() {
        let data = b"a{}b>r{}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    // Investigate why this test case is so slow
    // #[test]
    // fn text_reader_oom() {
    //     let data = b"w{w={w={w={a={w={w={.=w={W={={w={w={a={w={w={.=w={W={w={w={b=}.{B{6={b={w={w={.b=}}}ws!=}}}}={=b}}=}}}}={w=}}}}}w={w={b=}.{B{6={b={w={w={.b=}}}ws!=}}}}={=b}}=}}}}={w=}}}}}";
    //     if let Ok(tape) = TextTape::from_slice(data) {
    //         dbg!(&tape);
    //         assert!(false);
    //         let reader = tape.windows1252_reader();
    //         iterate_object(reader);
    //     }
    // }
}
