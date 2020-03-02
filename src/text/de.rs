use crate::{TextError, TextErrorKind, TextTape, TextToken};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::borrow::Cow;

pub fn from_slice<'a, T>(data: &'a [u8]) -> Result<T, TextError>
where
    T: Deserialize<'a>,
{
    let tape = TextTape::from_slice(data)?;
    let mut seen = vec![0; tape.token_tape.len()];
    let mut deserializer = RootDeserializer {
        doc: &tape,
        seen: &mut seen,
    };
    T::deserialize(&mut deserializer)
}

struct RootDeserializer<'b, 'a: 'b> {
    doc: &'b TextTape<'a>,
    seen: &'b mut Vec<u8>,
}

impl<'b, 'de, 'r> de::Deserializer<'de> for &'r mut RootDeserializer<'b, 'de> {
    type Error = TextError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        Err(TextError {
            kind: TextErrorKind::Message(String::from(
                "root deserializer can only work with key value pairs",
            )),
        })
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(BinaryMap::new(
            self.doc,
            0,
            self.doc.token_tape.len(),
            self.seen,
        ))
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

struct BinaryMap<'a, 'de: 'a> {
    doc: &'a TextTape<'de>,
    seen: &'a mut Vec<u8>,
    tape_idx: usize,
    end_idx: usize,
    values: Vec<usize>,
}

impl<'a, 'de> BinaryMap<'a, 'de> {
    fn new(doc: &'a TextTape<'de>, tape_idx: usize, end_idx: usize, seen: &'a mut Vec<u8>) -> Self {
        BinaryMap {
            doc,
            tape_idx,
            end_idx,
            seen,
            values: Vec::new(),
        }
    }
}

impl<'de, 'a> MapAccess<'de> for BinaryMap<'a, 'de> {
    type Error = TextError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        self.values.clear();

        while self.tape_idx < self.end_idx {
            if self.seen[self.tape_idx] == 0 {
                let key = &self.doc.token_tape[self.tape_idx];
                self.seen[self.tape_idx] = 1;
                self.values.push(self.tape_idx + 1);

                let mut value_idx = self.tape_idx + 1;
                while value_idx < self.end_idx {
                    let add_for_key = match self.doc.token_tape[value_idx] {
                        TextToken::Array(x) => x,
                        TextToken::Object(x) => x,
                        _ => value_idx,
                    };

                    let next_key_idx = add_for_key + 1;
                    if next_key_idx >= self.end_idx {
                        break;
                    }

                    let next_key = &self.doc.token_tape[next_key_idx];
                    if next_key == key {
                        self.seen[next_key_idx] = 1;
                        self.values.push(next_key_idx + 1)
                    }

                    value_idx = next_key_idx + 1;
                }

                return seed
                    .deserialize(KeyDeserializer {
                        tape_idx: self.tape_idx,
                        doc: self.doc,
                    })
                    .map(Some);
            }

            let value_idx = self.tape_idx + 1;
            let next_key = match self.doc.token_tape[value_idx] {
                TextToken::Array(x) => x,
                TextToken::Object(x) => x,
                _ => value_idx,
            };

            self.tape_idx = next_key + 1;
        }

        Ok(None)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            value_indices: &self.values,
            doc: &self.doc,
            seen: self.seen,
        })
    }
}

struct KeyDeserializer<'b, 'de: 'b> {
    doc: &'b TextTape<'de>,
    tape_idx: usize,
}

fn visit_key<'c, 'b: 'c, 'de: 'b, V: Visitor<'de>>(
    tape_idx: usize,
    doc: &'b TextTape<'de>,
    visitor: V,
) -> Result<V::Value, TextError> {
    todo!()
}

impl<'b, 'de> de::Deserializer<'de> for KeyDeserializer<'b, 'de> {
    type Error = TextError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visit_key(self.tape_idx, self.doc, visitor)
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.doc.token_tape[self.tape_idx] {
            TextToken::Scalar(s) => match s.to_utf8() {
                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            },
            _ => panic!("EEEEK"),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum ignored_any struct
    }
}

struct ValueDeserializer<'b, 'de: 'b> {
    value_indices: &'b Vec<usize>,
    doc: &'b TextTape<'de>,
    seen: &'b mut Vec<u8>,
}

impl<'b, 'de> de::Deserializer<'de> for ValueDeserializer<'b, 'de> {
    type Error = TextError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_indices[0];
        match &self.doc.token_tape[idx] {
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                doc: self.doc,
                seen: self.seen,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
            }),
            TextToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.doc, idx + 1, *x, self.seen))
            }
            TextToken::End(_) => todo!(),
            _ => visit_key(idx, self.doc, visitor),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_indices[0];
        match &self.doc.token_tape[idx] {
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                doc: self.doc,
                seen: self.seen,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
            }),
            TextToken::End(_) => todo!(),
            _ => visitor.visit_seq(SpanningSequence {
                doc: self.doc,
                value_indices: self.value_indices,
                seen: self.seen,
                idx: 0,
            }),
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

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.doc.token_tape[self.value_indices[0]] {
            TextToken::Scalar(s) => visitor.visit_string(s.to_utf8_owned()),
            ref x => Err(TextError {
                kind: TextErrorKind::Message(format!("unable to deserialize {:?} as a string", x)),
            }),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.doc.token_tape[self.value_indices[0]] {
            TextToken::Scalar(s) => match s.to_utf8() {
                Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                Cow::Owned(s) => visitor.visit_string(s),
            },
            ref x => Err(TextError {
                kind: TextErrorKind::Message(format!("unable to deserialize {:?} as a string", x)),
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char
        bytes byte_buf unit unit_struct
        map enum identifier struct
    }
}

struct BinarySequence<'b, 'de: 'b> {
    doc: &'b TextTape<'de>,
    idx: usize,
    de_idx: usize,
    end_idx: usize,
    seen: &'b mut Vec<u8>,
}

impl<'b, 'de, 'r> de::Deserializer<'de> for &'r mut BinarySequence<'b, 'de> {
    type Error = TextError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match &self.doc.token_tape[self.de_idx] {
            TextToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.doc, self.de_idx + 1, *x, self.seen))
            }
            _ => visit_key(self.de_idx, self.doc, visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'b, 'de> SeqAccess<'de> for BinarySequence<'b, 'de> {
    type Error = TextError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.end_idx {
            Ok(None)
        } else {
            let next_key = match self.doc.token_tape[self.idx] {
                TextToken::Array(x) => x,
                TextToken::Object(x) => x,
                _ => self.idx,
            };

            self.de_idx = self.idx;
            self.idx = next_key + 1;
            let res = seed.deserialize(self).map(Some);
            res
        }
    }
}

struct SpanningSequence<'c, 'b: 'c, 'de: 'b> {
    doc: &'b TextTape<'de>,
    value_indices: &'c Vec<usize>,
    seen: &'c mut Vec<u8>,
    idx: usize,
}

impl<'c, 'b, 'de, 'r> de::Deserializer<'de> for &'r mut SpanningSequence<'c, 'b, 'de> {
    type Error = TextError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let idx = self.value_indices[self.idx - 1];
        match &self.doc.token_tape[idx] {
            TextToken::Array(x) => visitor.visit_seq(BinarySequence {
                doc: self.doc,
                seen: self.seen,
                de_idx: 0,
                idx: idx + 1,
                end_idx: *x,
            }),
            TextToken::Object(x) => {
                visitor.visit_map(BinaryMap::new(self.doc, idx + 1, *x, self.seen))
            }
            TextToken::End(_) => todo!(),
            _ => visit_key(idx, self.doc, visitor),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any identifier struct seq
    }
}

impl<'c, 'b, 'de> SeqAccess<'de> for SpanningSequence<'c, 'b, 'de> {
    type Error = TextError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= self.value_indices.len() {
            Ok(None)
        } else {
            self.idx += 1;
            seed.deserialize(self).map(Some)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[test]
    fn test_single_field() {
        let data = b"field1=ENG";

        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let actual: MyStruct = from_slice(&data[..]).unwrap();
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
}
