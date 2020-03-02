pub mod binary;

use crate::scalar::Scalar;
use crate::{TextEvent, TextParser};
use serde::de::{self, Deserialize, DeserializeSeed, MapAccess, SeqAccess, Visitor};
use std::error;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum PfarError {
    Message(String),
}

impl fmt::Display for PfarError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error")
    }
}

impl error::Error for PfarError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

impl de::Error for PfarError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        PfarError::Message(msg.to_string())
    }
}
/*
pub struct ReadingParser<R: Read> {
    first: bool,
    parser: Option<TextParser<'static>>,
    reader: R,
    buffer: Vec<u8>,
    item: Option<TextEvent<'static>>,
}

impl<R: Read> ReadingParser<R> {
    pub fn create(reader: R) -> Self {
        let buffer = Vec::new();
//        let parser = TextParser::new(&buffer);
        ReadingParser { reader, first: true, buffer, item: None, parser: None }
    }
}

impl<R> StreamingIterator for ReadingParser<R>
    where R: Read {
    type Item = TextEvent<'static>;

    fn advance(&mut self) {
        if self.first == true {
            self.parser = Some(TextParser::new(&self.buffer));
            self.first = false;
            self.reader.read_to_end(&mut self.buffer).unwrap();
        }
    }

    fn get(&self) -> Option<&Self::Item> {
        self.item.as_ref()
    }
}*/

pub fn from_slice<'a, T>(input: &'a [u8]) -> Result<T, PfarError>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer::from_slice(input);
    T::deserialize(&mut deserializer)
}

pub struct Deserializer<'a> {
    parser: TextParser<'a>,
    current: Option<TextEvent<'a>>,
    identifier: Option<Scalar<'a>>,
    at_value: bool,
}

impl<'a> Deserializer<'a> {
    pub fn from_slice(input: &'a [u8]) -> Self {
        Deserializer {
            parser: TextParser::new(input),
            current: None,
            identifier: None,
            at_value: false,
        }
    }
}

impl<'a> Deserializer<'a> {
    fn next(&mut self) -> Option<&TextEvent<'a>> {
        self.at_value = false;
        self.current = self.parser.next();
        while let Some(event) = &self.current {
            if !event.is_comment() {
                break;
            }

            self.current = self.parser.next();
        }

        self.current.as_ref()
    }

    fn get_value(&mut self) -> Option<&TextEvent<'a>> {
        if !self.at_value {
            self.next()
        } else {
            self.current.as_ref()
        }
    }
}

impl<'de, 'r> de::Deserializer<'de> for &'r mut Deserializer<'de> {
    type Error = PfarError;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, PfarError>
    where
        V: Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, PfarError>
    where
        V: Visitor<'de>,
    {
        if let Some(TextEvent::Scalar(scalar)) = self.get_value() {
            visitor.visit_i64(scalar.to_i64().unwrap())
        } else {
            todo!()
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, PfarError>
    where
        V: Visitor<'de>,
    {
        if let Some(TextEvent::Scalar(scalar)) = self.get_value() {
            visitor.visit_string(scalar.to_string())
        } else {
            todo!()
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if let Some(TextEvent::Scalar(scalar)) = self.current {
            let ident = scalar.to_utf8_owned();
            self.identifier = Some(scalar);
            visitor.visit_string(ident)
        } else {
            todo!()
        }
    }

    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(Sequence::new(&mut self))
    }

    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(FieldSeparated::new(&mut self))
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i128 u8 u16 u32 u64 u128 f32 f64 char str
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum ignored_any
    }
}

struct FieldSeparated<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> FieldSeparated<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        FieldSeparated { de }
    }
}

impl<'de, 'a> MapAccess<'de> for FieldSeparated<'a, 'de> {
    type Error = PfarError;
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DeserializeSeed<'de>,
    {
        loop {
            match self.de.next() {
                Some(TextEvent::Scalar(_)) => return seed.deserialize(&mut *self.de).map(Some),
                Some(_) => {}
                None => {
                    return Ok(None);
                }
            }
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DeserializeSeed<'de>,
    {
        loop {
            match self.de.next() {
                Some(TextEvent::Operator(_)) => {
                    return seed.deserialize(&mut *self.de);
                }
                None => todo!(),
                _ => {}
            }
            panic!("EEEK");
        }
    }
}

enum SequenceMode {
    Unknown,
    Single,
    Multiple,
}

struct Sequence<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    mode: SequenceMode,
    name: String,
}

impl<'de, 'a> Sequence<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        let name = de.identifier.unwrap().to_string();
        Sequence {
            de,
            mode: SequenceMode::Unknown,
            name,
        }
    }
}

impl<'de, 'a> SeqAccess<'de> for Sequence<'a, 'de> {
    type Error = PfarError;
    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        match self.mode {
            SequenceMode::Unknown => {
                let entry = self.de.next().unwrap();

                match entry {
                    TextEvent::Open => {
                        self.mode = SequenceMode::Multiple;
                    }
                    TextEvent::Scalar(_) => {
                        self.mode = SequenceMode::Single;
                    }
                    _ => todo!(),
                }

                self.de.at_value = true;
                seed.deserialize(&mut *self.de).map(Some)
            }
            SequenceMode::Single => {
                let entry = self.de.next();
                match entry {
                    None => Ok(None),
                    Some(TextEvent::End) => Ok(None),
                    Some(TextEvent::Scalar(s)) => {
                        if s.to_string() == self.name {
                            let _operator = self.de.next().unwrap();
                            let _value = self.de.next().unwrap();
                            self.de.at_value = true;
                            seed.deserialize(&mut *self.de).map(Some)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => todo!(),
                }
            }
            SequenceMode::Multiple => {
                let entry = self.de.next().unwrap();
                if entry == &TextEvent::End {
                    Ok(None)
                } else {
                    seed.deserialize(&mut *self.de).map(Some)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[test]
    fn test_single_field() {
        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: String,
        }

        let actual: MyStruct = from_slice(b"field1=abc").unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: "abc".to_string()
            }
        );
    }

    #[test]
    fn test_single_i64_field() {
        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
        }

        let actual: MyStruct = from_slice(b"field1=10").unwrap();
        assert_eq!(actual, MyStruct { field1: 10 });
    }

    #[test]
    fn test_two_field_deserialization() {
        #[derive(Deserialize, PartialEq, Eq, Debug)]
        struct MyStruct {
            field1: i64,
            field2: String,
        }

        let actual: MyStruct = from_slice(b"field1=10\nfield2=abc").unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: 10,
                field2: String::from("abc"),
            }
        );
    }

    #[test]
    fn test_single_vec_field() {
        #[derive(Deserialize, PartialEq, Debug)]
        struct MyStruct {
            field1: Vec<String>,
        }

        let actual: MyStruct = from_slice(b"field1=abc\nfield1=def").unwrap();
        assert_eq!(
            actual,
            MyStruct {
                field1: vec!["abc".to_string(), "def".to_string()]
            }
        );
    }
}
