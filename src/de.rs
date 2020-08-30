use crate::{DeserializeError, Rgb};
use de::{DeserializeSeed, SeqAccess, Visitor};
use serde::de;

#[derive(Debug)]
pub(crate) struct ColorSequence {
    data: Rgb,
    idx: usize,
}

impl ColorSequence {
    pub(crate) fn new(data: Rgb) -> Self {
        ColorSequence { data, idx: 0 }
    }

    fn val(&self) -> u32 {
        match self.idx {
            1 => self.data.r,
            2 => self.data.g,
            3 => self.data.b,
            _ => unreachable!(),
        }
    }
}

impl<'b, 'de, 'r> de::Deserializer<'de> for &'r mut ColorSequence {
    type Error = DeserializeError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.val())
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

impl<'b, 'de> SeqAccess<'de> for ColorSequence {
    type Error = DeserializeError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if self.idx >= 3 {
            Ok(None)
        } else {
            self.idx += 1;
            seed.deserialize(self).map(Some)
        }
    }
}
