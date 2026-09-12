use crate::{Error, binary::Rgb};
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
            0 => self.data.r,
            1 => self.data.g,
            2 => self.data.b,
            3 => self.data.a.unwrap(),
            _ => unreachable!(),
        }
    }
}

impl<'de> de::Deserializer<'de> for &'_ mut ColorSequence {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.val())
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(self)
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
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct map struct enum identifier ignored_any
    }
}

impl<'de> de::Deserializer<'de> for ColorSequence {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.val())
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(self)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(self)
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
        visitor.visit_seq(self)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct map struct enum identifier ignored_any
    }
}

impl<'de> SeqAccess<'de> for ColorSequence {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DeserializeSeed<'de>,
    {
        if (self.idx >= 3 && self.data.a.is_none()) || (self.idx >= 4 && self.data.a.is_some()) {
            Ok(None)
        } else {
            let result = seed.deserialize(&mut *self);
            self.idx += 1;
            result.map(Some)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        let remaining = if self.data.a.is_none() {
            3 - self.idx
        } else {
            4 - self.idx
        };
        Some(remaining)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binary::Rgb;
    use serde::Deserialize;

    #[test]
    fn test_color_sequence_rgb_deserialization() {
        let rgb = Rgb {
            r: 255,
            g: 128,
            b: 64,
            a: None,
        };
        let seq = ColorSequence::new(rgb);

        #[derive(Debug, PartialEq, Deserialize)]
        struct RgbTuple(u32, u32, u32);

        let result: RgbTuple = RgbTuple::deserialize(seq).unwrap();
        assert_eq!(result, RgbTuple(255, 128, 64));
    }

    #[test]
    fn test_color_sequence_rgba_deserialization() {
        let rgb = Rgb {
            r: 255,
            g: 128,
            b: 64,
            a: Some(192),
        };
        let seq = ColorSequence::new(rgb);

        #[derive(Debug, PartialEq, Deserialize)]
        struct RgbaTuple(u32, u32, u32, u32);

        let result: RgbaTuple = RgbaTuple::deserialize(seq).unwrap();
        assert_eq!(result, RgbaTuple(255, 128, 64, 192));
    }

    #[test]
    fn test_color_sequence_sequential_values() {
        let rgb = Rgb {
            r: 10,
            g: 20,
            b: 30,
            a: Some(40),
        };
        let mut seq = ColorSequence::new(rgb);

        use std::marker::PhantomData;

        let result1 = seq.next_element_seed(PhantomData::<u32>).unwrap().unwrap();
        assert_eq!(result1, 10);

        let result2 = seq.next_element_seed(PhantomData::<u32>).unwrap().unwrap();
        assert_eq!(result2, 20);

        let result3 = seq.next_element_seed(PhantomData::<u32>).unwrap().unwrap();
        assert_eq!(result3, 30);

        let result4 = seq.next_element_seed(PhantomData::<u32>).unwrap().unwrap();
        assert_eq!(result4, 40);
    }

    #[test]
    fn test_color_sequence_size_hint_rgb() {
        let rgb = Rgb {
            r: 255,
            g: 128,
            b: 64,
            a: None,
        };
        let seq = ColorSequence::new(rgb);

        assert_eq!(seq.size_hint(), Some(3));
    }

    #[test]
    fn test_color_sequence_size_hint_rgba() {
        let rgb = Rgb {
            r: 255,
            g: 128,
            b: 64,
            a: Some(192),
        };
        let seq = ColorSequence::new(rgb);

        assert_eq!(seq.size_hint(), Some(4));
    }

    #[test]
    fn test_color_sequence_size_hint_with_progress() {
        let rgb = Rgb {
            r: 255,
            g: 128,
            b: 64,
            a: None,
        };
        let mut seq = ColorSequence::new(rgb);

        use std::marker::PhantomData;

        seq.next_element_seed(PhantomData::<u32>).unwrap();
        seq.next_element_seed(PhantomData::<u32>).unwrap();

        assert_eq!(seq.size_hint(), Some(1));
    }

    #[test]
    fn test_color_sequence_deserialize_any() {
        let rgb = Rgb {
            r: 100,
            g: 200,
            b: 50,
            a: None,
        };
        let mut seq = ColorSequence::new(rgb);

        let value: u32 = u32::deserialize(&mut seq).unwrap();
        assert_eq!(value, 100);
    }
}
