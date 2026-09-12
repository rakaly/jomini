use serde::{
    de::{self, SeqAccess},
    Deserializer,
};
use std::fmt;

pub(crate) fn deserialize_list_overflow_byte<'de, D, const N: usize>(
    deserializer: D,
) -> Result<[u8; N], D::Error>
where
    D: Deserializer<'de>,
{
    struct ListVisitor<const N: usize>;

    impl<'de, const N: usize> de::Visitor<'de> for ListVisitor<N> {
        type Value = [u8; N];

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a seq of bytes allowed to overflow")
        }

        fn visit_seq<A>(self, seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            collect_into_default(seq)
        }
    }

    deserializer.deserialize_seq(ListVisitor)
}

/// Deserializes a sequence of elements into a fixed size array. If the input is
/// not long enough, the default value is used. Extraneous elements are ignored.
/// This is useful for deserializing sequences that are expected to be of a
/// fixed size, but being tolerant is more important than meeting expectations.
fn collect_into_default<'de, A, const N: usize>(
    mut seq: A,
) -> Result<[u8; N], <A as SeqAccess<'de>>::Error>
where
    A: SeqAccess<'de>,
{
    let mut result = [0u8; N];
    for i in 0..N {
        let Some(x) = seq.next_element::<u16>()? else {
            return Ok(result);
        };
        result[i] = x as u8;
    }

    // If the sequence is not finished, we need to consume the rest of the elements
    // so that we drive a potential parser that underlies the deserializer
    while let Some(_x) = seq.next_element::<de::IgnoredAny>()? {}

    Ok(result)
}

pub(crate) fn deserialize_list_overflow_byte_opt<'de, D, const N: usize>(
    deserializer: D,
) -> Result<Option<[u8; N]>, D::Error>
where
    D: Deserializer<'de>,
{
    deserialize_list_overflow_byte(deserializer).map(Some)
}
