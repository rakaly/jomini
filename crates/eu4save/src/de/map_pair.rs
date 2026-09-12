use serde::{de, Deserialize, Deserializer};
use std::{fmt, marker::PhantomData};

pub fn deserialize_map_pair<'de, D, K, V>(deserializer: D) -> Result<Vec<(K, V)>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de>,
    V: Deserialize<'de>,
{
    struct MapPairVisitor<K1, V1> {
        marker: PhantomData<Vec<(K1, V1)>>,
    }

    impl<'de, K1, V1> de::Visitor<'de> for MapPairVisitor<K1, V1>
    where
        K1: Deserialize<'de>,
        V1: Deserialize<'de>,
    {
        type Value = Vec<(K1, V1)>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map containing key value tuples")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: de::SeqAccess<'de>,
        {
            let mut values = Vec::with_capacity(6);
            while seq
                .next_element_seed(ExtendVec {
                    values: &mut values,
                })?
                .is_some()
            {}

            Ok(values)
        }
    }

    deserializer.deserialize_seq(MapPairVisitor {
        marker: PhantomData,
    })
}

// https://docs.rs/serde/latest/serde/de/trait.DeserializeSeed.html
struct ExtendVec<'a, K, V> {
    values: &'a mut Vec<(K, V)>,
}

impl<'de, K, V> de::DeserializeSeed<'de> for ExtendVec<'_, K, V>
where
    K: Deserialize<'de>,
    V: Deserialize<'de>,
{
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExtendVecVisitor<'a, K1, V1> {
            values: &'a mut Vec<(K1, V1)>,
        }

        impl<'de, K1, V1> de::Visitor<'de> for ExtendVecVisitor<'_, K1, V1>
        where
            K1: Deserialize<'de>,
            V1: Deserialize<'de>,
        {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "map pair")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                while let Some(key) = seq.next_element::<K1>()? {
                    let value = seq
                        .next_element::<V1>()?
                        .ok_or_else(|| de::Error::custom("expected value with map pair key"))?;
                    self.values.push((key, value))
                }

                Ok(())
            }
        }

        deserializer.deserialize_seq(ExtendVecVisitor {
            values: self.values,
        })
    }
}
