use serde::{de, Deserialize, Deserializer};
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

/// An id that the save writes as an integer or as a quoted string. Binary
/// saves write province ids as quoted strings.
struct Id(u32);

impl<'de> Deserialize<'de> for Id {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct IdVisitor;

        impl de::Visitor<'_> for IdVisitor {
            type Value = Id;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("an integer id")
            }

            fn visit_i32<E: de::Error>(self, v: i32) -> Result<Self::Value, E> {
                self.visit_i64(i64::from(v))
            }

            fn visit_i64<E: de::Error>(self, v: i64) -> Result<Self::Value, E> {
                u32::try_from(v)
                    .map(Id)
                    .map_err(|_| E::invalid_value(de::Unexpected::Signed(v), &self))
            }

            fn visit_u32<E: de::Error>(self, v: u32) -> Result<Self::Value, E> {
                Ok(Id(v))
            }

            fn visit_u64<E: de::Error>(self, v: u64) -> Result<Self::Value, E> {
                u32::try_from(v)
                    .map(Id)
                    .map_err(|_| E::invalid_value(de::Unexpected::Unsigned(v), &self))
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
                v.parse()
                    .map(Id)
                    .map_err(|_| E::invalid_value(de::Unexpected::Str(v), &self))
            }
        }

        deserializer.deserialize_u32(IdVisitor)
    }
}

/// Deserialize a map with integer id keys
pub fn deserialize_id_map<'de, D, V>(deserializer: D) -> Result<HashMap<u32, V>, D::Error>
where
    D: Deserializer<'de>,
    V: Deserialize<'de>,
{
    struct IdMapVisitor<V1> {
        marker: PhantomData<HashMap<u32, V1>>,
    }

    impl<'de, V1> de::Visitor<'de> for IdMapVisitor<V1>
    where
        V1: Deserialize<'de>,
    {
        type Value = HashMap<u32, V1>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map with integer id keys")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut values = HashMap::with_capacity(map.size_hint().unwrap_or(0));
            while let Some((Id(id), value)) = map.next_entry()? {
                values.insert(id, value);
            }

            Ok(values)
        }
    }

    deserializer.deserialize_map(IdMapVisitor {
        marker: PhantomData,
    })
}
