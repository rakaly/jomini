use std::{collections::HashMap, marker::PhantomData};

/// Deserializes a map (HashMap) with a specified initial capacity determined by the const generic parameter.
/// This can improve performance for maps where the approximate size is known in advance.
pub(crate) fn deserialize_map_with_capacity<'de, D, K, V, const CAPACITY: usize>(
    deserializer: D,
) -> Result<HashMap<K, V>, D::Error>
where
    D: serde::Deserializer<'de>,
    K: serde::Deserialize<'de> + std::hash::Hash + Eq,
    V: serde::Deserialize<'de>,
{
    struct MapWithCapacityVisitor<K1, V1, const CAPACITY: usize> {
        marker: PhantomData<HashMap<K1, V1>>,
    }

    impl<'de, K1, V1, const CAPACITY: usize> serde::de::Visitor<'de>
        for MapWithCapacityVisitor<K1, V1, CAPACITY>
    where
        K1: serde::Deserialize<'de> + std::hash::Hash + Eq,
        V1: serde::Deserialize<'de>,
    {
        type Value = HashMap<K1, V1>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a map")
        }

        fn visit_map<M>(self, mut map: M) -> Result<Self::Value, M::Error>
        where
            M: serde::de::MapAccess<'de>,
        {
            let mut result = HashMap::with_capacity(CAPACITY);

            while let Some((key, value)) = map.next_entry()? {
                result.insert(key, value);
            }

            Ok(result)
        }
    }

    let visitor = MapWithCapacityVisitor::<K, V, CAPACITY> {
        marker: PhantomData,
    };

    deserializer.deserialize_map(visitor)
}
