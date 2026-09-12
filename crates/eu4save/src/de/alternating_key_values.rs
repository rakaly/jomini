use serde::{de, Deserialize, Deserializer};
use std::marker::PhantomData;
use std::{collections::HashMap, fmt, hash::Hash};

pub(crate) fn deserialize_alternating_key_values<'de, D, K, V>(
    deserializer: D,
) -> Result<HashMap<K, V>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Hash + Eq,
    V: Deserialize<'de>,
{
    struct AlternatePairVisitor<K1, V1> {
        marker: PhantomData<HashMap<K1, V1>>,
    }

    impl<'de, K1, V1> de::Visitor<'de> for AlternatePairVisitor<K1, V1>
    where
        K1: Deserialize<'de> + Hash + Eq,
        V1: Deserialize<'de>,
    {
        type Value = HashMap<K1, V1>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map containing key value tuples")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut result = if let Some(size) = map.size_hint() {
                HashMap::with_capacity(size / 2)
            } else {
                HashMap::new()
            };

            loop {
                let key = if let Some((_key, key_value)) = map.next_entry()? {
                    let _ignore_value: serde::de::IgnoredAny = _key;
                    key_value
                } else {
                    return Ok(result);
                };

                let value = if let Some((_value, value_value)) = map.next_entry()? {
                    let _ignore_value: serde::de::IgnoredAny = _value;
                    value_value
                } else {
                    return Ok(result);
                };

                result.insert(key, value);
            }
        }
    }

    deserializer.deserialize_map(AlternatePairVisitor {
        marker: PhantomData,
    })
}
