use serde::{de, de::IgnoredAny, Deserialize, Deserializer};
use std::collections::HashMap;

#[derive(Deserialize)]
#[serde(untagged)]
enum MaybeF64 {
    F64(f64),
    Other(IgnoredAny),
}

pub fn deserialize_hashmap_f64<'de, D>(deserializer: D) -> Result<HashMap<String, f64>, D::Error>
where
    D: Deserializer<'de>,
{
    struct Visitor;

    impl<'de> de::Visitor<'de> for Visitor {
        type Value = HashMap<String, f64>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a map with string keys and numeric values")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut values = HashMap::new();
            while let Some(key) = map.next_key::<String>()? {
                match map.next_value::<MaybeF64>()? {
                    MaybeF64::F64(v) => {
                        values.insert(key, v);
                    }
                    MaybeF64::Other(_) => {
                        // Skip non-f64 values (lists, objects, etc.)
                    }
                }
            }
            Ok(values)
        }
    }

    deserializer.deserialize_map(Visitor)
}
