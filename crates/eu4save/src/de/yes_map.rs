use serde::{de, Deserializer};
use std::collections::HashMap;
use std::fmt;

pub(crate) fn deserialize_yes_map<'de, D>(
    deserializer: D,
) -> Result<HashMap<String, bool>, D::Error>
where
    D: Deserializer<'de>,
{
    struct YesMapVisitor;

    impl<'de> de::Visitor<'de> for YesMapVisitor {
        type Value = HashMap<String, bool>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a yes map")
        }
        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut values = if let Some(size) = map.size_hint() {
                HashMap::with_capacity(size)
            } else {
                HashMap::new()
            };

            while let Some(key) = map.next_key::<String>()? {
                let _: de::IgnoredAny = map.next_value()?;
                values.insert(key, true);
            }

            Ok(values)
        }
    }

    deserializer.deserialize_map(YesMapVisitor)
}
