use crate::CountryTag;
use serde::{de, Deserialize, Deserializer};
use std::fmt;

/// Deserialize a string. An empty string becomes `None`.
pub fn empty_string_is_none<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    Ok(Some(s).filter(|x| !x.is_empty()))
}

/// Deserialize a country tag. An empty string becomes `None`.
pub fn empty_tag_is_none<'de, D>(deserializer: D) -> Result<Option<CountryTag>, D::Error>
where
    D: Deserializer<'de>,
{
    struct TagVisitor;

    impl de::Visitor<'_> for TagVisitor {
        type Value = Option<CountryTag>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a country tag or an empty string")
        }

        fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
            if v.is_empty() {
                Ok(None)
            } else {
                v.parse().map(Some).map_err(de::Error::custom)
            }
        }
    }

    deserializer.deserialize_str(TagVisitor)
}
