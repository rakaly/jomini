use serde::{de, Deserializer};
use std::fmt;

pub(crate) fn deserialize_token_bool<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    struct TokenBoolVisitor;

    impl de::Visitor<'_> for TokenBoolVisitor {
        type Value = bool;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string containing bool data")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(v == "yes")
        }
    }

    deserializer.deserialize_any(TokenBoolVisitor)
}
