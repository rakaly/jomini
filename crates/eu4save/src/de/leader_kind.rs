use crate::models::LeaderKind;
use serde::{de, Deserialize, Deserializer};
use std::fmt;

impl<'de> Deserialize<'de> for LeaderKind {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct LeaderKindVisitor;

        impl de::Visitor<'_> for LeaderKindVisitor {
            type Value = LeaderKind;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string containing leader type")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "admiral" => Ok(LeaderKind::Admiral),
                    "general" => Ok(LeaderKind::General),
                    "conquistador" => Ok(LeaderKind::Conquistador),
                    "explorer" => Ok(LeaderKind::Explorer),
                    _ => Err(de::Error::custom(format!("unknown leader key: {}", v))),
                }
            }
        }

        deserializer.deserialize_any(LeaderKindVisitor)
    }
}
