use jomini::{Encoding, Utf8Encoding};
use serde::{de, Deserializer};
use std::fmt;

/// Deserializes a string that the game wrote as utf-8 instead of Windows-1252
///
/// The game writes the save file name from the OS file name, which bypasses
/// the EU4dll string hook that the Japanese and Chinese mods use. A save from
/// one of these mods stores the file name as utf-8 while every other string
/// is EU4dll escaped, so this field is decoded from the raw bytes.
pub(crate) fn deserialize_utf8_string<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    struct Utf8StringVisitor;

    impl de::Visitor<'_> for Utf8StringVisitor {
        type Value = String;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a utf-8 string")
        }

        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(Utf8Encoding::new().decode(v).into_owned())
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(String::from(v))
        }
    }

    deserializer.deserialize_bytes(Utf8StringVisitor)
}
