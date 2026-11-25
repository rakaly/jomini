use crate::flavor::decode_eu4_mixed_text;
use serde::de::{self, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use std::fmt;

pub(crate) fn deserialize_eu4_string<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    struct Eu4StringVisitor;

    impl de::Visitor<'_> for Eu4StringVisitor {
        type Value = String;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string encoded as UTF-8, Windows-1252, or EU4dll escapes")
        }

        fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(decode_eu4_mixed_text(value))
        }

        fn visit_borrowed_bytes<E>(self, value: &'_ [u8]) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(decode_eu4_mixed_text(value))
        }

        fn visit_byte_buf<E>(self, value: Vec<u8>) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(decode_eu4_mixed_text(&value))
        }

        fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(String::from(value))
        }

        fn visit_string<E>(self, value: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Ok(value)
        }
    }

    deserializer.deserialize_bytes(Eu4StringVisitor)
}

pub(crate) fn deserialize_eu4_string_vec<'de, D>(deserializer: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    struct Eu4StringVecVisitor;

    impl<'de> Visitor<'de> for Eu4StringVecVisitor {
        type Value = Vec<String>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a sequence of EU4 strings")
        }

        fn visit_seq<A>(self, mut sequence: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut values = Vec::with_capacity(sequence.size_hint().unwrap_or_default());
            while let Some(value) = sequence.next_element::<Eu4String>()? {
                values.push(value.0);
            }
            Ok(values)
        }
    }

    struct Eu4String(String);

    impl<'de> Deserialize<'de> for Eu4String {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserialize_eu4_string(deserializer).map(Eu4String)
        }
    }

    deserializer.deserialize_seq(Eu4StringVecVisitor)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    #[derive(Deserialize)]
    struct Fields {
        #[serde(deserialize_with = "deserialize_eu4_string")]
        mod_name: String,
        #[serde(deserialize_with = "deserialize_eu4_string_vec")]
        players_countries: Vec<String>,
    }

    #[test]
    fn deserializes_utf8_mod_names_and_mixed_player_names() {
        let mut data = b"mod_name=\"".to_vec();
        data.extend_from_slice("女仆事件框/Maid Event Window".as_bytes());
        data.extend_from_slice(b"\"\nplayers_countries={ \"");
        data.extend_from_slice(
            b"\xe5\x8d\x83\xe5\x88\x83\xe4\xb8\x87\xe6\xb6\x9b\xe6\x9f\x93\xe6\xa1\x83\x10\x07\x7f\x10\x05Z4",
        );
        data.extend_from_slice(b"\" \"NEV\" }");

        let deserializer = jomini::TextDeserializer::from_windows1252_slice(&data).unwrap();
        let fields = Fields::deserialize(&deserializer).unwrap();

        assert_eq!(fields.mod_name, "女仆事件框/Maid Event Window");
        assert_eq!(fields.players_countries[0], "千刃万涛染桃缇娅4");
        assert_eq!(fields.players_countries[1], "NEV");
    }
}
