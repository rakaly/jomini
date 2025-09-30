use jomini_derive::JominiDeserialize;
use serde::{Deserializer, de};
use std::fmt;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(deserialize_with = "deserialize_token_bool")]
    human: bool,
    first: u16,
    fourth: u16,
    #[jomini(duplicated)]
    core: Vec<u32>,
    names: Vec<String>,
}

fn deserialize_token_bool<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    struct TokenBoolVisitor;

    impl de::Visitor<'_> for TokenBoolVisitor {
        type Value = bool;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a string containing json data")
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

#[test]
fn test_deserialize_with() {
    let data = r#"
        {
            "human": "yes",
            "first": 1,
            "core": 10,
            "fourth": 2,
            "core": 20,
            "names": [ "CCC", "DDD" ]
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert!(m.human);
    assert_eq!(m.first, 1);
    assert_eq!(m.fourth, 2);
    assert_eq!(m.core, vec![10, 20]);
    assert_eq!(m.names, vec!["CCC".to_string(), "DDD".to_string()]);
}
