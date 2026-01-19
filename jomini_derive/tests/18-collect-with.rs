use jomini_derive::JominiDeserialize;
use serde::{Deserializer, de};
use std::fmt;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(collect_with = "add_country_node")]
    countries: Vec<(String, u16)>,
    first: u16,
    fourth: u16,
    #[jomini(duplicated)]
    core: Vec<u32>,
    names: Vec<String>,
}

fn add_country_node<'de, A: de::MapAccess<'de>>(
    countries: &mut Vec<(String, u16)>,
    key: &str,
    map: &mut A,
) -> Result<(), A::Error> {
    if key.len() <= 3 {
        countries.push((key.into(), map.next_value()?));
    } else {
        map.next_value::<de::IgnoredAny>()?;
    }
    Ok(())
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
            "first": 1,
            "core": 10,
            "fourth": 2,
            "core": 20,
            "names": [ "CCC", "DDD" ],
            "TAG": 10,
            "NOTATAG": 40,
            "MEE": 5
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.first, 1);
    assert_eq!(m.fourth, 2);
    assert_eq!(m.core, vec![10, 20]);
    assert_eq!(m.names, vec!["CCC".to_string(), "DDD".to_string()]);
    assert_eq!(
        m.countries,
        vec![("TAG".to_string(), 10), ("MEE".to_string(), 5)]
    );
}
