use jomini_derive::JominiDeserialize;
use serde::{Deserialize, Deserializer, de::DeserializeOwned};
use std::{fmt, marker::PhantomData};

#[derive(JominiDeserialize)]
pub struct Model {
    database: Manager<String>,
}

#[derive(JominiDeserialize)]
pub struct Manager<Of>
where
    Of: DeserializeOwned,
{
    #[jomini(deserialize_with = "maybe_option")]
    value: Option<Of>,
    #[jomini(deserialize_with = "maybe_option")]
    value2: Option<Of>,
}

fn maybe_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    struct MaybeVisitor<T> {
        marker: PhantomData<Option<T>>,
    }

    impl<'de, T1> serde::de::Visitor<'de> for MaybeVisitor<T1>
    where
        T1: Deserialize<'de>,
    {
        type Value = Option<T1>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_fmt(format_args!(
                "struct {} or none",
                std::any::type_name::<T1>()
            ))
        }
        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: serde::de::Error,
        {
            match v {
                "none" => Ok(None),
                _ => T1::deserialize(serde::de::value::StrDeserializer::new(v)).map(Some),
            }
        }
    }

    deserializer.deserialize_any(MaybeVisitor {
        marker: PhantomData,
    })
}

#[test]
fn test_options() {
    let data = r#"
        {
            "database": {
               "value": "hello",
               "value2": "none"
            }
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.database.value, Some(String::from("hello")));
    assert_eq!(m.database.value2, None);
}
