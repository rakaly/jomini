use jomini_derive::JominiDeserialize;
use serde::Deserialize;

#[derive(JominiDeserialize, Debug)]
pub struct Model {
    #[jomini(deserialize_with = "deserialize_array_with_length::<_, 5>")]
    data: [u8; 5],
}

fn deserialize_array_with_length<'de, D, const N: usize>(
    deserializer: D,
) -> Result<[u8; N], D::Error>
where
    D: serde::Deserializer<'de>,
{
    // This is a simple implementation that would parse a string of bytes
    let s = String::deserialize(deserializer)?;
    let bytes: Vec<u8> = s.bytes().collect();

    if bytes.len() != N {
        return Err(serde::de::Error::custom(format!(
            "Expected array of length {}, got {}",
            N,
            bytes.len()
        )));
    }

    let mut array = [0u8; N];
    array.copy_from_slice(&bytes[..N]);
    Ok(array)
}

#[test]
fn test_deserialize_with_const_generics() {
    let json = r#"{"data":"hello"}"#;
    let model: Model = serde_json::from_str(json).unwrap();
    assert_eq!(model.data, [104, 101, 108, 108, 111]); // "hello" as bytes
}

#[test]
fn test_deserialize_with_const_generics_error() {
    let json = r#"{"data":"hi"}"#;
    let result: Result<Model, _> = serde_json::from_str(json);
    assert!(result.is_err());
}
