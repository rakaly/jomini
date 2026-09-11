use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    database: Manager<String>,
}

#[derive(JominiDeserialize)]
pub struct Manager<Of> {
    value: Of,
}

#[test]
fn test_options() {
    let data = r#"
        {
            "database": {
               "value": "hello"
            }
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.database.value, String::from("hello"));
}
