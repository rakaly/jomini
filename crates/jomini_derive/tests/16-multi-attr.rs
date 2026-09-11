use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(default, duplicated, alias = "core")]
    cores: Vec<String>,
}

#[test]
fn test_duplicated() {
    let data = r#"
        {
            "core": "AAA",
            "core": "BBB"
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.cores, vec!["AAA".to_string(), "BBB".to_string()]);
}
