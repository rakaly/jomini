use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    human: bool,
    #[jomini(take_last)]
    checksum: String,
    fourth: u16,
}

#[test]
fn test_options() {
    let data = r#"
        {
            "human": true,
            "checksum": "true",
            "fourth": 4,
            "checksum": "false"
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.checksum, String::from("false"));
    assert_eq!(m.human, true);
    assert_eq!(m.fourth, 4);
}
