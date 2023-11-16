use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    human: bool,
    #[jomini(take_last)]
    checksum: String,
    fourth: u16,
}


#[derive(JominiDeserialize)]
pub struct Model2 {
    human: bool,
    fourth: u16,
    #[jomini(take_last)]
    checksum: String,
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
    let m2: Model2 = serde_json::from_str(data).unwrap();
    assert_eq!(m.checksum, String::from("false"));
    assert_eq!(m.human, true);
    assert_eq!(m.fourth, 4);
    assert_eq!(m.checksum, m2.checksum);
    assert_eq!(m.human, m2.human);
    assert_eq!(m.fourth, m2.fourth);

}
