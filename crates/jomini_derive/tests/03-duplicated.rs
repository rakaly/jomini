use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    human: bool,
    first: u16,
    fourth: u16,
    #[jomini(duplicated)]
    core: Vec<String>,
    names: Vec<String>,
}

#[test]
fn test_duplicated() {
    let data = r#"
        {
            "human": true,
            "first": 1,
            "core": "AAA",
            "fourth": 2,
            "core": "BBB",
            "names": [ "CCC", "DDD" ]
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert!(m.human);
    assert_eq!(m.first, 1);
    assert_eq!(m.fourth, 2);
    assert_eq!(m.core, vec!["AAA".to_string(), "BBB".to_string()]);
    assert_eq!(m.names, vec!["CCC".to_string(), "DDD".to_string()]);
}
