use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    human: bool,
    first: u16,
    fourth: u16,
    core: Vec<String>,
    names: Vec<String>,
}

fn main() {
    let data = r#"
        {
            "human": true,
            "first": 1,
            "fourth": 2,
            "core": [
                "AAA",
                "BBB"
            ],
            "names": [ "CCC", "DDD" ]
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.human, true);
    assert_eq!(m.first, 1);
    assert_eq!(m.fourth, 2);
    assert_eq!(m.core, vec!["AAA".to_string(), "BBB".to_string()]);
    assert_eq!(m.names, vec!["CCC".to_string(), "DDD".to_string()]);
}
