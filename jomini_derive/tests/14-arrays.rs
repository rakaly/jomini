use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(duplicated)]
    lst: smallvec::SmallVec<[u8; 4]>,
}

#[test]
fn test_array_deserializer() {
    let data = r#"
        {
            "lst": 1,
            "a": "b",
            "lst": 2,
            "lst": 3,
            "lst": 4
        }"#;

    let m: Model = serde_json::from_str(data).unwrap();
    assert_eq!(m.lst.as_slice(), &[1, 2, 3, 4]);
}
