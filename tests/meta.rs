use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Meta {
    date: String,
    save_game: String,
    player: String,
    displayed_country_name: String,
    savegame_versions: Vec<String>,
    multi_player: bool,
    campaign_length: u32,
    campaign_stats: Vec<Stat>,
    checksum: String,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Stat {
    id: u32,
    comparison: u32,
    key: String,
    selector: Option<String>,
    localization: Option<String>,
}

#[test]
fn test_text_deserialization() {
    let data = include_bytes!("../../../assets/fixtures/meta.txt");
    let actual: Meta = jomini::text::de::from_slice(&data["EU4txt".len()..]).unwrap();
    assert_eq!(actual.date, String::from("1444.11.11"));
}

#[test]
fn test_scalar_u64_overflow_crash() {
    let data = include_bytes!("../../../assets/fixtures/meta.txt.crash");
    let actual: Result<Meta, _> = jomini::text::de::from_slice(&data[..]);
    assert!(actual.is_err());
}
