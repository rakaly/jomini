use serde::Deserialize;
use std::collections::HashMap;

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

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct BinMeta {
    date: i32,
    save_game: String,
    player: String,
    displayed_country_name: String,
    savegame_versions: Vec<String>,
    multi_player: bool,
    campaign_length: u32,
    campaign_stats: Vec<Stat>,
    checksum: String,
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

#[test]
fn test_text_de_non_scalar_crash() {
    let data = include_bytes!("../../../assets/fixtures/meta.txt.crash2");
    let actual: Result<Meta, _> = jomini::text::de::from_slice(&data[..]);
    assert!(actual.is_err());
}

fn create_bin_lookup() -> HashMap<u16, &'static str> {
    let mut hash = HashMap::new();
    hash.insert(0x000bu16, "id");
    hash.insert(0x284du16, "date");
    hash.insert(0x2c69u16, "save_game");
    hash.insert(0x2a38u16, "player");
    hash.insert(0x32b8u16, "displayed_country_name");
    hash.insert(0x314bu16, "savegame_versions");
    hash.insert(0x3329u16, "multi_player");
    hash.insert(0x3382u16, "campaign_length");
    hash.insert(0x3551u16, "campaign_stats");
    hash.insert(0x179u16, "checksum");
    hash.insert(0x354du16, "comparison");
    hash.insert(0xdcu16, "key");
    hash.insert(0x354eu16, "selector");
    hash.insert(0x209u16, "localization");
    hash
}

#[test]
fn test_binary_meta_deserialization() {
    let data = include_bytes!("../../../assets/fixtures/meta.bin");
    let data = &data["EU4bin".len()..];
    let hash = create_bin_lookup();
    let actual = jomini::binary::de::from_slice::<_, BinMeta>(&data, hash).unwrap();
    assert_eq!(actual.date, 57790056);
}

#[test]
fn test_binary_slice_index_crash() {
    let data = include_bytes!("../../../assets/fixtures/meta.bin.crash");
    let hash = create_bin_lookup();
    assert!(jomini::binary::de::from_slice::<_, BinMeta>(&data[..], hash).is_err());
}

#[test]
fn test_binary_incomplete_array() {
    let data = include_bytes!("../../../assets/fixtures/meta.bin.crash2");
    let hash = create_bin_lookup();
    assert!(jomini::binary::de::from_slice::<_, BinMeta>(&data[..], hash).is_err());
}

#[test]
fn test_binary_heterogenous_object_crash() {
    let data = include_bytes!("../../../assets/fixtures/meta.bin.crash3");
    let hash = create_bin_lookup();
    assert!(jomini::binary::de::from_slice::<_, BinMeta>(&data[..], hash).is_err());
}
