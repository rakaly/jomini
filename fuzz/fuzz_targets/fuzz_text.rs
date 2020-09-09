#![no_main]
use libfuzzer_sys::fuzz_target;
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

fuzz_target!(|data: &[u8]| {
    let _: Result<Meta, _> = jomini::TextTape::from_slice(&data)
        .and_then(|tape| jomini::TextDeserializer::from_windows1252_tape(&tape));
});
