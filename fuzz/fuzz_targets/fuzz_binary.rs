#![no_main]
use jomini::{binary::BinaryFlavor, Encoding, Windows1252Encoding};
use libfuzzer_sys::fuzz_target;
use serde::Deserialize;
use std::{borrow::Cow, collections::HashMap};

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Meta {
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

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Stat {
    id: u32,
    comparison: u32,
    key: String,
    selector: Option<String>,
    localization: Option<String>,
}

#[derive(Debug, Default)]
pub struct BinaryTestFlavor;

impl BinaryFlavor for BinaryTestFlavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_le_bytes(data)
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        f64::from_le_bytes(data)
    }
}

impl Encoding for BinaryTestFlavor {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Windows1252Encoding::decode(data)
    }
}

fuzz_target!(|data: &[u8]| {
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

    let _: Result<Meta, _> = jomini::BinaryTape::from_slice(&data).and_then(|tape| {
        let tokens = tape.tokens();
        for (i, token) in tokens.iter().enumerate() {
            match token {
                jomini::BinaryToken::Array(ind)
                | jomini::BinaryToken::Object(ind)
                | jomini::BinaryToken::End(ind)
                    if *ind == 0 =>
                {
                    panic!("zero ind encountered");
                }
                jomini::BinaryToken::MixedContainer => {}
                jomini::BinaryToken::Equal => {}
                jomini::BinaryToken::Array(ind) | jomini::BinaryToken::Object(ind) => {
                    match tokens[*ind] {
                        jomini::BinaryToken::End(ind2) => {
                            assert_eq!(ind2, i)
                        }
                        _ => panic!("expected end"),
                    }
                }
                _ => {}
            }
        }
        jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor).from_tape(&tape, &hash)
    });
});
