#![no_main]
use libfuzzer_sys::fuzz_target;
use serde::Deserialize;
use jomini::{ArrayReader, ObjectReader, Encoding, TextToken};

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

fn iterate_array<'data, 'tokens, E>(mut reader: ArrayReader<E>) where E: crate::Encoding + Clone {
    while let Some(value) = reader.next_value() {
        match value.token() {
            TextToken::Object(_) | TextToken::HiddenObject(_) => {
                iterate_object(value.read_object().unwrap());
            }
            TextToken::Array(_)   => { iterate_array(value.read_array().unwrap()); }
            TextToken::End(_) => panic!("end!?"),
            TextToken::Operator(_) => panic!("end!?"),
            TextToken::Unquoted(_) | TextToken::Quoted(_) | TextToken::Header(_) => {
                let _ = value.read_str().unwrap();
            }
        }
    }
}

fn iterate_object<'data, 'tokens, E>(mut reader: ObjectReader<E>) where E: crate::Encoding + Clone {
    while let Some((key, _op, value)) = reader.next_field() {
        let _ = key.read_str();
        match value.token() {
            TextToken::Object(_) | TextToken::HiddenObject(_) => {
                iterate_object(value.read_object().unwrap());
            }
            TextToken::Array(_) | TextToken::Header(_)  => { iterate_array(value.read_array().unwrap()); }
            TextToken::End(_) => panic!("end!?"),
            TextToken::Operator(_) => panic!("end!?"),
            TextToken::Unquoted(_) | TextToken::Quoted(_) => {
                let _ = value.read_str().unwrap();
            }
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let _: Result<Meta, _> = jomini::TextTape::from_slice(&data)
        .and_then(|tape| {
            let tokens = tape.tokens();
            for (i, token) in tokens.iter().enumerate() {
                match token {
                    TextToken::Array(ind) |
                    TextToken::Object(ind) |
                    TextToken::HiddenObject(ind) |
                    TextToken::End(ind) if *ind == 0 => {
                        panic!("zero ind encountered");
                    }
                    TextToken::Array(ind) | TextToken::Object(ind) | TextToken::HiddenObject(ind) => {
                        match tokens[*ind] {
                            TextToken::End(ind2) => {
                                assert_eq!(ind2, i)
                            }
                            _ => panic!("expected end")
                        }
                    }
                    _ => {}
                }
            }

            iterate_object(tape.windows1252_reader());
            jomini::TextDeserializer::from_windows1252_tape(&tape)
        });
});
