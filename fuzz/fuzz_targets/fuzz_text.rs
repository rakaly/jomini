#![no_main]
use jomini::{ArrayReader, Encoding, ObjectReader, TextToken, ValueReader};
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

fn read_value<E>(value: ValueReader<E>)
where
    E: crate::Encoding + Clone,
{
    match value.token() {
        TextToken::Object(_) | TextToken::HiddenObject(_) => {
            iterate_object(value.read_object().unwrap());
        }
        TextToken::Array(_) => {
            iterate_array(value.read_array().unwrap());
        }
        TextToken::End(_) => panic!("end!?"),
        TextToken::Operator(_) => panic!("end!?"),
        TextToken::Unquoted(_) | TextToken::Quoted(_) | TextToken::Header(_) => {
            let _ = value.read_str().unwrap();
        }
    }
}

fn iterate_array<E>(mut reader: ArrayReader<E>)
where
    E: crate::Encoding + Clone,
{
    while let Some(value) = reader.next_value() {
        read_value(value)
    }
}

fn iterate_object<E>(mut reader: ObjectReader<E>)
where
    E: crate::Encoding + Clone,
{
    let mut fields_reader = reader.clone();
    while let Some((_key, entries)) = fields_reader.next_fields() {
        for (_op, value) in entries {
            read_value(value);
        }
    }

    while let Some((key, _op, value)) = reader.next_field() {
        let _ = key.read_str();
        read_value(value);
    }
}

fuzz_target!(|data: &[u8]| {
    let _: Result<Meta, _> = jomini::TextTape::from_slice(&data).and_then(|tape| {
        let tokens = tape.tokens();
        for (i, token) in tokens.iter().enumerate() {
            match token {
                TextToken::Array(ind)
                | TextToken::Object(ind)
                | TextToken::HiddenObject(ind)
                | TextToken::End(ind)
                    if *ind == 0 =>
                {
                    panic!("zero ind encountered");
                }
                TextToken::Array(ind) | TextToken::Object(ind) | TextToken::HiddenObject(ind) => {
                    match tokens[*ind] {
                        TextToken::End(ind2) => {
                            assert_eq!(ind2, i)
                        }
                        _ => panic!("expected end"),
                    }
                }
                _ => {}
            }
        }

        iterate_object(tape.windows1252_reader());
        jomini::TextDeserializer::from_windows1252_tape(&tape)
    });
});
