#![no_main]
use jomini::{Encoding, TextToken, text::{ArrayReader, ObjectReader, ValueReader}};
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
        TextToken::Unquoted(x)
        | TextToken::Quoted(x)
        | TextToken::Header(x)
        | TextToken::Parameter(x)
        | TextToken::UndefinedParameter(x) => {
            let _ = x.to_f64();
            let _ = x.to_u64();
            let _ = x.to_i64();
            let _ = x.is_ascii();
            let _ = x.to_bool();
            let stringed = value.read_str().unwrap();
            let _ = jomini::common::Date::parse(stringed.as_ref());
        }
    }
}

fn iterate_array<E>(reader: ArrayReader<E>)
where
    E: crate::Encoding + Clone,
{
    for value in reader.values() {
        read_value(value)
    }
}

fn iterate_object<E>(reader: ObjectReader<E>)
where
    E: crate::Encoding + Clone,
{
    for (_key, entries) in reader.field_groups() {
        for (_op, value) in entries.values() {
            read_value(value);
        }
    }

    let mut fields = reader.fields();
    for (key, _op, value) in fields.by_ref() {
        let _ = key.read_str();
        read_value(value);
    }

    if let Some(trailer) = fields.at_trailer() {
        iterate_array(trailer);
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
