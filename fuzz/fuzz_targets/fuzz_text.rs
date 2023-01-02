#![no_main]
use jomini::{
    text::{ArrayReader, ObjectReader, ValueReader},
    Encoding, TextDeserializer, TextToken,
};
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

static mut GROUPED: bool = false;

fn read_value<E>(value: ValueReader<E>)
where
    E: crate::Encoding + Clone,
{
    match value.token() {
        TextToken::Object { .. } => {
            let obj = value.read_object().unwrap();
            if unsafe { GROUPED } {
                iterate_object2(obj);
            } else {
                iterate_object(obj);
            }
        }
        TextToken::Array { .. } => {
            iterate_array(value.read_array().unwrap());
        }
        TextToken::End(_) => panic!("end!?"),
        TextToken::Operator(_) => {}
        TextToken::MixedContainer => {}
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
    let mut fields = reader.fields();
    for (key, _op, value) in fields.by_ref() {
        let _ = key.read_str();
        read_value(value);
    }
}

fn iterate_object2<E>(reader: ObjectReader<E>)
where
    E: crate::Encoding + Clone,
{
    let mut fields = reader.field_groups();
    for (_key, entries) in fields.by_ref() {
        for (_op, value) in entries.values() {
            read_value(value);
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let _: Result<Meta, _> = jomini::TextTape::from_slice(&data).and_then(|tape| {
        let tokens = tape.tokens();
        for (i, token) in tokens.iter().enumerate() {
            match token {
                TextToken::Array { end: ind, .. }
                | TextToken::Object { end: ind, .. }
                | TextToken::End(ind)
                    if *ind == 0 =>
                {
                    panic!("zero ind encountered");
                }
                TextToken::Array { end: ind, .. } | TextToken::Object { end: ind, .. } => {
                    match &tokens[*ind] {
                        TextToken::End(ind2) => {
                            assert_eq!(*ind2, i)
                        }
                        x => panic!("expected end not {:?}", x),
                    }
                }
                _ => {}
            }
        }

        #[cfg(feature = "json")]
        tape.windows1252_reader().json().to_string();

        iterate_object(tape.windows1252_reader());

        unsafe {
            GROUPED = true;
        }
        iterate_object2(tape.windows1252_reader());
        TextDeserializer::from_windows1252_tape(&tape).deserialize()
    });
});
