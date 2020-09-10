#![cfg(feature = "derive")]
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Deserialize, Debug, PartialEq)]
struct Header<'a> {
    #[serde(borrow)]
    meta_data: Metadata<'a>,
}

#[derive(Deserialize, Debug, PartialEq)]
struct Metadata<'a> {
    save_game_version: i32,
    version: &'a str,
    meta_player_name: &'a str,
    meta_title_name: &'a str,

    #[serde(borrow)]
    meta_main_portrait: MainPortrait<'a>,
}

#[derive(Deserialize, Debug, PartialEq)]
struct MainPortrait<'a> {
    #[serde(alias = "type")]
    ty: &'a str,
    age: f32,
}

#[test]
fn deserialize_ck3_plain() {
    let data = include_bytes!("./fixtures/ck3-header.txt");
    let header: Header = jomini::TextDeserializer::from_utf8_slice(&data[..]).unwrap();
    assert_eq!(
        header.meta_data,
        Metadata {
            save_game_version: 3,
            version: "1.0.3",
            meta_player_name: "Chieftain Botulf",
            meta_title_name: "Chiefdom of Jåhkåmåhkke",
            meta_main_portrait: MainPortrait {
                ty: "male",
                age: 0.27
            }
        }
    )
}

#[test]
fn deserialize_ck3_binary() {
    let data = include_bytes!("./fixtures/ck3-header.bin");
    let mut hash: HashMap<u16, &str> = HashMap::new();
    hash.insert(0x3155, "meta_data");
    hash.insert(0xee, "version");
    hash.insert(0x58f, "save_game_version");
    hash.insert(0x29e6, "meta_player_name");
    hash.insert(0x29e7, "meta_title_name");
    hash.insert(0x3391, "meta_main_portrait");
    hash.insert(0xe1, "type");
    hash.insert(0x6ef, "age");
    hash.insert(0x626, "male");

    let header: Header = jomini::BinaryDeserializer::from_ck3(data, &hash).unwrap();
    assert_eq!(
        header.meta_data,
        Metadata {
            save_game_version: 3,
            version: "1.0.3",
            meta_player_name: "Chieftain Botulf",
            meta_title_name: "Chiefdom of Jåhkåmåhkke",
            meta_main_portrait: MainPortrait {
                ty: "male",
                age: 0.27
            }
        }
    )
}

#[test]
fn deserialize_ck3_plain_timeout_fix() {
    let data = include_bytes!("./fixtures/txt-timeout");
    let header: Result<Header, _> = jomini::TextDeserializer::from_utf8_slice(&data[..]);
    assert!(header.is_err());
}
