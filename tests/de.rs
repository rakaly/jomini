#![cfg(feature = "derive")]

use jomini::common::PdsDate;
use jomini::{BinaryDeserializer, BinaryFlavor, Encoding, TextDeserializer, Windows1252Encoding};
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer,
};
use std::collections::HashMap;
use std::fmt;
use std::{borrow::Cow, marker::PhantomData};

/// A UTF-8 little endian binary IEEE 754-2008 identity flavor
///
/// This is an example flavor. Create different flavors depending
/// on the game (eg: eu4, ck3, etc). No game currently uses this
/// exact flavor (though they should).
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

#[test]
fn same_deserializer_for_header_token() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyStruct {
        color: Color,
    }

    #[derive(Debug, PartialEq)]
    struct Color {
        red: u8,
        blue: u8,
        green: u8,
    }

    let bin_data = [
        0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00, 0x14,
        0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
    ];

    let mut map = HashMap::new();
    map.insert(0x053a, "color");

    let txt_data = b"color = rgb { 110 27 27 }";

    let bin_out: MyStruct = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice(&bin_data[..], &map)
        .unwrap();
    let txt_out: MyStruct = TextDeserializer::from_windows1252_slice(&txt_data[..]).unwrap();
    assert_eq!(bin_out, txt_out);
    assert_eq!(
        bin_out,
        MyStruct {
            color: Color {
                red: 110,
                blue: 27,
                green: 27,
            }
        }
    );

    impl<'de> Deserialize<'de> for Color {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            struct ColorVisitor;

            impl<'de> Visitor<'de> for ColorVisitor {
                type Value = Color;

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a color")
                }

                fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: de::SeqAccess<'de>,
                {
                    let ty = seq.next_element::<&str>()?.expect("value type");
                    match ty {
                        "rgb" => {
                            let (red, green, blue) =
                                seq.next_element::<(u8, u8, u8)>()?.expect("rgb channels");
                            Ok(Color { red, green, blue })
                        }
                        _ => panic!("unexpected color type"),
                    }
                }
            }

            deserializer.deserialize_seq(ColorVisitor)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct SaveVersion(pub String);

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Meta {
    date: jomini::common::Date,
    save_game: String,
    player: String,
    displayed_country_name: String,
    savegame_version: SaveVersion,
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

struct Stringer<'a>(pub Cow<'a, str>);

impl<'de: 'a, 'a> Deserialize<'de> for Stringer<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct StringerVisitor<'a>(PhantomData<&'a ()>);

        impl<'de: 'a, 'a> de::Visitor<'de> for StringerVisitor<'a> {
            type Value = Stringer<'a>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Stringer")
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Stringer(Cow::Borrowed(v)))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                Ok(Stringer(Cow::Owned(v.to_string())))
            }

            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                // for binary saves this field is an integer so we convert it to string
                Ok(Stringer(Cow::Owned(v.to_string())))
            }
        }

        deserializer.deserialize_str(StringerVisitor(PhantomData))
    }
}

impl<'de> Deserialize<'de> for SaveVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SaveVersionVisitor;

        impl<'de> de::Visitor<'de> for SaveVersionVisitor {
            type Value = SaveVersion;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct SaveVersion with arbitrary fields")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut version = String::new();
                while let Some(key) = map.next_key::<&str>()? {
                    match key {
                        "first" | "second" | "third" => {
                            version.push_str(map.next_value::<Stringer>()?.0.as_ref());
                            version.push('.');
                        }
                        "forth" => {
                            version.push_str(map.next_value::<Stringer>()?.0.as_ref());
                        }
                        _ => {
                            map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                Ok(SaveVersion(version))
            }
        }

        deserializer.deserialize_map(SaveVersionVisitor)
    }
}

#[test]
fn test_text_deserialization() {
    let data = include_bytes!("./fixtures/meta.txt");
    let actual: Meta =
        jomini::TextDeserializer::from_windows1252_slice(&data["EU4txt".len()..]).unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1444.11.11")
    );
    assert_eq!(actual.savegame_version.0, String::from("1.28.3.0"));
}

#[test]
fn test_scalar_u64_overflow_crash() {
    let data = include_bytes!("./fixtures/meta.txt.crash");
    let actual: Result<Meta, _> = jomini::TextDeserializer::from_windows1252_slice(&data[..]);
    assert!(actual.is_err());
}

#[test]
fn test_text_de_non_scalar_crash() {
    let data = include_bytes!("./fixtures/meta.txt.crash2");
    let actual: Result<Meta, _> = jomini::TextDeserializer::from_windows1252_slice(&data[..]);
    assert!(actual.is_err());
}

fn create_bin_lookup() -> HashMap<u16, &'static str> {
    let mut hash = HashMap::new();
    hash.insert(0x000bu16, "id");
    hash.insert(0x284du16, "date");
    hash.insert(0x2c69u16, "save_game");
    hash.insert(0x2a38u16, "player");
    hash.insert(0x2ec9u16, "savegame_version");
    hash.insert(0x28e2u16, "first");
    hash.insert(0x28e3u16, "second");
    hash.insert(0x2ec7u16, "third");
    hash.insert(0x2ec8u16, "forth");
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
    let data = include_bytes!("./fixtures/meta.bin");
    let data = &data["EU4bin".len()..];
    let hash = create_bin_lookup();
    let actual: Meta = jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice(&data, &hash)
        .unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1597.1.15")
    );
    assert_eq!(actual.savegame_version.0, String::from("1.29.4.0"));
}

#[test]
fn test_binary_meta_deserialization_boxed() {
    let data = include_bytes!("./fixtures/meta.bin");
    let data = &data["EU4bin".len()..];
    let hash = Box::new(create_bin_lookup());
    let flavor = Box::new(BinaryTestFlavor);
    let actual: Meta = jomini::BinaryDeserializer::builder_flavor(&flavor)
        .from_slice(&data, &hash)
        .unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1597.1.15")
    );
    assert_eq!(actual.savegame_version.0, String::from("1.29.4.0"));
}

#[test]
fn test_binary_slice_index_crash() {
    let data = include_bytes!("./fixtures/meta.bin.crash");
    let hash = create_bin_lookup();
    assert!(jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice::<_, Meta>(&data[..], &hash)
        .is_err());
}

#[test]
fn test_binary_incomplete_array() {
    let data = include_bytes!("./fixtures/meta.bin.crash2");
    let hash = create_bin_lookup();
    assert!(jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice::<_, Meta>(&data[..], &hash)
        .is_err());
}

#[test]
fn test_binary_heterogenous_object_crash() {
    let data = include_bytes!("./fixtures/meta.bin.crash3");
    let hash = create_bin_lookup();
    assert!(jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice::<_, Meta>(&data[..], &hash)
        .is_err());
}

#[test]
fn test_binary_unknown_key_object() {
    let data = include_bytes!("./fixtures/meta.bin.crash4");
    let hash = create_bin_lookup();
    assert!(jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice::<_, Meta>(&data[..], &hash)
        .is_err());
}

#[test]
fn test_binary_timeout() {
    let data = include_bytes!("./fixtures/bin-timeout");
    let hash = create_bin_lookup();
    assert!(jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice::<_, Meta>(&data[..], &hash)
        .is_err());
}
