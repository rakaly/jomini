#![cfg(feature = "derive")]

use jomini::{
    BinaryDeserializer, Encoding, JominiDeserialize, TextDeserializer, Windows1252Encoding,
    binary::{BinaryFlavor, TokenResolver},
    common::PdsDate,
    text::TokenReader,
};
use serde::{
    Deserialize, Deserializer,
    de::{self},
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

#[derive(JominiDeserialize, Debug, PartialEq)]
struct CollectData {
    known_field: u32,
    #[jomini(collect)]
    other: HashMap<String, u32>,
}

#[derive(JominiDeserialize, Debug, PartialEq)]
struct CollectCharData {
    known_field: u32,
    #[jomini(collect)]
    other: Vec<(char, u32)>,
}

#[derive(JominiDeserialize, Debug, PartialEq)]
struct CollectFilteredData {
    known_field: u32,
    #[jomini(collect, deserialize_with = "deserialize_filtered_pair")]
    other: Vec<(String, u32)>,
}

fn deserialize_filtered_pair<'de, D>(deserializer: D) -> Result<Option<(String, u32)>, D::Error>
where
    D: Deserializer<'de>,
{
    let (key, value): (String, u32) = Deserialize::deserialize(deserializer)?;
    if key.starts_with('a') {
        Ok(Some((key, value)))
    } else {
        Ok(None)
    }
}

#[test]
fn unified_rgb_deserializer() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyStruct {
        color: [u8; 3],
    }

    let bin_data = [
        0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00, 0x14,
        0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
    ];

    let mut map = HashMap::new();
    map.insert(0x053a, "color");

    let txt_data = b"color = { 110 27 27 }";

    let bin_out: MyStruct = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &map)
        .unwrap();
    let txt_out: MyStruct = jomini::text::de::from_windows1252_slice(&txt_data[..]).unwrap();
    assert_eq!(bin_out, txt_out);
    assert_eq!(
        bin_out,
        MyStruct {
            color: [110, 27, 27]
        }
    );
}

#[test]
fn test_collect_text_tape() {
    let data = b"known_field=10 a=20 b=30";
    let actual: CollectData = jomini::text::de::from_windows1252_slice(data).unwrap();
    assert_eq!(actual.known_field, 10);
    assert_eq!(actual.other.get("a").copied(), Some(20));
    assert_eq!(actual.other.get("b").copied(), Some(30));
}

#[test]
fn test_collect_text_streaming() {
    let data = b"known_field=10 a=20 b=30";
    let mut deserializer =
        TextDeserializer::from_windows1252_reader(TokenReader::new(&data[..]));
    let actual: CollectData = deserializer.deserialize().unwrap();
    assert_eq!(actual.known_field, 10);
    assert_eq!(actual.other.get("a").copied(), Some(20));
    assert_eq!(actual.other.get("b").copied(), Some(30));
}

#[test]
fn test_collect_text_filtering() {
    let data = b"known_field=10 a=20 b=30 something_else=40";
    let actual: CollectFilteredData = jomini::text::de::from_windows1252_slice(data).unwrap();
    assert_eq!(actual.known_field, 10);
    assert_eq!(actual.other, vec![("a".to_string(), 20)]);
}

#[test]
fn test_collect_text_non_string_key() {
    let data = b"known_field=10 a=20 b=30";
    let actual: CollectCharData = jomini::text::de::from_windows1252_slice(data).unwrap();
    assert_eq!(actual.known_field, 10);
    assert_eq!(actual.other, vec![('a', 20), ('b', 30)]);
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
    let actual: Meta = jomini::text::de::from_windows1252_slice(&data["EU4txt".len()..]).unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1444.11.11")
    );
    assert_eq!(actual.savegame_version.0, String::from("1.28.3.0"));
}

#[test]
fn test_scalar_u64_overflow_crash() {
    let data = include_bytes!("./fixtures/meta.txt.crash");
    let actual: Result<Meta, _> = jomini::text::de::from_windows1252_slice(&data[..]);
    assert!(actual.is_err());
}

#[test]
fn test_text_de_non_scalar_crash() {
    let data = include_bytes!("./fixtures/meta.txt.crash2");
    let actual: Result<Meta, _> = jomini::text::de::from_windows1252_slice(&data[..]);
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
    let actual: Meta = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(data, &hash)
        .unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1597.1.15")
    );
    assert_eq!(actual.savegame_version.0, String::from("1.29.4.0"));
}

#[test]
fn test_token_attribute_deserialization() {
    #[derive(JominiDeserialize, Debug, Clone, PartialEq)]
    struct Meta {
        #[jomini(token = 0x284d)]
        date: jomini::common::Date,
        #[jomini(token = 0x2a38)]
        player: String,
    }

    let data = include_bytes!("./fixtures/meta.bin");
    let data = &data["EU4bin".len()..];
    let mut hash = create_bin_lookup();
    hash.remove(&0x284d);
    hash.remove(&0x2a38);
    let actual: Meta = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(data, &hash)
        .unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1597.1.15")
    );
    assert_eq!(&actual.player, "RAG");

    let data = include_bytes!("./fixtures/meta.txt");
    let actual: Meta = jomini::text::de::from_windows1252_slice(&data["EU4txt".len()..]).unwrap();
    assert_eq!(
        actual.date.game_fmt().to_string(),
        String::from("1444.11.11")
    );
    assert_eq!(&actual.player, "ENG");
}

#[test]
fn test_binary_meta_deserialization_boxed() {
    let data = include_bytes!("./fixtures/meta.bin");
    let data = &data["EU4bin".len()..];
    let hash = Box::new(create_bin_lookup());
    let flavor = Box::new(BinaryTestFlavor);
    let actual: Meta = BinaryDeserializer::builder_flavor(&flavor)
        .deserialize_slice(data, &hash)
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
    assert!(
        BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .deserialize_slice::<_, Meta>(&data[..], &hash)
            .is_err()
    );
}

#[test]
fn test_binary_incomplete_array() {
    let data = include_bytes!("./fixtures/meta.bin.crash2");
    let hash = create_bin_lookup();
    assert!(
        BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .deserialize_slice::<_, Meta>(&data[..], &hash)
            .is_err()
    );
}

#[test]
fn test_binary_heterogenous_object_crash() {
    let data = include_bytes!("./fixtures/meta.bin.crash3");
    let hash = create_bin_lookup();
    assert!(
        BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .deserialize_slice::<_, Meta>(&data[..], &hash)
            .is_err()
    );
}

#[test]
fn test_binary_unknown_key_object() {
    let data = include_bytes!("./fixtures/meta.bin.crash4");
    let hash = create_bin_lookup();
    assert!(
        BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .deserialize_slice::<_, Meta>(&data[..], &hash)
            .is_err()
    );
}

#[test]
fn test_binary_timeout() {
    let data = include_bytes!("./fixtures/bin-timeout");
    let hash = create_bin_lookup();
    assert!(
        BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .deserialize_slice::<_, Meta>(&data[..], &hash)
            .is_err()
    );
}

#[test]
fn test_flatten_overflow() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyStruct2 {
        a: String,
    }

    #[derive(Deserialize, Debug)]
    struct MyStruct {
        #[serde(flatten)]
        meta: MyStruct2,
    }

    let data = b"a=b d=e {c d}";
    let txt_out: MyStruct = jomini::text::de::from_windows1252_slice(&data[..]).unwrap();
    assert_eq!(txt_out.meta.a, String::from("b"));
}

#[test]
fn test_flatten_overflow2() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyStruct2 {
        a: String,
    }

    #[derive(Deserialize, Debug)]
    struct MyStruct {
        #[serde(flatten)]
        meta: MyStruct2,
    }

    let data = b"a=b d=e {c<d}";
    let txt_out: MyStruct = jomini::text::de::from_windows1252_slice(&data[..]).unwrap();
    assert_eq!(txt_out.meta.a, String::from("b"));
}

#[test]
fn test_enum_map() {
    use serde_with::{EnumMap, serde_as};

    #[serde_as]
    #[derive(Clone, Debug, Deserialize, PartialEq)]
    struct Event {
        #[serde_as(as = "EnumMap")]
        requirements: Vec<Condition>,
    }

    #[derive(Clone, Debug, Deserialize, PartialEq)]
    #[serde(rename_all = "camelCase")]
    enum Condition {
        Country(String),
        Prestige(u32),
        #[serde(other)]
        Other,
    }

    let data = b"requirements = { country = ENG test = abc prestige = 10 }";
    let txt_out: Event = jomini::text::de::from_windows1252_slice(&data[..]).unwrap();
    assert_eq!(
        txt_out.requirements,
        vec![
            Condition::Country(String::from("ENG")),
            Condition::Other,
            Condition::Prestige(10)
        ]
    );
}

struct TestResolver {
    tokens: HashMap<u16, String>,
    lookups: HashMap<u32, String>,
}

impl TokenResolver for TestResolver {
    fn resolve(&self, token: u16) -> Option<&str> {
        self.tokens.get(&token).map(|s| s.as_str())
    }

    fn lookup(&self, index: u32) -> Option<&str> {
        self.lookups.get(&index).map(|s| s.as_str())
    }
}

#[test]
fn test_collect_binary() {
    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "known_field".to_string());
    tokens.insert(0x053b, "a".to_string());
    tokens.insert(0x053c, "b".to_string());
    let resolver = TestResolver {
        tokens,
        lookups: HashMap::new(),
    };

    let bin_data = [
        0x3a, 0x05, // Id token for "known_field"
        0x01, 0x00, // EQUAL token
        0x14, 0x00, // U32 token
        0x0a, 0x00, 0x00, 0x00, // 10
        0x3b, 0x05, // Id token for "a"
        0x01, 0x00, // EQUAL token
        0x14, 0x00, // U32 token
        0x14, 0x00, 0x00, 0x00, // 20
        0x3c, 0x05, // Id token for "b"
        0x01, 0x00, // EQUAL token
        0x14, 0x00, // U32 token
        0x1e, 0x00, 0x00, 0x00, // 30
    ];

    let actual: CollectData = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(actual.known_field, 10);
    assert_eq!(actual.other.get("a").copied(), Some(20));
    assert_eq!(actual.other.get("b").copied(), Some(30));
}

#[test]
fn test_lookup_u8_to_string() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: String,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());
    let mut lookups = HashMap::new();
    lookups.insert(42, "french".to_string());
    let resolver = TestResolver { tokens, lookups };

    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x40, 0x0d, // LOOKUP_U8 token
        42,   // index
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(result.culture, "french");
}

#[test]
fn test_lookup_u16_to_string() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: String,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());
    let mut lookups = HashMap::new();
    lookups.insert(300, "prussian".to_string());
    let resolver = TestResolver { tokens, lookups };

    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x3e, 0x0d, // LOOKUP_U16 token
        0x2c, 0x01, // 300 in little-endian
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(result.culture, "prussian");
}

#[derive(Debug, PartialEq)]
struct InternedSymbol(u32);

impl<'de> Deserialize<'de> for InternedSymbol {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct SymbolVisitor;
        impl<'de> de::Visitor<'de> for SymbolVisitor {
            type Value = InternedSymbol;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a lookup index")
            }

            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E> {
                Ok(InternedSymbol(v))
            }
        }

        deserializer.deserialize_u32(SymbolVisitor)
    }
}

#[test]
fn test_lookup_u8_raw_index() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: InternedSymbol,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());

    // Binary data: field token 0x053a for "culture", EQUAL, followed by LOOKUP_U8(42)
    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x40, 0x0d, // LOOKUP_U8 token
        42,   // index
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &tokens)
        .unwrap();
    assert_eq!(result.culture.0, 42);
}

#[test]
fn test_lookup_u16_raw_index() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: InternedSymbol,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());

    // Binary data: field token 0x053a for "culture", EQUAL, followed by LOOKUP_U16(300)
    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x3e, 0x0d, // LOOKUP_U16 token
        0x2c, 0x01, // 300 in little-endian
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &tokens)
        .unwrap();
    assert_eq!(result.culture.0, 300);
}

#[test]
fn test_lookup_u24_to_string() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: String,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());
    let mut lookups = HashMap::new();
    lookups.insert(100000u32, "imperial".to_string());
    let resolver = TestResolver { tokens, lookups };

    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x41, 0x0d, // LOOKUP_U24 token (0x0d41)
        0xa0, 0x86, 0x01, // 100000 in little-endian 24-bit
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(result.culture, "imperial");
}

#[test]
fn test_lookup_u24_raw_index() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        culture: InternedSymbol,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());
    let lookups = HashMap::new();
    let resolver = TestResolver { tokens, lookups };

    let bin_data = [
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x41, 0x0d, // LOOKUP_U24 token
        0xa0, 0x86, 0x01, // 100000 in little-endian
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(result.culture.0, 100000);
}

#[test]
fn test_lookup_u24_in_nested_container() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Inner {
        culture: String,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Data {
        nested: Inner,
        name: String,
    }

    let mut tokens = HashMap::new();
    tokens.insert(0x053a, "culture".to_string());
    tokens.insert(0x053b, "nested".to_string());
    tokens.insert(0x053c, "name".to_string());
    let mut lookups = HashMap::new();
    lookups.insert(100000u32, "byzantine".to_string());
    let resolver = TestResolver { tokens, lookups };

    let bin_data = [
        0x3b, 0x05, // Id token for "nested"
        0x01, 0x00, // EQUAL token
        0x03, 0x00, // OPEN token
        0x3a, 0x05, // Id token for "culture"
        0x01, 0x00, // EQUAL token
        0x41, 0x0d, // LOOKUP_U24 token
        0xa0, 0x86, 0x01, // 100000 in little-endian
        0x04, 0x00, // CLOSE token
        0x3c, 0x05, // Id token for "name"
        0x01, 0x00, // EQUAL token
        0x0f, 0x00, // QUOTED string token
        0x04, 0x00, // String length (4)
        b't', b'e', b's', b't', // String data
    ];

    let result: Data = BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .deserialize_slice(&bin_data[..], &resolver)
        .unwrap();
    assert_eq!(result.nested.culture, "byzantine");
    assert_eq!(result.name, "test");
}
