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

    // Fuzz equality between the lexer and reader when the reader has a buffer
    // large enough to hold all possible binary tokens
    let mut lexer = jomini::binary::Lexer::new(data);
    let mut reader = jomini::binary::TokenReader::builder()
        .buffer_len(usize::from(u16::MAX) + 4)
        .build(data);

    loop {
        match (lexer.next_token(), reader.next()) {
            (Ok(None), Ok(None)) => {
                break;
            }
            (Ok(Some(t1)), Ok(Some(t2))) => assert_eq!(t1, t2),
            (Err(e1), Err(e2)) => match e2.kind() {
                jomini::binary::ReaderErrorKind::Lexer(e) => {
                    assert_eq!(e1.kind(), e);
                    break;
                }
                _ => panic!("different errors"),
            },
            (x, y) => panic!("{:?} {:?}", x, y),
        }
    }

    // Fuzz equality when reader doesn't have enough space to hold everything
    let mut lexer = jomini::binary::Lexer::new(data);
    let buffer_len = 100;
    let mut reader = jomini::binary::TokenReader::builder()
        .buffer_len(buffer_len + 4)
        .build(data);

    loop {
        match (lexer.read_token(), reader.read()) {
            (Ok(t1), Ok(t2)) => assert_eq!(t1, t2),
            (Ok(jomini::binary::Token::Quoted(t) | jomini::binary::Token::Unquoted(t)), _)
                if t.as_bytes().len() >= buffer_len =>
            {
                break;
            }
            (Err(_), Err(_)) => {
                break;
            }
            (x, y) => panic!("{:?} {:?}", x, y),
        }
    }

    // Fuzz tape parsing
    let ores = jomini::BinaryTape::from_slice(data);

    // Fuzz binary deserializers
    let _: Result<Meta, _> = jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_slice(data, &hash)
        .deserialize();

    let _: Result<Meta, _> = jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
        .from_reader(data, &hash)
        .deserialize();

    if let Ok(tape) = &ores {
        let _: Result<Meta, _> = jomini::BinaryDeserializer::builder_flavor(BinaryTestFlavor)
            .from_tape(tape, &hash)
            .deserialize();
    }

    // Fuzz structure of binary AST
    if let Ok(tape) = &ores {
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
    }

    #[cfg(unoptimized_build)]
    {
        let mut utape = jomini::BinaryTape::default();
        let ures =
            jomini::binary::BinaryTapeParser.parse_slice_into_tape_unoptimized(&data, &mut utape);

        match (ures, ores) {
            (Ok(t1), Ok(t2)) => assert_eq!(t1.tokens(), t2.tokens()),
            (Err(_), Err(_)) => {
                break;
            }
            (x, y) => panic!("{:?} {:?}", x, y),
        }
    }
});
