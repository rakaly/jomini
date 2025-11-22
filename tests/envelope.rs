#![cfg(feature = "envelope")]
use jomini::{
    Encoding, Utf8Encoding,
    binary::{
        BinaryDeserializerBuilder, BinaryFlavor, Token, TokenResolver, de::BinaryReaderDeserializer,
    },
    envelope::{
        BinaryEncoding, JominiFile, JominiFileKind, SaveContent, SaveContentKind, SaveData,
        SaveDataKind, SaveHeaderKind, SaveMetadata, SaveMetadataKind,
    },
};
use rawzip::ReaderAt;
use serde::Deserialize;
use std::{collections::HashMap, io::Read};

#[derive(Debug, PartialEq, Deserialize)]
struct Meta {
    meta: bool,
}

#[derive(Debug, PartialEq, Deserialize)]
struct Gamestate {
    meta: bool,
    gamestate: String,
}

#[derive(Debug, PartialEq, Deserialize)]
struct BinaryMeta {
    meta: u32,
}

#[derive(Debug, PartialEq, Deserialize)]
struct BinaryGamestate {
    meta: u32,
    gamestate: u32,
}

// Expected test data - text format
const EXPECTED_TEXT_META: &str = "meta=yes\n";
const EXPECTED_TEXT_GAMESTATE: &str = "meta=yes\ngamestate=\"hi\"\n";

// Expected test data - binary format
const EXPECTED_BINARY_META_TOKENS: &[Token] =
    &[Token::Id(0x1000), Token::Equal, Token::U32(0xdddd)];

const EXPECTED_BINARY_GAMESTATE_TOKENS: &[Token] = &[
    Token::Id(0x1000),
    Token::Equal,
    Token::U32(0xdddd),
    Token::Id(0x2000),
    Token::Equal,
    Token::U32(0xffff),
];

const EXPECTED_BINARY_META: BinaryMeta = BinaryMeta { meta: 0xdddd };
const EXPECTED_BINARY_GAMESTATE: BinaryGamestate = BinaryGamestate {
    meta: 0xdddd,
    gamestate: 0xffff,
};

fn plaintext_uncompressed_assertions(file: JominiFile<impl ReaderAt>) {
    // We can unwrap the file and work more effectively with the data
    let JominiFileKind::Uncompressed(SaveDataKind::Text(data_txt)) = file.kind() else {
        panic!("expected text uncompressed envelope");
    };

    let expected_gamestate = Gamestate {
        meta: true,
        gamestate: "hi".to_string(),
    };

    // Can extract meta
    let mut meta = data_txt.meta();
    let mut buf = String::new();
    meta.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_META);

    // And deserialize it
    let meta: Meta = data_txt.meta().deserializer().deserialize().unwrap();
    assert_eq!(meta, Meta { meta: true });

    // Can read gamestate
    let mut gamestate = data_txt.body().cursor();
    let mut buf = String::new();
    gamestate.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_GAMESTATE);

    // Can deserialize gamestate
    let gamestate: Gamestate = data_txt.deserializer().deserialize().unwrap();
    assert_eq!(gamestate, expected_gamestate);

    // But we also don't have to unwrap it and instead can work with the file directly

    // Can extract meta
    let mut meta = file.meta().unwrap();
    let mut buf = String::new();
    meta.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_META);

    // Can deserialize meta
    let meta = file.meta().unwrap();
    let SaveMetadataKind::Text(mut meta_txt) = meta else {
        panic!("expected text meta");
    };
    let meta: Meta = meta_txt.deserializer().deserialize().unwrap();
    assert_eq!(meta, Meta { meta: true });

    let mut gamestate = file.gamestate().unwrap();
    let mut buf = String::new();
    gamestate.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_GAMESTATE);

    let gamestate = file.gamestate().unwrap();
    let SaveContentKind::Text(mut gamestate_txt) = gamestate else {
        panic!("expected text gamestate");
    };
    let gamestate: Gamestate = gamestate_txt.deserializer().deserialize().unwrap();
    assert_eq!(gamestate, expected_gamestate);
}

fn binary_uncompressed_assertions(file: JominiFile<impl ReaderAt>) {
    // We can unwrap the file and work more effectively with the data
    let JominiFileKind::Uncompressed(SaveDataKind::Binary(data_bin)) = file.kind() else {
        panic!("expected binary uncompressed envelope");
    };

    let resolver = HashMap::<u16, &str>::from([(0x1000, "meta"), (0x2000, "gamestate")]);

    // Can extract meta
    let meta = data_bin.meta();
    expected_tokens(EXPECTED_BINARY_META_TOKENS, meta);

    // Can extract meta and deserialize
    let mut meta = data_bin.meta();
    let actual: BinaryMeta = meta.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_META);

    // Can read gamestate
    let gamestate = data_bin.body().cursor();
    expected_tokens(EXPECTED_BINARY_GAMESTATE_TOKENS, gamestate);

    // Can binary deserialize gamestate
    let mut gamestate = data_bin.body().cursor();
    let actual: BinaryGamestate = BinaryDeserializerBuilder::with_flavor(TestFlavor)
        .from_reader(&mut gamestate, &resolver)
        .deserialize()
        .unwrap();
    assert_eq!(actual, EXPECTED_BINARY_GAMESTATE);

    // But we also don't have to unwrap it and instead can work with the file directly

    // Can extract meta
    let meta = file.meta().unwrap();
    expected_tokens(EXPECTED_BINARY_META_TOKENS, meta);

    // Can binary deserialize meta
    let SaveMetadataKind::Binary(mut meta) = file.meta().unwrap() else {
        panic!("expected binary meta");
    };
    let actual: BinaryMeta = meta.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_META);

    let gamestate = file.gamestate().unwrap();
    expected_tokens(EXPECTED_BINARY_GAMESTATE_TOKENS, gamestate);

    // Can binary deserialize gamestate
    let SaveContentKind::Binary(mut gamestate) = file.gamestate().unwrap() else {
        panic!("expected binary gamestate");
    };
    let actual: BinaryGamestate = gamestate.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_GAMESTATE);
}

#[test]
fn plaintext_extraction() {
    let file = std::fs::File::open("tests/fixtures/envelopes/text.txt").unwrap();
    let file = JominiFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::Text);
    plaintext_uncompressed_assertions(file);

    let file = std::fs::read("tests/fixtures/envelopes/text.txt").unwrap();
    let file = JominiFile::from_slice(&file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::Text);
    plaintext_uncompressed_assertions(file);
}

fn expected_tokens(expected: &[Token], reader: impl std::io::Read) {
    let mut reader = jomini::binary::TokenReader::new(reader);
    let mut i = 0;
    while let Some(token) = reader.next().unwrap() {
        let expected_token = &expected[i];
        assert_eq!(&token, expected_token);
        i += 1;
    }
    assert_eq!(i, expected.len());
}

fn zip_binary_assertions(file: JominiFile<impl ReaderAt>) {
    // We can unwrap the file and work more effectively with the data
    let JominiFileKind::Zip(kind) = file.kind() else {
        panic!("expected zip binary envelope");
    };

    let resolver = HashMap::<u16, &str>::from([(0x1000, "meta"), (0x2000, "gamestate")]);

    // Can extract meta
    let meta = kind.meta().unwrap();
    expected_tokens(EXPECTED_BINARY_META_TOKENS, meta);

    // Can extract meta by matching on kind too
    let SaveMetadataKind::Binary(meta) = kind.meta().unwrap() else {
        panic!("expected binary meta");
    };

    expected_tokens(EXPECTED_BINARY_META_TOKENS, meta);

    // Can binary deserialize save meta
    let SaveMetadataKind::Binary(mut meta) = kind.meta().unwrap() else {
        panic!("expected binary meta");
    };
    let actual: BinaryMeta = meta.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_META);

    // Can read gamestate
    let gamestate = kind.gamestate().unwrap();
    expected_tokens(EXPECTED_BINARY_GAMESTATE_TOKENS, gamestate);

    // Can binary deserialize gamestate
    let SaveContentKind::Binary(mut gamestate) = kind.gamestate().unwrap() else {
        panic!("expected binary gamestate");
    };
    let actual: BinaryGamestate = gamestate.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_GAMESTATE);

    // But we also don't have to unwrap it and instead can work with the file directly

    // Can extract meta
    let meta = file.meta().unwrap();
    expected_tokens(EXPECTED_BINARY_META_TOKENS, meta);

    // Can binary deserialize meta
    let SaveMetadataKind::Binary(mut meta) = file.meta().unwrap() else {
        panic!("expected binary meta");
    };
    let actual: BinaryMeta = meta.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_META);

    let gamestate = file.gamestate().unwrap();
    expected_tokens(EXPECTED_BINARY_GAMESTATE_TOKENS, gamestate);

    // Can binary deserialize gamestate
    let SaveContentKind::Binary(mut gamestate) = file.gamestate().unwrap() else {
        panic!("expected binary gamestate");
    };
    let actual: BinaryGamestate = gamestate.deserializer(&resolver).deserialize().unwrap();
    assert_eq!(actual, EXPECTED_BINARY_GAMESTATE);
}

fn zip_text_assertions(file: JominiFile<impl ReaderAt>) {
    // We can unwrap the file and work more effectively with the data
    let JominiFileKind::Zip(kind) = file.kind() else {
        panic!("expected zip text envelope");
    };

    // Extract and validate metadata (ZIP text may have empty or actual metadata)
    let SaveMetadataKind::Text(mut meta) = kind.meta().unwrap() else {
        panic!("expected text meta");
    };
    // Just verify we can extract it, content may vary for ZIP text format
    let mut buf = String::new();
    let _ = meta.read_to_string(&mut buf);

    // Can extract gamestate
    let mut gamestate = kind.gamestate().unwrap();
    let mut buf = String::new();
    gamestate.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_GAMESTATE);

    // Can text deserialize gamestate
    let SaveContentKind::Text(mut gamestate) = kind.gamestate().unwrap() else {
        panic!("expected text gamestate");
    };
    let expected_gamestate = Gamestate {
        meta: true,
        gamestate: "hi".to_string(),
    };
    let actual: Gamestate = gamestate.deserializer().deserialize().unwrap();
    assert_eq!(actual, expected_gamestate);

    // But we also don't have to unwrap it and instead can work with the file directly

    // Can extract meta
    let SaveMetadataKind::Text(_meta) = file.meta().unwrap() else {
        panic!("expected text meta");
    };

    // Can deserialize meta
    let SaveMetadataKind::Text(mut meta_txt) = file.meta().unwrap() else {
        panic!("expected text meta");
    };
    let meta: Meta = meta_txt.deserializer().deserialize().unwrap();
    assert_eq!(meta, Meta { meta: true });

    let mut gamestate = file.gamestate().unwrap();
    let mut buf = String::new();
    gamestate.read_to_string(&mut buf).unwrap();
    assert_eq!(&buf, EXPECTED_TEXT_GAMESTATE);

    let SaveContentKind::Text(mut gamestate) = file.gamestate().unwrap() else {
        panic!("expected text gamestate");
    };
    let gamestate: Gamestate = gamestate.deserializer().deserialize().unwrap();
    assert_eq!(gamestate, expected_gamestate);
}

#[test]
fn zip_bin_compressed_meta() {
    let file = std::fs::File::open("tests/fixtures/envelopes/split.zip").unwrap();
    let file = JominiFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::SplitBinary);
    zip_binary_assertions(file);

    let file = std::fs::read("tests/fixtures/envelopes/split.zip").unwrap();
    let file = JominiFile::from_slice(&file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::SplitBinary);
    zip_binary_assertions(file);
}

#[test]
fn zip_bin_uncompressed_meta() {
    let file = std::fs::File::open("tests/fixtures/envelopes/header.zip").unwrap();
    let file = JominiFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    zip_binary_assertions(file);

    let file = std::fs::read("tests/fixtures/envelopes/header.zip").unwrap();
    let file = JominiFile::from_slice(&file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    zip_binary_assertions(file);
}

#[test]
fn bin_uncompressed() {
    // a file that mimics CK3 binary autosave
    let file = std::fs::File::open("tests/fixtures/envelopes/autosave.bin").unwrap();
    let file = JominiFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::Binary);
    binary_uncompressed_assertions(file);

    let file = std::fs::read("tests/fixtures/envelopes/autosave.bin").unwrap();
    let file = JominiFile::from_slice(&file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::Binary);
    binary_uncompressed_assertions(file);
}

#[test]
fn zip_txt() {
    // a file that mimics CK3 text autosave in ZIP
    let file = std::fs::File::open("tests/fixtures/envelopes/text.zip").unwrap();
    let file = JominiFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);
    zip_text_assertions(file);

    let file = std::fs::read("tests/fixtures/envelopes/text.zip").unwrap();
    let file = JominiFile::from_slice(&file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);
    zip_text_assertions(file);
}

#[test]
fn envelope_vec() {
    // test to ensure that passing in a Vec<u8> still has full functionality
    let file = std::fs::read("tests/fixtures/envelopes/text.zip").unwrap();
    let file = JominiFile::from_slice(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);
    zip_text_assertions(file);
}

#[test]
fn malformed_zip_doesnt_panic() {
    // Regression test for invalid metadata range with malformed ZIP central directory
    // This file has a ZIP with an invalid central directory offset that would previously
    // cause an integer underflow panic when accessing metadata
    let data = std::fs::read("tests/fixtures/envelopes/malformed_zip.bin").unwrap();
    let file = JominiFile::from_slice(&data).unwrap();

    let header = file.header();
    assert_eq!(header.kind(), SaveHeaderKind::UnifiedText);

    // Verify we can attempt to access metadata without panicking
    if let Ok(mut meta) = file.meta() {
        let _bytes_read = std::io::copy(&mut meta, &mut std::io::sink());
    }

    // Verify we can attempt to access gamestate without panicking
    if let Ok(mut gamestate) = file.gamestate() {
        let _bytes_read = std::io::copy(&mut gamestate, &mut std::io::sink());
    }
}

struct TestFlavor;

impl Encoding for TestFlavor {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

impl BinaryFlavor for TestFlavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_le_bytes(data)
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        f64::from_le_bytes(data)
    }
}

type BinaryDeserializer<'res, RES, R> = BinaryReaderDeserializer<'res, RES, TestFlavor, R>;

trait DeserializeBinary {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> BinaryDeserializer<'res, RES, impl Read + '_>;
}

impl<R: ReaderAt> DeserializeBinary for SaveData<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> BinaryDeserializer<'res, RES, impl Read + '_> {
        BinaryDeserializerBuilder::with_flavor(TestFlavor)
            .from_reader(self.body().cursor(), resolver)
    }
}

impl<R: Read> DeserializeBinary for SaveContent<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> BinaryDeserializer<'res, RES, impl Read + '_> {
        BinaryDeserializerBuilder::with_flavor(TestFlavor).from_reader(self, resolver)
    }
}

impl<R: Read> DeserializeBinary for SaveMetadata<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> BinaryDeserializer<'res, RES, impl Read + '_> {
        BinaryDeserializerBuilder::with_flavor(TestFlavor).from_reader(self, resolver)
    }
}
