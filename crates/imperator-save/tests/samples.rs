use core::panic;
use imperator_save::{
    models::{GameState, Metadata, Save},
    BasicTokenResolver, DeserializeImperator, ImperatorBinaryDeserialization, ImperatorFile,
    ImperatorMelt, JominiFileKind, MeltOptions, SaveDataKind, SaveHeaderKind, SaveMetadataKind,
};
use jomini::binary::TokenResolver;
use std::{
    io::{Cursor, Read},
    sync::LazyLock,
};

mod utils;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("imperator");
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

macro_rules! skip_if_no_tokens {
    () => {
        if TOKENS.is_empty() {
            return;
        }
    };
}

#[test]
fn test_debug_save() {
    skip_if_no_tokens!();
    let file = utils::request_file("debug-save.rome");

    let file = ImperatorFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::Text);
    let JominiFileKind::Uncompressed(SaveDataKind::Text(text)) = file.kind() else {
        panic!("Expected a text file");
    };

    let save: Metadata = text.deserializer().deserialize().unwrap();
    assert_eq!(save.version, String::from("1.4.2"));

    let save: GameState = text.deserializer().deserialize().unwrap();
    assert_eq!(save.speed, 2);
}

#[test]
fn test_observer_save() {
    skip_if_no_tokens!();
    let file = utils::request_file("observer1.5.rome");
    let file = ImperatorFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);

    let JominiFileKind::Zip(zip) = file.kind() else {
        panic!("Expected a zip file");
    };

    let save: Metadata = match zip.meta().unwrap() {
        SaveMetadataKind::Text(mut x) => x.deserializer().deserialize(),
        SaveMetadataKind::Binary(mut x) => x.deserializer(&*TOKENS).deserialize(),
    }
    .unwrap();

    assert_eq!(save.version, String::from("1.5.3"));

    let save: Save = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(save.meta.version, String::from("1.5.3"));
}

#[test]
fn test_observer_melt() {
    skip_if_no_tokens!();
    let mut melt = utils::request_file("observer1.5_melted.rome");
    let file = utils::request_file("observer1.5.rome");
    let file = ImperatorFile::from_file(file).unwrap();
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new();
    (&file).melt(options, &*TOKENS, &mut out).unwrap();
    let mut buf = Vec::new();
    melt.read_to_end(&mut buf).unwrap();
    assert_eq!(
        &buf[..],
        out.get_ref(),
        "observer 1.5 did not melt correctly"
    );
}

#[test]
fn test_patch_20() {
    skip_if_no_tokens!();
    let file = utils::request_file("Oponia.rome");
    let file = ImperatorFile::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);

    let JominiFileKind::Zip(zip) = file.kind() else {
        panic!("Expected a zip file");
    };

    let save: Metadata = match zip.meta().unwrap() {
        SaveMetadataKind::Text(mut x) => x.deserializer().deserialize(),
        SaveMetadataKind::Binary(mut x) => x.deserializer(&*TOKENS).deserialize(),
    }
    .unwrap();
    assert_eq!(save.version, String::from("2.0.5"));

    let save: Save = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(save.meta.version, String::from("2.0.5"));
}

#[test]
fn test_patch_20_slice() {
    skip_if_no_tokens!();
    let mut file = utils::request_file("Oponia.rome");
    let mut content = Vec::new();
    file.read_to_end(&mut content).unwrap();
    let file = ImperatorFile::from_slice(&content).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);

    let JominiFileKind::Zip(zip) = file.kind() else {
        panic!("Expected a zip file");
    };

    let save: Metadata = match zip.meta().unwrap() {
        SaveMetadataKind::Text(mut x) => x.deserializer().deserialize(),
        SaveMetadataKind::Binary(mut x) => x.deserializer(&*TOKENS).deserialize(),
    }
    .unwrap();
    assert_eq!(save.version, String::from("2.0.5"));

    let save: Save = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(save.meta.version, String::from("2.0.5"));
}

#[test]
fn test_non_ascii_save() {
    skip_if_no_tokens!();
    let file = utils::request_file("non-ascii.rome");
    let file = ImperatorFile::from_file(file).unwrap();
    let save: Save = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    assert_eq!(save.meta.version, String::from("1.5.3"));
}

#[test]
fn test_roundtrip_header_melt() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/header");
    let file = ImperatorFile::from_slice(&data[..]).unwrap();

    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new();
    (&file).melt(options, &*TOKENS, &mut out).unwrap();

    let binding = out.get_ref();
    let file = ImperatorFile::from_slice(&binding).unwrap();
    let JominiFileKind::Uncompressed(SaveDataKind::Text(text)) = file.kind() else {
        panic!("Expected a text file");
    };
    let meta: Metadata = text.deserializer().deserialize().unwrap();

    assert_eq!(meta.version, String::from("1.5.3"));
}

#[test]
fn test_header_melt() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/header");
    let melted = include_bytes!("fixtures/header.melted");

    let file = ImperatorFile::from_slice(&data[..]).unwrap();
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new();
    (&file).melt(options, &*TOKENS, &mut out).unwrap();
    assert_eq!(&melted[..], out.get_ref());
}
