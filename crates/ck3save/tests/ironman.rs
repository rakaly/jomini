use ck3save::{
    models::{Gamestate, Header},
    BasicTokenResolver, Ck3BinaryDeserialization, Ck3File, Ck3Melt, DeserializeCk3, JominiFileKind,
    MeltOptions, SaveDataKind, SaveHeaderKind,
};
use highway::{HighwayHash, HighwayHasher};
use jomini::binary::TokenResolver;
use std::{
    io::{BufWriter, Cursor, Read},
    sync::LazyLock,
};

mod utils;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("ck3");
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
fn test_ck3_binary_header() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/header.bin");
    let mut file = Ck3File::from_slice(&data[..]).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);

    let JominiFileKind::Uncompressed(SaveDataKind::Binary(bin)) = file.kind_mut() else {
        panic!("expected binary");
    };
    let header: Header = (&*bin)
        .deserializer(&*TOKENS)
        .unwrap()
        .deserialize()
        .unwrap();
    assert_eq!(header.meta_data.version, String::from("1.0.2"));
}

#[test]
fn test_ck3_binary_save() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    let file = utils::request_file("af_Munso_867_Ironman.ck3");
    let file = Ck3File::from_file(file)?;
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    let game: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(game.meta_data.version, String::from("1.0.2"));
    Ok(())
}

#[test]
fn test_ck3_binary_compressed_header() {
    skip_if_no_tokens!();
    let file = utils::request_file("von_konigsberg_867_01_01.ck3");
    let file = Ck3File::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::SplitBinary);
    let JominiFileKind::Zip(zip) = file.kind() else {
        panic!("unexpected type");
    };

    let mut meta = zip.meta().unwrap();
    let mut out = Cursor::new(Vec::new());
    meta.melt(MeltOptions::new(), &*TOKENS, &mut out).unwrap();
    memchr::memmem::find(out.get_ref(), b"meta_real_date=124.11.1").unwrap();
}

#[test]
fn test_ck3_binary_autosave() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("autosave.ck3");

    let file = Ck3File::from_file(file)?;
    assert_eq!(file.header().kind(), SaveHeaderKind::Binary);

    let game: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(game.meta_data.version, String::from("1.0.2"));

    let JominiFileKind::Uncompressed(SaveDataKind::Binary(ref binary)) = file.kind() else {
        panic!("unexpected type");
    };

    let header: Header = (&*binary).deserializer(&*TOKENS).unwrap().deserialize()?;
    assert_eq!(header.meta_data.version, String::from("1.0.2"));

    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;
    memchr::memmem::find(out.get_ref(), b"gold=0.044").unwrap();
    memchr::memmem::find(out.get_ref(), b"gold=4.647").unwrap();

    Ok(())
}

#[test]
fn test_ck3_binary_save_tokens() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("af_Munso_867_Ironman.ck3");
    let file = Ck3File::from_file(file)?;
    let save: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    assert_eq!(save.meta_data.version, String::from("1.0.2"));
    Ok(())
}

#[test]
fn test_roundtrip_header_melt() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/header.bin");
    let file = Ck3File::from_slice(&data[..]).unwrap();
    let JominiFileKind::Uncompressed(SaveDataKind::Binary(ref binary)) = file.kind() else {
        panic!("unexpected type");
    };
    let mut out = Cursor::new(Vec::new());
    (&*binary)
        .melt(MeltOptions::new(), &*TOKENS, &mut out)
        .unwrap();

    let file = Ck3File::from_slice(out.get_ref()).unwrap();
    let JominiFileKind::Uncompressed(SaveDataKind::Text(ref text)) = file.kind() else {
        panic!("unexpected type");
    };
    let header: Header = text.deserializer().deserialize().unwrap();

    assert_eq!(file.header().kind(), SaveHeaderKind::Text);
    assert_eq!(header.meta_data.version, String::from("1.0.2"));
}

#[test]
fn test_header_melt() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/header.bin");
    let file = Ck3File::from_slice(&data[..]).unwrap();
    let JominiFileKind::Uncompressed(SaveDataKind::Binary(ref binary)) = file.kind() else {
        panic!("unexpected type");
    };
    let mut out = Cursor::new(Vec::new());
    (&*binary)
        .melt(MeltOptions::new(), &*TOKENS, &mut out)
        .unwrap();

    let melted = include_bytes!("fixtures/header.melted");
    assert_eq!(
        &melted[..],
        out.get_ref().as_slice(),
        "header did not melt correctly"
    );
}

#[test]
fn test_melt_no_crash() {
    skip_if_no_tokens!();
    let data = include_bytes!("fixtures/melt.crash1");
    assert!(Ck3File::from_slice(&data[..]).is_err());
}

#[test]
fn test_ck3_binary_save_patch_1_3() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("ck3-1.3-test.ck3");
    let file = Ck3File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;

    let file = Ck3File::from_slice(out.get_ref())?;
    let save: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(save.meta_data.version, String::from("1.3.0"));
    Ok(())
}

#[test]
fn test_ck3_1_0_3_old_cloud_and_local_tokens() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("ck3-1.0.3-local.ck3");
    let file = Ck3File::from_file(file)?;
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;

    let file = Ck3File::from_slice(out.get_ref())?;
    assert_eq!(file.header().kind(), SaveHeaderKind::Text);
    let save: Gamestate = (&file).deserialize(&*TOKENS).unwrap();

    assert_eq!(save.meta_data.version, String::from("1.0.3"));
    Ok(())
}

#[test]
fn decode_and_melt_gold_correctly() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("ck3-1.3.1.ck3");
    let file = Ck3File::from_file(file)?;
    let save: Gamestate = (&file).deserialize(&*TOKENS).unwrap();

    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);

    let character = save.living.get(&16322).unwrap();
    assert_eq!(
        character.alive_data.as_ref().and_then(|x| x.health),
        Some(4.728)
    );
    assert_eq!(
        character.alive_data.as_ref().and_then(|x| x.income),
        Some(11.087)
    );
    assert_eq!(
        character
            .alive_data
            .as_ref()
            .and_then(|x| x.gold.as_ref())
            .map(|x| x.value()),
        Some(133.04397)
    );

    let mut out = Cursor::new(Vec::new());
    let file = utils::request_file("ck3-1.3.1.ck3");
    let file = Ck3File::from_file(file)?;
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;

    memchr::memmem::find(out.get_ref(), b"gold=133.04397").unwrap();
    memchr::memmem::find(out.get_ref(), b"vassal_power_value=200").unwrap();
    Ok(())
}

#[test]
fn parse_patch16() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    let file = utils::request_file("ck3-1.6.ck3");
    let file = Ck3File::from_file(file)?;
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedBinary);
    let save: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(save.meta_data.version.as_str(), "1.6.0");
    Ok(())
}

#[test]
fn melt_patch14() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("ck3-1.4-normal.ck3");
    let file = Ck3File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;

    let checksum = HighwayHasher::default().hash256(out.get_ref());
    let hex = format!(
        "0x{:016x}{:016x}{:016x}{:016x}",
        checksum[0], checksum[1], checksum[2], checksum[3]
    );
    assert_eq!(
        hex, "0xd731c320e2968e28cf7d2642d6a456b3d97b614c734bf4d9d0f6fb3acb1a3ee7",
        "patch 1.4 slice did not melt to expected checksum"
    );
    Ok(())
}

#[test]
fn melt_patch15() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("ck3-1.5-normal.ck3");
    let file = Ck3File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;

    let checksum = HighwayHasher::default().hash256(out.get_ref());
    let hex = format!(
        "0x{:016x}{:016x}{:016x}{:016x}",
        checksum[0], checksum[1], checksum[2], checksum[3]
    );
    assert_eq!(
        hex, "0x6b01e43ba332ead0350af9c08372792ece74005268014fbff1c597c8d774ed7e",
        "patch 1.5 slice did not melt to expected checksum"
    );
    Ok(())
}

#[test]
fn melt_patch15_slice() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let mut file = utils::request_file("ck3-1.5-normal.ck3");
    let mut content = Vec::new();
    file.read_to_end(&mut content)?;
    let file = Ck3File::from_slice(&content)?;
    let mut out = Cursor::new(Vec::new());
    (&file).melt(MeltOptions::new(), &*TOKENS, &mut out)?;
    let checksum = HighwayHasher::default().hash256(out.get_ref());
    let hex = format!(
        "0x{:016x}{:016x}{:016x}{:016x}",
        checksum[0], checksum[1], checksum[2], checksum[3]
    );
    assert_eq!(
        hex, "0x6b01e43ba332ead0350af9c08372792ece74005268014fbff1c597c8d774ed7e",
        "patch 1.5 slice did not melt to expected checksum"
    );

    Ok(())
}

#[test]
fn parse_patch_1_16_slice() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let mut file = utils::request_file("patch_1_16.ck3");
    let mut content = Vec::new();
    file.read_to_end(&mut content)?;
    let file = Ck3File::from_slice(&content)?;
    let result: Gamestate = (&file).deserialize(&*TOKENS).unwrap();
    assert_eq!(result.meta_data.version, String::from("1.16.2.3"));
    Ok(())
}

#[test]
fn patch_1_16_meta_melt() -> Result<(), Box<dyn std::error::Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }
    let file = utils::request_file("patch_1_16.ck3");
    let file = Ck3File::from_file(file)?;
    let JominiFileKind::Zip(ck3_zip) = file.kind() else {
        panic!("expected a zip file");
    };

    let mut meta = ck3_zip.meta().unwrap();

    let hasher = highway::HighwayHasher::default();
    let mut writer = BufWriter::with_capacity(0x8000, hasher);
    meta.melt(MeltOptions::new(), &*TOKENS, &mut writer)?;
    let hash = writer.into_inner().unwrap().finalize256();
    let hex = format!(
        "0x{:016x}{:016x}{:016x}{:016x}",
        hash[0], hash[1], hash[2], hash[3]
    );
    assert_eq!(
        hex,
        "0x8c504b0729ac9dbd2f9172d6252bf6d523b66f66356f60ec7c479685a6fd0cf8"
    );

    let mut file = utils::request_file("patch_1_16.ck3");
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    let file = Ck3File::from_slice(&buf)?;
    let JominiFileKind::Zip(ck3_zip) = file.kind() else {
        panic!("expected a zip file");
    };

    let mut meta = ck3_zip.meta().unwrap();
    let hasher = highway::HighwayHasher::default();
    let mut writer = BufWriter::with_capacity(0x8000, hasher);
    meta.melt(MeltOptions::new(), &*TOKENS, &mut writer)?;
    let hash = writer.into_inner().unwrap().finalize256();
    let hex = format!(
        "0x{:016x}{:016x}{:016x}{:016x}",
        hash[0], hash[1], hash[2], hash[3]
    );
    assert_eq!(
        hex,
        "0x8c504b0729ac9dbd2f9172d6252bf6d523b66f66356f60ec7c479685a6fd0cf8"
    );

    Ok(())
}
