use highway::HighwayHash;
use hoi4save::{
    file::Hoi4FsFileKind, models::Hoi4Save, BasicTokenResolver, Encoding, Hoi4BinaryFormat,
    Hoi4Date, Hoi4File, MeltOptions, PdsDate,
};
use jomini::binary::{BinaryFormatDeserializer, TokenResolver};
use serde::Deserialize;
use std::{error::Error, sync::LazyLock};

mod utils;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("hoi4");
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

#[test]
fn test_hoi4_text() -> Result<(), Box<dyn Error>> {
    let file = utils::request_file("1.10-normal-text.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Plaintext);
    assert_eq!(save.player.as_deref(), Some("FRA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_hoi4_text_custom_deserialization_file() -> Result<(), Box<dyn Error>> {
    let file = utils::request_file("1.10-normal-text.hoi4");
    let hoi4file = Hoi4File::from_file(file)?;
    let Hoi4FsFileKind::Text(hoi4txt) = hoi4file.kind() else {
        panic!("expected text file kind");
    };

    #[derive(Deserialize, Debug, Clone)]
    pub struct CustomHoi4Save {
        pub date: Hoi4Date,
    }

    let save: CustomHoi4Save = hoi4txt.as_ref().deserializer().deserialize()?;
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_hoi4_normal_bin() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    let file = utils::request_file("1.10-normal.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Binary);
    assert_eq!(save.player.as_deref(), Some("FRA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_hoi4_ironman() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    let file = utils::request_file("1.10-ironman.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Binary);
    assert_eq!(save.player.as_deref(), Some("FRA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_hoi4_new_binary_format() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    let file = utils::request_file("1.17-new-ironman-format.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Binary);
    assert_eq!(save.player.as_deref(), Some("USA"));
    assert_eq!(save.date.game_fmt().to_string(), "1936.1.1.12");
    Ok(())
}

#[test]
fn test_skip_modern_fixed_point_in_nested_value() -> Result<(), Box<dyn Error>> {
    #[derive(Debug, Deserialize, PartialEq)]
    struct PlayerOnly {
        player: String,
    }

    const SAVE_VERSION: u16 = 0x349d;
    const IGNORED: u16 = 0x4000;
    const VALUE: u16 = 0x4001;
    const PLAYER: u16 = 0x4002;

    let mut data = Vec::new();
    data.extend_from_slice(&SAVE_VERSION.to_le_bytes());
    data.extend_from_slice(&0x000c_u16.to_le_bytes());
    data.extend_from_slice(&30_i32.to_le_bytes());
    data.extend_from_slice(&IGNORED.to_le_bytes());
    data.extend_from_slice(&0x0003_u16.to_le_bytes());
    data.extend_from_slice(&VALUE.to_le_bytes());
    data.extend_from_slice(&0x000d_u16.to_le_bytes());
    // The high four bytes begin with a CLOSE lexeme. A legacy four-byte skip
    // therefore terminates the ignored container early and desynchronizes.
    data.extend_from_slice(&[1, 2, 3, 4, 4, 0, 9, 9]);
    data.extend_from_slice(&0x0004_u16.to_le_bytes());
    data.extend_from_slice(&PLAYER.to_le_bytes());
    data.extend_from_slice(&0x000f_u16.to_le_bytes());
    data.extend_from_slice(&3_u16.to_le_bytes());
    data.extend_from_slice(b"USA");

    let resolver = [
        (SAVE_VERSION, "save_version"),
        (IGNORED, "ignored"),
        (VALUE, "value"),
        (PLAYER, "player"),
    ]
    .into_iter()
    .collect::<std::collections::HashMap<_, _>>();
    let mut deser = BinaryFormatDeserializer::from_slice(&data, Hoi4BinaryFormat::new(&resolver));
    let actual: PlayerOnly = deser.deserialize()?;
    assert_eq!(
        actual,
        PlayerOnly {
            player: "USA".into()
        }
    );
    Ok(())
}

#[test]
fn test_normal_roundtrip() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    use std::io::Cursor;
    let file = utils::request_file("1.10-normal.hoi4");

    let mut file = Hoi4File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new().on_failed_resolve(hoi4save::FailedResolveStrategy::Error);
    file.melt(options, &*TOKENS, &mut out)?;

    let out = out.into_inner();
    let file = Hoi4File::from_slice(&out)?;
    let save: Hoi4Save = file.parse_save(&*TOKENS)?;

    assert_eq!(file.encoding(), Encoding::Plaintext);
    assert_eq!(save.player.as_deref(), Some("FRA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_ironman_roundtrip() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    use std::io::Cursor;

    let file = utils::request_file("1.10-ironman.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new().on_failed_resolve(hoi4save::FailedResolveStrategy::Error);
    file.melt(options, &*TOKENS, &mut out)?;

    let out = out.into_inner();
    let hash = highway::HighwayHasher::default().hash256(&out);
    let checksum = format!(
        "{:016x}{:016x}{:016x}{:016x}",
        hash[0], hash[1], hash[2], hash[3]
    );
    assert_eq!(
        &checksum,
        "6e8f589e8d181c5205d051834617abbb7b90b4d16b3062d0ac0b85474fe41aa1"
    );

    let file = Hoi4File::from_slice(&out)?;
    let save: Hoi4Save = file.parse_save(&*TOKENS)?;

    assert_eq!(file.encoding(), Encoding::Plaintext);
    assert_eq!(save.player.as_deref(), Some("FRA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}

#[test]
fn test_comp_bin_melt_checksum() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    use std::io::Cursor;

    let file = utils::request_file("comp_bin.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new().on_failed_resolve(hoi4save::FailedResolveStrategy::Error);
    file.melt(options, &*TOKENS, &mut out)?;

    let out = out.into_inner();
    let hash = highway::HighwayHasher::default().hash256(&out);
    let checksum = format!(
        "{:016x}{:016x}{:016x}{:016x}",
        hash[0], hash[1], hash[2], hash[3]
    );
    assert_eq!(
        &checksum,
        "8fbd2292046f67eb76aa50dcddc79aa75d8bee25ef7088c1aaff3d0882a48838"
    );
    Ok(())
}

#[test]
fn test_ironman_roundtrip_with_nulls() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    use std::io::Cursor;

    let file = utils::request_file("1.17-new-ironman-format.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let mut out = Cursor::new(Vec::new());
    let options = MeltOptions::new().on_failed_resolve(hoi4save::FailedResolveStrategy::Error);
    file.melt(options, &*TOKENS, &mut out)?;

    let out = out.into_inner();

    let file = Hoi4File::from_slice(&out)?;
    let save: Hoi4Save = file.parse_save(&*TOKENS)?;

    assert_eq!(file.encoding(), Encoding::Plaintext);
    assert_eq!(save.player.as_deref(), Some("USA"));
    assert_eq!(
        save.date.game_fmt().to_string(),
        String::from("1936.1.1.12")
    );
    Ok(())
}
