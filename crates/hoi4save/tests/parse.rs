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
fn test_hoi4_text_map_data() -> Result<(), Box<dyn Error>> {
    let file = utils::request_file("1.10-normal-text.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;

    let corsica = &save.states[&1];
    assert_eq!(corsica.owner.map(|x| x.to_string()).as_deref(), Some("FRA"));
    assert!(save.states.len() > 500);
    assert!(save.version.is_some());

    let (_, france) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"FRA"))
        .unwrap();
    assert_eq!(france.capital, 16);
    assert_eq!(france.cosmetic_tag, None);
    let ruling_party = france
        .politics
        .as_ref()
        .and_then(|x| x.ruling_party.as_deref());
    assert_eq!(ruling_party, Some("democratic"));

    let (_, canada) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"CAN"))
        .unwrap();
    assert_eq!(canada.cosmetic_tag.as_deref(), Some("CAN_UK"));
    Ok(())
}

#[test]
fn test_empty_tags_are_none() -> Result<(), Box<dyn Error>> {
    let data = br#"HOI4txt
player="FRA"
date="1936.1.1.12"
countries={
    FRA={
        cosmetic_tag=""
    }
}
states={
    1={
        owner=""
        controller=""
    }
}
provinces={
    2={
        controller=""
    }
}
"#;
    let file = Hoi4File::from_slice(data)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(save.countries[0].1.cosmetic_tag, None);
    assert!(save.states[&1].owner.is_none());
    assert!(save.states[&1].controller.is_none());
    assert!(save.provinces[&2].controller.is_none());
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
fn test_hoi4_1_19_text_map_data() -> Result<(), Box<dyn Error>> {
    let file = utils::request_file("MAN_1938.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Plaintext);
    assert_eq!(save.player.as_deref(), Some("MAN"));
    assert_eq!(save.date.game_fmt().to_string(), "1938.1.11.3");
    assert_eq!(
        save.version.as_deref(),
        Some("Operation Postern v1.19.3.0.c01a (5632)")
    );

    let (_, manchukuo) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"MAN"))
        .unwrap();
    assert_eq!(manchukuo.capital, 328);
    let ruling_party = manchukuo
        .politics
        .as_ref()
        .and_then(|x| x.ruling_party.as_deref());
    assert_eq!(ruling_party, Some("fascism"));

    let (_, canada) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"CAN"))
        .unwrap();
    assert_eq!(canada.cosmetic_tag.as_deref(), Some("CAN_UK"));

    let state = &save.states[&608];
    assert_eq!(state.owner.map(|x| x.to_string()).as_deref(), Some("CHI"));
    assert_eq!(
        state.controller.map(|x| x.to_string()).as_deref(),
        Some("JAP")
    );

    let province = &save.provinces[&1027];
    assert_eq!(
        province.controller.map(|x| x.to_string()).as_deref(),
        Some("JAP")
    );
    Ok(())
}

#[test]
fn test_hoi4_1_19_ironman_map_data() -> Result<(), Box<dyn Error>> {
    if TOKENS.is_empty() {
        return Ok(());
    }

    // Binary saves write province ids as quoted strings and state ids as
    // integers. Both must deserialize as ids.
    let file = utils::request_file("ironman-1.19.hoi4");
    let mut file = Hoi4File::from_file(file)?;
    let save = file.parse_save(&*TOKENS)?;
    assert_eq!(file.encoding(), Encoding::Binary);
    assert_eq!(save.player.as_deref(), Some("BHU"));
    assert_eq!(save.date.game_fmt().to_string(), "1941.2.13.17");
    assert_eq!(
        save.version.as_deref(),
        Some("Operation Postern v1.19.3.0.c01a (5632)")
    );

    let (_, bhutan) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"BHU"))
        .unwrap();
    assert_eq!(bhutan.capital, 324);
    let ruling_party = bhutan
        .politics
        .as_ref()
        .and_then(|x| x.ruling_party.as_deref());
    assert_eq!(ruling_party, Some("neutrality"));

    let (_, canada) = save
        .countries
        .iter()
        .find(|(tag, _)| tag.is(b"CAN"))
        .unwrap();
    assert_eq!(canada.cosmetic_tag.as_deref(), Some("CAN_UK"));

    let state = &save.states[&6];
    assert_eq!(state.owner.map(|x| x.to_string()).as_deref(), Some("BEL"));
    assert_eq!(
        state.controller.map(|x| x.to_string()).as_deref(),
        Some("GER")
    );

    assert!(save.provinces.contains_key(&2));
    let province = &save.provinces[&10];
    assert_eq!(
        province.controller.map(|x| x.to_string()).as_deref(),
        Some("GER")
    );
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
        "52d41bfbe0da90125e47a5d10543fc6be6b678ec7e0aced4894332ad748f062f"
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
