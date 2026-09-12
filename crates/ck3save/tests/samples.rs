use ck3save::{
    models::{Gamestate, Header},
    Ck3Date, Ck3File, DeserializeCk3, JominiFileKind, SaveDataKind, SaveHeaderKind,
};
use std::collections::HashMap;
mod utils;

#[test]
fn test_ck3_text_header() {
    let data = include_bytes!("fixtures/header.txt");
    let file = Ck3File::from_slice(&data[..]).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);

    let JominiFileKind::Uncompressed(SaveDataKind::Text(text)) = file.kind() else {
        panic!("expected text");
    };
    let header: Header = text.deserializer().deserialize().unwrap();
    assert_eq!(header.meta_data.version, String::from("1.0.2"));
    assert_eq!(header.meta_data.meta_date, Ck3Date::from_ymd(867, 1, 1));
}

#[test]
fn test_ck3_text_save() -> Result<(), Box<dyn std::error::Error>> {
    let file = utils::request_file("Jarl_Ivar_of_the_Isles_867_01_01.ck3");
    let file = Ck3File::from_file(file).unwrap();
    assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);
    let game: Gamestate = (&file).deserialize(HashMap::<u16, &str>::new()).unwrap();
    assert_eq!(game.meta_data.version, String::from("1.0.2"));
    Ok(())
}
