use jomini::{BinaryEvent, BinaryParser, BinaryReader, EventRecord, Scalar};
use std::io::{Cursor, Read};

#[test]
fn test_ragusa() {
    let ragusa = std::fs::read("../../assets/eu4-saves/ironman/ragusa.bin.eu4").unwrap();
    let reader = Cursor::new(&ragusa);
    let mut zip = zip::ZipArchive::new(reader).unwrap();
    let mut buffer = Vec::with_capacity(0);
    let mut zip_file = zip.by_name("gamestate").unwrap();
    buffer.reserve(zip_file.size() as usize);
    zip_file.read_to_end(&mut buffer).unwrap();

    let data = &buffer["EU4bin".len()..];

    let mut parser = BinaryParser::new();
    let mut count = 0;
    for event in parser.events(data) {
        event.unwrap();
        count += 1;
    }

    assert_eq!(count, 9584367);

    let mut record = EventRecord::new();
    let mut reader = BinaryReader::from_reader(data);
    let mut count2 = 0;

    while reader.read_event(&mut record).unwrap() {
        count2 += 1;
    }

    assert_eq!(count, count2);
}

fn get_scalars<'a>(data: &'a [u8]) -> Vec<Scalar<'a>> {
    let mut parser = BinaryParser::new();
    let mut scalars = Vec::new();
    for event in parser.events(data) {
        let res = event.unwrap();
        match res {
            BinaryEvent::Text(s) => scalars.push(s),
            _ => {}
        }
    }
    scalars
}

#[test]
fn test_iter_lifetime() {
    assert_eq!(0, get_scalars(&[][..]).len());
}
