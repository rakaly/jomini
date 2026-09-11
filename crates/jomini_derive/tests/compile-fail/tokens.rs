#[allow(dead_code)]
use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(token = 0x10)]
    human: bool,
    #[jomini(token = 0x11)]
    checksum: String,
    fourth: u16,
}

fn main() {}
