#[allow(dead_code)]
use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(duplicated)]
    field: u32,
}

fn main() {}
