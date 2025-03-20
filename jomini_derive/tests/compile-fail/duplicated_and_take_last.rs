#[allow(dead_code)]
use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(duplicated, take_last)]
    field: Vec<String>,
}

fn main() {}
