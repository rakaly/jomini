#[allow(dead_code)]
use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    #[jomini(token = "not-a-number")]
    field: String,
}

fn main() {}
