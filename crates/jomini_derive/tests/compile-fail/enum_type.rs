#[allow(dead_code)]
use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub enum Model {
    A,
    B,
}

fn main() {}
