#![allow(dead_code)]

use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
pub struct Model {
    human: bool,
    first: u16,
    fourth: u16,
    core: Vec<String>,
    names: Vec<String>,
}

fn main() {}
