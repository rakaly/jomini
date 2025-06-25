use jomini_derive::JominiDeserialize;
use std::borrow::Cow;

#[derive(JominiDeserialize)]
struct BadStruct<'a> {
    #[jomini(borrow, duplicated, take_last)]
    field: Vec<Cow<'a, str>>,
}

fn main() {}