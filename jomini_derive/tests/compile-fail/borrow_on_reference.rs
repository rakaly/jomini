use jomini_derive::JominiDeserialize;

#[derive(JominiDeserialize)]
struct BadStruct<'a> {
    #[jomini(borrow)]
    field: &'a str,
}

fn main() {}