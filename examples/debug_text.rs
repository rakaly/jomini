use jomini::TextParser;
use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;

    let reader = TextParser::new(&data);

    for event in reader {
        println!("{:?}", event);
    }

    Ok(())
}
