use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    println!("{:#?}", jomini::scratch::parse(&data).unwrap().token_tape);

    /*    let mut parser = BinaryParser::new();
    for event in parser.events(&data) {
        println!("{:?}", event);
    }*/

    Ok(())
}
