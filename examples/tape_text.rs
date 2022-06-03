use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    match jomini::TextTape::from_slice(&data) {
        Ok(t) => println!("{:#?}", t.tokens().len()),
        Err(e) => println!("errored with {}", e),
    }

    Ok(())
}
