use std::error;
use std::io::{self, Read};
use std::time::Instant;

fn main() -> Result<(), Box<dyn error::Error>> {
    let start = Instant::now();
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    println!("ingest: {}ms", start.elapsed().as_millis());
    let read = Instant::now();

    match jomini::TextTape::from_slice(&data) {
        Ok(_) => {
            println!("parse: {}ms", read.elapsed().as_millis())
        }
        Err(e) => println!("errored with {}", e),
    }

    Ok(())
}
