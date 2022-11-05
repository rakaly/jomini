use jomini::BinaryTape;
use std::error;
use std::io::{self, Read};
use std::time::Instant;

fn main() -> Result<(), Box<dyn error::Error>> {
    let start = Instant::now();
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    println!("ingest: {}ms", start.elapsed().as_millis());
    let read = Instant::now();

    let mut tape = BinaryTape::new();
    for _ in 0..100 {
        let parser = jomini::binary::BinaryTapeParser;
        match parser.parse_eu4_slice_into_tape(data.as_slice(), &mut tape) {
            Ok(_) => {
                println!("parse: {}ms", read.elapsed().as_millis())
            }
            Err(e) => println!("errored with {}", e),
        }
    }

    Ok(())
}
