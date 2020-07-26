use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    let res = jomini::BinaryTape::from_slice(&data);
    match res {
        Ok(t) => println!("{:#?}", t.token_tape),
        Err(e) => println!("errored with {}", e),
    }

    Ok(())
}
