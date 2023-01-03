use std::error;
use std::io::{self, Read};
use jomini::BinaryToken;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    let tape = jomini::BinaryTape::from_slice(&data)?;
    let tokens = tape.tokens();
    for (i, token) in tokens.iter().enumerate() {
        if matches!(token, BinaryToken::Token(0x3145)) {
            let offset = match tokens[i + 1] {
                BinaryToken::Array{..} | BinaryToken::Object {..} => 2,
                _ => 1
            };

            println!("{:?}", tokens[i + offset]);
        }
    }

    Ok(())
}