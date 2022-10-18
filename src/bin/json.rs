use jomini::json::JsonOptions;
use std::error;
use std::io::{self, Read};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    match jomini::TextTape::from_slice(&data) {
        Ok(t) => {
            let out = std::io::stdout();
            let handle = out.lock();
            t.windows1252_reader()
                .json()
                .with_options(JsonOptions::new().with_prettyprint(true))
                .to_writer(handle)
                .unwrap();
        }
        Err(e) => println!("errored with {}", e),
    }

    Ok(())
}
