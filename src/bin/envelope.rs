//! Extract metadata or gamestate from Paradox save files.
//!
//! This will output the raw, decompressed gamestate or metadata to stdout.

use jomini::envelope::JominiFile;
use std::error;
use std::fs::File;
use std::io;

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <meta|gamestate> <file>", args[0]);
        std::process::exit(1);
    }

    let body_type = &args[1];
    let file_path = &args[2];

    let file = File::open(file_path)?;
    let jomini_file = JominiFile::from_file(file)?;

    let mut stdout = io::stdout().lock();
    match body_type.as_str() {
        "meta" => {
            let mut meta = jomini_file.meta()?;
            io::copy(&mut meta, &mut stdout)?;
        }
        "gamestate" => {
            let mut gamestate = jomini_file.gamestate()?;
            io::copy(&mut gamestate, &mut stdout)?;
        }
        _ => {
            eprintln!(
                "Error: body type must be 'meta' or 'gamestate', got '{}'",
                body_type
            );
            std::process::exit(1);
        }
    }

    Ok(())
}
