//! Development CLI for the game save crates.
//!
//! The game is chosen from the file extension. Token files are found the
//! same way the tests find them (see `pdx-fixtures`).
//!
//! ```text
//! pdx melt  <save>   Convert a save to plain text on stdout
//! pdx json  <save>   Convert a save to JSON on stdout
//! pdx debug <save>   Parse a save into its model and print a summary
//! pdx fmt   <file>   Reformat a plain text file on stdout
//! ```

use std::{
    error::Error,
    io::{BufWriter, Write},
    path::Path,
    time::Instant,
};

mod ck3;
mod eu4;
mod hoi4;
mod imperator;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

const USAGE: &str = "usage: pdx <melt|json|debug|fmt> <file>";

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let (command, path) = match args.as_slice() {
        [_, command, path] => (command.as_str(), Path::new(path)),
        _ => return Err(USAGE.into()),
    };

    let stdout = std::io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    match command {
        "melt" => game(path)?.melt(&mut out)?,
        "json" => {
            let mut text = Vec::new();
            game(path)?.melt(&mut text)?;
            json(&text, &mut out)?;
        }
        "debug" => {
            let start = Instant::now();
            let summary = game(path)?.debug()?;
            writeln!(out, "{summary}")?;
            writeln!(out, "parse: {}ms", start.elapsed().as_millis())?;
        }
        "fmt" => fmt(path, &mut out)?,
        _ => return Err(USAGE.into()),
    }
    out.flush()?;
    Ok(())
}

/// The operations each game implements.
trait Game {
    /// Write the save as plain text.
    fn melt(&self, out: &mut dyn Write) -> Result<()>;

    /// Parse the save into its model and return a one line summary.
    fn debug(&self) -> Result<String>;
}

fn game(path: &Path) -> Result<Box<dyn Game>> {
    let ext = path.extension().and_then(|x| x.to_str()).unwrap_or("");
    let game: Box<dyn Game> = match ext {
        "eu4" => Box::new(eu4::Eu4::open(path)?),
        "ck3" => Box::new(ck3::Ck3::open(path)?),
        "hoi4" => Box::new(hoi4::Hoi4::open(path)?),
        "rome" => Box::new(imperator::Imperator::open(path)?),
        _ => return Err(format!("unknown save extension: {}", path.display()).into()),
    };
    Ok(game)
}

/// Write a plain text document as JSON, skipping a `<GAME>txt` header line
/// when there is one.
fn json(text: &[u8], out: &mut dyn Write) -> Result<()> {
    let body = strip_header(text).unwrap_or(text);
    let tape = jomini::TextTape::from_slice(body)?;
    tape.utf8_reader().json().to_writer(out)?;
    Ok(())
}

/// Reformat a plain text file, keeping a `<GAME>txt` header line when there
/// is one.
fn fmt(path: &Path, out: &mut dyn Write) -> Result<()> {
    let data = std::fs::read(path)?;
    let body = match strip_header(&data) {
        Some(body) => {
            out.write_all(&data[..data.len() - body.len()])?;
            body
        }
        None => &data,
    };

    let tape = jomini::TextTape::from_slice(body)?;
    let mut writer = jomini::TextWriterBuilder::new().from_writer(out);
    writer.write_tape(&tape)?;
    Ok(())
}

/// Return the data after the header line, if the data starts with one.
///
/// Two header shapes exist: `<GAME>txt` for EU4 and HOI4, and the `SAV...`
/// envelope header for CK3 and Imperator.
fn strip_header(data: &[u8]) -> Option<&[u8]> {
    if let Ok(header) = jomini::envelope::SaveHeader::from_slice(data) {
        return Some(&data[header.header_len()..]);
    }

    let end = data.iter().position(|&c| c == b'\n')?;
    let line = data[..end].strip_suffix(b"\r").unwrap_or(&data[..end]);
    let valid =
        line.len() < 16 && line.ends_with(b"txt") && line.iter().all(|c| c.is_ascii_alphanumeric());
    valid.then(|| &data[end + 1..])
}
