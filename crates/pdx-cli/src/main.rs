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
//!
//! `fmt` writes its output even when the file has syntax errors. It reports
//! the errors on stderr and exits with status 1. It refuses binary and
//! compressed files.
//!
//! Some games offer more commands. See each game's module.

use jomini::text::syntax;
use std::{
    error::Error,
    io::{BufWriter, Write},
    path::Path,
    process::ExitCode,
    time::Instant,
};

mod ck3;
mod eu4;
mod eu5;
mod hoi4;
mod imperator;
mod vic3;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

const USAGE: &str = "usage: pdx <melt|json|debug|fmt|game command> <file>";

/// The number of syntax errors that `fmt` prints before it prints a count of
/// the rest.
const MAX_REPORTED_ERRORS: usize = 20;

fn main() -> ExitCode {
    match run() {
        Ok(status) => status,
        Err(error) => {
            eprintln!("Error: {error}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<ExitCode> {
    let args: Vec<String> = std::env::args().collect();
    let (command, path) = match args.as_slice() {
        [_, command, path] => (command.as_str(), Path::new(path)),
        _ => return Err(USAGE.into()),
    };

    let stdout = std::io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    let mut status = ExitCode::SUCCESS;
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
        "fmt" => {
            if fmt(path, &mut out)? > 0 {
                status = ExitCode::FAILURE;
            }
        }
        command => {
            if !game(path)?.extra(command, &mut out)? {
                return Err(USAGE.into());
            }
        }
    }
    out.flush()?;
    Ok(status)
}

/// The operations each game implements.
trait Game {
    /// Write the save as plain text.
    fn melt(&self, out: &mut dyn Write) -> Result<()>;

    /// Parse the save into its model and return a summary.
    fn debug(&self) -> Result<String>;

    /// Run a command that only this game offers.
    ///
    /// Returns `Ok(false)` when the game does not have the command.
    fn extra(&self, _command: &str, _out: &mut dyn Write) -> Result<bool> {
        Ok(false)
    }
}

fn game(path: &Path) -> Result<Box<dyn Game>> {
    let ext = path.extension().and_then(|x| x.to_str()).unwrap_or("");
    let game: Box<dyn Game> = match ext {
        "eu4" => Box::new(eu4::Eu4::open(path)?),
        "eu5" => Box::new(eu5::Eu5::open(path)?),
        "ck3" => Box::new(ck3::Ck3::open(path)?),
        "hoi4" => Box::new(hoi4::Hoi4::open(path)?),
        "rome" => Box::new(imperator::Imperator::open(path)?),
        "v3" => Box::new(vic3::Vic3::open(path)?),
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

/// Reformat a plain text file, keeping a `<GAME>txt` or `SAV...` header
/// line when there is one, and return the number of syntax errors.
///
/// The formatter recovers from syntax errors, so the output is written even
/// when there are errors. After the output is flushed, the first errors go
/// to stderr as `path:line:col: message`. A binary or compressed file is an
/// error, and nothing is written.
fn fmt(path: &Path, out: &mut dyn Write) -> Result<usize> {
    let data = std::fs::read(path)?;
    let (header, body) = split_text(&data)?;

    let parse = syntax::parse(body);
    out.write_all(header)?;
    parse.format_to_writer(&syntax::FormatOptions::default(), &mut *out)?;
    out.flush()?;

    let errors = parse.errors();
    if !errors.is_empty() {
        // The ranges are offsets in the body, and the lines count from the
        // start of the file. A leading BOM is not part of the first line, the
        // same as for the lexer. Other columns count bytes.
        let index = syntax::LineIndex::new(&data);
        let bom = if data.starts_with(BOM) { BOM.len() } else { 0 };
        let at = |offset| {
            let mut at = index.line_col(offset + syntax::TextSize::of(header));
            if at.line == 0 {
                at.col = at.col.saturating_sub(bom as u32);
            }
            at
        };
        let mut stderr = std::io::stderr().lock();
        for error in errors.iter().take(MAX_REPORTED_ERRORS) {
            let (path, message) = (path.display(), error.message());
            write!(stderr, "{path}:{}: {message}", at(error.range.start()))?;
            match &error.missing_delimiter {
                Some(open) => writeln!(stderr, " (opened at {})", at(open.opening_range.start()))?,
                None => writeln!(stderr)?,
            }
        }
        match errors.len().saturating_sub(MAX_REPORTED_ERRORS) {
            0 => {}
            1 => writeln!(stderr, "{}: 1 more syntax error", path.display())?,
            more => writeln!(stderr, "{}: {more} more syntax errors", path.display())?,
        }
    }
    Ok(errors.len())
}

const BOM: &[u8] = b"\xEF\xBB\xBF";

/// Split a plain text file into its header line, which can be empty, and
/// its body. Return an error for a zip file, a save header that is not for
/// plain text, and a body with a NUL byte, which plain text does not have.
fn split_text(data: &[u8]) -> Result<(&[u8], &[u8])> {
    if data.starts_with(b"PK\x03\x04") {
        return Err("a zip file is not plain text; melt it first".into());
    }
    if let Ok(header) = jomini::envelope::SaveHeader::from_slice(data) {
        let kind = header.kind();
        if kind != jomini::envelope::SaveHeaderKind::Text {
            return Err(
                format!("a save with a {kind:?} header is not plain text; melt it first").into(),
            );
        }
    }
    let body = strip_header(data).unwrap_or(data);
    if let Some(at) = body.iter().position(|&b| b == 0) {
        let at = data.len() - body.len() + at;
        return Err(format!("a NUL byte at offset {at} is not plain text; melt it first").into());
    }
    Ok((&data[..data.len() - body.len()], body))
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
