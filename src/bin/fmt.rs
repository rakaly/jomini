//! Utility to format jomini text files from stdin to stdout.
//!
//! Useful when compare a game generated debug file and a melted file and
//! identify differences.

use std::{
    error,
    io::{self, BufWriter},
};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut reader = jomini::text::TokenReader::new(io::stdin().lock());

    let stdout = io::stdout().lock();
    let writer = BufWriter::new(stdout);
    let mut writer = jomini::TextWriterBuilder::new().from_writer(writer);

    while let Some(token) = reader.next()? {
        match token {
            jomini::text::Token::Open => {
                writer.write_start()?;
            }
            jomini::text::Token::Close => writer.write_end()?,
            jomini::text::Token::Operator(op) => writer.write_operator(op)?,
            jomini::text::Token::Unquoted(x) => writer.write_unquoted(x.as_bytes())?,
            jomini::text::Token::Quoted(x) => writer.write_quoted(x.as_bytes())?,
        }
    }

    Ok(())
}
