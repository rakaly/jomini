use std::{error, io};

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut reader = jomini::text::TokenReader::new(io::stdin().lock());

    let stdout = io::stdout().lock();
    let mut writer = jomini::TextWriterBuilder::new().from_writer(stdout);

    while let Some(token) = reader.next()? {
        match token {
            jomini::text::Token::Open => {
                // Start by assuming an array, which gets corrected when an
                // operator is present
                // https://github.com/rakaly/jomini/pull/155
                writer.write_array_start()?;
            }
            jomini::text::Token::Close => writer.write_end()?,
            jomini::text::Token::Operator(op) => writer.write_operator(op)?,
            jomini::text::Token::Unquoted(x) => writer.write_unquoted(x.as_bytes())?,
            jomini::text::Token::Quoted(x) => writer.write_quoted(x.as_bytes())?,
        }
    }

    Ok(())
}
