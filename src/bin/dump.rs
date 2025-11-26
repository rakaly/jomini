//! Dump binary data as text with byte offsets for debugging
//!
//! Reads binary data from stdin. If using a save from a Jomini-engine game
//! (EU5, Vic3, CK3, Imperator), first extract the gamestate (or meta) by
//! running the `envelope` command.
//!
//! Here is some sample output:
//!
//! ```text
//!          0: id:0x284d=i32:59611248
//!         10: id:0x2c69='kandy2.eu4'
//!         28: id:0x2a38='BHA'
//!         39: id:0x32b8='Bharat'
//!         53: id:0x2ec9={
//!         59:   id:0x28e2=i32:1
//!         69:   id:0x28e3=i32:29
//!         79:   id:0x2ec7=i32:5
//!         89:   id:0x2ec8=i32:0
//!         99:   id:0x1b='Manchu'
//!        113: }
//! ```

use jomini::binary::TokenKind;
use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut reader = jomini::binary::TokenReader::new(stdin.lock());

    let stdout = io::stdout();
    let mut position_writer = PositionWriter::new(stdout.lock());
    let mut writer = jomini::TextWriterBuilder::new().from_writer(&mut position_writer);

    let mut buf = Vec::new();
    let mut next_position = 0;

    while let Some(token) = reader.next_token()? {
        let current_position = reader.position();

        // Set position for this line (from previous iteration)
        writer.inner().pending_position = next_position;

        // Write token
        match token {
            TokenKind::Open => {
                writer.write_start()?;
            }
            TokenKind::Close => {
                writer.write_end()?;
            }
            TokenKind::Equal => {
                writer.write_operator(jomini::text::Operator::Equal)?;
            }
            _ => {
                buf.clear();
                format_value(&reader, token, &mut buf)?;
                writer.write_unquoted(buf.as_slice())?;
            }
        }

        // Save position for NEXT iteration
        next_position = current_position;
    }

    println!();
    Ok(())
}

fn format_value(
    reader: &jomini::binary::TokenReader<io::StdinLock>,
    kind: TokenKind,
    buf: &mut Vec<u8>,
) -> Result<(), Box<dyn std::error::Error>> {
    match kind {
        TokenKind::Open | TokenKind::Close | TokenKind::Equal => unreachable!(),
        TokenKind::U32 => {
            let token = reader.u32_data();
            write!(buf, "u32:{}", token)?;
        }
        TokenKind::U64 => {
            let token = reader.u64_data();
            write!(buf, "u64:{}", token)?;
        }
        TokenKind::I32 => {
            let token = reader.i32_data();
            write!(buf, "i32:{}", token)?;
        }
        TokenKind::Bool => {
            let token = reader.bool_data();
            write!(buf, "{}", token)?;
        }
        TokenKind::Quoted => {
            buf.push(b'\'');
            buf.extend_from_slice(unsafe { reader.scalar_data().as_bytes() });
            buf.push(b'\'');
        }
        TokenKind::Unquoted => {
            buf.extend_from_slice(unsafe { reader.scalar_data().as_bytes() });
        }
        TokenKind::F32 => {
            let token = reader.f32_data();
            write!(buf, "f32:0x{:x}", u32::from_le_bytes(token))?;
        }
        TokenKind::F64 => {
            let token = reader.f64_data();
            write!(buf, "f64:0x{:x}", u64::from_le_bytes(token))?;
        }
        TokenKind::Rgb => {
            let token = reader.rgb_data();
            write!(buf, "rgb:{:02x}{:02x}{:02x}", token.r, token.g, token.b)?;
        }
        TokenKind::I64 => {
            let token = reader.i64_data();
            write!(buf, "i64:{}", token)?;
        }
        TokenKind::LookupU8 => {
            let token = reader.lookup_u8_data();
            write!(buf, "lookup_u8:0x{:02x}", token)?;
        }
        TokenKind::LookupU16 => {
            let token = reader.lookup_u16_data();
            write!(buf, "lookup_u16:0x{:04x}", token)?;
        }
        TokenKind::Id => {
            let token = reader.token_id();
            write!(buf, "id:0x{:x}", token)?;
        }
    }

    Ok(())
}

struct PositionWriter<W> {
    inner: W,
    at_line_start: bool,
    pending_position: usize,
}

impl<W> PositionWriter<W> {
    fn new(inner: W) -> Self {
        PositionWriter {
            inner,
            at_line_start: true,
            pending_position: 0,
        }
    }
}

impl<W: Write> Write for PositionWriter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if self.at_line_start && !buf.is_empty() {
            write!(self.inner, "{:>10}: ", self.pending_position)?;
            self.at_line_start = false;
        }

        let result = self.inner.write(buf)?;

        // Track newlines to reset line start state
        if buf[..result].contains(&b'\n') {
            self.at_line_start = true;
        }

        Ok(result)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
