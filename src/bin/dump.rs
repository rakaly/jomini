//! Dump binary data as text with byte offsets for debugging
//!
//! Reads binary data from stdin. If using a save from a Jomini-engine game
//! (EU5, Vic3, CK3, Imperator), first extract the gamestate (or meta) by
//! running the `envelope` command.
//!
//! ## Sample Output
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

use jomini::binary::{LexemeId, Lexer};
use std::io::{self, Read, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut data = Vec::new();
    stdin.lock().read_to_end(&mut data)?;

    let mut lexer = Lexer::new(&data);

    let stdout = io::stdout();
    let mut position_writer = PositionWriter::new(stdout.lock());
    let mut writer = jomini::TextWriterBuilder::new()
        .verbatim(true)
        .from_writer(&mut position_writer);

    let mut buf = Vec::new();
    let mut next_position = 0;

    while let Some(lexeme_id) = lexer.next_id()? {
        let current_position = lexer.position();

        // Set position for this line (from previous iteration)
        writer.inner().pending_position = next_position;

        // Write token
        match lexeme_id {
            LexemeId::OPEN => {
                writer.write_start()?;
            }
            LexemeId::CLOSE => {
                writer.write_end()?;
            }
            LexemeId::EQUAL => {
                writer.write_operator(jomini::text::Operator::Equal)?;
            }
            _ => {
                buf.clear();
                format_lexeme(&mut lexer, lexeme_id, &mut buf)?;
                writer.write_unquoted(buf.as_slice())?;
            }
        }

        // Save position for NEXT iteration
        next_position = current_position;
    }

    println!();
    Ok(())
}

fn format_lexeme(
    lexer: &mut Lexer,
    lexeme_id: LexemeId,
    buf: &mut Vec<u8>,
) -> Result<(), Box<dyn std::error::Error>> {
    match lexeme_id {
        LexemeId::U32 => {
            let value = lexer.read_u32()?;
            write!(buf, "u32:{}", value)?;
        }
        LexemeId::U64 => {
            let value = lexer.read_u64()?;
            write!(buf, "u64:{}", value)?;
        }
        LexemeId::I32 => {
            let value = lexer.read_i32()?;
            write!(buf, "i32:{}", value)?;
        }
        LexemeId::I64 => {
            let value = lexer.read_i64()?;
            write!(buf, "i64:{}", value)?;
        }
        LexemeId::BOOL => {
            let value = lexer.read_bool()?;
            write!(buf, "{}", value)?;
        }
        LexemeId::QUOTED => {
            let value = lexer.read_string()?;
            buf.push(b'\'');
            buf.extend_from_slice(value.as_bytes());
            buf.push(b'\'');
        }
        LexemeId::UNQUOTED => {
            let value = lexer.read_string()?;
            buf.extend_from_slice(value.as_bytes());
        }
        LexemeId::F32 => {
            let bytes = lexer.read_f32()?;
            write!(buf, "f32:0x{:x}", u32::from_le_bytes(bytes))?;
        }
        LexemeId::F64 => {
            let bytes = lexer.read_f64()?;
            write!(buf, "f64:0x{:x}", u64::from_le_bytes(bytes))?;
        }
        LexemeId::RGB => {
            let rgb = lexer.read_rgb()?;
            write!(buf, "rgb:{:02x}{:02x}{:02x}", rgb.r, rgb.g, rgb.b)?;
        }
        LexemeId::LOOKUP_U8 => {
            let value = lexer.read_lookup_u8()?;
            write!(buf, "lookup_u8:0x{:02x}", value)?;
        }
        LexemeId::LOOKUP_U8_ALT => {
            let value = lexer.read_lookup_u8()?;
            write!(buf, "lookup_u8_alt:0x{:02x}", value)?;
        }
        LexemeId::LOOKUP_U16 => {
            let value = lexer.read_lookup_u16()?;
            write!(buf, "lookup_u16:0x{:04x}", value)?;
        }
        LexemeId::LOOKUP_U16_ALT => {
            let value = lexer.read_lookup_u16()?;
            write!(buf, "lookup_u16_alt:0x{:04x}", value)?;
        }
        // Handle Fixed5 lexemes with special formatting
        lexeme if lexeme >= LexemeId::FIXED5_ZERO && lexeme <= LexemeId::FIXED5_I56 => {
            let offset = lexeme.0 - LexemeId::FIXED5_ZERO.0;
            let is_negative = offset > 7;
            let byte_count = offset - (is_negative as u16 * 7);
            let bytes = lexer.read_bytes(byte_count as usize)?;
            let mut tmp = [0u8; 8];
            tmp[..byte_count as usize].copy_from_slice(bytes);
            let raw = u64::from_le_bytes(tmp);
            let sign = if is_negative { -1.0 } else { 1.0 };
            let val = (raw as f64) * sign / 100_000.0;
            write!(
                buf,
                "fixed5_{}{}(0x{:x})[{:.5}]",
                if is_negative { "i" } else { "u" },
                byte_count * 8,
                raw,
                val
            )?;
        }
        _ => {
            // ID token - lexeme_id itself is the value
            write!(buf, "id:0x{:x}", lexeme_id.0)?;
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
