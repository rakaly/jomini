use std::error;
use std::io::{self, Read};

pub(crate) const END: u16 = 0x0004;
pub(crate) const OPEN: u16 = 0x0003;
pub(crate) const EQUAL: u16 = 0x0001;
pub(crate) const U32: u16 = 0x0014;
pub(crate) const U64: u16 = 0x029c;
pub(crate) const I32: u16 = 0x000c;
pub(crate) const BOOL: u16 = 0x000e;
pub(crate) const QUOTED_STRING: u16 = 0x000f;
pub(crate) const UNQUOTED_STRING: u16 = 0x0017;
pub(crate) const F32: u16 = 0x000d;
pub(crate) const F64: u16 = 0x0167;
pub(crate) const RGB: u16 = 0x0243;
pub(crate) const I64: u16 = 0x0317;

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    let stdout = io::stdout();
    let lock = stdout.lock();
    io::stdin().read_to_end(&mut data)?;
    let mut parser = jomini::binary::de::OndemandParser::new(&data);
    let mut writer = jomini::TextWriterBuilder::new().from_writer(lock);
    while let Some(x) = parser.next() {
        match x {
            QUOTED_STRING => writer.write_quoted(parser.read_string()?)?,
            UNQUOTED_STRING => writer.write_unquoted(parser.read_string()?)?,
            U32 => writer.write_u32(parser.read_u32()?)?,
            I32 => writer.write_i32(parser.read_i32()?)?,
            U64 => writer.write_u64(parser.read_u64()?)?,
            I64 => writer.write_i64(parser.read_i64()?)?,
            BOOL => writer.write_bool(parser.read_bool()?)?,
            F32 => writer.write_f32(f32::from_le_bytes(parser.read_f32()?).trunc())?,
            F64 => writer.write_f64(f64::from_le_bytes(parser.read_f64()?).trunc())?,
            END => writer.write_end()?,
            OPEN => writer.write_array_start()?,
            EQUAL => writer.write_operator(jomini::text::Operator::Equal)?,
            RGB => writer.write_header(b"rgb")?,
            x => write!(writer, "0x{:04x}", x)?,
        }
    }

    Ok(())
}
