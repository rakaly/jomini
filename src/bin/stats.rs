use jomini::BinaryToken;
use std::error;
use std::io::{self, Read};
use std::ops::Range;

#[derive(Debug, Default)]
struct Stats {
    array: u32,
    object: u32,
    mixed: u32,
    bool: u32,
    u32: u32,
    i32: u32,
    u64: u32,
    quoted: u32,
    unquoted: u32,
    f32: u32,
    f64: u32,
    token: u32,
    rgb: u32,
    i64: u32,
}

impl Stats {
    fn update(&mut self, token: &BinaryToken) {
        match token {
            BinaryToken::Array(_) => self.array += 1,
            BinaryToken::Object(_) => self.object += 1,
            BinaryToken::MixedContainer => self.mixed += 1,
            BinaryToken::Equal => {}
            BinaryToken::End(_) => {}
            BinaryToken::Bool(_) => self.bool += 1,
            BinaryToken::U32(_) => self.u32 += 1,
            BinaryToken::U64(_) => self.u64 += 1,
            BinaryToken::I64(_) => self.i64 += 1,
            BinaryToken::I32(_) => self.i32 += 1,
            BinaryToken::Quoted(_) => self.quoted += 1,
            BinaryToken::Unquoted(_) => self.unquoted += 1,
            BinaryToken::F32(_) => self.f32 += 1,
            BinaryToken::F64(_) => self.f64 += 1,
            BinaryToken::Token(_) => self.token += 1,
            BinaryToken::Rgb(_) => self.rgb += 1,
        }
    }
}

impl std::fmt::Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let total = self.array
            + self.object
            + self.mixed
            + self.bool
            + self.u32
            + self.u64
            + self.i32
            + self.quoted
            + self.unquoted
            + self.f32
            + self.f64
            + self.token
            + self.rgb
            + self.i64;

        let total = total as f64;

        if self.array != 0 {
            writeln!(
                f,
                "array:\t\t{:<8}({:.2}%)",
                self.array,
                (self.array as f64) / total * 100.0
            )?;
        }

        if self.object != 0 {
            writeln!(
                f,
                "object:\t\t{:<8}({:.2}%)",
                self.object,
                (self.object as f64) / total * 100.0
            )?;
        }

        if self.bool != 0 {
            writeln!(
                f,
                "bool:\t\t{:<8}({:.2}%)",
                self.bool,
                (self.bool as f64) / total * 100.0
            )?;
        }

        if self.u32 != 0 {
            writeln!(
                f,
                "u32:\t\t{:<8}({:.2}%)",
                self.u32,
                (self.u32 as f64) / total * 100.0
            )?;
        }

        if self.u64 != 0 {
            writeln!(
                f,
                "u64:\t\t{:<8}({:.2}%)",
                self.u64,
                (self.u64 as f64) / total * 100.0
            )?;
        }

        if self.i32 != 0 {
            writeln!(
                f,
                "i32:\t\t{:<8}({:.2}%)",
                self.i32,
                (self.i32 as f64) / total * 100.0
            )?;
        }

        if self.quoted != 0 {
            writeln!(
                f,
                "quoted:\t\t{:<8}({:.2}%)",
                self.quoted,
                (self.quoted as f64) / total * 100.0
            )?;
        }

        if self.unquoted != 0 {
            writeln!(
                f,
                "unquoted:\t{:<8}({:.2}%)",
                self.unquoted,
                (self.unquoted as f64) / total * 100.0
            )?;
        }

        if self.f32 != 0 {
            writeln!(
                f,
                "f32:\t\t{:<8}({:.2}%)",
                self.f32,
                (self.f32 as f64) / total * 100.0
            )?;
        }

        if self.f64 != 0 {
            writeln!(
                f,
                "f64:\t\t{:<8}({:.2}%)",
                self.f64,
                (self.f64 as f64) / total * 100.0
            )?;
        }

        if self.token != 0 {
            writeln!(
                f,
                "token:\t\t{:<8}({:.2}%)",
                self.token,
                (self.token as f64) / total * 100.0
            )?;
        }

        if self.rgb != 0 {
            writeln!(
                f,
                "rgb:\t\t{:<8}({:.2}%)",
                self.rgb,
                (self.rgb as f64) / total * 100.0
            )?;
        }

        if self.i64 != 0 {
            writeln!(
                f,
                "i64:\t\t{:<8}({:.2}%)",
                self.i64,
                (self.i64 as f64) / total * 100.0
            )?;
        }

        writeln!(f, "total:\t\t{:<8}", total)?;

        Ok(())
    }
}

fn read_array(
    keys: &mut Stats,
    values: &mut Stats,
    array: &mut Stats,
    tokens: &[BinaryToken],
    range: Range<usize>,
) {
    let mut ind = range.start;
    while ind < range.end {
        let token = tokens[ind];
        array.update(&token);
        match token {
            BinaryToken::Array(x) => {
                read_array(keys, values, array, tokens, ind + 1..x);
                ind = x + 1;
            }
            BinaryToken::Object(x) => {
                read_object(keys, values, array, tokens, ind + 1..x);
                ind = x + 1;
            }
            BinaryToken::MixedContainer => {
                break;
            }
            _ => ind += 1,
        }
    }
}

fn read_object(
    keys: &mut Stats,
    values: &mut Stats,
    array: &mut Stats,
    tokens: &[BinaryToken],
    range: Range<usize>,
) {
    let mut ind = range.start;
    while ind < range.end {
        let token = tokens[ind];
        keys.update(&token);
        if let BinaryToken::MixedContainer = token {
            break;
        }

        let value = tokens[ind + 1];
        values.update(&value);

        match value {
            BinaryToken::Array(x) => {
                read_array(keys, values, array, tokens, ind + 2..x);
                ind = x + 1;
            }
            BinaryToken::Object(x) => {
                read_object(keys, values, array, tokens, ind + 2..x);
                ind = x + 1;
            }
            _ => ind += 2,
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;

    let tape = jomini::BinaryTape::from_slice(&data).expect("to parse binary tape");

    let mut keys = Stats::default();
    let mut values = Stats::default();
    let mut array = Stats::default();
    let tokens = tape.tokens();
    read_object(&mut keys, &mut values, &mut array, tokens, 0..tokens.len());
    println!("Object key tokens:");
    println!("{}", keys);

    println!("Object value tokens:");
    println!("{}", values);

    println!("Array value tokens:");
    println!("{}", array);

    Ok(())
}
