use jomini::binary::{Token, TokenReader};
use std::error;
use std::io::{self, Read};

#[derive(Debug, Default)]
struct Stats {
    open: u32,
    close: u32,
    equal: u32,
    bool: u32,
    u32: u32,
    i32: u32,
    u64: u32,
    i64: u32,
    quoted: u32,
    unquoted: u32,
    f32: u32,
    f64: u32,
    id: u32,
    rgb: u32,
    frequencies: Vec<u64>,
}

impl Stats {
    fn new() -> Self {
        Self {
            frequencies: vec![0; 100],
            ..Default::default()
        }
    }

    fn update(&mut self, token: &Token) {
        match token {
            Token::Open => self.open += 1,
            Token::Close => self.close += 1,
            Token::Equal => self.equal += 1,
            Token::Bool(_) => self.bool += 1,
            Token::U32(_) => self.u32 += 1,
            Token::U64(_) => self.u64 += 1,
            Token::I64(_) => self.i64 += 1,
            Token::I32(_) => self.i32 += 1,
            Token::Quoted(x) => {
                let len = x.as_bytes().len();
                if len < self.frequencies.len() {
                    self.frequencies[len] += 1;
                }
                self.quoted += 1;
            }
            Token::Unquoted(x) => {
                let len = x.as_bytes().len();
                if len < self.frequencies.len() {
                    self.frequencies[len] += 1;
                }
                self.unquoted += 1;
            }
            Token::F32(_) => self.f32 += 1,
            Token::F64(_) => self.f64 += 1,
            Token::Id(_) => self.id += 1,
            Token::Rgb(_) => self.rgb += 1,
        }
    }
}

impl std::fmt::Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let total = self.open
            + self.close
            + self.equal
            + self.bool
            + self.u32
            + self.u64
            + self.i32
            + self.i64
            + self.quoted
            + self.unquoted
            + self.f32
            + self.f64
            + self.id
            + self.rgb;

        let total = total as f64;

        if self.open != 0 {
            writeln!(
                f,
                "open:\t\t{:<8}({:.2}%)",
                self.open,
                (self.open as f64) / total * 100.0
            )?;
        }

        if self.close != 0 {
            writeln!(
                f,
                "close:\t\t{:<8}({:.2}%)",
                self.close,
                (self.close as f64) / total * 100.0
            )?;
        }

        if self.equal != 0 {
            writeln!(
                f,
                "equal:\t\t{:<8}({:.2}%)",
                self.equal,
                (self.equal as f64) / total * 100.0
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

        if self.i64 != 0 {
            writeln!(
                f,
                "i64:\t\t{:<8}({:.2}%)",
                self.i64,
                (self.i64 as f64) / total * 100.0
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

        if self.id != 0 {
            writeln!(
                f,
                "id:\t\t{:<8}({:.2}%)",
                self.id,
                (self.id as f64) / total * 100.0
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

        writeln!(f, "total:\t\t{:<8}", total)?;

        let count = self.frequencies.iter().sum::<u64>();
        if count > 0 {
            let sum = self
                .frequencies
                .iter()
                .enumerate()
                .map(|(i, x)| (i as u64) * *x)
                .sum::<u64>();
            let median_ind = count.div_ceil(2);
            let mut counter = 0;
            let mut median = 0;
            for (i, freq) in self.frequencies.iter().enumerate() {
                counter += *freq;
                if counter > median_ind {
                    median = i;
                    break;
                }
            }

            writeln!(f, "text count: {}", count)?;
            writeln!(f, "text average length: {:.2}", sum as f64 / count as f64)?;
            writeln!(f, "text median length: {:.2}", median)?;
        }

        Ok(())
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;

    let mut stats = Stats::new();
    let mut reader = TokenReader::new(&data[..]);
    while let Ok(Some(token)) = reader.next() {
        stats.update(&token);
    }

    println!("Binary token statistics:");
    println!("{}", stats);

    Ok(())
}
