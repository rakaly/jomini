use std::collections::BTreeSet;
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
    lookup: [u32; 5],
    lookup_max: u32,
    fixed5: [u32; 15],
    token_ids: BTreeSet<u16>,
    frequencies: Vec<u64>,
}

impl Stats {
    fn new() -> Self {
        Self {
            frequencies: vec![0; 100],
            ..Default::default()
        }
    }

    fn scan(&mut self, mut data: &[u8]) {
        while data.len() >= 2 {
            let opcode = u16::from_le_bytes([data[0], data[1]]);
            data = &data[2..];
            let skip: usize = match opcode {
                0x0003 => { self.open += 1; 0 }
                0x0004 => { self.close += 1; 0 }
                0x0001 => { self.equal += 1; 0 }
                0x000e => { self.bool += 1; 1 }
                0x0014 => { self.u32 += 1; 4 }
                0x000c => { self.i32 += 1; 4 }
                0x000d => { self.f32 += 1; 4 }
                0x029c => { self.u64 += 1; 8 }
                0x0317 => { self.i64 += 1; 8 }
                0x0167 => { self.f64 += 1; 8 }
                0x000f | 0x0017 => {
                    if data.len() < 2 { break; }
                    let len = u16::from_le_bytes([data[0], data[1]]) as usize;
                    if data.len() < 2 + len { break; }
                    if len < self.frequencies.len() { self.frequencies[len] += 1; }
                    if opcode == 0x000f { self.quoted += 1; } else { self.unquoted += 1; }
                    2 + len
                }
                0x0243 => { self.rgb += 1; 0 }
                0x0d40 => {
                    if data.is_empty() { break; }
                    self.lookup_max = self.lookup_max.max(data[0] as u32);
                    self.lookup[0] += 1;
                    1
                }
                0x0d3e => {
                    if data.len() < 2 { break; }
                    self.lookup_max = self.lookup_max.max(u16::from_le_bytes([data[0], data[1]]) as u32);
                    self.lookup[1] += 1;
                    2
                }
                0x0d41 => {
                    if data.len() < 3 { break; }
                    self.lookup_max = self.lookup_max.max(u32::from_le_bytes([data[0], data[1], data[2], 0]));
                    self.lookup[2] += 1;
                    3
                }
                0x0d43 => {
                    if data.is_empty() { break; }
                    self.lookup_max = self.lookup_max.max(data[0] as u32);
                    self.lookup[3] += 1;
                    1
                }
                0x0d44 => {
                    if data.len() < 2 { break; }
                    self.lookup_max = self.lookup_max.max(u16::from_le_bytes([data[0], data[1]]) as u32);
                    self.lookup[4] += 1;
                    2
                }
                0x0d47..=0x0d55 => {
                    let off = (opcode - 0x0d47) as usize;
                    let bytes = if off > 7 { off - 7 } else { off };
                    if data.len() < bytes { break; }
                    self.fixed5[off] += 1;
                    bytes
                }
                _ => { self.token_ids.insert(opcode); self.id += 1; 0 }
            };
            if skip > data.len() { break; }
            data = &data[skip..];
        }
    }

    fn find_large_gaps(&self, threshold: u16) -> Vec<(u16, u16, u32)> {
        let mut gaps = Vec::new();

        if self.token_ids.is_empty() {
            return gaps;
        }

        // Check gap before first token
        if let Some(&first) = self.token_ids.iter().next()
            && first >= threshold
        {
            gaps.push((0, first - 1, first as u32));
        }

        // Check gaps between consecutive tokens using iterator zip
        let mut iter1 = self.token_ids.iter().copied();
        let mut iter2 = self.token_ids.iter().copied().skip(1);

        for (curr_id, next_id) in iter1.by_ref().zip(iter2.by_ref()) {
            let gap_size = next_id - curr_id - 1;
            if gap_size >= threshold {
                gaps.push((curr_id + 1, next_id - 1, gap_size as u32));
            }
        }

        // Check gap after last token
        if let Some(&last) = self.token_ids.iter().next_back() {
            let gap_to_max = u16::MAX - last;
            if gap_to_max >= threshold {
                gaps.push((last + 1, u16::MAX, gap_to_max as u32));
            }
        }

        gaps
    }
}

impl std::fmt::Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lookup_total: u32 = self.lookup.iter().sum();
        let fixed5_total: u32 = self.fixed5.iter().sum();
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
            + self.rgb
            + lookup_total
            + fixed5_total;

        let total = total as f64;

        if self.open != 0 {
            writeln!(f, "open:\t\t{:<8}({:.2}%)", self.open, (self.open as f64) / total * 100.0)?;
        }
        if self.close != 0 {
            writeln!(f, "close:\t\t{:<8}({:.2}%)", self.close, (self.close as f64) / total * 100.0)?;
        }
        if self.equal != 0 {
            writeln!(f, "equal:\t\t{:<8}({:.2}%)", self.equal, (self.equal as f64) / total * 100.0)?;
        }
        if self.bool != 0 {
            writeln!(f, "bool:\t\t{:<8}({:.2}%)", self.bool, (self.bool as f64) / total * 100.0)?;
        }
        if self.u32 != 0 {
            writeln!(f, "u32:\t\t{:<8}({:.2}%)", self.u32, (self.u32 as f64) / total * 100.0)?;
        }
        if self.u64 != 0 {
            writeln!(f, "u64:\t\t{:<8}({:.2}%)", self.u64, (self.u64 as f64) / total * 100.0)?;
        }
        if self.i32 != 0 {
            writeln!(f, "i32:\t\t{:<8}({:.2}%)", self.i32, (self.i32 as f64) / total * 100.0)?;
        }
        if self.i64 != 0 {
            writeln!(f, "i64:\t\t{:<8}({:.2}%)", self.i64, (self.i64 as f64) / total * 100.0)?;
        }
        if self.quoted != 0 {
            writeln!(f, "quoted:\t\t{:<8}({:.2}%)", self.quoted, (self.quoted as f64) / total * 100.0)?;
        }
        if self.unquoted != 0 {
            writeln!(f, "unquoted:\t{:<8}({:.2}%)", self.unquoted, (self.unquoted as f64) / total * 100.0)?;
        }
        if self.f32 != 0 {
            writeln!(f, "f32:\t\t{:<8}({:.2}%)", self.f32, (self.f32 as f64) / total * 100.0)?;
        }
        if self.f64 != 0 {
            writeln!(f, "f64:\t\t{:<8}({:.2}%)", self.f64, (self.f64 as f64) / total * 100.0)?;
        }
        if self.id != 0 {
            writeln!(f, "id:\t\t{:<8}({:.2}%)", self.id, (self.id as f64) / total * 100.0)?;
        }
        if self.rgb != 0 {
            writeln!(f, "rgb:\t\t{:<8}({:.2}%)", self.rgb, (self.rgb as f64) / total * 100.0)?;
        }

        if lookup_total != 0 {
            const LOOKUP_NAMES: [&str; 5] = ["u8", "u16", "u24", "u8_alt", "u16_alt"];
            writeln!(
                f,
                "lookup:\t\t{:<8}({:.2}%) (max: {})",
                lookup_total,
                lookup_total as f64 / total * 100.0,
                self.lookup_max
            )?;
            for (i, &count) in self.lookup.iter().enumerate() {
                if count != 0 {
                    writeln!(
                        f,
                        "  {:8}:\t{:<8}({:.2}%)",
                        LOOKUP_NAMES[i],
                        count,
                        count as f64 / total * 100.0
                    )?;
                }
            }
        }

        if fixed5_total != 0 {
            const FIXED5_NAMES: [&str; 15] = [
                "zero", "u8", "u16", "u24", "u32", "u40", "u48", "u56",
                "i8", "i16", "i24", "i32", "i40", "i48", "i56",
            ];
            writeln!(
                f,
                "fixed5:\t\t{:<8}({:.2}%)",
                fixed5_total,
                fixed5_total as f64 / total * 100.0
            )?;
            for (i, &count) in self.fixed5.iter().enumerate() {
                if count != 0 {
                    writeln!(
                        f,
                        "  {:8}:\t{:<8}({:.2}%)",
                        FIXED5_NAMES[i],
                        count,
                        count as f64 / total * 100.0
                    )?;
                }
            }
        }

        if let Some(max_id) = self.token_ids.iter().next_back() {
            writeln!(f, "max token id:\t0x{:04x}", max_id)?;
        }

        writeln!(f, "total:\t\t{:<8}", total)?;

        // Report large unused token ranges
        let threshold = 250;
        let large_gaps = self.find_large_gaps(threshold);
        if !large_gaps.is_empty() {
            writeln!(f)?;
            writeln!(f, "Large unused token ranges ({threshold}+ tokens):")?;
            for (start, end, size) in large_gaps {
                writeln!(
                    f,
                    "Unused range: 0x{:04x}-0x{:04x} ({} tokens)",
                    start, end, size
                )?;
            }
            writeln!(f)?;
        }

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
    let mut stats = Stats::new();

    #[cfg(feature = "envelope")]
    {
        use jomini::envelope::{JominiFile, JominiFileKind};
        use std::fs::File;

        let args: Vec<String> = std::env::args().collect();

        // If envelope feature is enabled and file argument is provided, read from envelope
        if args.len() >= 2 {
            let file_path = &args[1];
            let file = File::open(file_path)?;
            let jomini_file = JominiFile::from_file(file)?;

            let JominiFileKind::Zip(zip) = jomini_file.kind() else {
                eprintln!("Error: file is not a envelope");
                std::process::exit(1);
            };

            // Process metadata
            if let Ok(mut meta) = zip.meta() {
                let mut buf = Vec::new();
                meta.read_to_end(&mut buf)?;
                stats.scan(&buf);
            }

            // Process gamestate
            if let Ok(mut gamestate) = zip.gamestate() {
                let mut buf = Vec::new();
                gamestate.read_to_end(&mut buf)?;
                stats.scan(&buf);
            }

            println!("Binary token statistics:");
            println!("{}", stats);
            return Ok(());
        }
    }

    // Fall back to stdin reading
    let mut data = Vec::new();
    io::stdin().read_to_end(&mut data)?;
    stats.scan(&data);

    println!("Binary token statistics:");
    println!("{}", stats);

    Ok(())
}
