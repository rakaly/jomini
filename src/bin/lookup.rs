use std::io::{self, Read};

fn main() {
    let stdin = io::stdin();
    let mut data = Vec::new();
    stdin.lock().read_to_end(&mut data).unwrap();
    
    let mut data = data.as_slice();

    data = &data[5..];

    let mut last = "";
    let mut count = 0;
    while !data.is_empty() {
        let (len, rest) = data.split_first_chunk::<2>().unwrap();
        let len = u16::from_le_bytes(*len) as usize;
        let (chunk, rest) = rest.split_at(len);

        last = std::str::from_utf8(chunk).unwrap();
        data = rest;
        count += 1;
    }

    println!("{count} {last}");
}