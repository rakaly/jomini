use jomini::binary;
use std::{error, io};

#[derive(Debug)]
enum Needle {
    Token(u16),
    String(String),
}

impl PartialEq<binary::Token<'_>> for Needle {
    fn eq(&self, other: &binary::Token<'_>) -> bool {
        match other {
            binary::Token::Unquoted(tok) => match self {
                Needle::String(val) => tok.as_bytes() == val.as_bytes(),
                _ => false,
            },
            binary::Token::Id(tok) => match self {
                Needle::Token(val) => val == tok,
                _ => false,
            },
            _ => false,
        }
    }
}

fn string_lookup_parse(mut data: &[u8]) -> Vec<&'_ str> {
    let mut result = Vec::new();
    data = &data[5..];

    let mut last = "";
    while !data.is_empty() {
        let (len, rest) = data.split_first_chunk::<2>().unwrap();
        let len = u16::from_le_bytes(*len) as usize;
        let (chunk, rest) = rest.split_at(len);

        result.push(std::str::from_utf8(chunk).unwrap());
        data = rest;
    }


    result
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    // let needle = match args.get(1) {
    //     Some(x) => match x.strip_prefix("0x") {
    //         Some(hex) => Needle::Token(u16::from_str_radix(hex, 16).expect("hex")),
    //         None => match x.parse::<u16>() {
    //             Ok(token) => Needle::Token(token),
    //             Err(_) => Needle::String(String::from(x)),
    //         },
    //     },
    //     None => Needle::Token(0),
    // };

    let lookup_data = std::fs::read("./string_lookup").unwrap();
    let string_lookup = string_lookup_parse(&lookup_data);

    let stdin = io::stdin();
    let mut reader = jomini::binary::TokenReader::new(stdin.lock());
    let mut opens = 0;
    let mut closes = 0;
    let mut writer = io::stdout().lock();
    let mut writer = jomini::TextWriterBuilder::new().from_writer(&mut writer);

    while let Some(token) = reader.next_token()? {
        let position = reader.position();

        // match token {
        //     binary::TokenKind::Lookup => {
        //         println!("LOOKUP {}", reader.token_id());
        //     },
        //     _ => {}
        // }
        // println!("{} {:?}", position, reader.token_data(token) );
        match reader.token_from_kind(token) {
            binary::Token::Open => writer.write_start(),
            binary::Token::Close => writer.write_end(),
            binary::Token::Equal => writer.write_operator(jomini::text::Operator::Equal),
            binary::Token::U32(x) => writer.write_u32(x),
            binary::Token::U64(x) => writer.write_u64(x),
            binary::Token::I32(x) => writer.write_i32(x),
            binary::Token::Bool(x) => writer.write_bool(x),
            binary::Token::Quoted(scalar) => writer.write_quoted(scalar.as_bytes()),
            binary::Token::Unquoted(scalar) => writer.write_unquoted(scalar.as_bytes()),
            binary::Token::F32(x) => writer.write_f32_precision(f32::from_le_bytes(x), 3),
            binary::Token::F64(x) => writer.write_f64_precision(f64::from_le_bytes(x), 5),
            binary::Token::Rgb(rgb) => writer.write_rgb(&rgb),
            binary::Token::I64(x) => writer.write_i64(x),
            binary::Token::Id(x) => writer.write_unquoted(format!("id:0x{:x}", x).as_bytes()),
            binary::Token::Lookup2(x) => writer.write_unquoted(format!("lookup2:0x{:x}@{} \'{}\'", x, position, string_lookup[x as usize]).as_bytes()),
            binary::Token::Lookup(x) => writer.write_unquoted(format!("lookup:0x{:x}@{} \'{}\'", x, position, string_lookup[x as usize]).as_bytes()),
        }?
    }

    // loop {
    //     match reader.next() {
    //         Ok(Some(jomini::binary::Token::Open)) => {
    //             opens += 1;
    //         }
    //         Ok(Some(jomini::binary::Token::Close)) => {
    //             closes += 1;
    //         }
    //         Ok(Some(_)) => {}
    //         Ok(None) => break,
    //         Err(e) => {
    //             println!("opens: {}, closes: {}", opens, closes);
    //             return Err(Box::new(e));
    //         }
    //     }
    // }

    // while let Some(token) = reader.next()? {
    //     match token {
    //         jomini::binary::Token::Open => {
    //             opens += 1;
    //         }
    //         jomini::binary::Token::Close => {
    //             closes += 1;
    //         }
    //         other => {}
    //     }
    // }

    println!("opens: {}, closes: {}", opens, closes);

    // let mut state = 0;
    // while let Some(token) = reader.next()? {
    //     match token {
    //         jomini::binary::Token::Equal => {
    //             if state == 1 {
    //                 state = 2;
    //             }
    //         }
    //         token => match state {
    //             0 if needle == token => state = 1,
    //             1 => {
    //                 println!("value");
    //                 state = 0;
    //             }
    //             2 => {
    //                 if !matches!(token, jomini::binary::Token::Open) {
    //                     println!("{:?}", token);
    //                     state = 0;
    //                 }
    //             }
    //             _ => {}
    //         },
    //     }
    // }

    Ok(())
}
