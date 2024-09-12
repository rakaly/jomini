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

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let needle = match args.get(1) {
        Some(x) => match x.strip_prefix("0x") {
            Some(hex) => Needle::Token(u16::from_str_radix(hex, 16).expect("hex")),
            None => match x.parse::<u16>() {
                Ok(token) => Needle::Token(token),
                Err(_) => Needle::String(String::from(x)),
            },
        },
        None => Needle::Token(0),
    };

    let stdin = io::stdin();
    let mut reader = jomini::binary::TokenReader::new(stdin.lock());
    let mut state = 0;
    while let Some(token) = reader.next()? {
        match token {
            jomini::binary::Token::Equal => {
                if state == 1 {
                    state = 2;
                }
            }
            token => match state {
                0 if needle == token => state = 1,
                1 => {
                    println!("value");
                    state = 0;
                }
                2 => {
                    if !matches!(token, jomini::binary::Token::Open) {
                        println!("{:?}", token);
                        state = 0;
                    }
                }
                _ => {}
            },
        }
    }

    Ok(())
}
