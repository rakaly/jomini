use jomini::{
    binary::{BasicTokenResolver, TokenResolver},
    envelope::{JominiFile, JominiFileKind, SaveContentKind},
};

fn string_lookup_parse(mut data: &[u8]) -> Vec<&'_ str> {
    let mut result = Vec::new();
    data = &data[5..];

    while !data.is_empty() {
        let (len, rest) = data.split_first_chunk::<2>().unwrap();
        let len = u16::from_le_bytes(*len) as usize;
        let (chunk, rest) = rest.split_at(len);

        result.push(std::str::from_utf8(chunk).unwrap());
        data = rest;
    }

    result
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let token_data = std::fs::read(&args[2]).unwrap();
    let resolver = BasicTokenResolver::from_text_lines(token_data.as_slice()).unwrap();

    let mut out = std::io::stdout().lock();
    let file = std::fs::File::open(&args[1]).unwrap();
    let file = JominiFile::from_file(file).unwrap();
    let JominiFileKind::Zip(zip) = file.kind() else {
        panic!("expected zip file");
    };

    let mut lookup_data = Vec::new();
    zip.get_file("string_lookup")
        .unwrap()
        .read_to_end(&mut lookup_data)
        .unwrap();
    let string_lookup = string_lookup_parse(&lookup_data);

    let mut new_header = zip.header().clone();
    new_header.set_kind(jomini::envelope::SaveHeaderKind::Text);
    new_header.write(&mut out).unwrap();
    // let mut meta: Vec<u8> = Vec::new();
    // let cursor = Cursor::new(&mut meta);
    let mut writer = jomini::TextWriterBuilder::new()
        .indent_char(b'\t')
        .indent_factor(1)
        .from_writer(out);

    let gamestate = zip.gamestate().unwrap();
    let SaveContentKind::Binary(bin) = gamestate else {
        panic!("expected binary gamestate");
    };

    let mut reader = jomini::binary::TokenReader::new(bin);
    while let Some(token) = reader.next_token().unwrap() {
        match reader.token_from_kind(token) {
            jomini::binary::Token::Open => writer.write_start(),
            jomini::binary::Token::Close => writer.write_end(),
            jomini::binary::Token::Equal => writer.write_operator(jomini::text::Operator::Equal),
            jomini::binary::Token::I64(x) => writer.write_i64(x),
            jomini::binary::Token::F64(x) => writer.write_f64(visit_f64(x)),
            jomini::binary::Token::Bool(b) => writer.write_bool(b),
            jomini::binary::Token::Rgb(rgb) => writer.write_rgb(&rgb),
            jomini::binary::Token::Id(x) => {
                writer.write_unquoted(resolver.resolve(x).unwrap().as_bytes())
            }
            jomini::binary::Token::Lookup(x) => {
                writer.write_unquoted(string_lookup[x as usize].as_bytes())
            }
            jomini::binary::Token::Lookup1(x) => {
                writer.write_unquoted(string_lookup[x as usize].as_bytes())
            }
            jomini::binary::Token::U32(x) => writer.write_u32(x),
            jomini::binary::Token::U64(x) => writer.write_u64(x),
            jomini::binary::Token::I32(x) => writer.write_i32(x),
            jomini::binary::Token::Quoted(scalar) => writer.write_quoted(scalar.as_bytes()),
            jomini::binary::Token::Unquoted(scalar) => writer.write_unquoted(scalar.as_bytes()),
            jomini::binary::Token::F32(x) => writer.write_f32(visit_f32(x)),
        }
        .unwrap();
    }
}

fn visit_f32(data: [u8; 4]) -> f32 {
    debug_assert!(false, "first save with f32 data");
    f32::from_bits(u32::from_le_bytes(data))
}

fn visit_f64(data: [u8; 8]) -> f64 {
    let x = i64::from_le_bytes(data) as f64;
    let eps = f64::from(f32::EPSILON);
    (x + (eps * x.signum())).trunc() / 100_000.0
}
