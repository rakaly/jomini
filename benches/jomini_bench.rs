use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use flate2::read::GzDecoder;
use jomini::{
    binary::{BinaryFlavor, TokenResolver},
    common::Date,
    Encoding, Scalar, TextTape, Utf8Encoding, Windows1252Encoding,
};
use std::{borrow::Cow, io::Read};

const METADATA_TXT: &[u8] = include_bytes!("../tests/fixtures/meta.txt");
const CK3_TXT: &[u8] = include_bytes!("../tests/fixtures/ck3-header.txt");

pub fn windows1252_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("windows1252");
    for size in [2, 4, 8, 16, 32, 64, 128, 256, 512].iter() {
        let data = vec![b'a'; *size as usize];
        let data2 = vec![0xfe; *size as usize];
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("ascii-to-utf8", size),
            size,
            |b, &_size| b.iter(|| Windows1252Encoding::decode(&data)),
        );
        group.bench_with_input(BenchmarkId::new("1252-to-utf8", size), size, |b, &_size| {
            b.iter(|| Windows1252Encoding::decode(&data2))
        });
    }
    group.finish();
}

pub fn utf8_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("utf8");
    for size in [2, 4, 8, 16, 32, 64, 128, 256, 512].iter() {
        let mut ascii_str = String::with_capacity(*size);
        let mut utf8_str = String::with_capacity(*size / 2_usize);
        for _ in 0..*size {
            ascii_str.push('a');
        }

        for _ in 0..size / 2 {
            utf8_str.push('å');
        }
        let data = ascii_str.as_bytes();
        let data2 = utf8_str.as_bytes();
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(
            BenchmarkId::new("ascii-to-utf8", size),
            size,
            |b, &_size| b.iter(|| Utf8Encoding::decode(data)),
        );
        group.bench_with_input(BenchmarkId::new("utf8-to-utf8", size), size, |b, &_size| {
            b.iter(|| Utf8Encoding::decode(data2))
        });
    }
    group.finish();
}

pub fn to_u64_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_u64");
    for input in [
        &b"7"[..],
        &b"30"[..],
        &b"1444"[..],
        &b"20405029"[..],
        &b"20405029553322"[..],
    ] {
        let data = Scalar::new(input);
        let ins = std::str::from_utf8(input).unwrap();
        group.bench_with_input(BenchmarkId::from_parameter(ins), &data, |b, &data| {
            b.iter(|| black_box(data.to_u64().unwrap()))
        });
    }
    group.finish();
}

pub fn to_f64_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_f64");
    for input in [
        &b"10"[..],
        &b"-1000"[..],
        &b"0.000"[..],
        &b"20405029.125"[..],
    ] {
        let data = Scalar::new(input);
        let ins = std::str::from_utf8(input).unwrap();
        group.bench_with_input(BenchmarkId::from_parameter(ins), &data, |b, &data| {
            b.iter(|| black_box(data.to_f64().unwrap()))
        });
    }
    group.finish();
}

pub fn binary_deserialize_benchmark(c: &mut Criterion) {
    #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
    struct Gamestate {
        pub current_age: String,
    }

    #[derive(Debug, Default)]
    pub struct BinaryTestFlavor;

    impl BinaryFlavor for BinaryTestFlavor {
        fn visit_f32(&self, data: [u8; 4]) -> f32 {
            f32::from_le_bytes(data)
        }

        fn visit_f64(&self, data: [u8; 8]) -> f64 {
            f64::from_le_bytes(data)
        }
    }

    impl Encoding for BinaryTestFlavor {
        fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
            Windows1252Encoding::decode(data)
        }
    }

    struct MyBinaryResolver;

    impl TokenResolver for MyBinaryResolver {
        fn resolve(&self, token: u16) -> Option<&str> {
            if token == 0x3564 {
                Some("current_age")
            } else {
                None
            }
        }
    }

    let mut group = c.benchmark_group("binary-deserialize");
    let data = request("jomini/eu4-bin");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("ondemand", |b| {
        b.iter(|| {
            let _res: Gamestate = BinaryTestFlavor
                .deserializer()
                .deserialize_slice(&data[..], &MyBinaryResolver)
                .unwrap();
        })
    });
    group.bench_function("ondemand-reader", |b| {
        b.iter(|| {
            let _res: Gamestate = BinaryTestFlavor
                .deserializer()
                .deserialize_reader(&data[..], &MyBinaryResolver)
                .unwrap();
        })
    });
    group.finish();
}

pub fn text_deserialize_benchmark(c: &mut Criterion) {
    #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
    struct Meta {
        campaign_id: String,
    }

    let data = &METADATA_TXT["EU4txt".len()..];
    let mut group = c.benchmark_group("deserialize");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("text", |b| {
        b.iter(|| {
            let _res: Meta = jomini::text::de::from_windows1252_slice(data).unwrap();
        })
    });
    group.finish();
}

pub fn binary_parse_benchmark(c: &mut Criterion) {
    // For the binary parse benchmarks we actually benchmark against a real
    // world corpus of save files. Previously it was only against the binary
    // metadata section of saves, but found out that improvements in benchmarks
    // didn't translate to world world change.

    let mut group = c.benchmark_group("parse");

    for game in &["eu4", "ck3", "v3"] {
        let data = request(format!("jomini/{game}-bin"));
        group.throughput(Throughput::Bytes(data.len() as u64));

        group.bench_function(BenchmarkId::new("lexer", game), |b| {
            b.iter(|| {
                let mut lexer = jomini::binary::Lexer::new(data.as_slice());
                let mut counter = 0;
                while let Ok(Some(token)) = lexer.next_token() {
                    if matches!(token, jomini::binary::Token::Id(_)) {
                        counter += 1;
                    }
                }
                black_box(counter);
            })
        });

        group.bench_function(BenchmarkId::new("reader", game), |b| {
            b.iter(|| {
                let mut reader = jomini::binary::TokenReader::new(data.as_slice());
                let mut counter = 0;
                while let Ok(Some(token)) = reader.next() {
                    if matches!(token, jomini::binary::Token::Id(_)) {
                        counter += 1;
                    }
                }
                black_box(counter);
            })
        });
    }

    group.finish();
}

pub fn text_parse_benchmark(c: &mut Criterion) {
    let data = &METADATA_TXT["EU4txt".len()..];
    let mut group = c.benchmark_group("parse");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function(BenchmarkId::new("text", "eu4"), |b| {
        let mut tape = TextTape::default();
        b.iter(|| {
            TextTape::parser()
                .parse_slice_into_tape(data, &mut tape)
                .unwrap();
        })
    });

    let data = CK3_TXT;
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function(BenchmarkId::new("text", "ck3"), |b| {
        let mut tape = TextTape::default();
        b.iter(|| {
            TextTape::parser()
                .parse_slice_into_tape(data, &mut tape)
                .unwrap();
        })
    });

    group.finish();
}

#[cfg(not(feature = "json"))]
pub fn json_benchmark(_c: &mut Criterion) {}

#[cfg(feature = "json")]
pub fn json_benchmark(c: &mut Criterion) {
    use jomini::json::{DuplicateKeyMode, JsonOptions};

    let data = &METADATA_TXT["EU4txt".len()..];
    let tape = TextTape::from_slice(data).unwrap();

    let mut group = c.benchmark_group("json");

    let bytes = tape.windows1252_reader().json().to_string().len();
    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("preserve", "eu4"), |b| {
        b.iter(|| tape.windows1252_reader().json().to_string())
    });

    let bytes = tape
        .windows1252_reader()
        .json()
        .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group))
        .to_string()
        .len();

    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("group", "eu4"), |b| {
        b.iter(|| {
            tape.windows1252_reader()
                .json()
                .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group))
                .to_string()
        })
    });

    let bytes = tape
        .windows1252_reader()
        .json()
        .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::KeyValuePairs))
        .to_string()
        .len();

    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("typed", "eu4"), |b| {
        b.iter(|| {
            tape.windows1252_reader()
                .json()
                .with_options(
                    JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::KeyValuePairs),
                )
                .to_string()
        })
    });

    let data = &CK3_TXT[..];
    let tape = TextTape::from_slice(data).unwrap();
    let bytes = tape.windows1252_reader().json().to_string().len();
    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("preserve", "ck3"), |b| {
        b.iter(|| tape.windows1252_reader().json().to_string())
    });

    let bytes = tape
        .windows1252_reader()
        .json()
        .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group))
        .to_string()
        .len();

    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("group", "ck3"), |b| {
        b.iter(|| {
            tape.windows1252_reader()
                .json()
                .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group))
                .to_string()
        })
    });

    let bytes = tape
        .windows1252_reader()
        .json()
        .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::KeyValuePairs))
        .to_string()
        .len();

    group.throughput(Throughput::Bytes(bytes as u64));
    group.bench_function(BenchmarkId::new("typed", "ck3"), |b| {
        b.iter(|| {
            tape.windows1252_reader()
                .json()
                .with_options(
                    JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::KeyValuePairs),
                )
                .to_string()
        })
    });

    group.finish();
}

pub fn date_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("date");
    for date in [
        "1444.11.11",
        "1444.3.5",
        "1444.12.3",
        "1444.2.19",
        "1.1.1",
        "-2500.1.1",
        "invalid-date",
        "longer-invalid-date",
    ] {
        group.bench_with_input(BenchmarkId::from_parameter(date), date, |b, date| {
            b.iter(|| Date::parse(date))
        });
    }

    for date in [56379360, 0].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(date), date, |b, &date| {
            b.iter(|| Date::from_binary(date))
        });
    }

    group.finish();
}

pub fn date_comparison_benchmark(c: &mut Criterion) {
    let date_pairs = [
        (
            Date::parse("1444.11.11").unwrap(),
            Date::parse("1444.3.5").unwrap(),
        ),
        (
            Date::parse("1500.1.1").unwrap(),
            Date::parse("1500.12.31").unwrap(),
        ),
        (
            Date::parse("1600.6.15").unwrap(),
            Date::parse("1700.6.15").unwrap(),
        ),
        (
            Date::parse("1400.1.1").unwrap(),
            Date::parse("1800.12.31").unwrap(),
        ),
        (
            Date::parse("-10.5.5").unwrap(),
            Date::parse("10.5.5").unwrap(),
        ),
        (
            Date::parse("-100.1.1").unwrap(),
            Date::parse("-50.12.31").unwrap(),
        ),
        (
            Date::parse("2000.2.28").unwrap(),
            Date::parse("2000.3.1").unwrap(),
        ),
    ];

    c.bench_function("date_comparison", |b| {
        b.iter(|| {
            for (date1, date2) in &date_pairs {
                black_box(date1 < date2);
                black_box(date1 > date2);
                black_box(date1 == date2);
            }
        });
    });
}

criterion_group!(
    benches,
    utf8_benchmark,
    windows1252_benchmark,
    binary_parse_benchmark,
    text_parse_benchmark,
    binary_deserialize_benchmark,
    text_deserialize_benchmark,
    to_u64_benchmark,
    to_f64_benchmark,
    date_benchmark,
    json_benchmark,
    date_comparison_benchmark,
);
criterion_main!(benches);

pub fn request<S: AsRef<str>>(input: S) -> Vec<u8> {
    use std::fs;
    use std::path::Path;

    let reffed = input.as_ref();
    let cache = Path::new("assets").join("saves").join(reffed);
    if cache.exists() {
        println!("cache hit: {}", reffed);
        fs::read(cache).unwrap()
    } else {
        println!("cache miss: {}", reffed);
        let url = format!(
            "https://eu4saves-test-cases.s3.us-west-002.backblazeb2.com/{}.gz",
            reffed
        );
        let resp = attohttpc::get(&url).send().unwrap();

        if !resp.is_success() {
            panic!("expected a 200 code from s3");
        } else {
            let raw_data = resp.bytes().unwrap();

            let mut data = Vec::new();
            GzDecoder::new(raw_data.as_slice())
                .read_to_end(&mut data)
                .unwrap();

            std::fs::create_dir_all(cache.parent().unwrap()).unwrap();
            std::fs::write(&cache, &data).unwrap();
            data
        }
    }
}
