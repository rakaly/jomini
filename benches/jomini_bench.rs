use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jomini::{
    common::Date, BinaryDeserializer, BinaryTape, Scalar, TextDeserializer, TextTape, Utf8Encoding,
    Windows1252Encoding,
};
use std::collections::HashMap;

const METADATA_BIN: &'static [u8] = include_bytes!("../tests/fixtures/meta.bin");
const METADATA_TXT: &'static [u8] = include_bytes!("../tests/fixtures/meta.txt");
const CK3_BIN: &'static [u8] = include_bytes!("../tests/fixtures/ck3-header.bin");
const CK3_TXT: &'static [u8] = include_bytes!("../tests/fixtures/ck3-header.txt");

pub fn is_ascii_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("is_ascii");
    for size in [1, 4, 8, 16, 32, 64, 128, 256, 512].iter() {
        let data = vec![b'a'; *size as usize];
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, &_size| {
            b.iter(|| Scalar::new(black_box(&data)).is_ascii());
        });
    }
    group.finish();
}

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
        let mut ascii_str = String::with_capacity(*size as usize);
        let mut utf8_str = String::with_capacity(*size / 2 as usize);
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
            |b, &_size| b.iter(|| Utf8Encoding::decode(&data)),
        );
        group.bench_with_input(BenchmarkId::new("utf8-to-utf8", size), size, |b, &_size| {
            b.iter(|| Utf8Encoding::decode(&data2))
        });
    }
    group.finish();
}

pub fn to_u64_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_u64");
    for input in [&b"7"[..], &b"1444"[..], &b"20405029"[..]].iter() {
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
    for input in [&b"10"[..], &b"-1000"[..], &b"20405029.125"[..]].iter() {
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
    struct Meta {
        campaign_id: String,
    }

    let data = &METADATA_BIN["EU4bin".len()..];
    let mut group = c.benchmark_group("deserialize");
    let mut map = HashMap::new();
    map.insert(0x337f, "campaign_id");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("binary", |b| {
        b.iter(|| {
            let _res: Meta = BinaryDeserializer::eu4_builder()
                .from_slice(&data[..], &map)
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
            let _res: Meta = TextDeserializer::from_windows1252_slice(&data[..]).unwrap();
        })
    });
    group.finish();
}

pub fn binary_parse_benchmark(c: &mut Criterion) {
    let data = &METADATA_BIN["EU4bin".len()..];
    let mut group = c.benchmark_group("parse");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function(BenchmarkId::new("binary", "eu4"), |b| {
        let mut tape = BinaryTape::default();
        b.iter(move || {
            BinaryTape::eu4_parser()
                .parse_slice_into_tape(&data[..], &mut tape)
                .unwrap();
        })
    });

    let data = &CK3_BIN[..];
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function(BenchmarkId::new("binary", "ck3"), |b| {
        let mut tape = BinaryTape::default();
        b.iter(move || {
            BinaryTape::eu4_parser()
                .parse_slice_into_tape(&data[..], &mut tape)
                .unwrap();
        })
    });

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
                .parse_slice_into_tape(&data[..], &mut tape)
                .unwrap();
        })
    });

    let data = &CK3_TXT[..];
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function(BenchmarkId::new("text", "ck3"), |b| {
        let mut tape = TextTape::default();
        b.iter(|| {
            TextTape::parser()
                .parse_slice_into_tape(&data[..], &mut tape)
                .unwrap();
        })
    });

    group.finish();
}

pub fn date_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("eu4date-parse");
    group.bench_function("valid-date", |b| {
        b.iter(|| Date::parse_from_str("1444.11.11").unwrap())
    });
    group.bench_function("invalid-date", |b| {
        b.iter(|| Date::parse_from_str("marketplace").is_none())
    });
    group.bench_function("long-invalid-date", |b| {
        b.iter(|| Date::parse_from_str("incidents_bur_inheritance.5").is_none())
    });
    group.finish();
}

criterion_group!(
    benches,
    is_ascii_benchmark,
    utf8_benchmark,
    windows1252_benchmark,
    binary_parse_benchmark,
    text_parse_benchmark,
    binary_deserialize_benchmark,
    text_deserialize_benchmark,
    to_u64_benchmark,
    to_f64_benchmark,
    date_benchmark,
);
criterion_main!(benches);
