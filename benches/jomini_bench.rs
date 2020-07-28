use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jomini::{BinaryDeserializer, BinaryTape, Scalar, TextDeserializer, TextTape};
use std::collections::HashMap;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

const METADATA_BIN: &'static [u8] = include_bytes!("../tests/fixtures/meta.bin");
const METADATA_TXT: &'static [u8] = include_bytes!("../tests/fixtures/meta.txt");

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

pub fn from_ascii_string_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("from_ascii_string");
    for size in [1, 4, 8, 16, 32, 64, 128, 256, 512].iter() {
        let data = vec![b'a'; *size as usize];
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::new("to-utf8", size), size, |b, &_size| {
            b.iter(|| Scalar::new(black_box(&data)).to_utf8())
        });
        group.bench_with_input(
            BenchmarkId::new("to-utf8-owned", size),
            size,
            |b, &_size| b.iter(|| Scalar::new(black_box(&data)).to_utf8_owned()),
        );
    }
    group.finish();
}

pub fn from_windows_string_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("from_windows1252_string");
    for size in [1, 4, 8, 16, 32, 64, 128, 256, 512].iter() {
        let data = vec![0xfe; *size as usize];
        group.throughput(Throughput::Bytes(*size as u64));
        group.bench_with_input(BenchmarkId::new("to-utf8", size), size, |b, &_size| {
            b.iter(|| Scalar::new(black_box(&data)).to_utf8())
        });
    }
    group.finish();
}

pub fn to_u64_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_u64");
    for input in [&b"1444"[..], &b"20405029"[..]].iter() {
        let data = Scalar::new(input);
        let ins = std::str::from_utf8(input).unwrap();
        group.bench_with_input(BenchmarkId::from_parameter(ins), &data, |b, &data| {
            b.iter(|| black_box(data.to_u64().unwrap()))
        });
    }
    group.finish();
}

pub fn to_f64_benchmark(c: &mut Criterion) {
    c.bench_function("to_f64", |b| {
        b.iter(|| Scalar::new(b"20405029.125").to_f64())
    });
}

pub fn binary_deserialize_benchmark(c: &mut Criterion) {
    #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
    struct Meta {
        campaign_id: String,
    }

    let data = &METADATA_BIN["EU4bin".len()..];
    let mut group = c.benchmark_group("binary_deserialize");
    let mut map = HashMap::new();
    map.insert(0x337f, "campaign_id");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta-tape", |b| {
        b.iter(|| {
            let _res: Meta = BinaryDeserializer::from_slice(&data[..], &map).unwrap();
        })
    });
    group.finish();
}

pub fn binary_parse_benchmark(c: &mut Criterion) {
    let data = &METADATA_BIN["EU4bin".len()..];
    let mut group = c.benchmark_group("binary_parse");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta-tape", |b| {
        let mut tape = BinaryTape::default();
        b.iter(|| {
            tape.parse(&data[..]).unwrap();
            tape.clear();
        })
    });
    group.finish();
}

pub fn text_parse_benchmark(c: &mut Criterion) {
    let data = &METADATA_TXT["EU4txt".len()..];
    let mut group = c.benchmark_group("text_parse");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta-tape", |b| {
        let mut tape = TextTape::default();
        b.iter(|| {
            tape.parse(&data[..]).unwrap();
            tape.clear();
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
    let mut group = c.benchmark_group("text_deserialize");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta-tape", |b| {
        b.iter(|| {
            let _res: Meta = TextDeserializer::from_slice(&data[..]).unwrap();
        })
    });
    group.finish();
}

criterion_group!(
    benches,
    is_ascii_benchmark,
    from_ascii_string_benchmark,
    from_windows_string_benchmark,
    binary_parse_benchmark,
    text_parse_benchmark,
    binary_deserialize_benchmark,
    text_deserialize_benchmark,
    to_u64_benchmark,
    to_f64_benchmark,
);
criterion_main!(benches);
