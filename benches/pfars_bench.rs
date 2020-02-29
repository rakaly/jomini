use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use jomini::{BinaryParser, Scalar};
use std::collections::HashMap;
use std::io::Read;

#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

const META_DATA: &'static [u8] = include_bytes!("../../../assets/fixtures/meta.bin");

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

pub fn to_string_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("to_string");
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

pub fn binary_deserialize_benchmark(c: &mut Criterion) {
    #[derive(serde::Deserialize, PartialEq, Eq, Debug)]
    struct Meta {
        campaign_id: String,
    }

    let data = &META_DATA["EU4bin".len()..];
    let mut group = c.benchmark_group("binary_deserialize");
    let mut map = HashMap::new();
    map.insert(0x337f, "campaign_id");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta", |b| {
        b.iter(|| { let _res: Meta = jomini::de::binary::from_slice(&data[..], &map).unwrap();})
    });
    group.bench_function("meta-tape", |b| {
        b.iter(|| { let _res: Meta = jomini::de::tape::from_slice(&data[..], &map).unwrap();})
    });
    group.finish();
}

pub fn binary_parse_benchmark(c: &mut Criterion) {
    let data = &META_DATA["EU4bin".len()..];
    let mut group = c.benchmark_group("binary_parse");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.bench_function("meta", |b| {
        b.iter(|| {
            let mut parser = BinaryParser::new();
            let mut count = 0;
            for event in parser.events(&data[..]) {
                event.unwrap();
                count += 1;
            }
            count
        })
    });
    group.bench_function("meta-document", |b| {
        b.iter(|| jomini::document::document_from_slice(&data[..]).unwrap())
    });
    group.bench_function("meta-nom", |b| {
        b.iter(|| jomini::nommer::parse(&data[..]).unwrap())
    });
    group.bench_function("meta-tape", |b| {
        b.iter(|| jomini::scratch::parse(&data[..]).unwrap())
    });
    group.finish();
}

criterion_group!(
    benches,
    is_ascii_benchmark,
    to_string_benchmark,
    binary_parse_benchmark,
    binary_deserialize_benchmark,
);
criterion_main!(benches);
