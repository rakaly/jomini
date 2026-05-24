use jomini::{Utf8Encoding, Windows1252Encoding};
use std::{borrow::Cow, hint::black_box};

const SIZES: [usize; 9] = [2, 4, 8, 16, 32, 64, 128, 256, 512];

#[inline(never)]
pub(crate) fn decode_windows1252(data: &[u8]) -> Cow<'_, str> {
    Windows1252Encoding::decode(data)
}

#[inline(never)]
pub(crate) fn decode_utf8(data: &[u8]) -> Cow<'_, str> {
    Utf8Encoding::decode(data)
}

#[inline(never)]
fn decoded_len(decoded: Cow<'_, str>) -> usize {
    black_box(decoded.len())
}

fn ascii_input(size: usize) -> Vec<u8> {
    vec![b'a'; size]
}

fn windows1252_input(size: usize) -> Vec<u8> {
    vec![0xfe; size]
}

fn utf8_input(size: usize) -> Vec<u8> {
    let mut utf8 = String::with_capacity(size);
    for _ in 0..size / 2 {
        utf8.push('å');
    }
    utf8.into_bytes()
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion, Throughput};

    pub fn windows1252(c: &mut Criterion) {
        let mut group = c.benchmark_group("windows1252");
        for size in SIZES {
            let ascii = ascii_input(size);
            let encoded = windows1252_input(size);
            group.throughput(Throughput::Bytes(size as u64));
            group.bench_function(BenchmarkId::new("ascii-to-utf8", size), |b| {
                b.iter(|| decode_windows1252(&ascii))
            });
            group.bench_function(BenchmarkId::new("1252-to-utf8", size), |b| {
                b.iter(|| decode_windows1252(&encoded))
            });
        }
        group.finish();
    }

    pub fn utf8(c: &mut Criterion) {
        let mut group = c.benchmark_group("utf8");
        for size in SIZES {
            let ascii = ascii_input(size);
            let utf8 = utf8_input(size);
            group.throughput(Throughput::Bytes(size as u64));
            group.bench_function(BenchmarkId::new("ascii-to-utf8", size), |b| {
                b.iter(|| decode_utf8(&ascii))
            });
            group.bench_function(BenchmarkId::new("utf8-to-utf8", size), |b| {
                b.iter(|| decode_utf8(&utf8))
            });
        }
        group.finish();
    }

    criterion::criterion_group!(encoding_benches, windows1252, utf8);
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::size_2(args = (2), setup = ascii_input)]
    #[bench::size_4(args = (4), setup = ascii_input)]
    #[bench::size_8(args = (8), setup = ascii_input)]
    #[bench::size_16(args = (16), setup = ascii_input)]
    #[bench::size_32(args = (32), setup = ascii_input)]
    #[bench::size_64(args = (64), setup = ascii_input)]
    #[bench::size_128(args = (128), setup = ascii_input)]
    #[bench::size_256(args = (256), setup = ascii_input)]
    #[bench::size_512(args = (512), setup = ascii_input)]
    fn windows1252_ascii(data: Vec<u8>) -> usize {
        decoded_len(decode_windows1252(&data))
    }

    #[library_benchmark]
    #[bench::size_2(args = (2), setup = windows1252_input)]
    #[bench::size_4(args = (4), setup = windows1252_input)]
    #[bench::size_8(args = (8), setup = windows1252_input)]
    #[bench::size_16(args = (16), setup = windows1252_input)]
    #[bench::size_32(args = (32), setup = windows1252_input)]
    #[bench::size_64(args = (64), setup = windows1252_input)]
    #[bench::size_128(args = (128), setup = windows1252_input)]
    #[bench::size_256(args = (256), setup = windows1252_input)]
    #[bench::size_512(args = (512), setup = windows1252_input)]
    fn windows1252_encoded(data: Vec<u8>) -> usize {
        decoded_len(decode_windows1252(&data))
    }

    #[library_benchmark]
    #[bench::size_2(args = (2), setup = ascii_input)]
    #[bench::size_4(args = (4), setup = ascii_input)]
    #[bench::size_8(args = (8), setup = ascii_input)]
    #[bench::size_16(args = (16), setup = ascii_input)]
    #[bench::size_32(args = (32), setup = ascii_input)]
    #[bench::size_64(args = (64), setup = ascii_input)]
    #[bench::size_128(args = (128), setup = ascii_input)]
    #[bench::size_256(args = (256), setup = ascii_input)]
    #[bench::size_512(args = (512), setup = ascii_input)]
    fn utf8_ascii(data: Vec<u8>) -> usize {
        decoded_len(decode_utf8(&data))
    }

    #[library_benchmark]
    #[bench::size_2(args = (2), setup = utf8_input)]
    #[bench::size_4(args = (4), setup = utf8_input)]
    #[bench::size_8(args = (8), setup = utf8_input)]
    #[bench::size_16(args = (16), setup = utf8_input)]
    #[bench::size_32(args = (32), setup = utf8_input)]
    #[bench::size_64(args = (64), setup = utf8_input)]
    #[bench::size_128(args = (128), setup = utf8_input)]
    #[bench::size_256(args = (256), setup = utf8_input)]
    #[bench::size_512(args = (512), setup = utf8_input)]
    fn utf8_encoded(data: Vec<u8>) -> usize {
        decoded_len(decode_utf8(&data))
    }

    library_benchmark_group!(
        name = encoding_benches,
        benchmarks = [
            windows1252_ascii,
            windows1252_encoded,
            utf8_ascii,
            utf8_encoded,
        ]
    );
}
