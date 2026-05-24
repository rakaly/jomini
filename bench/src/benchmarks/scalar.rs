use jomini::Scalar;
use std::hint::black_box;

#[inline(never)]
pub(crate) fn to_u64(data: &[u8]) -> u64 {
    black_box(Scalar::new(black_box(data)).to_u64().unwrap())
}

#[inline(never)]
pub(crate) fn to_f64(data: &[u8]) -> f64 {
    black_box(Scalar::new(black_box(data)).to_f64().unwrap())
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion};

    pub fn to_u64_benchmark(c: &mut Criterion) {
        let mut group = c.benchmark_group("to_u64");
        for input in [
            &b"7"[..],
            &b"30"[..],
            &b"1444"[..],
            &b"20405029"[..],
            &b"20405029553322"[..],
        ] {
            let name = std::str::from_utf8(input).unwrap();
            group.bench_function(BenchmarkId::from_parameter(name), |b| {
                b.iter(|| to_u64(input))
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
            let name = std::str::from_utf8(input).unwrap();
            group.bench_function(BenchmarkId::from_parameter(name), |b| {
                b.iter(|| to_f64(input))
            });
        }
        group.finish();
    }

    criterion::criterion_group!(scalar_benches, to_u64_benchmark, to_f64_benchmark);
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::n_7(b"7")]
    #[bench::n_30(b"30")]
    #[bench::n_1444(b"1444")]
    #[bench::n_20405029(b"20405029")]
    #[bench::n_20405029553322(b"20405029553322")]
    fn u64_parse(data: &[u8]) -> u64 {
        to_u64(data)
    }

    #[library_benchmark]
    #[bench::n_10(b"10")]
    #[bench::n_neg_1000(b"-1000")]
    #[bench::n_0_000(b"0.000")]
    #[bench::n_20405029_125(b"20405029.125")]
    fn f64_parse(data: &[u8]) -> f64 {
        to_f64(data)
    }

    library_benchmark_group!(name = scalar_benches, benchmarks = [u64_parse, f64_parse,]);
}
