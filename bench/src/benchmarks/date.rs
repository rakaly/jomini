use jomini::common::{Date, DateError};
use std::hint::black_box;

#[inline(never)]
pub(crate) fn parse_date(date: &str) -> Result<Date, DateError> {
    Date::parse(black_box(date))
}

#[inline(never)]
pub(crate) fn date_from_binary(date: i32) -> Option<Date> {
    Date::from_binary(black_box(date))
}

#[inline(never)]
pub(crate) fn compare_dates(date1: Date, date2: Date) -> (bool, bool, bool) {
    black_box((date1 < date2, date1 > date2, date1 == date2))
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion};

    pub fn date(c: &mut Criterion) {
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
            group.bench_function(BenchmarkId::from_parameter(date), |b| {
                b.iter(|| parse_date(date))
            });
        }

        for date in [56379360, 0] {
            group.bench_function(BenchmarkId::from_parameter(date), |b| {
                b.iter(|| date_from_binary(date))
            });
        }

        group.finish();
    }

    pub fn date_comparison(c: &mut Criterion) {
        let date_pairs = date_pairs();
        c.bench_function("date_comparison", |b| {
            b.iter(|| {
                for (date1, date2) in &date_pairs {
                    compare_dates(*date1, *date2);
                }
            })
        });
    }

    criterion::criterion_group!(date_benches, date, date_comparison);
}

fn date_pairs() -> [(Date, Date); 7] {
    [
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
    ]
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::d_1444_11_11("1444.11.11")]
    #[bench::d_1444_3_5("1444.3.5")]
    #[bench::d_1444_12_3("1444.12.3")]
    #[bench::d_1444_2_19("1444.2.19")]
    #[bench::d_1_1_1("1.1.1")]
    #[bench::d_neg_2500_1_1("-2500.1.1")]
    #[bench::invalid_date("invalid-date")]
    #[bench::longer_invalid_date("longer-invalid-date")]
    fn parse(date: &str) -> Result<Date, DateError> {
        parse_date(date)
    }

    #[library_benchmark]
    #[bench::d_56379360(56379360)]
    #[bench::d_0(0)]
    fn from_binary(date: i32) -> Option<Date> {
        date_from_binary(date)
    }

    #[library_benchmark]
    #[bench::all(setup = date_pairs)]
    fn comparison(date_pairs: [(Date, Date); 7]) -> (bool, bool, bool) {
        let mut result = (false, false, false);
        for (date1, date2) in date_pairs {
            result = compare_dates(date1, date2);
        }
        result
    }

    library_benchmark_group!(
        name = date_benches,
        benchmarks = [parse, from_binary, comparison,]
    );
}
