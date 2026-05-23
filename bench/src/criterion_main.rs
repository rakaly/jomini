use criterion::criterion_main;
use jomini_bench::benchmarks::{binary, date, encoding, scalar, text};

#[cfg(not(feature = "json"))]
criterion_main!(
    encoding::criterion_benches::encoding_benches,
    binary::criterion_benches::binary_benches,
    text::criterion_benches::text_benches,
    scalar::criterion_benches::scalar_benches,
    date::criterion_benches::date_benches,
);

#[cfg(feature = "json")]
criterion_main!(
    encoding::criterion_benches::encoding_benches,
    binary::criterion_benches::binary_benches,
    text::criterion_benches::text_benches,
    scalar::criterion_benches::scalar_benches,
    date::criterion_benches::date_benches,
    jomini_bench::benchmarks::json::criterion_benches::json_benches,
);
