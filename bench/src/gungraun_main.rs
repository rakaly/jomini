use jomini_bench::benchmarks::{
    binary::gungraun_benches::binary_benches, date::gungraun_benches::date_benches,
    encoding::gungraun_benches::encoding_benches, scalar::gungraun_benches::scalar_benches,
    text::gungraun_benches::text_benches,
};

#[cfg(not(feature = "json"))]
gungraun::main!(
    library_benchmark_groups = encoding_benches,
    binary_benches,
    text_benches,
    scalar_benches,
    date_benches
);

#[cfg(feature = "json")]
use jomini_bench::benchmarks::json::gungraun_benches::json_benches;

#[cfg(feature = "json")]
gungraun::main!(
    library_benchmark_groups = encoding_benches,
    binary_benches,
    text_benches,
    scalar_benches,
    date_benches,
    json_benches
);
