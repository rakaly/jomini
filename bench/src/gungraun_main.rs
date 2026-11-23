#[cfg(not(target_family = "wasm"))]
use jomini_bench::benchmarks::{
    arena::gungraun_benches::arena_benches, binary::gungraun_benches::binary_benches,
    date::gungraun_benches::date_benches, encoding::gungraun_benches::encoding_benches,
    eu4::parse_save::gungraun_benches::parse_save_benches, eu5::gungraun_benches::eu5_benches,
    melt::gungraun_benches::melt_benches, scalar::gungraun_benches::scalar_benches,
    text::gungraun_benches::text_benches,
};

#[cfg(all(not(target_family = "wasm"), not(feature = "json")))]
gungraun::main!(
    config = gungraun::LibraryBenchmarkConfig::default().pass_through_env("PDX_TOKENS_DIR");
    library_benchmark_groups = encoding_benches,
    binary_benches,
    text_benches,
    scalar_benches,
    date_benches,
    parse_save_benches,
    arena_benches,
    eu5_benches,
    melt_benches
);

#[cfg(all(not(target_family = "wasm"), feature = "json"))]
use jomini_bench::benchmarks::json::gungraun_benches::json_benches;

#[cfg(all(not(target_family = "wasm"), feature = "json"))]
gungraun::main!(
    config = gungraun::LibraryBenchmarkConfig::default().pass_through_env("PDX_TOKENS_DIR");
    library_benchmark_groups = encoding_benches,
    binary_benches,
    text_benches,
    scalar_benches,
    date_benches,
    parse_save_benches,
    arena_benches,
    eu5_benches,
    melt_benches,
    json_benches
);

// Gungraun needs Valgrind, which does not run wasm.
#[cfg(target_family = "wasm")]
fn main() {}
