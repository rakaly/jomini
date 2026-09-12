use criterion::criterion_main;
use jomini_bench::benchmarks::{arena, binary, date, encoding, eu4, scalar, text};

#[cfg(not(feature = "json"))]
criterion_main!(
    encoding::criterion_benches::encoding_benches,
    binary::criterion_benches::binary_benches,
    text::criterion_benches::text_benches,
    scalar::criterion_benches::scalar_benches,
    date::criterion_benches::date_benches,
    eu4::parse_save::criterion_benches::parse_save_benches,
    eu4::deserialize_ledger::criterion_benches::deserialize_ledger_benches,
    eu4::country_tag_hashing::criterion_benches::country_tag_hashing_benches,
    arena::criterion_benches::arena_benches,
);

#[cfg(feature = "json")]
criterion_main!(
    encoding::criterion_benches::encoding_benches,
    binary::criterion_benches::binary_benches,
    text::criterion_benches::text_benches,
    scalar::criterion_benches::scalar_benches,
    date::criterion_benches::date_benches,
    eu4::parse_save::criterion_benches::parse_save_benches,
    eu4::deserialize_ledger::criterion_benches::deserialize_ledger_benches,
    eu4::country_tag_hashing::criterion_benches::country_tag_hashing_benches,
    arena::criterion_benches::arena_benches,
    jomini_bench::benchmarks::json::criterion_benches::json_benches,
);
