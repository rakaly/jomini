use criterion::criterion_main;
use eu4save_bench::benchmarks::{country_tag_hashing, deserialize_ledger, parse_save};

criterion_main!(
    parse_save::criterion_benches::parse_save_benches,
    deserialize_ledger::criterion_benches::deserialize_ledger_benches,
    country_tag_hashing::criterion_benches::country_tag_hashing_benches,
);
