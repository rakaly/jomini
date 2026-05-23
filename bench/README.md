# Jomini Benchmarks

Large save fixtures are downloaded on first use into `assets/saves/jomini`.

Run wall-clock benchmarks:

```bash
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-criterion
```

Run instruction-count benchmarks:

```bash
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-gungraun
```
