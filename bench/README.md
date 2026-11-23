# Jomini Benchmarks

Large save fixtures are downloaded on first use into `assets/saves/jomini`.
The `twitter.json` corpus of the arena benchmarks is downloaded on first use
into `assets/twitter.json`.

Run wall-clock benchmarks:

```bash
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-criterion
```

Run instruction-count benchmarks:

```bash
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-gungraun
```

## Wasm

The Criterion benchmarks also run on `wasm32-wasip1` with Node.js 24 or later,
which has the same wasm engine (V8) as Chrome. Run the native benchmarks once
before, as wasm cannot download the fixtures. Run from this directory, where
cargo finds the runner:

```bash
rustup target add wasm32-wasip1
cd bench
cargo bench --target wasm32-wasip1 --bench jomini-bench-criterion -- 'parse_save|eu5'
```

Results are written to `bench/target/criterion-wasm`, in a directory for each
Node.js version. `wasm-opt` is not run.
