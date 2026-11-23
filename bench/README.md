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
before, as wasm cannot download the fixtures.

```bash
rustup target add wasm32-wasip1
mise trust
mise run bench:wasm 'parse_save|eu5'
```

Without mise, run what the task runs:

```bash
export CARGO_TARGET_WASM32_WASIP1_RUNNER="node --no-warnings $PWD/bench/wasi-runner.mjs"
cargo bench --manifest-path bench/Cargo.toml --target wasm32-wasip1 --profile wasm --bench jomini-bench-criterion -- 'parse_save|eu5'
```

Results are written to `bench/target/criterion-wasm`. They depend on the Node.js
version, which the mise task pins, so compare only runs from the same version.
The `wasm` profile has the settings of the pdx-tools release build, but
`wasm-opt` is not run.
