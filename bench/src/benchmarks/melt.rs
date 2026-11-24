//! Melting of EU4 and EU5 saves, from the zip in memory to a buffer.
//!
//! The melted text is written to a vector that is large enough, so that the
//! measurement includes the copies to the output but not its growth.

use eu4save::Eu4File;
use eu5save::{Eu5File, Eu5Melt};
use jomini::binary::{FailedResolveStrategy, TokenResolver};

pub struct Eu4 {
    save: Vec<u8>,
    tokens: eu4save::BasicTokenResolver,
}

impl Eu4 {
    /// Returns `None` when no EU4 token file is installed.
    pub fn setup() -> Option<Self> {
        let tokens = pdx_fixtures::tokens("eu4");
        let tokens = eu4save::BasicTokenResolver::from_text_lines(tokens.as_slice()).unwrap();
        if tokens.is_empty() {
            return None;
        }
        let save = pdx_fixtures::read("eu4", "kandy2.bin.eu4");
        Some(Eu4 { save, tokens })
    }

    pub fn melt(&self, out: &mut Vec<u8>) {
        out.clear();
        let file = Eu4File::from_slice(&self.save).unwrap();
        let options = eu4save::MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Ignore);
        file.melt(options, &self.tokens, out).unwrap();
    }
}

/// The save of the deserialization benchmark.
pub struct Eu5(super::eu5::Setup);

impl Eu5 {
    /// Returns `None` when no EU5 token file is installed.
    pub fn setup() -> Option<Self> {
        super::eu5::setup().map(Eu5)
    }

    pub fn melt(&self, out: &mut Vec<u8>) {
        out.clear();
        let file = Eu5File::from_slice(self.0.save.as_slice()).unwrap();
        let options = eu5save::MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Ignore);
        (&file).melt(options, &self.0.tokens, out).unwrap();
    }
}

pub mod criterion_benches {
    use criterion::{Criterion, SamplingMode, Throughput};

    fn melt(c: &mut Criterion) {
        let mut group = c.benchmark_group("melt");
        group.sample_size(10);
        group.sampling_mode(SamplingMode::Flat);

        let mut out = Vec::new();
        if let Some(save) = super::Eu4::setup() {
            save.melt(&mut out);
            group.throughput(Throughput::Bytes(out.len() as u64));
            group.bench_function("eu4", |b| b.iter(|| save.melt(&mut out)));
        }

        if let Some(save) = super::Eu5::setup() {
            save.melt(&mut out);
            group.throughput(Throughput::Bytes(out.len() as u64));
            group.bench_function("eu5", |b| b.iter(|| save.melt(&mut out)));
        }
        group.finish();
    }

    criterion::criterion_group!(melt_benches, melt);
}

/// The setup melts once, so that the output does not grow in the measurement.
/// The harness drops a return value outside of the measurement, so the
/// benchmarks return their input to exclude its drop.
#[cfg(not(target_family = "wasm"))]
pub mod gungraun_benches {
    use crate::benchmarks::melt::{Eu4, Eu5};
    use gungraun::{library_benchmark, library_benchmark_group};

    /// Returns `None` when no EU4 token file is installed, and the benchmark
    /// does nothing.
    fn setup_eu4() -> Option<(Eu4, Vec<u8>)> {
        let save = Eu4::setup()?;
        let mut out = Vec::new();
        save.melt(&mut out);
        Some((save, out))
    }

    /// Returns `None` when no EU5 token file is installed, and the benchmark
    /// does nothing.
    fn setup_eu5() -> Option<(Eu5, Vec<u8>)> {
        let save = Eu5::setup()?;
        let mut out = Vec::new();
        save.melt(&mut out);
        Some((save, out))
    }

    #[library_benchmark]
    #[bench::save(setup = setup_eu4)]
    fn melt_eu4(input: Option<(Eu4, Vec<u8>)>) -> Option<(Eu4, Vec<u8>)> {
        let (save, mut out) = input?;
        save.melt(&mut out);
        Some((save, out))
    }

    #[library_benchmark]
    #[bench::save(setup = setup_eu5)]
    fn melt_eu5(input: Option<(Eu5, Vec<u8>)>) -> Option<(Eu5, Vec<u8>)> {
        let (save, mut out) = input?;
        save.melt(&mut out);
        Some((save, out))
    }

    library_benchmark_group!(name = melt_benches, benchmarks = [melt_eu4, melt_eu5]);
}
