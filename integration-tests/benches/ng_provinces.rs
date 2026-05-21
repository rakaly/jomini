//! Bench comparing three binary deserializer dispatch strategies on real EU4
//! save data. Decision driver for whether the `ng` rewrite ships and whether
//! `BinaryValueFormat` speculation pays its complexity cost.
//!
//! Three deserializers under test:
//!   * `master`: the shipping `jomini::binary::BinaryDeserializer` (current API).
//!   * `ng_spec`: `jomini::binary::ng::from_slice` with `Eu4Format`'s native
//!     speculative `BinaryValueFormat` impls.
//!   * `ng_nospec`: same `Eu4Format`, wrapped in `NoSpeculate` which forces
//!     dispatch to always go through `deserialize_any`/`next_token`.
//!
//! Three target shapes (skip-pressure axis):
//!   * dense (~20 named province scalars)
//!   * medium (~8 named province scalars)
//!   * sparse (~2 named province scalars)
//!
//! See `task.md` for the full plan and decision thresholds.

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use eu4save::flavor::{Eu4Flavor, Eu4Format};
use eu4save::{FailedResolveStrategy, SegmentedResolver};
use jomini::binary::ng::{
    BinaryTokenFormat, BinaryValueFormat, ParserState, PdxVisitor, TokenResult, VisitResult,
};
use jomini::binary::BinaryDeserializer;
use jomini::Error;
use serde::Deserialize;
use std::hint::black_box;

// ---- NoSpeculate wrapper: token-driven counterpart over the same format. ----

/// Wraps a `BinaryFormat` and forces every typed deserialize to fall back to
/// `deserialize_any`. The default trait methods on `BinaryValueFormat` already
/// forward to `deserialize_any`, so we only need to delegate the methods the
/// inner format actually specializes.
struct NoSpeculate<F>(F);

impl<F: BinaryTokenFormat> BinaryTokenFormat for NoSpeculate<F> {
    type Token<'a> = F::Token<'a>;

    fn next_token<'a>(
        &mut self,
        reader: &'a mut ParserState,
    ) -> Result<TokenResult<Self::Token<'a>>, Error> {
        self.0.next_token(reader)
    }
}

impl<F: BinaryValueFormat> BinaryValueFormat for NoSpeculate<F> {
    fn decode_scalar<'a>(&self, data: &'a [u8]) -> Result<Cow<'a, str>, Error> {
        self.0.decode_scalar(data)
    }

    fn deserialize_any<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        self.0.deserialize_any(reader, visitor)
    }

    fn deserialize_identifier<'de, V: PdxVisitor<'de>>(
        &mut self,
        reader: &mut ParserState,
        visitor: V,
    ) -> VisitResult<'de, V> {
        // Identifier resolution is mandatory, not speculation — keep the inner
        // implementation. All other deserialize_* methods inherit the default
        // trait impl that forwards to deserialize_any (i.e. token-driven).
        self.0.deserialize_identifier(reader, visitor)
    }
}

// ---------------- Target structs: dense / medium / sparse. ----------------

#[derive(Debug, Deserialize, Default)]
#[allow(dead_code)]
struct ProvinceDense {
    #[serde(default)]
    name: Option<String>,
    #[serde(default)]
    owner: Option<String>,
    #[serde(default)]
    controller: Option<String>,
    #[serde(default)]
    previous_controller: Option<String>,
    #[serde(default)]
    culture: Option<String>,
    #[serde(default)]
    original_culture: Option<String>,
    #[serde(default)]
    religion: Option<String>,
    #[serde(default)]
    original_religion: Option<String>,
    #[serde(default)]
    trade_goods: Option<String>,
    #[serde(default)]
    trade: Option<String>,
    #[serde(default)]
    capital: Option<String>,
    #[serde(default)]
    base_tax: Option<f32>,
    #[serde(default)]
    base_production: Option<f32>,
    #[serde(default)]
    base_manpower: Option<f32>,
    #[serde(default)]
    devastation: Option<f32>,
    #[serde(default)]
    local_autonomy: Option<f32>,
    #[serde(default)]
    trade_power: Option<f32>,
    #[serde(default)]
    center_of_trade: Option<u8>,
    #[serde(default)]
    num_centralize_state: Option<i32>,
    #[serde(default)]
    expand_infrastructure: Option<i32>,
}

#[derive(Debug, Deserialize, Default)]
#[allow(dead_code)]
struct ProvinceMedium {
    #[serde(default)]
    name: Option<String>,
    #[serde(default)]
    owner: Option<String>,
    #[serde(default)]
    culture: Option<String>,
    #[serde(default)]
    religion: Option<String>,
    #[serde(default)]
    base_tax: Option<f32>,
    #[serde(default)]
    base_production: Option<f32>,
    #[serde(default)]
    base_manpower: Option<f32>,
}

#[derive(Debug, Deserialize, Default)]
#[allow(dead_code)]
struct ProvinceSparse {
    #[serde(default)]
    name: Option<String>,
    #[serde(default)]
    owner: Option<String>,
}

#[derive(Debug, Deserialize, Default)]
struct Save<P> {
    #[serde(default)]
    provinces: HashMap<i32, P>,
}

// --------------------------- Input plumbing ---------------------------

const SAVE_PATH: &str = "../assets/saves/jomini/bin.eu4";
const TOKENS_PATH: &str = "/home/nick/projects/tokens/tokens/eu4.txt";

fn load_save() -> Vec<u8> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(SAVE_PATH);
    let file = std::fs::File::open(&path)
        .unwrap_or_else(|e| panic!("open {}: {}", path.display(), e));
    let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
    let archive = rawzip::ZipArchive::from_file(file, &mut buf).unwrap();
    let mut entries = archive.entries(&mut buf);
    let mut max_size = 0;
    let mut max_entry = None;
    while let Some(entry) = entries.next_entry().unwrap() {
        if entry.uncompressed_size_hint() > max_size {
            max_entry = Some((entry.wayfinder(), entry.compression_method()));
            max_size = entry.uncompressed_size_hint();
        }
    }
    let (wayfinder, compression_method) = max_entry.expect("zip empty");
    assert_eq!(compression_method, rawzip::CompressionMethod::Deflate);
    let entry = archive.get_entry(wayfinder).unwrap();
    let reader = flate2::read::DeflateDecoder::new_with_buf(entry.reader(), buf);
    let mut reader = entry.verifying_reader(reader);

    // Skip "EU4bin" magic header.
    let mut hdr = [0u8; 6];
    reader.read_exact(&mut hdr).unwrap();
    assert_eq!(&hdr, b"EU4bin");

    let mut out = Vec::with_capacity(max_size as usize);
    reader.read_to_end(&mut out).unwrap();
    out
}

fn load_resolver() -> SegmentedResolver<'static> {
    let path = PathBuf::from(TOKENS_PATH);
    let file = std::fs::File::open(&path)
        .unwrap_or_else(|e| panic!("open {}: {}", path.display(), e));
    let builder = SegmentedResolver::parse(BufReader::new(file)).unwrap();
    // Leak the builder so resolver borrows 'static strings.
    let leaked: &'static eu4save::SegmentedResolverBuilder = Box::leak(Box::new(builder));
    leaked.resolver()
}

// --------------------------- Benches ---------------------------

fn bench_one<P>(c: &mut Criterion, group_name: &str)
where
    P: for<'de> Deserialize<'de> + Default + 'static,
{
    let data = load_save();
    let resolver = load_resolver();

    let mut group = c.benchmark_group(group_name);
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.sample_size(20);

    group.bench_function("master", |b| {
        b.iter(|| {
            let mut builder = BinaryDeserializer::builder_flavor(Eu4Flavor::new());
            builder.on_failed_resolve(FailedResolveStrategy::Ignore);
            let save: Save<P> = builder.deserialize_slice(&data[..], &resolver).unwrap();
            black_box(save.provinces.len());
        })
    });

    group.bench_function("ng_spec", |b| {
        b.iter(|| {
            let format = Eu4Format::new(&resolver)
                .with_failed_resolve_strategy(FailedResolveStrategy::Ignore);
            let save: Save<P> = jomini::binary::ng::from_slice(&data[..], format).unwrap();
            black_box(save.provinces.len());
        })
    });

    group.bench_function("ng_nospec", |b| {
        b.iter(|| {
            let format = NoSpeculate(
                Eu4Format::new(&resolver)
                    .with_failed_resolve_strategy(FailedResolveStrategy::Ignore),
            );
            let save: Save<P> = jomini::binary::ng::from_slice(&data[..], format).unwrap();
            black_box(save.provinces.len());
        })
    });

    group.finish();
}

fn bench_tokens(c: &mut Criterion) {
    let data = load_save();
    let resolver = load_resolver();

    let mut group = c.benchmark_group("tokens");
    group.throughput(Throughput::Bytes(data.len() as u64));
    group.sample_size(20);

    group.bench_function("master-kind", |b| {
        b.iter(|| {
            let mut reader = jomini::binary::TokenReader::from_slice(&data[..]);
            while let Some(tok) = reader.next_token().unwrap() {
                black_box(tok);
            }
        })
    });

    group.bench_function("master", |b| {
        b.iter(|| {
            let mut reader = jomini::binary::TokenReader::from_slice(&data[..]);
            while let Some(tok) = reader.next().unwrap() {
                black_box(tok);
            }
        })
    });

    group.bench_function("ng", |b| {
        b.iter(|| {
            let format = Eu4Format::new(&resolver)
                .with_failed_resolve_strategy(FailedResolveStrategy::Ignore);
            let mut reader = jomini::binary::ng::TokenReader::from_slice(&data[..], format);
            while let Some(tok) = reader.next_token().unwrap() {
                black_box(tok);
            }
        })
    });

    group.finish();
}

fn bench_dense(c: &mut Criterion) {
    bench_one::<ProvinceDense>(c, "provinces/dense");
}

fn bench_medium(c: &mut Criterion) {
    bench_one::<ProvinceMedium>(c, "provinces/medium");
}

fn bench_sparse(c: &mut Criterion) {
    bench_one::<ProvinceSparse>(c, "provinces/sparse");
}

criterion_group!(benches, bench_tokens, bench_dense, bench_medium, bench_sparse);
criterion_main!(benches);
