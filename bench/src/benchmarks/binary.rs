use crate::corpus::{self, Corpus, CorpusArchive, CorpusBytes, Game};
use jomini::{
    Encoding, Windows1252Encoding,
    binary::{BinaryFlavor, TokenResolver},
};
use std::{borrow::Cow, hint::black_box};

const EU4_BINARY: Corpus = Corpus::binary(Game::Eu4);
const CK3_BINARY: Corpus = Corpus::binary(Game::Ck3);
const VIC3_BINARY: Corpus = Corpus::binary(Game::Vic3);

#[derive(Debug, Default)]
struct BinaryTestFlavor;

impl BinaryFlavor for BinaryTestFlavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_le_bytes(data)
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        f64::from_le_bytes(data)
    }
}

impl Encoding for BinaryTestFlavor {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Windows1252Encoding::decode(data)
    }
}

struct MyBinaryResolver;

impl TokenResolver for MyBinaryResolver {
    fn resolve(&self, token: u16) -> Option<&str> {
        if token == 0x3564 {
            Some("current_age")
        } else {
            None
        }
    }
}

#[derive(serde::Deserialize)]
pub(crate) struct Gamestate {
    #[allow(dead_code)]
    pub current_age: String,
}

#[inline(never)]
pub(crate) fn lex_count_ids(data: &[u8]) -> u64 {
    let mut lexer = jomini::binary::Lexer::new(data);
    let mut counter: u64 = 0;
    while let Ok(Some(token)) = lexer.next_token() {
        if let jomini::binary::Token::Id(x) = token {
            counter = counter.wrapping_add(u64::from(x));
        }
    }
    black_box(counter)
}

#[inline(never)]
pub(crate) fn lex_token_kind_count_ids(data: &[u8]) -> u64 {
    let mut lexer = jomini::binary::Lexer::new(data);
    let mut counter: u64 = 0;
    while let Ok(Some(token)) = lexer.next_id() {
        match token.into_kind() {
            jomini::binary::TokenKind::Id => {
                counter = counter.wrapping_add(u64::from(token.0));
            }
            jomini::binary::TokenKind::Open => {}
            _ => lexer.skip_value(token).unwrap(),
        }
    }
    black_box(counter)
}

#[inline(never)]
pub(crate) fn reader_token_kind_count_ids(data: &[u8]) -> u64 {
    let mut reader = jomini::binary::TokenReader::new(data);
    let mut counter: u64 = 0;
    while let Ok(Some(token)) = reader.next_token() {
        if matches!(token, jomini::binary::TokenKind::Id) {
            counter = counter.wrapping_add(u64::from(reader.token_id()));
        }
    }
    black_box(counter)
}

#[inline(never)]
pub(crate) fn reader_count_ids(data: &[u8]) -> u64 {
    let mut reader = jomini::binary::TokenReader::new(data);
    let mut counter: u64 = 0;
    while let Ok(Some(token)) = reader.next() {
        if let jomini::binary::Token::Id(x) = token {
            counter = counter.wrapping_add(u64::from(x));
        }
    }
    black_box(counter)
}

#[inline(never)]
pub(crate) fn deserialize_slice(data: &[u8]) -> Gamestate {
    BinaryTestFlavor
        .deserializer()
        .deserialize_slice(data, &MyBinaryResolver)
        .unwrap()
}

#[inline(never)]
pub(crate) fn deserialize_reader(data: &[u8]) -> Gamestate {
    BinaryTestFlavor
        .deserializer()
        .deserialize_reader(data, &MyBinaryResolver)
        .unwrap()
}

#[inline(never)]
pub(crate) fn read_compressed_archive(archive: &CorpusArchive) -> u64 {
    corpus::with_archive_reader(archive, |reader| {
        let mut reader = jomini::binary::TokenReader::new(reader);
        let mut counter: u64 = 0;
        while let Ok(Some(token)) = reader.next() {
            if let jomini::binary::Token::Id(x) = token {
                counter = counter.wrapping_add(u64::from(x));
            }
        }
        black_box(counter)
    })
}

fn setup_eu4() -> &'static [u8] {
    corpus::bytes(EU4_BINARY).data
}

fn setup_ck3() -> &'static [u8] {
    corpus::bytes(CK3_BINARY).data
}

fn setup_vic3() -> &'static [u8] {
    corpus::bytes(VIC3_BINARY).data
}

fn setup_eu4_archive() -> &'static CorpusArchive {
    corpus::archive(EU4_BINARY)
}

fn setup_ck3_archive() -> &'static CorpusArchive {
    corpus::archive(CK3_BINARY)
}

fn setup_vic3_archive() -> &'static CorpusArchive {
    corpus::archive(VIC3_BINARY)
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion, Throughput};

    pub fn binary(c: &mut Criterion) {
        let mut group = c.benchmark_group("binary");
        group.sample_size(30);

        for (game, corpus) in [
            ("eu4", EU4_BINARY),
            ("v3", VIC3_BINARY),
            ("ck3", CK3_BINARY),
        ] {
            let CorpusBytes { archive, data } = corpus::bytes(corpus);
            group.throughput(Throughput::Bytes(archive.uncompressed_size));
            group.bench_function(BenchmarkId::new("lex", game), |b| {
                b.iter(|| lex_count_ids(data))
            });
            group.bench_function(BenchmarkId::new("lex-token-kind", game), |b| {
                b.iter(|| lex_token_kind_count_ids(data))
            });
            group.bench_function(BenchmarkId::new("reader-token-kind", game), |b| {
                b.iter(|| reader_token_kind_count_ids(data))
            });
            group.bench_function(BenchmarkId::new("reader", game), |b| {
                b.iter(|| reader_count_ids(data))
            });
        }

        group.finish();
    }

    pub fn binary_deserialize(c: &mut Criterion) {
        let CorpusBytes { archive, data } = corpus::bytes(EU4_BINARY);
        let mut group = c.benchmark_group("binary/deserialize");
        group.throughput(Throughput::Bytes(archive.uncompressed_size));
        group.bench_function(BenchmarkId::new("eu4", "slice"), |b| {
            b.iter(|| deserialize_slice(data))
        });
        group.bench_function(BenchmarkId::new("eu4", "reader"), |b| {
            b.iter(|| deserialize_reader(data))
        });
        group.finish();
    }

    pub fn binary_compressed(c: &mut Criterion) {
        let mut group = c.benchmark_group("binary/compressed");
        group.sample_size(30);
        for (game, corpus) in [
            ("eu4", EU4_BINARY),
            ("v3", VIC3_BINARY),
            ("ck3", CK3_BINARY),
        ] {
            let archive = corpus::archive(corpus);
            group.throughput(Throughput::Bytes(archive.uncompressed_size));
            group.bench_function(BenchmarkId::from_parameter(game), |b| {
                b.iter(|| read_compressed_archive(archive))
            });
        }
        group.finish();
    }

    criterion::criterion_group!(
        binary_benches,
        binary,
        binary_deserialize,
        binary_compressed
    );
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::v3(setup = setup_vic3)]
    #[bench::ck3(setup = setup_ck3)]
    fn lex(data: &[u8]) -> u64 {
        lex_count_ids(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::v3(setup = setup_vic3)]
    #[bench::ck3(setup = setup_ck3)]
    fn lex_token_kind(data: &[u8]) -> u64 {
        lex_token_kind_count_ids(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::v3(setup = setup_vic3)]
    #[bench::ck3(setup = setup_ck3)]
    fn reader_token_kind(data: &[u8]) -> u64 {
        reader_token_kind_count_ids(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::v3(setup = setup_vic3)]
    #[bench::ck3(setup = setup_ck3)]
    fn reader(data: &[u8]) -> u64 {
        reader_count_ids(data)
    }

    #[library_benchmark]
    #[bench::eu4_slice(setup = setup_eu4)]
    fn deserialize_slice_bench(data: &[u8]) -> Gamestate {
        deserialize_slice(data)
    }

    #[library_benchmark]
    #[bench::eu4_reader(setup = setup_eu4)]
    fn deserialize_reader_bench(data: &[u8]) -> Gamestate {
        deserialize_reader(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4_archive)]
    #[bench::v3(setup = setup_vic3_archive)]
    #[bench::ck3(setup = setup_ck3_archive)]
    fn compressed_read(archive: &CorpusArchive) -> u64 {
        read_compressed_archive(archive)
    }

    library_benchmark_group!(
        name = binary_benches,
        benchmarks = [
            lex,
            lex_token_kind,
            reader_token_kind,
            reader,
            deserialize_slice_bench,
            deserialize_reader_bench,
            compressed_read,
        ]
    );
}
