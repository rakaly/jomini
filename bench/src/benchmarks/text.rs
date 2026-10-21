use crate::corpus::{self, Corpus, CorpusArchive, CorpusBytes, Game};
use eu4save::models::GameState;
use jomini::TextTape;
use std::hint::black_box;

const METADATA_TXT: &[u8] = include_bytes!("../../../crates/jomini/tests/fixtures/meta.txt");
const EU4_TEXT: Corpus = Corpus::text(Game::Eu4);
const CK3_TEXT: Corpus = Corpus::text(Game::Ck3);

#[derive(serde::Deserialize)]
pub(crate) struct Meta {
    #[allow(dead_code)]
    campaign_id: String,
}

#[inline(never)]
pub(crate) fn tape_parse(data: &[u8]) -> usize {
    let tape = TextTape::from_slice(data).unwrap();
    black_box(tape.tokens().len())
}

#[inline(never)]
pub(crate) fn lossless_tape_parse(data: &[u8]) -> usize {
    let tape = jomini::text::lossless::parse_tape(data);
    black_box(tape.tokens().len())
}

#[inline(never)]
pub(crate) fn lossless_parse(data: &[u8]) -> usize {
    let tree = jomini::text::lossless::parse(data);
    black_box(tree.root().children().count())
}

/// Visit every node and every leaf of `tree`, trivia included.
#[inline(never)]
pub(crate) fn lossless_walk(tree: &jomini::text::lossless::Parse<&[u8]>) -> usize {
    fn walk(node: jomini::text::lossless::SyntaxNode<'_, '_>) -> usize {
        node.children()
            .map(|child| child.as_node().map_or(1, walk))
            .sum::<usize>()
            + 1
    }
    black_box(walk(tree.root()))
}

#[inline(never)]
pub(crate) fn reader_count_equals(data: &[u8]) -> i32 {
    let mut reader = jomini::text::TokenReader::from_slice(data);
    let mut count = 0i32;
    while let Some(token) = reader.next_token().unwrap() {
        if matches!(
            token,
            jomini::text::TokenKind::Operator(jomini::text::Operator::Equal)
        ) {
            count += 1;
        }
    }
    black_box(count)
}

#[inline(never)]
pub(crate) fn deserialize_meta(data: &[u8]) -> Meta {
    jomini::text::de::from_windows1252_slice(data).unwrap()
}

/// Parse with `TextTape` and deserialize with `TextDeserializer`.
#[inline(never)]
pub(crate) fn deserialize_tape<'a, T: serde::Deserialize<'a>>(data: &'a [u8]) -> T {
    let tape = TextTape::from_slice(data).unwrap();
    jomini::TextDeserializer::from_windows1252_tape(&tape)
        .deserialize()
        .unwrap()
}

/// Parse with the lossless grammar into a `TextTape` and deserialize with
/// `TextDeserializer`.
#[inline(never)]
pub(crate) fn deserialize_lossless_tape<'a, T: serde::Deserialize<'a>>(data: &'a [u8]) -> T {
    let tape = jomini::text::lossless::parse_tape(data);
    jomini::TextDeserializer::from_windows1252_tape(&tape)
        .deserialize()
        .unwrap()
}

/// Parse into a syntax tree and deserialize the tree directly.
#[inline(never)]
pub(crate) fn deserialize_tree<'a, T: serde::Deserialize<'a>>(data: &'a [u8]) -> T {
    let parse = jomini::text::lossless::parse(data);
    jomini::text::lossless::de::from_tree(parse.tree(), jomini::Windows1252Encoding::new()).unwrap()
}

/// The EU4 save without the `EU4txt` header, for the `GameState` model.
fn setup_eu4_gamestate() -> &'static [u8] {
    &setup_eu4()["EU4txt".len()..]
}

#[inline(never)]
pub(crate) fn reader_count_compressed_archive(archive: &CorpusArchive) -> i32 {
    corpus::with_archive_reader(archive, |reader| {
        let mut reader = jomini::text::TokenReader::new(reader);
        let mut count = 0i32;
        while let Some(token) = reader.next_token().unwrap() {
            if matches!(
                token,
                jomini::text::TokenKind::Operator(jomini::text::Operator::Equal)
            ) {
                count += 1;
            }
        }
        black_box(count)
    })
}

fn setup_eu4() -> &'static [u8] {
    corpus::bytes(EU4_TEXT).data
}

fn setup_ck3() -> &'static [u8] {
    corpus::bytes(CK3_TEXT).data
}

fn setup_meta() -> &'static [u8] {
    &METADATA_TXT["EU4txt".len()..]
}

fn setup_eu4_archive() -> &'static CorpusArchive {
    corpus::archive(EU4_TEXT)
}

fn setup_ck3_archive() -> &'static CorpusArchive {
    corpus::archive(CK3_TEXT)
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion, Throughput};

    pub fn text(c: &mut Criterion) {
        let mut group = c.benchmark_group("text");
        group.sample_size(30);
        for (game, corpus) in [("eu4", EU4_TEXT), ("ck3", CK3_TEXT)] {
            let CorpusBytes { archive, data } = corpus::bytes(corpus);
            group.throughput(Throughput::Bytes(archive.uncompressed_size));
            group.bench_function(BenchmarkId::new("tape", game), |b| {
                b.iter(|| tape_parse(data))
            });
            group.bench_function(BenchmarkId::new("lossless_tape", game), |b| {
                b.iter(|| lossless_tape_parse(data))
            });
            group.bench_function(BenchmarkId::new("reader", game), |b| {
                b.iter(|| reader_count_equals(data))
            });
            group.bench_function(BenchmarkId::new("lossless", game), |b| {
                b.iter(|| lossless_parse(data))
            });
            let tree = jomini::text::lossless::parse(data);
            group.bench_function(BenchmarkId::new("lossless_walk", game), |b| {
                b.iter(|| lossless_walk(&tree))
            });
        }
        group.finish();
    }

    pub fn text_deserialize(c: &mut Criterion) {
        let data = setup_meta();
        let mut group = c.benchmark_group("text/deserialize");
        group.throughput(Throughput::Bytes(data.len() as u64));
        group.bench_function("meta", |b| b.iter(|| deserialize_meta(data)));
        group.bench_function(BenchmarkId::new("tape", "meta"), |b| {
            b.iter(|| deserialize_tape::<Meta>(data))
        });
        group.bench_function(BenchmarkId::new("lossless_tape", "meta"), |b| {
            b.iter(|| deserialize_lossless_tape::<Meta>(data))
        });
        group.bench_function(BenchmarkId::new("tree", "meta"), |b| {
            b.iter(|| deserialize_tree::<Meta>(data))
        });

        let data = setup_eu4_gamestate();
        group.sample_size(20);
        group.throughput(Throughput::Bytes(data.len() as u64));
        group.bench_function(BenchmarkId::new("tape", "eu4"), |b| {
            b.iter(|| deserialize_tape::<GameState>(data))
        });
        group.bench_function(BenchmarkId::new("lossless_tape", "eu4"), |b| {
            b.iter(|| deserialize_lossless_tape::<GameState>(data))
        });
        group.bench_function(BenchmarkId::new("tree", "eu4"), |b| {
            b.iter(|| deserialize_tree::<GameState>(data))
        });
        group.finish();
    }

    pub fn text_compressed(c: &mut Criterion) {
        let mut group = c.benchmark_group("text/compressed");
        group.sample_size(30);
        for (game, corpus) in [("eu4", EU4_TEXT), ("ck3", CK3_TEXT)] {
            let archive = corpus::archive(corpus);
            group.throughput(Throughput::Bytes(archive.uncompressed_size));
            group.bench_function(BenchmarkId::from_parameter(game), |b| {
                b.iter(|| reader_count_compressed_archive(archive))
            });
        }
        group.finish();
    }

    criterion::criterion_group!(text_benches, text, text_deserialize, text_compressed);
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::ck3(setup = setup_ck3)]
    fn tape(data: &[u8]) -> usize {
        tape_parse(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::ck3(setup = setup_ck3)]
    fn lossless_tape(data: &[u8]) -> usize {
        lossless_tape_parse(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::ck3(setup = setup_ck3)]
    fn reader(data: &[u8]) -> i32 {
        reader_count_equals(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4)]
    #[bench::ck3(setup = setup_ck3)]
    fn lossless(data: &[u8]) -> usize {
        lossless_parse(data)
    }

    #[library_benchmark]
    #[bench::meta(setup = setup_meta)]
    fn deserialize(data: &[u8]) -> Meta {
        deserialize_meta(data)
    }

    #[library_benchmark]
    #[bench::meta(setup = setup_meta)]
    fn deserialize_meta_tape(data: &[u8]) -> Meta {
        deserialize_tape(data)
    }

    #[library_benchmark]
    #[bench::meta(setup = setup_meta)]
    fn deserialize_meta_lossless_tape(data: &[u8]) -> Meta {
        deserialize_lossless_tape(data)
    }

    #[library_benchmark]
    #[bench::meta(setup = setup_meta)]
    fn deserialize_meta_tree(data: &[u8]) -> Meta {
        deserialize_tree(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4_gamestate)]
    fn deserialize_eu4_tape(data: &[u8]) -> GameState {
        deserialize_tape(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4_gamestate)]
    fn deserialize_eu4_lossless_tape(data: &[u8]) -> GameState {
        deserialize_lossless_tape(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4_gamestate)]
    fn deserialize_eu4_tree(data: &[u8]) -> GameState {
        deserialize_tree(data)
    }

    #[library_benchmark]
    #[bench::eu4(setup = setup_eu4_archive)]
    #[bench::ck3(setup = setup_ck3_archive)]
    fn compressed_read(archive: &CorpusArchive) -> i32 {
        reader_count_compressed_archive(archive)
    }

    library_benchmark_group!(
        name = text_benches,
        benchmarks = [
            tape,
            lossless_tape,
            reader,
            lossless,
            deserialize,
            deserialize_meta_tape,
            deserialize_meta_lossless_tape,
            deserialize_meta_tree,
            deserialize_eu4_tape,
            deserialize_eu4_lossless_tape,
            deserialize_eu4_tree,
            compressed_read,
        ]
    );
}
