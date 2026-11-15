use crate::corpus::{self, Corpus, CorpusArchive, CorpusBytes, Game};
use eu4save::models::GameState;
use jomini::{TextDeserializer, TextTape, Windows1252Encoding};
use serde::de::DeserializeOwned;
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
pub(crate) fn syntax_tape_parse(data: &[u8]) -> usize {
    let tape = jomini::text::syntax::parse_tape(data)
        .into_result()
        .unwrap();
    black_box(tape.tokens().len())
}

#[inline(never)]
pub(crate) fn syntax_parse(data: &[u8]) -> usize {
    let tree = jomini::text::syntax::parse(data);
    black_box(tree.root().children().count())
}

/// Visit every node and every leaf of `tree`, trivia included.
#[inline(never)]
pub(crate) fn syntax_walk(tree: &jomini::text::syntax::Parse<'_>) -> usize {
    fn walk(node: jomini::text::syntax::SyntaxNode<'_, '_>) -> usize {
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

/// Deserialize from a `TextTape` parsed from the slice.
#[inline(never)]
pub(crate) fn tape_de<T: DeserializeOwned>(data: &[u8]) -> T {
    jomini::text::de::TextTapeDeserializer::from_windows1252_slice(data)
        .unwrap()
        .deserialize()
        .unwrap()
}

#[inline(never)]
pub(crate) fn de_slice<T: DeserializeOwned>(data: &[u8]) -> T {
    TextDeserializer::from_slice(data, Windows1252Encoding::new())
        .deserialize()
        .unwrap()
}

#[inline(never)]
pub(crate) fn de_reader<T: DeserializeOwned>(data: &[u8]) -> T {
    TextDeserializer::from_reader(data, Windows1252Encoding::new())
        .deserialize()
        .unwrap()
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

fn setup_eu4_gamestate() -> &'static [u8] {
    &setup_eu4()["EU4txt".len()..]
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
            group.bench_function(BenchmarkId::new("syntax_tape", game), |b| {
                b.iter(|| syntax_tape_parse(data))
            });
            group.bench_function(BenchmarkId::new("reader", game), |b| {
                b.iter(|| reader_count_equals(data))
            });
            group.bench_function(BenchmarkId::new("syntax_tree", game), |b| {
                b.iter(|| syntax_parse(data))
            });
            let tree = jomini::text::syntax::parse(data);
            group.bench_function(BenchmarkId::new("syntax_walk", game), |b| {
                b.iter(|| syntax_walk(&tree))
            });
        }
        group.finish();
    }

    pub fn text_deserialize(c: &mut Criterion) {
        let data = setup_meta();
        let mut group = c.benchmark_group("text/deserialize");
        group.throughput(Throughput::Bytes(data.len() as u64));
        group.bench_function("meta", |b| b.iter(|| deserialize_meta(data)));
        group.bench_function("meta/tape", |b| b.iter(|| tape_de::<Meta>(data)));
        group.bench_function("meta/slice", |b| b.iter(|| de_slice::<Meta>(data)));
        group.bench_function("meta/reader", |b| b.iter(|| de_reader::<Meta>(data)));
        group.finish();

        let data = setup_eu4_gamestate();
        let mut group = c.benchmark_group("text/deserialize/gamestate");
        group.sample_size(10);
        group.throughput(Throughput::Bytes(data.len() as u64));
        group.bench_function("tape", |b| b.iter(|| tape_de::<GameState>(data)));
        group.bench_function("slice", |b| b.iter(|| de_slice::<GameState>(data)));
        group.bench_function("reader", |b| b.iter(|| de_reader::<GameState>(data)));
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
    fn syntax_tape(data: &[u8]) -> usize {
        syntax_tape_parse(data)
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
    fn syntax_tree(data: &[u8]) -> usize {
        syntax_parse(data)
    }

    #[library_benchmark]
    #[bench::meta(setup = setup_meta)]
    fn deserialize(data: &[u8]) -> Meta {
        deserialize_meta(data)
    }

    #[library_benchmark]
    #[bench::tape(args = (setup_meta(), tape_de::<Meta>))]
    #[bench::slice(args = (setup_meta(), de_slice::<Meta>))]
    #[bench::reader(args = (setup_meta(), de_reader::<Meta>))]
    fn deserialize_meta_with(data: &[u8], f: fn(&[u8]) -> Meta) -> Meta {
        f(data)
    }

    #[library_benchmark]
    #[bench::tape(args = (setup_eu4_gamestate(), tape_de::<GameState>))]
    #[bench::slice(args = (setup_eu4_gamestate(), de_slice::<GameState>))]
    #[bench::reader(args = (setup_eu4_gamestate(), de_reader::<GameState>))]
    fn deserialize_gamestate(data: &[u8], f: fn(&[u8]) -> GameState) -> GameState {
        f(data)
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
            syntax_tape,
            reader,
            syntax_tree,
            deserialize,
            deserialize_meta_with,
            deserialize_gamestate,
            compressed_read,
        ]
    );
}
