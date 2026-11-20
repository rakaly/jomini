use crate::corpus::{self, Corpus, CorpusArchive, CorpusBytes, Game};
use eu4save::models::GameState;
use jomini::text::de::TextTapeDeserializer;
use jomini::{TextDeserializer, TextTape, Windows1252Encoding};
use serde::de::DeserializeOwned;
use std::{hint::black_box, sync::OnceLock};

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

/// Parse with the syntax parser into a `TextTape` and deserialize the tape.
#[inline(never)]
pub(crate) fn syntax_tape_de<T: DeserializeOwned>(data: &[u8]) -> T {
    let tape = jomini::text::syntax::parse_tape(data)
        .into_result()
        .unwrap();
    TextTapeDeserializer::from_windows1252_tape(&tape)
        .deserialize()
        .unwrap()
}

/// Parse into a syntax tree and deserialize the tree.
#[inline(never)]
pub(crate) fn syntax_tree_de<T: DeserializeOwned>(data: &[u8]) -> T {
    jomini::text::syntax::parse(data)
        .deserialize(Windows1252Encoding::new())
        .unwrap()
}

/// A type that takes each value of a document, so that a benchmark measures a
/// full deserialization and not a skip.
pub(crate) struct Any;

impl<'de> serde::Deserialize<'de> for Any {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Any, D::Error> {
        deserializer.deserialize_any(AnyVisitor)
    }
}

/// [`Any`] for the root of a file. `TextTapeDeserializer` reads the root only
/// as a map.
pub(crate) struct AnyRoot;

impl<'de> serde::Deserialize<'de> for AnyRoot {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<AnyRoot, D::Error> {
        deserializer.deserialize_map(AnyVisitor).map(|Any| AnyRoot)
    }
}

struct AnyVisitor;

impl<'de> serde::de::Visitor<'de> for AnyVisitor {
    type Value = Any;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("any value")
    }

    fn visit_bool<E>(self, _: bool) -> Result<Any, E> {
        Ok(Any)
    }

    fn visit_i64<E>(self, _: i64) -> Result<Any, E> {
        Ok(Any)
    }

    fn visit_u64<E>(self, _: u64) -> Result<Any, E> {
        Ok(Any)
    }

    fn visit_f64<E>(self, _: f64) -> Result<Any, E> {
        Ok(Any)
    }

    fn visit_str<E>(self, value: &str) -> Result<Any, E> {
        black_box(value.len());
        Ok(Any)
    }

    fn visit_bytes<E>(self, value: &[u8]) -> Result<Any, E> {
        black_box(value.len());
        Ok(Any)
    }

    fn visit_unit<E>(self) -> Result<Any, E> {
        Ok(Any)
    }

    fn visit_seq<A: serde::de::SeqAccess<'de>>(self, mut seq: A) -> Result<Any, A::Error> {
        while seq.next_element::<Any>()?.is_some() {}
        Ok(Any)
    }

    fn visit_map<A: serde::de::MapAccess<'de>>(self, mut map: A) -> Result<Any, A::Error> {
        while map.next_entry::<Any, Any>()?.is_some() {}
        Ok(Any)
    }
}

/// The `.txt` files under the directory of `JOMINI_GAME_DIR`, such as the
/// `game/common` directory of CK3. Game files are not in the repository, so
/// the game file benchmarks run only when the variable is set. A file that
/// one of the paths rejects is left out, so each benchmark reads the same
/// files.
fn setup_game_files() -> Option<&'static [Vec<u8>]> {
    static FILES: OnceLock<Option<Vec<Vec<u8>>>> = OnceLock::new();
    FILES
        .get_or_init(|| {
            let root = std::env::var_os("JOMINI_GAME_DIR")?;
            let mut paths = Vec::new();
            let mut stack = vec![std::path::PathBuf::from(root)];
            while let Some(dir) = stack.pop() {
                for entry in std::fs::read_dir(&dir).ok()?.flatten() {
                    let path = entry.path();
                    if path.is_dir() {
                        stack.push(path);
                    } else if path.extension().is_some_and(|e| e == "txt") {
                        paths.push(path);
                    }
                }
            }
            paths.sort();
            let files = paths
                .iter()
                .filter_map(|path| std::fs::read(path).ok())
                .filter(|data| {
                    let encoding = Windows1252Encoding::new();
                    TextTapeDeserializer::from_windows1252_slice(data)
                        .is_ok_and(|de| de.deserialize::<AnyRoot>().is_ok())
                        && TextDeserializer::from_slice(data, encoding)
                            .deserialize::<AnyRoot>()
                            .is_ok()
                        && jomini::text::syntax::parse_tape(data).into_result().is_ok()
                        && jomini::text::syntax::parse(data)
                            .deserialize::<AnyRoot, _>(encoding)
                            .is_ok()
                })
                .collect();
            Some(files)
        })
        .as_deref()
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
        group.bench_function("meta/syntax_tape", |b| {
            b.iter(|| syntax_tape_de::<Meta>(data))
        });
        group.bench_function("meta/syntax_tree", |b| {
            b.iter(|| syntax_tree_de::<Meta>(data))
        });
        group.finish();

        let data = setup_eu4_gamestate();
        let mut group = c.benchmark_group("text/deserialize/gamestate");
        group.sample_size(10);
        group.throughput(Throughput::Bytes(data.len() as u64));
        group.bench_function("tape", |b| b.iter(|| tape_de::<GameState>(data)));
        group.bench_function("slice", |b| b.iter(|| de_slice::<GameState>(data)));
        group.bench_function("reader", |b| b.iter(|| de_reader::<GameState>(data)));
        group.bench_function("syntax_tape", |b| {
            b.iter(|| syntax_tape_de::<GameState>(data))
        });
        group.bench_function("syntax_tree", |b| {
            b.iter(|| syntax_tree_de::<GameState>(data))
        });
        group.finish();

        if let Some(files) = setup_game_files() {
            let mut group = c.benchmark_group("text/deserialize/game");
            let bytes = files.iter().map(|data| data.len() as u64).sum();
            group.throughput(Throughput::Bytes(bytes));
            type Path = fn(&[u8]) -> AnyRoot;
            let paths: [(&str, Path); 4] = [
                ("tape", tape_de),
                ("slice", de_slice),
                ("syntax_tape", syntax_tape_de),
                ("syntax_tree", syntax_tree_de),
            ];
            for (name, f) in paths {
                group.bench_function(name, |b| {
                    b.iter(|| files.iter().for_each(|data| _ = black_box(f(data))))
                });
            }
            group.finish();
        }
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
    #[bench::syntax_tape(args = (setup_meta(), syntax_tape_de::<Meta>))]
    #[bench::syntax_tree(args = (setup_meta(), syntax_tree_de::<Meta>))]
    fn deserialize_meta_with(data: &[u8], f: fn(&[u8]) -> Meta) -> Meta {
        f(data)
    }

    #[library_benchmark]
    #[bench::tape(args = (setup_eu4_gamestate(), tape_de::<GameState>))]
    #[bench::slice(args = (setup_eu4_gamestate(), de_slice::<GameState>))]
    #[bench::reader(args = (setup_eu4_gamestate(), de_reader::<GameState>))]
    #[bench::syntax_tape(args = (setup_eu4_gamestate(), syntax_tape_de::<GameState>))]
    #[bench::syntax_tree(args = (setup_eu4_gamestate(), syntax_tree_de::<GameState>))]
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
