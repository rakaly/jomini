use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};

use eu4save::flavor::Eu4Format;
use eu4save::{FailedResolveStrategy, SegmentedResolver};
use gungraun::{library_benchmark, library_benchmark_group, main};
use std::hint::black_box;

const SAVE_PATH: &str = "../assets/saves/jomini/bin.eu4";
const TOKENS_PATH: &str = "/home/nick/projects/tokens/tokens/eu4.txt";

struct MasterTokensInput {
    data: Vec<u8>,
}

struct NgTokensInput {
    data: Vec<u8>,
    resolver: SegmentedResolver<'static>,
}

fn load_save() -> Vec<u8> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join(SAVE_PATH);
    let file =
        std::fs::File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path.display(), e));
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

    let mut hdr = [0u8; 6];
    reader.read_exact(&mut hdr).unwrap();
    assert_eq!(&hdr, b"EU4bin");

    let mut out = Vec::with_capacity(max_size as usize);
    reader.read_to_end(&mut out).unwrap();
    out
}

fn load_resolver() -> SegmentedResolver<'static> {
    let path = PathBuf::from(TOKENS_PATH);
    let file =
        std::fs::File::open(&path).unwrap_or_else(|e| panic!("open {}: {}", path.display(), e));
    let builder = SegmentedResolver::parse(BufReader::new(file)).unwrap();
    let leaked: &'static eu4save::SegmentedResolverBuilder = Box::leak(Box::new(builder));
    leaked.resolver()
}

fn setup_master_tokens() -> MasterTokensInput {
    MasterTokensInput { data: load_save() }
}

fn setup_ng_tokens() -> NgTokensInput {
    NgTokensInput {
        data: load_save(),
        resolver: load_resolver(),
    }
}

fn count_master_kind(input: &MasterTokensInput) -> usize {
    let mut reader = jomini::binary::TokenReader::from_slice(&input.data);
    let mut count = 0;
    while let Some(tok) = reader.next_token().unwrap() {
        black_box(tok);
        count += 1;
    }
    count
}

fn count_master(input: &MasterTokensInput) -> usize {
    let mut reader = jomini::binary::TokenReader::from_slice(&input.data);
    let mut count = 0;
    while let Some(tok) = reader.next().unwrap() {
        black_box(tok);
        count += 1;
    }
    count
}

fn count_ng(input: &NgTokensInput) -> usize {
    let format = Eu4Format::new(&input.resolver)
        .with_failed_resolve_strategy(FailedResolveStrategy::Ignore);
    let mut reader = jomini::binary::ng::TokenReader::from_slice(&input.data, format);
    let mut count = 0;
    while let Some(tok) = reader.next_token().unwrap() {
        black_box(tok);
        count += 1;
    }
    count
}

#[library_benchmark(setup = setup_master_tokens)]
#[bench::master_kind()]
fn master_kind_tokens(input: MasterTokensInput) -> usize {
    count_master_kind(&input)
}

#[library_benchmark(setup = setup_master_tokens)]
#[bench::master()]
fn master_tokens(input: MasterTokensInput) -> usize {
    count_master(&input)
}

#[library_benchmark(setup = setup_ng_tokens)]
#[bench::ng()]
fn ng_tokens(input: NgTokensInput) -> usize {
    count_ng(&input)
}

library_benchmark_group!(
    name = token_gungraun_benches,
    benchmarks = [master_kind_tokens, master_tokens, ng_tokens]
);

main!(library_benchmark_groups = [token_gungraun_benches]);
