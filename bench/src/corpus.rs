use std::{
    collections::HashMap,
    fmt,
    io::Read,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Format {
    Binary,
    Text,
}

impl Format {
    fn slug(self) -> &'static str {
        match self {
            Self::Binary => "bin",
            Self::Text => "txt",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Game {
    Eu4,
    Ck3,
    Vic3,
}

impl Game {
    fn slug(self) -> &'static str {
        match self {
            Self::Eu4 => "eu4",
            Self::Ck3 => "ck3",
            Self::Vic3 => "v3",
        }
    }

    fn binary_header_skip(self) -> usize {
        match self {
            Self::Eu4 => "EU4bin".len(),
            Self::Ck3 | Self::Vic3 => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Corpus {
    pub format: Format,
    pub game: Game,
}

impl Corpus {
    pub const fn binary(game: Game) -> Self {
        Self {
            format: Format::Binary,
            game,
        }
    }

    pub const fn text(game: Game) -> Self {
        Self {
            format: Format::Text,
            game,
        }
    }

    fn relative_path(self) -> String {
        format!("jomini/{}.{}", self.format.slug(), self.game.slug())
    }

    fn cache_path(self) -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("bench crate should live under repository root")
            .join("assets")
            .join("saves")
            .join(self.relative_path().as_str())
    }

    fn header_skip(self) -> usize {
        match self.format {
            Format::Binary => self.game.binary_header_skip(),
            Format::Text => 0,
        }
    }
}

impl fmt::Display for Corpus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.format.slug(), self.game.slug())
    }
}

#[derive(Clone, Debug)]
pub struct CorpusArchive {
    pub corpus: Corpus,
    pub path: PathBuf,
    pub uncompressed_size: u64,
    pub header_skip: usize,
}

#[derive(Clone, Copy, Debug)]
pub struct CorpusBytes {
    pub archive: &'static CorpusArchive,
    pub data: &'static [u8],
}

static ARCHIVES: OnceLock<Mutex<HashMap<Corpus, &'static CorpusArchive>>> = OnceLock::new();
static BYTES: OnceLock<Mutex<HashMap<Corpus, &'static [u8]>>> = OnceLock::new();

pub fn archive(corpus: Corpus) -> &'static CorpusArchive {
    let archives = ARCHIVES.get_or_init(|| Mutex::new(HashMap::new()));
    let mut archives = archives.lock().expect("corpus archive cache poisoned");
    if let Some(archive) = archives.get(&corpus) {
        return archive;
    }

    let archive = Box::leak(Box::new(fetch_and_validate(corpus)));
    archives.insert(corpus, archive);
    archive
}

pub fn bytes(corpus: Corpus) -> CorpusBytes {
    let archive = archive(corpus);
    let bytes = BYTES.get_or_init(|| Mutex::new(HashMap::new()));
    let mut bytes = bytes.lock().expect("corpus byte cache poisoned");
    if let Some(data) = bytes.get(&corpus) {
        return CorpusBytes { archive, data };
    }

    let data = Box::leak(read_archive_to_vec(archive).into_boxed_slice());
    bytes.insert(corpus, data);
    CorpusBytes { archive, data }
}

pub fn read_archive_to_vec(archive: &CorpusArchive) -> Vec<u8> {
    let file = std::fs::File::open(&archive.path).unwrap_or_else(|e| {
        panic!(
            "failed to open corpus archive {} at {}: {e}",
            archive.corpus,
            archive.path.display()
        )
    });
    let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
    let zip = rawzip::ZipArchive::from_file(file, &mut buf)
        .unwrap_or_else(|e| panic!("failed to read zip archive {}: {e}", archive.corpus));
    let (wayfinder, compression_method, max_size) = largest_entry(&zip, &mut buf, archive.corpus);

    assert_eq!(
        compression_method,
        rawzip::CompressionMethod::Deflate,
        "expected largest entry in {} to use deflate compression",
        archive.corpus
    );

    let entry = zip
        .get_entry(wayfinder)
        .unwrap_or_else(|e| panic!("failed to get largest entry in {}: {e}", archive.corpus));
    let reader = flate2::read::DeflateDecoder::new_with_buf(entry.reader(), buf);
    let mut reader = entry.verifying_reader(reader);

    let mut skip_buf = vec![0u8; archive.header_skip];
    reader.read_exact(&mut skip_buf).unwrap_or_else(|e| {
        panic!(
            "failed to skip {} header bytes for {}: {e}",
            archive.header_skip, archive.corpus
        )
    });

    let capacity = max_size.saturating_sub(archive.header_skip as u64) as usize;
    let mut output = Vec::with_capacity(capacity);
    reader
        .read_to_end(&mut output)
        .unwrap_or_else(|e| panic!("failed to decompress {}: {e}", archive.corpus));
    output
}

fn fetch_and_validate(corpus: Corpus) -> CorpusArchive {
    let path = corpus.cache_path();
    if !path.exists() {
        let relative_path = corpus.relative_path();
        println!("cache miss: {relative_path}");
        let url = format!("https://cdn-dev.pdx.tools/{relative_path}");
        let mut resp = attohttpc::get(&url)
            .send()
            .unwrap_or_else(|e| panic!("failed to download {url}: {e}"));

        if !resp.is_success() {
            panic!("expected HTTP 200 from {url}, got {}", resp.status());
        }

        std::fs::create_dir_all(path.parent().expect("corpus path should have parent"))
            .unwrap_or_else(|e| panic!("failed to create {}: {e}", path.display()));
        let mut file = std::fs::File::create(&path)
            .unwrap_or_else(|e| panic!("failed to create {}: {e}", path.display()));
        std::io::copy(&mut resp, &mut file)
            .unwrap_or_else(|e| panic!("failed to write {}: {e}", path.display()));
    } else {
        println!("cache hit: {}", corpus.relative_path());
    }

    let file = std::fs::File::open(&path)
        .unwrap_or_else(|e| panic!("failed to open corpus archive {}: {e}", path.display()));
    let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
    let zip = rawzip::ZipArchive::from_file(file, &mut buf)
        .unwrap_or_else(|e| panic!("failed to read zip archive {}: {e}", path.display()));
    let (_, _, max_size) = largest_entry(&zip, &mut buf, corpus);

    assert!(
        max_size > 1024 * 1024,
        "expected largest entry in {corpus} to be larger than 1 MiB, got {max_size} bytes"
    );

    CorpusArchive {
        corpus,
        path,
        uncompressed_size: max_size,
        header_skip: corpus.header_skip(),
    }
}

fn largest_entry<R: rawzip::ReaderAt>(
    zip: &rawzip::ZipArchive<R>,
    buf: &mut [u8],
    corpus: Corpus,
) -> (
    rawzip::ZipArchiveEntryWayfinder,
    rawzip::CompressionMethod,
    u64,
) {
    let mut entries = zip.entries(buf);
    let mut max_size = 0;
    let mut max_entry = None;
    while let Some(entry) = entries
        .next_entry()
        .unwrap_or_else(|e| panic!("failed to read entry from {corpus}: {e}"))
    {
        if entry.uncompressed_size_hint() > max_size {
            max_entry = Some((entry.wayfinder(), entry.compression_method()));
            max_size = entry.uncompressed_size_hint();
        }
    }

    let (wayfinder, compression_method) =
        max_entry.unwrap_or_else(|| panic!("expected at least one zip entry in {corpus}"));
    (wayfinder, compression_method, max_size)
}
