//! Test support shared by the game crates: token files and save fixtures.
//!
//! Token files are private. They are read from `PDX_TOKENS_DIR`, or from
//! `assets/tokens` at the workspace root, with the layout of the tokens
//! repository (`eu4.txt`, `ck3.txt`, ...). A missing file is not an error,
//! so that tests can skip when no tokens are installed.
//!
//! Save fixtures are fetched from a CDN on first use and cached under
//! `assets/saves/<game>`. Each game crate lists the fixtures its tests use
//! in `tests/fixtures.txt`. CI keys the fixture cache on that file and, with
//! `PDX_FIXTURES_LOG` set, records every access to check the list. To
//! regenerate a list, run the tests with the variable set and `sort -u` the
//! file.

use std::fs::File;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::sync::Mutex;

fn assets_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("assets")
}

/// Read the token file for a game.
///
/// Returns an empty buffer when no token file is installed.
pub fn tokens(game: &str) -> Vec<u8> {
    let dir = std::env::var_os("PDX_TOKENS_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| assets_dir().join("tokens"));
    std::fs::read(dir.join(format!("{game}.txt"))).unwrap_or_default()
}

/// Read a save fixture into memory, fetching it into the cache on first use.
pub fn read(game: &str, name: &str) -> Vec<u8> {
    let mut data = Vec::new();
    request_file(game, name).read_to_end(&mut data).unwrap();
    data
}

/// Tests run in parallel, so each line is one append.
fn log_access(name: &str) {
    let Some(path) = std::env::var_os("PDX_FIXTURES_LOG") else {
        return;
    };
    let mut log = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .expect("PDX_FIXTURES_LOG to be writable");
    log.write_all(format!("{name}\n").as_bytes()).unwrap();
}

static FETCH: Mutex<()> = Mutex::new(());

/// Open a save fixture, fetching it into the cache on first use.
pub fn request_file(game: &str, name: &str) -> File {
    log_access(name);
    let cache = assets_dir().join("saves").join(game).join(name);
    if !cache.exists() {
        let _guard = FETCH.lock().unwrap();
        if !cache.exists() {
            println!("cache miss: {name}");
            let url = format!("https://cdn-dev.pdx.tools/{game}-saves/{name}");
            let mut resp = attohttpc::get(&url)
                .send()
                .unwrap_or_else(|e| panic!("failed to download {url}: {e}"));
            if !resp.is_success() {
                panic!("expected HTTP 200 from {url}, got {}", resp.status());
            }

            std::fs::create_dir_all(cache.parent().unwrap()).unwrap();
            let mut f = File::create(&cache).unwrap();
            std::io::copy(&mut resp, &mut f).unwrap();
        }
    } else {
        println!("cache hit: {name}");
    }

    File::open(cache).unwrap()
}
