//! Test support shared by the game crates.
//!
//! Two kinds of files live outside the repository:
//!
//! - Token files, which are private. They are read from the directory named
//!   by the `PDX_TOKENS_DIR` environment variable, or from `assets/tokens` at
//!   the workspace root. The directory has the same layout as the `tokens`
//!   folder of the tokens repository (`eu4.txt`, `ck3.txt`, ...). A missing
//!   file is not an error, so that tests can skip when no tokens are
//!   installed.
//! - Save fixtures, which are large. They are fetched from a public bucket on
//!   first use and cached under `assets/saves/<game>` at the workspace root.
//!   Previous implementations used git lfs, but ran out of the monthly free
//!   bandwidth on day one. The bucket is on backblaze, which provides 1GB of
//!   free download per day.

use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

/// The `assets` directory at the workspace root.
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

static FETCH: Mutex<()> = Mutex::new(());

/// Open a save fixture, fetching it into the cache on first use.
///
/// The fixture is cached at `assets/saves/<game>/<name>` at the workspace
/// root.
pub fn request_file(game: &str, name: &str) -> File {
    let cache = assets_dir().join("saves").join(game).join(name);
    if !cache.exists() {
        // Serialize downloads so that parallel tests do not fetch the same
        // file twice.
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
