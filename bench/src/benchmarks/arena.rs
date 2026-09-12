//! Heap deserialization against arena deserialization on `twitter.json` from
//! serde's `json-benchmark`, read as JSON and as jomini binary.

use arena_serde::{Arena, ArenaSeed};
use highway::{HighwayHash, HighwayHasher};
use jomini::binary::BinaryFlavor;
use model::{arena, heap};
use serde::de::DeserializeSeed;
use std::io::Cursor;
use std::path::{Path, PathBuf};

pub mod encode;

pub const TWITTER_URL: &str =
    "https://raw.githubusercontent.com/serde-rs/json-benchmark/master/data/twitter.json";

/// Pins the corpus so that every machine measures the same bytes.
const TWITTER_HASH: u64 = 0xbe92ec2eef033dc2;

fn twitter_cache_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("bench crate should live under repository root")
        .join("assets")
        .join("twitter.json")
}

/// The first run fetches the corpus into a cache that git ignores.
pub fn setup_twitter_data() -> Vec<u8> {
    let path = twitter_cache_path();
    if let Ok(data) = std::fs::read(&path) {
        return data;
    }

    let data = fetch_twitter_data(&path);
    std::fs::write(&path, &data)
        .unwrap_or_else(|err| panic!("failed to write {}: {err}", path.display()));
    data
}

fn fetch_twitter_data(path: &Path) -> Vec<u8> {
    eprintln!("fetching {TWITTER_URL} into {}", path.display());
    let resp = attohttpc::get(TWITTER_URL)
        .send()
        .unwrap_or_else(|err| panic!("failed to fetch {TWITTER_URL}: {err}"));
    if !resp.is_success() {
        panic!("failed to fetch {TWITTER_URL}: HTTP {}", resp.status());
    }
    let data = resp
        .bytes()
        .unwrap_or_else(|err| panic!("failed to read {TWITTER_URL}: {err}"));

    let actual = HighwayHasher::default().hash64(&data);
    if actual != TWITTER_HASH {
        panic!("hash of {TWITTER_URL} is {actual:#018x}, expected {TWITTER_HASH:#018x}");
    }

    data
}

pub mod model {
    pub mod heap {
        use serde::Deserialize;

        #[derive(Debug, Deserialize)]
        pub struct Twitter {
            pub statuses: Vec<Status>,
            pub search_metadata: SearchMetadata,
        }

        #[derive(Debug, Deserialize)]
        pub struct SearchMetadata {
            pub completed_in: f64,
            pub max_id: u64,
            pub max_id_str: String,
            pub next_results: String,
            pub query: String,
            pub refresh_url: String,
            pub count: u32,
            pub since_id: u64,
            pub since_id_str: String,
        }

        #[derive(Debug, Deserialize)]
        pub struct Status {
            pub metadata: Metadata,
            pub created_at: String,
            pub id: u64,
            pub id_str: String,
            pub text: String,
            pub source: String,
            pub truncated: bool,
            pub in_reply_to_status_id: Option<u64>,
            pub in_reply_to_status_id_str: Option<String>,
            pub in_reply_to_user_id: Option<u64>,
            pub in_reply_to_user_id_str: Option<String>,
            pub in_reply_to_screen_name: Option<String>,
            pub user: User,
            pub retweeted_status: Option<Box<Status>>,
            pub retweet_count: u32,
            pub favorite_count: u32,
            pub entities: Entities,
            pub favorited: bool,
            pub retweeted: bool,
            pub possibly_sensitive: Option<bool>,
            pub lang: String,
        }

        #[derive(Debug, Deserialize)]
        pub struct Metadata {
            pub result_type: String,
            pub iso_language_code: String,
        }

        #[derive(Debug, Deserialize)]
        pub struct User {
            pub id: u64,
            pub id_str: String,
            pub name: String,
            pub screen_name: String,
            pub location: String,
            pub description: String,
            pub url: Option<String>,
            pub entities: UserEntities,
            pub protected: bool,
            pub followers_count: u32,
            pub friends_count: u32,
            pub listed_count: u32,
            pub created_at: String,
            pub favourites_count: u32,
            pub utc_offset: Option<i32>,
            pub time_zone: Option<String>,
            pub geo_enabled: bool,
            pub verified: bool,
            pub statuses_count: u32,
            pub lang: String,
            pub contributors_enabled: bool,
            pub is_translator: bool,
            pub is_translation_enabled: bool,
            pub profile_background_color: String,
            pub profile_background_image_url: String,
            pub profile_background_image_url_https: String,
            pub profile_background_tile: bool,
            pub profile_image_url: String,
            pub profile_image_url_https: String,
            pub profile_banner_url: Option<String>,
            pub profile_link_color: String,
            pub profile_sidebar_border_color: String,
            pub profile_sidebar_fill_color: String,
            pub profile_text_color: String,
            pub profile_use_background_image: bool,
            pub default_profile: bool,
            pub default_profile_image: bool,
            pub following: bool,
            pub follow_request_sent: bool,
            pub notifications: bool,
        }

        #[derive(Debug, Deserialize)]
        pub struct UserEntities {
            pub url: Option<UserUrls>,
            pub description: UserUrls,
        }

        #[derive(Debug, Deserialize)]
        pub struct UserUrls {
            pub urls: Vec<Url>,
        }

        #[derive(Debug, Deserialize)]
        pub struct Entities {
            pub hashtags: Vec<Hashtag>,
            pub symbols: Vec<Hashtag>,
            pub urls: Vec<Url>,
            pub user_mentions: Vec<UserMention>,
            #[serde(default)]
            pub media: Vec<Media>,
        }

        #[derive(Debug, Deserialize)]
        pub struct Hashtag {
            pub text: String,
            pub indices: Vec<u32>,
        }

        #[derive(Debug, Deserialize)]
        pub struct Url {
            pub url: String,
            pub expanded_url: String,
            pub display_url: String,
            pub indices: Vec<u32>,
        }

        #[derive(Debug, Deserialize)]
        pub struct UserMention {
            pub screen_name: String,
            pub name: String,
            pub id: u64,
            pub id_str: String,
            pub indices: Vec<u32>,
        }

        #[derive(Debug, Deserialize)]
        pub struct Media {
            pub id: u64,
            pub id_str: String,
            pub indices: Vec<u32>,
            pub media_url: String,
            pub media_url_https: String,
            pub url: String,
            pub display_url: String,
            pub expanded_url: String,
            #[serde(rename = "type")]
            pub media_type: String,
            pub sizes: Sizes,
        }

        #[derive(Debug, Deserialize)]
        pub struct Sizes {
            pub medium: Size,
            pub small: Size,
            pub thumb: Size,
            pub large: Size,
        }

        #[derive(Debug, Deserialize)]
        pub struct Size {
            pub w: u32,
            pub h: u32,
            pub resize: String,
        }
    }

    pub mod arena {
        use arena_serde::ArenaDeserialize;

        #[derive(Debug, ArenaDeserialize)]
        pub struct Twitter<'bump> {
            pub statuses: &'bump [Status<'bump>],
            pub search_metadata: SearchMetadata<'bump>,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct SearchMetadata<'bump> {
            pub completed_in: f64,
            pub max_id: u64,
            pub max_id_str: &'bump str,
            pub next_results: &'bump str,
            pub query: &'bump str,
            pub refresh_url: &'bump str,
            pub count: u32,
            pub since_id: u64,
            pub since_id_str: &'bump str,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Status<'bump> {
            pub metadata: Metadata<'bump>,
            pub created_at: &'bump str,
            pub id: u64,
            pub id_str: &'bump str,
            pub text: &'bump str,
            pub source: &'bump str,
            pub truncated: bool,
            pub in_reply_to_status_id: Option<u64>,
            pub in_reply_to_status_id_str: Option<&'bump str>,
            pub in_reply_to_user_id: Option<u64>,
            pub in_reply_to_user_id_str: Option<&'bump str>,
            pub in_reply_to_screen_name: Option<&'bump str>,
            pub user: User<'bump>,
            pub retweeted_status: Option<&'bump Status<'bump>>,
            pub retweet_count: u32,
            pub favorite_count: u32,
            pub entities: Entities<'bump>,
            pub favorited: bool,
            pub retweeted: bool,
            pub possibly_sensitive: Option<bool>,
            pub lang: &'bump str,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Metadata<'bump> {
            pub result_type: &'bump str,
            pub iso_language_code: &'bump str,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct User<'bump> {
            pub id: u64,
            pub id_str: &'bump str,
            pub name: &'bump str,
            pub screen_name: &'bump str,
            pub location: &'bump str,
            pub description: &'bump str,
            pub url: Option<&'bump str>,
            pub entities: UserEntities<'bump>,
            pub protected: bool,
            pub followers_count: u32,
            pub friends_count: u32,
            pub listed_count: u32,
            pub created_at: &'bump str,
            pub favourites_count: u32,
            pub utc_offset: Option<i32>,
            pub time_zone: Option<&'bump str>,
            pub geo_enabled: bool,
            pub verified: bool,
            pub statuses_count: u32,
            pub lang: &'bump str,
            pub contributors_enabled: bool,
            pub is_translator: bool,
            pub is_translation_enabled: bool,
            pub profile_background_color: &'bump str,
            pub profile_background_image_url: &'bump str,
            pub profile_background_image_url_https: &'bump str,
            pub profile_background_tile: bool,
            pub profile_image_url: &'bump str,
            pub profile_image_url_https: &'bump str,
            pub profile_banner_url: Option<&'bump str>,
            pub profile_link_color: &'bump str,
            pub profile_sidebar_border_color: &'bump str,
            pub profile_sidebar_fill_color: &'bump str,
            pub profile_text_color: &'bump str,
            pub profile_use_background_image: bool,
            pub default_profile: bool,
            pub default_profile_image: bool,
            pub following: bool,
            pub follow_request_sent: bool,
            pub notifications: bool,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct UserEntities<'bump> {
            pub url: Option<UserUrls<'bump>>,
            pub description: UserUrls<'bump>,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct UserUrls<'bump> {
            pub urls: &'bump [Url<'bump>],
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Entities<'bump> {
            pub hashtags: &'bump [Hashtag<'bump>],
            pub symbols: &'bump [Hashtag<'bump>],
            pub urls: &'bump [Url<'bump>],
            pub user_mentions: &'bump [UserMention<'bump>],
            #[arena(default)]
            pub media: &'bump [Media<'bump>],
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Hashtag<'bump> {
            pub text: &'bump str,
            pub indices: &'bump [u32],
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Url<'bump> {
            pub url: &'bump str,
            pub expanded_url: &'bump str,
            pub display_url: &'bump str,
            pub indices: &'bump [u32],
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct UserMention<'bump> {
            pub screen_name: &'bump str,
            pub name: &'bump str,
            pub id: u64,
            pub id_str: &'bump str,
            pub indices: &'bump [u32],
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Media<'bump> {
            pub id: u64,
            pub id_str: &'bump str,
            pub indices: &'bump [u32],
            pub media_url: &'bump str,
            pub media_url_https: &'bump str,
            pub url: &'bump str,
            pub display_url: &'bump str,
            pub expanded_url: &'bump str,
            #[arena(alias = "type")]
            pub media_type: &'bump str,
            pub sizes: Sizes<'bump>,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Sizes<'bump> {
            pub medium: Size<'bump>,
            pub small: Size<'bump>,
            pub thumb: Size<'bump>,
            pub large: Size<'bump>,
        }

        #[derive(Debug, ArenaDeserialize)]
        pub struct Size<'bump> {
            pub w: u32,
            pub h: u32,
            pub resize: &'bump str,
        }
    }
}

/// One encoding of the corpus, and the two ways to read it.
pub trait Corpus {
    fn name(&self) -> &'static str;

    fn data(&self) -> &[u8];

    fn heap(&self) -> heap::Twitter;

    fn arena<'bump>(&self, arena: &'bump Arena) -> arena::Twitter<'bump>;
}

#[derive(Debug)]
pub struct SerdeJson(Vec<u8>);

impl Corpus for SerdeJson {
    fn name(&self) -> &'static str {
        "serde_json"
    }

    fn data(&self) -> &[u8] {
        &self.0
    }

    fn heap(&self) -> heap::Twitter {
        serde_json::from_slice(&self.0).expect("the corpus to match the heap model")
    }

    fn arena<'bump>(&self, arena: &'bump Arena) -> arena::Twitter<'bump> {
        let mut de = serde_json::Deserializer::from_slice(&self.0);
        ArenaSeed::new(arena)
            .deserialize(&mut de)
            .expect("the corpus to match the arena model")
    }
}

/// Read through a reader and not a slice. A slice lets the model borrow each
/// string from the input, but a save arrives as a stream out of a zip entry,
/// where every string must be copied. The arena and the heap are the two
/// places to copy it to.
#[derive(Debug)]
pub struct JominiBinary {
    data: Vec<u8>,
    tokens: encode::TokenTable,
}

impl Corpus for JominiBinary {
    fn name(&self) -> &'static str {
        "jomini_binary"
    }

    fn data(&self) -> &[u8] {
        &self.data
    }

    fn heap(&self) -> heap::Twitter {
        encode::TwitterFlavor
            .deserializer()
            .from_reader(Cursor::new(&self.data), &self.tokens)
            .deserialize()
            .expect("the corpus to match the heap model")
    }

    fn arena<'bump>(&self, arena: &'bump Arena) -> arena::Twitter<'bump> {
        let mut de = encode::TwitterFlavor
            .deserializer()
            .from_reader(Cursor::new(&self.data), &self.tokens);
        ArenaSeed::new(arena)
            .deserialize(&mut de)
            .expect("the corpus to match the arena model")
    }
}

pub fn setup_serde_json() -> Box<dyn Corpus> {
    Box::new(SerdeJson(setup_twitter_data()))
}

pub fn setup_jomini_binary() -> Box<dyn Corpus> {
    let document: serde_json::Value =
        serde_json::from_slice(&setup_twitter_data()).expect("the corpus to be JSON");
    let (data, tokens) = encode::to_binary(&document);
    Box::new(JominiBinary { data, tokens })
}

/// Walks a model so that the parse result is used.
macro_rules! checksum {
    ($name:ident, $model:ident, $s:ident => $retweet:expr) => {
        pub fn $name(twitter: &$model::Twitter) -> u64 {
            fn status(status: &$model::Status) -> u64 {
                // Keep the low bits of the id so that the sum does not overflow.
                (status.id & 0xffff)
                    + status.text.len() as u64
                    + status.user.screen_name.len() as u64
                    + status.user.description.len() as u64
                    + status.entities.hashtags.len() as u64
                    + status.entities.urls.len() as u64
                    + status.entities.user_mentions.len() as u64
                    + status.entities.media.len() as u64
                    + status.lang.len() as u64
            }

            let statuses: u64 = twitter
                .statuses
                .iter()
                .map(|$s| status($s) + $retweet.map_or(0, status))
                .sum();
            statuses + twitter.search_metadata.max_id_str.len() as u64
        }
    };
}

checksum!(heap_checksum, heap, s => s.retweeted_status.as_deref());
checksum!(arena_checksum, arena, s => s.retweeted_status);

pub mod criterion_benches {
    use super::Corpus;
    use arena_serde::Arena;
    use criterion::measurement::WallTime;
    use criterion::{BenchmarkGroup, Criterion, Throughput};
    use std::hint::black_box;

    fn bench_corpus(group: &mut BenchmarkGroup<'_, WallTime>, corpus: &dyn Corpus) {
        let name = corpus.name();

        group.throughput(Throughput::Bytes(corpus.data().len() as u64));

        let heap_model = || {
            let twitter = corpus.heap();
            black_box(super::heap_checksum(&twitter));
            twitter
        };

        // The arena owns the data, so it is what the benchmark returns and drops.
        let arena_model = || {
            let arena = Arena::new();
            black_box(super::arena_checksum(&corpus.arena(&arena)));
            arena
        };

        group.bench_function(format!("{name}/heap_parse"), |b| {
            b.iter_with_large_drop(heap_model)
        });
        group.bench_function(format!("{name}/heap_parse_drop"), |b| b.iter(heap_model));

        group.bench_function(format!("{name}/arena_parse"), |b| {
            b.iter_with_large_drop(arena_model)
        });
        group.bench_function(format!("{name}/arena_parse_drop"), |b| b.iter(arena_model));

        // One arena for all iterations, as a program that parses many documents.
        group.bench_function(format!("{name}/arena_reuse"), |b| {
            let mut arena = Arena::new();
            b.iter(|| {
                arena.reset();
                super::arena_checksum(&corpus.arena(&arena))
            })
        });
    }

    pub fn arena(c: &mut Criterion) {
        let mut group = c.benchmark_group("arena");
        bench_corpus(&mut group, &*super::setup_serde_json());
        bench_corpus(&mut group, &*super::setup_jomini_binary());
        group.finish();
    }

    criterion::criterion_group!(arena_benches, arena);
}

/// The harness drops a return value outside of the measurement, so a benchmark
/// that returns the model excludes the drop and one that returns the checksum
/// includes it.
pub mod gungraun_benches {
    use super::{Corpus, arena_checksum, heap_checksum, model::heap};
    use arena_serde::Arena;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::serde_json(setup = crate::benchmarks::arena::setup_serde_json)]
    #[bench::jomini_binary(setup = crate::benchmarks::arena::setup_jomini_binary)]
    fn heap_parse(corpus: Box<dyn Corpus>) -> (u64, heap::Twitter) {
        let twitter = corpus.heap();
        (heap_checksum(&twitter), twitter)
    }

    #[library_benchmark]
    #[bench::serde_json(setup = crate::benchmarks::arena::setup_serde_json)]
    #[bench::jomini_binary(setup = crate::benchmarks::arena::setup_jomini_binary)]
    fn heap_parse_drop(corpus: Box<dyn Corpus>) -> u64 {
        heap_checksum(&corpus.heap())
    }

    #[library_benchmark]
    #[bench::serde_json(setup = crate::benchmarks::arena::setup_serde_json)]
    #[bench::jomini_binary(setup = crate::benchmarks::arena::setup_jomini_binary)]
    fn arena_parse(corpus: Box<dyn Corpus>) -> (u64, Arena) {
        let arena = Arena::new();
        let checksum = arena_checksum(&corpus.arena(&arena));
        (checksum, arena)
    }

    #[library_benchmark]
    #[bench::serde_json(setup = crate::benchmarks::arena::setup_serde_json)]
    #[bench::jomini_binary(setup = crate::benchmarks::arena::setup_jomini_binary)]
    fn arena_parse_drop(corpus: Box<dyn Corpus>) -> u64 {
        let arena = Arena::new();
        arena_checksum(&corpus.arena(&arena))
    }

    library_benchmark_group!(
        name = arena_benches,
        benchmarks = [heap_parse, heap_parse_drop, arena_parse, arena_parse_drop]
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn corpora_agree() {
        let json = setup_serde_json();
        let expected = heap_checksum(&json.heap());

        for corpus in [json, setup_jomini_binary()] {
            let arena = Arena::new();
            assert_eq!(expected, heap_checksum(&corpus.heap()), "{}", corpus.name());
            assert_eq!(
                expected,
                arena_checksum(&corpus.arena(&arena)),
                "{} arena",
                corpus.name()
            );
        }
    }
}
