![ci](https://github.com/rakaly/jomini/workflows/ci/badge.svg)

# Arena serde

Arena serde extends [serde](https://serde.rs/)'s deserialization to allocate into an arena. It provides an `ArenaDeserialize` trait, which is similar to serde's `Deserialize`, but places data in a provided arena (currently [bumpalo](https://github.com/fitzgen/bumpalo)) instead of the global heap allocator.

Arenas and deserialization are a natural fit:

- A single drop for the deserialized model: no matter how many strings a model has, it takes constant time to free the data.
- The arena can pre-allocate the capacity expected for deserialization.
- Amortizes the allocation cost of many fields that need allocations.
- Minimizes the volatility of the system allocator. Especially well suited for Wasm, where one has minimal control of the underlying allocator.
- Allows efficient views into the underlying data without keeping the entire input around, unlike borrowed deserialization.
- Less brittle in performance than serde's `deserialize_in_place`.

This crate is experimental and is not published on crates.io.

## Benchmarks

Deserializing `twitter.json` corpus:

- Json: 961 µs → 798 µs (-17%)
- Binary: 447 µs → 326 µs (-27%)

Deserializing with `Drop`

- Json: 1025 µs → 791 µs (-23%)
- Binary: 515 µs → 324 µs (-37%)


```bash
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-criterion -- arena
cargo bench --manifest-path bench/Cargo.toml --bench jomini-bench-gungraun -- 'jomini_bench_gungraun::arena_benches::*'
```

## Quick example

```rust
use arena_serde::{Arena, ArenaDeserialize, ArenaSeed};
use serde::de::DeserializeSeed;

#[derive(ArenaDeserialize)]
struct User<'bump> {
    name: &'bump str,
    tags: &'bump [&'bump str],
    id: u64,
}

let arena = Arena::new();
let json = r#"{"name": "Alice", "email": "alice@example.com", "tags": ["admin", "verified"], "id": 42}"#;
let mut deserializer = serde_json::Deserializer::from_str(json);

let user: User = ArenaSeed::new(&arena)
    .deserialize(&mut deserializer)
    .unwrap();

assert_eq!(user.name, "Alice");
assert_eq!(user.tags, ["admin", "verified"]);
// All string data is allocated in the arena and freed when `arena` is dropped
```

## How the derive decides what to do

The derive does not guess from a field's type. Every field is deserialized through `ArenaDeserialize`, whether or not the struct has a lifetime parameter. Owned types (`String`, `Vec<T>`, the primitives, `Option<T>`, ...) implement the trait by forwarding to `Deserialize`, so they can appear as fields next to arena data. A `&'bump T` field puts the nested value in the arena, which permits recursive models without a `Box`.

A type with a hand written `Deserialize` impl does not get `ArenaDeserialize` for free. Write a short impl that forwards to it:

```rust
use arena_serde::{Arena, ArenaDeserialize};

# #[derive(serde::Deserialize)] struct Color;
impl<'bump> ArenaDeserialize<'bump> for Color {
    fn deserialize_in_arena<'de, D>(deserializer: D, _arena: &'bump Arena) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        serde::Deserialize::deserialize(deserializer)
    }
}
```

Field attributes:

- `#[arena(alias = "name")]`: accept another key for the field.
- `#[arena(default)]`: use `Default` when the key is missing.
- `#[arena(deserialize_with = "path")]`: call `path(deserializer, &arena)` for the field.
- `#[arena(duplicated)]`: collect every occurrence of the key into a `&'bump [T]` field.

Enums with only unit variants can also derive `ArenaDeserialize`:

- `#[arena(rename_all = "snake_case")]` on the enum, with the same rules as serde.
- `#[arena(rename = "name")]` on a variant.
- `#[arena(other)]` on one variant, which receives unknown identifiers.

Enums with data-carrying variants need a hand written impl.
