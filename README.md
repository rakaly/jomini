![ci](https://github.com/rakaly/jomini/workflows/ci/badge.svg) [![](https://docs.rs/jomini/badge.svg)](https://docs.rs/jomini) [![Version](https://img.shields.io/crates/v/jomini.svg?style=flat-square)](https://crates.io/crates/jomini)

# Jomini

A low level, performance oriented parser for
[EU4](https://en.wikipedia.org/wiki/Europa_Universalis_IV) save files and other
[PDS](https://www.paradoxplaza.com/) developed titles.

Jomini is the cornerstone of the [Rakaly](https://rakaly.com/eu4), an EU4 achievement leaderboard
and save file analyzer. This library is also used in the [Paradox Game Converters
project](https://github.com/ParadoxGameConverters/EU4toVic2) to parse ironman EU4, CK3, and
Imperator saves.

## Features

- ✔ Versatile: Handle both plaintext and binary encoded data
- ✔ Fast: Parse data at 1 GB/s
- ✔ Small: Compile with zero dependencies
- ✔ Safe: Extensively fuzzed against potential malicious input
- ✔ Ergonomic: Use [serde](https://serde.rs/derive.html)-like macros to have parsing logic automatically implemented
- ✔ Embeddable: Cross platform native apps, statically compiled services, or in the browser via [WASM](https://webassembly.org/)
- ✔ Agnostic: [Parse EU4](https://github.com/rakaly/eu4save), [HOI4](https://github.com/rakaly/hoi4save.git), [Imperator](https://github.com/rakaly/imperator-save), [CK3](https://github.com/rakaly/ck3save), etc save files

## Quick Start

Below is a demonstration on parsing plaintext data using jomini tools.

```rust
use jomini::{JominiDeserialize, TextDeserializer};

#[derive(JominiDeserialize, PartialEq, Debug)]
pub struct Model {
    human: bool,
    first: Option<u16>,
    #[jomini(alias = "forth")]
    fourth: u16,
    #[jomini(alias = "core", duplicated)]
    cores: Vec<String>,
    names: Vec<String>,
}

let data = br#"
    human = yes
    forth = 10
    core = "HAB"
    names = { "Johan" "Frederick" }
    core = FRA
"#;

let expected = Model {
    human: true,
    first: None,
    fourth: 10,
    cores: vec!["HAB".to_string(), "FRA".to_string()],
    names: vec!["Johan".to_string(), "Frederick".to_string()],
};

let actual: Model = TextDeserializer::from_windows1252_slice(data)?;
assert_eq!(actual, expected);
```

## Binary Parsing

Parsing data encoded in the binary format is done in a similar fashion but with an extra step.
Tokens can be encoded into 16 integers, and so one must provide a map from these integers to their
textual representations

```rust
use jomini::{JominiDeserialize, BinaryDeserializer};
use std::collections::HashMap;

#[derive(JominiDeserialize, PartialEq, Debug)]
struct MyStruct {
    field1: String,
}

let data = [ 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47 ];

let mut map = HashMap::new();
map.insert(0x2d82, "field1");

let actual: MyStruct = BinaryDeserializer::from_eu4(&data[..], &map)?;
assert_eq!(actual, MyStruct { field1: "ENG".to_string() });
```

When done correctly, one can use the same structure to represent both the plaintext and binary data
without any duplication.

One can configure the behavior when a token is unknown (ie: fail immediately or try to continue).

## Caveats

Caller is responsible for:

- Determining the correct format (text or binary) ahead of time
- Stripping off any header that may be present (eg: `EU4txt` / `EU4bin`)
- Providing the token resolver for the binary format
- Providing the conversion to reconcile how, for example, a date may be encoded as an integer in
the binary format, but as a string when in plaintext.

## The Mid-level API

If the automatic deserialization via `JominiDeserialize` is too high level, there is a mid-level
api where one can easily iterate through the parsed document and interrogate fields for
their information.

```rust
use jomini::TextTape;

let data = b"name=aaa name=bbb core=123 name=ccc name=ddd";
let tape = TextTape::from_slice(data).unwrap();
let mut reader = tape.windows1252_reader();

while let Some((key, _op, value)) = reader.next_field() {
    println!("{:?}={:?}", key.read_str(), value.read_str().unwrap());
}
```

## One Level Lower

At the lowest layer, one can interact with the raw data directly via `TextTape`
and `BinaryTape`.

```rust
use jomini::{TextTape, TextToken, Scalar};

let data = b"foo=bar";

assert_eq!(
    TextTape::from_slice(&data[..])?.tokens(),
    &[
        TextToken::Unquoted(Scalar::new(b"foo")),
        TextToken::Unquoted(Scalar::new(b"bar")),
    ]
);
```

If one will only use `TextTape` and `BinaryTape` then `jomini` can be compiled without default
features, resulting in a build without dependencies.

## Benchmarks

Benchmarks are ran with the following command:

```
cargo clean
cargo bench -- '/ck3'
find ./target -wholename "*/new/raw.csv" -print0 | xargs -0 xsv cat rows > assets/jomini-benchmarks.csv
```

And can be analyzed with the R script found in the assets directory.

Below is a graph generated from benchmarking on an arbitrary computer.

![jomini-bench-throughput.png](assets/jomini-bench-throughput.png)
