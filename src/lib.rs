/*!

A low level, performance orientated parser for
[EU4](https://en.wikipedia.org/wiki/Europa_Universalis_IV) save files and other
[PDS](https://www.paradoxplaza.com/) developed titles.

Jomini is the cornerstone of the [Rakaly](https://rakaly.com/eu4), an EU4 achievement leaderboard
and save file analyzer. This library is also used in the [Paradox Game Converters
project](https://github.com/ParadoxGameConverters/EU4toVic2) to parse ironman EU4 saves.

## Features

- ✔ Versatile: Handle both plaintext and binary encoded data
- ✔ Fast: Parse data at 1 GB/s
- ✔ Small: Compile with zero dependencies
- ✔ Safe: Extensively fuzzed against potential malicious input
- ✔ Ergonomic: Use [serde](https://serde.rs/derive.html)-like macros to have parsing logic automatically implemented
- ✔ Embeddable: Cross platform native apps, statically compiled services, or in the browser via [WASM](https://webassembly.org/)
- ✔ Agnostic: Parse EU4, HOIV, Imperator, etc save files

## Quick Start

Below is a demonstration on parsing plaintext data using jomini tools.

```rust
# #[cfg(feature = "derive")] {
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

let actual: Model = TextDeserializer::from_slice(data).unwrap();
assert_eq!(actual, expected);
# }
```

## Binary Parsing

Parsing data encoded in the binary format is done in a similar fashion but with an extra step.
Tokens can be encoded into 16 integers, and so one must provide a map from these integers to their
textual representations

```rust
# #[cfg(feature = "derive")] {
use jomini::{JominiDeserialize, BinaryDeserializer};
use std::collections::HashMap;

#[derive(JominiDeserialize, PartialEq, Debug)]
struct MyStruct {
    field1: String,
}

let data = [ 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47 ];

let mut map = HashMap::new();
map.insert(0x2d82, "field1");

let actual: MyStruct = BinaryDeserializer::from_slice(&data[..], &map).unwrap();
assert_eq!(actual, MyStruct { field1: "ENG".to_string() });
# }
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

The plaintext parser is geared towards save file parsing and is not yet general enough to handle
files that embed operators other than equals.

## One Level Lower

It can be the case that either that having the data automatically deserialized into a data
structure is inappropriate. Maybe there's a requirement to accomplish everything without pulling in
additional dependencies. Whatever the case, one can opt into using `TextTape` and `BinaryTape`
directly.

```rust
use jomini::{TextTape, TextToken, Scalar};

let data = b"foo=bar";

assert_eq!(
    TextTape::from_slice(&data[..]).unwrap().token_tape,
    vec![
        TextToken::Scalar(Scalar::new(b"foo")),
        TextToken::Scalar(Scalar::new(b"bar")),
    ]
);
```

*/

pub(crate) mod ascii;
mod binary;
mod data;
mod errors;
mod scalar;
mod text;
pub(crate) mod util;

pub use self::binary::*;
pub use self::errors::*;
pub use self::scalar::{Scalar, ScalarError};
pub use self::text::*;

#[cfg(feature = "derive")]
pub use jomini_derive::*;
