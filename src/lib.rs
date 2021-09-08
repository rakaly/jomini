/*!

A low level, performance oriented parser for
[EU4](https://en.wikipedia.org/wiki/Europa_Universalis_IV) save files and
other [PDS](https://www.paradoxplaza.com/) developed titles. Consult [the
write-up](https://rakaly.com/blog/a-tour-of-pds-clausewitz-syntax) for an in-depth
look at the Paradox Clausewitz format and the pitfalls that come trying to support
all variations. It's extremely difficult to write a robust and fast parser for
this format, but jomini accomplishes both tasks.

Jomini is the cornerstone of [Rakaly](https://rakaly.com/eu4), an EU4
achievement leaderboard and save file analyzer. This library is also powers
the [Paradox Game Converters](https://github.com/ParadoxGameConverters) and
[pdxu](https://github.com/crschnick/pdx_unlimiter) to parse ironman EU4, CK3,
HOI4, and Imperator saves.

## Features

- ✔ Versatile: Handle both plaintext and binary encoded data
- ✔ Fast: Parse data at 1 GB/s
- ✔ Small: Compile with zero dependencies
- ✔ Safe: Extensively fuzzed against potential malicious input
- ✔ Ergonomic: Use [serde](https://serde.rs/derive.html)-like macros to have parsing logic automatically implemented
- ✔ Embeddable: Cross platform native apps, statically compiled services, or in the browser via [WASM](https://webassembly.org/)
- ✔ Agnostic: [Parse EU4](https://github.com/rakaly/eu4save), [HOI4](https://github.com/rakaly/hoi4save.git), [Imperator](https://github.com/rakaly/imperator-save), [CK3](https://github.com/rakaly/ck3save), and Vic2 save and game files

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

let actual: Model = TextDeserializer::from_windows1252_slice(data)?;
assert_eq!(actual, expected);
# }
# Ok::<(), Box<dyn std::error::Error>>(())
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

let actual: MyStruct = BinaryDeserializer::from_eu4(&data[..], &map)?;
assert_eq!(actual, MyStruct { field1: "ENG".to_string() });
# }
# Ok::<(), Box<dyn std::error::Error>>(())
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
# Ok::<(), Box<dyn std::error::Error>>(())
```

If one will only use `TextTape` and `BinaryTape` then `jomini` can be compiled without default
features, resulting in a build without dependencies.

## Write API

There are two targeted use cases for the write API. One is when a text tape is on hand.
This is useful when one needs to reformat a document (note that comments are not
preserved):

```rust
use jomini::{TextTape, TextWriterBuilder};

let tape = TextTape::from_slice(b"hello   = world")?;
let mut out: Vec<u8> = Vec::new();
let mut writer = TextWriterBuilder::new().from_writer(&mut out);
writer.write_tape(&tape)?;
assert_eq!(&out, b"hello=world\n");
# Ok::<(), Box<dyn std::error::Error>>(())
```

The writer normalizes any formatting issues. The writer is not able to
losslessly write all parsed documents, but these are limited to truly
esoteric situations and hope to be resolved in future releases.

The other use case is geared more towards incremental writing that can be
found in melters or those crafting documents by hand. These use cases need to
manually drive the writer:

```rust
use jomini::TextWriterBuilder;
let mut out: Vec<u8> = Vec::new();
let mut writer = TextWriterBuilder::new().from_writer(&mut out);
writer.write_unquoted(b"hello")?;
writer.write_unquoted(b"world")?;
writer.write_unquoted(b"foo")?;
writer.write_unquoted(b"bar")?;
assert_eq!(&out, b"hello=world\nfoo=bar\n");
# Ok::<(), Box<dyn std::error::Error>>(())
```
*/
#![warn(missing_docs)]
mod binary;
pub mod common;
mod copyless;
mod data;
#[cfg(feature = "derive")]
pub(crate) mod de;
mod encoding;
mod errors;
mod scalar;
mod text;
pub(crate) mod util;

pub use self::binary::*;
pub use self::data::Rgb;
pub use self::encoding::*;
pub use self::errors::*;
pub use self::scalar::{Scalar, ScalarError};
pub use self::text::*;

#[cfg(feature = "derive")]
pub use jomini_derive::*;
