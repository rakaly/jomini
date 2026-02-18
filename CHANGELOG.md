## v0.34.1 - 2025-02-17

- Fix: panic on parsing text version triplets as f64
- Fix: remove header from envelope body when parsed from a slice

## v0.34.0 - 2025-12-09

Lookup index tokens introduced in EU5 1.0.8 can exceed 16 bits, as a save that used a 24 bit index was discovered. To accomodate larger than 16 bit lookups, `TokenResolver::lookup` needs to be updated to accept `u32` instead of `u16`.

With variable sized lookups (much like varible size f64 introduced in EU5 1.0.10), the lookup tokens have been consolidated to a single token type: `Token::Lookup` instead of having different types depending on the length of the field.

## v0.33.1 - 2025-12-06

Fix compact binary fixed point storage to better match the full-width version

## v0.33.0 - 2025-12-05

EU5 1.0.10 introduced several new binary lexemes:

- 15 for a more compact f64 representation. Instead of always using 8 bytes for f64 data, there are now representations for 0 and 1-7 bytes of data for unsigned and signed representations
- 2 that are altneratives to the 8 and 16 bit string index lookups.

Since there are no new semantics associated with change, `binary::Token` remains unchanged. 

## v0.32.0 - 2025-11-28

- **Breaking Change**: New binary token kinds: `LookupU8` and `LookupU16` for 8 bit and 16 bit lookup values introduced in EU5 1.0.8. There are no reported collisions within other games: EU4, CK3, Imperator, Vic3, and HOI4.
- Add `TokenResolver::lookup` to resolve these new lookup values to strings during deserialization. By default, the method will return `None`.
- Add `JominiZip::read_entry` to retrieve files other other than those related to gamestate.
- Update `SaveHeader::version` to be exposed so one can detect a `SAV02` save vs a `SAV01` save.
- Fix incorrect extraction of `SAV01` metadata (introduced with `SAV02` support)

## v0.31.2 - 2025-11-24

- Add support for parsing EU5 1.0.8 save headers via support for long format save headers

## v0.31.1 - 2025-11-22

- Update `JominiFile::from_slice` to accept any argument that has an impl `AsRef<[u8]>` so it can support owning the data too (like passing in a `Vec<u8>`)  

## v0.31.0 - 2025-11-19

- Improve performance of `binary::TokenReader::next` and `read` by 3x on real saves with fast-path optimizations.
- Add a new set of lower level APIs that decouple parsing from data extraction. `TokenKind` is a new, dataless enum informing the caller of the type of token. To extract the data related to this `TokenKind`, use one of the `*_data` methods.

## v0.30.1 - 2025-11-16

- Fix envelope underflow on invalid zip  

## v0.30.0 - 2025-11-16

Add the `envelope` feature which allows parsing the containing format of metadata and game state from modern Paradox games (EU5, CK3, Vic3, and Imperator), so that each game implementation doesn't need to reinvent the parsing logic.

## v0.29.0 - 2025-10-01

### Breaking Changes

- Remove `BinaryTape` and `BinaryToken`: These structs have been removed as they became unnecessary after the introduction of incremental parsing via `TokenReader` and `Token` (#182)
- Omit rgb header when deserializing binary colors: Binary color deserialization now directly emits 3 color elements instead of requiring the "rgb" header, simplifying save file deserialization (#198)
- Separate serde deserializers from derive macro: The serde deserializer implementations have been separated from the derive macro functionality
- Update to Rust edition 2024: The project now uses Rust edition 2024

### Updates

- Object template syntax support: Add parsing and JSON support for object template syntax like `obj={ { a = b }={ 1 2 3 } }` (#189, #190, #191, #192)
- Tape mutation capability: Add initial (experimental) API for mutating the tape (#193)
- Zero-copy deserialization: Implement `#[jomini(borrow)]` field attribute for zero-copy deserialization using borrowed types like `Cow<str>` (#186)
- Floating point 'f' suffix: Support optional 'f' suffix for floating point literals (e.g., `WIDTH = 5.0f`) (#193)
- Raw byte binary deserialization: Add binary deserialization support that bypasses encoding when `deserialize_bytes` is requiested
- Container header deserialization: Add support for container header deserialization in text reader
- Object reader creation: Allow creation of object reader from token list and encoding for interpolation support (#196)
- Descriptive derive macro errors: JominiDeserialize derive macro now shows descriptive errors instead of panicking (#176)
- SeqAccess::size_hint: Implement `size_hint` for `ColorSequence`
- Semicolon handling: Treat semicolons as equivalent to whitespace (#190)
- JSON representation: Collapse equals operator in JSON representation from `{"foo":{"EQUALS":10}}` to `{"foo": 10}` (#193)
- Key-operator-value JSON serialization: Serialize key-operator-value triplets as objects in JSON
- Number parsing performance: Consolidate overflow checks at end of f64/u64 parsing for 1.33x-1.67x speedup (#177)
- Exists and not-equal operators: JSON: Fix recognition of exists and not-equal operators when they appear as the first operator in an object (#194)

## v0.28.0 - 2025-03-09

- Update to syn2
- `#[jomini(deserialize_with = "")]` now support const generics
- Bump minimum dependencies to those released within the last year
- Bump minimum Rust version to 1.77

## v0.27.3 - 2025-02-08

- Allow calling `ValueReader::read_object` on a `TextToken::MixedContainer` value
- Improve performance of `binary::TokenReader::skip_container`

## v0.27.2 - 2024-11-19

- Fix incorrect v0.27.1 release

## v0.27.1 - 2024-11-19

- Add support for `#[duplicated]` smallvec deserialization:
  ```rust
  #[derive(JominiDeserialize)]
  pub struct Model {
      #[jomini(duplicated)]
      lst: smallvec::SmallVec<[u8; 4]>,
  }
  ```

## v0.27.0 - 2024-09-13

- Add `BasicTokenResolver`, a `TokenResolver` that facilitates token ingestion from space delimited text lines. It's intended to be ergonomic enough to be used in tests without the prohibited compile cost of generating a 10 thousand line match statement.

## v0.26.0 - 2024-06-16

- Add optional `TokenResolver::is_empty` implementation for resolvers to signal no token support.
- Add more ergonomic streaming write support with `TextWriter::write_start` when it is unknown if the block being written belongs to an array or object.

## v0.25.6 - 2024-04-09

- Fix trailing empty object causing the binary ondemand deserializer to raise an error

## v0.25.5 - 2024-02-27

Support generic type parameter for `JominiDeserialize`:

```rust
pub struct Manager<Of> {
    value: Of,
}
```
and
```rust
pub struct Manager<Of> where Of: DeserializeOwned {
    #[jomini(deserialize_with = "maybe_option")]
    value: Option<Of>,
    #[jomini(deserialize_with = "maybe_option")]
    value2: Option<Of>,
}

fn maybe_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>
{
    todo!()
}
```
Lots of edge cases still exist, but this should unlock downstream usages

## v0.25.4 - 2024-02-24

- Fix incorrect lexer skip of rgb data for binary slices
- Small optimizations to binary and text readers

## v0.25.3 - 2024-01-11

- Performance improvement (~20%) to `text::TokenReader` for save files

## v0.25.2 - 2023-12-28

- Fix remaining soundness issues identified by MIRI
- Add `binary::Token::write` for outputting binary data
- Remove the short lived, internal `SliceReader` type

## v0.25.1 - 2023-12-21

- Implement reader error translation logic

## v0.25.0 - 2023-12-21

- Fix panic for multiple text deserialization value calls
- Add `faster_writer` feature (based on itoa, enabled by default) for a significantly faster writer for numbers
- Add deserialize implementations over `Read` instances
- Add parsing implementations over `Read` instances
- Add deserialization methods on `BinaryFlavor` instances
- Change default binary byte slice deserialization algorithm to be ondemand instead of using the binary tape
- Text decoding algorithms optimized for shorter text

## v0.24.0 - 2023-11-21

- Fix incorrect date parsing for dates from years [-100, -999]
- Fix `#[jomini(take_last)]` compilation error for last field in struct
- Add support for optional alpha component in binary RGB values
- Add `write_rgb` to `TextWriter`
- Add text deserialization hint support
- Add direct struct field matching from binary token with `#[jomini(token = <num>)]`
- Improve performance of date parsing from 15-40%

## v0.23.0 - 2023-10-14

- Add new binary i64 data type introduced in HOI4 1.13
- Add TextWriter::write_binary and depth

## v0.22.2 - 2023-09-14

- Add `ObjectReader::deserialize` to deserialize from an arbitrary point in a text document
- Add `TextDeserializer::from_reader` to deserialize from an `ObjectReader`

## v0.22.1 - 2023-06-10

- Fix eagerly erroring when data does not follow deserialization hints for maps and sequences

## v0.22.0 - 2023-06-07

- Add ondemand binary deserializer for efficient one-shot parsing deserialization
- Deserialization error types updated from `DeserializeError` to `jomini::Error`

## v0.21.3 - 2023-05-25

- Add json type narrowing configuration
  
  When converting to JSON, values are eagerly narrowed from strings to numbers or booleans. This is now configurable: 
  - All (default). Current behavior of type narrowing all values
  - Unquoted. Only type narrow values that aren't quoted
  - None. Never type narrow

## v0.21.2 - 2023-04-03

- Improve enum deserialization for simple scalar values.

    ```
    type=general
    ```

    Can now be deserialized with:

    ```rust
    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "camelCase")]
    enum MyType {
      General,
      Admiral,
    }
    ```

## v0.21.1 - 2023-03-08

- Improve text date parsing by up to 30x

## v0.21.0 - 2023-01-02

Implement serde `Deserializer` trait on top level deserializers

There are serde crates that can wrap deserializers to enhance functionality. For instance, [`serde_path_to_err`][0.21.0-0], wraps a deserializer and will return the path to the field that caused a deserialization error. This example is especially important when dealing with imperfect information about the data being deserialized.

Last week, it took me a half hour to track down where and why an EU4 save failed to deserialize without any context. I couldn't use `serde_path_to_error` to help as the underlying `TextDeserializer` and `BinaryDeserializer` here don't implement [`serde::Deserializer`][0.21.0-1], which seems like an oversight or a misnomer.

This release implements `serde::Deserializer` for the aforementioned types, and allows the following example, which will show the path to the error.

```rust
#[derive(Debug, Deserialize)]
struct Package {
  name: String,
  dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Deserialize)]
struct Dependency {
  version: u32,
}

let data = b"name=demo dependencies={serde={version=alpha}}";
let jd = jomini::TextDeserializer::from_windows1252_slice(&data[..]).unwrap();
let result: Result<Package, _> = serde_path_to_error::deserialize(&jd);
let err = result.unwrap_err();
assert_eq!(err.path().to_string(), "dependencies.serde.version");
```

Since this is a low level crate, all deserializer wrappers will be opt-in, as wrappers may have some cost associated with them. My plan is for downstream save crates to re-deserialize data with `serde_path_to_err` if the first attempt fails.

This is a breaking change. The shorthand methods for parsing and deserializing text in one step is now:

```
jomini::text::de::from_windows1252_slice()
jomini::text::de::from_utf8_slice()
```

While the methods on `TextDeserializer` now return a `TextDeserializer`.

Same thing with `BinaryDeserializer` except `deserialize_slice` is the new method for parsing and deserializing in one step.

[0.21.0-0]: https://github.com/dtolnay/path-to-error
[0.21.0-1]: https://docs.rs/serde/latest/serde/trait.Deserializer.html

## v0.20.3 - 2022-12-24

- Up to a 75% throughput increase in binary parsing performance
- Disallow binary RGB values outside of object values

## v0.20.2 - 2022-11-05

- Include serde derive feature when the `derive` feature is enabled. This fixes compilation issues for users that used jomini deserialization but didn't reference `serde/derive` in their Cargo.toml.

## v0.20.1 - 2022-11-02

- Add `Property<T>` for plaintext deserialization that will capture field operators
- Expose `PdsDateFormatter` constructor for external date wrappers
- Expose `RawDate::from_binary`
- Up to 20% thoughput increase in binary parsing

## v0.20.0 - 2022-10-23

Esoteric text syntax where a container is both an array and an object is supported:

```
on_actions = {
  faith_holy_order_land_acquisition_pulse
  delay = { days = { 5 10 }}
  faith_heresy_events_pulse
  delay = { days = { 15 20 }}
  faith_fervor_events_pulse
}
```

The above can now be parsed. When converted to JSON it will be translated into:

```json
{
  "on_actions": [
    "faith_holy_order_land_acquisition_pulse",
    { "delay": { "days": [5, 10] } },
    "faith_heresy_events_pulse",
    { "delay": { "days": [15, 20] } },
    "faith_fervor_events_pulse"
  ]
}
```

This required a rework of the low level parsers, both binary and text -- but the high level deserialization should remain the same. The mid-level API also remains mostly the same outside of the `trailer` property getting renamed to `remaining`

## v0.19.1 - 2022-07-02

- Advise against using the `flatten` serde attribute in documentation
- Fix stack overflow in case someone uses `flatten` on a struct with input that
  contains a header (eg: `color = rgb { 10 10 10 }`)

## v0.19.0 - 2022-06-28

First some performance news, text decoding throughput has increased by up to
50%. This is benificial to those who are deserializing lots of strings.

A new feature flag has been added: `json`. When the json feature flag is
enabled, the mid level text readers gains a new `json()` function that will
assist in the conversion to json. There's several knobs to adjust, including how
to handle duplicate keys and if to generate pretty printed json. See [PR
#95](https://github.com/rakaly/jomini/pull/95) for more info.

The mid level text reader API sees a major overhaul. Iteration of containers
(arrays and objects) are now decoupled from reading. See [PR
#92](https://github.com/rakaly/jomini/pull/92) for more info.

The last breaking change is the organization of modules. Instead of clogging up
the root namespace, `jomini::{binary,text}` now house secondary and
complementary APIs, so this may require updating some imports.

## v0.18.1 - 2022-04-15

- Fix x86 heap buffer overflow on text parsing

## v0.18.0 - 2022-03-19

**Breaking change for only users of the write API**

- `TextWriter::write_quoted` doesn't redirect to an unquoted write if a key is expected
- Omit terminating newline from written output
- Remove `WriteVisitor` in favor of `write_fxx_precision` functions (or write directly to the stream with a custom Rust format)

## v0.17.0 - 2022-02-19

**Breaking change for only users of the binary API**

Instead of:

```rust
BinaryDeserializer::from_eu4(/* ... */)
```

write

```rust
BinaryDeserializer::builder_flavor(InsertFlavorHere)
    .from_slice(/* ... */);
```

Instead of:

```rust
BinaryTape::from_eu4(/* ... */)
```

write:

```rust
BinaryTape::from_slice(/* ... */)
```

These changes are necessary as a binary flavor may change between game patches, so this avoids
needing to reparse the binary data when the flavor is unknown.

## v0.16.4 - 2021-09-07

- Added `#[jomini(take_last)]` attribute to use the last instance when duplicate fields are encountered
- Improved binary parsing throughput by up to 15%
- Date classes (excluding `RawDate`) can now parse dates from textual numbers (for when other, 3rd party melters mistakenly encode a date as a number).

## v0.16.3 - 2021-07-25

- Support deserializing dates with a zero year
- Support CK2 exact operator (==)
- Support CK3 not equal operator (!=)
- Support skipping over semicolons when parsing text
- Support parsing text with UTF-8 BOM
- Support parsing files with missing brackets
- Small performance improvement to text parsing
- Improve support for parsing operators

## v0.16.2 - 2021-07-10

- Fix binary parsing where an object within several arrays would be decoded as an hidden object instead of a regular object

## v0.16.1 - 2021-07-10

- Add support for decoding numbers with leading plus sign

## v0.16.0 - 2021-07-04

- (Breaking change): `Scalar` and date methods now take self instead of self reference (no downstream code is expected to change because of this).
- (Breaking change): Rename `Scalar::view_data` to `Scalar::as_bytes`
- Fix missing escape character when writing out escaped text and the input ends with a character that needs to be escaped

## v0.15.1 - 2021-06-13

Fix overflow panic on new `DateHour::from_binary` methods when negative hours occur

## v0.15.0 - 2021-06-13

Dates have been overhauled to support HOI4 and Stellaris and to match chrono's API. [See this PR for more info](https://github.com/rakaly/jomini/pull/74)

## v0.14.2 - 2021-06-08

- Support parsing binary dates prior to 5000 BC

## v0.14.1 - 2021-05-29

- Introduce `TextWriter::write_hidden_object_start` to fix hidden objects increasing the indent
- Fix empty array values lacking proper indent when written

## v0.14.0 - 2021-05-27

- Adds an API to output text documents. [See documentation](https://github.com/rakaly/jomini/blob/409c1ac00464e507deb4fc9a3b9912516e77478b/README.md#write-api)

## v0.13.2 - 2021-05-03

In 1.31.2 EU4 saves there is syntax like:

```plain
history={
  capital=945 {}
  1444.11.11={
    # ..
  }
}
```

Previously the `945` would be parsed as the header to an empty array
(reminder that headers are those like `rgb`, `hsv`, `hsv360`, `LIST`).

This release recognizes it as an empty object and skips it.

This change leads to a 10-20% throughput increase in text parsing.

There's a minor bugfix included in this release that increases the
decoding accuracy of 64 bit floating point values in EU4 binary data
by up to a 10,000th.

## v0.13.1 - 2021-04-27

Remove cap on negative years in dates. With the release of leviathan we now
have prehistoric monuments with large negative dates like:
```
-2500.1.1
```

Previously these dates would fail to parse in both text and binary forms as
years below -100 were considered invalid due to binary integers and dates
being encoded the same way in the binary format.

Now one can construct a date with an unbounded negative date, and a function
was introduced a function that allows binary format users to ask if the data
is more likely a date than an integer. The new function is `Date::from_binary_heuristic`

## v0.13.0 - 2021-04-26

- Support for interpolated variables like `@[1-leopard_x]`
- EU4 and CK3 binary flavors now use full 8 bits to decode 64 bit floats
- Rename `BinaryFlavor::visit_f32_1` to `BinaryFlavor::visit_f32`
- Rename `BinaryFlavor::visit_f32_2` to `BinaryFlavor::visit_f64`
- `BinaryFlavor::visit_f32` receives an array instead of slice
- `BinaryFlavor::visit_f64` receives an array instead of slice

## v0.12.1 - 2021-04-07

Fix botched v0.12.0 release

## v0.12.0 - 2021-04-07

Support EU4 parameter definition syntax. Inside EU4's scripts directory there are files that contain the
parameter definition syntax

It has a simple key value version.

```
each_estate_effect = {
        if = {
                limit = {
                        has_estate = estate_brahmins
                }
                [[effect]
                        $effect$
                ]
                [[brahmins_effect]
                        $brahmins_effect$
                ]
        }
}
```

And a more complex object version:

```
pick_random_estate_if_present = {
        random_list = {
                1 = {
                        set_country_flag = estate_brahmins_$flag$
                        [[estate_action]
                        $estate_action$ = estate_brahmins
                        ]
                }
        }
}
```

This syntax seems specific to EU4 (checked imperator, ck3, hoi4, and
vic2). The syntax was introduced in the Dharma patch with the following
notes:

> Scripted triggers or effects now support conditional compilation on
arguments provided to them. You can now check for if an argument is
defined or not and make the script look entirely different based on
that. Syntax is [[var_name] code here ] for if variable is defined or
[[!var_name] code here ] for if it is not.

The parameter key is represented as a new text token:
`TextToken::Parameter`. There's another type of parameter usage -- an
unset parameter. Since the game has no instances of this, the following
is contrived:

```
pick_random_estate_if_present = {
        random_list = {
                1 = {
                        set_country_flag = estate_brahmins_$flag$
                        [[!estate_action]
                        $estate_action$ = estate_brahmins
                        ]
                }
        }
}
```

The key is represented as an `TextToken::UndefinedParamater`.

This release also stores which type of scalar the scalar reader is
abstracting over so that downstream clients can interpret parameters
differently.

## v0.11.2 - 2021-03-27

Allow extraneous closing brace at any point. Previously extraneous closing
braces could only be at the end of the document (something seen in Vic II
saves), but there are EU4 game files where an extraneous closing brace ocurrs
in the middle (looking at you verona.txt):

```
a = { 1 }
}
b = 2
```

This commit makes the parser purposely accept invalid documents in an
effort to be as flexible as possible. Any file that is parseable by PDS
should be parseable by us.

## v0.11.1 - 2021-03-25

Fix float parsing inaccuracy for (-1, 0) numbers. Previously `Scalar::to_f64`
would parse number that fall in the range of (-1, 0) would have their sign
incorrectly parsed, so `-0.5` would be parsed as `0.5`.

## v0.11.0 - 2021-03-24

Another small, but breaking change that shouldn't effect the majority of users.

There's PDS syntax that exhibits, what I call, object trailers:

```
brittany_area = { #5
    color = { 118  99  151 }
    169 170 171 172 4384
}
```

This syntax was previously parsed into an ambiguous format where the
trailer values are interpretted as alternating key value pairs (while
allowing a key to omit a value):

Now the parser wraps the trailer in an array so that the object key is
the trailer array:

```rust
TextToken::Array(14),
TextToken::Unquoted(Scalar::new(b"169")),
TextToken::Unquoted(Scalar::new(b"170")),
TextToken::Unquoted(Scalar::new(b"171")),
TextToken::Unquoted(Scalar::new(b"172")),
TextToken::Unquoted(Scalar::new(b"4384")),
TextToken::End(8),
```

This makes the detection of an object with an array trailer trivial:
check if the last key in the object is an array. This method has been
added to the `ObjectReader` struct which will expose the reader for the
trailer when the rest of the keys have been iterated.

Deserializing sees major ergonomic improvements here. Given our sample document
above, users can now decide how they want to deserialize in a much more
ergonomic fashion:

Both:

```rust
struct MyStruct {
    brittany_area: Vec<u16>,
}
```

and

```rust
struct MyStruct {
    brittany_area: MyArea,
}

struct MyArea {
    color: Option<Vec<u8>>,
    trailer: Vec<u16>,
}
```

Work out of the box with no need to write custom deserialization logic.

With all the code added, there was no performance hit as either the
conditional will almost always be static, making it friendly to the
branch predictor, or code was added to an already cold path.

This is a breaking change due to how the tape structure will change
and deserialization logic that previously handled object trailers
will need to be updated

## v0.10.1 - 2021-03-14

Bugfix for inputs like:

```
T&}
```

Which would be parsed successfully as a tape that contains only a key but would subsequently panic if the reader was iterated, as objects assume that every key has a value.

Now the above input will fail to parse.

Also squashed several edge cases that could arise when using a text object reader's `next_fields` method

## v0.10.0 - 2021-03-08

A small change, but an API change nontheless.

`ScalarError::PrecisionLoss(f64)` has been introduced for `Scalar::to_f64` which will be raised whenever the integer that is parsed can't be represented as a 64 bit float without precision loss. If precision loss is desirable, the old behavior can be restored by inspecting the scalar error for `PrecisionLoss` and returning the inner 64 bit float.

## v0.9.1 - 2021-02-18

Add Vic2 save file compatibility by allowing text documents with an extraneous closing brace to still be parsed.

## v0.9.0 - 2021-02-05

This release differentiates between quoted and unquoted values as some games (eg: EU4) may parse values differently depending if they are enclosed in quotes.

This means that binary and text tape tokens for now include a quoted and unquoted variant.

So the below

```rust
TextToken::Scalar
```

is replaced with with

```rust
TextToken::Quoted
TextToken::Unquoted
```

And the binary equivalent is

```rust
BinaryToken::Text
```

replaced with

```rust
BinaryToken::Quoted
BinaryToken::Unquoted
```

## v0.8.1 - 2020-12-06

With the newly found knowledge that EU4 savefiles contain dates that are negative, eg:

```
birth_date=-17.1.1
```

The `Date` struct needed to be reworked. At the API level `Date::year()` now returns an `i16` instead of `u16` as now the year can be negative. While technically a breaking change, this is treated more a necessary bugfix. The current max negative year is -100.

This patch release also clamps down on unexpected behavior. Previously

```
1444.257.1
```

would be parsed as equivalent to

```
1444.1.1
```

Now the former example will fail to be parsed as a date. This same fix has been applied to binary dates.

## v0.8.0 - 2020-10-29

First up, this release brings some performance benefits when deserializing. Optimizations went into ensuring that smaller numbers faster to decode and pre-allocating containers like vectors or hashmaps with the known size.

By far the biggest change is the introduction of mid-level API for the text format. Working with the low level tape -- well, it can be too low level. And while the
high level serde bindings are nice, but sometimes it not a good fit. This version
introduces a mid level API that bridges the two extremes.

The best thing about this mid-level API is that it reuses the low level tape
and the high level serde api can be implemented on top of the mid-level without
a performance penalty.

For now only the text format has this intermediate layer as the benefits are
more immediate. The binary format is a bit simpler and adding this intermediate
layer would seem to only complicate things.

There are a couple more benefits that the mid level API helped with:

Enums are deserializable.

```
color1 = rgb { 10 20 30 }
color2 = hsv { 0.3 0.2 0.8 }
color3 = hsv360 { 25 75 63 }
```

Can be deserialized to:

```rust
enum MyColor {
    #[serde(rename = "rgb")]
     Rgb(u8, u8, u8),
    #[serde(rename = "hsv")]
     Hsv(f32, f32, f32),
    #[serde(rename = "hsv360")]
     Hsv360(u8, u8, u8)
}
```

Also values that are both an array and object can be deserialized too:

```
brittany_area = { #5
    color = { 118  99  151 }
    169 170 171 172 4384
}
```

Just make sure the serde deserializer is written to understand the individual values passed to it.

Some sample usage:

```rust
let data = b"name=aaa name=bbb core=123 name=ccc name=ddd";
let tape = TextTape::from_slice(data).unwrap();
let mut reader = tape.windows1252_reader();

while let Some((key, _op, value)) = reader.next_field() {
    println!("{:?}={:?}", key.read_str(), value.read_str().unwrap());
}
```

This release needed a minor bump as the text deserializer now needs an encoding that implements `Clone` (if this is a problem, raise an issue). Also the `Token` types now implement `Clone`

## v0.7.2 - 2020-10-12

This release is all about performance:

- Increase tape pre-allocation by 33% for both binary and text parsers. This makes it so that underlying vector doesn't need to grow.
- Simplify text parser character classifications for 10% throughput improvement
- On x86 platforms, use SIMD instructions greatly speedup parsing scalars (quoted and non-quoted). Up to 60% improvement in throughput.


## v0.7.1 - 2020-10-06

The following data can now be parsed both in text and binary form.

```
history = {
  {}
  1689.10.2={
    decision="abc123"
  }
}
```

This format can be seen in some EU4 saves. What causes this is unknown but now doesn't cause the parser to fail.

## v0.7.0 - 2020-10-02

* Add `jomini::common::Date` structure for representing a game date -- a date
  that doesn't factor in leap years. This date structure had been copied
  between eu4, ck3, and imperator implementations and has now been consolidated
  here.
* Adds supports for parsing non-equal operators from text data to parse a wider
  range of game files. This is done by pushing a `TextToken::Operator` into the
  tape. Note that this operator will only appear if an operator is present and
  is non-equal.
* Expose hidden objects as `BinaryToken::HiddenObject` and
  `TextToken::HiddenObject`. They behave exactly like regular objects except
  that they denote the object is hidden (eg: `a = { 10 0=1 1=2 }`).
* Add support for generic token headers (eg: `color = hsv { 0.58 1.00 0.72 }`)
  via the `TextToken::Header("hsv")` token. This means that `TextToken::Rgb`
  has been removed in favor of `TextToken::Header("rgb")` followed by an array
  of 3 elements. `BinaryToken::Rgb` is still present.

## v0.6.0 - 2020-09-11

The performance release. Parsing binary throughput increased 30-80% depending on the workload. The binary parser can now consistently reach up to 1.2 GB/s parsing. The text parsers also saw marked improvement from v0.5, on the order of 20-30% increase in throughput.

This is a technically breaking change as the value inside `TextToken::Rgb` and `BinaryToken::Rgb` is no longer boxed.

## v0.5.0 - 2020-09-10

The big change here is that deserializers need to know what encoding strings are in: UTF-8 or Windows-1252. Previously the deserializer assumed strings were encoded as Windows-1252. This falls down when UTF-8 input is encountered like in CK3 saves. So now functions like `TextDeserializer::from_slice` have been split into two with the text api surface looking like:

- `TextDeserializer::from_utf8_slice`
- `TextDeserializer::from_windows1252_slice`
- `TextDeserializer::from_utf8_tape`
- `TextDeserializer::from_windows1252_tape`
- `TextDeserializer::from_encoded_tape`

The binary deserializer has undergone more intrusive changes. Now a `BinaryFlavor` embeds the string encoding. And since the `TextDeserializer` has been split such that there isn't a default parsing method, so too has the `DefaultFlavor` been renamed to `Eu4Flavor`.

Now there are a few main entry points for binary deserialization:

- `BinaryDeserializer::from_eu4`
- `BinaryDeserializer::from_ck3`
- `BinaryDeserializer::eu4_builder`
- `BinaryDeserializer::ck3_builder`
- `BinaryDeserializer::builder_flavor`

While these are breaking changes, hopefully the correct path forward is clear. Since I'm unsure how ubiquitous each binary format is across multiple games, I've named the format after the game titles. Each format may apply to multiple titles. Do note that other titles like imperator and HOI4 can be parsed with any flavored of binary parser but their floating point and string encoding will need to be double checked to ensure accuracy. 

The good news here is that I've incorporated `Ck3Flavor` directly instead of pushing that to `ck3save` crate.

While UTF-8 and Windows-1252 are the only encodings that are explicitly supported, one can derive their own encoding format via the `Encoding` trait. The `Encoding` trait only affects the deserialization, the parser still expects the input to be a subset of ascii and self synchronizing (I think I'm using that term correct, basically quotes `0x22` can't be part of a larger character point).

Having a user supplied `Encoding` allows for additional use case like deserializing all strings as upper case or performing ascii transliteration (eg: `ÿ` => `y`)

Other changes:

 - Removed `Scalar::to_utf8` as a scalar does not know the encoding
 - `ScalarError` no longer owns the faulty string as the encoding is not known when performing scalar functions
 - Made the tape parsers more robust against malformed inputs

## v0.4.2 - 2020-09-07

Disallow container values in hidden objects. For example:

```
levels={10 0=1 2={3 4}}
```

The above doesn't occur naturally (yet?) so to keep things simple, we'll disallow it for now.

## v0.4.1 - 2020-09-07

Bugfix for parsing the hidden object in `levels={ 10 0=2 1=2 }` for the text tape. Only files
that contain hidden objects would have seen the bug

## v0.4.0 - 2020-09-06

While this update brings with it breaking changes, hopefully most are unaffected. A change was necessitated as
CK3 uses a different encoding for rational values, so now one can parse a given `BinaryFlavor` to a tape.

Other changes:

- `BinaryToken::F32` renamed to `BinaryToken::F32_1`
- `BinaryToken::Q16` renamed to `BinaryToken::F32_2`
- The text tape parser represents the hidden object in `levels={ 10 0=2 1=2 }` the same way a binary tape does

## v0.3.1 - 2020-09-02

The binary parser can now handle the equivalent text format:

```
levels={ 10 0=2 1=2 }
```

It is represented in the BinaryTape as an object like so:

```
levels={ 10 { 0=2 1=2 } }
```

This format is found in Crusader Kings 3 (CK3) saves and so now both tapes are CK3 save compatible.

## v0.3.0 - 2020-08-28

- `BinaryDeserializer` now takes `TokenResolver` by reference so that resolved tokens can outlive
   the data to be parsed allowing for more zero-copy deserialization scenarios

## v0.2.1 - 2020-08-15

- Fix releasing issue crate

## v0.2.0 - 2020-08-14

Lots of little breaking changes:

- One accesses tokens produced by the tape parsers through `tokens()` instead of fields
- Removed `clear` method from tape parsers as that it now implicitly called for every `parse` call
- encoding_rs feature was removed due to lack of perceived usefulness
- Scalar and RGB tokens are now inlined into the `tokens()` field and not in a separate vec

Features:

- Introduce BinaryDeserializer::builder to simplify customization
- TextTape supports decoding RGB values
- Decoded strings and `Scalar::to_utf8` will trim trailing whitespace and unescape escape sequences

## v0.1.2 - 2020-08-06

- Fix scalar empty strings allowed to convert to zero
- Fix not skipping a comment at the end of text input for tape parser

## v0.1.1 - 2020-08-03

- Link readme for crates.io

## v0.1.0 - 2020-08-03

- Initial release
