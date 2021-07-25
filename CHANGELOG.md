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
