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
