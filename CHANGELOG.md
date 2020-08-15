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
