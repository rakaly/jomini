## v0.4.3 - 2022-10-24

- Add `birth` as a known date token
- Update jomini parser to 0.20
- Update libdeflater to 0.11

## v0.4.2 - 2022-09-18

- Fix 1.7 saves using old encoding format

## v0.4.1 - 2022-09-12

- Allow ownership transfer of melted output

## v0.4.0 - 2022-09-12

- Bump to edition 2021
- Add more efficient miniz and libdeflate inflation

## v0.3.1 - 2022-07-08

- Empty CK3_IRONMAN_TOKENS is same as unset

## v0.3.0 - 2022-07-02

- Saves can be converted into JSON
- One can deserialize and melt a save without parsing the save again
- Saves can be deserialized into arbitrary structures
- Using binary tokens from `CK3_IRONMAN_TOKENS` is no longer implicit
  and must use `EnvTokens`
- The inflated size of a save is exposed instead of hard capping it at
  200 MB
- Parsing via a `Read + Seek` (and an anonymous memory mapped file as
  storage for inflated contents) has been dropped in favor of parsing
  directly from a byte slice.
- Uncompressed text saves now supported

## v0.2.15 - 2022-06-01

- Support CK3 1.6 saves

## v0.2.14 - 2022-04-29

- Update zip dependency to latest

## v0.2.13 - 2022-03-20

- Bump parser dependency to latest, no changes

## v0.2.12 - 2022-02-22

- Support CK3 1.5 saves (new binary floating point format)
- Expose token stringification customization with `_with_tokens` methods

## v0.2.11 - 2021-07-04

- Fix improper melted output when a name ended with a quote

## v0.2.10 - 2021-05-29

- Fix large file size of melted output caused by an increasingly large indent from not properly accounting hidden objects

## v0.2.9 - 2021-05-28

- Melt with tabs instead of spaces
- Melted quoted values are now escaped as needed

## v0.2.8 - 2021-05-18

- When melting, rewrite save header line with new metadata size
- Omit carriage return when writing melted output
- Allow preservation of ironman fields in melted output with rewrite config

## v0.2.7 - 2021-05-03

- Additional bugfixes to the melted output preventing CK3 from loading the game 

## v0.2.6 - 2021-05-03

Significant effort was put into improving the melted output, such that
CK3 should recognize and continue playing the melted output

- Fix melted output containing quotes when plaintext has no quotes
- Rewrite save header to declare the melted output is uncompressed plaintext
- Increase accuracy of decoding 64 bit floats (alternative format) in ironman format
- Write numbers as integers when ignoring the fractional component would not result in a loss of accuracy
- Identified additional tokens that use the alternative float format

## v0.2.5 - 2021-04-29

- Update to latest parser API

## v0.2.4 - 2021-04-26

- Fix additional 64bit floating point values that are melted incorrectly

## v0.2.3 - 2021-04-25

- Fix incorrect melted gold value
- Bump internal parser to latest

## v0.2.2 - 2021-03-14

- Bump internal parser to latest

## v0.2.1 - 2021-02-05

- Melter will only quote values that are quoted in plaintext

## v0.2.0 - 2021-01-25

* Fixed seed properties being detected and melted as dates instead of numbers
* *Breaking*: Melter will return a set of unknown tokens (when melting does not fail)

## v0.1.4 - 2020-10-29

* Update internal parser for performance improvements

## v0.1.3 - 2020-10-02

* Fix botched release

## v0.1.2 - 2020-10-02

* Update parser dependency to 0.7
* Able to losslessly melt `levels = { 10 0=1 1=2 }`

## v0.1.1 - 2020-09-12

Update internal parser to latest which brings proper UTF-8 deserialization, performance improvements, and robustness against malicious input

## v0.1.0 - 2020-09-07

Initial commit with basic extraction and melting capabilities
