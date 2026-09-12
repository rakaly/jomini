## v0.4.2 - 2022-10-24

- Update jomini parser to 0.20
- Update libdeflater to 0.11

## v0.4.1 - 2022-09-12

- Allow ownership transfer of melted output

## v0.4.0 - 2022-09-12

- Bump to edition 2021
- Add default more efficient miniz inflation
- Add libdeflate inflation feature

## v0.3.1 - 2022-07-08

- Empty IMPERATOR_TOKENS is same as unset

## v0.3.0 - 2022-07-02

- Saves can be converted into JSON
- One can deserialize and melt a save without parsing the save again
- Saves can be deserialized into arbitrary structures
- Using binary tokens from `IMPERATOR_TOKENS` is no longer implicit
  and must use `EnvTokens`
- The inflated size of a save is exposed instead of hard capping it at
  200 MB
- Parsing via a `Read + Seek` (and an anonymous memory mapped file as
  storage for inflated contents) has been dropped in favor of parsing
  directly from a byte slice.

## v0.2.10 - 2022-04-29

- Update zip dependency to latest

## v0.2.9 - 2022-03-20

- Bump parser dependency to latest, no changes

## v0.2.8 - 2022-02-22

- Expose token stringification customization with `_with_tokens` methods

## v0.2.7 - 2021-07-04

- Fix improper melted output when a name ended with a quote

## v0.2.6 - 2021-05-28

- Melt with tabs instead of spaces
- Melted quoted values are now escaped as needed

## v0.2.5 - 2021-05-18

- Omit carriage return when writing melted output
- Preserve ironman fields in melted output with rewrite config

## v0.2.4 - 2021-04-29

- Update to latest parser API

## v0.2.3 - 2021-04-26

- More accurate melter for 64bit floating point values

## v0.2.2 - 2021-03-14

- Bump internal parser to latest

## v0.2.1 - 2021-02-05

- Melter will only quote values that are quoted in plaintext

## v0.2.0 - 2021-01-25

- Return unknown tokens when melting

## v0.1.2 - 2020-10-29

* Update internal parser for performance improvements

## v0.1.1 - 2020-10-02

* Update internal parser dependency to 0.7

## v0.1.0 - 2020-09-16

Initial commit with basic extraction and melting capabilities
