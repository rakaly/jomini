## v0.4.0 - 2025-07-24

- Complete overhaul of file parsing and handling

## v0.3.4 - 2023-05-14

- Update melter to exclude `ironman` key from output

## v0.3.3 - 2023-03-06

- Update melter to recognize `total`, `available`, and `locked` as numbers
- Update jomini parser to 0.21

## v0.3.2 - 2022-10-24

- Update jomini parser to 0.20

## v0.3.1 - 2022-09-12

- Allow ownership transfer of melted output

## v0.3.0 - 2022-09-12

- Bump to edition 2021

## v0.2.1 - 2022-07-08

Empty HOI4_IRONMAN_TOKENS same as unset

## v0.2.0 - 2022-07-02

New file API that makes it easier to parse, deserialize, melt, and convert to
JSON without parsing a save more than once. See [PR
#1](https://github.com/rakaly/hoi4save/pull/1) for more info

## v0.1.9 - 2022-03-20

- Bump parser dependency to latest, no changes

## v0.1.8 - 2022-02-22

- Expose token stringification customization with `_with_tokens` methods

## v0.1.7 - 2021-11-24

- Detect and melt known dates correctly

## v0.1.6 - 2021-07-04

- Fix improper melted output when a name ended with a quote

## v0.1.5 - 2021-05-28

- Melt with tabs instead of spaces
- Melted quoted values are now escaped as needed

## v0.1.4 - 2021-05-18

- Omit carriage return when writing melted output
- Preserve ironman fields in melted output with rewrite config

## v0.1.3 - 2021-04-26

More accurate melter for 64bit floating point values

## v0.1.2 - 2021-03-14

Bump internal parser to latest

## v0.1.1 - 2021-02-01

More accurate melter that leaves certain field values unquoted

## v0.1.0 - 2021-02-01

Initial commit with basic extraction and melting capabilities