## v0.8.2 - 2022-10-25

- Update jomini parser to 0.20
- Update libdeflater to 0.11

## v0.8.1 - 2022-09-12

- Allow ownership transfer of melted output

## v0.8.0 - 2022-09-12

- Update to 2021 edition
- Update to 1.34 data model
- Add default more efficient miniz inflation
- Add libdeflate inflation feature

## v0.7.1 - 2022-07-08

- Empty EU4_IRONMAN_TOKENS same as unset

## v0.7.0 - 2022-07-02

- Saves can be converted into JSON
- One can deserialize and melt a save without parsing the save again
- Saves can be deserialized into arbitrary structures
- Using binary tokens from `EU4_IRONMAN_TOKENS` is no longer implicit
  and must use `EnvTokens`
- The inflated size of a save is exposed instead of hard capping it at
  200 MB
- Parsing via a `Read + Seek` (and an anonymous memory mapped file as
  storage for inflated contents) has been dropped in favor of parsing
  directly from a byte slice.

## v0.6.2 - 2022-04-29

- Update zip dependency to latest

## v0.6.1 - 2022-03-20

- Improve accuracy of melting output
- Melted output is now compatible from loading from in game menu

## v0.6.0 - 2022-02-22

- Expose token stringification customization with `_with_tokens` methods
- Significant changes to the model structure to support latest use cases

## v0.5.3 - 2021-06-08

- Fix improper melted output when a name ended with a quote
- Include mod info in deserialization
- Track release and play as in ironman

## v0.5.2 - 2021-06-08

- Allow invalid dates to be melted except in error mode

## v0.5.1 - 2021-05-28

- Melt with tabs instead of spaces
- Melted quoted values are now escaped as needed

## v0.5.0 - 2021-05-18

- Melt output with same precision as game, so 32 bit numbers are written out with 3 decimal points and 64 bit numbers are written out with 5 decimal points
- Fix some id tokens being accidentally interpretted as dates in the melted output
- Additional fields from leviathan are deserialized
- Omit carriage return when writing melted output
- Rework melter api to match that of the other repos

## v0.4.5 - 2021-04-27

- Support melting prehistoric dates correctly

## v0.4.4 - 2021-04-27

- Additional fields deserialized
- Fix to melting large 64bit floating point values
- Leviathan support

## v0.4.3 - 2021-03-14

- Bump internal parser to latest

## v0.4.2 - 2021-02-05

- Fix to melter to keep quoted binary keys as unquoted in plaintext

## v0.4.1 - 2021-02-05

- Fix tech being corrupted on loading melted save by updating melter to better know if a value should be quoted
- Deserialize mercenary companies

## v0.4.0 - 2021-01-25

Expect significant breaking changes to query API. Additionally,

- Expose country ideas
- Expose war info
- Expose additional monarch, heir and queen fields
- Rework `CountryTag` to require a parsing step before creation
- Correctly deserialize and melt seeds
- Return unknown tokens when melting

## v0.3.3 - 2020-11-09

- `ProvinceId` implements `Ord` and `PartialOrd`
- `CountryTag` implements `Ord` and `PartialOrd`
- `ProvinceId` numeric id exposed in `as_u16`

## v0.3.2 - 2020-10-29

- Update internal parser for performance improvements

## v0.3.1 - 2020-10-13

- Deserialize saves that have no DLC enabled

## v0.3.0 - 2020-10-02

- Update internal parser to latest
- API change: `Eu4Date::eu4_fmt` -> `Eu4Date::game_fmt`
- API change: `Eu4Date::EU4_START_DATE` -> `Eu4Date::eu4_start_date()`

## v0.2.4 - 2020-09-12

- Update internal parser to latest which should bring additional performance and robustness against malicious input

## v0.2.3 - 2020-09-07

- Parsing dates from strings became 30% faster, so may have a marginal impact on save parsing performance
- Update internal parser, jomini, to latest version

## v0.2.2 - 2020-08-29

- Exclude other boolean province values from being detected as buildings in `Query::province_building_history`

## v0.2.1 - 2020-08-28

- Fix mmap feature compilation

## v0.2.0 - 2020-08-28

- Fix `Eu4Date::days_until` calculation involving September dates
- Add additional validation when constructing `Eu4Date` so that nonsensical dates can't be created
- `Eu4Date` now implements `Copy`
- Major performance improvements to parsing Eu4Dates
- Migrate memory map extraction to use anonymous region instead of temporary file
- Query engine reorganized to hide fields behind public methods to aid transparent caches
- Expose dlc function at root of docs
- Move data structs behind model module
- Include destroyed buildings in province building history

## v0.1.6 - 2020-08-24

- Include initial buildings in province building history query

## v0.1.5 - 2020-08-23

- Add province base tax, production, and manpower
- Start extracting province history
- Add province building history query to query engine

## v0.1.4 - 2020-08-20

- Fix province building deserialization for ironman saves

## v0.1.3 - 2020-08-20

- `Eu4Date::eu4_fmt` omits leading zeros in date
- Building information extraction from saves

## v0.1.2 - 2020-08-15

- Bump jomini to v0.2

## v0.1.1 - 2020-08-11

- Rerun the build when ironman token env var changes
- Encapsulate internals of Eu4Date
- Add length check when deserializing country tags

## v0.1.0 - 2020-08-10

Init commit
