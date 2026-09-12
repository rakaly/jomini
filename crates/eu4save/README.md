![ci](https://github.com/rakaly/eu4save/workflows/ci/badge.svg) [![](https://docs.rs/eu4save/badge.svg)](https://docs.rs/eu4save) [![Version](https://img.shields.io/crates/v/eu4save.svg?style=flat-square)](https://crates.io/crates/eu4save)

# EU4 Save

EU4 Save is a library to ergonomically work with EU4 saves (ironman + mp).

```rust
use eu4save::{Eu4File, Encoding, CountryTag, SegmentedResolver};
let data = std::fs::read("assets/saves/eng.txt.compressed.eu4")?;
let file = Eu4File::from_slice(&data)?;
let save = file.parse_save(&SegmentedResolver)?;
assert_eq!(file.encoding(), Encoding::TextZip);
assert_eq!(save.meta.player, "ENG".parse()?);
```

## Querying

Even once decoded, the data might be too low level. For example a country can
have an income ledger that looks like:

```ignore
income = { 1.000 0 2.000 0.000 1.500 }
```

While the structure will decoded successfully into a vector, the context of
what each index means is missing. What value represents the income from
trade?

To help solve questions like these, the `Query` API was created

```rust
use eu4save::{Eu4Extractor, Encoding, CountryTag, query::Query};

let data = std::fs::read("assets/saves/eng.txt.compressed.eu4")?;
let file = Eu4File::from_slice(&data)?;
let save = file.parse_save(&EnvTokens)?;
let save_query = Query::from_save(save);
let trade = save_query.country(&"ENG".parse()?)
    .map(|country| save_query.country_income_breakdown(country))
    .map(|income| income.trade);
assert_eq!(Some(17.982), trade);
```

## Ironman

Ironman saves are supported, but not by default, as the token resolver can't be distributed, per PDS counsel.

You may look to other projects EU4 ironman projects like ironmelt or paperman
for inspiration.
