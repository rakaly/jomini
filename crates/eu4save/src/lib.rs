/*!
EU4 Save is a library to ergonomically work with EU4 saves (ironman + mp).

```rust
use std::collections::HashMap;
use eu4save::{Eu4File, Encoding, CountryTag, SegmentedResolver};

# let mut data = Vec::new();
# std::io::Read::read_to_end(&mut pdx_fixtures::request_file("eu4", "eng.txt.compressed.eu4"), &mut data)?;
# /*
let data = std::fs::read("eng.txt.compressed.eu4")?;
# */
let file = Eu4File::from_slice(&data)?;
let save = file.parse_save(&SegmentedResolver::empty())?;
assert_eq!(file.encoding(), Encoding::TextZip);
assert_eq!(save.meta.player, "ENG");
# Ok::<(), Box<dyn std::error::Error>>(())
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
use std::collections::HashMap;
use eu4save::{Eu4File, Encoding, CountryTag, query::Query, SegmentedResolver};

# let mut data = Vec::new();
# std::io::Read::read_to_end(&mut pdx_fixtures::request_file("eu4", "eng.txt.compressed.eu4"), &mut data)?;
# /*
let data = std::fs::read("eng.txt.compressed.eu4")?;
# */
let file = Eu4File::from_slice(&data)?;
let save = file.parse_save(&SegmentedResolver::empty())?;
let save_query = Query::from_save(save);
let trade = save_query.country(&"ENG".parse()?)
    .map(|country| save_query.country_income_breakdown(country))
    .map(|income| income.trade);
assert_eq!(Some(17.982), trade);
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.

You may look to other projects EU4 ironman projects like ironmelt or paperman
for inspiration.
*/

mod country_tag;
pub mod de;
mod errors;
mod eu4date;
mod extraction;
pub mod file;
pub mod flavor;
mod melt;
/// Repository of raw structs extracted from a save file
pub mod models;
mod province_id;
/// Ergonomic module for querying info from a save file
pub mod query;
mod resolver;
mod tag_resolver;

pub use country_tag::*;
pub use errors::*;
pub use eu4date::*;
pub use extraction::*;
#[doc(inline)]
pub use file::Eu4File;
pub use jomini::binary::{BasicTokenResolver, FailedResolveStrategy};
pub use melt::*;
pub use province_id::*;
pub use resolver::{SegmentedResolver, SegmentedResolverBuilder};
pub use tag_resolver::*;
