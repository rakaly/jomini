/*!
# CK3 Save

CK3 Save is a library to ergonomically work with Crusader Kings 3 (CK3) saves (ironman + regular).

```rust
use ck3save::{models::Gamestate, Ck3File, DeserializeCk3, SaveHeaderKind};
use std::collections::HashMap;

# let data = pdx_fixtures::read("ck3", "Jarl_Ivar_of_the_Isles_867_01_01.ck3");
# /*
let data = std::fs::read("Jarl_Ivar_of_the_Isles_867_01_01.ck3").unwrap();
# */
let file = Ck3File::from_slice(&data).unwrap();

// Check the file type
assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);

// Deserialize the entire save into a structured format
let resolver = HashMap::<u16, &str>::new();
let game: Gamestate = (&file).deserialize(&resolver).unwrap();
assert_eq!(game.meta_data.version, String::from("1.0.2"));
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.

*/

mod ck3date;
mod errors;
pub mod file;
pub(crate) mod flavor;
mod melt;
pub mod models;

pub use ck3date::*;
pub use errors::*;
pub use file::*;
pub use jomini::binary::{BasicTokenResolver, FailedResolveStrategy};
pub use melt::*;
