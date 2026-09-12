/*!

# HOI4 Save

HOI4 Save is a library to ergonomically work with Hearts of Iron IV saves (plaintext + binary).

```rust,ignore
use std::collections::HashMap;
use hoi4save::{Hoi4File, Encoding, models::Hoi4Save};
let data = std::fs::read("assets/saves/1.10-normal-text.hoi4")?;
let file = Hoi4File::from_slice(&data)?;
let parsed_file = file.parse()?;
let resolver = HashMap::<u16, &str>::new();
let save: Hoi4Save = parsed_file.deserializer().build(&resolver)?;
assert_eq!(file.encoding(), Encoding::Plaintext);
assert_eq!(save.player, String::from("FRA"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

The HOI4 binary format can be converted to plaintext

```rust,ignore
use std::collections::HashMap;
use hoi4save::{Hoi4File, EnvTokens};

let data = std::fs::read("assets/saves/1.10-ironman.hoi4")?;
let file = Hoi4File::from_slice(&data)?;
let parsed_file = file.parse()?;
let binary = parsed_file.as_binary().unwrap();
let out = binary
    .melter()
    .on_failed_resolve(hoi4save::FailedResolveStrategy::Stringify)
    .melt(&EnvTokens)?;

# Ok::<(), Box<dyn std::error::Error>>(())
```

## Binary Saves

Binary saves are supported, but not by default, as the token resolver can't be distributed, per PDS counsel.

*/

mod country_tag;
mod date;
mod de;
mod errors;
mod extraction;
pub mod file;
mod flavor;
mod melt;
pub mod models;

pub use country_tag::*;
pub use date::*;
pub use errors::*;
pub use extraction::*;
#[doc(inline)]
pub use file::Hoi4File;
pub use flavor::Hoi4BinaryFormat;
pub use jomini::binary::{BasicTokenResolver, FailedResolveStrategy};
pub use melt::*;
