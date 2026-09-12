/*!
# Imperator Save

Imperator Save is a library to ergonomically work with Imperator Rome saves (debug + standard).

To load and parse a save file:

```rust,ignore
use imperator_save::{ImperatorFile, models::Save, BasicTokenResolver};
use imperator_save::DeserializeImperator;

// Load the file
let file_path = "assets/saves/observer1.5.rome";
let file = ImperatorFile::from_file(std::fs::File::open(file_path)?)?;

// Create a token resolver (for binary saves)
// Note: For ironman saves, you need the actual token data
let tokens = BasicTokenResolver::from_text_lines(&b""[..]).unwrap();

// Parse the save
let save: Save = (&file).deserialize(tokens)?;
assert_eq!(save.meta.version, String::from("1.5.3"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.

*/

mod date;
mod errors;
mod file;
mod flavor;
mod melt;
pub mod models;

pub use date::*;
pub use errors::*;
pub use file::*;
pub use jomini::binary::{BasicTokenResolver, FailedResolveStrategy};
pub use melt::*;
