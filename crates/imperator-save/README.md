![ci](https://github.com/rakaly/imperator-save/workflows/ci/badge.svg) [![](https://docs.rs/imperator-save/badge.svg)](https://docs.rs/imperator-save) [![Version](https://img.shields.io/crates/v/imperator-save.svg?style=flat-square)](https://crates.io/crates/imperator-save)

# Imperator Save

Imperator Save is a library to ergonomically work with Imperator Rome saves (debug + standard).

```rust
use imperator_save::{ImperatorFile, models::Save, BasicTokenResolver};

// Load the file
let file_path = "assets/saves/observer1.5.rome";
let mut file = ImperatorFile::from_file(std::fs::File::open(file_path)?)?;

// Create a token resolver (for binary saves)
// Note: For ironman saves, you need the actual token data
let tokens = BasicTokenResolver::from_text_lines(b"").unwrap();

// Parse the save
let save = Save::from_file(&mut file, &tokens)?;
assert_eq!(save.meta.version, String::from("1.5.3"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.

