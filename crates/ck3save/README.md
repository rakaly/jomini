![ci](https://github.com/rakaly/ck3save/workflows/ci/badge.svg) [![](https://docs.rs/ck3save/badge.svg)](https://docs.rs/ck3save) [![Version](https://img.shields.io/crates/v/ck3save.svg?style=flat-square)](https://crates.io/crates/ck3save)

# CK3 Save

CK3 Save is a library to ergonomically work with Crusader Kings III (CK3) saves (ironman + regular).

```rust
use ck3save::{models::Gamestate, Ck3File, DeserializeCk3, SaveHeaderKind};
use std::collections::HashMap;

let data = std::fs::read("assets/saves/Jarl_Ivar_of_the_Isles_867_01_01.ck3").unwrap();
let file = Ck3File::from_slice(&data).unwrap();

// Check the file type
assert_eq!(file.header().kind(), SaveHeaderKind::UnifiedText);

// Deserialize the entire save into a structured format
let resolver = HashMap::<u16, &str>::new();
let game: Gamestate = (&file).deserialize(&resolver).unwrap();
assert_eq!(game.meta_data.version, String::from("1.0.2"));
# Ok::<(), Box<dyn std::error::Error>>(())
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.
