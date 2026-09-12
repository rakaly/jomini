![ci](https://github.com/rakaly/hoi4save/workflows/ci/badge.svg) [![](https://docs.rs/hoi4save/badge.svg)](https://docs.rs/hoi4save) [![Version](https://img.shields.io/crates/v/hoi4save.svg?style=flat-square)](https://crates.io/crates/hoi4save)

# HOI4 Save

HOI4 Save is a library to ergonomically work with Hearts of Iron IV saves (plaintext + binary).

```rust
use std::collections::HashMap;
use hoi4save::{Hoi4File, Encoding, models::Hoi4Save};
let data = std::fs::read("assets/saves/1.10-normal-text.hoi4")?;
let file = Hoi4File::from_slice(&data)?;
let parsed_file = file.parse()?;
let resolver = HashMap::<u16, &str>::new();
let save: Hoi4Save = parsed_file.deserializer().build(&resolver)?;
assert_eq!(file.encoding(), Encoding::Plaintext);
assert_eq!(save.player.as_deref(), Some("FRA"));
```

The HOI4 binary format can be converted to plaintext

```rust
use std::collections::HashMap;
use hoi4save::Hoi4File;

let data = std::fs::read("assets/saves/1.10-ironman.hoi4")?;
let file = Hoi4File::from_slice(&data)?;
let parsed_file = file.parse()?;
let binary = parsed_file.as_binary().unwrap();
let resolver = HashMap::<u16, &str>::new();
let out = binary
    .melter()
    .on_failed_resolve(hoi4save::FailedResolveStrategy::Stringify)
    .melt(&resolver)?;
```

## Binary Saves

Binary saves are supported, but not by default, as the token resolver can't be distributed, per PDS counsel.
