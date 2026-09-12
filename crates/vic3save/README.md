![ci](https://github.com/rakaly/jomini/workflows/ci/badge.svg)

# Vic3 Save

Vic3 Save is a library to ergonomically work with Victoria 3 saves (ironman + debug).

```rust,ignore
use vic3save::{savefile::Vic3Save, BasicTokenResolver, DeserializeVic3, Vic3File};

let data = std::fs::read("assets/saves/vic3/egalitarian2.v3").unwrap();
let file = Vic3File::from_slice(&data).unwrap();

// Ironman saves need a token resolver. The data to construct one is not
// distributed here.
let tokens = std::fs::read("assets/tokens/vic3.txt").unwrap();
let resolver = BasicTokenResolver::from_text_lines(tokens.as_slice()).unwrap();

let save: Vic3Save = (&file).deserialize(&resolver).unwrap();
assert_eq!(save.meta_data.version, String::from("1.7.1"));
```

## Ironman

Ironman saves are supported through a provided `TokenResolver`. Per PDS counsel, the data to construct such a `TokenResolver` is not distributed here.
