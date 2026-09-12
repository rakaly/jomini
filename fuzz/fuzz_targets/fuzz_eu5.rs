#![no_main]
use arena_serde::{Arena, ArenaDeserialize};
use eu5save::{
    BasicTokenResolver, Eu5BinaryDeserialization, Eu5Melt, SaveContentKind, SaveResolver,
    models::Gamestate,
};
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("eu5");
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

fn run(data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    let file = eu5save::Eu5File::from_slice(data)?;

    // Melt the file
    let mut sink = std::io::sink();
    let _ = (&file).melt(eu5save::MeltOptions::new(), &*TOKENS, &mut sink);

    // Try to deserialize the gamestate
    let arena = Arena::new();
    match file.gamestate()? {
        SaveContentKind::Text(mut txt) => {
            let _ = Gamestate::deserialize_in_arena(&mut txt.deserializer(), &arena);
        }
        SaveContentKind::Binary(mut bin) => {
            let resolver = SaveResolver::from_file(&file, &*TOKENS)?;
            let _ = Gamestate::deserialize_in_arena(&mut bin.deserializer(&resolver), &arena);
        }
    }

    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
