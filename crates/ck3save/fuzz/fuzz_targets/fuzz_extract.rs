#![no_main]
use ck3save::{models::Gamestate, BasicTokenResolver, Ck3Melt, DeserializeCk3};
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = std::fs::read("assets/ck3.txt").unwrap();
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

fn run(data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    let file = ck3save::Ck3File::from_slice(&data)?;

    // Melt the file
    let mut sink = std::io::sink();
    let _ = (&file).melt(ck3save::MeltOptions::new(), &*TOKENS, &mut sink);

    // Try to deserialize the gamestate
    let _: Result<Gamestate, _> = (&file).deserialize(&*TOKENS);

    // Check header information
    let _ = file.header().kind();

    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
