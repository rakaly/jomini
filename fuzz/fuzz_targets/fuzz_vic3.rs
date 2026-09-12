#![no_main]
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;
use vic3save::{BasicTokenResolver, DeserializeVic3, Vic3Melt, savefile::Vic3Save};

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("vic3");
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

fn run(data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    let file = vic3save::Vic3File::from_slice(data)?;

    // Melt the file
    let mut sink = std::io::sink();
    let _ = (&file).melt(vic3save::MeltOptions::new(), &*TOKENS, &mut sink);

    // Try to deserialize the save
    let _: Result<Vic3Save, _> = (&file).deserialize(&*TOKENS);

    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
