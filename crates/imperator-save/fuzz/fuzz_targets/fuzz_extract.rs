#![no_main]
use imperator_save::{
    models::Save, BasicTokenResolver, DeserializeImperator, ImperatorFile, ImperatorMelt,
    JominiFileKind, MeltOptions, SaveDataKind,
};
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = std::fs::read("assets/imperator.txt").unwrap();
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

fn run(data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    let file = ImperatorFile::from_slice(data)?;

    let mut sink = std::io::sink();
    let _ = (&file).melt(MeltOptions::new(), &*TOKENS, &mut sink);
    let _ = (&file).deserialize::<Save>(&*TOKENS);

    if let JominiFileKind::Uncompressed(SaveDataKind::Text(text)) = file.kind() {
        let _ = text.deserializer().deserialize::<Save>();
    }

    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
