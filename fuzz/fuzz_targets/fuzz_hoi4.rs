#![no_main]
use hoi4save::{BasicTokenResolver, Hoi4File, MeltOptions};
use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;

static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("hoi4");
    BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
});

fn run(data: &[u8]) -> Result<(), Box<dyn std::error::Error>> {
    let file = Hoi4File::from_slice(data)?;

    let mut sink = std::io::sink();
    let _ = file.melt(MeltOptions::new(), &*TOKENS, &mut sink);
    let _ = file.parse_save(&*TOKENS);
    let _ = file.encoding();

    Ok(())
}

fuzz_target!(|data: &[u8]| {
    let _ = run(data);
});
