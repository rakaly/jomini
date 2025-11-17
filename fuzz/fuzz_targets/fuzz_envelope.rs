#![no_main]
use jomini::envelope::JominiFile;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // Parse the envelope from arbitrary bytes
    let Ok(file) = JominiFile::from_slice(data) else {
        return;
    };

    // Access header - should not panic
    let _header = file.header();

    // Try to extract metadata without panicking
    if let Ok(mut meta) = file.meta() {
        let _ = std::io::copy(&mut meta, &mut std::io::sink());
    }

    // Try to extract gamestate without panicking
    if let Ok(mut gamestate) = file.gamestate() {
        let _ = std::io::copy(&mut gamestate, &mut std::io::sink());
    }
});
