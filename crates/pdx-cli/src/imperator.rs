use crate::{Game, Result};
use imperator_save::{
    BasicTokenResolver, DeserializeImperator, FailedResolveStrategy, ImperatorFile, ImperatorMelt,
    MeltOptions, PdsDate, models::Save,
};
use std::{io::Write, path::Path};

pub struct Imperator {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Imperator {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("imperator"),
        })
    }
}

impl Game for Imperator {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = ImperatorFile::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new()
            .verbatim(true)
            .on_failed_resolve(FailedResolveStrategy::Error);
        (&file).melt(options, resolver, out)?;
        Ok(())
    }

    fn debug(&self) -> Result<String> {
        let file = ImperatorFile::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let save: Save = (&file).deserialize(resolver)?;
        Ok(format!(
            "imperator {} {}",
            save.meta.version,
            save.meta.date.game_fmt()
        ))
    }
}
