use crate::{Game, Result};
use hoi4save::{BasicTokenResolver, FailedResolveStrategy, Hoi4File, MeltOptions, PdsDate};
use std::{io::Write, path::Path};

pub struct Hoi4 {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Hoi4 {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("hoi4"),
        })
    }
}

impl Game for Hoi4 {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = Hoi4File::from_slice(&self.data)?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new()
            .verbatim(true)
            .on_failed_resolve(FailedResolveStrategy::Error);
        file.melt(options, &resolver, out)?;
        Ok(())
    }

    fn debug(&self) -> Result<String> {
        let file = Hoi4File::from_slice(&self.data)?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let save = file.parse_save(&resolver)?;
        Ok(format!(
            "hoi4 {} {}",
            save.player.as_deref().unwrap_or("-"),
            save.date.game_fmt()
        ))
    }
}
