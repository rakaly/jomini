use crate::{Game, Result};
use eu4save::{
    BasicTokenResolver, Eu4File, FailedResolveStrategy, MeltOptions, PdsDate, SegmentedResolver,
};
use std::{io::Write, path::Path};

pub struct Eu4 {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Eu4 {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("eu4"),
        })
    }
}

impl Game for Eu4 {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = Eu4File::from_slice(&self.data)?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new()
            .verbatim(true)
            .on_failed_resolve(FailedResolveStrategy::Error);
        file.melt(options, resolver, out)?;
        Ok(())
    }

    fn debug(&self) -> Result<String> {
        let file = Eu4File::from_slice(&self.data)?;
        let resolver = SegmentedResolver::parse(self.tokens.as_slice())?;
        let save = file.parse_save(resolver.resolver())?;
        let v = &save.meta.savegame_version;
        Ok(format!(
            "eu4 {}.{}.{}.{} {} {}",
            v.first,
            v.second,
            v.third,
            v.fourth,
            save.meta.player,
            save.meta.date.iso_8601()
        ))
    }
}
