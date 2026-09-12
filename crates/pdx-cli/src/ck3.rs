use crate::{Game, Result};
use ck3save::{
    BasicTokenResolver, Ck3File, Ck3Melt, DeserializeCk3, FailedResolveStrategy, MeltOptions,
    models::Gamestate,
};
use std::{io::Write, path::Path};

pub struct Ck3 {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Ck3 {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("ck3"),
        })
    }
}

impl Game for Ck3 {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = Ck3File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new()
            .verbatim(true)
            .on_failed_resolve(FailedResolveStrategy::Error);
        (&file).melt(options, resolver, out)?;
        Ok(())
    }

    fn debug(&self) -> Result<String> {
        let file = Ck3File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let save: Gamestate = (&file).deserialize(resolver)?;
        Ok(format!("ck3 {}", save.meta_data.version))
    }
}
