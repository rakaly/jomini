//! Vic3 support, with two research commands from the crate's old CLI:
//!
//! ```text
//! pdx stats  <save>   GDP, growth, GDP per capita, and standard of living
//!                     of the last played country over time
//! pdx market <save>   Goods prices in the last played country's states,
//!                     derived from its buildings
//! ```

use crate::{Game, Result};
use std::{io::Write, path::Path};
use vic3save::{
    BasicTokenResolver, DeserializeVic3, FailedResolveStrategy, MeltOptions, Vic3File, Vic3Melt,
    markets::goods_price_based_on_buildings, savefile::Vic3Save, stats::Vic3CountryStatsRateIter,
};

pub struct Vic3 {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Vic3 {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("vic3"),
        })
    }

    fn parse(&self) -> Result<Vic3Save> {
        let file = Vic3File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        Ok((&file).deserialize(resolver)?)
    }
}

impl Game for Vic3 {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = Vic3File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error);
        (&file).melt(options, resolver, out)?;
        Ok(())
    }

    fn debug(&self) -> Result<String> {
        let save = self.parse()?;
        Ok(format!("vic3 {}", save.meta_data.version))
    }

    fn extra(&self, command: &str, out: &mut dyn Write) -> Result<bool> {
        match command {
            "stats" => stats(&self.parse()?, out)?,
            "market" => market(&self.parse()?, out)?,
            _ => return Ok(false),
        }
        Ok(true)
    }
}

/// Write one line per date: GDP in millions, GDP growth, GDP per capita,
/// standard of living, and GDP per capita growth over a year.
fn stats(save: &Vic3Save, out: &mut dyn Write) -> Result<()> {
    let tag = save.get_last_played_country().definition.as_ref();
    let country = save.get_country(tag).ok_or("tag to be found")?;

    let gdp_line = country.gdp.iter();
    let sol_line = country.avgsoltrend.iter();
    let pop_line = || country.pop_statistics.trend_population.iter();
    let gdpc_line = || {
        pop_line()
            .zip_aligned(country.gdp.iter())
            .map(|(date, (pop, gdp))| (date, (gdp / (pop / 100_000.0))))
    };
    let gdpc_growth = Vic3CountryStatsRateIter::new(gdpc_line(), 365);
    for (date, [gdp, gdp_growth, sol, gdpc, gdpc_growth]) in gdp_line
        .zip_aligned(country.gdp.gdp_growth())
        .zip_aligned(sol_line)
        .zip_aligned(gdpc_line())
        .zip_aligned(gdpc_growth)
        .flat()
    {
        writeln!(
            out,
            "{:?}\t{:.2}\t{:0.2}\t{:.2}\t{:.2}\t{:0.2}",
            date,
            gdp / 1000000.0,
            gdp_growth,
            gdpc,
            sol,
            gdpc_growth,
        )?;
    }

    Ok(())
}

/// Write the price of each good in the last played country's market.
fn market(save: &Vic3Save, out: &mut dyn Write) -> Result<()> {
    let tag = save.get_last_played_country().definition.as_ref();
    writeln!(out, "Market for country {}", tag)?;
    let country = save.get_country(tag).ok_or("tag to be found")?;
    let states = &country.states;
    let goods_prices = goods_price_based_on_buildings(
        save.building_manager
            .database
            .values()
            .filter_map(|x| x.as_ref())
            .filter(|b| states.contains(&b.state)),
    )?;

    for (good, val) in goods_prices {
        writeln!(out, "{}: {}", good, val)?;
    }
    Ok(())
}
