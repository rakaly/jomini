//! EU4 support, with two research commands from the crate's old CLI:
//!
//! ```text
//! pdx csv     <save>   Date, tag, and prestige of each country as CSV
//! pdx deducer <save>   First non-default value at each ledger index, to
//!                      work out what the indices mean
//! ```

use crate::{Game, Result};
use eu4save::{
    BasicTokenResolver, CountryTag, Eu4File, FailedResolveStrategy, MeltOptions, PdsDate,
    SegmentedResolver, models::Eu4Save, query::Query,
};
use std::{collections::HashSet, fmt::Display, io::Write, path::Path, time::Instant};

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

    fn parse(&self) -> Result<Eu4Save> {
        let file = Eu4File::from_slice(&self.data)?;
        let resolver = SegmentedResolver::parse(self.tokens.as_slice())?;
        Ok(file.parse_save(resolver.resolver())?)
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

    /// Parse the save, then run the query pipeline that the old debug
    /// command timed: province owners, nation events, player histories,
    /// and the first player's nation size ledger.
    fn debug(&self) -> Result<String> {
        let save = self.parse()?;
        let v = save.meta.savegame_version.clone();
        let player = save.meta.player;
        let date = save.meta.date;

        let start = Instant::now();
        let query = Query::from_save(save);
        let owners = query.province_owners();
        let nation_events = query.nation_events(&owners);
        let players = query.player_histories(&nation_events);
        let ledger = players
            .first()
            .map(|p| query.nation_size_statistics_ledger(&p.history).len())
            .unwrap_or(0);
        let query_ms = start.elapsed().as_millis();

        Ok(format!(
            "eu4 {}.{}.{}.{} {} {} ledger: {} query: {}ms",
            v.first,
            v.second,
            v.third,
            v.fourth,
            player,
            date.iso_8601(),
            ledger,
            query_ms,
        ))
    }

    fn extra(&self, command: &str, out: &mut dyn Write) -> Result<bool> {
        match command {
            "csv" => csv(&self.parse()?, out)?,
            "deducer" => deducer(&self.parse()?, out)?,
            _ => return Ok(false),
        }
        Ok(true)
    }
}

/// Write the date, tag, and prestige of each country that owns a city.
fn csv(save: &Eu4Save, out: &mut dyn Write) -> Result<()> {
    writeln!(out, "date,tag,prestige")?;
    for (tag, country) in &save.game.countries {
        if country.num_of_cities > 0 {
            writeln!(
                out,
                "{},{},{}",
                save.meta.date.iso_8601(),
                tag,
                country.prestige
            )?;
        }
    }
    Ok(())
}

/// Write the first country with a non-default value at each index of the
/// income, expense, and losses ledgers. Some indices are not documented,
/// and a country that has a value at an index is a lead on what the index
/// means.
fn deducer(save: &Eu4Save, out: &mut dyn Write) -> Result<()> {
    let countries = &save.game.countries;
    deduce_vec(
        out,
        countries
            .iter()
            .map(|(tag, c)| (*tag, c.ledger.income.as_slice())),
    )?;
    deduce_vec(
        out,
        countries
            .iter()
            .map(|(tag, c)| (*tag, c.ledger.expense.as_slice())),
    )?;
    deduce_vec(
        out,
        countries
            .iter()
            .filter(|(_tag, c)| c.num_of_cities > 0)
            .map(|(tag, c)| (*tag, c.losses.members.as_slice())),
    )?;
    Ok(())
}

struct Deduce<N> {
    country: CountryTag,
    index: usize,
    value: N,
}

fn deduce_vec<'a, N>(
    out: &mut dyn Write,
    iter: impl Iterator<Item = (CountryTag, &'a [N])>,
) -> Result<()>
where
    N: 'a + PartialEq + Default + Display,
{
    let mut ded = Vec::new();
    let mut found_indices = HashSet::new();
    let default_val = N::default();

    for (tag, vals) in iter {
        for (i, value) in vals.iter().enumerate() {
            if value.ne(&default_val) && !found_indices.contains(&i) {
                found_indices.insert(i);
                ded.push(Deduce {
                    index: i,
                    value,
                    country: tag,
                });
            }
        }
    }

    ded.sort_by_key(|x| x.index);
    writeln!(out, "tag\tindex\tvalue")?;
    for item in &ded {
        writeln!(out, "{}\t{}\t{}", item.country, item.index, item.value)?;
    }
    Ok(())
}
