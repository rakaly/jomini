use crate::{Game, Result};
use arena_serde::{Arena, ArenaDeserialize, tracked};
use eu5save::{
    BasicTokenResolver, Eu5BinaryDeserialization, Eu5File, Eu5Melt, FailedResolveStrategy,
    MeltOptions, SaveContentKind, SaveResolver, models::Gamestate,
};
use jomini::common::PdsDate;
use std::{fmt::Write as _, io::Write, path::Path};

pub struct Eu5 {
    data: Vec<u8>,
    tokens: Vec<u8>,
}

impl Eu5 {
    pub fn open(path: &Path) -> Result<Self> {
        Ok(Self {
            data: std::fs::read(path)?,
            tokens: pdx_fixtures::tokens("eu5"),
        })
    }
}

impl Game for Eu5 {
    fn melt(&self, out: &mut dyn Write) -> Result<()> {
        let file = Eu5File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let options = MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error);
        (&file).melt(options, &resolver, out)?;
        Ok(())
    }

    /// Deserialize the gamestate with path tracking, so that a model error
    /// reports where in the save it occurred.
    fn debug(&self) -> Result<String> {
        let file = Eu5File::from_slice(self.data.as_slice())?;
        let resolver = BasicTokenResolver::from_text_lines(self.tokens.as_slice())?;
        let arena = Arena::new();
        let mut path_buf = Vec::new();
        let track = tracked::Track::new_with(&mut path_buf);

        let save = match file.gamestate()? {
            SaveContentKind::Text(mut txt) => {
                let mut deser = txt.deserializer();
                let deser = tracked::Deserializer::new(&mut deser, &track);
                Gamestate::deserialize_in_arena(deser, &arena)
                    .map_err(|e| format!("{}: {e}", track.path()))?
            }
            SaveContentKind::Binary(mut bin) => {
                let resolver = SaveResolver::from_file(&file, &resolver)?;
                let mut deser = bin.deserializer(&resolver);
                let deser = tracked::Deserializer::new(&mut deser, &track);
                Gamestate::deserialize_in_arena(deser, &arena)
                    .map_err(|e| format!("{}: {e}", track.path()))?
            }
        };

        let version = &save.metadata.version;
        let mut out = format!(
            "eu5 {}.{}.{} {} countries: {} locations: {} pops: {} arena bytes: {}",
            version.major,
            version.minor,
            version.patch,
            save.metadata.date.game_fmt(),
            save.countries.len(),
            save.locations.len(),
            save.population.database.len(),
            arena.allocated_bytes(),
        );

        for player in save.played_countries {
            match save.countries.get_entry(player.country) {
                None => write!(
                    out,
                    "\nplayer country not found: {} {:?}",
                    player.name, player.country
                )?,
                Some(country) => write!(
                    out,
                    "\nplayer country: {} {:?} {:?}",
                    player.name,
                    player.country,
                    country.tag().to_str()
                )?,
            }

            for dep in save.diplomacy_manager.dependencies() {
                if dep.first == player.country || dep.second == player.country {
                    write!(out, "\n  {dep:?}")?;
                }
            }
        }

        let max_pop_types = save
            .locations
            .iter()
            .map(|x| x.location().population.pops.len())
            .max()
            .unwrap_or(0);
        let (max_population, max_location) = save.location_max_population();
        write!(
            out,
            "\nmax population types in a location: {max_pop_types}\
             \nmax population: {max_population} at {max_location:?}"
        )?;

        let wars = save.war_manager.database.iter().count();
        let battles: usize = save
            .war_manager
            .database
            .iter()
            .map(|w| w.battles.len())
            .sum();
        write!(
            out,
            "\nwars: {wars} battles: {battles} units: {} sub-units: {} loans: {} trades: {}",
            save.unit_manager.database.iter().count(),
            save.subunit_manager.database.iter().count(),
            save.loan_manager.database.iter().count(),
            save.trade_manager.database.iter().count(),
        )?;

        let highest_market = save
            .market_manager
            .database
            .iter()
            .max_by(|a, b| a.market_value().total_cmp(&b.market_value()));
        if let Some(market) = highest_market {
            write!(
                out,
                "\nhighest market value: {:?} at {}",
                market.center,
                market.market_value()
            )?;
        }

        Ok(out)
    }
}
