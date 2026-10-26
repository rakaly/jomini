//! Deserialization of an EU5 save into an arena.
//!
//! The measurement follows the path of a program that reads a save: it opens
//! the zip in memory, reads the string lookup, and deserializes the gamestate
//! from a reader that inflates it. The arena is reset and used again for each
//! iteration, the same as a program that reads many saves one after the other.

use arena_serde::{Arena, ArenaDeserialize};
use eu5save::{
    BasicTokenResolver, Eu5BinaryDeserialization, Eu5File, SaveContentKind, SaveResolver,
    models::Gamestate,
};
use jomini::{binary::TokenResolver, envelope::JominiFileKind};

/// A 1.4 save.
const SAVE: &str = "SP_MLO_1617_04_01_166d38a1-bffb-4182-9e37-9540b0c2ac4f.eu5";

pub struct Setup {
    pub save: Vec<u8>,
    pub tokens: BasicTokenResolver,
}

impl Setup {
    /// The size of the inflated gamestate.
    pub fn gamestate_len(&self) -> u64 {
        let file = Eu5File::from_slice(self.save.as_slice()).unwrap();
        let JominiFileKind::Zip(zip) = file.kind() else {
            panic!("expected a zip save");
        };
        zip.gamestate_uncompressed_hint()
    }
}

/// Returns `None` when no EU5 token file is installed.
pub fn setup() -> Option<Setup> {
    let tokens = pdx_fixtures::tokens("eu5");
    let tokens = BasicTokenResolver::from_text_lines(tokens.as_slice()).unwrap();
    if tokens.is_empty() {
        return None;
    }

    let save = pdx_fixtures::read("eu5", SAVE);
    Some(Setup { save, tokens })
}

/// Deserialize the gamestate and return the number of buildings, so that the
/// result cannot be optimized away.
pub fn deserialize(setup: &Setup, arena: &Arena) -> usize {
    let file = Eu5File::from_slice(setup.save.as_slice()).unwrap();
    let resolver = SaveResolver::from_file(&file, &setup.tokens).unwrap();
    let SaveContentKind::Binary(mut gamestate) = file.gamestate().unwrap() else {
        panic!("expected a binary gamestate");
    };
    let mut deserializer = gamestate.deserializer(&resolver);
    let gamestate = Gamestate::deserialize_in_arena(&mut deserializer, arena).unwrap();
    gamestate.building_manager.database.iter().count()
}

/// Return an arena that a parse no longer grows. A reset keeps only the last
/// chunk, which is smaller than what a parse needs until a parse fits in it.
pub fn warm_arena(setup: &Setup) -> Arena {
    let mut arena = Arena::new();
    loop {
        deserialize(setup, &arena);
        let allocated = arena.allocated_bytes();
        arena.reset();
        if arena.allocated_bytes() == allocated {
            return arena;
        }
    }
}

pub mod criterion_benches {
    use criterion::{Criterion, SamplingMode, Throughput};

    fn eu5_gamestate(c: &mut Criterion) {
        let Some(setup) = super::setup() else {
            return;
        };

        let mut group = c.benchmark_group("eu5");
        group.sample_size(20);
        group.sampling_mode(SamplingMode::Flat);
        group.throughput(Throughput::Bytes(setup.gamestate_len()));
        let mut arena = super::warm_arena(&setup);
        group.bench_function("gamestate", |b| {
            b.iter(|| {
                arena.reset();
                super::deserialize(&setup, &arena)
            })
        });
        group.finish();
    }

    criterion::criterion_group!(eu5_benches, eu5_gamestate);
}

/// The harness drops a return value outside of the measurement, so the
/// benchmark returns the setup and the arena to exclude their drop.
#[cfg(not(target_family = "wasm"))]
pub mod gungraun_benches {
    use crate::benchmarks::eu5::Setup;
    use arena_serde::Arena;
    use gungraun::{library_benchmark, library_benchmark_group};

    /// Returns `None` when no EU5 token file is installed, and the benchmark
    /// does nothing.
    fn setup() -> Option<(Setup, Arena)> {
        let setup = crate::benchmarks::eu5::setup()?;
        let arena = crate::benchmarks::eu5::warm_arena(&setup);
        Some((setup, arena))
    }

    #[library_benchmark]
    #[bench::gamestate(setup = setup)]
    fn eu5_gamestate(input: Option<(Setup, Arena)>) -> Option<(usize, Setup, Arena)> {
        let (setup, arena) = input?;
        let buildings = crate::benchmarks::eu5::deserialize(&setup, &arena);
        Some((buildings, setup, arena))
    }

    library_benchmark_group!(name = eu5_benches, benchmarks = [eu5_gamestate]);
}
