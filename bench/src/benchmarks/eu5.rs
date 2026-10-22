//! Deserialization of an EU5 gamestate into an arena.
//!
//! The setup decompresses the gamestate, so that the measurement holds only
//! the deserializer. The arena is reset and used again for each iteration, the
//! same as a program that reads many saves one after the other.

use arena_serde::{Arena, ArenaDeserialize};
use eu5save::{
    BasicTokenResolver, Eu5BinaryFormat, Eu5File, SaveContentKind, SaveResolver, models::Gamestate,
};
use jomini::binary::{BinaryFormatDeserializer, TokenResolver};
use std::io::Read;

/// A 1.4 save, the newest format.
const SAVE: &str = "SP_MLO_1617_04_01_166d38a1-bffb-4182-9e37-9540b0c2ac4f.eu5";

pub struct Setup {
    pub gamestate: Vec<u8>,
    pub resolver: SaveResolver<BasicTokenResolver>,
}

/// Returns `None` when no EU5 token file is installed.
pub fn setup() -> Option<Setup> {
    let tokens = pdx_fixtures::tokens("eu5");
    let resolver = BasicTokenResolver::from_text_lines(tokens.as_slice()).unwrap();
    if resolver.is_empty() {
        return None;
    }

    let file = Eu5File::from_file(pdx_fixtures::request_file("eu5", SAVE)).unwrap();
    let resolver = SaveResolver::from_file(&file, resolver).unwrap();
    let mut gamestate = Vec::new();
    let SaveContentKind::Binary(mut reader) = file.gamestate().unwrap() else {
        panic!("expected a binary gamestate");
    };
    reader.read_to_end(&mut gamestate).unwrap();
    Some(Setup {
        gamestate,
        resolver,
    })
}

/// Deserialize the gamestate and return the number of buildings, so that the
/// result cannot be optimized away.
pub fn deserialize(setup: &Setup, arena: &Arena) -> usize {
    let format = Eu5BinaryFormat::new(&setup.resolver);
    let mut deserializer = BinaryFormatDeserializer::from_slice(&setup.gamestate, format);
    let gamestate = Gamestate::deserialize_in_arena(&mut deserializer, arena).unwrap();
    gamestate.building_manager.database.iter().count()
}

pub mod criterion_benches {
    use arena_serde::Arena;
    use criterion::{Criterion, Throughput};

    fn eu5_gamestate(c: &mut Criterion) {
        let Some(setup) = super::setup() else {
            return;
        };

        let mut group = c.benchmark_group("eu5");
        group.sample_size(20);
        group.throughput(Throughput::Bytes(setup.gamestate.len() as u64));
        let mut arena = Arena::new();
        group.bench_function("gamestate", |b| {
            b.iter(|| {
                arena.reset();
                crate::benchmarks::eu5::deserialize(&setup, &arena)
            })
        });
        group.finish();
    }

    criterion::criterion_group!(eu5_benches, eu5_gamestate);
}

#[cfg(not(target_family = "wasm"))]
pub mod gungraun_benches {
    use arena_serde::Arena;
    use gungraun::{library_benchmark, library_benchmark_group};

    fn setup() -> (crate::benchmarks::eu5::Setup, Arena) {
        let setup = super::setup().expect("the EU5 benchmark needs the EU5 token file");

        // Warm the arena with one parse, so that the measurement sees an arena
        // that is used again and not the growth of a new one.
        let mut arena = Arena::new();
        super::deserialize(&setup, &arena);
        arena.reset();
        (setup, arena)
    }

    #[library_benchmark]
    #[bench::gamestate(setup = setup)]
    fn eu5_gamestate((setup, arena): (crate::benchmarks::eu5::Setup, Arena)) -> usize {
        crate::benchmarks::eu5::deserialize(&setup, &arena)
    }

    library_benchmark_group!(name = eu5_benches, benchmarks = [eu5_gamestate]);
}
