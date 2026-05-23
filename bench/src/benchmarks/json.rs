use jomini::{
    TextTape,
    json::{DuplicateKeyMode, JsonOptions},
};

const METADATA_TXT: &[u8] = include_bytes!("../../../tests/fixtures/meta.txt");
const CK3_TXT: &[u8] = include_bytes!("../../../tests/fixtures/ck3-header.txt");

#[derive(Clone, Copy)]
enum JsonMode {
    Preserve,
    Group,
    Typed,
}

struct JsonCase {
    tape: TextTape<'static>,
    mode: JsonMode,
}

#[inline(never)]
fn json_to_string(tape: &TextTape, mode: JsonMode) -> String {
    match mode {
        JsonMode::Preserve => tape.windows1252_reader().json().to_string(),
        JsonMode::Group => tape
            .windows1252_reader()
            .json()
            .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::Group))
            .to_string(),
        JsonMode::Typed => tape
            .windows1252_reader()
            .json()
            .with_options(JsonOptions::new().with_duplicate_keys(DuplicateKeyMode::KeyValuePairs))
            .to_string(),
    }
}

fn eu4_tape() -> TextTape<'static> {
    TextTape::from_slice(&METADATA_TXT["EU4txt".len()..]).unwrap()
}

fn ck3_tape() -> TextTape<'static> {
    TextTape::from_slice(CK3_TXT).unwrap()
}

fn setup_eu4_preserve() -> JsonCase {
    JsonCase {
        tape: eu4_tape(),
        mode: JsonMode::Preserve,
    }
}

fn setup_eu4_group() -> JsonCase {
    JsonCase {
        tape: eu4_tape(),
        mode: JsonMode::Group,
    }
}

fn setup_eu4_typed() -> JsonCase {
    JsonCase {
        tape: eu4_tape(),
        mode: JsonMode::Typed,
    }
}

fn setup_ck3_preserve() -> JsonCase {
    JsonCase {
        tape: ck3_tape(),
        mode: JsonMode::Preserve,
    }
}

fn setup_ck3_group() -> JsonCase {
    JsonCase {
        tape: ck3_tape(),
        mode: JsonMode::Group,
    }
}

fn setup_ck3_typed() -> JsonCase {
    JsonCase {
        tape: ck3_tape(),
        mode: JsonMode::Typed,
    }
}

pub mod criterion_benches {
    use super::*;
    use criterion::{BenchmarkId, Criterion, Throughput};

    pub fn json(c: &mut Criterion) {
        let mut group = c.benchmark_group("json");

        for (game, tape) in [("eu4", eu4_tape()), ("ck3", ck3_tape())] {
            for (mode_name, mode) in [
                ("preserve", JsonMode::Preserve),
                ("group", JsonMode::Group),
                ("typed", JsonMode::Typed),
            ] {
                let bytes = json_to_string(&tape, mode).len();
                group.throughput(Throughput::Bytes(bytes as u64));
                group.bench_function(BenchmarkId::new(mode_name, game), |b| {
                    b.iter(|| json_to_string(&tape, mode))
                });
            }
        }

        group.finish();
    }

    criterion::criterion_group!(json_benches, json);
}

pub mod gungraun_benches {
    use super::*;
    use gungraun::{library_benchmark, library_benchmark_group};

    #[library_benchmark]
    #[bench::eu4_preserve(setup = setup_eu4_preserve)]
    #[bench::eu4_group(setup = setup_eu4_group)]
    #[bench::eu4_typed(setup = setup_eu4_typed)]
    #[bench::ck3_preserve(setup = setup_ck3_preserve)]
    #[bench::ck3_group(setup = setup_ck3_group)]
    #[bench::ck3_typed(setup = setup_ck3_typed)]
    fn json(case: JsonCase) -> String {
        json_to_string(&case.tape, case.mode)
    }

    library_benchmark_group!(name = json_benches, benchmarks = [json,]);
}
