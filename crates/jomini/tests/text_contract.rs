#![cfg(all(feature = "syntax", feature = "derive"))]

//! `TextDeserializer` gives the value of the syntax parser or an error that
//! names it. See the `contract` module.

mod contract;

use contract::{Choices, Difference, Verdict};
use quickcheck::{QuickCheck, TestResult};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

fn assert_contract(data: &[u8]) {
    for (reader, verdict) in contract::check(data) {
        if let Verdict::Broken { streaming, syntax } | Verdict::Unhinted { streaming, syntax } =
            verdict
        {
            panic!(
                "{reader}: {:?}\n  TextDeserializer: {streaming}\n  syntax parser:    {syntax}",
                String::from_utf8_lossy(data)
            );
        }
    }
}

#[test]
fn contract_probes() {
    for data in contract::PROBES {
        assert_contract(data);
    }
}

/// Each construct of the allow-list is a difference only through some
/// readers, and the other inputs are read the same.
#[test]
fn contract_known_differences() {
    let cases: &[(&[u8], Option<Difference>)] = &[
        (b"x.holder?= { a = 1 }", Some(Difference::AttachedExists)),
        (b"x.holder ?= { a = 1 }", None),
        (
            b"o = { x { a = 1 } s = 2 }",
            Some(Difference::OperatorlessBlock),
        ),
        (b"o = { s = 2 x { a = 1 } }", None),
        (b"l = { a 1 { } }", Some(Difference::OperatorlessBlock)),
        (b"l = { a = 1 }", None),
        (b"o = { s = { 1 } { 2 } }", Some(Difference::BlockAsKey)),
        (b"o = { s = \"q\" { 2 } }", None),
        (b"o = { { s = 1 } }", Some(Difference::BlockAsKey)),
        (b"o = { {} 1 }", None),
        (b"a = { }\n= b", Some(Difference::OperatorAfterBlock)),
        (
            b"a = { a = b }={ 1 2 3 }",
            Some(Difference::OperatorAfterBlock),
        ),
        (
            b"o = { {} \"rgb\" { } }",
            Some(Difference::OperatorlessBlock),
        ),
        (b"a = { } b = c", None),
    ];
    for (data, expected) in cases {
        let name = String::from_utf8_lossy(data);
        assert_eq!(contract::known_difference(data), *expected, "{name}");
    }

    let verdicts: Vec<_> = contract::check(b"o = { x { a = 1 } s = 2 }")
        .into_iter()
        .map(|(_, verdict)| verdict)
        .collect();
    assert_eq!(
        verdicts,
        [
            Verdict::Same,
            Verdict::Same,
            Verdict::Known(Difference::OperatorlessBlock),
            Verdict::Same,
            Verdict::Same,
            Verdict::Same,
        ]
    );
}

/// Generated documents of the syntax of game files and saves.
#[test]
fn contract_generated_documents() {
    fn prop(choices: Vec<u8>) -> TestResult {
        let data = contract::generate(&mut Choices::new(&choices));
        assert_contract(&data);
        TestResult::passed()
    }

    QuickCheck::new()
        .tests(5000)
        .quickcheck(prop as fn(Vec<u8>) -> TestResult);
}

/// Generated documents without the constructs of the allow-list, on which
/// no difference is excused: each reader gives the same value, or an error
/// that names the syntax parser, unless the syntax parser gives a syntax
/// error.
#[test]
fn contract_generated_strict() {
    fn prop(choices: Vec<u8>) -> TestResult {
        let data = contract::generate(&mut Choices::strict(&choices));
        if contract::known_difference(&data).is_some() {
            return TestResult::discard();
        }
        assert_strict(&data);
        TestResult::passed()
    }

    QuickCheck::new()
        .tests(5000)
        .quickcheck(prop as fn(Vec<u8>) -> TestResult);
}

fn assert_strict(data: &[u8]) {
    for (reader, verdict) in contract::check(data) {
        assert!(
            matches!(
                verdict,
                Verdict::Same | Verdict::Hinted | Verdict::SyntaxError
            ),
            "{reader}: {:?}: {verdict:?}",
            String::from_utf8_lossy(data)
        );
    }
}

/// The counts of each verdict over the files of a directory.
#[derive(Default)]
struct Counts {
    files: usize,
    verdicts: BTreeMap<(&'static str, String), usize>,
    broken: Vec<(PathBuf, &'static str, String, String)>,
}

fn walk(dir: &Path, files: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk(&path, files);
        } else if path.extension().is_some_and(|x| x == "txt") {
            files.push(path);
        }
    }
}

/// Check the contract on each `.txt` file under the directories that
/// `JOMINI_CONTRACT_CORPUS` lists, as the `PATH` variable does, such as the
/// `common`, `history`, and `events` directories of the games:
///
/// ```sh
/// JOMINI_CONTRACT_CORPUS=~/games/ck3/game/common:~/games/eu4/history \
///     cargo test --release --all-features -p jomini --test text_contract -- --nocapture corpus
/// ```
///
/// The test is skipped when the variable is not set.
#[test]
fn contract_corpus() {
    let Some(dirs) = std::env::var_os("JOMINI_CONTRACT_CORPUS") else {
        eprintln!("JOMINI_CONTRACT_CORPUS is not set, so no corpus is checked");
        return;
    };

    let mut files = Vec::new();
    for dir in std::env::split_paths(&dirs) {
        walk(&dir, &mut files);
    }
    files.sort();

    let mut counts = Counts::default();
    for path in files {
        let Ok(data) = std::fs::read(&path) else {
            continue;
        };
        counts.files += 1;
        for (reader, verdict) in contract::check_slice(&data) {
            if reader == "typed" {
                continue;
            }
            let key = match verdict {
                Verdict::Broken { streaming, syntax } => {
                    let cut = |s: String| s.chars().take(300).collect::<String>();
                    counts
                        .broken
                        .push((path.clone(), reader, cut(streaming), cut(syntax)));
                    String::from("broken")
                }
                Verdict::Known(difference) => format!("{difference:?}"),
                verdict => format!("{verdict:?}"),
            };
            *counts.verdicts.entry((reader, key)).or_default() += 1;
        }
    }

    eprintln!("files: {}", counts.files);
    for ((reader, verdict), count) in &counts.verdicts {
        eprintln!("{reader:>4} {verdict:<24} {count}");
    }
    for (path, reader, streaming, syntax) in &counts.broken {
        eprintln!(
            "{} ({reader})\n  TextDeserializer: {streaming}\n  syntax parser:    {syntax}",
            path.display()
        );
    }
    assert!(
        counts.broken.is_empty(),
        "{} silent differences",
        counts.broken.len()
    );
}
