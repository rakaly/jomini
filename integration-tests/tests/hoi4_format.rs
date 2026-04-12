#[macro_use]
mod support;

use hoi4save::{CountryTag, FailedResolveStrategy, Hoi4Format};
use serde::Deserialize;
use std::collections::HashMap;
use support::*;

const SAVE_VERSION_ID: u16 = 0x349d;

fn test_resolver() -> HashMap<u16, &'static str> {
    HashMap::from([
        (LABEL, "label"),
        (HAN, "han"),
        (SCORE32, "score32"),
        (SCORE64, "score64"),
        (FLAG, "flag"),
        (COUNT, "count"),
        (DELTA, "delta"),
        (WORD, "word"),
        (NESTED, "nested"),
        (INNER, "inner"),
        (MEMBERS, "members"),
        (CAMPAIGN_ID, "campaign_id"),
        (PLAYER, "player"),
        (TAG, "tag"),
        (NAME, "name"),
        (VALUE, "value"),
        (ITEMS, "items"),
        (FIELD1, "field1"),
        (FLAGS, "flags"),
        (OBJ, "obj"),
        (ID, "id"),
        (TYPE, "type"),
        (DYNASTY, "dynasty"),
        (SAVE_VERSION_ID, "save_version"),
    ])
}

fn make_format() -> Hoi4Format<HashMap<u16, &'static str>> {
    Hoi4Format::new(test_resolver())
}

shared_format_tests!(
    make_format,
    |strategy: FailedResolveStrategy| Hoi4Format::new(test_resolver())
        .with_failed_resolve_strategy(strategy)
);

#[test]
fn hoi4_legacy_f32() {
    // save_version defaults to 0 (< 30), so F32 reads 4 bytes: i32/1000.0
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score32: f32,
    }
    let data = build_binary(&[(SCORE32, Val::F32(1234))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score32, 1.234);
}

#[test]
fn hoi4_modern_f32() {
    // After save_version >= 30, F32 reads 8 bytes: i64/100000.0
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        save_version: i32,
        score32: f32,
    }
    let data = build_binary(&[
        (SAVE_VERSION_ID, Val::I32(30)),
        (SCORE32, Val::F32Wide(123400)),
    ]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.save_version, 30);
    assert_eq!(actual.score32, 1.234);
}

#[test]
fn hoi4_f64_floor_rounding() {
    // f64: i64/32768.0 with floor to 5 decimal places
    // raw 81920 → 81920/32768.0 = 2.5 exactly
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score64: f64,
    }
    let data = build_binary(&[(SCORE64, Val::F64(81920))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score64, 2.5);
}

#[test]
fn hoi4_f64_floor_truncation() {
    // Verify floor behavior: val * 100000 is floored then divided by 100000
    // raw 81921 → 81921/32768.0 = 2.500030517578125
    // floor(2.500030517578125 * 100000) / 100000 = 250003 / 100000 = 2.50003
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score64: f64,
    }
    let data = build_binary(&[(SCORE64, Val::F64(81921))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score64, 2.50003);
}

#[test]
fn hoi4_utf8_strings() {
    // HOI4 uses UTF-8 encoding (not Windows-1252)
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        label: String,
    }
    let data = build_binary(&[(LABEL, Val::Str("Ünïcödé".as_bytes()))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.label, "Ünïcödé");
}

#[test]
fn hoi4_country_tag() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        tag: CountryTag,
    }
    let data = build_binary(&[(TAG, Val::Str(b"GER"))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.tag.as_str(), "GER");
}

#[test]
fn hoi4_save_version_transitions() {
    // Verify that the format transitions from legacy to modern f32
    // within a single document: first F32 uses legacy (4 bytes), then after
    // save_version=30, subsequent F32s use modern (8 bytes)
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score32: f32,
        save_version: i32,
        delta: f32,
    }
    let data = build_binary(&[
        (SCORE32, Val::F32(5000)),       // legacy: 5000/1000 = 5.0
        (SAVE_VERSION_ID, Val::I32(30)), // triggers modern mode
        (DELTA, Val::F32Wide(500_000)),   // modern: 500000/100000 = 5.0
    ]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score32, 5.0);
    assert_eq!(actual.save_version, 30);
    assert_eq!(actual.delta, 5.0);
}
