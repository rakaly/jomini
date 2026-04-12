#[macro_use]
mod support;

use eu4save::flavor::Eu4Format;
use eu4save::{CountryTag, FailedResolveStrategy, SegmentedResolver};
use jomini::binary::ng::{from_slice, from_reader};
use jomini::binary::LexemeId;
use serde::Deserialize;
use std::collections::HashMap;
use support::*;

fn test_resolver() -> SegmentedResolver<'static> {
    let mut values: Vec<&'static str> = vec![""; 0x2017];
    values[LABEL as usize] = "label";
    values[HAN as usize] = "han";
    values[SCORE32 as usize] = "score32";
    values[SCORE64 as usize] = "score64";
    values[FLAG as usize] = "flag";
    values[COUNT as usize] = "count";
    values[DELTA as usize] = "delta";
    values[WORD as usize] = "word";
    values[NESTED as usize] = "nested";
    values[INNER as usize] = "inner";
    values[MEMBERS as usize] = "members";
    values[CAMPAIGN_ID as usize] = "campaign_id";
    values[PLAYER as usize] = "player";
    values[TAG as usize] = "tag";
    values[NAME as usize] = "name";
    values[VALUE as usize] = "value";
    values[ITEMS as usize] = "items";
    values[FIELD1 as usize] = "field1";
    values[FLAGS as usize] = "flags";
    values[OBJ as usize] = "obj";
    values[ID as usize] = "id";
    values[TYPE as usize] = "type";
    values[DYNASTY as usize] = "dynasty";
    SegmentedResolver::from_parts(values, 0x2017, 0x2017)
}

fn make_format() -> Eu4Format<SegmentedResolver<'static>> {
    Eu4Format::new(test_resolver())
}

shared_format_tests!(
    make_format,
    |strategy: FailedResolveStrategy| Eu4Format::new(test_resolver())
        .with_failed_resolve_strategy(strategy)
);

#[test]
fn eu4_f32_fixed_point() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score32: f32,
    }
    let data = build_binary(&[(SCORE32, Val::F32(1234))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score32, 1.234);
}

#[test]
fn eu4_f64_q49_15_rounding() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        score64: f64,
    }
    let raw = i64::from_le_bytes([210, 63, 1, 0, 0, 0, 0, 0]);
    let data = build_binary(&[(SCORE64, Val::F64(raw))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.score64, 2.49860);
}

#[test]
fn eu4_chinese_escaped_text() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        han: String,
    }
    let data = build_binary(&[(HAN, Val::Str(&[0x10, 0x60, 0x4f, 0x10, 0x7d, 0x59]))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.han, "你好");
}

#[test]
fn eu4_chinese_truncated_escape() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        han: String,
    }
    let data = build_binary(&[(HAN, Val::Str(&[0x10]))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.han, "…");
}

#[test]
fn eu4_windows1252_scalars() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct Data {
        label: String,
    }
    let data = build_binary(&[(LABEL, Val::Str(&[0xa7, b'G']))]);
    let actual: Data = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.label, "\u{a7}G");
}

#[derive(Debug, Deserialize, PartialEq)]
struct GhostOuter {
    inner: i32,
}

#[test]
fn ghost_object_empty() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct WithGhost {
        nested: GhostOuter,
    }

    let mut data = Vec::new();
    data.extend_from_slice(&NESTED.to_le_bytes());
    push_eq(&mut data);
    push_open(&mut data);
    push_open(&mut data);
    push_close(&mut data);
    data.extend_from_slice(&INNER.to_le_bytes());
    push_eq(&mut data);
    data.extend_from_slice(&LexemeId::I32.0.to_le_bytes());
    data.extend_from_slice(&1i32.to_le_bytes());
    push_close(&mut data);

    let actual: WithGhost = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual, WithGhost { nested: GhostOuter { inner: 1 } });
}

#[test]
fn ghost_object_trailing() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct WithGhost {
        nested: GhostOuter,
    }

    let mut data = Vec::new();
    data.extend_from_slice(&NESTED.to_le_bytes());
    push_eq(&mut data);
    push_open(&mut data);
    data.extend_from_slice(&INNER.to_le_bytes());
    push_eq(&mut data);
    data.extend_from_slice(&LexemeId::I32.0.to_le_bytes());
    data.extend_from_slice(&1i32.to_le_bytes());
    push_open(&mut data);
    push_close(&mut data);
    push_close(&mut data);

    let actual: WithGhost = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual, WithGhost { nested: GhostOuter { inner: 1 } });
}

#[test]
fn ghost_object_multiple() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct MultiGhost {
        flag: bool,
        inner: i32,
    }

    let mut data = Vec::new();
    data.extend_from_slice(&NESTED.to_le_bytes());
    push_eq(&mut data);
    push_open(&mut data);
    push_open(&mut data);
    push_close(&mut data);
    data.extend_from_slice(&FLAG.to_le_bytes());
    push_eq(&mut data);
    data.extend_from_slice(&LexemeId::BOOL.0.to_le_bytes());
    data.push(1);
    push_open(&mut data);
    push_close(&mut data);
    push_open(&mut data);
    push_close(&mut data);
    data.extend_from_slice(&INNER.to_le_bytes());
    push_eq(&mut data);
    data.extend_from_slice(&LexemeId::I32.0.to_le_bytes());
    data.extend_from_slice(&42i32.to_le_bytes());
    push_open(&mut data);
    push_close(&mut data);
    push_close(&mut data);

    #[derive(Debug, Deserialize, PartialEq)]
    struct Outer {
        nested: MultiGhost,
    }
    let actual: Outer = assert_slice_and_reader(&data, make_format);
    assert_eq!(
        actual,
        Outer {
            nested: MultiGhost {
                flag: true,
                inner: 42,
            },
        }
    );
}

#[test]
fn meta_fragment() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct MetaFragment {
        campaign_id: String,
        player: String,
    }
    let data = build_binary(&[
        (CAMPAIGN_ID, Val::Str(b"abc-123")),
        (PLAYER, Val::Str(b"ENG")),
    ]);
    let actual: MetaFragment = assert_slice_and_reader(&data, make_format);
    assert_eq!(
        actual,
        MetaFragment {
            campaign_id: String::from("abc-123"),
            player: String::from("ENG"),
        }
    );
}

#[test]
fn country_tag_parsing() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct TagWrap {
        tag: CountryTag,
    }
    let data = build_binary(&[(TAG, Val::Str(b"FRA"))]);
    let actual: TagWrap = assert_slice_and_reader(&data, make_format);
    assert_eq!(actual.tag, "FRA");
}

#[test]
fn province_id_as_key() {
    let data = build_binary_kv(&[
        (Val::I32(-1), Val::Str(b"Stockholm")),
        (Val::I32(-2), Val::Str(b"Norrland")),
    ]);
    let from_bytes: HashMap<i32, String> = from_slice(&data, make_format()).unwrap();
    let from_stream: HashMap<i32, String> =
        from_reader(ChunkedReader::new(&data, 3), make_format()).unwrap();
    assert_eq!(from_bytes, from_stream);
    assert_eq!(from_bytes.get(&-1), Some(&String::from("Stockholm")));
    assert_eq!(from_bytes.get(&-2), Some(&String::from("Norrland")));
}

#[test]
fn vec_pair_deserialization() {
    #[derive(Debug, Deserialize, PartialEq)]
    struct PairContainer {
        #[serde(deserialize_with = "eu4save::de::deserialize_vec_pair")]
        nested: Vec<(String, i32)>,
    }
    let data = build_binary(&[(
        NESTED,
        Val::Obj(&[(NAME, Val::I32(10)), (VALUE, Val::I32(20))]),
    )]);
    let actual: PairContainer = assert_slice_and_reader(&data, make_format);
    assert_eq!(
        actual,
        PairContainer {
            nested: vec![
                (String::from("name"), 10),
                (String::from("value"), 20),
            ],
        }
    );
}
