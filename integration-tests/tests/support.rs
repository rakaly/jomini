use jomini::binary::ng::{BinaryFormat, from_reader, from_slice};
use jomini::binary::LexemeId;
use serde::Deserialize;
use std::fmt::Debug;
use std::io::Read;

pub const LABEL: u16 = 0x2000;
pub const HAN: u16 = 0x2001;
pub const SCORE32: u16 = 0x2002;
pub const SCORE64: u16 = 0x2003;
pub const FLAG: u16 = 0x2004;
pub const COUNT: u16 = 0x2005;
pub const DELTA: u16 = 0x2006;
pub const WORD: u16 = 0x2007;
pub const NESTED: u16 = 0x2008;
pub const INNER: u16 = 0x2009;
pub const MEMBERS: u16 = 0x200a;
pub const CAMPAIGN_ID: u16 = 0x200b;
pub const PLAYER: u16 = 0x200c;
pub const TAG: u16 = 0x200d;
pub const NAME: u16 = 0x200e;
pub const VALUE: u16 = 0x200f;
pub const ITEMS: u16 = 0x2010;
pub const FIELD1: u16 = 0x2011;
pub const FLAGS: u16 = 0x2012;
pub const OBJ: u16 = 0x2013;
pub const ID: u16 = 0x2014;
pub const TYPE: u16 = 0x2015;
pub const DYNASTY: u16 = 0x2016;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Val<'a> {
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Bool(bool),
    F32(i32),
    F32Wide(i64),
    F64(i64),
    Str(&'a [u8]),
    Unquoted(&'a [u8]),
    Obj(&'a [(u16, Val<'a>)]),
    Arr(&'a [Val<'a>]),
    Ghost,
}

fn push_lexeme(buf: &mut Vec<u8>, id: LexemeId) {
    buf.extend_from_slice(&id.0.to_le_bytes());
}

fn push_val(buf: &mut Vec<u8>, val: &Val<'_>) {
    match val {
        Val::I32(v) => {
            push_lexeme(buf, LexemeId::I32);
            buf.extend_from_slice(&v.to_le_bytes());
        }
        Val::U32(v) => {
            push_lexeme(buf, LexemeId::U32);
            buf.extend_from_slice(&v.to_le_bytes());
        }
        Val::I64(v) => {
            push_lexeme(buf, LexemeId::I64);
            buf.extend_from_slice(&v.to_le_bytes());
        }
        Val::U64(v) => {
            push_lexeme(buf, LexemeId::U64);
            buf.extend_from_slice(&v.to_le_bytes());
        }
        Val::Bool(v) => {
            push_lexeme(buf, LexemeId::BOOL);
            buf.push(u8::from(*v));
        }
        Val::F32(raw) => {
            push_lexeme(buf, LexemeId::F32);
            buf.extend_from_slice(&raw.to_le_bytes());
        }
        Val::F32Wide(raw) => {
            push_lexeme(buf, LexemeId::F32);
            buf.extend_from_slice(&raw.to_le_bytes());
        }
        Val::F64(raw) => {
            push_lexeme(buf, LexemeId::F64);
            buf.extend_from_slice(&raw.to_le_bytes());
        }
        Val::Str(data) => {
            push_lexeme(buf, LexemeId::QUOTED);
            buf.extend_from_slice(&(data.len() as u16).to_le_bytes());
            buf.extend_from_slice(data);
        }
        Val::Unquoted(data) => {
            push_lexeme(buf, LexemeId::UNQUOTED);
            buf.extend_from_slice(&(data.len() as u16).to_le_bytes());
            buf.extend_from_slice(data);
        }
        Val::Obj(children) => {
            push_lexeme(buf, LexemeId::OPEN);
            for (field, child_val) in *children {
                if *field != 0 {
                    buf.extend_from_slice(&field.to_le_bytes());
                    push_lexeme(buf, LexemeId::EQUAL);
                }
                push_val(buf, child_val);
            }
            push_lexeme(buf, LexemeId::CLOSE);
        }
        Val::Arr(values) => {
            push_lexeme(buf, LexemeId::OPEN);
            for v in *values {
                push_val(buf, v);
            }
            push_lexeme(buf, LexemeId::CLOSE);
        }
        Val::Ghost => {
            push_lexeme(buf, LexemeId::OPEN);
            push_lexeme(buf, LexemeId::CLOSE);
        }
    }
}

pub fn build_binary(fields: &[(u16, Val<'_>)]) -> Vec<u8> {
    let mut buf = Vec::new();
    for (field, val) in fields {
        if *field != 0 {
            buf.extend_from_slice(&field.to_le_bytes());
            push_lexeme(&mut buf, LexemeId::EQUAL);
        }
        push_val(&mut buf, val);
    }
    buf
}

pub fn build_binary_kv(entries: &[(Val<'_>, Val<'_>)]) -> Vec<u8> {
    let mut buf = Vec::new();
    for (key, val) in entries {
        push_val(&mut buf, key);
        push_lexeme(&mut buf, LexemeId::EQUAL);
        push_val(&mut buf, val);
    }
    buf
}

pub struct ChunkedReader<'a> {
    data: &'a [u8],
    offset: usize,
    chunk_size: usize,
}

impl<'a> ChunkedReader<'a> {
    pub fn new(data: &'a [u8], chunk_size: usize) -> Self {
        Self {
            data,
            offset: 0,
            chunk_size,
        }
    }
}

impl Read for ChunkedReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let remaining = &self.data[self.offset..];
        if remaining.is_empty() {
            return Ok(0);
        }
        let amt = remaining.len().min(self.chunk_size).min(buf.len());
        buf[..amt].copy_from_slice(&remaining[..amt]);
        self.offset += amt;
        Ok(amt)
    }
}

pub fn assert_slice_and_reader<T, F, Make>(data: &[u8], make_format: Make) -> T
where
    T: for<'de> Deserialize<'de> + PartialEq + Debug,
    F: BinaryFormat,
    Make: Fn() -> F,
{
    let from_bytes: T = from_slice(data, make_format()).unwrap();
    let from_stream: T = from_reader(ChunkedReader::new(data, 3), make_format()).unwrap();
    assert_eq!(from_bytes, from_stream);
    from_bytes
}

pub fn push_eq(buf: &mut Vec<u8>) {
    buf.extend_from_slice(&LexemeId::EQUAL.0.to_le_bytes());
}

pub fn push_open(buf: &mut Vec<u8>) {
    buf.extend_from_slice(&LexemeId::OPEN.0.to_le_bytes());
}

pub fn push_close(buf: &mut Vec<u8>) {
    buf.extend_from_slice(&LexemeId::CLOSE.0.to_le_bytes());
}

#[allow(unused_macros)]
macro_rules! shared_format_tests {
    ($make_format:expr, $make_format_with_strategy:expr) => {
        #[test]
        fn empty_array() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                members: Vec<String>,
            }
            let data = support::build_binary(&[(support::MEMBERS, support::Val::Arr(&[]))]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(actual, Data { members: vec![] });
        }

        #[test]
        fn array_of_objects() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Item {
                inner: i32,
            }
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                items: Vec<Item>,
            }
            let data = support::build_binary(&[(
                support::ITEMS,
                support::Val::Arr(&[
                    support::Val::Obj(&[(support::INNER, support::Val::I32(1))]),
                    support::Val::Obj(&[(support::INNER, support::Val::I32(2))]),
                ]),
            )]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    items: vec![Item { inner: 1 }, Item { inner: 2 }]
                }
            );
        }

        #[test]
        fn optional_field_present() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                field1: Option<String>,
            }
            let data = support::build_binary(&[(support::FIELD1, support::Val::Str(b"ENG"))]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    field1: Some("ENG".to_string())
                }
            );
        }

        #[test]
        fn optional_field_absent() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                field1: Option<String>,
                flag: bool,
            }
            let data = support::build_binary(&[(support::FLAG, support::Val::Bool(true))]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    field1: None,
                    flag: true,
                }
            );
        }

        #[test]
        fn enum_from_string() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            #[serde(rename_all = "camelCase")]
            enum UnitType {
                General,
                Admiral,
            }
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                field1: UnitType,
            }
            let data =
                support::build_binary(&[(support::FIELD1, support::Val::Unquoted(b"general"))]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    field1: UnitType::General
                }
            );
        }

        #[test]
        fn newtype_struct() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct MyTag(String);
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                field1: MyTag,
            }
            let data = support::build_binary(&[(support::FIELD1, support::Val::Str(b"ENG"))]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    field1: MyTag("ENG".to_string())
                }
            );
        }

        #[test]
        fn string_keys_in_map() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                flags: std::collections::HashMap<String, String>,
            }
            let inner = support::build_binary_kv(&[
                (
                    support::Val::Str(b"schools_initiated"),
                    support::Val::Str(b"1444.11.11"),
                ),
                (
                    support::Val::Str(b"lucky_nation"),
                    support::Val::Str(b"1445.2.3"),
                ),
            ]);
            let mut data = Vec::new();
            data.extend_from_slice(&support::FLAGS.to_le_bytes());
            support::push_eq(&mut data);
            support::push_open(&mut data);
            data.extend_from_slice(&inner);
            support::push_close(&mut data);

            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(actual.flags.len(), 2);
            assert_eq!(
                actual.flags.get("schools_initiated"),
                Some(&"1444.11.11".to_string())
            );
            assert_eq!(
                actual.flags.get("lucky_nation"),
                Some(&"1445.2.3".to_string())
            );
        }

        #[test]
        fn object_template() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct TemplateKey {
                id: i32,
                #[serde(rename = "type")]
                key_type: String,
            }

            #[derive(Debug, PartialEq)]
            struct ObjectTemplateList(Vec<(TemplateKey, i32)>);

            impl<'de> serde::Deserialize<'de> for ObjectTemplateList {
                fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
                where
                    D: serde::Deserializer<'de>,
                {
                    struct Vis;
                    impl<'de> serde::de::Visitor<'de> for Vis {
                        type Value = ObjectTemplateList;
                        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                            f.write_str("alternating objects and values")
                        }
                        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                        where
                            A: serde::de::SeqAccess<'de>,
                        {
                            let mut entries = Vec::new();
                            while let Some(key) = seq.next_element::<TemplateKey>()? {
                                let value = seq
                                    .next_element::<i32>()?
                                    .ok_or_else(|| {
                                        serde::de::Error::custom("expected value after key")
                                    })?;
                                entries.push((key, value));
                            }
                            Ok(ObjectTemplateList(entries))
                        }
                    }
                    deserializer.deserialize_seq(Vis)
                }
            }

            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                obj: ObjectTemplateList,
            }

            let inner = support::build_binary_kv(&[
                (
                    support::Val::Obj(&[
                        (support::ID, support::Val::I32(31)),
                        (support::TYPE, support::Val::Unquoted(b"admin")),
                    ]),
                    support::Val::I32(16),
                ),
                (
                    support::Val::Obj(&[
                        (support::ID, support::Val::I32(32)),
                        (support::TYPE, support::Val::Unquoted(b"diplo")),
                    ]),
                    support::Val::I32(18),
                ),
            ]);

            let mut data = Vec::new();
            data.extend_from_slice(&support::OBJ.to_le_bytes());
            support::push_eq(&mut data);
            support::push_open(&mut data);
            data.extend_from_slice(&inner);
            support::push_close(&mut data);

            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    obj: ObjectTemplateList(vec![
                        (
                            TemplateKey {
                                id: 31,
                                key_type: "admin".to_string(),
                            },
                            16
                        ),
                        (
                            TemplateKey {
                                id: 32,
                                key_type: "diplo".to_string(),
                            },
                            18
                        ),
                    ])
                }
            );
        }

        #[test]
        fn duplicated_fields() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct ItemData {
                inner: i32,
            }
            #[derive(Debug, jomini::JominiDeserialize, PartialEq)]
            struct Data {
                #[jomini(duplicated)]
                items: Vec<ItemData>,
                field1: String,
            }
            let data = support::build_binary(&[
                (
                    support::ITEMS,
                    support::Val::Obj(&[(support::INNER, support::Val::I32(1))]),
                ),
                (
                    support::ITEMS,
                    support::Val::Obj(&[(support::INNER, support::Val::I32(2))]),
                ),
                (support::FIELD1, support::Val::Str(b"ENG")),
            ]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    items: vec![ItemData { inner: 1 }, ItemData { inner: 2 }],
                    field1: "ENG".to_string(),
                }
            );
        }

        #[test]
        fn array_without_equals() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                members: Vec<i32>,
            }
            let data = support::build_binary(&[(
                support::MEMBERS,
                support::Val::Arr(&[
                    support::Val::I32(1),
                    support::Val::I32(2),
                    support::Val::I32(3),
                ]),
            )]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(actual.members, vec![1, 2, 3]);
        }

        #[test]
        fn mixed_map_array() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                members: Vec<i32>,
                word: String,
            }
            let data = support::build_binary(&[
                (
                    support::MEMBERS,
                    support::Val::Arr(&[support::Val::I32(10), support::Val::I32(20)]),
                ),
                (support::WORD, support::Val::Unquoted(b"hello")),
            ]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    members: vec![10, 20],
                    word: String::from("hello"),
                }
            );
        }

        #[test]
        fn skip_nested_object() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                flag: bool,
                score32: f32,
            }
            let data = support::build_binary(&[
                (support::FLAG, support::Val::Bool(true)),
                (
                    support::NESTED,
                    support::Val::Obj(&[(support::INNER, support::Val::I32(9))]),
                ),
                (support::WORD, support::Val::Unquoted(b"ignored")),
                (support::SCORE32, support::Val::F32(1234)),
            ]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    flag: true,
                    score32: 1.234
                }
            );
        }

        #[test]
        fn skip_nested_object_with_strings() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                flag: bool,
                score32: f32,
            }
            let data = support::build_binary(&[
                (support::FLAG, support::Val::Bool(true)),
                (
                    support::NESTED,
                    support::Val::Obj(&[(support::LABEL, support::Val::Str(b"abcdef"))]),
                ),
                (support::SCORE32, support::Val::F32(1234)),
            ]);
            let from_bytes: Data =
                jomini::binary::ng::from_slice(&data, ($make_format)()).unwrap();
            let from_stream: Data = jomini::binary::ng::from_reader(
                support::ChunkedReader::new(&data, 21),
                ($make_format)(),
            )
            .unwrap();
            assert_eq!(from_bytes, from_stream);
            let from_small: Data = jomini::binary::ng::from_reader(
                support::ChunkedReader::new(&data, 3),
                ($make_format)(),
            )
            .unwrap();
            assert_eq!(from_bytes, from_small);
            assert_eq!(
                from_bytes,
                Data {
                    flag: true,
                    score32: 1.234,
                }
            );
        }

        #[test]
        fn skip_deeply_nested() {
            #[derive(Debug, serde::Deserialize, PartialEq)]
            struct Data {
                flag: bool,
                score32: f32,
            }
            let data = support::build_binary(&[
                (support::FLAG, support::Val::Bool(true)),
                (
                    support::NESTED,
                    support::Val::Obj(&[(
                        support::INNER,
                        support::Val::Obj(&[(
                            support::LABEL,
                            support::Val::Obj(&[(support::WORD, support::Val::Str(b"deep"))]),
                        )]),
                    )]),
                ),
                (support::SCORE32, support::Val::F32(5000)),
            ]);
            let actual: Data = support::assert_slice_and_reader(&data, $make_format);
            assert_eq!(
                actual,
                Data {
                    flag: true,
                    score32: 5.0,
                }
            );
        }

        #[test]
        fn unknown_field_error() {
            let data = support::build_binary(&[(0x3000, support::Val::I32(7))]);
            let format = ($make_format_with_strategy)(
                jomini::binary::FailedResolveStrategy::Error,
            );
            let err = jomini::binary::ng::from_slice::<
                std::collections::HashMap<String, i32>,
                _,
            >(&data, format)
            .unwrap_err();
            assert!(err.to_string().contains("unknown field token"));
        }

        #[test]
        fn unknown_field_stringify() {
            let data = support::build_binary(&[(0x3000, support::Val::I32(7))]);
            let format = ($make_format_with_strategy)(
                jomini::binary::FailedResolveStrategy::Stringify,
            );
            let actual: std::collections::HashMap<String, i32> =
                jomini::binary::ng::from_slice(&data, format).unwrap();
            assert_eq!(actual.get("0x3000"), Some(&7));
        }

        #[test]
        fn unknown_field_ignore() {
            let data = support::build_binary(&[(0x3000, support::Val::I32(7))]);
            let format = ($make_format_with_strategy)(
                jomini::binary::FailedResolveStrategy::Ignore,
            );
            let actual: std::collections::HashMap<String, i32> =
                jomini::binary::ng::from_slice(&data, format).unwrap();
            assert_eq!(actual.get("__internal_identifier_ignore"), Some(&7));
        }
    };
}
