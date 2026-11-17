use super::*;
use crate::{ErrorKind, Utf8Encoding, Windows1252Encoding, text::Property};
use rstest::rstest;
use serde::{
    Deserialize,
    de::{self, DeserializeOwned, Deserializer as _},
};
use std::{collections::HashMap, fmt};

fn from_windows1252_slice<'a, T: Deserialize<'a>>(data: &'a [u8]) -> Result<T, Error> {
    TextDeserializer::from_slice(data, Windows1252Encoding::new()).deserialize()
}

fn from_windows1252_reader<T: DeserializeOwned>(data: &[u8]) -> Result<T, Error> {
    TextDeserializer::from_reader(data, Windows1252Encoding::new()).deserialize()
}

fn reader_with_buf<T: DeserializeOwned>(data: &[u8], size: usize) -> Result<T, Error> {
    let encoding = Windows1252Encoding::new();
    TextDeserializer::from_reader_with_buf(data, encoding, vec![0; size]).deserialize()
}

/// The buffer sizes in `sizes`, or under miri, which is slow, every third
/// one and the last.
fn buffer_sizes(sizes: std::ops::RangeInclusive<usize>) -> impl Iterator<Item = usize> {
    let (start, end) = sizes.into_inner();
    let step = if cfg!(miri) { 3 } else { 1 };
    (start..end).step_by(step).chain([end])
}

/// A reader that returns one byte per read.
struct Trickle<'a>(&'a [u8]);

impl std::io::Read for Trickle<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match (self.0.split_first(), buf.first_mut()) {
            (Some((&x, rest)), Some(out)) => {
                *out = x;
                self.0 = rest;
                Ok(1)
            }
            _ => Ok(0),
        }
    }
}

fn trickle<T: DeserializeOwned>(data: &[u8], size: usize) -> Result<T, Error> {
    let encoding = Windows1252Encoding::new();
    TextDeserializer::from_reader_with_buf(Trickle(data), encoding, vec![0; size]).deserialize()
}

/// Deserialize `data` from a slice, from a reader, from a reader that
/// returns a byte at a time, and from a reader with each buffer size in
/// `sizes`, and assert that each gives the same value.
fn all_paths<T>(data: &[u8], sizes: std::ops::RangeInclusive<usize>) -> T
where
    T: DeserializeOwned + PartialEq + fmt::Debug,
{
    let expected: T = from_windows1252_slice(data).unwrap();
    assert_eq!(from_windows1252_reader::<T>(data).unwrap(), expected);
    assert_eq!(trickle::<T>(data, 4096).unwrap(), expected);
    for size in buffer_sizes(sizes) {
        let actual: T = reader_with_buf(data, size).unwrap();
        assert_eq!(actual, expected, "buffer size {size}");
        let actual: T = trickle(data, size).unwrap();
        assert_eq!(actual, expected, "trickle buffer size {size}");
    }
    expected
}

#[derive(Debug, PartialEq)]
enum AnyValue {
    Str(String),
    Seq(Vec<AnyValue>),
    Map(Vec<(String, AnyValue)>),
}

struct AnyVisitor;

impl<'de> de::Visitor<'de> for AnyVisitor {
    type Value = AnyValue;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any value")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        Ok(AnyValue::Str(String::from(v)))
    }

    fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
        let mut values = Vec::new();
        while let Some(value) = seq.next_element()? {
            values.push(value);
        }
        Ok(AnyValue::Seq(values))
    }

    fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let mut entries = Vec::new();
        while let Some(entry) = map.next_entry()? {
            entries.push(entry);
        }
        Ok(AnyValue::Map(entries))
    }
}

impl<'de> Deserialize<'de> for AnyValue {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_any(AnyVisitor)
    }
}

/// Writes `{key=value}` for maps and `[a b]` for sequences.
impl fmt::Display for AnyValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AnyValue::Str(x) => f.write_str(x),
            AnyValue::Seq(values) => {
                f.write_str("[")?;
                for (i, value) in values.iter().enumerate() {
                    let sep = if i == 0 { "" } else { " " };
                    write!(f, "{sep}{value}")?;
                }
                f.write_str("]")
            }
            AnyValue::Map(entries) => {
                f.write_str("{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    let sep = if i == 0 { "" } else { " " };
                    write!(f, "{sep}{key}={value}")?;
                }
                f.write_str("}")
            }
        }
    }
}

/// The `deserialize_any` table of the streaming reader in `text::de`, over
/// a slice and over buffers small enough that refills land inside the
/// lookahead. The lookahead drops the whitespace and comments it looks
/// past, so the buffer needs to hold the longest token and the byte after
/// it: exactly the buffers smaller than `min_buf` fail with
/// `BufferTooSmall`.
#[rstest]
#[case::block_kind(
    b"object={ a=b c<2 } array={ 1 2 } empty={} objects={ { a=b } { c=d } } empty_key={ =b }",
    "{object={a=b c=2} array=[1 2] empty=[] objects=[{a=b} {c=d}] empty_key={=b}}",
    10
)]
// An empty block is an element of a sequence, as in a typed sequence, and
// not a value in an object.
#[case::empty_blocks(
    b"leading={ {} {} 3 } only={ {} {} } object={ {} a=b }",
    "{leading=[[] [] 3] only=[[] []] object={a=b}}",
    8
)]
#[case::key_block_field(
    b"later={ a=b c { d=e } } first={ c { d=e } }",
    "{later={a=b c={d=e}} first=[c {d=e}]}",
    6
)]
#[case::header(
    b"color = rgb { 1 2 3 } object = obj { a = b } empty = x {} list = { rgb { 1 } }",
    "{color=[1 2 3] object={a=b} empty=x list=[rgb [1]]}",
    7
)]
#[case::comment(
    b"object={ a # note\n = b }\ncolor = rgb # note\n { 1 }",
    "{object={a=b} color=[1]}",
    7
)]
#[case::exact_operator(b"object={ a == b c ==d }", "{object={a=b c=d}}", 7)]
#[case::remainder(b"object={ a=b c d }", "{object={a=b remainder=[c d]}}", 7)]
#[case::field_after_remainder(b"object={ a=b c d=e }", "{object={a=b remainder=[c d = e]}}", 7)]
#[case::operator_in_array(b"array={ a b=c }", "{array=[a b = c]}", 6)]
#[case::root(b"a=b }\nc={ d=e } f=rgb { 1 }", "{a=b c={d=e} f=[1]}", 4)]
#[case::root_remainder(b"a=b c d", "{a=b remainder=[c d]}", 4)]
#[case::empty(b"", "{}", 4)]
#[case::refill_in_lookahead(
    b"first={ a_long_key\n\n\t\t= value }\nsecond={ alpha # comment\n  beta }\ncolor = a_header_scalar  \n\n  { 1 2 }\nlast=z",
    "{first={a_long_key=value} second=[alpha beta] color=[1 2] last=z}",
    16
)]
#[case::remainder_after_skipped_block(b"v={ s={ } { } 2 } w=1", "{v={s=[] remainder=[2]} w=1}", 4)]
#[case::refill_after_replay(
    b"list={ alpha beta gamma delta epsilon zeta eta theta }\nobj={ k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6 }\nrem={ a=b c d e f g h i j k l }\ncolor=hdr { 1 2 3 4 5 6 7 8 9 10 11 12 }",
    "{list=[alpha beta gamma delta epsilon zeta eta theta] obj={k1=v1 k2=v2 k3=v3 k4=v4 k5=v5 k6=v6} rem={a=b remainder=[c d e f g h i j k l]} color=[1 2 3 4 5 6 7 8 9 10 11 12]}",
    8
)]
fn test_any(#[case] data: &[u8], #[case] expected: &str, #[case] min_buf: usize) {
    let actual: AnyValue = from_windows1252_slice(data).unwrap();
    assert_eq!(actual.to_string(), expected);
    for size in buffer_sizes(4..=40) {
        match reader_with_buf::<AnyValue>(data, size) {
            Ok(actual) => {
                assert_eq!(actual.to_string(), expected, "buffer size {size}");
                assert!(size >= min_buf, "buffer size {size}");
            }
            Err(e) => {
                assert!(matches!(e.kind(), ErrorKind::BufferTooSmall), "{e}");
                assert!(size < min_buf, "buffer size {size}");
            }
        }
    }

    // The old reader gives the same values, except that it drops the empty
    // blocks at the start of a sequence and the operators in a remainder.
    if !expected.contains("[] ") && !expected.contains("remainder=") {
        let reader = crate::text::TokenReader::from_slice(data);
        let mut old = crate::TextTapeDeserializer::from_windows1252_reader(reader);
        let old = AnyValue::deserialize(&mut old).unwrap().to_string();
        assert_eq!(old, expected);
    }
}

#[test]
fn test_borrowed_str() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct MyStruct<'a> {
        name: &'a str,
        #[serde(borrow)]
        tags: Vec<&'a str>,
    }

    let data = b"name=\"Jolly John\" tags={ abc \"d e\" }";
    let actual: MyStruct = from_windows1252_slice(data).unwrap();
    assert_eq!(
        actual,
        MyStruct {
            name: "Jolly John",
            tags: vec!["abc", "d e"],
        }
    );

    // The str points into the input.
    let range = data.as_ptr_range();
    assert!(range.contains(&actual.name.as_ptr()));

    // A scalar that must be decoded or unescaped can't be borrowed.
    let data = b"name=\"Jolly \\\"John\\\"\" tags={}";
    let err = from_windows1252_slice::<MyStruct>(data).unwrap_err();
    assert!(
        err.to_string().contains("expected a borrowed string"),
        "{err}"
    );
}

#[test]
fn test_borrowed_and_copied_bytes() {
    struct Bytes(Vec<u8>, bool);

    impl<'de> Deserialize<'de> for Bytes {
        fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct BytesVisitor;
            impl<'de> de::Visitor<'de> for BytesVisitor {
                type Value = Bytes;
                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("bytes")
                }
                fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Bytes, E> {
                    Ok(Bytes(v.to_vec(), true))
                }
                fn visit_bytes<E>(self, v: &[u8]) -> Result<Bytes, E> {
                    Ok(Bytes(v.to_vec(), false))
                }
            }
            deserializer.deserialize_bytes(BytesVisitor)
        }
    }

    #[derive(Deserialize)]
    struct MyStruct {
        a: Bytes,
    }

    let data = b"a=\"x\\\"y\"";
    let actual: MyStruct = from_windows1252_slice(data).unwrap();
    assert_eq!((actual.a.0.as_slice(), actual.a.1), (&b"x\\\"y"[..], true));
    let actual: MyStruct = from_windows1252_reader(&data[..]).unwrap();
    assert_eq!((actual.a.0.as_slice(), actual.a.1), (&b"x\\\"y"[..], false));
}

type Header = (String, Vec<String>);

fn header() -> Header {
    let numbers = ["10", "20", "30", "40", "50", "60", "70", "80", "90"];
    (String::from("rgb"), numbers.map(String::from).to_vec())
}

#[derive(Deserialize, PartialEq, Debug)]
struct HeaderNewtype(Header);

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct HeaderFields {
    value: Option<Header>,
    property: Option<Property<Header>>,
    keys: HashMap<Header, String>,
    elements: Vec<Header>,
    options: Vec<Option<Header>>,
    newtypes: Vec<HeaderNewtype>,
}

/// A header and its block read into a sequence type, in each position that
/// a header can take, with buffers small enough that the source refills
/// inside the block while the header is read.
#[rstest]
#[case::value(b"value = rgb { 10 20 30 40 50 60 70 80 90 }", HeaderFields {
    value: Some(header()),
    ..HeaderFields::default()
})]
#[case::property(b"property < rgb { 10 20 30 40 50 60 70 80 90 }", HeaderFields {
    property: Some(Property::new(Operator::LessThan, header())),
    ..HeaderFields::default()
})]
#[case::key(b"keys = { rgb { 10 20 30 40 50 60 70 80 90 } = x }", HeaderFields {
    keys: HashMap::from([(header(), String::from("x"))]),
    ..HeaderFields::default()
})]
#[case::element(b"elements = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
    elements: vec![header()],
    ..HeaderFields::default()
})]
#[case::option_element(b"options = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
    options: vec![Some(header())],
    ..HeaderFields::default()
})]
#[case::newtype_element(b"newtypes = { rgb { 10 20 30 40 50 60 70 80 90 } }", HeaderFields {
    newtypes: vec![HeaderNewtype(header())],
    ..HeaderFields::default()
})]
fn test_header_seq(#[case] data: &[u8], #[case] expected: HeaderFields) {
    let actual: HeaderFields = all_paths(data, 12..=24);
    assert_eq!(actual, expected);
}

const EMPTY_KEY_FIRST: &[u8] = b"cooldowns={\n\t=1825.1.11\n\ttrade_favors_for_men=1826.6.22\n}";
const EMPTY_KEY_EXACT: &[u8] =
    b"idea_dates={\n\t==-5000.1.1\n\tnoble_knights=1461.11.17\n\tmilitary_traditions=1473.5.13\n}";

#[test]
fn test_empty_key() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Cooldowns {
        cooldowns: HashMap<String, String>,
    }

    let actual: Cooldowns = all_paths(EMPTY_KEY_FIRST, 21..=32);
    let expected = HashMap::from([
        (String::from(""), String::from("1825.1.11")),
        (
            String::from("trade_favors_for_men"),
            String::from("1826.6.22"),
        ),
    ]);
    assert_eq!(actual.cooldowns, expected);

    #[derive(Deserialize, PartialEq, Debug)]
    struct Ideas {
        idea_dates: HashMap<String, Property<String>>,
    }

    let actual: Ideas = all_paths(EMPTY_KEY_EXACT, 20..=32);
    assert_eq!(
        actual.idea_dates[""],
        Property::new(Operator::Exact, String::from("-5000.1.1"))
    );
    assert_eq!(
        actual.idea_dates["noble_knights"],
        Property::new(Operator::Equal, String::from("1461.11.17"))
    );

    // An object read as a sequence is an error, where the old streaming
    // reader gives each token, operators too.
    #[derive(Deserialize, PartialEq, Debug)]
    struct Seq {
        cooldowns: Vec<String>,
    }

    assert_eq!(
        all_paths_err::<Seq>(EMPTY_KEY_FIRST, 21..=32),
        "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 13)"
    );
}

#[test]
fn test_skips_field_with_block_key() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct MyStruct {
        obj: HashMap<String, String>,
    }

    let data = b"obj={ { a = b }={ 1 2 3 } { c = d }=16 e=f }";
    let actual: MyStruct = all_paths(data, 8..=16);
    let expected = HashMap::from([(String::from("e"), String::from("f"))]);
    assert_eq!(actual, MyStruct { obj: expected });
}

/// Deserialize `data` with a `TextTape`.
fn from_tape<T: DeserializeOwned>(data: &[u8]) -> Result<T, Error> {
    crate::TextTapeDeserializer::from_windows1252_slice(data)?.deserialize()
}

#[derive(Deserialize, PartialEq, Debug)]
struct RootFields {
    a: Option<u32>,
    b: Option<u32>,
}

/// A `}` at the root, which closes no block, is skipped wherever it is, as a
/// `TextTape` does, by a struct, a map, and `deserialize_any`. A `}` in a
/// block closes the block.
#[rstest]
#[case::between_fields(b"a=1 } b=2", (Some(1), Some(2)), "{a=1 b=2}")]
#[case::after_block(b"c={ x=y } } a=1 b=2", (Some(1), Some(2)), "{c={x=y} a=1 b=2}")]
#[case::first(b"} a=1", (Some(1), None), "{a=1}")]
#[case::last(b"a=1 }", (Some(1), None), "{a=1}")]
#[case::repeated(b"a=1 } } b=2", (Some(1), Some(2)), "{a=1 b=2}")]
fn test_close_at_root(
    #[case] data: &[u8],
    #[case] expected: (Option<u32>, Option<u32>),
    #[case] any: &str,
) {
    let (a, b) = expected;
    let actual: RootFields = all_paths(data, 4..=16);
    assert_eq!(actual, RootFields { a, b });
    assert_eq!(from_tape::<RootFields>(data).unwrap(), actual);

    // A `TextTape` reads no root with `deserialize_any`.
    let actual: AnyValue = all_paths(data, 4..=16);
    assert_eq!(actual.to_string(), any);
    let AnyValue::Map(entries) = actual else {
        panic!("the root is a map");
    };
    let mut keys: Vec<_> = entries.iter().map(|(key, _)| key.clone()).collect();
    keys.sort_unstable();

    let actual: HashMap<String, AnyValue> = all_paths(data, 4..=16);
    assert_eq!(actual, entries.into_iter().collect());
    assert_eq!(
        from_tape::<HashMap<String, AnyValue>>(data).unwrap(),
        actual
    );

    let actual: HashMap<String, de::IgnoredAny> = all_paths(data, 4..=16);
    let mut actual: Vec<_> = actual.into_keys().collect();
    actual.sort_unstable();
    assert_eq!(actual, keys);
}

/// A block at the start of the root that is not empty is a value without
/// a key, which starts the remainder of the root, as in a file of blocks
/// without keys, and a `}` at the root in the remainder is skipped too. An
/// operator in the remainder needs a value after it, as after any operator.
#[rstest]
#[case(b"{ x } } b=2", Ok("{remainder=[[x] b = 2]}"))]
#[case(b"{ x }= } b=2", Err("did not expect end (offset: 7)"))]
fn test_close_at_root_after_block_key(#[case] data: &[u8], #[case] any: Result<&str, &str>) {
    match any {
        Ok(any) => {
            assert_eq!(all_paths::<AnyValue>(data, 4..=16).to_string(), any);
            let actual: RootFields = all_paths(data, 4..=16);
            assert_eq!(actual, RootFields { a: None, b: None });
            let actual: HashMap<String, de::IgnoredAny> = all_paths(data, 4..=16);
            assert_eq!(actual.keys().collect::<Vec<_>>(), ["remainder"]);
        }
        Err(err) => {
            assert!(all_paths_err::<AnyValue>(data, 4..=16).ends_with(err));
            assert!(all_paths_err::<RootFields>(data, 4..=16).ends_with(err));
        }
    }
}

/// An empty block at the start of the root is skipped, as a `TextTape`
/// does.
#[test]
fn test_empty_block_at_root() {
    let actual: RootFields = all_paths(b"{ } b=2", 4..=16);
    assert_eq!(
        actual,
        RootFields {
            a: None,
            b: Some(2)
        }
    );
    let actual: AnyValue = all_paths(b"{ } b=2", 4..=16);
    assert_eq!(actual.to_string(), "{b=2}");
}

/// The scalars that trail the fields of the root run past a `}` there.
#[test]
fn test_close_at_root_in_remainder() {
    // The scalars that trail the fields of the root run past it.
    let actual: AnyValue = all_paths(b"a=1 c } d", 4..=16);
    assert_eq!(actual.to_string(), "{a=1 remainder=[c d]}");
}

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct Unclosed {
    list: Vec<u32>,
    map: HashMap<String, u32>,
    nested: Option<UnclosedNested>,
    pair: Option<(u8, u8)>,
    header: Option<Header>,
    color: Option<Color>,
    last: Option<u32>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct UnclosedNested {
    c: u32,
    d: HashMap<String, u32>,
}

/// The end of the data closes every block that is open, objects and
/// arrays, nested to any depth. A `TextTape` closes only one object that
/// the end of the data leaves open.
#[rstest]
#[case::object(b"map={ b=1", Unclosed {
    map: HashMap::from([(String::from("b"), 1)]),
    ..Unclosed::default()
}, true)]
#[case::array(b"list={ 1 2", Unclosed {
    list: vec![1, 2],
    ..Unclosed::default()
}, false)]
#[case::empty(b"list={", Unclosed::default(), false)]
#[case::nested(b"nested={ c=2 d={ e=1", Unclosed {
    nested: Some(UnclosedNested {
        c: 2,
        d: HashMap::from([(String::from("e"), 1)]),
    }),
    ..Unclosed::default()
}, false)]
#[case::tuple(b"pair={ 1 2", Unclosed {
    pair: Some((1, 2)),
    ..Unclosed::default()
}, false)]
#[case::header(b"header=rgb { 10 20 30 40 50 60 70 80 90", Unclosed {
    header: Some(header()),
    ..Unclosed::default()
}, false)]
#[case::header_enum(b"color=rgb { 10 20 30", Unclosed {
    color: Some(Color::Rgb(10, 20, 30)),
    ..Unclosed::default()
}, false)]
#[case::skipped(b"last=3 skipped={ a={ b c # d", Unclosed {
    last: Some(3),
    ..Unclosed::default()
}, false)]
#[case::skipped_key(b"last=3 { a={ b c", Unclosed {
    last: Some(3),
    ..Unclosed::default()
}, false)]
fn test_end_closes_blocks(#[case] data: &[u8], #[case] expected: Unclosed, #[case] tape: bool) {
    let actual: Unclosed = all_paths(data, 12..=32);
    assert_eq!(actual, expected);
    let tape_value = from_tape::<Unclosed>(data);
    assert_eq!(
        tape_value.as_ref().ok(),
        tape.then_some(&expected),
        "{tape_value:?}"
    );
}

/// `deserialize_any` gives the blocks that the end of the data closes.
#[rstest]
#[case::object(b"a={ b=1", "{a={b=1}}")]
#[case::array(b"a={ 1 2", "{a=[1 2]}")]
#[case::empty(b"a={", "{a=[]}")]
#[case::nested(b"x={ c=2 d={ e=1", "{x={c=2 d={e=1}}}")]
#[case::nested_array(b"x={ { 1 2 } { 3", "{x=[[1 2] [3]]}")]
#[case::remainder(b"x={ a=b c d", "{x={a=b remainder=[c d]}}")]
#[case::header(b"color = rgb { 1 2", "{color=[1 2]}")]
#[case::header_empty(b"color = rgb {", "{color=rgb}")]
#[case::block_key(b"a={ 1 } { b=2", "{a=[1] remainder=[{b=2}]}")]
fn test_end_closes_blocks_any(#[case] data: &[u8], #[case] expected: &str) {
    let actual: AnyValue = all_paths(data, 8..=24);
    assert_eq!(actual.to_string(), expected);
}

/// The end of the data is still an error in a quoted string, also in a
/// skipped block, and where a value is expected.
#[rstest]
#[case::quote(b"last=\"1 2", 5)]
#[case::skipped_quote(b"skipped={ a=\"b c", 16)]
#[case::missing_value(b"map={ b=", 8)]
#[case::missing_header_block(b"color=rgb {", 6)]
fn test_end_errors(#[case] data: &[u8], #[case] offset: usize) {
    let check = |err: Error| {
        if offset == 6 {
            assert!(err.to_string().contains("enum variant with data"), "{err}");
        } else {
            assert!(matches!(err.kind(), ErrorKind::Eof), "{err}");
        }
        let expected = if offset == 6 { offset } else { data.len() };
        assert_eq!(err.offset(), Some(expected), "{err}");
    };
    check(from_windows1252_slice::<Unclosed>(data).unwrap_err());
    check(from_windows1252_reader::<Unclosed>(data).unwrap_err());
    for size in buffer_sizes(12..=32) {
        check(reader_with_buf::<Unclosed>(data, size).unwrap_err());
        check(trickle::<Unclosed>(data, size).unwrap_err());
    }
}

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct Trailing {
    inner: Option<TrailingInner>,
    keys: Option<HashMap<String, de::IgnoredAny>>,
    list: Option<Vec<String>>,
    after: Option<u32>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct TrailingInner {
    known: u32,
    #[serde(default)]
    opt: Option<u32>,
}

/// A value without a key, as a scalar that trails the fields of an object,
/// starts the remainder of the object, and the `}` after it closes its
/// block, so the fields after the block stay in their place. A scalar read
/// as a sequence is the scalar alone unless a block follows it.
#[rstest]
#[case::ignored(b"inner = { known = 1 stray } after = 2", Trailing {
    inner: Some(TrailingInner { known: 1, opt: None }),
    after: Some(2),
    ..Trailing::default()
}, true)]
#[case::scalar_seq(b"list = x after = 2", Trailing {
    list: Some(vec![String::from("x")]),
    after: Some(2),
    ..Trailing::default()
}, false)]
#[case::scalar_seq_empty_block(b"list = x { } after = 2", Trailing {
    list: Some(vec![String::from("x")]),
    after: Some(2),
    ..Trailing::default()
}, false)]
#[case::scalar_seq_in_block(b"inner = { known = 1 } list = x } after = 2", Trailing {
    inner: Some(TrailingInner { known: 1, opt: None }),
    list: Some(vec![String::from("x")]),
    after: Some(2),
    ..Trailing::default()
}, false)]
fn test_value_never_closes_block(
    #[case] data: &[u8],
    #[case] expected: Trailing,
    #[case] tape: bool,
) {
    let actual: Trailing = all_paths(data, 8..=24);
    assert_eq!(actual, expected);
    if tape {
        assert_eq!(from_tape::<Trailing>(data).unwrap(), expected);
    }
}

/// A value without a key in a map that is not a struct, which has no field
/// for the remainder, is an error at the value, where the lossless parser
/// gives the map the key `remainder`.
#[rstest]
#[case::map(b"keys = { k } after = 2", 9)]
#[case::end(b"after = 2 keys = { k", 19)]
#[case::first(b"keys = { k a = b } after = 2", 9)]
fn test_map_key_without_value_error(#[case] data: &[u8], #[case] offset: usize) {
    assert_eq!(
        all_paths_err::<Trailing>(data, 8..=24),
        format!(
            "TextDeserializer doesn't read a value without a key, as the `80` of `{{ 80 0=1 }}`, except into the `remainder` field of a struct, which jomini::text::syntax::parse reads (offset: {offset})"
        )
    );
}

/// A value without a key that matches a field is an error.
#[test]
fn test_key_without_value_error() {
    let data = b"inner = { known = 1 opt } after = 2";
    assert_eq!(
        all_paths_err::<Trailing>(data, 8..=24),
        "TextDeserializer doesn't read a value without a key, as the `80` of `{ 80 0=1 }`, except into the `remainder` field of a struct, which jomini::text::syntax::parse reads (offset: 20)"
    );
}

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct Sparse {
    remainder: Vec<String>,
    #[serde(rename = "0")]
    zero: Option<String>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct SparseRoot {
    x: Sparse,
    after: Option<u32>,
}

/// A value without a key in an object, as the length that starts a sparse
/// array in a save, `income_taxes={ 80 0=278.75 1=433.88 }`, starts the
/// remainder of the object: a map yields each part of each entry after it
/// as a sequence under the key `remainder`, and ends. The values are those
/// of the lossless parser.
#[rstest]
#[case::leading(b"x={ 80 0=1.5 1=2.5 } after=3", &["80", "0", "=", "1.5", "1", "=", "2.5"], None)]
#[case::trailing(b"x={ 0=1.5 1=2.5 80 } after=3", &["80"], Some("1.5"))]
#[case::trailing_two(b"x={ a=b c d } after=3", &["c", "d"], None)]
#[case::quoted(b"x={ \"80\" 0 = 1.5 } after=3", &["80", "0", "=", "1.5"], None)]
#[case::comment(b"x={ 80 # length\n 0=1.5 } after=3", &["80", "0", "=", "1.5"], None)]
fn test_remainder(#[case] data: &[u8], #[case] remainder: &[&str], #[case] zero: Option<&str>) {
    let actual: SparseRoot = all_paths(data, 8..=24);
    let expected = SparseRoot {
        x: Sparse {
            remainder: remainder.iter().map(|x| String::from(*x)).collect(),
            zero: zero.map(String::from),
        },
        after: Some(3),
    };
    assert_eq!(actual, expected);

    // A struct that ignores the object, or its remainder.
    #[derive(Deserialize, PartialEq, Debug)]
    struct Ignored {
        after: Option<u32>,
    }
    assert_eq!(all_paths::<Ignored>(data, 8..=24).after, Some(3));

    // A map that is not a struct has no field for the remainder, so it is
    // an error, where the lossless parser gives it the key `remainder`.
    #[derive(Deserialize, PartialEq, Debug)]
    struct NoRemainder {
        x: HashMap<String, de::IgnoredAny>,
        after: Option<u32>,
    }
    assert!(all_paths_err::<NoRemainder>(data, 8..=24).contains("jomini::text::syntax::parse"));
}

/// The remainder holds each part of each entry, and blocks as values.
#[test]
fn test_remainder_parts() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Parts {
        x: PartsInner,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct PartsInner {
        a: String,
        remainder: Vec<AnyValue>,
    }

    let data = b"x={ a=b c d={ e=f } g { 1 2 } h }";
    let actual: Parts = all_paths(data, 8..=24);
    let parts: Vec<String> = actual.x.remainder.iter().map(|x| x.to_string()).collect();
    assert_eq!(parts, ["c", "d", "=", "{e=f}", "g", "[1 2]", "h"]);
    assert_eq!(actual.x.a, "b");

    // At the root, the remainder runs to the end of the data, past a `}`
    // that closes no block.
    #[derive(Deserialize, PartialEq, Debug)]
    struct RootParts {
        a: String,
        remainder: Vec<String>,
    }
    let actual: RootParts = all_paths(b"a=b 80 } c=d", 8..=24);
    assert_eq!(actual.remainder, ["80", "c", "=", "d"]);

    // A map of strings, which can't hold the sequence, is an error.
    for data in [&b"x={ 80 0=1.5 1=2.5 }"[..], b"x={ b c }", b"x={ a=b c d }"] {
        let err = from_windows1252_slice::<HashMap<String, HashMap<String, String>>>(data);
        assert!(err.is_err(), "{err:?}");
    }
}

/// A `}` where a value is expected is an error for every type, also an
/// ignored one, and is not read as the value.
#[rstest]
#[case::ignored(b"inner = { known = 1 skipped = } after = 2", 30)]
#[case::option(b"inner = { known = 1 opt = } after = 2", 26)]
#[case::seq(b"list = } after = 2", 7)]
#[case::map(b"keys = } after = 2", 7)]
fn test_close_is_not_a_value(#[case] data: &[u8], #[case] offset: usize) {
    let expected = format!("invalid syntax encountered: did not expect end (offset: {offset})");
    let err = from_windows1252_slice::<Trailing>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(8..=24) {
        let err = reader_with_buf::<Trailing>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "buffer size {size}");
        let err = trickle::<Trailing>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "trickle buffer size {size}");
    }
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
enum Color {
    Rgb(u8, u8, u8),
    Hsv(Vec<f32>),
    Point { x: u8, y: u8 },
    Named(Nested2),
    None,
}

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct Colors {
    color: Option<Color>,
    property: Option<Property<Color>>,
    named: HashMap<String, Color>,
    list: Vec<Color>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct RequiredColor {
    color: Color,
}

/// An enum variant with data is a field value with a header, whose block is
/// the data, as a `TextTape` reads it, with buffers small enough that the
/// source refills between the variant and its block and inside the block.
#[rstest]
#[case::tuple(b"color = rgb { 10 20 30 }", Colors {
    color: Some(Color::Rgb(10, 20, 30)),
    ..Colors::default()
})]
#[case::newtype_seq(b"color = hsv { 0.25 1 2.5 }", Colors {
    color: Some(Color::Hsv(vec![0.25, 1.0, 2.5])),
    ..Colors::default()
})]
#[case::newtype_struct(b"color = named { id = 17 }", Colors {
    color: Some(Color::Named(Nested2 { id: 17 })),
    ..Colors::default()
})]
#[case::struct_variant(b"color = point { y = 2 x = 1 }", Colors {
    color: Some(Color::Point { x: 1, y: 2 }),
    ..Colors::default()
})]
#[case::unit(b"color = none", Colors {
    color: Some(Color::None),
    ..Colors::default()
})]
#[case::unit_with_block(b"color = none { 1 2 }", Colors {
    color: Some(Color::None),
    ..Colors::default()
})]
#[case::comment(b"color = rgb # a comment that is longer than the buffer\n { 10 20 30 }", Colors {
    color: Some(Color::Rgb(10, 20, 30)),
    ..Colors::default()
})]
#[case::property(b"property < rgb { 10 20 30 }", Colors {
    property: Some(Property::new(Operator::LessThan, Color::Rgb(10, 20, 30))),
    ..Colors::default()
})]
#[case::map_value(b"named = { a = rgb { 10 20 30 } b = none }", Colors {
    named: HashMap::from([
        (String::from("a"), Color::Rgb(10, 20, 30)),
        (String::from("b"), Color::None),
    ]),
    ..Colors::default()
})]
#[case::unit_elements(b"list = { none none }", Colors {
    list: vec![Color::None, Color::None],
    ..Colors::default()
})]
#[case::later_fields(b"color = rgb { 10 20 30 } list = { none }", Colors {
    color: Some(Color::Rgb(10, 20, 30)),
    list: vec![Color::None],
    ..Colors::default()
})]
fn test_header_enum(#[case] data: &[u8], #[case] expected: Colors) {
    let actual: Colors = all_paths(data, 12..=32);
    assert_eq!(actual, expected);
    assert_eq!(from_tape::<Colors>(data).unwrap(), expected);

    if let Some(color) = expected.color {
        let actual: RequiredColor = all_paths(data, 12..=32);
        assert_eq!(actual.color, color);
    }
}

#[derive(Deserialize, PartialEq, Debug, Default)]
#[serde(default)]
struct Script {
    my_effect: Option<Effect>,
    a: Option<u32>,
    b: Option<u32>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Effect {
    add_gold: u32,
}

/// The error from each path, which must be the same.
fn all_paths_err<T>(data: &[u8], sizes: std::ops::RangeInclusive<usize>) -> String
where
    T: DeserializeOwned + fmt::Debug,
{
    let expected = from_windows1252_slice::<T>(data).unwrap_err().to_string();
    let err = from_windows1252_reader::<T>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(sizes) {
        let err = reader_with_buf::<T>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "buffer size {size}");
        let err = trickle::<T>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "trickle buffer size {size}");
    }
    expected
}

/// A parameter block of script, as in `[[scope] set_variable = x ]`, is an
/// error that names the lossless parser, wherever it is read, so the fields
/// after it are never lost. A block that holds one is skipped as any other.
#[rstest]
#[case::in_block(b"my_effect = { [[scope] set_variable = x ] add_gold = 1 }", 14)]
#[case::root(b"a=1 [[x] y=1 ] b=2", 4)]
#[case::first(b"[[x] a=3 ] b=2", 0)]
#[case::value(b"a = [[x] 1 ] b=2", 4)]
fn test_parameter_block_error(#[case] data: &[u8], #[case] offset: usize) {
    let expected = format!(
        "TextDeserializer doesn't read a parameter block or a code block, as in `[[name] effect = x ]` or `code [[ x ]]`, which jomini::text::syntax::parse reads (offset: {offset})"
    );
    assert_eq!(all_paths_err::<Script>(data, 10..=24), expected);
    assert_eq!(all_paths_err::<AnyValue>(data, 10..=24), expected);

    let data = b"skipped = { [[x] y={ \"}\" } ] } b=2";
    let expected = Script {
        b: Some(2),
        ..Script::default()
    };
    assert_eq!(all_paths::<Script>(data, 10..=24), expected);
    assert_eq!(from_tape::<Script>(data).unwrap(), expected);
}

#[derive(Deserialize, PartialEq, Debug)]
struct Area {
    color: Vec<u8>,
    remainder: Vec<u16>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct ObjectKey {
    a: String,
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
enum BlockEnum {
    Rgb(u8, u8, u8),
}

/// The errors for data that only the lossless parser reads name it: an
/// object read as a sequence, as for the scalars that trail its fields or
/// an object used as a key, and an enum written as a block. The scalars
/// that trail the fields of an object are read into `remainder`.
#[test]
fn test_lossless_hints() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Remainder {
        area: Area,
    }

    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct Sequence {
        area: Vec<u16>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct Key {
        obj: (ObjectKey, Vec<i32>),
    }

    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct Enum {
        a: BlockEnum,
    }

    let area = b"area = { color = { 1 2 3 } 169 170 }";
    let expected = Area {
        color: vec![1, 2, 3],
        remainder: vec![169, 170],
    };
    assert_eq!(all_paths::<Remainder>(area, 10..=24).area, expected);
    assert_eq!(from_tape::<Remainder>(area).unwrap().area, expected);

    assert_eq!(
        all_paths_err::<Sequence>(area, 10..=24),
        "invalid type: string \"color\", expected u16; TextDeserializer doesn't read an object as a sequence, as the scalars that trail its fields or an object used as a key, which jomini::text::syntax::parse reads (offset: 9)"
    );

    // The lookahead past the field for its operator, through a comment,
    // makes room in a buffer that the field fills.
    let long = b"area = { abcdefghijklmn# a comment longer than the buffer\n = 1 }";
    assert_eq!(
        all_paths_err::<Sequence>(long, 15..=24),
        "invalid type: string \"abcdefghijklmn\", expected u16; TextDeserializer doesn't read an object as a sequence, as the scalars that trail its fields or an object used as a key, which jomini::text::syntax::parse reads (offset: 9)"
    );

    let key = b"obj = { { a = b }={ 1 2 3 } }";
    assert_eq!(
        all_paths_err::<Key>(key, 10..=24),
        "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 17)"
    );
    assert!(from_tape::<Key>(key).is_ok());

    let block = b"a = { rgb = { 1 2 3 } }";
    assert_eq!(
        all_paths_err::<Enum>(block, 10..=24),
        "invalid type: sequence, expected variant identifier; TextDeserializer doesn't read an enum written as a block, as in `a = { rgb = { 1 2 3 } }`, which jomini::text::syntax::parse reads (offset: 4)"
    );
    assert!(from_tape::<Enum>(block).is_ok());
}

/// A block that a visitor rejects without reading it, as a number, is
/// skipped, so a value that defaults on an error is followed by the fields
/// after the block, as from a `TextTape`.
#[test]
fn test_rejected_block_is_skipped() {
    fn lenient<'de, D: de::Deserializer<'de>>(deserializer: D) -> Result<i32, D::Error> {
        Ok(i32::deserialize(deserializer).unwrap_or(0))
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Inner {
        b: i32,
        #[serde(deserialize_with = "lenient")]
        a: i32,
        #[serde(default)]
        c: i32,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Outer {
        x: Inner,
        y: i32,
    }

    let data = b"x = { b = 2 a = { c = 9 } } y = 5";
    let expected = Outer {
        x: Inner { b: 2, a: 0, c: 0 },
        y: 5,
    };
    assert_eq!(all_paths::<Outer>(data, 8..=24), expected);
    assert_eq!(from_tape::<Outer>(data).unwrap(), expected);
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
enum One {
    V(u8),
    P { x: u8 },
}

#[derive(Deserialize, PartialEq, Debug)]
struct OneField {
    c: One,
}

/// The lookahead past an enum variant for its block keeps only the open
/// token, so errors inside the block are at the same offset from readers
/// that remove whitespace and comments to make room as from a slice, and a
/// buffer that holds the longest token is enough.
#[rstest]
#[case::newtype(
    b"c = v           {           1 }",
    "invalid type: sequence, expected u8 (offset: 16)"
)]
#[case::newtype_scalar(
    b"c = v           {           x }",
    "invalid type: sequence, expected u8 (offset: 16)"
)]
#[case::no_block(
    b"c=# long comment here\n p  ",
    "TextDeserializer reads an enum variant with data only from a field value followed by a block that is not empty, as in `color = rgb { 1 2 3 }`; jomini::text::syntax::parse reads the others (offset: 23)"
)]
fn test_header_enum_offsets(#[case] data: &[u8], #[case] expected: &str) {
    let err = from_windows1252_slice::<OneField>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(4..=24) {
        for err in [
            reader_with_buf::<OneField>(data, size).unwrap_err(),
            trickle::<OneField>(data, size).unwrap_err(),
        ] {
            if !matches!(err.kind(), ErrorKind::BufferTooSmall) {
                assert_eq!(err.to_string(), expected, "buffer size {size}");
            }
        }
    }
}

/// The same error offsets with the default buffer, whose refill lands
/// anywhere around the variant.
#[test]
#[cfg_attr(miri, ignore)] // too slow
fn test_header_enum_offsets_default_buffer() {
    for shift in 0..64 {
        let mut data = vec![b' '; 32_740 + shift];
        data.extend_from_slice(b"c = v          {          1 }");
        let slice = from_windows1252_slice::<OneField>(&data).unwrap_err();
        let reader = from_windows1252_reader::<OneField>(&data[..]).unwrap_err();
        assert_eq!(reader.to_string(), slice.to_string(), "shift {shift}");
    }
}

/// A variant with data needs only the longest token and the byte after it
/// in the buffer.
#[test]
fn test_header_enum_small_buffer() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Field {
        color: Color,
    }

    let data = b"color\t= # c\nhsv\t{\n2  30 30  } # c\n";
    let actual: Field = all_paths(data, 6..=24);
    assert_eq!(actual.color, Color::Hsv(vec![2.0, 30.0, 30.0]));
}

/// A variant with data needs a block that isn't empty, after the variant of
/// a field value: a sequence element is not a header, as a `TextTape` reads
/// it. The error is at the variant.
#[rstest]
#[case::no_block(b"color = rgb", 8)]
#[case::empty_block(b"color = rgb { }", 8)]
#[case::element(b"list = { none rgb { 10 20 30 } }", 14)]
fn test_header_enum_errors(#[case] data: &[u8], #[case] offset: usize) {
    let expected = format!(
        "TextDeserializer reads an enum variant with data only from a field value followed by a block that is not empty, as in `color = rgb {{ 1 2 3 }}`; jomini::text::syntax::parse reads the others (offset: {offset})"
    );
    let err = from_windows1252_slice::<Colors>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    let err = from_windows1252_reader::<Colors>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(12..=32) {
        let err = reader_with_buf::<Colors>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "buffer size {size}");
        let err = trickle::<Colors>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "trickle buffer size {size}");
    }
    assert!(from_tape::<Colors>(data).is_err());

    // The data of a variant that doesn't match its type is an error there.
    let err = from_windows1252_slice::<Colors>(b"color = rgb { 10 x 30 }").unwrap_err();
    assert_eq!(
        err.to_string(),
        "invalid type: string \"x\", expected u8 (offset: 17)"
    );
}

#[derive(Deserialize, PartialEq, Debug)]
struct Typed {
    name: String,
    flag: bool,
    small: u8,
    neg: i32,
    float: f64,
    date: crate::common::Date,
    list: Vec<u16>,
    nested: Nested,
    #[serde(default)]
    missing: Option<String>,
    unit: (),
    pair: (String, Vec<u8>),
    letter: char,
    kind: Kind2,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Nested {
    a: Vec<Nested2>,
    b: HashMap<String, f32>,
}

#[derive(Deserialize, PartialEq, Debug)]
struct Nested2 {
    id: u32,
}

#[derive(Deserialize, PartialEq, Debug)]
#[serde(rename_all = "lowercase")]
enum Kind2 {
    Alpha,
    Beta,
}

const TYPED: &[u8] = b"\xef\xbb\xbfname=\"Kr\xe4ftig \\\"Leo\\\"\" # comment\n\
    skipped={ a={ \"}\" b } } flag=yes small=200 neg=-7 float=1.250\n\
    date=1444.11.11 list={ 1 2 3 } nested={ a={ { id=1 } { id=2 } } b={ x=0.5 y=-1 } }\n\
    unit={ 1 2 } pair=hdr { 9 } letter=z kind=beta";

#[test]
fn test_typed() {
    let actual: Typed = all_paths(TYPED, 24..=48);
    assert_eq!(
        actual,
        Typed {
            name: String::from("Kräftig \"Leo\""),
            flag: true,
            small: 200,
            neg: -7,
            float: 1.25,
            date: crate::common::Date::from_ymd(1444, 11, 11),
            list: vec![1, 2, 3],
            nested: Nested {
                a: vec![Nested2 { id: 1 }, Nested2 { id: 2 }],
                b: HashMap::from([(String::from("x"), 0.5), (String::from("y"), -1.0)]),
            },
            missing: None,
            unit: (),
            pair: (String::from("hdr"), vec![9]),
            letter: 'z',
            kind: Kind2::Beta,
        }
    );

    // The old streaming reader gives the same value.
    let old: Typed = crate::text::de::from_windows1252_reader(TYPED).unwrap();
    assert_eq!(old, actual);
}

#[test]
fn test_utf8() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct MyStruct {
        name: String,
    }

    let data = "name=\"Kräftig\"".as_bytes();
    let actual: MyStruct = TextDeserializer::from_slice(data, Utf8Encoding::new())
        .deserialize()
        .unwrap();
    assert_eq!(actual.name, "Kräftig");
    let actual: MyStruct = TextDeserializer::from_reader(data, Utf8Encoding::new())
        .deserialize()
        .unwrap();
    assert_eq!(actual.name, "Kräftig");
}

#[test]
fn test_errors() {
    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct MyStruct {
        a: Vec<u8>,
    }

    let err = from_windows1252_slice::<MyStruct>(b"a=").unwrap_err();
    assert!(matches!(err.kind(), ErrorKind::Eof), "{err}");
    let err = from_windows1252_slice::<MyStruct>(b"a=\"1 2").unwrap_err();
    assert!(matches!(err.kind(), ErrorKind::Eof), "{err}");
    let err = reader_with_buf::<MyStruct>(b"a=\"a long quoted value\"", 8).unwrap_err();
    assert!(matches!(err.kind(), ErrorKind::BufferTooSmall), "{err}");
}

/// Deserialize with buffers whose refills land inside each lookahead: past
/// a header scalar and its comment, past an empty block, and past the first
/// item of a block.
/// An `=` after a whole word of whitespace at the end of the window, as
/// with an empty key indented by a newline and seven tabs.
#[test]
fn test_equal_after_whitespace_word() {
    let data = b"a\n\t\t\t\t\t\t\t= 1\n";
    let actual: AnyValue = all_paths(data, 4..=data.len() + 1);
    assert_eq!(actual.to_string(), "{a=1}");

    let data = b"a\t\t\t\t\t\t\t\t=";
    assert!(reader_matches_slice(data));
}

/// A token that spans many reads is scanned again only when the buffer is
/// full, not after each read.
#[test]
#[cfg_attr(miri, ignore)] // too slow
fn test_long_token_from_small_reads() {
    let mut data = b"a=\"".to_vec();
    data.extend(std::iter::repeat_n(b'x', 30_000));
    data.extend_from_slice(b"\" b=");
    data.extend(std::iter::repeat_n(b'y', 30_000));

    let encoding = Windows1252Encoding::new();
    let mut deserializer = TextDeserializer::from_reader(Trickle(&data), encoding);
    let actual: HashMap<String, String> = deserializer.deserialize().unwrap();
    assert_eq!(actual["a"].len(), 30_000);
    assert_eq!(actual["b"].len(), 30_000);
}

#[test]
fn test_lookahead_across_refills() {
    let data = b"a = b\nhdr = x # c\n\n{ k = v }\nempty = y { }\nblk = { m { n = o } p q }";
    let expected = "{a=b hdr={k=v} empty=y blk=[m {n=o} p q]}";
    for size in buffer_sizes(14..=64) {
        let actual = reader_with_buf::<AnyValue>(data, size).unwrap();
        assert_eq!(actual.to_string(), expected, "buffer size {size}");
    }
    let actual: AnyValue = from_windows1252_slice(data).unwrap();
    assert_eq!(actual.to_string(), expected);
}

/// Comments longer than the buffer after a scalar that may be a header, and
/// inside the block that the lookahead looks into, are dropped while the
/// scalar is held.
#[test]
fn test_lookahead_past_long_comments() {
    let comment = format!("# {}\n", "x".repeat(100));
    let data = format!(
        "namespace = court\n{comment}{comment}a = hdr {comment} {{ {comment} }} b = {{ {comment} c d }}"
    );
    let expected = "{namespace=court a=hdr b=[c d]}";
    for size in buffer_sizes(10..=24) {
        let actual = reader_with_buf::<AnyValue>(data.as_bytes(), size).unwrap();
        assert_eq!(actual.to_string(), expected, "buffer size {size}");
    }
}

#[test]
fn test_ignored_any_skips_blocks() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Skip {}

    let data = b"a={ b={ \"}{\" c } # }\n } d={} e=f";
    let _: Skip = all_paths(data, 8..=16);

    let mut deserializer = TextDeserializer::from_slice(&data[..], Windows1252Encoding::new());
    let actual = (&mut deserializer).deserialize_map(PairCounter).unwrap();
    assert_eq!(actual, 3);

    struct PairCounter;

    impl<'de> de::Visitor<'de> for PairCounter {
        type Value = usize;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map")
        }

        fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<usize, A::Error> {
            let mut count = 0;
            while map
                .next_entry::<de::IgnoredAny, de::IgnoredAny>()?
                .is_some()
            {
                count += 1;
            }
            Ok(count)
        }
    }
}

#[derive(Deserialize, Debug)]
#[allow(dead_code)]
struct Fields {
    a: Vec<u8>,
    #[serde(default)]
    b: Option<u8>,
    #[serde(default)]
    c: Option<HashMap<String, String>>,
}

/// An error is at the start of the token that caused it, or at the length
/// of the data when the data ends where a value is expected or in a quoted
/// string, from a slice and from readers that refill anywhere. The old
/// streaming reader gives only an unexpected close an offset, which is
/// after the close.
#[rstest]
#[case::missing_value_in_block(b"a={ 1 2 } c={ x=", "unexpected end of file (offset: 16)", None)]
#[case::unexpected_close(
    b"a={ 1 2 }\nb=}",
    "invalid syntax encountered: did not expect end (offset: 12)",
    Some(13)
)]
#[case::bad_number(
    b"a={ 1 2 } b=abc",
    "invalid type: string \"abc\", expected u8 (offset: 12)",
    None
)]
#[case::bad_quoted_number(
    b"a={ 1 2 } b=\"abc\"",
    "invalid type: string \"abc\", expected u8 (offset: 12)",
    None
)]
#[case::bad_element(
    b"a={ 1 x }",
    "invalid type: string \"x\", expected u8 (offset: 6)",
    None
)]
#[case::missing_value(b"a={ 1 2 } b=", "unexpected end of file (offset: 12)", None)]
#[case::unclosed_quote(b"a={ 1 2 } b=\"abc", "unexpected end of file (offset: 16)", None)]
#[case::missing_field(b"b=1\n", "missing field `a` (offset: 4)", None)]
#[case::missing_map_value(
    b"a={ 1 2 } c={ x=y z }",
    "TextDeserializer doesn't read a value without a key, as the `80` of `{ 80 0=1 }`, except into the `remainder` field of a struct, which jomini::text::syntax::parse reads (offset: 18)",
    Some(21)
)]
fn test_error_offsets(
    #[case] data: &[u8],
    #[case] expected: &str,
    #[case] old_offset: Option<usize>,
) {
    let err = from_windows1252_slice::<Fields>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    let err = from_windows1252_reader::<Fields>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(8..=32) {
        let err = reader_with_buf::<Fields>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "buffer size {size}");
        let err = trickle::<Fields>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "trickle buffer size {size}");
    }

    let old = crate::text::de::from_windows1252_reader::<Fields, _>(data).unwrap_err();
    assert_eq!(old.offset(), old_offset, "{old}");
}

/// A token that doesn't fit in the buffer is located where it starts, after
/// the buffer has been refilled many times.
#[test]
fn test_buffer_too_small_offset() {
    let data = b"a={ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 } b=\"a long quoted value\"";
    for size in buffer_sizes(8..=16) {
        let err = reader_with_buf::<Fields>(data, size).unwrap_err();
        assert!(matches!(err.kind(), ErrorKind::BufferTooSmall), "{err}");
        assert_eq!(err.offset(), Some(47), "buffer size {size}");
        let err = trickle::<Fields>(data, size).unwrap_err();
        assert_eq!(err.offset(), Some(47), "trickle buffer size {size}");
    }

    let err = reader_with_buf::<Fields>(data, 16).unwrap_err();
    assert_eq!(
        err.to_string(),
        "token exceeds buffer capacity (offset: 47)"
    );

    // The old streaming reader gives no offset.
    let reader = crate::text::TokenReader::from_reader_with_buf(&data[..], vec![0; 16]);
    let mut old = crate::TextTapeDeserializer::from_windows1252_reader(reader);
    let err = old.deserialize::<Fields>().unwrap_err();
    assert!(matches!(err.kind(), ErrorKind::BufferTooSmall), "{err}");
    assert_eq!(err.offset(), None);
}

/// A string other than `bad`, read with `deserialize_any`.
#[derive(Debug)]
struct NotBad;

impl<'de> Deserialize<'de> for NotBad {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct NotBadVisitor;

        impl<'de> de::Visitor<'de> for NotBadVisitor {
            type Value = NotBad;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<NotBad, E> {
                if v == "bad" {
                    Err(E::custom("bad value"))
                } else {
                    Ok(NotBad)
                }
            }
        }

        deserializer.deserialize_any(NotBadVisitor)
    }
}

/// A value that is rejected after `deserialize_any` looked past it for a
/// block, through a comment that the reader drops to make room, is located
/// where it starts.
#[test]
fn test_error_offset_after_lookahead() {
    let data = b"a = ok\nx = bad # a comment that is longer than the buffer\n\ny = z";
    let expected = "bad value (offset: 11)";
    let err = from_windows1252_slice::<HashMap<String, NotBad>>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in buffer_sizes(8..=72) {
        let err = reader_with_buf::<HashMap<String, NotBad>>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "buffer size {size}");
        let err = trickle::<HashMap<String, NotBad>>(data, size).unwrap_err();
        assert_eq!(err.to_string(), expected, "trickle buffer size {size}");
    }
}

#[derive(Deserialize, PartialEq, Debug)]
struct Meta {
    date: crate::common::Date,
    save_game: String,
    player: String,
    savegame_version: SavegameVersion,
    savegame_versions: Vec<String>,
    dlc_enabled: Vec<String>,
    multi_player: bool,
    not_observer: bool,
    checksum: String,
}

#[derive(Deserialize, PartialEq, Debug)]
struct SavegameVersion {
    first: u16,
    second: u16,
    third: u16,
    forth: u16,
    name: String,
}

/// The typed fields of an EU4 save's meta are the same from each path and
/// from the old streaming reader.
#[test]
#[cfg_attr(miri, ignore)] // too slow
fn test_meta_fixture() {
    let data = &include_bytes!("../../../../tests/fixtures/meta.txt")["EU4txt".len()..];
    let actual: Meta = all_paths(data, 64..=96);
    assert_eq!(actual.player, "ENG");
    assert_eq!(actual.savegame_version.second, 28);
    assert_eq!(actual.dlc_enabled.len(), 13);
    let old: Meta = crate::text::de::from_windows1252_reader(data).unwrap();
    assert_eq!(old, actual);
}

/// `deserialize_any` reads the fixtures into the same values from a slice,
/// from readers of each buffer size, and from the old streaming reader.
#[rstest]
#[case::meta(&include_bytes!("../../../../tests/fixtures/meta.txt")["EU4txt".len()..], 44)]
#[case::ck3_header(&include_bytes!("../../../../tests/fixtures/ck3-header.txt")[..], 36)]
#[case::savegame(&include_bytes!("../../../../tests/fixtures/savegame.txt")[..], 17)]
#[case::string_array(&include_bytes!("../../../../tests/fixtures/string-array.txt")[..], 23)]
#[case::nested_hidden_obj(&include_bytes!("../../../../tests/fixtures/nested-hidden-obj.txt")[..], 16)]
#[cfg_attr(miri, ignore)] // too slow
fn test_any_fixture(#[case] data: &[u8], #[case] min_buf: usize) {
    let reader = crate::text::TokenReader::from_slice(data);
    let mut old = crate::TextTapeDeserializer::from_windows1252_reader(reader);
    let expected = AnyValue::deserialize(&mut old).unwrap();

    assert_eq!(from_windows1252_slice::<AnyValue>(data).unwrap(), expected);
    assert_eq!(from_windows1252_reader::<AnyValue>(data).unwrap(), expected);
    assert_eq!(trickle::<AnyValue>(data, 4096).unwrap(), expected);
    for size in 4..=128 {
        match reader_with_buf::<AnyValue>(data, size) {
            Ok(actual) => assert_eq!(actual, expected, "buffer size {size}"),
            Err(e) => {
                assert!(matches!(e.kind(), ErrorKind::BufferTooSmall), "{e}");
                assert!(size < min_buf, "buffer size {size}");
            }
        }
    }
}

/// The bytes that a generated document is made of: tokens, whitespace, and
/// comments that run into each other.
const FRAGMENTS: &[&[u8]] = &[
    b"a",
    b"bb",
    b"1",
    b"-2.5",
    b"\"q\"",
    b"\"x y\"",
    b"\"e\\\"\"",
    b"@[a+1]",
    b"=",
    b"==",
    b"<",
    b">=",
    b"?=",
    b"!=",
    b"{",
    b"}",
    b"{ }",
    b" ",
    b"\n",
    b"\t",
    b"# c\n",
    b";",
];

fn document(choices: &[u8]) -> Vec<u8> {
    choices
        .iter()
        .flat_map(|&x| FRAGMENTS[usize::from(x) % FRAGMENTS.len()])
        .copied()
        .collect()
}

/// The value, or the error and whether the buffer was too small.
fn outcome(result: Result<AnyValue, Error>) -> Result<String, (bool, String)> {
    result.map(|x| x.to_string()).map_err(|e| {
        let small = matches!(e.kind(), ErrorKind::BufferTooSmall);
        (small, e.to_string())
    })
}

/// Readers give the same value or error as a slice, with its offset, unless
/// the buffer is too small for a token.
fn reader_matches_slice(data: &[u8]) -> bool {
    let expected = outcome(from_windows1252_slice(data));
    let same = |actual: Result<String, (bool, String)>| {
        actual == expected || matches!(actual, Err((true, _)))
    };

    outcome(from_windows1252_reader(data)) == expected
        && outcome(trickle(data, 4096)) == expected
        && [8, 13, 32].into_iter().all(|size| {
            same(outcome(reader_with_buf(data, size))) && same(outcome(trickle(data, size)))
        })
}

#[quickcheck_macros::quickcheck]
#[cfg_attr(miri, ignore)] // too slow
fn test_reader_matches_slice_documents(choices: Vec<u8>) -> bool {
    reader_matches_slice(&document(&choices))
}

/// Arbitrary bytes.
#[quickcheck_macros::quickcheck]
#[cfg_attr(miri, ignore)] // too slow
fn test_reader_matches_slice_bytes(data: Vec<u8>) -> bool {
    reader_matches_slice(&data)
}

/// A visitor of `deserialize_any` that rejects a block before it reads it
/// has the block skipped, as a typed value does, so that a caller that
/// recovers from the error continues after the block.
#[test]
fn test_rejected_any_block_is_skipped() {
    struct Word(String);

    impl<'de> Deserialize<'de> for Word {
        fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct WordVisitor;
            impl de::Visitor<'_> for WordVisitor {
                type Value = Word;
                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a word")
                }
                fn visit_str<E: de::Error>(self, v: &str) -> Result<Word, E> {
                    Ok(Word(v.to_string()))
                }
            }
            deserializer.deserialize_any(WordVisitor)
        }
    }

    fn lenient<'de, D: de::Deserializer<'de>>(deserializer: D) -> Result<String, D::Error> {
        Ok(Word::deserialize(deserializer).map_or_else(|_| String::new(), |x| x.0))
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Inner {
        #[serde(deserialize_with = "lenient")]
        a: String,
        #[serde(default)]
        c: i32,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Outer {
        x: Inner,
        y: i32,
    }

    let expected = Outer {
        x: Inner {
            a: String::new(),
            c: 0,
        },
        y: 5,
    };
    for data in [
        &b"x = { a = { c = 9 } } y = 5"[..],
        b"x = { a = { 1 2 } } y = 5",
        b"x = { a = { \"}\" c = 9 } } y = 5",
        b"x = { a = { { c = 9 } } } y = 5",
    ] {
        assert_eq!(all_paths::<Outer>(data, 8..=24), expected);
    }
}

#[derive(Deserialize, PartialEq, Debug)]
struct After {
    after: Option<u32>,
}

/// A quote after the first byte of an unquoted scalar is part of it, so a
/// skipped block that holds one ends where the tokenizer ends it, as in
/// `common/units/names_divisions/FIN_names_divisions.txt` of HoI4.
#[test]
fn test_skip_quote_in_scalar() {
    let data = b"skipped = { name = abc\" x = 1 } after = 3 b = \"}\"";
    assert_eq!(all_paths::<After>(data, 8..=24).after, Some(3));
}

/// A scalar that starts with a bracket, as an interpolation in game files,
/// and a `!` or `?` that is not part of an operator are errors that name
/// the lossless parser, and a skipped block that holds one is skipped.
#[rstest]
#[case::bracket(
    b"after = [GetFoo] x = 1",
    "a scalar that starts with a bracket, as in `picture = [GetName]`",
    8
)]
#[case::bracket_key(
    b"x = 1 [GetFoo] = 1",
    "a scalar that starts with a bracket, as in `picture = [GetName]`",
    6
)]
#[case::not(
    b"x ! y = 1",
    "a `!` or `?` that is not part of an operator, as in `a ! b`",
    2
)]
#[case::exists_end(
    b"x = y ?",
    "a `!` or `?` that is not part of an operator, as in `a ! b`",
    6
)]
fn test_unsupported_token_error(#[case] data: &[u8], #[case] what: &str, #[case] offset: usize) {
    assert_eq!(
        all_paths_err::<After>(data, 8..=24),
        format!(
            "TextDeserializer doesn't read {what}, which jomini::text::syntax::parse reads (offset: {offset})"
        )
    );

    let mut skipped = b"skipped = { ".to_vec();
    skipped.extend_from_slice(data);
    skipped.extend_from_slice(b" } after = 3");
    assert_eq!(all_paths::<After>(&skipped, 8..=24).after, Some(3));
}

/// A value that is read again after its read failed, as a retry in another
/// type, is an error at the value, rather than a read of the key after it.
#[test]
fn test_value_retry_error() {
    #[derive(Debug)]
    struct Retry;

    impl<'de> Deserialize<'de> for Retry {
        fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct RetryVisitor;
            impl<'de> de::Visitor<'de> for RetryVisitor {
                type Value = Retry;
                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a map")
                }
                fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Retry, A::Error> {
                    while map.next_key::<String>()?.is_some() {
                        if map.next_value::<i64>().is_err() {
                            map.next_value::<String>()?;
                        }
                    }
                    Ok(Retry)
                }
            }
            deserializer.deserialize_map(RetryVisitor)
        }
    }

    let expected = "TextDeserializer doesn't read a value again after its read failed, which jomini::text::syntax::parse does";
    assert_eq!(
        all_paths_err::<Retry>(b"a = 1 b = x c = 2", 4..=24),
        format!("{expected} (offset: 10)")
    );
    assert_eq!(
        all_paths_err::<Retry>(b"a = 1 b = { x } c = 2", 4..=24),
        format!("{expected} (offset: 10)")
    );
    assert_eq!(
        all_paths_err::<Retry>(b"a = { b = x }", 4..=24),
        format!("{expected} (offset: 4)")
    );
    assert_eq!(
        all_paths_err::<Retry>(b"a = \"x\" b = 1", 4..=24),
        format!("{expected} (offset: 4)")
    );
}

/// A header before a map, as in `o = rgb { s = 1 }`, is an error that
/// names the lossless parser, and so is an object read as a sequence.
#[test]
fn test_header_map_and_object_seq_errors() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Inner {
        s: u32,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Struct {
        o: Inner,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Map {
        o: HashMap<String, u32>,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Seq {
        o: Vec<String>,
    }

    let data = b"o = rgb { s = 1 }";
    let hint = "TextDeserializer doesn't read a header before a map, as in `o = rgb { s = 1 }`, which jomini::text::syntax::parse reads (offset: 4)";
    assert!(all_paths_err::<Struct>(data, 8..=24).ends_with(hint));
    assert!(all_paths_err::<Map>(data, 8..=24).ends_with(hint));

    assert_eq!(
        all_paths_err::<Seq>(b"o = { a = 1 }", 8..=24),
        "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 8)"
    );
}

/// A quoted scalar is not a header: the block after it is a value without
/// a key, and not the data of an enum variant.
#[test]
fn test_quoted_scalar_is_not_header() {
    let actual: AnyValue = all_paths(b"a = \"x\" { 1 } b = 2", 8..=24);
    assert_eq!(actual.to_string(), "{a=x remainder=[[1] b = 2]}");

    let err = all_paths_err::<RequiredColor>(b"color = \"rgb\" { 1 2 3 }", 8..=24);
    assert_eq!(
        err,
        "TextDeserializer reads an enum variant with data only from a field value followed by a block that is not empty, as in `color = rgb { 1 2 3 }`; jomini::text::syntax::parse reads the others (offset: 8)"
    );
}

/// The offsets of errors are exact for every buffer size, after bytes are
/// removed from the window at more than one place to look past comments.
#[test]
fn test_error_offset_after_two_removals() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Seq {
        v: Vec<String>,
    }

    let data = b"v = abc # comment\n{ # comment\n1 }";
    let expected = from_windows1252_slice::<Seq>(data).unwrap_err().to_string();
    assert!(expected.ends_with("(offset: 18)"), "{expected}");
    assert_eq!(all_paths_err::<Seq>(data, 11..=40), expected);

    // A smaller buffer that holds the lookahead gives the same offset.
    for size in 4..=10 {
        for err in [reader_with_buf::<Seq>(data, size), trickle(data, size)] {
            let err = err.unwrap_err();
            if !matches!(err.kind(), ErrorKind::BufferTooSmall) {
                assert_eq!(err.to_string(), expected, "buffer size {size}");
            }
        }
    }
}

/// A key without a value read as an enum is the error of a value without
/// a key, and not that of an enum written as a block.
#[test]
fn test_enum_without_value_error() {
    #[derive(Deserialize, PartialEq, Debug)]
    struct Inner {
        color: Option<Color>,
    }

    #[derive(Deserialize, PartialEq, Debug)]
    struct Outer {
        x: Inner,
    }

    let err = all_paths_err::<Outer>(b"x = { a = 1 color }", 8..=24);
    assert!(!err.contains("written as a block"), "{err}");
    assert!(err.contains("jomini::text::syntax::parse"), "{err}");
}

/// An operator in a sequence, as in the remainder of an object, needs a
/// value after it, as the lossless parser requires.
#[rstest]
#[case::remainder(b"a = { b = c d e = }", "did not expect end (offset: 18)")]
#[case::sequence(b"a = { 1 2 = }", "did not expect end (offset: 12)")]
#[case::remainder_end(b"a = { b = c d e =", "unexpected end of file (offset: 17)")]
#[case::sequence_end(b"a = { 1 2 =", "unexpected end of file (offset: 11)")]
fn test_operator_element_without_value(#[case] data: &[u8], #[case] expected: &str) {
    assert!(all_paths_err::<AnyValue>(data, 8..=24).ends_with(expected));
}

/// An operator right after the operator of an empty key is the value, as
/// the lossless parser reads it, which is an error whatever the buffer.
#[rstest]
#[case::exact(b"a=1 ===s", 6)]
#[case::equal(b"a=1\n= =s", 6)]
fn test_empty_key_operator_value(#[case] data: &[u8], #[case] offset: usize) {
    let expected = format!(
        "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: {offset})"
    );
    assert_eq!(
        all_paths_err::<HashMap<String, de::IgnoredAny>>(data, 4..=16),
        expected
    );
    assert_eq!(
        all_paths_err::<HashMap<String, String>>(data, 4..=16),
        expected
    );
}

/// An `=` right after the operator of a field with an empty key is read as
/// the key, an operator as a value, also when a reader's window ends at the
/// operator.
#[test]
fn test_empty_key_operator_at_window_end() {
    let data = b"{ }==a!==";
    let expected = "TextDeserializer doesn't read an operator as a value, as in `a = =` or in an object read as a sequence, which jomini::text::syntax::parse reads (offset: 8)";
    assert_eq!(all_paths_err::<AnyValue>(data, 4..=32), expected);
    assert_eq!(
        all_paths_err::<HashMap<String, String>>(data, 4..=32),
        expected
    );
}

/// A map whose values are read as a number, and again as a string when
/// that fails.
#[derive(Debug, PartialEq)]
struct Retry(String);

impl<'de> Deserialize<'de> for Retry {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct RetryVisitor;
        impl<'de> de::Visitor<'de> for RetryVisitor {
            type Value = Retry;
            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map")
            }
            fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Retry, A::Error> {
                let mut out = String::new();
                while let Some(key) = map.next_key::<String>()? {
                    let value = match map.next_value::<i64>() {
                        Ok(x) => x.to_string(),
                        Err(_) => map.next_value::<String>()?,
                    };
                    out.push_str(&format!("{key}={value} "));
                }
                Ok(Retry(out))
            }
        }
        deserializer.deserialize_map(RetryVisitor)
    }
}

const READ_AGAIN: &str = "TextDeserializer doesn't read a value again after its read failed, which jomini::text::syntax::parse does";

/// A value whose token doesn't scan, as a `?` that is not part of an
/// operator, or that the end of the data cuts off, is not read again by a
/// retry in another type, which gives the error of a value read again.
#[test]
fn test_retry_after_scan_error() {
    let cases = [
        (&b"a ?"[..], 2),
        (b"a = ?", 4),
        (b"a = [[x] b ]", 4),
        (b"a = ", 4),
        (b"= ", 2),
    ];
    for (data, offset) in cases {
        let expected = format!("{READ_AGAIN} (offset: {offset})");
        let encoding = Windows1252Encoding::new();
        let err = TextDeserializer::from_slice(data, encoding)
            .deserialize::<Retry>()
            .err()
            .unwrap();
        assert_eq!(err.to_string(), expected);
        for size in buffer_sizes(4..=8) {
            let reader = reader_with_buf::<Retry>(data, size);
            assert_eq!(reader.err().map(|e| e.to_string()), Some(expected.clone()));
        }
    }
}

/// A token that doesn't fit in a reader's buffer stays the error when the
/// block whose first item it is gets skipped, where the skip read the
/// second quote of the scalar `a"e\""` as the start of a string and ran to
/// the end of the data.
#[rstest]
#[case(b"{\"q\"={ }\"q\"# c\na\"e\\\"\"", 4, 8)]
#[case(b"{bba-2.5{bb\"e\\\"\"# c\n", 8, 9)]
fn test_buffer_too_small_in_rejected_block(
    #[case] data: &[u8],
    #[case] size: usize,
    #[case] offset: usize,
) {
    assert!(from_windows1252_slice::<AnyValue>(data).is_ok());
    let expected = format!("token exceeds buffer capacity (offset: {offset})");
    let err = reader_with_buf::<AnyValue>(data, size).unwrap_err();
    assert_eq!(err.to_string(), expected);
    let err = trickle::<AnyValue>(data, size).unwrap_err();
    assert_eq!(err.to_string(), expected);
}

/// A reader that fails once when it is asked for the byte at `fail`.
struct Flaky<'a> {
    data: &'a [u8],
    pos: usize,
    fail: Option<usize>,
}

impl std::io::Read for Flaky<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if let Some(fail) = self.fail
            && self.pos >= fail
        {
            self.fail = None;
            return Err(std::io::Error::other("flaky"));
        }
        let mut n = buf.len().min(self.data.len() - self.pos);
        if let Some(fail) = self.fail {
            n = n.min(fail - self.pos);
        }
        buf[..n].copy_from_slice(&self.data[self.pos..self.pos + n]);
        self.pos += n;
        Ok(n)
    }
}

fn flaky<T: DeserializeOwned>(data: &[u8], size: usize, fail: usize) -> Result<T, Error> {
    let reader = Flaky {
        data,
        pos: 0,
        fail: Some(fail),
    };
    TextDeserializer::from_reader_with_buf(reader, Windows1252Encoding::new(), vec![0; size])
        .deserialize()
}

/// An I/O error while a rejected block is skipped is the error, rather
/// than the visitor's, which a caller that recovers from the visitor's
/// error would take as the end of the block.
#[test]
fn test_skip_keeps_io_error() {
    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct Plain {
        a: i64,
    }

    let data = b"a={ 1 2 3 4 5 6 7 8 9 10 11 12 13 } b=3";
    let err = flaky::<Plain>(data, 8, 8).unwrap_err();
    assert_eq!(err.to_string(), "io error: flaky (offset: 8)");
}

/// A retry after an I/O error in the scan of a value gives the error of a
/// value read again, at the I/O error, and nothing panics wherever a read
/// fails.
#[test]
fn test_retry_after_io_error() {
    let data = b"a= bbbbbbbbbbbbbbbbbbbbbbbbb c=d";
    for fail in 8..=10 {
        let err = flaky::<Retry>(data, 8, fail).unwrap_err();
        assert_eq!(err.to_string(), format!("{READ_AGAIN} (offset: {fail})"));
    }

    let docs: [&[u8]; 4] = [
        data,
        b"a={ 1 2 3 4 5 6 7 8 9 10 11 12 13 } b=3",
        b"a={ x=1 y={ 1 2 } } b=\"q\" c=d e=f=g # c\n h=rgb { 1 2 }",
        b"a={ 80 0=1 1=2 } b={ { } c } d",
    ];
    for data in docs {
        for size in [4, 8, 16] {
            for fail in 0..data.len() {
                let _ = flaky::<Retry>(data, size, fail);
                let _ = flaky::<AnyValue>(data, size, fail);
                let _ = flaky::<HashMap<String, String>>(data, size, fail);
            }
        }
    }
}

/// A retry of a value whose read failed doesn't read the field after it,
/// also when the `=` of that field follows the value, which the read of an
/// operator after a key reads without a look at the failure.
#[test]
fn test_retry_before_equal() {
    let cases = [
        (&b"a=b=c"[..], 2),
        (b"a=b =c d=1", 2),
        (b"x=1 a=bb=c", 6),
        (b"a={ 1 }=b", 2),
    ];
    for (data, offset) in cases {
        let expected = format!("{READ_AGAIN} (offset: {offset})");
        assert_eq!(all_paths_err::<Retry>(data, 4..=16), expected);
    }
}

/// A backspace, a vertical tab, and a form feed before a token are
/// whitespace, wherever the token is in a reader's window.
#[rstest]
#[case(b"a = \x0bb c = d e = f")]
#[case(b"a = \x08b c = d e = f")]
#[case(b"a = \x0cb c = d e = f")]
#[case(b"a =\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0bb c = d e = f")]
#[case(b"a\x0c=\x0cb c\x0c\x0c= d e = f")]
fn test_control_whitespace(#[case] data: &[u8]) {
    let actual: HashMap<String, String> = all_paths(data, 4..=16);
    let expected = [("a", "b"), ("c", "d"), ("e", "f")];
    let expected = expected.map(|(k, v)| (k.to_string(), v.to_string()));
    assert_eq!(actual, HashMap::from(expected));
}

/// The error of a remainder that a visitor rejects is at the remainder,
/// where it was found, rather than where its recovery stopped, unless the
/// recovery needs more room than a reader's buffer has.
#[rstest]
#[case(b"{a}b=c d=e")]
#[case(b"{\"q\"}!=\"q\"<=!=# c\nbbbbbbbbb\"q\"=")]
fn test_remainder_error_offset(#[case] data: &[u8]) {
    let expected = "invalid type: sequence, expected a string (offset: 0)";
    let err = from_windows1252_slice::<HashMap<String, String>>(data).unwrap_err();
    assert_eq!(err.to_string(), expected);
    for size in 4..=16 {
        for err in [
            reader_with_buf::<HashMap<String, String>>(data, size).unwrap_err(),
            trickle::<HashMap<String, String>>(data, size).unwrap_err(),
        ] {
            if !matches!(err.kind(), ErrorKind::BufferTooSmall) {
                assert_eq!(err.to_string(), expected, "buffer size {size}");
            }
        }
    }
}

/// A string, or `None` when the value isn't one, except for a token that
/// doesn't fit in a reader's buffer, which stays an error.
#[derive(Debug, PartialEq)]
struct Lenient(Option<String>);

impl<'de> Deserialize<'de> for Lenient {
    fn deserialize<D: de::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;
        impl de::Visitor<'_> for V {
            type Value = String;
            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a string")
            }
            fn visit_str<E: de::Error>(self, v: &str) -> Result<String, E> {
                Ok(v.to_string())
            }
        }
        match d.deserialize_any(V) {
            Ok(x) => Ok(Lenient(Some(x))),
            Err(e) if e.to_string().contains("exceeds buffer capacity") => Err(e),
            Err(_) => Ok(Lenient(None)),
        }
    }
}

/// The skip of a block that a visitor rejects, after its first item was
/// looked past, reads what follows as a slice does. The look past the
/// first item removed the comment after it from the window, which made the
/// scalar `1` and the string after it read as one scalar with a quote.
#[test]
fn test_rejected_block_after_lookahead() {
    /// The fields of a map in order.
    #[derive(Debug, PartialEq)]
    struct Fields(Vec<(String, Lenient)>);

    impl<'de> Deserialize<'de> for Fields {
        fn deserialize<D: de::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            struct V;
            impl<'de> de::Visitor<'de> for V {
                type Value = Fields;
                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("a map")
                }
                fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Fields, A::Error> {
                    let mut out = Vec::new();
                    while let Some(key) = map.next_key::<String>()? {
                        out.push((key, map.next_value()?));
                    }
                    Ok(Fields(out))
                }
            }
            d.deserialize_map(V)
        }
    }

    let data = b"d2\n\t{ # c\n1 # c\n\"\"\n} h\n<=\n{\n}\nh\n<=\n{\n}\nh\n<=\n{\n}\nh\n<=\n{\n}";
    let fields = ["d2", "h", "h", "h", "h"].map(|k| (k.to_string(), Lenient(None)));
    let expected = Fields(fields.into());
    assert_eq!(from_windows1252_slice::<Fields>(data).unwrap(), expected);
    for size in 4..=40 {
        for actual in [
            reader_with_buf::<Fields>(data, size),
            trickle::<Fields>(data, size),
        ] {
            match actual {
                Ok(actual) => assert_eq!(actual, expected, "buffer size {size}"),
                Err(e) => assert!(matches!(e.kind(), ErrorKind::BufferTooSmall), "{e}"),
            }
        }
    }
}

/// A block that a sequence rejects, which an `=` follows, as an object used
/// as a key, gives the same error from a slice and from a reader whose
/// window ends after the block.
#[test]
fn test_rejected_block_before_equal() {
    let data = b"c = { 1 2 { 6 }={ 1 } }";
    assert_eq!(
        all_paths_err::<HashMap<String, Vec<u8>>>(data, 4..=32),
        "invalid type: sequence, expected u8; TextDeserializer doesn't read an object as a sequence, as the scalars that trail its fields or an object used as a key, which jomini::text::syntax::parse reads (offset: 10)"
    );
}
