#![cfg(all(feature = "lossless", feature = "derive"))]

//! Tests for the serde deserializer of the syntax tree, its wrapper types,
//! the owned value, and the document access.
//!
//! The differential tests compare the tree deserializer with
//! `TextDeserializer` on a `TextTape`. When `TextDeserializer` gives a
//! result, the tree deserializer must give the same result.

use jomini::{
    JominiDeserialize, TextDeserializer, TextTape, Utf8Encoding, Windows1252Encoding,
    text::{
        Operator, Property,
        lossless::{
            self,
            de::{
                Document, Error, Headered, MaybeQuoted, Spanned, WithOp, from_slice, from_tree,
                from_utf8_slice, from_windows1252_slice,
            },
            value::{Block, Field, Item, Value},
        },
    },
};
use serde::{
    Deserialize,
    de::{
        self, DeserializeOwned, Deserializer, IgnoredAny, IntoDeserializer, MapAccess, SeqAccess,
    },
};
use std::{collections::HashMap, fmt};

// ---------------------------------------------------------------------------
// Generic types for the differential tests
// ---------------------------------------------------------------------------

/// Any value, read with `deserialize_any`.
#[derive(Debug, PartialEq)]
enum Any {
    Str(String),
    Seq(Vec<Any>),
    Map(Vec<(String, Any)>),
}

struct AnyVisitor;

impl<'de> de::Visitor<'de> for AnyVisitor {
    type Value = Any;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Any, E> {
        Ok(Any::Str(v.to_string()))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Any, A::Error> {
        let mut out = Vec::new();
        while let Some(x) = seq.next_element()? {
            out.push(x);
        }
        Ok(Any::Seq(out))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Any, A::Error> {
        let mut out = Vec::new();
        while let Some(x) = map.next_entry()? {
            out.push(x);
        }
        Ok(Any::Map(out))
    }
}

impl<'de> Deserialize<'de> for Any {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(AnyVisitor)
    }
}

/// A document read as a map of any values.
#[derive(Debug, PartialEq)]
struct AnyRoot(Any);

impl<'de> Deserialize<'de> for AnyRoot {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_map(AnyVisitor).map(AnyRoot)
    }
}

/// A typed request at each level: a map of strings, maps, and sequences of
/// strings.
#[derive(Debug, PartialEq)]
enum Typed {
    Str(String),
    Strs(Vec<String>),
    Map(Vec<(String, Typed)>),
}

impl<'de> Deserialize<'de> for Typed {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;
        impl<'de> de::Visitor<'de> for V {
            type Value = Typed;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a typed value")
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<Typed, E> {
                Ok(Typed::Str(v.to_string()))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Typed, A::Error> {
                let mut out = Vec::new();
                while let Some(x) = seq.next_element::<String>()? {
                    out.push(x);
                }
                Ok(Typed::Strs(out))
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Typed, A::Error> {
                let mut out = Vec::new();
                while let Some(x) = map.next_entry()? {
                    out.push(x);
                }
                Ok(Typed::Map(out))
            }
        }
        d.deserialize_map(V)
    }
}

/// The keys of a map in order, duplicates too.
#[derive(Debug, PartialEq)]
struct Keys(Vec<String>);

impl<'de> Deserialize<'de> for Keys {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;
        impl<'de> de::Visitor<'de> for V {
            type Value = Keys;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a map")
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Keys, A::Error> {
                let mut out = Vec::new();
                while let Some(key) = map.next_key::<String>()? {
                    map.next_value::<IgnoredAny>()?;
                    out.push(key);
                }
                Ok(Keys(out))
            }
        }
        d.deserialize_map(V)
    }
}

fn legacy<T: DeserializeOwned>(data: &[u8]) -> Option<T> {
    let tape = TextTape::from_slice(data).ok()?;
    TextDeserializer::from_windows1252_tape(&tape)
        .deserialize()
        .ok()
}

fn tree<T: DeserializeOwned>(data: &[u8]) -> Result<T, Error> {
    let parse = lossless::parse(data);
    from_tree(parse.tree(), Windows1252Encoding::new())
}

/// Compare the tree deserializer, the borrowed value, and the owned value
/// with `TextDeserializer`. Return `true` when `TextDeserializer` gives a
/// result.
fn compare<T: DeserializeOwned + PartialEq + fmt::Debug>(data: &[u8]) -> bool {
    let Some(expected) = legacy::<T>(data) else {
        return false;
    };
    let name = String::from_utf8_lossy(data);
    let actual: T = tree(data).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(actual, expected, "tree: {name}");

    let parse = lossless::parse(data);
    let value = Value::from_tree(parse.tree(), Windows1252Encoding::new());
    let borrowed = T::deserialize(&value).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(borrowed, expected, "borrowed value: {name}");
    let owned = T::deserialize(value).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(owned, expected, "owned value: {name}");
    true
}

/// Inputs from the tests of `TextTape` and `TextDeserializer`, and the
/// shapes that game files use.
const INPUTS: &[&str] = &[
    "foo=bar",
    "foo=\"bar\" bar=\"qux\"",
    "foo=bar foo=baz foo=qux",
    "a = 1 b = 2.5 c = -3 d = yes e = no f = 1444.11.11",
    "core={FRA BUR}",
    "nums={1 2 3 4 5}",
    "nested={a={b={c=1}}}",
    "obj={a=1 b={c=2} d={e f}}",
    "array_of_objects={{a=1} {a=2} {b=3}}",
    "empty={} after=1",
    "a = { {} 1 2 }",
    "a = { x = 1 {} y = 2 }",
    "color = rgb { 100 200 150 } color2 = hsv { 0.3 0.2 0.8 }",
    "color = { rgb { 1 2 3 } }",
    "color = rgb {}",
    "brittany_area = { color = { 118 99 151 } 169 170 171 172 4384 }",
    "a = { b = 1 2 3 }",
    "a = { b = 1 2 c = { d = 3 } e = 4 }",
    "a = { 1 2 b = c d }",
    "a = { b { 1 } c = 2 }",
    "a = { x > 1 y = 2 z < 3 w >= 4 v <= 5 u != 6 t == 7 s ?= 8 }",
    "modifier = { abc=yes factor=2 num > 2 }",
    "OPERATOR = <=",
    "potential = { OR { a = b } NOT = { c = d } }",
    "flags = { a b c } flags = { d }",
    "@var = 10 value = @var calc = @[var * 2]",
    "a = \"x\" b = @[1+2] [[p] c = 1 ]",
    "[[!undefined] a = 1 ]",
    "[[defined] value ]",
    "text = [Root.GetName]",
    "dupe = { a = 1 } other = 2 dupe = { a = 2 }",
    "levels = { 10 0=2 1=2 }",
    "a = { 10 0 = 1 }",
    "date = 1444.11.11 events = { 1444.11.12 = { x = 1 } }",
    "quoted_key = { \"key with space\" = 1 }",
    "escaped = \"Joe \\\"Captain\\\" Rogers\"",
    "a = 1 }",
    "a = { b = 1",
    "\u{feff}bom = 1",
    "a = { b = { c = { d = { e = { f = 1 } } } } }",
    "tags = { \"FRA\" \"ENG\" } names = { a \"b c\" }",
    "neg = -1.5 pos = +2 big = 18446744073709551615",
    "a = { b = c d = { e } f g }",
    "window = { blockoverride \"header\" { } widget = { size = { 42 42 } } }",
    "a = }",
    "x = { y = { } z = { 1 } }",
];

#[test]
fn differential_inline_inputs() {
    let mut compared = 0;
    for input in INPUTS {
        let data = input.as_bytes();
        compared += compare::<AnyRoot>(data) as usize;
        compared += compare::<Typed>(data) as usize;
        compared += compare::<Keys>(data) as usize;
        compared += compare::<HashMap<String, IgnoredAny>>(data) as usize;
        compared += compare::<Vec<String>>(data) as usize;
    }
    // Most inputs work with TextDeserializer, so most cases are compared.
    assert!(compared > INPUTS.len() * 3, "{compared}");
}

#[test]
fn differential_fixtures() {
    let fixtures: &[&[u8]] = &[
        &include_bytes!("./fixtures/meta.txt")["EU4txt".len()..],
        include_bytes!("./fixtures/ck3-header.txt"),
        include_bytes!("./fixtures/campaign_stats.txt"),
        include_bytes!("./fixtures/nested-hidden-obj.txt"),
        include_bytes!("./fixtures/savegame.txt"),
        include_bytes!("./fixtures/string-array.txt"),
    ];
    for data in fixtures {
        assert!(compare::<AnyRoot>(data) | compare::<Typed>(data) | compare::<Keys>(data));
    }
}

#[test]
fn differential_typed_structs() {
    #[derive(JominiDeserialize, Debug, PartialEq)]
    struct Sprites {
        #[jomini(duplicated, alias = "spriteType")]
        sprites: Vec<Sprite>,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Sprite {
        name: String,
        #[serde(rename = "texturefile")]
        texture: String,
        #[serde(default)]
        frames: Option<u32>,
    }

    let data = br#"
        spriteType = { name = "a" texturefile = "gfx/a.dds" }
        other = { x = 1 }
        spriteType = { name = "b" texturefile = "gfx/b.dds" frames = 4 }
    "#;
    assert!(compare::<Sprites>(data));

    #[derive(JominiDeserialize, Debug, PartialEq)]
    struct Country {
        tag: String,
        #[jomini(default)]
        capital: u32,
        #[jomini(take_last)]
        name: String,
        #[jomini(duplicated)]
        core: Vec<String>,
        color: Vec<u8>,
        modifier: Modifier,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Modifier {
        factor: Property<f32>,
        num: Property<u8>,
    }

    let data = br#"
        tag = FRA name = "France" core = FRA name = "Kingdom of France" core = BUR
        color = { 20 50 210 } modifier = { factor = 1.5 num > 2 }
    "#;
    assert!(compare::<Country>(data));

    #[derive(Deserialize, Debug, PartialEq)]
    #[serde(rename_all = "lowercase")]
    enum Color {
        Rgb(Vec<u8>),
        Hsv(Vec<f32>),
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Colors {
        a: Color,
        b: Color,
    }

    assert!(compare::<Colors>(
        b"a = rgb { 1 2 3 } b = hsv { 0.5 0.5 1.0 }"
    ));
}

// ---------------------------------------------------------------------------
// Behavior that the tree deserializer adds or changes
// ---------------------------------------------------------------------------

#[test]
fn mixed_container_as_seq_and_map() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Area {
        #[serde(default)]
        color: Vec<u8>,
        remainder: Vec<u16>,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Areas {
        brittany_area: Vec<u16>,
        normandy_area: Area,
    }

    let data = b"brittany_area = { color = { 1 2 3 } 169 170 }
        normandy_area = { color = { 4 5 6 } 171 172 }";
    let areas: Areas = from_windows1252_slice(data).unwrap();
    assert_eq!(areas.brittany_area, vec![169, 170]);
    assert_eq!(areas.normandy_area.color, vec![4, 5, 6]);
    assert_eq!(areas.normandy_area.remainder, vec![171, 172]);
}

#[test]
fn headered_value_as_seq() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct File {
        color: (String, Vec<u8>),
    }

    let file: File = from_utf8_slice(b"color = rgb { 1 2 3 }").unwrap();
    assert_eq!(file.color, (String::from("rgb"), vec![1, 2, 3]));
}

#[test]
fn borrow_from_the_source() {
    #[derive(Deserialize)]
    struct File<'a> {
        name: &'a str,
        tags: Vec<&'a str>,
    }

    let data = b"name = \"Paris\" tags = { a b }";
    let file: File = from_utf8_slice(data).unwrap();
    assert_eq!(file.name, "Paris");
    assert_eq!(file.tags, vec!["a", "b"]);
}

#[test]
fn encodings() {
    #[derive(Deserialize)]
    struct File {
        name: String,
    }

    let file: File = from_windows1252_slice(b"name = \"K\xe4rnten\"").unwrap();
    assert_eq!(file.name, "K\u{e4}rnten");

    let file: File = from_utf8_slice("\u{feff}name = \"K\u{e4}rnten\"".as_bytes()).unwrap();
    assert_eq!(file.name, "K\u{e4}rnten");

    let file: File = from_slice("name = Zürich".as_bytes(), Utf8Encoding::new()).unwrap();
    assert_eq!(file.name, "Zürich");
}

#[test]
fn enum_as_map_key() {
    #[derive(Deserialize, Debug, PartialEq, Eq, Hash)]
    #[serde(rename_all = "lowercase")]
    enum Kind {
        Infantry,
        Cavalry,
    }

    let units: HashMap<Kind, u32> = from_utf8_slice(b"infantry = 10 cavalry = 4").unwrap();
    assert_eq!(units[&Kind::Infantry], 10);
    assert_eq!(units[&Kind::Cavalry], 4);
}

#[test]
fn ignored_any_skips_large_values() {
    #[derive(Deserialize)]
    struct File {
        last: u32,
    }

    let mut data = String::from("big = { ");
    for i in 0..10_000 {
        data.push_str(&format!("a{i} = {{ b = {{ c = {i} }} }} "));
    }
    data.push_str("} last = 7");
    let file: File = from_utf8_slice(data.as_bytes()).unwrap();
    assert_eq!(file.last, 7);
}

// ---------------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------------

#[test]
fn errors_tell_the_location() {
    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct State {
        id: u32,
        provinces: Vec<u32>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(dead_code)]
    struct File {
        state: State,
    }

    let data = b"state = {\n\tid = 1\n\tprovinces = { 1 2 x }\n}";
    let parse = lossless::parse(&data[..]);
    let error = lossless::de::Deserializer::new(parse.tree(), Utf8Encoding::new())
        .with_file("history/states/1-France.txt")
        .deserialize::<File>()
        .unwrap_err();
    assert_eq!(error.path(), "state.provinces[2]");
    assert_eq!(error.file(), Some("history/states/1-France.txt"));
    let at = error.line_col().unwrap();
    assert_eq!((at.line, at.col), (2, 19));
    assert_eq!(&data[error.range().unwrap().to_range()], b"x");
    assert_eq!(
        error.to_string(),
        "history/states/1-France.txt:3:20: state.provinces[2]: invalid type: string \"x\", expected u32"
    );

    let error = from_utf8_slice::<File>(b"state = { provinces = { } }").unwrap_err();
    assert_eq!(error.path(), "state");
    assert!(error.message().contains("missing field `id`"), "{error}");

    let boxed: Box<dyn std::error::Error> = Box::new(error);
    assert!(!boxed.to_string().is_empty());
}

#[test]
fn syntax_errors() {
    let error = from_utf8_slice::<HashMap<String, String>>(b"a = \"x\nb = 2").unwrap_err();
    assert!(matches!(error.kind(), lossless::de::ErrorKind::Syntax(_)));
    assert_eq!(error.line_col().unwrap().to_string(), "1:5");

    // An extra closing brace is not an error, as with `TextTape`.
    let map: HashMap<String, u32> = from_utf8_slice(b"a = 1 } b = 2").unwrap();
    assert_eq!(map["b"], 2);

    // A tree with errors can still be deserialized.
    let parse = lossless::parse(b"a = { b = 1");
    let map: HashMap<String, HashMap<String, u32>> =
        from_tree(parse.tree(), Utf8Encoding::new()).unwrap();
    assert_eq!(map["a"]["b"], 1);
}

// ---------------------------------------------------------------------------
// Wrapper types
// ---------------------------------------------------------------------------

#[test]
fn quoted_and_unquoted_scalars() {
    // The use case of issue #138: expand a variable, but not quoted text.
    #[derive(Deserialize, Debug)]
    struct GameData {
        a: MaybeQuoted<String>,
        b: MaybeQuoted<String>,
        c: MaybeQuoted<u32>,
    }

    let data = b"@text = \"hello world\"\na = \"@text\"\nb = @text\nc = 3";
    let game: GameData = from_utf8_slice(data).unwrap();
    assert_eq!(game.a, MaybeQuoted::Quoted(String::from("@text")));
    assert_eq!(game.b, MaybeQuoted::Unquoted(String::from("@text")));
    assert_eq!(game.c, MaybeQuoted::Unquoted(3));
    assert!(game.a.is_quoted());

    let variables: HashMap<String, String> =
        HashMap::from([(String::from("@text"), String::from("hello world"))]);
    let expand = |value: &MaybeQuoted<String>| match value {
        MaybeQuoted::Unquoted(s) if s.starts_with('@') => variables[s].clone(),
        other => other.value().clone(),
    };
    assert_eq!(expand(&game.a), "@text");
    assert_eq!(expand(&game.b), "hello world");

    // A different deserializer gives no quote information.
    let tape = TextTape::from_slice(data).unwrap();
    let game: GameData = TextDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(game.a, MaybeQuoted::Unquoted(String::from("@text")));
}

#[test]
fn operators() {
    #[derive(Deserialize, Debug)]
    struct Trigger {
        age: WithOp<u32>,
        gold: WithOp<f32>,
        limit: WithOp<Vec<u32>>,
        legacy: Property<u32>,
        items: Vec<WithOp<u32>>,
    }

    let data = b"age >= 16 gold = 2.5 limit { 1 2 } legacy < 3 items = { 1 2 }";
    let trigger: Trigger = from_utf8_slice(data).unwrap();
    assert_eq!(trigger.age.op(), Some(Operator::GreaterThanEqual));
    assert_eq!(*trigger.age.value(), 16);
    assert_eq!(trigger.gold.op(), Some(Operator::Equal));
    assert_eq!(trigger.limit.op(), None);
    assert_eq!(trigger.legacy, Property::new(Operator::LessThan, 3));
    assert_eq!(trigger.items[0].op(), None);

    // A different deserializer gives no operator.
    #[derive(Deserialize, Debug)]
    struct Simple {
        age: WithOp<u32>,
    }
    let tape = TextTape::from_slice(b"age >= 16").unwrap();
    let simple: Simple = TextDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!((simple.age.op(), *simple.age.value()), (None, 16));
}

#[test]
fn headers_and_ranges() {
    #[derive(Deserialize, Debug)]
    struct Country {
        color: Headered<Vec<f32>>,
        plain: Headered<Vec<f32>>,
        name: Spanned<String>,
        block: Spanned<Headered<Spanned<Vec<u8>>>>,
    }

    let data =
        b"color = hsv { 0.5 0.5 1.0 } plain = { 1 2 } name = \"Paris\" block = rgb { 1 2 3 }";
    let country: Country = from_utf8_slice(data).unwrap();
    assert_eq!(country.color.header().map(String::as_str), Some("hsv"));
    assert_eq!(country.color.value(), &vec![0.5, 0.5, 1.0]);
    assert_eq!(country.plain.header(), None);
    assert_eq!(
        &data[country.name.range().unwrap().to_range()],
        b"\"Paris\""
    );

    let block = &country.block;
    assert_eq!(&data[block.range().unwrap().to_range()], b"rgb { 1 2 3 }");
    let inner = block.value().value();
    assert_eq!(&data[inner.range().unwrap().to_range()], b"{ 1 2 3 }");
    assert_eq!(inner.value(), &vec![1, 2, 3]);

    // A different deserializer gives no header and no range.
    #[derive(Deserialize, Debug)]
    struct Simple {
        name: Spanned<String>,
        plain: Headered<Vec<f32>>,
    }
    let tape = TextTape::from_slice(b"name = Paris plain = { 1 2 }").unwrap();
    let simple: Simple = TextDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(simple.name.range(), None);
    assert_eq!(simple.plain.header(), None);
}

// ---------------------------------------------------------------------------
// The owned value
// ---------------------------------------------------------------------------

#[test]
fn value_keeps_everything() {
    // The example of issue #148.
    let data = br#"
        unquoted = 1234
        quoted = "567"
        operator > 0.5
        color = hsv { 0.3 0.4 0.5 }
        sequence = { 1 2 3 4 }
        map = { a = 1 b = 2 }
        map = { a = 3 }
        mixed = { a = 1 10 }
        calc = @[1 + 2]
        [[param] x = 1 ]
    "#;
    let root: Value = from_utf8_slice(data).unwrap();
    let root = root.as_block().unwrap();

    let unquoted = root.get("unquoted").unwrap();
    assert_eq!(f32::deserialize(unquoted).unwrap(), 1234.0);
    assert_eq!(f32::deserialize(unquoted.clone()).unwrap(), 1234.0);
    assert!(!unquoted.is_quoted());
    assert!(root.get("quoted").unwrap().is_quoted());

    let operator = root.fields().find(|f| f.key == "operator").unwrap();
    assert_eq!(operator.op, Some(Operator::GreaterThan));
    assert_eq!(operator.value.to_f64(), Some(0.5));

    assert_eq!(root.get("color").unwrap().header(), Some("hsv"));
    assert!(root.get("sequence").unwrap().as_block().unwrap().is_array());
    assert!(root.get("map").unwrap().as_block().unwrap().is_object());
    assert_eq!(root.get_all("map").count(), 2);
    assert_eq!(
        root.get_last("map").unwrap().get("a").unwrap().to_i64(),
        Some(3)
    );
    assert!(root.get("mixed").unwrap().as_block().unwrap().is_mixed());
    assert_eq!(root.get("calc").unwrap().as_calc(), Some("@[1 + 2]"));
    assert!(matches!(root.items().last(), Some(Item::Parameter(p)) if p.name == "param"));

    // A value in a map of values.
    let map: HashMap<String, Value> = from_utf8_slice(b"a = { 1 2 } b = \"x\"").unwrap();
    assert_eq!(map["b"], Value::quoted("x"));
    let numbers: Vec<u8> = Vec::deserialize(map["a"].clone().into_deserializer()).unwrap();
    assert_eq!(numbers, vec![1, 2]);

    // The debug form shows the structure.
    let debug = format!("{:?}", root.get("color").unwrap());
    assert!(debug.starts_with("HeaderedBlock(\"hsv\""), "{debug}");
}

#[test]
fn value_from_a_different_deserializer() {
    // `TextDeserializer` does not know the private protocol, so the value
    // has no quotes, no operators, and no headers.
    // The root of `TextDeserializer` is only a map, so the value is in a map.
    let tape = TextTape::from_slice(b"root = { a = \"x\" b > 1 c = { 1 2 } }").unwrap();
    let map: HashMap<String, Value> = TextDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    let block = map["root"].as_block().unwrap();
    assert_eq!(block.get("a"), Some(&Value::unquoted("x")));
    assert_eq!(block.fields().nth(1).unwrap().op, Some(Operator::Equal));
    assert!(block.get("c").unwrap().as_block().unwrap().is_array());
}

/// The variable interpolation of PR #184: collect the `@` variables, then
/// replace each variable and each calculation with its value, then
/// deserialize the result into a type.
#[test]
fn value_variable_interpolation() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyObj {
        pos_x: i64,
        pos_y: i64,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct File {
        my_obj: MyObj,
    }

    let data = b"@my_var = 10\nmy_obj = {\n  pos_x = @[100-my_var]\n  pos_y = @my_var\n}";
    let mut value: Value = from_utf8_slice(data).unwrap();

    let variables: HashMap<String, f64> = value
        .as_block()
        .unwrap()
        .fields()
        .filter_map(|f| Some((f.key.strip_prefix('@')?.to_string(), f.value.to_f64()?)))
        .collect();

    fn interpolate(value: &mut Value, variables: &HashMap<String, f64>) {
        let replacement = match value {
            Value::Unquoted(s) => s.strip_prefix('@').and_then(|name| variables.get(name)),
            Value::Calc(s) => {
                let expr = s.trim_start_matches("@[").trim_end_matches(']');
                return *value = Value::Unquoted(eval(expr, variables).to_string());
            }
            Value::Block(block) | Value::HeaderedBlock(_, block) => {
                for entry in block.items_mut() {
                    match entry {
                        Item::Field(field) => interpolate(&mut field.value, variables),
                        Item::Value(value) => interpolate(value, variables),
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        };
        if let Some(x) = replacement {
            *value = Value::Unquoted(x.to_string());
        }
    }

    interpolate(&mut value, &variables);
    let file = File::deserialize(&value).unwrap();
    assert_eq!(
        file.my_obj,
        MyObj {
            pos_x: 90,
            pos_y: 10
        }
    );
}

/// Evaluate `a - b`, `a + b`, `a * b`, or `a / b` with numbers and variables.
fn eval(expr: &str, variables: &HashMap<String, f64>) -> f64 {
    let operand = |s: &str| {
        let s = s.trim();
        s.parse::<f64>().unwrap_or_else(|_| variables[s])
    };
    for op in ['+', '-', '*', '/'] {
        if let Some((a, b)) = expr.rsplit_once(op) {
            let (a, b) = (eval(a, variables), operand(b));
            return match op {
                '+' => a + b,
                '-' => a - b,
                '*' => a * b,
                _ => a / b,
            };
        }
    }
    operand(expr)
}

#[test]
fn value_construction() {
    let mut block = Block::new();
    block.push(Field::new("a", Some(Operator::Equal), Value::unquoted("1")));
    block.push(Value::quoted("loose"));
    let value = Value::from(block);
    assert!(value.as_block().unwrap().is_mixed());
    assert_eq!(value.get("a").unwrap().to_u64(), Some(1));

    let map: HashMap<String, Value> = HashMap::deserialize(&value).unwrap();
    assert_eq!(map["a"], Value::unquoted("1"));
}

// ---------------------------------------------------------------------------
// Document
// ---------------------------------------------------------------------------

#[test]
fn document_access() {
    let data = b"state = {\n  id = 1\n  name = \"STATE_1\"\n  color = rgb { 1 2 3 }\n  core = FRA core = BUR\n}";
    let parse = lossless::parse(&data[..]);
    let doc = Document::new(parse.tree(), Windows1252Encoding::new()).with_file("1.txt");
    doc.check().unwrap();

    let state = doc.root().require("state").unwrap().block().unwrap();
    assert_eq!(
        state.keys().collect::<Vec<_>>(),
        ["id", "name", "color", "core", "core"]
    );
    assert_eq!(
        state.require("id").unwrap().deserialize::<u32>().unwrap(),
        1
    );
    assert!(state.require("name").unwrap().is_quoted());
    assert_eq!(state.require("name").unwrap().str().unwrap(), "STATE_1");
    let cores: Vec<_> = state.get_all("core").map(|v| v.str().unwrap()).collect();
    assert_eq!(cores, ["FRA", "BUR"]);

    let color = state.require("color").unwrap();
    assert_eq!(color.header().as_deref(), Some("rgb"));
    assert_eq!(color.block().unwrap().values().count(), 3);
    assert_eq!(color.line_col().to_string(), "4:11");
    assert!(color.to_value().header().is_some());

    let error = color.str().unwrap_err();
    assert_eq!(error.path(), "state.color");
    assert_eq!(
        error.to_string(),
        "1.txt:4:11: state.color: expected a scalar, found a block"
    );

    let error = state
        .require("id")
        .unwrap()
        .deserialize::<bool>()
        .unwrap_err();
    assert_eq!(error.path(), "state.id");

    #[derive(JominiDeserialize)]
    struct State {
        id: u32,
        #[jomini(duplicated)]
        core: Vec<String>,
    }
    let typed: State = state.deserialize().unwrap();
    assert_eq!(typed.id, 1);
    assert_eq!(typed.core, ["FRA", "BUR"]);
}
