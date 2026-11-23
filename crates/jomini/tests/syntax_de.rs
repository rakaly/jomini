#![cfg(all(feature = "syntax", feature = "derive"))]

//! Tests for the serde deserializer of the syntax tree, its wrapper types,
//! the owned value, and the document access.
//!
//! The differential tests compare the tree deserializer with
//! `TextDeserializer`. When `TextDeserializer` gives a result, the tree
//! deserializer must give the same result, except for the constructs of
//! game files that the `contract` module lists.

use jomini::{
    JominiDeserialize, TextDeserializer, TextTape, Utf8Encoding, Windows1252Encoding,
    text::{
        Headered, MaybeQuoted, Operator, Property,
        de::TextTapeDeserializer,
        syntax::{
            self, OwnedBlock, OwnedField, OwnedItem, OwnedScalar, OwnedValue,
            de::{Document, Error, Spanned},
        },
    },
};
use serde::{
    Deserialize,
    de::{
        self, DeserializeOwned, Deserializer, IgnoredAny, IntoDeserializer, MapAccess, SeqAccess,
    },
};
use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

mod contract;

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

/// `TextDeserializer` from a slice, which gives the same values as from a
/// reader.
fn streaming<T: DeserializeOwned>(data: &[u8]) -> Option<T> {
    TextDeserializer::from_slice(data, Windows1252Encoding::new())
        .deserialize()
        .ok()
}

/// A flavor that reads `@name` as an unquoted scalar.
struct NoVariables;

impl jomini::Encoding for NoVariables {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

impl jomini::text::TextFlavor for NoVariables {
    fn variables(&self) -> bool {
        false
    }
}

fn tree<T: DeserializeOwned>(data: &[u8]) -> Result<T, Error> {
    let parse = syntax::parse(data);
    parse.document(Windows1252Encoding::new()).deserialize()
}

/// Compare the tree deserializer, the borrowed value, and the owned value
/// with `TextDeserializer`. Return `true` when `TextDeserializer` gives a
/// result.
fn compare<T: DeserializeOwned + PartialEq + fmt::Debug>(data: &[u8]) -> bool {
    let Some(expected) = streaming::<T>(data) else {
        return false;
    };
    let name = String::from_utf8_lossy(data);
    let actual = tree::<T>(data);
    // The constructs of game files that `TextDeserializer` reads differently.
    if actual.as_ref().ok() != Some(&expected) && contract::known_difference(data).is_some() {
        return false;
    }
    let actual = actual.unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(actual, expected, "tree: {name}");

    let parse = syntax::parse(data);
    let root = OwnedBlock::from_tree(parse.tree(), Windows1252Encoding::new());
    let borrowed = T::deserialize(&root).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(borrowed, expected, "borrowed block: {name}");
    let owned = T::deserialize(root).unwrap_or_else(|e| panic!("{name}: {e}"));
    assert_eq!(owned, expected, "owned block: {name}");
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
    r#"path = "gfx\models\ships\cog.mesh" icon = "gfx\\interface\English_flag.tga""#,
    r#"effect = "set_variable = { value = \"a\\b\" }" tip = "+50%\n" dir = gfx\\icons"#,
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
        if compare::<AnyRoot>(data) {
            compared += 1;
            compare_owned_blocks(data);
        }
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
        compare_owned_blocks(data);
    }
}

/// The owned block of `TextDeserializer`, from a slice and from a reader,
/// is the owned block of the tree, and the block of the tape deserializes
/// into the same result where the tape reads the data as `TextDeserializer`
/// does. The blocks of the tape can have a different form: the tape does not
/// keep the difference between `key = { }` and `key { }`, it drops the empty
/// blocks at the start of an array, and in an array it reads `key { }` as
/// two values.
fn compare_owned_blocks(data: &[u8]) {
    let name = String::from_utf8_lossy(&data[..data.len().min(40)]);
    let encoding = Windows1252Encoding::new();
    let parse = syntax::parse(data);
    let from_tree = OwnedBlock::from_tree(parse.tree(), encoding);
    let from_slice: OwnedBlock = TextDeserializer::from_slice(data, encoding)
        .deserialize()
        .unwrap();
    assert_eq!(from_slice, from_tree, "{name}");
    let from_reader: OwnedBlock =
        TextDeserializer::from_reader_with_buf(data, encoding, vec![0; 64])
            .deserialize()
            .unwrap();
    assert_eq!(from_reader, from_tree, "{name}");

    let tape = TextTape::from_slice(data).unwrap();
    let from_tape: OwnedBlock = TextTapeDeserializer::from_windows1252_tape(&tape)
        .deserialize()
        .unwrap();
    let tape_any = TextTapeDeserializer::from_windows1252_tape(&tape).deserialize::<AnyRoot>();
    if tape_any.ok() == streaming::<AnyRoot>(data) {
        let expected = AnyRoot::deserialize(&from_tree).unwrap();
        assert_eq!(
            AnyRoot::deserialize(&from_tape).unwrap(),
            expected,
            "{name}"
        );
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
    let areas: Areas = syntax::parse(data)
        .deserialize(Windows1252Encoding::new())
        .unwrap();
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

    let file: File = syntax::parse(b"color = rgb { 1 2 3 }")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.color, (String::from("rgb"), vec![1, 2, 3]));
}

/// EU5 `named_colors/02_map.txt` has a color or the name of a different
/// color in the same position. `TextDeserializer` gives a
/// scalar as a sequence of one element, so a visitor can accept both forms.
#[test]
fn scalar_as_seq() {
    #[derive(Debug, PartialEq)]
    enum Color {
        Rgb(Vec<u8>),
        Alias(String),
    }

    impl<'de> Deserialize<'de> for Color {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            struct ColorVisitor;
            impl<'de> de::Visitor<'de> for ColorVisitor {
                type Value = Color;
                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("a color or a color name")
                }
                fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Color, A::Error> {
                    let head: String = seq.next_element()?.unwrap();
                    if head != "rgb" {
                        return Ok(Color::Alias(head));
                    }
                    Ok(Color::Rgb(seq.next_element()?.unwrap()))
                }
            }
            deserializer.deserialize_seq(ColorVisitor)
        }
    }

    let data = b"colors = { map_tunis = rgb { 1 2 3 } governor = map_tunis }";
    let reader: HashMap<String, HashMap<String, Color>> =
        jomini::text::de::from_utf8_reader(&data[..]).unwrap();
    let tree: HashMap<String, HashMap<String, Color>> = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(tree, reader);
    assert_eq!(
        tree["colors"]["governor"],
        Color::Alias(String::from("map_tunis"))
    );

    let tags = Vec::<String>::deserialize(&OwnedValue::unquoted("FRA")).unwrap();
    assert_eq!(tags, vec![String::from("FRA")]);
}

#[test]
fn borrow_from_the_source() {
    #[derive(Deserialize)]
    struct File<'a> {
        name: &'a str,
        tags: Vec<&'a str>,
    }

    let data = b"name = \"Paris\" tags = { a b }";
    let file: File = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.name, "Paris");
    assert_eq!(file.tags, vec!["a", "b"]);
}

#[test]
fn encodings() {
    #[derive(Deserialize)]
    struct File {
        name: String,
    }

    let file: File = syntax::parse(b"name = \"K\xe4rnten\"")
        .deserialize(Windows1252Encoding::new())
        .unwrap();
    assert_eq!(file.name, "K\u{e4}rnten");

    let file: File = syntax::parse("\u{feff}name = \"K\u{e4}rnten\"".as_bytes())
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.name, "K\u{e4}rnten");

    let file: File = syntax::parse("name = Zürich".as_bytes())
        .deserialize(Utf8Encoding::new())
        .unwrap();
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

    let units: HashMap<Kind, u32> = syntax::parse(b"infantry = 10 cavalry = 4")
        .deserialize(Utf8Encoding::new())
        .unwrap();
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
    let file: File = syntax::parse(data.as_bytes())
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.last, 7);
}

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
    let parse = syntax::parse(&data[..]);
    let error = parse
        .document(Utf8Encoding::new())
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

    let error = syntax::parse(b"state = { provinces = { } }")
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(error.path(), "state");
    assert!(error.message().contains("missing field `id`"), "{error}");
}

#[test]
fn syntax_errors() {
    let error = syntax::parse(b"a = \"x\nb = 2")
        .deserialize::<HashMap<String, String>, _>(Utf8Encoding::new())
        .unwrap_err();
    assert!(matches!(error.kind(), syntax::de::ErrorKind::Syntax(_)));
    assert_eq!(error.line_col().unwrap().to_string(), "1:5");

    // An extra closing brace is not an error, as with `TextTape`.
    let map: HashMap<String, u32> = syntax::parse(b"a = 1 } b = 2")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(map["b"], 2);
}

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
    let game: GameData = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
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

    // `TextDeserializer` also knows the quotes, from a slice and a reader.
    let slice: GameData = jomini::text::de::from_utf8_slice(data).unwrap();
    assert_eq!(slice.a, MaybeQuoted::Quoted(String::from("@text")));
    assert_eq!(slice.b, MaybeQuoted::Unquoted(String::from("@text")));
    let reader: GameData = jomini::text::de::from_utf8_reader(&data[..]).unwrap();
    assert_eq!(reader.a, MaybeQuoted::Quoted(String::from("@text")));
    assert_eq!(reader.b, MaybeQuoted::Unquoted(String::from("@text")));
}

#[test]
fn quoted_scalars_are_unescaped() {
    let data = br#"path = "gfx\models\cog.mesh" icon = "gfx\\interface\flag.tga" effect = "x = \"a\\b\"" dir = gfx\\icons"#;
    let expected = BTreeMap::from([
        ("dir", r"gfx\\icons"),
        ("effect", r#"x = "a\b""#),
        ("icon", r"gfx\interface\flag.tga"),
        ("path", r"gfx\models\cog.mesh"),
    ])
    .into_iter()
    .map(|(k, v)| (k.to_string(), v.to_string()))
    .collect::<BTreeMap<_, _>>();

    let tree: BTreeMap<String, String> = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(tree, expected);
    let streaming: BTreeMap<String, String> = jomini::text::de::from_utf8_slice(data).unwrap();
    assert_eq!(streaming, expected);
}

/// A source of `depth` nested `a` blocks with `x=1` in the innermost one.
fn deep_blocks(depth: usize) -> String {
    format!("{}x=1{}", "a={".repeat(depth), "}".repeat(depth))
}

/// The number of `a` blocks that nest in `block`, and the innermost block.
fn nested_a_blocks(mut block: &OwnedBlock) -> (usize, &OwnedBlock) {
    let mut levels = 0;
    while let Some(inner) = block.get("a").and_then(OwnedValue::as_block) {
        block = inner;
        levels += 1;
    }
    (levels, block)
}

#[derive(Deserialize)]
struct DeepField {
    a: OwnedValue,
}

#[test]
fn owned_block_of_deep_blocks() {
    // EU4 saves nest blocks more than 2,000 levels deep.
    let depth = 3000;
    let source = deep_blocks(depth);
    let parse = syntax::parse(source.as_bytes());
    let root = OwnedBlock::from_tree(parse.tree(), Utf8Encoding::new());
    let (levels, inner) = nested_a_blocks(&root);
    assert_eq!(levels, depth);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));
}

#[test]
fn deserialize_deep_blocks_from_tree() {
    let depth = 3000;
    let source = deep_blocks(depth);
    let parse = syntax::parse(source.as_bytes());

    let root: OwnedBlock = parse.document(Utf8Encoding::new()).deserialize().unwrap();
    let (levels, inner) = nested_a_blocks(&root);
    assert_eq!(levels, depth);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));

    let field: DeepField = parse.document(Utf8Encoding::new()).deserialize().unwrap();
    let (levels, inner) = nested_a_blocks(field.a.as_block().unwrap());
    assert_eq!(levels, depth - 1);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));

    // An owned block is also a deserializer of the same rules.
    let copy = OwnedBlock::deserialize(&root).unwrap();
    let (levels, inner) = nested_a_blocks(&copy);
    assert_eq!(levels, depth);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));
}

#[test]
fn deserialize_deep_blocks_from_tape() {
    let depth = 3000;
    let source = deep_blocks(depth);
    let tape = syntax::parse_tape(source.as_bytes()).into_result().unwrap();

    let root: OwnedBlock = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    let (levels, inner) = nested_a_blocks(&root);
    assert_eq!(levels, depth);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));

    let field: DeepField = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    let (levels, inner) = nested_a_blocks(field.a.as_block().unwrap());
    assert_eq!(levels, depth - 1);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));
}

#[test]
fn drop_deep_blocks() {
    // The block is made in a loop, so only the drop could overflow the
    // stack of the test thread.
    let depth = 50_000;
    let mut block = OwnedBlock::new();
    block.push(OwnedField::new(
        "x",
        Some(Operator::Equal),
        OwnedValue::unquoted("1"),
    ));
    for _ in 0..depth {
        let mut outer = OwnedBlock::new();
        outer.push(OwnedField::new("a", Some(Operator::Equal), block.into()));
        block = outer;
    }
    let (levels, inner) = nested_a_blocks(&block);
    assert_eq!(levels, depth);
    assert_eq!(inner.get("x"), Some(&OwnedValue::unquoted("1")));
    drop(block);
}

#[test]
fn operators() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Trigger {
        age: Property<u32>,
        gold: Property<f32>,
        limit: Property<Vec<u32>>,
    }

    // A field with no operator has `=`, as with each deserializer of the
    // crate.
    let data = b"age >= 16 gold = 2.5 limit { 1 2 }";
    let expected = Trigger {
        age: Property::new(Operator::GreaterThanEqual, 16),
        gold: Property::new(Operator::Equal, 2.5),
        limit: Property::new(Operator::Equal, vec![1, 2]),
    };
    assert_eq!(
        syntax::parse(data)
            .deserialize::<Trigger, _>(Utf8Encoding::new())
            .unwrap(),
        expected
    );
    let root: OwnedBlock = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(Trigger::deserialize(&root).unwrap(), expected);
    let tape: Trigger = jomini::text::de::from_utf8_slice(data).unwrap();
    assert_eq!(tape, expected);
    let reader: Trigger = jomini::text::de::from_utf8_reader(&data[..]).unwrap();
    assert_eq!(reader, expected);

    // An element of a sequence is not a field, so it has no operator.
    let error = syntax::parse(b"items = { 1 2 }")
        .deserialize::<HashMap<String, Vec<Property<u32>>>, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "1:11: items[0]: invalid type: string \"1\", expected struct Property"
    );
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
    let country: Country = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
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

    // `TextDeserializer` knows the header, but not the range.
    #[derive(Deserialize, Debug)]
    struct Simple {
        name: Spanned<String>,
        color: Headered<Vec<f32>>,
    }
    let tape = TextTape::from_slice(b"name = Paris color = hsv { 1 2 }").unwrap();
    let simple: Simple = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(simple.name.range(), None);
    assert_eq!(simple.color.header().map(String::as_str), Some("hsv"));
    assert_eq!(simple.color.value(), &[1.0, 2.0]);
}

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
    let root: OwnedValue = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let root = root.as_block().unwrap();

    let unquoted = root.get("unquoted").unwrap();
    assert_eq!(f32::deserialize(unquoted).unwrap(), 1234.0);
    assert_eq!(f32::deserialize(unquoted.clone()).unwrap(), 1234.0);
    assert_eq!(unquoted, &OwnedValue::unquoted("1234"));
    assert_eq!(root.get("quoted"), Some(&OwnedValue::quoted("567")));

    let operator = root.fields().find(|f| f.key == "operator").unwrap();
    assert_eq!(operator.op, Some(Operator::GreaterThan));
    assert_eq!(operator.value.to_f64(), Some(0.5));

    assert_eq!(root.get("color").unwrap().header().unwrap(), "hsv");
    assert!(root.get("sequence").unwrap().as_block().unwrap().is_array());
    assert!(root.get("map").unwrap().as_block().unwrap().is_object());
    assert_eq!(root.get_all("map").count(), 2);
    assert_eq!(
        root.get_last("map").unwrap().get("a").unwrap().to_i64(),
        Some(3)
    );
    assert!(root.get("mixed").unwrap().as_block().unwrap().is_mixed());
    assert_eq!(root.get("calc").unwrap().as_calc(), Some("@[1 + 2]"));
    assert!(matches!(root.items().last(), Some(OwnedItem::Parameter(p)) if p.name == "param"));

    // A value in a map of values.
    let map: HashMap<String, OwnedValue> = syntax::parse(b"a = { 1 2 } b = \"x\"")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(map["b"], OwnedValue::quoted("x"));
    let numbers: Vec<u8> = Vec::deserialize(map["a"].clone().into_deserializer()).unwrap();
    assert_eq!(numbers, vec![1, 2]);
}

#[test]
fn value_from_text_deserializer() {
    // The example of issue #148, with `TextDeserializer`.
    let data = br#"
        unquoted = 1234
        quoted = "567"
        operator > 0.5
        color = hsv { 0.3 0.4 0.5 }
        sequence = { 1 2 3 4 }
        map = { a = 1 b = 2 }
    "#;

    // A map of values has no place for the operator of a field.
    let map: HashMap<String, OwnedValue> = jomini::text::de::from_utf8_slice(data).unwrap();
    assert_eq!(f32::deserialize(map["unquoted"].clone()).unwrap(), 1234.0);
    assert_eq!(map["quoted"], OwnedValue::quoted("567"));
    assert_eq!(map["operator"], OwnedValue::unquoted("0.5"));
    let (header, body) = map["color"].as_headered_block().unwrap();
    assert_eq!(
        (header, body.len()),
        (&OwnedScalar::Unquoted("hsv".into()), 3)
    );

    // A block keeps the operator, and it is the same as the block of the
    // tree.
    let root: OwnedBlock = jomini::text::de::from_utf8_slice(data).unwrap();
    let operator = root.fields().find(|f| f.key == "operator").unwrap();
    assert_eq!(operator.op, Some(Operator::GreaterThan));
    let tree: OwnedBlock = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(root, tree);

    // A reader makes the values of the tree too.
    let reader: HashMap<String, OwnedValue> =
        jomini::text::de::from_utf8_reader(&data[..]).unwrap();
    let tree: HashMap<String, OwnedValue> = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(reader, tree);
    let reader: OwnedBlock = jomini::text::de::from_utf8_reader(&data[..]).unwrap();
    assert_eq!(reader, root);
}

#[test]
fn value_keeps_variables() {
    #[derive(Deserialize)]
    struct File {
        size: String,
        text: MaybeQuoted<String>,
    }

    let data = b"@size = 0.5\nsize = @size\ntext = @size\nquoted = \"@size\"";
    let value: OwnedValue = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let root = value.as_block().unwrap();

    let definition = root.fields().next().unwrap();
    assert_eq!(definition.key, OwnedScalar::Variable(String::from("@size")));
    assert_eq!(
        root.fields().nth(1).unwrap().key,
        OwnedScalar::Unquoted(String::from("size"))
    );

    assert_eq!(
        root.get("size"),
        Some(&OwnedValue::Scalar(OwnedScalar::Variable(String::from(
            "@size"
        ))))
    );
    assert_eq!(root.get("size").unwrap().as_str(), Some("@size"));
    assert_eq!(root.get("quoted"), Some(&OwnedValue::quoted("@size")));

    // A value deserializes into a value with the same variables.
    assert_eq!(OwnedValue::deserialize(&value).unwrap(), value);

    // A text target gets the source text, and `MaybeQuoted` tells that the
    // text had no quotes.
    for file in [
        syntax::parse(data)
            .deserialize::<File, _>(Utf8Encoding::new())
            .unwrap(),
        File::deserialize(&value).unwrap(),
    ] {
        assert_eq!(file.size, "@size");
        assert_eq!(file.text, MaybeQuoted::Unquoted(String::from("@size")));
    }

    let tree = syntax::parse(data);
    let doc = tree.document(Utf8Encoding::new());
    let root = doc.root();
    assert!(root.fields().next().unwrap().is_variable_definition());
    let variable = |key: &[u8]| {
        let value = doc.require(root, key).unwrap();
        value.as_scalar_token().unwrap().variable_name()
    };
    assert_eq!(variable(b"size"), Some(&b"size"[..]));
    assert_eq!(variable(b"quoted"), None);

    // A flavor without variables makes `@size` an unquoted scalar.
    let tree = syntax::parse_with(data, &NoVariables);
    let root = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    assert_eq!(
        root.fields().next().unwrap().key,
        OwnedScalar::Unquoted(String::from("@size"))
    );
    assert_eq!(root.get("size"), Some(&OwnedValue::unquoted("@size")));
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
    let mut value: OwnedValue = syntax::parse(data)
        .deserialize(Utf8Encoding::new())
        .unwrap();

    let variables: HashMap<String, f64> = value
        .as_block()
        .unwrap()
        .fields()
        .filter(|f| f.key.is_variable())
        .filter_map(|f| Some((f.key.as_str()[1..].to_string(), f.value.to_f64()?)))
        .collect();

    fn interpolate(value: &mut OwnedValue, variables: &HashMap<String, f64>) {
        let replacement = match value {
            OwnedValue::Scalar(OwnedScalar::Variable(s)) => variables.get(&s[1..]),
            OwnedValue::Calc(s) => {
                let expr = s.trim_start_matches("@[").trim_end_matches(']');
                return *value = OwnedValue::unquoted(eval(expr, variables).to_string());
            }
            OwnedValue::Block(block) | OwnedValue::HeaderedBlock(_, block) => {
                for entry in block.items_mut() {
                    match entry {
                        OwnedItem::Field(field) => interpolate(&mut field.value, variables),
                        OwnedItem::Value(value) => interpolate(value, variables),
                        _ => {}
                    }
                }
                None
            }
            _ => None,
        };
        if let Some(x) = replacement {
            *value = OwnedValue::unquoted(x.to_string());
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
    let mut block = OwnedBlock::new();
    block.push(OwnedField::new(
        "a",
        Some(Operator::Equal),
        OwnedValue::unquoted("1"),
    ));
    block.push(OwnedValue::quoted("loose"));
    let value = OwnedValue::from(block);
    assert!(value.as_block().unwrap().is_mixed());
    assert_eq!(value.get("a").unwrap().to_u64(), Some(1));

    let map: HashMap<String, OwnedValue> = HashMap::deserialize(&value).unwrap();
    assert_eq!(map["a"], OwnedValue::unquoted("1"));
}

#[test]
fn document_access() {
    let data = b"state = {\n  id = 1\n  name = \"STATE_1\"\n  color = rgb { 1 2 3 }\n  core = FRA core = BUR\n  empty =\n}";
    let parse = syntax::parse(&data[..]);
    let doc = parse
        .document(Windows1252Encoding::new())
        .with_file("1.txt");
    let error = doc.check().unwrap_err();
    assert_eq!(error.to_string(), "1.txt:6:9: operator has no value");

    let state = doc
        .block(doc.require(doc.root(), b"state").unwrap())
        .unwrap();
    assert_eq!(
        doc.deserialize_value::<u32>(doc.require(state, b"id").unwrap())
            .unwrap(),
        1
    );
    let name = doc.require(state, b"name").unwrap();
    assert!(name.as_scalar_token().unwrap().is_quoted());
    assert_eq!(doc.str(name).unwrap(), "STATE_1");
    let cores: Vec<_> = state
        .get_all(b"core")
        .map(|f| doc.str(doc.value(f).unwrap()).unwrap())
        .collect();
    assert_eq!(cores, ["FRA", "BUR"]);

    let color = doc.require(state, b"color").unwrap();
    assert_eq!(doc.block(color).unwrap().values().count(), 3);
    assert_eq!(doc.line_col(color.text_range().start()).to_string(), "4:11");
    assert_eq!(doc.to_owned_value(color).unwrap().header().unwrap(), "rgb");

    let error = doc.str(color).unwrap_err();
    assert_eq!(error.path(), "state.color");
    assert_eq!(
        error.to_string(),
        "1.txt:4:11: state.color: expected a scalar, found a block"
    );

    let error = doc.require(state, b"capital").unwrap_err();
    assert_eq!(error.to_string(), "1.txt:1:9: state: missing key `capital`");

    let error = doc.require(state, b"empty").unwrap_err();
    assert_eq!(error.path(), "state");
    let empty = state.get(b"empty").unwrap();
    let error = doc.value(empty).unwrap_err();
    assert_eq!(
        error.to_string(),
        "1.txt:6:3: state.empty: expected a value, found none"
    );

    let error = doc
        .deserialize_value::<bool>(doc.require(state, b"id").unwrap())
        .unwrap_err();
    assert_eq!(error.path(), "state.id");

    #[derive(JominiDeserialize)]
    struct State {
        id: u32,
        #[jomini(duplicated)]
        core: Vec<String>,
    }
    let typed: State = doc.deserialize_value(syntax::Value::Block(state)).unwrap();
    assert_eq!(typed.id, 1);
    assert_eq!(typed.core, ["FRA", "BUR"]);
}

#[test]
fn key_errors_tell_the_key() {
    #[derive(Deserialize, Debug, Hash, PartialEq, Eq)]
    #[serde(rename_all = "lowercase")]
    enum Kind {
        Foo,
    }

    #[derive(Deserialize, Debug)]
    struct File {
        #[allow(dead_code)]
        m: HashMap<Kind, u32>,
    }

    let error = syntax::parse(b"m = {\n  foo = 1\n  bar = 2\n}")
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(error.path(), "m.bar");
    assert_eq!(error.line_col().unwrap().to_string(), "3:3");
}

#[test]
fn reader_directives_are_not_entries() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct File {
        x: String,
        b: u32,
        list: Vec<u32>,
    }

    let source = b"@:define m = { a = 1 }\n@:insert m\nx = @:load_variable v\nb = 2\nlist = { 1 @:assert 2 }";
    let file: File = syntax::parse(source)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(
        file,
        File {
            x: String::from("@:load_variable v"),
            b: 2,
            list: vec![1, 2],
        }
    );
}

#[test]
fn document_rejects_an_item_of_a_different_tree() {
    let a = syntax::parse(b"x = 1");
    let b = syntax::parse(b"x = 2");
    let doc = a.document(Utf8Encoding::new()).with_file("a.txt");
    let field = b.ast().get(b"x").unwrap();
    let value = field.value().unwrap();

    let message = "a.txt: the item is from a different tree";
    assert_eq!(
        doc.deserialize_value::<u32>(value).unwrap_err().to_string(),
        message
    );
    assert_eq!(doc.to_owned_value(value).unwrap_err().to_string(), message);
    assert_eq!(doc.str(value).unwrap_err().to_string(), message);
    assert_eq!(doc.block(value).unwrap_err().to_string(), message);
    assert_eq!(doc.value(field).unwrap_err().to_string(), message);
    assert_eq!(doc.require(b.ast(), b"x").unwrap_err().to_string(), message);

    let own = a.ast().get(b"x").unwrap().value().unwrap();
    assert_eq!(doc.deserialize_value::<u32>(own).unwrap(), 1);
}

#[test]
fn header_deserializes_into_an_enum_or_a_newtype() {
    #[derive(Deserialize, Debug, PartialEq)]
    enum Space {
        #[serde(rename = "rgb")]
        Rgb,
        #[serde(rename = "hsv")]
        Hsv,
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct Name(String);

    #[derive(Deserialize, Debug)]
    struct Colors {
        a: Headered<[u8; 3], Space>,
        b: Headered<[u8; 3], Space>,
        c: Headered<[u8; 3], Name>,
    }

    let colors: Colors = syntax::parse(b"a = rgb { 1 2 3 } b = hsv { 4 5 6 } c = hsv { 7 8 9 }")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(colors.a.into_parts(), (Some(Space::Rgb), [1, 2, 3]));
    assert_eq!(colors.b.into_parts(), (Some(Space::Hsv), [4, 5, 6]));
    assert_eq!(colors.c.into_parts(), (Some(Name("hsv".into())), [7, 8, 9]));

    let error = syntax::parse(b"a = cmyk { 1 2 3 }")
        .deserialize::<Colors, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "1:5: a: unknown variant `cmyk`, expected `rgb` or `hsv`"
    );
}

#[test]
fn root_error_has_the_position_of_the_root() {
    #[derive(Deserialize, Debug)]
    struct State {
        #[allow(dead_code)]
        id: u32,
    }

    let error = syntax::parse(b"\nother = 1")
        .deserialize::<State, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(
        error.range(),
        Some(syntax::TextRange::new(0.into(), 10.into()))
    );
    assert_eq!(error.to_string(), "1:1: missing field `id`");

    let tree = syntax::parse(b"\nother = 1");
    let doc = tree.document(Utf8Encoding::new()).with_file("s.txt");
    let error = doc.deserialize::<State>().unwrap_err();
    assert_eq!(error.to_string(), "s.txt:1:1: missing field `id`");
}

#[test]
fn deserialize_field_keeps_the_operator() {
    let tree = syntax::parse(b"age >= 16 legacy < 3 size = 2");
    let doc = tree.document(Utf8Encoding::new());
    let field = |key: &[u8]| doc.root().get(key).unwrap();

    let age: Property<u32> = doc.deserialize_field(field(b"age")).unwrap();
    assert_eq!(age, Property::new(Operator::GreaterThanEqual, 16));
    let legacy: Property<u32> = doc.deserialize_field(field(b"legacy")).unwrap();
    assert_eq!(legacy, Property::new(Operator::LessThan, 3));
    let size: u32 = doc.deserialize_field(field(b"size")).unwrap();
    assert_eq!(size, 2);

    // The value alone does not know the operator.
    let error = doc
        .deserialize_value::<Property<u32>>(doc.value(field(b"age")).unwrap())
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "1:8: age: invalid type: string \"16\", expected struct Property"
    );
}

#[test]
fn document_rejects_an_operator_value_that_is_not_an_operator() {
    let tree = syntax::parse(b"# comment\na = 1");
    let doc = tree.document(Utf8Encoding::new());
    let comment = doc.tree().root().first_token().unwrap();
    let error = doc.str(syntax::Value::Operator(comment)).unwrap_err();
    assert_eq!(
        error.to_string(),
        "1:1: expected an operator, found Comment"
    );

    let key = doc.root().get(b"a").unwrap().key().syntax();
    let error = doc
        .deserialize_value::<u32>(syntax::Value::Operator(key))
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "2:1: expected an operator, found Unquoted"
    );
}

#[test]
fn get_all_borrows_only_the_block() {
    fn matches(block: &OwnedBlock, key: String) -> Vec<&OwnedValue> {
        block.get_all(&key).collect()
    }

    let root: OwnedValue = syntax::parse(b"core = FRA core = BUR")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let cores = matches(root.as_block().unwrap(), String::from("core"));
    assert_eq!(
        cores,
        [&OwnedValue::unquoted("FRA"), &OwnedValue::unquoted("BUR")]
    );
}

#[test]
fn root_conversion_error_has_the_position_of_the_root() {
    #[derive(Deserialize)]
    struct Raw {
        id: u32,
    }

    #[derive(Deserialize, Debug)]
    #[serde(try_from = "Raw")]
    struct Validated {
        #[allow(dead_code)]
        id: u32,
    }

    impl TryFrom<Raw> for Validated {
        type Error = String;

        fn try_from(raw: Raw) -> Result<Self, String> {
            match raw.id {
                0 => Err(String::from("id must not be 0")),
                id => Ok(Validated { id }),
            }
        }
    }

    let tree = syntax::parse(b"id = 0");
    let error = tree
        .document(Utf8Encoding::new())
        .with_file("a.txt")
        .deserialize::<Validated>()
        .unwrap_err();
    assert_eq!(error.to_string(), "a.txt:1:1: id must not be 0");
}

/// The size hint that a map or a sequence gives to its visitor.
#[derive(Debug, PartialEq)]
struct Hint(Option<usize>);

impl<'de> Deserialize<'de> for Hint {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct HintVisitor;

        impl<'de> serde::de::Visitor<'de> for HintVisitor {
            type Value = Hint;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a map or a sequence")
            }

            fn visit_seq<A: serde::de::SeqAccess<'de>>(self, mut seq: A) -> Result<Hint, A::Error> {
                let hint = seq.size_hint();
                while seq.next_element::<serde::de::IgnoredAny>()?.is_some() {}
                Ok(Hint(hint))
            }

            fn visit_map<A: serde::de::MapAccess<'de>>(self, mut map: A) -> Result<Hint, A::Error> {
                let hint = map.size_hint();
                while map
                    .next_entry::<serde::de::IgnoredAny, serde::de::IgnoredAny>()?
                    .is_some()
                {}
                Ok(Hint(hint))
            }
        }

        deserializer.deserialize_any(HintVisitor)
    }
}

#[test]
fn size_hint_is_exact_or_none() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Hints {
        nested: Hint,
        leaf: Hint,
        headered: Hint,
    }

    let source = b"nested = { { 1 } { 2 } } leaf = { 1 2 3 } headered = { rgb { 1 } 2 }";
    let expected = Hints {
        nested: Hint(None),
        leaf: Hint(Some(3)),
        headered: Hint(None),
    };
    assert_eq!(
        syntax::parse(source)
            .deserialize::<Hints, _>(Utf8Encoding::new())
            .unwrap(),
        expected
    );
    assert_eq!(
        syntax::parse(b"a = 1")
            .deserialize::<Hint, _>(Utf8Encoding::new())
            .unwrap(),
        Hint(None)
    );

    let value: OwnedValue = syntax::parse(source)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let hint = |key: &str| Hint::deserialize(value.get(key).unwrap()).unwrap();
    assert_eq!(hint("leaf"), Hint(Some(3)));
    assert_eq!(hint("nested"), Hint(Some(2)));
    assert_eq!(hint("headered"), Hint(None));
}

#[test]
fn partial_read_error_has_the_index_of_the_row() {
    #[derive(Deserialize, Debug)]
    struct Row {
        #[allow(dead_code)]
        id: u32,
    }

    #[derive(Deserialize, Debug)]
    struct File {
        #[allow(dead_code)]
        rows: Vec<Row>,
    }

    let source = b"rows = { { id = 0 } { id = 1 } { id = bad } }";
    let whole = syntax::parse(source)
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(whole.path(), "rows[2].id");

    let tree = syntax::parse(source);
    let doc = tree.document(Utf8Encoding::new());
    let rows = doc
        .block(doc.require(doc.root(), b"rows").unwrap())
        .unwrap();
    let row = rows.values().nth(2).unwrap();
    let error = doc.deserialize_value::<Row>(row).unwrap_err();
    assert_eq!(error.path(), "rows[2].id");
    assert_eq!(error.to_string(), whole.to_string());
}

#[test]
fn partial_read_error_has_the_name_of_the_parameter() {
    #[derive(Deserialize, Debug)]
    struct Row {
        #[allow(dead_code)]
        id: u32,
    }

    #[derive(Deserialize, Debug)]
    struct File {
        #[allow(dead_code)]
        p: Row,
    }

    let source = b"[[p] id = bad ]";
    let whole = syntax::parse(source)
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(whole.path(), "p.id");

    let tree = syntax::parse(source);
    let doc = tree.document(Utf8Encoding::new());
    let item = doc.root().items().next().unwrap();
    let parameter = item.as_parameter().unwrap();
    let field = parameter.fields().next().unwrap();
    let error = doc.deserialize_field::<u32>(field).unwrap_err();
    assert_eq!(error.path(), "p.id");
}

#[test]
fn partial_read_errors_of_many_rows_have_their_indices() {
    #[derive(Deserialize, Debug)]
    struct Row {
        #[allow(dead_code)]
        id: u32,
    }

    // An empty block is an element.
    let source = b"rows = { {} { id = a } { id = 1 } { id = b } {} { id = c } }";
    let tree = syntax::parse(source);
    let doc = tree.document(Utf8Encoding::new());
    let rows = doc
        .block(doc.require(doc.root(), b"rows").unwrap())
        .unwrap();
    let paths: Vec<String> = rows
        .values()
        .filter_map(|row| doc.deserialize_value::<Row>(row).err())
        .map(|error| error.path())
        .collect();
    assert_eq!(
        paths,
        [
            "rows[0]",
            "rows[1].id",
            "rows[3].id",
            "rows[4]",
            "rows[5].id"
        ]
    );
}

#[test]
fn partial_read_index_counts_each_entry_that_the_deserializer_reads() {
    #[derive(Deserialize, Debug)]
    struct Tuple {
        #[allow(dead_code)]
        values: (String, u32),
    }

    #[derive(Deserialize, Debug)]
    struct List {
        #[allow(dead_code)]
        values: Vec<u32>,
    }

    let partial = |source: &[u8]| {
        let tree = syntax::parse(source);
        let doc = tree.document(Utf8Encoding::new());
        let values = doc
            .block(doc.require(doc.root(), b"values").unwrap())
            .unwrap();
        let bad = values.values().last().unwrap();
        doc.deserialize_value::<u32>(bad).unwrap_err().path()
    };

    // An interpolation is an entry.
    let source = b"values = { [ROOT.GetName] bad }";
    assert_eq!(
        syntax::parse(source)
            .deserialize::<Tuple, _>(Utf8Encoding::new())
            .unwrap_err()
            .path(),
        "values[1]"
    );
    assert_eq!(partial(source), "values[1]");

    // A block with only a reader directive is an empty block, which is an
    // element.
    let source = b"values = { { @:insert name } bad }";
    assert_eq!(
        syntax::parse(source)
            .deserialize::<List, _>(Utf8Encoding::new())
            .unwrap_err()
            .path(),
        "values[0]"
    );
    assert_eq!(partial(source), "values[1]");
}

#[test]
fn code_text_keeps_its_escapes() {
    let source = br#"code [[ name = "a\"b" ]]"#;
    let tree = syntax::parse(source);
    let owned = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    let code = owned.values().next().unwrap();
    assert_eq!(code.as_str(), Some(r#"code [[ name = "a\"b" ]]"#));

    #[derive(Deserialize, Debug, PartialEq)]
    struct File {
        x: String,
    }
    let file: File = syntax::parse(br#"x = code [[ "a\"b" ]]"#)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.x, r#"code [[ "a\"b" ]]"#);

    // The space before a backslash stays, and the encoding converts the
    // other bytes.
    let tree = syntax::parse(b"code [[ a \\\"b\xe9 ]]");
    let owned = OwnedBlock::from_tree(tree.tree(), Windows1252Encoding::new());
    let code = owned.values().next().unwrap();
    assert_eq!(code.as_str(), Some("code [[ a \\\"b\u{e9} ]]"));

    // A quoted token loses its escapes, as before.
    let file: File = syntax::parse(br#"x = "a\"b""#)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(file.x, r#"a"b"#);
}

#[test]
fn enum_errors_have_the_range_of_the_failing_token() {
    #[derive(Deserialize, Debug)]
    enum Entry {
        #[serde(rename = "amount")]
        Amount(#[allow(dead_code)] u32),
    }

    #[derive(Deserialize, Debug)]
    struct File {
        #[allow(dead_code)]
        entry: Entry,
    }

    let error = syntax::parse(b"entry = {\n  amount\n  bad\n}")
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(error.line_col().unwrap().to_string(), "3:3");

    let error = syntax::parse(b"entry = {\n  other\n  1\n}")
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "2:3: entry: unknown variant `other`, expected `amount`"
    );
}

#[test]
fn header_keeps_its_range_in_a_spanned() {
    #[derive(Deserialize, Debug)]
    struct File {
        color: Headered<[u8; 3], Spanned<String>>,
    }

    let file: File = syntax::parse(b"color = rgb { 1 2 3 }")
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let (header, value) = file.color.into_parts();
    let header = header.unwrap();
    assert_eq!(
        header.range(),
        Some(syntax::TextRange::new(8.into(), 11.into()))
    );
    assert_eq!(header.value(), "rgb");
    assert_eq!(value, [1, 2, 3]);

    let tree = syntax::parse(b"color = rgb { 1 2 3 }");
    let doc = tree.document(Utf8Encoding::new());
    let value = doc.require(doc.root(), b"color").unwrap();
    let color: Headered<[u8; 3], Spanned<String>> = doc.deserialize_value(value).unwrap();
    assert_eq!(
        color.header().unwrap().range(),
        Some(syntax::TextRange::new(8.into(), 11.into()))
    );
}

#[test]
fn partial_read_index_in_an_array_with_a_later_field() {
    #[derive(Deserialize, Debug)]
    struct Row {
        #[allow(dead_code)]
        id: u32,
    }

    #[derive(Deserialize, Debug)]
    struct File {
        #[allow(dead_code)]
        rows: Vec<Row>,
    }

    // The first entry is a bare value, so the block is an array, and the
    // later field does not change that.
    let source = b"rows = { { id = bad } tail = 1 }";
    assert_eq!(
        syntax::parse(source)
            .deserialize::<File, _>(Utf8Encoding::new())
            .unwrap_err()
            .path(),
        "rows[0].id"
    );

    let tree = syntax::parse(source);
    let doc = tree.document(Utf8Encoding::new());
    let rows = doc
        .block(doc.require(doc.root(), b"rows").unwrap())
        .unwrap();
    let row = rows.values().next().unwrap();
    let error = doc.deserialize_value::<Row>(row).unwrap_err();
    assert_eq!(error.path(), "rows[0].id");
}

#[test]
fn owned_value_keeps_a_variable_header() {
    #[derive(Deserialize, Debug)]
    struct File {
        color: Headered<Vec<u8>, OwnedValue>,
    }

    let source = b"color = @space { 1 2 3 }";
    let direct: File = syntax::parse(source)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(
        direct.color.header(),
        Some(&OwnedValue::Scalar(OwnedScalar::Variable("@space".into())))
    );

    let cached: OwnedBlock = syntax::parse(source)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    let header = cached.get("color").unwrap().header().unwrap();
    assert_eq!(header, &OwnedScalar::Variable("@space".into()));
    let later = File::deserialize(&cached).unwrap();
    assert_eq!(
        later.color.header(),
        Some(&OwnedValue::Scalar(OwnedScalar::Variable("@space".into())))
    );
    assert_eq!(later.color.value(), &[1, 2, 3]);

    // The tree builder keeps it too.
    let tree = syntax::parse(source);
    let owned = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    assert_eq!(owned, cached);

    // With no variables in the flavor, the header is text.
    let parse = syntax::parse_with(source, &NoVariables);
    let direct: File = parse.deserialize(Utf8Encoding::new()).unwrap();
    assert_eq!(direct.color.header(), Some(&OwnedValue::unquoted("@space")));
    let cached: OwnedBlock = parse.deserialize(Utf8Encoding::new()).unwrap();
    let header = cached.get("color").unwrap().header().unwrap();
    assert_eq!(header, &OwnedScalar::Unquoted("@space".into()));
    let later = File::deserialize(cached).unwrap();
    assert_eq!(later.color.header(), Some(&OwnedValue::unquoted("@space")));
}

/// A quoted header is a quoted scalar in the owned value of each engine:
/// the tree, the tape of `parse_tape`, and `TextDeserializer`. It is
/// unescaped.
#[test]
fn owned_value_keeps_a_quoted_header() {
    #[derive(Deserialize, Debug)]
    struct File {
        block: OwnedValue,
        other: Headered<Vec<u8>, OwnedValue>,
    }

    let source = b"block = \"tab_glow\" { visible = no } other = \"a\\\"b\" { 1 }";
    let glow = OwnedScalar::Quoted("tab_glow".into());
    let escaped = OwnedValue::Scalar(OwnedScalar::Quoted("a\"b".into()));

    let tree = syntax::parse(source);
    let owned = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    let (header, body) = owned.get("block").unwrap().as_headered_block().unwrap();
    assert_eq!(header, &glow);
    assert_eq!(body.get("visible"), Some(&OwnedValue::unquoted("no")));
    assert_eq!(
        owned.get("other").unwrap().header(),
        Some(&OwnedScalar::Quoted("a\"b".into()))
    );

    let tape = syntax::parse_tape(source).into_result().unwrap();
    let from_tape: OwnedBlock = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(from_tape, owned);

    let direct: File = tree.deserialize(Utf8Encoding::new()).unwrap();
    let streaming: File = TextDeserializer::from_slice(source, Utf8Encoding::new())
        .deserialize()
        .unwrap();
    for file in [direct, streaming] {
        assert_eq!(file.block.header(), Some(&glow));
        assert_eq!(file.other.header(), Some(&escaped));
        assert_eq!(file.other.value(), &[1]);
    }
}

/// A tape classifies `@` and a byte that is not a letter as a variable, as
/// the lexer of the tree does, in a key and in a header.
#[test]
fn tape_owned_values_classify_variables_as_the_tree() {
    let source = b"@-x = 1 a = @.x { 1 }";
    let tree = syntax::parse(source);
    let expected = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    let OwnedItem::Field(field) = &expected.items()[0] else {
        panic!("expected a field");
    };
    assert_eq!(field.key, OwnedScalar::Variable("@-x".into()));
    let header = expected.get("a").unwrap().header().unwrap();
    assert_eq!(header, &OwnedScalar::Variable("@.x".into()));

    let tape = syntax::parse_tape(source).into_result().unwrap();
    let from_tape: OwnedBlock = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(from_tape, expected);

    let tape = TextTape::from_slice(source).unwrap();
    let from_legacy: OwnedBlock = TextTapeDeserializer::from_utf8_tape(&tape)
        .deserialize()
        .unwrap();
    assert_eq!(from_legacy, expected);
}

#[test]
fn seeds_read_a_field_with_its_operator_and_borrow_the_source() {
    use serde::de::DeserializeSeed;

    /// Collect each borrowed text into a vector of the caller.
    struct Push<'v, 's>(&'v mut Vec<&'s str>);

    impl<'de: 's, 's> DeserializeSeed<'de> for Push<'_, 's> {
        type Value = ();

        fn deserialize<D: serde::Deserializer<'de>>(self, d: D) -> Result<(), D::Error> {
            self.0.push(<&str>::deserialize(d)?);
            Ok(())
        }
    }

    let source = b"name = FRA other = BUR age >= 16";
    let tree = syntax::parse(source);
    let doc = tree.document(Utf8Encoding::new());
    let mut names = Vec::new();
    for key in [&b"name"[..], b"other"] {
        let field = doc.root().get(key).unwrap();
        doc.deserialize_field_seed(field, Push(&mut names)).unwrap();
    }
    assert_eq!(names, ["FRA", "BUR"]);

    let age = doc.root().get(b"age").unwrap();
    let seed = std::marker::PhantomData::<Property<u32>>;
    let age = doc.deserialize_field_seed(age, seed).unwrap();
    assert_eq!(age, Property::new(Operator::GreaterThanEqual, 16));
}

#[test]
fn parameter_body_keeps_the_items_after_an_inner_bracket() {
    for source in [&b"[[p] [[a b]] x = 1 ]"[..], b"[[!p] [[a b]] x = 1 ]"] {
        let name = String::from_utf8_lossy(source);
        let from_serde: OwnedBlock = syntax::parse(source)
            .deserialize(Utf8Encoding::new())
            .unwrap();
        let tree = syntax::parse(source);
        let from_tree = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
        assert_eq!(from_serde, from_tree, "{name}");

        let OwnedItem::Parameter(parameter) = &from_tree.items()[0] else {
            panic!("{name}: expected a parameter");
        };
        assert_eq!(
            parameter.body.get("x"),
            Some(&OwnedValue::unquoted("1")),
            "{name}"
        );
    }
}

#[test]
fn owned_block_reads_the_root_as_the_tree_does() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct Areas {
        brittany: Vec<u32>,
        b: u32,
    }

    // The first field of the root has no operator. In a block that is not
    // the root, such a field makes the block an array.
    let source = b"brittany { 169 170 } b = 3";
    let expected = Areas {
        brittany: vec![169, 170],
        b: 3,
    };
    let tree = syntax::parse(source);
    let direct: Areas = tree.document(Utf8Encoding::new()).deserialize().unwrap();
    assert_eq!(direct, expected);

    let root = OwnedBlock::from_tree(tree.tree(), Utf8Encoding::new());
    assert_eq!(Areas::deserialize(&root).unwrap(), expected);
    assert_eq!(Areas::deserialize(root.clone()).unwrap(), expected);
    let from_serde: OwnedBlock = syntax::parse(source)
        .deserialize(Utf8Encoding::new())
        .unwrap();
    assert_eq!(from_serde, root);

    let map: HashMap<String, OwnedValue> = HashMap::deserialize(&root).unwrap();
    let mut keys: Vec<&str> = map.keys().map(String::as_str).collect();
    keys.sort_unstable();
    assert_eq!(keys, ["b", "brittany"]);

    // A value reads the same block as a block in a document.
    let value = OwnedValue::from(root);
    assert_eq!(
        Areas::deserialize(&value).unwrap_err().to_string(),
        "missing field `brittany`"
    );

    // A scalar is not a block.
    let error = OwnedBlock::deserialize(&OwnedValue::unquoted("x")).unwrap_err();
    assert_eq!(error.to_string(), "expected a block");
}

#[test]
fn wrappers_do_not_read_ordinary_data_as_protocol_data() {
    use serde::de::value::Error as ValueError;

    // A sequence of numbers is not a range and a value.
    let numbers = IntoDeserializer::<ValueError>::into_deserializer(vec![1u32, 2, 3]);
    let error = Spanned::<u32>::deserialize(numbers).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid type: sequence, expected a value"
    );

    // A map must have the private keys.
    let map = HashMap::from([("start", 1u32)]);
    let map = IntoDeserializer::<ValueError>::into_deserializer(map);
    let error = Spanned::<u32>::deserialize(map).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid value: string \"start\", expected the private key `$__jomini_private_range`"
    );
}

#[test]
fn wrappers_fail_when_the_deserializer_does_not_know_them() {
    use serde::de::value::Error as ValueError;

    let quotes = "MaybeQuoted needs a jomini text deserializer; serde(flatten), \
        serde(untagged), and other deserializers do not keep the quotes";
    let header = "Headered needs a jomini text deserializer; serde(flatten), \
        serde(untagged), and other deserializers do not keep the header";

    // serde buffers the entries of a flattened map in its own model, which
    // has no quotes and no headers.
    #[derive(Deserialize, Debug)]
    struct Flat {
        #[serde(flatten)]
        #[allow(dead_code)]
        rest: HashMap<String, MaybeQuoted<String>>,
    }
    let source = b"a = \"@text\"";
    let streaming = jomini::text::de::from_utf8_slice::<Flat>(source).unwrap_err();
    assert_eq!(streaming.to_string(), format!("{quotes} (offset: 11)"));
    let tree = syntax::parse(source)
        .deserialize::<Flat, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(tree.to_string(), format!("1:1: {quotes}"));

    #[derive(Deserialize, Debug)]
    #[serde(untagged)]
    #[allow(dead_code)]
    enum Color {
        Headered(Headered<Vec<u8>>),
    }
    let error = syntax::parse(b"c = rgb { 1 2 3 }")
        .deserialize::<HashMap<String, Color>, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "1:5: c: data did not match any variant of untagged enum Color"
    );

    // A different deserializer, such as a serde value deserializer.
    let text = IntoDeserializer::<ValueError>::into_deserializer("@text");
    let error = MaybeQuoted::<String>::deserialize(text).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid type: string \"@text\", expected a value from a jomini text deserializer"
    );
    let unit = IntoDeserializer::<ValueError>::into_deserializer(());
    let error = Headered::<()>::deserialize(unit).unwrap_err();
    assert_eq!(
        error.to_string(),
        "invalid type: unit value, expected a value from a jomini text deserializer"
    );
    // The flattened header has the same error.
    #[derive(Deserialize, Debug)]
    struct FlatColor {
        #[serde(flatten)]
        #[allow(dead_code)]
        rest: HashMap<String, Headered<Vec<u8>>>,
    }
    let error = syntax::parse(b"c = rgb { 1 2 3 }")
        .deserialize::<FlatColor, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(error.to_string(), format!("1:1: {header}"));
}

#[test]
fn wrappers_give_their_value() {
    let mut quoted = MaybeQuoted::Quoted(1);
    *quoted.value_mut() += 1;
    assert_eq!(quoted, MaybeQuoted::Quoted(2));
    assert_eq!(quoted.into_value(), 2);

    let mut headered: Headered<u32> = Headered::new(Some(String::from("rgb")), 1);
    *headered.value_mut() += 1;
    assert_eq!(headered.clone().into_value(), 2);
    assert_eq!(headered.into_parts(), (Some(String::from("rgb")), 2));

    let mut property = Property::new(Operator::LessThan, 1);
    *property.value_mut() += 1;
    assert_eq!(property, Property::new(Operator::LessThan, 2));

    // A `Spanned` compares as its value.
    let range = syntax::TextRange::new(0.into(), 1.into());
    let mut spanned = Spanned::new(Some(range), 1);
    *spanned.value_mut() += 1;
    assert_eq!(spanned, Spanned::new(None, 2));
    assert_eq!(spanned.range(), Some(range));
    assert!(Spanned::new(Some(range), 1) < Spanned::new(None, 2));
}

#[test]
fn bytes_of_an_owned_value_are_decoded_text() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct File<'a> {
        #[serde(borrow)]
        name: &'a [u8],
    }

    #[derive(Deserialize, Debug, PartialEq)]
    struct OwnedFile {
        name: serde_bytes_like::ByteBuf,
    }

    // A Windows-1252 `é` and an escaped quote.
    let source = b"name = \"Caf\xe9 \\\"x\\\"\"";
    let tree: File = syntax::parse(source)
        .deserialize(Windows1252Encoding::new())
        .unwrap();
    assert_eq!(tree.name, b"Caf\xe9 \\\"x\\\"");
    let tape: File = jomini::text::de::from_windows1252_slice(source).unwrap();
    assert_eq!(tape.name, b"Caf\xe9 \\\"x\\\"");

    let root: OwnedBlock = syntax::parse(source)
        .deserialize(Windows1252Encoding::new())
        .unwrap();
    let owned = File::deserialize(&root).unwrap();
    assert_eq!(owned.name, "Café \"x\"".as_bytes());
    let owned = OwnedFile::deserialize(root).unwrap();
    assert_eq!(owned.name.0, "Café \"x\"".as_bytes());
}

/// A byte buffer target, as `serde_bytes::ByteBuf` is.
mod serde_bytes_like {
    use serde::de::{Deserialize, Deserializer, Error, Visitor};
    use std::fmt;

    #[derive(Debug, PartialEq)]
    pub struct ByteBuf(pub Vec<u8>);

    impl<'de> Deserialize<'de> for ByteBuf {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            struct BytesVisitor;

            impl Visitor<'_> for BytesVisitor {
                type Value = ByteBuf;

                fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str("bytes")
                }

                fn visit_bytes<E: Error>(self, v: &[u8]) -> Result<ByteBuf, E> {
                    Ok(ByteBuf(v.to_vec()))
                }
            }

            d.deserialize_byte_buf(BytesVisitor)
        }
    }
}

#[test]
fn from_tree_reads_a_recovered_tree() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct File {
        a: Option<u32>,
        b: u32,
    }

    // The field `a` has no value.
    let parse = syntax::parse(b"a = \nb = 2");
    let file: File = parse.document(Utf8Encoding::new()).deserialize().unwrap();
    assert_eq!(file, File { a: None, b: 2 });

    let doc = parse.document(Utf8Encoding::new());
    assert_eq!(
        doc.check().unwrap_err().to_string(),
        "1:3: operator has no value"
    );
    assert_eq!(
        syntax::parse(b"a = \nb = 2")
            .deserialize::<File, _>(Utf8Encoding::new())
            .unwrap_err()
            .to_string(),
        "1:3: operator has no value"
    );
}

/// An `OwnedParse` deserializes as a `Parse` does, and `Document::new` reads
/// a `SyntaxTree` without its parse.
#[test]
fn owned_parse_and_tree_documents() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct File<'a> {
        name: &'a str,
        size: u32,
    }

    let owned = syntax::parse_owned(b"name = Paris\nsize = 3".to_vec());
    let file: File = owned.deserialize(Utf8Encoding::new()).unwrap();
    assert_eq!(
        file,
        File {
            name: "Paris",
            size: 3
        }
    );

    let tree = owned.tree();
    let doc = Document::new(tree, Utf8Encoding::new()).with_file("a.txt");
    let file: File = doc.deserialize().unwrap();
    assert_eq!(file.size, 3);
    let size = doc.require(doc.root(), b"size").unwrap();
    assert_eq!(doc.str(size).unwrap(), "3");

    let open = syntax::parse_owned(b"name = Paris\nsize = { 3".to_vec());
    let error = open
        .deserialize::<File, _>(Utf8Encoding::new())
        .unwrap_err();
    assert_eq!(error.to_string(), "2:11: unclosed '{'");
    let error = open
        .document(Utf8Encoding::new())
        .deserialize::<File>()
        .unwrap_err();
    assert_eq!(
        error.to_string(),
        "2:8: size: invalid type: sequence, expected u32"
    );
}
