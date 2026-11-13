#![cfg(all(feature = "lossless", feature = "derive"))]

//! Tests for the resolution of variables and calculations, and for the
//! deserializer that uses a resolution.
//!
//! The first tests come from the coat of arms preprocessor of pdx-tools,
//! which this resolver replaces.

use jomini::{
    Utf8Encoding,
    text::{
        MaybeQuoted,
        lossless::{
            AstNode, Calc, Flavor, OwnedBlock, OwnedValue, SyntaxTree, Value,
            de::{Document, Error},
            parse, parse_with,
            resolve::{
                Resolution, ResolveErrorKind, ResolvedScalar, Variables, evaluate, evaluate_with,
                resolve, resolve_with,
            },
        },
    },
};
use serde::{Deserialize, de::DeserializeOwned};
use std::borrow::Cow;
use std::collections::HashMap;

fn resolved<T: DeserializeOwned>(
    source: &[u8],
    scope: &Variables,
) -> (Result<T, Error>, Resolution) {
    let parse = parse(source);
    assert!(parse.errors().is_empty(), "{:?}", parse.errors());
    let resolution = resolve_with(parse.tree(), scope);
    let result = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution.clone())
        .and_then(|de| de.deserialize());
    (result, resolution)
}

fn kinds(source: &[u8]) -> Vec<ResolveErrorKind> {
    let parse = parse(source);
    let resolution = resolve(parse.tree());
    resolution.errors().iter().map(|e| e.kind).collect()
}

#[derive(Deserialize, Debug, PartialEq)]
struct Emblem {
    #[serde(default)]
    scale: Vec<f64>,
    #[serde(default)]
    position: Vec<f64>,
}

#[test]
fn variables_and_calculations_in_arrays() {
    let source = b"@half = @[1/2]\n@semy = 0.27\nX = { scale = { @half @semy } }";
    let (emblems, resolution) = resolved::<HashMap<String, Emblem>>(source, &Variables::new());
    assert!(resolution.errors().is_empty());

    // The definitions are not entries of the map.
    let emblems = emblems.unwrap();
    assert_eq!(emblems.len(), 1);
    assert_eq!(emblems["X"].scale, [0.5, 0.27]);
}

#[test]
fn nested_calculation_with_variables() {
    let source = b"@o = 0.1\nX = { position = { @[0.5 + (o*2)] 0.5 } }";
    let (emblems, _) = resolved::<HashMap<String, Emblem>>(source, &Variables::new());
    assert_eq!(emblems.unwrap()["X"].position, [0.7, 0.5]);
}

#[test]
fn unsupported_and_not_finite_calculations() {
    use ResolveErrorKind::*;
    assert_eq!(kinds(b"x = @[min(1, 2)]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[1 / 0]"), [NotFinite]);
    assert_eq!(kinds(b"x = @[1 2]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[1 +]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[(1 2)]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[* * 2]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"x = @[-]"), [UnsupportedCalculation]);
    assert_eq!(kinds(b"@x = nope\ny = @[x + 1]"), [NotNumeric]);
    assert_eq!(NotNumeric.message(), "variable is not a number");
    assert_eq!(kinds(b"@x = { 1 }"), [UnsupportedDefinition]);
}

#[test]
fn calculation_with_an_unclosed_group_has_a_value() {
    // The parser reports the missing `)`, so the resolver does not.
    let parse = parse(b"x = @[(1 + 2]");
    assert_eq!(parse.errors().len(), 1);
    let resolution = resolve(parse.tree());
    assert!(resolution.errors().is_empty());
    let calc = parse.ast().get(b"x").unwrap().value().unwrap();
    assert_eq!(resolution.get(calc), Some(&ResolvedScalar::unquoted("3")));
}

#[test]
fn text_variable() {
    // A variable can hold text. A number target then gives a located error.
    let source = b"@x = nope\ny = @x";
    let (file, resolution) = resolved::<HashMap<String, f64>>(source, &Variables::new());
    assert!(resolution.errors().is_empty());
    let error = file.unwrap_err();
    assert_eq!(error.path(), "y");
    assert_eq!(error.line_col().unwrap().to_string(), "2:5");

    let source = b"@x = \"a b\"\nx = @x";
    let (file, _) = resolved::<HashMap<String, String>>(source, &Variables::new());
    assert_eq!(file.unwrap()["x"], "a b");
}

#[test]
fn a_quoted_value_stays_quoted() {
    #[derive(Deserialize)]
    struct File {
        n: MaybeQuoted<String>,
        m: MaybeQuoted<String>,
    }

    let source = b"@x = \"a b\"\n@y = 2\nn = @x\nm = @y";
    let (file, resolution) = resolved::<File>(source, &Variables::new());
    let file = file.unwrap();
    assert_eq!(file.n, MaybeQuoted::Quoted(String::from("a b")));
    assert_eq!(file.m, MaybeQuoted::Unquoted(String::from("2")));
    assert_eq!(
        resolution.variables().get("x"),
        Some(&ResolvedScalar::quoted("a b"))
    );

    // A linter can tell `2` from `"2"`.
    let parse = parse(b"@a = 2\n@b = \"2\"\nx = @a\ny = @b");
    let resolution = resolve(parse.tree());
    let value = |key: &[u8]| resolution.get(parse.ast().get(key).unwrap().value().unwrap());
    assert_eq!(value(b"x"), Some(&ResolvedScalar::unquoted("2")));
    assert_eq!(value(b"y"), Some(&ResolvedScalar::quoted("2")));

    // An owned value keeps the quotes, so a writer can give the text back.
    let source = br#"@x = "a \"b\" c"
y = @x"#;
    let (root, _) = resolved::<OwnedBlock>(source, &Variables::new());
    assert_eq!(
        root.unwrap().get("y"),
        Some(&OwnedValue::quoted(r#"a "b" c"#))
    );
}

#[test]
fn integer_targets() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyObj {
        pos_x: i64,
        pos_y: i64,
        neg: i64,
    }

    let source =
        b"@my_var = 10\nmy_obj = { pos_x = @[100-my_var] pos_y = @my_var neg = @[0 * -1] }";
    let (file, _) = resolved::<HashMap<String, MyObj>>(source, &Variables::new());
    let expected = MyObj {
        pos_x: 90,
        pos_y: 10,
        neg: 0,
    };
    assert_eq!(file.unwrap()["my_obj"], expected);
}

#[test]
fn undefined_variable_keeps_its_text() {
    let source = b"a = @missing\nb = @[missing * 2]";
    let (file, resolution) = resolved::<HashMap<String, String>>(source, &Variables::new());
    let file = file.unwrap();
    assert_eq!(file["a"], "@missing");
    assert_eq!(file["b"], "@[missing * 2]");

    let errors = resolution.errors();
    assert_eq!(errors.len(), 2);
    assert_eq!(errors[0].kind, ResolveErrorKind::UndefinedVariable);
    assert_eq!(&source[errors[0].range.to_range()], b"@missing");
    assert_eq!(&source[errors[1].range.to_range()], b"missing");
}

#[test]
fn failed_definition_has_one_error() {
    let source = b"@a = @[1 / 0]\nx = @a\ny = @[a + 1]";
    assert_eq!(kinds(source), [ResolveErrorKind::NotFinite]);

    // A second definition is an error, and the failed one stays.
    assert_eq!(
        kinds(b"@a = @[1 / 0]\n@a = 1\nx = @a"),
        [ResolveErrorKind::NotFinite, ResolveErrorKind::Redefined]
    );
}

#[test]
fn source_order_and_scope() {
    let mut scope = Variables::new();
    scope.insert("size", "1");
    scope.insert("other", "3");

    // A variable that the file does not define comes from the scope. A
    // definition of a name that the scope has is an error, and the value
    // of the scope stays, as for a CK3 `reader_export` variable.
    let source = b"a = @size\n@size = 2\nb = @size\nc = @[size + other]";
    let (file, resolution) = resolved::<HashMap<String, f64>>(source, &scope);
    let file = file.unwrap();
    assert_eq!((file["a"], file["b"], file["c"]), (1.0, 1.0, 4.0));
    assert_eq!(resolution.variables().get("size"), None);
    assert_eq!(resolution.variables().get("other"), None);
    let errors = resolution.errors();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ResolveErrorKind::DefinedInScope);
    assert_eq!(&source[errors[0].range.to_range()], b"@size");

    // A reference before the definition, and no scope.
    assert_eq!(
        kinds(b"a = @size\n@size = 2"),
        [ResolveErrorKind::UndefinedVariable]
    );
}

#[test]
fn value_and_wrappers_see_the_values() {
    #[derive(Deserialize)]
    struct File {
        a: MaybeQuoted<String>,
        b: MaybeQuoted<String>,
    }

    let source = b"@x = 0.5\na = @x\nb = @[x * 2]";
    let (root, _) = resolved::<OwnedBlock>(source, &Variables::new());
    let root = root.unwrap();
    assert_eq!(root.len(), 2);
    assert_eq!(root.get("a"), Some(&OwnedValue::unquoted("0.5")));
    assert_eq!(root.get("b"), Some(&OwnedValue::unquoted("1")));

    let (file, _) = resolved::<File>(source, &Variables::new());
    let file = file.unwrap();
    assert_eq!(file.a, MaybeQuoted::Unquoted(String::from("0.5")));
    assert_eq!(file.b, MaybeQuoted::Unquoted(String::from("1")));
}

#[test]
fn deserializer_without_resolution_keeps_the_source() {
    let source = b"@x = 0.5\na = @x\nb = @[x * 2]";
    let parse = parse(source);
    let file: HashMap<String, String> = Document::new(parse.tree(), Utf8Encoding::new())
        .deserialize()
        .unwrap();
    assert_eq!(file["@x"], "0.5");
    assert_eq!(file["a"], "@x");
    assert_eq!(file["b"], "@[x * 2]");
}

#[test]
fn code_payload_is_not_resolved() {
    // From EU5 `gfx/map/city_data/templates.txt`: `@mesh_list` is a
    // parameter of the template.
    let source = b"template default_bridge {\n\tdefault_variables = {}\n\tcode [[\n\t\tmesh_list = @mesh_list\n\t]]\n}";
    assert!(kinds(source).is_empty());
}

#[test]
fn parameter_block_is_not_resolved() {
    // No parameter is set, so the definition in the block does not apply.
    let source = b"[[p] @x = 2 ]\nb = @x";
    assert_eq!(kinds(source), [ResolveErrorKind::UndefinedVariable]);
    assert!(kinds(b"[[p] a = @x ]").is_empty());
}

#[test]
fn evaluate_one_calculation() {
    let parse = parse(b"x = @[-(2 + 1) * 4 / 8]");
    let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    assert_eq!(evaluate(calc), Ok(-1.5));
}

#[test]
fn resolution_of_a_different_tree() {
    let first = parse(b"@x = 1\na = @x");
    let resolution = resolve(first.tree());

    // A tree of a different size, and a tree of the same size, as after an
    // edit that changes one digit.
    for source in [&b"a = 1"[..], b"@x = 2\na = @x"] {
        let other = parse(source);
        let error = Document::new(other.tree(), Utf8Encoding::new())
            .with_file("e.txt")
            .with_resolution(resolution.clone())
            .err()
            .unwrap();
        assert_eq!(
            error.to_string(),
            "e.txt: the resolution is of a different tree"
        );
        assert!(!resolution.is_for(other.tree()));
    }
    assert!(resolution.is_for(first.tree()));

    // A value of a different tree has no value.
    let other = parse(b"@x = 2\na = @x");
    let a = other.ast().get(b"a").unwrap().value().unwrap();
    assert_eq!(resolution.get(a), None);
    let a = first.ast().get(b"a").unwrap().value().unwrap();
    assert_eq!(resolution.get(a), Some(&ResolvedScalar::unquoted("1")));
}

#[test]
fn calculation_of_a_definition_has_a_value() {
    let parse = parse(b"@half = @[1 / 2]");
    let resolution = resolve(parse.tree());
    let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    let value = Value::Calc(calc);
    assert_eq!(
        resolution.get(value),
        Some(&ResolvedScalar::unquoted("0.5"))
    );
}

#[test]
fn second_definition_keeps_the_first() {
    let source = b"@a = 1\n@a = 2\nx = @a";
    let (file, resolution) = resolved::<HashMap<String, f64>>(source, &Variables::new());
    assert_eq!(file.unwrap()["x"], 1.0);
    let errors = resolution.errors();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].kind, ResolveErrorKind::Redefined);
    assert_eq!(&source[errors[0].range.to_range()], b"@a");
}

/// A definition in a block applies after the block, as in the GUI files of
/// Vic3 and Imperator.
#[test]
fn definition_in_a_block_applies_after_the_block() {
    let source = b"a = { @w = 4 }\nb = @w";
    let (file, resolution) = resolved::<HashMap<String, OwnedValue>>(source, &Variables::new());
    assert!(resolution.errors().is_empty());
    assert_eq!(file.unwrap()["b"], OwnedValue::unquoted("4"));
}

/// The CK3 reader directives that define and read variables.
#[test]
fn reader_directive_variables() {
    let source = b"@:register_variable size = 0.5\n@:register_variable double = @[size * 2]\n\
        x = @:load_variable double\ny = @size\n@@size = key";
    let parse = parse(source);
    assert!(parse.errors().is_empty(), "{:?}", parse.errors());
    let resolution = resolve(parse.tree());
    assert!(resolution.errors().is_empty(), "{:?}", resolution.errors());
    assert_eq!(
        resolution.variables().get("double"),
        Some(&ResolvedScalar::unquoted("1"))
    );

    let file: HashMap<String, String> = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .and_then(|de| de.deserialize())
        .unwrap();
    assert_eq!(file["x"], "1");
    assert_eq!(file["y"], "0.5");
    // `@@size` puts the value of the variable in place of the key.
    assert_eq!(file["0.5"], "key");
}

/// The tokens of `@:define` and the arguments of `@:insert` apply where the
/// block is inserted, so they have no errors here. A directive is not an
/// undefined variable.
#[test]
fn reader_directive_blocks_are_not_examined() {
    assert_eq!(
        kinds(
            b"@:define m = { x = @undefined }\n@:insert m = { y = @other }\n@:log \"t\"\n@:assert"
        ),
        []
    );
    assert_eq!(
        kinds(b"x = @:load_variable missing"),
        [ResolveErrorKind::UndefinedVariable]
    );
    assert_eq!(
        kinds(b"@:register_variable a = 1\n@a = 2"),
        [ResolveErrorKind::Redefined]
    );
}

#[test]
fn references_share_one_value() {
    let parse = parse(b"@long = \"a long text\"\na = @long\nb = @long");
    let mut scope = Variables::new();
    scope.insert("shared", "from the scope");
    let resolution = resolve_with(parse.tree(), &scope);
    let value = |key: &[u8]| {
        let value = parse.ast().get(key).unwrap().value().unwrap();
        resolution.get(value).unwrap()
    };
    assert_eq!(value(b"a"), &ResolvedScalar::quoted("a long text"));
    assert_eq!(
        value(b"a").as_bytes().as_ptr(),
        value(b"b").as_bytes().as_ptr()
    );
    assert_eq!(
        value(b"a").as_bytes().as_ptr(),
        resolution
            .variables()
            .get("long")
            .unwrap()
            .as_bytes()
            .as_ptr()
    );

    let parse = jomini::text::lossless::parse(b"a = @shared\nb = @shared");
    let resolution = resolve_with(parse.tree(), &scope);
    let value = |key: &[u8]| {
        let value = parse.ast().get(key).unwrap().value().unwrap();
        resolution.get(value).unwrap()
    };
    assert_eq!(value(b"a"), &ResolvedScalar::unquoted("from the scope"));
    assert_eq!(
        value(b"a").as_bytes().as_ptr(),
        value(b"b").as_bytes().as_ptr()
    );
}

#[test]
fn a_very_small_calculation_deserializes_as_a_float() {
    let (file, resolution) = resolved::<HashMap<String, f64>>(
        b"x = @[1 / 1000000000000 / 1000000000000]",
        &Variables::new(),
    );
    assert_eq!(resolution.errors(), []);
    assert_eq!(file.unwrap()["x"], 1e-24);

    // Text that is not a calculation keeps the rules of TextDeserializer.
    let (file, _) = resolved::<HashMap<String, f64>>(b"@x = inf\ny = @x", &Variables::new());
    assert!(file.is_err());
}

#[test]
fn a_number_can_end_with_f() {
    let (file, resolution) = resolved::<HashMap<String, f64>>(
        b"@x = 10.0f\na = @[10.0f / 2.5]\nb = @[x / 2]",
        &Variables::new(),
    );
    assert_eq!(resolution.errors(), []);
    let file = file.unwrap();
    assert_eq!((file["a"], file["b"]), (4.0, 5.0));
}

#[test]
fn load_variable_gives_a_value_to_a_definition_and_to_get() {
    let source = b"@a = 1\n@b = @:load_variable a\n@:register_variable c = @:load_variable b\nx = @c\ny = @:load_variable a";
    let parse = parse(source);
    let resolution = resolve(parse.tree());
    assert_eq!(resolution.errors(), []);
    assert_eq!(
        resolution.variables().get("b"),
        Some(&ResolvedScalar::unquoted("1"))
    );
    assert_eq!(
        resolution.variables().get("c"),
        Some(&ResolvedScalar::unquoted("1"))
    );

    let y = parse.ast().get(b"y").unwrap().value().unwrap();
    assert_eq!(resolution.get(y), Some(&ResolvedScalar::unquoted("1")));

    let file: HashMap<String, u32> = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .and_then(|de| de.deserialize())
        .unwrap();
    assert_eq!((file["x"], file["y"]), (1, 1));
}

#[test]
fn document_reads_resolved_values() {
    let source =
        b"@size = 0.5\nemblem = { scale = @[size * 2] name = @size rows = { @size @[size + 1] } }";
    let parse = parse(source);
    let doc = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolve(parse.tree()))
        .unwrap();
    let emblem = doc
        .block(doc.require(doc.root(), b"emblem").unwrap())
        .unwrap();
    let scale: f64 = doc
        .deserialize_value(doc.require(emblem, b"scale").unwrap())
        .unwrap();
    assert_eq!(scale, 1.0);
    let name = doc.require(emblem, b"name").unwrap();
    assert_eq!(doc.str(name).unwrap(), "0.5");
    let rows: Vec<f64> = doc.deserialize_field(emblem.get(b"rows").unwrap()).unwrap();
    assert_eq!(rows, [0.5, 1.5]);

    // Without a resolution, the document gives the source text.
    let doc = Document::new(parse.tree(), Utf8Encoding::new());
    let emblem = doc
        .block(doc.require(doc.root(), b"emblem").unwrap())
        .unwrap();
    assert_eq!(
        doc.str(doc.require(emblem, b"name").unwrap()).unwrap(),
        "@size"
    );

    // A resolution of a different tree is an error.
    let other = jomini::text::lossless::parse(b"@size = 0.6");
    let error = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolve(other.tree()))
        .err()
        .unwrap();
    assert_eq!(error.to_string(), "the resolution is of a different tree");
}

#[test]
fn a_calculation_gives_its_exact_number_also_through_a_variable() {
    let (file, resolution) = resolved::<HashMap<String, f64>>(
        b"@tiny = @[1 / 1000000000000 / 1000000000000]\na = @tiny\nb = @[1 / 11]\n@c = @[1 / 11]\nd = @c",
        &Variables::new(),
    );
    assert_eq!(resolution.errors(), []);
    let file = file.unwrap();
    assert_eq!(file["a"], 1e-24);
    assert_eq!(file["b"], 1.0 / 11.0);
    assert_eq!(file["d"], 1.0 / 11.0);
}

#[test]
fn load_variable_is_an_element_of_a_resolved_array() {
    let source = b"@a = 1\nx = { @:load_variable a @a }";
    let parse = parse(source);
    let resolution = resolve(parse.tree());
    assert_eq!(resolution.errors(), []);
    let file: HashMap<String, Vec<u32>> = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .and_then(|de| de.deserialize())
        .unwrap();
    assert_eq!(file["x"], [1, 1]);

    // Without a resolution, a directive is not an entry.
    let file: HashMap<String, Vec<String>> = Document::new(parse.tree(), Utf8Encoding::new())
        .deserialize()
        .unwrap();
    assert_eq!(file["x"], ["@a"]);
}

#[test]
fn an_alias_definition_records_its_reference() {
    let parse = parse(b"@a = 1\n@b = @a");
    let resolution = resolve(parse.tree());
    assert_eq!(
        resolution.variables().get("b"),
        Some(&ResolvedScalar::unquoted("1"))
    );
    let value = parse.ast().get(b"@b").unwrap().value().unwrap();
    assert_eq!(resolution.get(value), Some(&ResolvedScalar::unquoted("1")));

    let doc = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .unwrap();
    assert_eq!(doc.str(value).unwrap(), "1");
    assert_eq!(doc.deserialize_value::<u32>(value).unwrap(), 1);
}

#[test]
fn a_shared_resolution_attaches_with_no_copy() {
    let parse = parse(b"@size = 2\nx = @size");
    let resolution = std::sync::Arc::new(resolve(parse.tree()));
    for _ in 0..2 {
        let doc = Document::new(parse.tree(), Utf8Encoding::new())
            .with_resolution(std::sync::Arc::clone(&resolution))
            .unwrap();
        let x = doc.require(doc.root(), b"x").unwrap();
        assert_eq!(doc.deserialize_value::<u32>(x).unwrap(), 2);
    }
    let file: HashMap<String, u32> = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(std::sync::Arc::clone(&resolution))
        .and_then(|de| de.deserialize())
        .unwrap();
    assert_eq!(file["x"], 2);
    assert_eq!(std::sync::Arc::strong_count(&resolution), 1);
}

#[test]
fn a_calculation_of_a_different_file_keeps_its_number() {
    let definitions = parse(b"@tiny = @[1 / 1000000000000 / 1000000000000]\n@third = @[1 / 3]");
    let mut shared = Variables::new();
    // A copy of the text of each value.
    let definitions = resolve(definitions.tree());
    shared.extend(
        definitions
            .variables()
            .iter()
            .map(|(name, value)| (name, value.as_bytes())),
    );

    let file = parse(b"x = @tiny\ny = @third\nz = @[third * 3]");
    let resolution = resolve_with(file.tree(), &shared);
    assert_eq!(resolution.errors(), []);
    let values: HashMap<String, f64> = Document::new(file.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .and_then(|de| de.deserialize())
        .unwrap();
    assert_eq!(values["x"], 1e-24);
    assert_eq!(values["y"], 1.0 / 3.0);
    assert_eq!(values["z"], 1.0);
}

#[test]
fn a_tree_of_a_different_flavor_is_a_different_tree() {
    let source = b"@x=1 a=@x";
    let scripted = parse(source);
    let plain = parse_with(source, Flavor::default().with_variables(false));

    let resolution = resolve(scripted.tree());
    assert!(!resolution.is_for(plain.tree()));
    let error = Document::new(plain.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .err()
        .unwrap();
    assert_eq!(error.to_string(), "the resolution is of a different tree");

    let resolution = resolve(plain.tree());
    let error = Document::new(scripted.tree(), Utf8Encoding::new())
        .with_resolution(resolution)
        .err()
        .unwrap();
    assert_eq!(error.to_string(), "the resolution is of a different tree");
}

#[test]
fn extend_from_shares_the_values() {
    let first = parse(b"@long = \"a long text\"\n@alias = @long");
    let resolution = resolve(first.tree());
    let mut shared = Variables::new();
    shared.extend_from(resolution.variables());
    let ours = resolution.variables().get("long").unwrap();
    assert_eq!(
        shared.get("long").unwrap().as_bytes().as_ptr(),
        ours.as_bytes().as_ptr()
    );
    assert_eq!(
        shared.get("alias").unwrap().as_bytes().as_ptr(),
        ours.as_bytes().as_ptr()
    );

    // A new value of a name replaces the old value.
    let second = parse(b"@long = 2");
    shared.extend_from(resolve(second.tree()).variables());
    assert_eq!(shared.get("long"), Some(&ResolvedScalar::unquoted("2")));
    assert_eq!(shared.len(), 2);
}

#[test]
fn document_str_borrows_the_source() {
    // The text has the lifetime of the source, not of the document.
    fn text<'t, 'a>(tree: SyntaxTree<'t, 'a>, value: Value<'t, 'a>) -> Cow<'a, str> {
        Document::new(tree, Utf8Encoding::new()).str(value).unwrap()
    }
    let parse = parse(b"@name = FRA\nx = @name\ny = @[1 + 2]\nz = SWE");
    let z = parse.ast().get(b"z").unwrap().value().unwrap();
    assert!(matches!(text(parse.tree(), z), Cow::Borrowed("SWE")));

    // A resolved value is a copy.
    let doc = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolve(parse.tree()))
        .unwrap();
    for (key, expected) in [(&b"x"[..], "FRA"), (b"y", "3")] {
        let value = doc.require(doc.root(), key).unwrap();
        let text = doc.str(value).unwrap();
        assert!(matches!(text, Cow::Owned(_)), "{expected}");
        assert_eq!(text, expected);
    }
    assert!(matches!(doc.str(z).unwrap(), Cow::Borrowed("SWE")));
}

#[test]
fn a_scope_of_variables_shares_its_values() {
    let definitions = parse(b"@a = \"large text\"\n@b = @a");
    let shared = resolve(definitions.tree());

    let file = parse(b"x = @a\ny = @b");
    let resolution = resolve_with(file.tree(), shared.variables());
    let value = |key: &[u8]| {
        resolution
            .get(file.ast().get(key).unwrap().value().unwrap())
            .unwrap()
    };
    let ours = shared.variables().get("a").unwrap();
    assert_eq!(value(b"x"), &ResolvedScalar::quoted("large text"));
    assert_eq!(value(b"x").as_bytes().as_ptr(), ours.as_bytes().as_ptr());
    assert_eq!(value(b"y").as_bytes().as_ptr(), ours.as_bytes().as_ptr());
}

#[test]
fn a_resolved_decimal_that_is_too_large_is_not_a_float() {
    let mut scope = Variables::new();
    scope.insert("huge", "9".repeat(400));
    scope.insert("negative", format!("-{}", "9".repeat(400)));
    let parse = parse(b"x = @huge\ny = @negative");
    let doc = Document::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolve_with(parse.tree(), &scope))
        .unwrap();
    let errors: Vec<String> = [&b"x"[..], b"y"]
        .into_iter()
        .map(|key| {
            let value = doc.require(doc.root(), key).unwrap();
            doc.deserialize_value::<f64>(value).unwrap_err().to_string()
        })
        .collect();
    let nines = "9".repeat(400);
    assert_eq!(
        errors,
        [
            format!("1:5: x: invalid type: string \"{nines}\", expected f64"),
            format!("2:5: y: invalid type: string \"-{nines}\", expected f64"),
        ]
    );
}

#[test]
fn an_operand_that_is_too_large_is_not_finite() {
    let mut scope = Variables::new();
    scope.insert("a", format!("1{}", "0".repeat(308)));
    scope.insert("b", format!("1{}", "0".repeat(309)));

    let parse = parse(b"x = @[a / b]");
    let resolution = resolve_with(parse.tree(), &scope);
    let errors: Vec<_> = resolution
        .errors()
        .iter()
        .map(|e| (e.kind, e.range))
        .collect();
    let b = jomini::text::lossless::TextRange::new(10.into(), 11.into());
    assert_eq!(errors, [(ResolveErrorKind::NotFinite, b)]);

    let literal = format!("x = @[1 / 1{}]", "0".repeat(309));
    let parse = jomini::text::lossless::parse(literal.as_bytes());
    let resolution = resolve(parse.tree());
    let errors: Vec<_> = resolution
        .errors()
        .iter()
        .map(|e| (e.kind, e.range))
        .collect();
    let literal = jomini::text::lossless::TextRange::new(10.into(), 320.into());
    assert_eq!(errors, [(ResolveErrorKind::NotFinite, literal)]);

    let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    let errors: Vec<_> = evaluate(calc)
        .unwrap_err()
        .iter()
        .map(|e| (e.kind, e.range))
        .collect();
    assert_eq!(errors, [(ResolveErrorKind::NotFinite, literal)]);
}

#[test]
fn each_undefined_variable_of_a_calculation_has_an_error() {
    let parse = parse(b"x = @[a + b * (c - 1)]");
    let resolution = resolve(parse.tree());
    let errors: Vec<_> = resolution
        .errors()
        .iter()
        .map(|e| (e.kind, &parse.tree().source()[e.range.to_range()]))
        .collect();
    assert_eq!(
        errors,
        [
            (ResolveErrorKind::UndefinedVariable, &b"a"[..]),
            (ResolveErrorKind::UndefinedVariable, b"b"),
            (ResolveErrorKind::UndefinedVariable, b"c"),
        ]
    );

    // evaluate gives the same errors as the resolution.
    let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    assert_eq!(evaluate(calc).unwrap_err(), resolution.errors());

    // A variable of the scope has a value.
    let scope: Variables = [("b", "2")].into_iter().collect();
    let errors: Vec<_> = evaluate_with(calc, &scope)
        .unwrap_err()
        .iter()
        .map(|e| &parse.tree().source()[e.range.to_range()])
        .collect();
    assert_eq!(errors, [&b"a"[..], b"c"]);
}

#[test]
fn a_resolution_is_for_one_parse_and_its_clones() {
    let source = b"@x = 1\na = @x";
    let first = parse(source);
    let resolution = resolve(first.tree());

    // A clone of the parse has the same tree, so each check agrees.
    let clone = first.clone();
    assert!(resolution.is_for(clone.tree()));
    for tree in [&first, &clone] {
        let a = tree.ast().get(b"a").unwrap().value().unwrap();
        assert_eq!(resolution.get(a), Some(&ResolvedScalar::unquoted("1")));
    }
    let doc = Document::new(clone.tree(), Utf8Encoding::new())
        .with_resolution(resolution.clone())
        .unwrap();
    assert_eq!(doc.deserialize::<HashMap<String, u32>>().unwrap()["a"], 1);

    // A second parse of the same source is a different parse.
    let again = parse(source);
    assert!(!resolution.is_for(again.tree()));
    let a = again.ast().get(b"a").unwrap().value().unwrap();
    assert_eq!(resolution.get(a), None);
    assert!(
        Document::new(again.tree(), Utf8Encoding::new())
            .with_resolution(resolution)
            .is_err()
    );
}

#[test]
fn a_file_resolves_again_without_its_old_variables() {
    let a = parse(b"@x = 1");
    let a_variables = resolve(a.tree()).variables().clone();
    let b = parse(b"@z = 3");
    let mut shared = Variables::new();
    shared.extend_from(&a_variables);
    shared.extend_from(resolve(b.tree()).variables());

    // A set that has the old variables of the file gives the old values.
    let edited = parse(b"@x = 2\ny = @x");
    let stale = resolve_with(edited.tree(), &shared);
    let kinds: Vec<_> = stale.errors().iter().map(|e| e.kind).collect();
    assert_eq!(kinds, [ResolveErrorKind::DefinedInScope]);
    let y = edited.ast().get(b"y").unwrap().value().unwrap();
    assert_eq!(stale.get(y), Some(&ResolvedScalar::unquoted("1")));
    assert!(stale.variables().is_empty());

    // Remove the old variables of the file, and the file gets its own.
    for (name, value) in &a_variables {
        assert_eq!(shared.remove(name).as_ref(), Some(value));
    }
    assert!(!shared.contains("x"));
    assert!(shared.contains("z"));
    let resolution = resolve_with(edited.tree(), &shared);
    assert_eq!(resolution.errors(), []);
    assert_eq!(resolution.get(y), Some(&ResolvedScalar::unquoted("2")));

    // insert gives the old value.
    assert_eq!(shared.insert("z", "4"), Some(ResolvedScalar::unquoted("3")));
    assert_eq!(shared.insert("w", "5"), None);
    let mut names: Vec<_> = shared.iter().map(|(name, _)| name).collect();
    names.sort();
    assert_eq!(names, [&b"w"[..], b"z"]);
    assert_eq!(shared.iter().len(), 2);
}
