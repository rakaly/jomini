#![cfg(all(feature = "lossless", feature = "derive"))]

//! Tests for the resolution of variables and calculations, and for the
//! deserializer that uses a resolution.
//!
//! The first tests come from the coat of arms preprocessor of pdx-tools,
//! which this resolver replaces.

use jomini::{
    Utf8Encoding,
    text::lossless::{
        AstNode, Calc,
        de::{Deserializer, Error, MaybeQuoted},
        parse,
        resolve::{Resolution, ResolveErrorKind, Variables, evaluate, resolve},
        value::Value,
    },
};
use serde::{Deserialize, de::DeserializeOwned};
use std::collections::HashMap;

fn resolved<T: DeserializeOwned>(
    source: &[u8],
    scope: &Variables,
) -> (Result<T, Error>, Resolution) {
    let parse = parse(source);
    assert!(parse.errors().is_empty(), "{:?}", parse.errors());
    let resolution = resolve(parse.tree(), scope);
    let result = Deserializer::new(parse.tree(), Utf8Encoding::new())
        .with_resolution(resolution.clone())
        .deserialize();
    (result, resolution)
}

fn kinds(source: &[u8]) -> Vec<ResolveErrorKind> {
    let parse = parse(source);
    let resolution = resolve(parse.tree(), &());
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
    assert_eq!(kinds(b"@x = nope\ny = @[x + 1]"), [NotANumber]);
    assert_eq!(kinds(b"@x = { 1 }"), [UnsupportedDefinition]);
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

    // A new definition replaces the failed one.
    assert_eq!(
        kinds(b"@a = @[1 / 0]\n@a = 1\nx = @a"),
        [ResolveErrorKind::NotFinite]
    );
}

#[test]
fn source_order_and_scope() {
    let mut scope = Variables::new();
    scope.insert("size", "1");
    scope.insert("other", "3");

    // A reference before the definition comes from the scope. The
    // definition of the file then replaces it.
    let source = b"a = @size\n@size = 2\nb = @size\nc = @[size + other]";
    let (file, resolution) = resolved::<HashMap<String, f64>>(source, &scope);
    let file = file.unwrap();
    assert_eq!((file["a"], file["b"], file["c"]), (1.0, 2.0, 5.0));
    assert_eq!(resolution.variables().get("size"), Some(&b"2"[..]));
    assert_eq!(resolution.variables().get("other"), None);

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
    let (value, _) = resolved::<Value>(source, &Variables::new());
    let value = value.unwrap();
    let root = value.as_block().unwrap();
    assert_eq!(root.len(), 2);
    assert_eq!(root.get("a"), Some(&Value::unquoted("0.5")));
    assert_eq!(root.get("b"), Some(&Value::unquoted("1")));

    let (file, _) = resolved::<File>(source, &Variables::new());
    let file = file.unwrap();
    assert_eq!(file.a, MaybeQuoted::Unquoted(String::from("0.5")));
    assert_eq!(file.b, MaybeQuoted::Unquoted(String::from("1")));
}

#[test]
fn deserializer_without_resolution_keeps_the_source() {
    let source = b"@x = 0.5\na = @x\nb = @[x * 2]";
    let parse = parse(source);
    let file: HashMap<String, String> = Deserializer::new(parse.tree(), Utf8Encoding::new())
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
fn evaluate_one_calculation() {
    let parse = parse(b"x = @[-(2 + 1) * 4 / 8]");
    let calc = parse.root().descendants().find_map(Calc::cast).unwrap();
    assert_eq!(evaluate(calc, &()), Ok(-1.5));
}

#[test]
#[should_panic(expected = "the resolution is of a different tree")]
fn resolution_of_a_different_tree() {
    let first = parse(b"@x = 1\na = @x");
    let second = parse(b"a = 1");
    let resolution = resolve(first.tree(), &());
    let _ = Deserializer::new(second.tree(), Utf8Encoding::new()).with_resolution(resolution);
}
