//! The contract between `TextDeserializer` and the syntax parser:
//! `TextDeserializer` gives the value that
//! `jomini::text::syntax::parse(data).deserialize(encoding)` gives, or an
//! error that names `jomini::text::syntax::parse`.
//!
//! The contract holds where the syntax parser reads the data without a
//! syntax error (an extra `}` aside). A few constructs of game files that no
//! save has are read differently, since telling them apart would cost
//! `TextDeserializer` work for each field. [`known_difference`] finds them
//! in the data.
//!
//! The tests of the crate, a test over a directory of game files, and the
//! `fuzz_text_contract` fuzz target share this module.

#![allow(dead_code)]

use jomini::text::syntax::OwnedValue;
use jomini::text::{Headered, MaybeQuoted, Property};
use jomini::{TextDeserializer, Windows1252Encoding};
use serde::Deserialize;
use serde::de::{self, DeserializeOwned, Deserializer, IgnoredAny, MapAccess, SeqAccess, Visitor};
use std::fmt::{self, Write as _};

/// The name in the errors of `TextDeserializer` for data that only the
/// syntax parser reads.
pub const HINT: &str = "jomini::text::syntax::parse";

/// A construct of game files that `TextDeserializer` reads differently from
/// the syntax parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Difference {
    /// `?=` attached to its key, as in `x.holder?= { }`.
    AttachedExists,
    /// A key that a block follows without an operator, as in `x { }`, as the
    /// first item of a block, after its empty blocks, or in a block without
    /// fields. The syntax parser reads it as a value without a key, which
    /// starts the remainder of an object, and as two elements of a sequence.
    /// `TextDeserializer` reads it as a field of an object, and, in a
    /// sequence, as a header and its block, as a `TextTape` does.
    OperatorlessBlock,
    /// A block where a key is expected: after a block, as in
    /// `a = { } { b }`, or first in a block, as in `a = { { b } }`
    /// read as a map. `TextDeserializer` skips it, as a `TextTape` does, and
    /// the syntax parser reads it as a value without a key.
    BlockAsKey,
    /// An operator right after a block, as in `a = { }\n= b` or the object
    /// template `{ a = b }={ 1 2 3 }`, which the syntax parser reads as a
    /// field with an empty key, or as a value without a key, and
    /// `TextDeserializer` as a field whose key is the block.
    OperatorAfterBlock,
    /// An operator at the start of a line after a value in a sequence or in
    /// the remainder of an object, as in `l = { 1\n= 2 }`, which the syntax
    /// parser reads as a field with an empty key, the parts `""`, `=`, and
    /// `2`, and `TextDeserializer` as the parts `=` and `2`, since it doesn't
    /// keep where lines start.
    LineStartOperator,
    /// A calculation that a block follows, as in `@[ x ] { }`, which the
    /// syntax parser reads as a value, as a calculation is not a key or a
    /// header, and `TextDeserializer` as the key of the block.
    CalcBeforeBlock,
}

/// What `TextDeserializer` gives for one reader of the data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// The syntax parser gives a syntax error, so the contract doesn't
    /// apply.
    SyntaxError,
    /// The same value or the same failure to read it.
    Same,
    /// An error that names the syntax parser.
    Hinted,
    /// A difference that [`known_difference`] explains.
    Known(Difference),
    /// An error that doesn't name the syntax parser, which reads a value.
    Unhinted { streaming: String, syntax: String },
    /// A silent difference, a different value, or a value where the syntax
    /// parser gives an error: the contract is broken.
    Broken { streaming: String, syntax: String },
}

/// The readers that the contract is checked with.
pub const READERS: [&str; 6] = ["any", "keys", "typed", "wrapped", "owned", "any-owned"];

/// Check the contract for each of [`READERS`]. The reader of `TextDeserializer`
/// must give the same as its slice, or an error that its buffer is too
/// small.
pub fn check(data: &[u8]) -> [(&'static str, Verdict); 6] {
    check_readers(data, true)
}

/// Check the contract for each of [`READERS`] with only the slice of
/// `TextDeserializer`.
pub fn check_slice(data: &[u8]) -> [(&'static str, Verdict); 6] {
    check_readers(data, false)
}

fn check_readers(data: &[u8], readers: bool) -> [(&'static str, Verdict); 6] {
    [
        ("any", check_with::<AnyRoot>(data, readers)),
        ("keys", check_with::<Keys>(data, readers)),
        ("typed", check_with::<Dyn>(data, readers)),
        ("wrapped", check_with::<Fields<Wrapped>>(data, readers)),
        ("owned", check_with::<Fields<OwnedValue>>(data, readers)),
        ("any-owned", check_with::<AnyOwned>(data, readers)),
    ]
}

fn show<T: fmt::Debug, E: fmt::Display>(result: &Result<T, E>) -> String {
    match result {
        Ok(x) => format!("Ok {x:?}"),
        Err(e) => format!("Err {e}"),
    }
}

fn check_with<T: DeserializeOwned + fmt::Debug>(data: &[u8], readers: bool) -> Verdict {
    let encoding = Windows1252Encoding::new();
    let parse = jomini::text::syntax::parse(data);
    let syntax = parse.deserialize::<T, _>(encoding);
    if let Err(err) = &syntax
        && matches!(err.kind(), jomini::text::syntax::de::ErrorKind::Syntax(_))
    {
        return Verdict::SyntaxError;
    }

    let streaming = TextDeserializer::from_slice(data, encoding).deserialize::<T>();
    if readers {
        for size in [16, 64, 4096] {
            let reader = TextDeserializer::from_reader_with_buf(data, encoding, vec![0; size])
                .deserialize::<T>();
            let small =
                matches!(&reader, Err(e) if matches!(e.kind(), jomini::ErrorKind::BufferTooSmall));
            if !small {
                assert_eq!(
                    show(&reader).split(" (offset").next(),
                    show(&streaming).split(" (offset").next(),
                    "the reader with a {size} byte buffer differs from the slice: {:?}",
                    String::from_utf8_lossy(data)
                );
            }
        }
    }

    let (streaming_text, syntax_text) = (show(&streaming), show(&syntax));
    match (&streaming, &syntax) {
        (Ok(_), Ok(_)) if streaming_text == syntax_text => return Verdict::Same,
        (Err(e), _) if e.to_string().contains(HINT) => return Verdict::Hinted,
        (Err(_), Err(_)) => return Verdict::Same,
        _ => {}
    }

    match known_difference(data) {
        Some(difference) => Verdict::Known(difference),
        None if streaming.is_err() => Verdict::Unhinted {
            streaming: streaming_text,
            syntax: syntax_text,
        },
        None => Verdict::Broken {
            streaming: streaming_text,
            syntax: syntax_text,
        },
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lexeme {
    Open,
    Close,
    Operator,
    Exists,
    Quoted,
    Unquoted,
    Calc,
}

#[derive(Debug, Clone, Copy)]
struct Token {
    lexeme: Lexeme,
    /// Whitespace or a comment is before the token.
    spaced: bool,
    /// A line break is before the token.
    line_start: bool,
}

/// Split `data` into tokens as both engines do, for the constructs of
/// [`Difference`].
fn tokens(data: &[u8]) -> Vec<Token> {
    let mut out = Vec::new();
    let mut i = 0;
    let mut spaced = true;
    let mut line_start = true;
    while i < data.len() {
        let x = data[i];
        let lexeme = match x {
            b'\n' => {
                (spaced, line_start) = (true, true);
                i += 1;
                continue;
            }
            b' ' | b'\t' | b'\r' | b';' | 0x0b | 0x0c => {
                spaced = true;
                i += 1;
                continue;
            }
            b'#' => {
                while i < data.len() && data[i] != b'\n' {
                    i += 1;
                }
                spaced = true;
                continue;
            }
            b'{' => {
                i += 1;
                Lexeme::Open
            }
            b'}' => {
                i += 1;
                Lexeme::Close
            }
            b'@' if data.get(i + 1) == Some(&b'[') => {
                while i < data.len() && data[i] != b']' {
                    i += 1;
                }
                i += 1;
                Lexeme::Calc
            }
            b'"' => {
                i += 1;
                while i < data.len() && data[i] != b'"' {
                    i += if data[i] == b'\\' { 2 } else { 1 };
                }
                i += 1;
                Lexeme::Quoted
            }
            b'?' if data.get(i + 1) == Some(&b'=') => {
                i += 2;
                Lexeme::Exists
            }
            b'=' | b'<' | b'>' | b'!' => {
                i += 1;
                if data.get(i) == Some(&b'=') {
                    i += 1;
                }
                Lexeme::Operator
            }
            _ => {
                while i < data.len()
                    && !matches!(
                        data[i],
                        b' ' | b'\t'
                            | b'\r'
                            | b'\n'
                            | b';'
                            | b'{'
                            | b'}'
                            | b'"'
                            | b'#'
                            | b'='
                            | b'<'
                            | b'>'
                            | b'!'
                            | b'?'
                    )
                {
                    i += 1;
                }
                if i < data.len()
                    && matches!(data[i], b'!' | b'?')
                    && data.get(i + 1) != Some(&b'=')
                {
                    i += 1;
                }
                Lexeme::Unquoted
            }
        };
        out.push(Token {
            lexeme,
            spaced,
            line_start,
        });
        (spaced, line_start) = (false, false);
    }
    out
}

/// Whether the token at `at` is the first item of a block, after the empty
/// blocks that start it.
fn first_in_block(tokens: &[Token], mut at: usize) -> bool {
    while at >= 2 && tokens[at - 1].lexeme == Lexeme::Close && tokens[at - 2].lexeme == Lexeme::Open
    {
        at -= 2;
    }
    at >= 1 && tokens[at - 1].lexeme == Lexeme::Open
}

/// Whether the block that the open token at `open` starts has fields: its
/// first item that is not an empty block is a key and an operator, or an
/// operator.
fn has_fields(tokens: &[Token], open: usize) -> bool {
    let at = |i: usize| tokens.get(i).map(|t| t.lexeme);
    let mut i = open + 1;
    while at(i) == Some(Lexeme::Open) && at(i + 1) == Some(Lexeme::Close) {
        i += 2;
    }
    match at(i) {
        Some(Lexeme::Operator | Lexeme::Exists) => true,
        Some(Lexeme::Unquoted | Lexeme::Quoted) => {
            matches!(at(i + 1), Some(Lexeme::Operator | Lexeme::Exists))
        }
        _ => false,
    }
}

/// The construct of game files in `data` that explains a difference between
/// `TextDeserializer` and the syntax parser, if any.
pub fn known_difference(data: &[u8]) -> Option<Difference> {
    let tokens = tokens(data);
    let at = |i: usize| tokens.get(i).map(|t| t.lexeme);
    let mut open = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        let prev = i.checked_sub(1).and_then(at);
        let before = i.checked_sub(2).and_then(at);
        match token.lexeme {
            Lexeme::Exists if !token.spaced && prev == Some(Lexeme::Unquoted) => {
                return Some(Difference::AttachedExists);
            }
            Lexeme::Operator | Lexeme::Exists if prev == Some(Lexeme::Close) => {
                return Some(Difference::OperatorAfterBlock);
            }
            Lexeme::Operator | Lexeme::Exists
                if token.line_start
                    && matches!(prev, Some(Lexeme::Unquoted | Lexeme::Quoted | Lexeme::Calc))
                    && !matches!(before, Some(Lexeme::Operator | Lexeme::Exists)) =>
            {
                return Some(Difference::LineStartOperator);
            }
            Lexeme::Open if prev == Some(Lexeme::Calc) => {
                return Some(Difference::CalcBeforeBlock);
            }
            Lexeme::Open => {
                let key = matches!(prev, Some(Lexeme::Unquoted | Lexeme::Quoted))
                    && !matches!(before, Some(Lexeme::Operator | Lexeme::Exists));
                if let Some(&parent) = open.last()
                    && key
                    && (first_in_block(&tokens, i - 1) || !has_fields(&tokens, parent))
                {
                    return Some(Difference::OperatorlessBlock);
                }

                if at(i + 1) != Some(Lexeme::Close)
                    && (prev == Some(Lexeme::Close) || (prev == Some(Lexeme::Open) && i >= 2))
                {
                    return Some(Difference::BlockAsKey);
                }
                open.push(i);
            }
            Lexeme::Close => {
                open.pop();
            }
            _ => {}
        }
    }
    None
}

/// A source of choices for [`generate`]: the bytes of a fuzzer or of
/// quickcheck, then 255, which chooses the end of each block.
pub struct Choices<'a> {
    data: &'a [u8],
    strict: bool,
}

impl<'a> Choices<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Choices {
            data,
            strict: false,
        }
    }

    /// Choices for a document without the constructs of [`Difference`], so
    /// that `TextDeserializer` and the syntax parser must agree on it.
    pub fn strict(data: &'a [u8]) -> Self {
        Choices { data, strict: true }
    }

    fn byte(&mut self) -> u8 {
        match self.data.split_first() {
            Some((&x, rest)) => {
                self.data = rest;
                x
            }
            None => 255,
        }
    }

    fn below(&mut self, n: usize) -> usize {
        usize::from(self.byte()) % n
    }

    /// True with a chance of `n` in 256.
    fn chance(&mut self, n: u8) -> bool {
        self.byte() < n
    }

    fn pick<'s>(&mut self, items: &[&'s str]) -> &'s str {
        items[self.below(items.len())]
    }
}

const SCALARS: &[&str] = &[
    "1",
    "2",
    "-3",
    "1.5",
    "yes",
    "no",
    "abc",
    "rgb",
    "hsv",
    "none",
    "pt",
    "\"q x\"",
    "\"\"",
    "1444.11.11",
    "x",
    "@v",
    "\"rgb\"",
    "\"a \\\" b\"",
    "@[ 1 + 2 ]",
    "caf\u{e9}",
    "\"\u{f6}\"",
    "-0.25",
];

/// The first letter of a key chooses its type in [`Dyn`].
const KEYS: &[&str] = &[
    "a",
    "s",
    "i",
    "f",
    "b",
    "v",
    "w",
    "n",
    "o",
    "l",
    "e",
    "g",
    "u",
    "p",
    "t",
    "h",
    "k",
    "x",
    "remainder",
    "c",
    "\"s\"",
    "y",
    "d",
    "m",
    "q",
    "r",
    "z",
    "j",
    "1",
    "80",
    "1444.11.11",
    "-2",
];

const SPACES: &[&str] = &[" ", " ", " ", "\n", "\n\t", "  ", " # c\n"];

const OPERATORS: &[&str] = &["<", ">", ">=", "<=", "?=", "!=", "=="];

/// Generate a document of the syntax of game files and saves from the
/// choices: fields, blocks, headers, values without keys, empty blocks,
/// operators, and comments.
pub fn generate(choices: &mut Choices) -> Vec<u8> {
    let mut out = String::new();
    if choices.chance(8) {
        out.push('\u{feff}');
    }
    let n = 1 + choices.below(5);
    for i in 0..n {
        if i > 0 {
            out.push_str(choices.pick(SPACES));
        }
        field(choices, 0, &mut out);
    }
    if choices.chance(12) {
        let mut len = choices.below(out.len() + 1);
        while !out.is_char_boundary(len) {
            len -= 1;
        }
        out.truncate(len);
    }
    out.into_bytes()
}

fn key(choices: &mut Choices, out: &mut String) {
    let key = choices.pick(KEYS);
    out.push_str(key);
    if !key.starts_with('"') && key != "remainder" && choices.chance(128) {
        let _ = write!(out, "{}", choices.below(3));
    }
}

/// The depth of blocks after which values are scalars.
const MAX_DEPTH: usize = 6;

fn value(choices: &mut Choices, depth: usize, header: bool, out: &mut String) {
    let x = choices.byte();
    if depth > MAX_DEPTH || x < 102 {
        out.push_str(choices.pick(SCALARS));
    } else if x < 128 && header {
        out.push_str(choices.pick(&[
            "rgb",
            "hsv",
            "pt",
            "none",
            "x",
            "\"rgb\"",
            "\"tab_glow\"",
            "\"a \\\" b\"",
        ]));
        out.push_str(choices.pick(SPACES));
        block(choices, depth + 1, out);
    } else if x < 140 {
        out.push_str("{ }");
    } else if x < 150 {
        sparse(choices, out);
    } else if choices.strict && !header {
        // A block that is an element of an array is a key without an
        // operator, or a block where a key is expected, when the array is
        // read as an object.
        out.push_str(choices.pick(SCALARS));
    } else {
        block(choices, depth + 1, out);
    }
}

/// A sparse array of a save, as `{ 80 0=278.75 1=433.88 }`: its length,
/// and then the elements that are set, by index.
fn sparse(choices: &mut Choices, out: &mut String) {
    let n = choices.below(4);
    let _ = write!(out, "{{ {}", 80 + n);
    for i in 0..n {
        let _ = write!(out, " {i}={}", choices.pick(SCALARS));
    }
    out.push_str(" }");
}

fn block(choices: &mut Choices, depth: usize, out: &mut String) {
    out.push('{');
    out.push_str(choices.pick(SPACES));
    let array = choices.chance(90);
    // Deep blocks are short, so a document stays small.
    let n = choices.below(if depth > 3 { 2 } else { 5 });
    for _ in 0..n {
        if array {
            if choices.chance(205) {
                value(choices, depth, false, out);
            } else {
                out.push_str("{ }");
            }
        } else {
            field(choices, depth, out);
        }
        out.push_str(choices.pick(SPACES));
    }
    if choices.chance(31) {
        out.push_str(choices.pick(SCALARS));
        out.push_str(choices.pick(SPACES));
        if choices.chance(128) {
            out.push_str(choices.pick(SCALARS));
            out.push_str(choices.pick(SPACES));
            if choices.chance(128) && depth <= MAX_DEPTH {
                field(choices, depth, out);
                out.push_str(choices.pick(SPACES));
            }
        }
    }
    if choices.chance(20) && !(choices.strict && after_value(out)) {
        out.push_str("{ } ");
    }
    out.push('}');
}

/// Whether the last token of `out` is a value without a key, after which
/// an operator at the start of a line is a [`Difference::LineStartOperator`],
/// or a calculation, after which a block is a
/// [`Difference::CalcBeforeBlock`]. The generator of a strict document
/// leaves these out.
fn after_value(out: &str) -> bool {
    let tokens = tokens(out.as_bytes());
    let at = |i: usize| tokens.get(i).map(|t| t.lexeme);
    let n = tokens.len();
    match n.checked_sub(1).and_then(at) {
        Some(Lexeme::Calc) => true,
        Some(Lexeme::Unquoted | Lexeme::Quoted) => !matches!(
            n.checked_sub(2).and_then(at),
            Some(Lexeme::Operator | Lexeme::Exists)
        ),
        _ => false,
    }
}

fn field(choices: &mut Choices, depth: usize, out: &mut String) {
    let x = choices.byte();

    // A field with an empty key, as `=1825.6.28` in an EU4 save, at the
    // start of a line, where the syntax parser reads it, and not after a
    // block, where it reads it as a value without a key.
    if x >= 250 && !out.trim_end().ends_with('}') && !(choices.strict && after_value(out)) {
        out.push_str("\n=");
        out.push_str(choices.pick(SCALARS));
        return;
    }

    key(choices, out);
    out.push_str(choices.pick(SPACES));
    if x < 10 && depth <= MAX_DEPTH && !choices.strict {
        block(choices, depth + 1, out);
        return;
    }
    out.push_str(if choices.chance(230) {
        "="
    } else {
        choices.pick(OPERATORS)
    });
    out.push_str(choices.pick(SPACES));
    value(choices, depth, true, out);
}

/// Inputs that cover each rule of the contract.
pub const PROBES: &[&[u8]] = &[
    b"a = { a = 1 }",
    b"v = { a = 1 }",
    b"v = { 1 = 2 }",
    b"o = { c d a1 = b }",
    b"c d a1 = b",
    b"o = { s = b c d s2 = f }",
    b"a = { x = 1 c d e = f }",
    b"a = { x = 1 c d e < f }",
    b"o = rgb { s = 1 }",
    b"v = rgb { 1 2 3 }",
    b"l = { rgb { 1 2 3 } }",
    b"a = { {} 1 2 }",
    b"l = { {} { 1 } }",
    b"n = { {} { s = 1 } }",
    b"a = { {} { s = 1 } }",
    b"a = { {} {} }",
    b"e = rgb { 1 2 3 }",
    b"e = { rgb = { 1 2 3 } }",
    b"e = pt { x = 1 }",
    b"t = { 1 2 3 }",
    b"h = { a = b c d }",
    b"o = { s = b remainder }",
    b"a = { x = 1 c } b1 = yes",
    b"s = x }  s2 = y",
    b"a = { [[p] x = 1 ] }",
    b"x = { code [[ v = { a = b } ]] } s = 1",
    b"\n= 1\ns = 2",
    b"a = { = = 1 b }",
    b"v = x { }",
    b"a = x {",
    b"a = { a = b }={ 1 2 3 } c = d",
    b"a = { 1 2 3",
    b"a = @[ 1 + 2 ]",
    b"a = { 1 a = 2 { 3 } }",
    b"a = { x = 1 y } z = 2",
    b"a = rgb { a = 1 } b = 2",
    b"o = { s = 1 rgb { 1 2 } s2 = 3 }",
    b"o = { x { 1 } s = 2 }",
    b"a = { 1 } b = { 2 } }",
    b"a = \"x\" { 1 }",
    b"a = \"x\" { }",
    b"e = \"rgb\" { 1 2 3 }",
    b"o = { s = \"q \\\" r\" { a = 1 } t = 2 }",
    b"e = rgb { {} 1 2 3 }",
    b"a = { a = b } = 3",
    b"a == b",
    b"y = abc",
    b"d = { 1 }",
    b"a = { x = 1 {} y }",
    b"l = { { 1 } {} { 2 } }",
    b"o = { i = 1 b = yes c { a = 1 } d = { e } = f }",
    b"scripted_trigger name = { a = 1 } other = { b = 2 }",
    b"x.holder?= { a = 1 }",
    b"a = { x { a = 1 } s = 2 }",
    b"a = { }\n= b",
    b"income_taxes={ 80 0=278.75 1=433.88 }",
    // `d` swallows the error of a 16 byte buffer.
    b"\xef\xbb\xbfa = { a = 1 { } } a0 = 1 a0 =\n\t{ } a  = { 80 } d  = { { } # c\n1444.11.11 # c\n}",
];

/// Any value, read with `deserialize_any`.
#[derive(Debug, PartialEq)]
pub enum AnyValue {
    Unit,
    Str(String),
    Seq(Vec<AnyValue>),
    Map(Vec<(String, AnyValue)>),
}

struct AnyVisitor;

impl<'de> Visitor<'de> for AnyVisitor {
    type Value = AnyValue;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("any value")
    }

    fn visit_unit<E: de::Error>(self) -> Result<AnyValue, E> {
        Ok(AnyValue::Unit)
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<AnyValue, E> {
        Ok(AnyValue::Str(v.to_string()))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<AnyValue, A::Error> {
        let mut out = Vec::new();
        while let Some(x) = seq.next_element()? {
            out.push(x);
        }
        Ok(AnyValue::Seq(out))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<AnyValue, A::Error> {
        let mut out = Vec::new();
        while let Some(key) = map.next_key::<String>()? {
            out.push((key, map.next_value()?));
        }
        Ok(AnyValue::Map(out))
    }
}

impl<'de> Deserialize<'de> for AnyValue {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(AnyVisitor)
    }
}

/// The root read with `deserialize_any`.
#[derive(Debug, PartialEq)]
pub struct AnyRoot(pub AnyValue);

impl<'de> Deserialize<'de> for AnyRoot {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_any(AnyVisitor).map(AnyRoot)
    }
}

/// The keys of the root, whose values are skipped.
#[derive(Debug, PartialEq)]
pub struct Keys(pub Vec<String>);

impl<'de> Deserialize<'de> for Keys {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = Keys;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a map")
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Keys, A::Error> {
                let mut keys = Vec::new();
                while let Some(key) = map.next_key::<String>()? {
                    map.next_value::<IgnoredAny>()?;
                    keys.push(key);
                }
                Ok(Keys(keys))
            }
        }

        d.deserialize_map(V)
    }
}

/// An enum read from a header, as in `color = rgb { 1 2 3 }`.
#[derive(Deserialize, Debug, PartialEq)]
pub enum Color {
    #[serde(rename = "rgb")]
    Rgb(u8, u8, u8),
    #[serde(rename = "hsv")]
    Hsv(Vec<f32>),
    #[serde(rename = "pt")]
    Pt { x: i32, y: Option<i32> },
    #[serde(rename = "none")]
    None,
}

/// A value that is a string, or `None` when it can't be one. An error that
/// names the syntax parser is not hidden, since it tells the caller that
/// `TextDeserializer` doesn't read the data.
#[derive(Debug, PartialEq)]
pub struct Lenient(Option<String>);

impl<'de> Deserialize<'de> for Lenient {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
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
            Err(e) if e.to_string().contains(HINT) => Err(e),
            Err(_) => Ok(Lenient(None)),
        }
    }
}

/// A map whose value types are chosen by the first letter of the key, so a
/// generated document is read into many types.
#[derive(Debug, PartialEq)]
pub struct Dyn(pub String);

impl<'de> Deserialize<'de> for Dyn {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_map(DynVisitor)
    }
}

struct DynVisitor;

impl<'de> Visitor<'de> for DynVisitor {
    type Value = Dyn;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a map")
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Dyn, A::Error> {
        use std::collections::BTreeMap;
        let mut out = String::from("{");
        while let Some(key) = map.next_key::<String>()? {
            let first = key.chars().next().unwrap_or('_');
            let value = if key == "remainder" {
                format!("{:?}", map.next_value::<AnyValue>()?)
            } else {
                match first {
                    'a' => format!("{:?}", map.next_value::<AnyValue>()?),
                    's' => format!("{:?}", map.next_value::<String>()?),
                    'i' => format!("{:?}", map.next_value::<i64>()?),
                    'f' => format!("{:?}", map.next_value::<f64>()?),
                    'b' => format!("{:?}", map.next_value::<bool>()?),
                    'v' => format!("{:?}", map.next_value::<Vec<String>>()?),
                    'w' => format!("{:?}", map.next_value::<Vec<i64>>()?),
                    'n' => format!("{:?}", map.next_value::<Vec<Dyn>>()?),
                    'o' => format!("{:?}", map.next_value::<Dyn>()?),
                    'l' => format!("{:?}", map.next_value::<Vec<Vec<String>>>()?),
                    'e' => format!("{:?}", map.next_value::<Color>()?),
                    'g' => format!("{:?}", map.next_value::<Vec<Color>>()?),
                    'u' => format!("{:?}", map.next_value::<()>()?),
                    'p' => format!("{:?}", map.next_value::<Option<String>>()?),
                    't' => format!("{:?}", map.next_value::<(i64, i64)>()?),
                    'h' => format!("{:?}", map.next_value::<BTreeMap<String, String>>()?),
                    'k' => format!("{:?}", map.next_value::<Vec<(String, i64)>>()?),
                    'y' => match map.next_value::<i64>() {
                        Ok(v) => format!("int {v}"),
                        Err(_) => format!("retry {:?}", map.next_value::<String>()?),
                    },
                    'd' => format!("{:?}", map.next_value::<Lenient>()?),
                    'm' => format!("{:?}", map.next_value::<BTreeMap<String, AnyValue>>()?),
                    _ => {
                        map.next_value::<IgnoredAny>()?;
                        String::from("_")
                    }
                }
            };
            let _ = write!(out, "{key:?}:{value} ");
        }
        out.push('}');
        Ok(Dyn(out))
    }
}

/// The fields of the root in order.
#[derive(Debug, PartialEq)]
pub struct Fields<W>(pub Vec<(String, W)>);

impl<'de, W: Deserialize<'de>> Deserialize<'de> for Fields<W> {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V<W>(std::marker::PhantomData<W>);

        impl<'de, W: Deserialize<'de>> Visitor<'de> for V<W> {
            type Value = Fields<W>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("a map")
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
                let mut out = Vec::new();
                while let Some(key) = map.next_key::<String>()? {
                    out.push((key, map.next_value()?));
                }
                Ok(Fields(out))
            }
        }

        d.deserialize_map(V(std::marker::PhantomData))
    }
}

/// A value whose every part is wrapped in [`MaybeQuoted`] and [`Headered`],
/// and whose fields with an operator other than `=` are a [`Property`], as
/// the first letter of the key `z` asks.
#[derive(Debug, PartialEq)]
pub enum Wrapped {
    Unit,
    Str(String),
    Seq(Vec<MaybeQuoted<Headered<Wrapped>>>),
    Map(Vec<(String, WrappedField)>),
}

/// The value of a field of [`Wrapped`]: a [`Property`] under a key that
/// starts with `z`.
#[derive(Debug, PartialEq)]
pub enum WrappedField {
    Plain(MaybeQuoted<Headered<Wrapped>>),
    Property(Property<MaybeQuoted<Headered<Wrapped>>>),
}

impl<'de> Deserialize<'de> for Wrapped {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = Wrapped;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("any value")
            }

            fn visit_unit<E: de::Error>(self) -> Result<Wrapped, E> {
                Ok(Wrapped::Unit)
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<Wrapped, E> {
                Ok(Wrapped::Str(v.to_string()))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Wrapped, A::Error> {
                let mut out = Vec::new();
                while let Some(x) = seq.next_element()? {
                    out.push(x);
                }
                Ok(Wrapped::Seq(out))
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Wrapped, A::Error> {
                let mut out = Vec::new();
                while let Some(key) = map.next_key::<String>()? {
                    let value = if key.starts_with('z') {
                        WrappedField::Property(map.next_value()?)
                    } else {
                        WrappedField::Plain(map.next_value()?)
                    };
                    out.push((key, value));
                }
                Ok(Wrapped::Map(out))
            }
        }

        d.deserialize_any(V)
    }
}

/// Any value read with `deserialize_any`, whose `remainder` is an owned
/// value, at the root and in blocks.
#[derive(Debug, PartialEq)]
pub enum AnyOwned {
    Unit,
    Str(String),
    Seq(Vec<AnyOwned>),
    Map(Vec<(String, AnyOwnedField)>),
}

/// The value of a field of [`AnyOwned`].
#[derive(Debug, PartialEq)]
pub enum AnyOwnedField {
    Remainder(OwnedValue),
    Value(AnyOwned),
}

impl<'de> Deserialize<'de> for AnyOwned {
    fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        struct V;

        impl<'de> Visitor<'de> for V {
            type Value = AnyOwned;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str("any value")
            }

            fn visit_unit<E: de::Error>(self) -> Result<AnyOwned, E> {
                Ok(AnyOwned::Unit)
            }

            fn visit_str<E: de::Error>(self, v: &str) -> Result<AnyOwned, E> {
                Ok(AnyOwned::Str(v.to_string()))
            }

            fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<AnyOwned, A::Error> {
                let mut out = Vec::new();
                while let Some(x) = seq.next_element()? {
                    out.push(x);
                }
                Ok(AnyOwned::Seq(out))
            }

            fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<AnyOwned, A::Error> {
                let mut out = Vec::new();
                while let Some(key) = map.next_key::<String>()? {
                    let value = if key == "remainder" {
                        AnyOwnedField::Remainder(map.next_value()?)
                    } else {
                        AnyOwnedField::Value(map.next_value()?)
                    };
                    out.push((key, value));
                }
                Ok(AnyOwned::Map(out))
            }
        }

        d.deserialize_any(V)
    }
}
