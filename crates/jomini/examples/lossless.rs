//! Examine a Clausewitz text file through the **lossless** syntax tree
//! ([`jomini::text::lossless`]).
//!
//! The default mode colors the source by token kind. The leaves of the tree
//! cover the source exactly, so with `--no-color` the output is the input.
//! The other modes:
//!
//! - `--tree` prints the tree. Each line shows a kind and a byte range, such
//!   as `Field@0..10`.
//! - `--tokens` prints one token on each line.
//! - `--no-trivia` hides whitespace and comments from `--tree` and `--tokens`.
//! - `--format` prints the file in the house style of the formatter.
//!
//! Each mode then writes a footer to stderr: the round-trip check and each
//! syntax error at its line and column.
//!
//! ```sh
//! cargo run --features lossless --example lossless -- tests/fixtures/meta.txt
//! cargo run --features lossless --example lossless -- --tree tests/fixtures/meta.txt
//! cargo run --features lossless --example lossless -- --tree --no-trivia tests/fixtures/meta.txt
//! cargo run --features lossless --example lossless -- --tokens tests/fixtures/meta.txt
//! cargo run --features lossless --example lossless -- --format tests/fixtures/meta.txt
//! printf 'rate = @[ (1 - x) * 2 ]\n' | cargo run --features lossless --example lossless -- --tree
//! ```

use jomini::text::lossless::{FormatOptions, LineIndex, SyntaxElement, SyntaxKind, parse};
use std::io::{Read, Write};

#[derive(PartialEq)]
enum Mode {
    Highlight,
    Tree,
    Tokens,
    Format,
}

fn main() {
    let mut path: Option<String> = None;
    let mut mode = Mode::Highlight;
    let mut trivia = true;
    let mut color = true;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--tree" => mode = Mode::Tree,
            "--tokens" => mode = Mode::Tokens,
            "--format" => mode = Mode::Format,
            "--no-trivia" => trivia = false,
            "--no-color" => color = false,
            "-h" | "--help" => {
                eprintln!(
                    "usage: lossless [--tree | --tokens | --format] [--no-trivia] [--no-color] [FILE]\n\
                     (reads stdin if no FILE)"
                );
                return;
            }
            _ => path = Some(arg),
        }
    }

    let source = match &path {
        Some(p) => std::fs::read(p).unwrap_or_else(|e| {
            eprintln!("error reading {p}: {e}");
            std::process::exit(1);
        }),
        None => {
            let mut buf = Vec::new();
            std::io::stdin().read_to_end(&mut buf).expect("read stdin");
            buf
        }
    };

    let tree = parse(&source[..]);
    let mut out = std::io::stdout().lock();

    match mode {
        Mode::Format => {
            let _ = out.write_all(&tree.format(&FormatOptions::default()));
        }
        Mode::Tree if trivia => {
            let _ = out.write_all(tree.debug_tree().as_bytes());
        }
        Mode::Tree => {
            // `debug_tree` always includes the trivia. This walk uses
            // `significant_children`, which skips it.
            fn go(el: SyntaxElement<'_, '_>, depth: usize, out: &mut impl Write) {
                let _ = write!(
                    out,
                    "{:indent$}{:?}@{}",
                    "",
                    el.kind(),
                    el.text_range(),
                    indent = depth * 2
                );
                match el {
                    SyntaxElement::Node(n) => {
                        let _ = writeln!(out);
                        for c in n.significant_children() {
                            go(c, depth + 1, out);
                        }
                    }
                    SyntaxElement::Token(t) => {
                        let _ = writeln!(out, " {:?}", String::from_utf8_lossy(t.text()));
                    }
                }
            }
            go(SyntaxElement::Node(tree.root()), 0, &mut out);
        }
        Mode::Tokens => {
            let tokens: Box<dyn Iterator<Item = _>> = if trivia {
                Box::new(tree.tokens())
            } else {
                Box::new(tree.significant_tokens())
            };
            for tok in tokens {
                let _ = writeln!(
                    out,
                    "{:?}@{} {:?}",
                    tok.kind(),
                    tok.text_range(),
                    String::from_utf8_lossy(tok.text())
                );
            }
        }
        Mode::Highlight => {
            // Write each leaf in order, in the color of its kind. With
            // --no-color, stdout is the same bytes as the input.
            for tok in tree.tokens() {
                match (color, ansi(tok.kind())) {
                    (true, Some(sgr)) => {
                        let _ = write!(out, "\x1b[{sgr}m");
                        let _ = out.write_all(tok.text());
                        let _ = write!(out, "\x1b[0m");
                    }
                    _ => {
                        let _ = out.write_all(tok.text());
                    }
                }
            }
        }
    }
    let _ = out.flush();

    // The footer goes to stderr, so that a piped stdout stays clean.
    let (dim, red, reset) = if color {
        ("\x1b[90m", "\x1b[31m", "\x1b[0m")
    } else {
        ("", "", "")
    };
    let lossless = tree.tokens().flat_map(|t| t.text()).eq(source.iter());
    eprintln!(
        "\n{dim}— {} bytes · {} tokens · round-trip {} · {} error(s){reset}",
        source.len(),
        tree.tokens().count(),
        if lossless { "✓" } else { "✗ MISMATCH" },
        tree.errors().len(),
    );
    let lines = LineIndex::new(&source);
    let name = path.as_deref().unwrap_or("<stdin>");
    for e in tree.errors() {
        let at = lines.line_col(e.range.start());
        let opened = match &e.recovery {
            Some(r) => format!(", opened at {}", lines.line_col(r.opening_range.start())),
            None => String::new(),
        };
        eprintln!(
            "{red}  {name}:{at}: {} ({}{opened}){reset}",
            e.message(),
            e.range
        );
    }
}

/// The ANSI SGR parameter for a token kind, or `None` to print it with no
/// color (bare identifiers, numbers, and whitespace).
fn ansi(kind: SyntaxKind) -> Option<&'static str> {
    use SyntaxKind::*;
    Some(match kind {
        Comment | Bom => "90",                                  // grey
        Quoted => "32",                                         // green
        Variable | CalcOpen | CalcClose => "36",                // cyan: @-things
        MacroParam | Plus | Minus | Star | Slash => "35",       // magenta: substitutions & calc ops
        Operator | Number => "33",                              // yellow
        CalcIdent => "34",                                      // blue: calc operands
        OpenBrace | CloseBrace | OpenParen | CloseParen => "1", // bold: grouping
        OpenBracket | CloseBracket | Bang => "31",              // red: param brackets
        _ => return None,                                       // Unquoted, Whitespace
    })
}
