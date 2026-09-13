//! Inspect a Clausewitz text file through the **lossless** syntax tree
//! ([`jomini::text::syntax`]).
//!
//! Default mode highlights the source by token kind — and because the tree's
//! leaves tile the input exactly, the highlighted bytes (sans the ANSI codes)
//! *are* the input, which is the whole point of a lossless tree. With `--tree`
//! it instead dumps the parse tree as an indented S-expression. With `--format`
//! it reprints the file in the formatter's house style. Either way it prints a
//! footer (to stderr) reporting the round-trip check and diagnostics.
//!
//! ```sh
//! cargo run --example syntax -- tests/fixtures/ck3-header.txt
//! cargo run --example syntax -- --tree tests/fixtures/meta.txt
//! cargo run --example syntax -- --format tests/fixtures/meta.txt
//! printf 'rate = @[ (1 - x) * 2 ]\n' | cargo run --example syntax
//! ```

use jomini::text::syntax::{format, parse, SyntaxKind};
use std::io::{Read, Write};

fn main() {
    let mut path: Option<String> = None;
    let mut tree_mode = false;
    let mut format_mode = false;
    let mut color = true;
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--tree" => tree_mode = true,
            "--format" => format_mode = true,
            "--no-color" => color = false,
            "-h" | "--help" => {
                eprintln!(
                    "usage: syntax [--tree] [--format] [--no-color] [FILE]   \
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

    let tree = parse(&source);

    if format_mode {
        let _ = std::io::stdout().lock().write_all(&format(&source));
    } else if tree_mode {
        print!("{}", tree.debug_tree());
    } else {
        // Re-emit every leaf in order, wrapping it in its kind's color. With
        // --no-color, stdout is byte-for-byte identical to the input.
        let mut out = std::io::stdout().lock();
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
        let _ = out.flush();
    }

    // Footer on stderr, so a piped stdout stays clean.
    let lossless = tree.reconstruct() == source;
    eprintln!(
        "\n\x1b[90m— {} bytes · {} tokens · round-trip {} · {} error(s)\x1b[0m",
        source.len(),
        tree.tokens().count(),
        if lossless { "✓" } else { "✗ MISMATCH" },
        tree.errors().len(),
    );
    for e in tree.errors() {
        eprintln!("\x1b[31m  [{}..{}] {}\x1b[0m", e.range.0, e.range.1, e.message);
    }
}

/// ANSI SGR parameter for a token kind, or `None` to print it uncolored
/// (bare identifiers/numbers and whitespace are left in the terminal default).
fn ansi(kind: SyntaxKind) -> Option<&'static str> {
    use SyntaxKind::*;
    Some(match kind {
        Comment | Bom => "90",                     // grey
        Quoted => "32",                            // green
        Variable | CalcOpen | CalcClose => "36",   // cyan: @-things
        MacroParam | Plus | Minus | Star | Slash => "35", // magenta: substitutions & calc ops
        Operator | Number => "33",                 // yellow
        CalcIdent => "34",                         // blue: calc operands
        OpenBrace | CloseBrace | OpenParen | CloseParen => "1", // bold: grouping
        OpenBracket | CloseBracket | Bang => "31", // red: param brackets
        Error => "41",                             // red background
        _ => return None,                          // Unquoted, Whitespace
    })
}
