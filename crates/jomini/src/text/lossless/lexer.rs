//! The SIMD lexer. It divides the source into significant tokens and finds
//! the trivia in the gaps between them.
//!
//! The lexer classifies bytes with a lookup table. It scans unquoted scalars,
//! quoted strings, and comments 16 bytes at a time with [`fearless_simd`].
//! It scans whitespace one byte at a time, because almost all whitespace runs
//! are shorter than 9 bytes.

use super::Flavor;
use super::format::quote_is_closed;
use super::{SyntaxKind, TextRange};
use fearless_simd::{Level, Simd, dispatch, prelude::*, u8x16};

/// A significant token in the token buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) start: u32,
    pub(super) len: u32,
    pub(super) kind: SyntaxKind,
    pub(super) flags: u8,
}

/// The gap before this token contains a comment.
pub(super) const TOKEN_COMMENT_BEFORE: u8 = 1 << 0;
/// A diagnostic starts at this token.
pub(super) const TOKEN_ERROR: u8 = 1 << 1;

impl Token {
    #[inline]
    pub(super) fn end(self) -> u32 {
        self.start + self.len
    }

    #[inline]
    pub(super) fn range(self) -> TextRange {
        TextRange::span(self.start, self.end())
    }
}

const CLASS_OTHER: u8 = 0;
const CLASS_WS: u8 = 1;
pub(super) const CLASS_COMMENT: u8 = 2;
pub(super) const CLASS_QUOTE: u8 = 3;
pub(super) const CLASS_OPEN_BRACE: u8 = 4;
pub(super) const CLASS_CLOSE_BRACE: u8 = 5;
pub(super) const CLASS_OPEN_BRACKET: u8 = 6;
pub(super) const CLASS_CLOSE_BRACKET: u8 = 7;
pub(super) const CLASS_OPERATOR: u8 = 8;
pub(super) const CLASS_BANG: u8 = 9;
const CLASS_QUESTION: u8 = 10;
const CLASS_AT: u8 = 11;

/// The lexer class of each byte.
pub(super) static CLASS: [u8; 256] = {
    let mut table = [CLASS_OTHER; 256];
    table[b' ' as usize] = CLASS_WS;
    table[b'\t' as usize] = CLASS_WS;
    table[b'\r' as usize] = CLASS_WS;
    table[b'\n' as usize] = CLASS_WS;
    table[0x0b] = CLASS_WS;
    table[0x0c] = CLASS_WS;
    table[b';' as usize] = CLASS_WS;
    table[b'#' as usize] = CLASS_COMMENT;
    table[b'"' as usize] = CLASS_QUOTE;
    table[b'{' as usize] = CLASS_OPEN_BRACE;
    table[b'}' as usize] = CLASS_CLOSE_BRACE;
    table[b'[' as usize] = CLASS_OPEN_BRACKET;
    table[b']' as usize] = CLASS_CLOSE_BRACKET;
    table[b'=' as usize] = CLASS_OPERATOR;
    table[b'<' as usize] = CLASS_OPERATOR;
    table[b'>' as usize] = CLASS_OPERATOR;
    table[b'!' as usize] = CLASS_BANG;
    table[b'?' as usize] = CLASS_QUESTION;
    table[b'@' as usize] = CLASS_AT;
    table
};

/// Bytes that terminate an unquoted run. These either start their own token
/// (braces, brackets, operators, comment) or are whitespace. Notably `"`, `@`,
/// `$`, `!`, `?`, `:`, `.`, `-`, `/`, `|` and high bytes are *not* here, so they
/// are absorbed mid-identifier and only dispatch specially as a first byte.
///
/// A `"` in a run is part of the scalar, as in `TextTape`. Game files contain
/// typos such as `{ name" }`. If such a quote started a string, the string
/// would contain the `}` and change the structure of the remainder of the file.
pub(super) static STOP: [bool; 256] = {
    let mut table = [false; 256];
    let mut i = 0;
    while i < 256 {
        table[i] = matches!(
            CLASS[i],
            CLASS_WS
                | CLASS_COMMENT
                | CLASS_OPEN_BRACE
                | CLASS_CLOSE_BRACE
                | CLASS_OPEN_BRACKET
                | CLASS_CLOSE_BRACKET
                | CLASS_OPERATOR
        );
        i += 1;
    }
    table
};

/// What an unquoted run does at each byte: continue, stop, or note a `$`.
const RUN_CONTINUE: u8 = 0;
const RUN_STOP: u8 = 1;
const RUN_DOLLAR: u8 = 2;

static RUN: [u8; 256] = {
    let mut table = [RUN_CONTINUE; 256];
    let mut i = 0;
    while i < 256 {
        if STOP[i] {
            table[i] = RUN_STOP;
        }
        i += 1;
    }
    table[b'$' as usize] = RUN_DOLLAR;
    table
};

#[inline]
pub(super) fn is_ws(b: u8) -> bool {
    CLASS[b as usize] == CLASS_WS
}

/// A resumable lexer. Each [`Lexer::fill`] call appends the next tokens, so
/// the parser can read a small window of tokens instead of a full buffer.
pub(super) struct Lexer {
    level: Level,
    flavor: Flavor,
    /// The byte offset where lexing continues.
    offset: usize,
    /// The flags for the next token.
    flags: u8,
    /// All the source is lexed.
    pub(super) done: bool,
    /// The opening quote of a quoted string that has no closing quote.
    pub(super) unclosed_quote: Option<TextRange>,
    /// The offset of the opening quote of each string for which
    /// [`quote_hides_fields`] is true, in source order.
    pub(super) hiding_quotes: Vec<u32>,
}

/// Return `true` when `text`, a quoted string with both of its quotes, spans
/// a line break and its content ends with an operator, as in `"My Mod` and
/// `version = "` on the next line. Most likely, a closing quote is missing
/// before the line break, and the string hides the fields after it. No game
/// file has such a string.
#[inline]
pub(super) fn quote_hides_fields(text: &[u8]) -> bool {
    let [b'"', content @ .., b'"'] = text else {
        return false;
    };
    content
        .trim_ascii_end()
        .last()
        .is_some_and(|b| matches!(b, b'=' | b'<' | b'>'))
        && content.contains(&b'\n')
}

pub(super) const BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

impl Lexer {
    pub(super) fn new(source: &[u8], flavor: Flavor) -> Self {
        Lexer {
            level: Level::new(),
            flavor,
            offset: if source.starts_with(BOM) { 3 } else { 0 },
            flags: 0,
            done: source.is_empty(),
            unclosed_quote: None,
            hiding_quotes: Vec::new(),
        }
    }

    /// Make a lexer that starts at the byte `offset`. A token or the trivia
    /// before a token must start at `offset`.
    pub(super) fn at(source: &[u8], flavor: Flavor, offset: usize) -> Self {
        Lexer {
            level: Level::new(),
            flavor,
            offset,
            flags: 0,
            done: offset >= source.len(),
            unclosed_quote: None,
            hiding_quotes: Vec::new(),
        }
    }

    /// Append at least `min` tokens to `out`, or all the remaining tokens.
    #[inline(never)]
    pub(super) fn fill(&mut self, source: &[u8], out: &mut Vec<Token>, min: usize) {
        if self.done {
            return;
        }
        let limit = out.len().saturating_add(min);
        let (offset, flags) = dispatch!(self.level, simd => lex_simd(
            simd,
            source,
            self.flavor,
            (self.offset, self.flags),
            limit,
            out,
            &mut self.hiding_quotes,
        ));
        self.offset = offset;
        self.flags = flags;
        self.done = offset >= source.len();

        // Only the last token can be a quoted string without a closing quote,
        // because such a string continues to the end of the source.
        if self.done
            && let Some(t) = out.last()
            && t.kind == SyntaxKind::Quoted
            && t.flags & TOKEN_ERROR != 0
            && !quote_is_closed(&source[t.start as usize..])
        {
            self.unclosed_quote = Some(TextRange::span(t.start, t.start + 1));
        }
    }

    /// Return `true` when the gap after the last token contains a comment.
    /// The value is correct only when the lexer is done.
    pub(super) fn trailing_comment(&self) -> bool {
        self.flags & TOKEN_COMMENT_BEFORE != 0
    }
}

/// All the tokens of a source.
#[cfg(test)]
pub(super) struct Lexed {
    pub(super) tokens: Vec<Token>,
    /// The gap after the last token contains a comment.
    pub(super) trailing_comment: bool,
}

#[cfg(test)]
pub(super) fn lex(source: &[u8], flavor: Flavor) -> Lexed {
    let mut lexer = Lexer::new(source, flavor);
    let mut tokens = Vec::new();
    lexer.fill(source, &mut tokens, usize::MAX);
    Lexed {
        tokens,
        trailing_comment: lexer.trailing_comment(),
    }
}

/// Lex `source` from the state `(offset, flags)` into `out` until `out` has
/// `limit` tokens or the source ends. Return the state after the last token.
/// Push the offset of each string for which [`quote_hides_fields`] is true to
/// `hiding_quotes`.
///
/// The lexer pushes only significant tokens, so the bytes between two tokens
/// are always trivia. It accepts all input: each byte that matches no other
/// rule starts an unquoted run, and each branch moves forward one or more
/// bytes.
///
/// This function and the other generic `S: Simd` helpers use
/// `#[inline(always)]`. They must inline into the function that `dispatch!`
/// makes for each target feature, or they compile without the vector
/// instructions.
#[inline(always)]
pub(super) fn lex_simd<S: Simd>(
    simd: S,
    source: &[u8],
    flavor: Flavor,
    (mut i, mut flags): (usize, u8),
    limit: usize,
    out: &mut Vec<Token>,
    hiding_quotes: &mut Vec<u32>,
) -> (usize, u8) {
    let n = source.len();

    while i < n && out.len() < limit {
        let mut b = source[i];
        if is_ws(b) {
            // Most tokens follow whitespace. Skip it here, so that it does
            // not need a separate dispatch.
            i = skip_whitespace(source, i + 1);
            if i >= n {
                break;
            }
            b = source[i];
        }
        let start = i;
        let kind = match CLASS[b as usize] {
            CLASS_WS => {
                i = skip_whitespace(source, i + 1);
                continue;
            }
            CLASS_COMMENT => {
                i = find_line_end(simd, source, i + 1);
                flags |= TOKEN_COMMENT_BEFORE;
                continue;
            }
            CLASS_QUOTE => {
                i = scan_quoted(simd, source, i + 1);
                if i == n && !quote_is_closed(&source[start..]) {
                    flags |= TOKEN_ERROR;
                } else if quote_hides_fields(&source[start..i]) {
                    flags |= TOKEN_ERROR;
                    hiding_quotes.push(start as u32);
                }
                SyntaxKind::Quoted
            }
            CLASS_OPEN_BRACE => {
                i += 1;
                SyntaxKind::OpenBrace
            }
            CLASS_CLOSE_BRACE => {
                i += 1;
                SyntaxKind::CloseBrace
            }
            CLASS_OPEN_BRACKET => {
                i += 1;
                SyntaxKind::OpenBracket
            }
            CLASS_CLOSE_BRACKET => {
                i += 1;
                SyntaxKind::CloseBracket
            }
            CLASS_OPERATOR => {
                i += 1;
                if i < n && source[i] == b'=' {
                    i += 1; // ==, <=, >=
                }
                SyntaxKind::Operator
            }
            CLASS_BANG | CLASS_QUESTION if i + 1 < n && source[i + 1] == b'=' => {
                i += 2; // != or ?=
                SyntaxKind::Operator
            }
            CLASS_BANG => {
                i += 1;
                SyntaxKind::Bang
            }
            CLASS_AT if flavor.variables && i + 1 < n && source[i + 1] == b'[' => {
                // `@[ ... ]` calc: emit structured interior tokens so the
                // arithmetic expression can be parsed into a subtree.
                i = lex_calc(source, i, flags, out);
                flags = 0;
                continue;
            }
            _ => {
                // Ordinary unquoted run: consume up to the next terminator. This
                // catch-all is what keeps the lexer total (every byte classified).
                // The first byte is not a stop byte, so the scan can include
                // it in the `$` check.
                let (mut end, dollar) = scan_unquoted(simd, source, i);
                if matches!(source[end - 1], b'!' | b'?')
                    && end - start > 1
                    && end < n
                    && source[end] == b'='
                {
                    // `a!=b` is `a`, `!=`, `b`, as in `TextTape`. Also,
                    // `c:GBR?= THIS` (Vic3) is `c:GBR`, `?=`, `THIS`.
                    end -= 1;
                }
                i = end;
                let kind = classify_run(&source[start..i], dollar, flavor);
                // Most keys have the form `key = `. Lex the operator here,
                // because a branch is easier to predict than the jump table.
                if let Some(&[b' ', b'=', after]) = source.get(i..i + 3)
                    && is_ws(after)
                {
                    out.push(Token {
                        start: start as u32,
                        len: (i - start) as u32,
                        kind,
                        flags,
                    });
                    out.push(Token {
                        start: i as u32 + 1,
                        len: 1,
                        kind: SyntaxKind::Operator,
                        flags: 0,
                    });
                    flags = 0;
                    i += 2;
                    continue;
                }
                kind
            }
        };

        out.push(Token {
            start: start as u32,
            len: (i - start) as u32,
            kind,
            flags,
        });
        flags = 0;
    }

    (i, flags)
}

/// Classify an unquoted run. `dollar` is `true` when the run contains a `$`.
#[inline(always)]
fn classify_run(run: &[u8], dollar: bool, flavor: Flavor) -> SyntaxKind {
    match run {
        [b'@', b':', _, ..] if flavor.variables => SyntaxKind::Directive,
        [b'@', b'@', _, ..] if flavor.variables => SyntaxKind::VariableKey,
        [b'@', second, ..] if *second != b'@' && flavor.variables => SyntaxKind::Variable,
        _ => classify_plain_run(run, dollar, flavor),
    }
}

/// Classify an unquoted run that is not a reader variable or directive.
#[inline(always)]
fn classify_plain_run(run: &[u8], dollar: bool, flavor: Flavor) -> SyntaxKind {
    if dollar && flavor.macros && has_macro(run) {
        SyntaxKind::MacroParam
    } else {
        SyntaxKind::Unquoted
    }
}

/// Return `true` when `run` contains a `$...$` pair. An unquoted run has no
/// stop bytes, so any two `$` bytes enclose a macro name.
#[cold]
pub(super) fn has_macro(run: &[u8]) -> bool {
    run.iter().filter(|&&b| b == b'$').nth(1).is_some()
}

/// The run classes of a byte come from two tables: one for the low nibble and
/// one for the high nibble. The class of a byte is the bitwise AND of the two
/// values. Each low bit is one group of [`STOP`] bytes:
///
/// - `0x01`: `\t` `\n` `\v` `\f` `\r` (high nibble 0, low nibble 9 to D).
/// - `0x02`: ` ` `#` (high nibble 2, low nibble 0 or 3).
/// - `0x04`: `;` `<` `=` `>` (high nibble 3, low nibble B to E).
/// - `0x08`: `[` `]` `{` `}` (high nibble 5 or 7, low nibble B or D).
///
/// The class of `$` is [`RUN_CLASS_DOLLAR`] alone.
const RUN_CLASS_STOP: u8 = 0x0F;
const RUN_CLASS_DOLLAR: u8 = 0x80;

static RUN_LOW_NIBBLE: [u8; 16] = [
    0x02, 0, 0, 0x02, 0x80, 0, 0, 0, 0, 0x01, 0x01, 0x0D, 0x05, 0x0D, 0x04, 0,
];

static RUN_HIGH_NIBBLE: [u8; 16] = [
    0x01, 0, 0x82, 0x04, 0, 0x08, 0, 0x08, 0, 0, 0, 0, 0, 0, 0, 0,
];

/// Return the lane bits of the bytes in [`STOP`] and of the `$` bytes.
#[inline(always)]
pub(super) fn run_lanes<S: Simd>(simd: S, v: u8x16<S>) -> (u64, u64) {
    let low = u8x16::from_slice(simd, &RUN_LOW_NIBBLE).swizzle_dyn(v & 0x0Fu8);
    let high = u8x16::from_slice(simd, &RUN_HIGH_NIBBLE).swizzle_dyn(v >> 4);
    let class = low & high;
    let stops = !(class & RUN_CLASS_STOP).simd_eq(0u8).to_bitmask() & 0xFFFF;
    let dollars = class.simd_eq(RUN_CLASS_DOLLAR).to_bitmask();
    (stops, dollars)
}

#[inline(always)]
fn load<S: Simd>(simd: S, source: &[u8], i: usize) -> u8x16<S> {
    u8x16::from_slice(simd, &source[i..i + 16])
}

/// Return the end of an unquoted run that continues at `i`, and whether the
/// scanned bytes contain a `$`.
#[inline(always)]
pub(super) fn scan_unquoted<S: Simd>(simd: S, source: &[u8], mut i: usize) -> (usize, bool) {
    let mut dollar = false;
    while i + 16 <= source.len() {
        let (stops, dollars) = run_lanes(simd, load(simd, source, i));
        if stops != 0 {
            let at = stops.trailing_zeros();
            dollar |= dollars & ((1u64 << at) - 1) != 0;
            return (i + at as usize, dollar);
        }
        dollar |= dollars != 0;
        i += 16;
    }
    while i < source.len() {
        match RUN[source[i] as usize] {
            RUN_CONTINUE => {}
            RUN_STOP => break,
            _ => dollar = true,
        }
        i += 1;
    }
    (i, dollar)
}

/// Return the index of the first non-whitespace byte at or after `i`.
///
/// This function does not use SIMD. Almost all whitespace runs in saves and
/// game files are shorter than 9 bytes. On these runs, a byte loop is faster
/// than a 16-byte vector scan.
#[inline(always)]
pub(super) fn skip_whitespace(source: &[u8], mut i: usize) -> usize {
    while i < source.len() && is_ws(source[i]) {
        i += 1;
    }
    i
}

/// Return the index of the first `\n` or `\r` at or after `i`, or the end of
/// the source.
#[inline(always)]
pub(super) fn find_line_end<S: Simd>(simd: S, source: &[u8], mut i: usize) -> usize {
    while i + 16 <= source.len() {
        let v = load(simd, source, i);
        let hits = (v.simd_eq(b'\n') | v.simd_eq(b'\r')).to_bitmask();
        if hits != 0 {
            return i + hits.trailing_zeros() as usize;
        }
        i += 16;
    }
    while i < source.len() && source[i] != b'\n' && source[i] != b'\r' {
        i += 1;
    }
    i
}

/// Return the end of a quoted string whose body starts at `i`. The end is
/// after the closing quote. An unclosed string ends at the end of the source.
#[inline(always)]
pub(super) fn scan_quoted<S: Simd>(simd: S, source: &[u8], mut i: usize) -> usize {
    loop {
        let Some(at) = find_quote_stop(simd, source, i) else {
            return source.len();
        };
        if source[at] == b'"' {
            return at + 1;
        }
        // A backslash escapes the next byte.
        i = (at + 2).min(source.len());
    }
}

/// Return the index of the first quote or backslash at or after `i`.
#[inline(always)]
fn find_quote_stop<S: Simd>(simd: S, source: &[u8], mut i: usize) -> Option<usize> {
    while i + 16 <= source.len() {
        let v = load(simd, source, i);
        let hits = (v.simd_eq(b'"') | v.simd_eq(b'\\')).to_bitmask();
        if hits != 0 {
            return Some(i + hits.trailing_zeros() as usize);
        }
        i += 16;
    }
    source[i..]
        .iter()
        .position(|&b| matches!(b, b'"' | b'\\'))
        .map(|at| i + at)
}

/// A byte that ends a calc operand identifier or number.
#[inline]
pub(super) fn is_calc_stop(b: u8) -> bool {
    is_ws(b)
        || matches!(
            b,
            b'(' | b')' | b'+' | b'-' | b'*' | b'/' | b'[' | b']' | b'{' | b'}'
        )
}

/// Scan a calc numeric literal beginning at `i` (`source[i]` is a digit or a `.`
/// directly followed by a digit). Consumes digits, an optional fractional part,
/// and an optional `f` suffix (e.g. `1`, `10.0`, `.5`, `10.0f`).
pub(super) fn scan_number(source: &[u8], mut i: usize) -> usize {
    let n = source.len();
    while i < n && source[i].is_ascii_digit() {
        i += 1;
    }
    if i < n && source[i] == b'.' {
        i += 1;
        while i < n && source[i].is_ascii_digit() {
            i += 1;
        }
    }
    if i < n && source[i] == b'f' {
        i += 1;
    }
    i
}

/// Lex a `@[ ... ]` calc region. `i` points at the leading `@`. Emits a
/// `CalcOpen` token, then structured interior tokens (operands and operators)
/// until the matching `]` (emitted as `CalcClose`) or end of input, and returns
/// the index just past what it consumed. Interior whitespace stays in the gaps.
#[inline(always)]
fn lex_calc(source: &[u8], mut i: usize, flags: u8, out: &mut Vec<Token>) -> usize {
    let n = source.len();
    out.push(Token {
        start: i as u32,
        len: 2,
        kind: SyntaxKind::CalcOpen,
        flags,
    });
    i += 2; // past `@[`

    while i < n {
        let b = source[i];
        let start = i;
        let kind;

        if b == b']' {
            out.push(Token {
                start: start as u32,
                len: 1,
                kind: SyntaxKind::CalcClose,
                flags: 0,
            });
            return i + 1; // leave calc mode
        } else if matches!(b, b'{' | b'}') {
            // A brace belongs to the surrounding Clausewitz construct. Keep it
            // outside the calc so a mismatched brace cannot receive a fix.
            return i;
        } else if is_ws(b) {
            i = skip_whitespace(source, i + 1);
            continue;
        } else if b == b'(' {
            i += 1;
            kind = SyntaxKind::OpenParen;
        } else if b == b')' {
            i += 1;
            kind = SyntaxKind::CloseParen;
        } else if b == b'+' {
            i += 1;
            kind = SyntaxKind::Plus;
        } else if b == b'-' {
            i += 1;
            kind = SyntaxKind::Minus;
        } else if b == b'*' {
            i += 1;
            kind = SyntaxKind::Star;
        } else if b == b'/' {
            i += 1;
            kind = SyntaxKind::Slash;
        } else if b.is_ascii_digit() || (b == b'.' && i + 1 < n && source[i + 1].is_ascii_digit()) {
            i = scan_number(source, i);
            kind = SyntaxKind::Number;
        } else {
            // An operand name, as in `tier`, `leopard_x` or `@var`. In
            // incorrect input, this is any other byte that is not a stop
            // byte. This branch always consumes one or more bytes.
            i += 1;
            while i < n && !is_calc_stop(source[i]) {
                i += 1;
            }
            kind = SyntaxKind::CalcIdent;
        }

        out.push(Token {
            start: start as u32,
            len: (i - start) as u32,
            kind,
            flags: 0,
        });
    }

    i // reached EOF without a closing `]` (unterminated calc)
}

/// Lex one trivia piece of a gap. A gap holds only whitespace, comments, and a
/// leading BOM, because the lexer emits every other byte as a token.
pub(super) fn lex_trivia(source: &[u8], start: u32, end: u32) -> (SyntaxKind, u32) {
    let (s, e) = (start as usize, end as usize);
    let bytes = &source[s..e];
    if s == 0 && bytes.starts_with(BOM) {
        return (SyntaxKind::Bom, 3);
    }
    if bytes[0] == b'#' {
        let len = bytes
            .iter()
            .position(|&b| b == b'\n' || b == b'\r')
            .unwrap_or(bytes.len());
        return (SyntaxKind::Comment, len as u32);
    }
    debug_assert!(is_ws(bytes[0]), "a gap contains a significant byte");
    let len = bytes
        .iter()
        .position(|&b| !is_ws(b))
        .unwrap_or(bytes.len())
        .max(1);
    (SyntaxKind::Whitespace, len as u32)
}
