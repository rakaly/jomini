//! The formatter. It writes a tree again with normalized whitespace.

use super::cursor::Gap;
use super::lexer::BOM;
use super::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, SyntaxTree, parse};

/// Configuration for [`SyntaxTree::format`].
///
/// ```
/// use jomini::text::lossless::{parse, FormatOptions};
///
/// let opts = FormatOptions::default().with_indent("    ");
/// let parse = parse(b"a = { b = 1 }");
/// assert_eq!(parse.format(&opts), b"a = {\n    b = 1\n}\n");
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct FormatOptions {
    /// The string emitted once per nesting level of indentation. Defaults to a
    /// single tab (the Paradox convention); set it to spaces if preferred.
    pub indent: String,
}

impl FormatOptions {
    /// Set [`indent`](FormatOptions::indent).
    #[must_use]
    pub fn with_indent(mut self, indent: impl Into<String>) -> Self {
        self.indent = indent.into();
        self
    }
}

impl Default for FormatOptions {
    fn default() -> Self {
        FormatOptions {
            indent: String::from("\t"),
        }
    }
}

/// Parse `source` with the default [`Flavor`](super::Flavor) and reformat it with the
/// default [`FormatOptions`]. See [`SyntaxTree::format`] for the style. For a
/// different flavor, such as the HoI4 flavor, use
/// [`parse_with`](super::parse_with) and [`SyntaxTree::format`].
///
/// ```
/// use jomini::text::lossless::format;
///
/// let source = b"building={cost=100 # gold\nsize={1 2}}";
/// let expected = "building = {\n\tcost = 100 # gold\n\tsize = { 1 2 }\n}\n";
/// assert_eq!(String::from_utf8(format(source)).unwrap(), expected);
/// ```
#[must_use]
pub fn format(source: &[u8]) -> Vec<u8> {
    parse(source).format(&FormatOptions::default())
}

impl<'t, 'a> SyntaxTree<'t, 'a> {
    /// Reformat the tree with the given [`FormatOptions`].
    ///
    /// The formatter changes only the whitespace. It writes each significant
    /// token, each comment, and the BOM with the same bytes. Thus a parse of
    /// the output gives the same significant tokens, and a second format
    /// makes no change.
    ///
    /// The style is:
    ///
    /// - Each entry is on its own line. Each nesting level adds one
    ///   [`indent`](FormatOptions::indent).
    /// - A field is `key = value`, with one space on each side of the
    ///   operator.
    /// - A block that has only scalars stays on one line (`{ 1 2 3 }`). A
    ///   block with fields, blocks, or comments has one entry on each line. An
    ///   empty block becomes `{}`.
    /// - A comment always ends its line, so it cannot hide the token after it.
    ///   A comment inside a field moves to a line before the field.
    /// - One blank line between entries stays. More blank lines become one.
    /// - The document ends with one newline, unless the last token is a
    ///   quoted string that continues to the end of the source.
    ///
    /// The formatter writes parameter, code, calc, and `Bogus` nodes with
    /// their source bytes, and each loose token on its own line.
    pub fn format(&self, opts: &FormatOptions) -> Vec<u8> {
        let mut f = Fmt {
            out: Vec::with_capacity(self.source.len()),
            opts,
            comment_open: false,
            quote_open: false,
        };
        f.fmt_root(self.root());
        f.finish()
    }
}

/// Count the line breaks in `bytes`. A line break is `\n`, `\r\n`, or a `\r`
/// alone, the same as for the lexer.
fn count_newlines(bytes: &[u8]) -> usize {
    let lf = bytes.iter().filter(|&&b| b == b'\n').count();
    let lone_cr = bytes
        .iter()
        .enumerate()
        .filter(|&(i, &b)| b == b'\r' && bytes.get(i + 1) != Some(&b'\n'))
        .count();
    lf + lone_cr
}

/// Return `false` when `text` starts with a `"` that has no closing `"`. A
/// backslash escapes the next byte, as in the lexer.
///
/// The lexer uses this to find a string that continues to the end of the
/// source. The formatter uses it because such a string takes the bytes that
/// follow it.
pub(super) fn quote_is_closed(text: &[u8]) -> bool {
    if text.first() != Some(&b'"') {
        return true;
    }
    let mut i = 1;
    while i < text.len() {
        match text[i] {
            b'\\' => i += 2,
            b'"' => return true,
            _ => i += 1,
        }
    }
    false
}

/// Whether `el` is a bare scalar token eligible to keep its block inline.
fn is_inline_scalar(el: SyntaxElement<'_, '_>) -> bool {
    el.kind().is_scalar() && quote_is_closed(el.text())
}

/// Collect the comments that sit *inline* within a field/headered-block line —
/// directly inside the node, or inside a nested headered block (a field's value
/// can be `rgb # c { … }`). Comments inside a [`SyntaxKind::Block`] are *not*
/// gathered: those belong to the block's own layout. Such inline comments are
/// hoisted ahead of the line so they can never sit mid-line or fall inside a
/// multi-line block value (which would break idempotence).
fn collect_inline_comments<'t, 'a>(node: SyntaxNode<'t, 'a>, out: &mut Vec<SyntaxToken<'t, 'a>>) {
    for el in node.children() {
        match el {
            SyntaxElement::Token(t) if t.kind() == SyntaxKind::Comment => out.push(t),
            SyntaxElement::Node(n)
                if matches!(n.kind(), SyntaxKind::Field | SyntaxKind::HeaderedBlock) =>
            {
                collect_inline_comments(n, out);
            }
            _ => {}
        }
    }
}

struct Fmt<'o> {
    out: Vec<u8>,
    opts: &'o FormatOptions,
    /// True when the current output line ends in a comment, so the next thing
    /// emitted must start on a new line (a comment runs to end of line).
    comment_open: bool,
    /// True when the last token emitted is an unterminated quoted string, which
    /// would absorb a following newline; suppresses the trailing newline.
    quote_open: bool,
}

impl Fmt<'_> {
    fn finish(mut self) -> Vec<u8> {
        // Exactly one trailing newline when there is content, unless the last
        // token is a quoted string that continues to the end of the source.
        if !self.out.is_empty() && self.out.last() != Some(&b'\n') && !self.quote_open {
            self.out.push(b'\n');
        }
        self.out
    }

    /// Emit raw layout bytes (braces, spaces, indentation). These reset both
    /// "open" flags: the last thing on the line is now ordinary content.
    fn push(&mut self, bytes: &[u8]) {
        self.out.extend_from_slice(bytes);
        self.comment_open = false;
        self.quote_open = false;
    }

    fn push_byte(&mut self, b: u8) {
        self.out.push(b);
        self.comment_open = false;
        self.quote_open = false;
    }

    /// Emit a token verbatim, tracking whether it leaves a quote open.
    fn emit_text(&mut self, text: &[u8]) {
        self.push(text);
        self.quote_open = !quote_is_closed(text);
    }

    /// Emit a comment verbatim, marking the line as comment-closed.
    fn emit_comment(&mut self, text: &[u8]) {
        self.push(text);
        self.comment_open = true;
    }

    /// Emit a node verbatim. Then set the "open" flags from the end of the
    /// node: an unclosed quote, or a comment in the trailing gap of a node that
    /// missing tokens close at EOF.
    fn emit_node_text(&mut self, node: SyntaxNode<'_, '_>) {
        self.push(node.text());
        let tree = node.tree;
        let data = node.data();
        let tokens = &tree.data.tokens[data.first_token as usize..data.token_end as usize];
        let Some(last) = tokens.iter().rev().find(|t| !t.kind.is_missing()) else {
            return;
        };
        let mut gap = Gap {
            start: last.end(),
            end: node.text_range().end,
            next: 0,
        };
        let mut last_trivia = None;
        while let Some(token) = gap.next_token(tree) {
            last_trivia = Some(token.kind());
        }
        match last_trivia {
            Some(kind) => self.comment_open = kind == SyntaxKind::Comment,
            None => {
                self.quote_open = last.kind == SyntaxKind::Quoted
                    && !quote_is_closed(&tree.source[last.range().to_range()]);
            }
        }
    }

    /// Emit a newline (two for a preserved blank line), reopening the line.
    fn line_break(&mut self, blank: bool) {
        // An open quoted string can end with an escaped `\n`. That byte is
        // part of the string, so it does not end the line.
        if self.quote_open || self.out.last() != Some(&b'\n') {
            self.out.push(b'\n');
        }
        if blank && !self.out.ends_with(b"\n\n") {
            self.out.push(b'\n');
        }
        self.comment_open = false;
        self.quote_open = false;
    }

    fn write_indent(&mut self, level: usize) {
        for _ in 0..level {
            self.out.extend_from_slice(self.opts.indent.as_bytes());
        }
    }

    fn fmt_root(&mut self, root: SyntaxNode<'_, '_>) {
        let children: Vec<SyntaxElement<'_, '_>> = root.children().collect();
        // A leading BOM is reproduced verbatim at the very top of the file.
        let start = match children.first() {
            Some(el) if el.kind() == SyntaxKind::Bom => {
                self.push(el.text());
                1
            }
            _ => {
                // The formatter removes leading whitespace. If the first token
                // starts with the bytes of a BOM, the lexer would then read
                // them as a BOM. Keep one space before the token.
                let first = root.tree.significant_tokens().next();
                if first.is_some_and(|t| t.text().starts_with(BOM)) {
                    self.push_byte(b' ');
                }
                0
            }
        };
        self.fmt_items(&children[start..], 0, true);
    }

    /// Format a run of container children (significant items interleaved with
    /// trivia), one significant item per line at `indent`. `suppress_first`
    /// omits the break before the first item — true for the document root
    /// (no leading blank line), false for a block (the break follows its `{`).
    fn fmt_items(&mut self, items: &[SyntaxElement<'_, '_>], indent: usize, suppress_first: bool) {
        let mut first = true;
        let mut pending_blank = false;
        let mut ws_had_newline = true; // a fresh container behaves like a new line

        for &el in items {
            match el.kind() {
                kind if kind.is_missing() => {}
                SyntaxKind::Whitespace => {
                    let nls = count_newlines(el.text());
                    ws_had_newline = nls >= 1;
                    if nls >= 2 && !first {
                        pending_blank = true;
                    }
                }
                // A stray BOM mid-stream cannot occur (the lexer only emits it
                // first), but reproduce it verbatim if it ever does.
                SyntaxKind::Bom => self.push(el.text()),
                SyntaxKind::Comment => {
                    // Keep it trailing only if the previous item is on this very
                    // line and that line is not already closed by a comment.
                    let trailing =
                        !first && !ws_had_newline && !self.comment_open && !self.quote_open;
                    if trailing {
                        self.push_byte(b' ');
                    } else {
                        if !(first && suppress_first) {
                            self.line_break(pending_blank);
                        }
                        self.write_indent(indent);
                    }
                    self.emit_comment(el.text());
                    pending_blank = false;
                    ws_had_newline = true;
                    first = false;
                }
                _ => {
                    if !(first && suppress_first) {
                        self.line_break(pending_blank);
                    }
                    self.write_indent(indent);
                    self.hoist_inline_comments(el, indent);
                    self.fmt_item(el, indent);
                    pending_blank = false;
                    ws_had_newline = false;
                    first = false;
                }
            }
        }
    }

    /// Emit any inline comments of a field/headered-block item as their own
    /// leading lines (see [`collect_inline_comments`]); a no-op otherwise. The
    /// caller has already written this line's indent.
    fn hoist_inline_comments(&mut self, el: SyntaxElement<'_, '_>, indent: usize) {
        let node = match el {
            SyntaxElement::Node(n)
                if matches!(n.kind(), SyntaxKind::Field | SyntaxKind::HeaderedBlock) =>
            {
                n
            }
            _ => return,
        };
        let mut leading = Vec::new();
        collect_inline_comments(node, &mut leading);
        for c in leading {
            self.emit_comment(c.text());
            self.line_break(false);
            self.write_indent(indent);
        }
    }

    /// Format one significant element (already positioned at the line start).
    fn fmt_item(&mut self, el: SyntaxElement<'_, '_>, indent: usize) {
        match el {
            SyntaxElement::Node(n) => match n.kind() {
                SyntaxKind::Field | SyntaxKind::HeaderedBlock => self.fmt_spaced(n, indent),
                SyntaxKind::Block => self.fmt_block(n, indent),
                // A calc is a self-contained expression and a Bogus node wraps
                // unstructured bytes — reprint either verbatim.
                _ => self.emit_node_text(n),
            },
            // A loose scalar / operator / bracket / bang token: verbatim.
            SyntaxElement::Token(t) => self.emit_text(t.text()),
        }
    }

    /// Format a [`SyntaxKind::Field`] or [`SyntaxKind::HeaderedBlock`]: their
    /// significant children joined by single spaces (`key = value`,
    /// `rgb { … }`). Interior comments are *not* emitted here — the caller has
    /// already hoisted them ahead of the line via [`Fmt::hoist_inline_comments`]
    /// — so this only lays out significant tokens.
    fn fmt_spaced(&mut self, node: SyntaxNode<'_, '_>, indent: usize) {
        let mut first = true;
        for el in node.children() {
            match el.kind() {
                kind if kind.is_missing() => {}
                SyntaxKind::Whitespace | SyntaxKind::Bom | SyntaxKind::Comment => {}
                _ => {
                    if !first {
                        self.push_byte(b' ');
                    }
                    first = false;
                    self.fmt_item(el, indent);
                }
            }
        }
    }

    /// Format a [`SyntaxKind::Block`]. Inline when it holds only bare scalars;
    /// `{}` when empty; multiline otherwise. The closing `}` is emitted only if
    /// the block actually has one (an unclosed block keeps its token count).
    fn fmt_block(&mut self, block: SyntaxNode<'_, '_>, indent: usize) {
        self.push_byte(b'{');

        let children: Vec<SyntaxElement<'_, '_>> = block.children().collect();
        let has_close = matches!(children.last(), Some(el) if el.kind() == SyntaxKind::CloseBrace);
        // children[0] is always the opening `{`; drop it and the closing `}`.
        let inner_end = if has_close {
            children.len() - 1
        } else {
            children.len()
        };
        let inner = &children[1..inner_end];

        let has_comment = inner.iter().any(|el| el.kind() == SyntaxKind::Comment);
        let sig: Vec<SyntaxElement<'_, '_>> = inner
            .iter()
            .copied()
            .filter(|el| !el.kind().is_trivia() && !el.kind().is_missing())
            .collect();

        if sig.is_empty() && !has_comment {
            if has_close {
                self.push_byte(b'}');
            }
            return;
        }

        if has_close && !has_comment && sig.iter().all(|el| is_inline_scalar(*el)) {
            self.push_byte(b' ');
            for (i, el) in sig.iter().enumerate() {
                if i > 0 {
                    self.push_byte(b' ');
                }
                self.emit_text(el.text());
            }
            self.push(b" }");
            return;
        }

        self.fmt_items(inner, indent + 1, false);
        if has_close {
            self.line_break(false);
            self.write_indent(indent);
            self.push_byte(b'}');
        }
    }
}
