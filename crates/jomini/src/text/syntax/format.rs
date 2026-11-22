//! The formatter. It writes a tree again with normalized whitespace.

use std::io;

use super::cursor::Gap;
use super::lexer::BOM;
use super::parser::MAX_DEPTH;
use super::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken, SyntaxTree, TextSize, parse};

/// Configuration for [`SyntaxTree::format`].
///
/// ```
/// use jomini::text::syntax::{parse, FormatOptions};
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

/// Parse `source` with the syntax of every game except HoI4 and reformat it
/// with the default [`FormatOptions`]. See [`SyntaxTree::format`] for the
/// style. For a different flavor, such as the HoI4 flavor, use
/// [`parse_with`](super::parse_with) and [`SyntaxTree::format`].
///
/// ```
/// use jomini::text::syntax::format;
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
    ///
    /// To write a large file, use [`format_to_writer`](Self::format_to_writer),
    /// which does not keep the whole output in memory.
    pub fn format(&self, opts: &FormatOptions) -> Vec<u8> {
        let mut f = Fmt::new(opts, None);
        f.out.reserve(self.source.len() + self.source.len() / 8);
        f.fmt_root(self.root());
        f.finish()
    }

    /// Reformat the tree with the given [`FormatOptions`] and write the
    /// output to `writer`. The output is the same as the output of
    /// [`format`](Self::format).
    ///
    /// The formatter writes the output in chunks of about 64 KiB. Its memory
    /// grows with the longest entry, not with the size of the output: it
    /// buffers whole an inline block, a node that it writes with its source
    /// bytes, and a block deeper than the nesting limit. It does not flush
    /// `writer`.
    ///
    /// ```
    /// use jomini::text::syntax::{parse, FormatOptions};
    ///
    /// let mut out = Vec::new();
    /// parse(b"a={b=1}").format_to_writer(&FormatOptions::default(), &mut out)?;
    /// assert_eq!(out, b"a = {\n\tb = 1\n}\n");
    /// # Ok::<(), std::io::Error>(())
    /// ```
    ///
    /// # Errors
    ///
    /// Returns the first error of `writer`. The formatter stops at the error,
    /// so `writer` can have a part of the output.
    pub fn format_to_writer<W: io::Write>(
        &self,
        opts: &FormatOptions,
        mut writer: W,
    ) -> io::Result<()> {
        let mut f = Fmt::new(opts, Some(&mut writer));
        f.out.reserve(CHUNK + CHUNK / 2);
        f.fmt_root(self.root());
        f.finish_writer()
    }
}

/// The output size at which [`SyntaxTree::format_to_writer`] writes the
/// output that it has.
const CHUNK: usize = 64 * 1024;

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

/// Whether `el` is an operator that has no key and does not start a line,
/// as in `x={ a = 1 < }`. At the start of a line, the operator would start a
/// field with an empty key, so the formatter keeps it on the line of the item
/// before it.
fn is_stray_operator(el: SyntaxElement<'_, '_>) -> bool {
    matches!(el, SyntaxElement::Node(n)
        if n.kind() == SyntaxKind::Bogus
            && n.significant_children().next().is_some_and(|c| c.kind() == SyntaxKind::Operator))
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
    /// The output. With a writer, the formatter writes all but the last two
    /// bytes of each chunk and keeps those two bytes here, because it reads
    /// them to find the end of a line.
    out: Vec<u8>,
    /// The number of bytes that are written to the writer and removed from
    /// `out`.
    written: usize,
    writer: Option<&'o mut dyn io::Write>,
    /// The first error of the writer.
    error: Option<io::Error>,
    opts: &'o FormatOptions,
    /// True when the current output line ends in a comment, so the next thing
    /// emitted must start on a new line (a comment runs to end of line).
    comment_open: bool,
    /// True when the last token emitted is an unterminated quoted string, which
    /// would absorb a following newline; suppresses the trailing newline.
    quote_open: bool,
}

impl<'o> Fmt<'o> {
    fn new(opts: &'o FormatOptions, writer: Option<&'o mut dyn io::Write>) -> Self {
        Fmt {
            out: Vec::new(),
            written: 0,
            writer,
            error: None,
            opts,
            comment_open: false,
            quote_open: false,
        }
    }

    fn finish(mut self) -> Vec<u8> {
        self.end_document();
        self.out
    }

    fn finish_writer(mut self) -> io::Result<()> {
        if let Some(error) = self.error {
            return Err(error);
        }
        self.end_document();
        self.write_out(self.out.len());
        self.error.map_or(Ok(()), Err)
    }

    /// Write exactly one trailing newline when there is content, unless the
    /// last token is a quoted string that continues to the end of the source.
    fn end_document(&mut self) {
        let empty = self.written == 0 && self.out.is_empty();
        if !empty && self.out.last() != Some(&b'\n') && !self.quote_open {
            self.out.push(b'\n');
        }
    }

    /// With a writer, write the output when it is at least [`CHUNK`] bytes.
    /// Return `false` after an error of the writer, so that the caller stops
    /// the walk of the tree.
    #[inline]
    fn write_chunk(&mut self) -> bool {
        if self.error.is_some() {
            return false;
        }
        if self.writer.is_some() && self.out.len() >= CHUNK {
            self.write_out(self.out.len() - 2);
        }
        self.error.is_none()
    }

    /// Write the first `len` bytes of `out` to the writer and remove them.
    #[cold]
    fn write_out(&mut self, len: usize) {
        let Some(writer) = self.writer.as_mut() else {
            return;
        };
        match writer.write_all(&self.out[..len]) {
            Ok(()) => {
                self.out.drain(..len);
                self.written += len;
            }
            Err(e) => self.error = Some(e),
        }
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
        let Some(last) = tokens.iter().rev().find(|t| !t.kind.is_synthetic()) else {
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
        // A leading BOM is reproduced verbatim at the very top of the file.
        let source = root.tree.source;
        if source.starts_with(BOM) {
            self.push(BOM);
        } else {
            // The formatter removes leading whitespace. If the first token
            // starts with the bytes of a BOM, the lexer would then read them
            // as a BOM. Keep one space before the token.
            let first = root.tree.significant_tokens().next();
            if first.is_some_and(|t| t.text().starts_with(BOM)) {
                self.push_byte(b' ');
            }
        }
        self.fmt_items(root, 0, true);
    }

    /// Format the items of the root or of a block, one significant item per
    /// line at `indent`. `suppress_first` omits the break before the first
    /// item — true for the document root (no leading blank line), false for a
    /// block (the break follows its `{`). The `{` and the `}` of a block are
    /// not items.
    fn fmt_items(&mut self, container: SyntaxNode<'_, '_>, indent: usize, suppress_first: bool) {
        let is_block = container.kind() == SyntaxKind::Block;
        let is_item =
            |el: &SyntaxElement<'_, '_>| !(is_block && el.kind() == SyntaxKind::CloseBrace);
        if container.has_comment() {
            let items = container
                .children()
                .skip(usize::from(is_block))
                .filter(is_item);
            return self.fmt_items_with_trivia(items, indent, suppress_first);
        }

        // Without comments, the gap between two items is whitespace, so the
        // formatter reads the blank lines from the source and does not lex
        // the gaps.
        let source = container.tree.source;
        let mut first = true;
        let mut prev_end = None;
        let items = container.significant_children().skip(usize::from(is_block));
        for el in items.filter(is_item) {
            if el.kind().is_synthetic() {
                continue;
            }
            if !self.write_chunk() {
                return;
            }
            let range = el.text_range();
            let blank = prev_end.is_some_and(|end: TextSize| {
                range.start() - end >= TextSize::from(2)
                    && count_newlines(&source[end.to_usize()..range.start().to_usize()]) >= 2
            });
            self.fmt_entry(el, indent, first && suppress_first, blank);
            first = false;
            prev_end = Some(range.end());
        }
    }

    /// Format items interleaved with whitespace and comments.
    fn fmt_items_with_trivia<'t, 'a>(
        &mut self,
        items: impl Iterator<Item = SyntaxElement<'t, 'a>>,
        indent: usize,
        suppress_first: bool,
    ) {
        let mut first = true;
        let mut pending_blank = false;
        let mut ws_had_newline = true; // a fresh container behaves like a new line

        for el in items {
            if !self.write_chunk() {
                return;
            }
            match el.kind() {
                kind if kind.is_synthetic() => {}
                SyntaxKind::Whitespace => {
                    let nls = count_newlines(el.text());
                    ws_had_newline = nls >= 1;
                    if nls >= 2 && !first {
                        pending_blank = true;
                    }
                }
                // The BOM is only at the start of the source, where
                // `fmt_root` writes it.
                SyntaxKind::Bom => {}
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
                    self.fmt_entry(el, indent, first && suppress_first, pending_blank);
                    pending_blank = false;
                    ws_had_newline = false;
                    first = false;
                }
            }
        }
    }

    /// Format one significant item of a container on its own line, after a
    /// blank line when `blank` is true. A stray operator stays on the line
    /// before it. `at_start` is true for the first item of the document,
    /// which has no line break before it.
    fn fmt_entry(&mut self, el: SyntaxElement<'_, '_>, indent: usize, at_start: bool, blank: bool) {
        if !at_start && !self.comment_open && is_stray_operator(el) {
            self.push_byte(b' ');
            self.fmt_item(el, indent);
            return;
        }
        if !at_start {
            self.line_break(blank);
        }
        self.write_indent(indent);
        self.hoist_inline_comments(el, indent);
        self.fmt_item(el, indent);
    }

    /// Emit any inline comments of a field/headered-block item as their own
    /// leading lines (see [`collect_inline_comments`]); a no-op otherwise. The
    /// caller has already written this line's indent.
    fn hoist_inline_comments(&mut self, el: SyntaxElement<'_, '_>, indent: usize) {
        let node = match el {
            SyntaxElement::Node(n)
                if n.has_comment()
                    && matches!(n.kind(), SyntaxKind::Field | SyntaxKind::HeaderedBlock) =>
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
        for el in node.significant_children() {
            if el.kind().is_synthetic() {
                continue;
            }
            if !first {
                self.push_byte(b' ');
            }
            first = false;
            self.fmt_item(el, indent);
        }
    }

    /// Format a [`SyntaxKind::Block`]. Inline when it holds only bare scalars;
    /// `{}` when empty; multiline otherwise. The closing `}` is emitted only if
    /// the block actually has one (an unclosed block keeps its token count).
    ///
    /// The formatter recurses for each level of blocks. A block deeper than
    /// [`MAX_DEPTH`] keeps its source text, so that the recursion has a limit.
    fn fmt_block(&mut self, block: SyntaxNode<'_, '_>, indent: usize) {
        if indent >= MAX_DEPTH as usize {
            self.emit_node_text(block);
            return;
        }
        self.push_byte(b'{');

        // The first child is the opening `{`. A `}` child is the closing `}`,
        // which a block that ends at the end of the source does not have.
        // The other children are the items.
        let mut empty = true;
        let mut inline = true;
        let mut has_close = false;
        for el in block.significant_children().skip(1) {
            match el.kind() {
                SyntaxKind::CloseBrace => has_close = true,
                kind if kind.is_synthetic() => {}
                _ => {
                    empty = false;
                    inline &= is_inline_scalar(el);
                }
            }
        }

        // The block has only scalar tokens when it is empty or inline, so a
        // comment in its subtree is one of its items.
        let has_comment = block.has_comment();
        if empty && !has_comment {
            if has_close {
                self.push_byte(b'}');
            }
            return;
        }

        if has_close && inline && !has_comment {
            for el in block.significant_children().skip(1) {
                if el.kind() != SyntaxKind::CloseBrace && !el.kind().is_synthetic() {
                    self.push_byte(b' ');
                    self.emit_text(el.text());
                }
            }
            self.push(b" }");
            return;
        }

        self.fmt_items(block, indent + 1, false);
        if has_close {
            self.line_break(false);
            self.write_indent(indent);
            self.push_byte(b'}');
        }
    }
}
