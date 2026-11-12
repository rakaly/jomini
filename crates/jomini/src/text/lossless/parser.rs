//! The recursive-descent parser. It sends its events (start node, token,
//! finish node) to a [`Sink`]. One sink builds the tree, and the sink in
//! `tape` writes a `TextTape`, so the grammar is independent of the storage.
//!
//! `parse_tape` first runs `tape/fast.rs`, a second, hand-written reader for
//! ordinary fields and blocks. The fast path stops at a bracket construct or a
//! recovery case. This parser continues from the start of the root item that
//! contains that construct or case. A change to the grammar must keep the two
//! in agreement.

use super::calc::{CalcElem, parse_calc_interior};
use super::error::closing_text;
use super::lexer::{Lexer, TOKEN_COMMENT_BEFORE, TOKEN_ERROR, Token};
use super::tree::{Node, own_flag_bits};
use super::{
    DirectiveKind, MissingDelimiter, NodeFlags, SyntaxError, SyntaxErrorKind, SyntaxKind,
    TextRange, TextSize, UnclosedConstruct,
};

/// Receives the parser's events in source order. The grammar knows only this
/// interface, so a different storage can reuse the same grammar.
pub(super) trait Sink {
    /// The sink needs every token and every diagnostic after the parse. When
    /// this is `false`, the parser keeps only a small window of tokens and
    /// does not record diagnostics.
    const KEEP_TOKENS: bool;

    /// Open a node. Its first token is the next token.
    fn start_node(&mut self, kind: SyntaxKind);
    /// Add the next token to the open node.
    fn token(&mut self, token: Token);
    /// Close the most recently opened node.
    fn finish_node(&mut self);
    /// Record that the open node contains a diagnostic. The parser also calls
    /// it when [`Sink::KEEP_TOKENS`] is `false`.
    fn error(&mut self);

    /// Add a field node whose key, operator, and value are single tokens. A
    /// sink can do this faster than the four separate events.
    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        self.start_node(SyntaxKind::Field);
        self.token(key);
        self.token(op);
        self.token(value);
        self.finish_node();
    }

    /// Add a `{` or a `}` token of a block. A sink that does not use the
    /// braces can do this faster than [`Sink::token`].
    #[inline]
    fn brace(&mut self, token: Token) {
        self.token(token);
    }

    /// Open a field node and add its key and operator. The value follows.
    #[inline]
    fn field_head(&mut self, key: Token, op: Token) {
        self.start_node(SyntaxKind::Field);
        self.token(key);
        self.token(op);
    }
}

/// Build the node tape and record the positions of missing tokens.
pub(super) struct Builder {
    pub(super) nodes: Vec<Node>,
    /// The open nodes. Each entry holds the index of a node and the flags
    /// of its parent. The flags of the innermost open node are in `flags`,
    /// because each token changes them.
    pub(super) stack: Vec<(u32, NodeFlags)>,
    /// The flags collected for the innermost open node.
    flags: NodeFlags,
    /// The index of the next token.
    next_token: u32,
    missing_tokens: Vec<(u32, Token)>,
}

impl Builder {
    pub(super) fn with_capacity(tokens: usize) -> Self {
        // Nodes are a fraction of tokens: approximately one per field.
        Builder {
            nodes: Vec::with_capacity(tokens / 2 + 1),
            stack: Vec::new(),
            flags: NodeFlags(0),
            next_token: 0,
            missing_tokens: Vec::new(),
        }
    }

    /// Record the diagnostic and the comment of a token.
    #[cold]
    #[inline(never)]
    fn token_flags(&mut self, token: Token, index: u32) {
        if token.flags & TOKEN_ERROR != 0 {
            self.flags.insert(NodeFlags::HAS_ERROR);
        }
        if token.flags & TOKEN_COMMENT_BEFORE != 0 {
            // The comment belongs to the deepest open node that also holds
            // the previous token. The root holds a leading comment.
            let nodes = &self.nodes;
            let owner = self
                .stack
                .iter()
                .rposition(|(node, _)| nodes[*node as usize].first_token < index)
                .unwrap_or(0);
            if !self.stack.is_empty() {
                self.open_flags(owner).insert(NodeFlags::HAS_COMMENT);
            }
        }
    }

    /// The flags of the open node at `depth` in the stack.
    fn open_flags(&mut self, depth: usize) -> &mut NodeFlags {
        match self.stack.get_mut(depth + 1) {
            Some((_, parent)) => parent,
            None => &mut self.flags,
        }
    }

    pub(super) fn finish(
        mut self,
        trailing_comment: bool,
        tokens: &mut Vec<Token>,
        lexed: usize,
    ) -> Vec<Node> {
        if !self.missing_tokens.is_empty() {
            let mut ordered = Vec::with_capacity(tokens.len());
            let mut significant = tokens[..lexed].iter().copied().peekable();
            for (index, mut token) in self.missing_tokens {
                while ordered.len() < index as usize {
                    ordered.push(significant.next().expect("missing significant token"));
                }
                if let Some(next) = significant.peek() {
                    token.start = next.start;
                }
                ordered.push(token);
            }
            ordered.extend(significant);
            *tokens = ordered;
        }
        debug_assert!(self.stack.is_empty(), "unfinished nodes remain");
        if trailing_comment {
            self.nodes[0].flags.insert(NodeFlags::HAS_COMMENT);
        }
        self.nodes
    }
}

impl Sink for Builder {
    const KEEP_TOKENS: bool = true;

    #[inline]
    fn scalar_field(&mut self, key: Token, op: Token, value: Token) {
        if (key.flags | op.flags | value.flags) & TOKEN_COMMENT_BEFORE != 0 {
            self.start_node(SyntaxKind::Field);
            self.token(key);
            self.token(op);
            self.token(value);
            self.finish_node();
            return;
        }
        let mut flags = own_flag_bits(key.kind);
        flags.insert(own_flag_bits(value.kind));
        if (key.flags | op.flags | value.flags) & TOKEN_ERROR != 0 {
            flags.insert(NodeFlags::HAS_ERROR);
        }
        let first_token = self.next_token;
        self.next_token += 3;
        let subtree_end = self.nodes.len() as u32 + 1;
        self.nodes.push(Node {
            kind: SyntaxKind::Field,
            flags,
            first_token,
            token_end: self.next_token,
            subtree_end,
        });
        self.flags.insert(flags);
    }

    #[inline]
    fn start_node(&mut self, kind: SyntaxKind) {
        let idx = self.nodes.len() as u32;
        self.nodes.push(Node {
            kind,
            flags: NodeFlags(0),
            first_token: self.next_token,
            token_end: 0,
            subtree_end: 0,
        });
        self.stack.push((idx, self.flags));
        self.flags = own_flag_bits(kind);
    }

    #[inline]
    fn token(&mut self, token: Token) {
        let index = self.next_token;
        self.next_token += 1;
        if token.kind.is_missing() {
            self.missing_tokens.push((index, token));
        }
        self.flags.insert(own_flag_bits(token.kind));
        if token.flags & (TOKEN_ERROR | TOKEN_COMMENT_BEFORE) != 0 {
            self.token_flags(token, index);
        }
    }

    #[inline]
    fn finish_node(&mut self) {
        let (idx, parent) = self.stack.pop().expect("finish_node without start_node");
        let flags = self.flags;
        let subtree_end = self.nodes.len() as u32;
        let node = &mut self.nodes[idx as usize];
        node.flags = flags;
        node.token_end = self.next_token;
        node.subtree_end = subtree_end;
        self.flags = parent;
        self.flags.insert(flags);
    }

    fn error(&mut self) {
        if !self.stack.is_empty() {
            self.flags.insert(NodeFlags::HAS_ERROR);
        }
    }
}

/// The number of levels that each kind of construct can nest: blocks, EU4
/// parameter blocks, EU5 code payloads, CK3 reader directives, and the
/// expressions of a calculation. At the next level, the parser records an
/// error and reads the rest of the construct with no recursion. This prevents
/// a stack overflow on input such as 100,000 `{`. Real game files nest much
/// less deep.
pub(super) const MAX_DEPTH: u32 = 256;

/// The number of tokens that the parser can always read after its position
/// with [`Parser::kind_at`]. The grammar looks at most five tokens ahead.
const LOOKAHEAD: usize = 8;

/// The number of tokens that the lexer adds to the window at a time. Tests
/// use a small value so that they exercise the window logic.
const CHUNK: usize = if cfg!(test) { 3 } else { 1024 };

/// The number of tokens before the parser position that stay in the window.
/// The grammar looks at most two tokens back.
const LOOKBEHIND: usize = 4;

pub(super) struct Parser<'t, S: Sink> {
    source: &'t [u8],
    pub(super) lexer: Lexer,
    /// A window of the lexed tokens: `tokens[0]` has the index `base`. When
    /// the sink keeps tokens, the window holds all the tokens and `base` is
    /// zero.
    pub(super) tokens: Vec<Token>,
    pub(super) base: usize,
    /// The number of lexed tokens. When the lexer is done, a position at or
    /// after it is EOF.
    pub(super) lexed: usize,
    /// The window keeps the tokens at and after this index.
    pin: usize,
    pos: usize,
    pub(super) sink: S,
    pub(super) errors: Vec<SyntaxError>,
    /// Current block-nesting depth, compared against [`MAX_DEPTH`].
    depth: u32,
    /// Current EU4 parameter nesting depth.
    parameter_depth: u32,
    /// Unclosed ordinary square brackets inside the current parameter body.
    bracket_depth: u32,
    /// Current EU5 code-payload nesting depth.
    code_depth: u32,
    /// Current CK3 reader-directive nesting depth: the value of a directive
    /// can be a directive.
    directive_depth: u32,
    /// Number of non-recoverable interruptions seen below this parser.
    recovery_barrier: u32,
    /// Number of missing delimiters inserted at EOF.
    eof_delimiters: u32,
    /// A missing delimiter was emitted at EOF.
    missing_at_eof: bool,
    /// The sorted indexes of the `[` tokens that start an interpolation,
    /// for the `[` tokens before `resolved_until`.
    interpolations: Vec<u32>,
    resolved_until: usize,
}

impl<'t, S: Sink> Parser<'t, S> {
    pub(super) fn new(source: &'t [u8], lexer: Lexer, sink: S) -> Self {
        // Save files average approximately five bytes per significant token.
        let capacity = if S::KEEP_TOKENS {
            source.len() / 5 + 16
        } else {
            2 * CHUNK + LOOKAHEAD
        };
        let mut parser = Parser {
            source,
            lexer,
            tokens: Vec::with_capacity(capacity),
            base: 0,
            lexed: 0,
            pin: usize::MAX,
            pos: 0,
            sink,
            errors: Vec::new(),
            depth: 0,
            parameter_depth: 0,
            bracket_depth: 0,
            code_depth: 0,
            directive_depth: 0,
            recovery_barrier: 0,
            eof_delimiters: 0,
            missing_at_eof: false,
            interpolations: Vec::new(),
            resolved_until: 0,
        };
        parser.refill();
        parser
    }

    /// Lex more tokens into the window. When the sink does not keep tokens,
    /// first remove the tokens that the parser cannot read again.
    #[inline(never)]
    fn fill(&mut self) {
        if !S::KEEP_TOKENS {
            let keep = self.pos.saturating_sub(LOOKBEHIND).min(self.pin);
            if keep >= self.base + CHUNK {
                self.tokens.drain(..keep - self.base);
                self.base = keep;
            }
        }
        self.lexer.fill(self.source, &mut self.tokens, CHUNK);
        self.lexed = self.base + self.tokens.len();
    }

    /// Lex until [`LOOKAHEAD`] tokens follow the position, or the source ends.
    #[inline]
    fn refill(&mut self) {
        while self.pos + LOOKAHEAD > self.lexed && !self.lexer.done {
            self.fill();
        }
    }

    /// Move the position `n` tokens forward.
    #[inline]
    fn advance(&mut self, n: usize) {
        self.pos += n;
        self.refill();
    }

    /// The token at `index`. It must be in the window.
    #[inline]
    fn tok(&self, index: usize) -> Token {
        self.tokens[index - self.base]
    }

    #[inline]
    fn tok_mut(&mut self, index: usize) -> &mut Token {
        &mut self.tokens[index - self.base]
    }

    /// Insert a zero-width `missing` token at EOF and record its error.
    fn missing(
        &mut self,
        open: usize,
        missing: SyntaxKind,
        construct: UnclosedConstruct,
        kind: SyntaxErrorKind,
    ) {
        self.push_missing(missing);
        if !S::KEEP_TOKENS {
            self.sink.error();
            return;
        }
        self.record_missing(open, missing, construct, kind);
    }

    /// Record the error of a construct that the end of the source leaves
    /// open. `open` is the index of the token that opens the construct.
    fn record_missing(
        &mut self,
        open: usize,
        missing: SyntaxKind,
        construct: UnclosedConstruct,
        kind: SyntaxErrorKind,
    ) {
        let at = self.source.len() as u32;
        let order = self.eof_delimiters;
        self.eof_delimiters += 1;
        // A delimiter appended to an unterminated line comment remains comment
        // text. Start a new line and preserve the file's established ending.
        let line_break = if order == 0
            && self.lexer.trailing_comment()
            && !self.source.ends_with(b"\n")
            && !self.source.ends_with(b"\r")
        {
            if self.source.windows(2).any(|w| w == b"\r\n") {
                "\r\n"
            } else {
                "\n"
            }
        } else {
            ""
        };
        // The lexer is done at EOF, so it already knows if the last token is
        // a quoted string without a closing quote. Do not scan it again for
        // each missing delimiter.
        let unsafe_quote = self.lexer.unclosed_quote.is_some();
        self.errors.push(SyntaxError {
            kind,
            range: TextRange::empty(TextSize(at)),
            missing_delimiter: Some(MissingDelimiter {
                construct,
                opening_range: self.tok(open).range(),
                missing,
                order,
                text: closing_text(missing, line_break),
                safe: !unsafe_quote,
            }),
        });
    }

    /// Send a zero-width token at EOF to the sink. The window does not hold
    /// it: the sink records where it goes.
    fn push_missing(&mut self, kind: SyntaxKind) {
        // A comment at EOF comes before the first missing delimiter. Mark it
        // there, so that the nodes that own the comment get its flag.
        let flags = if !self.missing_at_eof && self.lexer.trailing_comment() {
            TOKEN_COMMENT_BEFORE
        } else {
            0
        };
        self.missing_at_eof = true;
        let token = Token {
            start: self.source.len() as u32,
            len: 0,
            kind,
            flags,
        };
        self.sink.token(token);
    }

    /// Record a diagnostic that starts at the emitted token `index`. Call it
    /// while the nodes that hold the token are still open.
    fn error_at(&mut self, index: usize, kind: SyntaxErrorKind) {
        // A sink that keeps no tokens still learns that the input has an
        // error, so that it can report the error after the parse.
        self.sink.error();
        if !S::KEEP_TOKENS {
            return;
        }
        self.tok_mut(index).flags |= TOKEN_ERROR;
        self.errors.push(SyntaxError {
            kind,
            range: self.tok(index).range(),
            missing_delimiter: None,
        });
    }

    #[inline]
    fn kind_at(&self, index: usize) -> Option<SyntaxKind> {
        debug_assert!(index >= self.base, "lookbehind before the token window");
        let token = self.tokens.get(index.wrapping_sub(self.base));
        debug_assert!(
            token.is_some() || self.lexer.done,
            "lookahead past the token window"
        );
        token.map(|token| token.kind)
    }

    /// Return the kind at `index` like [`Parser::kind_at`], but lex as far as
    /// necessary.
    fn kind_far(&mut self, index: usize) -> Option<SyntaxKind> {
        while index >= self.lexed && !self.lexer.done {
            self.fill();
        }
        self.kind_at(index)
    }

    #[inline]
    fn peek(&self) -> Option<SyntaxKind> {
        self.kind_at(self.pos)
    }

    #[inline]
    fn bump(&mut self) {
        let t = self.tok(self.pos);
        self.sink.token(t);
        self.advance(1);
    }

    /// Add the brace at the position to the sink, and move to the next token.
    #[inline]
    fn bump_brace(&mut self) {
        let t = self.tok(self.pos);
        self.sink.brace(t);
        self.advance(1);
    }

    /// Return `true` when the field at the current position has a scalar
    /// value that is a single token. The key and the operator are known.
    /// A value that a brace or a bracket follows can be a header or a code
    /// payload, and a scalar that an operator follows is the next key, so
    /// these go on the general path.
    #[inline]
    fn is_scalar_field(&self) -> bool {
        self.kind_at(self.pos + 2)
            .is_some_and(SyntaxKind::is_scalar)
            && !matches!(
                self.kind_at(self.pos + 3),
                Some(SyntaxKind::OpenBrace | SyntaxKind::OpenBracket | SyntaxKind::Operator)
            )
    }

    /// Return `true` when the `]` at the current position is the key of a
    /// field, as in `active_idea_groups = { ]=0 }`.
    fn is_bracket_key(&self) -> bool {
        self.parameter_depth == 0
            && self.code_depth == 0
            && self.kind_at(self.pos + 1) == Some(SyntaxKind::Operator)
    }

    fn is_code_close(&self, index: usize) -> bool {
        self.kind_at(index) == Some(SyntaxKind::CloseBracket)
            && self.kind_at(index + 1) == Some(SyntaxKind::CloseBracket)
    }

    pub(super) fn parse_items(&mut self, in_block: bool) {
        loop {
            match self.peek() {
                None => break,
                Some(SyntaxKind::CloseBracket)
                    if self.parameter_depth > 0 && self.bracket_depth == 0 =>
                {
                    break;
                }
                Some(SyntaxKind::CloseBracket) if self.is_bracket_key() => {
                    // EU4 saves contain `]=0` as a field. The `]` is a scalar.
                    self.tok_mut(self.pos).kind = SyntaxKind::Unquoted;
                    self.parse_item();
                }
                Some(SyntaxKind::CloseBracket)
                    if self.code_depth > 0 && self.is_code_close(self.pos) =>
                {
                    break;
                }
                Some(SyntaxKind::CloseBracket) if self.parameter_depth == 0 => {
                    // A stray `]`, as in `{ 0.5 0.9] }`. Keep it in a bogus
                    // node and continue, so that the enclosing blocks keep
                    // their structure. The `]` makes the intent of the author
                    // unclear, thus refuse a virtual `}` at EOF.
                    let index = self.pos;
                    self.recovery_barrier += 1;
                    self.sink.start_node(SyntaxKind::Bogus);
                    self.bump();
                    self.error_at(index, SyntaxErrorKind::UnmatchedCloseBracket);
                    self.sink.finish_node();
                }
                Some(SyntaxKind::CloseBrace)
                    if in_block && self.parameter_depth > 0 && self.bracket_depth > 0 =>
                {
                    self.parse_item()
                }
                Some(SyntaxKind::CloseBrace) if in_block => break, // caller eats `}`
                Some(SyntaxKind::CloseBrace) => {
                    // Unmatched `}` at the top level: wrap in Bogus and record.
                    let index = self.pos;
                    self.sink.start_node(SyntaxKind::Bogus);
                    self.bump();
                    self.error_at(index, SyntaxErrorKind::UnmatchedCloseBrace);
                    self.sink.finish_node();
                }
                Some(_) => self.parse_item(),
            }
        }
    }

    // Without `always`, the `text/lossless` benchmark is 7% slower on EU4 and
    // 1% slower on CK3.
    #[inline(always)]
    fn parse_item(&mut self) {
        match self.peek() {
            Some(k) if k.is_scalar() => {
                if self.is_code_statement() {
                    self.parse_code_statement();
                    return;
                }
                match self.kind_at(self.pos + 1) {
                    Some(SyntaxKind::Operator) if self.is_scalar_field() => {
                        let (key, op, value) = (
                            self.tok(self.pos),
                            self.tok(self.pos + 1),
                            self.tok(self.pos + 2),
                        );
                        self.sink.scalar_field(key, op, value);
                        self.advance(3);
                    }
                    Some(SyntaxKind::Operator) => {
                        // key <op> value
                        let (key, op) = (self.tok(self.pos), self.tok(self.pos + 1));
                        self.sink.field_head(key, op);
                        self.advance(2);
                        self.parse_value();
                        self.sink.finish_node();
                    }
                    Some(SyntaxKind::OpenBrace) => {
                        // key { ... }: a field with no operator. The semantic
                        // layer decides if the scalar is a key or a header.
                        self.sink.start_node(SyntaxKind::Field);
                        self.bump(); // key
                        self.parse_block();
                        self.sink.finish_node();
                    }
                    // bare scalar (array element / loose value)
                    _ => self.bump(),
                }
            }
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            Some(SyntaxKind::Directive) => self.parse_directive(),
            Some(SyntaxKind::OpenBracket) => self.parse_open_bracket(),
            Some(SyntaxKind::CloseBracket) if self.parameter_depth > 0 => {
                self.bump();
                self.bracket_depth = self.bracket_depth.saturating_sub(1);
            }
            // An operator after a block makes the block a key, as in
            // `{ a = b }={ 1 2 3 }`. Any other operator has no key.
            Some(SyntaxKind::Operator)
                if self.pos == 0 || self.kind_at(self.pos - 1) != Some(SyntaxKind::CloseBrace) =>
            {
                let index = self.pos;
                self.sink.start_node(SyntaxKind::Bogus);
                self.bump();
                self.error_at(index, SyntaxErrorKind::UnexpectedOperator);
                self.sink.finish_node();
            }
            // Operators after a block, bang, and macros in item position: keep
            // verbatim.
            Some(_) => self.bump(),
            None => {}
        }
    }

    fn parse_value(&mut self) {
        match self.peek() {
            Some(SyntaxKind::OpenBrace) => self.parse_block(),
            Some(SyntaxKind::CalcOpen) => self.parse_calc(),
            Some(SyntaxKind::Directive) => self.parse_directive(),
            Some(SyntaxKind::OpenBracket) => self.parse_open_bracket(),
            Some(SyntaxKind::Unquoted) if self.is_code_statement() => self.parse_code_statement(),
            Some(SyntaxKind::Unquoted | SyntaxKind::MacroParam | SyntaxKind::Variable)
                if self.kind_at(self.pos + 1) == Some(SyntaxKind::OpenBrace) =>
            {
                // headered block: `rgb { ... }`, `hsv { ... }`, tag { ... }.
                // TextTape also accepts `$NAME$ { ... }` and `@name { ... }`.
                self.sink.start_node(SyntaxKind::HeaderedBlock);
                self.bump(); // header scalar
                self.parse_block();
                self.sink.finish_node();
            }
            // A `]` that closes a parameter body or a code payload is not a
            // value, as in `[[p] a = ]`.
            Some(SyntaxKind::CloseBracket)
                if (self.parameter_depth > 0 && self.bracket_depth == 0)
                    || (self.code_depth > 0 && self.is_code_close(self.pos)) =>
            {
                self.missing_value()
            }
            // A scalar that an operator follows is the key of the next field,
            // as in `b =` followed by `c = 2` on the next line. The item loop
            // reads that field.
            Some(k)
                if k.is_scalar() && self.kind_at(self.pos + 1) == Some(SyntaxKind::Operator) =>
            {
                self.missing_value()
            }
            // An operator after an operator is the value, as in `a = =`.
            Some(k) if k.is_scalar() || k == SyntaxKind::Operator => self.bump(),
            // `a =` at EOF, `a = }`, or a token that cannot be a value, such
            // as a stray `]` or a `!`. The item loop reads that token again.
            _ => self.missing_value(),
        }
    }

    /// Record that the operator before the current position has no value.
    /// The field gets no value node.
    #[cold]
    fn missing_value(&mut self) {
        self.error_at(self.pos - 1, SyntaxErrorKind::MissingValue);
    }

    /// Parse a CK3 reader directive. The directive name sets the arguments:
    /// a name or text for each directive except `@:assert`, and `= value` for
    /// `@:define`, `@:insert`, and `@:register_variable`. A directive that is
    /// not known has no arguments.
    ///
    /// At [`MAX_DEPTH`] nested directives, the directive gets no value, and
    /// the item loop reads the rest of the source with no recursion.
    fn parse_directive(&mut self) {
        let index = self.pos;
        let directive = self.tok(index);
        let form = DirectiveKind::from_token(&self.source[directive.range().to_range()]).form();
        self.sink.start_node(SyntaxKind::ReaderDirective);
        self.bump();
        if form.argument && self.peek().is_some_and(SyntaxKind::is_scalar) {
            self.bump();
            if form.value && self.peek() == Some(SyntaxKind::Operator) {
                self.bump();
                if self.directive_depth >= MAX_DEPTH {
                    self.recovery_barrier += 1;
                    self.error_at(index, SyntaxErrorKind::DirectiveDepthExceeded);
                } else {
                    self.directive_depth += 1;
                    self.parse_value();
                    self.directive_depth -= 1;
                }
            }
        }
        self.sink.finish_node();
    }

    /// Return the EU4 parameter node kind at the current token position.
    ///
    /// EU5 uses `code [[ ... ]]` for a different payload format. The EU4 form
    /// has a compact header with a name immediately before its first `]`.
    fn parameter_kind(&self) -> Option<SyntaxKind> {
        let open = self.pos;
        if self.code_depth > 0 {
            return None;
        }
        if self.kind_at(open)? != SyntaxKind::OpenBracket
            || self.kind_at(open + 1)? != SyntaxKind::OpenBracket
        {
            return None;
        }

        let mut name = open + 2;
        let kind = if self.kind_at(name)? == SyntaxKind::Bang {
            name += 1;
            SyntaxKind::UndefinedParameter
        } else {
            SyntaxKind::Parameter
        };

        if self.kind_at(name)? != SyntaxKind::Unquoted {
            return None;
        }

        // The header must close directly after the name, or the input must
        // end there (an incomplete header).
        let header_close = self.kind_at(name + 1);
        if header_close.is_some_and(|kind| kind != SyntaxKind::CloseBracket) {
            return None;
        }

        // Keep the EU5 `code [[...]]` payload out of the EU4 node grammar even
        // when a compact payload happens to resemble a parameter header.
        if self.preceded_by_code(open) {
            return None;
        }

        Some(kind)
    }

    fn code_payload_start(&self, open: usize) -> bool {
        self.kind_at(open) == Some(SyntaxKind::OpenBracket)
            && self.kind_at(open + 1) == Some(SyntaxKind::OpenBracket)
            && self.preceded_by_code(open)
    }

    #[inline]
    fn is_code_keyword(&self, index: usize) -> bool {
        self.kind_at(index) == Some(SyntaxKind::Unquoted)
            && self.tok(index).len == 4
            && self.token_text(index) == b"code"
    }

    #[inline]
    fn is_code_statement(&self) -> bool {
        self.is_code_keyword(self.pos) && self.code_payload_start(self.pos + 1)
    }

    fn code_statement_open(&self, index: usize) -> Option<usize> {
        if !self.is_code_keyword(index) {
            return None;
        }

        let mut next = index + 1;
        if self.kind_at(next) == Some(SyntaxKind::Operator) && self.token_text(next) == b"=" {
            next += 1;
        }
        self.code_payload_start(next).then_some(next)
    }

    fn token_text(&self, index: usize) -> &'t [u8] {
        &self.source[self.tok(index).range().to_range()]
    }

    fn preceded_by_code(&self, open: usize) -> bool {
        let Some(previous) = open.checked_sub(1) else {
            return false;
        };
        if self.is_code_keyword(previous) {
            return true;
        }
        if self.tok(previous).kind != SyntaxKind::Operator || self.token_text(previous) != b"=" {
            return false;
        }
        previous
            .checked_sub(1)
            .is_some_and(|code| self.is_code_keyword(code))
    }

    fn parse_open_bracket(&mut self) {
        if self.code_payload_start(self.pos) {
            self.parse_code_payload();
        } else if let Some(kind) = self.parameter_kind() {
            if self.parameter_depth >= MAX_DEPTH {
                let index = self.pos;
                self.recovery_barrier += 1;
                self.bump();
                self.error_at(index, SyntaxErrorKind::ParameterDepthExceeded);
                self.bracket_depth += 1;
            } else {
                self.parse_parameter(kind);
            }
        } else if self.is_interpolation_start() {
            self.parse_interpolation();
        } else {
            let index = self.pos;
            self.bump();
            if self.parameter_depth > 0 {
                self.bracket_depth += 1;
            } else {
                // A bare `[` has no clear construct at this position. Report
                // it, but do not let an outer closing edit hide that ambiguity.
                self.recovery_barrier += 1;
                self.error_at(index, SyntaxErrorKind::UnexpectedOpenBracket);
            }
        }
    }

    fn is_interpolation_start(&mut self) -> bool {
        if self.peek() != Some(SyntaxKind::OpenBracket)
            || self.kind_at(self.pos + 1) == Some(SyntaxKind::OpenBracket)
        {
            return false;
        }
        if self.pos >= self.resolved_until {
            let mut starts = std::mem::take(&mut self.interpolations);
            self.resolved_until =
                resolve_interpolations(self.pos, |index| self.kind_far(index), &mut starts);
            self.interpolations = starts;
        }
        self.interpolations
            .binary_search(&(self.pos as u32))
            .is_ok()
    }

    fn parse_interpolation(&mut self) {
        let open = self.pos;
        self.sink.start_node(SyntaxKind::Interpolation);
        self.bump(); // `[`

        let mut bracket_depth = 1u32;
        let mut closed = false;
        loop {
            match self.peek() {
                Some(SyntaxKind::OpenBracket) => {
                    bracket_depth += 1;
                    self.bump();
                }
                Some(SyntaxKind::CloseBracket) => {
                    bracket_depth -= 1;
                    self.bump();
                    if bracket_depth == 0 {
                        closed = true;
                        break;
                    }
                }
                Some(SyntaxKind::CloseBrace) if bracket_depth == 1 => {
                    self.recovery_barrier += 1;
                    break;
                }
                None => break,
                Some(_) => self.bump(),
            }
        }

        if !closed {
            if self.peek().is_none() {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBracket,
                    UnclosedConstruct::Interpolation,
                    SyntaxErrorKind::UnclosedInterpolation,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedInterpolation);
            }
        }
        self.sink.finish_node();
    }

    fn parse_code_statement(&mut self) {
        self.sink.start_node(SyntaxKind::Code);
        self.bump(); // `code`
        self.parse_code_payload_body();
        self.sink.finish_node();
    }

    fn parse_code_payload(&mut self) {
        self.sink.start_node(SyntaxKind::Code);
        self.parse_code_payload_body();
        self.sink.finish_node();
    }

    fn parse_code_payload_body(&mut self) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.bump(); // first `[`
        self.bump(); // second `[`

        self.code_depth += 1;
        if self.code_depth > MAX_DEPTH {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::CodeDepthExceeded);
            self.flatten_to_code_close();
            self.code_depth -= 1;
            // The depth diagnostic owns this flattened region.
            return;
        }

        let mut closed = false;
        // The index of the last lone `]` that the payload took directly.
        let mut lone_close = usize::MAX;
        loop {
            match self.peek() {
                Some(SyntaxKind::CloseBracket) if self.is_code_close(self.pos) => {
                    self.bump();
                    self.bump();
                    closed = true;
                    break;
                }
                Some(SyntaxKind::CloseBrace) => {
                    // Keep an outer block delimiter available when the payload
                    // is incomplete. A brace directly before `]]` is payload.
                    if !self.is_code_close(self.pos + 1) {
                        if self.kind_at(self.pos + 1).is_some() {
                            self.recovery_barrier += 1;
                        }
                        break;
                    }
                    self.bump();
                }
                None => break,
                Some(kind) => {
                    if kind == SyntaxKind::CloseBracket {
                        lone_close = self.pos;
                    }
                    self.parse_item();
                }
            }
        }
        self.code_depth -= 1;

        if !closed {
            if self.peek().is_none() && self.recovery_barrier == barrier {
                // Only a lone `]` of the payload is the first half of `]]`.
                // A `]` that closes an interpolation is not, as in `a[b]`.
                let missing_count = if self.pos.checked_sub(1) == Some(lone_close)
                    && self.tok(lone_close).end() == self.source.len() as u32
                {
                    1
                } else {
                    2
                };
                for index in 0..missing_count {
                    let kind = if missing_count == 1 || index == 1 {
                        SyntaxErrorKind::CodeMissingSecondBracket
                    } else {
                        SyntaxErrorKind::CodeMissingFirstBracket
                    };
                    self.missing(
                        open,
                        SyntaxKind::MissingCloseBracket,
                        UnclosedConstruct::Code,
                        kind,
                    );
                }
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedCode);
            }
        }
    }

    /// Consume a code payload without recursing into nested code statements.
    fn flatten_to_code_close(&mut self) -> bool {
        let mut payload_depth = 1u32;
        while let Some(kind) = self.peek() {
            if self.is_code_close(self.pos) {
                self.bump();
                self.bump();
                payload_depth -= 1;
                if payload_depth == 0 {
                    return true;
                }
                continue;
            }

            if kind == SyntaxKind::Unquoted && self.code_statement_open(self.pos).is_some() {
                payload_depth += 1;
            }

            if kind == SyntaxKind::CloseBrace && !self.is_code_close(self.pos + 1) {
                break;
            }
            self.bump();
        }
        false
    }

    /// Parse an EU4 conditional parameter and its complete body.
    fn parse_parameter(&mut self, kind: SyntaxKind) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.sink.start_node(kind);
        self.bump(); // first `[`
        self.bump(); // second `[`
        if kind == SyntaxKind::UndefinedParameter {
            self.bump(); // `!`
        }
        self.bump(); // parameter name
        if self.peek() == Some(SyntaxKind::CloseBracket) {
            self.bump(); // header `]`
        } else {
            self.missing(
                open,
                SyntaxKind::MissingCloseBracket,
                UnclosedConstruct::Parameter,
                SyntaxErrorKind::UnclosedParameterHeader,
            );
        }

        self.parameter_depth += 1;
        let outer_bracket_depth = self.bracket_depth;
        self.bracket_depth = 0;
        let mut closed = false;
        loop {
            match self.peek() {
                Some(SyntaxKind::CloseBracket) if self.bracket_depth == 0 => {
                    self.bump();
                    closed = true;
                    break;
                }
                Some(SyntaxKind::CloseBrace) if self.bracket_depth == 0 => {
                    // EU4 parameter bodies can carry a brace across two
                    // parameter blocks, as in `... = { ]` followed by
                    // `[[name] } ]`. Keep that brace in the body when its
                    // parameter close follows it.
                    if self.kind_at(self.pos + 1) == Some(SyntaxKind::CloseBracket) {
                        self.bump();
                        continue;
                    }
                    self.recovery_barrier += 1;
                    break;
                }
                None => break,
                Some(_) => self.parse_item(),
            }
        }
        self.parameter_depth -= 1;
        self.bracket_depth = outer_bracket_depth;

        if !closed {
            if self.peek().is_none() && self.recovery_barrier == barrier {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBracket,
                    UnclosedConstruct::Parameter,
                    SyntaxErrorKind::UnclosedParameter,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedParameter);
            }
        }
        self.sink.finish_node();
    }

    fn parse_block(&mut self) {
        let open = self.pos;
        let barrier = self.recovery_barrier;
        self.sink.start_node(SyntaxKind::Block);
        self.bump_brace(); // `{`
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            self.recovery_barrier += 1;
            // The nesting is too deep. Do not recurse, because recursion can
            // cause a stack overflow. Consume the remaining tokens of this
            // block as flat tokens. The nested blocks are also flat. The tree
            // keeps all bytes, but it has no structure after this point.
            self.error_at(open, SyntaxErrorKind::BlockDepthExceeded);
            self.flatten_to_block_close();
        } else {
            self.parse_items(true);
            if self.peek() == Some(SyntaxKind::CloseBrace) {
                self.bump_brace(); // `}`
            } else if self.peek() == Some(SyntaxKind::CloseBracket)
                && self.parameter_depth > 0
                && self.bracket_depth == 0
            {
                // The `]` of a parameter can close a block body. A later
                // parameter block continues that block body.
            } else if self.peek() == Some(SyntaxKind::CloseBracket) {
                self.recovery_barrier += 1;
                self.error_at(open, SyntaxErrorKind::UnclosedBlock);
            } else if self.peek().is_none() && self.recovery_barrier == barrier {
                self.missing(
                    open,
                    SyntaxKind::MissingCloseBrace,
                    UnclosedConstruct::Block,
                    SyntaxErrorKind::UnclosedBlock,
                );
            } else {
                self.error_at(open, SyntaxErrorKind::UnclosedBlock);
            }
        }
        self.depth -= 1;
        self.sink.finish_node();
    }

    /// Consume the remaining tokens of the current block as flat tokens. The
    /// nested braces are also flat. Stop after the matching `}` or at EOF.
    /// The caller consumed the `{`, thus the brace depth starts at 1. The
    /// depth guard uses this function to limit recursion. The parser emits
    /// all tokens, thus the tree keeps all bytes.
    fn flatten_to_block_close(&mut self) {
        let mut balance = 1u32;
        while let Some(k) = self.peek() {
            match k {
                SyntaxKind::OpenBrace => balance += 1,
                SyntaxKind::CloseBrace => balance -= 1,
                _ => {}
            }
            self.bump();
            if balance == 0 {
                return; // emitted the matching `}`
            }
        }
        // The block did not close before EOF. The depth diagnostic covers
        // this block, thus do not add an "unclosed" diagnostic.
    }

    /// Parse a `@[ ... ]` calc. The current token is [`SyntaxKind::CalcOpen`].
    ///
    /// The lexer has already split the interior into calc tokens, so the whole
    /// region is a contiguous run `CalcOpen, <interior...>, [CalcClose]`. The
    /// interior is parsed (precedence-climbing) into a temporary element tree by
    /// [`parse_calc_interior`], then emitted to the sink in source order.
    fn parse_calc(&mut self) {
        let open = self.pos;
        self.sink.start_node(SyntaxKind::Calc);
        self.bump(); // `@[`

        // The interior is everything up to the matching `]` (or EOF). The lexer
        // only ever emits calc tokens between a CalcOpen and its CalcClose, so
        // no calc token can leak past this slice into the outer parser.
        let from = self.pos;
        self.pin = from;
        while !matches!(
            self.peek(),
            Some(SyntaxKind::CalcClose | SyntaxKind::OpenBrace | SyntaxKind::CloseBrace) | None
        ) {
            self.advance(1);
        }
        let at_eof = self.peek().is_none();
        let interior = parse_calc_interior(
            &self.tokens[from - self.base..self.pos - self.base],
            from as u32,
            at_eof,
        );
        // A `(` that the end of the source cuts off gets a missing delimiter
        // below.
        // Each other `(` with no `)` gets an error at the `(`.
        let missing_at_open: &[u32] = if at_eof {
            &[]
        } else {
            &interior.missing_parens
        };
        for el in &interior.elems {
            self.emit_calc(el, missing_at_open, &interior.unclosed_parens);
        }
        if !missing_at_open.is_empty() || !interior.unclosed_parens.is_empty() {
            self.recovery_barrier += 1;
        }
        if at_eof && !interior.overflowed && !interior.missing_parens.is_empty() && !S::KEEP_TOKENS
        {
            self.sink.error();
        }
        if at_eof && !interior.overflowed && S::KEEP_TOKENS {
            for paren_open in interior.missing_parens {
                // The zero-width leaf was emitted inside its ParenExpr above.
                self.record_missing(
                    paren_open as usize,
                    SyntaxKind::MissingCloseParen,
                    UnclosedConstruct::CalcParen,
                    SyntaxErrorKind::UnclosedCalcParen,
                );
            }
        }
        if interior.overflowed {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::CalcDepthExceeded);
        }

        if self.peek() == Some(SyntaxKind::CalcClose) {
            self.bump(); // `]`
        } else if at_eof && !interior.overflowed {
            self.missing(
                open,
                SyntaxKind::MissingCalcClose,
                UnclosedConstruct::Calc,
                SyntaxErrorKind::UnclosedCalc,
            );
        } else if !at_eof {
            self.recovery_barrier += 1;
            self.error_at(open, SyntaxErrorKind::UnclosedCalc);
        }
        self.pin = usize::MAX;
        self.sink.finish_node();
    }

    /// Walk a [`CalcElem`] in pre-order and send its tokens and nodes to the
    /// sink. Record an [`SyntaxErrorKind::UnclosedCalcParen`] at each `(` in
    /// `missing_parens` or `unclosed_parens`.
    fn emit_calc(&mut self, el: &CalcElem, missing_parens: &[u32], unclosed_parens: &[u32]) {
        match el {
            CalcElem::Leaf(index) => {
                let token = self.tok(*index as usize);
                self.sink.token(token);
            }
            CalcElem::Missing(kind) => self.push_missing(*kind),
            CalcElem::Node { kind, children, .. } => {
                self.sink.start_node(*kind);
                if *kind == SyntaxKind::ParenExpr
                    && let Some(CalcElem::Leaf(open)) = children.first()
                    && (missing_parens.contains(open) || unclosed_parens.contains(open))
                {
                    self.error_at(*open as usize, SyntaxErrorKind::UnclosedCalcParen);
                }
                for c in children {
                    self.emit_calc(c, missing_parens, unclosed_parens);
                }
                self.sink.finish_node();
            }
        }
    }
}

/// Find each `[` that starts a single-bracket interpolation, from the `[` at
/// `from` to the returned end index. Push their indexes to `starts` in sorted
/// order. `kind_at` gives the kind of a token, or `None` at EOF.
///
/// A `[` starts an interpolation when its matching `]` comes before a `}` that
/// is directly inside it. At EOF, a still-open `[` that is followed by more
/// tokens is a trailing interpolation. A bare final `[` is not.
///
/// The result for a `[` depends only on the tokens after it. The scan stops
/// when the `[` at `from` is resolved. At that point, each `[` inside the
/// scanned region is also resolved, so the total work is linear.
pub(super) fn resolve_interpolations(
    from: usize,
    mut kind_at: impl FnMut(usize) -> Option<SyntaxKind>,
    starts: &mut Vec<u32>,
) -> usize {
    starts.clear();
    // Open brackets and whether a `}` interrupted each one.
    let mut stack: Vec<(u32, bool)> = vec![(from as u32, false)];
    let mut index = from + 1;
    let end = loop {
        match kind_at(index) {
            Some(SyntaxKind::OpenBracket) => stack.push((index as u32, false)),
            Some(SyntaxKind::CloseBracket) => {
                if let Some((open, false)) = stack.pop() {
                    starts.push(open);
                }
                if stack.is_empty() {
                    break index + 1;
                }
            }
            Some(SyntaxKind::CloseBrace) => {
                if stack.len() == 1 {
                    // A `}` interrupts the `[` at `from`.
                    break index;
                }
                if let Some(top) = stack.last_mut() {
                    top.1 = true;
                }
            }
            Some(_) => {}
            None => {
                starts.extend(
                    stack
                        .iter()
                        .filter(|&&(open, interrupted)| !interrupted && open as usize + 1 < index)
                        .map(|&(open, _)| open),
                );
                break index;
            }
        }
        index += 1;
    };
    starts.sort_unstable();
    end
}
