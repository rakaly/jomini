//! Parse game and mod files into a lossless tree for diagnostics, formatting,
//! linting, and editing.
//!
//! This module's API is **experimental**. Enable it with the `syntax` feature.
//!
//! For saves you only need to deserialize, see
//! [choosing a text deserializer](super#choosing-a-text-deserializer).
//!
//! # Read a field and find a comment
//!
//! Parse this snippet, then read its capital ID and find the comment in the
//! same tree.
//!
//! ```
//! use jomini::text::syntax::{parse, SyntaxKind};
//!
//! let parsed = parse(b"capital = 169 # Paris");
//! assert!(parsed.errors().is_empty());
//!
//! let capital = parsed.ast().get(b"capital").unwrap();
//! assert_eq!(capital.value().unwrap().to_u64(), Some(169));
//!
//! let comment = parsed.tokens().find(|t| t.kind() == SyntaxKind::Comment).unwrap();
//! assert_eq!(comment.text(), b"# Paris");
//! ```
//!
//! [`Parse::ast`] gives you a typed [`Root`] for reading fields. The same
//! parse gives you the original comments and whitespace through [`Parse::tokens`].
//!
//! # How-to guides
//!
//! ## Store the tree with its source
//!
//! If you're storing the tree in a cache, pass the source as a `Vec<u8>`,
//! `String`, or `Arc<[u8]>` to [`parse_owned`]. The returned [`OwnedParse`]
//! holds both the source and the tree.
//!
//! ## Change a value
//!
//! To change a value, replace the byte range of its token. This example
//! updates two values and preserves the surrounding comments and whitespace.
//!
//! ```
//! use jomini::text::syntax::{apply_edits, parse, TextEdit};
//!
//! let source = b"# Set by the history file\nbase_tax = 3 # was 2\nbase_production = 2\n";
//! let parse = parse(source);
//! let root = parse.ast();
//! let scalar = |key: &[u8]| {
//!     let value = root.get(key).and_then(|f| f.value());
//!     value.and_then(|v| v.as_scalar_token()).unwrap()
//! };
//!
//! let edits = [
//!     TextEdit::new(scalar(b"base_tax").text_range(), "10"),
//!     TextEdit::new(scalar(b"base_production").text_range(), "4"),
//! ];
//! let edited = apply_edits(source, &edits)?;
//! assert_eq!(
//!     edited,
//!     b"# Set by the history file\nbase_tax = 10 # was 2\nbase_production = 4\n"
//! );
//! # Ok::<(), jomini::text::syntax::EditError>(())
//! ```
//!
//! Each range is a byte range in the source that you parsed. To change more than
//! one value, collect the edits from one parse and give them all to
//! [`apply_edits`]. Then parse the result again to get new ranges.
//! [`format`](fn@format) writes the whole file again in a normalized style,
//! and it keeps each comment.
//!
//! ## Report and repair syntax errors
//!
//! Read [`Parse::errors`] for the kind and byte range of each error.
//! Use [`LineIndex`] to convert a byte offset to a line and column.
//!
//! If a file ends with an open construct, [`Parse::closing_edit`] can supply
//! an edit that appends the missing delimiters. Show the opening delimiter
//! in the diagnostic. If the author meant to close the block earlier,
//! appending a delimiter leaves later fields inside the block. In this
//! example, `name = Jean` stays inside `core`.
//!
//! ```
//! use jomini::text::syntax::{apply_edits, parse, LineIndex, SyntaxErrorKind};
//!
//! let source = b"core = { FRA BUR\nname = Jean";
//! let parse = parse(source);
//! let [error] = parse.errors() else { panic!("expected one error") };
//! assert_eq!(error.kind, SyntaxErrorKind::UnclosedBlock);
//! let delimiter = error.missing_delimiter.unwrap();
//! assert_eq!(&source[delimiter.opening_range.to_range()], b"{");
//! let opening = LineIndex::new(source).line_col(delimiter.opening_range.start());
//! assert_eq!(opening.to_string(), "1:8");
//!
//! let core = parse.ast().get(b"core").unwrap();
//! assert!(core.value().unwrap().as_block().unwrap().get(b"name").is_some());
//!
//! let fixed = apply_edits(source, parse.closing_edit())?;
//! assert_eq!(fixed, b"core = { FRA BUR\nname = Jean}");
//! # Ok::<(), jomini::text::syntax::EditError>(())
//! ```
//!
//! ## Read into a `TextTape`
//!
//! [`parse_tape`] builds a [`TextTape`] for existing tape readers and returns
//! the syntax errors alongside it. The tape drops comments and whitespace.
//!
#![cfg_attr(
    feature = "serde",
    doc = r#"
## Deserialize into your own types

Use [`Parse::deserialize`] for a strict read or [`Parse::document`] to read
a file with syntax errors. See [`de`] for partial reads and diagnostics
and [`resolve`] for reading variables and calculations as values.
"#
)]
//!
//! ## Walk the tree
//!
//! Use [`Root::get`] or [`Block::get`] to look up a field by key. For
//! editor positions or access to trivia, get the [`SyntaxTree`] with
//! [`Parse::tree`]. Use [`SyntaxTree::token_at_offset`] or
//! [`SyntaxTree::covering_element`] to find an element by byte offset.
//! [`SyntaxTree::debug_tree`] displays nodes and tokens with their byte ranges.
//!
//! ## Parse HoI4 files
//!
//! Pass [`Hoi4Flavor`] or the flavor from the `hoi4save` crate to
//! [`parse_with`], [`parse_owned_with`], or [`parse_tape_with`].
//!
//! # Syntax reference
//!
//! ## Fields and values
//!
//! Keys, scalar values, and block headers are all [`ScalarToken`]s. A token
//! gives you the bytes with or without quotes, and the decoded text.
//!
//! Where a new entry can begin, `rgb { 1 2 3 }` parses as a [`Field`] with no
//! operator. Its key is `rgb` and its value is the block. What `rgb` means
//! there depends on the game's rules.
//!
//! In `color = rgb { 1 2 3 }`, the key is `color`. The parser groups `rgb`
//! and the block into a single value, a [`HeaderedBlock`] with `rgb` as its
//! header. A header can be quoted, as GUI files write
//! `blockoverride = "tab_glow" { visible = no }`;
//! [`ScalarToken::is_quoted`] tells.
//!
//! The inner part of `@[calc]` is an expression subtree: see [`Calc`] and
//! [`Expr`].
//!
//! ## Error recovery
//!
//! The parser continues after syntax errors. A missing closing quote can
//! hide fields up to the next quote. See
//! [`ProbablyUnclosedQuote`](SyntaxErrorKind::ProbablyUnclosedQuote).
//! When a construct reaches the end of the source without its closing
//! delimiter, [`MissingDelimiter`] gives the opening delimiter's range.
//!
//! A field can span multiple lines. When the next token is a scalar
//! followed by an operator, the parser starts a new field. For example,
//! `b =` followed by `c = 2` on the next line gives a
//! [`MissingValue`](SyntaxErrorKind::MissingValue) error at `b =` and
//! a field for `c = 2`.
//!
//! ## Nodes, tokens, and trivia
//!
//! Each [`SyntaxNode`] and [`SyntaxToken`] has a [`SyntaxKind`] and a byte
//! range ([`TextRange`]). [`SyntaxNode::children`] includes trivia, while
//! [`SyntaxNode::significant_children`] skips it. [`NodeId`] and [`TokenId`]
//! are dense indices that can index a `Vec` of per-element data.
//!
//! A [`Parse`] borrows the source, so token text can outlive the parse.
//! The tree views and cursors have two lifetimes, `'t` for the tree and
//! `'a` for the source.
//!
//! The trivia between two tokens belongs to the nearest node that contains
//! both tokens. Here, the comment between two top-level fields belongs to
//! the root.
//!
//! ```
//! use jomini::text::syntax::{parse, AstNode, SyntaxKind};
//!
//! let parse = parse(b"a = 1 # note\nb = 2");
//! let field = parse.ast().get(b"a").unwrap();
//! assert!(!field.syntax().has_comment());
//!
//! let comment = parse.tokens().find(|t| t.kind() == SyntaxKind::Comment).unwrap();
//! assert_eq!(comment.parent().unwrap().kind(), SyntaxKind::Root);
//! ```
//!
//! # How game support works
//!
//! The games share a grammar for fields and blocks. The parser also
//! recognizes constructs from individual games, including `@variables`,
//! `@[calc]`, `$macros$`, `[[param] ... ]`, and `code [[ ... ]]`. A tool can
//! use the same tree APIs across games and apply the rules of its target
//! game when it interprets that tree.
//!
//! A [`TextFlavor`](crate::text::TextFlavor) handles differences in how the
//! games tokenize text. For example, in HoI4, `$NAME$` is ordinary text.
//!
//! [`TextTape`]: crate::TextTape

mod ast;
mod calc;
mod cursor;
#[cfg(feature = "serde")]
pub mod de;
mod edit;
mod error;
mod flavor;
mod format;
mod kind;
mod lexer;
mod line_index;
mod parser;
pub mod resolve;
mod tape;
#[cfg(test)]
mod tests;
mod text_size;
mod tree;
#[cfg(feature = "serde")]
pub(crate) mod value;

pub use ast::{
    AstNode, BinOp, BinaryExpr, Block, Calc, Code, DirectiveKind, Expr, Field, HeaderedBlock,
    Interpolation, Item, Parameter, ParenExpr, ReaderDirective, Root, ScalarToken, UnaryExpr,
    UnaryOp, UndefinedParameter, Value,
};
pub use cursor::{
    Children, NodeId, Preorder, SignificantChildren, SyntaxElement, SyntaxNode, SyntaxToken,
    TokenAtOffset, TokenId, Tokens, WalkEvent,
};
pub use edit::{EditError, TextEdit, apply_edits};
pub use error::{MissingDelimiter, SyntaxError, SyntaxErrorKind, UnclosedConstruct};
pub use flavor::Hoi4Flavor;
use flavor::LexFlavor;
pub use format::{FormatOptions, format};
pub use kind::SyntaxKind;
pub use line_index::{LineCol, LineIndex};
pub use tape::{TapeParse, parse_tape, parse_tape_with};
pub use text_size::{TextRange, TextSize};
pub use tree::{
    NodeFlags, OwnedParse, Parse, SyntaxTree, parse, parse_owned, parse_owned_with, parse_with,
};
#[cfg(feature = "serde")]
pub use value::{OwnedBlock, OwnedField, OwnedItem, OwnedParameter, OwnedScalar, OwnedValue};

// The tape parser in `tape` reads these private items through this module.
use ast::strip_quotes;
use cursor::operator;
use format::quote_is_closed;
use lexer::{
    BOM, CLASS, CLASS_BANG, CLASS_CLOSE_BRACE, CLASS_CLOSE_BRACKET, CLASS_COMMENT,
    CLASS_OPEN_BRACE, CLASS_OPEN_BRACKET, CLASS_OPERATOR, CLASS_QUOTE, Token, find_line_end, is_ws,
    quote_hides_fields, scan_quoted, scan_unquoted, skip_whitespace, starts_line,
};
use parser::Sink;
use tree::run_items;
#[cfg(test)]
use tree::{parse_lex, run};
