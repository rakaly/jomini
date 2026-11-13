//! A parser for the Clausewitz text format that keeps every byte of the
//! source.
//!
//! **Experimental:** the API can change in any release. Enable the
//! `lossless` feature to use it.
//!
//! The tree keeps the comments, the whitespace, and each operator, and each
//! token knows its byte range in the source. The leaves of the tree
//! reproduce the source exactly:
//!
//! ```text
//! concat(text of every leaf token, in order) == source
//! ```
//!
//! A syntax error does not stop the parse. One pass finds every syntax error
//! in a file, and the fields after most errors are still available. A missing
//! closing quote is the exception: the string can continue to the next `"`
//! and hide the fields in between. Usually a
//! [`ProbablyUnclosedQuote`](SyntaxErrorKind::ProbablyUnclosedQuote) error
//! shows such a string, but not always.
//!
//! Use this parser on game and mod files when you need positions, comments,
//! or error recovery. For example, it can extract data and report each error
//! at its position, or it can be the base of a formatter, linter,
//! highlighter, or editor tool. For other jobs, the crate has better
//! choices:
//!
//! - To only deserialize a file into your own types, use serde with
//!   [`TextDeserializer`](crate::text::de::TextDeserializer). It is faster,
//!   but its errors do not give a line.
//! - To read a large save with little memory, use the streaming
//!   [`TokenReader`](crate::text::TokenReader) or
//!   [`from_windows1252_reader`](crate::text::de::from_windows1252_reader).
//! - To run existing code that reads a [`TextTape`], see
//!   [Read into a `TextTape`](#read-into-a-texttape).
//!
//! # Read a file
//!
//! [`parse`] gives a [`Parse`]. [`Parse::ast`] gives the typed view of the
//! file: a [`Root`] of items, where each [`Field`] has a key, an optional
//! operator, and a [`Value`]. See [`Root`] for an overview of the typed
//! view.
//! [`Root::get`] and [`Block::get`] find a field by its key:
//!
//! ```
//! use jomini::text::{Operator, lossless::parse};
//!
//! let source = b"
//! # Brittany
//! capital = 169
//! is_city = yes
//! discovered_by = { western eastern }
//! trigger = { num_of_cities >= 5 }
//! ";
//! let parse = parse(source);
//! assert!(parse.errors().is_empty());
//!
//! let root = parse.ast();
//! let field = |key: &[u8]| root.get(key).unwrap();
//!
//! assert_eq!(field(b"capital").value().unwrap().to_u64(), Some(169));
//! assert_eq!(field(b"is_city").value().unwrap().to_bool(), Some(true));
//!
//! let discovered_by = field(b"discovered_by").value().unwrap().as_block().unwrap();
//! assert_eq!(discovered_by.values().count(), 2);
//!
//! let trigger = field(b"trigger").value().unwrap().as_block().unwrap();
//! let condition = trigger.fields().next().unwrap();
//! assert_eq!(condition.key().as_bytes(), b"num_of_cities");
//! assert_eq!(condition.op(), Some(Operator::GreaterThanEqual));
//! ```
//!
//! A key, a scalar value, and a header (`rgb` in `rgb { 1 2 3 }`) are each a
//! [`ScalarToken`]. It gives the bytes with or without the quotes, and the
//! decoded text.
//!
//! In item position, a scalar directly before a block (`key { ... }`) is a
//! [`Field`] with no operator. The parser does not decide if the scalar is a
//! key or a header. In value position (`color = rgb { ... }`), the scalar and
//! the block are a [`HeaderedBlock`], so the field has one value.
//!
//! The inner part of `@[calc]` is an expression subtree: see [`Calc`] and
//! [`Expr`].
//!
//! # Keep a parse
//!
//! [`parse`] does not copy the source. A [`Parse`] borrows it, and the text
//! of each token has the lifetime of the source, so the text can outlive the
//! `Parse`. To keep the tree and the source together, for example in a
//! struct, a cache, or on another thread, use [`parse_owned`]. The
//! [`OwnedParse`] that it returns owns the source and has the same query
//! methods as `Parse`. The source can be a `Vec<u8>`, a `String`, or an
//! `Arc<[u8]>`:
//!
//! ```
//! use jomini::text::lossless::{parse_owned, OwnedParse};
//!
//! struct ModFile {
//!     path: String,
//!     parse: OwnedParse<Vec<u8>>,
//! }
//!
//! let file = ModFile {
//!     path: String::from("common/buildings/00_buildings.txt"),
//!     parse: parse_owned(b"temple = { cost = 100 }".to_vec()),
//! };
//! assert_eq!(file.parse.ast().fields().count(), 1);
//! ```
//!
//! # Change a value
//!
//! To change a value, replace the byte range of its token. The comments, the
//! whitespace, and the order of the fields do not change:
//!
//! ```
//! use jomini::text::lossless::{apply_edits, parse, TextEdit};
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
//! # Ok::<(), jomini::text::lossless::EditError>(())
//! ```
//!
//! Each range is a byte range in the source that you parsed. To change more than
//! one value, collect the edits from one parse and give them all to
//! [`apply_edits`]. Then parse the result again to get new ranges.
//! [`format`](fn@format) writes the whole file again in a normalized style,
//! and it keeps each comment.
//!
//! # Syntax errors
//!
//! [`Parse::errors`] gives each syntax error with its kind and its byte range.
//! When the source ends before a `}`, `]`, or `)` closes its construct, the
//! error has a [`MissingDelimiter`], and [`Parse::closing_edit`] gives one
//! edit that inserts all of the missing delimiters at the end of the source.
//!
//! The edit keeps the meaning of the file, because the parser and the game
//! also close each open construct at the end of the source. When the file is
//! cut off, that is also what the author wants. When a `}` was lost earlier
//! in the file, the fields after it stay in the block, as `name = Jean` does
//! here. The error gives the position of the `{`, so a tool can show it:
//!
//! ```
//! use jomini::text::lossless::{apply_edits, parse, SyntaxErrorKind};
//!
//! let source = b"core = { FRA BUR\nname = Jean";
//! let parse = parse(source);
//! let [error] = parse.errors() else { panic!("expected one error") };
//! assert_eq!(error.kind, SyntaxErrorKind::UnclosedBlock);
//! let delimiter = error.missing_delimiter.unwrap();
//! assert_eq!(&source[delimiter.opening_range.to_range()], b"{");
//!
//! let core = parse.ast().get(b"core").unwrap();
//! assert!(core.value().unwrap().as_block().unwrap().get(b"name").is_some());
//!
//! let fixed = apply_edits(source, parse.closing_edit())?;
//! assert_eq!(fixed, b"core = { FRA BUR\nname = Jean}");
//! # Ok::<(), jomini::text::lossless::EditError>(())
//! ```
//!
//! A line break does not end a field, but a scalar that an operator follows
//! is the key of the next field. Thus `b =` followed by `c = 2` on the next
//! line gives a [`MissingValue`](SyntaxErrorKind::MissingValue) error at `b =`
//! and the field `c = 2`.
//!
//! To report the position of an error, [`LineIndex`] converts its byte offset
//! to a line and a column.
//!
//! # Read into a `TextTape`
//!
//! For existing code that uses the
//! [`ObjectReader`](crate::text::ObjectReader), the deserializer, or the JSON
//! conversion, use [`parse_tape`]. It parses with the same grammar as
//! [`parse`], and it writes a [`TextTape`] that this code reads without a
//! change. Unlike [`TextTape::from_slice`](crate::TextTape::from_slice), it
//! does not stop at the first syntax error: a [`TapeParse`] holds the tape
//! and all of the errors, and the caller decides which errors are
//! acceptable. The tape discards the trivia. [`parse_tape`] lists where its
//! tape differs from the tape of `from_slice`.
//!
//! # Deserialize into your own types
//!
//! The [`de`] module deserializes the tree into serde types with no tape. A
//! type that derives `Deserialize` for `TextDeserializer` needs no change.
//! Each error gives the line, the column, and the path of keys.
//! To read `@name` variables and `@[ ... ]` calculations as their values,
//! not as their source text, use [`resolve`].
//!
//! To keep a whole document as data, read it into an [`OwnedBlock`]. The
//! block keeps the quotes, the operators, the headers, and the order of the
//! items. Later, you can deserialize the block into a type.
//!
//! # Walk the tree
//!
//! Under the typed view are the [`SyntaxNode`] and [`SyntaxToken`] cursors.
//! [`Parse::tree`] gives the [`SyntaxTree`] view, which finds nodes and
//! tokens by position or by identity. The views and the cursors have two
//! lifetimes: `'t` for the tree and `'a` for the source.
//! Each node and token has a [`SyntaxKind`] and a [`TextRange`].
//! [`SyntaxNode::children`] gives the trivia too, and
//! [`SyntaxNode::significant_children`] does not. [`SyntaxTree::token_at_offset`]
//! and [`SyntaxTree::covering_element`] find the element at an editor
//! position. [`NodeId`] and [`TokenId`] are dense indices, so a pass can keep
//! its data for each node in a `Vec`.
//!
//! The trivia between two tokens belongs to the nearest node that contains
//! both tokens. Thus a comment at the end of a line is often not in the
//! field before it:
//!
//! ```
//! use jomini::text::lossless::{parse, AstNode, SyntaxKind};
//!
//! let parse = parse(b"a = 1 # note\nb = 2");
//! let field = parse.ast().get(b"a").unwrap();
//! assert!(!field.syntax().has_comment());
//!
//! let comment = parse.tokens().find(|t| t.kind() == SyntaxKind::Comment).unwrap();
//! assert_eq!(comment.parent().unwrap().kind(), SyntaxKind::Root);
//! ```
//!
//! [`SyntaxTree::debug_tree`] shows each node and token with its kind and its
//! byte range. The `lossless` example prints it for a file or for stdin. It
//! can also print the tokens, the formatted file, and the syntax errors at
//! their line and column:
//!
//! ```sh
//! printf 'building = { cost = 100 } # wood\n' | cargo run --features lossless --example lossless -- --tree --no-trivia
//! cargo run --features lossless --example lossless -- --tokens --no-trivia FILE
//! cargo run --features lossless --example lossless -- --help
//! ```
//!
//! # Games
//!
//! One parser reads the files of all of the games. The lexer accepts the
//! syntax of each game (`@variables`, `@[calc]`, `$macros$`,
//! `[[param] ... ]`, and `code [[ ... ]]`).
//! Your code, not the parser, decides if a construct is correct for a game.
//! [`Flavor`] selects the few cases where the lexers of the games are
//! different. For HoI4 files, pass [`Flavor::hoi4`] to [`parse_with`],
//! [`parse_owned_with`], or [`parse_tape_with`]: HoI4 ends a quoted string
//! at a line break, and the default flavor does not.
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
pub use flavor::Flavor;
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
    quote_hides_fields, scan_quoted, scan_unquoted, skip_whitespace,
};
use parser::{MAX_DEPTH, Sink};
#[cfg(test)]
use tree::run;
use tree::run_items;
