//! The syntax errors and the edits that the parser records.

use super::{SyntaxKind, TextRange};
use std::fmt;

/// The kind of a [`SyntaxError`].
///
/// When the source ends before a construct closes, the error usually has a
/// [`missing_delimiter`](SyntaxError::missing_delimiter).
///
/// A `*DepthExceeded` error means that constructs nest more than 256 levels
/// deep. Each kind of construct has its own count. The tree keeps the bytes of
/// the deep construct, but not its structure.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum SyntaxErrorKind {
    /// A `}` that closes no block. The tree keeps it in a
    /// [`SyntaxKind::Bogus`] node.
    UnmatchedCloseBrace,
    /// A `]` that closes no parameter, code payload, or interpolation.
    UnmatchedCloseBracket,
    /// A `{` that has no matching `}`. The range is the end of the source;
    /// [`MissingDelimiter::opening_range`] gives the `{`.
    UnclosedBlock,
    /// A single-bracket interpolation `[ ... ]` cut off at the end of the
    /// source.
    UnclosedInterpolation,
    /// An EU4 parameter header, such as `[[name`, that has no `]`.
    UnclosedParameterHeader,
    /// An EU4 parameter block `[[name] ... ]` that has no closing `]`.
    UnclosedParameter,
    /// An EU5 code payload `[[ ... ]]` ended by a `}` before its closing `]]`.
    UnclosedCode,
    /// An EU5 code payload cut off at the end of the source before its `]]`.
    /// This error is for the first `]`.
    CodeMissingFirstBracket,
    /// An EU5 code payload cut off at the end of the source before its `]]`.
    /// This error is for the second `]`.
    CodeMissingSecondBracket,
    /// A `@[` calculation that has no `]`.
    UnclosedCalc,
    /// A `(` in a calculation that has no `)`.
    UnclosedCalcParen,
    /// Blocks nest too deep.
    BlockDepthExceeded,
    /// EU4 parameter blocks nest too deep.
    ParameterDepthExceeded,
    /// EU5 code payloads nest too deep.
    CodeDepthExceeded,
    /// The expressions of a calculation nest too deep. A chain of more than
    /// 256 operators, such as `1 + 1 + ... + 1`, also nests too deep.
    CalcDepthExceeded,
    /// CK3 reader directives nest too deep.
    DirectiveDepthExceeded,
    /// A `[` that starts no parameter, code payload, or interpolation.
    UnexpectedOpenBracket,
    /// A quoted string that continues to the end of the source.
    UnclosedQuote,
    /// A quoted string that spans a line break and ends with an operator, as
    /// in `title = "My Mod` followed by `version = "1.0"` on the next line.
    /// Most likely, the string has no closing quote at the end of its first
    /// line, and it hides the fields after it. The range is the opening
    /// quote.
    ProbablyUnclosedQuote,
    /// An operator that has no value after it, as in `a = }` or `a =` at the
    /// end of the source. The range is the range of the operator.
    ///
    /// A scalar that an operator follows is the key of the next field, not a
    /// value. Thus `b =` followed by `c = 2` on the next line is a field `b`
    /// with this error, and a field `c = 2`.
    MissingValue,
    /// An operator that has no key before it, as in `= 2`. The tree keeps it
    /// in a [`SyntaxKind::Bogus`] node. An operator directly after a block is
    /// not an error, because the block is the key, as in `{ a = b }={ 1 }`.
    UnexpectedOperator,
}

impl SyntaxErrorKind {
    /// A human-readable description.
    pub fn message(self) -> &'static str {
        match self {
            Self::UnmatchedCloseBrace => "unmatched '}'",
            Self::UnmatchedCloseBracket => "unmatched ']'",
            Self::UnclosedBlock => "unclosed '{'",
            Self::UnclosedInterpolation => "unclosed bracket interpolation",
            Self::UnclosedParameterHeader => "unclosed parameter header",
            Self::UnclosedParameter => "unclosed parameter block",
            Self::UnclosedCode => "unclosed code payload",
            Self::CodeMissingFirstBracket => "unclosed code payload (missing first ']')",
            Self::CodeMissingSecondBracket => "unclosed code payload (missing second ']')",
            Self::UnclosedCalc => "unclosed '@['",
            Self::UnclosedCalcParen => "unclosed parenthesized calculation",
            Self::BlockDepthExceeded => "maximum nesting depth exceeded; structure flattened",
            Self::ParameterDepthExceeded => {
                "maximum parameter nesting depth exceeded; structure flattened"
            }
            Self::CodeDepthExceeded => "maximum code nesting depth exceeded; structure flattened",
            Self::CalcDepthExceeded => "maximum calc nesting depth exceeded; structure flattened",
            Self::DirectiveDepthExceeded => "maximum directive nesting depth exceeded",
            Self::UnexpectedOpenBracket => "'[' starts no construct",
            Self::UnclosedQuote => "unclosed '\"'",
            Self::ProbablyUnclosedQuote => "quoted string probably has no closing '\"'",
            Self::MissingValue => "operator has no value",
            Self::UnexpectedOperator => "operator has no key",
        }
    }
}

impl fmt::Display for SyntaxErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message())
    }
}

/// A problem that the parser found and recovered from. A parse never fails,
/// and the tree always keeps all of the bytes. The tree keeps these problems
/// for tools.
///
/// The parser continues after an error, so one parse finds all of the
/// problems in a file, and the fields after an error are still available:
///
/// ```
/// use jomini::text::lossless::{parse, SyntaxErrorKind};
///
/// let parse = parse(b"a = { b = 1 } } c = 2");
/// let error = &parse.errors()[0];
/// assert_eq!(error.kind, SyntaxErrorKind::UnmatchedCloseBrace);
/// assert_eq!(error.to_string(), "unmatched '}' at 14..15");
/// assert_eq!(parse.ast().fields().count(), 2);
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct SyntaxError {
    /// What is wrong.
    pub kind: SyntaxErrorKind,
    /// The half-open byte range the problem covers.
    pub range: TextRange,
    /// The delimiter that the source does not have, when the source ends
    /// before a construct closes. [`SyntaxTree::closing_edit`] gives the edit
    /// that inserts it.
    ///
    /// [`SyntaxTree::closing_edit`]: super::SyntaxTree::closing_edit
    pub missing_delimiter: Option<MissingDelimiter>,
}

impl SyntaxError {
    /// A human-readable description.
    pub fn message(&self) -> &'static str {
        self.kind.message()
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.message(), self.range)
    }
}

impl std::error::Error for SyntaxError {}

/// The construct that a [`MissingDelimiter`] leaves open.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum UnclosedConstruct {
    /// A `{ ... }` block.
    Block,
    /// A single-bracket interpolation `[ ... ]`.
    Interpolation,
    /// An EU4 conditional parameter block `[[name] ... ]` or `[[!name] ... ]`.
    Parameter,
    /// A `@[ ... ]` calculation.
    Calc,
    /// An EU5 code payload `[[ ... ]]`.
    Code,
    /// A `( ... )` group inside a calculation.
    CalcParen,
}

/// A delimiter that the source does not have, because the source ends before
/// the construct closes. The tree has a zero-width token in its place, at the
/// end of the source.
///
/// All of the missing delimiters of a source share one edit:
/// [`SyntaxTree::closing_edit`](super::SyntaxTree::closing_edit).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MissingDelimiter {
    /// The construct that the delimiter closes.
    pub construct: UnclosedConstruct,
    /// The range of the token that opens the construct, such as its `{`.
    pub opening_range: TextRange,
    /// The kind of the zero-width token, such as
    /// [`SyntaxKind::MissingCloseBrace`].
    pub(super) missing: SyntaxKind,
    /// The place of this delimiter in the closing edit. The innermost
    /// construct is first.
    pub(super) order: u32,
    /// The text that this delimiter adds to the closing edit.
    pub(super) text: &'static str,
    /// The parser found that the end of the source is a safe place for this
    /// delimiter.
    pub(super) safe: bool,
}

impl MissingDelimiter {
    /// The kind of the delimiter that the source does not have, such as
    /// [`SyntaxKind::CloseBrace`].
    pub fn expected(&self) -> SyntaxKind {
        self.missing.expected_kind().unwrap_or(self.missing)
    }
}

/// The text that inserts the delimiter of `missing` after `line_break`, which
/// is `""`, `"\n"` or `"\r\n"`.
pub(super) fn closing_text(missing: SyntaxKind, line_break: &str) -> &'static str {
    // Each text is the end of the text after a `\r\n`.
    let text = match missing {
        SyntaxKind::MissingCloseBrace => "\r\n}",
        SyntaxKind::MissingCloseBracket | SyntaxKind::MissingCalcClose => "\r\n]",
        SyntaxKind::MissingCloseParen => "\r\n)",
        kind => unreachable!("{kind:?} is not a missing delimiter"),
    };
    &text[2 - line_break.len()..]
}
