//! The kinds of the nodes and tokens in a syntax tree.

/// The kind of every node (interior) and token (leaf) in a [`SyntaxTree`](super::SyntaxTree).
///
/// The tree does not store the trivia kinds ([`Bom`](Self::Bom),
/// [`Whitespace`](Self::Whitespace), and [`Comment`](Self::Comment)). The
/// cursors lex them again from the gaps between the stored tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
#[non_exhaustive]
pub enum SyntaxKind {
    /// A leading UTF-8 byte-order-mark (`EF BB BF`).
    Bom,
    /// A run of spaces, tabs, newlines, carriage returns, or semicolons.
    Whitespace,
    /// A `# ...` comment running to end of line.
    Comment,

    /// A bare scalar: identifier, number, date, `yes`/`no`, etc.
    Unquoted,
    /// A `"..."` quoted scalar (raw text, including the quotes).
    Quoted,
    /// A `@name` reader-variable reference.
    Variable,
    /// An unquoted scalar that contains one or more `$NAME$` macro
    /// parameters, such as `$NAME$` or `$key$_modifier`.
    MacroParam,
    /// A CK3 reader directive, such as `@:insert` or `@:define`.
    Directive,
    /// A `@@name` reader-variable reference in key position. The reader puts
    /// the value of the variable in place of the key.
    VariableKey,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `!`, the undefined-parameter marker in `[[!name] ...]`.
    Bang,
    /// An operator: `=` `==` `?=` `!=` `<` `<=` `>` `>=`.
    Operator,
    /// A synthetic, zero-width `}` inserted after an unclosed block at EOF.
    MissingCloseBrace,
    /// A synthetic, zero-width `]` inserted after an unclosed bracket construct.
    /// Code payloads use two consecutive tokens because their `]]` delimiter
    /// consists of two source tokens.
    MissingCloseBracket,

    /// `@[`, opening a parse-time calculation.
    CalcOpen,
    /// `]`, closing a parse-time calculation.
    CalcClose,
    /// A synthetic, zero-width `]` inserted after an unclosed calculation.
    MissingCalcClose,
    /// `+` inside a calc.
    Plus,
    /// `-` inside a calc.
    Minus,
    /// `*` inside a calc.
    Star,
    /// `/` inside a calc.
    Slash,
    /// `(` inside a calc.
    OpenParen,
    /// `)` inside a calc.
    CloseParen,
    /// A synthetic, zero-width `)` inserted after an unclosed calc expression.
    MissingCloseParen,
    /// A numeric literal inside a calc (e.g. `1`, `10.0`, `10.0f`).
    Number,
    /// An operand identifier inside a calc (e.g. `tier`, `leopard_x`, `@var`).
    CalcIdent,

    /// The whole document.
    Root,
    /// A `key <op> value` field, or a `key { ... }` field with no operator.
    Field,
    /// A `{ ... }` block.
    Block,
    /// A tagged block in value position, such as `rgb { 1 2 3 }` in
    /// `color = rgb { 1 2 3 }` (a header scalar plus a block). This node
    /// gives the field exactly one value. In item position, `rgb { 1 2 3 }`
    /// is a [`SyntaxKind::Field`] with no operator.
    HeaderedBlock,
    /// A `@[ ... ]` parse-time calculation wrapping an arithmetic expression.
    Calc,
    /// An EU4 conditional parameter block, `[[name] ... ]`.
    Parameter,
    /// An EU4 undefined conditional parameter block, `[[!name] ... ]`.
    UndefinedParameter,
    /// An EU5 code payload, `code [[ ... ]]`.
    Code,
    /// A single-bracket interpolation, such as `[ROOT.GetName]`.
    Interpolation,
    /// A CK3 reader directive and its arguments, such as
    /// `@:insert name = { ... }`. See [`ReaderDirective`](super::ReaderDirective).
    ReaderDirective,
    /// A binary arithmetic expression `lhs <op> rhs` inside a [`SyntaxKind::Calc`].
    BinaryExpr,
    /// A prefix `-`/`+` expression inside a [`SyntaxKind::Calc`].
    UnaryExpr,
    /// A parenthesized expression `( ... )` inside a [`SyntaxKind::Calc`].
    ParenExpr,
    /// An error-recovery wrapper around tokens that could not be placed.
    Bogus,
}

impl SyntaxKind {
    /// Return `true` for a synthetic recovery token.
    pub const fn is_missing(self) -> bool {
        matches!(
            self,
            Self::MissingCloseBrace
                | Self::MissingCloseBracket
                | Self::MissingCalcClose
                | Self::MissingCloseParen
        )
    }

    /// Return the physical token that a missing token represents.
    pub const fn expected_kind(self) -> Option<SyntaxKind> {
        Some(match self {
            Self::MissingCloseBrace => Self::CloseBrace,
            Self::MissingCloseBracket => Self::CloseBracket,
            Self::MissingCalcClose => Self::CalcClose,
            Self::MissingCloseParen => Self::CloseParen,
            _ => return None,
        })
    }

    /// Return the source spelling for a missing token.
    pub const fn missing_text(self) -> Option<&'static str> {
        Some(match self {
            Self::MissingCloseBrace => "}",
            Self::MissingCloseBracket | Self::MissingCalcClose => "]",
            Self::MissingCloseParen => ")",
            _ => return None,
        })
    }

    /// Return `true` when this kind is a physical closing delimiter.
    pub const fn is_physical_close(self) -> bool {
        matches!(
            self,
            Self::CloseBrace | Self::CloseBracket | Self::CalcClose | Self::CloseParen
        )
    }

    /// Return `true` for whitespace, a comment, or the BOM. These tokens have
    /// no effect on the structure, but the tree keeps their bytes.
    pub const fn is_trivia(self) -> bool {
        matches!(
            self,
            SyntaxKind::Whitespace | SyntaxKind::Comment | SyntaxKind::Bom
        )
    }

    /// A token that can stand as a scalar value (or an object key). This is the
    /// single definition of "scalar" shared by key detection, [`Value::Scalar`](super::Value::Scalar),
    /// and the formatter: `$macro$` parameters count, since in template files
    /// they appear in key, value, and array-element position alike.
    pub const fn is_scalar(self) -> bool {
        matches!(
            self,
            SyntaxKind::Unquoted
                | SyntaxKind::Quoted
                | SyntaxKind::Variable
                | SyntaxKind::VariableKey
                | SyntaxKind::MacroParam
        )
    }

    /// Whether this kind labels an interior node (rather than a leaf token).
    pub const fn is_node(self) -> bool {
        matches!(
            self,
            SyntaxKind::Root
                | SyntaxKind::Field
                | SyntaxKind::Block
                | SyntaxKind::HeaderedBlock
                | SyntaxKind::Calc
                | SyntaxKind::Parameter
                | SyntaxKind::UndefinedParameter
                | SyntaxKind::Code
                | SyntaxKind::Interpolation
                | SyntaxKind::ReaderDirective
                | SyntaxKind::BinaryExpr
                | SyntaxKind::UnaryExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::Bogus
        )
    }
}
