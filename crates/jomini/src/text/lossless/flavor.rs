//! The game-specific options of the lexer and the parser.

/// Game-specific lexer configuration.
///
/// [`Flavor::default`] fits every game except HoI4. HoI4 has no `$NAME$`
/// macro parameters, so [`Flavor::hoi4`] reads `$NAME$` as a plain scalar:
///
/// ```
/// use jomini::text::lossless::{parse, parse_with, Flavor, SyntaxKind};
///
/// let source = b"building = $NAME$_01";
/// let kind = |flavor| {
///     let parse = parse_with(&source[..], flavor);
///     parse.ast().fields().next().unwrap().value().unwrap().syntax().kind()
/// };
/// assert_eq!(kind(Flavor::default()), SyntaxKind::MacroParam);
/// assert_eq!(kind(Flavor::hoi4()), SyntaxKind::Unquoted);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub struct Flavor {
    /// Recognize `@name` reader variables and `@[ ... ]` calculations.
    pub variables: bool,
    /// Recognize `$NAME$` macro parameters.
    pub macros: bool,
}

impl Default for Flavor {
    fn default() -> Self {
        Flavor::DEFAULT
    }
}

impl Flavor {
    /// The flavor for every game except HoI4. The same as [`Flavor::default`].
    pub const DEFAULT: Flavor = Flavor {
        variables: true,
        macros: true,
    };

    /// Hearts of Iron IV: `@vars`, no `$macros$`.
    pub const fn hoi4() -> Self {
        Flavor {
            variables: true,
            macros: false,
        }
    }

    /// Set [`variables`](Flavor::variables).
    ///
    /// The struct is `#[non_exhaustive]`, so other crates cannot use the
    /// struct update syntax. Use these methods to change one field:
    ///
    /// ```
    /// use jomini::text::lossless::Flavor;
    ///
    /// const FLAVOR: Flavor = Flavor::DEFAULT.with_variables(false);
    /// assert!(!FLAVOR.variables);
    /// assert!(FLAVOR.macros);
    /// ```
    #[must_use]
    pub const fn with_variables(mut self, value: bool) -> Self {
        self.variables = value;
        self
    }

    /// Set [`macros`](Flavor::macros).
    #[must_use]
    pub const fn with_macros(mut self, value: bool) -> Self {
        self.macros = value;
        self
    }
}
