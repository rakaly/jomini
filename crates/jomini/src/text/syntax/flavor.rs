//! The game-specific options of the lexer.

use crate::text::TextFlavor;
use crate::{Encoding, Utf8Encoding};
use std::borrow::Cow;

/// The [`TextFlavor`] of Hearts of Iron IV: UTF-8 text with `@variables`
/// and no `$NAME$` macro parameters, so `$NAME$` is a plain scalar.
///
/// A program that reads HoI4 saves can use the flavor of its save crate
/// instead.
///
/// ```
/// use jomini::Utf8Encoding;
/// use jomini::text::syntax::{parse_with, Hoi4Flavor, SyntaxKind};
///
/// let source = b"building = $NAME$_01";
/// let kind = |parse: jomini::text::syntax::Parse| {
///     parse.ast().fields().next().unwrap().value().unwrap().syntax().kind()
/// };
/// assert_eq!(kind(parse_with(source, &Utf8Encoding::new())), SyntaxKind::MacroParam);
/// assert_eq!(kind(parse_with(source, &Hoi4Flavor)), SyntaxKind::Unquoted);
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Hoi4Flavor;

impl Encoding for Hoi4Flavor {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

impl TextFlavor for Hoi4Flavor {
    fn macros(&self) -> bool {
        false
    }
}

/// The options of a [`TextFlavor`], read once for each parse.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct LexFlavor {
    /// Read `@name` reader variables and `@[ ... ]` calculations.
    pub(crate) variables: bool,
    /// Read `$NAME$` macro parameters.
    pub(crate) macros: bool,
}

impl LexFlavor {
    /// The options of every game except HoI4.
    pub(crate) const DEFAULT: LexFlavor = LexFlavor {
        variables: true,
        macros: true,
    };

    /// The options of [`Hoi4Flavor`].
    #[cfg(test)]
    pub(crate) const HOI4: LexFlavor = LexFlavor {
        variables: true,
        macros: false,
    };

    pub(crate) fn of<F: TextFlavor + ?Sized>(flavor: &F) -> Self {
        LexFlavor {
            variables: flavor.variables(),
            macros: flavor.macros(),
        }
    }
}
