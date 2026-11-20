use crate::{Encoding, Utf8Encoding, Windows1252Encoding};

/// The text syntax of a game, and the encoding of its text.
///
/// A flavor tells the syntax parser which constructs a game's lexer reads,
/// as [`BinaryFlavor`](crate::binary::BinaryFlavor) tells the binary
/// deserializer how a game encodes numbers. Each method defaults to `true`,
/// which fits every game except HoI4, so an encoding is a flavor for those
/// games. HoI4 has no `$NAME$` macro parameters, so its flavor returns
/// `false` from [`macros`](TextFlavor::macros).
///
/// ```
/// use jomini::{Utf8Encoding, Encoding};
/// use jomini::text::TextFlavor;
/// use std::borrow::Cow;
///
/// struct Hoi4;
///
/// impl Encoding for Hoi4 {
///     fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
///         Utf8Encoding::decode(data)
///     }
/// }
///
/// impl TextFlavor for Hoi4 {
///     fn macros(&self) -> bool {
///         false
///     }
/// }
///
/// assert!(!Hoi4.macros());
/// assert!(Utf8Encoding::new().macros());
/// ```
pub trait TextFlavor: Encoding {
    /// Read `@name` reader variables and `@[ ... ]` calculations.
    fn variables(&self) -> bool {
        true
    }

    /// Read `$NAME$` macro parameters.
    fn macros(&self) -> bool {
        true
    }
}

impl TextFlavor for Windows1252Encoding {}

impl TextFlavor for Utf8Encoding {}

impl<T: TextFlavor + ?Sized> TextFlavor for &'_ T {
    fn variables(&self) -> bool {
        (**self).variables()
    }

    fn macros(&self) -> bool {
        (**self).macros()
    }
}

impl<T: TextFlavor + ?Sized> TextFlavor for Box<T> {
    fn variables(&self) -> bool {
        (**self).variables()
    }

    fn macros(&self) -> bool {
        (**self).macros()
    }
}
