use std::{collections::HashMap, io::BufRead};

use crate::Error;

/// Resolves binary 16bit tokens to field names
///
/// One can create their own `TokenResolver` or rely on the HashMap implementation
///
/// ```
/// use std::collections::HashMap;
/// use jomini::binary::TokenResolver;
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, String::from("field1"));
///
/// assert_eq!(map.resolve(0x2d82), Some("field1"));
/// ```
///
/// The HashMap implementation works with string slices as well
///
/// ```
/// use std::collections::HashMap;
/// use jomini::binary::TokenResolver;
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, "field1");
///
/// assert_eq!(map.resolve(0x0000), None);
/// ```
pub trait TokenResolver {
    /// Return the string field name of the 16bit token if found
    fn resolve(&self, token: u16) -> Option<&str>;

    /// Return whether [`TokenResolver::resolve`] will always return `None`.
    ///
    /// By default this returns `false`
    ///
    /// This method is not used by jomini itself, but rather targeted at
    /// downstream save file libraries, who accept an application configured
    /// [`TokenResolver`]. If the application is not configured for ironman
    /// support, save file parsers can still handle plain text files, so
    /// `is_empty` allows the save parsers to lazily check the validity of a
    /// [`TokenResolver`] when the binary format is encountered. Thus, allowing
    /// for better error messages. Instead of "missing field" errors, the save
    /// file libraries can raise a more descriptive "binary file encountered but
    /// tokens are not configured", as only they know if a non-zero amount of
    /// tokens need to be resolved for a successful deserialization.
    ///
    /// There's not a way for jomini to know whether an empty [`TokenResolver`]
    /// constitutes an error, as the client may only be deserializing data from
    /// keys that are already strings. Or, alternatively, direct token
    /// deserialization is exclusively used.
    fn is_empty(&self) -> bool {
        false
    }
}

impl<S, V> TokenResolver for HashMap<u16, V, S>
where
    S: ::std::hash::BuildHasher,
    V: AsRef<str>,
{
    fn resolve(&self, token: u16) -> Option<&str> {
        self.get(&token).map(|x| x.as_ref())
    }

    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl<T: TokenResolver> TokenResolver for &'_ T {
    fn resolve(&self, token: u16) -> Option<&str> {
        (**self).resolve(token)
    }

    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

impl<T: TokenResolver + ?Sized> TokenResolver for Box<T> {
    fn resolve(&self, token: u16) -> Option<&str> {
        (**self).resolve(token)
    }

    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }
}

/// Customize how the deserializer reacts when a token can't be resolved
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FailedResolveStrategy {
    /// Stop parsing and return an error
    Error,

    /// Stringify the token as hexadecimal
    Stringify,

    /// Ignore the token
    Ignore,
}

/// A basic token resolver that facilitates loading tokens from an external
/// source.
///
/// This token resolver is geared towards testing use cases and iteration.
///
/// It is recommended to use a different implementation if performance is a
/// concern.
pub struct BasicTokenResolver {
    lookup: HashMap<u16, String>,
}

impl BasicTokenResolver {
    /// Create resolver from a `BufRead` implementation over a space delimited
    /// text format:
    ///
    /// ```plain
    /// 0xffff my_test_token
    /// 0xeeee my_test_token2
    /// ```
    pub fn from_text_lines<T>(mut reader: T) -> Result<Self, Error>
    where
        T: BufRead,
    {
        let mut lookup = HashMap::new();
        let mut line = String::new();
        let mut pos = 0;
        while reader.read_line(&mut line)? != 0 {
            let (num, text) = line
                .split_once(' ')
                .ok_or_else(|| Error::invalid_syntax("expected to split line", pos))?;

            let z = u16::from_str_radix(num.trim_start_matches("0x"), 16)
                .map_err(|_| Error::invalid_syntax("invalid ironman token", pos))?;

            pos += line.len();
            lookup.insert(z, String::from(text.trim_ascii_end()));
            line.clear();
        }

        Ok(Self { lookup })
    }
}

impl TokenResolver for BasicTokenResolver {
    fn resolve(&self, token: u16) -> Option<&str> {
        self.lookup.get(&token).map(|x| x.as_str())
    }

    fn is_empty(&self) -> bool {
        self.lookup.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_create_resolve() {
        let data = b"0xffff my_test_token\n0xeeee my_test_token2";
        let resolver = BasicTokenResolver::from_text_lines(&data[..]).unwrap();
        assert_eq!(resolver.resolve(0xffff), Some("my_test_token"));
        assert_eq!(resolver.resolve(0xeeee), Some("my_test_token2"));
    }
}
