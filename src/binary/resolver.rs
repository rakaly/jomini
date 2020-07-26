use std::collections::HashMap;

/// Resolves binary 16bit tokens to field names
///
/// One can create their own `TokenResolver` or rely on the HashMap implementation
///
/// ```
/// use std::collections::HashMap;
/// use jomini::TokenResolver;
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
/// use jomini::TokenResolver;
///
/// let mut map = HashMap::new();
/// map.insert(0x2d82, "field1");
///
/// assert_eq!(map.resolve(0x0000), None);
/// ```
pub trait TokenResolver {
    /// Return the string field name of the 16bit token if found
    fn resolve(&self, token: u16) -> Option<&str>;
}

impl<S, V> TokenResolver for HashMap<u16, V, S>
where
    S: ::std::hash::BuildHasher,
    V: AsRef<str>,
{
    fn resolve(&self, token: u16) -> Option<&str> {
        self.get(&token).map(|x| x.as_ref())
    }
}

impl<'a, S, V> TokenResolver for &'a HashMap<u16, V, S>
where
    S: ::std::hash::BuildHasher,
    V: AsRef<str>,
{
    fn resolve(&self, token: u16) -> Option<&str> {
        self.get(&token).map(|x| x.as_ref())
    }
}

/// Customize how the deserializer reacts when a token can't be resolved
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FailedResolveStrategy {
    /// Stop parsing and return an error
    Error,

    /// Stringify the token as hexadecimal
    Stringify,

    /// Ignore the token
    Ignore,
}
