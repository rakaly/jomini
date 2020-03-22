use std::collections::HashMap;

/// Resolves binary 16bit tokens to field names
pub trait TokenResolver {
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
    /// Return an error
    Error,

    /// Stringify the token as hexidecimal
    Stringify,

    /// Ignore the token
    Ignore,
}
