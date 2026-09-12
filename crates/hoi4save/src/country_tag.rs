use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::{fmt, str::FromStr};

use crate::{Hoi4Error, Hoi4ErrorKind};

/// Wrapper around a Country's unique three byte tag
///
/// ```rust
/// use hoi4save::CountryTag;
/// let tag: CountryTag = "ENG".parse()?;
/// assert_eq!(tag.to_string(), String::from("ENG"));
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
#[derive(Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct CountryTag([u8; 3]);

impl CountryTag {
    /// Create a country tag from a byte slice. Returns error if input is not
    /// three bytes in length and not compose of dashes or alphanumeric data.
    ///
    /// ```
    /// use hoi4save::CountryTag;
    /// let tag: CountryTag = CountryTag::create(b"ENG")?;
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn create<T: AsRef<[u8]>>(s: T) -> Result<Self, Hoi4Error> {
        if let [a, b, c] = *s.as_ref() {
            if is_tagc(a) && is_tagc(b) && is_tagc(c) {
                Ok(CountryTag([a, b, c]))
            } else {
                Err(Hoi4Error::new(Hoi4ErrorKind::CountryTagInvalidCharacters))
            }
        } else {
            Err(Hoi4Error::new(Hoi4ErrorKind::CountryTagIncorrectSize))
        }
    }

    /// An ergonomic shortcut to determine if input byte slice contains the same
    /// data as the tag
    /// ```
    /// use hoi4save::CountryTag;
    /// let tag: CountryTag = CountryTag::create(b"ENG")?;
    /// assert!(tag.is(b"ENG"));
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn is<T: AsRef<[u8]>>(&self, s: T) -> bool {
        self.as_bytes() == s.as_ref()
    }

    /// Returns the country tag as a byte slice
    /// ```
    /// use hoi4save::CountryTag;
    /// let tag: CountryTag = CountryTag::create(b"ENG")?;
    /// assert_eq!(tag.as_bytes(), b"ENG");
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }

    /// Returns the country tag as a string slice
    /// ```
    /// use hoi4save::CountryTag;
    /// let tag: CountryTag = CountryTag::create(b"ENG")?;
    /// assert_eq!(tag.as_str(), "ENG");
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    pub fn as_str(&self) -> &str {
        // We know that this is safe as the CountryTag constructor only allows
        // ascii alphanumeric and dashes
        debug_assert!(std::str::from_utf8(&self.0).is_ok());
        unsafe { std::str::from_utf8_unchecked(&self.0) }
    }
}

#[inline]
pub(crate) const fn is_tagc(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'-'
}

impl FromStr for CountryTag {
    type Err = Hoi4Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        CountryTag::create(s)
    }
}

impl AsRef<str> for CountryTag {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for CountryTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl fmt::Display for CountryTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl Serialize for CountryTag {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_ref())
    }
}

impl<'de> Deserialize<'de> for CountryTag {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct CountryTagVisitor;

        impl de::Visitor<'_> for CountryTagVisitor {
            type Value = CountryTag;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryTag")
            }

            fn visit_str<A>(self, v: &str) -> Result<Self::Value, A>
            where
                A: de::Error,
            {
                v.parse().map_err(de::Error::custom)
            }
        }

        deserializer.deserialize_str(CountryTagVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_order() {
        let tag1: CountryTag = "AAA".parse().unwrap();
        let tag2: CountryTag = "BBB".parse().unwrap();
        assert!(tag1 < tag2);
    }

    #[test]
    fn parse_blank_tag() {
        let tag1: CountryTag = "---".parse().unwrap();
        assert_eq!(tag1.to_string(), String::from("---"));
    }

    #[test]
    fn tag_debug_representation() {
        let tag1: CountryTag = "FRA".parse().unwrap();
        let debug = format!("{:?}", tag1);
        assert_eq!(debug, String::from("FRA"));
    }
}
