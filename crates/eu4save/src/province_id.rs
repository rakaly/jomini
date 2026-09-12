use serde::{Deserialize, Serialize};
use std::fmt;

/// An province numerical identifier
///
/// Handles negative identifiers by taking their absolute value.
///
/// ```rust
/// let _ = eu4save::ProvinceId::new(10);
/// ```
#[derive(
    Debug, Copy, Clone, Serialize, Hash, Eq, PartialEq, Default, Deserialize, PartialOrd, Ord,
)]
#[serde(from = "i32")]
pub struct ProvinceId(i32);

impl ProvinceId {
    pub fn new(x: i32) -> Self {
        ProvinceId(x.abs())
    }

    pub fn as_u16(&self) -> u16 {
        self.0 as u16
    }
}

impl From<i32> for ProvinceId {
    fn from(x: i32) -> Self {
        ProvinceId::new(x)
    }
}

impl fmt::Display for ProvinceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn province_order() {
        assert!(ProvinceId::from(1) < ProvinceId::from(2));
        assert!(ProvinceId::from(1) < ProvinceId::from(-2));
    }
}
