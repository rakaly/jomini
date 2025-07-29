use std::fmt::Display;

/// An operator token
///
/// This enum contains only non-equal operators due to their rarity. Including
/// an equals operator would increase the size of the token list by up to 50%.
#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub enum Operator {
    /// A `<` token
    LessThan,

    /// A `<=` token
    LessThanEqual,

    /// A `>` token
    GreaterThan,

    /// A `>=` token
    GreaterThanEqual,

    /// A `!=` token
    NotEqual,

    /// A `==` token
    Exact,

    /// A `=` token
    Equal,

    /// A `?=` token
    Exists,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.symbol())
    }
}

impl Operator {
    /// Returns the name of the operator using only letters
    ///
    /// ```
    /// use jomini::text::Operator;
    /// assert_eq!(Operator::LessThan.symbol(), "<");
    /// assert_eq!(Operator::LessThanEqual.symbol(), "<=");
    /// assert_eq!(Operator::GreaterThan.symbol(), ">");
    /// assert_eq!(Operator::GreaterThanEqual.symbol(), ">=");
    /// assert_eq!(Operator::Exact.symbol(), "==");
    /// assert_eq!(Operator::NotEqual.symbol(), "!=");
    /// assert_eq!(Operator::Equal.symbol(), "=");
    /// assert_eq!(Operator::Exists.symbol(), "?=");
    /// ```
    pub fn symbol(&self) -> &'static str {
        match self {
            Operator::LessThan => "<",
            Operator::LessThanEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEqual => ">=",
            Operator::Exact => "==",
            Operator::Equal => "=",
            Operator::NotEqual => "!=",
            Operator::Exists => "?=",
        }
    }

    /// Returns the name of the operator using only letters
    ///
    /// ```
    /// use jomini::text::Operator;
    /// assert_eq!(Operator::LessThan.name(), "LESS_THAN");
    /// assert_eq!(Operator::LessThanEqual.name(), "LESS_THAN_EQUAL");
    /// assert_eq!(Operator::GreaterThan.name(), "GREATER_THAN");
    /// assert_eq!(Operator::GreaterThanEqual.name(), "GREATER_THAN_EQUAL");
    /// assert_eq!(Operator::Exact.name(), "EXACT");
    /// assert_eq!(Operator::NotEqual.name(), "NOT_EQUAL");
    /// assert_eq!(Operator::Equal.name(), "EQUAL");
    /// assert_eq!(Operator::Exists.name(), "EXISTS");
    /// ```
    pub fn name(&self) -> &'static str {
        match self {
            Operator::LessThan => "LESS_THAN",
            Operator::LessThanEqual => "LESS_THAN_EQUAL",
            Operator::GreaterThan => "GREATER_THAN",
            Operator::GreaterThanEqual => "GREATER_THAN_EQUAL",
            Operator::Exact => "EXACT",
            Operator::Equal => "EQUAL",
            Operator::NotEqual => "NOT_EQUAL",
            Operator::Exists => "EXISTS",
        }
    }
}

#[cfg(feature = "derive")]
impl<'de> serde::Deserialize<'de> for Operator {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct OperatorVisitor;

        impl<'de> serde::de::Visitor<'de> for OperatorVisitor {
            type Value = Operator;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("an operator")
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "<" => Ok(Operator::LessThan),
                    "<=" => Ok(Operator::LessThanEqual),
                    ">" => Ok(Operator::GreaterThan),
                    ">=" => Ok(Operator::GreaterThanEqual),
                    "==" => Ok(Operator::Exact),
                    "=" => Ok(Operator::Equal),
                    "!=" => Ok(Operator::NotEqual),
                    "?=" => Ok(Operator::Exists),
                    _ => Err(E::custom("did not receive a known operator")),
                }
            }
        }
        deserializer.deserialize_str(OperatorVisitor)
    }
}
