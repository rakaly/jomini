use std::fmt::Display;

/// An operator token
///
/// This enum contains only non-equal operators due to their rarity. Including
/// an equals operator would increase the size of the token list by up to 50%.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Operator::LessThan => f.write_str("<"),
            Operator::GreaterThan => f.write_str(">"),
            Operator::LessThanEqual => f.write_str("<="),
            Operator::GreaterThanEqual => f.write_str(">="),
            Operator::NotEqual => f.write_str("!="),
            Operator::Exact => f.write_str("=="),
        }
    }
}

impl Operator {
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
    /// ```
    pub fn name(&self) -> &'static str {
        match self {
            Operator::LessThan => "LESS_THAN",
            Operator::LessThanEqual => "LESS_THAN_EQUAL",
            Operator::GreaterThan => "GREATER_THAN",
            Operator::GreaterThanEqual => "GREATER_THAN_EQUAL",
            Operator::Exact => "EXACT",
            Operator::NotEqual => "NOT_EQUAL",
        }
    }
}
