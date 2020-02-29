#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Equal,
    Lesser,
    Greater,
    LesserEqual,
    GreaterEqual,
    LesserGreater,
    NotEqual,
}
