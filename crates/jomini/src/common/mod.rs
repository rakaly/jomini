//! Common data structures used across games
mod date;
mod decimal;

pub(crate) use date::SHORT_DATE_LEN;
pub use date::*;
pub(crate) use decimal::DECIMAL_LEN;
pub use decimal::Decimal;
