use jomini::common::Date;

/// Struct specialized to parsing, formatting, and manipulating dates in EU4
pub use jomini::common::Date as Eu4Date;
pub use jomini::common::PdsDate;

/// EU4's start date
pub fn eu4_start_date() -> Eu4Date {
    Date::from_ymd(1444, 11, 11)
}
