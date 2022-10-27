use crate::scalar::{to_i64_t, to_u64_t};
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display};
use std::str::FromStr;

/// A date error.
#[derive(Debug, PartialEq, Eq)]
pub struct DateError;

impl std::error::Error for DateError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl std::fmt::Display for DateError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "unable to decode date")
    }
}

const DAYS_PER_MONTH: [u8; 13] = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

/// Common set of methods between all the date components
pub trait PdsDate {
    /// Return the year
    fn year(&self) -> i16;

    /// Returns the month. Range: [1, 12]
    fn month(&self) -> u8;

    /// Return the day
    fn day(&self) -> u8;

    /// Formats the date in the game format
    fn game_fmt(&self) -> PdsDateFormatter;

    /// Formats the date in an iso8601 format
    fn iso_8601(&self) -> PdsDateFormatter;
}

/// Controls the output format of a date
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateFormat {
    /// ISO-8601 format
    Iso8601,

    /// Y.M.D[.H] where month, day, and hour don't have zero padding
    DotShort,

    /// Y.M.D[.H] where month, day, and hour are zero padded to two digits
    DotWide,
}

/// A temporary object which can be used as an argument to `format!`.
///
/// Used to avoid a needless intermediate allocation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PdsDateFormatter {
    raw: RawDate,
    format: DateFormat,
}

impl PdsDateFormatter {
    /// Creates new formatter with a given date and desired format
    pub fn new(raw: RawDate, format: DateFormat) -> Self {
        Self { raw, format }
    }
}

impl Display for PdsDateFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.format == DateFormat::Iso8601 {
            write!(
                f,
                "{:04}-{:02}-{:02}",
                self.raw.year(),
                self.raw.month(),
                self.raw.day(),
            )?;

            if self.raw.has_hour() {
                write!(f, "T{:02}", self.raw.hour() - 1)
            } else {
                Ok(())
            }
        } else {
            let fmt = self.format;
            let width = if fmt == DateFormat::DotWide { 2 } else { 0 };
            write!(
                f,
                "{}.{:03$}.{:03$}",
                self.raw.year(),
                self.raw.month(),
                self.raw.day(),
                width,
            )?;

            if self.raw.has_hour() {
                write!(f, ".{:01$}", self.raw.hour(), width)
            } else {
                Ok(())
            }
        }
    }
}

/// A [RawDate] where each component is a full data type
#[derive(Debug)]
struct ExpandedRawDate {
    year: i16,
    month: u8,
    day: u8,
    hour: u8,
}

impl ExpandedRawDate {
    fn from_binary(mut s: i32) -> Option<Self> {
        // quite annoying that the binary format uses a 24 hour clock
        // indexed at 0 so it is up to a higher level API to determine
        // to map the hour to 1 based.
        let hour = s % 24;
        s /= 24;

        let days_since_jan1 = s % 365;
        if hour < 0 || days_since_jan1 < 0 {
            return None;
        }

        s /= 365;
        let year = s.checked_sub(5000).and_then(|x| i16::try_from(x).ok())?;
        let (month, day) = month_day_from_julian(days_since_jan1);
        Some(ExpandedRawDate {
            year,
            month,
            day,
            hour: hour as u8,
        })
    }

    fn parse<T: AsRef<[u8]>>(s: T) -> Option<Self> {
        let data = s.as_ref();

        let (year, data) = to_i64_t(data).ok()?;
        if data.is_empty() {
            return i32::try_from(year).ok().and_then(Self::from_binary);
        }

        let year = i16::try_from(year).ok()?;
        let (delim1, data) = data.split_first()?;

        let (month, data) = to_u64_t(data, 0).ok()?;
        let month = u8::try_from(month).ok()?;
        let (delim2, data) = data.split_first()?;

        let (day, data) = to_u64_t(data, 0).ok()?;
        let day = u8::try_from(day).ok()?;

        if data.is_empty() && *delim1 == b'.' && *delim2 == b'.' {
            return Some(ExpandedRawDate {
                year,
                month,
                day,
                hour: 0,
            });
        }

        let (delim3, data) = data.split_first()?;
        let (hour, data) = to_u64_t(data, 0).ok()?;
        let hour = u8::try_from(hour).ok()?;

        if data.is_empty() && hour != 0 && *delim1 == b'.' && *delim2 == b'.' && *delim3 == b'.' {
            Some(ExpandedRawDate {
                year,
                month,
                day,
                hour,
            })
        } else {
            None
        }
    }
}

/// Common implementation between the different date and time formats.
///
/// Space optimized to only need 4 bytes
///
/// It may or may not have an hour component.
///
/// Paradox games do not follow any traditional calendar and instead view the
/// world on simpler terms: that every year should be treated as a non-leap
/// year.
///
/// Years can be negative but can't be zero.
///
/// An hour component is considered present if it is non-zero. This means that
/// games with hours run on a non-traditional clock from 1-24 instead of the
/// traditional 24 hour clock (0-23). An exception is Victoria 3, which has day
/// cycle of (0, 6, 12, 18) hours, but remains consistent in omitting the hour
/// when it is zero.
///
/// A raw date has very minimal validation and can support any calendar system
/// as it holds abitrary values for year, month, day, and hours
///
/// It is typically recommended to use one of the specialized types: [Date],
/// [DateHour], or [UniformDate] as date formats aren't variable within a game
/// and have less pitfalls.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct RawDate {
    year: i16,

    // month: 4 bits
    // day: 5 bits
    // hour: 5 bits
    // empty: 2 bits
    data: u16,
}

impl Debug for RawDate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RawDate {{ year: {} month: {} day: {} hour: {} }}",
            self.year(),
            self.month(),
            self.day(),
            self.hour()
        )
    }
}

impl PartialOrd for RawDate {
    fn partial_cmp(&self, other: &RawDate) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RawDate {
    fn cmp(&self, other: &RawDate) -> Ordering {
        self.year()
            .cmp(&other.year())
            .then_with(|| self.data.cmp(&other.data))
    }
}

impl RawDate {
    fn from_expanded(data: ExpandedRawDate) -> Option<Self> {
        Self::from_ymdh_opt(data.year, data.month, data.day, data.hour)
    }

    /// Creates a raw date from a binary integer
    ///
    /// ```
    /// use jomini::common::{RawDate, PdsDate};
    /// let date = RawDate::from_binary(56379360).unwrap();
    /// assert_eq!(date.iso_8601().to_string(), String::from("1436-01-01"));
    ///
    /// let date2 = RawDate::from_binary(60759371).unwrap();
    /// assert_eq!(date2.iso_8601().to_string(), String::from("1936-01-01T10"));
    /// ```
    pub fn from_binary(s: i32) -> Option<Self> {
        ExpandedRawDate::from_binary(s).and_then(Self::from_expanded)
    }

    /// Create a raw date from individual components.
    ///
    /// Will return none for an invalid date
    pub fn from_ymdh_opt(year: i16, month: u8, day: u8, hour: u8) -> Option<Self> {
        if month != 0 && month < 13 && day != 0 && day < 32 && hour < 25 {
            let data = (u16::from(month) << 12) + (u16::from(day) << 7) + (u16::from(hour) << 2);
            Some(RawDate { year, data })
        } else {
            None
        }
    }

    /// Create a raw date from individual components.
    ///
    /// Will panic on invalid dates
    pub fn from_ymdh(year: i16, month: u8, day: u8, hour: u8) -> Self {
        Self::from_ymdh_opt(year, month, day, hour).unwrap()
    }

    /// Return the hour component. Range [1, 24]. If zero, then there is no hour
    pub fn hour(&self) -> u8 {
        ((self.data >> 2) & 0x1f) as u8
    }

    /// Return if this date has an hour component
    pub fn has_hour(&self) -> bool {
        self.data & 0x7c != 0
    }

    /// Parses date components from the following formatted text:
    ///
    /// - `Y.M.D`
    /// - `Y.M.D.H`
    /// - `YYYY.MM.DD.HH`
    /// - or any variation of the above
    ///
    /// A zero component for the hour is disallowed, so the hour
    /// must be omitted when parsing to only a date without a time component.
    ///
    /// Unlike [`Date::parse`], this will not parse the textual form of the
    /// date's binary representation.
    pub fn parse<T: AsRef<[u8]>>(s: T) -> Result<Self, DateError> {
        ExpandedRawDate::parse(&s)
            .and_then(Self::from_expanded)
            .and_then(|x| {
                if to_i64_t(s.as_ref()).ok()?.1.is_empty() {
                    None
                } else {
                    Some(x)
                }
            })
            .ok_or(DateError)
    }
}

impl PdsDate for RawDate {
    /// Return year of date
    fn year(&self) -> i16 {
        self.year
    }

    /// Return month of date
    fn month(&self) -> u8 {
        (self.data >> 12) as u8
    }

    /// Return day of date
    fn day(&self) -> u8 {
        ((self.data >> 7) & 0x1f) as u8
    }

    fn game_fmt(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(*self, DateFormat::DotShort)
    }

    fn iso_8601(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(*self, DateFormat::Iso8601)
    }
}

impl FromStr for RawDate {
    type Err = DateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s.as_bytes())
    }
}

/// A date without a time component
///
/// See [RawDate] for additional date / time commentary
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Date {
    raw: RawDate,
}

impl Debug for Date {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Date {}", self.game_fmt())
    }
}

impl Date {
    fn from_expanded(date: ExpandedRawDate) -> Option<Self> {
        if date.hour != 0 {
            None
        } else {
            Self::from_ymd_opt(date.year, date.month, date.day)
        }
    }

    fn days(&self) -> i32 {
        let month_days = julian_ordinal_day(self.month());
        let year_day = i32::from(self.year()) * 365;
        if year_day < 0 {
            year_day - month_days - i32::from(self.day())
        } else {
            year_day + month_days + i32::from(self.day())
        }
    }

    /// Create a new date from year, month, and day parts
    ///
    /// Will return `None` if the date does not exist
    ///
    /// ```
    /// use jomini::common::Date;
    /// assert_eq!(Date::from_ymd_opt(1444, 11, 11), Some(Date::from_ymd(1444, 11, 11)));
    /// assert_eq!(Date::from_ymd_opt(800, 5, 3), Some(Date::from_ymd(800, 5, 3)));
    /// assert!(Date::from_ymd_opt(800, 0, 3).is_none());
    /// assert!(Date::from_ymd_opt(800, 1, 0).is_none());
    /// assert!(Date::from_ymd_opt(800, 13, 1).is_none());
    /// assert!(Date::from_ymd_opt(800, 12, 32).is_none());
    /// assert!(Date::from_ymd_opt(2020, 2, 29).is_none());
    /// ```
    pub fn from_ymd_opt(year: i16, month: u8, day: u8) -> Option<Self> {
        RawDate::from_ymdh_opt(year, month, day, 0).and_then(|raw| {
            let days = DAYS_PER_MONTH[usize::from(month)];
            if day <= days {
                Some(Date { raw })
            } else {
                None
            }
        })
    }

    /// Create a new date from year, month, and day parts
    ///
    /// Will panic if the date does not exist.
    pub fn from_ymd(year: i16, month: u8, day: u8) -> Self {
        Self::from_ymd_opt(year, month, day).unwrap()
    }

    /// Parses a string and returns a new [`Date`] if valid. The expected
    /// format is either YYYY.MM.DD or a number representing of the equivalent
    /// binary representation.
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::parse("1444.11.11").expect("to parse date");
    /// assert_eq!(date.year(), 1444);
    /// assert_eq!(date.month(), 11);
    /// assert_eq!(date.day(), 11);
    /// ```
    pub fn parse<T: AsRef<[u8]>>(s: T) -> Result<Self, DateError> {
        ExpandedRawDate::parse(s)
            .and_then(Self::from_expanded)
            .ok_or(DateError)
    }

    /// Returns the number of days between two dates
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse("1400.1.2").unwrap();
    /// let date2 = Date::parse("1400.1.3").unwrap();
    /// let date3 = Date::parse("1401.1.2").unwrap();
    /// let date4 = Date::parse("1401.12.31").unwrap();
    /// assert_eq!(1, date.days_until(&date2));
    /// assert_eq!(365, date.days_until(&date3));
    /// assert_eq!(728, date.days_until(&date4));
    /// assert_eq!(-728, date4.days_until(&date));
    /// ```
    pub fn days_until(self, other: &Date) -> i32 {
        other.days() - self.days()
    }

    /// Return a new date that is the given number of days in the future
    /// from the current date
    ///
    /// ```
    /// use jomini::common::Date;
    ///
    /// let date = Date::parse("1400.1.2").unwrap();
    /// let expected = Date::parse("1400.1.3").unwrap();
    /// let expected2 = Date::parse("1400.1.1").unwrap();
    /// assert_eq!(expected, date.add_days(1));
    /// assert_eq!(expected2, date.add_days(-1));
    /// ```
    ///
    /// Will panic on overflow or underflow.
    pub fn add_days(self, days: i32) -> Date {
        let new_days = self
            .days()
            .checked_add(days)
            .expect("adding days overflowed");

        let days_since_jan1 = (new_days % 365).abs();
        let year = new_days / 365;
        let (month, day) = month_day_from_julian(days_since_jan1);

        let year = i16::try_from(year).expect("year to fit inside signed 16bits");
        Date {
            raw: RawDate::from_ymdh(year, month, day, self.raw.hour()),
        }
    }

    /// Decodes a date from a number that had been parsed from binary data
    ///
    /// The hour component, if present, will be ignored
    pub fn from_binary(s: i32) -> Option<Self> {
        // I've not yet found a binary date that has an hour component but shouldn't
        // but for consistency sake we zero out the hour so that we maintain the
        // invariant that a Date does not have an hour component
        ExpandedRawDate::from_binary(s)
            .map(|x| ExpandedRawDate { hour: 0, ..x })
            .and_then(Self::from_expanded)
    }

    /// Decodes a date from a number that had been parsed from binary data with the
    /// added check that the date is not too far fetched. This function is useful
    /// when working with binary data and it's not clear with an encountered integer
    /// is supposed to represent a date or a number.
    ///
    /// We use -100 as a cut off dates for years. Antonio I (EU4) holds the
    /// record with a birth date of `-58.1.1`. The exception is monuments,
    /// which date back to -2500 or even farther back (mods), but this
    /// function is just a heuristic so direct any extreme dates towards
    /// [`Date::from_binary`].
    pub fn from_binary_heuristic(s: i32) -> Option<Self> {
        ExpandedRawDate::from_binary(s).and_then(|x| {
            if x.year > -100 {
                Self::from_expanded(x)
            } else {
                None
            }
        })
    }

    /// Converts a date into the binary representation
    ///
    /// ```rust
    /// use jomini::common::Date;
    /// let date = Date::from_ymd(1, 1, 1);
    /// assert_eq!(43808760, date.to_binary());
    /// ```
    pub fn to_binary(self) -> i32 {
        let ordinal_day = julian_ordinal_day(self.month()) + i32::from(self.day());
        to_binary(self.year(), ordinal_day, 0)
    }
}

impl PdsDate for Date {
    /// Year of the date
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::from_ymd(1444, 2, 3);
    /// assert_eq!(date.year(), 1444);
    /// ```
    fn year(&self) -> i16 {
        self.raw.year()
    }

    /// Month of the date
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::from_ymd(1444, 2, 3);
    /// assert_eq!(date.month(), 2);
    /// ```
    fn month(&self) -> u8 {
        self.raw.month()
    }

    /// Day of the date
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::from_ymd(1444, 2, 3);
    /// assert_eq!(date.day(), 3);
    /// ```
    fn day(&self) -> u8 {
        self.raw.day()
    }

    /// Formats a date in the ISO 8601 format: YYYY-MM-DD
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::from_ymd(1400, 1, 2);
    /// assert_eq!(date.iso_8601().to_string(), String::from("1400-01-02"));
    /// ```
    fn iso_8601(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::Iso8601)
    }

    /// Formats a date in the game format: Y.M.D
    ///
    /// ```
    /// use jomini::common::{Date, PdsDate};
    /// let date = Date::from_ymd(1400, 1, 2);
    /// assert_eq!(date.game_fmt().to_string(), String::from("1400.1.2"));
    /// ```
    fn game_fmt(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::DotShort)
    }
}

impl FromStr for Date {
    type Err = DateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s.as_bytes())
    }
}

/// A date with an hour component
///
/// Geared towards the hearts of iron games.
///
/// See [RawDate] for additional date / time commentary
///
/// ```rust
/// use jomini::common::{DateHour, PdsDate};
/// let date = DateHour::from_ymdh(1936, 1, 1, 24);
/// let iso = date.iso_8601().to_string();
/// assert_eq!(iso, String::from("1936-01-01T23"));
/// ```
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DateHour {
    raw: RawDate,
}

impl Debug for DateHour {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DateHour {}", self.game_fmt())
    }
}

impl DateHour {
    #[inline]
    fn from_expanded(date: ExpandedRawDate) -> Option<Self> {
        Self::from_ymdh_opt(date.year, date.month, date.day, date.hour)
    }

    /// Create a new [DateHour] from individual components
    ///
    /// The hour is expected to be non-zero in addition to the validation
    /// that a regular [Date] object undergoes.
    ///
    /// ```rust
    /// use jomini::common::DateHour;
    /// assert!(DateHour::from_ymdh_opt(1936, 1, 1, 24).is_some());
    /// assert!(DateHour::from_ymdh_opt(1936, 1, 1, 0).is_none());
    /// ```
    #[inline]
    pub fn from_ymdh_opt(year: i16, month: u8, day: u8, hour: u8) -> Option<Self> {
        RawDate::from_ymdh_opt(year, month, day, hour).and_then(|raw| {
            let days = DAYS_PER_MONTH[usize::from(month)];
            if hour > 0 && day <= days {
                Some(Self { raw })
            } else {
                None
            }
        })
    }

    /// ```rust
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 3, 12);
    /// assert_eq!(date.day(), 3);
    /// ```
    #[inline]
    pub fn from_ymdh(year: i16, month: u8, day: u8, hour: u8) -> Self {
        Self::from_ymdh_opt(year, month, day, hour).unwrap()
    }

    /// hour of the date. Range: [1, 24]
    ///
    /// ```
    /// use jomini::common::DateHour;
    /// let date = DateHour::from_ymdh(1936, 1, 2, 12);
    /// assert_eq!(date.hour(), 12);
    /// ```
    pub fn hour(&self) -> u8 {
        // we know that this is > 0, per DateHour invariant
        self.raw.hour()
    }

    /// Parse a [DateHour] from text. Follows the same logic as [RawDate::parse]
    /// except an hour component is enforced.
    ///
    /// ```rust
    /// use jomini::common::DateHour;
    /// assert_eq!(DateHour::parse("1936.1.1.24"), Ok(DateHour::from_ymdh(1936, 1, 1, 24)));
    /// ```
    #[inline]
    pub fn parse<T: AsRef<[u8]>>(s: T) -> Result<Self, DateError> {
        ExpandedRawDate::parse(s)
            .and_then(Self::from_expanded)
            .ok_or(DateError)
    }

    /// Decode a number extracted from the binary format into a date.
    #[inline]
    pub fn from_binary(s: i32) -> Option<Self> {
        ExpandedRawDate::from_binary(s).and_then(|mut raw| {
            // Shift hour from 0 based 24 hour clock to 1 based 24 hour clock
            raw.hour += 1;
            Self::from_expanded(raw)
        })
    }

    /// Decode a number extracted from the binary format into a date, but
    /// ensure that the date is at least in the 1800's (an arbitrary chosen
    /// value to support HOI4 mods that move the start date up). There is a
    /// special exception made for 1.1.1.1 and -1.1.1.1 which represents an
    /// event that has not occurred yet.
    #[inline]
    pub fn from_binary_heuristic(s: i32) -> Option<Self> {
        Self::from_binary(s).and_then(|x| {
            let is_min_year = x.year() == 1 || x.year() == -1;
            let is_min_date = is_min_year && x.month() == 1 && x.day() == 1 && x.hour() == 1;
            if x.year() < 1800 && !is_min_date {
                None
            } else {
                Some(x)
            }
        })
    }

    /// Converts a date into the binary representation
    ///
    /// ```rust
    /// use jomini::common::DateHour;
    /// let date = DateHour::from_ymdh(1, 1, 1, 1);
    /// assert_eq!(43808760, date.to_binary());
    /// ```
    #[inline]
    pub fn to_binary(self) -> i32 {
        let ordinal_day = julian_ordinal_day(self.month()) + i32::from(self.day());
        to_binary(self.year(), ordinal_day, self.hour())
    }
}

impl PdsDate for DateHour {
    /// Year of the date
    ///
    /// ```
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 2, 24);
    /// assert_eq!(date.year(), 1936);
    /// ```
    fn year(&self) -> i16 {
        self.raw.year()
    }

    /// Month of the date
    ///
    /// ```
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 2, 24);
    /// assert_eq!(date.month(), 1);
    /// ```
    fn month(&self) -> u8 {
        self.raw.month()
    }

    /// Day of the date
    ///
    /// ```
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 2, 24);
    /// assert_eq!(date.day(), 2);
    /// ```
    fn day(&self) -> u8 {
        self.raw.day()
    }

    /// Return the date as an iso8601 compatible string
    ///
    /// The hour component is converted to a range of [0, 23] per the spec
    ///
    /// ```rust
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 2, 12);
    /// assert_eq!(String::from("1936-01-02T11"), date.iso_8601().to_string());
    /// ```
    fn iso_8601(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::Iso8601)
    }

    /// Return the date in the game format
    ///
    /// ```rust
    /// use jomini::common::{DateHour, PdsDate};
    /// let date = DateHour::from_ymdh(1936, 1, 2, 12);
    /// assert_eq!(String::from("1936.1.2.12"), date.game_fmt().to_string());
    /// ```
    fn game_fmt(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::DotShort)
    }
}

impl FromStr for DateHour {
    type Err = DateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s.as_bytes())
    }
}

/// A date without a time component where each month has 30 days
///
/// Useful for Stellaris
///
/// See [RawDate] for additional date / time commentary
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniformDate {
    raw: RawDate,
}

impl Debug for UniformDate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UniformDate {}", self.game_fmt())
    }
}

impl UniformDate {
    #[inline]
    fn from_expanded(date: ExpandedRawDate) -> Option<Self> {
        if date.hour != 0 {
            None
        } else {
            Self::from_ymd_opt(date.year, date.month, date.day)
        }
    }

    /// Create a new date from year, month, and day parts
    ///
    /// Will return `None` if the date does not exist
    ///
    /// ```
    /// use jomini::common::{PdsDate, UniformDate};
    /// assert_eq!(UniformDate::from_ymd_opt(1444, 11, 11), Some(UniformDate::from_ymd(1444, 11, 11)));
    /// assert_eq!(UniformDate::from_ymd_opt(800, 5, 3), Some(UniformDate::from_ymd(800, 5, 3)));
    /// assert!(UniformDate::from_ymd_opt(800, 0, 3).is_none());
    /// assert!(UniformDate::from_ymd_opt(800, 1, 0).is_none());
    /// assert!(UniformDate::from_ymd_opt(800, 13, 1).is_none());
    /// assert!(UniformDate::from_ymd_opt(800, 12, 32).is_none());
    /// assert!(UniformDate::from_ymd_opt(2020, 2, 29).is_some());
    /// ```
    #[inline]
    pub fn from_ymd_opt(year: i16, month: u8, day: u8) -> Option<Self> {
        if day > 30 {
            None
        } else {
            RawDate::from_ymdh_opt(year, month, day, 0).map(|raw| Self { raw })
        }
    }

    /// Create a new date from year, month, and day parts
    ///
    /// Will panic if the date does not exist.
    #[inline]
    pub fn from_ymd(year: i16, month: u8, day: u8) -> Self {
        Self::from_ymd_opt(year, month, day).unwrap()
    }

    /// Parse a [DateHour] from text. Follows the same logic as [RawDate::parse]
    /// except that a 12 month calendar of 30 days is enforced.
    ///
    /// ```rust
    /// use jomini::common::UniformDate;
    /// assert_eq!(UniformDate::parse("2200.02.30"), Ok(UniformDate::from_ymd(2200, 2, 30)));
    /// ```
    #[inline]
    pub fn parse<T: AsRef<[u8]>>(s: T) -> Result<Self, DateError> {
        ExpandedRawDate::parse(s)
            .and_then(Self::from_expanded)
            .ok_or(DateError)
    }
}

impl PdsDate for UniformDate {
    /// Year of the date
    ///
    /// ```
    /// use jomini::common::{UniformDate, PdsDate};
    /// let date = UniformDate::from_ymd(1444, 2, 3);
    /// assert_eq!(date.year(), 1444);
    /// ```
    fn year(&self) -> i16 {
        self.raw.year()
    }

    /// Month of the date
    ///
    /// ```
    /// use jomini::common::{UniformDate, PdsDate};
    /// let date = UniformDate::from_ymd(1444, 2, 3);
    /// assert_eq!(date.month(), 2);
    /// ```
    fn month(&self) -> u8 {
        self.raw.month()
    }

    /// Day of the date
    ///
    /// ```
    /// use jomini::common::{UniformDate, PdsDate};
    /// let date = UniformDate::from_ymd(1444, 2, 3);
    /// assert_eq!(date.day(), 3);
    /// ```
    fn day(&self) -> u8 {
        self.raw.day()
    }

    /// Formats a date in the ISO 8601 format: YYYY-MM-DD
    ///
    /// ```
    /// use jomini::common::{UniformDate, PdsDate};
    /// let date = UniformDate::from_ymd(1400, 1, 2);
    /// assert_eq!(date.iso_8601().to_string(), String::from("1400-01-02"));
    /// ```
    fn iso_8601(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::Iso8601)
    }

    /// Formats a date in the game format: Y.MM.DD
    ///
    /// ```
    /// use jomini::common::{UniformDate, PdsDate};
    /// let date = UniformDate::from_ymd(1400, 1, 2);
    /// assert_eq!(date.game_fmt().to_string(), String::from("1400.01.02"));
    /// ```
    fn game_fmt(&self) -> PdsDateFormatter {
        PdsDateFormatter::new(self.raw, DateFormat::DotWide)
    }
}

impl FromStr for UniformDate {
    type Err = DateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s.as_bytes())
    }
}

fn month_day_from_julian(days_since_jan1: i32) -> (u8, u8) {
    // https://landweb.modaps.eosdis.nasa.gov/browse/calendar.html
    // except we start at 0 instead of 1
    let (month, day) = match days_since_jan1 {
        0..=30 => (1, days_since_jan1 + 1),
        31..=58 => (2, days_since_jan1 - 30),
        59..=89 => (3, days_since_jan1 - 58),
        90..=119 => (4, days_since_jan1 - 89),
        120..=150 => (5, days_since_jan1 - 119),
        151..=180 => (6, days_since_jan1 - 150),
        181..=211 => (7, days_since_jan1 - 180),
        212..=242 => (8, days_since_jan1 - 211),
        243..=272 => (9, days_since_jan1 - 242),
        273..=303 => (10, days_since_jan1 - 272),
        304..=333 => (11, days_since_jan1 - 303),
        334..=364 => (12, days_since_jan1 - 333),
        _ => unreachable!(),
    };

    debug_assert!(day < 255);
    (month, day as u8)
}

fn julian_ordinal_day(month: u8) -> i32 {
    match month {
        1 => -1,
        2 => 30,
        3 => 58,
        4 => 89,
        5 => 119,
        6 => 150,
        7 => 180,
        8 => 211,
        9 => 242,
        10 => 272,
        11 => 303,
        12 => 333,
        _ => unreachable!(),
    }
}

fn to_binary(year: i16, ordinal_day: i32, hour: u8) -> i32 {
    let year_part = (i32::from(year) + 5000) * 365;
    let hour = i32::from(hour.saturating_sub(1));
    (year_part + ordinal_day) * 24 + hour
}

#[cfg(feature = "derive")]
mod datederive {
    use super::{Date, DateHour, PdsDate, UniformDate};
    use serde::{de, de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt;

    impl Serialize for Date {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.iso_8601().to_string().as_str())
        }
    }

    struct DateVisitor;

    impl<'de> Visitor<'de> for DateVisitor {
        type Value = Date;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a date")
        }

        fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Date::from_binary(v)
                .ok_or_else(|| de::Error::custom(format!("invalid binary date: {}", v)))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            Date::parse(v).map_err(|_e| de::Error::custom(format!("invalid date: {}", v)))
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            self.visit_str(v.as_str())
        }
    }

    impl<'de> Deserialize<'de> for Date {
        fn deserialize<D>(deserializer: D) -> Result<Date, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(DateVisitor)
        }
    }

    impl Serialize for DateHour {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.iso_8601().to_string().as_str())
        }
    }

    struct DateHourVisitor;

    impl<'de> Visitor<'de> for DateHourVisitor {
        type Value = DateHour;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a date hour")
        }

        fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            DateHour::from_binary(v)
                .ok_or_else(|| de::Error::custom(format!("invalid binary date hour: {}", v)))
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            DateHour::parse(v).map_err(|_e| de::Error::custom(format!("invalid date hour: {}", v)))
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            self.visit_str(v.as_str())
        }
    }

    impl<'de> Deserialize<'de> for DateHour {
        fn deserialize<D>(deserializer: D) -> Result<DateHour, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(DateHourVisitor)
        }
    }

    struct UniformDateVisitor;

    impl<'de> Visitor<'de> for UniformDateVisitor {
        type Value = UniformDate;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a uniform date")
        }

        fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            UniformDate::parse(v)
                .map_err(|_e| de::Error::custom(format!("invalid uniform date: {}", v)))
        }

        fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
        where
            E: de::Error,
        {
            self.visit_str(v.as_str())
        }
    }

    impl<'de> Deserialize<'de> for UniformDate {
        fn deserialize<D>(deserializer: D) -> Result<UniformDate, D::Error>
        where
            D: Deserializer<'de>,
        {
            deserializer.deserialize_any(UniformDateVisitor)
        }
    }
}

#[cfg(not(feature = "derive"))]
mod datederive {}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn test_date_roundtrip() {
        let date = Date::parse("1400.1.2").unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1400-01-02"));
    }

    #[test]
    fn test_date_parse() {
        assert_eq!(Date::parse("1.01.01").unwrap(), Date::from_ymd(1, 1, 1));
    }

    #[test]
    fn test_game_fmt() {
        let test_cases = [
            "1400.1.2",
            "1457.3.5",
            "1.1.1",
            "1444.11.11",
            "1444.11.30",
            "1444.2.19",
        ];

        for case in &test_cases {
            let date = Date::parse(case).unwrap();
            assert_eq!(date.game_fmt().to_string(), case.to_string());
        }
    }

    #[test]
    fn test_first_bin_date() {
        let date = Date::from_binary(56379360).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1436-01-01"));
    }

    #[test]
    fn test_text_date_overflow() {
        assert!(Date::parse("1444.257.1").is_err());
        assert!(Date::parse("1444.1.257").is_err());
        assert!(Date::parse("60000.1.1").is_err());
        assert!(Date::parse("-60000.1.1").is_err());
    }

    #[test]
    fn test_binary_date_overflow() {
        assert_eq!(Date::from_binary(999379360), None);
    }

    #[test]
    #[should_panic]
    fn test_add_adds_year_overflow() {
        let date = Date::parse("1400.1.2").unwrap();
        let _ = date.add_days(100000000);
    }

    #[test]
    #[should_panic]
    fn test_add_adds_day_overflow() {
        let date = Date::parse("1400.1.2").unwrap();
        let _ = date.add_days(i32::MAX);
    }

    #[test]
    fn test_ignore_bin_dates() {
        // These are numbers from a savefile that shouldn't be interpreted as dates
        assert_eq!(Date::from_binary_heuristic(0), None);
        assert_eq!(Date::from_binary_heuristic(380947), None);
        assert_eq!(Date::from_binary_heuristic(21282204), None);
        assert_eq!(Date::from_binary_heuristic(33370842), None);
        assert_eq!(Date::from_binary_heuristic(42267422), None);
        assert_eq!(Date::from_binary_heuristic(693362154), None);
    }

    #[test]
    fn test_negative_date() {
        // EU4 Monarch birth dates can be negative, no idea what those mean
        let date = Date::parse("-17.1.1").unwrap();
        assert_eq!(date.game_fmt().to_string(), String::from("-17.1.1"));

        let date2 = Date::from_binary(43651080).unwrap();
        assert_eq!(date.game_fmt().to_string(), String::from("-17.1.1"));

        assert_eq!(date, date2);
    }

    #[test]
    fn test_zero_date() {
        let date = Date::from_binary(43800000).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("0000-01-01"));
    }

    #[test]
    fn test_negative_datehour_binary() {
        let date = DateHour::from_binary(43791240).unwrap();
        assert_eq!(date.game_fmt().to_string(), String::from("-1.1.1.1"));
        assert_eq!(Some(date), DateHour::from_binary_heuristic(43791240));
    }

    #[test]
    fn test_very_negative_date() {
        // EU4 stonehenge and pyramids
        let date = Date::parse("-2500.1.1").unwrap();
        assert_eq!(date.game_fmt().to_string(), String::from("-2500.1.1"));

        let date2 = Date::from_binary(21900000).unwrap();
        assert_eq!(date2.game_fmt().to_string(), String::from("-2500.1.1"));
        assert_eq!(date, date2);
    }

    #[test]
    fn test_very_negative_date2() {
        // EU4 monuments expanded
        let date = Date::parse("-10000.1.1").unwrap();
        assert_eq!(date.game_fmt().to_string(), String::from("-10000.1.1"));

        let date2 = Date::from_binary(-43800000).unwrap();
        assert_eq!(date2.game_fmt().to_string(), String::from("-10000.1.1"));
        assert_eq!(date, date2);
    }

    #[test]
    fn test_november_date_regression() {
        let date = Date::from_binary(56379360).unwrap().add_days(303);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-10-31"));
        let date = Date::from_binary(56379360).unwrap().add_days(304);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-11-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 30);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-10-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-09-30"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31 - 29);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-09-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31 - 30);
        assert_eq!(date.iso_8601().to_string(), String::from("1436-08-31"));
    }

    #[test]
    fn test_past_leap_year_bin_date() {
        let date = Date::from_binary(59611248).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1804-12-09"));
    }

    #[test]
    fn test_early_leap_year_bin_date() {
        let date = Date::from_binary(57781584).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1596-01-27"));
    }

    #[test]
    fn test_non_leap_year_bin_date() {
        let date = Date::from_binary(57775944).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1595-06-06"));
    }

    #[test]
    fn test_early_date() {
        let date = Date::from_binary(43808760).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("0001-01-01"));
    }

    #[test]
    fn test_days_until() {
        let date = Date::parse("1400.1.2").unwrap();
        let date2 = Date::parse("1400.1.3").unwrap();
        assert_eq!(1, date.days_until(&date2));
    }

    #[test]
    fn test_days_until2() {
        let date = Date::parse("1400.1.2").unwrap();
        let date2 = Date::parse("1401.1.2").unwrap();
        assert_eq!(365, date.days_until(&date2));
    }

    #[test]
    fn test_days_until3() {
        let date = Date::parse("1400.1.1").unwrap();
        let date2 = Date::parse("1401.12.31").unwrap();
        assert_eq!(729, date.days_until(&date2));
    }

    #[test]
    fn test_days_until4() {
        let date = Date::parse("1400.1.2").unwrap();
        let date2 = Date::parse("1400.1.2").unwrap();
        assert_eq!(0, date.days_until(&date2));
    }

    #[test]
    fn test_days_until5() {
        let date = Date::parse("1400.1.1").unwrap();
        let date2 = Date::parse("1401.12.31").unwrap();
        assert_eq!(-729, date2.days_until(&date));
    }

    #[test]
    fn test_add_days() {
        let date = Date::parse("1400.1.2").unwrap();
        let actual = date.add_days(1);
        let expected = Date::parse("1400.1.3").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days2() {
        let date = Date::parse("1400.1.2").unwrap();
        let actual = date.add_days(365);
        let expected = Date::parse("1401.1.2").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days3() {
        let date = Date::parse("1400.1.1").unwrap();
        let actual = date.add_days(729);
        let expected = Date::parse("1401.12.31").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days4() {
        let date = Date::parse("1400.1.2").unwrap();
        let actual = date.add_days(0);
        let expected = Date::parse("1400.1.2").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_all_days() {
        let start = Date::parse("1400.1.1").unwrap();
        for i in 0..364 {
            let (month, day) = month_day_from_julian(i);
            let next = Date::parse(format!("1400.{}.{}", month, day)).unwrap();
            assert_eq!(start.add_days(i), next);
            assert_eq!(start.days_until(&next), i);
        }
    }

    #[test]
    fn test_cmp() {
        let date = Date::parse("1457.3.5").unwrap();
        let date2 = Date::parse("1457.3.4").unwrap();
        assert!(date2 < date);
    }

    #[test]
    fn test_binary_date_regression() {
        let input = i32::from_le_bytes([14, 54, 43, 253]);
        let _ = Date::from_binary(input);
    }

    #[test]
    fn test_day_overflow_regression() {
        let _ = Date::from_ymd_opt(1222, 12, 222);
    }

    #[test]
    fn test_date_days() {
        let date = Date::parse("1.1.1").unwrap();
        assert_eq!(date.days(), 365);

        let date = Date::parse("-1.1.1").unwrap();
        assert_eq!(date.days(), -365);

        let date = Date::parse("-1.1.2").unwrap();
        assert_eq!(date.days(), -366);

        let date = Date::parse("-1.2.2").unwrap();
        assert_eq!(date.days(), -397);
    }

    #[test]
    fn test_negative_date_math() {
        let date = Date::parse("-1.1.2").unwrap();
        let d1 = date.add_days(1);
        assert_eq!(d1.game_fmt().to_string(), "-1.1.1");
        assert_eq!(date.days_until(&d1), 1);

        let date = Date::parse("-3.6.3").unwrap();
        let d1 = date.add_days(1);
        assert_eq!(d1.game_fmt().to_string(), "-3.6.2");
        assert_eq!(date.days_until(&d1), 1);
    }

    #[test]
    fn test_datehour_roundtrip() {
        let date = DateHour::parse("1936.1.1.24").unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1936-01-01T23"));
    }

    #[test]
    fn test_date_zeros_hour() {
        let data = i32::from_le_bytes([0x4b, 0x1d, 0x9f, 0x03]);
        let date = Date::from_binary(data).unwrap();
        let date_hour = DateHour::from_binary(data).unwrap();
        assert_eq!(date.iso_8601().to_string(), String::from("1936-01-01"));
        assert_eq!(
            date_hour.iso_8601().to_string(),
            String::from("1936-01-01T11")
        );
    }

    #[test]
    fn test_non_zero_binary_hours_are_not_heuristic_dates() {
        let data = i32::from_le_bytes([0x4b, 0x1d, 0x9f, 0x03]);
        assert_eq!(Date::from_binary_heuristic(data), None);
    }

    #[test]
    fn test_date_disallow_hour_parse_str() {
        assert!(Date::parse("1936.1.1.0").is_err())
    }

    #[test]
    fn test_date_state_of_wide_number() {
        assert_eq!(Date::parse("1936.01.01"), Ok(Date::from_ymd(1936, 1, 1)));
        assert_eq!(
            DateHour::parse("1936.01.01.12"),
            Ok(DateHour::from_ymdh(1936, 1, 1, 12))
        );
    }

    #[test]
    fn test_date_to_binary() {
        let date = Date::from_ymd(1, 1, 1);
        let bin = date.to_binary();
        assert_eq!(Date::from_binary(bin).unwrap(), date);
        assert_eq!(Date::from_binary_heuristic(bin).unwrap(), date);
    }

    #[test]
    fn test_date_hour_to_binary() {
        let date = DateHour::from_ymdh(1, 1, 1, 1);
        let bin = date.to_binary();
        assert_eq!(DateHour::from_binary(bin).unwrap(), date);
        assert_eq!(DateHour::from_binary_heuristic(bin).unwrap(), date);

        assert_eq!(DateHour::from_binary_heuristic(1), None);
    }

    #[test]
    fn test_uniform_date() {
        let date = UniformDate::from_ymd(2205, 2, 30);
        assert_eq!(date.iso_8601().to_string(), String::from("2205-02-30"));
        assert_eq!(date.game_fmt().to_string(), String::from("2205.02.30"));

        let date2 = UniformDate::parse("2205.02.30").unwrap();
        assert_eq!(date, date2);

        let date3 = UniformDate::parse("1.01.01").unwrap();
        assert_eq!(date3.game_fmt().to_string(), String::from("1.01.01"));
    }

    #[test]
    fn test_date_converted_into_number() {
        // So that we can decode paperman melted saves where a date is
        // detected as a number
        assert!(RawDate::parse(b"43808760").is_err());
        assert_eq!(Date::parse(b"43808760").unwrap(), Date::from_ymd(1, 1, 1));
    }

    #[test]
    fn test_from_str_impl() {
        let _date: RawDate = "1444.11.11".parse().unwrap();
        let _date: Date = "1444.11.11".parse().unwrap();
        let _date: DateHour = "1936.1.1.1".parse().unwrap();
        let _date: UniformDate = "2200.01.01".parse().unwrap();
    }

    #[test]
    fn test_date_hour_negative_regression() {
        assert!(Date::from_binary(-1).is_none());
        assert!(DateHour::from_binary(-1).is_none());
        assert!(Date::from_binary(-24).is_none());
    }

    #[test]
    fn test_memory_size() {
        // https://users.rust-lang.org/t/guidelines-for-self-ownership-on-copy-types/61262/2
        assert!(std::mem::size_of::<Date>() <= 2 * std::mem::size_of::<usize>());
    }

    #[quickcheck]
    fn test_binary_date_equality(data: i32) -> bool {
        Date::from_binary(data)
            .map(|x| x == Date::from_binary(x.to_binary()).unwrap())
            .unwrap_or(true)
    }

    #[quickcheck]
    fn test_binary_date_hour_equality(data: i32) -> bool {
        DateHour::from_binary(data)
            .map(|x| x == DateHour::from_binary(x.to_binary()).unwrap())
            .unwrap_or(true)
    }
}
