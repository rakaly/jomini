use crate::Scalar;
use std::cmp::Ordering;
use std::convert::TryFrom;

const DAYS_PER_MONTH: [u8; 13] = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

/// Struct specialized to parsing, formatting, and manipulating dates in games
///
/// A game date does not follow any traditional calendar and instead views the
/// world on simpler terms: that every year should be treated as a non-leap year.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Date {
    year: i16,
    month: u8,
    day: u8,
}

impl PartialOrd for Date {
    fn partial_cmp(&self, other: &Date) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Date {
    fn cmp(&self, other: &Date) -> Ordering {
        self.year
            .cmp(&other.year)
            .then_with(|| self.month.cmp(&other.month))
            .then_with(|| self.day.cmp(&other.day))
    }
}

impl Date {
    /// Create a new date from year, month, and day parts
    ///
    /// Will return `None` if the date does not exist
    ///
    /// ```
    /// use jomini::common::Date;
    /// assert_eq!(Date::new(1444, 11, 11), Date::parse_from_str("1444.11.11"));
    /// assert_eq!(Date::new(800, 5, 3), Date::parse_from_str("800.5.3"));
    /// assert!(Date::new(800, 0, 3).is_none());
    /// assert!(Date::new(800, 1, 0).is_none());
    /// assert!(Date::new(800, 13, 1).is_none());
    /// assert!(Date::new(800, 12, 32).is_none());
    /// assert!(Date::new(2020, 2, 29).is_none());
    /// ```
    pub fn new(year: i16, month: u8, day: u8) -> Option<Self> {
        // Years can be negative in EU4 (but seemingly can't be zero). Antonio
        // I holds the record at with `birth_date=-58.1.1` and since I don't
        // anticipate more rulers to be added that nearly double the negative,
        // we cap what we're looking for at -100 years (we don't want the
        // melter to accidentally think a number is a date)
        if year != 0 && month != 0 && day != 0 && year > -100 {
            if let Some(&days) = DAYS_PER_MONTH.get(usize::from(month)) {
                if day <= days {
                    return Some(Date { year, month, day });
                }
            }
        }

        None
    }

    /// Year of the date
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1445.02.03").expect("to parse date");
    /// assert_eq!(date.year(), 1445);
    /// ```
    pub fn year(&self) -> i16 {
        self.year
    }

    /// Month of the date
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1445.02.03").expect("to parse date");
    /// assert_eq!(date.month(), 2);
    /// ```
    pub fn month(&self) -> u8 {
        self.month
    }

    /// Day of the date
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1445.02.03").expect("to parse date");
    /// assert_eq!(date.day(), 3);
    /// ```
    pub fn day(&self) -> u8 {
        self.day
    }

    /// Parses a string and returns a new Date if valid.
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1444.11.11").expect("to parse date");
    /// assert_eq!(date.year(), 1444);
    /// assert_eq!(date.month(), 11);
    /// assert_eq!(date.day(), 11);
    /// ```
    pub fn parse_from_str<T: AsRef<str>>(s: T) -> Option<Self> {
        let data = s.as_ref().as_bytes();
        let mut state = 0;
        let mut span1: &[u8] = &[];
        let mut span2: &[u8] = &[];
        let mut start = 0;

        // micro-optimization: check the first byte to see if the first character (if available)
        // is outside our upper bound (ie: not a number). This micro optimization doesn't
        // harm the happy path (input is a date) by more than a few percent, but if the input
        // is not a date, this shaves off 20-25% in date parsing benchmarks.
        if data.get(0).map_or(true, |c| *c > b'9') {
            return None;
        }

        for (pos, &c) in data.iter().enumerate() {
            if c == b'.' {
                match state {
                    0 => {
                        span1 = &data[start..pos];
                        state = 1;
                    }
                    1 => {
                        span2 = &data[start..pos];
                        state = 2;
                    }
                    _ => return None,
                }
                start = pos + 1;
            }
        }

        let span3 = &data[start..];

        let year = Scalar::new(span1)
            .to_i64()
            .ok()
            .and_then(|x| i16::try_from(x).ok());

        let year = match year {
            Some(x) => x,
            None => return None,
        };

        let month = Scalar::new(span2)
            .to_u64()
            .ok()
            .and_then(|x| u8::try_from(x).ok());

        let month = match month {
            Some(x) => x,
            None => return None,
        };

        let day = Scalar::new(span3)
            .to_u64()
            .ok()
            .and_then(|x| u8::try_from(x).ok());

        let day = match day {
            Some(x) => x,
            None => return None,
        };

        Date::new(year, month, day)
    }

    fn days(&self) -> i32 {
        let month_days = match self.month {
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
        };

        let year_day = i32::from(self.year) * 365;
        if year_day < 0 {
            year_day - month_days - i32::from(self.day)
        } else {
            year_day + month_days + i32::from(self.day)
        }
    }

    /// Returns the number of days between two dates
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1400.1.2").unwrap();
    /// let date2 = Date::parse_from_str("1400.1.3").unwrap();
    /// let date3 = Date::parse_from_str("1401.1.2").unwrap();
    /// let date4 = Date::parse_from_str("1401.12.31").unwrap();
    /// assert_eq!(1, date.days_until(&date2));
    /// assert_eq!(365, date.days_until(&date3));
    /// assert_eq!(728, date.days_until(&date4));
    /// assert_eq!(-728, date4.days_until(&date));
    /// ```
    pub fn days_until(&self, other: &Date) -> i32 {
        other.days() - self.days()
    }

    /// Return a new date that is the given number of days in the future
    /// from the current date
    ///
    /// ```
    /// use jomini::common::Date;
    ///
    /// let date = Date::parse_from_str("1400.1.2").unwrap();
    /// let expected = Date::parse_from_str("1400.1.3").unwrap();
    /// let expected2 = Date::parse_from_str("1400.1.1").unwrap();
    /// assert_eq!(expected, date.add_days(1));
    /// assert_eq!(expected2, date.add_days(-1));
    /// ```
    ///
    /// Will panic on overflow or underflow.
    pub fn add_days(&self, days: i32) -> Date {
        let new_days = self
            .days()
            .checked_add(days)
            .expect("adding days overflowed");

        let days_since_jan1 = (new_days % 365).abs();
        let year = new_days / 365;
        let (month, day) = month_day_from_julian(days_since_jan1);

        let year = i16::try_from(year).expect("year to fit inside signed 32bits");
        Date { year, month, day }
    }

    /// Decodes a date from a number that had been parsed from binary data
    pub fn from_binary(mut s: i32) -> Option<Self> {
        if s < 0 {
            return None;
        }

        let _hours = s % 24;
        s /= 24;
        let days_since_jan1 = s % 365;
        s /= 365;
        let year = match s.checked_sub(5000).and_then(|x| i16::try_from(x).ok()) {
            Some(y) => y,
            None => return None,
        };

        let (month, day) = month_day_from_julian(days_since_jan1);
        Date::new(year, month, day)
    }

    /// Formats a date in the ISO 8601 format: YYYY-MM-DD
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1400.1.2").expect("to parse date");
    /// assert_eq!(date.iso_8601(), String::from("1400-01-02"));
    /// ```
    pub fn iso_8601(&self) -> String {
        format!("{:04}-{:02}-{:02}", self.year, self.month, self.day)
    }

    /// Formats a date in the game format: Y.M.D
    ///
    /// ```
    /// use jomini::common::Date;
    /// let date = Date::parse_from_str("1400.1.2").expect("to parse date");
    /// let end_date = date.add_days(30);
    /// assert_eq!(end_date.game_fmt(), String::from("1400.2.1"));
    /// ```
    pub fn game_fmt(&self) -> String {
        format!("{}.{}.{}", self.year, self.month, self.day)
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

#[cfg(feature = "derive")]
mod datederive {
    use super::Date;
    use serde::{de, de::Visitor, Deserialize, Deserializer, Serialize, Serializer};
    use std::fmt;

    impl Serialize for Date {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            serializer.serialize_str(self.iso_8601().as_str())
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
            Date::parse_from_str(v).ok_or_else(|| de::Error::custom(format!("invalid date: {}", v)))
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
}

#[cfg(not(feature = "derive"))]
mod datederive {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_date_roundtrip() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        assert_eq!(date.iso_8601(), String::from("1400-01-02"));
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
            let date = Date::parse_from_str(case).unwrap();
            assert_eq!(date.game_fmt(), case.to_string());
        }
    }

    #[test]
    fn test_first_bin_date() {
        let date = Date::from_binary(56379360).unwrap();
        assert_eq!(date.iso_8601(), String::from("1436-01-01"));
    }

    #[test]
    fn test_text_date_overflow() {
        assert_eq!(Date::parse_from_str("1444.257.1"), None);
        assert_eq!(Date::parse_from_str("1444.1.257"), None);
        assert_eq!(Date::parse_from_str("60000.1.1"), None);
        assert_eq!(Date::parse_from_str("-60000.1.1"), None);
    }

    #[test]
    fn test_binary_date_overflow() {
        assert_eq!(Date::from_binary(999379360), None);
    }

    #[test]
    #[should_panic]
    fn test_add_adds_year_overflow() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let _ = date.add_days(100000000);
    }

    #[test]
    #[should_panic]
    fn test_add_adds_day_overflow() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let _ = date.add_days(i32::MAX);
    }

    #[test]
    fn test_ignore_bin_dates() {
        // These are numbers from a savefile that shouldn't be interpreted as dates
        assert_eq!(Date::from_binary(380947), None);
        assert_eq!(Date::from_binary(21282204), None);
        assert_eq!(Date::from_binary(33370842), None);
        assert_eq!(Date::from_binary(42267422), None);
        assert_eq!(Date::from_binary(693362154), None);
    }

    #[test]
    fn test_negative_date() {
        // EU4 Monarch birth dates can be negative, no idea what those mean
        let date = Date::parse_from_str("-17.1.1").unwrap();
        assert_eq!(date.game_fmt(), String::from("-17.1.1"));

        let date2 = Date::from_binary(43651080).unwrap();
        assert_eq!(date.game_fmt(), String::from("-17.1.1"));

        assert_eq!(date, date2);
    }

    #[test]
    fn test_november_date_regression() {
        let date = Date::from_binary(56379360).unwrap().add_days(303);
        assert_eq!(date.iso_8601(), String::from("1436-10-31"));
        let date = Date::from_binary(56379360).unwrap().add_days(304);
        assert_eq!(date.iso_8601(), String::from("1436-11-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 30);
        assert_eq!(date.iso_8601(), String::from("1436-10-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31);
        assert_eq!(date.iso_8601(), String::from("1436-09-30"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31 - 29);
        assert_eq!(date.iso_8601(), String::from("1436-09-01"));
        let date = Date::from_binary(56379360).unwrap().add_days(303 - 31 - 30);
        assert_eq!(date.iso_8601(), String::from("1436-08-31"));
    }

    #[test]
    fn test_past_leap_year_bin_date() {
        let date = Date::from_binary(59611248).unwrap();
        assert_eq!(date.iso_8601(), String::from("1804-12-09"));
    }

    #[test]
    fn test_early_leap_year_bin_date() {
        let date = Date::from_binary(57781584).unwrap();
        assert_eq!(date.iso_8601(), String::from("1596-01-27"));
    }

    #[test]
    fn test_non_leap_year_bin_date() {
        let date = Date::from_binary(57775944).unwrap();
        assert_eq!(date.iso_8601(), String::from("1595-06-06"));
    }

    #[test]
    fn test_early_date() {
        let date = Date::from_binary(43808760).unwrap();
        assert_eq!(date.iso_8601(), String::from("0001-01-01"));
    }

    #[test]
    fn test_days_until() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let date2 = Date::parse_from_str("1400.1.3").unwrap();
        assert_eq!(1, date.days_until(&date2));
    }

    #[test]
    fn test_days_until2() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let date2 = Date::parse_from_str("1401.1.2").unwrap();
        assert_eq!(365, date.days_until(&date2));
    }

    #[test]
    fn test_days_until3() {
        let date = Date::parse_from_str("1400.1.1").unwrap();
        let date2 = Date::parse_from_str("1401.12.31").unwrap();
        assert_eq!(729, date.days_until(&date2));
    }

    #[test]
    fn test_days_until4() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let date2 = Date::parse_from_str("1400.1.2").unwrap();
        assert_eq!(0, date.days_until(&date2));
    }

    #[test]
    fn test_days_until5() {
        let date = Date::parse_from_str("1400.1.1").unwrap();
        let date2 = Date::parse_from_str("1401.12.31").unwrap();
        assert_eq!(-729, date2.days_until(&date));
    }

    #[test]
    fn test_add_days() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let actual = date.add_days(1);
        let expected = Date::parse_from_str("1400.1.3").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days2() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let actual = date.add_days(365);
        let expected = Date::parse_from_str("1401.1.2").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days3() {
        let date = Date::parse_from_str("1400.1.1").unwrap();
        let actual = date.add_days(729);
        let expected = Date::parse_from_str("1401.12.31").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_add_days4() {
        let date = Date::parse_from_str("1400.1.2").unwrap();
        let actual = date.add_days(0);
        let expected = Date::parse_from_str("1400.1.2").unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_all_days() {
        let start = Date::parse_from_str("1400.1.1").unwrap();
        for i in 0..364 {
            let (month, day) = month_day_from_julian(i);
            let next = Date::parse_from_str(format!("1400.{}.{}", month, day)).unwrap();
            assert_eq!(start.add_days(i), next);
            assert_eq!(start.days_until(&next), i);
        }
    }

    #[test]
    fn test_cmp() {
        let date = Date::parse_from_str("1457.3.5").unwrap();
        let date2 = Date::parse_from_str("1457.3.4").unwrap();
        assert!(date2 < date);
    }

    #[test]
    fn test_binary_date_regression() {
        let input = i32::from_le_bytes([14, 54, 43, 253]);
        let _ = Date::from_binary(input);
    }

    #[test]
    fn test_date_days() {
        let date = Date::parse_from_str("1.1.1").unwrap();
        assert_eq!(date.days(), 365);

        let date = Date::parse_from_str("-1.1.1").unwrap();
        assert_eq!(date.days(), -365);

        let date = Date::parse_from_str("-1.1.2").unwrap();
        assert_eq!(date.days(), -366);

        let date = Date::parse_from_str("-1.2.2").unwrap();
        assert_eq!(date.days(), -397);
    }

    #[test]
    fn test_negative_date_math() {
        let date = Date::parse_from_str("-1.1.2").unwrap();
        let d1 = date.add_days(1);
        assert_eq!(d1.game_fmt(), "-1.1.1");
        assert_eq!(date.days_until(&d1), 1);

        let date = Date::parse_from_str("-3.6.3").unwrap();
        let d1 = date.add_days(1);
        assert_eq!(d1.game_fmt(), "-3.6.2");
        assert_eq!(date.days_until(&d1), 1);
    }
}
