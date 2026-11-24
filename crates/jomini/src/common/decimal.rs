use std::fmt::{self, Display};

/// Room for the sign, the point, and the nineteen digits of an `i64`.
pub(crate) const DECIMAL_LEN: usize = 21;

/// A decimal number with `SCALE` decimal places, kept as an integer.
///
/// Binary saves store most of their numbers like this. The number of decimal
/// places is in the type, so that it can't get separated from the digits.
///
/// ```
/// use jomini::common::Decimal;
///
/// let number = Decimal::<5>::from_raw(-150_000);
/// assert_eq!(number.to_string(), "-1.5");
/// assert!(number < Decimal::from_raw(0));
/// ```
///
/// Numbers with a different `SCALE` can't be compared:
///
/// ```compile_fail,E0308
/// use jomini::common::Decimal;
///
/// let _ = Decimal::<3>::from_raw(1_500) == Decimal::<5>::from_raw(150_000);
/// ```
///
/// `SCALE` is at most 18, and a larger one does not compile:
///
/// ```
/// use jomini::common::Decimal;
///
/// let _ = Decimal::<18>::from_raw(1);
/// ```
///
/// ```compile_fail,E0080
/// use jomini::common::Decimal;
///
/// let _ = Decimal::<19>::from_raw(1);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Decimal<const SCALE: u32>(i64);

impl<const SCALE: u32> Decimal<SCALE> {
    const UNIT: u64 = {
        assert!(SCALE <= 18, "SCALE must be at most 18");
        10u64.pow(SCALE)
    };

    /// Create a number from its digits, so `1.5` with five decimal places is
    /// `150_000`.
    #[inline]
    pub const fn from_raw(raw: i64) -> Self {
        let _ = Self::UNIT;
        Decimal(raw)
    }

    /// Format without the trailing zeros of the fraction.
    #[inline]
    pub(crate) fn format_trimmed(self, buf: &mut [u8; DECIMAL_LEN]) -> &[u8] {
        let mut fraction = self.0.unsigned_abs() % Self::UNIT;
        let mut places = 0;
        if fraction != 0 {
            places = SCALE;
            while fraction.is_multiple_of(10) {
                fraction /= 10;
                places -= 1;
            }
        }
        self.format(buf, fraction, places)
    }

    /// Format with all decimal places.
    #[inline]
    pub(crate) fn format_padded(self, buf: &mut [u8; DECIMAL_LEN]) -> &[u8] {
        self.format(buf, self.0.unsigned_abs() % Self::UNIT, SCALE)
    }

    /// Format with the last `places` digits of `fraction` after the point.
    #[inline]
    fn format(self, buf: &mut [u8; DECIMAL_LEN], mut fraction: u64, places: u32) -> &[u8] {
        let mut i = DECIMAL_LEN;
        if places != 0 {
            for _ in 0..places {
                i -= 1;
                buf[i] = b'0' + (fraction % 10) as u8;
                fraction /= 10;
            }
            i -= 1;
            buf[i] = b'.';
        }

        let mut whole = self.0.unsigned_abs() / Self::UNIT;
        loop {
            i -= 1;
            buf[i] = b'0' + (whole % 10) as u8;
            whole /= 10;
            if whole == 0 {
                break;
            }
        }

        if self.0 < 0 {
            i -= 1;
            buf[i] = b'-';
        }
        &buf[i..]
    }
}

/// Writes the number without the trailing zeros of its fraction. A precision is
/// ignored, the same as for an integer.
impl<const SCALE: u32> Display for Decimal<SCALE> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buf = [0; DECIMAL_LEN];
        let digits = self.format_trimmed(&mut buf);
        let digits = digits.strip_prefix(b"-").unwrap_or(digits);
        let digits = std::str::from_utf8(digits).map_err(|_| fmt::Error)?;
        f.pad_integral(self.0 >= 0, "", digits)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn padded<const SCALE: u32>(raw: i64) -> String {
        let mut buf = [0; DECIMAL_LEN];
        let digits = Decimal::<SCALE>::from_raw(raw).format_padded(&mut buf);
        String::from_utf8(digits.to_vec()).unwrap()
    }

    fn trimmed<const SCALE: u32>(raw: i64) -> String {
        Decimal::<SCALE>::from_raw(raw).to_string()
    }

    #[test]
    fn digits() {
        assert_eq!(padded::<0>(-7), "-7");
        assert_eq!(trimmed::<0>(70), "70");
        assert_eq!(padded::<3>(0), "0.000");
        assert_eq!(trimmed::<3>(0), "0");
        assert_eq!(padded::<3>(-5), "-0.005");
        assert_eq!(trimmed::<3>(-10), "-0.01");
        assert_eq!(trimmed::<5>(1_000_000), "10");
        assert_eq!(padded::<6>(2_147_483_647_000), "2147483.647000");
        assert_eq!(padded::<5>(i64::MIN), "-92233720368547.75808");
        assert_eq!(trimmed::<5>(i64::MAX), "92233720368547.75807");
        assert_eq!(padded::<18>(i64::MIN), "-9.223372036854775808");
        assert_eq!(padded::<18>(-1), "-0.000000000000000001");
        assert_eq!(trimmed::<18>(-1), "-0.000000000000000001");
    }

    #[test]
    fn display_flags() {
        let number = Decimal::<5>::from_raw(-150_000);
        assert_eq!(format!("{number:8}"), "    -1.5");
        assert_eq!(format!("{number:<8}"), "-1.5    ");
        assert_eq!(format!("{number:08}"), "-00001.5");
        assert_eq!(format!("{number:.3}"), "-1.5");
        assert_eq!(format!("{:+}", Decimal::<5>::from_raw(1)), "+0.00001");
    }

    /// A number of at most fifteen digits prints the same as its double.
    #[test]
    fn matches_double() {
        fn check<const SCALE: u32>(raw: i64) {
            let double = raw as f64 / Decimal::<SCALE>::UNIT as f64;
            assert_eq!(trimmed::<SCALE>(raw), double.to_string());
        }

        let mut seed = 1u64;
        let edges = [0, 1, -1, 99_999, 100_000, -100_000, 999_999_999_999_999];
        let mut random = move || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            let limit = 10i64.pow((seed % 15 + 1) as u32);
            ((seed >> 8) as i64 % limit) * if seed & 64 == 0 { 1 } else { -1 }
        };
        for i in 0..if cfg!(miri) { 200 } else { 1_000_000 } {
            let raw = edges.get(i).copied().unwrap_or_else(&mut random);
            check::<3>(raw);
            check::<5>(raw);
        }
    }
}
