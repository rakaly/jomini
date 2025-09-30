use crate::decode_windows1252;
use std::convert::TryFrom;
use std::error;
use std::fmt;

/// An error that can occur when converting a scalar into the requested type.
#[derive(Debug, Clone, PartialEq)]
pub enum ScalarError {
    /// The scalar did not contain only numbers
    AllDigits,

    /// The scalar caused an overflow when calculating its numerical value
    Overflow,

    /// The scalar was not a recognized boolean value
    InvalidBool,

    /// The scalar would lose precision if the given float was returned
    PrecisionLoss(f64),
}

impl fmt::Display for ScalarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScalarError::AllDigits => write!(f, "did not contain all digits"),
            ScalarError::InvalidBool => write!(f, "is not a valid bool"),
            ScalarError::Overflow => write!(f, "caused an overflow"),
            ScalarError::PrecisionLoss(_) => write!(f, "precision loss"),
        }
    }
}

impl error::Error for ScalarError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// A byte slice that represents a single value.
///
/// A scalars does not carry with it an encoding, so an appropriate encoder must be used
/// if text is wished to be extracted from a scalar
///
/// ```
/// use jomini::Scalar;
///
/// let v1 = Scalar::new(b"10");
/// assert_eq!(v1.to_u64(), Ok(10));
/// assert_eq!(v1.to_i64(), Ok(10));
/// assert_eq!(v1.to_f64(), Ok(10.0));
/// assert!(v1.to_bool().is_err());
/// ```
#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Scalar<'a> {
    data: &'a [u8],
}

impl<'a> Scalar<'a> {
    /// Create a new scalar backed by a byte slice
    #[inline]
    pub fn new(data: &'a [u8]) -> Scalar<'a> {
        Scalar { data }
    }

    /// View the raw data
    #[inline]
    pub fn as_bytes(self) -> &'a [u8] {
        self.data
    }

    /// Try converting the scalar to f64
    ///
    /// Supports optional 'f' suffix for floating point literals.
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"1.000");
    /// assert_eq!(v1.to_f64(), Ok(1.0));
    ///
    /// let v2 = Scalar::new(b"-5.67821");
    /// assert_eq!(v2.to_f64(), Ok(-5.67821));
    ///
    /// let v3 = Scalar::new(b"10.0f");
    /// assert_eq!(v3.to_f64(), Ok(10.0));
    /// ```
    #[inline]
    pub fn to_f64(self) -> Result<f64, ScalarError> {
        to_f64(self.data)
    }

    /// Try converting the scalar to boolean, only "yes" and "no" can be mapped:
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"yes");
    /// assert_eq!(v1.to_bool(), Ok(true));
    ///
    /// let v2 = Scalar::new(b"no");
    /// assert_eq!(v2.to_bool(), Ok(false));
    /// ```
    #[inline]
    pub fn to_bool(self) -> Result<bool, ScalarError> {
        to_bool(self.data)
    }

    /// Try converting the scalar to i64
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"-50");
    /// assert_eq!(v1.to_i64(), Ok(-50));
    ///
    /// let v2 = Scalar::new(b"120");
    /// assert_eq!(v2.to_i64(), Ok(120));
    /// ```
    #[inline]
    pub fn to_i64(self) -> Result<i64, ScalarError> {
        to_i64(self.data)
    }

    /// Try converting the scalar to u64
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"50");
    /// assert_eq!(v1.to_i64(), Ok(50));
    ///
    /// let v2 = Scalar::new(b"120");
    /// assert_eq!(v2.to_i64(), Ok(120));
    /// ```
    #[inline]
    pub fn to_u64(self) -> Result<u64, ScalarError> {
        to_u64(self.data)
    }

    /// Returns if the scalar contains only ascii values
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"a");
    /// assert!(v1.is_ascii());
    ///
    /// let v2 = Scalar::new(&[255][..]);
    /// assert!(!v2.is_ascii());
    /// ```
    #[inline]
    pub fn is_ascii(self) -> bool {
        self.data.is_ascii()
    }
}

impl fmt::Debug for Scalar<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Scalar {{ {} }}", self)
    }
}

impl fmt::Display for Scalar<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_ascii() {
            write!(f, "{}", decode_windows1252(self.data))
        } else {
            write!(f, "non-ascii string of {} length", self.data.len())
        }
    }
}

#[inline]
fn to_bool(d: &[u8]) -> Result<bool, ScalarError> {
    match d {
        [b'y', b'e', b's'] => Ok(true),
        [b'n', b'o'] => Ok(false),
        _ => Err(ScalarError::InvalidBool),
    }
}

const OVERFLOW_CUTOFF: usize = digits_in(u64::MAX);
const SAFE_INTEGER: u64 = 2u64.pow(53) - 1;
const SAFE_INTEGER_LEN: usize = digits_in(SAFE_INTEGER);

/// Inspired by https://github.com/lemire/fast_double_parser
#[inline]
fn to_f64(mut d: &[u8]) -> Result<f64, ScalarError> {
    let mut acc = 0;
    let mut integer_part = d;

    let (&c, rest) = d.split_first().ok_or(ScalarError::AllDigits)?;
    let negative = c == b'-';
    if negative {
        integer_part = rest;
        d = rest;
    } else if c.is_ascii_digit() {
        acc = u64::from(c - b'0');
        d = rest;
    } else if c == b'+' {
        integer_part = rest;
        d = rest;
    } else if c != b'.' {
        return Err(ScalarError::AllDigits);
    }

    let sign = -((negative as i64 * 2).wrapping_sub(1));
    while let Some((&c, mut rest)) = d.split_first() {
        if c.is_ascii_digit() {
            acc = acc.wrapping_mul(10);
            acc = acc.wrapping_add(u64::from(c - b'0'));
            d = rest;
        } else if c == b'.' {
            let mut total = acc;
            let mut nondigit = false;
            if let Some((&last, fractions)) = rest.split_last() {
                for &x in fractions {
                    nondigit |= !x.is_ascii_digit();
                    total = total.wrapping_mul(10);
                    total = total.wrapping_add(u64::from(x - b'0'));
                }

                if nondigit {
                    return Err(ScalarError::AllDigits);
                }

                if last.is_ascii_digit() {
                    total = total.wrapping_mul(10);
                    total = total.wrapping_add(u64::from(last - b'0'));
                } else if last != b'f' {
                    return Err(ScalarError::AllDigits);
                } else {
                    rest = &rest[..rest.len() - 1];
                }
            }

            let fractional_digits = rest.len();
            let whole_digits = integer_part.len() - fractional_digits - 1;

            if fractional_digits + whole_digits >= OVERFLOW_CUTOFF - 1 {
                check_overflow_init(rest, acc)?;
            }

            let pow = POWER_OF_TEN
                .get(fractional_digits)
                .ok_or(ScalarError::Overflow)?;
            let d = (total as f64) / *pow;
            return Ok((sign as f64) * d);
        } else if c == b'f' && rest.is_empty() {
            integer_part = &integer_part[..integer_part.len().saturating_sub(1)];
            d = rest;
        } else {
            return Err(ScalarError::AllDigits);
        }
    }

    if integer_part.len() < SAFE_INTEGER_LEN {
        return Ok((sign * (acc as i64)) as f64);
    }

    check_precision_and_overflow(sign, acc, integer_part)
}

#[cold]
fn check_precision_and_overflow(
    sign: i64,
    acc: u64,
    integer_part: &[u8],
) -> Result<f64, ScalarError> {
    if integer_part.len() >= OVERFLOW_CUTOFF {
        check_overflow(integer_part)?;
    }

    let val = i64::try_from(acc)
        .map(|x| x * sign)
        .map_err(|_| ScalarError::Overflow);

    if acc > SAFE_INTEGER {
        let approx = if sign == 1 { acc as f64 } else { val? as f64 };
        return Err(ScalarError::PrecisionLoss(approx));
    }

    Ok(val? as f64)
}

#[inline]
fn to_i64(d: &[u8]) -> Result<i64, ScalarError> {
    let (result, left) = to_i64_t(d)?;
    if left.is_empty() {
        Ok(result)
    } else {
        Err(ScalarError::AllDigits)
    }
}

#[inline]
pub(crate) fn to_i64_t(d: &[u8]) -> Result<(i64, &[u8]), ScalarError> {
    let (&c, data) = d.split_first().ok_or(ScalarError::AllDigits)?;
    let mut sign = 1;

    let start = if c.is_ascii_digit() {
        c - b'0'
    } else if c == b'-' {
        sign = -1;
        0
    } else if c == b'+' {
        0
    } else {
        return Err(ScalarError::AllDigits);
    };

    let (val, rest) = to_u64_partial(data, u64::from(start));
    if d.len() >= OVERFLOW_CUTOFF - 1 {
        check_overflow(d)?;
    }

    let val = i64::try_from(val)
        .map(|x| sign * x)
        .map_err(|_| ScalarError::Overflow)?;
    Ok((val, rest))
}

/// Convert a buffer to an u64. This function is micro-optimized for small
/// inputs. Previous implementations had higher throughput on larger input but
/// couldn't parse small inputs as quickly. Micro-optimizing this function for
/// small inputs shaved ~7% off deserializing 120MB save as 10% of all time
/// was spent in this function. Dates are a common occurrence of numbers that
/// are 1-4 digits in length
#[inline]
pub(crate) fn to_u64(d: &[u8]) -> Result<u64, ScalarError> {
    let (&c, data) = d.split_first().ok_or(ScalarError::AllDigits)?;
    let mut result = if c.is_ascii_digit() {
        u64::from(c - b'0')
    } else if c == b'+' {
        0
    } else {
        return Err(ScalarError::AllDigits);
    };

    for &x in data {
        if !x.is_ascii_digit() {
            return Err(ScalarError::AllDigits);
        }

        result = result.wrapping_mul(10);
        result = result.wrapping_add(u64::from(x - b'0'));
    }

    // Check for overflow. We know the overflow possibility exists only when
    // there are at least 20 digits to match u64::MAX (184467440737095516105)
    if d.len() >= OVERFLOW_CUTOFF - 1 {
        check_overflow(d)?;
    }

    Ok(result)
}

#[cold]
fn check_overflow(mut d: &[u8]) -> Result<u64, ScalarError> {
    if d.is_empty() {
        return Err(ScalarError::AllDigits);
    }

    if matches!(d[0], b'+' | b'-') {
        d = &d[1..];
    }

    check_overflow_init(d, 0)
}

#[cold]
fn check_overflow_init(d: &[u8], start: u64) -> Result<u64, ScalarError> {
    let mut acc = start;
    for &x in d {
        // The input should already be validated by this point, so we just
        // return the accumulator if we find a non-digit.
        if !x.is_ascii_digit() {
            return Ok(acc);
        }

        acc = acc
            .checked_mul(10)
            .and_then(|acc| acc.checked_add(u64::from(x - b'0')))
            .ok_or(ScalarError::Overflow)?;
    }

    Ok(acc)
}

#[inline]
fn to_u64_partial(mut d: &[u8], start: u64) -> (u64, &[u8]) {
    let mut result = start;

    while let Some((c, rest)) = d.split_first() {
        if !c.is_ascii_digit() {
            return (result, d);
        }

        result = result.wrapping_mul(10);
        result = result.wrapping_add(u64::from(c - b'0'));
        d = rest;
    }

    (result, &[])
}

const fn digits_in(n: u64) -> usize {
    if n == 0 { 1 } else { n.ilog10() as usize + 1 }
}

const POWER_OF_TEN: [f64; 23] = [
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16,
    1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
];

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn test_memory_size() {
        // https://users.rust-lang.org/t/guidelines-for-self-ownership-on-copy-types/61262/2
        assert!(std::mem::size_of::<Scalar>() <= 2 * std::mem::size_of::<usize>());
    }

    #[test]
    fn scalar_to_bool() {
        assert_eq!((Scalar::new(b"yes").to_bool()), Ok(true));
        assert_eq!((Scalar::new(b"no").to_bool()), Ok(false));
        assert_eq!((Scalar::new(b"-1").to_f64()), Ok(-1.0));
    }

    #[test]
    fn scalar_to_f64() {
        assert_eq!((Scalar::new(b"0").to_f64()), Ok(0.0));
        assert_eq!((Scalar::new(b"1").to_f64()), Ok(1.0));
        assert_eq!((Scalar::new(b"-1").to_f64()), Ok(-1.0));
        assert_eq!((Scalar::new(b"-10000").to_f64()), Ok(-10000.0));
        assert_eq!((Scalar::new(b"10000").to_f64()), Ok(10000.0));
        assert_eq!((Scalar::new(b"20405029").to_f64()), Ok(20405029.0));
        assert_eq!((Scalar::new(b"-20405029").to_f64()), Ok(-20405029.0));
        assert_eq!(
            (Scalar::new(b"20405029553322").to_f64()),
            Ok(20405029553322.0)
        );
        assert_eq!(
            (Scalar::new(b"-20405029553322").to_f64()),
            Ok(-20405029553322.0)
        );

        assert_eq!((Scalar::new(b"0.504").to_f64()), Ok(0.504));
        assert_eq!((Scalar::new(b"-0.504").to_f64()), Ok(-0.504));
        assert_eq!((Scalar::new(b".504").to_f64()), Ok(0.504));
        assert_eq!((Scalar::new(b"-.504").to_f64()), Ok(-0.504));
        assert_eq!((Scalar::new(b"1.00125").to_f64()), Ok(1.00125));
        assert_eq!((Scalar::new(b"-1.50000").to_f64()), Ok(-1.5));
        assert_eq!((Scalar::new(b"-10000.0").to_f64()), Ok(-10000.0));
        assert_eq!((Scalar::new(b"10000.000").to_f64()), Ok(10000.0));
        assert_eq!((Scalar::new(b"20405029.125").to_f64()), Ok(20405029.125));
        assert_eq!((Scalar::new(b"-20405029.125").to_f64()), Ok(-20405029.125));
        assert_eq!(
            (Scalar::new(b"20405029553322.015").to_f64()),
            Ok(20405029553322.015)
        );
        assert_eq!(
            (Scalar::new(b"-20405029553322.015").to_f64()),
            Ok(-20405029553322.015)
        );
        assert_eq!(
            Scalar::new(b"10.99999999999999").to_f64(),
            Ok(10.99999999999999)
        );
        assert_eq!((Scalar::new(b"+0.5").to_f64()), Ok(0.5));

        assert!(Scalar::new(b"E").to_f64().is_err());
        assert!(Scalar::new(b"").to_f64().is_err());
    }

    #[test]
    fn scalar_to_f64_with_f_suffix() {
        assert_eq!((Scalar::new(b"0.0f").to_f64()), Ok(0.0));
        assert_eq!((Scalar::new(b"-5.5f").to_f64()), Ok(-5.5));
        assert_eq!((Scalar::new(b"10.0f").to_f64()), Ok(10.0));
        assert_eq!((Scalar::new(b"0.40f").to_f64()), Ok(0.4));
        assert_eq!((Scalar::new(b"123.456f").to_f64()), Ok(123.456));
        assert_eq!((Scalar::new(b"-0.001f").to_f64()), Ok(-0.001));
        assert_eq!((Scalar::new(b"+42.0f").to_f64()), Ok(42.0));
        assert_eq!((Scalar::new(b".5f").to_f64()), Ok(0.5));
        assert_eq!((Scalar::new(b"1f").to_f64()), Ok(1.0));
        assert_eq!((Scalar::new(b"-1f").to_f64()), Ok(-1.0));
        assert_eq!((Scalar::new(b"10.f").to_f64()), Ok(10.0));

        assert!(Scalar::new(b"f").to_f64().is_err());
        assert!(Scalar::new(b"invalidf").to_f64().is_err());
        assert_eq!((Scalar::new(b"0f").to_f64()), Ok(0.0));
    }

    #[test]
    fn scalar_f64_fraction_too_long() {
        assert!(Scalar::new(b"0.00000000000000000000000").to_f64().is_err());
    }

    #[test]
    fn scalar_to_i64() {
        assert_eq!((Scalar::new(b"0").to_i64()), Ok(0));
        assert_eq!((Scalar::new(b"1").to_i64()), Ok(1));
        assert_eq!((Scalar::new(b"-1").to_i64()), Ok(-1));
        assert_eq!((Scalar::new(b"-10000").to_i64()), Ok(-10000));
        assert_eq!((Scalar::new(b"10000").to_i64()), Ok(10000));
        assert_eq!((Scalar::new(b"20405029").to_i64()), Ok(20405029));
        assert_eq!((Scalar::new(b"-20405029").to_i64()), Ok(-20405029));
        assert_eq!(
            (Scalar::new(b"20405029553322").to_i64()),
            Ok(20405029553322)
        );
        assert_eq!(
            (Scalar::new(b"-20405029553322").to_i64()),
            Ok(-20405029553322)
        );

        assert_eq!((Scalar::new(b"+0").to_i64()), Ok(0));
        assert_eq!((Scalar::new(b"+1").to_i64()), Ok(1));

        assert_eq!(
            Scalar::new(b"9223372036854775807").to_i64(),
            Ok(9223372036854775807)
        );
        assert!(Scalar::new(b"-9223372036854775809").to_i64().is_err());
        assert!(Scalar::new(b"9223372036854775808").to_i64().is_err());
    }

    #[test]
    fn scalar_to_u64() {
        assert_eq!((Scalar::new(b"0").to_u64()), Ok(0));
        assert_eq!((Scalar::new(b"1").to_u64()), Ok(1));
        assert_eq!((Scalar::new(b"45").to_u64()), Ok(45));
        assert_eq!((Scalar::new(b"+45").to_u64()), Ok(45));
        assert_eq!((Scalar::new(b"10000").to_u64()), Ok(10000));
        assert_eq!((Scalar::new(b"20405029").to_u64()), Ok(20405029));
        assert_eq!(
            (Scalar::new(b"20405029553322").to_u64()),
            Ok(20405029553322)
        );
        assert_eq!(
            (Scalar::new(b"+20405029553322").to_u64()),
            Ok(20405029553322)
        );
        assert_eq!(
            (Scalar::new(b"18446744073709551615").to_u64()),
            Ok(18446744073709551615)
        );
        assert_eq!(
            (Scalar::new(b"+18446744073709551615").to_u64()),
            Ok(18446744073709551615)
        );
    }

    #[test]
    fn scalar_to_u64_overflow() {
        assert!(
            Scalar::new(b"888888888888888888888888888888888")
                .to_u64()
                .is_err()
        );
        assert!(Scalar::new(b"666666666666666685902").to_u64().is_err());
        assert!(Scalar::new(b"184467440737095516106").to_u64().is_err());
    }

    #[test]
    fn scalar_to_f64_overflow() {
        assert!(
            Scalar::new(b"9999999999.99999999999999999")
                .to_f64()
                .is_err()
        );
        assert!(
            Scalar::new(b"999999999999999999999.999999999")
                .to_f64()
                .is_err()
        );
        assert!(Scalar::new(b"10.99999990999999999999999").to_f64().is_err());
    }

    #[test]
    fn scalar_empty_string() {
        let s = Scalar::new(b"");
        assert!(s.to_bool().is_err());
        assert!(s.to_f64().is_err());
        assert!(s.to_i64().is_err());
        assert!(s.to_u64().is_err());
    }

    #[test]
    fn scalar_precision() {
        let s = Scalar::new(b"90071992547409097");
        assert_eq!(s.to_i64(), Ok(90071992547409097));
        assert_eq!(s.to_u64(), Ok(90071992547409097));
        let fl = s.to_f64().unwrap_err();
        assert_eq!(fl, ScalarError::PrecisionLoss(90071992547409100.0));

        let s = Scalar::new(b"18446744073709547616");
        assert!(s.to_i64().is_err());
        assert_eq!(s.to_u64(), Ok(18446744073709547616));
        let fl = s.to_f64().unwrap_err();
        assert_eq!(fl, ScalarError::PrecisionLoss(18446744073709548000.0));

        let s = Scalar::new(b"-90071992547409097");
        assert_eq!(s.to_i64(), Ok(-90071992547409097));
        assert!(s.to_u64().is_err());
        let fl = s.to_f64().unwrap_err();
        assert_eq!(fl, ScalarError::PrecisionLoss(-90071992547409100.0));
    }

    #[quickcheck]
    fn to_string_equality(data: Vec<u8>) -> bool {
        use encoding_rs::*;
        let (cow, _) = WINDOWS_1252.decode_without_bom_handling(&data);
        let actual: String = data
            .iter()
            .map(|&x| crate::data::WINDOWS_1252[x as usize])
            .collect();

        cow.into_owned() == actual
    }
}
