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
#[derive(PartialEq, Copy, Clone)]
pub struct Scalar<'a> {
    data: &'a [u8],
}

impl<'a> Scalar<'a> {
    /// Create a new scalar backed by a byte slice
    pub fn new(data: &'a [u8]) -> Scalar<'a> {
        Scalar { data }
    }

    /// View the raw data
    pub fn view_data(self) -> &'a [u8] {
        self.data
    }

    /// Try converting the scalar to f64
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"1.000");
    /// assert_eq!(v1.to_f64(), Ok(1.0));
    ///
    /// let v2 = Scalar::new(b"-5.67821");
    /// assert_eq!(v2.to_f64(), Ok(-5.67821));
    /// ```
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
    pub fn is_ascii(self) -> bool {
        self.data.is_ascii()
    }
}

impl<'a> fmt::Debug for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Scalar {{ {} }}", self)
    }
}

impl<'a> fmt::Display for Scalar<'a> {
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

/// Inspired by https://github.com/lemire/fast_double_parser
#[inline]
fn to_f64(d: &[u8]) -> Result<f64, ScalarError> {
    let mut data = d;
    if data.is_empty() {
        return Err(ScalarError::AllDigits);
    }

    let mut negative = false;
    if data[0] == b'-' {
        negative = true;
        data = &data[1..];
    }

    let (lead, mut left) = if data.get(0).map_or(false, |&x| x == b'.') {
        (0, data)
    } else {
        to_u64_t(data, 0)?
    };

    if left.is_empty() {
        if negative {
            let val = i64::try_from(lead)
                .map(|x| -x)
                .map_err(|_| ScalarError::Overflow)?;
            let result = val as f64;

            if val < -9007199254740991 || val > 9007199254740991 {
                Err(ScalarError::PrecisionLoss(result))
            } else {
                Ok(result)
            }
        } else {
            let val = lead;
            let result = val as f64;
            if val > 9007199254740991 {
                Err(ScalarError::PrecisionLoss(result))
            } else {
                Ok(result)
            }
        }
    } else if left[0] == b'.' {
        left = &left[1..];
        let exponent = data.len() - (data.len() - left.len());
        let (i, left) = to_u64_t(left, lead)?;
        if !left.is_empty() {
            return Err(ScalarError::AllDigits);
        }

        let pow = POWER_OF_TEN.get(exponent).ok_or(ScalarError::Overflow)?;
        let d = (i as f64) / *pow;
        let sign = -((negative as i64 * 2).wrapping_sub(1)) as f64;
        Ok(sign * d)
    } else {
        Err(ScalarError::AllDigits)
    }
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
    let is_negative = d.get(0).map_or(false, |&x| x == b'-');
    let isn = is_negative as u64;
    let sign = -((isn as i64 * 2).wrapping_sub(1));
    let (val, rest) = to_u64_t(&d[isn as usize..], 0)?;
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
    let (result, left) = to_u64_t(d, 0)?;
    if left.is_empty() {
        Ok(result)
    } else {
        Err(ScalarError::AllDigits)
    }
}

#[inline]
pub(crate) fn to_u64_t(d: &[u8], start: u64) -> Result<(u64, &[u8]), ScalarError> {
    if d.is_empty() || !d[0].is_ascii_digit() {
        return Err(ScalarError::AllDigits);
    }

    let digit = d[0] - b'0';
    let mut result = if start == 0 {
        u64::from(digit)
    } else {
        overflow_mul_add(start, digit)?
    };

    for (i, &x) in (&d[1..]).iter().enumerate() {
        if !x.is_ascii_digit() {
            return Ok((result, &d[i + 1..]));
        }

        result = overflow_mul_add(result, x - b'0')?;
    }

    Ok((result, &[]))
}

#[inline]
fn overflow_mul_add(acc: u64, digit: u8) -> Result<u64, ScalarError> {
    let (new_result1, overflow1) = acc.overflowing_mul(10);
    let (new_result2, overflow2) = new_result1.overflowing_add(u64::from(digit));
    if overflow1 | overflow2 {
        return Err(ScalarError::Overflow);
    }

    Ok(new_result2)
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
        assert!(Scalar::new(b"E").to_f64().is_err());
        assert!(Scalar::new(b"").to_f64().is_err());
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
        assert_eq!((Scalar::new(b"10000").to_u64()), Ok(10000));
        assert_eq!((Scalar::new(b"20405029").to_u64()), Ok(20405029));
        assert_eq!(
            (Scalar::new(b"20405029553322").to_u64()),
            Ok(20405029553322)
        );
    }

    #[test]
    fn scalar_to_u64_overflow() {
        assert!(Scalar::new(b"888888888888888888888888888888888")
            .to_u64()
            .is_err());
        assert!(Scalar::new(b"666666666666666685902").to_u64().is_err());
    }

    #[test]
    fn scalar_to_f64_overflow() {
        assert!(Scalar::new(b"9999999999.99999999999999999")
            .to_f64()
            .is_err());
        assert!(Scalar::new(b"999999999999999999999.999999999")
            .to_f64()
            .is_err());
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
