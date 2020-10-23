use crate::{ascii::is_ascii, decode_windows1252};
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
}

impl fmt::Display for ScalarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScalarError::AllDigits => write!(f, "did not contain all digits"),
            ScalarError::InvalidBool => write!(f, "is not a valid bool"),
            ScalarError::Overflow => write!(f, "caused an overflow"),
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
    pub fn view_data(&self) -> &'a [u8] {
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
    pub fn to_f64(&self) -> Result<f64, ScalarError> {
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
    pub fn to_bool(&self) -> Result<bool, ScalarError> {
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
    pub fn to_i64(&self) -> Result<i64, ScalarError> {
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
    pub fn to_u64(&self) -> Result<u64, ScalarError> {
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
    pub fn is_ascii(&self) -> bool {
        is_ascii(self.data)
    }
}

impl<'a> fmt::Debug for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Scalar {{ {} }}", self)
    }
}

impl<'a> fmt::Display for Scalar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if is_ascii(self.data) {
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

#[inline]
fn to_f64(d: &[u8]) -> Result<f64, ScalarError> {
    match d.iter().position(|&x| x == b'.') {
        Some(idx) => {
            let lead = to_i64(&d[..idx])?;

            // https://graphics.stanford.edu/~seander/bithacks.html#CopyIntegerSign
            let sign = 1 | (lead >> (std::mem::size_of::<i64>() * 8 - 1));
            let leadf = lead as f64;
            let trail = &d[idx + 1..];
            let frac = to_i64(&trail)? as f64;
            let digits = 10u32
                .checked_pow(trail.len() as u32)
                .ok_or_else(|| ScalarError::Overflow)? as f64;
            Ok((sign as f64).mul_add(frac / digits, leadf))
        }
        None => to_i64(d).map(|x| x as f64),
    }
}

#[inline]
fn to_i64(d: &[u8]) -> Result<i64, ScalarError> {
    let is_negative = d.get(0).map_or(false, |&x| x == b'-');
    let isn = is_negative as u64;
    let sign = -((isn as i64 * 2).wrapping_sub(1));
    let rest = to_u64(&d[isn as usize..])?;
    Ok(sign * (rest as i64))
}

/// Convert a buffer to an u64. This function is micro-optimized for small
/// inputs. Previous implementations had higher throughput on larger input but
/// couldn't parse small inputs as quickly. Micro-optimizing this function for
/// small inputs shaved ~7% off deserializing 120MB save as 10% of all time
/// was spent in this function. Dates are a common occurrence of numbers that
/// are 1-4 digits in length
#[inline]
fn to_u64(d: &[u8]) -> Result<u64, ScalarError> {
    if d.is_empty() {
        return Err(ScalarError::AllDigits);
    }

    let mut result: u64 = 0;
    for digit in d.iter().map(|x| x.wrapping_sub(b'0')) {
        if digit > 9 {
            return Err(ScalarError::AllDigits);
        }

        let (new_result1, overflow1) = result.overflowing_mul(10);
        let (new_result2, overflow2) = new_result1.overflowing_add(u64::from(digit));
        if overflow1 | overflow2 {
            return Err(ScalarError::Overflow);
        }

        result = new_result2;
    }

    Ok(result)
}

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
        assert!(Scalar::new(b"10.99999999999999").to_f64().is_err());
    }

    #[test]
    fn scalar_empty_string() {
        let s = Scalar::new(b"");
        assert!(s.to_bool().is_err());
        assert!(s.to_f64().is_err());
        assert!(s.to_i64().is_err());
        assert!(s.to_u64().is_err());
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
