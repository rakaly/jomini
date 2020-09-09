use crate::ascii::is_ascii;
use crate::data::{is_whitespace, WINDOWS_1252};
use crate::util::{contains_zero_byte, le_u64, repeat_byte};
use std::borrow::Cow;
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

/// Single value encapsulating windows-1252 data.
///
/// Since windows-1252 is a single byte character encoding, a scalar will never fail to be created.
///
/// ```
/// use jomini::Scalar;
///
/// let v1 = Scalar::new(b"10");
/// assert_eq!(v1.to_utf8(), "10");
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
    /// Create a new scalar backed by windows-1252 encoded byte slice
    pub fn new(data: &'a [u8]) -> Scalar<'a> {
        Scalar { data }
    }

    /// View the underlying windows-1252 encoded data
    pub fn view_data(&self) -> &[u8] {
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

    /// Convert scalar data into utf8. Several transformations take place:
    ///
    /// - trailing whitespace is removed
    /// - escape sequences are unescaped
    /// - windows-1252 specific characters encoded as their utf-8 equivalent.
    ///
    /// This function is optimized for the typical scenario, where the utf-8
    /// string can be calculated without allocation. If escape sequences or
    /// windows-1252 specific characters are encountered, then allocation is
    /// necessary.
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"Common Sense");
    /// assert_eq!(v1.to_utf8(), "Common Sense");
    ///
    /// let v2 = Scalar::new(b"\xa7GRichard Plantagenet\xa7 ( 2 / 4 / 3 / 0 )");
    /// assert_eq!(v2.to_utf8(), "§GRichard Plantagenet§ ( 2 / 4 / 3 / 0 )");
    ///
    /// let v3 = Scalar::new(br#"Captain \"Joe\" Rogers"#);
    /// assert_eq!(v3.to_utf8(), r#"Captain "Joe" Rogers"#);
    ///
    /// let v4 = Scalar::new(b"1444.11.11\n");
    /// assert_eq!(v4.to_utf8(), "1444.11.11");
    /// ```
    pub fn to_utf8(&self) -> Cow<'a, str> {
        to_utf8(self.data)
    }

    /// Convert scalar data into an owned string
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"a");
    /// assert_eq!(v1.to_utf8(), String::from("a"));
    ///
    /// let v2 = Scalar::new(&[255][..]);
    /// assert_eq!(v2.to_utf8(), String::from("ÿ"));
    /// ```
    pub fn to_utf8_owned(&self) -> String {
        to_utf8_owned(self.data)
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
        write!(f, "{}", self.to_utf8())
    }
}

#[inline]
fn to_utf8_owned(d: &[u8]) -> String {
    to_utf8(d).to_string()
}

#[inline]
fn to_utf8(mut d: &[u8]) -> Cow<str> {
    // First we check if there is trailing whitespace and if there is, we trim it down.
    // This branch won't normally be taken, so it should be considered cheap
    if !d.is_empty() && is_whitespace(d[d.len() - 1]) {
        let ind = d
            .iter()
            .rev()
            .position(|x| !is_whitespace(*x))
            .unwrap_or_else(|| d.len());
        d = &d[0..d.len() - ind];
    }

    // Then we iterate through the data in 8 byte chunks and ensure that each chunk
    // is contained of ascii characters with no escape characters
    let mut chunk_iter = d.chunks_exact(8);
    let mut offset = 0;
    while let Some(n) = chunk_iter.next() {
        let wide = le_u64(n);
        if wide & 0x80808080_80808080 != 0 || contains_zero_byte(wide ^ repeat_byte(b'\\')) {
            let (upto, rest) = d.split_at(offset);
            let mut result = String::with_capacity(d.len());
            let head = unsafe { std::str::from_utf8_unchecked(upto) };
            result.push_str(head);
            for &c in rest.iter().filter(|&x| *x != b'\\') {
                result.push(WINDOWS_1252[c as usize]);
            }

            return Cow::Owned(result);
        }

        offset += 8;
    }

    // Same logic as before but instead of operating on 8 bytes at a time, work bytewise
    let remainder = chunk_iter.remainder();
    for &byte in remainder {
        if !byte.is_ascii() || byte == b'\\' {
            let (upto, rest) = d.split_at(offset);
            let mut result = String::with_capacity(d.len());
            let head = unsafe { std::str::from_utf8_unchecked(upto) };
            result.push_str(head);
            for &c in rest.iter().filter(|&x| *x != b'\\') {
                result.push(WINDOWS_1252[c as usize]);
            }

            return Cow::Owned(result);
        }

        offset += 1;
    }

    // This is safe as we just checked that the data is ascii and ascii is a subset of utf8
    debug_assert!(std::str::from_utf8(d).is_ok());
    let s = unsafe { std::str::from_utf8_unchecked(d) };
    Cow::Borrowed(s)
}

#[inline]
fn to_bool(d: &[u8]) -> Result<bool, ScalarError> {
    match d {
        [b'y', b'e', b's'] => Ok(true),
        [b'n', b'o'] => Ok(false),
        _ => Err(ScalarError::InvalidBool),
    }
}

fn is_digits_wide(d: &[u8]) -> bool {
    // Taken from simdjson: https://youtu.be/wlvKAT7SZIQ?t=2377
    const SIZE: usize = std::mem::size_of::<u64>();
    debug_assert!(d.len() == SIZE);

    let val = le_u64(d);
    val.checked_add(0x0606_0606_0606_0606).map_or(false, |x| {
        ((val & 0xF0F0_F0F0_F0F0_F0F0) | ((x & 0xF0F0_F0F0_F0F0_F0F0) >> 4))
            == 0x3333_3333_3333_3333
    })
}

fn is_digits(d: &[u8]) -> bool {
    !d.iter().any(|&x| x < b'0' || x > b'9')
}

#[inline]
fn ascii_u64_to_digits(mut val: u64) -> u64 {
    // Taken from simdjson: https://youtu.be/wlvKAT7SZIQ?t=2479
    val = (val & 0x0F0F_0F0F_0F0F_0F0F).wrapping_mul(2561) >> 8;
    val = (val & 0x00FF_00FF_00FF_00FF).wrapping_mul(6553601) >> 16;
    (val & 0x0000_FFFF_0000_FFFF).wrapping_mul(42949672960001) >> 32
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
                .ok_or_else(|| ScalarError::Overflow)?
                as f64;
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

#[inline]
fn to_u64(d: &[u8]) -> Result<u64, ScalarError> {
    const POWER10: [u64; 8] = [10_000_000, 1_000_000, 100_000, 10_000, 1_000, 100, 10, 1];

    if d.is_empty() {
        return Err(ScalarError::AllDigits);
    }

    let mut chunks = d.chunks_exact(8);
    let all_digits = chunks.all(is_digits_wide);
    let remainder = chunks.remainder();
    if !(all_digits & is_digits(&remainder)) {
        return Err(ScalarError::AllDigits);
    }

    let mut result: u64 = 0;
    let chunks = d.chunks_exact(8);
    for chunk in chunks {
        let val = le_u64(chunk);

        result = result
            .checked_mul(100_000_000)
            .and_then(|x| x.checked_add(ascii_u64_to_digits(val)))
            .and_then(|x| x.checked_add(result))
            .ok_or_else(|| ScalarError::Overflow)?;
    }

    if result != 0 {
        result = 10_u64
            .checked_pow(remainder.len() as u32)
            .and_then(|x| result.checked_mul(x))
            .ok_or_else(|| ScalarError::Overflow)?;
    }

    let maxxed = 8 - remainder.len();
    for (i, &x) in remainder.iter().enumerate() {
        result = result
            .checked_add(u64::from(x - b'0') * POWER10[maxxed + i])
            .ok_or_else(|| ScalarError::Overflow)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn scalar_to_string() {
        assert_eq!((Scalar { data: &[255][..] }).to_string(), "ÿ".to_string());
        assert_eq!((Scalar { data: &[138][..] }).to_string(), "Š".to_string());
        assert_eq!(
            (Scalar {
                data: b"hello world"
            })
            .to_string(),
            "hello world".to_string()
        );
        assert_eq!(
            (Scalar {
                data: &[104, 105, 129, 138][..]
            })
            .to_string(),
            "hi\u{81}Š".to_string()
        );

        assert_eq!(
            (Scalar {
                data: &[0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff, 0xfe, 0xff]
            })
            .to_string(),
            "þÿþÿþÿþÿþÿ".to_string()
        );
    }

    #[test]
    fn scalar_string_trim_end_newlines() {
        assert_eq!(Scalar::new(b"new\n").to_utf8().as_ref(), "new");
        assert_eq!(Scalar::new(b"\t").to_utf8().as_ref(), "");
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
    fn scalar_string_escapes() {
        let s = Scalar::new(br#"Joe \"Captain\" Rogers"#);
        assert_eq!(s.to_utf8().as_ref(), r#"Joe "Captain" Rogers"#);
    }

    #[test]
    fn scalar_to_string_undefined_characters() {
        // According to the information on Microsoft's and the Unicode Consortium's websites,
        // positions 81, 8D, 8F, 90, and 9D are unused; however, the Windows API
        // MultiByteToWideChar maps these to the corresponding C1 control codes. The "best fit"
        // mapping documents this behavior, too

        let data = &[0x81, 0x8d, 0x8f, 0x90, 0x9d];
        let scalar = Scalar::new(data);
        let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(data);
        assert_eq!(scalar.to_utf8(), cow);
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
