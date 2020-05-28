use crate::ascii::is_ascii;
use std::borrow::Cow;
use std::error;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ScalarError {
    AllDigits(String),
    Overflow(String),
    InvalidBool(String),
}

impl fmt::Display for ScalarError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ScalarError::AllDigits(x) => write!(f, "did not contain all digits: {}", x),
            ScalarError::InvalidBool(x) => write!(f, "is not a valid bool: {}", x),
            ScalarError::Overflow(x) => write!(f, "caused an overflow: {}", x),
        }
    }
}

impl error::Error for ScalarError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// Single value encapsulating windows-1252 data
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
    pub fn to_f64(&self) -> Result<f64, ScalarError> {
        to_f64(self.data)
    }

    /// Try converting the scalar to f64
    pub fn to_bool(&self) -> Result<bool, ScalarError> {
        to_bool(self.data)
    }

    /// Try converting the scalar to i64
    pub fn to_i64(&self) -> Result<i64, ScalarError> {
        to_i64(self.data)
    }

    /// Try converting the scalar to u64
    pub fn to_u64(&self) -> Result<u64, ScalarError> {
        to_u64(self.data)
    }

    /// Convert scalar data into utf8. Will allocate if the string is not utf8.
    ///
    /// ```
    /// use jomini::Scalar;
    ///
    /// let v1 = Scalar::new(b"a");
    /// assert_eq!(v1.to_utf8(), "a");
    ///
    /// let v2 = Scalar::new(&[255][..]);
    /// assert_eq!(v2.to_utf8(), "ÿ");
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
fn to_utf8<'a>(d: &'a [u8]) -> Cow<'a, str> {
    if is_ascii(d) {
        // This is safe as we just checked that the data is ascii and ascii is a subset of utf8
        debug_assert!(std::str::from_utf8(d).is_ok());
        let s = unsafe { std::str::from_utf8_unchecked(d) };
        Cow::Borrowed(s)
    } else {
        Cow::Owned(to_windows_1252(d))
    }
}

#[cfg(not(feature = "encoding_rs"))]
#[inline]
fn to_windows_1252(d: &[u8]) -> String {
    d.iter().map(|&x| crate::data::WINDOWS_1252[x as usize]).collect()
}

#[cfg(feature = "encoding_rs")]
#[inline]
fn to_windows_1252(d: &[u8]) -> String {
    // For short strings, creating the string a byte at a time is 50% faster than encoding_rs.
    // Since we deal with a lot of short strings, this optimization is worth it for when
    // encountering non-ascii text
    if d.len() < 10 {
        use crate::data::WINDOWS_1252;
        d.iter().map(|&x| WINDOWS_1252[x as usize]).collect()
    } else {
        let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(d);
        cow.into_owned()
    }
}

#[inline]
fn to_bool(d: &[u8]) -> Result<bool, ScalarError> {
    match d {
        [b'y', b'e', b's'] => Ok(true),
        [b'n', b'o'] => Ok(false),
        x => Err(ScalarError::InvalidBool(to_utf8_owned(x))),
    }
}

fn is_digits(d: &[u8]) -> bool {
    // Taken from simdjson: https://youtu.be/wlvKAT7SZIQ?t=2377
    const SIZE: usize = std::mem::size_of::<u64>();
    debug_assert!(d.len() <= SIZE);

    let mut local_buf: [u8; SIZE] = [b'0'; SIZE];
    let dst = &mut local_buf[..d.len()];
    dst.copy_from_slice(d);

    let val = u64::from_le_bytes(local_buf);
    val.checked_add(0x0606_0606_0606_0606).map_or(false, |x| {
        ((val & 0xF0F0_F0F0_F0F0_F0F0) | ((x & 0xF0F0_F0F0_F0F0_F0F0) >> 4))
            == 0x3333_3333_3333_3333
    })
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
    match memchr::memchr(b'.', d) {
        Some(idx) => {
            let lead = to_i64(&d[..idx])?;

            // https://graphics.stanford.edu/~seander/bithacks.html#CopyIntegerSign
            let sign = 1 | (lead >> (std::mem::size_of::<i64>() * 8 - 1));
            let leadf = lead as f64;
            let trail = &d[idx + 1..];
            let frac = to_i64(&trail)? as f64;
            let digits = 10u32.checked_pow(trail.len() as u32)
                .ok_or_else(|| ScalarError::Overflow(to_utf8_owned(d)))? as f64;
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
    let mut chunks = d.chunks_exact(8);
    let all_digits = chunks.all(is_digits);
    if !(all_digits & is_digits(chunks.remainder())) {
        return Err(ScalarError::AllDigits(to_utf8_owned(d)));
    }

    let mut result: u64 = 0;
    let chunks = d.chunks_exact(8);
    for chunk in chunks {
        let chunk_ptr = chunk.as_ptr() as *const u8 as *const u64;
        let val = unsafe { ::std::ptr::read_unaligned(chunk_ptr).to_le() };

        result = result.checked_mul(100_000_000)
            .and_then(|x| x.checked_add(ascii_u64_to_digits(val)))
            .and_then(|x| x.checked_add(result))
            .ok_or_else(|| ScalarError::Overflow(to_utf8_owned(d)))?;
    }

    let left_over = d.len() % 8;
    let remainder = &d[d.len() - left_over..];
    result = 10_u64.checked_pow(remainder.len() as u32)
        .and_then(|x| result.checked_mul(x))
        .ok_or_else(|| ScalarError::Overflow(to_utf8_owned(d)))?;

    let mut local_buf: [u8; 8] = [b'0'; 8];
    let dst = &mut local_buf[8 - remainder.len()..];
    dst.copy_from_slice(remainder);

    let val = u64::from_le_bytes(local_buf);
    let result = result.checked_add(ascii_u64_to_digits(val))
        .ok_or_else(|| ScalarError::Overflow(to_utf8_owned(d)))?;

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
        assert_eq!((Scalar::new(b"10000").to_u64()), Ok(10000));
        assert_eq!((Scalar::new(b"20405029").to_u64()), Ok(20405029));
        assert_eq!(
            (Scalar::new(b"20405029553322").to_u64()),
            Ok(20405029553322)
        );
    }

    #[test]
    fn scalar_to_u64_overflow() {
        assert!(Scalar::new(b"888888888888888888888888888888888").to_u64().is_err());
        assert!(Scalar::new(b"666666666666666685902").to_u64().is_err());
    }

    #[test]
    fn scalar_to_f64_overflow() {
        assert!(Scalar::new(b"9999999999.99999999999999999").to_f64().is_err());
        assert!(Scalar::new(b"999999999999999999999.999999999").to_f64().is_err());
        assert!(Scalar::new(b"10.99999990999999999999999").to_f64().is_err());
        assert!(Scalar::new(b"10.99999999999999").to_f64().is_err());
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

    #[quickcheck]
    fn to_string_equality(data: Vec<u8>) -> bool {
        use encoding_rs::*;
        let (cow, _, _) = WINDOWS_1252.decode(&data);
        let scalar = Scalar::new(&data);
        cow.into_owned() == scalar.to_utf8()
    }
}
