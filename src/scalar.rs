use crate::ascii::is_ascii;
use std::borrow::Cow;
use std::fmt;

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

    /// Try converting the scalar to i64
    pub fn to_i64(&self) -> i64 {
        self.to_string().parse::<i64>().unwrap()
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
        if self.data.is_ascii() {
            // This is safe as we just checked that the data is ascii and ascii is a subset of utf8
            debug_assert!(std::str::from_utf8(self.data).is_ok());
            let s = unsafe { std::str::from_utf8_unchecked(self.data) };
            Cow::Borrowed(s)
        } else {
            Cow::Owned(self.to_windows_1252())
        }
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
        self.to_utf8().into_owned()
    }

    fn to_windows_1252(&self) -> String {
        // For short strings, creating the string a byte at a time is 50% faster than encoding_rs.
        // Since we deal with a lot of short strings, this optimization is worth it for when
        // encountering non-ascii text
        if self.data.len() < 10 {
            use crate::data::WINDOWS_1252;
            self.data
                .iter()
                .map(|&x| WINDOWS_1252[x as usize])
                .collect()
        } else {
            let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(self.data);
            cow.into_owned()
        }
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
    fn scalar_to_i64() {
        assert_eq!((Scalar::new(b"0").to_i64()), 0);
        assert_eq!((Scalar::new(b"1").to_i64()), 1);
        assert_eq!((Scalar::new(b"-1").to_i64()), -1);
        assert_eq!((Scalar::new(b"-10000").to_i64()), -10000);
        assert_eq!((Scalar::new(b"10000").to_i64()), 10000);
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
