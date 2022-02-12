use crate::{
    data::is_whitespace, data::WINDOWS_1252, util::contains_zero_byte, util::le_u64,
    util::repeat_byte,
};
use std::borrow::Cow;

/// An encoding for interpreting byte data as UTF-8 text
///
/// Used in both text and binary format deserializers
///
/// It is heavily encouraged that encoding implementations are marked
/// as `Copy` to make sure they are as cheap to copy as possible. In
/// an experiment storing the encoding in a `Rc` resulted in a decrease
/// of deserialization throughput by over 10%, as encodings are passed
/// around everywhere.
///
/// An encoding should also perform additional actions when:
///
/// - trailing whitespace is removed
/// - escape sequences are unescaped
pub trait Encoding {
    /// Decodes bytes into a utf-8 compatible string -- allocating if necessary
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str>;
}

/// Decodes bytes according to the windows1252 code page
///
/// ```
/// use jomini::{Windows1252Encoding, Encoding};
///
/// let encoding = Windows1252Encoding::new();
/// assert_eq!(encoding.decode(b"Common Sense"), "Common Sense");
/// assert_eq!(
///     encoding.decode(b"\xa7GRichard Plantagenet\xa7 ( 2 / 4 / 3 / 0 )"),
///     "§GRichard Plantagenet§ ( 2 / 4 / 3 / 0 )"
/// );
/// assert_eq!(encoding.decode(br#"Captain \"Joe\" Rogers"#), r#"Captain "Joe" Rogers"#);
/// assert_eq!(encoding.decode(b"1444.11.11\n"), "1444.11.11");
/// assert_eq!(encoding.decode(b"\xff"), "ÿ");
/// assert_eq!(encoding.decode(b"\x8a"), "Š");
/// assert_eq!(encoding.decode(b"\xfe\xff\xfe\xff\xfe\xff\xfe\xff\xfe\xff"), "þÿþÿþÿþÿþÿ");
/// assert_eq!(encoding.decode(b"hi\x81\x8a"), "hi\u{81}Š");
/// ```
#[derive(Debug, Default, Copy, Clone)]
pub struct Windows1252Encoding;

impl Windows1252Encoding {
    /// Creates a new windows 1252 decoder
    pub fn new() -> Self {
        Self::default()
    }

    /// Static method for decoding windows 1252 data
    pub fn decode(data: &[u8]) -> Cow<str> {
        decode_windows1252(data)
    }
}

impl Encoding for Windows1252Encoding {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Windows1252Encoding::decode(data)
    }
}

impl<T: Encoding + ?Sized> Encoding for &'_ T {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        (**self).decode(data)
    }
}

impl<T: Encoding + ?Sized> Encoding for Box<T> {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        (**self).decode(data)
    }
}

/// Decodes bytes according to the utf8 standard
///
/// ```
/// use jomini::{Utf8Encoding, Encoding};
///
/// let encoding = Utf8Encoding::new();
/// assert_eq!(encoding.decode(b"Common Sense"), "Common Sense");
/// assert_eq!(encoding.decode(br#"Captain \"Joe\" Rogers"#), r#"Captain "Joe" Rogers"#);
/// assert_eq!(encoding.decode(b"1444.11.11\n"), "1444.11.11");
/// assert_eq!(encoding.decode(b"J\xc3\xa5hk\xc3\xa5m\xc3\xa5hkke"), "Jåhkåmåhkke");
/// assert_eq!(encoding.decode("Jåhkåmåhkke".as_bytes()), "Jåhkåmåhkke");
/// ```
#[derive(Debug, Default, Copy, Clone)]
pub struct Utf8Encoding;

impl Utf8Encoding {
    /// Creates a new utf8 decoder
    pub fn new() -> Self {
        Self::default()
    }

    /// Static method for decoding utf8 data
    pub fn decode(data: &[u8]) -> Cow<str> {
        decode_utf8(data)
    }
}

impl Encoding for Utf8Encoding {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

#[inline]
fn trim_trailing_whitepsace(d: &[u8]) -> &[u8] {
    // First we check if there is trailing whitespace and if there is, we trim it down.
    // This branch won't normally be taken, so it should be considered cheap
    if !d.is_empty() && is_whitespace(d[d.len() - 1]) {
        let ind = d
            .iter()
            .rev()
            .position(|x| !is_whitespace(*x))
            .unwrap_or_else(|| d.len());
        &d[0..d.len() - ind]
    } else {
        d
    }
}

#[inline]
pub(crate) fn decode_windows1252(d: &[u8]) -> Cow<str> {
    let d = trim_trailing_whitepsace(d);

    // Then we iterate through the data in 8 byte chunks and ensure that each chunk
    // is contained of ascii characters with no escape characters
    let mut chunk_iter = d.chunks_exact(8);
    let mut offset = 0;
    for n in &mut chunk_iter {
        let wide = le_u64(n);
        if wide & 0x8080_8080_8080_8080 != 0 || contains_zero_byte(wide ^ repeat_byte(b'\\')) {
            return Cow::Owned(windows_1252_create(d, offset));
        }

        offset += 8;
    }

    // Same logic as before but instead of operating on 8 bytes at a time, work bytewise
    let remainder = chunk_iter.remainder();
    for &byte in remainder {
        if !byte.is_ascii() || byte == b'\\' {
            return Cow::Owned(windows_1252_create(d, offset));
        }

        offset += 1;
    }

    // This is safe as we just checked that the data is ascii and ascii is a subset of utf8
    debug_assert!(std::str::from_utf8(d).is_ok());
    let s = unsafe { std::str::from_utf8_unchecked(d) };
    Cow::Borrowed(s)
}

fn windows_1252_create(d: &[u8], offset: usize) -> String {
    let (upto, rest) = d.split_at(offset);
    let mut result = String::with_capacity(d.len());
    let head = unsafe { std::str::from_utf8_unchecked(upto) };
    result.push_str(head);
    for &c in rest.iter().filter(|&x| *x != b'\\') {
        result.push(WINDOWS_1252[c as usize]);
    }

    result
}

#[inline]
pub(crate) fn decode_utf8(d: &[u8]) -> Cow<str> {
    let d = trim_trailing_whitepsace(d);

    // Then we iterate through the data in 8 byte chunks and ensure that each chunk
    // has no escape characters
    let mut chunk_iter = d.chunks_exact(8);
    let mut offset = 0;
    let mut is_ascii = true;
    for n in &mut chunk_iter {
        let wide = le_u64(n);
        is_ascii &= wide & 0x8080_8080_8080_8080 == 0;
        if contains_zero_byte(wide ^ repeat_byte(b'\\')) {
            return Cow::Owned(utf8_create(d, offset));
        }

        offset += 8;
    }

    // Same logic as before but instead of operating on 8 bytes at a time, work bytewise
    let remainder = chunk_iter.remainder();
    for &byte in remainder {
        is_ascii &= byte.is_ascii();
        if byte == b'\\' {
            return Cow::Owned(utf8_create(d, offset));
        }

        offset += 1;
    }

    // Most the strings we'll be decoding are ascii, so we have an ascii fast path. If we don't
    // detect any non-ascii characters then we can immediately create the borrowed string without
    // validation. This causes the string benchmarks to be 2-3x faster on ascii inputs (with ~5%
    // decrease on utf-8 inputs). Large string decodes saw perf increases of 5x.
    if is_ascii {
        // This is safe as we just checked that the data is ascii and ascii is a subset of utf8
        debug_assert!(std::str::from_utf8(d).is_ok());
        let s = unsafe { std::str::from_utf8_unchecked(d) };
        Cow::Borrowed(s)
    } else {
        String::from_utf8_lossy(d)
    }
}

fn utf8_create(d: &[u8], offset: usize) -> String {
    let (upto, rest) = d.split_at(offset);
    let mut result = Vec::with_capacity(d.len());
    result.extend_from_slice(upto);
    for &c in rest.iter().filter(|&x| *x != b'\\') {
        result.push(c);
    }

    String::from_utf8(result)
        .unwrap_or_else(|e| String::from_utf8_lossy(&e.into_bytes()).into_owned())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_string_trim_end_newlines() {
        assert_eq!(Windows1252Encoding::decode(b"new\n"), "new");
        assert_eq!(Windows1252Encoding::decode(b"\t"), "");
    }

    #[test]
    fn scalar_string_escapes() {
        let data = br#"Joe \"Captain\" Rogers"#;
        assert_eq!(Windows1252Encoding::decode(data), r#"Joe "Captain" Rogers"#);
    }

    #[test]
    fn scalar_utf8_string_escapes() {
        let data = br#"Joe \"Captain\" Rogers"#;
        assert_eq!(Utf8Encoding::decode(data), r#"Joe "Captain" Rogers"#);

        let data = br#"Joe Captain\"s"#;
        assert_eq!(Utf8Encoding::decode(data), r#"Joe Captain"s"#);
    }

    #[test]
    fn scalar_invalid_utf8_replace() {
        let data = b"Joe\xffcheeze";
        assert_eq!(Utf8Encoding::decode(data), "Joe�cheeze");
    }

    #[test]
    fn scalar_to_string_undefined_characters() {
        // According to the information on Microsoft's and the Unicode Consortium's websites,
        // positions 81, 8D, 8F, 90, and 9D are unused; however, the Windows API
        // MultiByteToWideChar maps these to the corresponding C1 control codes. The "best fit"
        // mapping documents this behavior, too

        let data = &[0x81, 0x8d, 0x8f, 0x90, 0x9d];
        let (cow, _) = encoding_rs::WINDOWS_1252.decode_without_bom_handling(data);
        assert_eq!(Windows1252Encoding::decode(data), cow);
    }
}
