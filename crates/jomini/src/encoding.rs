use crate::data::WINDOWS_1252;
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
/// An encoding should also remove trailing whitespace.
///
/// An encoding does not unescape. Escapes such as `\"` are part of the text
/// syntax, and binary strings have none, so the text deserializers and
/// readers unescape quoted scalars before they call the encoding. See
/// [`text::unescape`](crate::text::unescape).
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
/// assert_eq!(encoding.decode(br"gfx\\interface\icon.dds"), r"gfx\\interface\icon.dds");
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
        Windows1252Encoding
    }

    /// Static method for decoding windows 1252 data
    pub fn decode(data: &[u8]) -> Cow<'_, str> {
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
/// assert_eq!(encoding.decode(br"gfx\\interface\icon.dds"), r"gfx\\interface\icon.dds");
/// assert_eq!(encoding.decode(b"1444.11.11\n"), "1444.11.11");
/// assert_eq!(encoding.decode(b"J\xc3\xa5hk\xc3\xa5m\xc3\xa5hkke"), "Jåhkåmåhkke");
/// assert_eq!(encoding.decode("Jåhkåmåhkke".as_bytes()), "Jåhkåmåhkke");
/// ```
#[derive(Debug, Default, Copy, Clone)]
pub struct Utf8Encoding;

impl Utf8Encoding {
    /// Creates a new utf8 decoder
    pub fn new() -> Self {
        Utf8Encoding
    }

    /// Static method for decoding utf8 data
    pub fn decode(data: &[u8]) -> Cow<'_, str> {
        decode_utf8(data)
    }
}

impl Encoding for Utf8Encoding {
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        Utf8Encoding::decode(data)
    }
}

// https://github.com/rust-lang/rust/blob/767453eb7ca188e991ac5568c17b984dd4893e77/library/core/src/slice/ascii.rs#L159-L171
const fn trim_ascii_end(data: &[u8]) -> &[u8] {
    let mut bytes = data;

    // Note: A pattern matching based approach (instead of indexing) allows
    // making the function const.
    while let [rest @ .., last] = bytes {
        if last.is_ascii_whitespace() {
            bytes = rest;
        } else {
            break;
        }
    }
    bytes
}

/// A branch-free loop that LLVM vectorizes.
#[inline]
fn is_ascii(data: &[u8]) -> bool {
    let mut ascii = true;
    for &b in data {
        ascii &= b.is_ascii();
    }
    ascii
}

#[inline]
pub(crate) fn decode_windows1252(d: &[u8]) -> Cow<'_, str> {
    let bytes = trim_ascii_end(d);
    if is_ascii(bytes) {
        debug_assert!(std::str::from_utf8(bytes).is_ok());
        // SAFETY: every byte is ascii, and ascii is a subset of utf-8
        let s = unsafe { std::str::from_utf8_unchecked(bytes) };
        Cow::Borrowed(s)
    } else {
        Cow::Owned(windows_1252_create(bytes))
    }
}

#[inline(never)]
fn windows_1252_create(d: &[u8]) -> String {
    // size estimate: every byte needs translation
    let mut result = String::with_capacity(d.len() * 2);
    for &c in d {
        result.push(WINDOWS_1252[c as usize]);
    }
    result
}

#[inline]
pub(crate) fn decode_utf8(d: &[u8]) -> Cow<'_, str> {
    let d = trim_ascii_end(d);

    // Most the strings we'll be decoding are ascii, so we have an ascii fast path. If we don't
    // detect any non-ascii characters then we can immediately create the borrowed string without
    // validation. This causes the string benchmarks to be 2-3x faster on ascii inputs (with ~5%
    // decrease on utf-8 inputs). Large string decodes saw perf increases of 5x.
    if is_ascii(d) {
        debug_assert!(std::str::from_utf8(d).is_ok());
        // SAFETY: every byte is ascii, and ascii is a subset of utf-8
        let s = unsafe { std::str::from_utf8_unchecked(d) };
        Cow::Borrowed(s)
    } else {
        String::from_utf8_lossy(d)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_string_trim_end_newlines() {
        assert_eq!(Windows1252Encoding::decode(b""), "");
        assert_eq!(Windows1252Encoding::decode(b"new\n"), "new");
        assert_eq!(Windows1252Encoding::decode(b"\t"), "");
        assert_eq!(
            Windows1252Encoding::decode(b"new \\\"Captain\\\" \n"),
            r#"new \"Captain\""#
        );
        assert_eq!(Windows1252Encoding::decode(b"new\xF8 "), "newø")
    }

    #[test]
    fn scalar_string_trim_end_newlines_utf8() {
        assert_eq!(Utf8Encoding::decode(b""), "");
        assert_eq!(Utf8Encoding::decode(b"new\n"), "new");
        assert_eq!(Utf8Encoding::decode(b"\t"), "");
        assert_eq!(
            Utf8Encoding::decode(b"new \\\"Captain\\\" \n"),
            r#"new \"Captain\""#
        );
        assert_eq!(Utf8Encoding::decode("newø ".as_bytes()), "newø")
    }

    #[test]
    fn scalar_string_keeps_backslashes() {
        let data = br#"Joe \"Captain\" Rogers\n"#;
        assert_eq!(
            Windows1252Encoding::decode(data),
            r#"Joe \"Captain\" Rogers\n"#
        );
        assert_eq!(Utf8Encoding::decode(data), r#"Joe \"Captain\" Rogers\n"#);

        let data = b"\\\xa7G+20.0%\xa7!\\n";
        assert_eq!(Windows1252Encoding::decode(data), "\\§G+20.0%§!\\n");
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
