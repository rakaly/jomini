use jomini::{binary::BinaryFlavor, Encoding, Windows1252Encoding};

/// The eu4 binary flavor and text encoding
///
/// Strings are Windows-1252, except for strings from the Japanese and Chinese
/// localization mods (both built on EU4dll), which escape UCS-2 code points
/// behind the control bytes `0x10` to `0x13`. Vanilla saves never contain
/// these bytes, so each string is detected and decoded on its own.
#[derive(Debug, Default, Clone, Copy)]
pub struct Eu4Flavor(Windows1252Encoding);

impl Eu4Flavor {
    /// Creates a new eu4 flavor
    pub fn new() -> Self {
        Eu4Flavor(Windows1252Encoding::new())
    }
}

impl Encoding for Eu4Flavor {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        // Nearly all strings are plain ascii, so one branch-free pass finds
        // the bytes that need more work: non-ascii, a backslash escape, or an
        // EU4dll escape. The loop is written so that it vectorizes.
        let mut plain = true;
        for &b in data {
            plain &= b.is_ascii() & (b != b'\\') & !is_eu4dll_escape(b);
        }

        if plain {
            let trimmed = data.trim_ascii_end();
            debug_assert!(std::str::from_utf8(trimmed).is_ok());
            // SAFETY: every byte is ascii, and ascii is a subset of utf-8
            let s = unsafe { std::str::from_utf8_unchecked(trimmed) };
            std::borrow::Cow::Borrowed(s)
        } else if data.iter().any(|&b| is_eu4dll_escape(b)) {
            // EU4dll escapes can start anywhere in a string, such as a Latin
            // family name followed by a localized suffix
            std::borrow::Cow::Owned(decode_eu4_escaped_text(data))
        } else {
            self.0.decode(data)
        }
    }
}

/// Returns true for the control bytes that start an EU4dll escape sequence
#[inline]
fn is_eu4dll_escape(b: u8) -> bool {
    b & 0xFC == 0x10
}

impl BinaryFlavor for Eu4Flavor {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        // First encoding is an i32 that has a fixed point offset of 3 decimal digits
        i32::from_le_bytes(data) as f32 / 1000.0
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        // Second encoding is Q49.15 with 5 fractional digits
        // https://en.wikipedia.org/wiki/Q_(number_format)
        let val = i64::from_le_bytes(data) as f64 / 32768.0;
        (val * 10_0000.0).round() / 10_0000.0
    }
}

/// Converts the EU4dll escaped encoding (Japanese and Chinese mods) into a utf-8 string
///
/// This function was converted from the original C++ code:
/// https://github.com/matanki-saito/EU4dll/blob/4b5e5e16ec09c6977f1c96dabc7e6bab16590b02/Plugin64/escape_tool.cpp
///
/// The author describes the encoding as: "Escaped Text -> wide char (ucs2) -> UTF 8"
#[cold]
pub fn decode_eu4_escaped_text(mut input: &[u8]) -> String {
    const ELLIPSIS: u32 = '…' as u32;
    let mut wide_chars = Vec::with_capacity(input.len());

    while let Some((&cp, rest)) = input.split_first() {
        input = rest;
        let code_point = match cp {
            0x10..=0x13 => {
                match input.split_first_chunk::<2>() {
                    None => ELLIPSIS,
                    Some(([low, high], rest)) => {
                        input = rest;
                        let mut sp = (u32::from(*high) << 8) + u32::from(*low);

                        // Apply escape transformations
                        sp = match cp {
                            0x10 => sp,
                            0x11 => sp.saturating_sub(0xE),
                            0x12 => sp.saturating_add(0x900),
                            0x13 => sp.saturating_add(0x8F2),
                            _ => sp,
                        };

                        if sp > 0xFFFF {
                            ELLIPSIS
                        } else {
                            sp
                        }
                    }
                }
            }
            // Backslash escapes are dropped like in the Windows-1252 decoder
            b'\\' => continue,
            _ => cp1252_to_ucs2(cp),
        };

        wide_chars.push(code_point as u16);
    }

    // Trailing whitespace is trimmed after the decode, as the last byte of
    // an escape payload can be an ascii whitespace byte
    let mut result = String::from_utf16_lossy(&wide_chars);
    let trimmed_len = result
        .trim_end_matches(|c: char| c.is_ascii_whitespace())
        .len();
    result.truncate(trimmed_len);
    result
}

/// Converts a CP1252 byte to its UCS-2 equivalent
fn cp1252_to_ucs2(cp: u8) -> u32 {
    match cp {
        0x80 => 0x20AC,     // Euro
        0x82 => 0x201A,     // Single low-9 quotation
        0x83 => 0x0192,     // Latin small f with hook
        0x84 => 0x201E,     // Double low-9 quotation
        0x85 => 0x2026,     // Horizontal ellipsis
        0x86 => 0x2020,     // Dagger
        0x87 => 0x2021,     // Double dagger
        0x88 => 0x02C6,     // Modifier letter circumflex
        0x89 => 0x2030,     // Per mille
        0x8A => 0x0160,     // Latin capital S with caron
        0x8B => 0x2039,     // Single left-pointing angle quotation
        0x8C => 0x0152,     // Latin capital ligature OE
        0x8E => 0x017D,     // Latin capital Z with caron
        0x91 => 0x2018,     // Left single quotation
        0x92 => 0x2019,     // Right single quotation
        0x93 => 0x201C,     // Left double quotation
        0x94 => 0x201D,     // Right double quotation
        0x95 => 0x2022,     // Bullet
        0x96 => 0x2013,     // En dash
        0x97 => 0x2014,     // Em dash
        0x98 => 0x02DC,     // Small tilde
        0x99 => 0x2122,     // Trade mark
        0x9A => 0x0161,     // Latin small s with caron
        0x9B => 0x203A,     // Single right-pointing angle quotation
        0x9C => 0x0153,     // Latin small ligature oe
        0x9E => 0x017E,     // Latin small z with caron
        0x9F => 0x0178,     // Latin capital Y with diaeresis
        _ => u32::from(cp), // Default: use the code point as-is
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn eu4_flavor_f64_rounding() {
        // This test was taken by running an observer game (plaintext)
        // with cloud auto save (binary) and comparing the two and
        // noticing that truncation instead of rounding would yield
        // `2.49859` instead of the expected `2.49860`
        let flavor = Eu4Flavor(Windows1252Encoding::new());
        let data: [u8; 8] = [210, 63, 1, 0, 0, 0, 0, 0];
        let actual = flavor.visit_f64(data);
        assert_eq!(actual, 2.49860);
    }

    #[test]
    fn eu4_escaped_text_drops_backslashes() {
        // A latin name in quotes with a localized suffix: `"Foo" 隊`
        let data = b"\\\"Foo\\\" \x10\x8A\x96";
        let flavor = Eu4Flavor::new();
        assert_eq!(flavor.decode(data), "\"Foo\" 隊");
    }

    #[test]
    fn eu4_escaped_text_trims_trailing_whitespace() {
        let flavor = Eu4Flavor::new();
        assert_eq!(flavor.decode(b"\x10\x8A\x96 \n"), "隊");

        // The trailing space is the high byte of the em dash escape
        assert_eq!(flavor.decode(b"a\x10\x14\x20"), "a\u{2014}");
    }
}
