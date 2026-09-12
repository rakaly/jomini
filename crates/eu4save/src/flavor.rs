use jomini::{binary::BinaryFlavor, Encoding, Windows1252Encoding};

/// The eu4 binary flavor
#[derive(Debug, Default)]
pub struct Eu4Flavor(Windows1252Encoding);

impl Eu4Flavor {
    /// Creates a new eu4 flavor
    pub fn new() -> Self {
        Eu4Flavor(Windows1252Encoding::new())
    }
}

impl Encoding for Eu4Flavor {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        // Heuristic to detect chinese escaped strings
        if matches!(data.first(), Some(0x10..=0x13)) {
            std::borrow::Cow::Owned(decode_eu4_escaped_text(data))
        } else {
            self.0.decode(data)
        }
    }
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

/// Converts the EU4 chinese encoding into a utf-8 string
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
            _ => cp1252_to_ucs2(cp),
        };

        wide_chars.push(code_point as u16);
    }

    String::from_utf16_lossy(&wide_chars)
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
}
