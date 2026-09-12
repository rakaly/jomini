use crate::{Ck3Error, Ck3ErrorKind};
use jomini::{
    binary::{BinaryFlavor, Token},
    Encoding, Utf8Encoding,
};
use std::io::{Cursor, Read};

pub(crate) fn reencode_float(f: f64) -> f64 {
    // first reverse the flavor decoding to get raw val
    let f = f * 1000.0;

    // Then apply the eu4 decoding step (Q49.15) with 5 digits of precision.
    // For some unknown reason we need to incorporate float epsilon so that a
    // number decoded as 251.24999999999 should be decoded as 251.25000 and
    // using 32 bits epsilon was the only way I could get equivalent plaintext
    // and binary saves to agree on reencoded float values.
    let eps = f64::from(f32::EPSILON);
    let num = (f / 32768.0 * 100_000.0 + (eps * f.signum())).trunc();
    num / 100_000.0
}

pub(crate) trait Ck3BinaryFlavor: BinaryFlavor + jomini::Encoding {
    /// Designates this flavor as having floats that have more than two binary representations
    fn float_reencoding(&self) -> bool;

    /// Even if the following quoted strings are found, write them out unquoted
    fn unquote_token(&self, token: &str) -> bool;
}

impl<T: Ck3BinaryFlavor + ?Sized> Ck3BinaryFlavor for Box<T> {
    fn float_reencoding(&self) -> bool {
        (**self).float_reencoding()
    }

    fn unquote_token(&self, token: &str) -> bool {
        (**self).unquote_token(token)
    }
}

pub fn flavor_reader<R>(mut reader: R) -> Result<(impl Read, Box<dyn Ck3BinaryFlavor>), Ck3Error>
where
    R: Read,
{
    let mut buf = [0u8; 16];
    reader.read_exact(&mut buf)?;

    // metadata = { version = <version> }
    let mut lexer = jomini::binary::Lexer::new(&buf[10..]);
    let Ok(Some(Token::I32(version))) = lexer.next_token() else {
        return Err(Ck3Error::from(Ck3ErrorKind::InvalidHeader));
    };

    let flavor: Box<dyn Ck3BinaryFlavor> = if version > 5 {
        Box::new(Ck3Flavor15::new())
    } else {
        Box::new(Ck3Flavor10::new())
    };
    let chained = Cursor::new(buf).chain(reader);
    Ok((chained, flavor))
}

/// The ck3 binary flavor 1.5+
#[derive(Debug, Default)]
pub struct Ck3Flavor15(Utf8Encoding);

impl Ck3Flavor15 {
    /// Creates a new ck3 flavor
    pub fn new() -> Self {
        Ck3Flavor15(Utf8Encoding::new())
    }
}

impl Encoding for Ck3Flavor15 {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        self.0.decode(data)
    }
}

impl Ck3BinaryFlavor for Ck3Flavor15 {
    fn float_reencoding(&self) -> bool {
        false
    }

    fn unquote_token(&self, token: &str) -> bool {
        matches!(
            token,
            "save_game_version"
                | "portraits_version"
                | "meta_date"
                | "color1"
                | "color2"
                | "color3"
                | "color4"
                | "color5"
                | "traits_lookup"
                | "features"
                | "modifiers"
                | "traditions"
                | "name_list"
                | "localization_key"
        )
    }
}

impl BinaryFlavor for Ck3Flavor15 {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_bits(u32::from_le_bytes(data))
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        let x = i64::from_le_bytes(data) as f64;
        let eps = f64::from(f32::EPSILON);
        (x + (eps * x.signum())).trunc() / 100_000.0
    }
}

/// The ck3 binary flavor pre 1.5
#[derive(Debug, Default)]
pub struct Ck3Flavor10(Utf8Encoding);

impl Ck3Flavor10 {
    /// Creates a new ck3 flavor
    pub fn new() -> Self {
        Ck3Flavor10(Utf8Encoding::new())
    }
}

impl Encoding for Ck3Flavor10 {
    fn decode<'a>(&self, data: &'a [u8]) -> std::borrow::Cow<'a, str> {
        self.0.decode(data)
    }
}

impl BinaryFlavor for Ck3Flavor10 {
    fn visit_f32(&self, data: [u8; 4]) -> f32 {
        f32::from_bits(u32::from_le_bytes(data))
    }

    fn visit_f64(&self, data: [u8; 8]) -> f64 {
        i64::from_le_bytes(data) as f64 / 1000.0
    }
}

impl Ck3BinaryFlavor for Ck3Flavor10 {
    fn float_reencoding(&self) -> bool {
        true
    }

    fn unquote_token(&self, token: &str) -> bool {
        matches!(
            token,
            "save_game_version"
                | "portraits_version"
                | "meta_date"
                | "color1"
                | "color2"
                | "color3"
                | "color4"
                | "color5"
                | "traits_lookup"
                | "features"
                | "modifiers"
                | "traditions"
                | "name_list"
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reencode_accuracy() {
        let data: [u8; 8] = [0, 160, 125, 0, 0, 0, 0, 0];
        let flavor = Ck3Flavor10::default();
        let f = flavor.visit_f64(data);
        let newf = reencode_float(f);
        assert_eq!(newf, 251.25000);
    }

    #[test]
    fn reencode_accuracy_2() {
        let data: [u8; 8] = [6, 193, 0, 0, 0, 0, 0, 0];
        let flavor = Ck3Flavor10::default();
        let f = flavor.visit_f64(data);
        let newf = reencode_float(f);
        assert_eq!(newf, 1.50799);
    }

    #[test]
    fn reencode_accuracy_3() {
        let data: [u8; 8] = [0, 0, 81, 255, 255, 255, 255, 255];
        let flavor = Ck3Flavor10::default();
        let f = flavor.visit_f64(data);
        let newf = reencode_float(f);
        assert_eq!(newf, -350.0);
    }
}
