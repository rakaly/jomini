use crate::{
    Encoding,
    util::{contains_zero_byte, le_u64, repeat_byte},
};
use std::borrow::Cow;

/// Remove the escapes from the content of a quoted text scalar.
///
/// `\\` becomes `\` and `\"` becomes `"`. A backslash before any other byte
/// is kept, as the games write paths such as `"gfx\models\ships\cog.mesh"`
/// with single backslashes, and EU4 saves before 1.31 write the tooltip
/// line break `\n` without escaping its backslash.
///
/// Binary strings have no escapes, so only text needs this. The text
/// deserializers and readers unescape quoted scalars before they decode them
/// with an [`Encoding`]. Use this function to do the same with the tokens of
/// a [`TokenReader`](crate::text::TokenReader).
///
/// ```
/// use jomini::text::unescape;
///
/// assert_eq!(unescape(br#"C:\\Users\\me"#).as_ref(), br"C:\Users\me");
/// assert_eq!(unescape(br#"say \"hi\""#).as_ref(), br#"say "hi""#);
/// assert_eq!(unescape(br"gfx\models\cog.mesh").as_ref(), br"gfx\models\cog.mesh");
/// assert_eq!(unescape(br"gfx\\interface\English.tga").as_ref(), br"gfx\interface\English.tga");
/// ```
pub fn unescape(data: &[u8]) -> Cow<'_, [u8]> {
    let Some(start) = find_escape(data) else {
        return Cow::Borrowed(data);
    };

    let mut out = Vec::with_capacity(data.len() - 1);
    out.extend_from_slice(&data[..start]);
    let mut rest = &data[start..];
    while let Some((&byte, tail)) = rest.split_first() {
        match (byte, tail.split_first()) {
            (b'\\', Some((&next @ (b'\\' | b'"'), after))) => {
                out.push(next);
                rest = after;
            }
            _ => {
                out.push(byte);
                rest = tail;
            }
        }
    }

    Cow::Owned(out)
}

/// Decode the content of a quoted text scalar: unescape it, then decode it
/// with the encoding.
#[inline]
pub(crate) fn decode_quoted<'a, E>(encoding: &E, data: &'a [u8]) -> Cow<'a, str>
where
    E: Encoding + ?Sized,
{
    match unescape(data) {
        Cow::Borrowed(data) => encoding.decode(data),
        Cow::Owned(data) => Cow::Owned(encoding.decode(&data).into_owned()),
    }
}

/// The index of the first `\\` or `\"` pair. A backslash before any other
/// byte does not start a pair, and the byte after it is not a backslash.
#[inline]
fn find_escape(data: &[u8]) -> Option<usize> {
    let mut at = 0;
    loop {
        at += find_backslash(&data[at..])?;
        match data.get(at + 1) {
            Some(b'\\' | b'"') => return Some(at),
            _ => at += 1,
        }
    }
}

#[inline]
fn find_backslash(data: &[u8]) -> Option<usize> {
    let (chunks, rest) = data.as_chunks::<8>();
    for (i, chunk) in chunks.iter().enumerate() {
        if contains_zero_byte(le_u64(chunk) ^ repeat_byte(b'\\')) {
            let offset = chunk.iter().position(|&b| b == b'\\')?;
            return Some(i * 8 + offset);
        }
    }

    let offset = rest.iter().position(|&b| b == b'\\')?;
    Some(chunks.len() * 8 + offset)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Utf8Encoding, Windows1252Encoding};
    use rstest::rstest;

    #[rstest]
    #[case(b"", b"")]
    #[case(b"plain text", b"plain text")]
    #[case(br#"C:\\Users\\me"#, br"C:\Users\me")]
    #[case(br"\\", br"\")]
    #[case(br#"\\\""#, br#"\""#)]
    #[case(br#"say \"hi\""#, br#"say "hi""#)]
    #[case(br"gfx\models\ships\cog.mesh", br"gfx\models\ships\cog.mesh")]
    #[case(
        br"gfx\\interface\English_flag.tga",
        br"gfx\interface\English_flag.tga"
    )]
    #[case(br"Traditions: +50%\n", br"Traditions: +50%\n")]
    #[case(br"Traditions: +50%\\n", br"Traditions: +50%\n")]
    #[case(br"trailing\", br"trailing\")]
    #[case(br"a\b\\c", br"a\b\c")]
    #[case(br"\\\\\\\\\\", br"\\\\\")]
    #[case(br"0123456789abcdef\\", br"0123456789abcdef\")]
    #[case(br"0123456\789abcdef\\", br"0123456\789abcdef\")]
    fn unescape_cases(#[case] input: &[u8], #[case] expected: &[u8]) {
        assert_eq!(unescape(input).as_ref(), expected);
    }

    #[test]
    fn unescape_borrows_without_escape_pairs() {
        assert!(matches!(
            unescape(br"gfx\models\cog.mesh"),
            Cow::Borrowed(_)
        ));
        assert!(matches!(unescape(b"0123456789abcdef"), Cow::Borrowed(_)));
    }

    #[test]
    fn decode_quoted_unescapes_then_decodes() {
        assert_eq!(
            decode_quoted(&Utf8Encoding, br#"C:\\Users\\me "#),
            r"C:\Users\me"
        );
        assert_eq!(
            decode_quoted(&Windows1252Encoding, b"Kr\xe4ftig \\\"Leo\\\""),
            "Kräftig \"Leo\""
        );
        assert_eq!(decode_quoted(&Utf8Encoding, br"gfx\models"), r"gfx\models");
    }
}
