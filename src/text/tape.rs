use crate::{Scalar, TextError, TextErrorKind};

#[derive(Debug, PartialEq)]
pub enum TextToken<'a> {
    Array(usize),
    Object(usize),
    Scalar(Scalar<'a>),
    End(usize),
}

#[derive(Debug, Default)]
pub struct TextTape<'a> {
    pub token_tape: Vec<TextToken<'a>>,
}

/*
CHARACTER CLASSES:

- WHITESPACE: (' ', '\t', '\n', '\r' / 0x09, 0x20, 0x0a, 0x0d)
- OPERATOR: ('!', '>', '=', '<' / 0x3d)
- BRACKET: ('{', '}' / 0x7b, 0x7d)
- SCALAR

WHITESPACE:

0000 1001
0010 0000
0000 1010
0000 1101

OPERATOR:

0011 1101

BRACKET:

0111 1011
0111 1101

*/

impl<'a> TextTape<'a> {
    pub fn from_slice(data: &'a [u8]) -> Result<TextTape<'a>, TextError> {
        let mut tape = TextTape::default();
        tape.slurp_body(data)?;
        Ok(tape)
    }

    /// Skips whitespace, but we must not reach the end of the file
    #[inline]
    fn skip_ws(&mut self, d: &'a [u8]) -> Result<&'a [u8], TextError> {
        let ind = d
            .iter()
            .position(|&x| !is_whitespace(x))
            .ok_or_else(|| TextError {
                kind: TextErrorKind::Eof,
            })?;

        let ret = &d[ind..];
        if ret[0] == b'#' {
            let end_idx = 
                memchr::memchr(b'\n', &ret)
                    .ok_or_else(|| TextError {
                        kind: TextErrorKind::Eof,
                    })?;
            self.skip_ws(&ret[end_idx..])
        } else {
            Ok(ret)
        }
    }

    /// Skips whitespace that may terminate the file
    #[inline]
    fn skip_ws_t(&mut self, d: &'a [u8]) -> &'a [u8] {
        let ind = d
            .iter()
            .position(|&x| !is_whitespace(x))
            .unwrap_or_else(|| d.len());
        let (_, rest) = d.split_at(ind);
        if rest.get(0).map_or(false, |x| *x == b'#') {
            if let Some(idx) = memchr::memchr(b'\n', &rest) {
                self.skip_ws_t(&rest[idx..])
            } else {
                rest
            }
        } else {
            rest
        }
    }

    #[inline]
    fn parse_scalar(&mut self, d: &'a [u8]) -> &'a [u8] {
        if d[0] == b'"' {
            let sd = &d[1..];
            let end_idx = memchr::memchr(b'"', &sd).expect("EEK");
            self.token_tape
                .push(TextToken::Scalar(Scalar::new(&sd[..end_idx])));
            &d[end_idx + 2..]
        } else {
            let ind = d
                .iter()
                .position(|&x| is_boundary(x))
                .unwrap_or_else(|| d.len());
            let (scalar, rest) = d.split_at(ind);
            self.token_tape.push(TextToken::Scalar(Scalar::new(scalar)));
            rest
        }
    }

    #[inline]
    fn first_val(&mut self, mut d: &'a [u8], open_idx: usize) -> Result<&'a [u8], TextError> {
        d = self.parse_scalar(d);
        d = self.skip_ws(d)?;

        match d[0] {
            b'=' => self.parse_inner_object(&d[1..], open_idx),
            _ => self.parse_array(d, open_idx),
        }
    }

    #[inline]
    fn parse_array(&mut self, mut d: &'a [u8], open_idx: usize) -> Result<&'a [u8], TextError> {
        loop {
            d = self.skip_ws(d)?;

            if d[0] == b'}' {
                let end_idx = self.token_tape.len();
                self.token_tape[open_idx] = TextToken::Array(end_idx);
                self.token_tape.push(TextToken::End(open_idx));
                return Ok(&d[1..]);
            }

            d = self.parse_value(d)?;
        }
    }

    #[inline]
    fn parse_inner_object(
        &mut self,
        mut d: &'a [u8],
        open_idx: usize,
    ) -> Result<&'a [u8], TextError> {
        d = self.skip_ws(d)?;
        d = self.parse_value(d)?;

        loop {
            d = self.skip_ws(d)?;

            match d[0] {
                b'}' => {
                    let end_idx = self.token_tape.len();
                    self.token_tape[open_idx] = TextToken::Object(end_idx);
                    self.token_tape.push(TextToken::End(open_idx));
                    return Ok(&d[1..]);
                }

                // Empty object! Skip
                b'{' => {
                    d = self.skip_ws(&d[1..])?;
                    if d[0] != b'}' {
                        panic!("EEEK");
                    }

                    d = &d[1..];
                }
                _ => {
                    d = self.parse_scalar(d);
                    d = self.skip_ws(d)?;
                    d = self.parse_key_value_separator(d)?;
                    d = self.skip_ws(d)?;

                    // Check to not parse too far into the object's array trailer
                    if d[0] != b'}' {
                        d = self.parse_value(d)?;
                    }
                }
            }
        }
    }

    #[inline]
    fn parse_open(&mut self, mut d: &'a [u8], open_idx: usize) -> Result<&'a [u8], TextError> {
        d = self.skip_ws(d)?;
        match d[0] {
            // Empty array
            b'}' => {
                let end_idx = self.token_tape.len();
                self.token_tape[open_idx] = TextToken::Array(end_idx);
                self.token_tape.push(TextToken::End(open_idx));
                Ok(&d[1..])
            }

            // array of objects
            b'{' => self.parse_array(d, open_idx),
            _ => self.first_val(d, open_idx),
        }
    }

    #[inline]
    fn parse_value(&mut self, d: &'a [u8]) -> Result<&'a [u8], TextError> {
        match d[0] {
            b'{' => {
                let open_idx = self.token_tape.len();
                self.token_tape.push(TextToken::Array(0));
                self.parse_open(&d[1..], open_idx)
            }
            _ => Ok(self.parse_scalar(d)),
        }
    }

    #[inline]
    fn parse_key_value_separator(&mut self, d: &'a [u8]) -> Result<&'a [u8], TextError> {
        // Most key values are separated by an equal sign but there are some fields like
        // map_area_data that does not have a separator
        match d[0] {
            b'=' => Ok(&d[1..]),
            b'{' => Ok(d),
            _ => {
                // This should normally be an error, but there are some formats eg:
                // brittany_area = { color = { 10 10 10 } 100 200 300 }
                Ok(d)
            }
        }
    }

    #[inline]
    fn slurp_body(&mut self, mut d: &'a [u8]) -> Result<(), TextError> {
        while !d.is_empty() {
            d = self.skip_ws_t(d);
            if d.is_empty() {
                break;
            }

            d = self.parse_scalar(d);
            d = self.skip_ws(d)?;
            d = self.parse_key_value_separator(d)?;
            d = self.skip_ws(d)?;
            d = self.parse_value(d)?;
        }

        Ok(())
    }
}

#[inline]
fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}

#[inline]
fn is_boundary(b: u8) -> bool {
    is_whitespace(b) || b == b'{' || b == b'}' || is_operator(b)
}

#[inline]
fn is_operator(b: u8) -> bool {
    b == b'=' || b == b'!' || b == b'>' || b == b'<'
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'a>(data: &'a [u8]) -> Result<TextTape<'a>, TextError> {
        TextTape::from_slice(data)
    }

    #[test]
    fn test_simple_event() {
        let data = b"foo=bar";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"bar")),
            ]
        );
    }

    #[test]
    fn test_simple_event_with_spaces() {
        let data = b"  \t\t foo =bar \r\ndef=\tqux";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"def")),
                TextToken::Scalar(Scalar::new(b"qux")),
            ]
        );
    }

    #[test]
    fn test_scalars_with_quotes() {
        let data = br#""foo"="bar" "3"="1444.11.11""#;
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"3")),
                TextToken::Scalar(Scalar::new(b"1444.11.11")),
            ]
        );
    }

    #[test]
    fn test_numbers_are_scalars() {
        let data = b"foo=1.000";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"1.000")),
            ]
        );
    }

    #[test]
    fn test_object_event() {
        let data = b"foo={bar=qux}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"qux")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_object_multi_field_event() {
        let data = b"foo={bar=1 qux=28}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Object(6),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"1")),
                TextToken::Scalar(Scalar::new(b"qux")),
                TextToken::Scalar(Scalar::new(b"28")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_event() {
        let data = b"versions={\r\n\t\"1.28.3.0\"\r\n}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"versions")),
                TextToken::Array(3),
                TextToken::Scalar(Scalar::new(b"1.28.3.0")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_multievent() {
        let data = b"versions={\r\n\t\"1.28.3.0\"\r\n foo}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"versions")),
                TextToken::Array(4),
                TextToken::Scalar(Scalar::new(b"1.28.3.0")),
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_no_equal_object_event() {
        let data = b"foo{bar=qux}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"qux")),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_array() {
        let data = b"discovered_by={}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"discovered_by")),
                TextToken::Array(2),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_of_objects() {
        let data = b"stats={{id=0 type=general} {id=1 type=admiral}}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"stats")),
                TextToken::Array(14),
                TextToken::Object(7),
                TextToken::Scalar(Scalar::new(b"id")),
                TextToken::Scalar(Scalar::new(b"0")),
                TextToken::Scalar(Scalar::new(b"type")),
                TextToken::Scalar(Scalar::new(b"general")),
                TextToken::End(2),
                TextToken::Object(13),
                TextToken::Scalar(Scalar::new(b"id")),
                TextToken::Scalar(Scalar::new(b"1")),
                TextToken::Scalar(Scalar::new(b"type")),
                TextToken::Scalar(Scalar::new(b"admiral")),
                TextToken::End(8),
                TextToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects2() {
        let data = b"foo={bar=val {}} me=you";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Object(4),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"val")),
                TextToken::End(1),
                TextToken::Scalar(Scalar::new(b"me")),
                TextToken::Scalar(Scalar::new(b"you")),
            ]
        );
    }

    #[test]
    fn test_spanning_objects() {
        let data = b"army={name=abc} army={name=def}";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"army")),
                TextToken::Object(4),
                TextToken::Scalar(Scalar::new(b"name")),
                TextToken::Scalar(Scalar::new(b"abc")),
                TextToken::End(1),
                TextToken::Scalar(Scalar::new(b"army")),
                TextToken::Object(9),
                TextToken::Scalar(Scalar::new(b"name")),
                TextToken::Scalar(Scalar::new(b"def")),
                TextToken::End(6),
            ]
        );
    }

    #[test]
    fn test_mixed_object_array() {
        // This is something that probably won't have a deserialized test
        // as ... how should one interpret it?
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"brittany_area")),
                TextToken::Object(13),
                TextToken::Scalar(Scalar::new(b"color")),
                TextToken::Array(7),
                TextToken::Scalar(Scalar::new(b"118")),
                TextToken::Scalar(Scalar::new(b"99")),
                TextToken::Scalar(Scalar::new(b"151")),
                TextToken::End(3),
                TextToken::Scalar(Scalar::new(b"169")),
                TextToken::Scalar(Scalar::new(b"170")),
                TextToken::Scalar(Scalar::new(b"171")),
                TextToken::Scalar(Scalar::new(b"172")),
                TextToken::Scalar(Scalar::new(b"4384")),
                TextToken::End(1),
            ]
        );
    }
}
