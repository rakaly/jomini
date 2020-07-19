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

#[derive(Debug, PartialEq)]
enum ParseState {
    AtKey,
    AtKeyValueSeparator,
    AtObjectValue,
    AtArrayValue,
    AtParseOpen,
    AtFirstValue,
    AtEmptyObject,
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
        tape.parse(data)?;
        Ok(tape)
    }

    pub fn clear(&mut self) {
        self.token_tape.clear();
    }

    /// Skips whitespace that may terminate the file
    #[inline]
    fn skip_ws_t(&mut self, mut data: &'a [u8]) -> &'a [u8] {
        loop {
            let ind = data
                .iter()
                .position(|&x| !is_whitespace(x))
                .unwrap_or_else(|| data.len());
            let (_, rest) = data.split_at(ind);
            data = rest;

            if data.get(0).map_or(false, |x| *x == b'#') {
                if let Some(idx) = data.iter().position(|&x| x == b'\n') {
                    data = &data[idx..];
                } else {
                    return data;
                }
            } else {
                return data;
            }
        }
    }

    #[inline]
    fn parse_scalar(&mut self, d: &'a [u8]) -> Result<&'a [u8], TextError> {
        if d[0] == b'"' {
            let sd = &d[1..];
            let mut offset = 0;
            let mut chunk_iter = sd.chunks_exact(8);
            while let Some(n) = chunk_iter.next() {
                let acc = unsafe { (n.as_ptr() as *const u64).read_unaligned() };
                if contains_zero_byte(acc ^ repeat_byte(b'"')) {
                    let end_idx = n.iter().position(|&x| x == b'"').unwrap_or(0) + offset;
                    self.token_tape
                        .push(TextToken::Scalar(Scalar::new(&sd[..end_idx])));
                    return Ok(&d[end_idx + 2..]);
                }

                offset += 8;
            }

            let pos = chunk_iter
                .remainder()
                .iter()
                .position(|&x| x == b'"')
                .ok_or_else(|| TextError {
                    kind: TextErrorKind::Eof,
                })?;

            let end_idx = pos + offset;
            self.token_tape
                .push(TextToken::Scalar(Scalar::new(&sd[..end_idx])));
            Ok(&d[end_idx + 2..])
        } else {
            let mut offset = 0;
            let mut chunk_iter = d.chunks_exact(8);

            while let Some(n) = chunk_iter.next() {
                let acc = unsafe { (n.as_ptr() as *const u64).read_unaligned() };
                if contains_zero_byte(acc ^ repeat_byte(b' '))
                    || contains_zero_byte(acc ^ repeat_byte(b'\t'))
                    || contains_zero_byte(acc ^ repeat_byte(b'\n'))
                    || contains_zero_byte(acc ^ repeat_byte(b'\r'))
                    || contains_zero_byte(acc ^ repeat_byte(b'{'))
                    || contains_zero_byte(acc ^ repeat_byte(b'}'))
                    || contains_zero_byte(acc ^ repeat_byte(b'='))
                    || contains_zero_byte(acc ^ repeat_byte(b'#'))
                    || contains_zero_byte(acc ^ repeat_byte(b'!'))
                    || contains_zero_byte(acc ^ repeat_byte(b'>'))
                    || contains_zero_byte(acc ^ repeat_byte(b'<'))
                {
                    let mut ind = n.iter().position(|&x| is_boundary(x)).unwrap_or(0) + offset;
                    // To work with cases where we have "==bar" we ensure that found index is at least one
                    ind = std::cmp::max(ind, 1);
                    let (scalar, rest) = d.split_at(ind);
                    self.token_tape.push(TextToken::Scalar(Scalar::new(scalar)));
                    return Ok(rest);
                }

                offset += 8;
            }

            let mut ind = chunk_iter
                .remainder()
                .iter()
                .position(|&x| is_boundary(x))
                .map(|x| x + offset)
                .unwrap_or_else(|| d.len());

            // To work with cases where we have "==bar" we ensure that found index is at least one
            ind = std::cmp::max(ind, 1);
            let (scalar, rest) = d.split_at(ind);
            self.token_tape.push(TextToken::Scalar(Scalar::new(scalar)));
            Ok(rest)
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
    pub fn parse(&mut self, mut data: &'a [u8]) -> Result<(), TextError> {
        self.token_tape.reserve(data.len() / 5);
        let mut state = ParseState::AtKey;

        let mut parent_ind = 0;
        loop {
            data = self.skip_ws_t(data);
            if data.is_empty() {
                if parent_ind == 0 && state == ParseState::AtKey {
                    return Ok(());
                } else {
                    return Err(TextError {
                        kind: TextErrorKind::Eof,
                    });
                }
            }

            match state {
                ParseState::AtEmptyObject => {
                    if data[0] != b'}' {
                        return Err(TextError {
                            kind: TextErrorKind::Message(String::from("expected first non-whitespace character after an empty object starter to be a close group")),
                        });
                    }
                    data = &data[1..];
                    state = ParseState::AtKey;
                }
                ParseState::AtKey => {
                    match data[0] {
                        b'}' => {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(x)) => *x,
                                Some(TextToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(TextToken::Array(_x)) => ParseState::AtArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::AtKey,
                                _ => ParseState::AtKey,
                            };

                            let end_idx = self.token_tape.len();
                            if parent_ind == 0 && grand_ind == 0 {
                                return Err(TextError {
                                    kind: TextErrorKind::StackExhausted,
                                });
                            }

                            if let Some(parent) = self.token_tape.get_mut(parent_ind) {
                                *parent = TextToken::Object(end_idx);
                            }
                            self.token_tape.push(TextToken::End(parent_ind));
                            parent_ind = grand_ind;
                            data = &data[1..];
                        }

                        // Empty object! Skip
                        b'{' => {
                            data = &data[1..];
                            state = ParseState::AtEmptyObject;
                        }

                        _ => {
                            data = self.parse_scalar(data)?;
                            state = ParseState::AtKeyValueSeparator;
                        }
                    }
                }
                ParseState::AtKeyValueSeparator => {
                    data = self.parse_key_value_separator(data)?;
                    state = ParseState::AtObjectValue;
                }
                ParseState::AtObjectValue => {
                    match data[0] {
                        b'{' => {
                            self.token_tape.push(TextToken::Array(0));
                            state = ParseState::AtParseOpen;
                            data = &data[1..];
                        }

                        // Check to not parse too far into the object's array trailer
                        b'}' => {
                            state = ParseState::AtKey;
                        }

                        _ => {
                            data = self.parse_scalar(data)?;
                            state = ParseState::AtKey;
                        }
                    }
                }
                ParseState::AtParseOpen => {
                    let ind = self.token_tape.len() - 1;

                    match data[0] {
                        // Empty array
                        b'}' => {
                            state = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(_x)) => ParseState::AtArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::AtKey,
                                _ => ParseState::AtKey,
                            };

                            self.token_tape[ind] = TextToken::Array(ind + 1);
                            self.token_tape.push(TextToken::End(ind));
                            data = &data[1..];
                        }

                        // Array of objects
                        b'{' => {
                            self.token_tape[ind] = TextToken::Array(parent_ind);
                            parent_ind = ind;
                            state = ParseState::AtArrayValue;
                        }

                        _ => {
                            self.token_tape[ind] = TextToken::Object(parent_ind);
                            parent_ind = ind;
                            data = self.parse_scalar(data)?;
                            state = ParseState::AtFirstValue;
                        }
                    }
                }
                ParseState::AtFirstValue => match data[0] {
                    b'=' => {
                        data = &data[1..];
                        state = ParseState::AtObjectValue;
                    }
                    _ => {
                        state = ParseState::AtArrayValue;
                    }
                },
                ParseState::AtArrayValue => match data[0] {
                    b'{' => {
                        self.token_tape.push(TextToken::Array(0));
                        state = ParseState::AtParseOpen;
                        data = &data[1..];
                    }
                    b'}' => {
                        let grand_ind = match self.token_tape.get(parent_ind) {
                            Some(TextToken::Array(x)) => *x,
                            Some(TextToken::Object(x)) => *x,
                            _ => 0,
                        };

                        state = match self.token_tape.get(grand_ind) {
                            Some(TextToken::Array(_x)) => ParseState::AtArrayValue,
                            Some(TextToken::Object(_x)) => ParseState::AtKey,
                            _ => ParseState::AtKey,
                        };

                        let end_idx = self.token_tape.len();
                        self.token_tape[parent_ind] = TextToken::Array(end_idx);
                        self.token_tape.push(TextToken::End(parent_ind));
                        parent_ind = grand_ind;
                        data = &data[1..];
                    }
                    _ => {
                        data = self.parse_scalar(data)?;
                        state = ParseState::AtArrayValue;
                    }
                },
            }
        }
    }
}

/// From the memchr crate which bases its implementation on several others
#[inline(always)]
fn contains_zero_byte(x: u64) -> bool {
    const LO_U64: u64 = 0x0101010101010101;
    const HI_U64: u64 = 0x8080808080808080;
    x.wrapping_sub(LO_U64) & !x & HI_U64 != 0
}

#[inline(always)]
const fn repeat_byte(b: u8) -> u64 {
    (b as u64) * (u64::MAX / 255)
}

#[inline]
fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}

#[inline]
fn is_boundary(b: u8) -> bool {
    // Function is written in such a way to reduce the number of operators needed. This function is
    // essentially:
    // is_whitespace(b) || b == b'{' || b == b'}' || is_operator(b) || b == b'#'
    (b < 0x40 || (b == b'{' || b == b'}')) && !(b > 0x24 && b < 0x3c)
}

#[allow(dead_code)]
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

    #[test]
    fn test_regression() {
        let data = [0, 32, 34, 0];
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_regression2() {
        let data = [0, 4, 33, 0];
        let _ = parse(&data[..]);
    }

    #[test]
    fn test_too_heavily_nested() {
        let mut data = Vec::new();
        data.extend_from_slice(b"foo=");
        for _ in 0..100000 {
            data.extend_from_slice(b"{");
        }
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_no_ws_comment() {
        let data = b"foo=abc#def\nbar=qux";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"abc")),
                TextToken::Scalar(Scalar::new(b"bar")),
                TextToken::Scalar(Scalar::new(b"qux")),
            ]
        );
    }

    #[test]
    fn test_period_in_identifiers() {
        let data = b"flavor_tur.8=yes";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"flavor_tur.8")),
                TextToken::Scalar(Scalar::new(b"yes")),
            ]
        );
    }

    #[test]
    fn test_dashed_identifiers() {
        // From stellaris saves
        let data = b"dashed-identifier=yes";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"dashed-identifier")),
                TextToken::Scalar(Scalar::new(b"yes")),
            ]
        );
    }

    #[test]
    fn test_colon_values() {
        let data = b"province_id = event_target:agenda_province";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"province_id")),
                TextToken::Scalar(Scalar::new(b"event_target:agenda_province")),
            ]
        );
    }

    #[test]
    fn test_variables() {
        let data = b"@planet_standard_scale = 11";

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"@planet_standard_scale")),
                TextToken::Scalar(Scalar::new(b"11")),
            ]
        );
    }

    #[test]
    fn test_equal_identifier() {
        let data = br#"=="bar""#;

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"=")),
                TextToken::Scalar(Scalar::new(b"bar")),
            ]
        );
    }

    #[test]
    fn test_many_line_comment() {
        let mut data = Vec::new();
        data.extend_from_slice(b"foo=1.000\n");
        for _ in 0..100000 {
            data.extend_from_slice(b"# this is a comment\n");
        }
        data.extend_from_slice(b"foo=2.000\n");

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"1.000")),
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"2.000")),
            ]
        );
    }

    #[test]
    fn test_initial_end_does_not_panic() {
        let res = parse(&b"}"[..]);
        assert!(res.is_ok() || res.is_err());
    }
}
