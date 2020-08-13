use crate::data::{BOUNDARY, CHARACTER_CLASS, WHITESPACE};
use crate::util::le_u64;
use crate::{Error, ErrorKind, Scalar};

/// Represents a valid text value
#[derive(Debug, PartialEq)]
pub enum TextToken<'a> {
    /// Index of the `TextToken::End` that signifies this array's termination
    Array(usize),

    /// Index of the `TextToken::End` that signifies this array's termination
    Object(usize),

    /// Extracted scalar value
    Scalar(Scalar<'a>),

    /// Index of the start of this object
    End(usize),
}

/// Houses the tape of tokens that is extracted from plaintext data
#[derive(Debug, Default)]
pub struct TextTape<'a> {
    pub token_tape: Vec<TextToken<'a>>,
    original_length: usize,
}

#[derive(Debug, PartialEq)]
enum ParseState {
    Key,
    KeyValueSeparator,
    ObjectValue,
    ArrayValue,
    ParseOpen,
    FirstValue,
    EmptyObject,
}

impl<'a> TextTape<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn from_slice(data: &'a [u8]) -> Result<TextTape<'a>, Error> {
        let mut tape = TextTape::new();
        tape.parse(data)?;
        Ok(tape)
    }

    fn offset(&self, data: &[u8]) -> usize {
        self.original_length - data.len()
    }

    /// Skips whitespace that may terminate the file
    #[inline]
    fn skip_ws_t(&mut self, mut data: &'a [u8]) -> &'a [u8] {
        loop {
            let start_ptr = data.as_ptr();
            let end_ptr = unsafe { start_ptr.add(data.len()) };

            let nind = unsafe { forward_search(start_ptr, end_ptr, |x| !is_whitespace(x)) };
            let ind = nind.unwrap_or_else(|| data.len());
            let (_, rest) = data.split_at(ind);
            data = rest;

            if data.get(0).map_or(false, |x| *x == b'#') {
                if let Some(idx) = data.iter().position(|&x| x == b'\n') {
                    data = &data[idx..];
                } else {
                    return &[];
                }
            } else {
                return data;
            }
        }
    }

    #[inline]
    fn parse_quote_scalar(&mut self, d: &'a [u8]) -> Result<&'a [u8], Error> {
        let sd = &d[1..];
        let mut offset = 0;
        let mut chunk_iter = sd.chunks_exact(8);
        while let Some(n) = chunk_iter.next() {
            let acc = le_u64(n);
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
            .ok_or_else(Error::eof)?;

        let end_idx = pos + offset;
        self.token_tape
            .push(TextToken::Scalar(Scalar::new(&sd[..end_idx])));
        Ok(&d[end_idx + 2..])
    }

    #[inline]
    fn parse_scalar(&mut self, d: &'a [u8]) -> &'a [u8] {
        let start_ptr = d.as_ptr();
        let end_ptr = unsafe { start_ptr.add(d.len()) };

        let nind = unsafe { forward_search(start_ptr, end_ptr, is_boundary) };
        let mut ind = nind.unwrap_or_else(|| d.len());

        // To work with cases where we have "==bar" we ensure that found index is at least one
        ind = std::cmp::max(ind, 1);
        let (scalar, rest) = d.split_at(ind);
        self.token_tape.push(TextToken::Scalar(Scalar::new(scalar)));
        rest
    }

    #[inline]
    fn parse_key_value_separator(&mut self, d: &'a [u8]) -> &'a [u8] {
        // Most key values are separated by an equal sign but there are some fields like
        // map_area_data that does not have a separator.
        //
        // ```
        // map_area_data{
        //   brittany_area={
        //   # ...
        // ```
        //
        // Additionally it's possible for there to be heterogenus objects:
        //
        // ```
        // brittany_area = { color = { 10 10 10 } 100 200 300 }
        // ```
        //
        // These are especially tricky, but essentially this function's job is to skip the equal
        // token (the 99.9% typical case) if possible.
        if d[0] == b'=' {
            &d[1..]
        } else {
            d
        }
    }

    #[inline]
    pub fn parse(&mut self, mut data: &'a [u8]) -> Result<(), Error> {
        self.original_length = data.len();
        self.token_tape.clear();
        self.token_tape.reserve(data.len() / 5);
        let mut state = ParseState::Key;

        let mut parent_ind = 0;
        loop {
            data = self.skip_ws_t(data);
            if data.is_empty() {
                if parent_ind == 0 && state == ParseState::Key {
                    return Ok(());
                } else {
                    return Err(Error::eof());
                }
            }

            match state {
                ParseState::EmptyObject => {
                    if data[0] != b'}' {
                        return Err(Error::new(ErrorKind::InvalidEmptyObject {
                            offset: self.offset(data),
                        }));
                    }
                    data = &data[1..];
                    state = ParseState::Key;
                }
                ParseState::Key => {
                    match data[0] {
                        b'}' => {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(x)) => *x,
                                Some(TextToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            let end_idx = self.token_tape.len();
                            if parent_ind == 0 && grand_ind == 0 {
                                return Err(Error::new(ErrorKind::StackEmpty {
                                    offset: self.offset(data),
                                }));
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
                            state = ParseState::EmptyObject;
                        }

                        b'"' => {
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::KeyValueSeparator;
                        }
                        _ => {
                            data = self.parse_scalar(data);
                            state = ParseState::KeyValueSeparator;
                        }
                    }
                }
                ParseState::KeyValueSeparator => {
                    data = self.parse_key_value_separator(data);
                    state = ParseState::ObjectValue;
                }
                ParseState::ObjectValue => {
                    match data[0] {
                        b'{' => {
                            self.token_tape.push(TextToken::Array(0));
                            state = ParseState::ParseOpen;
                            data = &data[1..];
                        }

                        // Check to not parse too far into the object's array trailer
                        b'}' => {
                            state = ParseState::Key;
                        }

                        b'"' => {
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::Key;
                        }
                        _ => {
                            data = self.parse_scalar(data);
                            state = ParseState::Key;
                        }
                    }
                }
                ParseState::ParseOpen => {
                    let ind = self.token_tape.len() - 1;

                    match data[0] {
                        // Empty array
                        b'}' => {
                            state = match self.token_tape.get(parent_ind) {
                                Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                                Some(TextToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            self.token_tape[ind] = TextToken::Array(ind + 1);
                            self.token_tape.push(TextToken::End(ind));
                            data = &data[1..];
                        }

                        // Array of objects
                        b'{' => {
                            self.token_tape[ind] = TextToken::Array(parent_ind);
                            parent_ind = ind;
                            state = ParseState::ArrayValue;
                        }
                        b'"' => {
                            self.token_tape[ind] = TextToken::Object(parent_ind);
                            parent_ind = ind;
                            data = self.parse_quote_scalar(data)?;
                            state = ParseState::FirstValue;
                        }
                        _ => {
                            self.token_tape[ind] = TextToken::Object(parent_ind);
                            parent_ind = ind;
                            data = self.parse_scalar(data);
                            state = ParseState::FirstValue;
                        }
                    }
                }
                ParseState::FirstValue => match data[0] {
                    b'=' => {
                        data = &data[1..];
                        state = ParseState::ObjectValue;
                    }
                    _ => {
                        state = ParseState::ArrayValue;
                    }
                },
                ParseState::ArrayValue => match data[0] {
                    b'{' => {
                        self.token_tape.push(TextToken::Array(0));
                        state = ParseState::ParseOpen;
                        data = &data[1..];
                    }
                    b'}' => {
                        let grand_ind = match self.token_tape.get(parent_ind) {
                            Some(TextToken::Array(x)) => *x,
                            Some(TextToken::Object(x)) => *x,
                            _ => 0,
                        };

                        state = match self.token_tape.get(grand_ind) {
                            Some(TextToken::Array(_x)) => ParseState::ArrayValue,
                            Some(TextToken::Object(_x)) => ParseState::Key,
                            _ => ParseState::Key,
                        };

                        let end_idx = self.token_tape.len();
                        self.token_tape[parent_ind] = TextToken::Array(end_idx);
                        self.token_tape.push(TextToken::End(parent_ind));
                        parent_ind = grand_ind;
                        data = &data[1..];
                    }
                    b'"' => {
                        data = self.parse_quote_scalar(data)?;
                        state = ParseState::ArrayValue;
                    }
                    _ => {
                        data = self.parse_scalar(data);
                        state = ParseState::ArrayValue;
                    }
                },
            }
        }
    }
}

fn sub(a: *const u8, b: *const u8) -> usize {
    debug_assert!(a >= b);
    (a as usize) - (b as usize)
}

#[inline(always)]
unsafe fn forward_search<F: Fn(u8) -> bool>(
    start_ptr: *const u8,
    end_ptr: *const u8,
    confirm: F,
) -> Option<usize> {
    let mut ptr = start_ptr;
    while ptr < end_ptr {
        if confirm(*ptr) {
            return Some(sub(ptr, start_ptr));
        }
        ptr = ptr.offset(1);
    }

    None
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
    CHARACTER_CLASS[usize::from(b)] & WHITESPACE == WHITESPACE
}

#[inline]
fn is_boundary(b: u8) -> bool {
    CHARACTER_CLASS[usize::from(b)] & BOUNDARY == BOUNDARY
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'a>(data: &'a [u8]) -> Result<TextTape<'a>, Error> {
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
    fn test_error_offset() {
        let data = b"foo={}} a=c";
        let mut tape = TextTape::new();
        let err = tape.parse(&data[..]).unwrap_err();
        match err.kind() {
            ErrorKind::StackEmpty { offset, .. } => {
                assert_eq!(*offset, 6);
            }
            _ => assert!(false)
        }
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
    fn test_terminating_comment() {
        let data = b"# boo\r\n# baa\r\nfoo=a\r\n# bee";
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                TextToken::Scalar(Scalar::new(b"foo")),
                TextToken::Scalar(Scalar::new(b"a")),
            ]
        );
    }

    #[test]
    fn test_initial_end_does_not_panic() {
        let res = parse(&b"}"[..]);
        assert!(res.is_ok() || res.is_err());
    }
}
