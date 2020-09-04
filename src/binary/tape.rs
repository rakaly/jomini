use crate::util::{le_i32, le_u16, le_u32, le_u64};
use crate::{Error, ErrorKind, Rgb, Scalar, BinaryFlavor, DefaultFlavor};

/// Represents any valid binary value
#[derive(Debug, PartialEq)]
pub enum BinaryToken<'a> {
    /// Index of the `BinaryToken::End` that signifies this array's termination
    Array(usize),

    /// Index of the `BinaryToken::End` that signifies this object's termination
    Object(usize),

    /// Index of the start of this object
    End(usize),

    /// Represents a binary boolean.
    Bool(bool),

    /// Represents a binary unsigned 32bit integer
    U32(u32),

    /// Represents a binary unsigned 64bit integer
    U64(u64),

    /// Represents a binary signed 32bit integer
    I32(i32),

    /// Represents a binary encoded string
    Text(Scalar<'a>),

    /// Represents a binary 32bit floating point number
    F32(f32),

    /// Represents a binary Q16.16 floating point number
    Q16(f32),

    /// Represents a 16bit token key that can be resolved to an equivalent textual representation.
    Token(u16),

    /// Represents a binary encoded rgb value
    Rgb(Box<Rgb>),
}

const END: u16 = 0x0004;
const OPEN: u16 = 0x0003;
const EQUAL: u16 = 0x0001;
const U32: u16 = 0x0014;
const U64: u16 = 0x029c;
const I32: u16 = 0x000c;
const BOOL: u16 = 0x000e;
const STRING_1: u16 = 0x000f;
const STRING_2: u16 = 0x0017;
const F32: u16 = 0x000d;
const Q16: u16 = 0x0167;
const RGB: u16 = 0x0243;

#[derive(Debug)]
pub struct BinaryTapeParser<F> {
    flavor: F,
}

impl<F> BinaryTapeParser<F> where F: BinaryFlavor {
    pub fn with_flavor(flavor: F) -> Self {
        BinaryTapeParser {
            flavor
        }
    }

    pub fn parse_slice<'a>(self, data: &'a [u8]) -> Result<BinaryTape<'a>, Error> {
        let mut state = ParserState {
            data,
            flavor: self.flavor,
            original_length: data.len(),
            token_tape: Vec::with_capacity(data.len() / 100 * 15),
        };

        state.parse()?;
        Ok(BinaryTape {
            token_tape: state.token_tape,
        })
    }

    pub fn parse_slice_with_tape<'a>(self, data: &'a [u8], tape: BinaryTape<'a>) -> Result<BinaryTape<'a>, Error> {
        let mut token_tape = tape.token_tape;
        token_tape.clear();
        token_tape.reserve(data.len() / 100 * 15);
        let mut state = ParserState {
            data,
            flavor: self.flavor,
            original_length: data.len(),
            token_tape,
        };

        state.parse()?;
        Ok(BinaryTape {
            token_tape: state.token_tape,
        })
    }
}

struct ParserState<'a, F> {
    data: &'a [u8],
    flavor: F,
    original_length: usize,
    token_tape: Vec<BinaryToken<'a>>,
}

impl<'a, F> ParserState<'a, F> where F: BinaryFlavor {
    fn offset(&self, data: &[u8]) -> usize {
        self.original_length - data.len()
    }

    #[inline]
    fn parse_next_id(&mut self, data: &'a [u8]) -> Result<(&'a [u8], u16), Error> {
        let val = data.get(..2).map(le_u16).ok_or_else(Error::eof)?;
        Ok((&data[2..], val))
    }

    #[inline]
    fn parse_u32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data.get(..4).map(le_u32).ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::U32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_u64(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data.get(..8).map(le_u64).ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::U64(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_i32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data.get(..4).map(le_i32).ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::I32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_f32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data
            .get(..4)
            .map(|x| self.flavor.visit_f32(x))
            .ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::F32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_q16(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data
            .get(..8)
            .map(le_i32)
            .map(|x| {
                let mut val = x as f32;
                val = val * 2.0 / 65536.0 * 100_000.0;
                val.floor() / 100_000.0
            })
            .ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::Q16(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_bool(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data.get(0).map(|&x| x != 0).ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::Bool(val));
        Ok(&data[1..])
    }

    #[inline]
    fn parse_rgb(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let val = data
            .get(..22) // u16 `{` + (u16 + u32) * 3 + u16 `}`
            .map(|x| Rgb {
                r: le_u32(&x[4..]),
                g: le_u32(&x[10..]),
                b: le_u32(&x[16..]),
            })
            .ok_or_else(Error::eof)?;
        self.token_tape.push(BinaryToken::Rgb(Box::new(val)));
        Ok(&data[22..])
    }

    #[inline]
    fn parse_string(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        if data.len() >= 2 {
            let (text_len_data, rest) = data.split_at(2);
            let text_len = usize::from(le_u16(text_len_data));
            if rest.len() >= text_len {
                let (text, rest) = rest.split_at(text_len);
                let scalar = Scalar::new(text);
                self.token_tape.push(BinaryToken::Text(scalar));
                return Ok(rest);
            }
        }

        Err(Error::eof())
    }

    /// Clear previously parsed data and parse the given data
    pub fn parse(&mut self) -> Result<(), Error> {
        let mut data = self.data;
        let mut state = ParseState::Key;

        // This variable keeps track of outer array when we're parsing a hidden object.
        // A hidden object textually looks like:
        //     levels={ 10 0=2 1=2 }
        // which we will translate into
        //     levels={ 10 { 0=2 1=2 } }
        // with the help of this variable. As when we'll only see one END token to signify
        // both the end of the array and object, but we'll produce two BinaryToken::End.
        let mut array_ind_of_hidden_obj = None;

        let mut parent_ind = 0;
        while !data.is_empty() {
            match state {
                ParseState::Key => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }
                        Q16 => {
                            let res = self.parse_q16(data)?;
                            state = ParseState::KeyValueSeparator;
                            res
                        }

                        // Skip empty object
                        OPEN => {
                            let (d, token_id) = self.parse_next_id(d)?;
                            if token_id != END {
                                return Err(Error::new(ErrorKind::InvalidEmptyObject {
                                    offset: self.offset(data) - 2,
                                }));
                            }
                            d
                        }
                        END => {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(BinaryToken::Array(x)) => *x,
                                Some(BinaryToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(BinaryToken::Array(_x)) => ParseState::ArrayValue,
                                Some(BinaryToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            let end_idx = self.token_tape.len();
                            if parent_ind == 0 && grand_ind == 0 {
                                return Err(Error::new(ErrorKind::StackEmpty {
                                    offset: self.offset(data),
                                }));
                            }

                            if let Some(parent) = self.token_tape.get_mut(parent_ind) {
                                *parent = BinaryToken::Object(end_idx);
                            }

                            self.token_tape.push(BinaryToken::End(parent_ind));

                            if let Some(array_ind) = array_ind_of_hidden_obj.take() {
                                let end_idx = self.token_tape.len();
                                self.token_tape.push(BinaryToken::End(array_ind));

                                // Grab the grand parent from the outer array. Even though the logic should
                                // be more strict (ie: throwing an error when if the parent array index doesn't exist,
                                // or if the parent doesn't exist), but since hidden objects are such a rather rare
                                // occurrence, it's better to be flexible
                                let grand_ind =
                                    if let Some(parent) = self.token_tape.get_mut(array_ind) {
                                        let grand_ind = match parent {
                                            BinaryToken::Array(x) => *x,
                                            _ => 0,
                                        };
                                        *parent = BinaryToken::Array(end_idx);
                                        grand_ind
                                    } else {
                                        0
                                    };

                                state = match self.token_tape.get(grand_ind) {
                                    Some(BinaryToken::Array(_x)) => ParseState::ArrayValue,
                                    Some(BinaryToken::Object(_x)) => ParseState::Key,
                                    _ => ParseState::Key,
                                };

                                parent_ind = grand_ind;
                            } else {
                                parent_ind = grand_ind;
                            }

                            data
                        }
                        RGB => {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("RGB not valid for a key"),
                                offset: self.offset(data) - 2,
                            }));
                        }
                        EQUAL => {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("EQUAL not valid for a key"),
                                offset: self.offset(data) - 2,
                            }));
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::KeyValueSeparator;
                            &data
                        }
                    }
                }
                ParseState::KeyValueSeparator => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = match token_id {
                        EQUAL => Ok(&d),
                        OPEN => Ok(&data),
                        _ => Err(Error::new(ErrorKind::InvalidSyntax {
                            msg: String::from("expected an equal to separate key values"),
                            offset: self.offset(data),
                        })),
                    }?;
                    state = ParseState::ObjectValue;
                }
                ParseState::ObjectValue => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::Key;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::Key;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::Key;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::Key;
                            res
                        }
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::Key;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::Key;
                            res
                        }
                        Q16 => {
                            let res = self.parse_q16(data)?;
                            state = ParseState::Key;
                            res
                        }
                        OPEN => {
                            let ind = self.token_tape.len();
                            self.token_tape.push(BinaryToken::Array(0));

                            let (d, token_id) = self.parse_next_id(data)?;
                            let old_data = data;
                            data = d;

                            match token_id {
                                // Empty array
                                END => {
                                    state = match self.token_tape.get(parent_ind) {
                                        Some(BinaryToken::Array(_x)) => ParseState::ArrayValue,
                                        Some(BinaryToken::Object(_x)) => ParseState::Key,
                                        _ => ParseState::Key,
                                    };

                                    self.token_tape[ind] = BinaryToken::Array(ind + 1);
                                    self.token_tape.push(BinaryToken::End(ind));
                                    continue;
                                }

                                // array of objects or another array
                                OPEN => {
                                    self.token_tape[ind] = BinaryToken::Array(parent_ind);
                                    parent_ind = ind;

                                    state = ParseState::ArrayValue;

                                    // Rewind the data so that we can parse the nested open
                                    data = old_data;
                                    continue;
                                }

                                U32 => {
                                    data = self.parse_u32(data)?;
                                }
                                U64 => {
                                    data = self.parse_u64(data)?;
                                }
                                I32 => {
                                    data = self.parse_i32(data)?;
                                }
                                BOOL => {
                                    data = self.parse_bool(data)?;
                                }
                                STRING_1 | STRING_2 => {
                                    data = self.parse_string(data)?;
                                }
                                F32 => {
                                    data = self.parse_f32(data)?;
                                }
                                Q16 => {
                                    data = self.parse_q16(data)?;
                                }
                                RGB => {
                                    data = self.parse_rgb(data)?;
                                }
                                x => {
                                    self.token_tape.push(BinaryToken::Token(x));
                                }
                            }

                            let (d, token_id) = self.parse_next_id(data)?;
                            match token_id {
                                OPEN => {
                                    self.token_tape[ind] = BinaryToken::Object(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ObjectValue;
                                    data
                                }
                                EQUAL => {
                                    self.token_tape[ind] = BinaryToken::Object(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ObjectValue;
                                    d
                                }
                                _ => {
                                    self.token_tape[ind] = BinaryToken::Array(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ArrayValue;
                                    data
                                }
                            }
                        }
                        END => {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("END not valid for an object value"),
                                offset: self.offset(data) - 2,
                            }));
                        }
                        RGB => {
                            let res = self.parse_rgb(data)?;
                            state = ParseState::Key;
                            res
                        }
                        EQUAL => {
                            return Err(Error::new(ErrorKind::InvalidSyntax {
                                msg: String::from("EQUAL not valid for an object value"),
                                offset: self.offset(data) - 2,
                            }));
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::Key;
                            &data
                        }
                    }
                }
                ParseState::ArrayValue => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        Q16 => {
                            let res = self.parse_q16(data)?;
                            state = ParseState::ArrayValue;
                            res
                        }
                        OPEN => {
                            let ind = self.token_tape.len();
                            self.token_tape.push(BinaryToken::Array(0));
                            let (d, token_id) = self.parse_next_id(data)?;
                            data = d;

                            match token_id {
                                // Empty array
                                END => {
                                    state = match self.token_tape.get(parent_ind) {
                                        Some(BinaryToken::Array(_x)) => ParseState::ArrayValue,
                                        Some(BinaryToken::Object(_x)) => ParseState::Key,
                                        _ => ParseState::Key,
                                    };

                                    self.token_tape[ind] = BinaryToken::Array(ind + 1);
                                    self.token_tape.push(BinaryToken::End(ind));
                                    continue;
                                }

                                // array of objects or another array
                                OPEN => {
                                    self.token_tape.push(BinaryToken::Array(0));
                                    state = ParseState::ArrayValue;
                                    continue;
                                }

                                U32 => {
                                    data = self.parse_u32(data)?;
                                }
                                U64 => {
                                    data = self.parse_u64(data)?;
                                }
                                I32 => {
                                    data = self.parse_i32(data)?;
                                }
                                BOOL => {
                                    data = self.parse_bool(data)?;
                                }
                                STRING_1 | STRING_2 => {
                                    data = self.parse_string(data)?;
                                }
                                F32 => {
                                    data = self.parse_f32(data)?;
                                }
                                Q16 => {
                                    data = self.parse_q16(data)?;
                                }
                                RGB => {
                                    data = self.parse_rgb(data)?;
                                }
                                x => {
                                    self.token_tape.push(BinaryToken::Token(x));
                                }
                            }

                            let (d, token_id) = self.parse_next_id(data)?;
                            match token_id {
                                OPEN => {
                                    self.token_tape[ind] = BinaryToken::Object(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ObjectValue;
                                    data
                                }
                                EQUAL => {
                                    self.token_tape[ind] = BinaryToken::Object(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ObjectValue;
                                    d
                                }
                                _ => {
                                    self.token_tape[ind] = BinaryToken::Array(parent_ind);
                                    parent_ind = ind;
                                    state = ParseState::ArrayValue;
                                    data
                                }
                            }
                        }
                        END => {
                            let grand_ind = match self.token_tape.get(parent_ind) {
                                Some(BinaryToken::Array(x)) => *x,
                                Some(BinaryToken::Object(x)) => *x,
                                _ => 0,
                            };

                            state = match self.token_tape.get(grand_ind) {
                                Some(BinaryToken::Array(_x)) => ParseState::ArrayValue,
                                Some(BinaryToken::Object(_x)) => ParseState::Key,
                                _ => ParseState::Key,
                            };

                            let end_idx = self.token_tape.len();
                            self.token_tape[parent_ind] = BinaryToken::Array(end_idx);
                            self.token_tape.push(BinaryToken::End(parent_ind));
                            parent_ind = grand_ind;
                            data
                        }
                        RGB => self.parse_rgb(data)?,
                        EQUAL => {
                            // CK3 introduced hidden object inside lists so we work around it by trying to
                            // make the object explicit
                            let hidden_object = BinaryToken::Object(parent_ind);
                            array_ind_of_hidden_obj = Some(parent_ind);
                            parent_ind = self.token_tape.len() - 1;
                            self.token_tape
                                .insert(self.token_tape.len() - 1, hidden_object);
                            state = ParseState::ObjectValue;
                            data
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::ArrayValue;
                            &data
                        }
                    }
                }
            }
        }

        if parent_ind == 0 && state == ParseState::Key {
            Ok(())
        } else {
            Err(Error::eof())
        }
    }
}

/// Houses the tape of tokens that is extracted from binary data
#[derive(Debug, Default)]
pub struct BinaryTape<'a> {
    token_tape: Vec<BinaryToken<'a>>,
}

#[derive(Debug, PartialEq)]
enum ParseState {
    Key,
    KeyValueSeparator,
    ObjectValue,
    ArrayValue,
}

impl<'a> BinaryTape<'a> {
    /// Convenience method for creating a binary tape and parsing the given input
    pub fn from_slice(data: &[u8]) -> Result<BinaryTape<'_>, Error> {
        BinaryTapeParser::with_flavor(DefaultFlavor).parse_slice(data)
    }

    pub fn parser() -> BinaryTapeParser<DefaultFlavor> {
        BinaryTape::parser_flavor(DefaultFlavor)
    }

    pub fn parser_flavor<F>(flavor: F) -> BinaryTapeParser<F> where F: BinaryFlavor {
        BinaryTapeParser::with_flavor(flavor)
    }

    /// Return the parsed tokens
    pub fn tokens(&self) -> &[BinaryToken<'a>] {
        self.token_tape.as_slice()
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'a>(data: &'a [u8]) -> Result<BinaryTape<'a>, Error> {
        BinaryTape::from_slice(data)
    }

    #[test]
    fn test_size_of_binary_token() {
        assert_eq!(std::mem::size_of::<BinaryToken>(), 24);
    }

    #[test]
    fn test_parse_offset() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28, 0x01, 0x00, 0x4c, 0x28];
        let err = BinaryTape::from_slice(&data[..]).unwrap_err();
        match err.kind() {
            ErrorKind::InvalidSyntax { offset, .. } => {
                assert_eq!(*offset, 6);
            }
            _ => assert!(false),
        }

        let data2 = [0x82, 0x2d, 0x01, 0x00, 0x01, 0x00];
        let err = BinaryTape::from_slice(&data2[..]).unwrap_err();
        match err.kind() {
            ErrorKind::InvalidSyntax { offset, .. } => {
                assert_eq!(*offset, 4);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_false_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x2d82), BinaryToken::Token(0x284c)]
        );
    }

    #[test]
    fn test_i32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x59, 0x00, 0x00, 0x00];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x2d82), BinaryToken::I32(89),]
        );
    }

    #[test]
    fn test_u32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x2d82), BinaryToken::U32(89),]
        );
    }

    #[test]
    fn test_f32_event() {
        let base_data = vec![0x82, 0x2d, 0x01, 0x00, 0x0d, 0x00];
        let f32_data = [
            [0x17, 0x00, 0x00, 0x00],
            [0x29, 0x00, 0x00, 0x00],
            [0x12, 0x00, 0x00, 0x00],
            [0x1e, 0x02, 0x00, 0x00],
            [0xe8, 0x03, 0x00, 0x00],
            [0xc0, 0xc6, 0x2d, 0x00],
        ];

        let f32_results = [0.023, 0.041, 0.018, 0.542, 1.000, 3000.000];

        for (bin, result) in f32_data.iter().zip(f32_results.iter()) {
            let full_data = [base_data.clone(), bin.to_vec()].concat();

            assert_eq!(
                parse(&full_data[..]).unwrap().token_tape,
                vec![BinaryToken::Token(0x2d82), BinaryToken::F32(*result),]
            );
        }
    }

    #[test]
    fn test_custom_f32_event() {
        struct Ck3Flavor;
        impl BinaryFlavor for Ck3Flavor {
            fn visit_f32(&self, data: &[u8]) -> f32 {
                f32::from_le_bytes([data[0], data[1], data[2], data[3]])
            }
        }

        let base_data = vec![0x82, 0x2d, 0x01, 0x00, 0x0d, 0x00];
        let f32_data = [
            [0x8f, 0xc2, 0x75, 0x3e],
        ];

        let f32_results = [0.24];

        for (bin, result) in f32_data.iter().zip(f32_results.iter()) {
            let full_data = [base_data.clone(), bin.to_vec()].concat();

            assert_eq!(
                BinaryTapeParser::with_flavor(Ck3Flavor).parse_slice(&full_data[..]).unwrap().token_tape,
                vec![BinaryToken::Token(0x2d82), BinaryToken::F32(*result),]
            );
        }
    }

    #[test]
    fn test_q16_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x67, 0x01, 0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x2d82), BinaryToken::Q16(1.78732),]
        );
    }

    #[test]
    fn test_string1_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2d82),
                BinaryToken::Text(Scalar::new(b"ENG")),
            ]
        );
    }

    #[test]
    fn test_string2_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2d82),
                BinaryToken::Text(Scalar::new(b"ENG")),
            ]
        );
    }

    #[test]
    fn test_multiple_top_level_events() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x2d82),
                BinaryToken::Token(0x284b),
                BinaryToken::Token(0x284d),
                BinaryToken::Token(0x284c),
            ]
        );
    }

    #[test]
    fn test_string_array() {
        let data = [
            0xe1, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x0a, 0x00, 0x41, 0x72, 0x74, 0x20,
            0x6f, 0x66, 0x20, 0x57, 0x61, 0x72, 0x0f, 0x00, 0x14, 0x00, 0x43, 0x6f, 0x6e, 0x71,
            0x75, 0x65, 0x73, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x50, 0x61, 0x72, 0x61, 0x64, 0x69,
            0x73, 0x65, 0x0f, 0x00, 0x0b, 0x00, 0x52, 0x65, 0x73, 0x20, 0x50, 0x75, 0x62, 0x6c,
            0x69, 0x63, 0x61, 0x0f, 0x00, 0x11, 0x00, 0x57, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x20,
            0x6f, 0x66, 0x20, 0x4e, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2ee1),
                BinaryToken::Array(6),
                BinaryToken::Text(Scalar::new(b"Art of War")),
                BinaryToken::Text(Scalar::new(b"Conquest of Paradise")),
                BinaryToken::Text(Scalar::new(b"Res Publica")),
                BinaryToken::Text(Scalar::new(b"Wealth of Nations")),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00, 0x00, 0x00, 0xc7, 0x2e,
            0x01, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x00, 0x00, 0xc8, 0x2e, 0x01, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x2ec9),
                BinaryToken::Object(10),
                BinaryToken::Token(0x28e2),
                BinaryToken::I32(1),
                BinaryToken::Token(0x28e3),
                BinaryToken::I32(11),
                BinaryToken::Token(0x2ec7),
                BinaryToken::I32(4),
                BinaryToken::Token(0x2ec8),
                BinaryToken::I32(0),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_numerical_identifiers() {
        let data = [
            0x0c, 0x00, 0x59, 0x00, 0x00, 0x00, 0x01, 0x00, 0x0c, 0x00, 0x1e, 0x00, 0x00, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::I32(89), BinaryToken::I32(30),]
        );
    }

    #[test]
    fn test_token_value() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28];
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x00e1), BinaryToken::Token(0x28be),]
        );
    }

    #[test]
    fn test_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x0b, 0x00]);
        data.extend_from_slice(b"1444.11.11\n");
        data.extend_from_slice(&END.to_le_bytes());
        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x29cc),
                BinaryToken::Object(4),
                BinaryToken::Text(Scalar::new(b"schools_initiated")),
                BinaryToken::Text(Scalar::new(b"1444.11.11\n")),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_no_equal_object() {
        let data = [
            0xf1, 0x36, 0x03, 0x00, 0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28, 0x04, 0x00,
        ];
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x36f1),
                BinaryToken::Object(4),
                BinaryToken::Token(0x00e1),
                BinaryToken::Token(0x28be),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_array() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00];
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x00e1),
                BinaryToken::Array(2),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_array_of_objects() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28, 0x04, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x2863),
                BinaryToken::Array(10),
                BinaryToken::Object(5),
                BinaryToken::Token(0x36f1),
                BinaryToken::Token(0x284b),
                BinaryToken::End(2),
                BinaryToken::Object(9),
                BinaryToken::Token(0x36f1),
                BinaryToken::Token(0x284b),
                BinaryToken::End(6),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects_to_skip() {
        let data = [
            0x38, 0x28, 0x01, 0x00, 0x03, 0x00, 0x63, 0x28, 0x01, 0x00, 0x17, 0x00, 0x07, 0x00,
            0x77, 0x65, 0x73, 0x74, 0x65, 0x72, 0x6e, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04,
            0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03,
            0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04,
            0x00, 0x0f, 0x00, 0x09, 0x00, 0x31, 0x34, 0x34, 0x36, 0x2e, 0x35, 0x2e, 0x33, 0x31,
            0x01, 0x00, 0x38, 0x28, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2838),
                BinaryToken::Object(6),
                BinaryToken::Token(0x2863),
                BinaryToken::Text(Scalar::new(b"western")),
                BinaryToken::Text(Scalar::new(b"1446.5.31")),
                BinaryToken::Token(0x2838),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_rgb() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x053a),
                BinaryToken::Rgb(Box::new(Rgb {
                    r: 110,
                    g: 27,
                    b: 27,
                })),
            ]
        );
    }

    #[test]
    fn test_u64() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        assert_eq!(
            parse(&data).unwrap().token_tape,
            vec![BinaryToken::Token(0x326b), BinaryToken::U64(128),]
        );
    }

    #[test]
    fn test_nested_arrays() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x9c, 0x02,
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x2863),
                BinaryToken::Array(7),
                BinaryToken::Array(3),
                BinaryToken::End(2),
                BinaryToken::Array(6),
                BinaryToken::U64(128),
                BinaryToken::End(4),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_heterogenous_list() {
        let data = [
            0x6f, 0x34, 0x01, 0x00, 0x03, 0x00, 0x0c, 0x00, 0x0a, 0x00, 0x00, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x14, 0x00, 0x02, 0x00, 0x00, 0x00, 0x0c, 0x00,
            0x01, 0x00, 0x00, 0x00, 0x01, 0x00, 0x14, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00,
            0xaa, 0xaa, 0x01, 0x00, 0x03, 0x00, 0xbb, 0xbb, 0x01, 0x00, 0xcc, 0xcc, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x346f),
                BinaryToken::Array(9),
                BinaryToken::I32(10),
                BinaryToken::Object(8),
                BinaryToken::I32(0),
                BinaryToken::U32(2),
                BinaryToken::I32(1),
                BinaryToken::U32(2),
                BinaryToken::End(3),
                BinaryToken::End(1),
                BinaryToken::Token(0xaaaa),
                BinaryToken::Object(14),
                BinaryToken::Token(0xbbbb),
                BinaryToken::Token(0xcccc),
                BinaryToken::End(11),
            ]
        );
    }

    #[test]
    fn test_incomplete_array() {
        let data = [0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x63, 0x28, 0x63, 0x28];
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_incomplete_object() {
        let data = [77, 40, 1, 0];
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_binary_regression() {
        let data = [3, 0, 0, 0, 1, 0, 1, 0];
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_binary_protect_against_deeply_nested() {
        let mut data = vec![0x63, 0x28, 0x01, 0x00];
        for _ in 0..10000 {
            data.extend(&[0x03, 0x00])
        }
        assert!(parse(&data[..]).is_err());
    }

    #[test]
    fn test_brace_regression() {
        let data = [137, 53, 3, 0, 4, 0];
        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(13705),
                BinaryToken::Array(2),
                BinaryToken::End(1)
            ]
        );
    }

    #[test]
    fn test_initial_end_does_not_panic() {
        let data = [4, 0];
        let res = parse(&data[..]);
        assert!(res.is_ok() || res.is_err());
    }
}
