use crate::BinaryDeError;

const MAX_DEPTH: usize = 16;

#[derive(Debug, PartialEq, Clone, Copy)]
enum DepthType {
    Object(usize),
    Array(usize),
}

impl Default for DepthType {
    fn default() -> Self {
        DepthType::Object(0)
    }
}

/// A pseudo fixed-length vector with a fallible push method. The core binary parser shouldn't
/// allocate so using a Vec was out of the question and outsourcing to an external crate (eg:
/// arrayvec) seemed overkill. Having a fixed length depth that a parser can reach has the nice
/// property that it prevents any sort of exhaustion or overflow.
#[derive(Debug, PartialEq, Clone, Default)]
struct Depth {
    depth: usize,
    values: [DepthType; MAX_DEPTH],
}

impl Depth {
    pub fn push(&mut self, val: DepthType) -> bool {
        if self.depth >= self.values.len() {
            false
        } else {
            self.values[self.depth] = val;
            self.depth += 1;
            true
        }
    }

    pub fn pop(&mut self) -> Option<DepthType> {
        if self.depth > 0 {
            self.depth -= 1;
            let res = self.values[self.depth];
            Some(res)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryToken {
    Array(usize),
    Object(usize),
    End(usize),
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    Text(usize),
    F32(f32),
    Q16(f32),
    Token(u16),
    Rgb(usize),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Rgb {
    pub r: u32,
    pub g: u32,
    pub b: u32,
}

#[derive(Debug, Default)]
pub struct BinTape<'a> {
    pub token_tape: Vec<BinaryToken>,
    pub data_tape: Vec<&'a [u8]>,
    pub rgb_tape: Vec<Rgb>,
    depth: Depth,
}

#[derive(Debug)]
enum ParseState {
    AtKey,
    AtKeyValueSeparator,
    AtObjectValue,
    AtArrayValue,
}

impl<'a> BinTape<'a> {
    pub fn from_slice(data: &[u8]) -> Result<BinTape<'_>, BinaryDeError> {
        let mut parser = BinTape::default();
        parser.parse(data)?;
        Ok(parser)
    }

    pub fn clear(&mut self) {
        self.token_tape.clear();
        self.data_tape.clear();
        self.rgb_tape.clear();
    }

    #[inline]
    fn parse_next_id(&mut self, data: &'a [u8]) -> Result<(&'a [u8], u16), BinaryDeError> {
        let val = data.get(..2).map(le_u16).ok_or(BinaryDeError::EarlyEof)?;
        Ok((&data[2..], val))
    }

    #[inline]
    fn parse_u32(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data.get(..4).map(le_u32).ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::U32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_u64(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data.get(..8).map(le_u64).ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::U64(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_i32(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data.get(..4).map(le_i32).ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::I32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_f32(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data
            .get(..4)
            .map(le_i32)
            .map(|x| (x as f32) / 1000.0)
            .ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::F32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_q16(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data
            .get(..8)
            .map(le_i32)
            .map(|x| {
                let mut val = x as f32;
                val = val * 2.0 / 65536.0 * 100_000.0;
                val.floor() / 100_000.0
            })
            .ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::Q16(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_bool(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data
            .get(0)
            .map(|&x| x != 0)
            .ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape.push(BinaryToken::Bool(val));
        Ok(&data[1..])
    }

    #[inline]
    fn parse_rgb(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let val = data
            .get(..22) // u16 `{` + (u16 + u32) * 3 + u16 `}`
            .map(|x| Rgb {
                r: le_u32(&x[4..]),
                g: le_u32(&x[10..]),
                b: le_u32(&x[16..]),
            })
            .ok_or(BinaryDeError::EarlyEof)?;
        let rgb_index = self.rgb_tape.len();
        self.token_tape.push(BinaryToken::Rgb(rgb_index));
        self.rgb_tape.push(val);
        Ok(&data[22..])
    }

    #[inline]
    fn parse_string(&mut self, data: &'a [u8]) -> Result<&'a [u8], BinaryDeError> {
        let text = data
            .get(..2)
            .and_then(|size| data.get(2..2 + usize::from(le_u16(size))).map(|data| data))
            .ok_or(BinaryDeError::EarlyEof)?;
        self.token_tape
            .push(BinaryToken::Text(self.data_tape.len()));
        self.data_tape.push(text);
        Ok(&data[text.len() + 2..])
    }

    pub fn parse(&mut self, mut data: &'a [u8]) -> Result<(), BinaryDeError> {
        self.token_tape.reserve(data.len() / 100 * 15);
        self.data_tape.reserve(data.len() / 10);
        let mut state = ParseState::AtKey;

        while !data.is_empty() {
            match state {
                ParseState::AtKey => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        },
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }
                        Q16 => { 
                            let res = self.parse_q16(data)?;
                            state = ParseState::AtKeyValueSeparator;
                            res
                        }

                        // Skip empty object
                        OPEN => {
                            let (d, token_id) = self.parse_next_id(d)?;
                            if token_id != END {
                                return Err(BinaryDeError::Message(String::from(
                                    "expected to skip empty object",
                                )));
                            }
                            d
                        }
                        END => {
                            let end_idx = self.token_tape.len();
                            let old_state = self.depth.pop().ok_or_else(|| BinaryDeError::StackEmpty)?;
                            let (old_parse_state, open_idx) = match old_state {
                                DepthType::Object(ind) => (ParseState::AtKey, ind),
                                DepthType::Array(ind) => (ParseState::AtArrayValue, ind),
                            };
                            state = old_parse_state;
                            self.token_tape[open_idx] = BinaryToken::Object(end_idx);
                            self.token_tape.push(BinaryToken::End(open_idx));
                            data
                        }
                        RGB => {
                            return Err(BinaryDeError::Message(String::from(
                                "RGB not valid for a key",
                            )));
                        }
                        EQUAL => {
                            return Err(BinaryDeError::Message(String::from(
                                "EQUAL not valid for a key",
                            )));
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::AtKeyValueSeparator;
                            &data
                        }
                    }
                }
                ParseState::AtKeyValueSeparator => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = match token_id {
                        EQUAL => Ok(&d),
                        OPEN => Ok(&data),
                        _ => Err(BinaryDeError::Message(String::from(
                            "expected an equal to separate key values",
                        ))),
                    }?;
                    state = ParseState::AtObjectValue;
                }
                ParseState::AtObjectValue => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::AtKey;
                            res
                        },
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        Q16 => { 
                            let res = self.parse_q16(data)?;
                            state = ParseState::AtKey;
                            res
                        }
                        OPEN => {
                            if !self.depth.push(DepthType::Object(self.token_tape.len())) {
                                return Err(BinaryDeError::StackExhausted);
                            }

                            self.token_tape.push(BinaryToken::Array(0));
                            let (d, token_id) = self.parse_next_id(data)?;
                            let old_data = data;
                            data = d;

                            match token_id {
                                // Empty array
                                END => {
                                    let end_idx = self.token_tape.len();
                                    let old_state = self.depth.pop().ok_or_else(|| BinaryDeError::StackEmpty)?;
                                    let (old_parse_state, open_idx) = match old_state {
                                        DepthType::Object(ind) => (ParseState::AtKey, ind),
                                        DepthType::Array(ind) => (ParseState::AtArrayValue, ind),
                                    };
                                    state = old_parse_state;
                                    self.token_tape[open_idx] = BinaryToken::Array(end_idx);
                                    self.token_tape.push(BinaryToken::End(open_idx));
                                    continue;
                                }

                                // array of objects or another array
                                OPEN => {
                                    state = ParseState::AtArrayValue;

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
                                    state = ParseState::AtObjectValue;
                                    data
                                },
                                EQUAL => {
                                    state = ParseState::AtObjectValue;
                                    d
                                },
                                _ => {
                                    state = ParseState::AtArrayValue;
                                    data
                                }
                            }
                        }
                        END => {
                            return Err(BinaryDeError::Message(String::from(
                                "END not valid for an object value",
                            )));
                        }
                        RGB => {
                            self.parse_rgb(data)?
                        }
                        EQUAL => {
                            return Err(BinaryDeError::Message(String::from(
                                "EQUAL not valid for an object value",
                            )));
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::AtKey;
                            &data
                        }
                    }
                }
                ParseState::AtArrayValue => {
                    let (d, token_id) = self.parse_next_id(data)?;
                    data = d;
                    data = match token_id {
                        U32 => {
                            let res = self.parse_u32(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        U64 => {
                            let res = self.parse_u64(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        I32 => {
                            let res = self.parse_i32(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        BOOL => {
                            let res = self.parse_bool(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        },
                        STRING_1 | STRING_2 => {
                            let res = self.parse_string(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        F32 => {
                            let res = self.parse_f32(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        Q16 => { 
                            let res = self.parse_q16(data)?;
                            state = ParseState::AtArrayValue;
                            res
                        }
                        OPEN => {
                            if !self.depth.push(DepthType::Array(self.token_tape.len())) {
                                return Err(BinaryDeError::StackExhausted);
                            }

                            self.token_tape.push(BinaryToken::Array(0));
                            let (d, token_id) = self.parse_next_id(data)?;
                            data = d;

                            match token_id {
                                // Empty array
                                END => {
                                    let end_idx = self.token_tape.len();
                                    let old_state = self.depth.pop().ok_or_else(|| BinaryDeError::StackEmpty)?;
                                    let (old_parse_state, open_idx) = match old_state {
                                        DepthType::Object(ind) => (ParseState::AtKey, ind),
                                        DepthType::Array(ind) => (ParseState::AtArrayValue, ind),
                                    };
                                    state = old_parse_state;
                                    self.token_tape[open_idx] = BinaryToken::Array(end_idx);
                                    self.token_tape.push(BinaryToken::End(open_idx));
                                    continue;
                                }

                                // array of objects or another array
                                OPEN => {
                                    if !self.depth.push(DepthType::Array(self.token_tape.len())) {
                                        return Err(BinaryDeError::StackExhausted);
                                    }

                                    self.token_tape.push(BinaryToken::Array(0));
                                    state = ParseState::AtArrayValue;
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
                                    state = ParseState::AtObjectValue;
                                    data
                                },
                                EQUAL => {
                                    state = ParseState::AtObjectValue;
                                    d
                                },
                                _ => {
                                    state = ParseState::AtArrayValue;
                                    data
                                }
                            }
                        }
                        END => {
                            let end_idx = self.token_tape.len();
                            let old_state = self.depth.pop().ok_or_else(|| BinaryDeError::StackEmpty)?;
                            let (old_parse_state, open_idx) = match old_state {
                                DepthType::Object(ind) => (ParseState::AtKey, ind),
                                DepthType::Array(ind) => (ParseState::AtArrayValue, ind),
                            };
                            state = old_parse_state;
                            self.token_tape[open_idx] = BinaryToken::Array(end_idx);
                            self.token_tape.push(BinaryToken::End(open_idx));
                            data
                        }
                        RGB => {
                            self.parse_rgb(data)?
                        }
                        EQUAL => {
                            return Err(BinaryDeError::Message(String::from(
                                "EQUAL not valid for an array value",
                            )));
                        }
                        x => {
                            self.token_tape.push(BinaryToken::Token(x));
                            state = ParseState::AtArrayValue;
                            &data
                        }
                    }
                }
            }
        }

        Ok(())
    }
}

#[inline]
fn le_u16(data: &[u8]) -> u16 {
    let ptr = data.as_ptr() as *const u8 as *const u16;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
fn le_u32(data: &[u8]) -> u32 {
    let ptr = data.as_ptr() as *const u8 as *const u32;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
fn le_u64(data: &[u8]) -> u64 {
    let ptr = data.as_ptr() as *const u8 as *const u64;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
fn le_i32(data: &[u8]) -> i32 {
    let ptr = data.as_ptr() as *const u8 as *const i32;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse<'a>(data: &'a [u8]) -> Result<BinTape<'a>, BinaryDeError> {
        BinTape::from_slice(data)
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
            vec![BinaryToken::Token(0x2d82), BinaryToken::Text(0),]
        );

        assert_eq!(tape.data_tape, vec![b"ENG"]);
    }

    #[test]
    fn test_string2_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![BinaryToken::Token(0x2d82), BinaryToken::Text(0),]
        );

        assert_eq!(tape.data_tape, vec![b"ENG"]);
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
                BinaryToken::Text(0),
                BinaryToken::Text(1),
                BinaryToken::Text(2),
                BinaryToken::Text(3),
                BinaryToken::End(1),
            ]
        );

        let data: Vec<&'static [u8]> = vec![
            b"Art of War",
            b"Conquest of Paradise",
            b"Res Publica",
            b"Wealth of Nations",
        ];

        assert_eq!(tape.data_tape, data);
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
                BinaryToken::Text(0),
                BinaryToken::Text(1),
                BinaryToken::End(1),
            ]
        );

        let data: Vec<&'static [u8]> = vec![b"schools_initiated", b"1444.11.11\n"];

        assert_eq!(tape.data_tape, data);
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
                BinaryToken::Text(0),
                BinaryToken::Text(1),
                BinaryToken::Token(0x2838),
                BinaryToken::End(1),
            ]
        );

        let data: Vec<&'static [u8]> = vec![b"western", b"1446.5.31"];

        assert_eq!(tape.data_tape, data);
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
            vec![BinaryToken::Token(0x053a), BinaryToken::Rgb(0),]
        );
        let rgb_tape = vec![Rgb {
            r: 110,
            g: 27,
            b: 27,
        }];
        assert_eq!(tape.rgb_tape, rgb_tape);
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
    fn test_binary_regression() {
        let data = [3, 0, 0, 0, 1, 0, 1, 0];
        assert!(parse(&data[..]).is_err());
    }
}
