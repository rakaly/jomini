#[derive(Debug, PartialEq)]
pub enum BinaryToken {
    //    Hsv(u32, u32, u32),
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
const HSV: u16 = 0x0243;

type MyError = String;

pub struct BinTape<'a> {
    pub token_tape: Vec<BinaryToken>,
    pub data_tape: Vec<&'a [u8]>,
}

impl<'a> BinTape<'a> {
    pub fn from_slice(data: &[u8]) -> Result<BinTape<'_>, MyError> {
        let mut parser = BinTape {
            token_tape: Vec::new(),
            data_tape: Vec::new(),
        };

        parser.parse(data)?;
        Ok(parser)
    }

    #[inline]
    fn parse_next_id(&mut self, data: &'a [u8]) -> Result<(&'a [u8], u16), MyError> {
        let val = data.get(..2).map(le_u16).expect("EEK");
        Ok((&data[2..], val))
    }

    #[inline]
    fn parse_u32(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data.get(..4).map(le_u32).expect("EEK");
        self.token_tape.push(BinaryToken::U32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_u64(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data.get(..8).map(le_u64).expect("EEK");
        self.token_tape.push(BinaryToken::U64(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_i32(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data
            .get(..4)
            .map(le_i32)
            .ok_or_else(|| MyError::from("EEK"))?;
        self.token_tape.push(BinaryToken::I32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_f32(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data
            .get(..4)
            .map(le_i32)
            .map(|x| (x as f32) / 1000.0)
            .expect("EEK");
        self.token_tape.push(BinaryToken::F32(val));
        Ok(&data[4..])
    }

    #[inline]
    fn parse_q16(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data
            .get(..8)
            .map(le_i32)
            .map(|x| {
                let mut val = x as f32;
                val = val * 2.0 / 65536.0 * 100_000.0;
                val.floor() / 100_000.0
            })
            .expect("EEK");
        self.token_tape.push(BinaryToken::Q16(val));
        Ok(&data[8..])
    }

    #[inline]
    fn parse_bool(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let val = data.get(0).map(|&x| x != 0).expect("EEK");
        self.token_tape.push(BinaryToken::Bool(val));
        Ok(&data[1..])
    }

    #[inline]
    fn parse_string(&mut self, data: &'a [u8]) -> Result<&'a [u8], MyError> {
        let text = data
            .get(..2)
            .and_then(|size| data.get(2..2 + usize::from(le_u16(size))).map(|data| data))
            .expect("EEK");
        self.token_tape
            .push(BinaryToken::Text(self.data_tape.len()));
        self.data_tape.push(text);
        Ok(&data[text.len() + 2..])
    }

    #[inline]
    fn parse_inner_object(
        &mut self,
        mut data: &'a [u8],
        open_idx: usize,
    ) -> Result<&'a [u8], MyError> {
        loop {
            let (d, token_id) = self.parse_next_id(data)?;
            data = d;

            data = match token_id {
                END => {
                    let end_idx = self.token_tape.len();
                    self.token_tape[open_idx] = BinaryToken::Object(end_idx);
                    self.token_tape.push(BinaryToken::End(open_idx));
                    return Ok(&data);
                }
                U32 => self.parse_u32(data)?,
                U64 => self.parse_u64(data)?,
                I32 => self.parse_i32(data)?,
                BOOL => self.parse_bool(data)?,
                STRING_1 | STRING_2 => self.parse_string(data)?,
                F32 => self.parse_f32(data)?,
                Q16 => self.parse_q16(data)?,
                OPEN => {
                    let open_idx2 = self.token_tape.len();
                    self.token_tape.push(BinaryToken::Array(0));
                    self.parse_open(data, open_idx2)?
                }
                HSV => todo!(),
                EQUAL => &data,
                x => {
                    self.token_tape.push(BinaryToken::Token(x));
                    &data
                }
            }
        }
    }

    #[inline]
    fn parse_array(&mut self, mut data: &'a [u8], open_idx: usize) -> Result<&'a [u8], MyError> {
        loop {
            let (d, token_id) = self.parse_next_id(data)?;
            data = d;
            data = match token_id {
                END => {
                    let end_idx = self.token_tape.len();
                    self.token_tape[open_idx] = BinaryToken::Array(end_idx);
                    self.token_tape.push(BinaryToken::End(open_idx));
                    return Ok(&data);
                }
                U32 => self.parse_u32(data)?,
                U64 => self.parse_u64(data)?,
                I32 => self.parse_i32(data)?,
                BOOL => self.parse_bool(data)?,
                STRING_1 | STRING_2 => self.parse_string(data)?,
                F32 => self.parse_f32(data)?,
                Q16 => self.parse_q16(data)?,
                OPEN => {
                    let open_idx2 = self.token_tape.len();
                    self.token_tape.push(BinaryToken::Object(0));
                    self.parse_inner_object(data, open_idx2)?
                }
                HSV | EQUAL => todo!(),
                x => {
                    self.token_tape.push(BinaryToken::Token(x));
                    &data
                }
            }
        }
    }

    #[inline]
    fn first_val(&mut self, data: &'a [u8], open_idx: usize) -> Result<&'a [u8], MyError> {
        let (d, token_id) = self.parse_next_id(data)?;
        match token_id {
            OPEN => self.parse_inner_object(data, open_idx),
            EQUAL => self.parse_inner_object(d, open_idx),
            _ => self.parse_array(data, open_idx),
        }
    }

    #[inline]
    fn parse_open(&mut self, mut data: &'a [u8], open_idx: usize) -> Result<&'a [u8], MyError> {
        let (d, token_id) = self.parse_next_id(data)?;
        let old_data = data;
        data = d;

        match token_id {
            // Empty array
            END => {
                let end_idx = self.token_tape.len();
                self.token_tape[open_idx] = BinaryToken::Array(end_idx);
                self.token_tape.push(BinaryToken::End(open_idx));
                Ok(&data)
            }

            // array of objects
            OPEN => self.parse_array(old_data, open_idx),

            U32 => {
                data = self.parse_u32(data)?;
                self.first_val(data, open_idx)
            }
            U64 => {
                data = self.parse_u64(data)?;
                self.first_val(data, open_idx)
            }
            I32 => {
                data = self.parse_i32(data)?;
                self.first_val(data, open_idx)
            }
            BOOL => {
                data = self.parse_bool(data)?;
                self.first_val(data, open_idx)
            }
            STRING_1 | STRING_2 => {
                data = self.parse_string(data)?;
                self.first_val(data, open_idx)
            }
            F32 => {
                data = self.parse_f32(data)?;
                self.first_val(data, open_idx)
            }
            Q16 => {
                data = self.parse_q16(data)?;
                self.first_val(data, open_idx)
            }
            HSV => todo!(),
            x => {
                self.token_tape.push(BinaryToken::Token(x));
                self.first_val(data, open_idx)
            }
        }
    }

    fn parse(&mut self, mut data: &'a [u8]) -> Result<(), MyError> {
        while !data.is_empty() {
            let (d, token_id) = self.parse_next_id(data)?;
            data = d;
            data = match token_id {
                U32 => self.parse_u32(data)?,
                U64 => self.parse_u64(data)?,
                I32 => self.parse_i32(data)?,
                BOOL => self.parse_bool(data)?,
                STRING_1 | STRING_2 => self.parse_string(data)?,
                F32 => self.parse_f32(data)?,
                Q16 => self.parse_q16(data)?,
                OPEN => {
                    let open_idx = self.token_tape.len();
                    self.token_tape.push(BinaryToken::Array(0));
                    self.parse_open(data, open_idx)?
                }
                END => todo!(),
                HSV => todo!(),
                EQUAL => &data,
                x => {
                    self.token_tape.push(BinaryToken::Token(x));
                    &data
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

    fn parse<'a>(data: &'a [u8]) -> Result<BinTape<'a>, MyError> {
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

    /*    #[test]
    fn test_empty_objects() {
        let mut data = vec![0x63, 0x28, 0x01, 0x00, 0x17, 0x00, 0x07, 0x00];
        data.extend_from_slice(b"western");
        data.extend_from_slice(&[
            0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00,
        ]);
        data.extend_from_slice(&[0x0f, 0x00, 0x08, 0x00]);
        data.extend_from_slice(b"1451.5.2");
        data.extend_from_slice(&[0x01, 0x00]);
        data.extend_from_slice(b"abc");
        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2863),
                BinaryToken::Text(0),Scalar::new(b"western")),
                BinaryToken::Text(1), Scalar::new(b"1451.5.2")),
            ]
        );

        let data: Vec<&'static [u8]> = vec![
                b"schools_initiated",
                b"1444.11.11\n",
            ];
    }*/

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

    /*#[test]
    fn test_hsv() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryToken::Token(0x053a),
                BinaryToken::Operator(Operator::Equal),
                BinaryToken::Hsv(110, 27, 27),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }*/

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
}
