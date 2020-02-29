use crate::scalar::Scalar;
use crate::text::Operator;
use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{le_i32, le_u16, le_u32, le_u64, le_u8},
    IResult,
};

#[derive(Debug, PartialEq)]
pub struct BinaryRefEntry<'a> {
    pub key: BinaryRef<'a>,
    pub operator: Option<Operator>,
    pub value: BinaryRef<'a>,
}

#[derive(Debug, PartialEq)]
pub enum BinaryRef<'a> {
    //    Scalar(ScalarValue<'a>),
    Hsv(u32, u32, u32),
    Array(Vec<BinaryRef<'a>>),
    Object(Vec<BinaryRefEntry<'a>>),
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    Text(Scalar<'a>),
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

fn parse_string(d: &[u8]) -> IResult<&[u8], Scalar<'_>> {
    let (d, size) = le_u16(d)?;
    let (d, data) = take(size)(d)?;
    Ok((d, Scalar::new(data)))
}

fn parse_q16(d: &[u8]) -> IResult<&[u8], f32> {
    let (d, x) = le_i32(d)?;
    let (d, _data) = le_i32(d)?;
    let mut val = x as f32;
    val = val * 2.0 / 65536.0 * 100_000.0;
    Ok((d, val.floor() / 100_000.0))
}

fn parse_array<'a>(mut d: &'a [u8], first_val: BinaryRef<'a>) -> IResult<&'a [u8], BinaryRef<'a>> {
    let mut result = vec![first_val];
    loop {
        let (d2, token_id) = le_u16(d)?;
        d = d2;
        match token_id {
            END => return Ok((d, BinaryRef::Array(result))),
            x => {
                let (d2, val) = parse_value2(d, x)?;
                d = d2;
                result.push(val);
            }
        }
    }
}

pub fn parse<'a>(mut d: &'a [u8]) -> IResult<&'a [u8], Vec<BinaryRefEntry<'a>>> {
    let mut entries = Vec::new();
    while !d.is_empty() {
        let (d2, key) = parse_value(d)?;
        d = d2;

        let (d2, _operator_id) = le_u16(d)?;
        d = d2;

        let (d2, value) = parse_value(d)?;
        d = d2;

        entries.push(BinaryRefEntry {
            key: key,
            operator: Some(Operator::Equal),
            value: value,
        });
    }

    Ok((d, entries))
}

fn parse_object2<'a>(
    mut d: &'a [u8],
    mut entries: Vec<BinaryRefEntry<'a>>,
) -> IResult<&'a [u8], Vec<BinaryRefEntry<'a>>> {
    loop {
        let (d2, token_id) = le_u16(d)?;
        d = d2;
        match token_id {
            END => return Ok((d, entries)),
            x => {
                let (d2, key) = parse_value2(d, x)?;
                d = d2;

                let (d2, _operator_id) = le_u16(d)?;
                d = d2;

                let (d2, value) = parse_value(d)?;
                d = d2;

                entries.push(BinaryRefEntry {
                    key: key,
                    operator: Some(Operator::Equal),
                    value: value,
                });
            }
        }
    }
}

fn parse_object<'a>(
    d: &'a [u8],
    first_key: BinaryRef<'a>,
) -> IResult<&'a [u8], Vec<BinaryRefEntry<'a>>> {
    let mut entries = Vec::new();
    let (d, first_value) = parse_value(d)?;
    entries.push(BinaryRefEntry {
        key: first_key,
        operator: Some(Operator::Equal),
        value: first_value,
    });

    parse_object2(d, entries)
}

fn first_val<'a>(init_d: &'a [u8], val: BinaryRef<'a>) -> IResult<&'a [u8], BinaryRef<'a>> {
    let (d, token_id) = le_u16(init_d)?;
    match token_id {
        EQUAL => {
            let (d, x) = parse_object(d, val)?;
            Ok((d, BinaryRef::Object(x)))
        }
        _ => parse_array(init_d, val),
    }
}

pub fn parse_open(d: &[u8]) -> IResult<&[u8], BinaryRef<'_>> {
    let (d, token_id) = le_u16(d)?;
    match token_id {
        // empty array
        END => Ok((d, BinaryRef::Array(Vec::new()))),

        // array of objects
        OPEN => {
            let entries = Vec::new();
            let (d, x) = parse_object2(d, entries)?;
            parse_array(d, BinaryRef::Object(x))
        }

        U32 => {
            let (d, val) = map(le_u32, |x| BinaryRef::U32(x))(d)?;
            first_val(d, val)
        }
        U64 => {
            let (d, val) = map(le_u64, |x| BinaryRef::U64(x))(d)?;
            first_val(d, val)
        }
        I32 => {
            let (d, val) = map(le_i32, |x| BinaryRef::I32(x))(d)?;
            first_val(d, val)
        }
        BOOL => {
            let (d, val) = map(le_u8, |x| BinaryRef::Bool(x != 0))(d)?;
            first_val(d, val)
        }
        STRING_1 | STRING_2 => {
            let (d, val) = map(parse_string, |x| BinaryRef::Text(x))(d)?;
            first_val(d, val)
        }
        F32 => {
            let (d, val) = map(le_i32, |x| BinaryRef::F32((x as f32) / 1000.0))(d)?;
            first_val(d, val)
        }
        Q16 => {
            let (d, val) = map(parse_q16, |x| BinaryRef::Q16(x))(d)?;
            first_val(d, val)
        }
        x => first_val(d, BinaryRef::Token(x)),
    }
}

pub fn parse_value2(d: &[u8], token_id: u16) -> IResult<&[u8], BinaryRef<'_>> {
    match token_id {
        U32 => map(le_u32, |x| BinaryRef::U32(x))(d),
        U64 => map(le_u64, |x| BinaryRef::U64(x))(d),
        I32 => map(le_i32, |x| BinaryRef::I32(x))(d),
        BOOL => map(le_u8, |x| BinaryRef::Bool(x != 0))(d),
        STRING_1 | STRING_2 => map(parse_string, |x| BinaryRef::Text(x))(d),
        F32 => map(le_i32, |x| BinaryRef::F32((x as f32) / 1000.0))(d),
        Q16 => map(parse_q16, |x| BinaryRef::Q16(x))(d),
        OPEN => parse_open(d),
        HSV => todo!(),
        x => Ok((d, BinaryRef::Token(x))),
    }
}

pub fn parse_value(d: &[u8]) -> IResult<&[u8], BinaryRef<'_>> {
    let (d, token_id) = le_u16(d)?;
    parse_value2(d, token_id)
}
