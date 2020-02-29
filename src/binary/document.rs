use crate::de::binary::BinaryDeError;
use crate::{BinaryError, BinaryEvent, BinaryParser, Operator, Scalar};

#[derive(Debug, PartialEq)]
pub enum ScalarValue<'a> {
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    Text(Scalar<'a>),
    F32(f32),
    Q16(f32),
    Token(u16),
}

#[derive(Debug, PartialEq)]
pub struct ValueEntry<'a> {
    pub key: ScalarValue<'a>,
    pub operator: Option<Operator>,
    pub value: BorrowedBinaryValue<'a>,
}

#[derive(Debug, PartialEq)]
pub enum BorrowedBinaryValue<'a> {
    Scalar(ScalarValue<'a>),
    Hsv(u32, u32, u32),
    Array(Vec<BorrowedBinaryValue<'a>>),
    Object(Vec<ValueEntry<'a>>),
}

fn parse_array<'a, P>(iter: &mut P) -> Result<Vec<BorrowedBinaryValue<'a>>, BinaryDeError>
where
    P: Iterator<Item = Result<BinaryEvent<'a>, BinaryError>>,
{
    let mut res = Vec::new();
    loop {
        let event = iter
            .next()
            .ok_or_else(|| BinaryDeError::Message(String::from("expected value in array")))?;

        let value = match event? {
            BinaryEvent::Bool(x) => BorrowedBinaryValue::Scalar(ScalarValue::Bool(x)),
            BinaryEvent::U32(x) => BorrowedBinaryValue::Scalar(ScalarValue::U32(x)),
            BinaryEvent::U64(x) => BorrowedBinaryValue::Scalar(ScalarValue::U64(x)),
            BinaryEvent::I32(x) => BorrowedBinaryValue::Scalar(ScalarValue::I32(x)),
            BinaryEvent::Text(x) => BorrowedBinaryValue::Scalar(ScalarValue::Text(x)),
            BinaryEvent::F32(x) => BorrowedBinaryValue::Scalar(ScalarValue::F32(x)),
            BinaryEvent::Q16(x) => BorrowedBinaryValue::Scalar(ScalarValue::Q16(x)),
            BinaryEvent::Token(x) => BorrowedBinaryValue::Scalar(ScalarValue::Token(x)),
            BinaryEvent::StartObject => BorrowedBinaryValue::Object(parse_object(iter)?),
            BinaryEvent::StartArray => BorrowedBinaryValue::Array(parse_array(iter)?),
            BinaryEvent::End => return Ok(res),
            BinaryEvent::Hsv(x, y, z) => BorrowedBinaryValue::Hsv(x, y, z),
            BinaryEvent::Operator(_) => {
                return Err(BinaryDeError::Message(String::from(
                    "did not expect an operator in an array",
                )))
            }
        };

        res.push(value);
    }
}

fn parse_object<'a, P>(iter: &mut P) -> Result<Vec<ValueEntry<'a>>, BinaryDeError>
where
    P: Iterator<Item = Result<BinaryEvent<'a>, BinaryError>>,
{
    let mut res = Vec::new();
    loop {
        let event = iter.next();
        match event {
            None => return Ok(res),
            Some(x) => {
                let key = match x? {
                    BinaryEvent::End => return Ok(res),
                    BinaryEvent::StartObject
                    | BinaryEvent::StartArray
                    | BinaryEvent::Operator(_)
                    | BinaryEvent::Hsv(_, _, _) => {
                        return Err(BinaryDeError::Message(String::from(
                            "expected a scalar value for a key",
                        )));
                    }
                    BinaryEvent::Bool(x) => ScalarValue::Bool(x),
                    BinaryEvent::U32(x) => ScalarValue::U32(x),
                    BinaryEvent::U64(x) => ScalarValue::U64(x),
                    BinaryEvent::I32(x) => ScalarValue::I32(x),
                    BinaryEvent::Text(x) => ScalarValue::Text(x),
                    BinaryEvent::F32(x) => ScalarValue::F32(x),
                    BinaryEvent::Q16(x) => ScalarValue::Q16(x),
                    BinaryEvent::Token(x) => ScalarValue::Token(x),
                };

                let operator_event = iter.next().ok_or_else(|| {
                    BinaryDeError::Message(String::from("expected operator in object"))
                })?;

                match operator_event? {
                    BinaryEvent::Operator(op) => {
                        let value_event = iter.next().ok_or_else(|| {
                            BinaryDeError::Message(String::from("expected value in object"))
                        })?;
                        let value = match value_event? {
                            BinaryEvent::Bool(x) => {
                                BorrowedBinaryValue::Scalar(ScalarValue::Bool(x))
                            }
                            BinaryEvent::U32(x) => BorrowedBinaryValue::Scalar(ScalarValue::U32(x)),
                            BinaryEvent::U64(x) => BorrowedBinaryValue::Scalar(ScalarValue::U64(x)),
                            BinaryEvent::I32(x) => BorrowedBinaryValue::Scalar(ScalarValue::I32(x)),
                            BinaryEvent::Text(x) => {
                                BorrowedBinaryValue::Scalar(ScalarValue::Text(x))
                            }
                            BinaryEvent::F32(x) => BorrowedBinaryValue::Scalar(ScalarValue::F32(x)),
                            BinaryEvent::Q16(x) => BorrowedBinaryValue::Scalar(ScalarValue::Q16(x)),
                            BinaryEvent::Token(x) => {
                                BorrowedBinaryValue::Scalar(ScalarValue::Token(x))
                            }
                            BinaryEvent::StartObject => {
                                BorrowedBinaryValue::Object(parse_object(iter)?)
                            }
                            BinaryEvent::Hsv(x, y, z) => BorrowedBinaryValue::Hsv(x, y, z),
                            BinaryEvent::StartArray => {
                                BorrowedBinaryValue::Array(parse_array(iter)?)
                            }
                            BinaryEvent::Operator(_) | BinaryEvent::End => {
                                return Err(BinaryDeError::Message(String::from(
                                    "unexpected value for object pair",
                                )));
                            }
                        };

                        res.push(ValueEntry {
                            key,
                            operator: Some(op),
                            value,
                        });
                    }
                    BinaryEvent::StartArray => {
                        let value = BorrowedBinaryValue::Array(parse_array(iter)?);
                        res.push(ValueEntry {
                            key,
                            operator: None,
                            value,
                        });
                    }
                    BinaryEvent::StartObject => {
                        let value = BorrowedBinaryValue::Object(parse_object(iter)?);
                        res.push(ValueEntry {
                            key,
                            operator: None,
                            value,
                        });
                    }
                    _ => {
                        return Err(BinaryDeError::Message(String::from(
                            "unexpected value for when searching for operator",
                        )));
                    }
                }
            }
        }
    }
}

pub fn document_from_slice<'a>(data: &'a [u8]) -> Result<Vec<ValueEntry<'a>>, BinaryDeError> {
    let mut parser = BinaryParser::new();
    let mut events = parser.events(data);
    parse_object(&mut events)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_single_field() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];

        let expected = vec![ValueEntry {
            key: ScalarValue::Token(0x2d82),
            operator: Some(Operator::Equal),
            value: BorrowedBinaryValue::Scalar(ScalarValue::Text(Scalar::new(b"ENG"))),
        }];

        assert_eq!(document_from_slice(&data[..]).unwrap(), expected);
    }

    #[test]
    fn test_consecutive_nested_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d,
            0x01, 0x00, 0x0c, 0x00, 0x02, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00, 0xc9, 0x2e,
            0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x03, 0x00, 0x82, 0x2d, 0x01, 0x00,
            0x0c, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        let expected = vec![
            ValueEntry {
                key: ScalarValue::Token(0x2ec9),
                operator: Some(Operator::Equal),
                value: BorrowedBinaryValue::Object(vec![ValueEntry {
                    key: ScalarValue::Token(0x28e2),
                    operator: Some(Operator::Equal),
                    value: BorrowedBinaryValue::Object(vec![ValueEntry {
                        key: ScalarValue::Token(0x2d82),
                        operator: Some(Operator::Equal),
                        value: BorrowedBinaryValue::Scalar(ScalarValue::I32(2)),
                    }]),
                }]),
            },
            ValueEntry {
                key: ScalarValue::Token(0x2ec9),
                operator: Some(Operator::Equal),
                value: BorrowedBinaryValue::Object(vec![ValueEntry {
                    key: ScalarValue::Token(0x28e2),
                    operator: Some(Operator::Equal),
                    value: BorrowedBinaryValue::Object(vec![ValueEntry {
                        key: ScalarValue::Token(0x2d82),
                        operator: Some(Operator::Equal),
                        value: BorrowedBinaryValue::Scalar(ScalarValue::I32(3)),
                    }]),
                }]),
            },
        ];

        assert_eq!(document_from_slice(&data[..]).unwrap(), expected);
    }
}
