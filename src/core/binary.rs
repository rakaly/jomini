use crate::core::depth::{Depth, DepthType};
use crate::scalar::Scalar;
use crate::text::Operator;
use byteorder::{ByteOrder, LE};
use std::error;
use std::fmt::{self, Display, Formatter};

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

/// The type of binary error that occurred
#[derive(Debug)]
pub enum BinaryErrorKind {
    /// When too many '}' are seen
    EmptyStack,

    /// When too many '{' are seen
    FullStack,

    /// When an expected token is decoded
    UnexpectedToken(&'static str, u16),
}

/// Error that occurred while parsing binary data with positional data
#[derive(Debug)]
pub struct BinaryError {
    pub kind: BinaryErrorKind,
    pub position: usize,
}

impl Display for BinaryError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.kind {
            BinaryErrorKind::EmptyStack => write!(f, "empty stack"),
            BinaryErrorKind::FullStack => write!(f, "full stack"),
            BinaryErrorKind::UnexpectedToken(section, token) => write!(
                f,
                "unexpected token of 0x{:x} during {} tokenization",
                token, section
            ),
        }?;

        write!(f, " at offset: {}", self.position)
    }
}

impl error::Error for BinaryError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

/// The binary event (if any) that was parsed
///
/// This enum encapsulates the possibility that there was not enough data in the input to parse
/// an event. It doesn't make sense to add this possibility (the eof) to the `BinaryEvent` as
/// it would be counter intuitive for iterators to yield an eof (or would they stop and never yield
/// an eof leaving the caller with unreachable match logic). To save ourselves from this trap,
/// introduce a new type that for the APIs that indicate eof.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryParseEvent<'a> {
    /// The event that was parsed
    Event(BinaryEvent<'a>),

    /// The input did not have enough data to read the next event
    Eof,
}

/// An extracted event from the binary data
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryEvent<'a> {
    StartObject,
    StartArray,
    End,
    Operator(Operator),
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    Text(Scalar<'a>),
    F32(f32),
    Q16(f32),
    Token(u16),
    Hsv(u32, u32, u32),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum EventType {
    Key,
    Operator,
    Value,
}

#[derive(Debug, PartialEq, Clone)]
struct BinaryParserState {
    position: usize,
    expectant: EventType,
    depth: Depth,
}

/// A low level, zero-allocation, pull based parser for binary data
///
/// There are two recommended ways to use this parser.
///
/// - If you have all the data that you need to parse into a single input, use the `events`
/// iterator. Ergonomic and efficient.
/// - If the data is in chunks use `read_event`. The function will return either the event and how
/// much of the input was consumed, or that the end of the buffer was reached. Since the parser
/// starts reading from the start of the input with each `read_event`, it is assumed that the
/// caller increments the underlying slice out of band. Since this is not ergonomic, consider the
/// `BinaryReader` which works with a `Read` trait.
///
/// Both parsing methods can fail due to a encoding error.
///
/// The binary format stores strings as Windows-1252 encoded, so the data being parsed is assumed
/// to be Windows-1252 encoded.
///
/// It is the caller's responsibility to also uncompress the data and strip any magic code prior
/// to parsing.
///
/// While low level, this parser requires the use of the standard library to provide a nice middle
/// ground between ergonomics (using the standard library `Result` type, and iterators),
/// readability (no need for a additional parser that is even lower level) and performance.
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryParser {
    state: BinaryParserState,
}

impl Default for BinaryParser {
    fn default() -> Self {
        Self::new()
    }
}

impl BinaryParser {
    /// Create a new parser, assuming that the parser starts in an object
    pub fn new() -> Self {
        let mut depth = Depth::new();
        let pushed = depth.push(DepthType::Object);
        debug_assert!(pushed);

        BinaryParser {
            state: BinaryParserState {
                expectant: EventType::Key,
                position: 0,
                depth,
            },
        }
    }

    /// The accumulated byte offset
    pub fn position(&self) -> usize {
        self.state.position
    }

    /// Returns whether the parser is inside an object
    pub fn in_object(&self) -> bool {
        self.current_depth()
            .map(|x| x == DepthType::Object)
            .unwrap_or(true)
    }

    /// Returns whether the parser is inside an array
    pub fn in_array(&self) -> bool {
        self.current_depth()
            .map(|x| x == DepthType::Array)
            .unwrap_or(false)
    }

    pub fn expect_key(&self) -> bool {
        self.state.expectant == EventType::Key
    }

    pub fn expect_operator(&self) -> bool {
        self.state.expectant == EventType::Operator
    }

    pub fn expect_value(&self) -> bool {
        self.state.expectant == EventType::Value
    }

    pub fn depth(&self) -> usize {
        // Minus 1 here as we root object has a depth of 1.
        self.state.depth.len() - 1
    }

    fn current_depth(&self) -> Result<DepthType, BinaryError> {
        self.state.depth.last().ok_or_else(|| BinaryError {
            kind: BinaryErrorKind::EmptyStack,
            position: self.state.position,
        })
    }

    fn push_depth(&mut self, depth: DepthType) -> Result<(), BinaryError> {
        if !self.state.depth.push(depth) {
            Err(BinaryError {
                kind: BinaryErrorKind::FullStack,
                position: self.state.position,
            })
        } else {
            Ok(())
        }
    }

    fn peek_inner_value<'a>(
        &mut self,
        input: &'a [u8],
    ) -> Result<(BinaryParseEvent<'a>, usize), BinaryError> {
        // Objects and array use the same opener so we peek at the next token to
        // get a better idea what we're looking at.
        //
        // - { x y # An array
        // - { x = # An object
        // - { {   # An array of objects
        // - { x } # An array of one element
        // - {   } # An empty object (should be skipped if at start of an key)

        // If we're already in an array (array of objects)
        if self.current_depth()? == DepthType::Array {
            self.push_depth(DepthType::Object)?;
            self.state.expectant = EventType::Key;
            return Ok((BinaryParseEvent::Event(BinaryEvent::StartObject), 0));
        }

        match input.get(..2) {
            None => Ok((BinaryParseEvent::Eof, 0)),
            Some(token_cursor) => {
                match LE::read_u16(token_cursor) {
                    // array of objects or an empty array
                    OPEN | END => {
                        self.push_depth(DepthType::Array)?;
                        self.state.expectant = EventType::Value;
                        Ok((BinaryParseEvent::Event(BinaryEvent::StartArray), 0))
                    }

                    // If we see a value it could still be an object! So we have to skip this value
                    // and read the next to know for sure.
                    token_id => {
                        let next_cursor = &input[2..];

                        let to_skip = match token_id {
                            U32 | I32 | F32 => 4,
                            BOOL => 1,
                            U64 | Q16 => 8,
                            STRING_1 | STRING_2 => {
                                if next_cursor.len() < 2 {
                                    return Ok((BinaryParseEvent::Eof, 0));
                                }

                                let string_len = LE::read_u16(&next_cursor) as usize;
                                if next_cursor.len() < 2 + string_len {
                                    return Ok((BinaryParseEvent::Eof, 0));
                                }

                                2 + string_len
                            }
                            _ => 0,
                        };

                        // Do we have enough to read the first key / value + the next id?
                        if next_cursor.len() < to_skip + 2 {
                            return Ok((BinaryParseEvent::Eof, 0));
                        }

                        // So what is the next id that we're interested in?
                        let next_token_id = LE::read_u16(&next_cursor[to_skip..]);

                        // Excellent we have enough data, so we can consume the initial "{"
                        if next_token_id == EQUAL {
                            self.push_depth(DepthType::Object)?;
                            self.state.expectant = EventType::Key;
                            Ok((BinaryParseEvent::Event(BinaryEvent::StartObject), 0))
                        } else {
                            self.push_depth(DepthType::Array)?;
                            self.state.expectant = EventType::Value;
                            Ok((BinaryParseEvent::Event(BinaryEvent::StartArray), 0))
                        }
                    }
                }
            }
        }
    }

    fn next_value<'a>(
        &mut self,
        input: &'a [u8],
        token_id: u16,
    ) -> Result<(BinaryParseEvent<'a>, usize), BinaryError> {
        match token_id {
            U32 => input
                .get(..4)
                .map(LE::read_u32)
                .map(BinaryEvent::U32)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 4)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            U64 => input
                .get(..8)
                .map(LE::read_u64)
                .map(BinaryEvent::U64)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 8)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            I32 => input
                .get(..4)
                .map(LE::read_i32)
                .map(BinaryEvent::I32)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 4)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            BOOL => input
                .get(0)
                .map(|&x| x != 0)
                .map(BinaryEvent::Bool)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 1)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            STRING_1 | STRING_2 => input
                .get(..2)
                .map(LE::read_u16)
                .map(|x| x as usize)
                .and_then(|len| input.get(2..len + 2))
                .map(|data| (data, data.len() + 2))
                .map(|(data, read)| {
                    Ok((
                        BinaryParseEvent::Event(BinaryEvent::Text(Scalar::new(data))),
                        read,
                    ))
                })
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            F32 => input
                .get(..4)
                .map(LE::read_i32)
                .map(|x| (x as f32) / 1000.0)
                .map(BinaryEvent::F32)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 4)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            Q16 => input
                .get(..8)
                .map(LE::read_i32)
                .map(|x| {
                    let mut val = x as f32;
                    val = val * 2.0 / 65536.0 * 100_000.0;
                    val.floor() / 100_000.0
                })
                .map(BinaryEvent::Q16)
                .map(BinaryParseEvent::Event)
                .map(|x| Ok((x, 8)))
                .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0))),
            OPEN => self.peek_inner_value(input),
            END => {
                // If we are in an object trying to decode a value and see a '}' -- that would be
                // unexpected so we should error
                if self.in_object() && self.state.expectant == EventType::Value {
                    return Err(BinaryError {
                        kind: BinaryErrorKind::UnexpectedToken("value", END),
                        position: self.state.position,
                    });
                }

                let popped = self.state.depth.pop().ok_or_else(|| BinaryError {
                    kind: BinaryErrorKind::EmptyStack,
                    position: self.state.position,
                })?;

                self.state.expectant = match popped {
                    DepthType::Array => EventType::Value,
                    _ => EventType::Key,
                };

                Ok((BinaryParseEvent::Event(BinaryEvent::End), 0))
            }
            HSV => {
                input
                    .get(..22) // u16 `{` + (u16 + u32) * 3 + u16 `}`
                    .map(|x| {
                        (
                            BinaryEvent::Hsv(
                                // We skip any intermediate checks that these are actual U32 events.
                                // It's probably ok as we do a check for the end token after this
                                // decode
                                LE::read_u32(&x[4..]),
                                LE::read_u32(&x[10..]),
                                LE::read_u32(&x[16..]),
                            ),
                            LE::read_u16(&x[20..]),
                        )
                    })
                    .map(|(hsv, should_end)| {
                        if should_end != END {
                            Err(BinaryError {
                                kind: BinaryErrorKind::UnexpectedToken("hsv", should_end),
                                position: self.state.position,
                            })
                        } else {
                            Ok((BinaryParseEvent::Event(hsv), 22))
                        }
                    })
                    .unwrap_or_else(|| Ok((BinaryParseEvent::Eof, 0)))
            }
            _ => Ok((BinaryParseEvent::Event(BinaryEvent::Token(token_id)), 0)),
        }
    }

    fn read_event_inner<'a>(
        &mut self,
        input: &'a [u8],
    ) -> Result<(BinaryParseEvent<'a>, usize), BinaryError> {
        if input.len() < 2 {
            return Ok((BinaryParseEvent::Eof, 0));
        }

        let token_id = LE::read_u16(input);
        let mut read = std::mem::size_of::<u16>();
        let val_buf = &input[read..];

        match self.state.expectant {
            EventType::Key if token_id == END => {
                self.state.depth.pop().ok_or_else(|| BinaryError {
                    kind: BinaryErrorKind::EmptyStack,
                    position: self.state.position,
                })?;

                self.state.expectant = match self.current_depth()? {
                    DepthType::Array => EventType::Value,
                    DepthType::Object => EventType::Key,
                };

                Ok((BinaryParseEvent::Event(BinaryEvent::End), read))
            }
            EventType::Key if token_id == OPEN => {
                // empty objects -- prepare to skip!
                match val_buf.get(..2).map(LE::read_u16) {
                    Some(x) if x == END => {
                        read += std::mem::size_of::<u16>();
                        let (next_event, next_read) = self.read_event_inner(&input[read..])?;
                        if next_event == BinaryParseEvent::Eof {
                            Ok((next_event, 0))
                        } else {
                            Ok((next_event, next_read + read))
                        }
                    }
                    Some(x) => Err(BinaryError {
                        kind: BinaryErrorKind::UnexpectedToken("end", x),
                        position: self.state.position + read,
                    }),
                    None => Ok((BinaryParseEvent::Eof, 0)),
                }
            }
            EventType::Key => match self.next_value(&input[2..], token_id)? {
                (event, next_read) if event != BinaryParseEvent::Eof => {
                    self.state.expectant = EventType::Operator;
                    Ok((event, next_read + read))
                }
                x => Ok(x),
            },
            EventType::Operator if token_id == EQUAL => {
                self.state.expectant = EventType::Value;
                Ok((
                    BinaryParseEvent::Event(BinaryEvent::Operator(Operator::Equal)),
                    read,
                ))
            }

            // An operator doesn't have to be an equal sign! Yes, you're correct. Sometimes there
            // is not operator. See the lovely "map_area_data" from eu4 save games. There is no
            // equal sign. It just jumps right to "{brittany_area=", so we do our best.
            EventType::Operator if token_id == OPEN => self
                .peek_inner_value(&input[read..])
                .map(|(evt, _)| (evt, read)),

            EventType::Operator => Err(BinaryError {
                kind: BinaryErrorKind::UnexpectedToken("operator", token_id),
                position: self.state.position,
            }),

            EventType::Value => {
                match self.next_value(&input[read..], token_id)? {
                    (event, next_read) if event != BinaryParseEvent::Eof => {
                        // After reading a value we need to know if we're in a array (so we're
                        // looking for another value or if we're in an object
                        self.state.expectant = match self.current_depth()? {
                            DepthType::Array => EventType::Value,
                            DepthType::Object => EventType::Key,
                        };

                        Ok((event, next_read + read))
                    }
                    x => Ok(x),
                }
            }
        }
    }

    pub fn read_event<'a>(
        &mut self,
        input: &'a [u8],
    ) -> Result<(BinaryParseEvent<'a>, usize), BinaryError> {
        match self.read_event_inner(input)? {
            (next_event, read) if next_event != BinaryParseEvent::Eof => {
                self.state.position += read;
                Ok((next_event, read))
            }
            x => Ok(x),
        }
    }

    /// Creates an iterator that will output events spanning the entire given input
    pub fn events<'a, 'b: 'a>(&'a mut self, input: &'b [u8]) -> BinaryEventsIter<'a, 'b> {
        BinaryEventsIter {
            parser: self,
            has_failed: false,
            input,
        }
    }
}

/// An iterator over borrowed binary events
///
/// Created from `BinaryParser::events`
#[derive(Debug, PartialEq)]
pub struct BinaryEventsIter<'a, 'b: 'a> {
    parser: &'a mut BinaryParser,
    input: &'b [u8],
    has_failed: bool,
}

impl<'a, 'b: 'a> Iterator for BinaryEventsIter<'a, 'b> {
    type Item = Result<BinaryEvent<'b>, BinaryError>;

    fn next(&mut self) -> Option<Result<BinaryEvent<'b>, BinaryError>> {
        if self.has_failed {
            return None;
        }

        let event = self
            .parser
            .read_event(&self.input[self.parser.state.position..]);

        match event {
            Err(x) => {
                self.has_failed = true;
                Some(Err(x))
            }
            Ok((BinaryParseEvent::Eof, _)) => None,
            Ok((BinaryParseEvent::Event(event), _)) => Some(Ok(event)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_false_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284c)
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_true_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284b)
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_i32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x0c, 0x00, 0x59, 0x00, 0x00, 0x00];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(89),
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_u32_event() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x14, 0x00, 0x59, 0x00, 0x00, 0x00];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::U32(89),
            ]
        );

        assert_eq!(parser.position(), data.len());
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
            let mut parser = BinaryParser::new();

            assert_eq!(
                parser
                    .events(&full_data)
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap(),
                vec![
                    BinaryEvent::Token(0x2d82),
                    BinaryEvent::Operator(Operator::Equal),
                    BinaryEvent::F32(*result),
                ]
            );
            assert_eq!(parser.position(), full_data.len());
        }
    }

    #[test]
    fn test_q16_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x67, 0x01, 0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Q16(1.78732),
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_string1_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Text(Scalar::new(b"ENG")),
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_string2_event() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x17, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47,
        ];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Text(Scalar::new(b"ENG")),
            ]
        );

        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_multiple_top_level_events() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];
        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2d82),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284b),
                BinaryEvent::Token(0x284d),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284c),
            ]
        );

        assert_eq!(parser.position(), data.len());
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

        let mut parser = BinaryParser::new();

        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2ee1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::StartArray,
                BinaryEvent::Text(Scalar::new(b"Art of War")),
                BinaryEvent::Text(Scalar::new(b"Conquest of Paradise")),
                BinaryEvent::Text(Scalar::new(b"Res Publica")),
                BinaryEvent::Text(Scalar::new(b"Wealth of Nations")),
                BinaryEvent::End,
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_nested_object() {
        let data = [
            0xc9, 0x2e, 0x01, 0x00, 0x03, 0x00, 0xe2, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x01, 0x00,
            0x00, 0x00, 0xe3, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x0b, 0x00, 0x00, 0x00, 0xc7, 0x2e,
            0x01, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x00, 0x00, 0xc8, 0x2e, 0x01, 0x00, 0x0c, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2ec9),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::StartObject,
                BinaryEvent::Token(0x28e2),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(1),
                BinaryEvent::Token(0x28e3),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(11),
                BinaryEvent::Token(0x2ec7),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(4),
                BinaryEvent::Token(0x2ec8),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(0),
                BinaryEvent::End,
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_numerical_identifiers() {
        let data = [
            0x0c, 0x00, 0x59, 0x00, 0x00, 0x00, 0x01, 0x00, 0x0c, 0x00, 0x1e, 0x00, 0x00, 0x00,
        ];

        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::I32(89),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::I32(30),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_token_value() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28];
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x00e1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x28be),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_string_keys() {
        let mut data = vec![0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00];
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x0b, 0x00]);
        data.extend_from_slice(b"1444.11.11\n");
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x29cc),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::StartObject,
                BinaryEvent::Text(Scalar::new(b"schools_initiated")),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Text(Scalar::new(b"1444.11.11\n")),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_no_equal_object() {
        let data = [
            0xf1, 0x36, 0x03, 0x00, 0xe1, 0x00, 0x01, 0x00, 0xbe, 0x28, 0x04, 0x00,
        ];
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x36f1),
                BinaryEvent::StartObject,
                BinaryEvent::Token(0x00e1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x28be),
                BinaryEvent::End,
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_empty_array() {
        let data = [0xe1, 0x00, 0x01, 0x00, 0x03, 0x00, 0x04, 0x00];
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x00e1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::StartArray,
                BinaryEvent::End,
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_empty_objects() {
        let mut data = vec![0x63, 0x28, 0x01, 0x00, 0x17, 0x00, 0x07, 0x00];
        data.extend_from_slice(b"western");
        data.extend_from_slice(&[
            0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00,
        ]);
        data.extend_from_slice(&[0x0f, 0x00, 0x08, 0x00]);
        data.extend_from_slice(b"1451.5.2");
        data.extend_from_slice(&[0x01, 0x00]);
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2863),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Text(Scalar::new(b"western")),
                BinaryEvent::Text(Scalar::new(b"1451.5.2")),
                BinaryEvent::Operator(Operator::Equal),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_array_of_objects() {
        let data = [
            0x63, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28,
            0x04, 0x00, 0x03, 0x00, 0xf1, 0x36, 0x01, 0x00, 0x4b, 0x28, 0x04, 0x00, 0x04, 0x00,
        ];

        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x2863),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::StartArray,
                BinaryEvent::StartObject,
                BinaryEvent::Token(0x36f1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284b),
                BinaryEvent::End,
                BinaryEvent::StartObject,
                BinaryEvent::Token(0x36f1),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Token(0x284b),
                BinaryEvent::End,
                BinaryEvent::End,
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_hsv() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x053a),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::Hsv(110, 27, 27),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_u64() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x9c, 0x02, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        let mut parser = BinaryParser::new();
        assert_eq!(
            parser.events(&data).collect::<Result<Vec<_>, _>>().unwrap(),
            vec![
                BinaryEvent::Token(0x326b),
                BinaryEvent::Operator(Operator::Equal),
                BinaryEvent::U64(128),
            ]
        );
        assert_eq!(parser.position(), data.len());
    }

    #[test]
    fn test_iterator_on_failure() {
        let data = [0u8; 100];
        let mut parser = BinaryParser::new();
        let mut events = parser.events(&data);

        assert_eq!(events.next().unwrap().unwrap(), BinaryEvent::Token(0));
        assert!(events.next().unwrap().is_err());
        assert!(events.next().is_none());
    }
}
