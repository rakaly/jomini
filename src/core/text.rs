#![allow(dead_code)]
#![allow(unused_variables)]

use crate::scalar::Scalar;
use byteorder::{ByteOrder, LE};

fn parse(data: &[u8]) {
    let mut tape = TextTape::default();
    tape.slurp_body(data);
}

#[derive(Debug, PartialEq)]
pub enum TextToken {
    Array(usize),
    Object(usize),
    Scalar(usize),
}

#[derive(Debug, Default)]
pub struct TextTape<'a> {
    pub token_tape: Vec<TextToken>,
    pub data_tape: Vec<Scalar<'a>>,
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

const fn aa() -> [u8; 256] {
    [0; 256]
}


static FJFJ: [u8; 256] = aa();

impl<'a> TextTape<'a> {
    fn skip_ws(&mut self, d: &'a [u8]) {
        let data = LE::read_u64(d);
    }


    fn slurp_body(&mut self, d: &'a [u8]) {

        while !d.is_empty() {

        }
    }
}

fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}


/*use crate::core::depth::{Depth, DepthType};
use crate::scalar::Scalar;
use crate::text::Operator;
use std::error;
use std::fmt::{self, Display, Formatter};
use std::ops::{Add, AddAssign};
use std::cmp::min;

/// The type of binary error that occurred
#[derive(Debug)]
pub enum TextErrorKind {
    /// When too many '}' are seen
    EmptyStack,

    /// When too many '{' are seen
    FullStack,
}

/// Error that occurred while parsing binary data with positional data
#[derive(Debug)]
pub struct TextError {
    pub kind: TextErrorKind,
    pub position: Position,
}

impl Display for TextError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.kind {
            TextErrorKind::EmptyStack => write!(f, "empty stack"),
            TextErrorKind::FullStack => write!(f, "full stack"),
        }?;

        write!(f, " {}", self.position)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub column: u32,
    pub line: u32,
    pub offset: usize,
}

impl Position {
    fn from_column(spaces: usize) -> Self {
        Position {
            line: 0,
            column: spaces as u32,
            offset: spaces,
        }
    }

    fn advance_column(&mut self, spaces: usize) {
        self.offset += spaces;
        self.column += spaces as u32;
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "(ln: {}, col: {}, offset: {})",
            self.line, self.column, self.offset
        )
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            column: self.column + other.column,
            line: self.line + other.line,
            offset: self.offset + other.offset,
        }
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}



#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TextEvent<'a> {
    StartArray,
    StartObject,
    End,
    Operator(Operator),
    Comment(Scalar<'a>),
    Quoted(Scalar<'a>),
    Value(Scalar<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum EventType {
    Key,
    Operator,
    Value,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TextParseEvent<'a> {
    /// The event that was parsed
    Event(TextEvent<'a>),

    /// The input did not have enough data to read the next event
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextParserState {
    position: Position,
    expectant: EventType,
    depth: Depth,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextParser {
    state: TextParserState,
}

impl TextParser {
    pub fn new() -> Self {
        let mut depth = Depth::new();
        let pushed = depth.push(DepthType::Object);
        debug_assert!(pushed);

        TextParser {
            state: TextParserState {
                expectant: EventType::Key,
                position: Position {
                    line: 1,
                    column: 1,
                    offset: 0,
                },
                depth,
            },
        }
    }

    fn skip_whitespace(input: &[u8]) -> Position {
        let mut position = Position::default();
        for (i, x) in input.iter().enumerate() {
            match x {
                b' ' | b'\r' | b'\t' => {
                    position.column += 1;
                    position.offset += 1;
                }
                b'\n' => {
                    position.column = 1;
                    position.line += 1;
                    position.offset += 1;
                }
                _ => break,
            }
        }

        position
    }

    fn consume_scalar<'a>(input: &'a [u8], is_final_block: bool) -> (TextParseEvent<'a>, Position) {
        let pos = input.iter().position(|&x| is_boundary(x));
        match pos {
            None => {
                if is_final_block {
                    let evt = TextEvent::Value(Scalar::new(input));
                    let position = Position::from_column(input.len());
                    (TextParseEvent::Event(evt), position)
                } else {
                    (TextParseEvent::Eof, Position::default())
                }
            }
            Some(idx) => {
                let evt = TextEvent::Value(Scalar::new(&input[..idx]));
                let position = Position::from_column(idx);
                (TextParseEvent::Event(evt), position)
            }
        }
    }

    fn consume_operator<'a>(
        input: &'a [u8],
        is_final_block: bool,
    ) -> Result<(TextParseEvent<'a>, Position), TextError> {
        // The only operator that begins with an equals is the equal operator
        // This is also a fast path as most (sometimes all) will be equal
        if input[0] == b'=' {
            return Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::Equal)),
                Position::from_column(1),
            ));
        }

        match input.get(..2).unwrap_or(input) {
            [b'>', b'='] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::GreaterEqual)),
                Position::from_column(2),
            )),
            [b'<', b'='] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::LesserEqual)),
                Position::from_column(2),
            )),
            [b'<', b'>'] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::LesserGreater)),
                Position::from_column(2),
            )),
            [b'!', b'='] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::NotEqual)),
                Position::from_column(2),
            )),
            [b'<', _] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::Lesser)),
                Position::from_column(1),
            )),
            [b'>', _] => Ok((
                TextParseEvent::Event(TextEvent::Operator(Operator::Greater)),
                Position::from_column(1),
            )),
            [a, b] => panic!("did not expect {} and {} operators", a, b),
            [x] => {
                if is_final_block {
                    Ok((TextParseEvent::Eof, Position::default()))
                } else {
                    panic!("early eof")
                }
            }
            x => unreachable!(),
        }
    }

    fn current_depth(&self) -> Result<DepthType, TextError> {
        self.state.depth.last().ok_or_else(|| TextError {
            kind: TextErrorKind::EmptyStack,
            position: self.state.position,
        })
    }

    fn read_event_inner<'a>(
        &mut self,
        input: &'a [u8],
        is_final_block: bool,
    ) -> Result<(TextParseEvent<'a>, Position), TextError> {
        let pos = TextParser::skip_whitespace(input);
        let input = &input[..pos.offset];
        if input.is_empty() {
            return Ok((TextParseEvent::Eof, Position::default()));
        }

        match input[0] {
            b'{' => {
                let total_pos = pos + Position::from_column(1);
                Ok((TextParseEvent::Event(TextEvent::End), total_pos))
            }
            b'=' | b'!' | b'<' | b'>' => {
                let (evt, new_pos) = TextParser::consume_operator(input, is_final_block)?;
                let total_pos = new_pos + pos;
                Ok((evt, total_pos))
            },
            _ => {
                let (evt, scalar_pos) = TextParser::consume_scalar(input, is_final_block);
                let total_pos = scalar_pos + pos;
                Ok((evt, total_pos))
            }
/*            b'}' => {
                self.advance_column(1);
                Some(TextEvent::End)
            }
            b'=' | b'!' | b'<' | b'>' => self.consume_operator(),
            b'#' => self.consume_comment().map(TextEvent::Comment),
            b'"' => self.consume_quoted().map(TextEvent::Quoted),
            _ => self.consume_scalar().map(TextEvent::Scalar),*/
        }

        /*        match self.state.expectant {
            EventType::Key if token == b'}' => {
                self.state.depth.pop().ok_or_else(|| TextError {
                    kind: TextErrorKind::EmptyStack,
                    position: self.state.position,
                })?;

                self.state.expectant = match self.current_depth()? {
                    DepthType::Array => EventType::Value,
                    DepthType::Object => EventType::Key,
                };

                Ok((TextParseEvent::Event(TextEvent::End), pos.offset))
            }
            _ => unimplemented!(),
        }*/
    }

    pub fn read_event<'a>(
        &mut self,
        input: &'a [u8],
        is_final_block: bool,
    ) -> Result<(TextParseEvent<'a>, usize), TextError> {
        match self.read_event_inner(input, is_final_block)? {
            (next_event, read) if next_event != TextParseEvent::Eof => {
                self.state.position += read;
                Ok((next_event, read.offset))
            }
            (next_event, read) => Ok((next_event, read.offset)),
        }
    }
}

fn is_operator(b: u8) -> bool {
    b == b'=' || b == b'!' || b == b'>' || b == b'<'
}

fn is_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\n' || b == b'\r'
}

fn is_boundary(b: u8) -> bool {
    is_whitespace(b) || b == b'{' || b == b'}' || is_operator(b)
}
*/
