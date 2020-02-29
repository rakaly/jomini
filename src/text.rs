use crate::scalar::Scalar;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Equal,
    Lesser,
    Greater,
    LesserEqual,
    GreaterEqual,
    LesserGreater,
    NotEqual,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TextEvent<'a> {
    Open,
    End,
    Operator(Operator),
    Comment(Scalar<'a>),
    Quoted(Scalar<'a>),
    Scalar(Scalar<'a>),
}

impl<'a> TextEvent<'a> {
    pub fn is_comment(&self) -> bool {
        match self {
            TextEvent::Comment(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct TextParserState {
    pub column: u32,
    pub line: u32,
    pub cursor_position: usize,
    pub final_block: bool,
}

#[derive(Debug, PartialEq)]
pub struct TextParser<'a> {
    data: &'a [u8],
    cursor: &'a [u8],
    state: TextParserState,
}

// API (tying parser to the lifetime of the data) inspired by pulldown-cmark and C#'s new json
// parser
impl<'a> TextParser<'a> {
    pub fn new(data: &'a [u8]) -> TextParser<'a> {
        let cursor = &data[..];
        TextParser {
            data,
            cursor,
            state: TextParserState {
                final_block: true,
                column: 0,
                line: 0,
                cursor_position: 0,
            },
        }
    }

    pub fn with_state(data: &'a [u8], state: TextParserState) -> TextParser<'a> {
        let cursor = &data[state.cursor_position..];
        TextParser {
            data,
            cursor,
            state,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.cursor.is_empty()
    }

    pub fn column(&self) -> u32 {
        self.state.column
    }

    pub fn line(&self) -> u32 {
        self.state.line
    }

    pub fn state(&self) -> TextParserState {
        self.state
    }

    fn increment_cursor_position(&mut self, bytes: usize) {
        self.state.cursor_position += bytes;
        self.cursor = &self.cursor[bytes..];
    }

    fn skip_whitespace(&mut self) {
        for (i, x) in self.cursor.iter().enumerate() {
            match x {
                b' ' | b'\r' | b'\t' => {
                    self.state.column += 1;
                }
                b'\n' => {
                    self.state.column = 0;
                    self.state.line += 1;
                }
                _ => {
                    self.increment_cursor_position(i);
                    return;
                }
            }
        }

        self.state.cursor_position = self.data.len();
        self.cursor = &self.data[self.state.cursor_position..];
    }

    fn advance_column(&mut self, spaces: usize) {
        self.increment_cursor_position(spaces);
        self.state.column += spaces as u32;
    }

    fn take_scalar(&mut self, end_idx: usize) -> Scalar<'a> {
        let result = Scalar::new(&self.cursor[..end_idx]);
        self.advance_column(end_idx);
        result
    }

    fn take_remaining(&mut self) -> Scalar<'a> {
        self.take_scalar(self.cursor.len())
    }

    fn consume_comment(&mut self) -> Option<Scalar<'a>> {
        match memchr::memchr2(b'\n', b'\r', self.cursor) {
            None if self.state.final_block => {
                self.advance_column(1);
                Some(self.take_remaining())
            }
            Some(idx) => {
                if self.cursor[idx] == b'\r'
                    && self.cursor.get(idx + 1).map_or(false, |&x| x == b'\n')
                {
                    self.state.line += 1;
                    self.state.column = 0;
                    let res = Some(Scalar::new(&self.cursor[1..idx]));
                    self.increment_cursor_position(idx + 2);
                    res
                } else if self.cursor[idx] == b'\n' {
                    self.state.line += 1;
                    self.state.column = 0;
                    let res = Some(Scalar::new(&self.cursor[1..idx]));
                    self.increment_cursor_position(idx + 1);
                    res
                } else {
                    self.advance_column(1);
                    let res = Some(self.take_scalar(idx - 1));
                    self.advance_column(1);
                    res
                }
            }
            None => None,
        }
    }

    fn consume_scalar(&mut self) -> Option<Scalar<'a>> {
        let pos = self.cursor.iter().position(|&x| is_boundary(x));
        match pos {
            None if self.state.final_block => Some(self.take_remaining()),
            None => None,
            Some(idx) => Some(self.take_scalar(idx)),
        }
    }

    fn consume_quoted(&mut self) -> Option<Scalar<'a>> {
        if let Some(idx) = memchr::memchr(b'"', &self.cursor[1..]) {
            let mut count = 0;
            let mut last = 0;
            for line in memchr::memchr_iter(b'\n', &self.cursor[1..=idx]) {
                count += 1;
                last = line;
            }

            if count == 0 {
                self.state.column += 2 + idx as u32;
            } else {
                self.state.column = (idx - last) as u32;
                self.state.line += count;
            }

            let result = Scalar::new(&self.cursor[1..=idx]);
            self.increment_cursor_position(idx + 1);
            Some(result)
        } else {
            None
        }
    }

    fn consume_operator(&mut self) -> Option<TextEvent<'a>> {
        let first = self.cursor[0];

        // The only operator that begins with an equals is the equal operator
        if first == b'=' {
            self.advance_column(1);
            Some(TextEvent::Operator(Operator::Equal))
        } else if self.cursor.len() >= 2 {
            let mut ops = &self.cursor[..2];
            match ops {
                [b'>', b'='] => {
                    self.advance_column(2);
                    Some(TextEvent::Operator(Operator::GreaterEqual))
                }
                [b'<', b'='] => {
                    self.advance_column(2);
                    Some(TextEvent::Operator(Operator::LesserEqual))
                }
                [b'<', b'>'] => {
                    self.advance_column(2);
                    Some(TextEvent::Operator(Operator::LesserGreater))
                }
                [b'!', b'='] => {
                    self.advance_column(2);
                    Some(TextEvent::Operator(Operator::NotEqual))
                }
                [b'<', _] => {
                    self.advance_column(1);
                    Some(TextEvent::Operator(Operator::Lesser))
                }
                [b'>', _] => {
                    self.advance_column(1);
                    Some(TextEvent::Operator(Operator::Greater))
                }

                // This should be exceedingly rare, so I'm not too worried about implementing
                // perfect logic rather than gracefully handling input and trying to recover
                [a, b] => {
                    if *a == b'\n' {
                        self.state.column = 0;
                        self.state.line += 1;
                        self.increment_cursor_position(1);
                        ops = &ops[1..];
                    } else {
                        self.advance_column(1);
                    }

                    if *b == b'\n' {
                        self.state.column = 0;
                        self.state.line += 1;
                        self.increment_cursor_position(1);
                        ops = &ops[1..];
                    } else {
                        self.advance_column(1);
                    }

                    Some(TextEvent::Scalar(Scalar::new(&ops)))
                }
                _ => unreachable!(),
            }
        } else if !self.state.final_block {
            None
        } else if first == b'>' {
            self.advance_column(1);
            Some(TextEvent::Operator(Operator::Greater))
        } else if first == b'<' {
            self.advance_column(1);
            Some(TextEvent::Operator(Operator::Lesser))
        } else {
            None
        }
    }
}

impl<'a> Iterator for TextParser<'a> {
    type Item = TextEvent<'a>;

    fn next(&mut self) -> Option<TextEvent<'a>> {
        self.skip_whitespace();

        if self.is_empty() {
            return None;
        }

        match self.cursor[0] {
            b'{' => {
                self.advance_column(1);
                Some(TextEvent::Open)
            }
            b'}' => {
                self.advance_column(1);
                Some(TextEvent::End)
            }
            b'=' | b'!' | b'<' | b'>' => self.consume_operator(),
            b'#' => self.consume_comment().map(TextEvent::Comment),
            b'"' => self.consume_quoted().map(TextEvent::Quoted),
            _ => self.consume_scalar().map(TextEvent::Scalar),
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
