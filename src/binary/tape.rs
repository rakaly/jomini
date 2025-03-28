use super::{
    lexer::{
        read_bool, read_f32, read_f64, read_i32, read_i64, read_id, read_rgb, read_string,
        read_u32, read_u64,
    },
    LexError, LexemeId,
};
use crate::{binary::Rgb, copyless::VecHelper, util::get_split, Error, ErrorKind, Scalar};

/// Represents any valid binary value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryToken<'a> {
    /// Index of the `BinaryToken::End` that signifies this array's termination
    Array(usize),

    /// Index of the `BinaryToken::End` that signifies this object's termination
    ///
    /// A key's value immediately follows the key.
    Object(usize),

    /// Denotes the start of where a homogenous object or array becomes
    /// heterogenous.
    MixedContainer,

    /// An equal operator inside the mixed container
    Equal,

    /// Index of the start of this object
    End(usize),

    /// Represents a binary boolean.
    Bool(bool),

    /// Represents a binary unsigned 32bit integer
    U32(u32),

    /// Represents a binary unsigned 64bit integer
    U64(u64),

    /// Represents a binary signed 64bit integer
    I64(i64),

    /// Represents a binary signed 32bit integer
    I32(i32),

    /// Represents a binary encoded quoted string
    Quoted(Scalar<'a>),

    /// Represents a binary encoded quoted string
    Unquoted(Scalar<'a>),

    /// Represents the first binary encoding for representing a rational number
    F32([u8; 4]),

    /// Represents the second binary encoding for representing a rational number
    F64([u8; 8]),

    /// Represents a 16bit token key that can be resolved to an equivalent textual representation.
    Token(u16),

    /// Represents the index of the encoded rgb value
    Rgb(Rgb),
}

/// Customizes how the binary tape is parsed from data
#[derive(Debug)]
pub struct BinaryTapeParser;

impl BinaryTapeParser {
    /// Parse the binary format return the data tape
    pub fn parse_slice(self, data: &[u8]) -> Result<BinaryTape, Error> {
        let mut res = BinaryTape::default();
        self.parse_slice_into_tape(data, &mut res)?;
        Ok(res)
    }

    /// Parse the binary format into the given tape.
    pub fn parse_slice_into_tape<'a>(
        self,
        data: &'a [u8],
        tape: &mut BinaryTape<'a>,
    ) -> Result<(), Error> {
        self.parse_slice_into_tape_core::<true>(data, tape)
    }

    #[cfg(unoptimized_build)]
    pub fn parse_slice_into_tape_unoptimized<'a>(
        self,
        data: &'a [u8],
        tape: &mut BinaryTape<'a>,
    ) -> Result<(), Error> {
        self.parse_slice_into_tape_core::<false>(data, tape)
    }

    fn parse_slice_into_tape_core<'a, const ENABLE_OPTIMIZATION: bool>(
        self,
        data: &'a [u8],
        tape: &mut BinaryTape<'a>,
    ) -> Result<(), Error> {
        let token_tape = &mut tape.token_tape;
        token_tape.clear();

        token_tape.reserve((data.len() / 5).max(10));
        // Write to the first element so we can assume it is initialized
        unsafe { token_tape.as_mut_ptr().write(BinaryToken::Equal) };

        let mut state = ParserState {
            data,
            original_length: data.len(),
            token_tape,
        };

        state.parse::<ENABLE_OPTIMIZATION>()?;
        Ok(())
    }
}

struct ParserState<'a, 'b> {
    data: &'a [u8],
    original_length: usize,
    token_tape: &'b mut Vec<BinaryToken<'a>>,
}

/// Enum where the next state is computed by multiplying the current state by 2.
#[derive(Debug, PartialEq, Copy, Clone, Eq)]
#[repr(u8)]
enum ParseState {
    ArrayValue = 0,

    // Whether the current parent container is both an object and array.
    // Unlike the text format, we only support one level of a mixed
    // container for performance.
    ArrayValueMixed = 2,

    ObjectValue = 4,
    Key = 8,
    KeyValueSeparator = 16,

    // Need to convert current object into an array
    ObjectToArray = 32,

    // Start of a container, TBD if array or object
    OpenFirst = 64,

    // First scalar read after container start, still TBD if array or object
    OpenSecond = 128,
}

impl<'a> ParserState<'a, '_> {
    fn offset(&self, data: &[u8]) -> usize {
        self.original_length - data.len()
    }

    #[inline]
    fn parse_next_id_opt(&self, data: &'a [u8]) -> Option<(&'a [u8], u16)> {
        get_split::<2>(data).map(|(head, rest)| (rest, u16::from_le_bytes(*head)))
    }

    #[inline]
    fn parse_next_id(&self, data: &'a [u8]) -> Result<(&'a [u8], LexemeId), Error> {
        read_id(data)
            .map(|(id, rest)| (rest, id))
            .map_err(|e| self.err_position(e, data))
    }

    #[inline]
    fn parse_u32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_u32(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::U32(result));
        Ok(rest)
    }

    #[inline]
    fn parse_u64(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_u64(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::U64(result));
        Ok(rest)
    }

    #[inline]
    fn parse_i64(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_i64(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::I64(result));
        Ok(rest)
    }

    #[inline]
    fn parse_i32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_i32(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::I32(result));
        Ok(rest)
    }

    #[inline]
    fn parse_f32(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_f32(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::F32(*result));
        Ok(rest)
    }

    #[inline]
    fn parse_f64(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_f64(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::F64(*result));
        Ok(rest)
    }

    #[inline]
    fn parse_bool(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_bool(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::Bool(result));
        Ok(rest)
    }

    fn parse_rgb(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (result, rest) = read_rgb(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::Rgb(result));
        Ok(rest)
    }

    #[inline(always)]
    fn parse_quoted_string(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (scalar, rest) = read_string(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::Quoted(scalar));
        Ok(rest)
    }

    #[inline(always)]
    fn parse_unquoted_string(&mut self, data: &'a [u8]) -> Result<&'a [u8], Error> {
        let (scalar, rest) = read_string(data).map_err(|e| self.err_position(e, data))?;
        self.token_tape.alloc().init(BinaryToken::Unquoted(scalar));
        Ok(rest)
    }

    #[inline(always)]
    fn next_state(state: ParseState) -> ParseState {
        let num = state as u8;
        let offset = num & 2;
        let next = num.wrapping_mul(2) - offset;
        unsafe { core::mem::transmute(next) }
    }

    #[inline]
    unsafe fn set_parent_to_object(&mut self, parent_ind: usize) {
        let x = self.token_tape.get_unchecked_mut(parent_ind);
        if let BinaryToken::Array(end) = x {
            *x = BinaryToken::Object(*end)
        } else {
            debug_assert!(false, "expected an array to be present");
            core::hint::unreachable_unchecked();
        }
    }

    fn parse<const ENABLE_OPTIMIZATION: bool>(&mut self) -> Result<(), Error> {
        use super::LexemeId as L;

        let mut data = self.data;
        let mut state = ParseState::Key;

        // tape index of the parent container
        let mut parent_ind = 0;

        macro_rules! push_end {
            () => {
                let end_idx = self.token_tape.len();
                match self.token_tape.get_mut(parent_ind) {
                    Some(BinaryToken::Array(end) | BinaryToken::Object(end)) => {
                        let grand_ind = *end;
                        *end = end_idx;
                        let val = BinaryToken::End(parent_ind);
                        self.token_tape.alloc().init(val);
                        parent_ind = grand_ind;
                        state = match unsafe { self.token_tape.get_unchecked(grand_ind) } {
                            BinaryToken::Array(_) => ParseState::ArrayValue,
                            _ => ParseState::Key,
                        }
                    }
                    _ => return Err(self.end_location_error(data)),
                }
            };
        }

        'outer: while let Some((mut d, token_id)) = self.parse_next_id_opt(data) {
            let mut token_id = LexemeId(token_id);

            // This conditional is purely an optimization to parse an entire
            // <key> = <value> in one iteration of the loop, and can be removed
            // or ignored to ease understanding. See PR #111 for a breakdown on
            // field and value frequency.
            if ENABLE_OPTIMIZATION && state == ParseState::Key {
                if token_id > L::UNQUOTED || token_id == L(0xb) {
                    // 65-90% of keys are tokens
                    // 5% of these keys are id (0xb)
                    if token_id != L::F64 && token_id != L::U64 {
                        self.token_tape.alloc().init(BinaryToken::Token(token_id.0));

                        let (d2, token_id2) = self.parse_next_id(d)?;
                        if token_id2 == L::EQUAL {
                            let (d3, token_id3) = self.parse_next_id(d2)?;
                            if token_id3 == L::I32 {
                                data = self.parse_i32(d3)?;
                                continue;
                            } else if token_id3 == L::OPEN {
                                // We could be looking at a primitive array
                                // so we should attempt to parse it in one go
                                let ind = self.token_tape.len();
                                self.token_tape.alloc().init(BinaryToken::Array(parent_ind));
                                parent_ind = ind;
                                let (d4, token_id4) = self.parse_next_id(d3)?;

                                macro_rules! parse_array_field {
                                    ($fn:ident, $token:expr) => {
                                        let d4 = self.$fn(d4)?;
                                        let (d5, token_id5) = self.parse_next_id(d4)?;

                                        if token_id5 == $token {
                                            let mut nd = self.$fn(d5)?;
                                            loop {
                                                let (nd2, x) = self.parse_next_id(nd)?;
                                                if x == $token {
                                                    nd = self.$fn(nd2)?;
                                                } else if x == L::CLOSE {
                                                    data = nd2;
                                                    let end_idx = self.token_tape.len();
                                                    match unsafe {
                                                        self.token_tape
                                                            .get_unchecked_mut(parent_ind)
                                                    } {
                                                        BinaryToken::Array(end) => {
                                                            let grand_ind = *end;
                                                            *end = end_idx;
                                                            let val = BinaryToken::End(parent_ind);
                                                            self.token_tape.alloc().init(val);
                                                            parent_ind = grand_ind;
                                                            continue 'outer;
                                                        }
                                                        _ => unsafe {
                                                            core::hint::unreachable_unchecked()
                                                        },
                                                    }
                                                } else {
                                                    d = nd2;
                                                    token_id = x;
                                                    state = ParseState::ArrayValue;
                                                    break;
                                                }
                                            }
                                        } else {
                                            d = d5;
                                            token_id = token_id5;
                                            state = ParseState::OpenSecond;
                                        }
                                    };
                                }

                                // These three array types cover 99.6% of EU4 arrays
                                if token_id4 == L::I32 {
                                    parse_array_field!(parse_i32, L::I32);
                                } else if token_id4 == L::QUOTED {
                                    parse_array_field!(parse_quoted_string, L::QUOTED);
                                } else if token_id4 == L::F32 {
                                    parse_array_field!(parse_f32, L::F32);
                                } else if (token_id4 > L::UNQUOTED
                                    && token_id4 != L::F64
                                    && token_id4 != L::U64)
                                    || token_id4 == L(0xb)
                                {
                                    self.token_tape
                                        .alloc()
                                        .init(BinaryToken::Token(token_id4.0));
                                    let (d4, token_id4) = self.parse_next_id(d4)?;
                                    if token_id4 == L::EQUAL {
                                        unsafe { self.set_parent_to_object(parent_ind) };
                                        state = ParseState::ObjectValue;
                                        (d, token_id) = self.parse_next_id(d4)?;
                                    } else {
                                        d = d4;
                                        token_id = token_id4;
                                        state = ParseState::OpenSecond;
                                    }
                                } else {
                                    d = d4;
                                    token_id = token_id4;
                                    state = ParseState::OpenFirst;
                                }
                            } else if token_id3 == L::QUOTED {
                                data = self.parse_quoted_string(d3)?;
                                continue;
                            } else if token_id3 == L::F32 {
                                data = self.parse_f32(d3)?;
                                continue;
                            } else {
                                d = d3;
                                token_id = token_id3;
                                state = ParseState::ObjectValue;
                            }
                        } else {
                            d = d2;
                            token_id = token_id2;
                            state = ParseState::KeyValueSeparator;
                        }
                    }
                } else if token_id == L::CLOSE {
                    push_end!();
                    data = d;
                    continue;
                } else if token_id == L::QUOTED {
                    // over 20% of EU4 object keys are quoted strings and they
                    // nearly always are objects
                    let d2 = self.parse_quoted_string(d)?;
                    let (d3, token_id2) = self.parse_next_id(d2)?;
                    if token_id2 == L::EQUAL {
                        let (d4, token_id3) = self.parse_next_id(d3)?;

                        if token_id3 == L::OPEN {
                            let ind = self.token_tape.len();
                            self.token_tape.alloc().init(BinaryToken::Array(parent_ind));
                            parent_ind = ind;
                            state = ParseState::OpenFirst;
                            (d, token_id) = self.parse_next_id(d4)?;

                            // Expect an object that follows a quoted string to start with a token
                            if token_id > L::UNQUOTED && token_id != L::F64 && token_id != L::U64 {
                                self.token_tape.alloc().init(BinaryToken::Token(token_id.0));
                                (d, token_id) = self.parse_next_id(d)?;
                                if token_id == L::EQUAL {
                                    unsafe { self.set_parent_to_object(parent_ind) };
                                    state = ParseState::ObjectValue;
                                    (d, token_id) = self.parse_next_id(d)?;
                                    if token_id == L::BOOL {
                                        data = self.parse_bool(d)?;
                                        state = ParseState::Key;
                                        continue;
                                    } else if token_id == L::QUOTED {
                                        data = self.parse_quoted_string(d)?;
                                        state = ParseState::Key;
                                        continue;
                                    }
                                } else {
                                    state = ParseState::OpenSecond;
                                }
                            }
                        } else {
                            d = d4;
                            token_id = token_id3;
                            state = ParseState::ObjectValue;
                        }
                    } else {
                        d = d3;
                        token_id = token_id2;
                        state = ParseState::KeyValueSeparator;
                    }
                } else if token_id == L::I32 {
                    // 8% of Vic3 and EU4 object keys are i32
                    // 96% of i32 keys have an i32 value
                    let d2 = self.parse_i32(d)?;
                    let (d3, token_id2) = self.parse_next_id(d2)?;
                    if token_id2 == L::EQUAL {
                        let (d4, token_id3) = self.parse_next_id(d3)?;

                        if token_id3 == L::I32 {
                            data = self.parse_i32(d4)?;
                            continue;
                        } else {
                            d = d4;
                            token_id = token_id3;
                            state = ParseState::ObjectValue;
                        }
                    } else {
                        d = d3;
                        token_id = token_id2;
                        state = ParseState::KeyValueSeparator;
                    }
                }
            }

            if state == ParseState::ObjectToArray {
                // This branch should never be taken on any save. It represents
                // `area = {a=b 10 20}`, which only happens in the text format.
                // Instead of trapping it and erroring, we just switch to mixed
                // mode like we do for the text format.
                state = ParseState::ArrayValueMixed;
                self.mixed_insert2();
            }

            match token_id {
                L::U32 => {
                    data = self.parse_u32(d)?;
                    state = Self::next_state(state);
                }
                L::U64 => {
                    data = self.parse_u64(d)?;
                    state = Self::next_state(state);
                }
                L::I32 => {
                    data = self.parse_i32(d)?;
                    state = Self::next_state(state);

                    if ENABLE_OPTIMIZATION && state == ParseState::ArrayValue {
                        let mut nd = data;
                        loop {
                            let (nd2, x) = self.parse_next_id(nd)?;
                            if x == L::I32 {
                                nd = self.parse_i32(nd2)?;
                            } else if x == L::CLOSE {
                                push_end!();
                                data = nd2;
                                break;
                            } else {
                                state = ParseState::ArrayValue;
                                data = nd;
                                break;
                            }
                        }
                    }
                }
                L::BOOL => {
                    data = self.parse_bool(d)?;
                    state = Self::next_state(state);
                }
                L::QUOTED => {
                    data = self.parse_quoted_string(d)?;
                    state = Self::next_state(state);
                }
                L::UNQUOTED => {
                    data = self.parse_unquoted_string(d)?;
                    state = Self::next_state(state);
                }
                L::F32 => {
                    data = self.parse_f32(d)?;
                    state = Self::next_state(state);
                }
                L::F64 => {
                    data = self.parse_f64(d)?;
                    state = Self::next_state(state);
                }

                L::OPEN => {
                    if state != ParseState::Key {
                        let ind = self.token_tape.len();
                        self.token_tape.alloc().init(BinaryToken::Array(parent_ind));
                        parent_ind = ind;
                        state = ParseState::OpenFirst;
                        data = d;
                    } else if self.token_tape.is_empty() {
                        return Err(self.open_empty_err(data));
                    } else {
                        // Skip empty containers if they occur in the key
                        // position eg: `a={b=c {} d=1}`. These occur in every
                        // EU4 save, even in 1.34.
                        match self.parse_next_id(d)? {
                            (nd, L::CLOSE) => data = nd,
                            _ => return Err(self.empty_object_err(data)),
                        }
                    }
                }
                L::CLOSE => {
                    match state {
                        ParseState::KeyValueSeparator => {
                            // `a={b=c 10}`
                            self.mixed_insert1();
                        }
                        ParseState::ObjectValue => {
                            // err on `a={b=}`
                            return Err(self.end_valid_err(data));
                        }
                        _ => {}
                    }

                    // Update the container token with now discovered location
                    // of when the container ends
                    push_end!();
                    data = d;
                }
                L::EQUAL => {
                    data = d;
                    if state == ParseState::KeyValueSeparator {
                        state = ParseState::ObjectValue;
                        continue; // prevents turning this into jump table
                    } else if state == ParseState::OpenSecond {
                        unsafe { self.set_parent_to_object(parent_ind) }
                        state = ParseState::ObjectValue;
                    } else if state == ParseState::ArrayValueMixed {
                        self.token_tape.alloc().init(BinaryToken::Equal);
                    } else if state == ParseState::ArrayValue {
                        self.token_tape.reserve(2);
                        let last = unsafe { self.token_tape.pop().unwrap_unchecked() };
                        if matches!(last, BinaryToken::Array(_) | BinaryToken::End(_)) {
                            return Err(self.equal_in_array_err(data));
                        }

                        // before we enter mixed mode, let's make sure we're not
                        // proceeded by only empty objects eg: `a={{} {} c=d}`. Only
                        // found in a smattering of EU4 saves
                        let mut pairs = self
                            .token_tape
                            .get(parent_ind + 1..)
                            .unwrap_or_default()
                            .chunks_exact(2);

                        let only_empties = pairs.len() > 0
                            && pairs.all(|tokens| {
                                matches!(tokens, [BinaryToken::Array(x), BinaryToken::End(y)] if *x == y + 1)
                            });

                        let ptr = self.token_tape.as_mut_ptr();
                        if only_empties {
                            unsafe { self.set_parent_to_object(parent_ind) }
                            unsafe { ptr.add(parent_ind + 1).write(last) };
                            unsafe { self.token_tape.set_len(parent_ind + 2) }
                            state = ParseState::ObjectValue;
                        } else {
                            let len = self.token_tape.len();
                            unsafe { ptr.add(len).write(BinaryToken::MixedContainer) };
                            unsafe { ptr.add(len + 1).write(last) };
                            unsafe { ptr.add(len + 2).write(BinaryToken::Equal) };
                            unsafe { self.token_tape.set_len(len + 3) }
                            state = ParseState::ArrayValueMixed;
                        }
                    } else {
                        return Err(self.equal_key_error(data));
                    }
                }
                L::RGB if state == ParseState::ObjectValue => {
                    data = self.parse_rgb(d)?;
                    state = ParseState::Key;
                }
                L::I64 => {
                    data = self.parse_i64(d)?;
                    state = Self::next_state(state);
                }
                x => {
                    data = d;
                    self.token_tape.alloc().init(BinaryToken::Token(x.0));
                    state = Self::next_state(state);
                }
            }
        }

        if parent_ind == 0 && state == ParseState::Key {
            Ok(())
        } else {
            Err(Error::eof())
        }
    }

    #[inline(never)]
    #[cold]
    fn mixed_insert2(&mut self) {
        let stashed1 = match self.token_tape.pop() {
            Some(x) => x,
            None => {
                debug_assert!(false, "empty token tape");
                return;
            }
        };

        let stashed2 = match self.token_tape.pop() {
            Some(x) => x,
            None => {
                debug_assert!(false, "empty token tape");
                return;
            }
        };

        self.token_tape.alloc().init(BinaryToken::MixedContainer);
        self.token_tape.alloc().init(stashed2);
        self.token_tape.alloc().init(stashed1);
    }

    #[inline(never)]
    fn mixed_insert1(&mut self) {
        let stashed1 = match self.token_tape.pop() {
            Some(x) => x,
            None => {
                debug_assert!(false, "empty token tape");
                return;
            }
        };

        self.token_tape.alloc().init(BinaryToken::MixedContainer);
        self.token_tape.alloc().init(stashed1);
    }

    #[inline]
    fn err_position(&self, err: LexError, data: &[u8]) -> Error {
        match err {
            LexError::Eof => Error::eof(),
            LexError::InvalidRgb => Error::invalid_syntax("invalid rgb", self.offset(data)),
        }
    }

    #[inline(never)]
    #[cold]
    fn equal_key_error(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("EQUAL not valid for a key"),
            offset: self.offset(data),
        })
    }

    #[inline(never)]
    #[cold]
    fn end_location_error(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("END token must be accompanies by an array or object token"),
            offset: self.offset(data),
        })
    }

    #[inline(never)]
    #[cold]
    fn empty_object_err(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("expected an empty object"),
            offset: self.offset(data),
        })
    }

    #[inline(never)]
    #[cold]
    fn equal_in_array_err(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("EQUAL in an array should come after a scalar"),
            offset: self.offset(data),
        })
    }

    #[inline(never)]
    #[cold]
    fn open_empty_err(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("Starting open token is not valid"),
            offset: self.offset(data),
        })
    }

    #[inline(never)]
    #[cold]
    fn end_valid_err(&self, data: &[u8]) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("END only valid for object or array"),
            offset: self.offset(data),
        })
    }
}

/// Houses the tape of tokens that is extracted from binary data
#[derive(Debug, Default)]
pub struct BinaryTape<'a> {
    token_tape: Vec<BinaryToken<'a>>,
}

impl<'a> BinaryTape<'a> {
    /// Creates an empty tape
    pub fn new() -> Self {
        BinaryTape::default()
    }

    /// Convenience method for creating a binary parser and parsing the given input
    pub fn from_slice(data: &[u8]) -> Result<BinaryTape<'_>, Error> {
        BinaryTapeParser.parse_slice(data)
    }

    /// Return the parsed tokens
    pub fn tokens(&self) -> &[BinaryToken<'a>] {
        self.token_tape.as_slice()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(data: &[u8]) -> Result<BinaryTape<'_>, Error> {
        BinaryTape::from_slice(data)
    }

    #[test]
    fn test_size_of_binary_token() {
        let token_size = std::mem::size_of::<BinaryToken>();
        assert!(token_size <= 24);
    }

    #[test]
    fn test_binary_tokens_dont_need_to_be_dropped() {
        assert!(!std::mem::needs_drop::<BinaryToken>());
    }

    #[test]
    fn test_parse_offset() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28, 0x01, 0x00, 0x4c, 0x28];
        let err = BinaryTape::from_slice(&data[..]).unwrap_err();
        let ErrorKind::InvalidSyntax { offset, .. } = err.kind() else {
            panic!("expected InvalidSyntax error");
        };
        assert_eq!(*offset, 8);

        let data2 = [0x82, 0x2d, 0x01, 0x00, 0x01, 0x00];
        let err = BinaryTape::from_slice(&data2[..]).unwrap_err();
        let ErrorKind::InvalidSyntax { offset, .. } = err.kind() else {
            panic!("expected InvalidSyntax error");
        };
        assert_eq!(*offset, 6);
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

        // let f32_results = [0.023, 0.041, 0.018, 0.542, 1.000, 3000.000];
        for bin in f32_data.iter() {
            let full_data = [base_data.clone(), bin.to_vec()].concat();

            assert_eq!(
                parse(&full_data[..]).unwrap().token_tape,
                vec![BinaryToken::Token(0x2d82), BinaryToken::F32(*bin),]
            );
        }
    }

    #[test]
    fn test_custom_float_event() {
        let base_data = vec![0x82, 0x2d, 0x01, 0x00, 0x0d, 0x00];
        let f32_data = [[0x8f, 0xc2, 0x75, 0x3e]];

        // let f32_results = [0.24f32];
        for bin in f32_data.iter() {
            let full_data = [base_data.clone(), bin.to_vec()].concat();

            assert_eq!(
                BinaryTape::from_slice(&full_data[..]).unwrap().tokens(),
                vec![BinaryToken::Token(0x2d82), BinaryToken::F32(*bin),]
            );
        }

        let base_data = vec![0x82, 0x2d, 0x01, 0x00, 0x67, 0x01];
        let q16_data = [
            [0xe2, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
            [0x5f, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
        ];

        // let f32_results = [1.25, 1.375];
        for bin in q16_data.iter() {
            let full_data = [base_data.clone(), bin.to_vec()].concat();

            assert_eq!(
                BinaryTape::from_slice(&full_data[..]).unwrap().tokens(),
                vec![BinaryToken::Token(0x2d82), BinaryToken::F64(*bin),]
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
            vec![
                BinaryToken::Token(0x2d82),
                BinaryToken::F64([0xc7, 0xe4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
            ]
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
                BinaryToken::Quoted(Scalar::new(b"ENG")),
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
                BinaryToken::Unquoted(Scalar::new(b"ENG")),
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
                BinaryToken::Quoted(Scalar::new(b"Art of War")),
                BinaryToken::Quoted(Scalar::new(b"Conquest of Paradise")),
                BinaryToken::Quoted(Scalar::new(b"Res Publica")),
                BinaryToken::Quoted(Scalar::new(b"Wealth of Nations")),
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
        data.extend_from_slice(&LexemeId::CLOSE.0.to_le_bytes());
        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x29cc),
                BinaryToken::Object(4),
                BinaryToken::Quoted(Scalar::new(b"schools_initiated")),
                BinaryToken::Quoted(Scalar::new(b"1444.11.11\n")),
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
    fn test_empty_objects_to_skip1() {
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
                BinaryToken::Unquoted(Scalar::new(b"western")),
                BinaryToken::Quoted(Scalar::new(b"1446.5.31")),
                BinaryToken::Token(0x2838),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects_to_skip2() {
        let data = [
            0x38, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x38, 0x29, 0x01, 0x00,
            0x03, 0x00, 0x38, 0x30, 0x04, 0x00, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2838),
                BinaryToken::Object(6),
                BinaryToken::Token(0x2938),
                BinaryToken::Array(5),
                BinaryToken::Token(0x3038),
                BinaryToken::End(3),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_empty_objects_to_skip3() {
        let data = [
            0x38, 0x28, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x04, 0x00, 0x03, 0x00, 0x04, 0x00,
            0x38, 0x29, 0x01, 0x00, 0x03, 0x00, 0x38, 0x30, 0x04, 0x00, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2838),
                BinaryToken::Object(6),
                BinaryToken::Token(0x2938),
                BinaryToken::Array(5),
                BinaryToken::Token(0x3038),
                BinaryToken::End(3),
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
                BinaryToken::Rgb(Rgb {
                    r: 110,
                    g: 27,
                    b: 27,
                    a: None
                })
            ]
        );
    }

    #[test]
    fn test_rgb_no_key() {
        let data = [0x43, 0x02, 0x01, 0x00, 0xbe, 0x28];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![BinaryToken::Token(0x0243), BinaryToken::Token(0x28be),]
        );
    }

    #[test]
    fn test_rgba() {
        let data = [
            0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00,
            0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00,
            0x1c, 0x00, 0x00, 0x00, 0x04, 0x00,
        ];

        let tape = parse(&data[..]).unwrap();
        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x053a),
                BinaryToken::Rgb(Rgb {
                    r: 110,
                    g: 27,
                    b: 27,
                    a: Some(28)
                })
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
    fn test_i64() {
        let data = [
            0x6b, 0x32, 0x01, 0x00, 0x17, 0x03, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        ];
        assert_eq!(
            parse(&data).unwrap().token_tape,
            vec![BinaryToken::Token(0x326b), BinaryToken::I64(-1),]
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
    fn test_mixed_container_1() {
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
                BinaryToken::Array(10),
                BinaryToken::I32(10),
                BinaryToken::MixedContainer,
                BinaryToken::I32(0),
                BinaryToken::Equal,
                BinaryToken::U32(2),
                BinaryToken::I32(1),
                BinaryToken::Equal,
                BinaryToken::U32(2),
                BinaryToken::End(1),
                BinaryToken::Token(0xaaaa),
                BinaryToken::Object(15),
                BinaryToken::Token(0xbbbb),
                BinaryToken::Token(0xcccc),
                BinaryToken::End(12),
            ]
        );
    }

    #[test]
    fn test_mixed_container_2() {
        let data = [
            0x1e, 0x3d, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x03, 0x00, 0x0b, 0x00, 0x01, 0x00,
            0x14, 0x00, 0x03, 0x00, 0x00, 0x00, 0xe1, 0x00, 0x01, 0x00, 0x14, 0x00, 0x69, 0x12,
            0x00, 0x00, 0x04, 0x00, 0x0c, 0x00, 0x07, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x3d1e),
                BinaryToken::Array(11),
                BinaryToken::Array(10),
                BinaryToken::Object(8),
                BinaryToken::Token(0x0b),
                BinaryToken::U32(3),
                BinaryToken::Token(0xe1),
                BinaryToken::U32(4713),
                BinaryToken::End(3),
                BinaryToken::I32(7),
                BinaryToken::End(2),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_mixed_container_3() {
        let data = [
            0x6f, 0x34, 0x01, 0x00, 0x03, 0x00, 0xbb, 0xbb, 0x01, 0x00, 0x0e, 0x00, 0x01, 0x0e,
            0x00, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x346f),
                BinaryToken::Object(6),
                BinaryToken::Token(0xbbbb),
                BinaryToken::Bool(true),
                BinaryToken::MixedContainer,
                BinaryToken::Bool(false),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_mixed_container_4() {
        let data = [
            0x6f, 0x34, 0x01, 0x00, 0x03, 0x00, 0xbb, 0xbb, 0x01, 0x00, 0x0e, 0x00, 0x01, 0x0e,
            0x00, 0x00, 0x0e, 0x00, 0x01, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x346f),
                BinaryToken::Object(7),
                BinaryToken::Token(0xbbbb),
                BinaryToken::Bool(true),
                BinaryToken::MixedContainer,
                BinaryToken::Bool(false),
                BinaryToken::Bool(true),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_mixed_container_5() {
        let data = [
            0x6f, 0x34, 0x01, 0x00, 0x03, 0x00, 0xbb, 0xbb, 0x01, 0x00, 0x0e, 0x00, 0x01, 0x0e,
            0x00, 0x00, 0x0e, 0x00, 0x01, 0x0e, 0x00, 0x00, 0x04, 0x00,
        ];

        assert_eq!(
            parse(&data[..]).unwrap().token_tape,
            vec![
                BinaryToken::Token(0x346f),
                BinaryToken::Object(8),
                BinaryToken::Token(0xbbbb),
                BinaryToken::Bool(true),
                BinaryToken::MixedContainer,
                BinaryToken::Bool(false),
                BinaryToken::Bool(true),
                BinaryToken::Bool(false),
                BinaryToken::End(1),
            ]
        );
    }

    #[test]
    fn test_binary_tape_parser() {
        let mut tape = BinaryTape::new();

        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        BinaryTapeParser
            .parse_slice_into_tape(&data[..], &mut tape)
            .unwrap();

        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2d82),
                BinaryToken::Token(0x284b),
                BinaryToken::Token(0x284d),
                BinaryToken::Token(0x284c),
            ]
        );

        let data2 = [
            0x83, 0x2d, 0x01, 0x00, 0x4c, 0x28, 0x4e, 0x28, 0x01, 0x00, 0x4d, 0x28,
        ];

        BinaryTapeParser
            .parse_slice_into_tape(&data2[..], &mut tape)
            .unwrap();

        assert_eq!(
            tape.token_tape,
            vec![
                BinaryToken::Token(0x2d83),
                BinaryToken::Token(0x284c),
                BinaryToken::Token(0x284e),
                BinaryToken::Token(0x284d),
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
    fn test_should_not_parse_on_eof() {
        let data = [65, 1, 3, 0, 3, 0, 3, 0, 4, 0];
        let res = parse(&data[..]);
        assert!(res.is_err());
    }

    #[test]
    fn test_should_not_parse_on_eof2() {
        let data = [65, 1, 3, 0, 3, 0, 3, 0, 4, 0, 4, 0];
        let res = parse(&data[..]);
        assert!(res.is_err());
    }

    #[test]
    fn test_should_not_parse_on_eof3() {
        let data = [12, 2, 3, 0, 3, 0, 3, 0, 1, 0, 0, 0, 4, 0];
        let res = parse(&data[..]);
        assert!(res.is_err());
    }

    #[test]
    fn test_hidden_object_needs_a_value() {
        let data = [160, 0, 3, 0, 3, 0, 4, 0, 1, 0, 0, 0, 4, 0];
        let res = parse(&data[..]);
        res.unwrap_err();
    }

    #[test]
    fn test_early_eof1() {
        let data = [0x42, 0x2a, 0x4, 0x0];
        let res = parse(&data[..]);
        res.unwrap_err();
    }

    #[test]
    fn test_early_eof2() {
        let data = [0xff, 0xff, 0xff, 0xff, 0x4, 0x0];
        let res = parse(&data[..]);
        res.unwrap_err();
    }

    #[test]
    fn test_early_eof3() {
        let data = [0x3, 0x0, 0xf, 0x3, 0x1, 0x0, 0x0, 0x0];
        let res = parse(&data[..]);
        res.unwrap_err();
    }

    #[test]
    fn test_initial_end_does_not_panic() {
        let data = [4, 0];
        let res = parse(&data[..]);
        assert!(res.is_ok() || res.is_err());
    }
}
