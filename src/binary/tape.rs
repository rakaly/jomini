use crate::{
    binary::Rgb,
    copyless::VecHelper,
    util::{get_split, le_u32},
    Error, ErrorKind, Scalar,
};

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

const EQUAL: u16 = 0x0001;
const OPEN: u16 = 0x0003;
const END: u16 = 0x0004;
const I32: u16 = 0x000c;
const F32: u16 = 0x000d;
const BOOL: u16 = 0x000e;
const QUOTED_STRING: u16 = 0x000f;
const U32: u16 = 0x0014;
const UNQUOTED_STRING: u16 = 0x0017;

const U64: u16 = 0x029c;
const F64: u16 = 0x0167;
const RGB: u16 = 0x0243;

/// Customizes how the binary tape is parsed from data
#[derive(Debug)]
pub struct BinaryTapeParser;

impl BinaryTapeParser {
    /// Parse the binary format into the given tape.
    fn parse_slice_into_tape_full<'a, const IS_EU4: bool>(
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
            parent_ind: 0,
            state: ParseState::Key,
        };

        state.parse::<IS_EU4>()?;
        Ok(())
    }

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
        self.parse_slice_into_tape_full::<false>(data, tape)
    }

    /// Parse the binary format into the given tape.
    ///
    /// The data is assumed to have resemble the more simple EU4 binary format,
    /// which may allow for optimizations.
    pub fn parse_eu4_slice_into_tape<'a>(
        self,
        data: &'a [u8],
        tape: &mut BinaryTape<'a>,
    ) -> Result<(), Error> {
        self.parse_slice_into_tape_full::<true>(data, tape)
    }
}

struct ParserState<'a, 'b> {
    data: &'a [u8],
    token_tape: &'b mut Vec<BinaryToken<'a>>,

    // tape index of the parent container
    parent_ind: usize,
    state: ParseState,
    original_length: usize,
}

/// Enum where the next state is computed by multiplying the current state by 2.
#[derive(Debug, PartialEq, Copy, Clone, Eq)]
#[repr(u8)]
enum ParseState {
    ArrayValue = 0,

    // Whether the current parent container is both an object and array.
    // Unlike the text format, we only support one level of a mixed
    // container for performance.
    ArrayValueMixed = 1,

    ObjectValue = 2,
    Key = 3,
    KeyValueSeparator = 4,
}

const _: () = assert!(
    ParserState::next_state(ParseState::ArrayValue) == ParseState::ArrayValue as u8,
    "State transition incorrect"
);
const _: () = assert!(
    ParserState::next_state(ParseState::ArrayValueMixed) == ParseState::ArrayValueMixed as u8,
    "State transition incorrect"
);
const _: () = assert!(
    ParserState::next_state(ParseState::ObjectValue) == ParseState::Key as u8,
    "State transition incorrect"
);
const _: () = assert!(
    ParserState::next_state(ParseState::Key) == ParseState::KeyValueSeparator as u8,
    "State transition incorrect"
);

// will never give false positive
#[inline]
fn is_token<const IS_EU4: bool>(token_id: u16) -> bool {
    if IS_EU4 {
        // masks out F64
        (token_id & 0xFF7E) > UNQUOTED_STRING || token_id == 0xb
    } else {
        (token_id > UNQUOTED_STRING && matches!(token_id, F64 | RGB | U64)) || token_id == 0xb
    }
}

#[inline]
fn peek_token<'a>(data: &'a [u8]) -> Option<(&'a [u8], u16)> {
    get_split::<2>(data).map(|(head, rest)| (rest, u16::from_le_bytes(head)))
}

#[inline]
fn expect_token<'a>(data: &'a [u8]) -> Result<(&'a [u8], u16), Error> {
    peek_token(data).ok_or_else(Error::eof)
}

impl<'a, 'b> ParserState<'a, 'b> {
    fn offset(&self) -> usize {
        self.original_length - self.data.len()
    }

    #[inline]
    fn parse_token(&mut self) -> Result<u16, Error> {
        let (d, token_id) = peek_token(self.data).ok_or_else(Error::eof)?;
        self.data = d;
        Ok(token_id)
    }

    #[inline]
    fn parse_u32(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        let val = u32::from_le_bytes(head);
        self.token_tape.alloc().init(BinaryToken::U32(val));
        self.data = rest;
        Ok(())
    }

    #[inline]
    fn parse_u64(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<8>(self.data).ok_or_else(Error::eof)?;
        let val = u64::from_le_bytes(head);
        self.token_tape.alloc().init(BinaryToken::U64(val));
        self.data = rest;
        Ok(())
    }

    #[inline]
    fn parse_i32(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        let val = i32::from_le_bytes(head);
        self.token_tape.alloc().init(BinaryToken::I32(val));
        self.data = rest;
        Ok(())
    }

    #[inline]
    fn parse_f32(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<4>(self.data).ok_or_else(Error::eof)?;
        self.token_tape.alloc().init(BinaryToken::F32(head));
        self.data = rest;
        Ok(())
    }

    #[inline]
    fn parse_f64(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<8>(self.data).ok_or_else(Error::eof)?;
        self.token_tape.alloc().init(BinaryToken::F64(head));
        self.data = rest;
        Ok(())
    }

    #[inline]
    fn parse_bool(&mut self) -> Result<(), Error> {
        let val = self.data.first().map(|&x| x != 0).ok_or_else(Error::eof)?;
        self.token_tape.alloc().init(BinaryToken::Bool(val));
        self.data = &self.data[1..];
        Ok(())
    }

    fn parse_rgb(&mut self) -> Result<(), Error> {
        let (head, rest) = get_split::<22>(self.data).ok_or_else(Error::eof)?;
        let val = Rgb {
            r: le_u32(&head[4..]),
            g: le_u32(&head[10..]),
            b: le_u32(&head[16..]),
        };
        self.data = rest;
        self.token_tape.alloc().init(BinaryToken::Rgb(val));
        Ok(())
    }

    #[inline(always)]
    fn parse_string_inner(&mut self) -> Result<Scalar<'a>, Error> {
        let (head, rest) = get_split::<2>(self.data).ok_or_else(Error::eof)?;
        let text_len = usize::from(u16::from_le_bytes(head));
        if text_len <= rest.len() {
            let (text, rest) = rest.split_at(text_len);
            let scalar = Scalar::new(text);
            self.data = rest;
            Ok(scalar)
        } else {
            Err(Error::eof())
        }
    }

    #[inline(always)]
    fn parse_quoted_string(&mut self) -> Result<(), Error> {
        let scalar = self.parse_string_inner()?;
        self.token_tape.alloc().init(BinaryToken::Quoted(scalar));
        Ok(())
    }

    #[inline(always)]
    fn parse_unquoted_string(&mut self) -> Result<(), Error> {
        let scalar = self.parse_string_inner()?;
        self.token_tape.alloc().init(BinaryToken::Unquoted(scalar));
        Ok(())
    }

    // Specialized routine that will parse an entire array of a single type
    #[inline]
    fn parse_homogenous_array<const TOKEN: u16, F>(&mut self, mut f: F) -> Result<(), Error>
    where
        F: FnMut(&mut Self) -> Result<(), Error>,
    {
        f(self)?;
        loop {
            let (d, x) = expect_token(self.data)?;
            if x == TOKEN {
                self.data = d;
                f(self)?;
            } else if x == END {
                self.data = d;
                unsafe { self.push_end_container().unwrap_unchecked() };
                return Ok(());
            } else {
                return Ok(());
            }
        }
    }

    // Specialized routine that will parse an entire array of a single type that
    // knows to reset the container type to back to a key (it's known that the
    // grand-parent is an object).
    #[inline]
    fn parse_homogenous_array_field<const TOKEN: u16, F>(&mut self, mut f: F) -> Result<(), Error>
    where
        F: FnMut(&mut Self) -> Result<(), Error>,
    {
        f(self)?;
        loop {
            let (d, x) = expect_token(self.data)?;
            if x == TOKEN {
                self.data = d;
                f(self)?;
            } else if x == END {
                self.data = d;
                let end_idx = self.token_tape.len();
                match unsafe { self.token_tape.get_unchecked_mut(self.parent_ind) } {
                    BinaryToken::Array(end) => {
                        let grand_ind = *end;
                        *end = end_idx;
                        let val = BinaryToken::End(self.parent_ind);
                        self.token_tape.alloc().init(val);
                        self.parent_ind = grand_ind;
                        self.state = ParseState::Key;
                    }
                    _ => unsafe { core::hint::unreachable_unchecked() },
                }
                return Ok(());
            } else {
                return Ok(());
            }
        }
    }

    #[inline(always)]
    const fn next_state(state: ParseState) -> u8 {
        let num = state as u8;
        let offset1 = num >> 1;
        num + offset1
    }

    #[inline(always)]
    fn next_scalar_state(&mut self) {
        self.state = if self.state == ParseState::KeyValueSeparator {
            self.mixed_insert2()
        } else {
            let new_state = Self::next_state(self.state);
            unsafe { core::mem::transmute(new_state) }
        }
    }

    #[inline]
    fn push_new_container(&mut self) {
        // Until we know otherwise, assume the new container is an array.
        let ind = self.token_tape.len();
        self.token_tape
            .alloc()
            .init(BinaryToken::Array(self.parent_ind));
        self.parent_ind = ind;
        self.state = ParseState::ArrayValue;
    }

    #[inline]
    fn push_end_container(&mut self) -> Result<(), Error> {
        // Update the container token with now discovered location
        // of when the container ends
        let end_idx = self.token_tape.len();
        match unsafe { self.token_tape.get_unchecked_mut(self.parent_ind) } {
            BinaryToken::Array(end) | BinaryToken::Object(end) => {
                let grand_ind = *end;
                *end = end_idx;
                let val = BinaryToken::End(self.parent_ind);
                self.token_tape.alloc().init(val);
                self.parent_ind = grand_ind;
                self.state = match unsafe { self.token_tape.get_unchecked(grand_ind) } {
                    BinaryToken::Array(_) => ParseState::ArrayValue,
                    _ => ParseState::Key,
                };
                Ok(())
            }
            _ => Err(self.end_location_error()),
        }
    }

    #[inline]
    unsafe fn set_parent_to_object(&mut self) {
        let x = self.token_tape.get_unchecked_mut(self.parent_ind);
        if let BinaryToken::Array(end) = x {
            *x = BinaryToken::Object(*end);
            self.state = ParseState::ObjectValue;
        } else {
            debug_assert!(false, "expected an array to be present");
            core::hint::unreachable_unchecked();
        }
    }

    fn parse<const IS_EU4: bool>(&mut self) -> Result<(), Error> {
        while let Some((mut d, mut token_id)) = peek_token(self.data) {
            // Parse the happy path of <key> = <primitive or list of primitives>
            if self.state == ParseState::Key && is_token::<IS_EU4>(token_id) {
                if let (nd, EQUAL) = expect_token(d)? {
                    self.token_tape.alloc().init(BinaryToken::Token(token_id));
                    (d, token_id) = expect_token(nd)?;
                    if token_id == OPEN {
                        self.push_new_container();
                        self.data = d;

                        let mut is_token = false;
                        match self.parse_token()? {
                            EQUAL => return Err(self.equal_key_error()),
                            OPEN | END => {
                                self.data = d;
                                continue;
                            },
                            I32 => self.parse_i32()?,
                            F32 => self.parse_f32()?,
                            BOOL => self.parse_bool()?,
                            QUOTED_STRING => self.parse_quoted_string()?,
                            U32 => self.parse_u32()?,
                            UNQUOTED_STRING => self.parse_unquoted_string()?,
                            U64 if !IS_EU4 => self.parse_u64()?,
                            F64 => self.parse_f64()?,
                            RGB if !IS_EU4 => self.parse_rgb()?,
                            x => {
                                is_token = true;
                                self.token_tape.alloc().init(BinaryToken::Token(x));
                            }
                        };

                        let (d2, id2) = expect_token(self.data)?;

                        if is_token && id2 == EQUAL {
                            self.data = d2;
                            unsafe { self.set_parent_to_object() }
                            continue;
                        }

                        match id2 {
                            EQUAL => {
                                self.data = d2;
                                unsafe { self.set_parent_to_object() }
                            }
                            OPEN => continue,
                            END => continue,
                            I32 => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<I32, _>(Self::parse_i32)?;
                            }
                            F32 => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<F32, _>(Self::parse_f32)?;
                            }
                            BOOL => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<BOOL, _>(Self::parse_bool)?;
                            }
                            QUOTED_STRING => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<QUOTED_STRING, _>(
                                    Self::parse_quoted_string,
                                )?;
                            }
                            U32 => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<U32, _>(Self::parse_u32)?;
                            }
                            UNQUOTED_STRING => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<UNQUOTED_STRING, _>(
                                    Self::parse_unquoted_string,
                                )?;
                            }
                            U64 if !IS_EU4 => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<U64, _>(Self::parse_u64)?;
                            }
                            F64 => {
                                self.data = d2;
                                self.parse_homogenous_array_field::<F64, _>(Self::parse_f64)?;
                            }
                            RGB if !IS_EU4 => continue,
                            x => {
                                self.data = d2;
                                self.token_tape.alloc().init(BinaryToken::Token(x));
                            }
                        }
                        continue;
                    } else if token_id >= I32 && token_id <= QUOTED_STRING {
                        if token_id == I32 {
                            self.data = d;
                            self.parse_i32()?;
                            continue;
                        } else if token_id == F32 {
                            self.data = d;
                            self.parse_f32()?;
                            continue;
                        } else if token_id == QUOTED_STRING {
                            self.data = d;
                            self.parse_quoted_string()?;
                            continue;
                        } else {
                            self.data = d;
                            self.parse_bool()?;
                            continue;
                        }
                    } else if token_id == UNQUOTED_STRING {
                            self.data = d;
                            self.parse_unquoted_string()?;
                        continue;
                    } else if token_id == U32 {
                            self.data = d;
                            self.parse_u32()?;
                        continue;
                    } else {
                            self.state = ParseState::ObjectValue;
                    }
                }
            }

            // a switch statement that is dense up until 0x17. 5-6% perf
            match token_id {
                0x0 => return Err(self.unknown_token()),
                EQUAL => {
                    self.data = d;
                    if self.state == ParseState::KeyValueSeparator {
                        self.state = ParseState::ObjectValue;
                        continue;
                    } else if self.state == ParseState::ArrayValueMixed {
                        self.token_tape.alloc().init(BinaryToken::Equal);
                    } else if self.state == ParseState::ArrayValue {
                        self.token_tape.reserve(2);
                        let last = unsafe { self.token_tape.pop().unwrap_unchecked() };
                        if matches!(last, BinaryToken::Array(_) | BinaryToken::End(_)) {
                            return Err(self.equal_in_array_err());
                        }

                        // before we enter mixed mode, let's make sure we're not
                        // proceeded by only empty objects eg: `a={{} {} c=d}`. Only
                        // found in a smattering of EU4 saves
                        let mut pairs = self
                            .token_tape
                            .get(self.parent_ind + 1..)
                            .unwrap_or_default()
                            .chunks_exact(2);

                        let only_empties = pairs.len() > 0
                                && pairs.all(|tokens| {
                                    matches!(tokens, [BinaryToken::Array(x), BinaryToken::End(y)] if *x == y + 1)
                                });

                        let ptr = self.token_tape.as_mut_ptr();
                        if only_empties {
                            unsafe { self.set_parent_to_object() }
                            unsafe { ptr.add(self.parent_ind + 1).write(last) };
                            unsafe { self.token_tape.set_len(self.parent_ind + 2) }
                            self.state = ParseState::ObjectValue;
                        } else {
                            let len = self.token_tape.len();
                            unsafe { ptr.add(len).write(BinaryToken::MixedContainer) };
                            unsafe { ptr.add(len + 1).write(last) };
                            unsafe { ptr.add(len + 2).write(BinaryToken::Equal) };
                            unsafe { self.token_tape.set_len(len + 3) }
                            self.state = ParseState::ArrayValueMixed;
                        }
                    } else {
                        return Err(self.equal_key_error());
                    }
                }
                0x2 => return Err(self.unknown_token()),
                OPEN => {
                    if self.state != ParseState::Key {
                        self.data = d;
                        self.push_new_container();

                        let mut is_token = false;
                        match self.parse_token()? {
                            EQUAL => return Err(self.equal_key_error()),
                            OPEN | END => {
                                self.data = d;
                                continue;
                            },
                            I32 => self.parse_i32()?,
                            F32 => self.parse_f32()?,
                            BOOL => self.parse_bool()?,
                            QUOTED_STRING => self.parse_quoted_string()?,
                            U32 => self.parse_u32()?,
                            UNQUOTED_STRING => self.parse_unquoted_string()?,
                            U64 if !IS_EU4 => self.parse_u64()?,
                            F64 => self.parse_f64()?,
                            RGB if !IS_EU4 => self.parse_rgb()?,
                            x => {
                                is_token = true;
                                self.token_tape.alloc().init(BinaryToken::Token(x));
                            }
                        };

                        let (d2, id2) = expect_token(self.data)?;

                        if is_token && id2 == EQUAL {
                            self.data = d2;
                            unsafe { self.set_parent_to_object() }
                            continue;
                        }

                        match id2 {
                            EQUAL => {
                                self.data = d2;
                                unsafe { self.set_parent_to_object() }
                            }
                            OPEN => continue,
                            END => continue,
                            I32 => {
                                self.data = d2;
                                self.parse_homogenous_array::<I32, _>(Self::parse_i32)?;
                            }
                            F32 => {
                                self.data = d2;
                                self.parse_homogenous_array::<F32, _>(Self::parse_f32)?;
                            }
                            BOOL => {
                                self.data = d2;
                                self.parse_homogenous_array::<BOOL, _>(Self::parse_bool)?;
                            }
                            QUOTED_STRING => {
                                self.data = d2;
                                self.parse_homogenous_array::<QUOTED_STRING, _>(
                                    Self::parse_quoted_string,
                                )?;
                            }
                            U32 => {
                                self.data = d2;
                                self.parse_homogenous_array::<U32, _>(Self::parse_u32)?;
                            }
                            UNQUOTED_STRING => {
                                self.data = d2;
                                self.parse_homogenous_array::<UNQUOTED_STRING, _>(
                                    Self::parse_unquoted_string,
                                )?;
                            }
                            U64 if !IS_EU4 => {
                                self.data = d2;
                                self.parse_homogenous_array::<U64, _>(Self::parse_u64)?;
                            }
                            F64 => {
                                self.data = d2;
                                self.parse_homogenous_array::<F64, _>(Self::parse_f64)?;
                            }
                            x => {
                                self.data = d2;
                                self.token_tape.alloc().init(BinaryToken::Token(x));
                            }
                        };
                    } else if self.token_tape.is_empty() {
                        return Err(self.open_empty_err());
                    } else {
                        // Skip empty containers if they occur in the key
                        // position eg: `a={b=c {} d=1}`. These occur in every
                        // EU4 save, even in 1.34.
                        self.data = d;
                        if self.parse_token()? != END {
                            return Err(self.empty_object_err());
                        }
                    }
                }
                END => {
                    match self.state {
                        ParseState::KeyValueSeparator => {
                            // `a={b=c 10}`
                            self.mixed_insert1();
                        }
                        ParseState::ObjectValue => {
                            // err on `a={b=}`
                            return Err(self.end_valid_err());
                        }
                        _ => {}
                    }

                    self.data = d;
                    self.push_end_container()?;
                }
                0x5 => return Err(self.unknown_token()),
                0x6 => return Err(self.unknown_token()),
                0x7 => return Err(self.unknown_token()),
                0x8 => return Err(self.unknown_token()),
                0x9 => return Err(self.unknown_token()),
                0xa => return Err(self.unknown_token()),
                0xb => {
                    self.data = d;
                    self.token_tape.alloc().init(BinaryToken::Token(0xb));
                    self.next_scalar_state();
                }
                I32 => {
                    self.data = d;
                    self.parse_i32()?;
                    self.next_scalar_state();
                }
                F32 => {
                    self.data = d;
                    self.parse_f32()?;
                    self.next_scalar_state();
                }
                BOOL => {
                    self.data = d;
                    self.parse_bool()?;
                    self.next_scalar_state();
                }
                QUOTED_STRING => {
                    self.data = d;
                    self.parse_quoted_string()?;
                    self.next_scalar_state();
                }
                0x10 => return Err(self.unknown_token()),
                0x11 => return Err(self.unknown_token()),
                0x12 => return Err(self.unknown_token()),
                0x13 => return Err(self.unknown_token()),
                U32 => {
                    self.data = d;
                    self.parse_u32()?;
                    self.next_scalar_state();
                }
                0x15 => {
                    self.data = d;
                    self.token_tape.alloc().init(BinaryToken::Token(0x15));
                    self.next_scalar_state();
                }
                0x16 => {
                    self.data = d;
                    self.token_tape.alloc().init(BinaryToken::Token(0x16));
                    self.next_scalar_state();
                }
                UNQUOTED_STRING => {
                    self.data = d;
                    self.parse_unquoted_string()?;
                    self.next_scalar_state();
                }
                U64 if !IS_EU4 => {
                    self.data = d;
                    self.parse_u64()?;
                    self.next_scalar_state();
                }
                F64 => {
                    self.data = d;
                    self.parse_f64()?;
                    self.next_scalar_state();
                }
                RGB if !IS_EU4 => {
                    self.data = d;
                    self.parse_rgb()?;
                    self.next_scalar_state();
                }
                x => {
                    self.data = d;
                    self.token_tape.alloc().init(BinaryToken::Token(x));
                    self.next_scalar_state();
                }
            }
        }

        if self.parent_ind == 0 && self.state == ParseState::Key {
            Ok(())
        } else {
            Err(Error::eof())
        }
    }

    #[cold]
    #[inline(never)]
    fn mixed_insert2(&mut self) -> ParseState {
        self.token_tape.reserve(1);
        let stashed1 = unsafe { self.token_tape.pop().unwrap_unchecked() };
        match self.token_tape.pop() {
            Some(stashed2) => {
                let ptr = self.token_tape.as_mut_ptr();
                let len = self.token_tape.len();
                unsafe { ptr.add(len).write(BinaryToken::MixedContainer) };
                unsafe { ptr.add(len + 1).write(stashed2) };
                unsafe { ptr.add(len + 2).write(stashed1) };
                unsafe { self.token_tape.set_len(len + 3) }
                ParseState::ArrayValueMixed
            }
            None => {
                let ptr = self.token_tape.as_mut_ptr();
                let len = self.token_tape.len();
                unsafe { ptr.add(len).write(stashed1) };
                unsafe { self.token_tape.set_len(len + 1) }
                ParseState::ObjectValue
            }
        }
    }

    #[cold]
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

    #[inline(never)]
    #[cold]
    fn equal_key_error(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("EQUAL not valid for a key"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn end_location_error(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("END token must be accompanied by an array or object token"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn empty_object_err(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("expected an empty object"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn unknown_token(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("a token in bad range detected"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn equal_in_array_err(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("EQUAL in an array should come after a scalar"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn open_empty_err(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("Starting open token is not valid"),
            offset: self.offset(),
        })
    }

    #[inline(never)]
    #[cold]
    fn end_valid_err(&self) -> Error {
        Error::new(ErrorKind::InvalidSyntax {
            msg: String::from("END only valid for object or array"),
            offset: self.offset(),
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

    fn parse<'a>(data: &'a [u8]) -> Result<BinaryTape<'a>, Error> {
        BinaryTape::from_slice(data)
    }

    #[test]
    fn test_size_of_binary_token() {
        let token_size = std::mem::size_of::<BinaryToken>();
        let maxed = std::cmp::max(std::mem::size_of::<Scalar>(), std::mem::size_of::<Rgb>());
        assert!(token_size <= 24);
        assert_eq!(token_size, maxed + std::mem::size_of::<usize>());
    }

    #[test]
    fn test_binary_tokens_dont_need_to_be_dropped() {
        assert!(!std::mem::needs_drop::<BinaryToken>());
    }

    #[test]
    fn test_parse_offset() {
        let data = [0x82, 0x2d, 0x01, 0x00, 0x4c, 0x28, 0x01, 0x00, 0x4c, 0x28];
        let err = BinaryTape::from_slice(&data[..]).unwrap_err();
        match err.kind() {
            ErrorKind::InvalidSyntax { offset, .. } => {
                assert_eq!(*offset, 8);
            }
            _ => assert!(false),
        }

        let data2 = [0x82, 0x2d, 0x01, 0x00, 0x01, 0x00];
        let err = BinaryTape::from_slice(&data2[..]).unwrap_err();
        match err.kind() {
            ErrorKind::InvalidSyntax { offset, .. } => {
                assert_eq!(*offset, 6);
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
        data.extend_from_slice(&END.to_le_bytes());
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
