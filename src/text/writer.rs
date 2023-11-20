use crate::{
    common::PdsDateFormatter,
    text::{ArrayReader, ObjectReader, Operator, ValueReader},
    BinaryToken, Encoding, Error, ErrorKind, TextTape, TextToken,
};
use std::{fmt::Arguments, io::Write, ops::Deref};

/// Write data in PDS format.
///
/// Instantiated via `TextWriterBuilder`
#[derive(Debug)]
pub struct TextWriter<W> {
    writer: W,
    scratch: Vec<u8>,
    mode: DepthMode,
    depth: Vec<DepthMode>,
    state: WriteState,
    indent_char: u8,
    indent_factor: u8,
    needs_line_terminator: bool,
    mixed_mode: MixedMode,
}

/// Construct a customized text writer
///
/// ```
/// use jomini::TextWriterBuilder;
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let mut out: Vec<u8> = Vec::new();
/// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
/// writer.write_unquoted(b"hello")?;
/// writer.write_unquoted(b"world")?;
/// assert_eq!(std::str::from_utf8(&out).unwrap(), "hello=world");
/// # Ok(())
/// # }
/// ```
#[derive(Debug)]
pub struct TextWriterBuilder {
    indent_char: u8,
    indent_factor: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DepthMode {
    Object,
    Array,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MixedMode {
    Disabled,
    Started,
    Keyed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WriteState {
    Error = 0,
    Key = 1,
    ObjectValue = 2,
    KeyValueSeparator = 3,
    ArrayValue = 4,
    ArrayValueFirst = 5,
    FirstKey = 6,
}

const WRITE_STATE_NEXT: [WriteState; 7] = [
    WriteState::Error,
    WriteState::KeyValueSeparator,
    WriteState::Key,
    WriteState::Key,
    WriteState::ArrayValue,
    WriteState::ArrayValue,
    WriteState::KeyValueSeparator,
];

impl<W> TextWriter<W>
where
    W: Write,
{
    /// Get inner writer, keeping ownership
    pub fn inner(&mut self) -> &mut W {
        &mut self.writer
    }

    /// Consumes this Writer, returning the underlying writer
    pub fn into_inner(self) -> W {
        self.writer
    }

    /// Returns true if the next write event would be a key
    #[inline]
    pub fn expecting_key(&self) -> bool {
        matches!(self.state, WriteState::Key | WriteState::FirstKey)
    }

    /// Returns the number of objects deep the writer is from the root object
    #[inline]
    pub fn depth(&self) -> usize {
        self.depth.len()
    }

    /// Write out the start of an object
    pub fn write_object_start(&mut self) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_all(b"{")?;
        self.depth.push(self.mode);
        self.needs_line_terminator = true;
        self.mode = DepthMode::Object;
        self.state = WriteState::FirstKey;
        Ok(())
    }

    /// Write out the start of an array
    pub fn write_array_start(&mut self) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_all(b"{")?;
        self.depth.push(self.mode);
        self.mode = DepthMode::Array;
        self.state = WriteState::ArrayValueFirst;
        self.needs_line_terminator = true;
        Ok(())
    }

    /// Write the end of an array or object
    pub fn write_end(&mut self) -> Result<(), Error> {
        let old_state = self.state;
        if let Some(mode) = self.depth.pop() {
            self.mode = mode;
            self.state = match mode {
                DepthMode::Object => WriteState::Key,
                DepthMode::Array => WriteState::ArrayValue,
            };
        } else {
            return Err(Error::new(ErrorKind::StackEmpty { offset: 0 }));
        }

        if old_state != WriteState::ArrayValueFirst && old_state != WriteState::FirstKey {
            self.writer.write_all(b"\n")?;
            self.write_indent()?;
        } else {
            self.writer.write_all(b" ")?;
        }

        self.writer.write_all(b"}")?;
        self.needs_line_terminator = true;
        self.mixed_mode = MixedMode::Disabled;
        Ok(())
    }

    /// Write a boolean
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"hello")?;
    /// writer.write_bool(true)?;
    /// writer.write_unquoted(b"foo")?;
    /// writer.write_bool(false)?;
    /// assert_eq!(&out, b"hello=yes\nfoo=no");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_bool(&mut self, data: bool) -> Result<(), Error> {
        match data {
            true => write!(self, "yes"),
            false => write!(self, "no"),
        }
    }

    /// Write an operator. Writing an equal operator is optional whenever
    /// an object is being written.
    ///
    /// If an array was being written, the operator will switch to writing
    /// an object
    ///
    /// ```
    /// use jomini::{text::Operator, TextWriterBuilder};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"a")?;
    /// writer.write_operator(Operator::LessThan)?;
    /// writer.write_unquoted(b"b")?;
    /// assert_eq!(&out, b"a < b");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_operator(&mut self, data: Operator) -> Result<(), Error> {
        if self.mixed_mode == MixedMode::Disabled {
            if data == Operator::Equal {
                write!(self.writer, "{}", data)?;
            } else {
                write!(self.writer, " {} ", data)?;
            }

            self.mode = DepthMode::Object;
            self.state = WriteState::ObjectValue;
        } else {
            write!(self.writer, "{}", data)?;
            self.mixed_mode = MixedMode::Keyed;
        }
        Ok(())
    }

    /// Write bytes directly to the writer.
    ///
    /// The contents of the bytes are not checked, so it is up to the caller
    /// to ensure that the data is a valid unquoted field (no spaces or
    /// control characters).
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"a")?;
    /// writer.write_unquoted(b"b")?;
    /// assert_eq!(&out, b"a=b");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_unquoted(&mut self, data: &[u8]) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_all(data)?;
        self.write_epilogue()?;
        Ok(())
    }

    /// Write a field to be encapsulated in quotes.
    ///
    /// Unlike the unquoted variant, this method will inspect the data to
    /// ensure everything is properly escaped, like quotes and escape
    /// characters. And will trim trailing newlines. The textual data to write
    /// out is assumed to already be in the correct encoding (windows-1252 or
    /// UTF-8)
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_quoted(b"name")?;
    /// writer.write_quoted(br#"captain "joe" rogers"#)?;
    /// assert_eq!(&out, b"\"name\"=\"captain \\\"joe\\\" rogers\"");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_quoted(&mut self, data: &[u8]) -> Result<(), Error> {
        self.write_preamble()?;
        let esc_buf = self.scratch.split_off(0);
        let esc = escape(data, esc_buf);
        self.writer.write_all(b"\"")?;
        self.writer.write_all(&esc)?;
        self.writer.write_all(b"\"")?;
        self.scratch = esc.buffer();
        self.write_epilogue()?;
        Ok(())
    }

    /// Write a signed 32bit integer.
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"stability")?;
    /// writer.write_i32(-3);
    /// assert_eq!(&out, b"stability=-3");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_i32(&mut self, data: i32) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write an unsigned 32bit integer.
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"stability")?;
    /// writer.write_u32(3);
    /// assert_eq!(&out, b"stability=3");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_u32(&mut self, data: u32) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write an unsigned 64bit integer.
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"seed")?;
    /// writer.write_u64(1000000000000);
    /// assert_eq!(&out, b"seed=1000000000000");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_u64(&mut self, data: u64) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write a signed 64bit integer.
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"ratio")?;
    /// writer.write_i64(-1);
    /// assert_eq!(&out, b"ratio=-1");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_i64(&mut self, data: i64) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write a 32 bit floating point at full precision
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"morale")?;
    /// writer.write_f32(4.566);
    /// assert_eq!(&out, b"morale=4.566");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_f32(&mut self, data: f32) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write a 32 bit floating point at a given precision
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"morale")?;
    /// writer.write_f32_precision(4.0, 3);
    /// assert_eq!(&out, b"morale=4.000");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_f32_precision(&mut self, data: f32, precision: usize) -> Result<(), Error> {
        write!(self, "{0:.1$}", data, precision)
    }

    /// Write a 64 bit floating point at full precision
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"strength")?;
    /// writer.write_f64(6790.35609);
    /// assert_eq!(&out, b"strength=6790.35609");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_f64(&mut self, data: f64) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write a 64 bit floating point at a given precision
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"morale")?;
    /// writer.write_f64_precision(4.0, 3);
    /// assert_eq!(&out, b"morale=4.000");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_f64_precision(&mut self, data: f64, precision: usize) -> Result<(), Error> {
        write!(self, "{0:.1$}", data, precision)
    }

    /// Write a header.
    ///
    ///
    /// It is undefined if the next commands do not write out the start of an array
    /// or object
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"color")?;
    /// writer.write_header(b"rgb")?;
    /// writer.write_array_start()?;
    /// writer.write_i32(100)?;
    /// writer.write_i32(200)?;
    /// writer.write_i32(50)?;
    /// writer.write_end()?;
    /// assert_eq!(&out, b"color=rgb {\n  100 200 50\n}");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_header(&mut self, header: &[u8]) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_all(header)?;
        self.writer.write_all(b" ")?;
        self.state = WriteState::ObjectValue;
        Ok(())
    }

    /// Write a date formatted in the game style
    ///
    /// ```
    /// use jomini::{common::{Date, PdsDate}, TextWriterBuilder};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// let date = Date::from_ymd(1444, 11, 11);
    /// writer.write_unquoted(b"start")?;
    /// writer.write_date(date.game_fmt())?;
    /// assert_eq!(&out, b"start=1444.11.11");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_date(&mut self, data: PdsDateFormatter) -> Result<(), Error> {
        write!(self, "{}", data)
    }

    /// Write formatted data
    ///
    /// Typically not invoked directly but instead through the `write!` macro
    ///
    /// ```
    /// use jomini::TextWriterBuilder;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_unquoted(b"start")?;
    /// write!(writer, "unknown_{}", 5)?;
    /// assert_eq!(&out, b"start=unknown_5");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_fmt(&mut self, fmt: Arguments) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_fmt(fmt)?;
        self.write_epilogue()?;
        Ok(())
    }

    fn write_line_terminator(&mut self) -> Result<(), Error> {
        if self.needs_line_terminator {
            self.writer.write_all(b"\n")?;
            self.needs_line_terminator = false;
        }

        Ok(())
    }

    fn write_preamble(&mut self) -> Result<(), Error> {
        let just_wrote_line_terminator = self.needs_line_terminator;
        self.write_line_terminator()?;
        match self.state {
            WriteState::ArrayValue => {
                if just_wrote_line_terminator {
                    self.write_indent()?;
                } else if self.mixed_mode == MixedMode::Keyed {
                    self.mixed_mode = MixedMode::Started;
                } else {
                    self.writer.write_all(b" ")?;
                }
            }
            WriteState::Key => {
                self.write_indent()?;
            }
            WriteState::KeyValueSeparator => {
                self.writer.write_all(b"=")?;
            }
            WriteState::ArrayValueFirst | WriteState::FirstKey => {
                self.write_indent()?;
            }
            _ => {}
        };

        Ok(())
    }

    /// advance the state
    fn write_epilogue(&mut self) -> Result<(), Error> {
        self.state = WRITE_STATE_NEXT[self.state as usize];
        self.needs_line_terminator = self.state == WriteState::Key;
        Ok(())
    }

    /// Write the indent characters
    fn write_indent(&mut self) -> Result<(), Error> {
        for _ in 0..self.depth.len() * usize::from(self.indent_factor) {
            self.writer.write_all(&[self.indent_char])?;
        }

        Ok(())
    }

    /// Enter mixed mode for writing a container that is a list and an object
    pub fn start_mixed_mode(&mut self) {
        self.mode = DepthMode::Array;
        self.mixed_mode = MixedMode::Started;
    }

    /// Write the binary token with reasonable defaults
    ///
    /// This function is intended to be used as a fallback for a higher level
    /// melter that doesn't need special handling for a given token and thus can
    /// rely on the default behavior of forwarding the token to the writer's
    /// `write_x` methods.
    ///
    /// Any caller that uses this function will want to provide the floating
    /// point conversion and token translation, which is different from game to
    /// game.
    ///
    /// ## Example
    ///
    /// ```
    /// use jomini::{binary::{BinaryTape, BinaryToken}, TextTape, TextWriterBuilder};
    ///
    /// let data = [ 0x82, 0x2d, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x45, 0x4e, 0x47 ];
    /// let tape = BinaryTape::from_slice(&data[..]).unwrap();
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// for token in tape.tokens() {
    ///     match token {
    ///         // Define a floating point handler as there isn't a fallback format
    ///         BinaryToken::F32(x) => writer.write_f32(f32::from_le_bytes(*x))?,
    ///         BinaryToken::F64(x) => writer.write_f64(f64::from_le_bytes(*x))?,
    ///
    ///         // Every melter will want a custom token resolver
    ///         BinaryToken::Token(x) => {
    ///             if *x == 0x2d82 {
    ///                 writer.write_unquoted(b"field1")?;
    ///             } else {
    ///                 writer.write_unquoted(b"unknown")?;
    ///             }
    ///         },
    ///         x => writer.write_binary(x)?,
    ///     }
    /// }
    /// assert_eq!(&out, b"field1=\"ENG\"");
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    #[inline]
    pub fn write_binary(&mut self, token: &BinaryToken<'_>) -> Result<(), Error> {
        match token {
            BinaryToken::Array(_) => self.write_array_start(),
            BinaryToken::Object(_) => self.write_object_start(),
            BinaryToken::MixedContainer => {
                self.start_mixed_mode();
                Ok(())
            }
            BinaryToken::Equal => self.write_operator(Operator::Equal),
            BinaryToken::End(_) => self.write_end(),
            BinaryToken::Bool(x) => self.write_bool(*x),
            BinaryToken::U32(x) => self.write_u32(*x),
            BinaryToken::U64(x) => self.write_u64(*x),
            BinaryToken::I64(x) => self.write_i64(*x),
            BinaryToken::I32(x) => self.write_i32(*x),
            BinaryToken::Quoted(x) => self.write_quoted(x.as_bytes()),
            BinaryToken::Unquoted(x) => self.write_unquoted(x.as_bytes()),
            BinaryToken::F32(x) => self.write_f32(f32::from_le_bytes(*x)),
            BinaryToken::F64(x) => self.write_f64(f64::from_le_bytes(*x)),
            BinaryToken::Token(x) => {
                write!(self, "__unknown_0x{:x}", x)
            }
            BinaryToken::Rgb(color) => {
                self.write_header(b"rgb")?;
                self.write_array_start()?;
                self.write_u32(color.r)?;
                self.write_u32(color.g)?;
                self.write_u32(color.b)?;
                self.write_end()
            }
        }
    }

    /// Writes a text tape
    ///
    /// Formatting is not preserved.
    /// ```
    /// use jomini::{TextTape, TextWriterBuilder};
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(b"hello=world")?;
    /// let mut out: Vec<u8> = Vec::new();
    /// let mut writer = TextWriterBuilder::new().from_writer(&mut out);
    /// writer.write_tape(&tape)?;
    /// assert_eq!(&out, b"hello=world");
    /// # Ok(())
    /// # }
    /// ```
    pub fn write_tape(&mut self, tape: &TextTape) -> Result<(), Error> {
        // it doesn't actually matter the encoding that is chosen as everything
        // will be blindly written out and we don't need to know the encoding
        // for that.
        let reader = tape.windows1252_reader();
        self.write_object_core(reader)?;
        Ok(())
    }

    fn write_object_core<R>(&mut self, reader: ObjectReader<R>) -> Result<(), Error>
    where
        R: Encoding + Clone,
    {
        for (key, op, value) in reader.fields() {
            match key.token() {
                TextToken::Parameter(x) => {
                    self.write_preamble()?;
                    write!(self.writer, "[[")?;
                    self.writer.write_all(x.as_bytes())?;
                    self.writer.write_all(b"]\n")?;

                    if let Ok(obj) = value.read_object() {
                        self.write_object_core(obj)?;
                        self.writer.write_all(b"\n")?;
                        self.write_indent()?;
                    } else {
                        self.write_value(value)?;
                    }
                    self.writer.write_all(b"]")?;
                }
                TextToken::UndefinedParameter(x) => {
                    self.write_preamble()?;
                    write!(self.writer, "[[!")?;
                    self.writer.write_all(x.as_bytes())?;
                    self.writer.write_all(b"]\n")?;

                    if let Ok(obj) = value.read_object() {
                        self.write_object_core(obj)?;
                        self.writer.write_all(b"\n")?;
                        self.write_indent()?;
                    } else {
                        self.write_value(value)?;
                    }
                    self.writer.write_all(b"]")?;
                }
                TextToken::Quoted(x) => {
                    self.write_escaped_quotes(x.as_bytes())?;
                    if let Some(op) = op {
                        self.write_operator(op)?;
                    }

                    self.write_value(value)?;
                }
                _ => {
                    self.write_unquoted(key.read_scalar().as_bytes())?;
                    if let Some(op) = op {
                        self.write_operator(op)?;
                    }

                    self.write_value(value)?;
                }
            }
        }

        Ok(())
    }

    // For when the data is already escaped
    fn write_escaped_quotes(&mut self, x: &[u8]) -> Result<(), Error> {
        self.write_preamble()?;
        self.writer.write_all(b"\"")?;
        self.writer.write_all(x)?;
        self.writer.write_all(b"\"")?;
        self.write_epilogue()?;
        Ok(())
    }

    fn write_value<R>(&mut self, value: ValueReader<R>) -> Result<(), Error>
    where
        R: Encoding + Clone,
    {
        match value.token() {
            TextToken::Array { .. } => self.write_array(value.read_array().unwrap())?,
            TextToken::Object { .. } => self.write_object(value.read_object().unwrap())?,
            TextToken::MixedContainer => self.start_mixed_mode(),
            TextToken::Unquoted(x) => {
                self.write_unquoted(x.as_bytes())?;
            }
            TextToken::Quoted(x) => {
                self.write_escaped_quotes(x.as_bytes())?;
            }
            TextToken::Parameter(_) => unreachable!(),
            TextToken::UndefinedParameter(_) => unreachable!(),
            TextToken::Operator(op) => {
                if self.mixed_mode == MixedMode::Disabled {
                    self.writer.write_all(&b" "[..])?;
                } else {
                    self.mixed_mode = MixedMode::Keyed;
                }
                self.writer.write_all(op.symbol().as_bytes())?;
            }
            TextToken::End(_) => unreachable!(),
            TextToken::Header(x) => {
                let arr = value.read_array().unwrap();
                let mut values = arr.values();
                self.write_header(x.as_bytes())?;
                values.next().unwrap();
                let elem = values.next().unwrap();
                self.write_value(elem)?;
            }
        }
        Ok(())
    }

    fn write_array<R>(&mut self, array: ArrayReader<R>) -> Result<(), Error>
    where
        R: Encoding + Clone,
    {
        self.write_array_start()?;

        for value in array.values() {
            self.write_value(value)?;
        }

        self.write_end()?;
        Ok(())
    }

    fn write_object<R>(&mut self, object: ObjectReader<R>) -> Result<(), Error>
    where
        R: Encoding + Clone,
    {
        self.write_object_start()?;
        self.write_object_core(object)?;
        self.write_end()?;
        Ok(())
    }
}

impl TextWriterBuilder {
    /// Construct a new TextWriterBuilder with default values
    pub fn new() -> TextWriterBuilder {
        TextWriterBuilder::default()
    }

    /// The character to indent line.
    ///
    /// The default is a space.
    pub fn indent_char(&mut self, indent_char: u8) -> &mut TextWriterBuilder {
        self.indent_char = indent_char;
        self
    }

    /// The number of indents per increased depth
    ///
    /// The default is 2
    pub fn indent_factor(&mut self, indent_factor: u8) -> &mut TextWriterBuilder {
        self.indent_factor = indent_factor;
        self
    }

    /// Construct a text writer from a builder and a writer.
    pub fn from_writer<R>(&self, writer: R) -> TextWriter<R>
    where
        R: Write,
    {
        TextWriter {
            writer,
            scratch: Vec::new(),
            state: WriteState::Key,
            mode: DepthMode::Object,
            depth: Vec::with_capacity(16),
            indent_char: self.indent_char,
            indent_factor: self.indent_factor,
            needs_line_terminator: false,
            mixed_mode: MixedMode::Disabled,
        }
    }
}

impl Default for TextWriterBuilder {
    fn default() -> Self {
        TextWriterBuilder {
            indent_char: b' ',
            indent_factor: 2,
        }
    }
}

/// A cow that let's us reuse allocations
enum ReuseVec<'a> {
    Owned(Vec<u8>),
    Borrowed((&'a [u8], Vec<u8>)),
}

impl<'a> ReuseVec<'a> {
    fn buffer(self) -> Vec<u8> {
        match self {
            ReuseVec::Owned(x) => x,
            ReuseVec::Borrowed((_, x)) => x,
        }
    }
}

impl<'a> Deref for ReuseVec<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            ReuseVec::Owned(x) => x.as_slice(),
            ReuseVec::Borrowed((a, _)) => a,
        }
    }
}

#[inline]
fn escape(data: &[u8], mut buffer: Vec<u8>) -> ReuseVec {
    for (i, &x) in data.iter().enumerate() {
        if x == b'\\' || x == b'"' {
            buffer.clear();
            buffer.extend_from_slice(&data[..i]);
            for &x in data[i..data.len() - 1].iter() {
                if x == b'\\' || x == b'"' {
                    buffer.push(b'\\');
                }

                buffer.push(x);
            }

            if let Some(&last) = data.last() {
                if last != b'\n' {
                    if last == b'\\' || last == b'"' {
                        buffer.push(b'\\');
                    }
                    buffer.push(last);
                }
            }

            return ReuseVec::Owned(buffer);
        }
    }

    if let Some(&last) = data.last() {
        if last == b'\n' {
            ReuseVec::Borrowed((&data[..data.len() - 1], buffer))
        } else {
            ReuseVec::Borrowed((data, buffer))
        }
    } else {
        ReuseVec::Borrowed((data, buffer))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{Date, DateHour, PdsDate};
    use std::error::Error;

    #[test]
    fn write_two_fields() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"hello")?;
        writer.write_unquoted(b"world")?;
        assert!(writer.expecting_key());
        writer.write_unquoted(b"foo")?;
        writer.write_unquoted(b"bar")?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "hello=world\nfoo=bar");
        Ok(())
    }

    #[test]
    fn write_array() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"hello")?;
        writer.write_array_start()?;
        writer.write_unquoted(b"world")?;
        writer.write_unquoted(b"foo")?;
        writer.write_end()?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "hello={\n  world foo\n}"
        );
        Ok(())
    }

    #[test]
    fn write_quoted_array() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"hello")?;
        writer.write_array_start()?;
        writer.write_quoted(b"The Punic Wars")?;
        writer.write_end()?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "hello={\n  \"The Punic Wars\"\n}"
        );
        Ok(())
    }

    #[test]
    fn write_bool() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"hello")?;
        writer.write_bool(true)?;
        writer.write_unquoted(b"foo")?;
        writer.write_bool(false)?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "hello=yes\nfoo=no");
        Ok(())
    }

    #[test]
    fn write_operator() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"a")?;
        writer.write_operator(Operator::LessThan)?;
        writer.write_unquoted(b"b")?;
        writer.write_unquoted(b"c")?;
        writer.write_operator(Operator::LessThanEqual)?;
        writer.write_unquoted(b"f")?;
        writer.write_unquoted(b"d")?;
        writer.write_operator(Operator::GreaterThan)?;
        writer.write_unquoted(b"g")?;
        writer.write_unquoted(b"e")?;
        writer.write_operator(Operator::GreaterThanEqual)?;
        writer.write_unquoted(b"h")?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "a < b\nc <= f\nd > g\ne >= h"
        );
        Ok(())
    }

    #[test]
    fn write_quoted() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_quoted(b"name")?;
        writer.write_quoted(br#"captain "joe" rogers"#)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "\"name\"=\"captain \\\"joe\\\" rogers\""
        );
        Ok(())
    }

    #[test]
    fn write_alternative_float_format() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"morale")?;
        writer.write_f32_precision(1.0, 3)?;
        writer.write_unquoted(b"strength")?;
        writer.write_f64_precision(1.0, 5)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "morale=1.000\nstrength=1.00000"
        );
        Ok(())
    }

    #[test]
    fn write_primitives() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"primitives")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"bool")?;
        writer.write_bool(true)?;
        writer.write_unquoted(b"date")?;
        writer.write_date(Date::from_ymd(1444, 11, 11).game_fmt())?;
        writer.write_unquoted(b"f32")?;
        writer.write_f32(1.)?;
        writer.write_unquoted(b"f64")?;
        writer.write_f64(2.)?;
        writer.write_unquoted(b"i32")?;
        writer.write_i32(3)?;
        writer.write_unquoted(b"u32")?;
        writer.write_u32(4)?;
        writer.write_unquoted(b"u64")?;
        writer.write_u32(5)?;
        writer.write_end()?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "primitives={\n  bool=yes\n  date=1444.11.11\n  f32=1\n  f64=2\n  i32=3\n  u32=4\n  u64=5\n}"
        );
        Ok(())
    }

    #[test]
    fn write_deeper_object() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"id")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"id")?;
        writer.write_i32(10)?;
        writer.write_end()?;
        writer.write_unquoted(b"name")?;
        writer.write_unquoted(b"world")?;
        writer.write_end()?;
        writer.write_unquoted(b"a")?;
        writer.write_unquoted(b"b")?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  id={\n    id=10\n  }\n  name=world\n}\na=b"
        );
        Ok(())
    }

    #[test]
    fn write_deeper_array() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"settings")?;
        writer.write_array_start()?;
        writer.write_i32(0)?;
        writer.write_i32(1)?;
        writer.write_end()?;
        writer.write_unquoted(b"name")?;
        writer.write_unquoted(b"world")?;
        writer.write_end()?;
        writer.write_unquoted(b"a")?;
        writer.write_unquoted(b"b")?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  settings={\n    0 1\n  }\n  name=world\n}\na=b"
        );
        Ok(())
    }

    #[test]
    fn write_empty_nested_arrays() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_array_start()?;
        writer.write_array_start()?;
        writer.write_end()?;
        writer.write_array_start()?;
        writer.write_end()?;
        writer.write_end()?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  { }\n  { }\n}"
        );
        Ok(())
    }

    #[test]
    fn write_object_array() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_array_start()?;
        writer.write_object_start()?;
        writer.write_unquoted(b"a")?;
        writer.write_unquoted(b"b")?;
        writer.write_end()?;
        writer.write_object_start()?;
        writer.write_unquoted(b"c")?;
        writer.write_unquoted(b"d")?;
        writer.write_end()?;
        writer.write_end()?;

        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  {\n    a=b\n  }\n  {\n    c=d\n  }\n}"
        );
        Ok(())
    }

    #[test]
    fn write_operator_equal() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_array_start()?;
        writer.write_unquoted(b"a")?;
        writer.write_operator(Operator::Equal)?;
        writer.write_unquoted(b"b")?;
        writer.write_end()?;
        writer.write_unquoted(b"data")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"a")?;
        writer.write_operator(Operator::Equal)?;
        writer.write_unquoted(b"b")?;
        writer.write_end()?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  a=b\n}\ndata={\n  a=b\n}"
        );
        Ok(())
    }

    #[test]
    fn write_mixed_container_1() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_array_start()?;
        writer.write_unquoted(b"10")?;
        writer.start_mixed_mode();
        writer.write_unquoted(b"d")?;
        writer.write_operator(Operator::Equal)?;
        writer.write_unquoted(b"e")?;
        writer.write_unquoted(b"f")?;
        writer.write_operator(Operator::Equal)?;
        writer.write_unquoted(b"g")?;
        writer.write_end()?;
        writer.write_unquoted(b"a")?;
        writer.write_unquoted(b"b")?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n  10 d=e f=g\n}\na=b"
        );
        Ok(())
    }

    #[test]
    fn write_empty_array() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_array_start()?;
        writer.write_end()?;

        assert_eq!(std::str::from_utf8(&out).unwrap(), "data={ }");
        Ok(())
    }

    #[test]
    fn write_empty_object() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_object_start()?;
        assert!(writer.expecting_key());
        writer.write_end()?;

        assert_eq!(std::str::from_utf8(&out).unwrap(), "data={ }");
        Ok(())
    }

    #[test]
    fn write_with_alternate_indent() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new()
            .indent_char(b'\t')
            .indent_factor(1)
            .from_writer(&mut out);
        writer.write_unquoted(b"data")?;
        writer.write_object_start()?;
        writer.write_unquoted(b"settings")?;
        writer.write_array_start()?;
        writer.write_i32(0)?;
        writer.write_i32(1)?;
        writer.write_end()?;
        writer.write_unquoted(b"name")?;
        writer.write_unquoted(b"world")?;
        writer.write_end()?;
        writer.write_unquoted(b"a")?;
        writer.write_unquoted(b"b")?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "data={\n\tsettings={\n\t\t0 1\n\t}\n\tname=world\n}\na=b"
        );
        Ok(())
    }

    #[test]
    fn write_raw() -> Result<(), Box<dyn Error>> {
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"hello")?;
        write!(writer, "__unknown_{}", 5)?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "hello=__unknown_5");
        Ok(())
    }

    #[test]
    fn write_tape() -> Result<(), Box<dyn Error>> {
        let data = b"army={name=abc} army={name=def}";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "army={\n  name=abc\n}\narmy={\n  name=def\n}"
        );
        Ok(())
    }

    #[test]
    fn write_array_tape() -> Result<(), Box<dyn Error>> {
        let data = b"vals={1 2}";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "vals={\n  1 2\n}");
        Ok(())
    }

    #[test]
    fn write_mixed_container_tape() -> Result<(), Box<dyn Error>> {
        let data = b"vals={1 a=b d=f}";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "vals={\n  1 a=b d=f\n}");
        Ok(())
    }

    #[test]
    fn write_mixed_container_tape_2() -> Result<(), Box<dyn Error>> {
        let data = br"on_actions = {
            faith_holy_order_land_acquisition_pulse
            delay = { days = { 5 10 }}
            faith_heresy_events_pulse
            delay = { days = { 15 20 }}
            faith_fervor_events_pulse
          }";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "on_actions={\n  faith_holy_order_land_acquisition_pulse delay={\n    days={\n      5 10\n    }\n  }\n  faith_heresy_events_pulse delay = {\n    days={\n      15 20\n    }\n  }\n  faith_fervor_events_pulse\n}"
        );
        Ok(())
    }

    #[test]
    fn write_rgb_tape() -> Result<(), Box<dyn Error>> {
        let data = b"color= rgb{100 50 200}";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "color=rgb {\n  100 50 200\n}"
        );
        Ok(())
    }

    #[test]
    fn write_parameter_object() -> Result<(), Box<dyn Error>> {
        let data = b"generate_advisor = { [[scaled_skill] a=b ] }";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "generate_advisor={\n  [[scaled_skill]\n  a=b\n  ]\n}"
        );
        Ok(())
    }

    #[test]
    fn write_undefined_parameter_object() -> Result<(), Box<dyn Error>> {
        let data = b"generate_advisor = { [[!scaled_skill] a=b ] }";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "generate_advisor={\n  [[!scaled_skill]\n  a=b\n  ]\n}"
        );
        Ok(())
    }

    #[test]
    fn write_quoted_tape() -> Result<(), Box<dyn Error>> {
        let data = b"a=\"\"";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(std::str::from_utf8(&out).unwrap(), "a=\"\"");
        Ok(())
    }

    #[test]
    fn test_escape() {
        let scratch: Vec<u8> = Vec::new();
        let actual = escape(b"hello", scratch);
        assert_eq!(b"hello"[..], *actual);
        let actual = escape(b"", actual.buffer());
        assert_eq!(b""[..], *actual);
        let actual = escape(br#"Joe "Captain" Rogers"#, actual.buffer());
        assert_eq!(br#"Joe \"Captain\" Rogers"#[..], *actual);
        let actual = escape(br#"Joe Captain"s"#, actual.buffer());
        assert_eq!(br#"Joe Captain\"s"#[..], *actual);
        let actual = escape(b"\n", actual.buffer());
        assert_eq!(b""[..], *actual);
        let actual = escape(b"abc\n", actual.buffer());
        assert_eq!(b"abc"[..], *actual);
        let actual = escape(b"Joe \"Captain\" Rogers\n", actual.buffer());
        assert_eq!(br#"Joe \"Captain\" Rogers"#[..], *actual);
        let actual = escape(br#"Project "Eagle""#, actual.buffer());
        assert_eq!(br#"Project \"Eagle\""#[..], *actual);
        let actual = escape(br#"Project Eagle""#, actual.buffer());
        assert_eq!(br#"Project Eagle\""#[..], *actual);
        let actual = escape(br#"\abc\"#, actual.buffer());
        assert_eq!(br#"\\abc\\"#[..], *actual);
    }

    #[test]
    fn test_date_hour_write() -> Result<(), Box<dyn Error>> {
        let date = DateHour::from_ymdh(1936, 1, 1, 1);
        let date2 = DateHour::from_ymdh(1936, 1, 1, 24);
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_unquoted(b"date")?;
        writer.write_date(date.game_fmt())?;
        writer.write_unquoted(b"date2")?;
        writer.write_date(date2.game_fmt())?;
        assert_eq!(
            std::str::from_utf8(&out).unwrap(),
            "date=1936.1.1.1\ndate2=1936.1.1.24"
        );
        Ok(())
    }

    #[test]
    fn test_non_ascii_tape_write() -> Result<(), Box<dyn Error>> {
        let data = b"name=\"'Abb\xe2 Osmanoglu\"";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(&out, data);
        Ok(())
    }

    #[test]
    fn test_non_ascii_obj_tape_write() -> Result<(), Box<dyn Error>> {
        let data = b"obj={\n  name=\"'Abb\xe2 Osmanoglu\"\n}";
        let tape = TextTape::from_slice(data)?;
        let mut out: Vec<u8> = Vec::new();
        let mut writer = TextWriterBuilder::new().from_writer(&mut out);
        writer.write_tape(&tape)?;
        assert_eq!(&out, data);
        Ok(())
    }
}
