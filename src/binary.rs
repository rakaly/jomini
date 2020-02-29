use crate::core::{BinaryError, BinaryEvent, BinaryParseEvent, BinaryParser};
use crate::scalar::Scalar;
use crate::text::Operator;
use std::error;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io;
use std::path::Path;

#[derive(Debug)]
pub enum BinaryReadingError {
    Parse(BinaryError),
    Io(io::Error),
}

impl Display for BinaryReadingError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "unable to read binary data")
    }
}

impl error::Error for BinaryReadingError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            BinaryReadingError::Parse(x) => Some(x),
            BinaryReadingError::Io(x) => Some(x),
        }
    }
}

impl From<BinaryError> for BinaryReadingError {
    fn from(error: BinaryError) -> Self {
        BinaryReadingError::Parse(error)
    }
}

impl From<io::Error> for BinaryReadingError {
    fn from(error: io::Error) -> Self {
        BinaryReadingError::Io(error)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EventRecord {
    buffer: Vec<u8>,
    buffer_len: usize,
    stored_event: BinaryReaderEventType,
}

impl Default for EventRecord {
    fn default() -> Self {
        Self::new()
    }
}

impl EventRecord {
    pub fn new() -> Self {
        EventRecord {
            buffer: Vec::new(),
            buffer_len: 0,
            stored_event: BinaryReaderEventType::StartObject,
        }
    }

    pub(crate) fn clone_from(&mut self, event: BinaryEvent) {
        match event {
            BinaryEvent::Text(s) => {
                let data = s.view_data();
                if self.buffer.len() < data.len() {
                    self.buffer.resize(
                        data.len()
                            .checked_next_power_of_two()
                            .unwrap_or_else(|| data.len()),
                        0,
                    );
                }

                self.buffer[..data.len()].copy_from_slice(data);
                self.buffer_len = data.len();
                self.stored_event = BinaryReaderEventType::Text;
            }
            BinaryEvent::StartObject => self.stored_event = BinaryReaderEventType::StartObject,
            BinaryEvent::StartArray => self.stored_event = BinaryReaderEventType::StartArray,
            BinaryEvent::End => self.stored_event = BinaryReaderEventType::End,
            BinaryEvent::Operator(x) => self.stored_event = BinaryReaderEventType::Operator(x),
            BinaryEvent::Bool(x) => self.stored_event = BinaryReaderEventType::Bool(x),
            BinaryEvent::U32(x) => self.stored_event = BinaryReaderEventType::U32(x),
            BinaryEvent::U64(x) => self.stored_event = BinaryReaderEventType::U64(x),
            BinaryEvent::I32(x) => self.stored_event = BinaryReaderEventType::I32(x),
            BinaryEvent::F32(x) => self.stored_event = BinaryReaderEventType::F32(x),
            BinaryEvent::Q16(x) => self.stored_event = BinaryReaderEventType::Q16(x),
            BinaryEvent::Token(x) => self.stored_event = BinaryReaderEventType::Token(x),
            BinaryEvent::Hsv(x, y, z) => self.stored_event = BinaryReaderEventType::Hsv(x, y, z),
        }
    }

    pub fn event(&self) -> BinaryEvent {
        match self.stored_event {
            BinaryReaderEventType::Text => {
                BinaryEvent::Text(Scalar::new(&self.buffer[..self.buffer_len]))
            }
            BinaryReaderEventType::StartObject => BinaryEvent::StartObject,
            BinaryReaderEventType::StartArray => BinaryEvent::StartArray,
            BinaryReaderEventType::End => BinaryEvent::End,
            BinaryReaderEventType::Operator(x) => BinaryEvent::Operator(x),
            BinaryReaderEventType::Bool(x) => BinaryEvent::Bool(x),
            BinaryReaderEventType::U32(x) => BinaryEvent::U32(x),
            BinaryReaderEventType::U64(x) => BinaryEvent::U64(x),
            BinaryReaderEventType::I32(x) => BinaryEvent::I32(x),
            BinaryReaderEventType::F32(x) => BinaryEvent::F32(x),
            BinaryReaderEventType::Q16(x) => BinaryEvent::Q16(x),
            BinaryReaderEventType::Token(x) => BinaryEvent::Token(x),
            BinaryReaderEventType::Hsv(x, y, z) => BinaryEvent::Hsv(x, y, z),
        }
    }
}

/// Same BinaryEvent except that the Text variant is decoupled from data
#[derive(Debug, PartialEq, Clone)]
enum BinaryReaderEventType {
    StartObject,
    StartArray,
    End,
    Operator(Operator),
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    Text,
    F32(f32),
    Q16(f32),
    Token(u16),
    Hsv(u32, u32, u32),
}

/// Builds a tweaked binary reader
#[derive(Debug)]
pub struct BinaryReaderBuilder {
    capacity: usize,
}

impl Default for BinaryReaderBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl BinaryReaderBuilder {
    pub fn new() -> Self {
        BinaryReaderBuilder { capacity: 0x8000 }
    }

    /// Set the capacity (in bytes) of the buffer used in binary parsing
    pub fn buffer_capacity(&mut self, capacity: usize) -> &mut Self {
        self.capacity = capacity;
        self
    }

    /// Creates a binary reader from the given read
    pub fn from_reader<R: io::Read>(&self, rdr: R) -> BinaryReader<R> {
        BinaryReader::new(self, rdr)
    }

    /// Creates a binary reader from a path
    pub fn from_path<P: AsRef<Path>>(
        &self,
        path: P,
    ) -> Result<BinaryReader<File>, BinaryReadingError> {
        Ok(BinaryReader::new(self, File::open(path)?))
    }
}

/// A pull based reader for binary data
///
/// The binary format stores strings as Windows-1252 encoded, so the data being parsed is assumed
/// to be Windows-1252 encoded.
///
/// It is the caller's responsibility to also uncompress the data and strip any magic code prior
/// to parsing.
#[derive(Debug)]
pub struct BinaryReader<R> {
    parser: BinaryParser,
    rdr: R,
    buffer: Vec<u8>,
    mid_buf: Vec<u8>,
    buffer_pos: usize,
    buffer_len: usize,
}

impl<R: io::Read> BinaryReader<R> {
    fn new(builder: &BinaryReaderBuilder, rdr: R) -> Self {
        BinaryReader {
            parser: BinaryParser::new(),
            buffer: vec![0; builder.capacity],
            mid_buf: Vec::new(),
            buffer_pos: 0,
            buffer_len: 0,
            rdr,
        }
    }

    /// Read bytes from the given reader
    pub fn from_reader(rdr: R) -> Self {
        BinaryReaderBuilder::new().from_reader(rdr)
    }

    /// Read bytes from the given file path
    pub fn from_path<P: AsRef<Path>>(path: P) -> Result<BinaryReader<File>, BinaryReadingError> {
        BinaryReaderBuilder::new().from_path(path)
    }

    /// Returns if the reader is in an object
    pub fn in_object(&self) -> bool {
        self.parser.in_object()
    }

    /// Returns if the reader is in an array
    pub fn in_array(&self) -> bool {
        self.parser.in_array()
    }

    pub fn expect_key(&self) -> bool {
        self.parser.expect_key()
    }

    pub fn expect_operator(&self) -> bool {
        self.parser.expect_operator()
    }

    pub fn expect_value(&self) -> bool {
        self.parser.expect_value()
    }

    pub fn depth(&self) -> usize {
        self.parser.depth()
    }

    pub fn read_event<'a>(
        &mut self,
        record: &'a mut EventRecord,
    ) -> Result<bool, BinaryReadingError> {
        let (event, read) = self
            .parser
            .read_event(&self.buffer[self.buffer_pos..self.buffer_len])?;
        match event {
            BinaryParseEvent::Event(x) => {
                record.clone_from(x);
                self.buffer_pos += read;
                Ok(true)
            }
            BinaryParseEvent::Eof => {
                // The amount of data we didn't read
                let leftover_size = self.buffer_len - self.buffer_pos;
                if leftover_size > self.mid_buf.len() {
                    self.mid_buf.resize(leftover_size, 0);
                }
                self.mid_buf[..leftover_size]
                    .copy_from_slice(&self.buffer[self.buffer_pos..self.buffer_len]);

                // Read data into the tail of the buffer.
                let rdr_read = self.rdr.read(&mut self.buffer[leftover_size..])?;

                self.buffer[..leftover_size].copy_from_slice(&self.mid_buf[..leftover_size]);
                self.buffer_pos = 0;
                self.buffer_len = leftover_size + rdr_read;

                let (event, read) = self
                    .parser
                    .read_event(&self.buffer[self.buffer_pos..self.buffer_len])?;

                match event {
                    BinaryParseEvent::Event(x) => {
                        record.clone_from(x);
                        self.buffer_pos += read;
                        Ok(true)
                    }
                    BinaryParseEvent::Eof => Ok(false),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let mut reader = BinaryReader::from_reader(&data[..]);
        let mut record = EventRecord::new();
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x2ee1));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::StartArray);
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Art of War"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Conquest of Paradise"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Res Publica"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Wealth of Nations"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::End);
        assert!(!reader.read_event(&mut record).unwrap());
    }

    #[test]
    fn test_multiple_top_level_events() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        let mut reader = BinaryReader::from_reader(&data[..]);
        let mut record = EventRecord::new();
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x2d82));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284b));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284d));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284c));
        assert!(!reader.read_event(&mut record).unwrap());
    }

    #[test]
    fn test_buffer_reads() {
        let data = [
            0x82, 0x2d, 0x01, 0x00, 0x4b, 0x28, 0x4d, 0x28, 0x01, 0x00, 0x4c, 0x28,
        ];

        let mut record = EventRecord::new();
        let mut reader = BinaryReaderBuilder::new()
            .buffer_capacity(3)
            .from_reader(&data[..]);

        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x2d82));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284b));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284d));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x284c));
        assert!(!reader.read_event(&mut record).unwrap());
    }

    #[test]
    fn test_string_array_buffer_reads() {
        let data = [
            0xe1, 0x2e, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x0a, 0x00, 0x41, 0x72, 0x74, 0x20,
            0x6f, 0x66, 0x20, 0x57, 0x61, 0x72, 0x0f, 0x00, 0x14, 0x00, 0x43, 0x6f, 0x6e, 0x71,
            0x75, 0x65, 0x73, 0x74, 0x20, 0x6f, 0x66, 0x20, 0x50, 0x61, 0x72, 0x61, 0x64, 0x69,
            0x73, 0x65, 0x0f, 0x00, 0x0b, 0x00, 0x52, 0x65, 0x73, 0x20, 0x50, 0x75, 0x62, 0x6c,
            0x69, 0x63, 0x61, 0x0f, 0x00, 0x11, 0x00, 0x57, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x20,
            0x6f, 0x66, 0x20, 0x4e, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x04, 0x00,
        ];

        let mut reader = BinaryReaderBuilder::new()
            .buffer_capacity(b"Conquest of Paradise".len() + 4)
            .from_reader(&data[..]);

        let mut record = EventRecord::new();
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Token(0x2ee1));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::Operator(Operator::Equal));
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::StartArray);
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Art of War"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Conquest of Paradise"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Res Publica"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(
            record.event(),
            BinaryEvent::Text(Scalar::new(b"Wealth of Nations"))
        );
        assert!(reader.read_event(&mut record).unwrap());
        assert_eq!(record.event(), BinaryEvent::End);
        assert!(!reader.read_event(&mut record).unwrap());
    }
}
