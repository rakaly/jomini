use jomini::{
    binary::LexemeId,
    binary::ng::{BinaryTokenFormat, BinaryValueFormat, FieldResolver, from_reader, from_slice},
};
use serde::Deserialize;
use std::{fmt::Debug, io::Read};

pub(crate) fn push_lexeme(buf: &mut Vec<u8>, id: LexemeId) {
    buf.extend_from_slice(&id.0.to_le_bytes());
}

pub(crate) fn push_field(buf: &mut Vec<u8>, id: u16) {
    buf.extend_from_slice(&id.to_le_bytes());
}

pub(crate) fn push_i32(buf: &mut Vec<u8>, value: i32) {
    push_lexeme(buf, LexemeId::I32);
    buf.extend_from_slice(&value.to_le_bytes());
}

pub(crate) fn push_u32(buf: &mut Vec<u8>, value: u32) {
    push_lexeme(buf, LexemeId::U32);
    buf.extend_from_slice(&value.to_le_bytes());
}

pub(crate) fn push_bool(buf: &mut Vec<u8>, value: bool) {
    push_lexeme(buf, LexemeId::BOOL);
    buf.push(u8::from(value));
}

pub(crate) fn push_f32_raw(buf: &mut Vec<u8>, raw: [u8; 4]) {
    push_lexeme(buf, LexemeId::F32);
    buf.extend_from_slice(&raw);
}

pub(crate) fn push_f64_raw(buf: &mut Vec<u8>, raw: [u8; 8]) {
    push_lexeme(buf, LexemeId::F64);
    buf.extend_from_slice(&raw);
}

pub(crate) fn push_quoted(buf: &mut Vec<u8>, data: &[u8]) {
    push_lexeme(buf, LexemeId::QUOTED);
    buf.extend_from_slice(&(data.len() as u16).to_le_bytes());
    buf.extend_from_slice(data);
}

pub(crate) fn push_unquoted(buf: &mut Vec<u8>, data: &[u8]) {
    push_lexeme(buf, LexemeId::UNQUOTED);
    buf.extend_from_slice(&(data.len() as u16).to_le_bytes());
    buf.extend_from_slice(data);
}

pub(crate) fn push_lookup_u8(buf: &mut Vec<u8>, value: u8) {
    push_lexeme(buf, LexemeId::LOOKUP_U8);
    buf.push(value);
}

pub(crate) fn push_fixed5_i16(buf: &mut Vec<u8>, raw: u16) {
    push_lexeme(buf, LexemeId::FIXED5_I16);
    buf.extend_from_slice(&raw.to_le_bytes());
}

pub(crate) struct ChunkedReader<'a> {
    data: &'a [u8],
    offset: usize,
    chunk_size: usize,
}

impl<'a> ChunkedReader<'a> {
    pub(crate) fn new(data: &'a [u8], chunk_size: usize) -> Self {
        Self {
            data,
            offset: 0,
            chunk_size,
        }
    }
}

impl Read for ChunkedReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let remaining = &self.data[self.offset..];
        if remaining.is_empty() {
            return Ok(0);
        }

        let amt = remaining.len().min(self.chunk_size).min(buf.len());
        buf[..amt].copy_from_slice(&remaining[..amt]);
        self.offset += amt;
        Ok(amt)
    }
}

pub(crate) fn assert_slice_and_reader<T, F, RES, Make>(
    data: &[u8],
    make_format: Make,
    resolver: RES,
) -> T
where
    T: for<'de> Deserialize<'de> + PartialEq + Debug,
    F: BinaryValueFormat + BinaryTokenFormat,
    RES: FieldResolver + Copy,
    Make: Fn() -> F,
{
    let from_bytes: T = from_slice(data, make_format(), resolver).unwrap();
    let from_stream: T = from_reader(ChunkedReader::new(data, 3), make_format(), resolver).unwrap();
    assert_eq!(from_bytes, from_stream);
    from_bytes
}
