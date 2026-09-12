use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read, Write},
};

use crate::{
    flavor::Hoi4BinaryFormat, melt, models::Hoi4Save, Encoding, Hoi4Error, Hoi4ErrorKind,
    MeltOptions, MeltedDocument,
};
use jomini::{
    binary::{BinaryFormatDeserializer, TokenResolver},
    text::ObjectReader,
    TextDeserializer, TextTape, Utf8Encoding,
};
use serde::de::DeserializeOwned;

const TXT_HEADER: &[u8] = b"HOI4txt";
const BIN_HEADER: &[u8] = b"HOI4bin";

enum FileHeader {
    Text,
    Binary,
}

fn file_header(data: &[u8]) -> Option<(FileHeader, &[u8])> {
    if data.len() < TXT_HEADER.len() {
        return None;
    }

    let (header, rest) = data.split_at(TXT_HEADER.len());
    match header {
        TXT_HEADER => Some((FileHeader::Text, rest)),
        BIN_HEADER => Some((FileHeader::Binary, rest)),
        _ => None,
    }
}

/// Entrypoint for parsing HOI4 saves
///
/// Only consumes enough data to determine encoding of the file
pub struct Hoi4File {}

impl Hoi4File {
    /// Parse a HOI4 file from a slice of data
    pub fn from_slice(data: &[u8]) -> Result<Hoi4SliceFile<'_>, Hoi4Error> {
        match file_header(data) {
            Some((FileHeader::Text, data)) => Ok(Hoi4SliceFile {
                kind: Hoi4SliceFileKind::Text(Hoi4Text(data)),
            }),
            Some((FileHeader::Binary, data)) => Ok(Hoi4SliceFile {
                kind: Hoi4SliceFileKind::Binary(Hoi4Binary(data)),
            }),
            None => Err(Hoi4Error::new(Hoi4ErrorKind::UnknownHeader)),
        }
    }

    /// Parse a HOI4 file from a file
    pub fn from_file(mut file: File) -> Result<Hoi4FsFile, Hoi4Error> {
        let mut header = [0u8; TXT_HEADER.len()];
        file.read_exact(&mut header)?;
        match file_header(&header) {
            Some((FileHeader::Text, _)) => Ok(Hoi4FsFile {
                kind: Hoi4FsFileKind::Text(Hoi4TextReader::from_reader(file)),
            }),
            Some((FileHeader::Binary, _)) => Ok(Hoi4FsFile {
                kind: Hoi4FsFileKind::Binary(Hoi4Binary(file)),
            }),
            None => Err(Hoi4Error::new(Hoi4ErrorKind::UnknownHeader)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Hoi4SliceFileKind<'a> {
    Text(Hoi4Text<'a>),
    Binary(Hoi4Binary<&'a [u8]>),
}

#[derive(Debug, Clone)]
pub struct Hoi4SliceFile<'a> {
    pub kind: Hoi4SliceFileKind<'a>,
}

impl<'a> Hoi4SliceFile<'a> {
    pub fn kind(&self) -> &Hoi4SliceFileKind<'a> {
        &self.kind
    }

    pub fn kind_mut(&'a mut self) -> &'a mut Hoi4SliceFileKind<'a> {
        &mut self.kind
    }

    pub fn encoding(&self) -> Encoding {
        match &self.kind {
            Hoi4SliceFileKind::Text(_) => Encoding::Plaintext,
            Hoi4SliceFileKind::Binary(_) => Encoding::Binary,
        }
    }

    pub fn parse_save<R>(&self, resolver: R) -> Result<Hoi4Save, Hoi4Error>
    where
        R: TokenResolver,
    {
        self.parse(resolver)
    }

    pub fn parse<T, R>(&self, resolver: R) -> Result<T, Hoi4Error>
    where
        R: TokenResolver,
        T: DeserializeOwned,
    {
        match &self.kind {
            Hoi4SliceFileKind::Text(data) => data.deserializer().deserialize(),
            Hoi4SliceFileKind::Binary(data) => data.clone().deserializer(resolver).deserialize(),
        }
    }

    pub fn melt<Resolver, Writer>(
        &self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Hoi4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match &self.kind {
            Hoi4SliceFileKind::Text(data) => {
                output.write_all(TXT_HEADER)?;
                output.write_all(
                    b"
",
                )?;
                output.write_all(data.0)?;
                Ok(MeltedDocument::new())
            }
            Hoi4SliceFileKind::Binary(data) => {
                output.write_all(TXT_HEADER)?;
                output.write_all(
                    b"
",
                )?;
                let doc = melt::melt(data.0, &mut output, resolver, options)?;
                output.write_all(
                    b"
",
                )?;
                Ok(doc)
            }
        }
    }
}

pub enum Hoi4FsFileKind {
    Text(Hoi4TextReader<File>),
    Binary(Hoi4Binary<File>),
}

pub struct Hoi4FsFile {
    pub kind: Hoi4FsFileKind,
}

impl Hoi4FsFile {
    pub fn kind(&self) -> &Hoi4FsFileKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut Hoi4FsFileKind {
        &mut self.kind
    }

    pub fn encoding(&self) -> Encoding {
        match &self.kind {
            Hoi4FsFileKind::Text(_) => Encoding::Plaintext,
            Hoi4FsFileKind::Binary(_) => Encoding::Binary,
        }
    }

    pub fn parse_save<RES>(&mut self, resolver: RES) -> Result<Hoi4Save, Hoi4Error>
    where
        RES: TokenResolver,
    {
        match &mut self.kind {
            Hoi4FsFileKind::Text(file) => file.as_ref().deserializer().deserialize(),
            Hoi4FsFileKind::Binary(file) => file.as_ref().deserializer(resolver).deserialize(),
        }
    }

    pub fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Hoi4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        output.write_all(b"HOI4txt\n")?;
        match &mut self.kind {
            Hoi4FsFileKind::Text(file) => {
                std::io::copy(&mut file.0, &mut output)?;
                Ok(MeltedDocument::new())
            }
            Hoi4FsFileKind::Binary(file) => file.melt(options, resolver, output),
        }
    }
}

/// A Hoi4 text save
#[derive(Debug, Clone)]
pub struct Hoi4Text<'a>(&'a [u8]);

impl<'a> Hoi4Text<'a> {
    pub fn get_ref(&self) -> &'a [u8] {
        self.0
    }

    pub fn deserializer(&self) -> Hoi4Modeller<'a, HashMap<u16, String>> {
        Hoi4Modeller::from_reader(Box::new(self.0), HashMap::new(), Encoding::Plaintext)
    }
}

#[derive(Debug)]
pub struct Hoi4TextReader<R>(R);

impl<R> Hoi4TextReader<R>
where
    R: Read,
{
    pub fn from_reader(reader: R) -> Self {
        Self(reader)
    }

    pub fn as_ref(&self) -> Hoi4TextReader<&R> {
        Hoi4TextReader(&self.0)
    }

    pub fn deserializer<'a>(self) -> Hoi4Modeller<'a, HashMap<u16, String>>
    where
        R: Read + 'a,
    {
        Hoi4Modeller::from_reader(self.0, HashMap::new(), Encoding::Plaintext)
    }

    // pub fn parse<T: DeserializeOwned>(&self) -> Result<T, Hoi4Error> {
    //     self.deserializer().deserialize()
    // }
}

impl<R: Read> Read for Hoi4TextReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

#[derive(Debug, Clone)]
pub struct Hoi4Binary<R>(R);

impl<R> Hoi4Binary<R>
where
    R: Read,
{
    pub fn get_ref(&self) -> &R {
        &self.0
    }

    pub fn as_ref(&self) -> Hoi4Binary<&R> {
        Hoi4Binary(&self.0)
    }

    pub fn deserializer<'a, Resolver>(self, resolver: Resolver) -> Hoi4Modeller<'a, Resolver>
    where
        R: Read + 'a,
        Resolver: TokenResolver,
    {
        Hoi4Modeller::from_reader(self.0, resolver, Encoding::Binary)
    }

    pub fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Hoi4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        melt::melt(&mut self.0, &mut output, resolver, options)
    }
}

/// A parsed Hoi4 text document
pub struct Hoi4ParsedText<'a> {
    tape: TextTape<'a>,
}

impl<'a> Hoi4ParsedText<'a> {
    pub fn from_slice(data: &'a [u8]) -> Result<Self, Hoi4Error> {
        file_header(data)
            .filter(|(header, _)| matches!(header, FileHeader::Text))
            .map(|(_, data)| data)
            .ok_or_else(|| Hoi4ErrorKind::UnknownHeader.into())
            .and_then(Self::from_raw)
    }

    pub fn from_raw(data: &'a [u8]) -> Result<Self, Hoi4Error> {
        let tape = TextTape::from_slice(data).map_err(Hoi4ErrorKind::Parse)?;
        Ok(Hoi4ParsedText { tape })
    }

    pub fn reader(&self) -> ObjectReader<'_, '_, Utf8Encoding> {
        self.tape.utf8_reader()
    }
}

pub struct Hoi4Modeller<'obj, Resolver> {
    reader: Box<dyn Read + 'obj>,
    resolver: Resolver,
    encoding: Encoding,
}

impl<'obj, Resolver: TokenResolver> Hoi4Modeller<'obj, Resolver> {
    pub fn from_reader<Reader: Read + 'obj>(
        reader: Reader,
        resolver: Resolver,
        encoding: Encoding,
    ) -> Self {
        Hoi4Modeller {
            reader: Box::new(reader),
            resolver,
            encoding,
        }
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn deserialize<T>(&mut self) -> Result<T, Hoi4Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl<'de, 'a: 'de, Resolver: TokenResolver> serde::de::Deserializer<'de>
    for &'a mut Hoi4Modeller<'_, Resolver>
{
    type Error = Hoi4Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(Hoi4Error::new(Hoi4ErrorKind::DeserializeImpl {
            msg: String::from("only struct supported"),
        }))
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        if matches!(self.encoding, Encoding::Binary) {
            let mut deser = BinaryFormatDeserializer::from_reader(
                &mut self.reader,
                Hoi4BinaryFormat::new(&self.resolver),
            );
            Ok(deser.deserialize_struct(name, fields, visitor)?)
        } else {
            let reader = jomini::text::TokenReader::new(&mut self.reader);
            let mut deser = TextDeserializer::from_utf8_reader(reader);
            Ok(deser.deserialize_struct(name, fields, visitor)?)
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum identifier ignored_any
    }
}
