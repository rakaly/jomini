//! Parsing and deserializing EU4 save files
use crate::{
    flavor::Eu4Flavor,
    melt,
    models::{Eu4Save, GameState, Meta},
    resolver::SegmentedResolver,
    Encoding, Eu4Error, Eu4ErrorKind, MeltOptions, MeltedDocument,
};
use jomini::{binary::TokenResolver, text::ObjectReader, TextDeserializer, TextTape};
use rawzip::{CompressionMethod, FileReader, ReaderAt};
use serde::de::DeserializeOwned;
use std::{
    collections::HashSet,
    fmt::Display,
    fs::File,
    io::{Cursor, Read, Seek, Write},
};

#[cfg(feature = "zstd_c")]
use std::io::BufReader;

#[cfg(feature = "zstd_rust")]
use ruzstd::decoding::{FrameDecoder, StreamingDecoder};

const TXT_HEADER: &[u8] = b"EU4txt";
const BIN_HEADER: &[u8] = b"EU4bin";

pub struct Eu4File {}

impl Eu4File {
    pub fn from_slice(data: &[u8]) -> Result<Eu4SliceFile<'_>, Eu4Error> {
        match file_header(data) {
            Some((FileHeader::Text, data)) => Ok(Eu4SliceFile {
                kind: Eu4SliceFileKind::Text(Eu4Text(data)),
            }),
            Some((FileHeader::Binary, data)) => Ok(Eu4SliceFile {
                kind: Eu4SliceFileKind::Binary(Eu4Binary(data)),
            }),
            None => {
                let archive = rawzip::ZipArchive::from_slice(data).map_err(Eu4ErrorKind::Zip)?;
                let archive = archive.into_cursor_archive();

                let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
                let archive = Eu4Zip::try_from_archive(archive, &mut buf)?;
                Ok(Eu4SliceFile {
                    kind: Eu4SliceFileKind::Zip(Box::new(archive)),
                })
            }
        }
    }

    pub fn from_file(mut file: File) -> Result<Eu4FsFile<FileReader>, Eu4Error> {
        let mut header = [0u8; TXT_HEADER.len()];
        file.read_exact(&mut header)?;
        match file_header(&header) {
            Some((FileHeader::Text, _)) => Ok(Eu4FsFile {
                kind: Eu4FsFileKind::Text(file),
            }),
            Some((FileHeader::Binary, _)) => Ok(Eu4FsFile {
                kind: Eu4FsFileKind::Binary(Eu4Binary(file)),
            }),
            None => {
                let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
                let archive =
                    rawzip::ZipArchive::from_file(file, &mut buf).map_err(Eu4ErrorKind::Zip)?;
                let archive = Eu4Zip::try_from_archive(archive, &mut buf)?;
                Ok(Eu4FsFile {
                    kind: Eu4FsFileKind::Zip(Box::new(archive)),
                })
            }
        }
    }
}

const EMPTY_RESOLVER: SegmentedResolver<'static> = SegmentedResolver::empty();

pub struct Eu4Text<'a>(&'a [u8]);

impl<'a> Eu4Text<'a> {
    pub fn get_ref(&self) -> &[u8] {
        self.0
    }

    pub fn deserializer(&self) -> Eu4Modeller<'a, SegmentedResolver<'static>> {
        Eu4Modeller::from_reader(Box::new(self.0), EMPTY_RESOLVER).with_encoding(Encoding::Text)
    }
}

pub struct Eu4Binary<R>(R);
impl<R> Eu4Binary<R>
where
    R: Read,
{
    pub fn get_ref(&self) -> &R {
        &self.0
    }

    pub fn as_ref(&self) -> Eu4Binary<&R> {
        Eu4Binary(&self.0)
    }

    pub fn deserializer<'a, Resolver>(self, resolver: Resolver) -> Eu4Modeller<'a, Resolver>
    where
        R: Read + 'a,
        Resolver: TokenResolver,
    {
        Eu4Modeller::from_reader(self.0, resolver).with_encoding(Encoding::Binary)
    }

    pub fn melt<Resolver, Writer>(
        self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Eu4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        output.write_all(b"EU4txt\n")?;
        melt::melt(self.0, output, resolver, options.check_header(false))
    }
}

impl<R> Clone for Eu4Binary<R>
where
    R: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<R> Copy for Eu4Binary<R> where R: Copy {}

pub enum Eu4SliceFileKind<'a> {
    Text(Eu4Text<'a>),
    Binary(Eu4Binary<&'a [u8]>),
    Zip(Box<Eu4Zip<Cursor<&'a [u8]>>>),
}

pub struct Eu4SliceFile<'a> {
    kind: Eu4SliceFileKind<'a>,
}

impl<'a> Eu4SliceFile<'a> {
    pub fn kind(&self) -> &Eu4SliceFileKind<'_> {
        &self.kind
    }

    pub fn kind_mut(&'a mut self) -> &'a mut Eu4SliceFileKind<'a> {
        &mut self.kind
    }

    pub fn encoding(&self) -> Encoding {
        match &self.kind {
            Eu4SliceFileKind::Text(_) => Encoding::Text,
            Eu4SliceFileKind::Binary(_) => Encoding::Binary,
            Eu4SliceFileKind::Zip(archive) => {
                if archive.is_text {
                    Encoding::TextZip
                } else {
                    Encoding::BinaryZip
                }
            }
        }
    }

    pub fn parse_save<Resolver>(&self, resolver: Resolver) -> Result<Eu4Save, Eu4Error>
    where
        Resolver: TokenResolver,
    {
        match &self.kind {
            Eu4SliceFileKind::Text(data) => {
                Eu4Modeller::from_reader(Box::new(data.0), EMPTY_RESOLVER)
                    .with_encoding(Encoding::Text)
                    .deserialize()
            }
            Eu4SliceFileKind::Binary(data) => data.deserializer(resolver).deserialize(),
            Eu4SliceFileKind::Zip(archive) => {
                let meta: Meta = archive.deserialize_entry(archive.meta, &resolver)?;
                let game: GameState = archive.deserialize_entry(archive.gamestate, &resolver)?;
                Ok(Eu4Save { meta, game })
            }
        }
    }

    pub fn size(&self) -> usize {
        match &self.kind {
            Eu4SliceFileKind::Text(data) => data.0.len(),
            Eu4SliceFileKind::Binary(data) => data.0.len(),
            Eu4SliceFileKind::Zip(archive) => {
                (archive.meta.uncompressed_size_hint()
                    + archive.gamestate.uncompressed_size_hint()
                    + archive.ai.uncompressed_size_hint()) as usize
            }
        }
    }

    pub fn melt<Resolver, Writer>(
        &self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Eu4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match &self.kind {
            Eu4SliceFileKind::Text(data) => {
                output.write_all(b"EU4txt\n")?;
                output.write_all(data.0)?;
                Ok(MeltedDocument::new())
            }
            Eu4SliceFileKind::Binary(data) => data.melt(options, resolver, output),
            Eu4SliceFileKind::Zip(zip) => zip.melt(options, resolver, output),
        }
    }
}

pub struct Eu4ZipEntry<R: Read> {
    reader: ZipEntryVerifier<CompressedFileReader<R>>,
}

struct ZipEntryVerifier<R> {
    reader: R,
    crc: platform_crc::Crc,
    expected: rawzip::ZipVerification,
    size: u64,
}

impl<R: Read> ZipEntryVerifier<R> {
    fn new(reader: R, expected: rawzip::ZipVerification) -> Self {
        Self {
            reader,
            crc: platform_crc::Crc::new(),
            expected,
            size: 0,
        }
    }
}

impl<R: Read> Read for ZipEntryVerifier<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        let read = self.reader.read(buf)?;
        self.crc.update(&buf[..read]);
        self.size += read as u64;

        if read == 0 || self.size >= self.expected.uncompressed_size {
            verify_zip_entry(
                self.expected,
                rawzip::ZipVerification {
                    crc: self.crc.checksum(),
                    uncompressed_size: self.size,
                },
            )?;
        }

        Ok(read)
    }
}

fn verify_zip_entry(
    expected: rawzip::ZipVerification,
    actual: rawzip::ZipVerification,
) -> std::io::Result<()> {
    expected
        .valid(actual)
        .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, err))
}

#[cfg(target_family = "wasm")]
mod platform_crc {
    pub(super) struct Crc(rawzip::Crc32);

    impl Crc {
        pub(super) fn new() -> Self {
            Self(rawzip::Crc32::new())
        }

        pub(super) fn update(&mut self, data: &[u8]) {
            self.0.update(data);
        }

        pub(super) fn checksum(&self) -> u32 {
            self.0.checksum()
        }
    }
}

#[cfg(not(target_family = "wasm"))]
mod platform_crc {
    pub(super) struct Crc(flate2::Crc);

    impl Crc {
        pub(super) fn new() -> Self {
            Self(flate2::Crc::new())
        }

        pub(super) fn update(&mut self, data: &[u8]) {
            self.0.update(data);
        }

        pub(super) fn checksum(&self) -> u32 {
            self.0.sum()
        }
    }
}

impl<R> Eu4ZipEntry<R>
where
    R: Read,
{
    pub fn deserialize<T, Resolver>(&mut self, resolver: Resolver) -> Result<T, Eu4Error>
    where
        T: DeserializeOwned,
        Resolver: TokenResolver,
    {
        let mut modeller = Eu4Modeller::from_reader(&mut self.reader, resolver);
        let data: T = modeller.deserialize()?;
        Ok(data)
    }

    pub fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Eu4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        output.write_all(b"EU4txt\n")?;
        melt(
            &mut self.reader,
            output,
            resolver,
            options.skip_checksum(false),
        )
    }
}

impl<R> Read for Eu4ZipEntry<R>
where
    R: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

#[derive(Debug, Clone)]
pub struct Eu4Zip<R> {
    archive: rawzip::ZipArchive<R>,
    compression: rawzip::CompressionMethod,
    meta: rawzip::ZipArchiveEntryWayfinder,
    gamestate: rawzip::ZipArchiveEntryWayfinder,
    ai: rawzip::ZipArchiveEntryWayfinder,
    is_text: bool,
}

impl<R> Eu4Zip<R>
where
    R: rawzip::ReaderAt,
{
    pub fn try_from_archive(
        archive: rawzip::ZipArchive<R>,
        buf: &mut [u8],
    ) -> Result<Self, Eu4Error> {
        let mut meta = None;
        let mut gamestate = None;
        let mut ai = None;
        let mut entries = archive.entries(buf);
        let mut is_text = true;
        let mut header = [0u8; TXT_HEADER.len()];

        while let Ok(Some(entry)) = entries.next_entry() {
            let compression = entry.compression_method();
            match entry.file_path().as_ref() {
                b"meta" => {
                    let wayfinder = entry.wayfinder();
                    meta = Some(wayfinder);

                    let ent = archive.get_entry(wayfinder).map_err(Eu4ErrorKind::Zip)?;
                    let mut reader =
                        CompressedFileReader::from_compressed(ent.reader(), compression)?;
                    reader.read_exact(&mut header)?;

                    let header_type = file_header(&header)
                        .ok_or_else(|| Eu4Error::new(Eu4ErrorKind::UnknownHeader))?;
                    is_text = matches!(header_type, (FileHeader::Text, _));
                }
                b"gamestate" => gamestate = Some(entry.wayfinder()),
                b"ai" => ai = Some(entry.wayfinder()),
                _ => {}
            }

            if let (Some(meta), Some(gamestate), Some(ai)) = (meta, gamestate, ai) {
                return Ok(Eu4Zip {
                    archive,
                    meta,
                    gamestate,
                    ai,
                    compression,
                    is_text,
                });
            }
        }

        if meta.is_none() {
            return Err(Eu4ErrorKind::MissingFile(Eu4FileEntryName::Meta).into());
        }

        if gamestate.is_none() {
            return Err(Eu4ErrorKind::MissingFile(Eu4FileEntryName::Gamestate).into());
        }

        Err(Eu4ErrorKind::MissingFile(Eu4FileEntryName::Ai).into())
    }

    pub fn get(
        &self,
        name: Eu4FileEntryName,
    ) -> Result<Eu4ZipEntry<rawzip::ZipReader<&R>>, Eu4Error> {
        let wayfinder = match name {
            Eu4FileEntryName::Meta => self.meta,
            Eu4FileEntryName::Gamestate => self.gamestate,
            Eu4FileEntryName::Ai => self.ai,
        };

        let entry = self
            .archive
            .get_entry(wayfinder)
            .map_err(Eu4ErrorKind::Zip)?;
        let compressed = entry.reader();
        let expected = compressed.claim_verifier();
        let reader = CompressedFileReader::from_compressed(compressed, self.compression)?;
        let reader = ZipEntryVerifier::new(reader, expected);

        Ok(Eu4ZipEntry { reader })
    }

    pub fn encoding(&self) -> Encoding {
        if self.is_text {
            Encoding::TextZip
        } else {
            Encoding::BinaryZip
        }
    }

    pub fn deserialize_entry<T, Resolver>(
        &self,
        entry: rawzip::ZipArchiveEntryWayfinder,
        resolver: Resolver,
    ) -> Result<T, Eu4Error>
    where
        T: DeserializeOwned,
        Resolver: TokenResolver,
    {
        let zip_entry = self.archive.get_entry(entry).map_err(Eu4ErrorKind::Zip)?;
        let compressed = zip_entry.reader();
        let expected = compressed.claim_verifier();
        let reader = CompressedFileReader::from_compressed(compressed, self.compression)?;
        let reader = ZipEntryVerifier::new(reader, expected);
        let data: T = Eu4Modeller::from_reader(reader, resolver).deserialize()?;
        Ok(data)
    }

    pub fn melt<Resolver, Writer>(
        &self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Eu4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        if self.is_text {
            let mut meta = self.get(Eu4FileEntryName::Meta)?;
            std::io::copy(&mut meta, &mut output)?;

            let mut header = [0u8; TXT_HEADER.len() + 1];
            let mut gamestate = self.get(Eu4FileEntryName::Gamestate)?;
            gamestate.read_exact(&mut header)?;
            std::io::copy(&mut gamestate, &mut output)?;

            let mut ai = self.get(Eu4FileEntryName::Ai)?;
            ai.read_exact(&mut header)?;
            std::io::copy(&mut ai, &mut output)?;

            Ok(MeltedDocument::new())
        } else {
            output.write_all(b"EU4txt\n")?;
            let meta = self.get(Eu4FileEntryName::Meta)?;
            let meta_result = melt(meta, &mut output, &resolver, options.skip_checksum(true))?;

            let gamestate = self.get(Eu4FileEntryName::Gamestate)?;
            let gamestate_result = melt(
                gamestate,
                &mut output,
                &resolver,
                options.skip_checksum(true),
            )?;

            let ai = self.get(Eu4FileEntryName::Ai)?;
            let ai_result = melt(ai, &mut output, &resolver, options.skip_checksum(false))?;

            let union = meta_result
                .unknown_tokens
                .iter()
                .chain(gamestate_result.unknown_tokens.iter())
                .chain(ai_result.unknown_tokens.iter())
                .copied()
                .collect::<HashSet<u16>>();

            Ok(MeltedDocument {
                unknown_tokens: union,
            })
        }
    }
}

pub enum Eu4FsFileKind<R> {
    Text(File),
    Binary(Eu4Binary<File>),
    Zip(Box<Eu4Zip<R>>),
}

pub struct Eu4FsFile<R> {
    kind: Eu4FsFileKind<R>,
}

impl<R> Eu4FsFile<R>
where
    R: ReaderAt,
{
    pub fn kind(&self) -> &Eu4FsFileKind<R> {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut Eu4FsFileKind<R> {
        &mut self.kind
    }

    pub fn encoding(&self) -> Encoding {
        match &self.kind {
            Eu4FsFileKind::Text(_) => Encoding::Text,
            Eu4FsFileKind::Binary(_) => Encoding::Binary,
            Eu4FsFileKind::Zip(archive) => {
                if archive.is_text {
                    Encoding::TextZip
                } else {
                    Encoding::BinaryZip
                }
            }
        }
    }

    pub fn parse_save<Resolver>(&self, resolver: Resolver) -> Result<Eu4Save, Eu4Error>
    where
        Resolver: TokenResolver + Clone,
    {
        match &self.kind {
            Eu4FsFileKind::Text(file) => Ok(Eu4Modeller::from_reader(file, resolver)
                .with_encoding(Encoding::Text)
                .deserialize()?),
            Eu4FsFileKind::Binary(file) => file.as_ref().deserializer(resolver).deserialize(),
            Eu4FsFileKind::Zip(archive) => {
                let meta: Meta = archive.deserialize_entry(archive.meta, resolver.clone())?;
                let game: GameState = archive.deserialize_entry(archive.gamestate, resolver)?;
                Ok(Eu4Save { meta, game })
            }
        }
    }

    pub fn melt<Resolver, Writer>(
        &self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<MeltedDocument, Eu4Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match &self.kind {
            Eu4FsFileKind::Text(file) => {
                let mut file = file;
                file.seek(std::io::SeekFrom::Start(0))?;
                std::io::copy(&mut file, &mut output)?;
                Ok(MeltedDocument::new())
            }
            Eu4FsFileKind::Binary(file) => file.as_ref().melt(options, resolver, output),
            Eu4FsFileKind::Zip(zip) => zip.melt(options, resolver, output),
        }
    }
}

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

pub struct Eu4Modeller<'obj, R: jomini::binary::TokenResolver> {
    reader: Box<dyn Read + 'obj>,
    resolver: R,
    encoding: Option<Encoding>,
}

impl<'obj, R: jomini::binary::TokenResolver> Eu4Modeller<'obj, R> {
    pub fn from_reader<Reader: Read + 'obj>(reader: Reader, resolver: R) -> Self {
        Eu4Modeller {
            reader: Box::new(reader),
            resolver,
            encoding: None,
        }
    }

    pub fn with_encoding(self, encoding: Encoding) -> Self {
        Eu4Modeller {
            encoding: Some(encoding),
            ..self
        }
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding.unwrap_or(Encoding::Text)
    }

    pub fn deserialize<T>(&mut self) -> Result<T, Eu4Error>
    where
        T: DeserializeOwned,
    {
        T::deserialize(self)
    }
}

impl<'de, 'a: 'de, R: jomini::binary::TokenResolver> serde::de::Deserializer<'de>
    for &'a mut Eu4Modeller<'_, R>
{
    type Error = Eu4Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(Eu4Error::new(Eu4ErrorKind::DeserializeImpl {
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
        let encoding = match self.encoding {
            Some(e) => e,
            None => {
                let mut header = [0u8; BIN_HEADER.len()];
                self.reader.read_exact(&mut header)?;
                let encoding = match file_header(&header) {
                    Some((FileHeader::Text, _)) => Encoding::Text,
                    Some((FileHeader::Binary, _)) => Encoding::Binary,
                    None => return Err(Eu4ErrorKind::UnknownHeader.into()),
                };
                self.encoding = Some(encoding);
                encoding
            }
        };

        if matches!(encoding, Encoding::Binary) {
            use jomini::binary::BinaryFlavor;
            let flavor = Eu4Flavor::new();
            let mut deser = flavor
                .deserializer()
                .from_reader(&mut self.reader, &self.resolver);
            Ok(deser.deserialize_struct(name, fields, visitor)?)
        } else {
            let reader = jomini::text::TokenReader::new(&mut self.reader);
            let mut deser = TextDeserializer::from_encoded_reader(reader, Eu4Flavor::new());
            Ok(deser.deserialize_struct(name, fields, visitor)?)
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq tuple
        tuple_struct map enum identifier ignored_any
    }
}

enum CompressedReaderKind<R: Read> {
    Deflate(flate2::read::DeflateDecoder<R>),
    #[cfg(feature = "zstd_c")]
    ZstdC(zstd::stream::Decoder<'static, BufReader<R>>),
    #[cfg(feature = "zstd_rust")]
    ZstdRust(StreamingDecoder<R, FrameDecoder>),
}

struct CompressedFileReader<R: Read> {
    reader: CompressedReaderKind<R>,
}

impl<R: Read> CompressedFileReader<R> {
    pub fn from_compressed(reader: R, compression: CompressionMethod) -> Result<Self, Eu4Error>
    where
        R: Read,
    {
        match compression {
            CompressionMethod::DEFLATE => {
                let inflater = flate2::read::DeflateDecoder::new(reader);
                Ok(CompressedFileReader {
                    reader: CompressedReaderKind::Deflate(inflater),
                })
            }
            #[cfg(any(feature = "zstd_c", feature = "zstd_rust"))]
            CompressionMethod::ZSTD => {
                #[cfg(feature = "zstd_c")]
                {
                    let inflater = zstd::Decoder::new(reader)?;
                    Ok(CompressedFileReader {
                        reader: CompressedReaderKind::ZstdC(inflater),
                    })
                }
                #[cfg(all(feature = "zstd_rust", not(feature = "zstd_c")))]
                {
                    let inflater = StreamingDecoder::new(reader).map_err(|e| {
                        Eu4ErrorKind::InvalidSyntax(format!(
                            "Failed to create ruzstd decoder: {}",
                            e
                        ))
                    })?;
                    Ok(CompressedFileReader {
                        reader: CompressedReaderKind::ZstdRust(inflater),
                    })
                }
            }
            _ => Err(Eu4ErrorKind::UnknownCompression.into()),
        }
    }
}

impl<R> std::io::Read for CompressedFileReader<R>
where
    R: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match &mut self.reader {
            CompressedReaderKind::Deflate(reader) => reader.read(buf),
            #[cfg(feature = "zstd_c")]
            CompressedReaderKind::ZstdC(reader) => reader.read(buf),
            #[cfg(feature = "zstd_rust")]
            CompressedReaderKind::ZstdRust(reader) => reader.read(buf),
        }
    }
}

/// Name of the EU4 entry
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Eu4FileEntryName {
    Gamestate,
    Meta,
    Ai,
}

impl Display for Eu4FileEntryName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eu4FileEntryName::Meta => write!(f, "meta"),
            Eu4FileEntryName::Gamestate => write!(f, "gamestate"),
            Eu4FileEntryName::Ai => write!(f, "ai"),
        }
    }
}

/// A parsed EU4 text document
pub struct Eu4ParsedText<'a> {
    tape: TextTape<'a>,
}

impl<'a> Eu4ParsedText<'a> {
    /// Parse EU4 text data that has the "EU4txt" header
    pub fn from_slice(data: &'a [u8]) -> Result<Self, Eu4Error> {
        file_header(data)
            .filter(|(header, _)| matches!(header, FileHeader::Text))
            .map(|(_, data)| data)
            .ok_or_else(|| Eu4ErrorKind::UnknownHeader.into())
            .and_then(Self::from_raw)
    }

    /// Parse headerless EU4 text data
    pub fn from_raw(data: &'a [u8]) -> Result<Self, Eu4Error> {
        let tape = TextTape::from_slice(data).map_err(Eu4ErrorKind::Parse)?;
        Ok(Eu4ParsedText { tape })
    }

    pub fn reader(&self) -> ObjectReader<'_, '_, Eu4Flavor> {
        ObjectReader::new(&self.tape, Eu4Flavor::new())
    }
}
