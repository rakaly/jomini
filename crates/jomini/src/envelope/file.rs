use crate::envelope::{
    SaveHeader,
    errors::{EnvelopeError, EnvelopeErrorKind},
};
#[cfg(feature = "serde")]
use crate::{
    TextDeserializer, Utf8Encoding,
    text::{self, de::TextReaderDeserializer},
};
use rawzip::{
    CompressionMethod, FileReader, RangeReader, ReaderAt, ZipArchiveEntryWayfinder, ZipReader,
};
use std::{
    fs::File,
    io::{Cursor, Read},
    ops::Range,
};

/// Read modern Paradox save files from memory or from the file system
#[derive(Debug, Clone)]
pub struct JominiFile<R> {
    kind: JominiFileKind<R>,
}

impl JominiFile<()> {
    /// Creates a Jomini file from a slice of data
    pub fn from_slice<R>(data: R) -> Result<JominiFile<Cursor<R>>, EnvelopeError>
    where
        R: AsRef<[u8]>,
    {
        let header = SaveHeader::from_slice(data.as_ref())?;
        let header_len = header.header_len() as u64;

        // Before looking at the kind of header we just parsed, always attempt
        // to decode the end of the central directory from the end of the input.
        // This allows us to handle misleading headers that claim to be
        // uncompressed but are actually Zip saves. The performance cost is
        // minimal as scanning up to 64KB from the end of the input is cheap.
        match rawzip::ZipArchive::with_max_search_space(64 * 1024).locate_in_slice(data) {
            Ok(archive) => {
                let archive = archive.into_cursor_archive();
                let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];
                let zip = JominiZip::try_from_archive(archive, &mut buf, header.clone())?;
                Ok(JominiFile {
                    kind: JominiFileKind::Zip(zip),
                })
            }
            Err((data, _)) if header.kind().is_binary() => {
                let mut cursor = Cursor::new(data);
                cursor.set_position(header_len);
                Ok(JominiFile {
                    kind: JominiFileKind::Uncompressed(SaveDataKind::Binary(SaveData::new(
                        header,
                        SaveContent::with_offset(cursor, header_len),
                    ))),
                })
            }
            Err((data, _)) => {
                let mut cursor = Cursor::new(data);
                cursor.set_position(header_len);
                Ok(JominiFile {
                    kind: JominiFileKind::Uncompressed(SaveDataKind::Text(SaveData::new(
                        header,
                        SaveContent::with_offset(cursor, header_len),
                    ))),
                })
            }
        }
    }

    /// Creates a Jomini file from a file handle
    pub fn from_file(mut file: File) -> Result<JominiFile<FileReader>, EnvelopeError> {
        let mut buf = [0u8; SaveHeader::SIZE];
        file.read_exact(&mut buf)?;
        let header = SaveHeader::from_slice(&buf)?;
        let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];

        let archive =
            rawzip::ZipArchive::with_max_search_space(64 * 1024).locate_in_file(file, &mut buf);

        match archive {
            Ok(archive) => {
                let zip = JominiZip::try_from_archive(archive, &mut buf, header.clone())?;
                Ok(JominiFile {
                    kind: JominiFileKind::Zip(zip),
                })
            }
            Err((file, _)) => {
                let reader = FileReader::from(file);
                if header.kind().is_binary() {
                    let body = SaveContent::with_offset(reader, header.header_len() as u64);
                    Ok(JominiFile {
                        kind: JominiFileKind::Uncompressed(SaveDataKind::Binary(SaveData::new(
                            header, body,
                        ))),
                    })
                } else {
                    let body = SaveContent::with_offset(reader, header.header_len() as u64);
                    Ok(JominiFile {
                        kind: JominiFileKind::Uncompressed(SaveDataKind::Text(SaveData::new(
                            header, body,
                        ))),
                    })
                }
            }
        }
    }
}

/// The underlying kind of Jomini file: either ZIP-compressed or uncompressed
#[derive(Debug, Clone)]
pub enum JominiFileKind<R> {
    /// Uncompressed save data (text or binary)
    Uncompressed(SaveDataKind<R>),
    /// ZIP-compressed archive with gamestate and metadata
    Zip(JominiZip<R>),
}

impl<R> JominiFile<R> {
    /// Returns a reference to the file kind (compressed or uncompressed)
    pub fn kind(&self) -> &JominiFileKind<R> {
        &self.kind
    }

    /// Returns a mutable reference to the file kind
    pub fn kind_mut(&mut self) -> &mut JominiFileKind<R> {
        &mut self.kind
    }

    /// Returns the save file header with version and format information
    pub fn header(&self) -> &SaveHeader {
        match self.kind() {
            JominiFileKind::Zip(archive) => archive.header(),
            JominiFileKind::Uncompressed(x) => x.header(),
        }
    }
}

impl<R: ReaderAt> JominiFile<R> {
    /// Returns the gamestate reader
    ///
    /// If the gamestate is compressed, the reader will return decompressed data
    pub fn gamestate(&self) -> Result<SaveContentKind<Box<dyn Read + '_>>, EnvelopeError> {
        match self.kind() {
            JominiFileKind::Zip(archive) => {
                let gamestate = archive.gamestate()?;
                match gamestate {
                    SaveContentKind::Text(body) => {
                        let reader = body.into_inner();
                        Ok(SaveContentKind::Text(SaveContent::new(
                            Box::new(reader) as Box<dyn Read>
                        )))
                    }
                    SaveContentKind::Binary(x) => {
                        let reader = x.into_inner();
                        Ok(SaveContentKind::Binary(SaveContent::new(
                            Box::new(reader) as Box<dyn Read>
                        )))
                    }
                }
            }
            JominiFileKind::Uncompressed(SaveDataKind::Text(x)) => Ok(SaveContentKind::Text(
                SaveContent::new(Box::new(x.body().cursor()) as Box<dyn Read>),
            )),
            JominiFileKind::Uncompressed(SaveDataKind::Binary(x)) => Ok(SaveContentKind::Binary(
                SaveContent::new(Box::new(x.body().cursor()) as Box<dyn Read>),
            )),
        }
    }

    /// Returns the metadata reader
    ///
    /// If the metadata is compressed, the reader will return decompressed data
    pub fn meta(&self) -> Result<SaveMetadataKind<Box<dyn Read + '_>>, EnvelopeError> {
        match self.kind() {
            JominiFileKind::Zip(archive) => archive.meta(),
            JominiFileKind::Uncompressed(x) => match x.meta() {
                SaveMetadataKind::Text(m) => Ok(SaveMetadataKind::Text(SaveMetadata::new(
                    Box::new(m.reader) as Box<dyn Read>,
                    m.header,
                ))),
                SaveMetadataKind::Binary(m) => Ok(SaveMetadataKind::Binary(SaveMetadata::new(
                    Box::new(m.reader) as Box<dyn Read>,
                    m.header,
                ))),
            },
        }
    }
}

/// ZIP-compressed Jomini save file with separately accessible gamestate and metadata
#[derive(Debug, Clone)]
pub struct JominiZip<R> {
    pub(crate) archive: rawzip::ZipArchive<R>,
    pub(crate) metadata: MetaFormatKind,
    pub(crate) gamestate: (CompressionMethod, ZipArchiveEntryWayfinder),
    pub(crate) header: SaveHeader,
}

impl<R> JominiZip<R> {
    /// Returns the save file header with version and format information
    pub fn header(&self) -> &SaveHeader {
        &self.header
    }

    /// Returns the uncompressed size of the gamestate entry as a hint
    pub fn gamestate_uncompressed_hint(&self) -> u64 {
        self.gamestate.1.uncompressed_size_hint()
    }
}

impl<R> JominiZip<R>
where
    R: ReaderAt,
{
    /// Creates a JominiZip from a ZIP archive, locating the gamestate and metadata entries
    pub fn try_from_archive(
        archive: rawzip::ZipArchive<R>,
        buf: &mut [u8],
        header: SaveHeader,
    ) -> Result<Self, EnvelopeError> {
        let mut offset = archive.directory_offset();
        let mut entries = archive.entries(buf);
        let mut gamestate = None;
        let mut meta = None;

        while let Some(entry) = entries.next_entry().map_err(EnvelopeErrorKind::Zip)? {
            offset = offset.min(entry.local_header_offset());
            match entry.file_path().as_ref() {
                b"gamestate" => gamestate = Some((entry.compression_method(), entry.wayfinder())),
                b"meta" => meta = Some((entry.compression_method(), entry.wayfinder())),
                _ => {}
            };
        }

        let gamestate =
            gamestate.ok_or_else(|| EnvelopeErrorKind::ZipMissingEntry("gamestate".to_string()))?;

        let metadata = match meta {
            Some(meta) => MetaFormatKind::Zip(meta),
            None => MetaFormatKind::Inlined(
                header.header_len()..(offset as usize).max(header.header_len()),
            ),
        };

        Ok(JominiZip {
            archive,
            metadata,
            gamestate,
            header,
        })
    }

    /// Return the metadata reader based on whether it's inlined or in the zip
    pub fn meta(&self) -> Result<SaveMetadataKind<Box<dyn Read + '_>>, EnvelopeError> {
        let reader = match &self.metadata {
            MetaFormatKind::Inlined(range) => {
                let meta_reader =
                    RangeReader::new(self.archive.get_ref(), range.start as u64..range.end as u64);
                Box::new(meta_reader) as Box<dyn Read>
            }
            MetaFormatKind::Zip((compression, wayfinder)) => {
                let zip_entry = self
                    .archive
                    .get_entry(*wayfinder)
                    .map_err(EnvelopeErrorKind::Zip)?;

                let reader = CompressedReader::from_zip(*compression, zip_entry)?;
                Box::new(reader) as Box<dyn Read>
            }
        };

        if self.header.kind().is_text() {
            Ok(SaveMetadataKind::Text(SaveMetadata::new(
                reader,
                self.header.clone(),
            )))
        } else {
            Ok(SaveMetadataKind::Binary(SaveMetadata::new(
                reader,
                self.header.clone(),
            )))
        }
    }

    /// Opens the gamestate entry, returning the decompressing reader alongside
    /// the expected verification recorded in the archive.
    fn gamestate_entry(&self) -> Result<(ZipEntry<&R>, SaveVerification), EnvelopeError> {
        let (compression, wayfinder) = &self.gamestate;
        let zip_entry = self
            .archive
            .get_entry(*wayfinder)
            .map_err(EnvelopeErrorKind::Zip)?;
        let expected = SaveVerification::from_zip(zip_entry.reader().claim_verifier());
        let reader = CompressedReader::from_zip(*compression, zip_entry)?;
        Ok((ZipEntry { reader }, expected))
    }

    /// Wraps a reader in the text/binary content kind dictated by the header.
    fn wrap_content<T>(&self, reader: T) -> SaveContentKind<T> {
        if self.header.kind().is_text() {
            SaveContentKind::Text(SaveContent::new(reader))
        } else {
            SaveContentKind::Binary(SaveContent::new(reader))
        }
    }

    /// Returns the gamestate reader, decompressing if necessary
    pub fn gamestate(&self) -> Result<SaveContentKind<ZipEntry<&R>>, EnvelopeError> {
        let (entry, _) = self.gamestate_entry()?;
        Ok(self.wrap_content(entry))
    }

    /// Returns a gamestate reader that verifies the decompressed data against
    /// the CRC32 and size recorded in the archive.
    ///
    /// Verification runs when the reader is read to EOF, or on demand via
    /// [`VerifyingReader::finish`]. See [`VerifyingReader`] for the details.
    pub fn gamestate_verified(
        &self,
    ) -> Result<SaveContentKind<VerifyingReader<ZipEntry<&R>>>, EnvelopeError> {
        let (entry, expected) = self.gamestate_entry()?;
        Ok(self.wrap_content(VerifyingReader::new(entry, expected)))
    }

    /// Returns a reader for a file in the ZIP archive by path
    ///
    /// Useful for retrieving additional files beyond the gamestate and
    /// metadata.
    ///
    /// Will return a `EnvelopeErrorKind::ZipMissingEntry` if the requested path
    /// is not found.
    pub fn read_entry(&self, path: &str) -> Result<impl Read + '_, EnvelopeError> {
        let path_bytes = path.as_bytes();
        let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];

        let mut entries = self.archive.entries(&mut buf);
        while let Some(entry) = entries.next_entry().map_err(EnvelopeErrorKind::Zip)? {
            if entry.file_path().as_ref() == path_bytes {
                let zip_entry = self
                    .archive
                    .get_entry(entry.wayfinder())
                    .map_err(EnvelopeErrorKind::Zip)?;

                return CompressedReader::from_zip(entry.compression_method(), zip_entry);
            }
        }

        Err(EnvelopeErrorKind::ZipMissingEntry(path.to_string()).into())
    }

    /// Returns a reader for a file in the ZIP archive by path that verifies the
    /// decompressed data against the CRC32 and size recorded in the archive.
    ///
    /// Like [`JominiZip::read_entry`], but the returned [`VerifyingReader`]
    /// validates the entry when read to EOF or via [`VerifyingReader::finish`].
    pub fn read_entry_verified(
        &self,
        path: &str,
    ) -> Result<VerifyingReader<ZipEntry<&R>>, EnvelopeError> {
        let path_bytes = path.as_bytes();
        let mut buf = vec![0u8; rawzip::RECOMMENDED_BUFFER_SIZE];

        let mut entries = self.archive.entries(&mut buf);
        while let Some(entry) = entries.next_entry().map_err(EnvelopeErrorKind::Zip)? {
            if entry.file_path().as_ref() == path_bytes {
                let zip_entry = self
                    .archive
                    .get_entry(entry.wayfinder())
                    .map_err(EnvelopeErrorKind::Zip)?;

                let expected = SaveVerification::from_zip(zip_entry.reader().claim_verifier());
                let reader = CompressedReader::from_zip(entry.compression_method(), zip_entry)?;
                return Ok(VerifyingReader::new(ZipEntry { reader }, expected));
            }
        }

        Err(EnvelopeErrorKind::ZipMissingEntry(path.to_string()).into())
    }
}

/// Marker type for text-encoded save file content
#[derive(Debug, Clone)]
pub struct TextEncoding;

/// Marker type for binary-encoded save file content
#[derive(Debug, Clone)]
pub struct BinaryEncoding;

/// Save file data with header and encoded content
#[derive(Debug, Clone)]
pub struct SaveData<E, R> {
    header: SaveHeader,
    body: SaveContent<E, R>,
}

impl<E, R> SaveData<E, R> {
    fn new(header: SaveHeader, body: SaveContent<E, R>) -> Self {
        SaveData { header, body }
    }

    /// Returns the save file header with version and format information
    pub fn header(&self) -> &SaveHeader {
        &self.header
    }

    /// Returns a reference to the encoded content
    pub fn body(&self) -> &SaveContent<E, R> {
        &self.body
    }

    /// Returns a mutable reference to the encoded content
    pub fn body_mut(&mut self) -> &mut SaveContent<E, R> {
        &mut self.body
    }
}

impl<E, R: ReaderAt> SaveData<E, R> {
    /// Returns a reader for the metadata section
    pub fn meta(&self) -> SaveContent<E, RangeReader<&R>> {
        let header_len = self.header().header_len() as u64;
        let meta_reader = RangeReader::new(
            self.body.get_ref(),
            header_len..header_len + self.header().metadata_len(),
        );

        SaveContent::new(meta_reader)
    }
}

#[cfg(feature = "serde")]
impl<R: ReaderAt> SaveData<TextEncoding, R> {
    /// Creates a text deserializer for this save data
    pub fn deserializer(&self) -> TextReaderDeserializer<'_, Utf8Encoding> {
        text_deserializer(self.body.cursor())
    }
}

/// Save data with dynamic encoding type (text or binary)
#[derive(Debug, Clone)]
pub enum SaveDataKind<R> {
    /// Text-encoded save data
    Text(SaveData<TextEncoding, R>),
    /// Binary-encoded save data
    Binary(SaveData<BinaryEncoding, R>),
}

impl<R> SaveDataKind<R> {
    /// Returns the save file header with version and format information
    pub fn header(&self) -> &SaveHeader {
        match self {
            SaveDataKind::Text(data) => data.header(),
            SaveDataKind::Binary(data) => data.header(),
        }
    }

    /// Returns a reference to the save data with borrowed content
    pub fn as_ref(&self) -> SaveDataKind<&R> {
        match self {
            SaveDataKind::Text(data) => {
                SaveDataKind::Text(SaveData::new(data.header().clone(), data.body().as_ref()))
            }
            SaveDataKind::Binary(data) => {
                SaveDataKind::Binary(SaveData::new(data.header().clone(), data.body().as_ref()))
            }
        }
    }
}

/// Indicates where metadata is stored in a ZIP file: either inlined or as a separate entry
#[derive(Debug, Clone)]
pub enum MetaFormatKind {
    /// Metadata is inlined between header and gamestate
    Inlined(Range<usize>),
    /// Metadata is stored as a separate ZIP entry with compression info
    Zip((CompressionMethod, ZipArchiveEntryWayfinder)),
}

/// Extracted metadata with dynamic encoding type (text or binary).
///
/// Allows working with metadata without knowing the encoding type at compile
/// time.
pub enum SaveMetadataKind<R> {
    /// Text-encoded metadata
    Text(SaveMetadata<TextEncoding, R>),
    /// Binary-encoded metadata (requires token resolver for deserialization)
    Binary(SaveMetadata<BinaryEncoding, R>),
}

impl<R: Read> Read for SaveMetadataKind<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            SaveMetadataKind::Text(meta) => meta.read(buf),
            SaveMetadataKind::Binary(meta) => meta.read(buf),
        }
    }
}

impl<R: ReaderAt> SaveDataKind<R> {
    /// Returns the metadata section
    pub fn meta(&self) -> SaveMetadataKind<RangeReader<&R>> {
        let header = self.header().clone();

        // Get the body content offset and reader
        let (body_offset, reader) = match self {
            SaveDataKind::Text(data) => (data.body.content_offset(), data.body.get_ref()),
            SaveDataKind::Binary(data) => (data.body.content_offset(), data.body.get_ref()),
        };

        // Metadata starts right after the header, accounting for the body offset
        let metadata_range = body_offset..body_offset + header.metadata_len();

        let meta_reader = RangeReader::new(reader, metadata_range.clone());

        if header.kind().is_text() {
            SaveMetadataKind::Text(SaveMetadata::new(meta_reader, header))
        } else {
            SaveMetadataKind::Binary(SaveMetadata::new(meta_reader, header))
        }
    }

    /// Returns the gamestate section
    pub fn gamestate(&self) -> SaveContentKind<&R> {
        match self {
            SaveDataKind::Text(data) => SaveContentKind::Text(data.body.as_ref()),
            SaveDataKind::Binary(data) => SaveContentKind::Binary(data.body.as_ref()),
        }
    }
}

/// Save file metadata with encoding information that is often separate from
/// gamestate data
#[derive(Debug, Clone)]
pub struct SaveMetadata<E, R> {
    reader: R,
    header: SaveHeader,
    _encoding: std::marker::PhantomData<E>,
}

impl<E, R> SaveMetadata<E, R> {
    /// Creates a new metadata reader
    pub fn new(reader: R, header: SaveHeader) -> Self {
        SaveMetadata {
            reader,
            header,
            _encoding: std::marker::PhantomData,
        }
    }

    /// Returns the save file header with version and format information
    pub fn header(&self) -> &SaveHeader {
        &self.header
    }
}

impl<R: Read> Read for SaveMetadata<TextEncoding, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

impl<R: Read> Read for SaveMetadata<BinaryEncoding, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

#[cfg(feature = "serde")]
impl<R: Read> SaveMetadata<TextEncoding, R> {
    /// Creates a text deserializer for this metadata
    pub fn deserializer(&mut self) -> TextReaderDeserializer<'_, Utf8Encoding> {
        text_deserializer(&mut self.reader)
    }
}

/// Save content with dynamic encoding type (text or binary).
#[derive(Debug, Clone)]
pub enum SaveContentKind<R> {
    /// Text-encoded content
    Text(SaveContent<TextEncoding, R>),
    /// Binary-encoded content
    Binary(SaveContent<BinaryEncoding, R>),
}

impl<R: Read> Read for SaveContentKind<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            SaveContentKind::Text(body) => body.read(buf),
            SaveContentKind::Binary(body) => body.read(buf),
        }
    }
}

/// Save file content (metadata or gamestate) with encoding information.
#[derive(Debug, Clone)]
pub struct SaveContent<E, R> {
    reader: R,
    content_offset: u64,
    encoding: std::marker::PhantomData<E>,
}

impl<R> SaveContent<(), R> {
    /// Creates a new save content reader
    fn new<E>(reader: R) -> SaveContent<E, R> {
        SaveContent {
            reader,
            content_offset: 0,
            encoding: std::marker::PhantomData,
        }
    }

    /// Creates a new save content reader with a content offset
    fn with_offset<E>(reader: R, content_offset: u64) -> SaveContent<E, R> {
        SaveContent {
            reader,
            content_offset,
            encoding: std::marker::PhantomData,
        }
    }
}

impl<R, E> SaveContent<E, R> {
    /// Returns a reference to the underlying reader.
    ///
    /// This does not apply [`SaveContent::content_offset`]. For content created
    /// with [`JominiFile::from_slice`], this returns the full original slice,
    /// including header bytes.
    ///
    /// # Example
    ///
    /// ```rust
    /// use jomini::envelope::{JominiFile, JominiFileKind, SaveDataKind};
    ///
    /// let data = std::fs::read("tests/fixtures/envelopes/text.txt").unwrap();
    /// let file = JominiFile::from_slice(&data).unwrap();
    /// let JominiFileKind::Uncompressed(SaveDataKind::Text(save_data)) = file.kind() else {
    ///     panic!("expected text uncompressed");
    /// };
    ///
    /// let body = save_data.body();
    /// let source = body.get_ref().get_ref().as_slice();
    /// let body_slice = &source[body.content_offset() as usize..];
    ///
    /// assert!(!body_slice.starts_with(b"SAV"));
    /// ```
    pub fn get_ref(&self) -> &R {
        &self.reader
    }

    /// Consumes this content and returns the inner reader.
    ///
    /// Useful for recovering the underlying reader, e.g. to call
    /// [`VerifyingReader::finish`] on the gamestate returned by
    /// [`JominiZip::gamestate_verified`].
    pub fn into_inner(self) -> R {
        self.reader
    }

    /// Returns a borrowed reference to this content
    pub fn as_ref(&self) -> SaveContent<E, &R> {
        SaveContent::with_offset(&self.reader, self.content_offset)
    }

    /// Returns a mutable borrowed reference to this content
    pub fn as_mut(&mut self) -> SaveContent<E, &mut R> {
        SaveContent::with_offset(&mut self.reader, self.content_offset)
    }

    /// Returns the offset of the content within the reader
    pub fn content_offset(&self) -> u64 {
        self.content_offset
    }
}

impl<E, R: ReaderAt> SaveContent<E, R> {
    /// Creates a Read adapter for this content
    pub fn cursor(&self) -> ReaderAtCursor<'_, R> {
        ReaderAtCursor::new_at(&self.reader, self.content_offset)
    }
}

impl<R: Read, E> Read for SaveContent<E, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

#[cfg(feature = "serde")]
impl<R: Read> SaveContent<TextEncoding, R> {
    /// Creates a text deserializer for this content
    pub fn deserializer(&mut self) -> TextReaderDeserializer<'_, Utf8Encoding> {
        text_deserializer(&mut self.reader)
    }
}

/// Wrapper around decompressed zip gamestate that implements Read and provides deserializer
#[derive(Debug)]
pub struct ZipEntry<R> {
    reader: CompressedReader<rawzip::ZipReader<R>>,
}

impl<R: ReaderAt> Read for ZipEntry<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

/// Wrapper that adapts ReaderAt to the Read trait
pub struct ReaderAtCursor<'a, R> {
    reader: &'a R,
    position: u64,
}

impl<'a, R: ReaderAt> ReaderAtCursor<'a, R> {
    fn new_at(reader: &'a R, position: u64) -> Self {
        ReaderAtCursor { reader, position }
    }
}

impl<'a, R: ReaderAt> Read for ReaderAtCursor<'a, R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let pos = self.position;
        let n = self.reader.read_at(buf, pos)?;
        self.position += n as u64;
        Ok(n)
    }
}

/// Wrapper that decompresses deflate-encoded data from a ZIP entry
#[derive(Debug)]
pub struct CompressedReader<R> {
    reader: flate2::read::DeflateDecoder<R>,
}

impl CompressedReader<()> {
    fn from_zip<R>(
        compression: CompressionMethod,
        zip_entry: rawzip::ZipEntry<'_, R>,
    ) -> Result<CompressedReader<ZipReader<&R>>, EnvelopeError>
    where
        R: ReaderAt,
    {
        if compression != CompressionMethod::DEFLATE {
            return Err(EnvelopeErrorKind::ZipUnsupportedCompression.into());
        }
        let reader = zip_entry.reader();
        let reader = flate2::read::DeflateDecoder::new(reader);
        Ok(CompressedReader { reader })
    }
}

impl<R> std::io::Read for CompressedReader<R>
where
    R: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.reader.read(buf)
    }
}

/// CRC32 engine selected per target.
///
/// On native platforms this uses flate2's `zlib-rs` backend, which has SIMD
/// implementations on x86_64 and aarch64. On wasm, where `zlib-rs` has no SIMD
/// path, it uses rawzip's table-based implementation instead.
#[cfg(not(target_family = "wasm"))]
#[derive(Debug)]
struct CrcEngine(flate2::Crc);

#[cfg(not(target_family = "wasm"))]
impl CrcEngine {
    fn new() -> Self {
        CrcEngine(flate2::Crc::new())
    }

    fn update(&mut self, data: &[u8]) {
        self.0.update(data);
    }

    fn checksum(&self) -> u32 {
        self.0.sum()
    }
}

#[cfg(target_family = "wasm")]
#[derive(Debug)]
struct CrcEngine(rawzip::Crc32);

#[cfg(target_family = "wasm")]
impl CrcEngine {
    fn new() -> Self {
        CrcEngine(rawzip::Crc32::new())
    }

    fn update(&mut self, data: &[u8]) {
        self.0.update(data);
    }

    fn checksum(&self) -> u32 {
        self.0.checksum()
    }
}

/// The expected CRC32 checksum and uncompressed size of a save entry.
///
/// These values are recorded in the ZIP central directory and are used by
/// [`VerifyingReader`] to validate decompressed data.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SaveVerification {
    crc: u32,
    uncompressed_size: u64,
}

impl SaveVerification {
    /// Creates a verification from an expected checksum and uncompressed size.
    ///
    /// Useful for wrapping a decompressor in a [`VerifyingReader`] when the
    /// expected values are known from another source.
    pub fn new(crc: u32, uncompressed_size: u64) -> Self {
        SaveVerification {
            crc,
            uncompressed_size,
        }
    }

    /// The expected CRC32 checksum of the decompressed data.
    pub fn crc(&self) -> u32 {
        self.crc
    }

    /// The expected size of the decompressed data in bytes.
    pub fn uncompressed_size(&self) -> u64 {
        self.uncompressed_size
    }

    pub(crate) fn from_zip(value: rawzip::ZipVerification) -> Self {
        SaveVerification {
            crc: value.crc,
            uncompressed_size: value.uncompressed_size,
        }
    }
}

/// A reader that verifies decompressed data against the checksum and size
/// recorded in the archive.
///
/// The check runs once the underlying stream reaches EOF, so reading the entry
/// to completion validates it automatically. A caller that would otherwise stop
/// early can force the check by calling [`VerifyingReader::finish`]. Note that a
/// partially-read reader that is simply dropped performs no verification.
#[derive(Debug)]
pub struct VerifyingReader<R> {
    reader: R,
    expected: SaveVerification,
    crc: CrcEngine,
    size: u64,
}

impl<R> VerifyingReader<R> {
    /// Wraps a decompressed reader with the expected verification values.
    pub fn new(reader: R, expected: SaveVerification) -> Self {
        VerifyingReader {
            reader,
            expected,
            crc: CrcEngine::new(),
            size: 0,
        }
    }

    /// Returns the expected checksum and size this reader validates against.
    pub fn verification(&self) -> SaveVerification {
        self.expected
    }

    /// Consumes this reader, returning the inner reader without verifying.
    pub fn into_inner(self) -> R {
        self.reader
    }

    fn verify(&self) -> Result<(), EnvelopeError> {
        if self.size != self.expected.uncompressed_size {
            return Err(EnvelopeErrorKind::SizeMismatch {
                expected: self.expected.uncompressed_size,
                actual: self.size,
            }
            .into());
        }

        let actual = self.crc.checksum();
        if actual != self.expected.crc {
            return Err(EnvelopeErrorKind::ChecksumMismatch {
                expected: self.expected.crc,
                actual,
            }
            .into());
        }

        Ok(())
    }
}

impl<R: Read> VerifyingReader<R> {
    /// Reads from the inner reader, accumulating the CRC and size but performing
    /// no validation.
    fn fill(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let n = self.reader.read(buf)?;
        self.crc.update(&buf[..n]);
        self.size += n as u64;
        Ok(n)
    }

    /// Reads any remaining data, then verifies the checksum and size.
    ///
    /// Returns the inner reader on success so the decompressor can be recovered.
    /// A checksum or size mismatch is returned as an [`EnvelopeError`].
    pub fn finish(mut self) -> Result<R, EnvelopeError> {
        // Drain through the accumulate-only view so a genuine IO error surfaces
        // as `EnvelopeErrorKind::Io` while the check below yields the owned
        // checksum/size error rather than an IO-wrapped one.
        std::io::copy(&mut Accumulate(&mut self), &mut std::io::sink())?;
        self.verify()?;
        Ok(self.reader)
    }
}

/// Accumulates CRC/size without triggering the inline end-of-stream validation,
/// letting [`VerifyingReader::finish`] surface the owned error.
struct Accumulate<'a, R>(&'a mut VerifyingReader<R>);

impl<R: Read> Read for Accumulate<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.fill(buf)
    }
}

impl<R: Read> Read for VerifyingReader<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        let n = self.fill(buf)?;
        if n == 0 || self.size >= self.expected.uncompressed_size {
            self.verify()
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        }
        Ok(n)
    }
}

#[cfg(feature = "serde")]
fn text_deserializer<'r, R: Read + 'r>(reader: R) -> TextReaderDeserializer<'r, Utf8Encoding> {
    TextDeserializer::from_utf8_reader(text::TokenReader::new(reader))
}
