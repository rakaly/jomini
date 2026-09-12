use crate::{flavor::flavor_reader, melt, Ck3Error, Ck3ErrorKind, MeltOptions};
use jomini::{
    binary::{
        de::BinaryReaderDeserializer, BinaryDeserializerBuilder, BinaryFlavor, TokenResolver,
    },
    text::de::TextReaderDeserializer,
    Utf8Encoding,
};
use serde::de::DeserializeOwned;
use std::io::{Read, Write};

pub use jomini::envelope::JominiFile as Ck3File;
pub use jomini::envelope::*;

/// Type alias for Ck3 text deserializer
///
/// A lazy way to avoid the need to reimplement deserializer
pub type Ck3TextDeserializer<'r> = TextReaderDeserializer<'r, Utf8Encoding>;
pub type Ck3BinaryDeserializer<'r, 'res, RES> =
    BinaryReaderDeserializer<'r, 'res, RES, Box<dyn BinaryFlavor>>;

pub trait Ck3BinaryDeserialization {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> Result<Ck3BinaryDeserializer<'_, 'res, RES>, Ck3Error>;
}

impl<R: ReaderAt> Ck3BinaryDeserialization for &'_ SaveData<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> Result<Ck3BinaryDeserializer<'_, 'res, RES>, Ck3Error> {
        let (read, flavor) = flavor_reader(self.body().cursor())?;

        let deser = BinaryDeserializerBuilder::with_flavor(flavor as Box<dyn BinaryFlavor>)
            .from_reader(read, resolver);

        Ok(deser)
    }
}

impl<R: Read> Ck3BinaryDeserialization for SaveContent<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> Result<Ck3BinaryDeserializer<'_, 'res, RES>, Ck3Error> {
        let (read, flavor) = flavor_reader(self)?;

        let deser = BinaryDeserializerBuilder::with_flavor(flavor as Box<dyn BinaryFlavor>)
            .from_reader(read, resolver);

        Ok(deser)
    }
}

impl<R: Read> Ck3BinaryDeserialization for SaveMetadata<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> Result<Ck3BinaryDeserializer<'_, 'res, RES>, Ck3Error> {
        let (read, flavor) = flavor_reader(self)?;

        let deser = BinaryDeserializerBuilder::with_flavor(flavor as Box<dyn BinaryFlavor>)
            .from_reader(read, resolver);

        Ok(deser)
    }
}

pub trait Ck3Melt {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write;
}

pub trait Ck3TextMelt {
    fn melt<Writer>(&mut self, output: Writer) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Writer: Write;
}

impl<R: ReaderAt> Ck3Melt for &'_ Ck3File<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match self.gamestate().map_err(Ck3ErrorKind::from)? {
            SaveContentKind::Text(mut save_body) => {
                let mut new_header = self.header().clone();
                new_header.set_kind(SaveHeaderKind::Text);
                new_header.write(&mut output)?;
                std::io::copy(&mut save_body, &mut output)?;
                Ok(melt::MeltedDocument::new())
            }
            SaveContentKind::Binary(mut save_body) => melt::melt(
                &mut save_body,
                &mut output,
                resolver,
                options,
                self.header().clone(),
            ),
        }
    }
}

impl<R: ReaderAt> Ck3Melt for &'_ JominiZip<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match self.gamestate().map_err(Ck3ErrorKind::from)? {
            SaveContentKind::Text(mut save_body) => {
                let mut new_header = self.header().clone();
                new_header.set_kind(SaveHeaderKind::Text);
                new_header.write(&mut output)?;
                std::io::copy(&mut save_body, &mut output)?;
                Ok(melt::MeltedDocument::new())
            }
            SaveContentKind::Binary(mut save_body) => melt::melt(
                &mut save_body,
                &mut output,
                resolver,
                options,
                self.header().clone(),
            ),
        }
    }
}

impl<R: ReaderAt> Ck3Melt for &'_ SaveData<BinaryEncoding, R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        melt::melt(
            &mut self.body().cursor(),
            &mut output,
            resolver,
            options,
            self.header().clone(),
        )
    }
}

impl<R: Read> Ck3Melt for SaveMetadataKind<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match self {
            SaveMetadataKind::Text(x) => x.melt(output),
            SaveMetadataKind::Binary(x) => x.melt(options, resolver, output),
        }
    }
}

impl<R: ReaderAt> Ck3TextMelt for &'_ SaveData<TextEncoding, R> {
    fn melt<Writer>(&mut self, mut output: Writer) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Writer: Write,
    {
        let mut new_header = self.header().clone();
        new_header.set_kind(SaveHeaderKind::Text);
        new_header.write(&mut output)?;
        std::io::copy(&mut self.body().cursor(), &mut output)?;
        Ok(melt::MeltedDocument::new())
    }
}

impl<R: Read> Ck3TextMelt for SaveMetadata<TextEncoding, R> {
    fn melt<Writer>(&mut self, mut output: Writer) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Writer: Write,
    {
        let mut new_header = self.header().clone();
        new_header.set_kind(SaveHeaderKind::Text);
        new_header.write(&mut output)?;
        std::io::copy(self, &mut output)?;
        Ok(melt::MeltedDocument::new())
    }
}

impl<R: Read> Ck3Melt for SaveMetadata<BinaryEncoding, R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, Ck3Error>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        let header = self.header().clone();
        melt::melt(self, output, resolver, options, header)
    }
}

pub trait DeserializeCk3 {
    fn deserialize<T>(&mut self, resolver: impl TokenResolver) -> Result<T, Ck3Error>
    where
        T: DeserializeOwned;
}

impl<R: ReaderAt> DeserializeCk3 for &'_ Ck3File<R> {
    fn deserialize<T>(&mut self, resolver: impl TokenResolver) -> Result<T, Ck3Error>
    where
        T: DeserializeOwned,
    {
        match self.kind() {
            JominiFileKind::Uncompressed(SaveDataKind::Text(x)) => Ok(x
                .deserializer()
                .deserialize()
                .map_err(Ck3ErrorKind::Deserialize)?),
            JominiFileKind::Uncompressed(SaveDataKind::Binary(x)) => Ok((&*x)
                .deserializer(&resolver)?
                .deserialize()
                .map_err(Ck3ErrorKind::Deserialize)?),
            JominiFileKind::Zip(x) => Ok(match x.gamestate().map_err(Ck3ErrorKind::Envelope)? {
                SaveContentKind::Text(mut x) => x
                    .deserializer()
                    .deserialize()
                    .map_err(Ck3ErrorKind::Deserialize)?,
                SaveContentKind::Binary(mut x) => x
                    .deserializer(&resolver)?
                    .deserialize()
                    .map_err(Ck3ErrorKind::Deserialize)?,
            }),
        }
    }
}
