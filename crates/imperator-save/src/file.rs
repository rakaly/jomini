use crate::{flavor::ImperatorFlavor, melt, ImperatorError, ImperatorErrorKind, MeltOptions};
use jomini::{
    binary::{de::BinaryReaderDeserializer, BinaryDeserializerBuilder, TokenResolver},
    text::de::TextReaderDeserializer,
    Utf8Encoding,
};
use serde::de::DeserializeOwned;
use std::io::{Read, Write};

pub use jomini::envelope::JominiFile as ImperatorFile;
pub use jomini::envelope::*;

/// Type alias for Imperator text deserializer
///
/// A lazy way to avoid the need to reimplement deserializer
pub type ImperatorTextDeserializer<'r> = TextReaderDeserializer<'r, Utf8Encoding>;
pub type ImperatorBinaryDeserializer<'r, 'res, RES> =
    BinaryReaderDeserializer<'r, 'res, RES, ImperatorFlavor>;

pub trait ImperatorBinaryDeserialization {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> ImperatorBinaryDeserializer<'_, 'res, RES>;
}

impl<R: ReaderAt> ImperatorBinaryDeserialization for &'_ SaveData<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> ImperatorBinaryDeserializer<'_, 'res, RES> {
        BinaryDeserializerBuilder::with_flavor(ImperatorFlavor::new())
            .from_reader(self.body().cursor(), resolver)
    }
}

impl<R: Read> ImperatorBinaryDeserialization for SaveContent<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> ImperatorBinaryDeserializer<'_, 'res, RES> {
        BinaryDeserializerBuilder::with_flavor(ImperatorFlavor::new()).from_reader(self, resolver)
    }
}

impl<R: Read> ImperatorBinaryDeserialization for SaveMetadata<BinaryEncoding, R> {
    fn deserializer<'res, RES: TokenResolver>(
        &mut self,
        resolver: &'res RES,
    ) -> ImperatorBinaryDeserializer<'_, 'res, RES> {
        BinaryDeserializerBuilder::with_flavor(ImperatorFlavor::new()).from_reader(self, resolver)
    }
}

pub trait ImperatorMelt {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
    where
        Resolver: TokenResolver,
        Writer: Write;
}

pub trait ImperatorTextMelt {
    fn melt<Writer>(&mut self, output: Writer) -> Result<melt::MeltedDocument, ImperatorError>
    where
        Writer: Write;
}

impl<R: ReaderAt> ImperatorMelt for &'_ ImperatorFile<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match self.gamestate().map_err(ImperatorErrorKind::from)? {
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

impl<R: ReaderAt> ImperatorMelt for &'_ JominiZip<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        match self.gamestate().map_err(ImperatorErrorKind::from)? {
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

impl<R: ReaderAt> ImperatorMelt for &'_ SaveData<BinaryEncoding, R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        mut output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
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

impl<R: Read> ImperatorMelt for SaveMetadataKind<R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
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

impl<R: ReaderAt> ImperatorTextMelt for &'_ SaveData<TextEncoding, R> {
    fn melt<Writer>(&mut self, mut output: Writer) -> Result<melt::MeltedDocument, ImperatorError>
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

impl<R: Read> ImperatorTextMelt for SaveMetadata<TextEncoding, R> {
    fn melt<Writer>(&mut self, mut output: Writer) -> Result<melt::MeltedDocument, ImperatorError>
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

impl<R: Read> ImperatorMelt for SaveMetadata<BinaryEncoding, R> {
    fn melt<Resolver, Writer>(
        &mut self,
        options: MeltOptions,
        resolver: Resolver,
        output: Writer,
    ) -> Result<melt::MeltedDocument, ImperatorError>
    where
        Resolver: TokenResolver,
        Writer: Write,
    {
        let header = self.header().clone();
        melt::melt(self, output, resolver, options, header)
    }
}

pub trait DeserializeImperator {
    fn deserialize<T>(&mut self, resolver: impl TokenResolver) -> Result<T, ImperatorError>
    where
        T: DeserializeOwned;
}

impl<R: ReaderAt> DeserializeImperator for &'_ ImperatorFile<R> {
    fn deserialize<T>(&mut self, resolver: impl TokenResolver) -> Result<T, ImperatorError>
    where
        T: DeserializeOwned,
    {
        match self.kind() {
            JominiFileKind::Uncompressed(SaveDataKind::Text(x)) => Ok(x
                .deserializer()
                .deserialize()
                .map_err(ImperatorErrorKind::Deserialize)?),
            JominiFileKind::Uncompressed(SaveDataKind::Binary(x)) => Ok((&*x)
                .deserializer(&resolver)
                .deserialize()
                .map_err(ImperatorErrorKind::Deserialize)?),
            JominiFileKind::Zip(x) => {
                Ok(match x.gamestate().map_err(ImperatorErrorKind::Envelope)? {
                    SaveContentKind::Text(mut x) => x
                        .deserializer()
                        .deserialize()
                        .map_err(ImperatorErrorKind::Deserialize)?,
                    SaveContentKind::Binary(mut x) => x
                        .deserializer(&resolver)
                        .deserialize()
                        .map_err(ImperatorErrorKind::Deserialize)?,
                })
            }
        }
    }
}
