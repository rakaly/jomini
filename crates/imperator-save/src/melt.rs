use crate::{flavor::ImperatorFlavor, ImperatorDate, ImperatorError, ImperatorErrorKind};
use jomini::{
    binary::{self, BinaryFlavor, FailedResolveStrategy, TokenReader, TokenResolver},
    common::PdsDate,
    envelope::{SaveHeader, SaveHeaderKind},
    TextWriterBuilder,
};
use std::{
    collections::HashSet,
    io::{Cursor, Read, Write},
};

/// Output from melting a binary save to plaintext
#[derive(Debug, Default)]
pub struct MeltedDocument {
    unknown_tokens: HashSet<u16>,
}

impl MeltedDocument {
    pub fn new() -> Self {
        Self::default()
    }

    /// The list of unknown tokens that the provided resolver accumulated
    pub fn unknown_tokens(&self) -> &HashSet<u16> {
        &self.unknown_tokens
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MeltOptions {
    verbatim: bool,
    on_failed_resolve: FailedResolveStrategy,
}

impl Default for MeltOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl MeltOptions {
    pub fn new() -> Self {
        Self {
            verbatim: false,
            on_failed_resolve: FailedResolveStrategy::Ignore,
        }
    }

    pub fn verbatim(self, verbatim: bool) -> Self {
        MeltOptions { verbatim, ..self }
    }

    pub fn on_failed_resolve(self, on_failed_resolve: FailedResolveStrategy) -> Self {
        MeltOptions {
            on_failed_resolve,
            ..self
        }
    }
}

fn update_header(data: &mut [u8], mut header: SaveHeader) {
    header.set_kind(SaveHeaderKind::Text);
    header.set_metadata_len((data.len() + 1 - header.header_len()) as u64);
    let _ = header.write(&mut data[..header.header_len()]);
}

pub(crate) fn melt<Reader, Writer, Resolver>(
    input: Reader,
    mut output: Writer,
    resolver: Resolver,
    options: MeltOptions,
    header: SaveHeader,
) -> Result<MeltedDocument, ImperatorError>
where
    Reader: Read,
    Writer: Write,
    Resolver: TokenResolver,
{
    let mut unknown_tokens = HashSet::new();
    let mut reader = TokenReader::new(input);
    let out = Vec::with_capacity((header.metadata_len() * 2) as usize);
    let mut cursor = Cursor::new(out);
    let _ = header.write(&mut cursor);

    let melter_return = melt_inner(
        &mut reader,
        &mut cursor,
        &resolver,
        options,
        Some(&header),
        false,
        &mut unknown_tokens,
    )?;

    let mut metadata = cursor.into_inner();
    update_header(&mut metadata, header);
    output.write_all(&metadata)?;
    output.write_all(&b"\n"[..])?;

    if melter_return != MelterReturn::Eof {
        melt_inner(
            &mut reader,
            &mut output,
            &resolver,
            options,
            None,
            matches!(melter_return, MelterReturn::StartOfGamestateField),
            &mut unknown_tokens,
        )?;
        output.write_all(&b"\n"[..])?;
    }

    Ok(MeltedDocument { unknown_tokens })
}

const START_OF_GAMESTATE_FIELD: &[u8] = b"speed";

#[derive(PartialEq)]
enum MelterReturn {
    Eof,
    StartOfGamestateField,
}

fn melt_inner<Writer, Resolver>(
    reader: &mut TokenReader,
    output: Writer,
    resolver: Resolver,
    options: MeltOptions,
    header: Option<&SaveHeader>,
    write_prefix: bool,
    unknown_tokens: &mut HashSet<u16>,
) -> Result<MelterReturn, ImperatorError>
where
    Writer: Write,
    Resolver: TokenResolver,
{
    let flavor = ImperatorFlavor;
    let mut wtr = TextWriterBuilder::new()
        .indent_char(b'\t')
        .indent_factor(1)
        .from_writer(output);

    if write_prefix {
        wtr.write_unquoted(START_OF_GAMESTATE_FIELD)?;
    }

    let mut known_date = false;
    let mut known_number = false;
    let mut quoted_buffer_enabled = false;
    let mut quoted_buffer: Vec<u8> = Vec::new();
    while let Some(token) = reader.next()? {
        if quoted_buffer_enabled {
            if matches!(token, binary::Token::Equal) {
                wtr.write_unquoted(&quoted_buffer)?;
            } else {
                wtr.write_quoted(&quoted_buffer)?;
            }
            quoted_buffer.clear();
            quoted_buffer_enabled = false;
        }

        match token {
            jomini::binary::Token::Open => wtr.write_start()?,
            jomini::binary::Token::Close => wtr.write_end()?,
            jomini::binary::Token::I32(x) => {
                if known_date {
                    if let Some(date) = ImperatorDate::from_binary(x) {
                        wtr.write_date(date.game_fmt())?;
                    } else if options.on_failed_resolve != FailedResolveStrategy::Error {
                        wtr.write_i32(x)?;
                    } else {
                        return Err(ImperatorError::new(ImperatorErrorKind::InvalidDate(x)));
                    }
                    known_date = false;
                } else if known_number {
                    wtr.write_i32(x)?;
                    known_number = false;
                } else if let Some(date) = ImperatorDate::from_binary_heuristic(x) {
                    wtr.write_date(date.game_fmt())?;
                } else {
                    wtr.write_i32(x)?;
                }
            }
            jomini::binary::Token::Quoted(x) => {
                if wtr.at_unknown_start() {
                    quoted_buffer_enabled = true;
                    quoted_buffer.extend_from_slice(x.as_bytes());
                } else if wtr.expecting_key() {
                    wtr.write_unquoted(x.as_bytes())?;
                } else {
                    wtr.write_quoted(x.as_bytes())?;
                }
            }
            jomini::binary::Token::Unquoted(x) => {
                wtr.write_unquoted(x.as_bytes())?;
            }
            jomini::binary::Token::F32(x) => wtr.write_f32(flavor.visit_f32(x))?,
            jomini::binary::Token::F64(x) => wtr.write_f64(flavor.visit_f64(x))?,
            jomini::binary::Token::Id(x) => match resolver.resolve(x) {
                Some(id) => {
                    if !options.verbatim && id == "is_ironman" && wtr.expecting_key() {
                        let mut next = reader.read()?;
                        if matches!(next, binary::Token::Equal) {
                            next = reader.read()?;
                        }

                        if matches!(next, binary::Token::Open) {
                            reader.skip_container()?;
                        }
                        continue;
                    }

                    if id.as_bytes() == START_OF_GAMESTATE_FIELD && header.is_some() {
                        return Ok(MelterReturn::StartOfGamestateField);
                    }

                    known_number = id == "seed";
                    known_date = id == "birth_date";
                    wtr.write_unquoted(id.as_bytes())?;
                }
                None => match options.on_failed_resolve {
                    FailedResolveStrategy::Error => {
                        return Err(ImperatorErrorKind::UnknownToken { token_id: x as u32 }.into());
                    }
                    FailedResolveStrategy::Ignore if wtr.expecting_key() => {
                        let mut next = reader.read()?;
                        if matches!(next, binary::Token::Equal) {
                            next = reader.read()?;
                        }

                        if matches!(next, binary::Token::Open) {
                            reader.skip_container()?;
                        }
                    }
                    _ => {
                        unknown_tokens.insert(x);
                        write!(wtr, "__unknown_0x{:x}", x)?;
                    }
                },
            },
            jomini::binary::Token::Equal => wtr.write_operator(jomini::text::Operator::Equal)?,
            jomini::binary::Token::U32(x) => wtr.write_u32(x)?,
            jomini::binary::Token::U64(x) => wtr.write_u64(x)?,
            jomini::binary::Token::Bool(x) => wtr.write_bool(x)?,
            jomini::binary::Token::Rgb(x) => wtr.write_rgb(&x)?,
            jomini::binary::Token::I64(x) => wtr.write_i64(x)?,
            jomini::binary::Token::Lookup(x) => {
                return Err(ImperatorError::new(ImperatorErrorKind::InvalidSyntax(
                    format!("encountered lookup token with id {x:#x} which is unsupported"),
                )))
            }
        }
    }

    Ok(MelterReturn::Eof)
}
