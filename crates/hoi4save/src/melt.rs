use crate::{
    flavor::{Hoi4BinaryFormat, Hoi4Flavor},
    Hoi4Date, Hoi4Error, Hoi4ErrorKind,
};
use jomini::{
    binary::{BinaryFlavor, FailedResolveStrategy, TokenResolver},
    common::PdsDate,
    TextWriterBuilder,
};
use std::{
    collections::HashSet,
    io::{Read, Write},
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

pub(crate) fn melt<Reader, Writer, Resolver>(
    mut input: Reader,
    output: Writer,
    resolver: Resolver,
    options: MeltOptions,
) -> Result<MeltedDocument, Hoi4Error>
where
    Reader: Read,
    Writer: Write,
    Resolver: TokenResolver,
{
    let mut buffer = Vec::new();
    input.read_to_end(&mut buffer)?;
    let mut data = buffer.as_slice();
    let mut save_version_id = false;

    let mut unknown_tokens = HashSet::new();
    let flavor = Hoi4Flavor;
    let mut format = Hoi4BinaryFormat::new(&resolver);

    let mut wtr = TextWriterBuilder::new()
        .indent_char(b'\t')
        .indent_factor(1)
        .from_writer(output);

    let mut known_number = false;
    let mut known_date = false;
    let mut quoted_buffer_enabled = false;
    let mut quoted_buffer: Vec<u8> = Vec::new();

    while !data.is_empty() {
        let (id, rest) = data.split_first_chunk::<2>().ok_or(Hoi4ErrorKind::Eof)?;
        let id = u16::from_le_bytes(*id);
        data = rest;

        if quoted_buffer_enabled {
            if matches!(id, 0x0001) {
                wtr.write_unquoted(&quoted_buffer)?;
            } else {
                wtr.write_quoted(&quoted_buffer)?;
            }
            quoted_buffer.clear();
            quoted_buffer_enabled = false;
        }

        match id {
            0x0001 => wtr.write_operator(jomini::text::Operator::Equal)?,
            0x0003 => wtr.write_start()?,
            0x0004 => wtr.write_end()?,
            0x0014 => {
                let (id, rest) = data.split_first_chunk::<4>().ok_or(Hoi4ErrorKind::Eof)?;
                let val = u32::from_le_bytes(*id);
                data = rest;
                wtr.write_u32(val)?
            }
            0x029c => {
                let (id, rest) = data.split_first_chunk::<8>().ok_or(Hoi4ErrorKind::Eof)?;
                let val = u64::from_le_bytes(*id);
                data = rest;
                wtr.write_u64(val)?
            }
            0x000c => {
                let (id, rest) = data.split_first_chunk::<4>().ok_or(Hoi4ErrorKind::Eof)?;
                let x = i32::from_le_bytes(*id);
                data = rest;

                if save_version_id {
                    format.observe_i32(x);
                    wtr.write_i32(x)?;
                } else if known_number {
                    wtr.write_i32(x)?;
                    known_number = false;
                } else if known_date {
                    if let Some(date) = Hoi4Date::from_binary(x) {
                        wtr.write_date(date.game_fmt())?;
                    } else if options.on_failed_resolve != FailedResolveStrategy::Error {
                        wtr.write_i32(x)?;
                    } else {
                        return Err(Hoi4Error::from(Hoi4ErrorKind::InvalidDate(x)));
                    }
                    known_date = false;
                } else if let Some(date) = Hoi4Date::from_binary_heuristic(x) {
                    wtr.write_date(date.game_fmt())?;
                } else {
                    wtr.write_i32(x)?;
                }
            }
            0x000e => {
                let (id, rest) = data.split_first().ok_or(Hoi4ErrorKind::Eof)?;
                data = rest;
                wtr.write_bool(*id != 0)?
            }
            0x000f | 0x0017 => {
                let (len, rest) = data.split_first_chunk::<2>().ok_or(Hoi4ErrorKind::Eof)?;
                let len = u16::from_le_bytes(*len);
                let (x, rest) = rest
                    .split_at_checked(len as usize)
                    .ok_or(Hoi4ErrorKind::Eof)?;
                data = rest;
                if id == 0x0017 {
                    wtr.write_unquoted(x)?;
                } else if wtr.at_unknown_start() {
                    quoted_buffer_enabled = true;
                    quoted_buffer.extend_from_slice(x);
                } else if wtr.expecting_key() {
                    wtr.write_unquoted(x)?;
                } else {
                    wtr.write_quoted(x)?;
                }
            }
            0x000d => {
                if format.modern_f32() {
                    let (id, rest) = data.split_first_chunk::<8>().ok_or(Hoi4ErrorKind::Eof)?;
                    let val = Hoi4Flavor::decode_modern_f64(*id);
                    data = rest;
                    wtr.write_f64(val)?
                } else {
                    let (id, rest) = data.split_first_chunk::<4>().ok_or(Hoi4ErrorKind::Eof)?;
                    let val = flavor.visit_f32(*id);
                    data = rest;
                    wtr.write_f32(val)?
                }
            }
            0x0167 => {
                let (id, rest) = data.split_first_chunk::<8>().ok_or(Hoi4ErrorKind::Eof)?;
                let val = flavor.visit_f64(*id);
                data = rest;
                wtr.write_f64(val)?
            }
            0x0317 => {
                let (id, rest) = data.split_first_chunk::<8>().ok_or(Hoi4ErrorKind::Eof)?;
                let val = i64::from_le_bytes(*id);
                data = rest;
                wtr.write_i64(val)?
            }
            id => match resolver.resolve(id) {
                Some(id) => {
                    format.observe_key(if id == "save_version" { 0x349d } else { 0 });
                    if !options.verbatim
                        && matches!(id, "is_ironman" | "ironman")
                        && wtr.expecting_key()
                    {
                        // skip equals
                        let rest = data.get(2..).ok_or(Hoi4ErrorKind::Eof)?;

                        let (token_id, rest) =
                            rest.split_first_chunk::<2>().ok_or(Hoi4ErrorKind::Eof)?;
                        let id = u16::from_le_bytes(*token_id);

                        // skip i32
                        if id == 0x000c {
                            data = rest.get(4..).ok_or(Hoi4ErrorKind::Eof)?;
                            continue;
                        } else if id == 0x000f {
                            // get str len
                            let (len, rest) =
                                rest.split_first_chunk::<2>().ok_or(Hoi4ErrorKind::Eof)?;
                            let len = u16::from_le_bytes(*len);

                            data = rest.get(len as usize..).ok_or(Hoi4ErrorKind::Eof)?;
                            continue;
                        }
                    }

                    known_number =
                        id.ends_with("seed") || matches!(id, "total" | "available" | "locked");
                    known_date = id == "date";
                    save_version_id = id == "save_version";
                    wtr.write_unquoted(id.as_bytes())?;
                }
                None => match options.on_failed_resolve {
                    FailedResolveStrategy::Error => {
                        return Err(Hoi4ErrorKind::UnknownToken { token_id: id }.into());
                    }
                    _ => {
                        unknown_tokens.insert(id);
                        write!(wtr, "__unknown_0x{:x}", id)?;
                    }
                },
            },
        }
    }

    Ok(MeltedDocument { unknown_tokens })
}
