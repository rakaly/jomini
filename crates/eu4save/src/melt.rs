use crate::{flavor::Eu4Flavor, Eu4Date, Eu4Error, Eu4ErrorKind};
use jomini::{
    binary::{self, BinaryFlavor, FailedResolveStrategy, TokenReader, TokenResolver},
    common::PdsDate,
    TextWriterBuilder,
};
use std::{
    collections::HashSet,
    io::{Read, Write},
};

#[derive(Debug, Clone, Copy)]
enum QuoteKind {
    // Regular quoting rules
    Inactive,

    // Unquote scalar and containers
    UnquoteAll,

    // Unquote only a scalar value
    UnquoteScalar,

    // Quote only a scalar value
    QuoteScalar,

    // Quote object keys
    ForceQuote,
}

#[derive(Debug, Default)]
struct Quoter {
    queued: Option<QuoteKind>,
    depth: Vec<QuoteKind>,
}

impl Quoter {
    #[inline]
    pub fn push(&mut self) {
        let next = match self.queued.take() {
            Some(x @ QuoteKind::ForceQuote | x @ QuoteKind::UnquoteAll) => x,
            _ => QuoteKind::Inactive,
        };

        self.depth.push(next);
    }

    #[inline]
    pub fn pop(&mut self) {
        let _ = self.depth.pop();
    }

    #[inline]
    pub fn take_scalar(&mut self) -> QuoteKind {
        match self.queued.take() {
            Some(x) => x,
            None => self.depth.last().copied().unwrap_or(QuoteKind::Inactive),
        }
    }

    #[inline]
    fn queue(&mut self, mode: QuoteKind) {
        self.queued = Some(mode);
    }

    #[inline]
    fn clear_queued(&mut self) {
        self.queued = None;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MeltOptions {
    skip_checksum: bool,
    verbatim: bool,
    on_failed_resolve: FailedResolveStrategy,
    check_header: bool,
}

impl Default for MeltOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl MeltOptions {
    pub fn new() -> Self {
        Self {
            skip_checksum: false,
            verbatim: false,
            on_failed_resolve: FailedResolveStrategy::Ignore,
            check_header: true,
        }
    }

    pub(crate) fn skip_checksum(self, skip_checksum: bool) -> Self {
        MeltOptions {
            skip_checksum,
            ..self
        }
    }

    pub fn verbatim(self, verbatim: bool) -> Self {
        MeltOptions { verbatim, ..self }
    }

    pub(crate) fn check_header(self, check_header: bool) -> Self {
        MeltOptions {
            check_header,
            ..self
        }
    }

    pub fn on_failed_resolve(self, on_failed_resolve: FailedResolveStrategy) -> Self {
        MeltOptions {
            on_failed_resolve,
            ..self
        }
    }
}

#[derive(Debug, Default)]
pub struct MeltedDocument {
    pub(crate) unknown_tokens: HashSet<u16>,
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

pub(crate) fn melt<Reader, Writer, Resolver>(
    input: Reader,
    output: Writer,
    resolver: Resolver,
    options: MeltOptions,
) -> Result<MeltedDocument, Eu4Error>
where
    Reader: Read,
    Writer: Write,
    Resolver: TokenResolver,
{
    let mut reader = TokenReader::new(input);
    if options.check_header && reader.read_bytes(6)? != b"EU4bin" {
        return Err(Eu4Error::new(Eu4ErrorKind::UnknownHeader));
    }

    let mut wtr = TextWriterBuilder::new()
        .indent_char(b'\t')
        .indent_factor(1)
        .from_writer(output);
    let flavor = Eu4Flavor::new();
    let mut unknown_tokens: HashSet<u16> = HashSet::new();
    let skip_checksum = options.skip_checksum;
    let verbatim = options.verbatim;
    let on_failed_resolve = options.on_failed_resolve;

    let mut quoter = Quoter::default();
    let mut known_number = false;
    let mut known_date = false;
    let mut long_format = false;
    while let Some(token) = reader.next()? {
        match token {
            jomini::binary::Token::Open => {
                quoter.push();
                wtr.write_array_start()?
            }
            jomini::binary::Token::Close => {
                quoter.pop();
                wtr.write_end()?
            }
            jomini::binary::Token::Equal => wtr.write_operator(jomini::text::Operator::Equal)?,
            jomini::binary::Token::U32(x) => wtr.write_u32(x)?,
            jomini::binary::Token::U64(x) => wtr.write_u64(x)?,
            jomini::binary::Token::I32(x) => {
                if known_number {
                    wtr.write_i32(x)?;
                    known_number = false;
                } else if known_date {
                    if let Some(date) = Eu4Date::from_binary(x) {
                        wtr.write_date(date.game_fmt())?;
                    } else if on_failed_resolve != FailedResolveStrategy::Error {
                        wtr.write_i32(x)?;
                    } else {
                        return Err(Eu4Error::new(Eu4ErrorKind::InvalidDate(x)));
                    }
                    known_date = false;
                } else if let Some(date) = Eu4Date::from_binary_heuristic(x) {
                    wtr.write_date(date.game_fmt())?;
                } else {
                    wtr.write_i32(x)?;
                }
            }
            jomini::binary::Token::Bool(x) => wtr.write_bool(x)?,
            jomini::binary::Token::Unquoted(x) => wtr.write_unquoted(x.as_bytes())?,
            jomini::binary::Token::Quoted(x) => match quoter.take_scalar() {
                QuoteKind::Inactive if wtr.expecting_key() => wtr.write_unquoted(x.as_bytes())?,
                QuoteKind::Inactive => wtr.write_quoted(x.as_bytes())?,
                QuoteKind::ForceQuote => wtr.write_quoted(x.as_bytes())?,
                QuoteKind::UnquoteAll => wtr.write_unquoted(x.as_bytes())?,
                QuoteKind::UnquoteScalar => wtr.write_unquoted(x.as_bytes())?,
                QuoteKind::QuoteScalar => wtr.write_quoted(x.as_bytes())?,
            },
            jomini::binary::Token::F32(x) if long_format => {
                write!(&mut wtr, "{:.6}", flavor.visit_f32(x))?
            }
            jomini::binary::Token::F32(x) => write!(&mut wtr, "{:.3}", flavor.visit_f32(x))?,
            jomini::binary::Token::F64(x) => write!(&mut wtr, "{:.5}", flavor.visit_f64(x))?,
            jomini::binary::Token::Rgb(x) => wtr.write_rgb(&x)?,
            jomini::binary::Token::I64(x) => wtr.write_i64(x)?,
            jomini::binary::Token::Id(x) => match resolver.resolve(x) {
                Some(id) => {
                    if (id == "checksum" && skip_checksum) || (id == "is_ironman" && !verbatim) {
                        let mut next = reader.read()?;
                        if matches!(next, binary::Token::Equal) {
                            next = reader.read()?;
                        }

                        if matches!(next, binary::Token::Open) {
                            reader.skip_container()?;
                        }
                        continue;
                    }

                    quoter.clear_queued();

                    // There are certain tokens that we know are integers and will dupe the date heuristic
                    known_number = id == "random" || id.ends_with("seed") || id == "id";
                    known_date = id == "date_built";

                    match id {
                        "friend"
                        | "production_leader_tag"
                        | "dynamic_countries"
                        | "electors"
                        | "cores"
                        | "named_unrest"
                        | "claims"
                        | "country_of_origin"
                        | "granted_privileges"
                        | "attackers"
                        | "defenders"
                        | "persistent_attackers"
                        | "persistent_defenders"
                        | "mission_slot"
                        | "votes"
                        | "ruler_flags"
                        | "neighbours"
                        | "home_neighbours"
                        | "core_neighbours"
                        | "call_to_arms_friends"
                        | "allies"
                        | "extended_allies"
                        | "trade_embargoed_by"
                        | "trade_embargoes"
                        | "transfer_trade_power_from"
                        | "friend_tags"
                        | "hidden_flags"
                        | "members"
                        | "colony_claim"
                        | "harsh"
                        | "concilatory"
                        | "current_at_war_with"
                        | "current_war_allies"
                        | "participating_countries"
                        | "subjects"
                        | "support_independence"
                        | "transfer_trade_power_to"
                        | "guarantees"
                        | "warnings"
                        | "flags" => quoter.queue(QuoteKind::UnquoteAll),
                        _ => {}
                    }

                    if wtr.depth() == 2
                        && matches!(id, "discovered_by" | "tribal_owner" | "active_disaster")
                    {
                        quoter.queue(QuoteKind::UnquoteAll);
                    }

                    match id {
                        "culture_group" | "saved_names" | "tech_level_dates"
                        | "incident_variables" => {
                            quoter.queue(QuoteKind::ForceQuote);
                        }
                        "subjects" => {
                            quoter.queue(QuoteKind::QuoteScalar);
                        }
                        _ => {}
                    }

                    if wtr.depth() == 4 && id == "leader" {
                        quoter.queue(QuoteKind::UnquoteScalar);
                    }

                    if wtr.depth() == 0 && id == "ai" {
                        long_format = true;
                    }

                    wtr.write_unquoted(id.as_bytes())?;
                }
                None => match on_failed_resolve {
                    FailedResolveStrategy::Error => {
                        return Err(Eu4Error::new(Eu4ErrorKind::UnknownToken {
                            token_id: x as u32,
                        }))
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
            jomini::binary::Token::Lookup(_) => {
                return Err(Eu4Error::new(Eu4ErrorKind::InvalidSyntax(
                    "lookup tokens are not supported in EU4 files".to_string(),
                )))
            }
        }
    }

    Ok(MeltedDocument { unknown_tokens })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BasicTokenResolver, Eu4File};
    use std::{io::Cursor, sync::LazyLock};

    static TOKENS: LazyLock<BasicTokenResolver> = LazyLock::new(|| {
        let file_data = std::fs::read("assets/eu4.txt").unwrap_or_default();
        BasicTokenResolver::from_text_lines(file_data.as_slice()).unwrap()
    });

    #[test]
    fn test_melt_meta() {
        if TOKENS.is_empty() {
            return;
        }

        let meta = include_bytes!("../tests/it/fixtures/meta.bin");
        let expected = include_bytes!("../tests/it/fixtures/meta.bin.melted");
        let file = Eu4File::from_slice(&meta[..]).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(out.into_inner().as_slice(), &expected[..]);
    }

    #[test]
    fn test_melt_skip_ironman() {
        if TOKENS.is_empty() {
            return;
        }

        let data = [
            0x45, 0x55, 0x34, 0x62, 0x69, 0x6e, 0x4d, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x70, 0x98,
            0x8d, 0x03, 0x89, 0x35, 0x01, 0x00, 0x0e, 0x00, 0x01, 0x38, 0x2a, 0x01, 0x00, 0x0f,
            0x00, 0x03, 0x00, 0x42, 0x48, 0x41,
        ];

        let expected = b"EU4txt\ndate=1804.12.9\nplayer=\"BHA\"";
        let file = Eu4File::from_slice(&data).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(out.into_inner().as_slice(), &expected[..]);
    }

    #[test]
    fn test_melt_skip_ironman_in_object() {
        if TOKENS.is_empty() {
            return;
        }

        let data = [
            0x45, 0x55, 0x34, 0x62, 0x69, 0x6e, 0x4d, 0x28, 0x01, 0x00, 0x0c, 0x00, 0x70, 0x98,
            0x8d, 0x03, 0x23, 0x2d, 0x01, 0x00, 0x03, 0x00, 0x89, 0x35, 0x01, 0x00, 0x0e, 0x00,
            0x01, 0x04, 0x00, 0x38, 0x2a, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x42, 0x48, 0x41,
        ];

        let expected = "EU4txt\ndate=1804.12.9\nimpassable={ }\nplayer=\"BHA\"";
        let file = Eu4File::from_slice(&data).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(
            std::str::from_utf8(out.into_inner().as_slice()).unwrap(),
            expected
        );
    }

    #[test]
    fn test_skip_quoting_flags() {
        if TOKENS.is_empty() {
            return;
        }

        let mut data = vec![];
        data.extend_from_slice(b"EU4bin");
        data.extend_from_slice(&[0xcc, 0x29, 0x01, 0x00, 0x03, 0x00, 0x0f, 0x00, 0x11, 0x00]);
        data.extend_from_slice(b"schools_initiated");
        data.extend_from_slice(&[0x01, 0x00, 0x0f, 0x00, 0x0b, 0x00]);
        data.extend_from_slice(b"1444.11.11\n");
        data.extend_from_slice(&0x0004u16.to_le_bytes());

        let expected = "EU4txt\nflags={\n\tschools_initiated=1444.11.11\n\n}";

        let file = Eu4File::from_slice(&data).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(
            std::str::from_utf8(out.into_inner().as_slice()).unwrap(),
            expected
        );
    }

    #[test]
    fn test_melt_skip_unknown_key() {
        if TOKENS.is_empty() {
            return;
        }

        let data = [
            0x45, 0x55, 0x34, 0x62, 0x69, 0x6e, 0xff, 0xff, 0x01, 0x00, 0x0c, 0x00, 0x70, 0x98,
            0x8d, 0x03, 0x89, 0x35, 0x01, 0x00, 0x0e, 0x00, 0x01, 0x38, 0x2a, 0x01, 0x00, 0x0f,
            0x00, 0x03, 0x00, 0x42, 0x48, 0x41,
        ];

        let expected = "EU4txt\nplayer=\"BHA\"";
        let file = Eu4File::from_slice(&data).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Ignore),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(
            std::str::from_utf8(out.into_inner().as_slice()).unwrap(),
            expected
        );
    }

    #[test]
    fn test_melt_skip_unknown_value() {
        if TOKENS.is_empty() {
            return;
        }

        let data = [
            0x45, 0x55, 0x34, 0x62, 0x69, 0x6e, 0x4d, 0x28, 0x01, 0x00, 0xff, 0xff, 0x89, 0x35,
            0x01, 0x00, 0x0e, 0x00, 0x01, 0x38, 0x2a, 0x01, 0x00, 0x0f, 0x00, 0x03, 0x00, 0x42,
            0x48, 0x41,
        ];

        let expected = "EU4txt\ndate=__unknown_0xffff\nplayer=\"BHA\"";
        let file = Eu4File::from_slice(&data).unwrap();
        let mut out = Cursor::new(Vec::new());
        file.melt(
            MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Ignore),
            &*TOKENS,
            &mut out,
        )
        .unwrap();
        assert_eq!(
            std::str::from_utf8(out.into_inner().as_slice()).unwrap(),
            expected
        );
    }
}
