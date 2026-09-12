use std::io::BufRead;

use crate::{Eu4Error, Eu4ErrorKind};

pub struct SegmentedResolver<'a> {
    values: Vec<&'a str>,

    // First sequence spans from 0 to lower_sequence_end
    lower_sequence_end: u16,

    // Upper sequence starts at upper_sequence_start, but is stored contiguously
    // after lower_sequence
    upper_sequence_start: u16,
}

impl<'a> SegmentedResolver<'a> {
    pub const fn empty() -> SegmentedResolver<'static> {
        SegmentedResolver {
            values: Vec::new(),
            lower_sequence_end: 0,
            upper_sequence_start: 0,
        }
    }

    pub fn from_parts(
        values: Vec<&'a str>,
        lower_sequence_end: u16,
        upper_sequence_start: u16,
    ) -> SegmentedResolver<'a> {
        SegmentedResolver {
            values,
            lower_sequence_end,
            upper_sequence_start,
        }
    }

    /// Create resolver from a `BufRead` implementation over a space delimited
    /// text format:
    ///
    /// ```plain
    /// 0xffff my_test_token
    /// 0xeeee my_test_token2
    /// ```
    pub fn parse<T>(mut data: T) -> Result<SegmentedResolverBuilder, Eu4Error>
    where
        T: BufRead,
    {
        let mut result = Vec::new();
        let mut line = String::new();
        while data.read_line(&mut line)? != 0 {
            let (num, text) = line.split_once(' ').ok_or_else(|| {
                Eu4Error::from(Eu4ErrorKind::InvalidSyntax(String::from(
                    "expected to split line",
                )))
            })?;

            let z = u16::from_str_radix(num.trim_start_matches("0x"), 16).map_err(|_| {
                Eu4Error::from(Eu4ErrorKind::InvalidSyntax(String::from(
                    "invalid ironman token",
                )))
            })?;

            if result.len() <= z as usize {
                result.resize(z as usize + 1, String::from(""));
            }
            result[z as usize] = String::from(text.trim_end());
            line.clear();
        }

        let lower_sequence_end = result.len() as u16;
        Ok(SegmentedResolverBuilder {
            values: result,
            lower_sequence_end,
            upper_sequence_start: 0,
        })
    }
}

impl jomini::binary::TokenResolver for SegmentedResolver<'_> {
    fn resolve(&self, token: u16) -> Option<&str> {
        let ind = if token < self.lower_sequence_end {
            usize::from(token)
        } else if token >= self.upper_sequence_start {
            usize::from(token - self.upper_sequence_start + self.lower_sequence_end)
        } else {
            return None;
        };

        match self.values.get(ind) {
            Some(&"") | None => None,
            Some(x) => Some(*x),
        }
    }

    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

pub struct SegmentedResolverBuilder {
    values: Vec<String>,
    lower_sequence_end: u16,
    upper_sequence_start: u16,
}

impl SegmentedResolverBuilder {
    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub fn resolver(&self) -> SegmentedResolver<'_> {
        SegmentedResolver {
            values: self.values.iter().map(|x| x.as_str()).collect(),
            lower_sequence_end: self.lower_sequence_end,
            upper_sequence_start: self.upper_sequence_start,
        }
    }
}
