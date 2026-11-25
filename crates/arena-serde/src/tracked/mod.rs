// Path tracking for arena deserialization errors.
//
// Ported from serde_path_to_error v0.1.20 with modified path tracking to use a
// single buffer that amortizes allocations.

mod de;
mod path;
mod wrap;

pub use de::{Deserializer, deserialize};
pub use path::{Path, Segment, Segments};

use core::fmt::{self, Display};
use serde::ser::StdError;

/// Original deserializer error together with the path at which it occurred.
#[derive(Clone, Debug)]
pub struct Error<E> {
    path: Path,
    original: E,
}

impl<E> Error<E> {
    pub fn new(path: Path, inner: E) -> Self {
        Error {
            path,
            original: inner,
        }
    }

    /// Element path at which this deserialization error occurred.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The Deserializer's underlying error that occurred.
    pub fn into_inner(self) -> E {
        self.original
    }

    /// Reference to the Deserializer's underlying error that occurred.
    pub fn inner(&self) -> &E {
        &self.original
    }
}

impl<E: Display> Display for Error<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.path.is_only_unknown() {
            write!(f, "{}: ", self.path)?;
        }
        write!(f, "{}", self.original)
    }
}

impl<E: StdError> StdError for Error<E> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.original.source()
    }
}

/// State for bookkeeping across nested deserializer calls.
///
/// Uses a mutable byte vector for efficient path tracking that truncates on backtrack.
pub struct Track<'buf> {
    path_buf: &'buf mut Vec<u8>,
    path: Option<Path>,
}

impl<'buf> Track<'buf> {
    /// Create a new tracker with a mutable byte vector for path tracking.
    pub fn new_with(buf: &'buf mut Vec<u8>) -> Self {
        Track {
            path_buf: buf,
            path: None,
        }
    }

    /// Append a key to the path buffer and return its start and length.
    ///
    /// Only the first 255 bytes are kept.
    pub(crate) fn push_bytes(&mut self, bytes: &[u8]) -> (usize, u8) {
        let len = bytes.len().min(u8::MAX as usize);
        let start = self.path_buf.len();
        self.path_buf.extend_from_slice(&bytes[..len]);
        (start, len as u8)
    }

    /// Remove the last `len` bytes of the path buffer.
    pub(crate) fn pop_bytes(&mut self, len: u8) {
        let new_len = self.path_buf.len().saturating_sub(len as usize);
        self.path_buf.truncate(new_len);
    }

    /// The bytes of a key, or nothing if they are no longer in the path buffer.
    pub(crate) fn get_bytes(&self, start: usize, len: u8) -> &[u8] {
        self.path_buf
            .get(start..start + len as usize)
            .unwrap_or_default()
    }

    /// Gets path at which the error occurred. Only meaningful after we know
    /// that an error has occurred. Returns an empty path otherwise.
    pub fn path(self) -> Path {
        self.path.unwrap_or_else(Path::empty)
    }

    #[inline]
    pub(crate) fn trigger<'a, E>(&mut self, chain: &Chain<'a>, err: E) -> E {
        self.trigger_impl(chain);
        err
    }

    pub(crate) fn trigger_impl<'a>(&mut self, chain: &Chain<'a>) {
        if self.path.is_none() {
            self.path = Some(Path::from_chain(chain, self));
        }
    }
}

#[derive(Clone)]
pub(crate) enum Chain<'a> {
    Root,
    Seq {
        parent: &'a Chain<'a>,
        index: usize,
    },
    Map {
        parent: &'a Chain<'a>,
        start: usize, // Absolute start position in path_buf
        len: u8,      // Length of this key
    },
    Enum {
        parent: &'a Chain<'a>,
        start: usize, // Absolute start position in path_buf
        len: u8,      // Length of this variant
    },
    Some {
        parent: &'a Chain<'a>,
    },
    NewtypeStruct {
        parent: &'a Chain<'a>,
    },
    NewtypeVariant {
        parent: &'a Chain<'a>,
    },
    NonStringKey {
        parent: &'a Chain<'a>,
    },
}
