use crate::text::lossless::{
    LineCol, LineIndex, SyntaxError, SyntaxErrorKind, TextRange, TextSize,
};
use std::fmt;

/// An error from the deserializer of the syntax tree.
///
/// The error tells where the problem is:
///
/// - [`range`](Error::range): the byte range of the value in the source.
/// - [`line_col`](Error::line_col): the line and column of the start of
///   that range.
/// - [`path`](Error::path): the keys and the indices from the root to the
///   value, such as `country.FRA.cores[2]`.
/// - [`file`](Error::file): the name of the file, when you give one.
///
/// The display form is `file:line:col: path: message`. Each part that is not
/// known is not shown.
///
/// ```
/// use jomini::text::lossless::de::from_utf8_slice;
/// use serde::Deserialize;
///
/// #[derive(Deserialize, Debug)]
/// struct State {
///     id: u32,
/// }
///
/// #[derive(Deserialize, Debug)]
/// struct File {
///     state: State,
/// }
///
/// let source = b"state = {\n  id = abc\n}";
/// let error = from_utf8_slice::<File>(source).unwrap_err();
/// assert_eq!(error.path(), "state.id");
/// assert_eq!(error.line_col().unwrap().to_string(), "2:8");
/// assert_eq!(
///     error.to_string(),
///     "2:8: state.id: invalid type: string \"abc\", expected u32"
/// );
/// ```
pub struct Error(Box<ErrorInner>);

struct ErrorInner {
    kind: ErrorKind,
    range: Option<TextRange>,
    line_col: Option<LineCol>,
    /// The path from the value to the root. The deserializer adds a segment
    /// when the error goes out of a container, so the order is reversed.
    path: Vec<Segment>,
    file: Option<String>,
}

#[derive(Debug, Clone)]
pub(crate) enum Segment {
    Key(String),
    Index(usize),
}

/// A part of the path of an [`Error`]: the key of a field or a parameter,
/// or the index of an element of a sequence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum PathSegment<'a> {
    /// The decoded key of a field, or the name of a parameter.
    Key(&'a str),
    /// The index of an element of a sequence.
    Index(usize),
}

/// The type of an [`Error`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum ErrorKind {
    /// A message from serde or from a [`Deserialize`](serde::Deserialize)
    /// implementation, such as "missing field" or "invalid type".
    Message(String),

    /// The parser found a syntax error in the source.
    Syntax(SyntaxErrorKind),
}

impl Error {
    pub(crate) fn new(kind: ErrorKind) -> Self {
        Error(Box::new(ErrorInner {
            kind,
            range: None,
            line_col: None,
            path: Vec::new(),
            file: None,
        }))
    }

    pub(crate) fn syntax(error: &SyntaxError) -> Self {
        let mut result = Error::new(ErrorKind::Syntax(error.kind));
        result.0.range = Some(error.range);
        result
    }

    /// The type of the error.
    pub fn kind(&self) -> &ErrorKind {
        &self.0.kind
    }

    /// The message of the error, without the location.
    pub fn message(&self) -> String {
        match &self.0.kind {
            ErrorKind::Message(msg) => msg.clone(),
            ErrorKind::Syntax(kind) => kind.message().to_string(),
        }
    }

    /// The byte range of the value that caused the error, when it is known.
    pub fn range(&self) -> Option<TextRange> {
        self.0.range
    }

    /// The line and column of the start of [`range`](Error::range), when it
    /// is known.
    pub fn line_col(&self) -> Option<LineCol> {
        self.0.line_col
    }

    /// The keys and indices from the root to the value, in that order. A key
    /// can contain a `.` or a bracket, so use this and not the text of
    /// [`path`](Error::path) to find the keys.
    ///
    /// ```
    /// use jomini::text::lossless::de::{PathSegment, from_utf8_slice};
    /// use serde::Deserialize;
    ///
    /// #[derive(Deserialize, Debug)]
    /// struct File {
    ///     #[serde(rename = "a.b")]
    ///     dotted: Vec<u32>,
    /// }
    ///
    /// let error = from_utf8_slice::<File>(b"a.b = { 1 x }").unwrap_err();
    /// assert_eq!(error.path(), "a.b[1]");
    /// let segments: Vec<_> = error.path_segments().collect();
    /// assert_eq!(segments, [PathSegment::Key("a.b"), PathSegment::Index(1)]);
    /// ```
    pub fn path_segments(
        &self,
    ) -> impl DoubleEndedIterator<Item = PathSegment<'_>> + ExactSizeIterator + std::iter::FusedIterator
    {
        self.0.path.iter().rev().map(|segment| match segment {
            Segment::Key(key) => PathSegment::Key(key),
            Segment::Index(index) => PathSegment::Index(*index),
        })
    }

    /// The keys and indices from the root to the value. The keys have a `.`
    /// between them, and an index is in brackets, as in `a.b[2].c`. The path
    /// of an error at the root is empty.
    pub fn path(&self) -> String {
        let mut out = String::new();
        for segment in self.0.path.iter().rev() {
            match segment {
                Segment::Key(key) => {
                    if !out.is_empty() {
                        out.push('.');
                    }
                    out.push_str(key);
                }
                Segment::Index(index) => {
                    out.push('[');
                    out.push_str(&index.to_string());
                    out.push(']');
                }
            }
        }
        out
    }

    /// The name of the file, when you give one with
    /// [`with_file`](Error::with_file).
    pub fn file(&self) -> Option<&str> {
        self.0.file.as_deref()
    }

    /// Set the name of the file that the error is in.
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.0.file = Some(file.into());
        self
    }

    /// Set the line and column from the range with `source`, the bytes
    /// that the tree was parsed from.
    pub(crate) fn locate(self, source: &[u8]) -> Self {
        self.locate_with(|offset| LineIndex::new(source).line_col(offset))
    }

    /// Set the line and column from the range with `line_col`, which gives
    /// the line and column of a byte offset.
    pub(crate) fn locate_with(mut self, line_col: impl FnOnce(TextSize) -> LineCol) -> Self {
        if let Some(range) = self.0.range
            && self.0.line_col.is_none()
        {
            self.0.line_col = Some(line_col(range.start()));
        }
        self
    }

    /// Set the range when the error has no range yet. The deserializer
    /// calls this from the inner value to the outer value, so the range of
    /// the inner value stays.
    #[cold]
    pub(crate) fn or_range(mut self, range: Option<TextRange>) -> Self {
        if self.0.range.is_none() {
            self.0.range = range;
        }
        self
    }

    #[cold]
    pub(crate) fn in_key(mut self, key: String) -> Self {
        self.0.path.push(Segment::Key(key));
        self
    }

    /// Add the path of the outer values, from the inner part to the outer
    /// part.
    #[cold]
    pub(crate) fn in_path(mut self, path: Vec<Segment>) -> Self {
        self.0.path.extend(path);
        self
    }

    #[cold]
    pub(crate) fn in_index(mut self, index: usize) -> Self {
        self.0.path.push(Segment::Index(index));
        self
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(file) = &self.0.file {
            write!(f, "{file}:")?;
            if self.0.line_col.is_none() {
                f.write_str(" ")?;
            }
        }
        if let Some(at) = self.0.line_col {
            write!(f, "{at}: ")?;
        } else if let Some(range) = self.0.range {
            write!(f, "{range}: ")?;
        }
        let path = self.path();
        if !path.is_empty() {
            write!(f, "{path}: ")?;
        }
        match &self.0.kind {
            ErrorKind::Message(msg) => f.write_str(msg),
            ErrorKind::Syntax(kind) => f.write_str(kind.message()),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &self.0.kind)
            .field("range", &self.0.range)
            .field("line_col", &self.0.line_col)
            .field("path", &self.path())
            .field("file", &self.0.file)
            .finish()
    }
}

impl std::error::Error for Error {}

impl serde::de::Error for Error {
    #[cold]
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::new(ErrorKind::Message(msg.to_string()))
    }
}
