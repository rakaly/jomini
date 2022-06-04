use crate::{
    text::Operator, DeserializeError, DeserializeErrorKind, Encoding, Scalar, TextTape, TextToken,
};
use std::{
    borrow::Cow,
    collections::{hash_map::Entry, HashMap},
};

pub type KeyValue<'data, 'tokens, E> = (
    ScalarReader<'data, E>,
    Option<Operator>,
    ValueReader<'data, 'tokens, E>,
);

pub type KeyValues<'data, 'tokens, E> = (ScalarReader<'data, E>, GroupEntry<'data, 'tokens, E>);

/// Calculate what index the next value is. This assumes that a header + value
/// are two separate values
#[inline]
fn next_idx_header(tokens: &[TextToken], idx: usize) -> usize {
    match tokens[idx] {
        TextToken::Array(x) | TextToken::Object(x) | TextToken::HiddenObject(x) => x + 1,
        TextToken::Operator(_) => idx + 2,
        _ => idx + 1,
    }
}

/// Calculate what index the next value is. This assumes that a header + value
/// is one value
#[inline]
fn next_idx(tokens: &[TextToken], idx: usize) -> usize {
    match tokens[idx] {
        TextToken::Array(x) | TextToken::Object(x) | TextToken::HiddenObject(x) => x + 1,
        TextToken::Operator(_) => next_idx(tokens, idx + 1),
        TextToken::Header(_) => next_idx_header(tokens, idx + 1),
        _ => idx + 1,
    }
}

#[inline]
fn fields_len(tokens: &[TextToken], start_ind: usize, end_ind: usize) -> usize {
    let mut ind = start_ind;
    let mut count = 0;
    while ind < end_ind {
        let key_ind = ind;
        if let TextToken::Array(_) = tokens[key_ind] {
            return count;
        }

        let value_ind = match tokens[key_ind + 1] {
            TextToken::Operator(_) => key_ind + 2,
            _ => key_ind + 1,
        };
        ind = next_idx(tokens, value_ind);
        count += 1;
    }

    count
}

#[inline]
pub fn values_len(tokens: &[TextToken], start_ind: usize, end_ind: usize) -> usize {
    let mut count = 0;
    let mut ind = start_ind;
    while ind < end_ind {
        ind = next_idx_header(tokens, ind);
        count += 1;
    }

    count
}

#[inline]
fn at_trailer<'data, 'tokens, E>(
    tokens: &'tokens [TextToken<'data>],
    encoding: &E,
    token_ind: usize,
) -> Option<ArrayReader<'data, 'tokens, E>>
where
    E: Encoding + Clone,
{
    if let Some(TextToken::Array(ind)) = tokens.get(token_ind) {
        // trailers must be at least one element in length, else we're just reading
        // an object as an array
        if *ind != token_ind + 1 {
            Some(ArrayReader {
                tokens,
                start_ind: token_ind + 1,
                end_ind: *ind,
                encoding: encoding.clone(),
            })
        } else {
            None
        }
    } else {
        None
    }
}

type OpValue<'data, 'tokens, E> = (Option<Operator>, ValueReader<'data, 'tokens, E>);

/// Iterator over values grouped by duplicate keys
///
/// See [FieldGroupsIter](crate::text::FieldGroupsIter) for a worked example
pub struct GroupEntryIter<'data, 'tokens, 'parent, E> {
    index: usize,
    parent: &'parent GroupEntry<'data, 'tokens, E>,
}

impl<'data, 'tokens, 'parent, E> Iterator for GroupEntryIter<'data, 'tokens, 'parent, E>
where
    E: Clone,
{
    type Item = (Option<Operator>, ValueReader<'data, 'tokens, E>);

    fn next(&mut self) -> Option<Self::Item> {
        match &self.parent {
            GroupEntry::One((op, val)) => {
                if self.index == 0 {
                    self.index += 1;
                    Some((*op, (*val).clone()))
                } else {
                    None
                }
            }
            GroupEntry::Multiple(entries) => {
                let result = entries.get(self.index);
                self.index += 1;
                result.map(|(op, val)| (*op, (*val).clone()))
            }
        }
    }
}

/// Represents a group of values for duplicate keys
///
/// May contain one or many values
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"name=a core=b core=c")?;
/// let reader = tape.windows1252_reader();
/// let mut fields = reader.field_groups();
/// let first_group = fields.next();
/// let first_key = first_group.as_ref().map(|(key, _)| key.read_str());
/// assert_eq!(first_key.as_deref(), Some("name"));
/// let first_values_len = first_group.as_ref().map(|(_, group)| group.len());
/// assert_eq!(first_values_len, Some(1));
/// let first_values = first_group.map(|(_, group)| {
///     group.values()
///         .filter_map(|(_op, val)| val.read_string().ok())
///         .collect()
/// });
/// assert_eq!(first_values, Some(vec![String::from("a")]));
///
/// let second_group = fields.next();
/// let second_key = second_group.as_ref().map(|(key, _)| key.read_str());
/// assert_eq!(second_key.as_deref(), Some("core"));
/// let second_values = second_group.as_ref().map(|(_, group)| group.len());
/// assert_eq!(second_values, Some(2));
/// let second_values = second_group.map(|(_, group)| {
///     group.values()
///         .filter_map(|(_op, val)| val.read_string().ok())
///         .collect()
/// });
/// assert_eq!(second_values, Some(vec![String::from("b"), String::from("c")]));
/// # Ok(())
/// # }
/// ```
pub enum GroupEntry<'data, 'tokens, E> {
    /// Represents that the group is composed of only one value
    ///
    /// Most fields should only occur once, so this variant is optimized to
    /// not require a memory allocation (unlike the `Multiple` variant).
    One(OpValue<'data, 'tokens, E>),

    /// Represents that the group is composed of several values
    Multiple(Vec<OpValue<'data, 'tokens, E>>),
}

impl<'data, 'tokens, E> GroupEntry<'data, 'tokens, E> {
    /// Returns an iterator that includes all the values
    pub fn values<'parent>(&'parent self) -> GroupEntryIter<'data, 'tokens, 'parent, E> {
        GroupEntryIter {
            index: 0,
            parent: self,
        }
    }

    /// A group can never be empty so this returns false
    pub fn is_empty(&self) -> bool {
        false
    }

    /// Returns the number of values in the group
    pub fn len(&self) -> usize {
        match &self {
            GroupEntry::One(_) => 1,
            GroupEntry::Multiple(x) => x.len(),
        }
    }
}

/// All possible text reader variants
#[derive(Debug, Clone)]
pub enum Reader<'data, 'tokens, E> {
    /// object reader
    Object(ObjectReader<'data, 'tokens, E>),

    /// array reader
    Array(ArrayReader<'data, 'tokens, E>),

    /// scalar reader
    Scalar(ScalarReader<'data, E>),

    /// value reader
    Value(ValueReader<'data, 'tokens, E>),
}

impl<'data, 'tokens, E> Reader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Interpret value as a string
    #[inline]
    pub fn read_str(&self) -> Result<Cow<'data, str>, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_str()),
            Reader::Value(x) => x.read_str(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }

    /// Interpret value as a string
    #[inline]
    pub fn read_string(&self) -> Result<String, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_string()),
            Reader::Value(x) => x.read_string(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }

    /// Interpret value as a scalar
    #[inline]
    pub fn read_scalar(&self) -> Result<Scalar<'data>, DeserializeError> {
        match &self {
            Reader::Scalar(x) => Ok(x.read_scalar()),
            Reader::Value(x) => x.read_scalar(),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            }),
        }
    }
}

/// Iterator over fields of an object grouped by key
///
/// Since objects can have duplicated keys across fields, this iterator
/// consolidates them such that all values with the same key are grouped
/// together in the order that they appear in the object. Key order is
/// also equivalent, except that already seen keys will be skipped, as
/// those values have already been seen in an earlier group.
///
/// The process of grouping values together is more expensive than simply
/// iterating the keys in order, so when possible prefer
/// [`ObjectReader::fields()`](crate::text::ObjectReader::fields) over
/// [`ObjectReader::field_groups()`](crate::text::ObjectReader::field_groups).
///
/// These groups can be easily iterated:
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"name=a core=b core=c")?;
/// let reader = tape.windows1252_reader();
/// for (key, group) in reader.field_groups() {
///     match key.read_str().as_ref() {
///         "name" => assert_eq!(group.len(), 1),
///         "core" => assert_eq!(group.len(), 2),
///         x => panic!("unexpected key: {}", x),
///     }
/// }
/// # Ok(())
/// # }
/// ```
///
/// And picked apart:
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"name=a core=b core=c")?;
/// let reader = tape.windows1252_reader();
/// let mut fields = reader.field_groups();
/// let first_group = fields.next();
/// let first_key = first_group.as_ref().map(|(key, _)| key.read_str());
/// assert_eq!(first_key.as_deref(), Some("name"));
/// let first_values_len = first_group.as_ref().map(|(_, group)| group.len());
/// assert_eq!(first_values_len, Some(1));
/// let first_values = first_group.map(|(_, group)| {
///     group.values()
///         .filter_map(|(_op, val)| val.read_string().ok())
///         .collect()
/// });
/// assert_eq!(first_values, Some(vec![String::from("a")]));
///
/// let second_group = fields.next();
/// let second_key = second_group.as_ref().map(|(key, _)| key.read_str());
/// assert_eq!(second_key.as_deref(), Some("core"));
/// let second_values = second_group.as_ref().map(|(_, group)| group.len());
/// assert_eq!(second_values, Some(2));
/// let second_values = second_group.map(|(_, group)| {
///     group.values()
///         .filter_map(|(_op, val)| val.read_string().ok())
///         .collect()
/// });
/// assert_eq!(second_values, Some(vec![String::from("b"), String::from("c")]));
/// # Ok(())
/// # }
/// ```
pub struct FieldGroupsIter<'data, 'tokens, E> {
    key_indices: HashMap<&'data [u8], Vec<OpValue<'data, 'tokens, E>>>,
    fields: FieldsIter<'data, 'tokens, E>,
}

impl<'data, 'tokens, E> FieldGroupsIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: &ObjectReader<'data, 'tokens, E>) -> Self {
        let mut key_indices = HashMap::with_capacity(reader.fields_len());
        for (key, op, val) in reader.fields() {
            let entry = key_indices.entry(key.read_scalar().as_bytes());

            match entry {
                Entry::Vacant(x) => {
                    x.insert(Vec::with_capacity(0));
                }
                Entry::Occupied(mut x) => {
                    x.get_mut().push((op, val));
                }
            }
        }

        let fields = reader.fields();

        FieldGroupsIter {
            key_indices,
            fields,
        }
    }

    /// See [the other `at_trailer` documentation](crate::text::FieldsIter::at_trailer)
    pub fn at_trailer(&self) -> Option<ArrayReader<'data, 'tokens, E>> {
        self.fields.at_trailer()
    }
}

impl<'data, 'tokens, E> Iterator for FieldGroupsIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Item = KeyValues<'data, 'tokens, E>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (key, op, value) = self.fields.next()?;

            if let Some((_key, mut entries)) =
                self.key_indices.remove_entry(key.read_scalar().as_bytes())
            {
                if entries.is_empty() {
                    return Some((key, GroupEntry::One((op, value))));
                } else {
                    entries.insert(0, (op, value));
                    return Some((key, GroupEntry::Multiple(entries)));
                }
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.key_indices.len(), None)
    }
}

/// Iterator over fields of an object in the order that they appear
///
/// Since objects can have duplicated keys across fields, this iterator
/// may yield items that have duplicate keys.
///
/// Fields can be easily iterated:
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"name=a core=b core=c")?;
/// let reader = tape.windows1252_reader();
/// let (names, cores) = reader
///     .fields()
///     .fold((0, 0), |(names, cores), (key, _op, _value)| {
///         match key.read_str().as_ref() {
///             "name" => (names + 1, cores),
///             "core" => (names, cores + 1),
///             x => panic!("unexpected key: {}", x),
///         }
///     });
/// assert_eq!((1, 2), (names, cores));
/// # Ok(())
/// # }
/// ```
///
/// And picked apart:
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"name=a core=b core=c")?;
/// let reader = tape.windows1252_reader();
/// let mut fields = reader.fields();
/// let (first_key, _op, first_val) = fields.next().unwrap();
/// assert_eq!(first_key.read_str(), "name");
/// assert_eq!(first_val.read_str().ok().as_deref(), Some("a"));
/// # Ok(())
/// # }
/// ```
pub struct FieldsIter<'data, 'tokens, E> {
    token_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> FieldsIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: &ObjectReader<'data, 'tokens, E>) -> Self {
        FieldsIter {
            token_ind: reader.start_ind,
            end_ind: reader.end_ind,
            tokens: reader.tokens,
            encoding: reader.encoding.clone(),
        }
    }

    /// Exposes the object trailer at the end of the object if it exists. It is
    /// the responsibility of the caller to make sure they have exhausted the
    /// iterator before calling this method, as the any trailer would be at the
    /// end of the object. An object trailer is looks like:
    ///
    /// ```
    /// use jomini::TextTape;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let data = b"brittany_area = { color = { 10 10 10 } 100 200 300 }";
    /// let tape = TextTape::from_slice(data)?;
    /// let reader = tape.windows1252_reader();
    /// let mut root_fields = reader.fields();
    /// let (_brittany, _op, brittany_val) = root_fields.next().unwrap();
    /// let mut brittany_fields = brittany_val.read_object()?.fields();
    ///
    /// // consume iterator
    /// brittany_fields.by_ref().for_each(drop);
    ///
    /// let trailer = brittany_fields.at_trailer().map(|array| {
    ///     array.values()
    ///         .filter_map(|value| value.read_str().ok())
    ///         .collect()
    /// });
    /// assert_eq!(trailer, Some(vec!["100".into(), "200".into(), "300".into()]));
    /// # Ok(())
    /// # }
    /// ```
    pub fn at_trailer(&self) -> Option<ArrayReader<'data, 'tokens, E>> {
        at_trailer(self.tokens, &self.encoding, self.token_ind)
    }
}

impl<'data, 'tokens, E> Iterator for FieldsIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Item = KeyValue<'data, 'tokens, E>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.token_ind >= self.end_ind {
            return None;
        }

        let key_ind = self.token_ind;
        let token = self.tokens[key_ind].clone();
        let key_scalar = match token {
            TextToken::Quoted(x)
            | TextToken::Unquoted(x)
            | TextToken::Parameter(x)
            | TextToken::UndefinedParameter(x) => x,
            TextToken::Array(_) => {
                return None;
            }
            _ => {
                // this is a broken invariant, so we safely recover by saying the object
                // has no more fields
                debug_assert!(false, "All keys should be scalars or have a trailer");
                return None;
            }
        };

        let key_reader = ScalarReader {
            scalar: key_scalar,
            token,
            encoding: self.encoding.clone(),
        };

        let (op, value_ind) = match self.tokens[key_ind + 1] {
            TextToken::Operator(x) => (Some(x), key_ind + 2),
            _ => (None, key_ind + 1),
        };

        // When reading an mixed object (a = { b = { c } 10 10 10 })
        // there is an uneven number of keys and values so we drop the last "field"
        if value_ind >= self.end_ind {
            return None;
        }

        let value_reader = ValueReader {
            value_ind,
            tokens: self.tokens,
            encoding: self.encoding.clone(),
        };
        self.token_ind = next_idx(self.tokens, value_ind);
        Some((key_reader, op, value_reader))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = fields_len(self.tokens, self.token_ind, self.end_ind);
        (len, None)
    }
}

/// A reader for objects
#[derive(Debug, Clone)]
pub struct ObjectReader<'data, 'tokens, E> {
    start_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ObjectReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Create a new object reader from parsed data with encoded strings
    pub fn new(tape: &'tokens TextTape<'data>, encoding: E) -> Self {
        let tokens = tape.tokens();
        ObjectReader {
            tokens,
            end_ind: tokens.len(),
            start_ind: 0,
            encoding,
        }
    }

    /// Return the number of key value pairs that the object contains.
    /// Does not count the object trailer if present
    pub fn fields_len(&self) -> usize {
        fields_len(self.tokens, self.start_ind, self.end_ind)
    }

    /// Iterator over fields as they appear in the object
    ///
    /// See [FieldsIter](crate::text::FieldsIter) for a worked example
    #[inline]
    pub fn fields(&self) -> FieldsIter<'data, 'tokens, E> {
        FieldsIter::new(self)
    }

    /// Iterator over fields that are grouped by key
    ///
    /// See [FieldGroupsIter](crate::text::FieldGroupsIter) for a worked example
    #[inline]
    pub fn field_groups(&self) -> FieldGroupsIter<'data, 'tokens, E> {
        FieldGroupsIter::new(self)
    }
}

/// A text reader that wraps an underlying scalar value
#[derive(Debug, Clone)]
pub struct ScalarReader<'data, E> {
    scalar: Scalar<'data>,
    token: TextToken<'data>,
    encoding: E,
}

impl<'data, E> ScalarReader<'data, E>
where
    E: Encoding,
{
    /// Decode the data with a given string encoding
    #[inline]
    pub fn read_str(&self) -> Cow<'data, str> {
        self.encoding.decode(self.scalar.as_bytes())
    }

    /// Decode the data with a given string encoding
    #[inline]
    pub fn read_string(&self) -> String {
        self.encoding.decode(self.scalar.as_bytes()).into_owned()
    }

    /// Return the underlying scalar
    #[inline]
    pub fn read_scalar(&self) -> Scalar<'data> {
        self.scalar
    }

    /// Return the token that the reader is abstracting
    #[inline]
    pub fn token(&self) -> &TextToken<'data> {
        &self.token
    }
}

/// A text reader for a text value
#[derive(Debug, Clone)]
pub struct ValueReader<'data, 'tokens, E> {
    value_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ValueReader<'data, 'tokens, E> {
    /// Return the token that the reader is abstracting
    #[inline]
    pub fn token(&self) -> &TextToken<'data> {
        &self.tokens[self.value_ind]
    }
}

impl<'data, 'tokens, E> Encoding for ValueReader<'data, 'tokens, E>
where
    E: Encoding,
{
    #[inline]
    fn decode<'a>(&self, data: &'a [u8]) -> Cow<'a, str> {
        self.encoding.decode(data)
    }
}

impl<'data, 'tokens, E> ValueReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Interpret the current value as string
    #[inline]
    pub fn read_str(&self) -> Result<Cow<'data, str>, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .map(|x| self.encoding.decode(x.as_bytes()))
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as string
    #[inline]
    pub fn read_string(&self) -> Result<String, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .map(|x| self.encoding.decode(x.as_bytes()).into_owned())
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as a scalar
    #[inline]
    pub fn read_scalar(&self) -> Result<Scalar<'data>, DeserializeError> {
        self.tokens[self.value_ind]
            .as_scalar()
            .ok_or_else(|| DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not a scalar")),
            })
    }

    /// Interpret the current value as an object
    #[inline]
    pub fn read_object(&self) -> Result<ObjectReader<'data, 'tokens, E>, DeserializeError> {
        match self.tokens[self.value_ind] {
            TextToken::Object(ind) | TextToken::HiddenObject(ind) => Ok(ObjectReader {
                tokens: self.tokens,
                start_ind: self.value_ind + 1,
                end_ind: ind,
                encoding: self.encoding.clone(),
            }),

            // An array can be an object if it is empty or interpreted as an object with only a trailer
            TextToken::Array(ind) => Ok(ObjectReader {
                tokens: self.tokens,
                start_ind: self.value_ind,
                end_ind: ind,
                encoding: self.encoding.clone(),
            }),
            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not an object")),
            }),
        }
    }

    /// Interpret the current value as an array
    #[inline]
    pub fn read_array(&self) -> Result<ArrayReader<'data, 'tokens, E>, DeserializeError> {
        match self.tokens[self.value_ind] {
            TextToken::Array(ind) => Ok(ArrayReader {
                tokens: self.tokens,
                start_ind: self.value_ind + 1,
                end_ind: ind,
                encoding: self.encoding.clone(),
            }),

            // An object can be an array when it has a trailer at the end
            TextToken::Object(_) => {
                let obj = self.read_object().unwrap();
                let mut fields = obj.fields();
                fields.by_ref().for_each(drop);
                Ok(fields.at_trailer().unwrap_or_else(|| ArrayReader {
                    tokens: self.tokens,
                    start_ind: 0,
                    end_ind: 0,
                    encoding: self.encoding.clone(),
                }))
            }

            // A header can be seen as a two element array
            TextToken::Header(_) => Ok(ArrayReader {
                tokens: self.tokens,
                start_ind: self.value_ind,
                end_ind: next_idx(self.tokens, self.value_ind + 1),
                encoding: self.encoding.clone(),
            }),

            _ => Err(DeserializeError {
                kind: DeserializeErrorKind::Unsupported(String::from("not an array")),
            }),
        }
    }
}

/// An iterator over the values of an array
///
/// ```
/// use jomini::TextTape;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let tape = TextTape::from_slice(b"cores={a b}")?;
/// let reader = tape.windows1252_reader();
///
/// let mut all_cores = Vec::new();
/// for (key, _op, value) in reader.fields() {
///     assert_eq!(key.read_str(), "cores");
///     let cores = value.read_array()?;
///     assert_eq!(cores.len(), 2);
///     for value in cores.values() {
///         all_cores.push(value.read_string()?);
///     }
/// }
/// assert_eq!(all_cores, vec![String::from("a"), String::from("b")]);
/// # Ok(())
/// # }
/// ```
pub struct ValuesIter<'data, 'tokens, E> {
    token_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ValuesIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    fn new(reader: &ArrayReader<'data, 'tokens, E>) -> Self {
        ValuesIter {
            token_ind: reader.start_ind,
            end_ind: reader.end_ind,
            tokens: reader.tokens,
            encoding: reader.encoding.clone(),
        }
    }
}

impl<'data, 'tokens, E> Iterator for ValuesIter<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    type Item = ValueReader<'data, 'tokens, E>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.token_ind < self.end_ind {
            let value_ind = self.token_ind;
            self.token_ind = next_idx_header(self.tokens, self.token_ind);
            Some(ValueReader {
                value_ind,
                tokens: self.tokens,
                encoding: self.encoding.clone(),
            })
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = values_len(self.tokens, self.token_ind, self.end_ind);
        (len, Some(len))
    }
}

/// A text reader for sequences of values
#[derive(Debug, Clone)]
pub struct ArrayReader<'data, 'tokens, E> {
    start_ind: usize,
    end_ind: usize,
    tokens: &'tokens [TextToken<'data>],
    encoding: E,
}

impl<'data, 'tokens, E> ArrayReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Iterator over values of an array
    ///
    /// See [ValuesIter](crate::text::ValuesIter) for a worked example
    #[inline]
    pub fn values(&self) -> ValuesIter<'data, 'tokens, E> {
        ValuesIter::new(self)
    }

    /// Returns if the array is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return the number of values in the array
    #[inline]
    pub fn len(&self) -> usize {
        values_len(self.tokens, self.start_ind, self.end_ind)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_value<E>(value: ValueReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        match value.token() {
            TextToken::Object(_) | TextToken::HiddenObject(_) => {
                iterate_object(value.read_object().unwrap());
            }
            TextToken::Array(_) => {
                iterate_array(value.read_array().unwrap());
            }
            TextToken::End(_) => panic!("end!?"),
            TextToken::Operator(_) => panic!("end!?"),
            TextToken::Unquoted(_)
            | TextToken::Quoted(_)
            | TextToken::Header(_)
            | TextToken::Parameter(_)
            | TextToken::UndefinedParameter(_) => {
                let _ = value.read_str().unwrap();
            }
        }
    }

    fn iterate_array<E>(reader: ArrayReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        for value in reader.values() {
            read_value(value);
        }
    }

    fn iterate_object<E>(reader: ObjectReader<E>)
    where
        E: crate::Encoding + Clone,
    {
        for (_key, group) in reader.field_groups() {
            for (_op, value) in group.values() {
                read_value(value);
            }
        }

        let mut fields = reader.fields();
        for (key, _op, value) in fields.by_ref() {
            let _ = key.read_str();
            read_value(value);
        }

        if let Some(trailer) = fields.at_trailer() {
            iterate_array(trailer);
        }
    }

    #[test]
    fn simple_text_reader_text() {
        let data = b"foo=bar";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        assert_eq!(reader.fields_len(), 1);

        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));
        assert_eq!(value.read_string().unwrap(), String::from("bar"));

        assert!(iter.next().is_none());
    }

    #[test]
    fn simple_text_reader_obj() {
        let data = b"foo={bar=qux}";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();

        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));

        let nested = value.read_object().unwrap();
        let mut nested_iter = nested.fields();
        let (key2, _op, value2) = nested_iter.next().unwrap();
        assert_eq!(key2.read_string(), String::from("bar"));
        assert_eq!(value2.read_string().unwrap(), String::from("qux"));
        assert!(nested_iter.next().is_none());
        assert!(iter.next().is_none());
    }

    #[test]
    fn simple_text_reader_array() {
        let data = b"foo={bar qux}";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();

        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("foo"));

        let nested = value.read_array().unwrap();
        let mut values = nested.values();
        assert_eq!(nested.len(), 2);
        let value1 = values.next().unwrap().read_string().unwrap();
        let value2 = values.next().unwrap().read_string().unwrap();

        assert!(values.next().is_none());
        assert_eq!(value1, String::from("bar"));
        assert_eq!(value2, String::from("qux"));
    }

    #[test]
    fn text_reader_hidden_object() {
        let data = b"levels={10 0=1 0=2}";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();

        assert_eq!(reader.fields_len(), 1);
        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("levels"));

        let nested = value.read_array().unwrap();
        assert_eq!(nested.len(), 2);

        let mut values = nested.values();
        let value1 = values.next().unwrap().read_string().unwrap();
        assert_eq!(value1, String::from("10"));

        let hidden = values.next().unwrap().read_object().unwrap();
        assert_eq!(hidden.fields_len(), 2);
        let mut hidden_iter = hidden.fields();
        let (key, _op, value) = hidden_iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("0"));
        assert_eq!(value.read_string().unwrap(), String::from("1"));

        let (key, _op, value) = hidden_iter.next().unwrap();
        assert_eq!(key.read_string(), String::from("0"));
        assert_eq!(value.read_string().unwrap(), String::from("2"));

        assert!(hidden_iter.next().is_none());
        assert!(values.next().is_none());
    }

    #[test]
    fn text_reader_read_fields() {
        let data = b"name=aaa name=bbb core=123 core=456 name=ccc name=ddd";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();

        let mut field_groups = reader.field_groups();
        let (key, values) = field_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));

        let values = values.values().collect::<Vec<_>>();
        assert_eq!(values.len(), 4);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("aaa"));
        assert_eq!(values[1].1.read_string().unwrap(), String::from("bbb"));
        assert_eq!(values[2].1.read_string().unwrap(), String::from("ccc"));
        assert_eq!(values[3].1.read_string().unwrap(), String::from("ddd"));

        let (key, values) = field_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("core"));

        let values = values.values().collect::<Vec<_>>();
        assert_eq!(values.len(), 2);
        assert_eq!(values[0].1.read_string().unwrap(), String::from("123"));
        assert_eq!(values[1].1.read_string().unwrap(), String::from("456"));
    }

    #[test]
    fn text_reader_read_fields_nested() {
        let data =
            b"army={name=aaa unit={name=bbb} unit={name=ccc}} army={name=ddd unit={name=eee}}";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut field_groups = reader.field_groups();

        let (key, army_values) = field_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("army"));
        assert_eq!(army_values.len(), 2);

        let army_values = army_values.values().collect::<Vec<_>>();
        let aaa = army_values[0].1.read_object().unwrap();
        let mut aaa_groups = aaa.field_groups();
        assert_eq!(aaa.fields_len(), 3);

        let (key, values) = aaa_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(values.len(), 1);
        assert_eq!(
            values.values().nth(0).unwrap().1.read_string().unwrap(),
            String::from("aaa")
        );

        let (key, values) = aaa_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("unit"));
        assert_eq!(values.len(), 2);

        let bbb = values.values().nth(0).unwrap().1.read_object().unwrap();
        let mut bbb_fields = bbb.fields();
        let (key, _, value) = bbb_fields.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("bbb"));

        let ccc = values.values().nth(1).unwrap().1.read_object().unwrap();
        let mut ccc_fields = ccc.fields();
        let (key, _, value) = ccc_fields.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("ccc"));

        let ddd = army_values[1].1.read_object().unwrap();
        assert_eq!(ddd.fields_len(), 2);

        let mut ddd_groups = ddd.field_groups();
        let (key, values) = ddd_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(values.len(), 1);
        assert_eq!(
            values.values().nth(0).unwrap().1.read_string().unwrap(),
            String::from("ddd")
        );

        let (key, values) = ddd_groups.next().unwrap();
        assert_eq!(key.read_string(), String::from("unit"));
        assert_eq!(values.len(), 1);

        let eee = values.values().nth(0).unwrap().1.read_object().unwrap();
        let mut eee_fields = eee.fields();
        let (key, _, value) = eee_fields.next().unwrap();
        assert_eq!(key.read_string(), String::from("name"));
        assert_eq!(value.read_string().unwrap(), String::from("eee"));
    }

    #[test]
    fn text_reader_read_fields_consume() {
        let data = b"name=aaa name=bbb core=123 name=ccc name=ddd";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut count = 0;
        for (_key, entries) in reader.field_groups() {
            for (_i, (_op, value)) in entries.values().enumerate() {
                count += value.read_scalar().map(|_| 1).unwrap_or(0);
            }
        }

        assert_eq!(count, 5);
    }

    #[test]
    fn text_reader_mixed_object() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_str(), "brittany_area");

        let mut keys = vec![];
        let brittany = value.read_object().unwrap();
        let mut fields = brittany.fields();
        while let Some((key, _op, _value)) = fields.next() {
            keys.push(key.read_str())
        }

        assert_eq!(keys, vec![String::from("color"),]);

        let mut values = vec![];
        let trailer = fields.at_trailer().unwrap();
        for value in trailer.values() {
            let nv = value.token();
            values.push((*nv).clone());
        }

        assert_eq!(
            values,
            vec![
                TextToken::Unquoted(Scalar::new(b"169")),
                TextToken::Unquoted(Scalar::new(b"170")),
                TextToken::Unquoted(Scalar::new(b"171")),
                TextToken::Unquoted(Scalar::new(b"172")),
                TextToken::Unquoted(Scalar::new(b"4384")),
            ]
        );
    }

    #[test]
    fn text_reader_mixed_object_fields() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_str(), "brittany_area");

        let brittany = value.read_object().unwrap();
        let mut field_groups = brittany.field_groups();
        field_groups.next().unwrap();
        assert!(field_groups.next().is_none());

        let mut values = vec![];
        let trailer = field_groups.at_trailer().unwrap();
        for value in trailer.values() {
            let nv = value.token();
            values.push((*nv).clone());
        }

        assert_eq!(
            values,
            vec![
                TextToken::Unquoted(Scalar::new(b"169")),
                TextToken::Unquoted(Scalar::new(b"170")),
                TextToken::Unquoted(Scalar::new(b"171")),
                TextToken::Unquoted(Scalar::new(b"172")),
                TextToken::Unquoted(Scalar::new(b"4384")),
            ]
        );
    }

    #[test]
    fn text_trailer_size_hints() {
        let data = br#"brittany_area = { #5
            color = { 118  99  151 }
            color = { 118  99  151 }
            169 170 171 172 4384
        }"#;

        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let (_key, _op, brittany) = reader.fields().next().unwrap();
        let brittany_reader = brittany.read_object().unwrap();

        let mut fields = brittany_reader.fields();
        let (lower_bound, upper_bound) = fields.size_hint();
        assert_eq!(lower_bound, brittany_reader.fields_len());
        assert_eq!(lower_bound, 2);
        assert!(upper_bound.is_none() || upper_bound == Some(7));

        let _ = fields.next();
        let (lower_bound, upper_bound) = fields.size_hint();
        assert_eq!(lower_bound, 1);
        assert!(upper_bound.is_none() || upper_bound == Some(6));

        let mut groups = brittany_reader.field_groups();
        let (lower_bound, upper_bound) = groups.size_hint();
        assert_eq!(lower_bound, 1);
        assert!(upper_bound.is_none() || upper_bound == Some(6));

        let _ = groups.next();
        let (lower_bound, upper_bound) = groups.size_hint();
        assert_eq!(lower_bound, 0);
        assert!(upper_bound.is_none() || upper_bound == Some(5));
    }

    #[test]
    fn text_reader_empty_container() {
        let data = b"active_idea_groups={ }";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_str(), "active_idea_groups");

        let empty_array = value.read_array().unwrap();
        assert_eq!(0, empty_array.len());
        assert!(empty_array.values().next().is_none());

        let empty_object = value.read_object().unwrap();
        let mut empty_object_iter = empty_object.fields();
        assert_eq!(0, empty_object.fields_len());
        assert!(empty_object_iter.next().is_none());
        assert!(empty_object_iter.at_trailer().is_none());
    }

    #[test]
    fn text_reader_header() {
        let data = b"color = rgb { 10 20 30 }";
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        let mut iter = reader.fields();
        let (key, _op, value) = iter.next().unwrap();
        assert_eq!(key.read_str(), "color");

        let header_array = value.read_array().unwrap();
        let mut values = header_array.values();
        let rgb = values.next().unwrap();
        assert_eq!(rgb.read_str().unwrap(), "rgb");

        let vals = values.next().unwrap();
        let s = vals.read_array().unwrap();
        let svals = s.values();

        let colors = svals
            .map(|x| x.read_scalar().unwrap())
            .map(|x| x.to_u64().unwrap())
            .collect::<Vec<u64>>();

        assert_eq!(colors, vec![10, 20, 30]);
    }

    #[test]
    fn reader_crash1() {
        let data = b"a=r{}";
        let tape = TextTape::from_slice(data).unwrap();
        iterate_object(tape.windows1252_reader());
    }

    #[test]
    fn text_reader_object_fields() {
        let data = b"a{b=}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_op2() {
        let data = b"a{}b>{}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_dupe() {
        let data = b"a{b=c d=E d}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_header() {
        let data = b"a{}b>r{}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    #[test]
    fn text_reader_object_fields_dupe2() {
        let data = b"a{b=c d b}";
        if let Ok(tape) = TextTape::from_slice(data) {
            let reader = tape.windows1252_reader();
            iterate_object(reader);
        }
    }

    // Investigate why this test case is so slow
    // #[test]
    // fn text_reader_oom() {
    //     let data = b"w{w={w={w={a={w={w={.=w={W={={w={w={a={w={w={.=w={W={w={w={b=}.{B{6={b={w={w={.b=}}}ws!=}}}}={=b}}=}}}}={w=}}}}}w={w={b=}.{B{6={b={w={w={.b=}}}ws!=}}}}={=b}}=}}}}={w=}}}}}";
    //     if let Ok(tape) = TextTape::from_slice(data) {
    //         dbg!(&tape);
    //         assert!(false);
    //         let reader = tape.windows1252_reader();
    //         iterate_object(reader);
    //     }
    // }
}
