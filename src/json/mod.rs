//! Handles conversion of plaintext clausewitz format to JSON
//!
//! ```
//! use jomini::{TextTape, json::{JsonOptions, DuplicateKeyMode}};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let tape = TextTape::from_slice(b"core=a core=b")?;
//! let reader = tape.windows1252_reader();
//!
//! let options = JsonOptions::new()
//!     .with_prettyprint(false)
//!     .with_duplicate_keys(DuplicateKeyMode::Preserve);
//!
//! // These are the default options
//! assert_eq!(options, JsonOptions::default());
//!
//! let actual = reader.json()
//!     .with_options(options)
//!     .to_string();
//! assert_eq!(actual, r#"{"core":"a","core":"b"}"#);
//! # Ok(())
//! # }
//! ```
//!
//! The scope of the JSON can be narrowed to an inner value. This comes in handy
//! when the parsed document is large but only a small subset of it needs to be
//! exposed with JSON.
//!
//! ```
//! use jomini::{TextTape};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let tape = TextTape::from_slice(b"nums={1 2 3 4}")?;
//! let reader = tape.windows1252_reader();
//! let mut fields = reader.fields();
//! let (_key, _op, value) = fields.next().unwrap();
//! let array = value.read_array()?;
//! let actual = array.json().to_string();
//! let expected = r#"[1,2,3,4]"#;
//! assert_eq!(&actual, expected);
//! # Ok(())
//! # }
//! ```

use crate::{
    text::{ArrayReader, GroupEntry, ObjectReader, Operator, ScalarReader, ValueReader},
    Encoding, TextToken,
};
use serde::{
    ser::{SerializeMap, SerializeSeq},
    Serialize, Serializer,
};
use std::ops::Deref;

/// Customizes the JSON output
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct JsonOptions {
    /// Controls if the JSON should be pretty printed
    pretty: bool,

    /// Controls the how duplicate keys are formatted
    duplicate_keys: DuplicateKeyMode,

    /// Controls how values are narrowed to a more specific type
    type_narrowing: TypeNarrowing,
}

impl JsonOptions {
    /// Creates the structure with default options
    pub fn new() -> Self {
        JsonOptions::default()
    }

    /// Sets if the JSON should be pretty printed or minified
    pub fn with_prettyprint(mut self, pretty: bool) -> JsonOptions {
        self.pretty = pretty;
        self
    }

    /// Sets how duplicate keys are formatted
    pub fn with_duplicate_keys(mut self, duplicate_keys: DuplicateKeyMode) -> JsonOptions {
        self.duplicate_keys = duplicate_keys;
        self
    }

    /// Sets when a value is attempted to be narrowed to a more specific type
    pub fn with_type_narrowing(mut self, type_narrowing: TypeNarrowing) -> JsonOptions {
        self.type_narrowing = type_narrowing;
        self
    }

    /// Returns the factor to multiply the token length for a size estimate.
    ///
    /// The numbers were found empirically by taking CK3 and EU4 meta data
    /// from saves and seeing how the JSON output compared with the token length
    pub(crate) fn output_len_factor(&self) -> usize {
        match (self.pretty, self.duplicate_keys) {
            (false, DuplicateKeyMode::Group | DuplicateKeyMode::Preserve) => 10,
            (true, DuplicateKeyMode::Group | DuplicateKeyMode::Preserve) => 20,
            (false, DuplicateKeyMode::KeyValuePairs) => 15,
            (true, DuplicateKeyMode::KeyValuePairs) => 60,
        }
    }
}

impl Default for JsonOptions {
    fn default() -> Self {
        Self {
            pretty: false,
            duplicate_keys: DuplicateKeyMode::Preserve,
            type_narrowing: TypeNarrowing::All,
        }
    }
}

/// Controls when a value is attempted to be narrowed to a more specific type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeNarrowing {
    /// Attempt to narrow all values to more specific type
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, TypeNarrowing}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(br#"a="01" b=02 c="yes" d=no"#)?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let options = JsonOptions::new()
    ///     .with_type_narrowing(TypeNarrowing::All);
    ///
    /// let actual = reader.json()
    ///     .with_options(options)
    ///     .to_string();
    /// assert_eq!(actual, r#"{"a":1,"b":2,"c":true,"d":false}"#);
    /// # Ok(())
    /// # }
    /// ```
    All,

    /// Only attempt to narrow unquoted values to a more specific type
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, TypeNarrowing}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(br#"a="01" b=02 c="yes" d=no"#)?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let options = JsonOptions::new()
    ///     .with_type_narrowing(TypeNarrowing::Unquoted);
    ///
    /// let actual = reader.json()
    ///     .with_options(options)
    ///     .to_string();
    /// assert_eq!(actual, r#"{"a":"01","b":2,"c":"yes","d":false}"#);
    /// # Ok(())
    /// # }
    /// ```
    Unquoted,

    /// Don't attempt any narrowing (all values will be strings)
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, TypeNarrowing}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(br#"a="01" b=02 c="yes" d=no"#)?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let options = JsonOptions::new()
    ///     .with_type_narrowing(TypeNarrowing::None);
    ///
    /// let actual = reader.json()
    ///     .with_options(options)
    ///     .to_string();
    /// assert_eq!(actual, r#"{"a":"01","b":"02","c":"yes","d":"no"}"#);
    /// # Ok(())
    /// # }
    /// ```
    None,
}

/// Controls JSON structure when duplicate keys are encountered
///
/// It's debatable whether [duplicate keys is valid
/// JSON](https://stackoverflow.com/q/21832701), so this allows one to customize
/// output depending how flexible a downstream client is at handling JSON.
///
/// The options are either:
///
/// - Group values into an array under a single field
/// - Preserve the duplicate keys
/// - Rewrite objects as an array of key value pairs
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DuplicateKeyMode {
    /// Group values into an array under a single field
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, DuplicateKeyMode}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(b"a={b=1} c={b=1 b=2}")?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let options = JsonOptions::new()
    ///     .with_duplicate_keys(DuplicateKeyMode::Group);
    ///
    /// let actual = reader.json()
    ///     .with_options(options)
    ///     .to_string();
    /// assert_eq!(actual, r#"{"a":{"b":1},"c":{"b":[1,2]}}"#);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// As shown above, downstream clients will need to be flexible enough to
    /// handle grouped and ungrouped keys, and may prove cumbersome or
    /// challenging to leverage automatic deserialization logic. Python or
    /// Javascript clients may like this output due to their more dynamic typing
    /// nature.
    ///
    /// Grouping keys together will have a small but measurable impact on
    /// performance
    Group,

    /// Preserve the duplicate keys (the default behavior)
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, DuplicateKeyMode}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(b"a={b=1} c={b=1 b=2}")?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let actual = reader.json()
    ///     .with_options(JsonOptions::new())
    ///     .to_string();
    /// assert_eq!(actual, r#"{"a":{"b":1},"c":{"b":1,"b":2}}"#);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Preserving duplicate keys is the default mode as it represents the most
    /// concise output and most closely matches the input.
    ///
    /// Insertion order of an object's keys are maintained.
    ///
    /// Python and Javascript clients may not like this output as their builtin
    /// JSON modules don't handle duplicate keys well. However, lower level JSON
    /// parsers like simd-json tend to handle duplicate keys just fine.
    Preserve,

    /// Rewrite objects as an array of 2 element arrays (the key and the value).
    ///
    /// ```
    /// use jomini::{TextTape, json::{JsonOptions, DuplicateKeyMode}};
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let tape = TextTape::from_slice(b"c=0 b={1 2}")?;
    /// let reader = tape.windows1252_reader();
    ///
    /// let options = JsonOptions::new()
    ///     .with_duplicate_keys(DuplicateKeyMode::KeyValuePairs);
    ///
    /// let actual = reader.json()
    ///     .with_options(options)
    ///     .to_string();
    /// assert_eq!(actual, r#"{"type":"obj","val":[["c",0],["b",{"type":"array","val":[1,2]}]]}"#);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Objects and arrays are now transformed into adjacently tagged objects
    /// (to borrow [a term from
    /// serde](https://serde.rs/enum-representations.html#adjacently-tagged)).
    /// Objects have a type of `obj` and arrays have a type of `array`. This
    /// adjacently tagged object is needed to disambiguate between objects and
    /// arrays if both are going to be represented with JSON arrays.
    ///
    /// This output has the largest departure from the input and is the most
    /// verbose, but it allows one to use inflexible DOM parsers like those seen
    /// in Python and Javascript and still maintain the positioning of duplicate
    /// keys. Preserving positioning is important when interpretting an object
    /// is dependant on the order of the keys and duplicate keys may affect
    /// subsequent fields.
    KeyValuePairs,
}

fn writer_json<W, S>(writer: W, pretty: bool, ser: S) -> Result<(), std::io::Error>
where
    W: std::io::Write,
    S: serde::Serialize,
{
    let result = if pretty {
        serde_json::to_writer_pretty(writer, &ser)
    } else {
        serde_json::to_writer(writer, &ser)
    };

    result.map_err(|e| e.into())
}

fn vec_json<F>(mut out: Vec<u8>, write_fn: F) -> Vec<u8>
where
    F: FnOnce(&mut Vec<u8>) -> Result<(), std::io::Error>,
{
    // Since we control the writer (and writing to a vec shouldn't fail) and
    // the type that is being serialized, any error that arises from here
    // would be a programmer error and doesn't need to propagate
    if let Err(e) = write_fn(&mut out) {
        panic!("failed to serialize json to vector: {}", e)
    } else {
        out
    }
}

fn string_json(json: Vec<u8>) -> String {
    // From serde_json source: "we don't generate invalid utf-8"
    unsafe { String::from_utf8_unchecked(json) }
}

/// Creates JSON from an object reader
pub struct JsonObjectBuilder<'data, 'tokens, E> {
    reader: ObjectReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> JsonObjectBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    /// Output JSON with the set of options
    pub fn with_options(mut self, options: JsonOptions) -> Self {
        self.options = options;
        self
    }

    /// Output JSON to the given writer
    pub fn to_writer<W>(self, writer: W) -> Result<(), std::io::Error>
    where
        W: std::io::Write,
    {
        writer_json(writer, self.options.pretty, &self)
    }

    /// Output JSON to vec that contains UTF-8 data
    pub fn to_vec(self) -> Vec<u8> {
        let out = Vec::with_capacity(self.reader.tokens_len() * self.options.output_len_factor());
        vec_json(out, |x| self.to_writer(x))
    }

    /// Output JSON to a string
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(self) -> String {
        string_json(self.to_vec())
    }
}

impl<'data, 'tokens, E> ObjectReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Converts the object to its JSON representation
    pub fn json(&self) -> JsonObjectBuilder<'data, 'tokens, E> {
        JsonObjectBuilder {
            reader: self.clone(),
            options: JsonOptions::default(),
        }
    }
}

/// Creates JSON from an array reader
pub struct JsonArrayBuilder<'data, 'tokens, E> {
    reader: ArrayReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> JsonArrayBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    /// Output JSON with the set of options
    pub fn with_options(mut self, options: JsonOptions) -> Self {
        self.options = options;
        self
    }

    /// Output JSON to the given writer
    pub fn to_writer<W>(self, writer: W) -> Result<(), std::io::Error>
    where
        W: std::io::Write,
    {
        writer_json(writer, self.options.pretty, &self)
    }

    /// Output JSON to vec that contains UTF-8 data
    pub fn to_vec(self) -> Vec<u8> {
        let out = Vec::with_capacity(self.reader.tokens_len() * self.options.output_len_factor());
        vec_json(out, |x| self.to_writer(x))
    }

    /// Output JSON to a string
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(self) -> String {
        string_json(self.to_vec())
    }
}

impl<'data, 'tokens, E> ArrayReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Converts the object to its JSON representation
    pub fn json(&self) -> JsonArrayBuilder<'data, 'tokens, E> {
        JsonArrayBuilder {
            reader: self.clone(),
            options: JsonOptions::default(),
        }
    }
}

/// Creates JSON from a value reader
pub struct JsonValueBuilder<'data, 'tokens, E> {
    reader: ValueReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> JsonValueBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    /// Output JSON with the set of options
    pub fn with_options(mut self, options: JsonOptions) -> Self {
        self.options = options;
        self
    }

    /// Output JSON to the given writer
    pub fn to_writer<W>(self, writer: W) -> Result<(), std::io::Error>
    where
        W: std::io::Write,
    {
        writer_json(writer, self.options.pretty, &self)
    }

    /// Output JSON to vec that contains UTF-8 data
    pub fn to_vec(self) -> Vec<u8> {
        let out = Vec::with_capacity(self.reader.tokens_len() * self.options.output_len_factor());
        vec_json(out, |x| self.to_writer(x))
    }

    /// Output JSON to a string
    #[allow(clippy::inherent_to_string)]
    pub fn to_string(self) -> String {
        string_json(self.to_vec())
    }
}

impl<'data, 'tokens, E> ValueReader<'data, 'tokens, E>
where
    E: Encoding + Clone,
{
    /// Converts the value to its JSON representation
    pub fn json(&self) -> JsonValueBuilder<'data, 'tokens, E> {
        JsonValueBuilder {
            reader: self.clone(),
            options: JsonOptions::default(),
        }
    }
}

fn serialize_scalar<E, S>(reader: &ValueReader<E>, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    E: Encoding + Clone,
{
    let scalar = reader.read_scalar().unwrap();
    if let Ok(x) = scalar.to_bool() {
        return s.serialize_bool(x);
    }

    let signed = scalar.to_i64();
    let unsigned = scalar.to_u64();
    let float = scalar.to_f64();

    // We only want to serialize numbers that are perfectly representable
    // with 64 bit floating point, else the value will be stringified
    match (signed, unsigned, float) {
        (Ok(x), _, Ok(_)) => s.serialize_i64(x),
        (_, Ok(x), Ok(_)) => s.serialize_u64(x),
        (_, _, Ok(f)) => s.serialize_f64(f),
        _ => s.serialize_str(reader.read_str().unwrap().deref()),
    }
}

fn serialize_parameter<S>(body: &str, defined: bool, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut result = String::with_capacity(body.len() + 3);
    result.push('[');

    if !defined {
        result.push('!');
    }

    result.push_str(body.as_ref());
    result.push(']');
    s.serialize_str(&result)
}

impl<E> Serialize for JsonValueBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.reader.token() {
            TextToken::Unquoted(_) if self.options.type_narrowing != TypeNarrowing::None => {
                serialize_scalar(&self.reader, serializer)
            }
            TextToken::Quoted(_) if self.options.type_narrowing == TypeNarrowing::All => {
                serialize_scalar(&self.reader, serializer)
            }
            TextToken::Unquoted(_) | TextToken::Quoted(_) => {
                serializer.serialize_str(self.reader.read_str().unwrap().deref())
            }
            TextToken::Array { .. } => {
                let array_reader = self.reader.read_array().unwrap();
                array_reader
                    .json()
                    .with_options(self.options)
                    .serialize(serializer)
            }
            TextToken::Object { .. } => {
                let object_reader = self.reader.read_object().unwrap();
                object_reader
                    .json()
                    .with_options(self.options)
                    .serialize(serializer)
            }
            TextToken::Header(_) => {
                let arr = self.reader.read_array().unwrap();
                let mut values = arr.values();
                let key_reader = values.next().unwrap();
                let value_reader = values.next().unwrap();

                let mut map = serializer.serialize_map(None)?;
                map.serialize_entry(
                    &key_reader.read_str().unwrap(),
                    &value_reader.json().with_options(self.options),
                )?;
                map.end()
            }
            TextToken::End(_)
            | TextToken::Operator(_)
            | TextToken::Parameter(_)
            | TextToken::UndefinedParameter(_)
            | TextToken::MixedContainer => serializer.serialize_none(),
        }
    }
}

impl<E> Serialize for JsonObjectBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self.options.duplicate_keys {
            DuplicateKeyMode::Group => {
                let mut field_groups = self.reader.field_groups();
                let mut map = serializer.serialize_map(None)?;

                for (key, group) in field_groups.by_ref() {
                    match group {
                        GroupEntry::One((op, val)) => {
                            let v = OperatorValue {
                                operator: op,
                                value: val,
                                options: self.options,
                            };
                            map.serialize_entry(&KeyScalarWrapper { reader: key }, &v)?;
                        }
                        GroupEntry::Multiple(values) => {
                            let values: Vec<_> = values
                                .iter()
                                .map(|(op, val)| OperatorValue {
                                    operator: *op,
                                    value: val.clone(),
                                    options: self.options,
                                })
                                .collect();
                            map.serialize_entry(&KeyScalarWrapper { reader: key }, &values)?;
                        }
                    }
                }

                let rest = field_groups.remainder();
                if !rest.is_empty() {
                    map.serialize_entry(
                        "remainder",
                        &InnerSerArray {
                            reader: rest,
                            options: self.options,
                        },
                    )?;
                }

                map.end()
            }
            DuplicateKeyMode::Preserve => {
                let mut map = serializer.serialize_map(None)?;
                let mut fields = self.reader.fields();
                for (key, op, val) in fields.by_ref() {
                    let v = OperatorValue {
                        operator: op,
                        value: val,
                        options: self.options,
                    };
                    map.serialize_entry(&KeyScalarWrapper { reader: key }, &v)?;
                }

                let remainder = fields.remainder();
                if !remainder.is_empty() {
                    map.serialize_entry(
                        "remainder",
                        &InnerSerArray {
                            reader: remainder,
                            options: self.options,
                        },
                    )?;
                }

                map.end()
            }
            DuplicateKeyMode::KeyValuePairs => {
                let mut map = serializer.serialize_map(None)?;
                map.serialize_entry("type", "obj")?;
                map.serialize_entry(
                    "val",
                    &SerTapeTyped {
                        reader: self.reader.clone(),
                        options: self.options,
                    },
                )?;
                map.end()
            }
        }
    }
}

pub(crate) struct OperatorValue<'data, 'tokens, E> {
    operator: Option<Operator>,
    value: ValueReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> Serialize for OperatorValue<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if let Some(op) = self.operator {
            let mut map = serializer.serialize_map(None)?;
            let reader = &self.value;
            map.serialize_entry(op.name(), &reader.json().with_options(self.options))?;
            map.end()
        } else {
            self.value
                .json()
                .with_options(self.options)
                .serialize(serializer)
        }
    }
}

pub(crate) struct InnerSerArray<'data, 'tokens, E> {
    reader: ArrayReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> Serialize for InnerSerArray<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;
        let mut iter = self.reader.values();
        let mut window = [iter.next(), iter.next(), iter.next()];

        while let Some(first_val) = &window[0] {
            if first_val.token() == &TextToken::MixedContainer {
                window.swap(1, 0);
                window.swap(2, 1);
                window[2] = iter.next();
                continue;
            }

            if let Some(op_reader) = &window[1] {
                if let TextToken::Operator(op) = op_reader.token() {
                    if let Some(value) = &window[2] {
                        seq.serialize_element(&SingleObject {
                            key: first_val.clone(),
                            op: *op,
                            value: value.clone(),
                            options: self.options,
                        })?;

                        window = [iter.next(), iter.next(), iter.next()];
                        continue;
                    }
                }
            }

            let v = OperatorValue {
                operator: None,
                value: first_val.clone(),
                options: self.options,
            };
            seq.serialize_element(&v)?;

            window.swap(1, 0);
            window.swap(2, 1);
            window[2] = iter.next();
        }

        seq.end()
    }
}

impl<E> Serialize for JsonArrayBuilder<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let inner = InnerSerArray {
            reader: self.reader.clone(),
            options: self.options,
        };

        if self.options.duplicate_keys != DuplicateKeyMode::KeyValuePairs {
            inner.serialize(serializer)
        } else {
            let mut map = serializer.serialize_map(None)?;
            map.serialize_entry("type", "array")?;
            map.serialize_entry("val", &inner)?;
            map.end()
        }
    }
}

pub(crate) struct SingleObject<'data, 'tokens, E> {
    key: ValueReader<'data, 'tokens, E>,
    op: Operator,
    value: ValueReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> Serialize for SingleObject<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(None)?;
        let op = if self.op == Operator::Equal {
            None
        } else {
            Some(self.op)
        };

        let value = OperatorValue {
            operator: op,
            value: self.value.clone(),
            options: self.options,
        };

        if let Ok(x) = self.key.read_str() {
            map.serialize_key(&x)?;
        } else {
            map.serialize_key("__invalid_key")?;
        }

        map.serialize_value(&value)?;
        map.end()
    }
}

pub(crate) struct SerTapeTyped<'data, 'tokens, E> {
    reader: ObjectReader<'data, 'tokens, E>,
    options: JsonOptions,
}

impl<E> Serialize for SerTapeTyped<'_, '_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(None)?;
        let mut fields = self.reader.fields();
        for (key, op, val) in fields.by_ref() {
            let v = OperatorValue {
                operator: op,
                value: val,
                options: self.options,
            };
            seq.serialize_element(&(KeyScalarWrapper { reader: key }, &v))?;
        }

        let remainder = fields.remainder();
        if !remainder.is_empty() {
            let trailer_array = InnerSerArray {
                reader: remainder,
                options: self.options,
            };

            seq.serialize_element(&trailer_array)?;
        }

        seq.end()
    }
}

pub(crate) struct KeyScalarWrapper<'data, E> {
    reader: ScalarReader<'data, E>,
}

impl<E> Serialize for KeyScalarWrapper<'_, E>
where
    E: Encoding + Clone,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let body = self.reader.read_str();
        match self.reader.token() {
            TextToken::Parameter(_) => {
                // Extract parameter name from [[param_name]] format
                if body.starts_with("[[") && body.ends_with("]]") {
                    let param_name = &body[2..body.len() - 2];
                    serialize_parameter(param_name, true, serializer)
                } else {
                    serialize_parameter(&body, true, serializer)
                }
            }
            TextToken::UndefinedParameter(_) => {
                // Extract parameter name from [[!param_name]] format
                if body.starts_with("[[!") && body.ends_with("]]") {
                    let param_name = &body[3..body.len() - 2];
                    serialize_parameter(param_name, false, serializer)
                } else {
                    serialize_parameter(&body, false, serializer)
                }
            }
            _ => serializer.serialize_str(&body),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{TextTape, Windows1252Encoding};

    fn serialize_with(data: &[u8], options: JsonOptions) -> String {
        let tape = TextTape::from_slice(data).unwrap();
        let reader = tape.windows1252_reader();
        reader.json().with_options(options).to_string()
    }

    fn serialize(data: &[u8]) -> String {
        serialize_with(data, JsonOptions::default())
    }

    #[test]
    fn test_serialize_to_json() {
        let json = serialize(b"foo=bar");
        assert_eq!(&json, r#"{"foo":"bar"}"#);
    }

    #[test]
    fn test_simple_types() {
        let json = serialize(b"foo=bar num=1 bool=no bool2=yes pi=3.14");
        let expected = r#"{"foo":"bar","num":1,"bool":false,"bool2":true,"pi":3.14}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_object() {
        let json = serialize(b"foo={prop=a bar={num=1}}");
        let expected = r#"{"foo":{"prop":"a","bar":{"num":1}}}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_array() {
        let json = serialize(b"nums={1 2 3 4}");
        let expected = r#"{"nums":[1,2,3,4]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_duplicate_fields() {
        let json = serialize(b"core=AAA core=BBB");
        let expected = r#"{"core":"AAA","core":"BBB"}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_duplicate_fields_grouped() {
        let json = serialize_with(
            b"core=AAA core=BBB",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::Group,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"core":["AAA","BBB"]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_duplicate_fields_typed() {
        let json = serialize_with(
            b"core=AAA core=BBB",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::KeyValuePairs,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"type":"obj","val":[["core","AAA"],["core","BBB"]]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_header() {
        let json = serialize(b"color = rgb { 100 200 150 }");
        let expected = r#"{"color":{"rgb":[100,200,150]}}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_large_numbers() {
        let json = serialize(b"identity = 18446744073709547616");
        let expected = r#"{"identity":"18446744073709547616"}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_large_negative_numbers() {
        let json = serialize(b"identity = -90071992547409097");
        let expected = r#"{"identity":"-90071992547409097"}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_object_pretty() {
        let json = serialize_with(
            b"foo={prop=a bar={num=1}}",
            JsonOptions {
                pretty: true,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{
  "foo": {
    "prop": "a",
    "bar": {
      "num": 1
    }
  }
}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_array_typed() {
        let json = serialize_with(
            b"nums={1 2}",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::KeyValuePairs,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"type":"obj","val":[["nums",{"type":"array","val":[1,2]}]]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_1() {
        let json = serialize(b"area = { color = { 10 } 1 2 }");
        let expected = r#"{"area":{"color":[10],"remainder":[1,2]}}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_2() {
        let json = serialize_with(
            b"area = { color = { 10 } 1 2 }",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::Group,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"area":{"color":[10],"remainder":[1,2]}}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_3() {
        let json = serialize_with(
            b"area = { color = { 10 } 1 2 }",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::KeyValuePairs,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"type":"obj","val":[["area",{"type":"obj","val":[["color",{"type":"array","val":[10]}],[1,2]]}]]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_4() {
        let json = serialize(b"levels={ 10 0=2 1=2 }");
        let expected = r#"{"levels":[10,{"0":2},{"1":2}]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_5() {
        let json = serialize(b"mixed={ a=b 10 c=d 20 }");
        let expected = r#"{"mixed":{"a":"b","remainder":[10,{"c":"d"},20]}}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_mixed_container_10() {
        let json = serialize(
            br"on_actions = {
            faith_holy_order_land_acquisition_pulse
            delay = { days = { 5 10 }}
            faith_heresy_events_pulse
            delay = { days = { 15 20 }}
            faith_fervor_events_pulse
          }",
        );
        let expected = r#"{"on_actions":["faith_holy_order_land_acquisition_pulse",{"delay":{"days":[5,10]}},"faith_heresy_events_pulse",{"delay":{"days":[15,20]}},"faith_fervor_events_pulse"]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_parameter_definitions_typed() {
        let json = serialize_with(
            b"generate_advisor = { [[scaled_skill] a=b ] [[!scaled_skill] c=d ]  }",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::KeyValuePairs,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"type":"obj","val":[["generate_advisor",{"type":"obj","val":[["[scaled_skill]",{"type":"obj","val":[["a","b"]]}],["[!scaled_skill]",{"type":"obj","val":[["c","d"]]}]]}]]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_parameter_definition_value_typed() {
        let json = serialize_with(
            b"foo = { [[add] $add$]}",
            JsonOptions {
                duplicate_keys: DuplicateKeyMode::KeyValuePairs,
                ..JsonOptions::default()
            },
        );
        let expected = r#"{"type":"obj","val":[["foo",{"type":"obj","val":[["[add]","$add$"]]}]]}"#;
        assert_eq!(&json, expected);
    }

    #[test]
    fn test_subobject() {
        let tape = TextTape::from_slice(b"bar={num=1}").unwrap();
        let reader = tape.windows1252_reader();
        let mut fields = reader.fields();
        let (_key, _op, value) = fields.next().unwrap();
        let actual = value.json().to_string();
        let expected = r#"{"num":1}"#;
        assert_eq!(&actual, expected);
    }

    #[test]
    fn test_array_direct() {
        let tape = TextTape::from_slice(b"nums={1 2 3 4}").unwrap();
        let reader = tape.windows1252_reader();
        let mut fields = reader.fields();
        let (_key, _op, value) = fields.next().unwrap();
        let array = value.read_array().unwrap();
        let actual = array.json().to_string();
        let expected = r#"[1,2,3,4]"#;
        assert_eq!(&actual, expected);
    }

    #[test]
    fn test_value_direct() {
        let tape = TextTape::from_slice(b"core=1").unwrap();
        let reader = tape.windows1252_reader();
        let mut fields = reader.fields();
        let (_key, _op, value) = fields.next().unwrap();
        let actual = value.json().to_string();
        let expected = r#"1"#;
        assert_eq!(&actual, expected);
    }

    #[test]
    fn test_builder_serialization() {
        #[derive(Serialize)]
        struct MyStruct<'a, 'b> {
            bar: JsonValueBuilder<'a, 'b, Windows1252Encoding>,
            qux: JsonValueBuilder<'a, 'b, Windows1252Encoding>,
        }

        let tape = TextTape::from_slice(b"bar={num=1} qux={num=2}").unwrap();
        let reader = tape.windows1252_reader();
        let mut fields = reader.fields();
        let (_key, _op, value) = fields.next().unwrap();
        let bar = value.json();

        let (_key, _op, value) = fields.next().unwrap();
        let qux = value.json();

        let actual = serde_json::to_string(&MyStruct { bar, qux }).unwrap();
        let expected = r#"{"bar":{"num":1},"qux":{"num":2}}"#;
        assert_eq!(&actual, expected);
    }
}
