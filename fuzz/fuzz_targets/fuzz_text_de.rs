#![no_main]
//! Differential fuzzing of `jomini::text::de::TextDeserializer`.
//!
//! The first byte of the input picks a buffer size of 8..=256 bytes, the
//! second byte picks the encoding, and the rest is the document. The
//! document is deserialized from a slice, from a reader with the default
//! buffer, from a reader with the small buffer, and from a reader that
//! returns a byte per read into the small buffer. These give the same value
//! or the same error, with its offset, except that a small buffer may be too
//! small for a token.
use jomini::{
    Encoding, Error, ErrorKind, Utf8Encoding, Windows1252Encoding,
    text::{Property, TextDeserializer},
};
use libfuzzer_sys::fuzz_target;
use serde::{
    Deserialize,
    de::{self, DeserializeOwned, IgnoredAny},
};
use std::{collections::BTreeMap, fmt, io::Read};

/// Every value, read with `deserialize_any`: nothing is skipped.
#[derive(Debug, PartialEq)]
enum AnyValue {
    Str(String),
    Seq(Vec<AnyValue>),
    Map(Vec<(String, AnyValue)>),
}

struct AnyVisitor;

impl<'de> de::Visitor<'de> for AnyVisitor {
    type Value = AnyValue;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("any value")
    }

    fn visit_str<E: de::Error>(self, v: &str) -> Result<Self::Value, E> {
        Ok(AnyValue::Str(String::from(v)))
    }

    fn visit_seq<A: de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
        let mut values = Vec::new();
        while let Some(value) = seq.next_element()? {
            values.push(value);
        }
        Ok(AnyValue::Seq(values))
    }

    fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Self::Value, A::Error> {
        let mut entries = Vec::new();
        while let Some(entry) = map.next_entry()? {
            entries.push(entry);
        }
        Ok(AnyValue::Map(entries))
    }
}

impl<'de> Deserialize<'de> for AnyValue {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_any(AnyVisitor)
    }
}

/// A few known fields of different types. Every other field is skipped
/// with `deserialize_ignored_any`.
#[derive(Deserialize, Debug, PartialEq)]
struct Typed {
    #[serde(default)]
    a: Option<String>,
    #[serde(default)]
    b: Option<i64>,
    #[serde(default)]
    c: Option<Vec<String>>,
    #[serde(default)]
    d: Option<Inner>,
    #[serde(default)]
    e: Option<bool>,
    #[serde(default)]
    f: Option<Kind>,
    #[serde(default)]
    g: Option<Property<String>>,
    #[serde(default)]
    h: Option<BTreeMap<String, String>>,
    #[serde(default)]
    rgb: Option<(String, Vec<u8>)>,
    #[serde(default)]
    color: Option<Color>,
}

/// An enum whose variants with data are read from a header, as in
/// `color = rgb { 1 2 3 }`.
#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Color {
    Rgb(u8, u8, u8),
    Hsv(Vec<f32>),
    Point { x: u8, y: Option<u8> },
    Named(Inner),
    None,
}

#[derive(Deserialize, Debug, PartialEq)]
struct Inner {
    #[serde(default)]
    x: Option<u32>,
    #[serde(default)]
    y: Option<Vec<Inner>>,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Kind {
    Alpha,
    Beta,
    #[serde(other)]
    Other,
}

/// The keys of the root, whose values are all skipped.
#[derive(Debug, PartialEq)]
struct Keys(Vec<String>);

impl<'de> Deserialize<'de> for Keys {
    fn deserialize<D: de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct KeysVisitor;

        impl<'de> de::Visitor<'de> for KeysVisitor {
            type Value = Keys;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map")
            }

            fn visit_map<A: de::MapAccess<'de>>(self, mut map: A) -> Result<Keys, A::Error> {
                let mut keys = Vec::new();
                while let Some((key, IgnoredAny)) = map.next_entry::<String, IgnoredAny>()? {
                    keys.push(key);
                }
                Ok(Keys(keys))
            }
        }

        deserializer.deserialize_map(KeysVisitor)
    }
}

/// A reader that returns one byte per read.
struct Trickle<'a>(&'a [u8]);

impl Read for Trickle<'_> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match (self.0.split_first(), buf.first_mut()) {
            (Some((&x, rest)), Some(out)) => {
                *out = x;
                self.0 = rest;
                Ok(1)
            }
            _ => Ok(0),
        }
    }
}

/// The value's debug text, or the error's display text and whether the
/// buffer was too small. Floats are compared through their debug text, so
/// a NaN equals itself.
type Outcome = Result<String, (bool, String)>;

fn outcome<T: fmt::Debug>(result: Result<T, Error>) -> Outcome {
    result.map(|x| format!("{x:?}")).map_err(|e| {
        let small = matches!(e.kind(), ErrorKind::BufferTooSmall);
        (small, e.to_string())
    })
}

fn check<T, E>(data: &[u8], size: usize, encoding: E)
where
    T: DeserializeOwned + fmt::Debug,
    E: Encoding + Clone,
{
    let slice = outcome(TextDeserializer::from_slice(data, encoding.clone()).deserialize::<T>());
    let reader = outcome(TextDeserializer::from_reader(data, encoding.clone()).deserialize::<T>());
    let small = outcome(
        TextDeserializer::from_reader_with_buf(data, encoding.clone(), vec![0; size])
            .deserialize::<T>(),
    );
    let trickle = outcome(
        TextDeserializer::from_reader_with_buf(Trickle(data), encoding.clone(), vec![0; size])
            .deserialize::<T>(),
    );

    let name = std::any::type_name::<T>();
    assert_eq!(reader, slice, "{name}: default reader vs slice");
    if !matches!(small, Err((true, _))) {
        assert_eq!(
            small, slice,
            "{name}: reader with {size} byte buffer vs slice"
        );
    }
    if !matches!(trickle, Err((true, _))) {
        assert_eq!(
            trickle, slice,
            "{name}: trickle with {size} byte buffer vs slice"
        );
    }
}

fn check_all<E: Encoding + Clone>(data: &[u8], size: usize, encoding: E) {
    check::<AnyValue, E>(data, size, encoding.clone());
    check::<Typed, E>(data, size, encoding.clone());
    check::<Keys, E>(data, size, encoding);
}

/// The stack of the thread that deserializes, large enough for a document
/// that nests a block in each byte, as the deserializer doesn't limit the depth.
const STACK_SIZE: usize = 256 << 20;

fuzz_target!(|input: &[u8]| {
    let [size, enc, doc @ ..] = input else {
        return;
    };
    let size = 8 + usize::from(*size) % 249;

    let data = doc.to_vec();

    let utf8 = enc & 1 == 1;
    let worker = move || {
        if utf8 {
            check_all(&data, size, Utf8Encoding::new());
        } else {
            check_all(&data, size, Windows1252Encoding::new());
        }
    };

    let result = std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(worker)
        .unwrap()
        .join();
    if let Err(panic) = result {
        std::panic::resume_unwind(panic);
    }
});
