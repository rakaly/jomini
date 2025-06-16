use crate::{text::Operator, Scalar};
use std::borrow::Cow;

#[cfg(feature = "derive")]
use serde::de::{self, Deserializer, IntoDeserializer, MapAccess, SeqAccess, Visitor};
#[cfg(feature = "derive")]
use serde::{ser::SerializeMap, ser::SerializeSeq, Serialize, Serializer};
#[cfg(feature = "derive")]
use std::fmt;

/// An owned, lossless representation of a jomini value that can represent any
/// value that is parsed from the jomini format.
///
/// This is similar to `serde_json::Value` but specifically designed for the
/// jomini format, preserving operators, headers, quotes, and mixed containers.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "derive", derive(serde::Deserialize))]
#[cfg_attr(
    feature = "derive",
    serde(rename(deserialize = "_internal_jomini_value"))
)]
pub enum Value {
    /// A boolean value (true/false, yes/no)
    Bool(bool),

    /// A signed integer value
    I64(i64),

    /// An unsigned integer value
    U64(u64),

    /// A floating point value
    F64(f64),

    /// A quoted scalar value (e.g., "hello", "@var")
    QuotedScalar(ScalarValue),

    /// An unquoted scalar value (e.g., hello, @var, 42)
    UnquotedScalar(ScalarValue),

    /// A scalar value with an operator (e.g., `foo > 42`)
    OperatorScalar {
        /// The operator used
        operator: Operator,
        /// The scalar value
        value: ScalarValue,
    },

    /// An array of values (e.g., `{ 1 2 3 }`)
    Array(Vec<Value>),

    /// An object/map with string keys (e.g., `{ a=1 b=2 }`)
    #[cfg_attr(
        feature = "derive",
        serde(deserialize_with = "deserialize_value_object")
    )]
    Object(Vec<(String, Value)>),

    /// A mixed container that can contain both array elements and key-value pairs
    /// This represents containers like `{ a=1 10 b=20 50 }`
    Mixed(Vec<ContainerValue>),

    /// A value with a header (e.g., `color = hsv { 0.3 0.4 0.5 }`)
    Header {
        /// The header name
        header: String,
        /// The nested value
        value: Box<Value>,
    },
}

/// Represents an element in a mixed container
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "derive", derive(serde::Deserialize))]
pub enum ContainerValue {
    /// A standalone value (array element)
    Value(Value),

    /// A key-value pair with an operator
    KeyValue {
        /// The key name
        key: String,
        /// The operator used
        operator: Operator,
        /// The associated value
        value: Value,
    },
}

impl Value {
    /// Returns `true` if the value is a scalar.
    pub fn is_scalar(&self) -> bool {
        matches!(
            self,
            Value::Bool(_)
                | Value::I64(_)
                | Value::U64(_)
                | Value::F64(_)
                | Value::QuotedScalar(_)
                | Value::UnquotedScalar(_)
                | Value::OperatorScalar { .. }
        )
    }

    /// Returns `true` if the value is an array.
    pub fn is_array(&self) -> bool {
        matches!(self, Value::Array(_))
    }

    /// Returns `true` if the value is an object.
    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    /// Returns `true` if the value is a mixed container.
    pub fn is_mixed(&self) -> bool {
        matches!(self, Value::Mixed(_))
    }

    /// Returns `true` if the value has a header.
    pub fn has_header(&self) -> bool {
        matches!(self, Value::Header { .. })
    }

    /// Returns `true` if the value has an operator.
    pub fn has_operator(&self) -> bool {
        matches!(self, Value::OperatorScalar { .. })
    }

    /// Returns the boolean value if this is a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Returns the i64 value if this is an i64.
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::I64(i) => Some(*i),
            _ => None,
        }
    }

    /// Returns the u64 value if this is a u64.
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Value::U64(u) => Some(*u),
            _ => None,
        }
    }

    /// Returns the f64 value if this is an f64.
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::F64(f) => Some(*f),
            _ => None,
        }
    }

    /// Returns the operator if the value has one.
    pub fn operator(&self) -> Option<Operator> {
        match self {
            Value::OperatorScalar { operator, .. } => Some(*operator),
            _ => None,
        }
    }

    /// Returns the header if the value has one.
    pub fn header(&self) -> Option<&str> {
        match self {
            Value::Header { header, .. } => Some(header),
            _ => None,
        }
    }

    /// Returns `true` if the value is a quoted scalar.
    pub fn is_quoted(&self) -> bool {
        matches!(self, Value::QuotedScalar(_))
    }

    /// Returns `true` if the value is an unquoted scalar.
    pub fn is_unquoted(&self) -> bool {
        matches!(self, Value::UnquotedScalar(_))
    }

    /// Returns the scalar value if this is a quoted or unquoted scalar.
    ///
    /// Note: For numeric and boolean values, use the specific accessor methods
    /// like `as_bool()`, `as_i64()`, `as_u64()`, `as_f64()` instead.
    pub fn as_scalar(&self) -> Option<Scalar<'_>> {
        match self {
            Value::QuotedScalar(scalar_value) | Value::UnquotedScalar(scalar_value) => {
                Some(scalar_value.as_scalar())
            }
            Value::OperatorScalar { value, .. } => Some(value.as_scalar()),
            _ => None,
        }
    }

    /// Returns the ScalarValue if this is a quoted or unquoted scalar.
    pub fn as_scalar_value(&self) -> Option<&ScalarValue> {
        match self {
            Value::QuotedScalar(scalar_value) | Value::UnquotedScalar(scalar_value) => {
                Some(scalar_value)
            }
            Value::OperatorScalar { value, .. } => Some(value),
            _ => None,
        }
    }

    /// Returns the quoted scalar value if this is a quoted scalar.
    pub fn as_quoted_scalar(&self) -> Option<Scalar<'_>> {
        match self {
            Value::QuotedScalar(scalar_value) => Some(scalar_value.as_scalar()),
            _ => None,
        }
    }

    /// Returns the quoted ScalarValue if this is a quoted scalar.
    pub fn as_quoted_scalar_value(&self) -> Option<&ScalarValue> {
        match self {
            Value::QuotedScalar(scalar_value) => Some(scalar_value),
            _ => None,
        }
    }

    /// Returns the unquoted scalar value if this is an unquoted scalar.
    pub fn as_unquoted_scalar(&self) -> Option<Scalar<'_>> {
        match self {
            Value::UnquotedScalar(scalar_value) => Some(scalar_value.as_scalar()),
            _ => None,
        }
    }

    /// Returns the unquoted ScalarValue if this is an unquoted scalar.
    pub fn as_unquoted_scalar_value(&self) -> Option<&ScalarValue> {
        match self {
            Value::UnquotedScalar(scalar_value) => Some(scalar_value),
            _ => None,
        }
    }

    /// Returns the array if this is an array value.
    pub fn as_array(&self) -> Option<&[Value]> {
        match self {
            Value::Array(arr) => Some(arr),
            Value::Object(obj) if obj.is_empty() => Some(&[]),
            _ => None,
        }
    }

    /// Returns the object if this is an object value.
    pub fn as_object(&self) -> Option<&[(String, Value)]> {
        match self {
            Value::Object(obj) => Some(obj),
            Value::Array(arr) if arr.is_empty() => Some(&[]),
            _ => None,
        }
    }

    /// Returns the mixed container if this is a mixed value.
    pub fn as_mixed(&self) -> Option<&[ContainerValue]> {
        match self {
            Value::Mixed(mixed) => Some(mixed),
            _ => None,
        }
    }

    /// Gets a value from an object by key.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.as_object()
            .and_then(|obj| obj.iter().find(|(k, _)| k == key).map(|(_, v)| v))
    }

    /// Returns an iterator over the key-value pairs if this is an object.
    pub fn iter(&self) -> Option<impl Iterator<Item = (&String, &Value)>> {
        self.as_object().map(|obj| obj.iter().map(|(k, v)| (k, v)))
    }

    /// Gets a value from an array by index.
    pub fn get_index(&self, index: usize) -> Option<&Value> {
        self.as_array().and_then(|arr| arr.get(index))
    }
}

#[cfg(feature = "derive")]
fn deserialize_value_object<'de, D>(deserializer: D) -> Result<Vec<(String, Value)>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct ValueObjectVisitor;

    impl<'de> serde::de::Visitor<'de> for ValueObjectVisitor {
        type Value = Vec<(String, Value)>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a map or sequence of key-value pairs")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::MapAccess<'de>,
        {
            let size = map.size_hint().unwrap_or(0);
            let mut pairs = Vec::with_capacity(size);
            while let Some((key, value)) = map.next_entry::<String, Value>()? {
                pairs.push((key, value));
            }
            Ok(pairs)
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let size = seq.size_hint().unwrap_or(0);
            let mut pairs = Vec::with_capacity(size);
            while let Some((key, value)) = seq.next_element::<(String, Value)>()? {
                pairs.push((key, value));
            }
            Ok(pairs)
        }
    }

    deserializer.deserialize_any(ValueObjectVisitor)
}

#[cfg(feature = "derive")]
impl IntoDeserializer<'_, ValueDeserializerError> for ContainerValue {
    type Deserializer = ContainerValueDirectDeserializer;

    fn into_deserializer(self) -> Self::Deserializer {
        ContainerValueDirectDeserializer {
            container_value: self,
        }
    }
}

#[cfg(feature = "derive")]
impl<'a> IntoDeserializer<'a, ValueDeserializerError> for &'a ContainerValue {
    type Deserializer = ContainerValueDirectDeserializer;

    fn into_deserializer(self) -> Self::Deserializer {
        ContainerValueDirectDeserializer {
            container_value: self.clone(),
        }
    }
}

#[cfg(feature = "derive")]
pub struct ContainerValueDirectDeserializer {
    container_value: ContainerValue,
}

#[cfg(feature = "derive")]
impl<'de> de::Deserializer<'de> for ContainerValueDirectDeserializer {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_enum("ContainerValue", &["Value", "KeyValue"], visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.container_value {
            ContainerValue::Value(value) => visitor.visit_enum(ContainerValueDirectEnumAccess {
                variant_name: "Value",
                value: Some(value),
                key: None,
                operator: None,
            }),
            ContainerValue::KeyValue {
                key,
                operator,
                value,
            } => visitor.visit_enum(ContainerValueDirectEnumAccess {
                variant_name: "KeyValue",
                value: Some(value),
                key: Some(key),
                operator: Some(operator),
            }),
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct identifier ignored_any seq
    }
}

#[cfg(feature = "derive")]
struct ContainerValueDirectEnumAccess {
    variant_name: &'static str,
    value: Option<Value>,
    key: Option<String>,
    operator: Option<crate::text::Operator>,
}

#[cfg(feature = "derive")]
impl<'de> de::EnumAccess<'de> for ContainerValueDirectEnumAccess {
    type Error = ValueDeserializerError;
    type Variant = ContainerValueDirectVariantAccess;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        let variant = seed.deserialize(self.variant_name.into_deserializer())?;
        let access = ContainerValueDirectVariantAccess {
            variant_name: self.variant_name,
            value: self.value,
            key: self.key,
            operator: self.operator,
        };
        Ok((variant, access))
    }
}

#[cfg(feature = "derive")]
struct ContainerValueDirectVariantAccess {
    variant_name: &'static str,
    value: Option<Value>,
    key: Option<String>,
    operator: Option<crate::text::Operator>,
}

#[cfg(feature = "derive")]
impl<'de> de::VariantAccess<'de> for ContainerValueDirectVariantAccess {
    type Error = ValueDeserializerError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.variant_name {
            "Value" => {
                if let Some(value) = self.value {
                    seed.deserialize(value.into_deserializer())
                } else {
                    Err(ValueDeserializerError::Custom(
                        "missing value for Value variant".to_string(),
                    ))
                }
            }
            "KeyValue" => {
                // For KeyValue, create a struct with key, operator, value
                if let (Some(key), Some(operator), Some(value)) =
                    (self.key, self.operator, self.value)
                {
                    seed.deserialize(
                        KeyValueStruct {
                            key,
                            operator,
                            value,
                        }
                        .into_deserializer(),
                    )
                } else {
                    Err(ValueDeserializerError::Custom(
                        "missing fields for KeyValue variant".to_string(),
                    ))
                }
            }
            _ => Err(ValueDeserializerError::Custom(
                "unknown variant".to_string(),
            )),
        }
    }

    fn tuple_variant<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        Err(ValueDeserializerError::Custom(
            "tuple variants not supported".to_string(),
        ))
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.variant_name {
            "KeyValue" => {
                if let (Some(key), Some(operator), Some(value)) =
                    (self.key, self.operator, self.value)
                {
                    visitor.visit_map(KeyValueStructDirectMapAccess {
                        key: Some(key),
                        operator: Some(operator),
                        value: Some(value),
                        state: 0,
                    })
                } else {
                    Err(ValueDeserializerError::Custom(
                        "missing fields for KeyValue variant".to_string(),
                    ))
                }
            }
            _ => Err(ValueDeserializerError::Custom(
                "unsupported struct variant".to_string(),
            )),
        }
    }
}

#[cfg(feature = "derive")]
#[derive(Clone)]
struct KeyValueStruct {
    key: String,
    operator: crate::text::Operator,
    value: Value,
}

#[cfg(feature = "derive")]
impl IntoDeserializer<'_, ValueDeserializerError> for KeyValueStruct {
    type Deserializer = KeyValueStructDirectDeserializer;

    fn into_deserializer(self) -> Self::Deserializer {
        KeyValueStructDirectDeserializer {
            key: self.key,
            operator: self.operator,
            value: self.value,
        }
    }
}

#[cfg(feature = "derive")]
struct KeyValueStructDirectDeserializer {
    key: String,
    operator: crate::text::Operator,
    value: Value,
}

#[cfg(feature = "derive")]
impl<'de> de::Deserializer<'de> for KeyValueStructDirectDeserializer {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_struct("KeyValue", &["key", "operator", "value"], visitor)
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_map(KeyValueStructDirectMapAccess {
            key: Some(self.key),
            operator: Some(self.operator),
            value: Some(self.value),
            state: 0,
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map enum identifier ignored_any seq
    }
}

#[cfg(feature = "derive")]
struct KeyValueStructDirectMapAccess {
    key: Option<String>,
    operator: Option<crate::text::Operator>,
    value: Option<Value>,
    state: u8,
}

#[cfg(feature = "derive")]
impl<'de> de::MapAccess<'de> for KeyValueStructDirectMapAccess {
    type Error = ValueDeserializerError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        match self.state {
            0 => {
                self.state = 1;
                seed.deserialize("key".into_deserializer()).map(Some)
            }
            1 => {
                self.state = 2;
                seed.deserialize("operator".into_deserializer()).map(Some)
            }
            2 => {
                self.state = 3;
                seed.deserialize("value".into_deserializer()).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        match self.state {
            1 => {
                if let Some(key) = self.key.take() {
                    seed.deserialize(key.into_deserializer())
                } else {
                    Err(ValueDeserializerError::Custom(
                        "key already consumed".to_string(),
                    ))
                }
            }
            2 => {
                if let Some(operator) = self.operator.take() {
                    // For operators, we use the existing Operator deserializer approach
                    // which deserializes to the string representation
                    seed.deserialize(operator.symbol().into_deserializer())
                } else {
                    Err(ValueDeserializerError::Custom(
                        "operator already consumed".to_string(),
                    ))
                }
            }
            3 => {
                if let Some(value) = self.value.take() {
                    seed.deserialize(value.into_deserializer())
                } else {
                    Err(ValueDeserializerError::Custom(
                        "value already consumed".to_string(),
                    ))
                }
            }
            _ => Err(ValueDeserializerError::Custom("invalid state".to_string())),
        }
    }
}

impl ContainerValue {
    /// Returns `true` if this is a standalone value.
    pub fn is_value(&self) -> bool {
        matches!(self, ContainerValue::Value(_))
    }

    /// Returns `true` if this is a key-value pair.
    pub fn is_key_value(&self) -> bool {
        matches!(self, ContainerValue::KeyValue { .. })
    }

    /// Returns the value portion of this container value.
    pub fn value(&self) -> &Value {
        match self {
            ContainerValue::Value(value) => value,
            ContainerValue::KeyValue { value, .. } => value,
        }
    }

    /// Returns the key if this is a key-value pair.
    pub fn key(&self) -> Option<&str> {
        match self {
            ContainerValue::KeyValue { key, .. } => Some(key),
            _ => None,
        }
    }

    /// Returns the operator if this is a key-value pair.
    pub fn operator(&self) -> Option<Operator> {
        match self {
            ContainerValue::KeyValue { operator, .. } => Some(*operator),
            _ => None,
        }
    }
}

#[cfg(feature = "derive")]
/// A deserializer implementation for owned jomini Values
#[derive(Debug)]
pub struct OwnedValueDeserializer {
    value: Value,
}

#[cfg(feature = "derive")]
impl IntoDeserializer<'_, ValueDeserializerError> for Value {
    type Deserializer = OwnedValueDeserializer;

    fn into_deserializer(self) -> Self::Deserializer {
        OwnedValueDeserializer { value: self }
    }
}

#[cfg(feature = "derive")]
impl<'de> IntoDeserializer<'de, ValueDeserializerError> for &'de Value {
    type Deserializer = ValueDeserializer<'de>;

    fn into_deserializer(self) -> Self::Deserializer {
        ValueDeserializer { value: self }
    }
}

#[cfg(feature = "derive")]
/// A deserializer implementation for jomini Values
#[derive(Debug)]
pub struct ValueDeserializer<'a> {
    value: &'a Value,
}

#[cfg(feature = "derive")]
/// Errors that can occur during Value deserialization
#[derive(Debug, Clone)]
pub enum ValueDeserializerError {
    /// A custom error message
    Custom(String),
    /// An invalid type was encountered
    InvalidType(String),
    /// An unsupported type was encountered
    UnsupportedType(String),
}

#[cfg(feature = "derive")]
impl fmt::Display for ValueDeserializerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueDeserializerError::Custom(msg) => write!(f, "{}", msg),
            ValueDeserializerError::InvalidType(msg) => write!(f, "invalid type: {}", msg),
            ValueDeserializerError::UnsupportedType(msg) => write!(f, "unsupported type: {}", msg),
        }
    }
}

#[cfg(feature = "derive")]
impl std::error::Error for ValueDeserializerError {}

#[cfg(feature = "derive")]
impl de::Error for ValueDeserializerError {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        ValueDeserializerError::Custom(msg.to_string())
    }
}

#[cfg(feature = "derive")]
impl<'de> Deserializer<'de> for OwnedValueDeserializer {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ValueDeserializer { value: &self.value }.deserialize_any(visitor)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        ValueDeserializer { value: &self.value }.deserialize_seq(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

#[cfg(feature = "derive")]
impl<'de> Deserializer<'de> for ValueDeserializer<'_> {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Bool(b) => visitor.visit_bool(*b),
            Value::I64(i) => visitor.visit_i64(*i),
            Value::U64(u) => visitor.visit_u64(*u),
            Value::F64(f) => visitor.visit_f64(*f),
            Value::QuotedScalar(scalar_value) | Value::UnquotedScalar(scalar_value) => {
                // Try to parse as different scalar types
                if let Ok(b) = scalar_value.to_bool() {
                    visitor.visit_bool(b)
                } else if let Ok(i) = scalar_value.to_i64() {
                    visitor.visit_i64(i)
                } else if let Ok(u) = scalar_value.to_u64() {
                    visitor.visit_u64(u)
                } else if let Ok(f) = scalar_value.to_f64() {
                    visitor.visit_f64(f)
                } else {
                    // Fall back to bytes without assuming encoding
                    visitor.visit_bytes(scalar_value.as_bytes())
                }
            }
            Value::OperatorScalar { value: _scalar, .. } => {
                // For operator scalars, we need to use the special Property handling
                self.deserialize_map(visitor)
            }
            Value::Array(arr) => visitor.visit_seq(ValueSeqAccess {
                iter: arr.iter(),
                remaining: arr.len(),
            }),
            Value::Object(obj) => visitor.visit_map(ValueMapAccess {
                iter: obj.iter(),
                value: None,
                remaining: obj.len(),
            }),
            Value::Mixed(mixed) => {
                // For Mixed values, we need to provide enum access for Value deserialization
                visitor.visit_enum(MixedEnumAccess {
                    mixed: mixed.clone(),
                })
            }
            Value::Header { value, .. } => ValueDeserializer {
                value: value.as_ref(),
            }
            .deserialize_any(visitor),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Value::Object(obj) => visitor.visit_seq(ObjectAsSeqAccess {
                iter: obj.iter(),
                remaining: obj.len(),
            }),
            _ => self.deserialize_any(visitor),
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        match self.value {
            Value::Mixed(mixed) => {
                // For Mixed values being deserialized into enums (like Value),
                // we need to provide proper enum access
                visitor.visit_enum(MixedEnumAccess {
                    mixed: mixed.clone(),
                })
            }
            _ => {
                // For other values, delegate to deserialize_any
                self.deserialize_any(visitor)
            }
        }
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct identifier ignored_any
    }
}

#[cfg(feature = "derive")]
struct MixedEnumAccess {
    mixed: Vec<ContainerValue>,
}

#[cfg(feature = "derive")]
impl<'de> de::EnumAccess<'de> for MixedEnumAccess {
    type Error = ValueDeserializerError;
    type Variant = MixedVariantAccess;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        // The Value enum expects the variant name "Mixed"
        let variant = seed.deserialize("Mixed".into_deserializer())?;
        let access = MixedVariantAccess { mixed: self.mixed };
        Ok((variant, access))
    }
}

#[cfg(feature = "derive")]
struct MixedVariantAccess {
    mixed: Vec<ContainerValue>,
}

#[cfg(feature = "derive")]
impl<'de> de::VariantAccess<'de> for MixedVariantAccess {
    type Error = ValueDeserializerError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        // For Mixed newtype variant, deserialize the Vec<ContainerValue>
        seed.deserialize(MixedDataDeserializer { mixed: self.mixed })
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_seq(MixedSeqAccess {
            iter: self.mixed.iter(),
            remaining: self.mixed.len(),
        })
    }

    fn struct_variant<V>(
        self,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Not used for Value enum
        visitor.visit_seq(MixedSeqAccess {
            iter: self.mixed.iter(),
            remaining: self.mixed.len(),
        })
    }
}

#[cfg(feature = "derive")]
struct MixedDataDeserializer {
    mixed: Vec<ContainerValue>,
}

#[cfg(feature = "derive")]
impl<'de> de::Deserializer<'de> for MixedDataDeserializer {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        // Deserialize the Vec<ContainerValue> as a sequence
        visitor.visit_seq(MixedSeqAccess {
            iter: self.mixed.iter(),
            remaining: self.mixed.len(),
        })
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct tuple
        tuple_struct map struct enum identifier ignored_any
    }
}

#[cfg(feature = "derive")]
struct ObjectAsSeqAccess<'a> {
    iter: std::slice::Iter<'a, (String, Value)>,
    remaining: usize,
}

#[cfg(feature = "derive")]
impl<'de> SeqAccess<'de> for ObjectAsSeqAccess<'_> {
    type Error = ValueDeserializerError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if let Some((key, value)) = self.iter.next() {
            self.remaining -= 1;
            // Create a tuple deserializer for (key, value)
            let tuple_deserializer = TupleDeserializer {
                key: key.as_str(),
                value,
                index: 0,
            };
            seed.deserialize(tuple_deserializer).map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

#[cfg(feature = "derive")]
struct TupleDeserializer<'a> {
    key: &'a str,
    value: &'a Value,
    index: usize,
}

#[cfg(feature = "derive")]
impl<'de> Deserializer<'de> for TupleDeserializer<'_> {
    type Error = ValueDeserializerError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.deserialize_tuple(2, visitor)
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_seq(TupleSeqAccess {
            key: self.key,
            value: self.value,
            index: self.index,
        })
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
        bytes byte_buf option unit unit_struct newtype_struct seq
        tuple_struct map struct enum identifier ignored_any
    }
}

#[cfg(feature = "derive")]
struct TupleSeqAccess<'a> {
    key: &'a str,
    value: &'a Value,
    index: usize,
}

#[cfg(feature = "derive")]
impl<'de> SeqAccess<'de> for TupleSeqAccess<'_> {
    type Error = ValueDeserializerError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        match self.index {
            0 => {
                self.index += 1;
                seed.deserialize(self.key.into_deserializer()).map(Some)
            }
            1 => {
                self.index += 1;
                let deserializer = ValueDeserializer { value: self.value };
                seed.deserialize(deserializer).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(2 - self.index)
    }
}

#[cfg(feature = "derive")]
struct ValueSeqAccess<'a> {
    iter: std::slice::Iter<'a, Value>,
    remaining: usize,
}

#[cfg(feature = "derive")]
impl<'de> SeqAccess<'de> for ValueSeqAccess<'_> {
    type Error = ValueDeserializerError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if let Some(value) = self.iter.next() {
            self.remaining -= 1;
            let deserializer = ValueDeserializer { value };
            seed.deserialize(deserializer).map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

#[cfg(feature = "derive")]
struct ValueMapAccess<'a> {
    iter: std::slice::Iter<'a, (String, Value)>,
    value: Option<&'a Value>,
    remaining: usize,
}

#[cfg(feature = "derive")]
impl<'de> MapAccess<'de> for ValueMapAccess<'_> {
    type Error = ValueDeserializerError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: de::DeserializeSeed<'de>,
    {
        if let Some((key, value)) = self.iter.next() {
            self.value = Some(value);
            self.remaining -= 1;
            seed.deserialize(key.as_str().into_deserializer()).map(Some)
        } else {
            Ok(None)
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: de::DeserializeSeed<'de>,
    {
        if let Some(value) = self.value.take() {
            let deserializer = ValueDeserializer { value };
            seed.deserialize(deserializer)
        } else {
            Err(ValueDeserializerError::Custom(
                "no value available".to_string(),
            ))
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

#[cfg(feature = "derive")]
struct MixedSeqAccess<'a> {
    iter: std::slice::Iter<'a, ContainerValue>,
    remaining: usize,
}

#[cfg(feature = "derive")]
impl<'de> SeqAccess<'de> for MixedSeqAccess<'_> {
    type Error = ValueDeserializerError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: de::DeserializeSeed<'de>,
    {
        if let Some(container_value) = self.iter.next() {
            self.remaining -= 1;
            // Use the ContainerValue deserializer directly
            seed.deserialize(container_value.into_deserializer())
                .map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.remaining)
    }
}

#[cfg(feature = "derive")]
impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Bool(b) => serializer.serialize_bool(*b),
            Value::I64(i) => serializer.serialize_i64(*i),
            Value::U64(u) => serializer.serialize_u64(*u),
            Value::F64(f) => serializer.serialize_f64(*f),
            Value::QuotedScalar(scalar_value) | Value::UnquotedScalar(scalar_value) => {
                // Try to parse as different scalar types first
                if let Ok(b) = scalar_value.to_bool() {
                    serializer.serialize_bool(b)
                } else if let Ok(i) = scalar_value.to_i64() {
                    serializer.serialize_i64(i)
                } else if let Ok(u) = scalar_value.to_u64() {
                    serializer.serialize_u64(u)
                } else if let Ok(f) = scalar_value.to_f64() {
                    serializer.serialize_f64(f)
                } else {
                    // Use the efficient string representation
                    let string_repr = scalar_value.to_string_lossy();
                    serializer.serialize_str(&string_repr)
                }
            }
            Value::OperatorScalar { operator, value } => {
                // Serialize operator scalars as objects like {">=\": value}
                let mut map = serializer.serialize_map(Some(1))?;
                let string_value = value.to_string_lossy();
                map.serialize_entry(operator.name(), &*string_value)?;
                map.end()
            }
            Value::Array(arr) => {
                let mut seq = serializer.serialize_seq(Some(arr.len()))?;
                for item in arr {
                    seq.serialize_element(item)?;
                }
                seq.end()
            }
            Value::Object(obj) => {
                let mut map = serializer.serialize_map(Some(2))?;
                map.serialize_entry("type", "obj")?;
                map.serialize_entry("val", &ObjectValuePairs { obj })?;
                map.end()
            }
            Value::Mixed(mixed) => {
                let mut seq = serializer.serialize_seq(Some(mixed.len()))?;
                for item in mixed {
                    seq.serialize_element(item)?;
                }
                seq.end()
            }
            Value::Header { header, value } => {
                // Serialize headers as {"header_name": value}
                let mut map = serializer.serialize_map(Some(1))?;
                map.serialize_entry(header, value.as_ref())?;
                map.end()
            }
        }
    }
}

#[cfg(feature = "derive")]
struct ObjectValuePairs<'a> {
    obj: &'a Vec<(String, Value)>,
}

#[cfg(feature = "derive")]
impl Serialize for ObjectValuePairs<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.obj.len()))?;
        for (key, value) in self.obj {
            let key_value_pair = (key, value);
            seq.serialize_element(&key_value_pair)?;
        }
        seq.end()
    }
}

#[cfg(feature = "derive")]
impl Serialize for ContainerValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            ContainerValue::Value(value) => value.serialize(serializer),
            ContainerValue::KeyValue {
                key,
                operator,
                value,
            } => {
                // Serialize key-value pairs as objects like {"key": value} or {"key": {">=\": value}}
                let mut map = serializer.serialize_map(Some(1))?;
                if *operator == Operator::Equal {
                    // For equality, just serialize as {"key": value}
                    map.serialize_entry(key, value)?;
                } else {
                    // For other operators, serialize as {"key": {"op": value}}
                    map.serialize_entry(
                        key,
                        &OpValue {
                            operator: *operator,
                            value,
                        },
                    )?;
                }
                map.end()
            }
        }
    }
}

#[cfg(feature = "derive")]
struct OpValue<'a> {
    operator: Operator,
    value: &'a Value,
}

#[cfg(feature = "derive")]
impl Serialize for OpValue<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(1))?;
        map.serialize_entry(self.operator.name(), self.value)?;
        map.end()
    }
}

/// An owned scalar value that preserves encoding information when possible.
///
/// This type is used internally to avoid unnecessary encoding conversions
/// when the original encoding is known (e.g., from UTF-8 strings) versus
/// when encoding must be heuristically determined (e.g., from raw bytes).
#[derive(Debug, Clone, PartialEq)]
pub struct ScalarValue {
    inner: ScalarValueInner,
}

#[derive(Debug, Clone, PartialEq)]
enum ScalarValueInner {
    /// A string that was originally UTF-8 encoded
    String(String),
    /// Raw bytes that may need encoding heuristics
    Bytes(Vec<u8>),
}

impl ScalarValue {
    /// Create a new ScalarValue from a UTF-8 string
    pub fn from_string(s: String) -> Self {
        Self {
            inner: ScalarValueInner::String(s),
        }
    }

    /// Create a new ScalarValue from bytes
    pub fn from_bytes(bytes: Vec<u8>) -> Self {
        Self {
            inner: ScalarValueInner::Bytes(bytes),
        }
    }

    /// Get the raw bytes representation
    pub fn as_bytes(&self) -> &[u8] {
        match &self.inner {
            ScalarValueInner::String(s) => s.as_bytes(),
            ScalarValueInner::Bytes(b) => b,
        }
    }

    /// Get a string representation, using UTF-8 first then falling back to Windows-1252
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        match &self.inner {
            ScalarValueInner::String(s) => Cow::Borrowed(s),
            ScalarValueInner::Bytes(bytes) => {
                // Try UTF-8 first, then fall back to Windows-1252
                if let Ok(utf8_str) = std::str::from_utf8(bytes) {
                    Cow::Borrowed(utf8_str)
                } else {
                    crate::Windows1252Encoding::decode(bytes)
                }
            }
        }
    }

    /// Get the underlying string if this was originally UTF-8
    pub fn as_str(&self) -> Option<&str> {
        match &self.inner {
            ScalarValueInner::String(s) => Some(s),
            ScalarValueInner::Bytes(_) => None,
        }
    }

    /// Get the return a scalar reference
    pub fn as_ref(&self) -> Scalar<'_> {
        self.as_scalar()
    }

    /// Convert to a Scalar for compatibility with existing APIs
    pub fn as_scalar(&self) -> Scalar<'_> {
        Scalar::new(self.as_bytes())
    }

    /// Parse as boolean using the same logic as Scalar
    pub fn to_bool(&self) -> Result<bool, crate::ScalarError> {
        self.as_scalar().to_bool()
    }

    /// Parse as i64 using the same logic as Scalar
    pub fn to_i64(&self) -> Result<i64, crate::ScalarError> {
        self.as_scalar().to_i64()
    }

    /// Parse as u64 using the same logic as Scalar
    pub fn to_u64(&self) -> Result<u64, crate::ScalarError> {
        self.as_scalar().to_u64()
    }

    /// Parse as f64 using the same logic as Scalar
    pub fn to_f64(&self) -> Result<f64, crate::ScalarError> {
        self.as_scalar().to_f64()
    }
}

#[cfg(feature = "derive")]
impl serde::Serialize for ScalarValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as the raw bytes to maintain compatibility with existing Vec<u8> serialization
        serializer.serialize_bytes(self.as_bytes())
    }
}

#[cfg(feature = "derive")]
impl<'de> serde::Deserialize<'de> for ScalarValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ScalarValueVisitor;

        impl<'de> serde::de::Visitor<'de> for ScalarValueVisitor {
            type Value = ScalarValue;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a byte sequence")
            }

            fn visit_bytes<E>(self, value: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ScalarValue::from_bytes(value.to_vec()))
            }

            fn visit_byte_buf<E>(self, value: Vec<u8>) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(ScalarValue::from_bytes(value))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                // Handle sequence of u8 values (which is how Vec<u8> gets deserialized)
                let mut bytes = Vec::new();
                while let Some(byte) = seq.next_element::<u8>()? {
                    bytes.push(byte);
                }
                Ok(ScalarValue::from_bytes(bytes))
            }
        }

        deserializer.deserialize_bytes(ScalarValueVisitor)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "derive")]
    use serde::de::IntoDeserializer;
    #[cfg(feature = "derive")]
    use serde::Deserialize;

    #[cfg(feature = "derive")]
    #[derive(Deserialize)]
    struct MyData {
        d: i32,
        foo: Value,
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_array_deserialization() {
        let data = r#"d=100
foo={ 10 20 30 50 }"#;

        let result: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();
        assert_eq!(result.d, 100);

        // The foo field should be parsed as an array
        assert!(result.foo.is_array());
        let arr = Vec::<u32>::deserialize(result.foo.into_deserializer()).unwrap();
        assert_eq!(arr, vec![10, 20, 30, 50]);
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_object_deserialization() {
        let data = r#"d=100
foo={
    a=1
    b=20
    c=30
    d=50
}"#;

        #[derive(Deserialize)]
        struct MyData {
            d: i32,
            foo: Value,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        struct FooValue {
            a: i32,
            b: i32,
            c: i32,
            d: i32,
        }

        let result: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();
        assert_eq!(result.d, 100);

        // The foo field should be parsed as an object
        assert!(result.foo.is_object());
        let foo_value: FooValue = FooValue::deserialize(result.foo.into_deserializer()).unwrap();
        assert_eq!(
            foo_value,
            FooValue {
                a: 1,
                b: 20,
                c: 30,
                d: 50
            }
        );
    }

    #[test]
    #[cfg(feature = "derive")]
    #[ignore] // TODO: Fix round-trip deserialization for Value::Mixed
    fn test_value_mixed_deserializer() {
        // Test that Value::Mixed can be deserialized properly
        use serde::de::IntoDeserializer;

        let mixed_value = Value::Mixed(vec![
            ContainerValue::KeyValue {
                key: "a".to_string(),
                operator: crate::text::Operator::Equal,
                value: Value::I64(1),
            },
            ContainerValue::Value(Value::I64(10)),
        ]);

        // Try to deserialize this Value::Mixed back into a Value
        let result: Value = Value::deserialize(mixed_value.into_deserializer()).unwrap();
        assert!(result.is_mixed());
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_mixed_container_deserialization() {
        let data = r#"d=100
foo={a=1 10 b=20 50}"#;

        #[derive(Deserialize)]
        struct MyData {
            d: i32,
            foo: Value,
        }

        let result: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();
        assert_eq!(result.d, 100);

        assert!(result.foo.is_mixed());
        let mixed = result.foo.as_mixed().unwrap();

        // Should contain ContainerValue entries for both key-value pairs and standalone values
        assert_eq!(mixed.len(), 4);

        // Check first entry: a=1
        assert!(mixed[0].is_key_value());
        assert_eq!(mixed[0].key(), Some("a"));
        assert_eq!(mixed[0].value().as_i64(), Some(1));

        // Check second entry: 10 (standalone value)
        assert!(mixed[1].is_value());
        assert_eq!(mixed[1].value().as_i64(), Some(10));

        // Check third entry: b=20
        assert!(mixed[2].is_key_value());
        assert_eq!(mixed[2].key(), Some("b"));
        assert_eq!(mixed[2].value().as_i64(), Some(20));

        // Check fourth entry: 50 (standalone value)
        assert!(mixed[3].is_value());
        assert_eq!(mixed[3].value().as_i64(), Some(50));
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_complex_nested_structures() {
        #[derive(Deserialize, Debug)]
        struct ComplexData {
            simple_field: String,
            nested_value: Value,
            array_field: Value,
        }

        let data = r#"
            simple_field = "test"
            nested_value = {
                level1 = {
                    level2 = {
                        deep_value = 42
                        deep_array = { 1 2 3 }
                    }
                    sibling = "hello"
                }
                top_level = yes
            }
            array_field = { 10 20 30 }
        "#;

        let data: ComplexData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();
        let array_field = Vec::<i32>::deserialize(data.array_field.into_deserializer()).unwrap();

        assert_eq!(data.simple_field, "test");
        assert_eq!(array_field, vec![10, 20, 30]);

        // Test nested Value access
        assert!(data.nested_value.is_object());
        let nested_obj = data.nested_value.as_object().unwrap();
        assert_eq!(nested_obj.len(), 2);
        assert_eq!(nested_obj[0].0, "level1");
        assert_eq!(nested_obj[1].0, "top_level");

        // Test deep nesting access
        let level1 = data.nested_value.get("level1").unwrap();
        assert!(level1.is_object());
        let level1_obj = level1.as_object().unwrap();
        assert!(level1_obj.iter().any(|(k, _)| k == "level2"));
        assert!(level1_obj.iter().any(|(k, _)| k == "sibling"));
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_value_with_operators() {
        #[derive(Deserialize, Debug)]
        struct DataWithOperators {
            operator_data: Value,
        }

        let data = r#"
            operator_data = {
                greater_than > 50
                less_than < 25
                equal_to = 42
                not_equal != 0
            }
        "#;

        let result: DataWithOperators = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        // The operator_data should capture the structure even if operators aren't fully implemented
        assert!(result.operator_data.is_object());
        let obj = result.operator_data.as_object().unwrap();
        assert_eq!(obj.len(), 4);

        assert_eq!(obj[0].0, "greater_than");
        assert_eq!(obj[0].1.operator().unwrap().symbol(), ">");

        assert_eq!(obj[1].0, "less_than");
        assert_eq!(obj[1].1.operator().unwrap().symbol(), "<");

        assert_eq!(obj[2].0, "equal_to");
        assert_eq!(obj[2].1.operator().unwrap().symbol(), "=");

        assert_eq!(obj[3].0, "not_equal");
        assert_eq!(obj[3].1.operator().unwrap().symbol(), "!=");
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_value_with_headers() {
        #[derive(Deserialize, Debug)]
        struct DataWithHeaders {
            simple: i32,
            color: Value,
        }

        let data = r#"simple = 42
            color = rgb { 200 100 50 }
        "#;

        let data: DataWithHeaders = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        assert_eq!(data.simple, 42);

        assert_eq!(data.color.header(), Some("rgb"));
        let vals: Vec<i32> = Vec::<i32>::deserialize(data.color.into_deserializer()).unwrap();
        assert_eq!(vals, vec![200, 100, 50]);
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_empty_case() {
        #[derive(Deserialize, Debug)]
        struct EdgeCaseData {
            empty: Value,
        }

        let data = r#"
            empty = {}
        "#;

        let data: EdgeCaseData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        assert!(data.empty.as_object().unwrap().is_empty());
        assert!(data.empty.as_array().unwrap().is_empty());
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_value_type_preservation() {
        let data = r#"
            string_val = "hello world"
            quoted_string = "42"
            unquoted_number = 42
            boolean_yes = yes
            boolean_no = no
            float_val = 4.25
            negative = -100
        "#;

        #[derive(Deserialize, Debug)]
        struct MyData {
            string_val: Value,
            quoted_string: Value,
            unquoted_number: Value,
            boolean_yes: Value,
            boolean_no: Value,
            float_val: Value,
            negative: Value,
        }

        let data: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        // Test string preservation
        let string_val = data.string_val.as_scalar().unwrap();
        let decoded = crate::Windows1252Encoding::decode(string_val.as_bytes());
        assert!(decoded.contains("hello world"));

        // Test that we can distinguish quoted vs unquoted (even if both are scalars)
        let quoted = data.quoted_string.as_quoted_scalar().unwrap();
        assert_eq!(quoted.as_bytes(), b"42");

        let num = data.unquoted_number.as_i64().unwrap();
        assert_eq!(num, 42);

        // Test boolean values
        let bool_yes = data.boolean_yes.as_bool().unwrap();
        assert!(bool_yes);

        let bool_no = data.boolean_no.as_bool().unwrap();
        assert!(!bool_no);

        // Test numeric values
        let float_val = data.float_val.as_f64().unwrap();
        assert!((float_val - 4.25).abs() < 0.0001);

        let negative = data.negative.as_i64().unwrap();
        assert_eq!(negative, -100);

        let negative = data.negative.as_i64().unwrap();
        assert_eq!(negative, -100);
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_value_round_trip() {
        // Test that we can deserialize into Value and then deserialize from Value
        let data = r#"
            number = 42
            text = "hello"
            flag = yes
        "#;

        #[derive(Deserialize, Debug)]
        struct MyData {
            number: Value,
            text: Value,
            flag: Value,
        }

        let data: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        // Now deserialize from Value back to concrete types using IntoDeserializer
        let number = i32::deserialize(data.number.clone().into_deserializer()).unwrap();
        assert_eq!(number, 42);

        let text = String::deserialize(data.text.clone().into_deserializer()).unwrap();
        assert_eq!(text, "hello");

        let flag = bool::deserialize(data.flag.clone().into_deserializer()).unwrap();
        assert!(flag);
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_parameter_deserialization_from_tokens() {
        // Test lossless parameter representation in object keys
        let data = b"generate_advisor = { [[scaled_skill] a=b ] [[!var_name] c=d ] }";

        #[derive(Deserialize, Debug)]
        struct TestData {
            generate_advisor: Value,
        }

        let result: TestData = crate::text::de::from_utf8_slice(data).unwrap();

        let advisor = result.generate_advisor.as_object().unwrap();
        assert_eq!(advisor.len(), 2);

        let (key, value) = &advisor[0];
        assert_eq!(key, "[[scaled_skill]]");

        let value = value.as_object().unwrap();
        assert_eq!(value.len(), 1);
        assert_eq!(value[0].0, "a");
        assert_eq!(value[0].1.as_scalar().unwrap().as_bytes(), b"b");

        let (key, value) = &advisor[1];
        assert_eq!(key, "[[!var_name]]");

        let value = value.as_object().unwrap();
        assert_eq!(value.len(), 1);
        assert_eq!(value[0].0, "c");
        assert_eq!(value[0].1.as_scalar().unwrap().as_bytes(), b"d");
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_scalar_bytes_fallback_utf8() {
        // Test that scalar values that don't parse as numbers/booleans fall back to bytes
        // and can handle UTF-8 encoded strings properly
        use serde::de::IntoDeserializer;

        // Create a Value with UTF-8 encoded bytes that won't parse as a number
        let utf8_string = "Hello 世界! 🌍";
        let utf8_bytes = utf8_string.as_bytes().to_vec();
        let value = Value::UnquotedScalar(ScalarValue::from_bytes(utf8_bytes.clone()));

        // Custom deserializer to test visit_bytes behavior
        struct ByteCollector;

        impl serde::de::Visitor<'_> for ByteCollector {
            type Value = Vec<u8>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("bytes")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.to_vec())
            }

            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v)
            }
        }

        // Test that the deserializer calls visit_bytes for non-numeric strings
        let deserializer = value.into_deserializer();
        let result_bytes = deserializer.deserialize_any(ByteCollector).unwrap();
        assert_eq!(result_bytes, utf8_bytes);

        // Verify we can reconstruct the UTF-8 string
        let reconstructed = String::from_utf8(result_bytes).unwrap();
        assert_eq!(reconstructed, utf8_string);
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_scalar_bytes_fallback_windows1252() {
        // Test that scalar values can handle Windows-1252 encoded strings properly
        use serde::de::IntoDeserializer;

        // Create a Value with Windows-1252 encoded bytes (using some extended characters)
        // Character 'é' (0xE9 in Windows-1252)
        // Character '®' (0xAE in Windows-1252)
        let windows1252_bytes = vec![0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0xE9, 0xAE]; // "Hello é®"
        let value = Value::UnquotedScalar(ScalarValue::from_bytes(windows1252_bytes.clone()));

        // Custom visitor to test visit_bytes behavior
        struct ByteCollector;

        impl serde::de::Visitor<'_> for ByteCollector {
            type Value = Vec<u8>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("bytes")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v.to_vec())
            }

            fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(v)
            }
        }

        // Test that the deserializer calls visit_bytes for non-numeric strings
        let deserializer = value.into_deserializer();
        let result_bytes = deserializer.deserialize_any(ByteCollector).unwrap();
        assert_eq!(result_bytes, windows1252_bytes);

        // Verify we can decode with Windows-1252 encoding
        let decoded = crate::Windows1252Encoding::decode(&result_bytes);
        assert_eq!(decoded, "Hello é®");
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_unquoted_deserializers() {
        use serde::de::IntoDeserializer;

        // Test integer
        let int_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"42".to_vec()));
        let result_int = i32::deserialize(int_value.into_deserializer()).unwrap();
        assert_eq!(result_int, 42);

        // Test float
        let float_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"4.25".to_vec()));
        let result_float = f64::deserialize(float_value.into_deserializer()).unwrap();
        assert!((result_float - 4.25).abs() < 0.0001);

        // Test boolean
        let bool_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"yes".to_vec()));
        let result_bool = bool::deserialize(bool_value.into_deserializer()).unwrap();
        assert!(result_bool);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_scalars() {
        // Test serialization of various scalar types
        use serde_json;

        // Test integer
        let int_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"42".to_vec()));
        let json = serde_json::to_string(&int_value).unwrap();
        assert_eq!(json, "42");

        // Test float
        let float_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"3.14".to_vec()));
        let json = serde_json::to_string(&float_value).unwrap();
        assert_eq!(json, "3.14");

        // Test boolean
        let bool_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"yes".to_vec()));
        let json = serde_json::to_string(&bool_value).unwrap();
        assert_eq!(json, "true");

        // Test string
        let string_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"hello world".to_vec()));
        let json = serde_json::to_string(&string_value).unwrap();
        assert_eq!(json, "\"hello world\"");
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_array() {
        use serde_json;

        let array_value = Value::Array(vec![
            Value::UnquotedScalar(ScalarValue::from_bytes(b"1".to_vec())),
            Value::UnquotedScalar(ScalarValue::from_bytes(b"2".to_vec())),
            Value::UnquotedScalar(ScalarValue::from_bytes(b"hello".to_vec())),
        ]);

        let json = serde_json::to_string(&array_value).unwrap();
        assert_eq!(json, "[1,2,\"hello\"]");
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_object_keyvaluepairs() {
        use serde_json;

        let object_value = Value::Object(vec![
            (
                "name".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"John".to_vec())),
            ),
            (
                "age".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"30".to_vec())),
            ),
            (
                "active".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"yes".to_vec())),
            ),
        ]);

        let json = serde_json::to_string(&object_value).unwrap();
        let expected = r#"{"type":"obj","val":[["name","John"],["age",30],["active",true]]}"#;
        assert_eq!(json, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_operator_scalar() {
        use serde_json;

        let op_value = Value::OperatorScalar {
            operator: Operator::GreaterThan,
            value: ScalarValue::from_bytes(b"50".to_vec()),
        };

        let json = serde_json::to_string(&op_value).unwrap();
        assert_eq!(json, r#"{"GREATER_THAN":"50"}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_header() {
        use serde_json;

        let header_value = Value::Header {
            header: "rgb".to_string(),
            value: Box::new(Value::Array(vec![
                Value::UnquotedScalar(ScalarValue::from_bytes(b"100".to_vec())),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"200".to_vec())),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"150".to_vec())),
            ])),
        };

        let json = serde_json::to_string(&header_value).unwrap();
        assert_eq!(json, r#"{"rgb":[100,200,150]}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_mixed_container() {
        use serde_json;

        let mixed_value = Value::Mixed(vec![
            ContainerValue::Value(Value::UnquotedScalar(ScalarValue::from_bytes(
                b"10".to_vec(),
            ))),
            ContainerValue::KeyValue {
                key: "a".to_string(),
                operator: Operator::Equal,
                value: Value::UnquotedScalar(ScalarValue::from_bytes(b"1".to_vec())),
            },
            ContainerValue::KeyValue {
                key: "b".to_string(),
                operator: Operator::GreaterThan,
                value: Value::UnquotedScalar(ScalarValue::from_bytes(b"abc".to_vec())),
            },
        ]);

        let json = serde_json::to_string(&mixed_value).unwrap();
        let expected = r#"[10,{"a":1},{"b":{"GREATER_THAN":"abc"}}]"#;
        assert_eq!(json, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_nested_complex() {
        use serde_json;

        let complex_value = Value::Object(vec![
            (
                "simple".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"text".to_vec())),
            ),
            (
                "numbers".to_string(),
                Value::Array(vec![
                    Value::UnquotedScalar(ScalarValue::from_bytes(b"1".to_vec())),
                    Value::UnquotedScalar(ScalarValue::from_bytes(b"2".to_vec())),
                    Value::UnquotedScalar(ScalarValue::from_bytes(b"3".to_vec())),
                ]),
            ),
            (
                "nested".to_string(),
                Value::Object(vec![
                    (
                        "inner".to_string(),
                        Value::UnquotedScalar(ScalarValue::from_bytes(b"value".to_vec())),
                    ),
                    (
                        "flag".to_string(),
                        Value::UnquotedScalar(ScalarValue::from_bytes(b"no".to_vec())),
                    ),
                ]),
            ),
            (
                "header_data".to_string(),
                Value::Header {
                    header: "hsv".to_string(),
                    value: Box::new(Value::Array(vec![
                        Value::UnquotedScalar(ScalarValue::from_bytes(b"0.3".to_vec())),
                        Value::UnquotedScalar(ScalarValue::from_bytes(b"0.4".to_vec())),
                        Value::UnquotedScalar(ScalarValue::from_bytes(b"0.5".to_vec())),
                    ])),
                },
            ),
        ]);

        let json = serde_json::to_string(&complex_value).unwrap();
        let expected = r#"{"type":"obj","val":[["simple","text"],["numbers",[1,2,3]],["nested",{"type":"obj","val":[["inner","value"],["flag",false]]}],["header_data",{"hsv":[0.3,0.4,0.5]}]]}"#;
        assert_eq!(json, expected);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_preserves_insertion_order() {
        use serde_json;

        // Test that object insertion order is preserved in JSON
        let object_value = Value::Object(vec![
            (
                "z_last".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"3".to_vec())),
            ),
            (
                "a_first".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"1".to_vec())),
            ),
            (
                "m_middle".to_string(),
                Value::UnquotedScalar(ScalarValue::from_bytes(b"2".to_vec())),
            ),
        ]);

        let json = serde_json::to_string(&object_value).unwrap();
        let expected = r#"{"type":"obj","val":[["z_last",3],["a_first",1],["m_middle",2]]}"#;
        assert_eq!(json, expected);

        // Verify the order is preserved by checking the structure
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        let val_array = parsed.get("val").unwrap().as_array().unwrap();
        assert_eq!(val_array[0][0], "z_last");
        assert_eq!(val_array[1][0], "a_first");
        assert_eq!(val_array[2][0], "m_middle");
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_utf8_priority() {
        // Test that UTF-8 encoding is preferred over Windows-1252
        use serde_json;

        // Create a value with UTF-8 encoded bytes that are also valid Windows-1252
        // The string "café" in UTF-8 vs Windows-1252
        let utf8_bytes = "café".as_bytes().to_vec(); // UTF-8: [99, 97, 102, 195, 169]
        let utf8_value = Value::UnquotedScalar(ScalarValue::from_bytes(utf8_bytes));

        let json = serde_json::to_string(&utf8_value).unwrap();
        // Should serialize as UTF-8 "café", not Windows-1252 interpretation
        assert_eq!(json, "\"café\"");

        // Test the same for operator scalars
        let op_value = Value::OperatorScalar {
            operator: Operator::Equal,
            value: ScalarValue::from_bytes("café".as_bytes().to_vec()),
        };

        let json = serde_json::to_string(&op_value).unwrap();
        assert_eq!(json, r#"{"EQUAL":"café"}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_windows1252_fallback() {
        // Test that Windows-1252 is used when UTF-8 fails
        use serde_json;

        // Create bytes that are valid Windows-1252 but invalid UTF-8
        // Byte 0xE9 is 'é' in Windows-1252 but invalid UTF-8 on its own
        let windows1252_bytes = vec![0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0xE9]; // "Hello é"
        let windows1252_value = Value::UnquotedScalar(ScalarValue::from_bytes(windows1252_bytes));

        let json = serde_json::to_string(&windows1252_value).unwrap();
        // Should fallback to Windows-1252 decoding and produce "Hello é"
        assert_eq!(json, "\"Hello é\"");

        // Test the same for operator scalars
        let op_value = Value::OperatorScalar {
            operator: Operator::LessThan,
            value: ScalarValue::from_bytes(vec![0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0xE9]),
        };

        let json = serde_json::to_string(&op_value).unwrap();
        assert_eq!(json, r#"{"LESS_THAN":"Hello é"}"#);
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_value_serialize_encoding_comparison() {
        // Test that we can distinguish between UTF-8 and Windows-1252 encodings
        use serde_json;

        // Test 1: Valid UTF-8 string with accented character
        let utf8_value =
            Value::UnquotedScalar(ScalarValue::from_bytes("résumé".as_bytes().to_vec()));
        let utf8_json = serde_json::to_string(&utf8_value).unwrap();
        assert_eq!(utf8_json, "\"résumé\"");

        // Test 2: Windows-1252 bytes that would be different if interpreted as UTF-8
        // Windows-1252 encoded "résumé" would be different bytes
        let windows1252_bytes = vec![0x72, 0xE9, 0x73, 0x75, 0x6D, 0xE9]; // "résumé" in Windows-1252
        let windows1252_value = Value::UnquotedScalar(ScalarValue::from_bytes(windows1252_bytes));
        let windows1252_json = serde_json::to_string(&windows1252_value).unwrap();
        // This should use Windows-1252 fallback since the bytes are invalid UTF-8
        assert_eq!(windows1252_json, "\"résumé\"");

        // Test 3: Pure ASCII (should work the same in both encodings)
        let ascii_value = Value::UnquotedScalar(ScalarValue::from_bytes(b"hello world".to_vec()));
        let ascii_json = serde_json::to_string(&ascii_value).unwrap();
        assert_eq!(ascii_json, "\"hello world\"");
    }

    #[test]
    #[cfg(feature = "json")]
    fn test_optimized_value_serialization() {
        // Test that the optimized Value variants serialize correctly
        use serde_json;

        // Test direct primitive serialization (no string conversion)
        let bool_value = Value::Bool(true);
        let json = serde_json::to_string(&bool_value).unwrap();
        assert_eq!(json, "true");

        let i64_value = Value::I64(-42);
        let json = serde_json::to_string(&i64_value).unwrap();
        assert_eq!(json, "-42");

        let u64_value = Value::U64(18446744073709551615);
        let json = serde_json::to_string(&u64_value).unwrap();
        assert_eq!(json, "18446744073709551615");

        let f64_value = Value::F64(4.25);
        let json = serde_json::to_string(&f64_value).unwrap();
        assert_eq!(json, "4.25");
    }

    #[test]
    #[cfg(feature = "derive")]
    fn test_quote_differentiation() {
        let data = r#"
            a = "@test"
            b = @test
            c = "hello world"
            d = hello_world
        "#;

        #[derive(Debug, Deserialize)]
        struct MyData {
            a: Value,
            b: Value,
            c: Value,
            d: Value,
        }

        let parsed: MyData = crate::text::de::from_utf8_slice(data.as_bytes()).unwrap();

        assert_eq!(parsed.a.as_quoted_scalar().unwrap().as_bytes(), b"@test");
        assert_eq!(parsed.a.as_unquoted_scalar(), None);
        assert_eq!(parsed.b.as_unquoted_scalar().unwrap().as_bytes(), b"@test");
        assert_eq!(parsed.b.as_quoted_scalar(), None);
        assert_eq!(
            parsed.c.as_quoted_scalar().unwrap().as_bytes(),
            b"hello world"
        );
        assert_eq!(parsed.c.as_unquoted_scalar(), None);
        assert_eq!(
            parsed.d.as_unquoted_scalar().unwrap().as_bytes(),
            b"hello_world"
        );
        assert_eq!(parsed.d.as_quoted_scalar(), None);
    }
}
