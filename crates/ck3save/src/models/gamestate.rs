use super::Metadata;
use crate::flavor::reencode_float;
use serde::{de::Visitor, Deserialize, Deserializer};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Deserialize)]
pub struct Gamestate {
    pub meta_data: Metadata,
    pub living: HashMap<u64, LivingCharacter>,
}

#[derive(Debug, Deserialize)]
pub struct LivingCharacter {
    pub alive_data: Option<AliveData>,
}

#[derive(Debug, Deserialize)]
pub struct AliveData {
    pub gold: Option<GoldData>,
    pub health: Option<f32>,
    pub income: Option<f32>,
}

#[derive(Debug, PartialEq)]
pub enum GoldData {
    /// The older style of representing gold in CK3 saves
    F64(f64),

    /// The newer (as of patch 1.16) way of representing gold in CK3 alive data
    Object(GoldObject),
}

#[derive(Debug, Deserialize, PartialEq)]
pub struct GoldObject {
    pub value: f64,
}

impl GoldData {
    pub fn value(&self) -> f64 {
        match self {
            GoldData::F64(value) => *value,
            GoldData::Object(obj) => obj.value,
        }
    }
}

struct GoldDataVisitor;

impl<'de> Visitor<'de> for GoldDataVisitor {
    type Value = GoldData;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a float or an object with a value field")
    }

    fn visit_f64<E>(self, value: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(GoldData::F64(reencode_float(value)))
    }

    fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        let mut obj = GoldObject::deserialize(serde::de::value::MapAccessDeserializer::new(map))?;
        obj.value = reencode_float(obj.value);
        Ok(GoldData::Object(obj))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let float_value: f64 = value.parse().map_err(serde::de::Error::custom)?;
        Ok(GoldData::F64(reencode_float(float_value)))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(GoldData::F64(reencode_float(value as f64)))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(GoldData::F64(reencode_float(value as f64)))
    }
}

impl<'de> Deserialize<'de> for GoldData {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(GoldDataVisitor)
    }
}
