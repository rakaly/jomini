use crate::models::ProvinceEventValue;
use serde::{de, Deserialize, Deserializer};
use std::fmt;

impl<'de> Deserialize<'de> for ProvinceEventValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ProvinceEventValueVisitor;

        impl<'de> de::Visitor<'de> for ProvinceEventValueVisitor {
            type Value = ProvinceEventValue;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum ProvinceEventValue")
            }

            fn visit_string<E>(self, v: String) -> Result<Self::Value, E> {
                if v == "yes" || v == "no" {
                    Ok(ProvinceEventValue::Bool(v == "yes"))
                } else {
                    Ok(ProvinceEventValue::String(v))
                }
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
                if v == "yes" || v == "no" {
                    Ok(ProvinceEventValue::Bool(v == "yes"))
                } else {
                    Ok(ProvinceEventValue::String(v.to_string()))
                }
            }

            fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E> {
                Ok(ProvinceEventValue::Bool(v))
            }

            fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E> {
                Ok(ProvinceEventValue::Int(v))
            }

            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E> {
                Ok(ProvinceEventValue::Int(v as i32))
            }

            fn visit_f32<E>(self, v: f32) -> Result<Self::Value, E> {
                Ok(ProvinceEventValue::Float(v))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E> {
                Ok(ProvinceEventValue::Float(v as f32))
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                while seq.next_element::<de::IgnoredAny>()?.is_some() {}

                Ok(ProvinceEventValue::Array)
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                while map
                    .next_entry::<de::IgnoredAny, de::IgnoredAny>()?
                    .is_some()
                {}

                Ok(ProvinceEventValue::Object)
            }
        }

        deserializer.deserialize_any(ProvinceEventValueVisitor)
    }
}
