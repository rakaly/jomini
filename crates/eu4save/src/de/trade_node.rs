use crate::{
    models::{CountryTrade, TradeNode},
    CountryTag,
};
use serde::{de, Deserialize, Deserializer};
use std::fmt;

impl<'de> Deserialize<'de> for TradeNode {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct TradeNodeVisitor;

        impl<'de> de::Visitor<'de> for TradeNodeVisitor {
            type Value = TradeNode;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct TradeNode")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut countries: Vec<_> = Vec::new();
                let mut country_section = false;
                while let Some(key) = map.next_key_seed(TnfSeed { country_section })? {
                    match key {
                        Tnf::HighestPower => {
                            country_section = true;
                            map.next_value::<de::IgnoredAny>()?;
                        }

                        // We need to know once the fixed fields are done as
                        // there are fields like "max" which may be accidentally
                        // interpretted as a country tag (country tag's can be
                        // lowercase).
                        Tnf::Tag(tag) => map.next_value_seed(ExtendVec {
                            tag,
                            countries: &mut countries,
                        })?,
                        Tnf::Other => {
                            map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                Ok(TradeNode { countries })
            }
        }

        deserializer.deserialize_map(TradeNodeVisitor)
    }
}

#[derive(Debug)]
struct TnfSeed {
    country_section: bool,
}

enum Tnf {
    HighestPower,
    Other,
    Tag(CountryTag),
}

impl<'de> de::DeserializeSeed<'de> for TnfSeed {
    type Value = Tnf;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor(bool);
        impl de::Visitor<'_> for Visitor {
            type Value = Tnf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum trade node field")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "highest_power" => Ok(Tnf::HighestPower),
                    _ => {
                        if self.0 {
                            if let Ok(tag) = CountryTag::create(v.as_bytes()) {
                                return Ok(Tnf::Tag(tag));
                            }
                        }

                        Ok(Tnf::Other)
                    }
                }
            }
        }

        deserializer.deserialize_str(Visitor(self.country_section))
    }
}

enum Tntf {
    PrivateerMoney,
    Other,
}

impl<'de> de::Deserialize<'de> for Tntf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl de::Visitor<'_> for Visitor {
            type Value = Tntf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("enum trade node tag field")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "privateer_money" => Ok(Tntf::PrivateerMoney),
                    _ => Ok(Tntf::Other),
                }
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}

// https://docs.rs/serde/latest/serde/de/trait.DeserializeSeed.html
struct ExtendVec<'a> {
    tag: CountryTag,
    countries: &'a mut Vec<CountryTrade>,
}

impl<'de> de::DeserializeSeed<'de> for ExtendVec<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExtendVecVisitor<'a> {
            tag: CountryTag,
            countries: &'a mut Vec<CountryTrade>,
        }

        impl<'de> de::Visitor<'de> for ExtendVecVisitor<'_> {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "country trade node")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut privateer_money = 0.0;
                let mut should_extend = false;
                while let Some(key) = map.next_key::<Tntf>()? {
                    // Every country seems to have a max_demand field that seems
                    // useless so we only add a country to the array when it has
                    // an interesting field
                    match key {
                        Tntf::PrivateerMoney => privateer_money = map.next_value()?,
                        Tntf::Other => {
                            map.next_value::<de::IgnoredAny>()?;
                            continue;
                        }
                    };

                    should_extend = true;
                }

                if should_extend {
                    self.countries.push(CountryTrade {
                        tag: self.tag,
                        privateer_money,
                    });
                }

                Ok(())
            }
        }

        deserializer.deserialize_map(ExtendVecVisitor {
            tag: self.tag,
            countries: self.countries,
        })
    }
}
