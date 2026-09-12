use crate::{
    models::{WarEvent, WarHistory},
    Eu4Date,
};
use serde::{
    de::{self, Error},
    Deserialize, Deserializer,
};
use std::fmt;

impl<'de> Deserialize<'de> for WarHistory {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct WarHistoryVisitor;

        impl<'de> de::Visitor<'de> for WarHistoryVisitor {
            type Value = WarHistory;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct WarHistory with arbitrary fields")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                // Hmm empty object
                let abc = seq.next_element::<de::IgnoredAny>()?;
                if abc.is_some() {
                    return Err(de::Error::custom("unexpected sequence!"));
                }

                Ok(WarHistory::default())
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut name = None;
                let mut war_goal = None;
                let mut succession = None;
                let hint = map.size_hint().unwrap_or_default();
                let estimate = hint.max(8);
                let mut events = Vec::with_capacity(estimate);

                while let Some(key) = map.next_key::<Whf>()? {
                    match key {
                        Whf::Name => name = map.next_value()?,
                        Whf::WarGoal => war_goal = map.next_value()?,
                        Whf::Succession => succession = map.next_value()?,
                        Whf::Date(date) => map.next_value_seed(ExtendVec {
                            date,
                            events: &mut events,
                        })?,
                        Whf::Other => {
                            return Err(A::Error::custom("unrecognized war history field"))
                        }
                    }
                }

                Ok(WarHistory {
                    name,
                    war_goal,
                    succession,
                    events,
                })
            }
        }

        deserializer.deserialize_map(WarHistoryVisitor)
    }
}

enum Whf {
    Date(Eu4Date),
    Name,
    Other,
    Succession,
    WarGoal,
}

impl<'de> de::Deserialize<'de> for Whf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl de::Visitor<'_> for Visitor {
            type Value = Whf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryHistoryField")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "name" => Ok(Whf::Name),
                    "war_goal" => Ok(Whf::WarGoal),
                    "succession" => Ok(Whf::Succession),
                    x => Eu4Date::parse(x).map(Whf::Date).or(Ok(Whf::Other)),
                }
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}

enum Wef {
    AddAttacker,
    AddDefender,
    Battle,
    Other,
    RemoveAttacker,
    RemoveDefender,
}

impl<'de> de::Deserialize<'de> for Wef {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl de::Visitor<'_> for Visitor {
            type Value = Wef;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryHistoryField")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "add_attacker" => Ok(Wef::AddAttacker),
                    "add_defender" => Ok(Wef::AddDefender),
                    "rem_attacker" => Ok(Wef::RemoveAttacker),
                    "rem_defender" => Ok(Wef::RemoveDefender),
                    "battle" => Ok(Wef::Battle),
                    _ => Ok(Wef::Other),
                }
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}

// https://docs.rs/serde/latest/serde/de/trait.DeserializeSeed.html
struct ExtendVec<'a> {
    date: Eu4Date,
    events: &'a mut Vec<(Eu4Date, WarEvent)>,
}

impl<'de> de::DeserializeSeed<'de> for ExtendVec<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExtendVecVisitor<'a> {
            date: Eu4Date,
            events: &'a mut Vec<(Eu4Date, WarEvent)>,
        }

        impl<'de> de::Visitor<'de> for ExtendVecVisitor<'_> {
            type Value = ();

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "province events")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                // Hmm empty object
                let abc = seq.next_element::<de::IgnoredAny>()?;
                if abc.is_some() {
                    return Err(de::Error::custom("unexpected sequence!"));
                }

                Ok(())
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                while let Some(key) = map.next_key::<Wef>()? {
                    let val = match key {
                        Wef::AddAttacker => WarEvent::AddAttacker(map.next_value()?),
                        Wef::AddDefender => WarEvent::AddDefender(map.next_value()?),
                        Wef::RemoveAttacker => WarEvent::RemoveAttacker(map.next_value()?),
                        Wef::RemoveDefender => WarEvent::RemoveDefender(map.next_value()?),
                        Wef::Battle => WarEvent::Battle(map.next_value()?),
                        Wef::Other => return Err(de::Error::custom("unknown battle key")),
                    };

                    self.events.push((self.date, val));
                }

                Ok(())
            }
        }

        deserializer.deserialize_map(ExtendVecVisitor {
            date: self.date,
            events: self.events,
        })
    }
}
