use crate::{
    models::{CountryEvent, CountryHistory},
    Eu4Date,
};
use serde::{de, Deserialize, Deserializer};
use std::fmt;

impl<'de> Deserialize<'de> for CountryHistory {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct CountryHistoryVisitor;

        impl<'de> de::Visitor<'de> for CountryHistoryVisitor {
            type Value = CountryHistory;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryHistory with arbitrary fields")
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

                Ok(CountryHistory::default())
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut government = None;
                let mut technology_group = None;
                let mut primary_culture = None;
                let mut religion = None;
                let mut add_government_reform = Vec::new();
                let mut capital = None;
                let mut events = Vec::new();
                let hint = map.size_hint().unwrap_or_default();
                let estimate = hint.max(8);

                while let Some(key) = map.next_key::<Chf>()? {
                    match key {
                        Chf::Government => government = Some(map.next_value()?),
                        Chf::TechnologyGroup => technology_group = Some(map.next_value()?),
                        Chf::PrimaryCulture => primary_culture = Some(map.next_value()?),
                        Chf::Religion => religion = Some(map.next_value()?),
                        Chf::AddGovernmentReform => add_government_reform.push(map.next_value()?),
                        Chf::Capital => capital = Some(map.next_value()?),
                        Chf::Date(date) => map.next_value_seed(ExtendVec {
                            date,
                            estimate,
                            events: &mut events,
                        })?,
                        Chf::Other => {
                            map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                Ok(CountryHistory {
                    government,
                    technology_group,
                    primary_culture,
                    religion,
                    add_government_reform,
                    events,
                    capital,
                })
            }
        }

        deserializer.deserialize_map(CountryHistoryVisitor)
    }
}

enum Chf {
    AddGovernmentReform,
    Date(Eu4Date),
    Government,
    Other,
    PrimaryCulture,
    Religion,
    TechnologyGroup,
    Capital,
}

impl<'de> de::Deserialize<'de> for Chf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl de::Visitor<'_> for Visitor {
            type Value = Chf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryHistoryField")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "government" => Ok(Chf::Government),
                    "technology_group" => Ok(Chf::TechnologyGroup),
                    "primary_culture" => Ok(Chf::PrimaryCulture),
                    "religion" => Ok(Chf::Religion),
                    "add_government_reform" => Ok(Chf::AddGovernmentReform),
                    "capital" => Ok(Chf::Capital),
                    x => Eu4Date::parse(x).map(Chf::Date).or(Ok(Chf::Other)),
                }
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}

enum Chdf {
    Capital,
    ChangedCountryAdjectiveFrom,
    ChangedCountryMapColorFrom,
    ChangedCountryNameFrom,
    ChangedTagFrom,
    Heir,
    Leader,
    Monarch,
    MonarchConsort,
    MonarchHeir,
    Other,
    Queen,
    Religion,
    NationalFocus,
    PrimaryCulture,
    AddAcceptedCulture,
    RemoveAcceptedCulture,
    Union,
    Decision,
}

impl<'de> de::Deserialize<'de> for Chdf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct Visitor;
        impl de::Visitor<'_> for Visitor {
            type Value = Chdf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct CountryHistoryField")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match v {
                    "monarch" => Ok(Chdf::Monarch),
                    "monarch_heir" => Ok(Chdf::MonarchHeir),
                    "monarch_consort" => Ok(Chdf::MonarchConsort),
                    "heir" => Ok(Chdf::Heir),
                    "queen" => Ok(Chdf::Queen),
                    "union" => Ok(Chdf::Union),
                    "capital" => Ok(Chdf::Capital),
                    "leader" => Ok(Chdf::Leader),
                    "changed_country_name_from" => Ok(Chdf::ChangedCountryNameFrom),
                    "changed_country_adjective_from" => Ok(Chdf::ChangedCountryAdjectiveFrom),
                    "changed_country_mapcolor_from" => Ok(Chdf::ChangedCountryMapColorFrom),
                    "changed_tag_from" => Ok(Chdf::ChangedTagFrom),
                    "religion" => Ok(Chdf::Religion),
                    "national_focus" => Ok(Chdf::NationalFocus),
                    "primary_culture" => Ok(Chdf::PrimaryCulture),
                    "remove_accepted_culture" => Ok(Chdf::RemoveAcceptedCulture),
                    "add_accepted_culture" => Ok(Chdf::AddAcceptedCulture),
                    "decision" => Ok(Chdf::Decision),
                    _ => Ok(Chdf::Other),
                }
            }
        }

        deserializer.deserialize_str(Visitor)
    }
}

// https://docs.rs/serde/latest/serde/de/trait.DeserializeSeed.html
struct ExtendVec<'a> {
    date: Eu4Date,
    estimate: usize,
    events: &'a mut Vec<(Eu4Date, CountryEvent)>,
}

impl<'de> de::DeserializeSeed<'de> for ExtendVec<'_> {
    type Value = ();

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ExtendVecVisitor<'a> {
            date: Eu4Date,
            estimate: usize,
            events: &'a mut Vec<(Eu4Date, CountryEvent)>,
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
                while let Some(key) = map.next_key::<Chdf>()? {
                    let val = match key {
                        Chdf::Monarch => CountryEvent::Monarch(map.next_value()?),
                        Chdf::MonarchHeir => CountryEvent::MonarchHeir(map.next_value()?),
                        Chdf::MonarchConsort => CountryEvent::MonarchConsort(map.next_value()?),
                        Chdf::Heir => CountryEvent::Heir(map.next_value()?),
                        Chdf::Queen => CountryEvent::Queen(map.next_value()?),
                        Chdf::Union => CountryEvent::Union(map.next_value()?),
                        Chdf::Capital => CountryEvent::Capital(map.next_value()?),
                        Chdf::Leader => CountryEvent::Leader(map.next_value()?),
                        Chdf::RemoveAcceptedCulture => {
                            CountryEvent::RemoveAcceptedCulture(map.next_value()?)
                        }
                        Chdf::ChangedCountryNameFrom => {
                            CountryEvent::ChangedCountryNameFrom(map.next_value()?)
                        }
                        Chdf::ChangedCountryAdjectiveFrom => {
                            CountryEvent::ChangedCountryAdjectiveFrom(map.next_value()?)
                        }
                        Chdf::ChangedCountryMapColorFrom => {
                            CountryEvent::ChangedCountryMapColorFrom(map.next_value()?)
                        }
                        Chdf::ChangedTagFrom => CountryEvent::ChangedTagFrom(map.next_value()?),
                        Chdf::Religion => CountryEvent::Religion(map.next_value()?),
                        Chdf::NationalFocus => CountryEvent::NationalFocus(map.next_value()?),
                        Chdf::PrimaryCulture => CountryEvent::PrimaryCulture(map.next_value()?),
                        Chdf::AddAcceptedCulture => {
                            CountryEvent::AddAcceptedCulture(map.next_value()?)
                        }
                        Chdf::Decision => CountryEvent::Decision(map.next_value()?),
                        Chdf::Other => {
                            map.next_value::<de::IgnoredAny>()?;
                            continue;
                        }
                    };

                    // Most countries tend to have 32 around events
                    if self.events.is_empty() {
                        self.events.reserve(self.estimate);
                    }

                    self.events.push((self.date, val));
                }

                Ok(())
            }
        }

        deserializer.deserialize_map(ExtendVecVisitor {
            date: self.date,
            estimate: self.estimate,
            events: self.events,
        })
    }
}
