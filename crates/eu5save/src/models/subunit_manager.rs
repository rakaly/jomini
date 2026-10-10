use crate::models::de::Maybe;
use crate::models::{CountryId, LocationId, PopId, UnitId};
use arena_serde::{ArenaDeserialize, ArenaSeed};
use serde::{Deserialize, de};
use std::fmt;

#[derive(Debug, PartialEq, ArenaDeserialize)]
pub struct SubUnitManager<'bump> {
    #[arena(deserialize_with = "deserialize_units")]
    pub database: SubUnitDatabase<'bump>,
}

impl<'bump> SubUnitManager<'bump> {}

#[derive(Debug, PartialEq)]
pub struct SubUnitDatabase<'bump> {
    ids: &'bump [SubUnitId],
    values: &'bump [Option<SubUnit<'bump>>],
}

impl<'bump> SubUnitDatabase<'bump> {
    /// Returns an iterator over all units in the database
    pub fn iter(&self) -> impl Iterator<Item = &SubUnit<'bump>> {
        self.values.iter().filter_map(|x| x.as_ref())
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Default, ArenaDeserialize,
)]
#[serde(transparent)]
pub struct SubUnitId(u32);

impl SubUnitId {
    #[inline]
    pub fn new(id: u32) -> Self {
        SubUnitId(id)
    }

    #[inline]
    pub fn value(self) -> u32 {
        self.0
    }
}

#[derive(Debug, ArenaDeserialize, PartialEq)]
pub struct SubUnit<'bump> {
    #[arena(default)]
    pub owner: CountryId,
    #[arena(default)]
    pub controller: CountryId,
    pub home: LocationId,
    pub unit: UnitId,
    // Some populated subunit records omit morale.
    #[arena(default)]
    pub morale: f64,
    #[arena(default)]
    pub experience: f64,
    #[arena(default)]
    pub strength: f64,
    #[arena(default)]
    pub max_strength: f64,
    #[arena(default)]
    pub levies: &'bump [PopId],
}

#[inline]
fn deserialize_units<'de, 'bump, D>(
    deserializer: D,
    allocator: &'bump arena_serde::Arena,
) -> Result<SubUnitDatabase<'bump>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct SubUnitVisitor<'bump>(&'bump arena_serde::Arena);

    impl<'de, 'bump> de::Visitor<'de> for SubUnitVisitor<'bump> {
        type Value = SubUnitDatabase<'bump>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map containing sub-unit entries")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut unit_ids = bumpalo::collections::Vec::with_capacity_in(4096, self.0);
            let mut unit_values = bumpalo::collections::Vec::with_capacity_in(4096, self.0);
            while let Some((key, value)) = map.next_entry_seed(
                ArenaSeed::new(self.0),
                ArenaSeed::<Maybe<SubUnit<'bump>>>::new(self.0),
            )? {
                unit_ids.push(key);
                unit_values.push(value.into_value());
            }

            let ids = unit_ids.into_bump_slice();
            let values = unit_values.into_bump_slice();
            Ok(SubUnitDatabase { ids, values })
        }
    }

    deserializer.deserialize_map(SubUnitVisitor(allocator))
}

#[cfg(test)]
mod tests {
    use super::*;
    use jomini::TextDeserializer;

    fn deserialize<'bump>(
        data: &str,
        allocator: &'bump arena_serde::Arena,
    ) -> Result<SubUnitManager<'bump>, jomini::Error> {
        let mut deserializer =
            TextDeserializer::from_utf8_reader(jomini::text::TokenReader::new(data.as_bytes()));
        SubUnitManager::deserialize_in_arena(&mut deserializer, allocator)
    }

    #[test]
    fn subunit_with_omitted_morale() {
        let allocator = arena_serde::Arena::new();
        let manager = deserialize(
            "database={ 1={ owner=767 controller=767 home=10254 unit=620757308 strength=0.098 } }",
            &allocator,
        )
        .unwrap();
        let unit = manager.database.iter().next().unwrap();
        assert_eq!(unit.morale, 0.0);
        assert_eq!(unit.strength, 0.098);
        assert_eq!(unit.home.value(), 10254);
        assert_eq!(unit.unit.value(), 620757308);
    }

    #[test]
    fn subunit_with_explicit_morale() {
        let allocator = arena_serde::Arena::new();
        let manager = deserialize(
            "database={ 1={ home=1 unit=2 morale=3.55509 } 2={ home=3 unit=4 morale=0 } 3=none }",
            &allocator,
        )
        .unwrap();
        let values: Vec<_> = manager.database.iter().map(|unit| unit.morale).collect();
        assert_eq!(values, [3.55509, 0.0]);
    }

    #[test]
    fn invalid_morale_remains_an_error() {
        let allocator = arena_serde::Arena::new();
        assert!(
            deserialize(
                "database={ 1={ home=1 unit=2 morale=invalid } }",
                &allocator
            )
            .is_err()
        );
    }

    #[test]
    fn missing_home_remains_an_error() {
        let allocator = arena_serde::Arena::new();
        let error = deserialize("database={ 1={ unit=2 morale=1 } }", &allocator).unwrap_err();
        assert!(error.to_string().contains("home"));
    }

    #[test]
    fn missing_parent_unit_remains_an_error() {
        let allocator = arena_serde::Arena::new();
        let error = deserialize("database={ 1={ home=1 morale=1 } }", &allocator).unwrap_err();
        assert!(error.to_string().contains("unit"));
    }
}
