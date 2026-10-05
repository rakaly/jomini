use std::fmt;

use crate::models::{CountryId, LocationId, bstr::BStr, de::Maybe};
use arena_serde::{ArenaDeserialize, ArenaSeed};
use serde::{Deserialize, de};

#[derive(Debug, PartialEq, ArenaDeserialize)]
pub struct BuildingManager<'bump> {
    #[arena(deserialize_with = "deserialize_buildings")]
    pub database: BuildingDatabase<'bump>,
}

#[derive(Debug, PartialEq)]
pub struct BuildingDatabase<'bump> {
    ids: &'bump [BuildingId],
    values: &'bump [Option<Building<'bump>>],
}

impl<'bump> BuildingDatabase<'bump> {
    /// Returns an iterator over all buildings in the database
    pub fn iter(&self) -> impl Iterator<Item = &Building<'bump>> {
        self.values.iter().filter_map(|x| x.as_ref())
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Deserialize, Default, ArenaDeserialize,
)]
#[serde(transparent)]
pub struct BuildingId(u32);

impl BuildingId {
    #[inline]
    pub fn new(id: u32) -> Self {
        BuildingId(id)
    }

    #[inline]
    pub fn value(self) -> u32 {
        self.0
    }
}

#[derive(Debug, PartialEq)]
pub struct Building<'bump> {
    pub kind: BStr<'bump>,
    pub level: f64,
    pub location: LocationId,
    pub owner: CountryId,
    pub employed: f64, // 0.08 => 80 employed
    /// The production methods that the building uses. Each method is a block
    /// with the method name as its key. EU5 1.4 building records have no other
    /// blocks at this level. A building can use more than one method.
    pub production_methods: &'bump [BStr<'bump>],
}

impl<'bump> ArenaDeserialize<'bump> for Building<'bump> {
    fn deserialize_in_arena<'de, D>(
        deserializer: D,
        allocator: &'bump arena_serde::Arena,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct BuildingVisitor<'bump>(&'bump arena_serde::Arena);

        impl<'de, 'bump> de::Visitor<'de> for BuildingVisitor<'bump> {
            type Value = Building<'bump>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a building")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut kind = None;
                let mut level = 0.0;
                let mut location = None;
                let mut owner = CountryId::default();
                let mut employed = 0.0;
                let mut methods = bumpalo::collections::Vec::new_in(self.0);

                while let Some(field) =
                    map.next_key_seed(ArenaSeed::<BuildingField<'bump>>::new(self.0))?
                {
                    match field {
                        BuildingField::Kind => {
                            kind = Some(map.next_value_seed(ArenaSeed::new(self.0))?);
                        }
                        BuildingField::Level => level = map.next_value()?,
                        BuildingField::Location => location = Some(map.next_value()?),
                        BuildingField::Owner => owner = map.next_value()?,
                        BuildingField::Employed => employed = map.next_value()?,
                        BuildingField::Other(key) => {
                            if map.next_value::<IsBlock>()?.0 {
                                methods.push(key);
                            }
                        }
                    }
                }

                Ok(Building {
                    kind: kind.ok_or_else(|| de::Error::missing_field("kind"))?,
                    level,
                    location: location.ok_or_else(|| de::Error::missing_field("location"))?,
                    owner,
                    employed,
                    production_methods: methods.into_bump_slice(),
                })
            }
        }

        deserializer.deserialize_map(BuildingVisitor(allocator))
    }
}

enum BuildingField<'bump> {
    Kind,
    Level,
    Location,
    Owner,
    Employed,
    Other(BStr<'bump>),
}

impl<'bump> ArenaDeserialize<'bump> for BuildingField<'bump> {
    fn deserialize_in_arena<'de, D>(
        deserializer: D,
        allocator: &'bump arena_serde::Arena,
    ) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct FieldVisitor<'bump>(&'bump arena_serde::Arena);

        impl<'de, 'bump> de::Visitor<'de> for FieldVisitor<'bump> {
            type Value = BuildingField<'bump>;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a building field")
            }

            fn visit_bytes<E: de::Error>(self, value: &[u8]) -> Result<Self::Value, E> {
                Ok(match value {
                    b"type" | b"kind" => BuildingField::Kind,
                    b"level" => BuildingField::Level,
                    b"location" => BuildingField::Location,
                    b"owner" => BuildingField::Owner,
                    b"employed" => BuildingField::Employed,
                    _ => BuildingField::Other(BStr::new(self.0.alloc_slice_copy(value))),
                })
            }

            fn visit_str<E: de::Error>(self, value: &str) -> Result<Self::Value, E> {
                self.visit_bytes(value.as_bytes())
            }
        }

        deserializer.deserialize_identifier(FieldVisitor(allocator))
    }
}

/// Skips a value and records whether it was a block.
struct IsBlock(bool);

impl<'de> Deserialize<'de> for IsBlock {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct IsBlockVisitor;

        impl<'de> de::Visitor<'de> for IsBlockVisitor {
            type Value = IsBlock;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("any value")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                while map
                    .next_entry::<de::IgnoredAny, de::IgnoredAny>()?
                    .is_some()
                {}
                Ok(IsBlock(true))
            }

            // An empty block has no keys, so it can come as a sequence.
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                while seq.next_element::<de::IgnoredAny>()?.is_some() {}
                Ok(IsBlock(true))
            }

            fn visit_bool<E>(self, _v: bool) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }

            fn visit_i64<E>(self, _v: i64) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }

            fn visit_u64<E>(self, _v: u64) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }

            fn visit_f64<E>(self, _v: f64) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }

            fn visit_str<E>(self, _v: &str) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }

            fn visit_bytes<E>(self, _v: &[u8]) -> Result<Self::Value, E> {
                Ok(IsBlock(false))
            }
        }

        deserializer.deserialize_any(IsBlockVisitor)
    }
}

#[inline]
fn deserialize_buildings<'de, 'bump, D>(
    deserializer: D,
    allocator: &'bump arena_serde::Arena,
) -> Result<BuildingDatabase<'bump>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct BuildingsVisitor<'bump>(&'bump arena_serde::Arena);

    impl<'de, 'bump> de::Visitor<'de> for BuildingsVisitor<'bump> {
        type Value = BuildingDatabase<'bump>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a map containing building entries")
        }

        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: de::MapAccess<'de>,
        {
            let mut building_ids = bumpalo::collections::Vec::with_capacity_in(65536, self.0);
            let mut building_values = bumpalo::collections::Vec::with_capacity_in(65536, self.0);
            while let Some((key, value)) = map.next_entry_seed(
                ArenaSeed::new(self.0),
                ArenaSeed::<Maybe<Building<'bump>>>::new(self.0),
            )? {
                building_ids.push(key);
                building_values.push(value.into_value());
            }

            let ids = building_ids.into_bump_slice();
            let values = building_values.into_bump_slice();

            Ok(BuildingDatabase { ids, values })
        }
    }

    deserializer.deserialize_map(BuildingsVisitor(allocator))
}

#[cfg(test)]
mod tests {
    use super::*;
    use jomini::TextDeserializer;
    use rstest::rstest;

    fn parse_building<'bump>(data: &str, arena: &'bump arena_serde::Arena) -> Building<'bump> {
        let deserializer = TextDeserializer::from_utf8_slice(data.as_bytes()).unwrap();
        Building::deserialize_in_arena(&deserializer, arena).unwrap()
    }

    #[test]
    fn buildings_from_eu5_1_4_save() {
        let arena = arena_serde::Arena::new();
        let data = include_bytes!("../../tests/fixtures/buildings-1.4.txt");
        let deserializer = TextDeserializer::from_utf8_slice(data).unwrap();
        let manager = BuildingManager::deserialize_in_arena(&deserializer, &arena).unwrap();
        assert_eq!(
            manager.database.ids,
            [
                BuildingId::new(0),
                BuildingId::new(199),
                BuildingId::new(16777932)
            ]
        );
        let buildings: Vec<_> = manager.database.iter().collect();
        assert_eq!(buildings.len(), 3);
        let brewery = buildings[0];
        assert_eq!(brewery.kind.to_str(), "brewery");
        assert_eq!(brewery.level, 5.0);
        assert_eq!(brewery.employed, 0.5);
        assert_eq!(brewery.location, LocationId::new(1));
        assert_eq!(brewery.owner, CountryId::new(3));
        assert_eq!(
            brewery.production_methods,
            [BStr::new(b"millet_brewery_maintenance")]
        );
        let jewelry = buildings[1];
        assert_eq!(jewelry.kind.to_str(), "jewelry_guild");
        assert_eq!(jewelry.level, 1.0);
        assert_eq!(jewelry.employed, 0.0);
        assert_eq!(jewelry.location, LocationId::new(125));
        assert_eq!(jewelry.owner, CountryId::new(1788));
        assert_eq!(
            jewelry.production_methods,
            [BStr::new(b"silver_base"), BStr::new(b"gems_enhancement")]
        );
        let barracks = buildings[2];
        assert_eq!(barracks.kind.to_str(), "barracks");
        assert_eq!(barracks.level, 0.0);
        assert_eq!(barracks.employed, 0.0);
        assert_eq!(barracks.location, LocationId::new(4873));
        assert_eq!(barracks.owner, CountryId::new(2061));
        assert_eq!(
            barracks.production_methods,
            [BStr::new(b"barracks_maintenance")]
        );
    }

    #[test]
    fn empty_method_and_unknown_scalars() {
        let arena = arena_serde::Arena::new();
        let building = parse_building(
            r#"
            type=jewelry_guild location=125
            future_text="value" future_integer=-1 future_decimal=0.5 future_bool=yes
            silver_base={ missing={ demand=silver_base silver=0.4 } input=0 }
            gems_enhancement={ }
        "#,
            &arena,
        );
        assert_eq!(building.owner, CountryId::default());
        assert_eq!(
            building.production_methods,
            [BStr::new(b"silver_base"), BStr::new(b"gems_enhancement")]
        );
    }

    #[test]
    fn absent_methods_and_kind_alias() {
        let arena = arena_serde::Arena::new();
        let building = parse_building("kind=barracks location=4873", &arena);
        assert_eq!(building.kind.to_str(), "barracks");
        assert!(building.production_methods.is_empty());
    }

    #[test]
    fn database_skips_none_entries() {
        let arena = arena_serde::Arena::new();
        let deserializer = TextDeserializer::from_utf8_slice(
            b"database={ 0=none 1={ type=barracks location=4873 } 2=none }",
        )
        .unwrap();
        let manager = BuildingManager::deserialize_in_arena(&deserializer, &arena).unwrap();
        assert_eq!(manager.database.iter().count(), 1);
        assert_eq!(manager.database.values[0], None);
        assert_eq!(manager.database.values[2], None);
    }

    #[rstest]
    #[case("type=barracks", "location")]
    #[case("location=4873", "kind")]
    fn missing_required_field(#[case] data: &str, #[case] field: &str) {
        let arena = arena_serde::Arena::new();
        let deserializer = TextDeserializer::from_utf8_slice(data.as_bytes()).unwrap();
        let error = Building::deserialize_in_arena(&deserializer, &arena).unwrap_err();
        assert!(
            error
                .to_string()
                .contains(&format!("missing field `{field}`")),
            "{error}"
        );
    }
}
