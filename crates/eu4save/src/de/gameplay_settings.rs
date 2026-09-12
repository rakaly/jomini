use crate::models::{GameDifficulty, GameplayOptions, TaxManpowerModifier};
use serde::{de, Deserialize, Deserializer};
use std::fmt;

impl<'de> Deserialize<'de> for GameplayOptions {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct GameplayOptionsVisitor;

        impl<'de> de::Visitor<'de> for GameplayOptionsVisitor {
            type Value = GameplayOptions;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct GameplayOptions")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: de::SeqAccess<'de>,
            {
                let difficulty = seq.next_element::<i32>()?;
                let difficulty =
                    difficulty.ok_or_else(|| de::Error::custom("missing difficulty setting"))?;
                let difficulty = match difficulty {
                    -1 => GameDifficulty::VeryEasy,
                    0 => GameDifficulty::Easy,
                    1 => GameDifficulty::Normal,
                    2 => GameDifficulty::Hard,
                    3 => GameDifficulty::VeryHard,
                    _ => return Err(de::Error::custom("unrecognized difficulty setting")),
                };

                let _handicap = seq.next_element::<i32>()?;
                let _lucky_nations = seq.next_element::<i32>()?;
                let _allow_players_to_hotjoin = seq.next_element::<i32>()?;
                let _allow_players_to_play_same_country = seq.next_element::<i32>()?;
                let _unknown1 = seq.next_element::<i32>()?;
                let _unknown2 = seq.next_element::<i32>()?;
                let _terra_incognita = seq.next_element::<i32>()?;
                let _only_host_observer_save = seq.next_element::<i32>()?;
                let _editable_savegame = seq.next_element::<i32>()?;
                let _locked_ledger = seq.next_element::<i32>()?;
                let _use_dynamic_province_names = seq.next_element::<i32>()?;
                let _custom_nation_difficulty = seq.next_element::<i32>()?;
                let _nations = seq.next_element::<i32>()?;

                let tax_manpower_modifier = seq.next_element::<i32>()?;
                let tax_manpower_modifier = tax_manpower_modifier
                    .ok_or_else(|| de::Error::custom("missing difficulty setting"))?;
                let tax_manpower_modifier = match tax_manpower_modifier {
                    0 => TaxManpowerModifier::Historical,
                    1 => TaxManpowerModifier::Random,
                    2 => TaxManpowerModifier::Equal,
                    _ => return Err(de::Error::custom("unrecognized difficulty setting")),
                };

                while seq.next_element::<de::IgnoredAny>()?.is_some() {}

                Ok(GameplayOptions {
                    difficulty,
                    tax_manpower_modifier,
                })
            }
        }

        deserializer.deserialize_seq(GameplayOptionsVisitor)
    }
}
