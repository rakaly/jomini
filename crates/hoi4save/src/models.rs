use crate::{
    de::{
        deserialize_hashmap_f64, deserialize_id_map, deserialize_vec_pair, empty_string_is_none,
        empty_tag_is_none,
    },
    CountryTag, Hoi4Date,
};
use jomini::JominiDeserialize;
use serde::Serialize;
use std::collections::HashMap;

#[derive(JominiDeserialize, Debug, Clone, Serialize)]
pub struct Hoi4Save {
    pub player: Option<String>,
    pub date: Hoi4Date,

    /// The game version that wrote the save (eg: "Operation Postern
    /// v1.19.3.0.c01a (5632)")
    #[jomini(default)]
    pub version: Option<String>,

    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub countries: Vec<(CountryTag, Country)>,

    /// The states of the world, keyed by state id. The provinces that make up
    /// each state are not in the save. They come from the game files.
    #[jomini(default, deserialize_with = "deserialize_id_map")]
    pub states: HashMap<u32, State>,

    /// Provinces keyed by province id. Older saves (for example, 1.10) write
    /// all provinces. Newer saves (for example, 1.19) write only some
    /// provinces. Do not use the presence of a province as a signal.
    #[jomini(default, deserialize_with = "deserialize_id_map")]
    pub provinces: HashMap<u32, Province>,
}

#[derive(JominiDeserialize, Debug, Clone, Serialize)]
pub struct Country {
    #[jomini(default)]
    pub stability: f64,
    #[jomini(default)]
    pub war_support: f64,
    #[jomini(default, deserialize_with = "deserialize_hashmap_f64")]
    pub variables: HashMap<String, f64>,

    /// The cosmetic tag that changes the name, flag, and color of the
    /// country. It is `None` when the country has no cosmetic tag.
    #[jomini(default, deserialize_with = "empty_string_is_none")]
    pub cosmetic_tag: Option<String>,

    /// The state id of the capital. Countries that do not exist on the map
    /// can also have a capital. To find the countries on the map, use the
    /// state owners.
    #[jomini(default)]
    pub capital: u32,

    #[jomini(default)]
    pub politics: Option<Politics>,
}

#[derive(JominiDeserialize, Debug, Clone, Serialize)]
pub struct Politics {
    /// The ideology group of the party that rules the country (eg: fascism)
    #[jomini(default)]
    pub ruling_party: Option<String>,
}

#[derive(JominiDeserialize, Debug, Clone, Serialize)]
pub struct State {
    /// The country that owns the state. An empty tag in the save becomes
    /// `None`.
    #[jomini(default, deserialize_with = "empty_tag_is_none")]
    pub owner: Option<CountryTag>,

    /// The country that controls the state, when it is not the owner. Older
    /// saves (for example, 1.10) do not write this value.
    #[jomini(default, deserialize_with = "empty_tag_is_none")]
    pub controller: Option<CountryTag>,
}

#[derive(JominiDeserialize, Debug, Clone, Serialize)]
pub struct Province {
    /// The country that controls the province, when it is different from the
    /// owner of the state. An empty tag in the save becomes `None`.
    #[jomini(default, deserialize_with = "empty_tag_is_none")]
    pub controller: Option<CountryTag>,
}
