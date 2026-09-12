use crate::de::*;
use crate::{CountryTag, Eu4Date, ProvinceId};
use jomini::JominiDeserialize;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify), tsify(into_wasm_abi))]
pub struct Meta {
    pub campaign_id: String,
    pub save_game: String,
    pub player: CountryTag,
    pub displayed_country_name: String,
    pub campaign_length: i32,
    pub date: Eu4Date,
    #[jomini(default)]
    pub is_ironman: bool,
    #[jomini(default, alias = "multi_player")]
    #[jomini(alias = "multi_player")]
    pub multiplayer: bool,
    pub not_observer: bool,
    #[jomini(default)]
    pub dlc_enabled: Vec<String>,
    #[jomini(default)]
    pub mod_enabled: Vec<String>,
    #[jomini(default)]
    pub mods_enabled_names: Vec<ModName>,
    #[jomini(take_last)]
    pub checksum: String,
    pub savegame_version: SavegameVersion,
    #[jomini(default)]
    pub is_random_new_world: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ModName {
    pub filename: String,
    pub name: String,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Eu4Save {
    #[cfg_attr(feature = "serialize", serde(flatten))]
    pub meta: Meta,

    #[cfg_attr(feature = "serialize", serde(flatten))]
    pub game: GameState,
}

impl<'de> Deserialize<'de> for Eu4Save {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Debug, JominiDeserialize)]
        struct Eu4SaveFlatten {
            pub campaign_id: String,
            pub save_game: String,
            pub player: CountryTag,
            pub displayed_country_name: String,
            pub campaign_length: i32,
            pub date: Eu4Date,
            #[jomini(default)]
            pub is_ironman: bool,
            #[jomini(default, alias = "multi_player")]
            #[jomini(alias = "multi_player")]
            pub multiplayer: bool,
            pub not_observer: bool,
            #[jomini(default)]
            pub dlc_enabled: Vec<String>,
            #[jomini(default)]
            pub mod_enabled: Vec<String>,
            #[jomini(default)]
            pub mods_enabled_names: Vec<ModName>,
            #[jomini(take_last)]
            pub checksum: String,
            pub savegame_version: SavegameVersion,
            #[jomini(default)]
            pub is_random_new_world: bool,

            #[jomini(default)]
            pub players_countries: Vec<String>,
            pub current_age: String,
            pub start_date: Eu4Date,
            pub map_area_data: HashMap<String, MapAreaDatum>,
            pub military_hegemon: Option<Hegemon>,
            pub naval_hegemon: Option<Hegemon>,
            pub economic_hegemon: Option<Hegemon>,
            pub trade: TradeNodes,
            #[jomini(duplicated, alias = "rebel_faction")]
            pub rebel_factions: Vec<RebelFaction>,
            #[jomini(default, deserialize_with = "deserialize_vec_pair")]
            pub religions: Vec<(String, ReligionGameState)>,
            pub religion_instance_data: HashMap<String, ReligionInstanceDatum>,
            pub empire: Option<HRE>,
            #[jomini(default, deserialize_with = "deserialize_vec_pair")]
            pub countries: Vec<(CountryTag, Country)>,
            pub provinces: HashMap<ProvinceId, Province>,
            pub income_statistics: LedgerData,
            pub nation_size_statistics: LedgerData,
            pub score_statistics: LedgerData,
            pub inflation_statistics: LedgerData,
            #[jomini(duplicated, alias = "active_war")]
            pub active_wars: Vec<ActiveWar>,
            #[jomini(duplicated, alias = "previous_war")]
            pub previous_wars: Vec<PreviousWar>,
            #[jomini(default)]
            pub achievement_ok: bool,
            #[jomini(default)]
            pub achievement: Vec<i32>,
            #[jomini(default)]
            pub completed_achievements: Vec<i32>,
            #[jomini(alias = "gameplaysettings")]
            pub gameplay_settings: GameplaySettings,
            pub diplomacy: Diplomacy,
            #[jomini(default)]
            pub institutions: Vec<i32>,
            pub random_world: Option<i32>,
        }

        let result = Eu4SaveFlatten::deserialize(deserializer)?;
        Ok(Eu4Save {
            meta: Meta {
                campaign_id: result.campaign_id,
                save_game: result.save_game,
                player: result.player,
                displayed_country_name: result.displayed_country_name,
                campaign_length: result.campaign_length,
                date: result.date,
                is_ironman: result.is_ironman,
                multiplayer: result.multiplayer,
                not_observer: result.not_observer,
                dlc_enabled: result.dlc_enabled,
                mod_enabled: result.mod_enabled,
                mods_enabled_names: result.mods_enabled_names,
                checksum: result.checksum,
                savegame_version: result.savegame_version,
                is_random_new_world: result.is_random_new_world,
            },
            game: GameState {
                players_countries: result.players_countries,
                current_age: result.current_age,
                start_date: result.start_date,
                map_area_data: result.map_area_data,
                military_hegemon: result.military_hegemon,
                naval_hegemon: result.naval_hegemon,
                economic_hegemon: result.economic_hegemon,
                trade: result.trade,
                rebel_factions: result.rebel_factions,
                religions: result.religions,
                religion_instance_data: result.religion_instance_data,
                empire: result.empire,
                countries: result.countries,
                provinces: result.provinces,
                income_statistics: result.income_statistics,
                nation_size_statistics: result.nation_size_statistics,
                score_statistics: result.score_statistics,
                inflation_statistics: result.inflation_statistics,
                active_wars: result.active_wars,
                previous_wars: result.previous_wars,
                achievement_ok: result.achievement_ok,
                achievement: result.achievement,
                completed_achievements: result.completed_achievements,
                gameplay_settings: result.gameplay_settings,
                diplomacy: result.diplomacy,
                institutions: result.institutions,
                random_world: result.random_world,
            },
        })
    }
}

impl Eu4Save {
    pub fn from_deserializer<'de, D>(
        deser: D,
    ) -> Result<Self, <D as serde::de::Deserializer<'de>>::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        Eu4Save::deserialize(deser)
    }
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct GameState {
    #[jomini(default)]
    pub players_countries: Vec<String>,
    pub current_age: String,
    pub start_date: Eu4Date,
    pub map_area_data: HashMap<String, MapAreaDatum>,
    pub military_hegemon: Option<Hegemon>,
    pub naval_hegemon: Option<Hegemon>,
    pub economic_hegemon: Option<Hegemon>,
    pub trade: TradeNodes,
    #[jomini(duplicated, alias = "rebel_faction")]
    pub rebel_factions: Vec<RebelFaction>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub religions: Vec<(String, ReligionGameState)>,
    pub religion_instance_data: HashMap<String, ReligionInstanceDatum>,
    pub empire: Option<HRE>,
    #[jomini(
        default,
        deserialize_with = "deserialize_vec_pair_with_capacity::<_, _, _, 1400>"
    )]
    pub countries: Vec<(CountryTag, Country)>,
    #[jomini(deserialize_with = "deserialize_map_with_capacity::<_, _, _, 5000>")]
    pub provinces: HashMap<ProvinceId, Province>,
    pub income_statistics: LedgerData,
    pub nation_size_statistics: LedgerData,
    pub score_statistics: LedgerData,
    pub inflation_statistics: LedgerData,
    #[jomini(duplicated, alias = "active_war")]
    pub active_wars: Vec<ActiveWar>,
    #[jomini(duplicated, alias = "previous_war")]
    pub previous_wars: Vec<PreviousWar>,
    #[jomini(default)]
    pub achievement_ok: bool,
    #[jomini(default)]
    pub achievement: Vec<i32>,
    #[jomini(default)]
    pub completed_achievements: Vec<i32>,
    #[jomini(alias = "gameplaysettings")]
    pub gameplay_settings: GameplaySettings,
    pub diplomacy: Diplomacy,
    #[jomini(default)]
    pub institutions: Vec<i32>,
    pub random_world: Option<i32>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct TradeNodes {
    #[jomini(duplicated, alias = "node")]
    pub nodes: Vec<TradeNode>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct TradeNode {
    pub countries: Vec<CountryTrade>,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryTrade {
    pub tag: CountryTag,
    pub privateer_money: f32,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct SavegameVersion {
    pub first: u16,
    pub second: u16,
    pub third: u16,
    #[serde(alias = "forth")]
    pub fourth: u16,
    pub name: String,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct RebelFaction {
    pub id: ObjId,
    #[serde(alias = "type")]
    pub kind: String,
    pub name: String,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct GameplaySettings {
    #[serde(alias = "setgameplayoptions")]
    pub options: GameplayOptions,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct MapAreaDatum {
    pub state: Option<MapAreaState>,
    #[jomini(default, duplicated, alias = "investments")]
    pub investments: Vec<TradeCompanyInvestment>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct MapAreaState {
    pub area: String,
    #[jomini(duplicated, alias = "country_state")]
    pub country_states: Vec<CountryState>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct TradeCompanyInvestment {
    pub tag: CountryTag,
    #[jomini(default)]
    pub investments: Vec<String>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryState {
    #[jomini(default)]
    pub prosperity: f32,
    pub country: CountryTag,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Hegemon {
    pub country: CountryTag,
    pub progress: f32,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ReligionGameState {
    #[serde(default)]
    pub amount_of_provinces: i32,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ReligionInstanceDatum {
    #[serde(default)]
    pub defender: Option<CountryTag>,
    #[serde(default)]
    pub defender_date: Option<Eu4Date>,
    #[serde(default)]
    pub papacy: Option<Papacy>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Papacy {
    pub controller: CountryTag,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct HRE {
    pub emperor: Option<CountryTag>,
    #[jomini(duplicated, alias = "passed_reform")]
    pub passed_reforms: Vec<String>,
    #[jomini(default)]
    pub electors: Vec<CountryTag>,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub enum TaxManpowerModifier {
    Historical,
    Random,
    Equal,
}

#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
#[cfg_attr(feature = "specta", derive(specta::Type))]
pub enum GameDifficulty {
    VeryEasy,
    Easy,
    Normal,
    Hard,
    VeryHard,
}

#[derive(Debug, Clone, Serialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify), tsify(into_wasm_abi))]
pub struct GameplayOptions {
    pub difficulty: GameDifficulty,
    pub tax_manpower_modifier: TaxManpowerModifier,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct LedgerData {
    #[jomini(duplicated, alias = "ledger_data")]
    pub ledger: Vec<LedgerDatum>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct LedgerDatum {
    pub name: CountryTag,
    #[serde(default, deserialize_with = "deserialize_vec_pair")]
    pub data: Vec<(u16, i32)>,
}

#[derive(Debug, Clone, JominiDeserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Province {
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub flags: Vec<(String, Eu4Date)>,
    pub name: String,
    pub owner: Option<CountryTag>,
    pub controller: Option<CountryTag>,
    pub previous_controller: Option<CountryTag>,
    pub occupying_rebel_faction: Option<ObjId>,
    #[jomini(default)]
    pub cores: Vec<CountryTag>,
    #[jomini(duplicated)] // thank lambda for this
    pub territorial_core: Vec<CountryTag>,
    #[jomini(default)]
    pub claims: Vec<CountryTag>,
    pub institutions: Vec<f32>,
    pub exploit_date: Option<Eu4Date>,
    pub trade: Option<String>,
    pub original_culture: Option<String>,
    pub culture: Option<String>,
    pub religion: Option<String>,
    pub original_religion: Option<String>,
    pub trade_goods: Option<String>,
    #[jomini(default, deserialize_with = "deserialize_alternating_key_values")]
    pub country_improve_count: HashMap<CountryTag, i32>,
    #[jomini(default)]
    pub latent_trade_goods: Vec<String>,
    #[jomini(default)]
    pub devastation: f32,
    #[jomini(default)]
    pub base_tax: f32,
    #[jomini(default)]
    pub base_production: f32,
    #[jomini(default)]
    pub base_manpower: f32,
    pub capital: Option<String>,
    #[jomini(default)]
    pub local_autonomy: f32,
    #[jomini(default)]
    pub is_city: bool,
    #[jomini(default, deserialize_with = "deserialize_token_bool")]
    pub active_trade_company: bool,
    #[jomini(default)]
    pub center_of_trade: u8,
    #[jomini(default)]
    pub trade_power: f32,
    #[jomini(default, deserialize_with = "deserialize_token_bool")]
    pub hre: bool,
    #[jomini(default, deserialize_with = "deserialize_yes_map")]
    pub buildings: HashMap<String, bool>,
    #[jomini(default)]
    pub building_builders: HashMap<String, CountryTag>,
    #[jomini(default, duplicated, alias = "modifier")]
    pub modifiers: Vec<Modifier>,
    #[jomini(default)]
    pub history: ProvinceHistory,
    #[jomini(default = "default_true")]
    pub ub: bool,
    #[jomini(alias = "colonysize")]
    pub colony_size: Option<f32>,
    pub change_culture_construction: Option<ChangeCultureConstruction>,
    pub centralize_state_construction: Option<CentralizeStateConstruction>,
    #[jomini(default)]
    pub num_centralize_state: i32,
    #[jomini(default)]
    pub expand_infrastructure: i32,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Modifier {
    pub modifier: String,
    pub date: Eu4Date,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ProvinceHistory {
    pub owner: Option<CountryTag>,
    pub base_tax: Option<f32>,
    pub base_production: Option<f32>,
    pub base_manpower: Option<f32>,
    pub religion: Option<String>,
    pub hre: bool,
    pub is_city: bool,
    pub other: HashMap<String, ProvinceEventValue>,
    pub events: Vec<(Eu4Date, ProvinceEvent)>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ProvinceEvent {
    BaseManpower(f32),
    BaseProduction(f32),
    BaseTax(f32),
    Controller(ControllerEvent),
    Hre(bool),
    IsCity(bool),
    Owner(CountryTag),
    Religion(String),
    TradeCompany(bool),
    KV((String, ProvinceEventValue)),
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ControllerEvent {
    pub tag: CountryTag,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum ProvinceEventValue {
    String(String),
    Float(f32),
    Int(i32),
    Bool(bool),
    Object,
    Array,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ChangeCultureConstruction {
    pub start_date: Eu4Date,
    pub total: u32,
    pub original_total: u32,
    #[serde(default)]
    pub progress: f32,
    pub date: Eu4Date,
    #[serde(default)]
    pub power: f32,
    pub envoy: i32,
    pub country: CountryTag,
    pub culture: String,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CentralizeStateConstruction {
    pub start_date: Eu4Date,
    #[serde(default)]
    pub progress: f32,
    pub date: Eu4Date,
    pub country: CountryTag,
}

#[derive(Debug, Clone, JominiDeserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Country {
    #[jomini(default, deserialize_with = "deserialize_token_bool")]
    pub human: bool,
    #[jomini(default)]
    pub was_player: bool,
    #[jomini(default)]
    pub has_switched_nation: bool,
    #[jomini(default)]
    pub is_great_power: bool,
    #[jomini(default, deserialize_with = "deserialize_token_bool")]
    pub luck: bool,
    #[jomini(default)]
    pub history: CountryHistory,
    #[jomini(duplicated)]
    pub previous_country_tags: Vec<CountryTag>,
    pub name: Option<String>,
    pub government_rank: i32,
    pub continent: Vec<i32>,
    pub institutions: Vec<i32>,
    pub capital: ProvinceId,
    pub original_capital: Option<ProvinceId>,
    pub trade_port: ProvinceId,
    pub golden_era_date: Option<Eu4Date>,
    #[jomini(default)]
    pub base_tax: f32,
    #[jomini(default)]
    pub development: f32,
    #[jomini(default)]
    pub prestige: f32,
    #[jomini(default)]
    pub stability: f32,
    #[jomini(default)]
    pub treasury: f32,
    #[jomini(default)]
    pub inflation: f32,
    #[jomini(default)]
    pub corruption: f32,
    #[jomini(default)]
    pub raw_development: f32,
    pub capped_development: f32,
    pub realm_development: f32,
    pub isolationism: i32,
    #[jomini(default)]
    pub manpower: f32,
    #[jomini(default)]
    pub max_manpower: f32,
    #[jomini(default)]
    pub sailors: f32,
    #[jomini(default)]
    pub max_sailors: f32,
    #[jomini(default, alias = "overextension_percentage")]
    pub overextension: f32,
    #[jomini(default)]
    pub innovativeness: f32,
    #[jomini(default)]
    pub religious_unity: f32,
    #[jomini(default)]
    pub church: Option<CountryChurch>,
    #[jomini(default)]
    pub national_focus: NationalFocus,
    pub recalculate_strategy: bool,
    pub colors: CountryColors,
    pub dirty_colony: bool,
    pub primary_culture: Option<String>,
    pub dominant_culture: Option<String>,
    #[jomini(duplicated, alias = "accepted_culture")]
    pub accepted_cultures: Vec<String>,
    #[jomini(duplicated, alias = "blessing")]
    pub blessings: Vec<String>,
    pub religion: Option<String>,
    pub dominant_religion: Option<String>,
    pub technology_group: Option<String>,
    pub unit_type: Option<String>,
    pub tribute_type: Option<i32>,
    pub technology: CountryTechnology,
    pub colonial_parent: Option<CountryTag>,
    pub ledger: CountryLedger,
    #[jomini(duplicated, alias = "loan")]
    pub loans: Vec<Loan>,
    #[jomini(duplicated, alias = "estate")]
    pub estates: Vec<Estate>,
    #[jomini(default)]
    pub subjects: Vec<CountryTag>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub flags: Vec<(String, Eu4Date)>,
    pub highest_possible_fort: Option<i32>,
    pub transfer_home_bonus: f32,
    #[jomini(duplicated, alias = "enemy")]
    pub enemies: Vec<String>,
    #[jomini(default)]
    pub current_power_projection: f32,
    #[jomini(default)]
    pub great_power_score: f32,
    #[jomini(default)]
    pub total_war_worth: u32,
    #[jomini(default)]
    pub war_exhaustion: f32,
    #[jomini(default)]
    pub land_maintenance: f32,
    #[jomini(default)]
    pub naval_maintenance: f32,
    #[jomini(default)]
    pub colonial_maintenance: f32,
    #[jomini(default)]
    pub missionary_maintenance: f32,
    #[jomini(default)]
    pub army_tradition: f32,
    #[jomini(default)]
    pub navy_tradition: f32,
    #[jomini(default)]
    pub army_professionalism: f32,
    #[jomini(duplicated, alias = "army")]
    pub armies: Vec<Army>,
    #[jomini(duplicated, alias = "navy")]
    pub navies: Vec<Navy>,
    pub custom_nation_points: Option<f32>,
    #[jomini(default)]
    pub num_of_cities: i32,
    #[jomini(default)]
    pub num_of_total_ports: i32,
    #[jomini(default)]
    pub completed_missions: Vec<String>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub active_idea_groups: Vec<(String, u8)>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub adm_spent_indexed: Vec<(i32, i32)>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub dip_spent_indexed: Vec<(i32, i32)>,
    #[jomini(default, deserialize_with = "deserialize_vec_pair")]
    pub mil_spent_indexed: Vec<(i32, i32)>,
    #[jomini(default)]
    pub losses: WarParticipantLosses,
    #[jomini(default)]
    pub decision_seed: i32,
    #[jomini(duplicated, alias = "mercenary_company")]
    pub mercenary_companies: Vec<MercenaryCompany>,
    #[jomini(duplicated, alias = "active_policy")]
    pub active_policies: Vec<CountryPolicy>,
    pub monarch: Option<ObjId>,
    pub heir: Option<ObjId>,
    #[jomini(duplicated, alias = "leader")]
    pub leaders: Vec<ObjId>,
    #[jomini(duplicated, alias = "previous_monarch")]
    pub previous_monarchs: Vec<ObjId>,
    pub government: Option<CountryGovernment>,
    #[jomini(default)]
    pub powers: [i32; 3],
    #[jomini(default)]
    pub mercantilism: f32,
    #[jomini(default)]
    pub republican_tradition: f32,
    #[jomini(default)]
    pub devotion: f32,
    #[jomini(default)]
    pub meritocracy: f32,
    #[jomini(default)]
    pub legitimacy: f32,
    #[jomini(default)]
    pub absolutism: f32,
    #[jomini(default)]
    pub horde_unity: f32,
    #[jomini(default)]
    pub splendor: f32,
    #[jomini(default)]
    pub merchants: EnvoyGroup,
    #[jomini(default)]
    pub colonists: EnvoyGroup,
    #[jomini(default)]
    pub diplomats: EnvoyGroup,
    #[jomini(default)]
    pub missionaries: EnvoyGroup,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryPolicy {
    pub policy: String,
    pub date: Eu4Date,
}

#[derive(Debug, Clone, JominiDeserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryGovernment {
    pub government: String,
    #[jomini(default)]
    pub reform_stack: CountryGovernmentReforms,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryGovernmentReforms {
    pub reforms: Vec<String>,
    #[serde(default)]
    pub history: Vec<String>,
}

#[derive(Debug, Clone, JominiDeserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct EnvoyGroup {
    #[jomini(duplicated, alias = "envoy")]
    pub envoys: Vec<Envoy>,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Envoy {
    #[serde(default)]
    pub action: i32,
    #[serde(default, alias = "type")]
    pub kind: u32,
    #[serde(default)]
    pub id: u32,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryLedger {
    #[serde(default)]
    pub income: Vec<f32>,
    #[serde(default)]
    pub expense: Vec<f32>,
    #[serde(alias = "lastmonthincome")]
    pub last_month_income: Option<f32>,
    #[serde(default, alias = "lastmonthincometable")]
    pub last_month_income_table: Vec<f32>,
    #[serde(default, alias = "lastmonthexpensetable")]
    pub last_month_expense_table: Vec<f32>,
    #[serde(
        default,
        alias = "totalexpensetable",
        deserialize_with = "ledger_vec_f32"
    )]
    pub total_expense_table: Vec<f32>,
    #[serde(default, alias = "lastyearincome", deserialize_with = "ledger_vec_f32")]
    pub last_year_income: Vec<f32>,
    #[serde(
        default,
        alias = "lastyearexpense",
        deserialize_with = "ledger_vec_f32"
    )]
    pub last_year_expense: Vec<f32>,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryHistory {
    pub government: Option<String>,
    pub technology_group: Option<String>,
    pub primary_culture: Option<String>,
    pub religion: Option<String>,
    pub capital: Option<ProvinceId>,
    pub add_government_reform: Vec<String>,
    pub events: Vec<(Eu4Date, CountryEvent)>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum CountryEvent {
    Monarch(Monarch),
    Heir(Monarch),
    MonarchHeir(Monarch),
    MonarchConsort(Monarch),
    Queen(Monarch),
    Union(u32),
    Capital(u32),
    ChangedCountryNameFrom(String),
    ChangedCountryAdjectiveFrom(String),
    ChangedCountryMapColorFrom(
        #[serde(default, deserialize_with = "deserialize_list_overflow_byte")] [u8; 3],
    ),
    ChangedTagFrom(CountryTag),
    Leader(Leader),
    NationalFocus(NationalFocus),
    PrimaryCulture(String),
    AddAcceptedCulture(String),
    RemoveAcceptedCulture(String),
    Religion(String),
    Decision(String),
}

impl CountryEvent {
    pub fn as_monarch(&self) -> Option<&Monarch> {
        match &self {
            CountryEvent::Monarch(x)
            | CountryEvent::Heir(x)
            | CountryEvent::MonarchHeir(x)
            | CountryEvent::MonarchConsort(x)
            | CountryEvent::Queen(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_leader(&self) -> Option<&Leader> {
        self.as_monarch().and_then(|x| x.leader.as_ref()).or({
            if let CountryEvent::Leader(x) = &self {
                Some(x)
            } else {
                None
            }
        })
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Monarch {
    pub id: ObjId,
    pub name: String,
    pub country: CountryTag,
    #[serde(alias = "DIP")]
    pub dip: i16,
    #[serde(alias = "ADM")]
    pub adm: i16,
    #[serde(alias = "MIL")]
    pub mil: i16,
    #[serde(default, deserialize_with = "deserialize_token_bool")]
    pub regent: bool,
    #[serde(default)]
    pub culture: Option<String>,
    #[serde(default)]
    pub religion: Option<String>,
    pub birth_date: Eu4Date,
    #[serde(default, deserialize_with = "deserialize_vec_pair")]
    pub personalities: Vec<(String, String)>,
    pub leader_id: Option<ObjId>,
    pub leader: Option<Leader>,
    pub dynasty: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub enum LeaderKind {
    Admiral,
    General,
    Explorer,
    Conquistador,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct Leader {
    pub name: String,
    #[serde(alias = "type")]
    pub kind: LeaderKind,
    #[serde(default)]
    pub maneuver: u16,
    #[serde(default)]
    pub shock: u16,
    #[serde(default)]
    pub fire: u16,
    #[serde(default)]
    pub siege: u16,
    pub monarch_id: Option<ObjId>,
    pub personality: Option<String>,

    // While activation and id can be none, it is so rare that there
    // is a test case for it to prevent regression.
    pub activation: Option<Eu4Date>,
    pub id: Option<ObjId>,
}

#[derive(Debug, Default, Clone, Serialize)]
pub enum NationalFocus {
    #[serde(rename = "ADM")]
    Adm,
    #[serde(rename = "DIP")]
    Dip,
    #[serde(rename = "MIL")]
    Mil,
    #[serde(rename = "none")]
    #[default]
    None,
}

impl<'de> Deserialize<'de> for NationalFocus {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct NationalFocusVisitor;
        impl serde::de::Visitor<'_> for NationalFocusVisitor {
            type Value = NationalFocus;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("national focus")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match v {
                    "ADM" => Ok(NationalFocus::Adm),
                    "DIP" => Ok(NationalFocus::Dip),
                    "MIL" => Ok(NationalFocus::Mil),
                    "none" => Ok(NationalFocus::None),
                    _ => Err(E::invalid_value(
                        serde::de::Unexpected::Str(v),
                        &"national focus",
                    )),
                }
            }
        }

        deserializer.deserialize_str(NationalFocusVisitor)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MercenaryCompany {
    pub id: ObjId,
    pub tag: String,
    pub manpower: Option<f32>,
    pub starting_manpower: Option<f32>,
    pub leader: Option<Leader>,
    pub unit: Option<ObjId>,
    pub hiring_date: Option<Eu4Date>,
    pub disband_date: Option<Eu4Date>,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryColors {
    #[serde(default, deserialize_with = "deserialize_list_overflow_byte")]
    pub revolutionary_colors: [u8; 3],
    #[serde(default, deserialize_with = "deserialize_list_overflow_byte")]
    pub map_color: [u8; 3],
    #[serde(default, deserialize_with = "deserialize_list_overflow_byte")]
    pub country_color: [u8; 3],
    #[serde(default, deserialize_with = "deserialize_list_overflow_byte_opt")]
    pub color: Option<[u8; 3]>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct CountryChurch {
    pub power: f32,
    #[jomini(duplicated, alias = "aspect")]
    pub aspects: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Loan {
    pub id: ObjId,
    pub lender: String,
    pub interest: f32,
    #[serde(default)]
    pub fixed_interest: bool,
    pub amount: i32,
    pub expiry_date: Eu4Date,
    #[serde(default)]
    pub spawned: bool,
}

#[derive(Debug, Clone, JominiDeserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Estate {
    #[jomini(alias = "type")]
    pub _type: String,
    #[jomini(default)]
    pub loyalty: f32,
    #[jomini(default)]
    pub territory: f32,
    #[jomini(default)]
    pub provinces: Vec<ProvinceId>,
    #[jomini(default)]
    pub active_influences: Vec<i32>,
    #[jomini(default, duplicated, alias = "influence_modifier")]
    pub influence_modifiers: Vec<InfluenceModifier>,
    #[jomini(default)]
    pub num_of_estate_agendas_completed: i32,
    #[jomini(default, deserialize_with = "deserialize_map_pair")]
    pub granted_privileges: Vec<(String, Eu4Date)>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct InfluenceModifier {
    #[serde(default)]
    pub value: f32,
    pub desc: String,
    pub date: Eu4Date,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct CountryTechnology {
    pub adm_tech: u8,
    pub dip_tech: u8,
    pub mil_tech: u8,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Army {
    pub id: ObjId,
    pub name: String,
    pub location: ProvinceId,
    #[jomini(duplicated, alias = "regiment")]
    pub regiments: Vec<Regiment>,
    pub movement_progress_last_updated: Eu4Date,
    pub graphical_culture: String,
    pub mercenary_company: Option<ObjId>,
    #[jomini(default)]
    pub main_army: bool,
    #[jomini(default)]
    pub is_invading: bool,
    #[jomini(default)]
    pub visible_to_ai: bool,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Navy {
    pub id: ObjId,
    pub name: String,
    pub location: ProvinceId,
    pub previous: Option<ProvinceId>,
    pub previous_war: Option<i32>,
    #[jomini(duplicated, alias = "ship")]
    pub ships: Vec<Ship>,
    pub movement_progress_last_updated: Eu4Date,
    pub graphical_culture: String,
    pub active_fraction_last_month: f32,
    #[jomini(default)]
    pub attrition: bool,
    #[jomini(default)]
    pub visible_to_ai: bool,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Ship {
    pub id: ObjId,
    pub name: String,
    pub home: ProvinceId,
    #[serde(alias = "type")]
    pub _type: String,
    pub morale: f32,
    #[serde(default = "default_strength")]
    pub strength: f32,
    pub flagship: Option<Flagship>,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Flagship {
    pub name: String,
    pub original_owner: CountryTag,
    pub is_captured: bool,
    #[jomini(duplicated, alias = "modification")]
    pub modifications: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize, Default)]
#[cfg_attr(feature = "tsify", derive(tsify::Tsify))]
pub struct ObjId {
    pub id: u32,
    #[serde(alias = "type")]
    pub _type: u32,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Regiment {
    pub id: ObjId,
    pub name: String,
    pub home: ProvinceId,
    #[serde(alias = "type")]
    pub _type: String,
    pub morale: f32,
    #[serde(default)]
    pub drill: f32,
    #[serde(default = "default_strength")]
    pub strength: f32,
}

fn default_strength() -> f32 {
    1.0
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct ActiveWar {
    pub name: String,
    pub history: WarHistory,
    #[jomini(duplicated, default)]
    pub participants: Vec<WarParticipant>,
    pub original_attacker: CountryTag,
    pub original_defender: CountryTag,
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct PreviousWar {
    pub name: String,
    pub history: WarHistory,
    #[jomini(duplicated, default)]
    pub participants: Vec<WarParticipant>,
    pub original_attacker: CountryTag,
    pub original_defender: CountryTag,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct WarParticipant {
    pub value: f32,
    pub tag: CountryTag,
    #[serde(default)]
    pub losses: WarParticipantLosses,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct WarParticipantLosses {
    #[serde(default)]
    pub members: Vec<i32>,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct WarGoal {
    #[serde(alias = "type")]
    pub _type: String,
    pub casus_belli: String,
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct WarHistory {
    pub name: Option<String>,
    pub war_goal: Option<WarGoal>,
    pub succession: Option<String>,
    pub events: Vec<(Eu4Date, WarEvent)>,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum WarEvent {
    AddAttacker(CountryTag),
    AddDefender(CountryTag),
    RemoveAttacker(CountryTag),
    RemoveDefender(CountryTag),
    Battle(Battle),
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Battle {
    pub name: String,
    pub location: ProvinceId,
    #[serde(alias = "result", deserialize_with = "deserialize_token_bool")]
    pub attacker_won: bool,
    pub attacker: BattleSide,
    pub defender: BattleSide,
    #[serde(default)]
    pub winner_alliance: f32,
    #[serde(default)]
    pub loser_alliance: f32,
}

#[derive(Debug, Clone, Deserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct BattleSide {
    #[serde(default)]
    pub cavalry: u32,
    #[serde(default)]
    pub infantry: u32,
    #[serde(default)]
    pub artillery: u32,
    #[serde(default)]
    pub heavy_ship: u32,
    #[serde(default)]
    pub light_ship: u32,
    #[serde(default)]
    pub galley: u32,
    #[serde(default)]
    pub transport: u32,
    pub losses: i32,
    pub country: CountryTag,

    #[serde(deserialize_with = "empty_string_is_none")]
    pub commander: Option<String>,
}

fn default_true() -> bool {
    true
}

#[derive(Debug, Clone, JominiDeserialize)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Diplomacy {
    #[jomini(duplicated, default, alias = "dependency")]
    pub dependencies: Vec<DiplomacyDependency>,

    #[jomini(duplicated, default, alias = "alliance")]
    pub alliances: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "royal_marriage")]
    pub royal_marriages: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "warning")]
    pub warnings: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "subsidies")]
    pub subsidies: Vec<DiplomacySubsidy>,

    #[jomini(duplicated, default, alias = "guarantee")]
    pub guarantees: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "transfer_trade_power")]
    pub transfer_trade_powers: Vec<DiplomacyTransferTradePower>,

    #[jomini(duplicated, default, alias = "war_reparations")]
    pub war_reparations: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "steer_trade")]
    pub steer_trades: Vec<DiplomacyRelationship>,

    #[jomini(duplicated, default, alias = "condottieri")]
    pub condottieris: Vec<DiplomacyCondottieri>,
}

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
pub struct DiplomacyDependency {
    pub first: CountryTag,
    pub second: CountryTag,
    #[jomini(default)]
    pub start_date: Option<Eu4Date>,
    #[jomini(default)]
    pub end_date: Option<Eu4Date>,
    pub subject_type: String,
}

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
pub struct DiplomacyRelationship {
    pub first: CountryTag,
    pub second: CountryTag,
    #[jomini(default)]
    pub start_date: Option<Eu4Date>,
    #[jomini(default)]
    pub end_date: Option<Eu4Date>,
    #[jomini(default)]
    pub is_enforced: bool,
}

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
pub struct DiplomacySubsidy {
    pub first: CountryTag,
    pub second: CountryTag,
    #[jomini(default)]
    pub start_date: Option<Eu4Date>,
    #[jomini(default)]
    pub amount: f32,
    #[jomini(default)]
    pub duration: u16,
}

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
pub struct DiplomacyTransferTradePower {
    pub first: CountryTag,
    pub second: CountryTag,
    #[jomini(default)]
    pub start_date: Option<Eu4Date>,
    #[jomini(default)]
    pub is_enforced: bool,
    #[jomini(default)]
    pub amount: f32,
}

#[derive(Debug, Clone, JominiDeserialize, Serialize)]
pub struct DiplomacyCondottieri {
    pub first: CountryTag,
    pub second: CountryTag,
    #[jomini(default)]
    pub start_date: Option<Eu4Date>,
    pub unit: ObjId,
    #[jomini(default)]
    pub amount: f32,
    #[jomini(default)]
    pub participation: f32,
}
