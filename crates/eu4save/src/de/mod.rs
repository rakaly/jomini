mod alternating_key_values;
mod country_history;
mod gameplay_settings;
mod leader_kind;
mod ledger_vec;
mod list_overflow_byte;
mod map_capacity;
mod map_pair;
mod province_event_value;
mod province_history;
mod token_bool;
mod trade_node;
mod vec_pair;
mod war_history;
mod yes_map;

pub(crate) use alternating_key_values::*;
pub(crate) use ledger_vec::*;
pub(crate) use list_overflow_byte::*;
pub(crate) use map_capacity::*;
pub(crate) use map_pair::*;
pub(crate) use token_bool::*;
pub use vec_pair::*;
pub(crate) use yes_map::*;

use serde::{Deserialize, Deserializer};

pub(crate) fn empty_string_is_none<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    if s.is_empty() {
        Ok(None)
    } else {
        Ok(Some(s))
    }
}
