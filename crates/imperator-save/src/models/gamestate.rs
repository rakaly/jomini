use crate::ImperatorDate;
use serde::Deserialize;

#[derive(Debug)]
pub struct Save {
    pub meta: Metadata,
    pub gamestate: GameState,
}

#[derive(Debug, Deserialize)]
pub struct Metadata {
    pub version: String,
    pub date: ImperatorDate,
    #[serde(default)]
    pub ironman: bool,
    pub meta_player_name: Option<String>,
    pub enabled_dlcs: Vec<String>,
    pub play_time: i32,
    #[serde(default)]
    pub iron: bool,
}

#[derive(Debug, Deserialize)]
pub struct GameState {
    pub speed: i32,
}

impl<'de> Deserialize<'de> for Save {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Debug, Deserialize)]
        struct ImperatorFlatten {
            pub version: String,
            pub date: ImperatorDate,
            #[serde(default)]
            pub ironman: bool,
            pub meta_player_name: Option<String>,
            pub enabled_dlcs: Vec<String>,
            pub play_time: i32,
            #[serde(default)]
            pub iron: bool,
            pub speed: i32,
        }

        let result = ImperatorFlatten::deserialize(deserializer)?;
        Ok(Save {
            meta: Metadata {
                version: result.version,
                date: result.date,
                ironman: result.ironman,
                meta_player_name: result.meta_player_name,
                enabled_dlcs: result.enabled_dlcs,
                play_time: result.play_time,
                iron: result.iron,
            },
            gamestate: GameState {
                speed: result.speed,
            },
        })
    }
}
