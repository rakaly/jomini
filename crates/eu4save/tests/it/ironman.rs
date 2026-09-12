use crate::utils;
use eu4save::{
    models::{GameDifficulty, ProvinceEvent, ProvinceEventValue, TaxManpowerModifier},
    query::{
        CountryManaUsage, LedgerPoint, NationEvent, NationEventKind, NationEvents, PlayerHistory,
        Query,
    },
    CountryTag, Encoding, Eu4Date, Eu4File, FailedResolveStrategy, MeltOptions, PdsDate,
    ProvinceId, SegmentedResolver, SegmentedResolverBuilder,
};
use highway::{HighwayHash, HighwayHasher};
use paste::paste;
use std::{io::Cursor, sync::LazyLock};

static TOKENS: LazyLock<SegmentedResolverBuilder> = LazyLock::new(|| {
    let file_data = pdx_fixtures::tokens("eu4");
    SegmentedResolver::parse(file_data.as_slice()).unwrap()
});

macro_rules! skip_if_no_tokens {
    () => {
        if TOKENS.is_empty() {
            return;
        }
    };
}

#[test]
fn test_eu4_bin() {
    skip_if_no_tokens!();
    let file = utils::request_file("ragusa2.bin.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    assert_eq!(file.encoding(), Encoding::BinaryZip);
    assert_eq!(save.meta.player, "CRO");

    let query = Query::from_save(save);
    let province_owners = query.province_owners();
    let nation_events = query.nation_events(&province_owners);
    let histories = query.player_histories(&nation_events);
    assert_eq!(
        query.starting_country(&histories),
        Some("RAG".parse().unwrap())
    );
    assert_eq!(query.players(), vec![]);

    let expected_histories = vec![PlayerHistory {
        history: NationEvents {
            initial: "RAG".parse().unwrap(),
            latest: "CRO".parse().unwrap(),
            stored: "CRO".parse().unwrap(),
            events: vec![NationEvent {
                date: Eu4Date::from_ymd(1769, 1, 2),
                kind: NationEventKind::TagSwitch("CRO".parse().unwrap()),
            }],
        },
        is_human: true,
        player_names: Vec::new(),
    }];
    assert_eq!(expected_histories, histories);

    let mut output = Vec::new();
    file.melt(MeltOptions::new(), TOKENS.resolver(), &mut output)
        .unwrap();
    let checksum = HighwayHasher::default().hash256(output.as_slice());
    insta::assert_snapshot!(format!("{:016x}{:016x}{:016x}{:016x}", checksum[0], checksum[1], checksum[2], checksum[3]), @"83441f7282de4f211b37fba460ff71c55c91c7552e68ac8ac56f4700a124fa7a");
}

#[test]
fn test_eu4_kandy_bin() {
    skip_if_no_tokens!();
    let file = utils::request_file("kandy2.bin.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    assert_eq!(file.encoding(), Encoding::BinaryZip);
    assert_eq!(save.meta.player, "BHA");

    let query = Query::from_save(save);
    let province_owners = query.province_owners();
    let nation_events = query.nation_events(&province_owners);
    let histories = query.player_histories(&nation_events);

    let player = query.country(&"BHA".parse().unwrap()).unwrap();
    assert!(!player.completed_missions.is_empty());

    assert_eq!(
        query.country_tag_hex_color(&"BHA".parse().unwrap()),
        Some(String::from("#50a50a"))
    );

    assert_eq!(
        query
            .save()
            .game
            .provinces
            .get(&ProvinceId::from(1))
            .unwrap()
            .owner
            .as_ref()
            .unwrap(),
        &"SCA"
    );

    assert_eq!(
        query.starting_country(&histories),
        Some("KND".parse().unwrap())
    );
    assert_eq!(
        query
            .players()
            .iter()
            .map(|x| x.name.as_str())
            .collect::<Vec<_>>(),
        vec!["comagoosie"]
    );

    let subjects: Vec<CountryTag> = vec![
        "TEO".parse().unwrap(),
        "YOK".parse().unwrap(),
        "C21".parse().unwrap(),
        "C23".parse().unwrap(),
    ];

    assert_eq!(player.subjects, subjects);

    // Testing binary encoded saves can extract province building history perfectly fine
    let london = query
        .save()
        .game
        .provinces
        .get(&ProvinceId::from(236))
        .unwrap();

    let (date, _marketplace, val) = london
        .history
        .events
        .iter()
        .filter_map(|(date, event)| match event {
            ProvinceEvent::KV((key, value)) => Some((date, key, value)),
            _ => None,
        })
        .find(|(_date, key, _value)| key.as_str() == "marketplace")
        .unwrap();
    assert!(matches!(val, ProvinceEventValue::Bool(v) if v == &true));
    assert_eq!(date.game_fmt().to_string(), String::from("1486.6.3"));

    let building_date = query
        .province_building_history(london)
        .iter()
        .find(|x| x.building == "marketplace")
        .map(|x| x.date.game_fmt().to_string());
    assert_eq!(building_date, Some(String::from("1486.6.3")));

    let building_date = query
        .province_building_history(london)
        .iter()
        .find(|x| x.building == "fort_15th")
        .map(|x| x.date);
    assert_eq!(building_date, Some(query.save().game.start_date));

    assert_eq!(player.active_idea_groups.len(), 9);
    assert_eq!(
        player.active_idea_groups[8],
        (String::from("economic_ideas"), 2)
    );
}

#[cfg(any(feature = "zstd_c", feature = "zstd_rust"))]
#[test]
fn test_eu4_kandy_bin_zst() {
    skip_if_no_tokens!();
    let file = utils::request_file("kandy2.bin.zst.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    assert_eq!(file.encoding(), Encoding::BinaryZip);
    assert_eq!(save.meta.player, "BHA");
}

#[test]
fn test_eu4_same_campaign_id() {
    skip_if_no_tokens!();
    let file = utils::request_file("ita2.eu4");
    let file2 = utils::request_file("ita2_later.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let file2 = Eu4File::from_file(file2).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    let save2 = file2.parse_save(&TOKENS.resolver()).unwrap();
    assert_eq!(save.meta.campaign_id, save2.meta.campaign_id);
    assert!(save.meta.date < save2.meta.date);
}

#[test]
fn test_eu4_ita1() {
    skip_if_no_tokens!();
    let file = utils::request_file("ita1.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    assert_eq!(file.encoding(), Encoding::BinaryZip);
    assert_eq!(save.meta.player, "ITA");
    let settings = &save.game.gameplay_settings.options;
    assert_eq!(settings.difficulty, GameDifficulty::Normal);
    assert_eq!(
        settings.tax_manpower_modifier,
        TaxManpowerModifier::Historical
    );

    let query = Query::from_save(save);
    let province_owners = query.province_owners();
    let nation_events = query.nation_events(&province_owners);
    let histories = query.player_histories(&nation_events);
    assert_eq!(
        query.starting_country(&histories),
        Some("LAN".parse().unwrap())
    );
    assert_eq!(
        query
            .players()
            .iter()
            .map(|x| x.name.as_str())
            .collect::<Vec<_>>(),
        vec!["comagoosie"]
    );
}

#[test]
fn test_inheritance_values() {
    skip_if_no_tokens!();
    let file = utils::request_file("patch132.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let save = file.parse_save(&TOKENS.resolver()).unwrap();
    let query = Query::from_save(save);

    let inherit = query.inherit(&query.save_country(&"WUR".parse().unwrap()).unwrap());

    assert_eq!(inherit.subtotal, 40672);
    assert_eq!(inherit.inheritance_value, 16);
    assert_eq!(inherit.start_t0_year, 1428);
    assert_eq!(inherit.end_t0_year, 1502);
    assert_eq!(inherit.start_t1_year, 1503);
    assert_eq!(inherit.end_t1_year, 1507);
    assert_eq!(inherit.start_t2_year, 1508);
    assert_eq!(inherit.end_t2_year, 1527);
}

#[test]
fn test_roundtrip_melt() {
    skip_if_no_tokens!();
    let file = utils::request_file("kandy2.bin.eu4");
    let file = Eu4File::from_file(file).unwrap();
    let mut out = Cursor::new(Vec::new());
    file.melt(
        MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
        TOKENS.resolver(),
        &mut out,
    )
    .unwrap();

    let file = Eu4File::from_slice(out.get_ref().as_slice()).unwrap();
    let save = file.parse_save(TOKENS.resolver()).unwrap();
    assert_eq!(file.encoding(), Encoding::Text);
    assert_eq!(save.meta.player, "BHA");
}

macro_rules! ironman_test {
    ($name:ident, $fp:expr, $query:expr, $further:expr) => {
        paste! {
            #[test]
            fn [<test_ $name>]() {
                skip_if_no_tokens!();
                let file = utils::request_file($fp);
                let file = Eu4File::from_file(file).unwrap();

                // Ensure that every ironman can be melted with all tokens resolvable.
                // Deserialization will not try and resolve tokens that aren't used. Melting
                // ensures that every token is seen
                let mut out = Cursor::new(Vec::new());
                file.melt(
                    MeltOptions::new().on_failed_resolve(FailedResolveStrategy::Error),
                    &TOKENS.resolver(),
                    &mut out,
                ).unwrap();

                let save = file.parse_save(&TOKENS.resolver()).unwrap();

                assert_eq!(file.encoding(), Encoding::BinaryZip);
                let expected = $query;
                assert_eq!(save.meta.player, expected.player.parse::<CountryTag>().unwrap());
                assert_eq!(save.meta.date, expected.date);

                let version = &save.meta.savegame_version;
                let patch = format!(
                    "{}.{}.{}.{}",
                    version.first,
                    version.second,
                    version.third,
                    version.fourth
                );
                assert_eq!(patch.as_str(), expected.patch);

                let query = Query::from_save(save);
                let province_owners = query.province_owners();
                let nation_events = query.nation_events(&province_owners);
                let histories = query.player_histories(&nation_events);

                assert_eq!(
                    query.starting_country(&histories).unwrap(),
                    expected.starting.parse::<CountryTag>().unwrap(),
                );

                ($further)(query, out.into_inner().as_slice());
            }
        }
    };

    ($name:ident, $fp:expr, $query:expr) => {
        paste! {
            fn [<test_ $name _cb>](_q: Query, _data: &[u8]) {
            }

            ironman_test!($name, $fp, $query,  [<test_ $name _cb>]);
        }
    };
}

struct IronmanQuery {
    starting: &'static str,
    player: &'static str,
    patch: &'static str,
    date: Eu4Date,
}

ironman_test!(
    kandy2,
    "kandy2.bin.eu4",
    IronmanQuery {
        starting: "KND",
        player: "BHA",
        patch: "1.29.5.0",
        date: Eu4Date::parse("1804.12.09").unwrap()
    }
);

ironman_test!(
    tryone,
    "tryone.eu4",
    IronmanQuery {
        starting: "TYR",
        player: "TYR",
        patch: "1.30.3.0",
        date: Eu4Date::parse("1581.03.01").unwrap()
    }
);

ironman_test!(
    modded,
    "modded.eu4",
    IronmanQuery {
        starting: "MOS",
        player: "MOS",
        patch: "1.30.4.0",
        date: Eu4Date::parse("1446.03.16").unwrap()
    }
);

fn trycone_expected_histories() -> Vec<PlayerHistory> {
    vec![PlayerHistory {
        history: NationEvents {
            initial: "TYR".parse().unwrap(),
            latest: "GBR".parse().unwrap(),
            stored: "GBR".parse().unwrap(),
            events: vec![
                NationEvent {
                    date: Eu4Date::from_ymd(1518, 1, 29),
                    kind: NationEventKind::TagSwitch("IRE".parse().unwrap()),
                },
                NationEvent {
                    date: Eu4Date::from_ymd(1606, 8, 4),
                    kind: NationEventKind::TagSwitch("GBR".parse().unwrap()),
                },
            ],
        },
        is_human: true,
        player_names: vec![String::from("comagoosie")],
    }]
}

fn leinster_history() -> NationEvents {
    NationEvents {
        initial: "LEI".parse().unwrap(),
        latest: "LEI".parse().unwrap(),
        stored: "LEI".parse().unwrap(),
        events: vec![
            NationEvent {
                date: Eu4Date::from_ymd(1451, 8, 2),
                kind: NationEventKind::Annexed,
            },
            NationEvent {
                date: Eu4Date::from_ymd(1588, 6, 15),
                kind: NationEventKind::Appeared,
            },
            NationEvent {
                date: Eu4Date::from_ymd(1605, 2, 15),
                kind: NationEventKind::Annexed,
            },
            NationEvent {
                date: Eu4Date::from_ymd(1716, 2, 9),
                kind: NationEventKind::Appeared,
            },
        ],
    }
}

ironman_test!(
    trycone,
    "trycone.eu4",
    IronmanQuery {
        starting: "TYR",
        player: "GBR",
        patch: "1.30.4.0",
        date: Eu4Date::parse("1725.05.12").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let province_owners = query.province_owners();
        let nation_events = query.nation_events(&province_owners);
        let lei = "LEI".parse().unwrap();
        let lei_events = nation_events.iter().find(|x| x.initial == lei).unwrap();
        assert_eq!(lei_events, &leinster_history());
        let histories = query.player_histories(&nation_events);
        assert_eq!(&histories, &trycone_expected_histories());
        let tag_resolver = query.tag_resolver(&nation_events);
        assert_eq!(
            tag_resolver
                .resolve("IRE".parse().unwrap(), Eu4Date::from_ymd(1529, 3, 1))
                .unwrap()
                .stored,
            "GBR"
        );

        let lei_income = query.income_statistics_ledger(lei_events);
        assert_eq!(
            lei_income,
            vec![
                LedgerPoint {
                    tag: lei,
                    year: 1445,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1446,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1447,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1448,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1449,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1450,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1451,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1589,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1590,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1591,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1592,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1593,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1594,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1595,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1596,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1597,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1598,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1599,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1600,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1601,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1602,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1603,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1604,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1605,
                    value: 1
                },
                LedgerPoint {
                    tag: lei,
                    year: 1717,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1718,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1719,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1720,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1721,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1722,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1723,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1724,
                    value: 3
                },
                LedgerPoint {
                    tag: lei,
                    year: 1725,
                    value: 3
                },
            ]
        );

        let income = query.income_statistics_ledger(&histories[0].history);
        assert_eq!(
            income[0],
            LedgerPoint {
                tag: "TYR".parse().unwrap(),
                year: 1445,
                value: 1
            }
        );
        assert!(income.contains(&LedgerPoint {
            tag: "TYR".parse().unwrap(),
            year: 1518,
            value: 8
        }));
        assert!(income.contains(&LedgerPoint {
            tag: "IRE".parse().unwrap(),
            year: 1519,
            value: 9
        }));
        assert!(income.contains(&LedgerPoint {
            tag: "IRE".parse().unwrap(),
            year: 1606,
            value: 70
        }));
        assert!(income.contains(&LedgerPoint {
            tag: "GBR".parse().unwrap(),
            year: 1607,
            value: 69
        }));
        assert_eq!(
            income.last().unwrap(),
            &LedgerPoint {
                tag: "GBR".parse().unwrap(),
                year: 1725,
                value: 717
            }
        );
    }
);

fn true_heir_expected_histories() -> Vec<PlayerHistory> {
    vec![PlayerHistory {
        history: NationEvents {
            initial: "SIS".parse().unwrap(),
            latest: "MUG".parse().unwrap(),
            stored: "MUG".parse().unwrap(),
            events: vec![
                NationEvent {
                    date: Eu4Date::from_ymd(1467, 12, 3),
                    kind: NationEventKind::TagSwitch("DLH".parse().unwrap()),
                },
                NationEvent {
                    date: Eu4Date::from_ymd(1467, 12, 3),
                    kind: NationEventKind::TagSwitch("MUG".parse().unwrap()),
                },
            ],
        },
        is_human: true,
        player_names: vec![String::from("lambdax.x")],
    }]
}

ironman_test!(
    true_heir_of_timur,
    "sis.eu4",
    IronmanQuery {
        starting: "SIS",
        player: "MUG",
        patch: "1.30.3.0",
        date: Eu4Date::parse("1508.04.27").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let province_owners = query.province_owners();
        let nation_events = query.nation_events(&province_owners);
        let histories = query.player_histories(&nation_events);
        assert_eq!(histories, true_heir_expected_histories());
    }
);

ironman_test!(
    revolution_center_cologne,
    "cologne2.eu4",
    IronmanQuery {
        starting: "KOL",
        player: "KOL",
        patch: "1.30.3.0",
        date: Eu4Date::parse("1821.01.03").unwrap()
    }
);

ironman_test!(
    ita2,
    "ita2_later13.eu4",
    IronmanQuery {
        starting: "MLO",
        player: "ITA",
        patch: "1.29.6.0",
        date: Eu4Date::parse("1547.03.05").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        assert_eq!(
            query
                .players()
                .iter()
                .map(|x| x.name.as_str())
                .collect::<Vec<_>>(),
            vec!["comagoosie"]
        );
    }
);

ironman_test!(
    burg,
    "burg.eu4",
    IronmanQuery {
        starting: "BUR",
        player: "BUR",
        patch: "1.30.4.0",
        date: Eu4Date::parse("1821.01.03").unwrap()
    }
);

ironman_test!(
    no_dlc,
    "no-dlc.eu4",
    IronmanQuery {
        starting: "BYZ",
        player: "BYZ",
        patch: "1.30.4.0",
        date: Eu4Date::parse("1513.05.25").unwrap()
    }
);

ironman_test!(
    chinese_supplementary,
    "chinese-supplementary.eu4",
    IronmanQuery {
        starting: "SZO",
        player: "SZO",
        patch: "1.30.4.0",
        date: Eu4Date::parse("1800.01.01").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        assert_eq!(query.save().meta.displayed_country_name, "萨卢佐");
    }
);

fn cilli_history() -> NationEvents {
    NationEvents {
        initial: "CLI".parse().unwrap(),
        latest: "CRO".parse().unwrap(),
        stored: "CRO".parse().unwrap(),
        events: vec![NationEvent {
            date: Eu4Date::from_ymd(1509, 6, 6),
            kind: NationEventKind::TagSwitch("CRO".parse().unwrap()),
        }],
    }
}

ironman_test!(
    dont_be,
    "dont_be.eu4",
    IronmanQuery {
        starting: "CLI",
        player: "CRO",
        patch: "1.30.1.0",
        date: Eu4Date::parse("1509.06.10").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let province_owners = query.province_owners();
        let nation_events = query.nation_events(&province_owners);
        let histories = query.player_histories(&nation_events);
        assert_eq!(&histories[0].history, &cilli_history());
        let cli = "CLI".parse().unwrap();

        let my_score = query.score_statistics_ledger(&histories[0].history);
        let mut expected_score = Vec::new();
        for i in 1445..1510 {
            expected_score.push(LedgerPoint {
                tag: cli,
                year: i,
                value: 0,
            });
        }
        assert_eq!(my_score, expected_score);

        let my_inflation = query.inflation_statistics_ledger(&histories[0].history);
        let mut expected_inflation = Vec::new();
        for i in 1445..1479 {
            expected_inflation.push(LedgerPoint {
                tag: cli,
                year: i,
                value: 0,
            });
        }

        #[rustfmt::skip]
        expected_inflation.extend_from_slice(&[
            LedgerPoint { tag: cli, year: 1479, value: 1 },
            LedgerPoint { tag: cli, year: 1480, value: 1 },
            LedgerPoint { tag: cli, year: 1481, value: 1 },
            LedgerPoint { tag: cli, year: 1482, value: 1 },
            LedgerPoint { tag: cli, year: 1483, value: 1 },
            LedgerPoint { tag: cli, year: 1484, value: 1 },
            LedgerPoint { tag: cli, year: 1485, value: 1 },
            LedgerPoint { tag: cli, year: 1486, value: 1 },
            LedgerPoint { tag: cli, year: 1487, value: 1 },
            LedgerPoint { tag: cli, year: 1488, value: 1 },
            LedgerPoint { tag: cli, year: 1489, value: 2 },
            LedgerPoint { tag: cli, year: 1490, value: 2 },
            LedgerPoint { tag: cli, year: 1491, value: 2 },
            LedgerPoint { tag: cli, year: 1492, value: 2 },
            LedgerPoint { tag: cli, year: 1493, value: 2 },
            LedgerPoint { tag: cli, year: 1494, value: 2 },
            LedgerPoint { tag: cli, year: 1495, value: 2 },
            LedgerPoint { tag: cli, year: 1496, value: 2 },
            LedgerPoint { tag: cli, year: 1497, value: 3 },
            LedgerPoint { tag: cli, year: 1498, value: 3 },
            LedgerPoint { tag: cli, year: 1499, value: 3 },
            LedgerPoint { tag: cli, year: 1500, value: 3 },
            LedgerPoint { tag: cli, year: 1501, value: 3 },
            LedgerPoint { tag: cli, year: 1502, value: 3 },
            LedgerPoint { tag: cli, year: 1503, value: 3 },
            LedgerPoint { tag: cli, year: 1504, value: 3 },
            LedgerPoint { tag: cli, year: 1505, value: 3 },
            LedgerPoint { tag: cli, year: 1506, value: 3 },
            LedgerPoint { tag: cli, year: 1507, value: 3 },
            LedgerPoint { tag: cli, year: 1508, value: 3 },
            LedgerPoint { tag: cli, year: 1509, value: 3 },
        ]);
        assert_eq!(my_inflation, expected_inflation);

        let my_income = query.income_statistics_ledger(&histories[0].history);

        #[rustfmt::skip]
        let expected_income = vec![
            LedgerPoint { tag: cli, year: 1445, value: 1 },
            LedgerPoint { tag: cli, year: 1446, value: 1 },
            LedgerPoint { tag: cli, year: 1447, value: 1 },
            LedgerPoint { tag: cli, year: 1448, value: 1 },
            LedgerPoint { tag: cli, year: 1449, value: 1 },
            LedgerPoint { tag: cli, year: 1450, value: 2 },
            LedgerPoint { tag: cli, year: 1451, value: 2 },
            LedgerPoint { tag: cli, year: 1452, value: 3 },
            LedgerPoint { tag: cli, year: 1453, value: 3 },
            LedgerPoint { tag: cli, year: 1454, value: 3 },
            LedgerPoint { tag: cli, year: 1455, value: 3 },
            LedgerPoint { tag: cli, year: 1456, value: 5 },
            LedgerPoint { tag: cli, year: 1457, value: 5 },
            LedgerPoint { tag: cli, year: 1458, value: 5 },
            LedgerPoint { tag: cli, year: 1459, value: 5 },
            LedgerPoint { tag: cli, year: 1460, value: 4 },
            LedgerPoint { tag: cli, year: 1461, value: 5 },
            LedgerPoint { tag: cli, year: 1462, value: 5 },
            LedgerPoint { tag: cli, year: 1463, value: 5 },
            LedgerPoint { tag: cli, year: 1464, value: 5 },
            LedgerPoint { tag: cli, year: 1465, value: 4 },
            LedgerPoint { tag: cli, year: 1466, value: 4 },
            LedgerPoint { tag: cli, year: 1467, value: 4 },
            LedgerPoint { tag: cli, year: 1468, value: 4 },
            LedgerPoint { tag: cli, year: 1469, value: 4 },
            LedgerPoint { tag: cli, year: 1470, value: 4 },
            LedgerPoint { tag: cli, year: 1471, value: 4 },
            LedgerPoint { tag: cli, year: 1472, value: 4 },
            LedgerPoint { tag: cli, year: 1473, value: 4 },
            LedgerPoint { tag: cli, year: 1474, value: 4 },
            LedgerPoint { tag: cli, year: 1475, value: 4 },
            LedgerPoint { tag: cli, year: 1476, value: 4 },
            LedgerPoint { tag: cli, year: 1477, value: 4 },
            LedgerPoint { tag: cli, year: 1478, value: 4 },
            LedgerPoint { tag: cli, year: 1479, value: 4 },
            LedgerPoint { tag: cli, year: 1480, value: 5 },
            LedgerPoint { tag: cli, year: 1481, value: 5 },
            LedgerPoint { tag: cli, year: 1482, value: 5 },
            LedgerPoint { tag: cli, year: 1483, value: 5 },
            LedgerPoint { tag: cli, year: 1484, value: 5 },
            LedgerPoint { tag: cli, year: 1485, value: 5 },
            LedgerPoint { tag: cli, year: 1486, value: 5 },
            LedgerPoint { tag: cli, year: 1487, value: 5 },
            LedgerPoint { tag: cli, year: 1488, value: 5 },
            LedgerPoint { tag: cli, year: 1489, value: 5 },
            LedgerPoint { tag: cli, year: 1490, value: 5 },
            LedgerPoint { tag: cli, year: 1491, value: 5 },
            LedgerPoint { tag: cli, year: 1492, value: 5 },
            LedgerPoint { tag: cli, year: 1493, value: 5 },
            LedgerPoint { tag: cli, year: 1494, value: 5 },
            LedgerPoint { tag: cli, year: 1495, value: 6 },
            LedgerPoint { tag: cli, year: 1496, value: 6 },
            LedgerPoint { tag: cli, year: 1497, value: 5 },
            LedgerPoint { tag: cli, year: 1498, value: 6 },
            LedgerPoint { tag: cli, year: 1499, value: 5 },
            LedgerPoint { tag: cli, year: 1500, value: 5 },
            LedgerPoint { tag: cli, year: 1501, value: 5 },
            LedgerPoint { tag: cli, year: 1502, value: 6 },
            LedgerPoint { tag: cli, year: 1503, value: 6 },
            LedgerPoint { tag: cli, year: 1504, value: 6 },
            LedgerPoint { tag: cli, year: 1505, value: 6 },
            LedgerPoint { tag: cli, year: 1506, value: 6 },
            LedgerPoint { tag: cli, year: 1507, value: 6 },
            LedgerPoint { tag: cli, year: 1508, value: 6 },
            LedgerPoint { tag: cli, year: 1509, value: 6 },
        ];

        assert_eq!(my_income, expected_income);
    }
);

ironman_test!(
    non_ironman_binary,
    "non-ironman-binary.eu4",
    IronmanQuery {
        starting: "CAS",
        player: "CAS",
        patch: "1.30.6.0",
        date: Eu4Date::parse("1505.11.25").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        assert!(!query.save().meta.is_ironman);
    }
);

ironman_test!(
    patch_131,
    "1.31.0.eu4",
    IronmanQuery {
        starting: "ENG",
        player: "ENG",
        patch: "1.31.0.0",
        date: Eu4Date::parse("1444.11.11").unwrap()
    },
    |_query: Query, melted_data: &[u8]| {
        // Find inukshuk
        memchr::memmem::find(melted_data, b"date_built=-2000.1.1").unwrap();
        memchr::memmem::find(melted_data, b"navy_strength=0.00000").unwrap();
    }
);

ironman_test!(
    nor_lev,
    "nor_lev.eu4",
    IronmanQuery {
        starting: "NOR",
        player: "NOR",
        patch: "1.31.4.0",
        date: Eu4Date::parse("1676.6.7").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let mod1 = &query.save().meta.mods_enabled_names[0];
        assert_eq!(mod1.filename.as_str(), "mod/ugc_2146396184.mod");
        assert_eq!(mod1.name.as_str(), "Bigger Stellaris");
    }
);

ironman_test!(
    tur,
    "tur.eu4",
    IronmanQuery {
        starting: "TUR",
        player: "TUR",
        patch: "1.31.5.2",
        date: Eu4Date::parse("1555.11.2").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let player = query.country(&"TUR".parse().unwrap()).unwrap();
        let m = query.country_mana_breakdown(player);

        #[rustfmt::skip]
        fn assertions(m: CountryManaUsage) {
            assert_eq!(m.adm.buy_idea + m.dip.buy_idea + m.mil.buy_idea, 6458);
            assert_eq!(m.adm.advance_tech + m.dip.advance_tech + m.mil.advance_tech, 12584);
            assert_eq!(m.adm.boost_stab + m.dip.boost_stab + m.mil.boost_stab, 138);
            assert_eq!(m.adm.buy_general + m.dip.buy_general + m.mil.buy_general, 0);
            assert_eq!(m.adm.buy_admiral + m.dip.buy_admiral + m.mil.buy_admiral, 0);
            assert_eq!(m.adm.buy_conq + m.dip.buy_conq + m.mil.buy_conq, 0);
            assert_eq!(m.adm.buy_explorer + m.dip.buy_explorer + m.mil.buy_explorer, 0);
            assert_eq!(m.adm.develop_prov + m.dip.develop_prov + m.mil.develop_prov, 1025);
            assert_eq!(m.adm.force_march + m.dip.force_march + m.mil.force_march, 0);
            assert_eq!(m.adm.assault + m.dip.assault + m.mil.assault, 65);
            assert_eq!(m.adm.seize_colony + m.dip.seize_colony + m.mil.seize_colony, 0);
            assert_eq!(m.adm.burn_colony + m.dip.burn_colony + m.mil.burn_colony, 0);
            assert_eq!(m.adm.attack_natives + m.dip.attack_natives + m.mil.attack_natives, 0);
            assert_eq!(m.adm.scorch_earth + m.dip.scorch_earth + m.mil.scorch_earth, 60);
            assert_eq!(m.adm.demand_non_wargoal_prov + m.dip.demand_non_wargoal_prov + m.mil.demand_non_wargoal_prov, 7551);
            assert_eq!(m.adm.reduce_inflation + m.dip.reduce_inflation + m.mil.reduce_inflation, 420);
            assert_eq!(m.adm.move_capital + m.dip.move_capital + m.mil.move_capital, 577);
            assert_eq!(m.adm.make_province_core + m.dip.make_province_core + m.mil.make_province_core, 13654);
            assert_eq!(m.adm.replace_rival + m.dip.replace_rival + m.mil.replace_rival, 0);
            assert_eq!(m.adm.change_gov + m.dip.change_gov + m.mil.change_gov, 0);
            assert_eq!(m.adm.change_culture + m.dip.change_culture + m.mil.change_culture, 0);
            assert_eq!(m.adm.harsh_treatment + m.dip.harsh_treatment + m.mil.harsh_treatment, 381);
            assert_eq!(m.adm.reduce_we + m.dip.reduce_we + m.mil.reduce_we, 502);
            assert_eq!(m.adm.boost_faction + m.dip.boost_faction + m.mil.boost_faction, 0);
            assert_eq!(m.adm.raise_war_taxes + m.dip.raise_war_taxes + m.mil.raise_war_taxes, 0);
            assert_eq!(m.adm.increse_tariffs + m.dip.increse_tariffs + m.mil.increse_tariffs, 0);
            assert_eq!(m.adm.promote_merc + m.dip.promote_merc + m.mil.promote_merc, 0);
            assert_eq!(m.adm.decrease_tariffs + m.dip.decrease_tariffs + m.mil.decrease_tariffs, 0);
            assert_eq!(m.adm.move_trade_port + m.dip.move_trade_port + m.mil.move_trade_port, 190);
            assert_eq!(m.adm.create_trade_post + m.dip.create_trade_post + m.mil.create_trade_post, 0);
            assert_eq!(m.adm.siege_sorties + m.dip.siege_sorties + m.mil.siege_sorties, 45);
            assert_eq!(m.adm.buy_religious_reform + m.dip.buy_religious_reform + m.mil.buy_religious_reform, 0);
            assert_eq!(m.adm.set_primary_culture + m.dip.set_primary_culture + m.mil.set_primary_culture, 0);
            assert_eq!(m.adm.add_accepted_culture + m.dip.add_accepted_culture + m.mil.add_accepted_culture, 93);
            assert_eq!(m.adm.remove_accepted_culture + m.dip.remove_accepted_culture + m.mil.remove_accepted_culture, 0);
            assert_eq!(m.adm.strengthen_government + m.dip.strengthen_government + m.mil.strengthen_government, 0);
            assert_eq!(m.adm.boost_militarization + m.dip.boost_militarization + m.mil.boost_militarization, 0);
            assert_eq!(m.adm.artillery_barrage + m.dip.artillery_barrage + m.mil.artillery_barrage, 862);
            assert_eq!(m.adm.establish_siberian_frontier + m.dip.establish_siberian_frontier + m.mil.establish_siberian_frontier, 0);
            assert_eq!(m.adm.government_interaction + m.dip.government_interaction + m.mil.government_interaction, 0);
            assert_eq!(m.adm.naval_barrage + m.dip.naval_barrage + m.mil.naval_barrage, 381);
            assert_eq!(m.adm.add_tribal_land + m.dip.add_tribal_land + m.mil.add_tribal_land, 0);
            assert_eq!(m.adm.force_march + m.dip.force_march + m.mil.force_march, 0);
            assert_eq!(m.adm.create_leader + m.dip.create_leader + m.mil.create_leader, 6977);
            assert_eq!(m.adm.enforce_culture + m.dip.enforce_culture + m.mil.enforce_culture, 0);
            assert_eq!(m.adm.effect + m.dip.effect + m.mil.effect, 0);
            assert_eq!(m.adm.minority_expulsion + m.dip.minority_expulsion + m.mil.minority_expulsion, 0);
            assert_eq!(m.adm.other + m.dip.other + m.mil.other, 0);
        }

        assertions(m)
    }
);

ironman_test!(
    bottom_achievy,
    "BottomAchievy.eu4",
    IronmanQuery {
        starting: "NOV",
        player: "RUS",
        patch: "1.35.6.0",
        date: Eu4Date::parse("1603.9.30").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        let player = query.country(&"RUS".parse().unwrap()).unwrap();
        let m = query.country_mana_breakdown(player);

        #[rustfmt::skip]
        fn assertions(m: CountryManaUsage) {
            assert_eq!((m.adm.buy_idea, m.dip.buy_idea, m.mil.buy_idea), (2572, 4808, 2445));
            assert_eq!((m.adm.advance_tech, m.dip.advance_tech, m.mil.advance_tech), (5306, 4681, 7296));
            assert_eq!((m.adm.boost_stab, m.dip.boost_stab, m.mil.boost_stab), (933, 0, 0));
            assert_eq!((m.adm.buy_general, m.dip.buy_general, m.mil.buy_general), (0, 0, 0));
            assert_eq!((m.adm.buy_admiral, m.dip.buy_admiral, m.mil.buy_admiral), (0, 0, 0));
            assert_eq!((m.adm.buy_conq, m.dip.buy_conq, m.mil.buy_conq), (0, 0, 0));
            assert_eq!((m.adm.buy_explorer, m.dip.buy_explorer, m.mil.buy_explorer), (0, 0, 0));
            assert_eq!((m.adm.develop_prov, m.dip.develop_prov, m.mil.develop_prov), (8065, 10804, 8645));
            assert_eq!((m.adm.force_march, m.dip.force_march, m.mil.force_march), (0, 0, 0));
            assert_eq!((m.adm.assault, m.dip.assault, m.mil.assault), (0, 0, 16));
            assert_eq!((m.adm.seize_colony, m.dip.seize_colony, m.mil.seize_colony), (0, 0, 0));
            assert_eq!((m.adm.burn_colony, m.dip.burn_colony, m.mil.burn_colony), (0, 0, 0));
            assert_eq!((m.adm.attack_natives, m.dip.attack_natives, m.mil.attack_natives), (0, 0, 0));
            assert_eq!((m.adm.scorch_earth, m.dip.scorch_earth, m.mil.scorch_earth), (0, 0, 32));
            assert_eq!((m.adm.demand_non_wargoal_prov, m.dip.demand_non_wargoal_prov, m.mil.demand_non_wargoal_prov), (0, 219, 0));
            assert_eq!((m.adm.reduce_inflation, m.dip.reduce_inflation, m.mil.reduce_inflation), (347, 0, 0));
            assert_eq!((m.adm.move_capital, m.dip.move_capital, m.mil.move_capital), (0, 0, 0));
            assert_eq!((m.adm.make_province_core, m.dip.make_province_core, m.mil.make_province_core), (3845, 0, 0));
            assert_eq!((m.adm.replace_rival, m.dip.replace_rival, m.mil.replace_rival), (0, 94, 0));
            assert_eq!((m.adm.change_gov, m.dip.change_gov, m.mil.change_gov), (0, 0, 0));
            assert_eq!((m.adm.change_culture, m.dip.change_culture, m.mil.change_culture), (0, 98, 0));
            assert_eq!((m.adm.harsh_treatment, m.dip.harsh_treatment, m.mil.harsh_treatment), (0, 0, 285));
            assert_eq!((m.adm.reduce_we, m.dip.reduce_we, m.mil.reduce_we), (0, 0, 0));
            assert_eq!((m.adm.boost_faction, m.dip.boost_faction, m.mil.boost_faction), (123, 117, 132));
            assert_eq!((m.adm.raise_war_taxes, m.dip.raise_war_taxes, m.mil.raise_war_taxes), (0, 0, 0));
            assert_eq!((m.adm.increse_tariffs, m.dip.increse_tariffs, m.mil.increse_tariffs), (0, 0, 0));
            assert_eq!((m.adm.promote_merc, m.dip.promote_merc, m.mil.promote_merc), (0, 0, 0));
            assert_eq!((m.adm.decrease_tariffs, m.dip.decrease_tariffs, m.mil.decrease_tariffs), (0, 0, 0));
            assert_eq!((m.adm.move_trade_port, m.dip.move_trade_port, m.mil.move_trade_port), (0, 0, 0));
            assert_eq!((m.adm.create_trade_post, m.dip.create_trade_post, m.mil.create_trade_post), (0, 0, 0));
            assert_eq!((m.adm.siege_sorties, m.dip.siege_sorties, m.mil.siege_sorties), (0, 0, 36));
            assert_eq!((m.adm.buy_religious_reform, m.dip.buy_religious_reform, m.mil.buy_religious_reform), (0, 0, 0));
            assert_eq!((m.adm.set_primary_culture, m.dip.set_primary_culture, m.mil.set_primary_culture), (0, 0, 0));
            assert_eq!((m.adm.add_accepted_culture, m.dip.add_accepted_culture, m.mil.add_accepted_culture), (0, 182, 0));
            assert_eq!((m.adm.remove_accepted_culture, m.dip.remove_accepted_culture, m.mil.remove_accepted_culture), (0, 0, 0));
            assert_eq!((m.adm.strengthen_government, m.dip.strengthen_government, m.mil.strengthen_government), (0, 0, 902));
            assert_eq!((m.adm.artillery_barrage, m.dip.artillery_barrage, m.mil.artillery_barrage), (0, 0, 45));
            assert_eq!((m.adm.establish_siberian_frontier, m.dip.establish_siberian_frontier, m.mil.establish_siberian_frontier), (0, 803, 0));
            assert_eq!((m.adm.naval_barrage, m.dip.naval_barrage, m.mil.naval_barrage), (0, 0, 0));
            assert_eq!((m.adm.add_tribal_land, m.dip.add_tribal_land, m.mil.add_tribal_land), (0, 0, 0));
            assert_eq!((m.adm.force_march, m.dip.force_march, m.mil.force_march), (0, 0, 0));
            assert_eq!((m.adm.create_leader, m.dip.create_leader, m.mil.create_leader), (0, 45, 1343));
            assert_eq!((m.adm.enforce_culture, m.dip.enforce_culture, m.mil.enforce_culture), (0, 0, 0));
            assert_eq!((m.adm.effect, m.dip.effect, m.mil.effect), (0, 0, 0));
            assert_eq!((m.adm.minority_expulsion, m.dip.minority_expulsion, m.mil.minority_expulsion), (0, 0, 0));
            assert_eq!((m.adm.other, m.dip.other, m.mil.other), (0, -329, 0));
        }

        assertions(m)
    }
);

ironman_test!(
    random_new_world,
    "rnw.eu4",
    IronmanQuery {
        starting: "QOM",
        player: "PER",
        patch: "1.35.6.0",
        date: Eu4Date::parse("1565.8.1").unwrap()
    },
    |query: Query, _melted_data: &[u8]| {
        assert_eq!(query.save().game.random_world, Some(381160688));
    }
);
