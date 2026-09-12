use crate::{
    query::{NationEventKind, NationEvents},
    CountryTag, Eu4Date,
};
use std::collections::HashMap;

/// A tag value and where it is stored in the save
#[derive(Debug, Clone, Copy)]
pub struct TagData {
    /// The tag at the configured resolver's date
    pub current: CountryTag,

    /// Where the `current` tag is stored in the save
    pub stored: CountryTag,
}

#[derive(Debug, Default)]
struct TagHistory {
    initial: Option<NationEvents>,
    timeline: Vec<(Eu4Date, NationEvents)>,
}

/// Tracks tag progression throughput history
///
/// The tag resolver is important to answering questions like "where are they
/// now?" when looking at historic events like province changes or wars. When
/// countries tag switch the tag resolver is able to connect historic events
/// to country's current selves. For instance in a TYR -> IRE -> GBR game, the
/// provinces and wars gained while playing as TYR should still be aggregated
/// under the current GBR tag.
#[derive(Debug)]
pub struct TagResolver {
    switches: HashMap<CountryTag, TagHistory>,
}

impl TagResolver {
    pub fn create(nation_events: &[NationEvents]) -> Self {
        let mut changes: HashMap<_, TagHistory> = HashMap::new();
        for nation in nation_events {
            let initial = changes.entry(nation.initial).or_default();
            initial.initial = Some(nation.clone());

            for event in nation.events.iter() {
                if let NationEventKind::TagSwitch(to) = event.kind {
                    let v = changes.entry(to).or_default();
                    v.timeline.push((event.date, nation.clone()));
                }
            }
        }

        for history in changes.values_mut() {
            history.timeline.sort_by_key(|(date, _)| *date);
        }

        TagResolver { switches: changes }
    }

    fn resolve_nation(&self, tag: CountryTag, date: Eu4Date) -> Option<&NationEvents> {
        self.switches.get(&tag).and_then(|x| {
            x.timeline
                .iter()
                .take_while(|(change_date, _)| *change_date <= date)
                .last()
                .map(|(_, nation)| nation)
                .or(x.initial.as_ref())
        })
    }

    /// Given a date and tag associated with the date, return the current tag where
    /// the argument is stored.
    pub fn resolve(&self, tag: CountryTag, date: Eu4Date) -> Option<TagData> {
        self.resolve_nation(tag, date).map(|x| TagData {
            current: x.latest,
            stored: x.stored,
        })
    }

    fn initial_nation(&self, tag: CountryTag) -> Option<&NationEvents> {
        self.switches.get(&tag).and_then(|x| x.initial.as_ref())
    }

    /// Return where the country initial's known as the given tag is now
    pub fn initial(&self, tag: CountryTag) -> Option<TagData> {
        self.initial_nation(tag).map(|x| TagData {
            current: x.latest,
            stored: x.stored,
        })
    }

    /// Create a resolver that only considers nation events priot to the given
    /// date.
    pub fn at(&self, date: Eu4Date) -> TagResolverDated<'_> {
        TagResolverDated { inner: self, date }
    }
}

/// Resolves tag up until a certain point in time
///
/// Created through [TagResolver::at](crate::TagResolver::at).
///
/// Useful to answer questions like:
///
/// - Given a save in the 1700
/// - Given that a province came under ENG rule in 1500
/// - What tag currently owns that province in 1600, as ENG may have tag
///   switched in the meantime.
#[derive(Debug)]
pub struct TagResolverDated<'a> {
    inner: &'a TagResolver,
    date: Eu4Date,
}

impl TagResolverDated<'_> {
    fn filter_events(&self, nation: &NationEvents) -> Option<(CountryTag, CountryTag)> {
        nation
            .events
            .iter()
            .take_while(|e| e.date <= self.date)
            .filter_map(|x| x.as_tag_switch())
            .last()
            .map(|(_date, tag)| (tag, nation.stored))
    }

    /// See [TagResolver::resolve](crate::TagResolver::resolve)
    pub fn resolve(&self, tag: CountryTag, date: Eu4Date) -> Option<TagData> {
        self.inner.resolve_nation(tag, date).map(|x| {
            self.filter_events(x)
                .map(|(current, stored)| TagData { current, stored })
                .unwrap_or_else(|| TagData {
                    current: x.initial,
                    stored: x.stored,
                })
        })
    }

    /// See [TagResolver::initial](crate::TagResolver::initial)
    pub fn initial(&self, tag: CountryTag) -> Option<TagData> {
        self.inner
            .initial_nation(tag)
            .and_then(|x| self.filter_events(x))
            .map(|(current, stored)| TagData { current, stored })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::query::NationEvent;

    #[test]
    pub fn tag_resolver_1() {
        let data = vec![NationEvents {
            initial: "OIR".parse().unwrap(),
            latest: "HLR".parse().unwrap(),
            stored: "HLR".parse().unwrap(),
            events: vec![
                NationEvent {
                    date: "1456.11.16".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("MCH".parse().unwrap()),
                },
                NationEvent {
                    date: "1460.5.25".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("TIM".parse().unwrap()),
                },
                NationEvent {
                    date: "1463.1.21".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("GLH".parse().unwrap()),
                },
                NationEvent {
                    date: "1463.6.7".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("SIA".parse().unwrap()),
                },
                NationEvent {
                    date: "1463.6.17".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("BAV".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.3.4".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("KOJ".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.8.12".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("POL".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.8.12".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("ENG".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.8.12".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("MOR".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.9.20".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("SCO".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.9.20".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("TUN".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.10.1".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("DLH".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.10.1".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("DAI".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.10.1".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("YUA".parse().unwrap()),
                },
                NationEvent {
                    date: "1467.10.1".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("MGE".parse().unwrap()),
                },
                NationEvent {
                    date: "1468.12.12".parse().unwrap(),
                    kind: NationEventKind::TagSwitch("HLR".parse().unwrap()),
                },
            ],
        }];

        let oirat: CountryTag = "OIR".parse().unwrap();
        let manchu: CountryTag = "MCH".parse().unwrap();
        let mongol: CountryTag = "MGE".parse().unwrap();
        let hre: CountryTag = "HLR".parse().unwrap();

        let resolver = TagResolver::create(&data);

        // Where is the player starting as oirat now?
        let x = resolver.initial(oirat).unwrap();
        assert_eq!(x.current, hre);
        assert_eq!(x.stored, hre);

        // Where is the player who was oirat in 1445
        let x = resolver
            .resolve(oirat, "1445.08.03".parse().unwrap())
            .unwrap();
        assert_eq!(x.current, hre);
        assert_eq!(x.stored, hre);

        // Where is the player who was manchu in 1458 now?
        let x = resolver
            .resolve(manchu, "1458.12.04".parse().unwrap())
            .unwrap();
        assert_eq!(x.current, hre);
        assert_eq!(x.stored, hre);

        let x = resolver
            .resolve("TUN".parse().unwrap(), "1467.9.20".parse().unwrap())
            .unwrap();
        assert_eq!(x.current, hre);
        assert_eq!(x.stored, hre);

        // at the date of 1458-12-04
        let dated = resolver.at("1458.12.04".parse().unwrap());

        // Olgii (4681) starts with OIR and never changes hands
        let olgii = dated.initial(oirat).unwrap();
        assert_eq!(olgii.current, manchu);
        assert_eq!(olgii.stored, hre);

        // OIR conquers Almaty (461) in 1445-08-03 which has tagged switched
        // into manchu by this time
        let almaty = dated.resolve(oirat, "1445.08.03".parse().unwrap()).unwrap();
        assert_eq!(almaty.current, manchu);
        assert_eq!(almaty.stored, hre);

        // MCH conquers Yongping (4194) in 1458-04-26
        let yongping = dated
            .resolve(manchu, "1458.12.04".parse().unwrap())
            .unwrap();
        assert_eq!(yongping.current, manchu);
        assert_eq!(yongping.stored, hre);

        // Then after several same-day tag switches, we should make sure we keep the latest
        let dated = resolver.at("1468.1.1".parse().unwrap());
        let x = dated.initial(oirat).unwrap();
        assert_eq!(x.current, mongol);
        assert_eq!(x.stored, hre);

        // and the latest should take effect on the day of the switch
        let dated = resolver.at("1467.10.1".parse().unwrap());
        let x = dated.initial(oirat).unwrap();
        assert_eq!(x.current, mongol);
        assert_eq!(x.stored, hre);

        let x = dated
            .resolve("TUN".parse().unwrap(), "1467.9.20".parse().unwrap())
            .unwrap();
        assert_eq!(x.current, mongol);
        assert_eq!(x.stored, hre);

        // provinces conquered before tag switch should resolve
        let almaty = resolver
            .resolve(oirat, "1445.08.03".parse().unwrap())
            .unwrap();
        assert_eq!(almaty.current, hre);
        assert_eq!(almaty.stored, hre);

        let dated = resolver.at("1447.03.11".parse().unwrap());
        let almaty = dated.resolve(oirat, "1445.08.03".parse().unwrap()).unwrap();
        assert_eq!(almaty.current, oirat);
        assert_eq!(almaty.stored, hre);
    }
}
