pub mod criterion_benches {
    use criterion::{Criterion, Throughput};
    use std::{
        collections::HashMap,
        hash::{BuildHasherDefault, Hasher},
    };

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct FnvHasher(u64);

    impl Default for FnvHasher {
        #[inline]
        fn default() -> FnvHasher {
            FnvHasher(0xcbf29ce484222325)
        }
    }

    impl Hasher for FnvHasher {
        #[inline]
        fn finish(&self) -> u64 {
            self.0
        }

        #[inline]
        fn write(&mut self, bytes: &[u8]) {
            let FnvHasher(mut hash) = *self;

            for byte in bytes {
                hash ^= *byte as u64;
                hash = hash.wrapping_mul(0x100000001b3);
            }

            *self = FnvHasher(hash);
        }
    }

    pub fn country_tag_hashing(c: &mut Criterion) {
        let mut group = c.benchmark_group("country_tag_hashing");

        let data = std::fs::read("../assets/saves/mp_Uesugi.eu4").unwrap();
        let file_data = std::fs::read("../assets/eu4.txt").unwrap_or_default();
        let segments = eu4save::SegmentedResolver::parse(file_data.as_slice()).unwrap();
        let file = eu4save::Eu4File::from_slice(&data).unwrap();
        let save = file.parse_save(segments.resolver()).unwrap();
        let country_tags = save
            .game
            .countries
            .iter()
            .map(|(tag, _)| *tag)
            .collect::<Vec<_>>();

        group.throughput(Throughput::Elements(country_tags.len() as u64));
        group.bench_function("default-hash", |b| {
            let map = country_tags
                .iter()
                .enumerate()
                .map(|(i, tag)| (*tag, i))
                .collect::<HashMap<_, _>>();
            b.iter(|| {
                let mut sum = 0;
                for tag in &country_tags {
                    sum += map.get(tag).unwrap_or(&0);
                }
                std::hint::black_box(sum)
            })
        });

        type FnvBuildHasher = BuildHasherDefault<FnvHasher>;
        type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;

        group.bench_function("fnv-hash", |b| {
            let map = country_tags
                .iter()
                .enumerate()
                .map(|(i, tag)| (*tag, i))
                .collect::<FnvHashMap<_, _>>();
            b.iter(|| {
                let mut sum = 0;
                for tag in &country_tags {
                    sum += map.get(tag).unwrap_or(&0);
                }
                std::hint::black_box(sum)
            })
        });
    }

    criterion::criterion_group!(country_tag_hashing_benches, country_tag_hashing);
}
