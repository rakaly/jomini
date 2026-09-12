pub mod country_tag_hashing;
pub mod deserialize_ledger;
pub mod parse_save;

/// Read an EU4 save fixture, fetching it into the cache on first use.
pub(super) fn save(name: &str) -> Vec<u8> {
    pdx_fixtures::read("eu4", name)
}
