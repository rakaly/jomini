use std::fs::File;

/// Open an Imperator save fixture, fetching it into the cache on first use.
pub fn request_file<S: AsRef<str>>(name: S) -> File {
    pdx_fixtures::request_file("imperator", name.as_ref())
}
