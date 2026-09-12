use std::fs::File;

/// Open an HOI4 save fixture, fetching it into the cache on first use.
pub fn request_file<S: AsRef<str>>(name: S) -> File {
    pdx_fixtures::request_file("hoi4", name.as_ref())
}
