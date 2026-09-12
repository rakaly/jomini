use std::fs::File;

/// Open a Vic3 save fixture, fetching it into the cache on first use.
pub fn request_file<S: AsRef<str>>(name: S) -> File {
    pdx_fixtures::request_file("vic3", name.as_ref())
}
