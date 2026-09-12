use std::fs::File;

/// Open an EU5 save fixture, fetching it into the cache on first use.
pub fn request_file<S: AsRef<str>>(name: S) -> File {
    pdx_fixtures::request_file("eu5", name.as_ref())
}
