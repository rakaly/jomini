use eu4save::file::Eu4ZipEntry;
use rawzip::{FileReader, ReaderAt, ZipReader};
use std::io::Cursor;

#[test]
fn zip_entry_is_send() {
    fn is_send<T: Send>() {}
    fn entry_of<R: ReaderAt + Sync>() {
        is_send::<Eu4ZipEntry<ZipReader<&R>>>();
    }

    entry_of::<Cursor<&[u8]>>();
    entry_of::<FileReader>();
}
