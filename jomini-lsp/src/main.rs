//! The `jomini-lsp` binary: wire the [`jomini_lsp::serve`] loop to stdio.

use lsp_server::Connection;

fn main() -> Result<(), jomini_lsp::DynError> {
    eprintln!("jomini-lsp: starting (stdio)");
    let (connection, io_threads) = Connection::stdio();
    jomini_lsp::serve(&connection)?;
    io_threads.join()?;
    eprintln!("jomini-lsp: stopped");
    Ok(())
}
