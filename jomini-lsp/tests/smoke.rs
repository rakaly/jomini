//! End-to-end smoke test: drive the real server (via [`jomini_lsp::serve`]) over
//! an in-memory [`Connection`] against the bundled `examples/mod-demo` corpus —
//! the same two-layer vanilla+mod project `examples/lint.rs` uses. No subprocess,
//! no wire framing: the client and server share a process over channel pairs.

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use serde_json::{Value, json};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// A tiny client over the memory connection: sends requests/notifications and
/// reads responses, stashing any server notifications (e.g. publishDiagnostics)
/// seen along the way for later assertions.
struct Client {
    conn: Connection,
    notifications: Vec<Notification>,
    next_id: i32,
}

impl Client {
    fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.conn
            .sender
            .send(Message::Request(Request {
                id: RequestId::from(id),
                method: method.into(),
                params,
            }))
            .unwrap();
        self.read_until_response(RequestId::from(id))
    }

    fn notify(&self, method: &str, params: Value) {
        self.conn
            .sender
            .send(Message::Notification(Notification {
                method: method.into(),
                params,
            }))
            .unwrap();
    }

    fn read_until_response(&mut self, id: RequestId) -> Value {
        loop {
            match self.conn.receiver.recv_timeout(Duration::from_secs(15)) {
                Ok(Message::Response(Response {
                    id: rid,
                    result,
                    error,
                })) if rid == id => {
                    assert!(error.is_none(), "request {id} errored: {error:?}");
                    return result.unwrap_or(Value::Null);
                }
                Ok(Message::Notification(not)) => self.notifications.push(not),
                Ok(_) => {} // a stray response/request — ignore
                Err(e) => panic!("timed out waiting for response to {id}: {e}"),
            }
        }
    }

    fn published_diagnostics(&self) -> Vec<(String, Value)> {
        self.notifications
            .iter()
            .filter(|n| n.method == "textDocument/publishDiagnostics")
            .map(|n| {
                let uri = n.params["uri"].as_str().unwrap_or_default().to_string();
                (uri, n.params["diagnostics"].clone())
            })
            .collect()
    }
}

fn demo_paths() -> (PathBuf, PathBuf) {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let demo = repo.join("examples/mod-demo");
    (demo.join("mod"), demo.join("vanilla"))
}

fn file_uri(path: &Path) -> String {
    url::Url::from_file_path(path).unwrap().to_string()
}

/// The LSP position (line, UTF-16 character) of the first occurrence of `needle`
/// in `text`. Target tokens live on ASCII lines, so UTF-16 = bytes there.
fn pos_of(text: &str, needle: &str) -> Value {
    let off = text
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` not found"));
    let prefix = &text[..off];
    let line = prefix.matches('\n').count();
    let line_start = prefix.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let character: usize = text[line_start..off].chars().map(|c| c.len_utf16()).sum();
    json!({ "line": line, "character": character })
}

#[test]
fn server_serves_the_mod_demo_project() {
    let (mod_dir, vanilla_dir) = demo_paths();
    let (server_conn, client_conn) = Connection::memory();

    // Run the server loop on a background thread; the test drives the client end.
    let handle = std::thread::spawn(move || jomini_lsp::serve(&server_conn));
    let mut client = Client {
        conn: client_conn,
        notifications: Vec::new(),
        next_id: 1,
    };

    // --- initialize: workspace = the mod dir, with the vanilla base as an option.
    let init = client.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": file_uri(&mod_dir),
            "capabilities": {},
            "initializationOptions": { "vanilla": vanilla_dir.to_str().unwrap() }
        }),
    );
    let caps = &init["capabilities"];
    assert_eq!(caps["definitionProvider"], json!(true));
    assert_eq!(caps["referencesProvider"], json!(true));
    assert_eq!(caps["documentSymbolProvider"], json!(true));
    assert!(caps["codeActionProvider"] != Value::Null);
    client.notify("initialized", json!({}));

    let mod_events = mod_dir.join("events/mod_events.txt");
    let mod_buildings = mod_dir.join("common/buildings/01_mod_buildings.txt");
    let vanilla_buildings = vanilla_dir.join("common/buildings/00_buildings.txt");
    let mod_events_text = std::fs::read_to_string(&mod_events).unwrap();
    let mod_buildings_text = std::fs::read_to_string(&mod_buildings).unwrap();

    // --- document symbols: the vanilla buildings file defines four buildings.
    let syms = client.request(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": file_uri(&vanilla_buildings) } }),
    );
    let names: Vec<String> = syms
        .as_array()
        .unwrap()
        .iter()
        .map(|s| s["name"].as_str().unwrap().to_string())
        .collect();
    assert_eq!(
        names,
        ["temple", "castle", "city", "barracks"],
        "document symbols"
    );

    // --- the cross-file diagnostic: `baracks` is undefined (published, by now).
    let diags = client.published_diagnostics();
    let baracks = diags.iter().find_map(|(uri, ds)| {
        ds.as_array()?.iter().find(|d| {
            d["code"] == json!("undefined-reference")
                && d["message"].as_str().unwrap_or("").contains("baracks")
        })?;
        Some(uri.clone())
    });
    let baracks_uri = baracks.expect("an undefined-reference diagnostic for `baracks`");
    assert!(
        baracks_uri.ends_with("mod_events.txt"),
        "in the events file: {baracks_uri}"
    );
    // The malformed-subtree reference is suppressed, not a hard error.
    let has_ghost_error = diags.iter().any(|(_, ds)| {
        ds.as_array().unwrap().iter().any(|d| {
            d["code"] == json!("undefined-reference")
                && d["message"]
                    .as_str()
                    .unwrap_or("")
                    .contains("ghost_building")
        })
    });
    assert!(
        !has_ghost_error,
        "ghost_building must be suppressed, not a hard undefined-reference"
    );

    // --- go-to-definition: `fortress` (used in mod_events) is defined in the mod
    //     buildings file — a cross-file jump.
    let def = client.request(
        "textDocument/definition",
        json!({
            "textDocument": { "uri": file_uri(&mod_events) },
            "position": pos_of(&mod_events_text, "fortress"),
        }),
    );
    let def_uri = def["uri"].as_str().expect("a single definition Location");
    assert!(
        def_uri.ends_with("01_mod_buildings.txt"),
        "jumps to the mod buildings file: {def_uri}"
    );

    // --- find-references: `castle` is referenced in the mod (upgrades_from) and
    //     in vanilla (has_building); with the declaration that is at least three.
    let refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": file_uri(&mod_buildings) },
            "position": pos_of(&mod_buildings_text, "castle"),
            "context": { "includeDeclaration": true },
        }),
    );
    assert!(
        refs.as_array().unwrap().len() >= 3,
        "castle references: {refs}"
    );

    // `temple` is overridden (declared in vanilla AND the mod). With
    // includeDeclaration, references must return EVERY declaration site — not just
    // the winning one — so the shadowed vanilla declaration appears. Vanilla's
    // buildings file contains no *use* of temple, so its presence here proves the
    // all-declarations behavior (use "temple = {" to skip the comment mentions).
    let temple_refs = client.request(
        "textDocument/references",
        json!({
            "textDocument": { "uri": file_uri(&mod_buildings) },
            "position": pos_of(&mod_buildings_text, "temple = {"),
            "context": { "includeDeclaration": true },
        }),
    );
    let temple_uris: Vec<&str> = temple_refs
        .as_array()
        .unwrap()
        .iter()
        .map(|l| l["uri"].as_str().unwrap())
        .collect();
    assert!(
        temple_uris.iter().any(|u| u.ends_with("00_buildings.txt")),
        "the shadowed vanilla temple declaration must be included: {temple_uris:?}"
    );
    assert!(
        temple_uris
            .iter()
            .any(|u| u.ends_with("01_mod_buildings.txt")),
        "the winning mod temple declaration must be included: {temple_uris:?}"
    );

    // --- code action: a quick fix on the `baracks` typo offers `barracks`.
    let start = pos_of(&mod_events_text, "baracks");
    let end = {
        let mut e = start.clone();
        e["character"] = json!(start["character"].as_u64().unwrap() + "baracks".len() as u64);
        e
    };
    let actions = client.request(
        "textDocument/codeAction",
        json!({
            "textDocument": { "uri": file_uri(&mod_events) },
            "range": { "start": start, "end": end },
            "context": { "diagnostics": [] },
        }),
    );
    let has_fix = actions.as_array().unwrap().iter().any(|a| {
        a["title"].as_str().unwrap_or("").contains("barracks")
            && a["edit"]["changes"] != Value::Null
    });
    assert!(has_fix, "a quick fix offering `barracks`: {actions}");

    // --- formatting: returns an edit list (possibly empty) without erroring.
    let fmt = client.request(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": file_uri(&vanilla_buildings) },
            "options": { "tabSize": 4, "insertSpaces": false }
        }),
    );
    assert!(fmt.is_array(), "formatting returns a TextEdit list: {fmt}");

    // --- shutdown / exit: the loop returns and the thread joins cleanly.
    let _ = client.request("shutdown", Value::Null);
    client.notify("exit", Value::Null);
    handle.join().unwrap().expect("server loop exits Ok");
}

#[test]
fn bare_exit_without_shutdown_terminates_the_server() {
    let (mod_dir, vanilla_dir) = demo_paths();
    let (server_conn, client_conn) = Connection::memory();
    let handle = std::thread::spawn(move || jomini_lsp::serve(&server_conn));
    let mut client = Client {
        conn: client_conn,
        notifications: Vec::new(),
        next_id: 1,
    };

    client.request(
        "initialize",
        json!({
            "processId": null,
            "rootUri": file_uri(&mod_dir),
            "capabilities": {},
            "initializationOptions": { "vanilla": vanilla_dir.to_str().unwrap() }
        }),
    );
    client.notify("initialized", json!({}));

    // A bare `exit` (no preceding `shutdown`) is the protocol's abrupt-termination
    // path. The loop must still stop — otherwise a persistent transport hangs —
    // and `serve` reports it as an error (non-zero process exit per the spec).
    client.notify("exit", Value::Null);
    let result = handle.join().expect("server thread did not panic / hang");
    assert!(
        result.is_err(),
        "bare exit terminates the loop with a non-Ok status"
    );
}
