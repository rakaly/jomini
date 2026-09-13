//! `jomini-lsp` — a Language Server Protocol server for Clausewitz (Paradox) game
//! files, built on jomini's lossless syntax tree and cross-file lint layer.
//!
//! A synchronous server on rust-analyzer's [`lsp_server`] stack. The first cut
//! serves: publish-diagnostics, document formatting, go-to-definition,
//! find-references, document & workspace symbols, and quick-fix code actions —
//! all reading off a single retained [`jomini::text::lint::Analysis`].
//!
//! [`serve`] runs the protocol against any [`Connection`] (stdio in the binary, an
//! in-memory pair in tests), so the whole server is driveable without a subprocess.

mod convert;
mod line_index;
mod server;

use line_index::PositionEncoding;
use lsp_server::{Connection, Message, Response, ResponseError};
use lsp_types::notification::Notification as _;
use lsp_types::{
    CodeActionProviderCapability, InitializeParams, InitializeResult, OneOf, PositionEncodingKind,
    ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use server::Server;

pub type DynError = Box<dyn std::error::Error + Sync + Send>;

// JSON-RPC `InvalidParams` (lsp-server 0.7 exposes no error-code enum).
const INVALID_PARAMS: i32 = -32602;

/// Run the full LSP lifecycle over `connection`: the `initialize` handshake (with
/// position-encoding negotiation and capability advertisement) followed by the
/// request/notification dispatch loop, until `shutdown`/`exit`.
pub fn serve(connection: &Connection) -> Result<(), DynError> {
    let (id, init_value) = connection.initialize_start()?;
    // `initialize_start` returns the id without responding (that is
    // `initialize_finish`'s job). If the params are malformed we must still answer
    // the request — otherwise the one request that matters most hangs unanswered.
    let init_params: InitializeParams = match serde_json::from_value(init_value) {
        Ok(params) => params,
        Err(e) => {
            let err = ResponseError {
                code: INVALID_PARAMS,
                message: format!("invalid initialize params: {e}"),
                data: None,
            };
            let _ = connection.sender.send(Message::Response(Response {
                id,
                result: None,
                error: Some(err),
            }));
            return Err(e.into());
        }
    };
    let encoding = negotiate_encoding(&init_params);
    let init_result = InitializeResult {
        capabilities: server_capabilities(encoding),
        server_info: Some(ServerInfo {
            name: "jomini-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    connection.initialize_finish(id, serde_json::to_value(init_result)?)?;

    let mut server = Server::new(&init_params, connection.sender.clone(), encoding);
    // `initialize_finish` already consumed the `initialized` notification, so this
    // is the first point we may push messages: publish the initial diagnostics.
    server.publish_all();
    main_loop(connection, &mut server)
}

fn main_loop(connection: &Connection, server: &mut Server) -> Result<(), DynError> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                // `handle_shutdown` answers a `shutdown` request and returns true
                // once the following `exit` arrives (it consumes that `exit`
                // itself), at which point we stop cleanly.
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                server.on_request(req);
            }
            Message::Notification(not) => {
                // A bare `exit` (no preceding `shutdown` — that path is consumed by
                // `handle_shutdown` above) is the protocol's abrupt-termination
                // signal: stop, and report it as an error so the process exits
                // non-zero per the spec.
                if not.method == lsp_types::notification::Exit::METHOD {
                    return Err("received `exit` without a prior `shutdown`".into());
                }
                server.on_notification(not);
            }
            // We issue no server→client requests yet, so any response is ignored.
            Message::Response(_) => {}
        }
    }
    Ok(())
}

/// Prefer UTF-8 columns when the client supports them (a straight byte mapping);
/// otherwise the LSP default, UTF-16.
fn negotiate_encoding(init: &InitializeParams) -> PositionEncoding {
    let supports_utf8 = init
        .capabilities
        .general
        .as_ref()
        .and_then(|g| g.position_encodings.as_ref())
        .is_some_and(|encs| encs.contains(&PositionEncodingKind::UTF8));
    if supports_utf8 {
        PositionEncoding::Utf8
    } else {
        PositionEncoding::Utf16
    }
}

fn server_capabilities(encoding: PositionEncoding) -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(encoding.to_lsp()),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        ..Default::default()
    }
}
