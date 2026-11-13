//! `jomini-lsp` is a language server for the script files of Paradox games.
//! It checks a mod and its base game together with
//! [`jomini::text::lint`], and gives the editor the diagnostics, quick fixes,
//! go to definition, find references, symbols, and formatting.
//!
//! To use the server in an editor, see the README: it tells how to build the
//! binary, start it, and give it the directory of the base game.
//!
//! [`serve`] runs the server on a [`Connection`] on one thread. The binary
//! uses stdin and stdout, and the tests use a connection in memory, so a test
//! does not start a process. [`serve_with`] also takes a [`Config`], which
//! sets the rules of the game and the parser flavor.
//!
//! # Examples
//!
//! Run the server on a different thread, open two files, and read the
//! diagnostics that the server publishes:
//!
//! ```
//! use lsp_server::{Connection, Message, Notification, Request, RequestId};
//! use serde_json::json;
//!
//! let (server, client) = Connection::memory();
//! let thread = std::thread::spawn(move || jomini_lsp::serve(server).unwrap());
//!
//! let request = |id: i32, method: &str, params| {
//!     let id = RequestId::from(id);
//!     let request = Request { id, method: method.into(), params };
//!     client.sender.send(Message::Request(request)).unwrap();
//! };
//! let notify = |method: &str, params| {
//!     let notification = Notification { method: method.into(), params };
//!     client.sender.send(Message::Notification(notification)).unwrap();
//! };
//! let open = |uri: &str, text: &str| {
//!     let document = json!({ "uri": uri, "languageId": "paradox", "version": 1, "text": text });
//!     notify("textDocument/didOpen", json!({ "textDocument": document }));
//! };
//!
//! request(1, "initialize", json!({ "capabilities": {} }));
//! notify("initialized", json!({}));
//! open("file:///project/common/buildings/b.txt", "barracks = { cost = 100 }");
//! open("file:///project/events/e.txt", "e = { add_building = baracks }");
//!
//! // Read the messages until the server reports the wrong building name.
//! let message = loop {
//!     let Message::Notification(n) = client.receiver.recv().unwrap() else { continue };
//!     let diagnostics = &n.params["diagnostics"];
//!     if n.params["uri"] == "file:///project/events/e.txt" && diagnostics[0].is_object() {
//!         break diagnostics[0]["message"].clone();
//!     }
//! };
//! assert_eq!(
//!     message,
//!     "no building named `baracks` is defined in any loaded file\ndid you mean `barracks`?"
//! );
//!
//! request(2, "shutdown", json!(null));
//! notify("exit", json!(null));
//! thread.join().unwrap();
//! ```

mod convert;
mod line_index;
mod server;

use crossbeam_channel::TryRecvError;
use jomini::text::lint::Schema;
use jomini::text::lossless::Flavor;
use line_index::PositionEncoding;
use lsp_server::{Connection, Message, RequestId, Response, ResponseError};
use lsp_types::notification::{Initialized, Notification as _};
use lsp_types::request::{Initialize, Request as _};
use lsp_types::{
    CodeActionProviderCapability, InitializeParams, InitializeResult, OneOf, PositionEncodingKind,
    ServerCapabilities, ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use serde_json::Value;
use server::Server;

pub type DynError = Box<dyn std::error::Error + Sync + Send>;

// JSON-RPC `InvalidParams` (lsp-server 0.7 exposes no error-code enum).
const INVALID_PARAMS: i32 = -32602;
// LSP `ServerNotInitialized`.
const SERVER_NOT_INITIALIZED: i32 = -32002;

/// The settings of the server that do not come from the editor: the rules of
/// the game, and the parser flavor for the lint and for formatting.
///
/// ```
/// use jomini::text::lint::Schema;
/// use jomini::text::lossless::Flavor;
/// use jomini_lsp::Config;
///
/// let mut schema = Schema::new();
/// schema
///     .define_dir("common/ideas", "idea")
///     .reference_key("has_idea", "idea");
/// let config = Config::new(schema).with_flavor(Flavor::hoi4());
/// # let _ = config;
/// ```
pub struct Config {
    schema: Schema,
    flavor: Flavor,
}

impl Config {
    /// A configuration with the rules of `schema` and the default
    /// [`Flavor`].
    pub fn new(schema: Schema) -> Self {
        Config {
            schema,
            flavor: Flavor::default(),
        }
    }

    /// Parse with `flavor`, such as [`Flavor::hoi4`] for HoI4 files.
    pub fn with_flavor(mut self, flavor: Flavor) -> Self {
        self.flavor = flavor;
        self
    }
}

impl Default for Config {
    /// The rules for buildings of the project in
    /// `crates/jomini/examples/mod-demo`, and the default [`Flavor`].
    fn default() -> Self {
        let mut schema = Schema::new();
        schema
            .define_dir("common/buildings", "building")
            .reference_key("has_building", "building")
            .reference_key("add_building", "building")
            .reference_key("remove_building", "building")
            .reference_key("upgrades_from", "building");
        Config::new(schema)
    }
}

/// Run the server over `connection` with the default [`Config`]. See
/// [`serve_with`].
pub fn serve(connection: Connection) -> Result<(), DynError> {
    serve_with(connection, Config::default())
}

/// Run the server over `connection`: the `initialize` handshake, and then the
/// messages until `shutdown` and `exit`.
///
/// The function takes the connection, and drops it before it returns. The
/// I/O threads of [`Connection::stdio`] stop only when no sender of the
/// connection remains, so join them after this function.
pub fn serve_with(connection: Connection, config: Config) -> Result<(), DynError> {
    let (id, init_value) = initialize_start(&connection)?;
    // If the params are malformed we must still answer the request — otherwise
    // the one request that matters most hangs unanswered.
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
        capabilities: server_capabilities(encoding, &init_params),
        server_info: Some(ServerInfo {
            name: "jomini-lsp".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
    };
    initialize_finish(&connection, id, serde_json::to_value(init_result)?)?;

    // `initialize_finish` received the `initialized` notification, so the
    // server can send messages now. It reports the files that it cannot
    // load, and then it publishes the first diagnostics.
    let mut server = Server::new(&init_params, config, connection.sender.clone(), encoding);
    server.publish_all();
    main_loop(&connection, &mut server)
}

/// Receive the messages until the `initialize` request, and return its id
/// and its params. A different request gets an error response. The
/// `lsp-server` helpers for the handshake panic when the client disconnects,
/// so the server does the handshake itself.
fn initialize_start(connection: &Connection) -> Result<(RequestId, Value), DynError> {
    loop {
        match connection.receiver.recv().map_err(|_| DISCONNECTED)? {
            Message::Request(req) if req.method == Initialize::METHOD => {
                return Ok((req.id, req.params));
            }
            Message::Request(req) => {
                let response = Response::new_err(
                    req.id,
                    SERVER_NOT_INITIALIZED,
                    format!("expected the initialize request, got `{}`", req.method),
                );
                send(connection, response.into())?;
            }
            Message::Notification(not) if not.method != lsp_types::notification::Exit::METHOD => {}
            message => {
                return Err(format!("expected the initialize request, got {message:?}").into());
            }
        }
    }
}

/// Send the response to the `initialize` request, and receive the
/// `initialized` notification.
fn initialize_finish(
    connection: &Connection,
    id: RequestId,
    result: Value,
) -> Result<(), DynError> {
    send(connection, Response::new_ok(id, result).into())?;
    match connection.receiver.recv().map_err(|_| DISCONNECTED)? {
        Message::Notification(not) if not.method == Initialized::METHOD => Ok(()),
        message => Err(format!("expected the initialized notification, got {message:?}").into()),
    }
}

/// Send `message` to the client, or return an error when the client
/// disconnected.
fn send(connection: &Connection, message: Message) -> Result<(), DynError> {
    connection
        .sender
        .send(message)
        .map_err(|_| DISCONNECTED.into())
}

const DISCONNECTED: &str = "the client disconnected";

fn main_loop(connection: &Connection, server: &mut Server) -> Result<(), DynError> {
    let mut waiting = None;
    loop {
        let message = match waiting.take() {
            Some(message) => message,
            None => match connection.receiver.recv() {
                Ok(message) => message,
                Err(_) => return Ok(()),
            },
        };
        match message {
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
            // The server sends no requests, so a response is not expected.
            Message::Response(_) => {}
        }
        // Analyze only when no message waits, so that a quick series of
        // changes causes one analysis. `try_recv` also receives from a
        // sender that waits on a channel with no capacity, such as the
        // channel of `Connection::stdio`. `is_empty` is always true for that
        // channel.
        match connection.receiver.try_recv() {
            Ok(message) => waiting = Some(message),
            Err(TryRecvError::Empty | TryRecvError::Disconnected) => server.flush(),
        }
    }
}

/// Use UTF-8 columns when the client supports them, because they are byte
/// columns. Else use UTF-16, the LSP default.
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

/// The features of the server. The server gives code actions only as
/// literals, so it offers them only to a client that supports literals.
fn server_capabilities(encoding: PositionEncoding, init: &InitializeParams) -> ServerCapabilities {
    let code_action_literals = init
        .capabilities
        .text_document
        .as_ref()
        .and_then(|t| t.code_action.as_ref())
        .is_some_and(|c| c.code_action_literal_support.is_some());
    ServerCapabilities {
        position_encoding: Some(encoding.to_lsp()),
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        document_formatting_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        code_action_provider: code_action_literals
            .then_some(CodeActionProviderCapability::Simple(true)),
        ..Default::default()
    }
}
