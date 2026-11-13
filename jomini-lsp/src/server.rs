//! The language server: project state plus one handler per LSP feature.
//!
//! State is a single owned [`Analysis`] (the lint snapshot) over a [`Fileset`]
//! whose bytes double as the open-document overlay. Every edit runs
//! [`Linter::reanalyze`], which parses only the changed file again. The
//! cross-file pass still runs over the whole project, because a one-file edit
//! can flip override winners project-wide.

use crate::Config;
use crate::convert;
use crate::line_index::{LineIndex, PositionEncoding, non_ascii_lines};
use crossbeam_channel::Sender;
use jomini::text::lint::{Analysis, FileId, FileKind, Fileset, Linter, lints};
use jomini::text::lossless::{self, Flavor, FormatOptions, TextRange};
use lsp_server::{Message, Notification, Request, RequestId, Response, ResponseError};
use lsp_types::notification::Notification as NotificationTrait;
use lsp_types::request::Request as RequestTrait;
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentChanges, DocumentFormattingParams, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    Location, MessageType, OneOf, OptionalVersionedTextDocumentIdentifier, Position,
    PublishDiagnosticsParams, Range, ReferenceParams, ShowMessageParams, SymbolInformation,
    TextDocumentEdit, TextEdit, Uri, WorkspaceEdit, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

// JSON-RPC error codes (LSP reserves the JSON-RPC range).
const METHOD_NOT_FOUND: i32 = -32601;
const INVALID_PARAMS: i32 = -32602;

pub struct Server {
    fileset: Fileset,
    linter: Linter,
    analysis: Analysis,
    flavor: Flavor,
    encoding: PositionEncoding,
    /// `true` when the client supports nested document symbols.
    nested_symbols: bool,
    /// The directories that the server loaded, each with its layer. A file
    /// that the editor opens later gets the layer of its directory.
    roots: Vec<Root>,
    sender: Sender<Message>,
    /// Canonicalized path → [`FileId`], for resolving an incoming document URL to
    /// its handle robustly (independent of `/tmp` vs `/private/tmp`-style aliases).
    path_to_id: HashMap<PathBuf, FileId>,
    /// The file of each open document, by the text of its URI. The editor
    /// sends the same URI until it closes the document, also when the
    /// canonical path of the file changes, such as when a new file is saved.
    open: HashMap<String, FileId>,
    /// The open document of each file that the editor has open.
    documents: HashMap<FileId, Document>,
    /// The open documents that the server ignores, by the text of their
    /// URI: a second URI, such as a path through a symbolic link, for a
    /// file that is already open. The fileset has one text for each file,
    /// so the first document keeps it.
    ignored: HashSet<String>,
    /// The [`non_ascii_lines`] of each file, made on the first request.
    /// [`source_changed`](Server::source_changed) clears the entry of a file.
    non_ascii: RefCell<HashMap<FileId, Rc<[u32]>>>,
    /// `true` when the client supports versioned edits
    /// (`documentChanges`).
    versioned_edits: bool,
    /// The documents that have diagnostics in the editor. The editor keeps
    /// diagnostics by URI, so the server clears them by URI.
    published: Vec<Uri>,
    /// `true` when a document changed after the last analysis. See
    /// [`flush`](Server::flush).
    dirty: bool,
}

/// A document that the editor has open.
struct Document {
    /// The URI that the editor uses for the document. The server uses it in
    /// its messages, also when a different path names the same file.
    uri: Uri,
    /// The version of the text that the server has.
    version: i32,
}

/// A directory that the server loaded, and its layer.
struct Root {
    path: PathBuf,
    /// The canonical form of `path`, to match a canonical file path.
    canonical: PathBuf,
    kind: FileKind,
}

impl Root {
    fn new(path: PathBuf, kind: FileKind) -> Self {
        Root {
            canonical: canonical(&path),
            path,
            kind,
        }
    }

    /// The path of `file` relative to this root, when the root holds it.
    fn game_path(&self, file: &Path) -> Option<PathBuf> {
        if let Ok(relative) = file.strip_prefix(&self.path) {
            return Some(relative.to_path_buf());
        }
        let file = canonical(file);
        Some(file.strip_prefix(&self.canonical).ok()?.to_path_buf())
    }
}

impl Server {
    /// Load the workspace folders and the base game into a [`Fileset`], and
    /// do the first [`Linter::analyze`]. Each workspace folder is one mod,
    /// and a later folder overrides an earlier one. The server shows an error
    /// message for each directory that it cannot load, and it continues with
    /// the files that it loaded.
    ///
    /// Call this function after the `initialized` notification, because it
    /// can send messages.
    pub fn new(
        init: &InitializeParams,
        config: Config,
        sender: Sender<Message>,
        encoding: PositionEncoding,
    ) -> Self {
        let mut roots = Vec::new();
        // An optional base game, from initializationOptions: { "vanilla": "<dir>" }.
        if let Some(vanilla) = init
            .initialization_options
            .as_ref()
            .and_then(|v| v.get("vanilla"))
            .and_then(|v| v.as_str())
        {
            roots.push(Root::new(PathBuf::from(vanilla), FileKind::Vanilla));
        }
        for (index, root) in workspace_roots(init).into_iter().enumerate() {
            let layer = u16::try_from(index).unwrap_or(u16::MAX);
            roots.push(Root::new(root, FileKind::Mod(layer)));
        }

        let mut fileset = Fileset::new();
        let mut load_errors = Vec::new();
        for root in &roots {
            if let Err(error) = fileset.load_dir(&root.path, root.kind) {
                load_errors.push(format!(
                    "jomini-lsp cannot load {}: {error}. The diagnostics do not include all of its files.",
                    root.path.display()
                ));
            }
        }

        let mut path_to_id = HashMap::new();
        for id in fileset.ids() {
            path_to_id.insert(canonical(fileset.path(id)), id);
        }

        let nested_symbols = init
            .capabilities
            .text_document
            .as_ref()
            .and_then(|t| t.document_symbol.as_ref())
            .and_then(|d| d.hierarchical_document_symbol_support)
            .unwrap_or(false);
        let versioned_edits = init
            .capabilities
            .workspace
            .as_ref()
            .and_then(|w| w.workspace_edit.as_ref())
            .and_then(|e| e.document_changes)
            .unwrap_or(false);
        let linter = Linter::new(config.schema).with_flavor(config.flavor);
        let analysis = linter.analyze(&fileset);
        let server = Server {
            fileset,
            linter,
            analysis,
            flavor: config.flavor,
            encoding,
            nested_symbols,
            roots,
            sender,
            path_to_id,
            open: HashMap::new(),
            documents: HashMap::new(),
            ignored: HashSet::new(),
            non_ascii: RefCell::default(),
            versioned_edits,
            published: Vec::new(),
            dirty: false,
        };
        for message in load_errors {
            server.show_error(message);
        }
        server
    }

    pub fn on_request(&mut self, req: Request) {
        // A request must see all the changes before it.
        self.flush();
        match req.method.as_str() {
            lsp_types::request::Formatting::METHOD => {
                self.dispatch::<lsp_types::request::Formatting>(req, |s, p| s.formatting(p))
            }
            lsp_types::request::GotoDefinition::METHOD => self
                .dispatch::<lsp_types::request::GotoDefinition>(req, |s, p| s.goto_definition(p)),
            lsp_types::request::References::METHOD => {
                self.dispatch::<lsp_types::request::References>(req, |s, p| s.references(p))
            }
            lsp_types::request::DocumentSymbolRequest::METHOD => {
                self.dispatch::<lsp_types::request::DocumentSymbolRequest>(req, |s, p| {
                    s.document_symbol(p)
                })
            }
            lsp_types::request::WorkspaceSymbolRequest::METHOD => {
                self.dispatch::<lsp_types::request::WorkspaceSymbolRequest>(req, |s, p| {
                    s.workspace_symbol(p)
                })
            }
            lsp_types::request::CodeActionRequest::METHOD => {
                self.dispatch::<lsp_types::request::CodeActionRequest>(req, |s, p| s.code_action(p))
            }
            other => self.respond_err(
                req.id,
                METHOD_NOT_FOUND,
                format!("unhandled request: {other}"),
            ),
        }
    }

    pub fn on_notification(&mut self, not: Notification) {
        match not.method.as_str() {
            lsp_types::notification::DidOpenTextDocument::METHOD => {
                if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(not.params) {
                    self.did_open(p);
                }
            }
            lsp_types::notification::DidChangeTextDocument::METHOD => {
                if let Ok(p) = serde_json::from_value::<DidChangeTextDocumentParams>(not.params) {
                    self.did_change(p);
                }
            }
            lsp_types::notification::DidCloseTextDocument::METHOD => {
                if let Ok(p) = serde_json::from_value::<DidCloseTextDocumentParams>(not.params) {
                    self.did_close(p);
                }
            }
            // FULL sync means the latest text already arrived via didChange.
            _ => {}
        }
    }

    fn dispatch<R>(&mut self, req: Request, handler: impl FnOnce(&mut Self, R::Params) -> R::Result)
    where
        R: RequestTrait,
    {
        let Request { id, params, .. } = req;
        match serde_json::from_value::<R::Params>(params) {
            Ok(params) => {
                let result = handler(self, params);
                self.respond(id, result);
            }
            Err(e) => self.respond_err(id, INVALID_PARAMS, format!("invalid params: {e}")),
        }
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        let bytes = doc.text.into_bytes();
        let id = match self
            .file_for_url(&doc.uri)
            .or_else(|| self.open_file_at(&doc.uri))
        {
            Some(id) => {
                if let Some(first) = self.documents.get(&id)
                    && first.uri != doc.uri
                {
                    self.show_message(
                        MessageType::WARNING,
                        format!(
                            "jomini-lsp ignores {}, because the same file is open as {}. Close one of them.",
                            doc.uri.as_str(),
                            first.uri.as_str()
                        ),
                    );
                    self.ignored.insert(doc.uri.as_str().to_owned());
                    return;
                }
                self.fileset.set_source(id, bytes);
                self.source_changed(id);
                id
            }
            // A file that is not in the set, such as a new file, gets the
            // layer of the directory that holds it. Its game path is relative
            // to that directory, so it replaces a file of a lower layer. A
            // file outside each directory is in the first mod layer.
            None => {
                let Some(path) = uri_to_path(&doc.uri) else {
                    return;
                };
                let root = self
                    .roots
                    .iter()
                    .filter_map(|root| Some((root.kind, root.game_path(&path)?)))
                    .min_by_key(|(_, game_path)| game_path.components().count());
                let (kind, game_path) = root.unwrap_or((FileKind::Mod(0), path.clone()));
                let id = self
                    .fileset
                    .add_with_game_path(path.clone(), game_path, kind, bytes);
                self.source_changed(id);
                self.path_to_id.insert(canonical(&path), id);
                id
            }
        };
        self.open.insert(doc.uri.as_str().to_owned(), id);
        self.documents.insert(
            id,
            Document {
                uri: doc.uri,
                version: doc.version,
            },
        );
        self.dirty = true;
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        let Some(id) = self.file_for_url(&params.text_document.uri) else {
            return;
        };
        // FULL sync: the last content change carries the whole document.
        if let Some(change) = params.content_changes.into_iter().next_back() {
            self.fileset.set_source(id, change.text.into_bytes());
            self.source_changed(id);
            self.dirty = true;
        }
        if let Some(document) = self.documents.get_mut(&id) {
            document.version = params.text_document.version;
        }
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        if self.ignored.remove(params.text_document.uri.as_str()) {
            return;
        }
        let Some(id) = self.file_for_url(&params.text_document.uri) else {
            return;
        };
        self.open.remove(params.text_document.uri.as_str());
        self.documents.remove(&id);
        // The text on the disk replaces the text of the editor. When the file
        // is not on the disk, such as a deleted file or a file that was never
        // saved, the server removes it, so it does not replace a file of a
        // lower layer. When the server cannot read the file, it keeps the
        // text of the editor and shows an error. Then the server publishes
        // again, which clears the diagnostics of a removed file.
        let path = self.fileset.path(id).to_path_buf();
        match std::fs::read(&path) {
            Ok(bytes) => {
                self.fileset.set_source(id, bytes);
                self.source_changed(id);
                // The file is on the disk now, so its canonical path can be
                // different from the path when the server added it.
                self.path_to_id.insert(canonical(&path), id);
            }
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                self.fileset.remove(id);
                self.source_changed(id);
                self.path_to_id.retain(|_, file| *file != id);
            }
            Err(error) => self.show_error(format!(
                "jomini-lsp cannot read {}: {error}. It keeps the text of the editor.",
                path.display()
            )),
        }
        self.dirty = true;
    }

    /// Analyze again and publish the diagnostics, when a document changed.
    ///
    /// The change notifications only mark the server dirty. The main loop
    /// calls this function when no message waits, and each request calls it
    /// first. Thus a quick series of changes, such as fast typing, causes one
    /// analysis, not one analysis for each change.
    pub fn flush(&mut self) {
        if self.dirty {
            self.dirty = false;
            self.linter.reanalyze(&self.fileset, &mut self.analysis);
            self.publish_all();
        }
    }

    /// Publish diagnostics for every file that has any, and clear the set for any
    /// file that had diagnostics last time but no longer does. Publishing for
    /// *all* files (not just open ones) is the point of a cross-file linter — an
    /// undefined reference often lives in a file the user has not opened.
    ///
    /// Called after the `initialize` handshake and by [`flush`](Server::flush).
    pub fn publish_all(&mut self) {
        let (payloads, now_published) = {
            let mut grouped: HashMap<FileId, Vec<&jomini::text::lint::Diagnostic>> = HashMap::new();
            for d in self.analysis.diagnostics() {
                grouped.entry(d.file).or_default().push(d);
            }

            let mut payloads: Vec<(Uri, Vec<lsp_types::Diagnostic>)> = Vec::new();
            let mut now_published: Vec<Uri> = Vec::new();
            for (&file, diags) in &grouped {
                let Some(url) = self.url_for_file(file) else {
                    continue;
                };
                let li = self.line_index(file);
                let lsp_diags = diags
                    .iter()
                    .map(|d| convert::to_lsp_diagnostic(&li, d))
                    .collect();
                now_published.push(url.clone());
                payloads.push((url, lsp_diags));
            }
            // Clear the documents that have no diagnostics now. A removed
            // file and a new file can have the same URI, so compare URIs.
            let now: HashSet<&str> = now_published.iter().map(|uri| uri.as_str()).collect();
            for uri in &self.published {
                if !now.contains(uri.as_str()) {
                    payloads.push((uri.clone(), Vec::new()));
                }
            }
            (payloads, now_published)
        };

        self.published = now_published;
        for (uri, diagnostics) in payloads {
            self.notify::<lsp_types::notification::PublishDiagnostics>(PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            });
        }
    }

    /// `textDocument/formatting` → one edit that replaces the whole document.
    /// The formatter changes only whitespace, so a file with syntax errors is
    /// safe to format.
    fn formatting(&mut self, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
        let id = self.file_for_url(&params.text_document.uri)?;
        let src = self.fileset.source(id);
        let indent = if params.options.insert_spaces {
            " ".repeat(params.options.tab_size as usize)
        } else {
            "\t".to_string()
        };
        let options = FormatOptions::default().with_indent(indent);
        let formatted = lossless::parse_with(src, self.flavor).format(&options);
        if formatted == src {
            return Some(Vec::new());
        }
        // Until encoding-aware formatting lands, refuse to emit an edit that would
        // lose non-UTF-8 (e.g. Windows-1252) bytes through a lossy decode — a
        // "format" must never corrupt content. Both sides valid UTF-8 ⇒ the
        // whole-document replace is safe; otherwise make it a no-op.
        let new_text = match (std::str::from_utf8(&formatted), std::str::from_utf8(src)) {
            (Ok(text), Ok(_)) => text.to_owned(),
            _ => return Some(Vec::new()),
        };
        let li = self.line_index(id);
        Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: li.end_position(),
            },
            new_text,
        }])
    }

    /// `textDocument/definition` → the winning [`Definition`] for the reference
    /// (or definition) under the cursor, possibly in another file.
    fn goto_definition(&mut self, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
        let tdp = params.text_document_position_params;
        let id = self.file_for_url(&tdp.text_document.uri)?;
        let offset = self.line_index(id).offset(tdp.position);
        let def = self.analysis.definition_for(id, offset)?;
        let (file, range) = (def.file, def.range);
        let url = self.url_for_file(file)?;
        let li = self.line_index(file);
        Some(GotoDefinitionResponse::Scalar(Location {
            uri: url,
            range: convert::to_range(&li, range),
        }))
    }

    /// `textDocument/references` → every use site of the `(kind, name)` under the
    /// cursor across the project, plus the declaration if requested.
    fn references(&mut self, params: ReferenceParams) -> Option<Vec<Location>> {
        let tdp = params.text_document_position;
        let id = self.file_for_url(&tdp.text_document.uri)?;
        let offset = self.line_index(id).offset(tdp.position);

        // The name under the cursor — from a reference, or from a definition site.
        let (kind, name) = if let Some(r) = self.analysis.reference_at(id, offset) {
            (r.kind.clone(), r.name.clone())
        } else if let Some(d) = self.analysis.definition_at(id, offset) {
            (d.kind.clone(), d.name.clone())
        } else {
            return Some(Vec::new());
        };

        // Gather (file, byte range) for every use site, plus — if requested —
        // *every* declaration site of the name. Paradox names share a single
        // namespace, so an override or same-layer duplicate are all declarations of
        // the symbol; report them all, not just the winner.
        let mut ranges_by_file: HashMap<FileId, Vec<TextRange>> = HashMap::new();
        for (file, r) in self.analysis.references_to(&kind, &name) {
            ranges_by_file.entry(file).or_default().push(r.range);
        }
        if params.context.include_declaration {
            for def in self.analysis.definitions() {
                if def.kind == kind && def.name == name {
                    ranges_by_file.entry(def.file).or_default().push(def.range);
                }
            }
        }

        // One [`LineIndex`] per distinct file, not one per result.
        let mut locations = Vec::new();
        for (file, ranges) in ranges_by_file {
            let Some(uri) = self.url_for_file(file) else {
                continue;
            };
            let li = self.line_index(file);
            for range in ranges {
                locations.push(Location {
                    uri: uri.clone(),
                    range: convert::to_range(&li, range),
                });
            }
        }
        Some(locations)
    }

    /// `textDocument/documentSymbol` → the definitions of the file. The range
    /// of a symbol is the full definition, and its selection range is the
    /// name. A client that does not support nested symbols gets a flat list.
    fn document_symbol(&mut self, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        let uri = params.text_document.uri;
        let id = self.file_for_url(&uri)?;
        let li = self.line_index(id);
        let summary = self.analysis.summary(id)?;
        if !self.nested_symbols {
            #[allow(deprecated)]
            let symbols = summary
                .defs
                .iter()
                .map(|d| SymbolInformation {
                    name: d.name.clone(),
                    kind: convert::symbol_kind(&d.kind),
                    tags: None,
                    deprecated: None,
                    location: Location {
                        uri: uri.clone(),
                        range: convert::to_range(&li, d.full_range),
                    },
                    container_name: Some(d.kind.to_string()),
                })
                .collect();
            return Some(DocumentSymbolResponse::Flat(symbols));
        }
        let symbols = summary
            .defs
            .iter()
            .map(|d| {
                #[allow(deprecated)]
                DocumentSymbol {
                    name: d.name.clone(),
                    detail: Some(d.kind.to_string()),
                    kind: convert::symbol_kind(&d.kind),
                    tags: None,
                    deprecated: None,
                    range: convert::to_range(&li, d.full_range),
                    selection_range: convert::to_range(&li, d.range),
                    children: None,
                }
            })
            .collect();
        Some(DocumentSymbolResponse::Nested(symbols))
    }

    /// `workspace/symbol` → every winning definition whose name matches `query`.
    fn workspace_symbol(
        &mut self,
        params: WorkspaceSymbolParams,
    ) -> Option<WorkspaceSymbolResponse> {
        let query = params.query.to_lowercase();
        let mut infos = Vec::new();
        for def in self.analysis.definitions() {
            // One entry per winning name (skip shadowed duplicates).
            if !def.winner {
                continue;
            }
            if !query.is_empty() && !def.name.to_lowercase().contains(&query) {
                continue;
            }
            let Some(location) = self.location(def.file, def.range) else {
                continue;
            };
            #[allow(deprecated)]
            infos.push(SymbolInformation {
                name: def.name.clone(),
                kind: convert::symbol_kind(&def.kind),
                tags: None,
                deprecated: None,
                location,
                container_name: Some(def.kind.to_string()),
            });
        }
        Some(WorkspaceSymbolResponse::Flat(infos))
    }

    /// `textDocument/codeAction` → a quick fix for each diagnostic with a
    /// [`TextEdit`](jomini::text::lossless::TextEdit) fix that overlaps the
    /// requested range, and one fix-all action for all the syntax repairs of
    /// the file. A fix with text that is not UTF-8 is not offered.
    fn code_action(&mut self, params: CodeActionParams) -> Option<CodeActionResponse> {
        let id = self.file_for_url(&params.text_document.uri)?;
        let uri = params.text_document.uri.clone();
        let source = self.fileset.source(id);
        // A byte edit cannot be mapped to a text position with confidence when
        // the open document is not UTF-8. The Rust lint API still exposes it.
        if std::str::from_utf8(source).is_err() {
            return Some(Vec::new());
        }
        let li = self.line_index(id);
        let sel_start = li.offset(params.range.start);
        let sel_end = li.offset(params.range.end);

        let mut actions = Vec::new();
        for d in self.analysis.diagnostics() {
            if d.file != id {
                continue;
            }
            let Some(edit) = d.fix.as_ref().and_then(|fix| lsp_edit(&li, fix)) else {
                continue;
            };
            // Keep only fixes whose diagnostic overlaps the requested range.
            if d.range.end() < sel_start || d.range.start() > sel_end {
                continue;
            }
            let title = d
                .help
                .clone()
                .unwrap_or_else(|| format!("Replace with `{}`", edit.new_text));
            #[allow(deprecated)]
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title,
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![convert::to_lsp_diagnostic(&li, d)]),
                edit: Some(self.workspace_edit(id, uri.clone(), vec![edit])),
                is_preferred: Some(true),
                ..Default::default()
            }));
        }

        let syntax_diagnostics: Vec<_> = self
            .analysis
            .diagnostics()
            .iter()
            .filter(|diagnostic| {
                diagnostic.file == id
                    && diagnostic.id == lints::SYNTAX_ERROR
                    && diagnostic.fix.is_some()
            })
            .collect();
        if !syntax_diagnostics.is_empty() {
            let mut edits: Vec<_> = syntax_diagnostics
                .iter()
                .filter_map(|diagnostic| diagnostic.fix.as_ref())
                .filter_map(|fix| lsp_edit(&li, fix))
                .collect();
            edits.sort_by_key(|edit| {
                (
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character,
                )
            });
            edits.dedup();
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title: "Apply all safe syntax repairs".into(),
                kind: Some(CodeActionKind::SOURCE_FIX_ALL),
                diagnostics: Some(
                    syntax_diagnostics
                        .iter()
                        .map(|diagnostic| convert::to_lsp_diagnostic(&li, diagnostic))
                        .collect(),
                ),
                edit: Some(self.workspace_edit(id, uri, edits)),
                is_preferred: Some(false),
                ..Default::default()
            }));
        }
        Some(actions)
    }

    /// The position conversion for `file`, on the line index that the fileset
    /// keeps for it.
    fn line_index(&self, file: FileId) -> LineIndex<'_> {
        let source = self.fileset.source(file);
        let lines = self.fileset.line_index(file);
        let non_ascii = Rc::clone(
            self.non_ascii
                .borrow_mut()
                .entry(file)
                .or_insert_with(|| non_ascii_lines(source, lines)),
        );
        LineIndex::new(source, lines, non_ascii, self.encoding)
    }

    /// Clear the data that the server keeps for the source of `file`.
    fn source_changed(&mut self, file: FileId) {
        self.non_ascii.get_mut().remove(&file);
    }

    /// The open file that is at the path of `uri` now. The path index has
    /// the canonical path of a file when the server added it, and that path
    /// can change, such as when the editor saves a new file under a symbolic
    /// link. Thus compare the canonical paths of the open files again, and
    /// update the index.
    fn open_file_at(&mut self, uri: &Uri) -> Option<FileId> {
        let path = canonical(&uri_to_path(uri)?);
        let id = self
            .documents
            .keys()
            .copied()
            .find(|&id| canonical(self.fileset.path(id)) == path)?;
        self.path_to_id.insert(path, id);
        Some(id)
    }

    /// The [`Location`] of a byte range in `file`.
    fn location(&self, file: FileId, range: TextRange) -> Option<Location> {
        let url = self.url_for_file(file)?;
        let li = self.line_index(file);
        Some(Location {
            uri: url,
            range: convert::to_range(&li, range),
        })
    }

    fn file_for_url(&self, uri: &Uri) -> Option<FileId> {
        if self.ignored.contains(uri.as_str()) {
            return None;
        }
        if let Some(&id) = self.open.get(uri.as_str()) {
            return Some(id);
        }
        let path = uri_to_path(uri)?;
        self.path_to_id.get(&canonical(&path)).copied()
    }

    /// The URI of `file`: the URI of its open document, or else the URI of
    /// its path.
    fn url_for_file(&self, file: FileId) -> Option<Uri> {
        match self.documents.get(&file) {
            Some(document) => Some(document.uri.clone()),
            None => path_to_uri(self.fileset.path(file)),
        }
    }

    /// The edit of `edits` to `file` at `uri`. When the client supports
    /// versioned edits, the edit has the version of the open document, so
    /// the client rejects it after a later change of the text.
    // `WorkspaceEdit.changes` is keyed by `Uri`, whose `fluent-uri` internals carry
    // a cache `Cell` (interior mutability) that never affects hashing — the key
    // type is mandated by lsp-types, so the lint is a false positive here.
    #[allow(clippy::mutable_key_type)]
    fn workspace_edit(&self, file: FileId, uri: Uri, edits: Vec<TextEdit>) -> WorkspaceEdit {
        if !self.versioned_edits {
            return WorkspaceEdit {
                changes: Some(HashMap::from([(uri, edits)])),
                ..Default::default()
            };
        }
        let version = self.documents.get(&file).map(|document| document.version);
        WorkspaceEdit {
            document_changes: Some(DocumentChanges::Edits(vec![TextDocumentEdit {
                text_document: OptionalVersionedTextDocumentIdentifier { uri, version },
                edits: edits.into_iter().map(OneOf::Left).collect(),
            }])),
            ..Default::default()
        }
    }

    fn respond<T: serde::Serialize>(&self, id: RequestId, result: T) {
        let result = serde_json::to_value(result).unwrap_or(serde_json::Value::Null);
        let _ = self.sender.send(Message::Response(Response {
            id,
            result: Some(result),
            error: None,
        }));
    }

    fn respond_err(&self, id: RequestId, code: i32, message: String) {
        let response = Response {
            id,
            result: None,
            error: Some(ResponseError {
                code,
                message,
                data: None,
            }),
        };
        let _ = self.sender.send(Message::Response(response));
    }

    /// Show an error message in the editor.
    fn show_error(&self, message: String) {
        self.show_message(MessageType::ERROR, message);
    }

    /// Show a message in the editor, and write it to stderr.
    fn show_message(&self, typ: MessageType, message: String) {
        eprintln!("{message}");
        self.notify::<lsp_types::notification::ShowMessage>(ShowMessageParams { typ, message });
    }

    fn notify<N: NotificationTrait>(&self, params: N::Params) {
        let not = Notification {
            method: N::METHOD.to_string(),
            params: serde_json::to_value(params).unwrap_or(serde_json::Value::Null),
        };
        let _ = self.sender.send(Message::Notification(not));
    }
}

/// The LSP edit of a lint fix, or `None` if the text of the fix is not UTF-8.
fn lsp_edit(li: &LineIndex<'_>, fix: &lossless::TextEdit) -> Option<TextEdit> {
    Some(TextEdit {
        range: convert::to_range(li, fix.range),
        new_text: String::from_utf8(fix.new_text.clone()).ok()?,
    })
}

/// The workspace folders to load as mods, in order: every `workspace_folder`,
/// or the deprecated `root_uri` as a fallback.
fn workspace_roots(init: &InitializeParams) -> Vec<PathBuf> {
    if let Some(folders) = &init.workspace_folders {
        return folders.iter().filter_map(|f| uri_to_path(&f.uri)).collect();
    }
    #[allow(deprecated)]
    if let Some(root) = &init.root_uri
        && let Some(p) = uri_to_path(root)
    {
        return vec![p];
    }
    Vec::new()
}

/// `lsp_types::Uri` → filesystem path. lsp-types 0.97 dropped the `url` crate for
/// `fluent-uri` (which has no path conversion), so we bridge through `url`.
fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    url::Url::parse(uri.as_str()).ok()?.to_file_path().ok()
}

/// Filesystem path → a `file://` `lsp_types::Uri`, via the `url` crate.
fn path_to_uri(path: &Path) -> Option<Uri> {
    let url = url::Url::from_file_path(path).ok()?;
    url.as_str().parse::<Uri>().ok()
}

/// Canonicalize a path for stable URL↔file matching, falling back to the path as
/// given when it does not exist on disk (e.g. an unsaved buffer).
fn canonical(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}
