//! The language server: project state plus one handler per LSP feature.
//!
//! State is a single owned [`Analysis`] (the lint snapshot) over a [`Fileset`]
//! whose bytes double as the open-document overlay. Every edit re-runs
//! [`Linter::analyze`] from scratch — correct and simple; incremental re-analysis
//! is a deliberate follow-up (a one-file edit can flip override winners
//! project-wide, so a naive single-file update would be wrong).

use crate::convert;
use crate::line_index::{LineIndex, PositionEncoding};
use crossbeam_channel::Sender;
use jomini::text::lint::{Analysis, FileId, FileKind, Fileset, Linter, Schema};
use jomini::text::syntax::{self, FormatOptions};
use lsp_server::{Message, Notification, Request, RequestId, Response, ResponseError};
use lsp_types::notification::Notification as NotificationTrait;
use lsp_types::request::Request as RequestTrait;
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentFormattingParams, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, Location, Position,
    PublishDiagnosticsParams, Range, ReferenceParams, SymbolInformation, TextEdit, Uri,
    WorkspaceEdit, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

// JSON-RPC error codes (LSP reserves the JSON-RPC range).
const METHOD_NOT_FOUND: i32 = -32601;
const INVALID_PARAMS: i32 = -32602;

pub struct Server {
    fileset: Fileset,
    linter: Linter,
    analysis: Analysis,
    encoding: PositionEncoding,
    sender: Sender<Message>,
    /// Canonicalized path → [`FileId`], for resolving an incoming document URL to
    /// its handle robustly (independent of `/tmp` vs `/private/tmp`-style aliases).
    path_to_id: HashMap<PathBuf, FileId>,
    /// Files currently open in the editor — so a close can revert the overlay.
    open: HashSet<FileId>,
    /// Files we last published non-empty diagnostics for, so stale sets get cleared.
    published: HashSet<FileId>,
}

impl Server {
    /// Build the server: load the workspace (and an optional vanilla base) into a
    /// [`Fileset`] and run the first [`Linter::analyze`]. No messages are sent
    /// yet — that waits for the `initialized` notification (per the LSP spec).
    pub fn new(
        init: &InitializeParams,
        sender: Sender<Message>,
        encoding: PositionEncoding,
    ) -> Self {
        let mut fileset = Fileset::new();

        // The mod layer: every workspace folder (or the deprecated root_uri).
        for root in workspace_roots(init) {
            let _ = fileset.load_dir(&root, FileKind::Mod(0));
        }

        // An optional vanilla base, from initializationOptions: { "vanilla": "<dir>" }.
        if let Some(vanilla) = init
            .initialization_options
            .as_ref()
            .and_then(|v| v.get("vanilla"))
            .and_then(|v| v.as_str())
        {
            let _ = fileset.load_dir(vanilla, FileKind::Vanilla);
        }

        let mut path_to_id = HashMap::new();
        for id in fileset.ids() {
            path_to_id.insert(canonical(fileset.path(id)), id);
        }

        let linter = Linter::new(demo_schema());
        let analysis = linter.analyze(&fileset);
        Server {
            fileset,
            linter,
            analysis,
            encoding,
            sender,
            path_to_id,
            open: HashSet::new(),
            published: HashSet::new(),
        }
    }

    // --- request / notification dispatch -----------------------------------

    pub fn on_request(&mut self, req: Request) {
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

    // --- document lifecycle -------------------------------------------------

    fn did_open(&mut self, params: DidOpenTextDocumentParams) {
        let doc = params.text_document;
        let bytes = doc.text.into_bytes();
        let id = match self.file_for_url(&doc.uri) {
            Some(id) => {
                self.fileset.set_source(id, bytes);
                id
            }
            // A file outside the loaded set (or never saved): add it as a mod file.
            None => {
                let Some(path) = uri_to_path(&doc.uri) else {
                    return;
                };
                let id = self.fileset.add(path.clone(), FileKind::Mod(0), bytes);
                self.path_to_id.insert(canonical(&path), id);
                id
            }
        };
        self.open.insert(id);
        self.reanalyze_and_publish();
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) {
        let Some(id) = self.file_for_url(&params.text_document.uri) else {
            return;
        };
        // FULL sync: the last content change carries the whole document.
        if let Some(change) = params.content_changes.into_iter().next_back() {
            self.fileset.set_source(id, change.text.into_bytes());
            self.reanalyze_and_publish();
        }
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) {
        let Some(id) = self.file_for_url(&params.text_document.uri) else {
            return;
        };
        self.open.remove(&id);
        // Drop the editor overlay: revert to disk content, or to empty bytes when
        // the file is gone or never existed (a never-saved buffer; there is no
        // Fileset removal API yet). Re-analyze and re-publish *unconditionally* so
        // any diagnostics for the now-closed buffer are cleared rather than left
        // stale — the server, not the client, owns clearing diagnostics.
        let path = self.fileset.path(id).to_path_buf();
        let reverted = std::fs::read(&path).unwrap_or_default();
        self.fileset.set_source(id, reverted);
        self.reanalyze_and_publish();
    }

    fn reanalyze_and_publish(&mut self) {
        self.analysis = self.linter.analyze(&self.fileset);
        self.publish_all();
    }

    // --- diagnostics --------------------------------------------------------

    /// Publish diagnostics for every file that has any, and clear the set for any
    /// file that had diagnostics last time but no longer does. Publishing for
    /// *all* files (not just open ones) is the point of a cross-file linter — an
    /// undefined reference often lives in a file the user has not opened.
    ///
    /// Called once after the `initialize` handshake (which `lsp_server` consumes
    /// the `initialized` notification within) and again after every edit.
    pub fn publish_all(&mut self) {
        let (payloads, now_published) = {
            let mut grouped: HashMap<FileId, Vec<&jomini::text::lint::Diagnostic>> = HashMap::new();
            for d in self.analysis.diagnostics() {
                grouped.entry(d.file).or_default().push(d);
            }

            let mut payloads: Vec<(Uri, Vec<lsp_types::Diagnostic>)> = Vec::new();
            let mut now_published: HashSet<FileId> = HashSet::new();
            for (&file, diags) in &grouped {
                let Some(url) = self.url_for_file(file) else {
                    continue;
                };
                let li = LineIndex::new(self.fileset.source(file), self.encoding);
                let lsp_diags = diags
                    .iter()
                    .map(|d| convert::to_lsp_diagnostic(&li, d))
                    .collect();
                now_published.insert(file);
                payloads.push((url, lsp_diags));
            }
            // Clear files whose diagnostics went away.
            for &file in &self.published {
                if !now_published.contains(&file)
                    && let Some(url) = self.url_for_file(file)
                {
                    payloads.push((url, Vec::new()));
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

    // --- features -----------------------------------------------------------

    /// `textDocument/formatting` → one whole-document edit from [`syntax::format`].
    /// Formatting a file with syntax errors is safe: the formatter round-trips
    /// every token losslessly, regenerating only whitespace.
    fn formatting(&mut self, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
        let id = self.file_for_url(&params.text_document.uri)?;
        let src = self.fileset.source(id);
        let indent = if params.options.insert_spaces {
            " ".repeat(params.options.tab_size as usize)
        } else {
            "\t".to_string()
        };
        let formatted = syntax::parse(src).format(&FormatOptions { indent });
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
        let li = LineIndex::new(src, self.encoding);
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
        let offset = LineIndex::new(self.fileset.source(id), self.encoding).offset(tdp.position);
        let def = self.analysis.definition_for(id, offset)?;
        let (file, range) = (def.file, def.range);
        let url = self.url_for_file(file)?;
        let li = LineIndex::new(self.fileset.source(file), self.encoding);
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
        let offset = LineIndex::new(self.fileset.source(id), self.encoding).offset(tdp.position);

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
        let mut ranges_by_file: HashMap<FileId, Vec<(u32, u32)>> = HashMap::new();
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
            let li = LineIndex::new(self.fileset.source(file), self.encoding);
            for range in ranges {
                locations.push(Location {
                    uri: uri.clone(),
                    range: convert::to_range(&li, range),
                });
            }
        }
        Some(locations)
    }

    /// `textDocument/documentSymbol` → the file's top-level definitions.
    fn document_symbol(&mut self, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
        let id = self.file_for_url(&params.text_document.uri)?;
        let li = LineIndex::new(self.fileset.source(id), self.encoding);
        let summary = self.analysis.summary(id)?;
        let symbols = summary
            .defs
            .iter()
            .map(|d| {
                let range = convert::to_range(&li, d.range);
                #[allow(deprecated)]
                DocumentSymbol {
                    name: d.name.clone(),
                    detail: Some(d.kind.clone()),
                    kind: convert::symbol_kind(&d.kind),
                    tags: None,
                    deprecated: None,
                    range,
                    selection_range: range,
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
                container_name: Some(def.kind.clone()),
            });
        }
        Some(WorkspaceSymbolResponse::Flat(infos))
    }

    /// `textDocument/codeAction` → a quick fix for each fixable diagnostic that
    /// overlaps the requested range. The lint [`Fix`] maps 1:1 to an LSP
    /// `TextEdit`, and because the tree is lossless the edit is byte-faithful.
    // `WorkspaceEdit.changes` is keyed by `Uri`, whose `fluent-uri` internals carry
    // a cache `Cell` (interior mutability) that never affects hashing — the key
    // type is mandated by lsp-types, so the lint is a false positive here.
    #[allow(clippy::mutable_key_type)]
    fn code_action(&mut self, params: CodeActionParams) -> Option<CodeActionResponse> {
        let id = self.file_for_url(&params.text_document.uri)?;
        let uri = params.text_document.uri.clone();
        let li = LineIndex::new(self.fileset.source(id), self.encoding);
        let sel_start = li.offset(params.range.start);
        let sel_end = li.offset(params.range.end);

        let mut actions = Vec::new();
        for d in self.analysis.diagnostics() {
            if d.file != id {
                continue;
            }
            let Some(fix) = &d.fix else { continue };
            // Keep only fixes whose diagnostic overlaps the requested range.
            if d.range.1 < sel_start || d.range.0 > sel_end {
                continue;
            }
            let edit = TextEdit {
                range: convert::to_range(&li, fix.range),
                new_text: fix.replacement.clone(),
            };
            let mut changes = HashMap::new();
            changes.insert(uri.clone(), vec![edit]);
            let title = d
                .help
                .clone()
                .unwrap_or_else(|| format!("Replace with `{}`", fix.replacement));
            #[allow(deprecated)]
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title,
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![convert::to_lsp_diagnostic(&li, d)]),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                is_preferred: Some(true),
                ..Default::default()
            }));
        }
        Some(actions)
    }

    // --- helpers ------------------------------------------------------------

    /// A [`Location`] for a byte range within `file`, building that file's
    /// [`LineIndex`] from its current (overlaid) bytes.
    fn location(&self, file: FileId, range: (u32, u32)) -> Option<Location> {
        let url = self.url_for_file(file)?;
        let li = LineIndex::new(self.fileset.source(file), self.encoding);
        Some(Location {
            uri: url,
            range: convert::to_range(&li, range),
        })
    }

    fn file_for_url(&self, uri: &Uri) -> Option<FileId> {
        let path = uri_to_path(uri)?;
        self.path_to_id.get(&canonical(&path)).copied()
    }

    fn url_for_file(&self, file: FileId) -> Option<Uri> {
        path_to_uri(self.fileset.path(file))
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

    fn notify<N: NotificationTrait>(&self, params: N::Params) {
        let not = Notification {
            method: N::METHOD.to_string(),
            params: serde_json::to_value(params).unwrap_or(serde_json::Value::Null),
        };
        let _ = self.sender.send(Message::Notification(not));
    }
}

/// The workspace folders to load as the mod layer: every `workspace_folder`, or
/// the deprecated `root_uri` as a fallback.
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

/// The prototype rule set: which directories define entities and which field keys
/// reference them. This stands in for a real per-game schema (a `.cwt`-config or
/// richer hand-written front-end is the follow-up); it mirrors `examples/lint.rs`
/// so the bundled `examples/mod-demo` corpus lights up end-to-end in an editor.
fn demo_schema() -> Schema {
    let mut schema = Schema::new();
    schema
        .define_dir("buildings", "building")
        .reference_key("has_building", "building")
        .reference_key("add_building", "building")
        .reference_key("remove_building", "building")
        .reference_key("upgrades_from", "building");
    schema
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
