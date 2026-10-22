//! **Experimental** cross-file linting over the [`lossless`] tree.
//!
//! A [`SyntaxTree`](lossless::SyntaxTree) describes one file, but many errors
//! in a mod are in the relation between files: a building that refers to a
//! culture that a different file defines, or that no file defines. This
//! module finds such errors in these steps:
//!
//! 1. A [`Fileset`] holds the source files. Each file has a [`FileKind`]
//!    that gives its load-order layer, so a mod definition overrides a base
//!    game definition.
//! 2. [`summarize`] parses each file into a [`FileSummary`]: the definitions,
//!    the references, and the syntax errors of the file. The summary owns its
//!    data and holds no tree, so the summaries of all the files of a game fit
//!    in memory, and [`Linter::reanalyze`] parses again only the files that
//!    changed.
//! 3. An [`Index`] holds the definitions of all the files by `(kind, name)`,
//!    and picks the definition that wins in the load order.
//! 4. The [`Linter`] resolves each reference with the index. A reference that
//!    resolves to nothing is an undefined-reference [`Diagnostic`]. When the
//!    reference is in a node that has a syntax error, the linter gives a weak
//!    diagnostic instead, because the syntax error is the cause.
//!
//! Each diagnostic has a [`Severity`] and a [`Confidence`]. See
//! `examples/lint.rs` for a full example.
//!
//! The [`Schema`] of which files define an entity and which keys refer to an
//! entity is a small set of rules written by hand, not a full rule set for a
//! game. The linter compares names as lossy UTF-8 strings, not with the
//! encoding of the game.
//!
//! # Examples
//!
//! Find a reference to a building that no file defines. The base game
//! defines `barracks`, and a mod event refers to `baracks`:
//!
//! ```
//! use jomini::text::lint::{FileKind, Fileset, Linter, Schema, lints, render};
//!
//! let mut fileset = Fileset::new();
//! fileset.add(
//!     "common/buildings/00_buildings.txt",
//!     FileKind::Vanilla,
//!     b"barracks = { cost = 100 }\nwharf = { cost = 50 }\n".to_vec(),
//! );
//! let events = fileset.add(
//!     "events/mod_events.txt",
//!     FileKind::Mod(0),
//!     b"my_event = {\n\tadd_building = baracks\n}\n".to_vec(),
//! );
//!
//! let mut schema = Schema::new();
//! schema
//!     .define_dir("buildings", "building")
//!     .reference_key("add_building", "building");
//!
//! let diagnostics = Linter::new(schema).run(&fileset);
//! assert_eq!(diagnostics.len(), 1);
//! let diagnostic = &diagnostics[0];
//! assert_eq!(diagnostic.id, lints::UNDEFINED_REFERENCE);
//! assert_eq!(diagnostic.file, events);
//! assert_eq!(diagnostic.help.as_deref(), Some("did you mean `barracks`?"));
//!
//! let report = render(diagnostic, &fileset, false);
//! assert!(report.contains("--> events/mod_events.txt:2:17"));
//! ```

#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::text::lossless::{
    self, Applicability, AstNode, Field, Flavor, LineCol, LineIndex, Repair, SyntaxNode, TextRange,
    TextSize, Value,
};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

/// A handle to a file in a [`Fileset`]. Use the fileset to get the path and
/// the source of the file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// The load-order layer of a file. A definition in a later layer overrides a
/// definition of the same `(kind, name)` in an earlier layer. The order is
/// `Vanilla < Dlc < Mod`, and `Mod(0) < Mod(1)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FileKind {
    /// Base game files.
    Vanilla,
    /// DLC content, layered above vanilla by index.
    Dlc(u16),
    /// A loaded mod, layered above DLC by index (load order).
    Mod(u16),
}

struct FileEntry {
    path: PathBuf,
    kind: FileKind,
    source: Vec<u8>,
    /// Increases each time the source changes.
    revision: u64,
    /// Made on the first request, and cleared when the source changes.
    lines: OnceLock<LineIndex>,
}

/// The source files of a project, each with its load-order [`FileKind`]. The
/// fileset owns the bytes of each file.
pub struct Fileset {
    /// Identifies this fileset, so that [`Linter::reanalyze`] does not reuse
    /// the summaries of a different fileset.
    tag: u64,
    files: Vec<FileEntry>,
}

impl Default for Fileset {
    fn default() -> Self {
        static NEXT_TAG: AtomicU64 = AtomicU64::new(0);
        Fileset {
            tag: NEXT_TAG.fetch_add(1, Ordering::Relaxed),
            files: Vec::new(),
        }
    }
}

impl Fileset {
    /// An empty fileset.
    pub fn new() -> Self {
        Fileset::default()
    }

    /// Add a file at `path` in the layer `kind`, and return its [`FileId`].
    pub fn add(&mut self, path: impl Into<PathBuf>, kind: FileKind, source: Vec<u8>) -> FileId {
        let id = FileId(self.files.len() as u32);
        self.files.push(FileEntry {
            path: path.into(),
            kind,
            source,
            revision: 0,
            lines: OnceLock::new(),
        });
        id
    }

    /// Add each `*.txt` file under `root` to the layer `kind`, and return their
    /// ids.
    ///
    /// The function adds the files in path order. Inside a layer, the last
    /// definition of a name wins, so the ids must follow the load order of the
    /// game, which is path order. The function does not follow a symbolic link
    /// to a directory.
    pub fn load_dir(
        &mut self,
        root: impl AsRef<Path>,
        kind: FileKind,
    ) -> std::io::Result<Vec<FileId>> {
        let mut paths: Vec<PathBuf> = Vec::new();
        let mut stack = vec![root.as_ref().to_path_buf()];
        while let Some(dir) = stack.pop() {
            for entry in std::fs::read_dir(&dir)? {
                let entry = entry?;
                // `file_type()` comes from the directory entry and does not
                // traverse symlinks, so a symlinked directory is skipped.
                let file_type = entry.file_type()?;
                let path = entry.path();
                if file_type.is_dir() {
                    stack.push(path);
                } else if file_type.is_file() && path.extension().is_some_and(|e| e == "txt") {
                    paths.push(path);
                }
            }
        }
        paths.sort();

        let mut ids = Vec::with_capacity(paths.len());
        for path in paths {
            let source = std::fs::read(&path)?;
            ids.push(self.add(path, kind, source));
        }
        Ok(ids)
    }

    /// The raw source bytes of a file.
    pub fn source(&self, id: FileId) -> &[u8] {
        &self.files[id.0 as usize].source
    }

    /// The line index of a file. The first call for a file makes the index.
    pub fn line_index(&self, id: FileId) -> &LineIndex {
        let file = &self.files[id.0 as usize];
        file.lines.get_or_init(|| LineIndex::new(&file.source))
    }

    /// The line and column of `offset` in a file.
    pub fn line_col(&self, id: FileId, offset: TextSize) -> LineCol {
        self.line_index(id).line_col(offset)
    }

    /// The display path of a file.
    pub fn path(&self, id: FileId) -> &Path {
        &self.files[id.0 as usize].path
    }

    /// The load-order layer of a file.
    pub fn kind(&self, id: FileId) -> FileKind {
        self.files[id.0 as usize].kind
    }

    /// Every [`FileId`] in the set.
    pub fn ids(&self) -> impl Iterator<Item = FileId> + '_ {
        (0..self.files.len() as u32).map(FileId)
    }

    /// The [`FileId`] of the file at `path`, when the set has one. The paths
    /// must be equal. An editor uses this to find the file of an open
    /// document, and then gives the unsaved text to
    /// [`set_source`](Fileset::set_source).
    pub fn id_for_path(&self, path: impl AsRef<Path>) -> Option<FileId> {
        let path = path.as_ref();
        self.files
            .iter()
            .position(|f| f.path == path)
            .map(|i| FileId(i as u32))
    }

    /// Replace the source of a file. The [`FileId`], the path, and the layer
    /// do not change. Then call [`Linter::reanalyze`] to update an
    /// [`Analysis`].
    pub fn set_source(&mut self, id: FileId, source: Vec<u8>) {
        let file = &mut self.files[id.0 as usize];
        file.source = source;
        file.revision += 1;
        file.lines = OnceLock::new();
    }
}

/// The kind of the entity of a definition or a reference, such as
/// `"building"`.
pub type Kind = String;

/// The rules that tell which files define entities and which field keys
/// refer to entities.
#[derive(Default)]
pub struct Schema {
    /// Directory component (e.g. `"buildings"`) → the [`Kind`] that each
    /// top-level key in a file under that directory defines.
    definition_dirs: HashMap<String, Kind>,
    /// Field key (e.g. `"add_building"`) → the [`Kind`] its value references.
    reference_keys: HashMap<String, Kind>,
}

impl Schema {
    /// An empty schema.
    pub fn new() -> Self {
        Schema::default()
    }

    /// Each top-level key of a file in a directory named `dir` defines an
    /// entity of kind `kind`, as in `define_dir("buildings", "building")`.
    pub fn define_dir(&mut self, dir: &str, kind: &str) -> &mut Self {
        self.definition_dirs
            .insert(dir.to_string(), kind.to_string());
        self
    }

    /// The value of a field with the key `key` refers to an entity of kind
    /// `kind`, as in `reference_key("add_building", "building")`.
    pub fn reference_key(&mut self, key: &str, kind: &str) -> &mut Self {
        self.reference_keys
            .insert(key.to_string(), kind.to_string());
        self
    }

    /// The kind that a file at `path` defines, if any directory component of
    /// the path is a known definition directory.
    fn definition_kind(&self, path: &Path) -> Option<&Kind> {
        path.components()
            .filter_map(|c| c.as_os_str().to_str())
            .find_map(|c| self.definition_dirs.get(c))
    }
}

/// A definition in a file: its kind, its name, and the range of its key.
#[derive(Debug, Clone)]
pub struct DefSite {
    pub kind: Kind,
    pub name: String,
    pub range: TextRange,
}

/// A reference in a file: the kind and the name that it refers to, and the
/// range of the value. `in_error_subtree` is `true` when a node that holds the
/// reference has a syntax error.
#[derive(Debug, Clone)]
pub struct RefSite {
    pub kind: Kind,
    pub name: String,
    pub range: TextRange,
    pub in_error_subtree: bool,
}

/// The definitions, the references, and the syntax errors of one file. The
/// summary owns its data and holds no tree.
pub struct FileSummary {
    pub file: FileId,
    /// The [`Fileset`] revision of the source that this summary comes from.
    pub revision: u64,
    pub defs: Vec<DefSite>,
    pub refs: Vec<RefSite>,
    pub syntax_errors: Vec<SyntaxDiagnosticSummary>,
}

/// A syntax error of a file, and its repair when the parser has one.
#[derive(Debug, Clone)]
pub struct SyntaxDiagnosticSummary {
    pub message: String,
    pub range: TextRange,
    pub opening_range: Option<TextRange>,
    pub applicability: Option<Applicability>,
    pub fix: Option<Repair>,
}

fn decode(s: crate::Scalar) -> String {
    String::from_utf8_lossy(s.as_bytes()).into_owned()
}

/// Parse one file and make its [`FileSummary`].
pub fn summarize(fileset: &Fileset, schema: &Schema, file: FileId) -> FileSummary {
    let source = fileset.source(file);
    let tree = lossless::parse_with(source, Flavor::default());

    let mut defs = Vec::new();
    // Top-level keys define entities only when the file lives in a known
    // definition directory. Do not index a definition from a recovered field:
    // references in the same error subtree are suppressed for the same reason.
    if let Some(kind) = schema.definition_kind(fileset.path(file)) {
        for field in tree.ast().fields() {
            if !field.syntax().has_error() {
                let key = field.key();
                defs.push(DefSite {
                    kind: kind.clone(),
                    name: decode(key.as_scalar()),
                    range: key.text_range(),
                });
            }
        }
    }

    // References can appear at any depth, so walk the whole tree.
    let mut refs = Vec::new();
    collect_refs(tree.root(), schema, false, &mut refs);

    let repairs = tree.repairs();
    let syntax_errors = tree
        .errors()
        .iter()
        .map(|e| {
            let recovery = e.recovery.as_ref();
            let fix = recovery
                .filter(|r| r.order == 0)
                .and_then(|r| repairs.iter().find(|f| f.range == r.insertion_range))
                .map(|f| Repair {
                    range: f.range,
                    replacement: f.replacement.clone(),
                });
            SyntaxDiagnosticSummary {
                message: e.message().to_owned(),
                range: e.range,
                opening_range: recovery.map(|r| r.opening_range),
                applicability: recovery.map(|r| tree.repair_applicability(r)),
                fix,
            }
        })
        .collect();

    FileSummary {
        file,
        revision: fileset.files[file.0 as usize].revision,
        defs,
        refs,
        syntax_errors,
    }
}

/// Collect the references in `node` and its descendants. `in_error` is `true`
/// when an ancestor of `node` has a syntax error.
fn collect_refs(node: SyntaxNode<'_, '_>, schema: &Schema, in_error: bool, out: &mut Vec<RefSite>) {
    let in_error = in_error || node.has_error();

    if let Some(field) = Field::cast(node)
        && let Some(value) = field.value()
        && let Some(kind) = schema.reference_keys.get(&decode(field.key().as_scalar()))
        && let Value::Scalar(tok) = value
    {
        out.push(RefSite {
            kind: kind.clone(),
            name: decode(tok.as_scalar()),
            range: tok.text_range(),
            in_error_subtree: in_error,
        });
    }

    for child in node.child_nodes() {
        collect_refs(child, schema, in_error, out);
    }
}

/// An index into [`Index::definitions`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefId(pub u32);

/// A definition in the [`Index`]. `winner` is `true` for the definition that
/// wins in the load order.
#[derive(Debug, Clone)]
pub struct Definition {
    pub kind: Kind,
    pub name: String,
    pub file: FileId,
    pub range: TextRange,
    pub winner: bool,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct Key {
    kind: Kind,
    name: String,
}

/// The result of resolving a reference with the [`Index`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    /// The name resolves to a definition.
    Found(DefId),
    /// No file defines this `(kind, name)`.
    NotFound,
    /// No file defines this `(kind, name)`, but the reference is in a node
    /// that has a syntax error. The syntax error can be the cause.
    UpstreamError,
}

/// The definitions of all the files, by `(kind, name)`. Add each definition
/// with [`define`](Index::define), then call [`finalize`](Index::finalize) to
/// pick the definition that wins for each name.
#[derive(Default)]
pub struct Index {
    definitions: Vec<Definition>,
    by_key: HashMap<Key, SmallVec<[u32; 2]>>,
    winners: HashMap<Key, u32>,
}

impl Index {
    /// An empty index.
    pub fn new() -> Self {
        Index::default()
    }

    /// Add a definition, and return its [`DefId`].
    pub fn define(&mut self, kind: Kind, name: String, file: FileId, range: TextRange) -> DefId {
        let id = self.definitions.len() as u32;
        let key = Key {
            kind: kind.clone(),
            name: name.clone(),
        };
        self.definitions.push(Definition {
            kind,
            name,
            file,
            range,
            winner: false,
        });
        self.by_key.entry(key).or_default().push(id);
        DefId(id)
    }

    /// All the definitions.
    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    /// Resolve a reference to the definition that wins. When no file defines
    /// the name and `in_error_subtree` is set, the result is
    /// [`Resolution::UpstreamError`]. Call [`finalize`](Index::finalize)
    /// first.
    pub fn resolve(&self, kind: &str, name: &str, in_error_subtree: bool) -> Resolution {
        let key = Key {
            kind: kind.to_string(),
            name: name.to_string(),
        };
        match self.winners.get(&key) {
            Some(&id) => Resolution::Found(DefId(id)),
            None if in_error_subtree => Resolution::UpstreamError,
            None => Resolution::NotFound,
        }
    }

    /// The defined name of the same `kind` that is nearest to `name`, for a
    /// "did you mean?" hint. Return `None` when no name is near enough. Only a
    /// name that wins is a candidate, so the suggested name resolves. Call
    /// [`finalize`](Index::finalize) first.
    pub fn suggest(&self, kind: &str, name: &str) -> Option<&str> {
        // A longer name can have more edits, from 1 to 3. Thus `baracks` gets
        // `barracks`, but a short name does not get an unrelated name.
        let threshold = (name.len() / 3).clamp(1, 3);
        self.definitions
            .iter()
            .filter(|d| d.winner && d.kind == kind)
            .map(|d| {
                (
                    levenshtein(name.as_bytes(), d.name.as_bytes()),
                    d.name.as_str(),
                )
            })
            .filter(|&(dist, _)| dist <= threshold)
            .min_by_key(|&(dist, candidate)| (dist, candidate.len()))
            .map(|(_, candidate)| candidate)
    }

    /// Pick the definition that wins for each `(kind, name)`: the last one in
    /// the highest layer. Return a tip for each definition that overrides a
    /// lower layer, and a warning for each duplicate definition in one layer.
    pub fn finalize(&mut self, fileset: &Fileset) -> Vec<Diagnostic> {
        let mut diags = Vec::new();

        // Collect keys first to avoid borrowing `self.by_key` while mutating.
        let keys: Vec<Key> = self.by_key.keys().cloned().collect();
        for key in keys {
            let ids = self.by_key[&key].clone();

            // The winning layer is the highest FileKind among the definitions.
            let max_layer = ids
                .iter()
                .map(|&id| fileset.kind(self.definitions[id as usize].file))
                .max()
                .unwrap();

            // Within the winning layer, the last-loaded definition wins
            // (Paradox load order). Earlier same-layer definitions are
            // duplicates; lower-layer definitions are overridden.
            let winners_in_layer: Vec<u32> = ids
                .iter()
                .copied()
                .filter(|&id| fileset.kind(self.definitions[id as usize].file) == max_layer)
                .collect();
            let winner = *winners_in_layer.last().unwrap();
            self.definitions[winner as usize].winner = true;
            self.winners.insert(key.clone(), winner);

            for &id in &ids {
                let def = &self.definitions[id as usize];
                let layer = fileset.kind(def.file);
                if id == winner {
                    // If the winner shadows lower layers, note the override.
                    if ids
                        .iter()
                        .any(|&o| fileset.kind(self.definitions[o as usize].file) < max_layer)
                    {
                        diags.push(Diagnostic {
                            id: lints::OVERRIDE,
                            severity: Severity::Tip,
                            confidence: Confidence::Strong,
                            file: def.file,
                            range: def.range,
                            message: format!(
                                "{} `{}` overrides a lower-layer definition",
                                def.kind, def.name
                            ),
                            help: None,
                            fix: None,
                        });
                    }
                } else if layer == max_layer {
                    // Same top layer but not the winner: a real duplicate.
                    diags.push(Diagnostic {
                        id: lints::DUPLICATE,
                        severity: Severity::Warning,
                        confidence: Confidence::Strong,
                        file: def.file,
                        range: def.range,
                        message: format!(
                            "duplicate definition of {} `{}` in the same layer",
                            def.kind, def.name
                        ),
                        help: None,
                        fix: None,
                    });
                }
            }
        }

        diags.sort_by_key(|d| (d.file, d.range.start()));
        diags
    }
}

/// The Levenshtein edit distance between two byte strings. The names are
/// short, so the O(n·m) time is not a problem.
fn levenshtein(a: &[u8], b: &[u8]) -> usize {
    if a.is_empty() {
        return b.len();
    }
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut curr = vec![0usize; b.len() + 1];
    for (i, &ca) in a.iter().enumerate() {
        curr[0] = i + 1;
        for (j, &cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j] + cost).min(prev[j + 1] + 1).min(curr[j] + 1);
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    prev[b.len()]
}

/// The stable ids of the lints. A user can refer to an id to configure or
/// suppress a lint.
pub mod lints {
    pub const UNDEFINED_REFERENCE: &str = "undefined-reference";
    pub const SUPPRESSED_REFERENCE: &str = "undefined-reference-suppressed";
    pub const SYNTAX_ERROR: &str = "syntax-error";
    pub const DUPLICATE: &str = "duplicate-definition";
    pub const OVERRIDE: &str = "override";
}

/// How serious a finding is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    /// An informational note.
    Tip,
    /// A problem of style.
    Untidy,
    /// Probably wrong.
    Warning,
    /// Almost certainly wrong.
    Error,
    /// The linter cannot continue with the file.
    Fatal,
}

/// How sure the linter is that a finding is correct.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Confidence {
    /// Possibly a false positive. Show it only when the user asks.
    Weak,
    /// A reasonable finding.
    Reasonable,
    /// High confidence.
    Strong,
}

/// One finding. A diagnostic holds a [`FileId`] and a range, not a tree, so
/// it can outlive the parse. Use [`render`] to show it.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub id: &'static str,
    pub severity: Severity,
    pub confidence: Confidence,
    pub file: FileId,
    pub range: TextRange,
    pub message: String,
    /// A hint, such as "did you mean `barracks`?".
    pub help: Option<String>,
    /// A repair that a tool can apply automatically.
    pub fix: Option<Repair>,
}

/// The cross-file linter. It collects the definitions of all the files, and
/// then it resolves each reference.
pub struct Linter {
    schema: Schema,
}

impl Linter {
    /// A linter driven by `schema`.
    pub fn new(schema: Schema) -> Self {
        Linter { schema }
    }

    /// Lint a [`Fileset`], and return the diagnostics in order of file and
    /// position. An editor calls [`analyze`](Linter::analyze) instead, and
    /// keeps the [`Analysis`] for navigation.
    pub fn run(&self, fileset: &Fileset) -> Vec<Diagnostic> {
        self.analyze(fileset).into_diagnostics()
    }

    /// Lint a [`Fileset`], and keep the [`Index`] and the summary of each file
    /// in an [`Analysis`]. Go to definition and find references then read the
    /// analysis, and do not lint again.
    pub fn analyze(&self, fileset: &Fileset) -> Analysis {
        let summaries: Vec<FileSummary> = fileset
            .ids()
            .map(|id| summarize(fileset, &self.schema, id))
            .collect();
        self.resolve(fileset, summaries)
    }

    /// Update `analysis` after some sources of `fileset` changed.
    ///
    /// The result is the same as [`analyze`](Linter::analyze), but the
    /// summaries in `analysis` are used again for each file that has not
    /// changed since then. Only the changed files and the new files are parsed.
    /// The cross-file pass runs on all the files, because one change can
    /// change the winner of an override in a different file.
    ///
    /// `analysis` must come from this linter. When it comes from a different
    /// fileset, all the files are parsed.
    ///
    /// An editor calls this function after each change to a file:
    ///
    /// ```
    /// use jomini::text::lint::{FileKind, Fileset, Linter, Schema};
    ///
    /// let mut fileset = Fileset::new();
    /// fileset.add("common/buildings/b.txt", FileKind::Vanilla, b"barracks = {}".to_vec());
    /// let events = fileset.add("events/e.txt", FileKind::Mod(0), b"add_building = barracks".to_vec());
    ///
    /// let mut schema = Schema::new();
    /// schema.define_dir("buildings", "building").reference_key("add_building", "building");
    /// let linter = Linter::new(schema);
    ///
    /// let mut analysis = linter.analyze(&fileset);
    /// assert!(analysis.diagnostics().is_empty());
    ///
    /// // The person types a name that no file defines.
    /// fileset.set_source(events, b"add_building = stables".to_vec());
    /// linter.reanalyze(&fileset, &mut analysis);
    /// assert_eq!(analysis.diagnostics().len(), 1);
    /// ```
    pub fn reanalyze(&self, fileset: &Fileset, analysis: &mut Analysis) {
        if analysis.fileset_tag != fileset.tag {
            *analysis = self.analyze(fileset);
            return;
        }
        let mut summaries = std::mem::take(&mut analysis.summaries);
        for (summary, file) in summaries.iter_mut().zip(&fileset.files) {
            if summary.revision != file.revision {
                *summary = summarize(fileset, &self.schema, summary.file);
            }
        }
        for id in summaries.len()..fileset.files.len() {
            summaries.push(summarize(fileset, &self.schema, FileId(id as u32)));
        }
        *analysis = self.resolve(fileset, summaries);
    }

    /// Pass 2 for [`analyze`](Linter::analyze) and
    /// [`reanalyze`](Linter::reanalyze): register the definitions, then
    /// resolve the references of all the summaries.
    fn resolve(&self, fileset: &Fileset, summaries: Vec<FileSummary>) -> Analysis {
        let mut index = Index::new();
        for s in &summaries {
            for d in &s.defs {
                index.define(d.kind.clone(), d.name.clone(), s.file, d.range);
            }
        }
        let mut diags = index.finalize(fileset);

        // Also record the references of each name for find references.
        let mut refs_by_name: HashMap<Key, Vec<(FileId, u32)>> = HashMap::new();
        for s in &summaries {
            for error in &s.syntax_errors {
                diags.push(Diagnostic {
                    id: lints::SYNTAX_ERROR,
                    severity: Severity::Error,
                    confidence: Confidence::Strong,
                    file: s.file,
                    range: error.range,
                    message: error.message.clone(),
                    help: error.applicability.map(|applicability| {
                        if applicability.is_machine_applicable() {
                            "insert the missing trailing delimiter".into()
                        } else {
                            "the missing trailing delimiter has no safe automatic repair".into()
                        }
                    }),
                    fix: error.fix.clone(),
                });
            }
            for (ref_index, r) in s.refs.iter().enumerate() {
                refs_by_name
                    .entry(Key {
                        kind: r.kind.clone(),
                        name: r.name.clone(),
                    })
                    .or_default()
                    .push((s.file, ref_index as u32));

                match index.resolve(&r.kind, &r.name, r.in_error_subtree) {
                    Resolution::Found(_) => {}
                    Resolution::NotFound => {
                        // A reference is one token, so the nearest name is
                        // also a safe replacement.
                        let (help, fix) = match index.suggest(&r.kind, &r.name) {
                            Some(name) => (
                                Some(format!("did you mean `{name}`?")),
                                Some(Repair { range: r.range, replacement: name.to_string() }),
                            ),
                            None => (None, None),
                        };
                        diags.push(Diagnostic {
                            id: lints::UNDEFINED_REFERENCE,
                            severity: Severity::Error,
                            confidence: Confidence::Strong,
                            file: s.file,
                            range: r.range,
                            message: format!(
                                "no {} named `{}` is defined in any loaded file",
                                r.kind, r.name
                            ),
                            help,
                            fix,
                        });
                    }
                    Resolution::UpstreamError => diags.push(Diagnostic {
                        id: lints::SUPPRESSED_REFERENCE,
                        severity: Severity::Tip,
                        confidence: Confidence::Weak,
                        file: s.file,
                        range: r.range,
                        message: format!(
                            "`{}` may be an undefined {}, but its block has a syntax error — suppressed",
                            r.name, r.kind
                        ),
                        help: None,
                        fix: None,
                    }),
                }
            }
        }

        diags.sort_by_key(|d| (d.file, d.range.start(), d.range.end()));
        Analysis {
            fileset_tag: fileset.tag,
            index,
            summaries,
            diagnostics: diags,
            refs_by_name,
        }
    }
}

/// The result of [`Linter::analyze`]: the diagnostics, the [`Index`], and the
/// summary of each file.
///
/// An editor reads go to definition, find references, and the symbols from
/// the analysis. The analysis owns its data and does not borrow the
/// [`Fileset`], so a server can keep it. When a file changes,
/// [`Linter::reanalyze`] updates it.
///
/// Go to the definition of a reference, and find all of the references to a
/// definition:
///
/// ```
/// use jomini::text::lint::{FileKind, Fileset, Linter, Schema};
/// use jomini::text::lossless::TextSize;
///
/// let mut fileset = Fileset::new();
/// let vanilla = fileset.add(
///     "common/buildings/00_buildings.txt",
///     FileKind::Vanilla,
///     b"barracks = { cost = 100 }".to_vec(),
/// );
/// let modded = fileset.add(
///     "common/buildings/01_mod_buildings.txt",
///     FileKind::Mod(0),
///     b"barracks = { cost = 80 }".to_vec(),
/// );
/// let source = b"a = { add_building = barracks }\nb = { add_building = barracks }";
/// let events = fileset.add("events/mod_events.txt", FileKind::Mod(0), source.to_vec());
///
/// let mut schema = Schema::new();
/// schema
///     .define_dir("buildings", "building")
///     .reference_key("add_building", "building");
/// let analysis = Linter::new(schema).analyze(&fileset);
///
/// // The mod definition overrides the vanilla one, so it is the target.
/// let offset = source.windows(8).position(|w| w == b"barracks").unwrap();
/// let offset = TextSize::try_from(offset).unwrap();
/// let definition = analysis.definition_for(events, offset).unwrap();
/// assert_eq!(definition.file, modded);
/// assert_ne!(definition.file, vanilla);
///
/// let references: Vec<_> = analysis.references_to("building", "barracks").collect();
/// assert_eq!(references.len(), 2);
/// assert!(references.iter().all(|(file, _)| *file == events));
/// ```
pub struct Analysis {
    /// The tag of the [`Fileset`] that the summaries come from.
    fileset_tag: u64,
    index: Index,
    /// `summaries[i]` is the summary of `FileId(i)`.
    summaries: Vec<FileSummary>,
    diagnostics: Vec<Diagnostic>,
    /// The references to each `(kind, name)`, as the file and the index into
    /// the `refs` of the summary of the file.
    refs_by_name: HashMap<Key, Vec<(FileId, u32)>>,
}

impl Analysis {
    /// The diagnostics, in order of file and position.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Discard all but the diagnostics.
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    /// The project-wide symbol table.
    pub fn index(&self) -> &Index {
        &self.index
    }

    /// All the [`Definition`]s, for the workspace symbols.
    pub fn definitions(&self) -> &[Definition] {
        self.index.definitions()
    }

    /// The [`FileSummary`] of `file`, or `None` when the analysis does not have
    /// the file. The `defs` of the summary are the document symbols.
    pub fn summary(&self, file: FileId) -> Option<&FileSummary> {
        self.summaries.get(file.0 as usize)
    }

    /// The reference at `offset` in `file`. The range includes its end, so a
    /// cursor directly after a name finds it. Two references are never
    /// adjacent, so one offset cannot find two. The search is linear.
    pub fn reference_at(&self, file: FileId, offset: TextSize) -> Option<&RefSite> {
        let summary = self.summary(file)?;
        summary
            .refs
            .iter()
            .find(|r| r.range.contains_inclusive(offset))
    }

    /// The definition key at `offset` in `file`. The range includes its end,
    /// as in [`reference_at`](Analysis::reference_at).
    pub fn definition_at(&self, file: FileId, offset: TextSize) -> Option<&DefSite> {
        let summary = self.summary(file)?;
        summary
            .defs
            .iter()
            .find(|d| d.range.contains_inclusive(offset))
    }

    /// Go to definition from `offset` in `file`. On a reference, return the
    /// [`Definition`] that wins for its name. On a definition key, also return
    /// the definition that wins, which can be a different one that overrides
    /// it. Return `None` for other text and for an undefined reference.
    pub fn definition_for(&self, file: FileId, offset: TextSize) -> Option<&Definition> {
        if let Some(r) = self.reference_at(file, offset) {
            return self.winning_definition(&r.kind, &r.name);
        }
        if let Some(d) = self.definition_at(file, offset) {
            return self.winning_definition(&d.kind, &d.name);
        }
        None
    }

    /// The winning [`Definition`] for `(kind, name)`, if the name is defined.
    fn winning_definition(&self, kind: &str, name: &str) -> Option<&Definition> {
        match self.index.resolve(kind, name, false) {
            Resolution::Found(DefId(id)) => self.index.definitions().get(id as usize),
            _ => None,
        }
    }

    /// Find references: each reference to `(kind, name)` in all the files.
    /// All the definitions of a name share the references, because each
    /// reference resolves to the definition that wins.
    pub fn references_to<'s>(
        &'s self,
        kind: &str,
        name: &str,
    ) -> impl Iterator<Item = (FileId, &'s RefSite)> + 's {
        let key = Key {
            kind: kind.to_string(),
            name: name.to_string(),
        };
        self.refs_by_name
            .get(&key)
            .into_iter()
            .flatten()
            .filter_map(move |&(file, idx)| {
                let r = self
                    .summaries
                    .get(file.0 as usize)?
                    .refs
                    .get(idx as usize)?;
                Some((file, r))
            })
    }
}

/// The bytes of the line that holds `offset`, without the line break, and the
/// offset of the start of the line.
fn line_bytes(fileset: &Fileset, file: FileId, offset: TextSize) -> (&[u8], usize) {
    let index = fileset.line_index(file);
    let range = index
        .line_range(index.line_col(offset).line)
        .expect("the line of an offset exists");
    let line = &fileset.source(file)[range];
    let line = line.strip_suffix(b"\n").unwrap_or(line);
    (line, range.start().to_usize())
}

fn severity_label(s: Severity) -> &'static str {
    match s {
        Severity::Tip => "tip",
        Severity::Untidy => "untidy",
        Severity::Warning => "warning",
        Severity::Error => "error",
        Severity::Fatal => "fatal",
    }
}

fn severity_color(s: Severity) -> &'static str {
    match s {
        Severity::Tip => "\x1b[36m",                        // cyan
        Severity::Untidy | Severity::Warning => "\x1b[33m", // yellow
        Severity::Error | Severity::Fatal => "\x1b[31m",    // red
    }
}

/// Write a diagnostic as a report in the style of a compiler, with the line
/// of the source and a caret under the range. With `color`, the severity and
/// the caret have ANSI colors.
pub fn render(diag: &Diagnostic, fileset: &Fileset, color: bool) -> String {
    let at = fileset.line_col(diag.file, diag.range.start());
    let (line, col) = (at.line + 1, at.col + 1);
    let (line_src, line_start) = line_bytes(fileset, diag.file, diag.range.start());
    let line_text = String::from_utf8_lossy(line_src);

    let (reset, bold, dim) = if color {
        ("\x1b[0m", "\x1b[1m", "\x1b[2m")
    } else {
        ("", "", "")
    };
    let sev = severity_color(diag.severity);
    let sevc = if color { sev } else { "" };

    let conf = match diag.confidence {
        Confidence::Weak => " (weak)",
        Confidence::Reasonable => "",
        Confidence::Strong => "",
    };

    // The caret stops at the end of the line.
    let caret_col = diag.range.start().to_usize() - line_start;
    // A `TextRange` cannot be inverted, so its length does not underflow.
    let span = diag.range.len().to_usize().max(1);
    let span = span.min(line_src.len().saturating_sub(caret_col).max(1));
    let pad: String = line_src[..caret_col.min(line_src.len())]
        .iter()
        .map(|&b| if b == b'\t' { '\t' } else { ' ' })
        .collect();
    let carets = "^".repeat(span);

    let gutter = format!("{:>4}", line);
    let blank: String = " ".repeat(4);

    let mut out = format!(
        "{sevc}{bold}{sev_label}{reset}{bold}[{id}]{reset}: {msg}{conf}\n\
         {blank} {dim}-->{reset} {path}:{line}:{col}\n\
         {blank} {dim}|{reset}\n\
         {gutter} {dim}|{reset} {line_text}\n\
         {blank} {dim}|{reset} {pad}{sevc}{carets}{reset}\n",
        sev_label = severity_label(diag.severity),
        id = diag.id,
        msg = diag.message,
        path = fileset.path(diag.file).display(),
    );
    if let Some(help) = &diag.help {
        let fixable = if diag.fix.is_some() {
            " (fixable with --fix)"
        } else {
            ""
        };
        out.push_str(&format!("{blank} {dim}={reset} help: {help}{fixable}\n"));
    }
    out
}

/// Apply [`Repair`]s to `source`, and return the result. The function applies
/// the repairs in order of position. Zero-width repairs at the same offset
/// keep their input order. A repair that overlaps an applied repair, or that
/// goes past the end of the source, is skipped. To format the result, use
/// [`format`](lossless::format).
///
/// Apply the fixes of the diagnostics of one file:
///
/// ```
/// use jomini::text::lint::{FileKind, Fileset, Linter, Schema, apply_repairs};
///
/// let mut fileset = Fileset::new();
/// fileset.add("common/buildings/b.txt", FileKind::Vanilla, b"barracks = {}".to_vec());
/// let events = fileset.add(
///     "events/e.txt",
///     FileKind::Mod(0),
///     b"a = { add_building = baracks } # keep this comment".to_vec(),
/// );
///
/// let mut schema = Schema::new();
/// schema.define_dir("buildings", "building").reference_key("add_building", "building");
/// let diagnostics = Linter::new(schema).run(&fileset);
///
/// let fixes: Vec<_> = diagnostics
///     .iter()
///     .filter(|d| d.file == events)
///     .filter_map(|d| d.fix.clone())
///     .collect();
/// let fixed = apply_repairs(fileset.source(events), &fixes);
/// assert_eq!(fixed, b"a = { add_building = barracks } # keep this comment");
/// ```
pub fn apply_repairs(source: &[u8], fixes: &[Repair]) -> Vec<u8> {
    let mut sorted: Vec<(usize, &Repair)> = fixes.iter().enumerate().collect();
    sorted.sort_by_key(|(index, fix)| (fix.range.start(), fix.range.end(), *index));

    let mut out = Vec::with_capacity(source.len());
    // The number of bytes of `source` that are in `out`.
    let mut cursor = 0;
    for (_, fix) in sorted {
        let (start, end) = (fix.range.start().to_usize(), fix.range.end().to_usize());
        if start < cursor || end > source.len() {
            continue;
        }
        out.extend_from_slice(&source[cursor..start]);
        out.extend_from_slice(fix.replacement.as_bytes());
        cursor = end;
    }
    out.extend_from_slice(&source[cursor..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A fileset with base game buildings, a mod that overrides and refers to
    /// them, and a file with a syntax error.
    fn demo() -> (Fileset, Schema) {
        let mut fs = Fileset::new();
        fs.add(
            "vanilla/common/buildings/00_buildings.txt",
            FileKind::Vanilla,
            b"temple = { cost = 100 }\ncastle = { cost = 250 }\ncity = { cost = 500 }\nbarracks = { cost = 150 }\n".to_vec(),
        );
        fs.add(
            "mod/common/buildings/01_mod.txt",
            FileKind::Mod(0),
            b"temple = { cost = 80 }\nfortress = { upgrades_from = castle }\nfortress = { cost = 401 }\n"
                .to_vec(),
        );
        // `baracks` is a typo for the vanilla `barracks` (edit distance 1).
        fs.add(
            "mod/events/mod_events.txt",
            FileKind::Mod(0),
            b"e = { effect = { add_building = fortress\n add_building = baracks\n remove_building = temple } }\n".to_vec(),
        );
        fs.add(
            "mod/events/broken.txt",
            FileKind::Mod(0),
            b"broken = { effect = { add_building = ghost_building\n}\n".to_vec(),
        );

        let mut schema = Schema::new();
        schema
            .define_dir("buildings", "building")
            .reference_key("add_building", "building")
            .reference_key("remove_building", "building")
            .reference_key("upgrades_from", "building");
        (fs, schema)
    }

    fn keys(analysis: &Analysis) -> Vec<(&'static str, FileId, TextRange)> {
        analysis
            .diagnostics()
            .iter()
            .map(|d| (d.id, d.file, d.range))
            .collect()
    }

    #[test]
    fn reanalyze_matches_analyze() {
        let (mut fs, schema) = demo();
        let linter = Linter::new(schema);
        let mut analysis = linter.analyze(&fs);

        // Fix the typo in the events file.
        fs.set_source(
            FileId(2),
            b"e = { effect = { add_building = fortress\n add_building = barracks\n remove_building = temple } }\n".to_vec(),
        );
        linter.reanalyze(&fs, &mut analysis);
        assert_eq!(keys(&analysis), keys(&linter.analyze(&fs)));
        assert!(
            analysis
                .reference_at(FileId(2), TextSize::new(60))
                .is_some()
        );

        // Remove `fortress` from the mod buildings. The events file does not
        // change, but its reference to `fortress` is now undefined.
        fs.set_source(FileId(1), b"temple = { cost = 80 }\n".to_vec());
        linter.reanalyze(&fs, &mut analysis);
        assert_eq!(keys(&analysis), keys(&linter.analyze(&fs)));
        assert!(
            analysis
                .diagnostics()
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.file == FileId(2))
        );

        // A new file is summarized too.
        let id = fs.add(
            "mod/common/buildings/02_more.txt",
            FileKind::Mod(0),
            b"fortress = { cost = 1 }\n".to_vec(),
        );
        linter.reanalyze(&fs, &mut analysis);
        assert_eq!(keys(&analysis), keys(&linter.analyze(&fs)));
        assert!(analysis.summary(id).is_some_and(|s| s.defs.len() == 1));
    }

    #[test]
    fn reanalyze_ignores_summaries_of_a_different_fileset() {
        let (fs, schema) = demo();
        let linter = Linter::new(schema);
        let mut analysis = linter.analyze(&fs);

        // The same revisions, but different bytes.
        let mut other = Fileset::new();
        for id in fs.ids() {
            other.add(
                fs.path(id),
                fs.kind(id),
                b"x = { add_building = nowhere }".to_vec(),
            );
        }
        linter.reanalyze(&other, &mut analysis);
        assert_eq!(keys(&analysis), keys(&linter.analyze(&other)));
    }

    #[test]
    fn undefined_reference_suggests_the_nearest_name() {
        let (mut fs, schema) = demo();
        let linter = Linter::new(schema);
        let diags = linter.run(&fs);

        // Only `baracks` is undefined. `ghost_building` is in a broken block.
        let undef: Vec<_> = diags
            .iter()
            .filter(|d| d.id == lints::UNDEFINED_REFERENCE)
            .collect();
        assert_eq!(undef.len(), 1, "{diags:#?}");
        let d = undef[0];
        assert_eq!(d.severity, Severity::Error);
        assert_eq!(&fs.source(d.file)[d.range], b"baracks");
        assert_eq!(d.help.as_deref(), Some("did you mean `barracks`?"));

        // The fix replaces only the name, and the result has no diagnostic
        // for it.
        let fixed = apply_repairs(fs.source(d.file), d.fix.as_slice());
        assert_eq!(
            fixed,
            b"e = { effect = { add_building = fortress\n add_building = barracks\n remove_building = temple } }\n"
        );
        fs.set_source(d.file, fixed);
        assert!(
            !linter
                .run(&fs)
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE)
        );
    }

    #[test]
    fn apply_fixes_splices_and_skips_overlap() {
        // A single in-range rename.
        let renamed = apply_repairs(
            b"add_building = baracks",
            &[Repair {
                range: TextRange::span(15, 22),
                replacement: "barracks".into(),
            }],
        );
        assert_eq!(renamed, b"add_building = barracks");

        // Overlapping/out-of-order edits are skipped, never corrupting output.
        let out = apply_repairs(
            b"abcdef",
            &[
                Repair {
                    range: TextRange::span(0, 3),
                    replacement: "X".into(),
                },
                Repair {
                    range: TextRange::span(2, 5),
                    replacement: "Y".into(),
                }, // overlaps the first
            ],
        );
        assert_eq!(out, b"Xdef");

        // Equal zero-width edits keep their input order.
        let out = apply_repairs(
            b"abc",
            &[
                Repair {
                    range: TextRange::span(3, 3),
                    replacement: "]".into(),
                },
                Repair {
                    range: TextRange::span(3, 3),
                    replacement: "}".into(),
                },
            ],
        );
        assert_eq!(out, b"abc]}");
    }

    #[test]
    fn flags_override_and_duplicate() {
        let (fs, schema) = demo();
        let diags = Linter::new(schema).run(&fs);
        assert!(
            diags
                .iter()
                .any(|d| d.id == lints::OVERRIDE && d.message.contains("temple")),
            "mod temple overrides vanilla temple"
        );
        assert!(
            diags
                .iter()
                .any(|d| d.id == lints::DUPLICATE && d.message.contains("fortress")),
            "fortress is defined twice in the mod layer"
        );
    }

    #[test]
    fn suppresses_reference_in_broken_subtree() {
        let (fs, schema) = demo();
        let diags = Linter::new(schema).run(&fs);
        // `ghost_building` is undefined, but its block is unterminated, so the
        // reference is suppressed (Weak) rather than reported as a hard error.
        assert!(
            !diags.iter().any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains("ghost_building")),
            "a reference in a malformed subtree must not be a hard undefined-reference"
        );
        assert!(
            diags.iter().any(
                |d| d.id == lints::SUPPRESSED_REFERENCE && d.message.contains("ghost_building")
            ),
            "it should instead be a suppressed/weak finding"
        );
        // And the underlying syntax error is still surfaced.
        assert!(diags.iter().any(|d| d.id == lints::SYNTAX_ERROR));
    }

    #[test]
    fn suppresses_definition_in_recovered_field() {
        let mut fs = Fileset::new();
        fs.add(
            "vanilla/common/buildings/broken.txt",
            FileKind::Vanilla,
            b"broken = { cost = 1".to_vec(),
        );
        let mut schema = Schema::new();
        schema.define_dir("buildings", "building");

        let summary = summarize(&fs, &schema, FileId(0));
        assert!(summary.defs.is_empty());
        assert!(
            summary
                .syntax_errors
                .iter()
                .any(|error| error.fix.is_some())
        );
    }

    #[test]
    fn syntax_summary_and_diagnostic_retain_repair_metadata() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);
        let summary = analysis.summary(FileId(3)).unwrap();
        let error = summary.syntax_errors.first().unwrap();
        assert_eq!(
            error.range,
            TextRange::empty(TextSize::of(fs.source(FileId(3))))
        );
        assert_eq!(error.opening_range, Some(TextRange::span(9, 10)));
        assert_eq!(error.applicability, Some(Applicability::MachineApplicable));
        assert_eq!(error.fix.as_ref().unwrap().replacement, "}");

        let diagnostic = analysis
            .diagnostics()
            .iter()
            .find(|diagnostic| diagnostic.id == lints::SYNTAX_ERROR)
            .unwrap();
        assert_eq!(diagnostic.range, error.range);
        assert_eq!(diagnostic.fix.as_ref().unwrap().replacement, "}");
    }

    #[test]
    fn resolution_enum_three_ways() {
        let mut index = Index::new();
        let fs = {
            let mut fs = Fileset::new();
            fs.add(
                "v/common/buildings/a.txt",
                FileKind::Vanilla,
                b"castle = {}".to_vec(),
            );
            fs
        };
        index.define(
            "building".into(),
            "castle".into(),
            FileId(0),
            TextRange::span(0, 6),
        );
        index.finalize(&fs);
        assert!(matches!(
            index.resolve("building", "castle", false),
            Resolution::Found(_)
        ));
        assert_eq!(
            index.resolve("building", "ghost", false),
            Resolution::NotFound
        );
        assert_eq!(
            index.resolve("building", "ghost", true),
            Resolution::UpstreamError
        );
    }

    #[test]
    fn render_is_panic_free_on_odd_ranges() {
        let mut fs = Fileset::new();
        let f = fs.add("a.txt", FileKind::Vanilla, b"a = 1\n".to_vec());
        // A past-EOF range and a zero-width range must render without
        // panicking (Diagnostic fields are public, so callers can construct
        // any range).
        for range in [
            TextRange::span(100, 200),
            TextRange::empty(TextSize::new(3)),
        ] {
            let d = Diagnostic {
                id: lints::SYNTAX_ERROR,
                severity: Severity::Error,
                confidence: Confidence::Strong,
                file: f,
                range,
                message: "x".into(),
                help: None,
                fix: None,
            };
            let _ = render(&d, &fs, false);
        }
    }

    #[test]
    fn line_col_follows_set_source() {
        let mut fs = Fileset::new();
        let id = fs.add("a.txt", FileKind::Vanilla, b"a = 1\nbb = 2\n".to_vec());
        assert_eq!(fs.line_col(id, TextSize::new(9)).to_string(), "2:4");
        fs.set_source(id, b"bb = 2\n".to_vec());
        assert_eq!(fs.line_col(id, TextSize::new(3)).to_string(), "1:4");
    }

    #[test]
    fn goto_definition_jumps_cross_file_to_the_winner() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);

        // The events file (FileId 2) references building `fortress`.
        let events = FileId(2);
        let fref = analysis
            .reference_at(events, TextSize::new(35)) // inside `fortress` (the add_building value)
            .expect("a reference under the cursor");
        assert_eq!(
            (fref.kind.as_str(), fref.name.as_str()),
            ("building", "fortress")
        );

        // Go to definition finds the `fortress` that wins, in a different file.
        let def = analysis
            .definition_for(events, TextSize::new(35))
            .expect("a definition to jump to");
        assert_eq!(
            (def.kind.as_str(), def.name.as_str()),
            ("building", "fortress")
        );
        assert_eq!(def.file, FileId(1));
        assert!(def.winner);

        // An undefined reference (`baracks`, the typo) resolves to no definition.
        assert_eq!(
            analysis
                .reference_at(events, TextSize::new(60))
                .unwrap()
                .name,
            "baracks"
        );
        assert!(analysis.definition_for(events, TextSize::new(60)).is_none());
    }

    #[test]
    fn goto_definition_on_a_definition_resolves_the_override_winner() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);
        // From the vanilla `temple` key, go to the mod `temple` that overrides
        // it.
        let def = analysis
            .definition_for(FileId(0), TextSize::new(2))
            .expect("temple resolves");
        assert_eq!(def.name, "temple");
        assert!(def.winner);
        assert_eq!(fs.kind(def.file), FileKind::Mod(0));
    }

    #[test]
    fn find_references_collects_use_sites_across_the_project() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);

        // `fortress` is *defined* twice (FileId 1) but *referenced* once: the
        // `add_building = fortress` in the events file.
        let uses: Vec<_> = analysis.references_to("building", "fortress").collect();
        assert_eq!(uses.len(), 1, "one use site for fortress: {uses:?}");
        let (file, r) = uses[0];
        assert_eq!(file, FileId(2));
        assert_eq!(r.name, "fortress");

        // `castle` is referenced once (the mod's `upgrades_from = castle`).
        assert_eq!(analysis.references_to("building", "castle").count(), 1);
        // A name used nowhere yields nothing.
        assert_eq!(analysis.references_to("building", "nonesuch").count(), 0);
    }

    #[test]
    fn document_symbols_come_from_per_file_summaries() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);
        // The vanilla buildings file defines four buildings, in source order.
        let names: Vec<&str> = analysis
            .summary(FileId(0))
            .unwrap()
            .defs
            .iter()
            .map(|d| d.name.as_str())
            .collect();
        assert_eq!(names, ["temple", "castle", "city", "barracks"]);
    }

    #[test]
    fn new_definition_in_one_file_resolves_a_reference_in_another() {
        let (mut fs, schema) = demo();
        let linter = Linter::new(schema);

        let buildings = fs
            .id_for_path("vanilla/common/buildings/00_buildings.txt")
            .expect("the vanilla buildings file is in the set");
        assert_eq!(buildings, FileId(0));
        assert!(fs.id_for_path("nope.txt").is_none());

        let mut analysis = linter.analyze(&fs);
        assert!(
            analysis
                .diagnostics()
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains("baracks"))
        );

        // Define `baracks` in the buildings file. Only that file changes.
        let mut edited = fs.source(buildings).to_vec();
        edited.extend_from_slice(b"baracks = { cost = 1 }\n");
        fs.set_source(buildings, edited);
        linter.reanalyze(&fs, &mut analysis);
        assert!(
            !analysis
                .diagnostics()
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains("baracks"))
        );
    }
}
