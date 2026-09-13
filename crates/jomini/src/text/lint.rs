//! **Experimental** cross-file linting layer over the lossless [`syntax`] tree.
//!
//! A single [`GreenTree`](syntax::GreenTree) describes *one* file. But the
//! interesting bugs in a Paradox mod are *cross-file*: a building that references
//! a culture defined in another file, or in no file at all. Catching those needs
//! a project-wide view, so this module adds the layers a per-file parser cannot
//! provide — mirroring how rust-analyzer, Roslyn, swift-syntax, and Carbon all
//! keep name resolution in a layer *above* a purely syntactic tree:
//!
//! 1. [`Fileset`] — the project model. Every source file plus a [`FileKind`]
//!    load-order layer, so a mod definition overrides a vanilla one (rust-analyzer's
//!    `vfs`/`FileId` shape plus Paradox override semantics).
//! 2. [`FileSummary`] — an *ItemTree-style* summary. Each file is parsed once and
//!    lowered into a small, **owned**, position-independent list of definitions
//!    and references (with source ranges). The borrowing [`GreenTree`] is then
//!    dropped, which sidesteps the "thousands of trees, each borrowing its source"
//!    lifetime problem entirely.
//! 3. [`Index`] — the symbol table. A flat arena of [`Definition`]s keyed by
//!    `(kind, name)`, with override/duplicate resolution by load order.
//! 4. [`Resolution`] / [`Linter`] — the two-pass resolver. Collect *all*
//!    definitions, *then* resolve every reference; a miss is an undefined-reference
//!    [`Diagnostic`]. Resolution is a three-way [`Resolution`] (Carbon's
//!    `Found`/`NotFound`/`Poisoned` shape) so a reference inside a subtree the
//!    parser already flagged as malformed is *suppressed* rather than piling a
//!    semantic error on top of a syntactic one.
//!
//! Diagnostics carry both a [`Severity`] and a [`Confidence`] (tiger's two-axis
//! model) so a young rule set can ship its uncertain findings without drowning
//! the user. See `examples/lint.rs` for an end-to-end demo.
//!
//! This is a deliberately small **prototype**: the "schema" of what defines and
//! what references an entity is a handful of hand-written rules ([`Schema`]),
//! not a full per-game rule set, and names are compared as lossy-UTF-8 strings
//! rather than encoding-aware (a documented follow-up).

#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::text::syntax::{self, AstNode, Field, Flavor, SyntaxKind, SyntaxNode, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

// ---------------------------------------------------------------------------
// Project model: files + load order
// ---------------------------------------------------------------------------

/// An opaque handle to a file in a [`Fileset`]. Cheap to copy and hash; the
/// path and bytes are recovered through the [`Fileset`] (never carried around,
/// mirroring rust-analyzer's `FileId`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// Where a file sits in the Paradox load order. Ordered so that a later layer
/// overrides an earlier one for the same `(kind, name)`: `Vanilla < Dlc < Mod`
/// (and `Mod(0) < Mod(1)` for sub-mods). This is the minimal slice of tiger's
/// richer `FileKind` overlay that the prototype needs.
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
}

/// The set of source files under consideration, each tagged with its load-order
/// [`FileKind`]. **Owns** the file bytes, so the parsed trees (which borrow
/// their source) can come and go while the project model stays alive.
#[derive(Default)]
pub struct Fileset {
    files: Vec<FileEntry>,
}

impl Fileset {
    /// An empty fileset.
    pub fn new() -> Self {
        Fileset::default()
    }

    /// Add one file's bytes at `path` in load-order layer `kind`, returning its
    /// [`FileId`].
    pub fn add(&mut self, path: impl Into<PathBuf>, kind: FileKind, source: Vec<u8>) -> FileId {
        let id = FileId(self.files.len() as u32);
        self.files.push(FileEntry {
            path: path.into(),
            kind,
            source,
        });
        id
    }

    /// Recursively add every `*.txt` file under `root` to layer `kind`. Files
    /// are added in **globally path-sorted** order, so that [`FileId`] assignment
    /// — which same-layer override resolution uses as the load order (last wins)
    /// — matches Paradox's lexicographic load order, regardless of directory
    /// traversal. Directory **symlinks are not followed** (no cycle risk). The
    /// stored path is the real filesystem path, used for display in diagnostics.
    pub fn load_dir(
        &mut self,
        root: impl AsRef<Path>,
        kind: FileKind,
    ) -> std::io::Result<Vec<FileId>> {
        // Collect first, then sort the whole set, then assign ids — rather than
        // letting the DFS traversal order decide ids.
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

    /// The [`FileId`] previously assigned to `path` (by exact path match), if the
    /// file is in the set. An editor uses this to map an on-disk path back to its
    /// handle so it can overlay the unsaved buffer with
    /// [`set_source`](Fileset::set_source) instead of rebuilding the whole set.
    pub fn id_for_path(&self, path: impl AsRef<Path>) -> Option<FileId> {
        let path = path.as_ref();
        self.files
            .iter()
            .position(|f| f.path == path)
            .map(|i| FileId(i as u32))
    }

    /// Replace a file's source bytes in place, keeping its [`FileId`], path, and
    /// layer. This is the editor-overlay hook: an LSP server swaps in the unsaved
    /// buffer on a keystroke and re-analyzes, without disturbing any other file's
    /// handle. Re-run [`Linter::analyze`] afterwards to refresh the snapshot.
    pub fn set_source(&mut self, id: FileId, source: Vec<u8>) {
        self.files[id.0 as usize].source = source;
    }
}

// ---------------------------------------------------------------------------
// Schema: the (tiny, hand-written) notion of what defines / references an entity
// ---------------------------------------------------------------------------

/// The entity kind of a definition or reference, e.g. `"building"`. A
/// real linter would intern these; the prototype keeps them as owned strings
/// for clarity.
pub type Kind = String;

/// A minimal, hand-written description of which files *define* entities and
/// which field keys *reference* them — the prototype's stand-in for a full
/// per-game rule set (the "rules-as-code" v1 from the design analysis).
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

    /// Declare that top-level keys in any file located under a `dir` directory
    /// component define an entity of kind `kind` (e.g. `("buildings",
    /// "building")`).
    pub fn define_dir(&mut self, dir: &str, kind: &str) -> &mut Self {
        self.definition_dirs
            .insert(dir.to_string(), kind.to_string());
        self
    }

    /// Declare that a field whose key is `key` references an entity of kind
    /// `kind` (e.g. `("add_building", "building")`).
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

// ---------------------------------------------------------------------------
// Per-file summary (the "ItemTree": owned, position-independent, tree-free)
// ---------------------------------------------------------------------------

/// A definition extracted from a file: a `(kind, name)` plus where it sits.
#[derive(Debug, Clone)]
pub struct DefItem {
    pub kind: Kind,
    pub name: String,
    pub range: (u32, u32),
}

/// A reference extracted from a file: a `(kind, name)` use site, plus whether
/// it lives inside a subtree the parser flagged as malformed (so resolution can
/// suppress a cascade — this is what the new [`NodeFlags`](syntax::NodeFlags)
/// `HAS_ERROR` bit buys us, looked up in O(1)).
#[derive(Debug, Clone)]
pub struct RefItem {
    pub kind: Kind,
    pub name: String,
    pub range: (u32, u32),
    pub in_error_subtree: bool,
}

/// The owned, tree-free summary of one file: its definitions, references, and
/// any syntax errors. Produced by [`summarize`], after which the parsed
/// [`GreenTree`](syntax::GreenTree) is dropped — the summary outlives it. This
/// is the invalidation barrier rust-analyzer's `ItemTree` provides: it depends
/// only on the items in the file, not on byte offsets within value bodies.
pub struct FileSummary {
    pub file: FileId,
    pub defs: Vec<DefItem>,
    pub refs: Vec<RefItem>,
    pub syntax_errors: Vec<(String, (u32, u32))>,
}

fn decode(s: crate::Scalar) -> String {
    String::from_utf8_lossy(s.as_bytes()).into_owned()
}

/// Parse one file and lower it into a [`FileSummary`]. The [`GreenTree`] is
/// local to this function — every datum kept is owned — so no tree outlives the
/// call, and the whole project's worth of summaries can be held at once without
/// the borrow-one-slice lifetime headache.
pub fn summarize(fileset: &Fileset, schema: &Schema, file: FileId) -> FileSummary {
    let source = fileset.source(file);
    let tree = syntax::parse_with(source, Flavor::default());

    let mut defs = Vec::new();
    // Top-level keys define entities only when the file lives in a known
    // definition directory.
    if let Some(kind) = schema.definition_kind(fileset.path(file)) {
        for field in tree.ast().fields() {
            if let Some(key) = field.key() {
                defs.push(DefItem {
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

    let syntax_errors = tree
        .errors()
        .iter()
        .map(|e| (e.message.clone(), e.range))
        .collect();

    FileSummary {
        file,
        defs,
        refs,
        syntax_errors,
    }
}

/// Recursively collect reference uses. `in_error` carries down whether any
/// enclosing subtree was flagged malformed by the parser, so a reference inside
/// a broken block is recorded as suppressible. The O(1) [`SyntaxNode::has_error`]
/// check means we learn this without re-walking the subtree.
fn collect_refs(node: SyntaxNode<'_, '_>, schema: &Schema, in_error: bool, out: &mut Vec<RefItem>) {
    // A reference is "in an error subtree" when the parser had trouble here:
    // either an explicit recovery element (the O(1) HAS_ERROR flag covers stray
    // `}` Bogus nodes), or an *unclosed* block — which is only ever a diagnostic,
    // creating no error element, so it must be detected structurally (no
    // `CloseBrace` child). Everything inside such a region is suspect, so a
    // failed name lookup there is suppressed rather than reported as a hard miss.
    let unclosed_block = node.kind() == SyntaxKind::Block
        && !node
            .child_tokens()
            .any(|t| t.kind() == SyntaxKind::CloseBrace);
    let in_error = in_error || node.has_error() || unclosed_block;

    if let Some(field) = Field::cast(node)
        && let (Some(key), Some(value)) = (field.key(), field.value())
        && let Some(kind) = schema.reference_keys.get(&decode(key.as_scalar()))
        && let Value::Scalar(tok) = value
    {
        out.push(RefItem {
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

// ---------------------------------------------------------------------------
// Symbol table (the Index): a flat arena of definitions, keyed by (kind, name)
// ---------------------------------------------------------------------------

/// An index into [`Index::definitions`]; a Carbon-style typed handle into a flat
/// arena rather than a pointer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DefId(pub u32);

/// A registered definition: a `(kind, name)` plus the file and range it came
/// from. `winner` marks the definition that wins after override resolution.
#[derive(Debug, Clone)]
pub struct Definition {
    pub kind: Kind,
    pub name: String,
    pub file: FileId,
    pub range: (u32, u32),
    pub winner: bool,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct Key {
    kind: Kind,
    name: String,
}

/// The outcome of resolving a reference against the [`Index`]. The third arm —
/// Carbon's `Poisoned` — distinguishes "genuinely undefined" from "the
/// reference sits in a region the parser already marked broken", which the
/// resolver suppresses to avoid cascade noise.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Resolution {
    /// The name resolves to a definition.
    Found(DefId),
    /// No definition of this `(kind, name)` exists in any loaded file.
    NotFound,
    /// Unresolved, but inside a malformed subtree — suppress the diagnostic.
    UpstreamError,
}

/// The project-wide symbol table: a flat arena of [`Definition`]s plus a
/// `(kind, name)` → definitions map. Built by registering every definition from
/// every [`FileSummary`], then [`finalize`](Index::finalize)d to pick winners.
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

    /// Register one definition, returning its [`DefId`].
    pub fn define(&mut self, kind: Kind, name: String, file: FileId, range: (u32, u32)) -> DefId {
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

    /// All registered definitions (read-only).
    pub fn definitions(&self) -> &[Definition] {
        &self.definitions
    }

    /// Resolve a reference. A present `(kind, name)` is [`Resolution::Found`];
    /// an absent one is [`Resolution::NotFound`], or [`Resolution::UpstreamError`]
    /// when the use site is inside a malformed subtree.
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

    /// The closest *winning* definition name of the same `kind` within a small
    /// edit distance of `name` — the "did you mean?" candidate for an undefined
    /// reference. Returns `None` if nothing is close enough. Only winners are
    /// suggested, so the proposed name is one that would actually resolve. Must
    /// run after [`finalize`](Index::finalize).
    pub fn suggest(&self, kind: &str, name: &str) -> Option<&str> {
        // A length-scaled threshold: tight for short identifiers, looser (capped
        // at 3) for long ones, so `baracks`→`barracks` (1) is offered but
        // unrelated names are not.
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

    /// Resolve override/duplicate conflicts across the load order and pick a
    /// winner per `(kind, name)`. Emits informational diagnostics for mod-over-
    /// vanilla overrides and warnings for duplicate definitions within one layer.
    ///
    /// This is the closest the prototype comes to cwtools' fixpoint index build,
    /// kept as a clean single pass (tiger-style) since no subtype-style
    /// conditional classification is involved.
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

        diags.sort_by_key(|d| (d.file, d.range.0));
        diags
    }
}

/// Levenshtein edit distance between two byte strings (the classic two-row DP).
/// Used by [`Index::suggest`]; inputs are short identifiers, so the O(n·m) cost
/// is negligible.
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

// ---------------------------------------------------------------------------
// Diagnostics: stable id + Severity + Confidence (Roslyn descriptor + tiger axes)
// ---------------------------------------------------------------------------

/// Stable lint ids. A real linter would attach a description/category/default-
/// severity to each (Roslyn's `DiagnosticDescriptor`); the prototype keeps just
/// the stable string id that users would reference to configure or suppress.
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
    /// A style/tidiness nit.
    Untidy,
    /// Probably wrong.
    Warning,
    /// Almost certainly wrong.
    Error,
    /// Fatal — the file cannot be meaningfully processed further.
    Fatal,
}

/// How sure the linter is that a finding is real — tiger's second axis, the
/// dial that lets a young rule set surface uncertain findings without crying
/// wolf.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Confidence {
    /// Likely a false positive; show only when asked.
    Weak,
    /// A reasonable finding.
    Reasonable,
    /// High confidence.
    Strong,
}

/// A machine-applicable fix: replace the bytes in `range` with `replacement`.
/// The range comes from a syntax element (a [`SyntaxToken`](syntax::SyntaxToken)
/// the rule points at), so a fix is "rewrite this token" expressed as a text
/// edit — the practical, conflict-checkable form of "splice the tree, then
/// reprint". Apply with [`apply_fixes`]; the result is guaranteed to reparse,
/// and you can run [`format`](syntax::format) over it for the house style.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fix {
    /// The half-open byte range to replace.
    pub range: (u32, u32),
    /// The text to substitute in.
    pub replacement: String,
}

/// One finding: a stable id, the two axes, and a located message. Carries a
/// [`FileId`] + range rather than a tree reference, so it long outlives the
/// parse and is rendered later against the [`Fileset`]. May also carry a
/// human-readable [`help`](Diagnostic::help) note and a machine-applicable
/// [`fix`](Diagnostic::fix).
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub id: &'static str,
    pub severity: Severity,
    pub confidence: Confidence,
    pub file: FileId,
    pub range: (u32, u32),
    pub message: String,
    /// An optional secondary hint (e.g. "did you mean `barracks`?").
    pub help: Option<String>,
    /// An optional machine-applicable fix.
    pub fix: Option<Fix>,
}

// ---------------------------------------------------------------------------
// The linter: the two-pass orchestration
// ---------------------------------------------------------------------------

/// The cross-file linter: a [`Schema`] plus the two-pass `collect → resolve`
/// run. Holds no per-file state — everything flows through the [`Fileset`] and
/// the [`Index`].
pub struct Linter {
    schema: Schema,
}

impl Linter {
    /// A linter driven by `schema`.
    pub fn new(schema: Schema) -> Self {
        Linter { schema }
    }

    /// Lint a whole [`Fileset`], returning just the diagnostics (sorted by file
    /// then position). A convenience wrapper over [`analyze`](Linter::analyze) for
    /// batch callers who only want the findings; an editor/LSP should call
    /// `analyze` instead and keep the [`Analysis`] around to answer navigation
    /// queries off the same data.
    pub fn run(&self, fileset: &Fileset) -> Vec<Diagnostic> {
        self.analyze(fileset).into_diagnostics()
    }

    /// Lint a whole [`Fileset`] and **retain** the working state — the [`Index`]
    /// and every per-file [`FileSummary`] — in a queryable [`Analysis`].
    ///
    /// Pass 1 summarizes every file and builds the [`Index`]; pass 2 surfaces
    /// syntax errors, resolves every reference, and (unlike [`run`](Linter::run))
    /// records each use site in a `(kind, name)` → use-sites reverse map. The
    /// diagnostics produced are identical to `run`; the difference is that the
    /// definitions, reference use-sites, and that reverse map survive the call —
    /// so go-to-definition / find-references / symbol queries cost a lookup
    /// rather than a re-lint.
    pub fn analyze(&self, fileset: &Fileset) -> Analysis {
        // Pass 1: lower every file to a summary, then register definitions.
        let summaries: Vec<FileSummary> = fileset
            .ids()
            .map(|id| summarize(fileset, &self.schema, id))
            .collect();

        let mut index = Index::new();
        for s in &summaries {
            for d in &s.defs {
                index.define(d.kind.clone(), d.name.clone(), s.file, d.range);
            }
        }
        let mut diags = index.finalize(fileset);

        // Pass 2: surface syntax errors, resolve references, and build the
        // reverse use-site map that find-references reads.
        let mut refs_by_name: HashMap<Key, Vec<(FileId, u32)>> = HashMap::new();
        for s in &summaries {
            for (message, range) in &s.syntax_errors {
                diags.push(Diagnostic {
                    id: lints::SYNTAX_ERROR,
                    severity: Severity::Error,
                    confidence: Confidence::Strong,
                    file: s.file,
                    range: *range,
                    message: message.clone(),
                    help: None,
                    fix: None,
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
                        // Offer the nearest defined name of the same kind as a
                        // "did you mean?" hint, and — since a reference is a
                        // single token — a machine-applicable rename fix.
                        let (help, fix) = match index.suggest(&r.kind, &r.name) {
                            Some(name) => (
                                Some(format!("did you mean `{name}`?")),
                                Some(Fix { range: r.range, replacement: name.to_string() }),
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

        diags.sort_by_key(|d| (d.file, d.range.0, d.range.1));
        Analysis {
            index,
            summaries,
            diagnostics: diags,
            refs_by_name,
        }
    }
}

// ---------------------------------------------------------------------------
// Analysis: the retained, queryable result an editor/LSP reads
// ---------------------------------------------------------------------------

/// A queryable snapshot of a linted project: the diagnostics, plus the retained
/// [`Index`] and per-file [`FileSummary`]s that produced them. Built by
/// [`Linter::analyze`].
///
/// This is the editor/LSP substrate. [`Linter::run`] discards its working state;
/// `Analysis` keeps it, so go-to-definition, find-references, and document/
/// workspace symbols are all reads off this one structure. Like [`FileSummary`]
/// it is fully **owned** — it holds no borrow on any parse tree or on the
/// [`Fileset`] — so it can live in a long-running server and be replaced wholesale
/// when a file changes (re-run [`Linter::analyze`] on the updated [`Fileset`]).
pub struct Analysis {
    index: Index,
    /// `summaries[i]` is the summary of `FileId(i)` (built in `fileset.ids()`
    /// order, which is `0..len`).
    summaries: Vec<FileSummary>,
    diagnostics: Vec<Diagnostic>,
    /// `(kind, name)` → the use sites of that name, each `(file, index into that
    /// file's `summary.refs`)`. The reverse of the forward resolution the
    /// [`Index`] does, and the thing find-references needs.
    refs_by_name: HashMap<Key, Vec<(FileId, u32)>>,
}

impl Analysis {
    /// All diagnostics, sorted by file then position (identical to what
    /// [`Linter::run`] returns).
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Consume the analysis, yielding just its diagnostics.
    pub fn into_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    /// The project-wide symbol table.
    pub fn index(&self) -> &Index {
        &self.index
    }

    /// Every registered [`Definition`] — the workspace-symbol source.
    pub fn definitions(&self) -> &[Definition] {
        self.index.definitions()
    }

    /// The owned [`FileSummary`] of `file` (its defs, refs, and syntax errors),
    /// or `None` if `file` is not in the analyzed set. The per-file
    /// document-symbol source is `summary(file).defs`.
    pub fn summary(&self, file: FileId) -> Option<&FileSummary> {
        self.summaries.get(file.0 as usize)
    }

    /// The reference use site whose byte range covers `offset` in `file`, if any.
    /// Ranges are matched inclusively at both ends so a cursor resting just after
    /// the last character of a token still hits it (references are sparse, so this
    /// never double-matches). The hit-test is a linear scan — fine for the small
    /// game files this layer targets; a spatial index is a later optimization.
    pub fn reference_at(&self, file: FileId, offset: u32) -> Option<&RefItem> {
        let summary = self.summary(file)?;
        summary
            .refs
            .iter()
            .find(|r| r.range.0 <= offset && offset <= r.range.1)
    }

    /// The definition site (its name token) whose byte range covers `offset` in
    /// `file`, if any. Inclusive at both ends, like [`reference_at`].
    pub fn definition_at(&self, file: FileId, offset: u32) -> Option<&DefItem> {
        let summary = self.summary(file)?;
        summary
            .defs
            .iter()
            .find(|d| d.range.0 <= offset && offset <= d.range.1)
    }

    /// Go-to-definition for the token at `offset` in `file`: resolve a reference
    /// there to its winning [`Definition`], or — if `offset` rests on a
    /// definition's own name — return that entity's winner (so jumping from a
    /// shadowed definition lands on the one that actually wins). `None` when there
    /// is nothing resolvable under the cursor (plain syntax, or an *undefined*
    /// reference, which resolves to no definition).
    pub fn definition_for(&self, file: FileId, offset: u32) -> Option<&Definition> {
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

    /// Find-references: every use site of `(kind, name)` across the whole project,
    /// as `(file, &RefItem)`. Because overrides share a single name, every use of
    /// that name conceptually targets the winning definition, so this is exactly
    /// the set find-references should return. Empty if the name is used nowhere.
    pub fn references_to<'s>(
        &'s self,
        kind: &str,
        name: &str,
    ) -> impl Iterator<Item = (FileId, &'s RefItem)> + 's {
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

// ---------------------------------------------------------------------------
// Rendering: lazy line/column + a compiler-style console report
// ---------------------------------------------------------------------------

/// Convert a byte `offset` into 1-based `(line, column)` by scanning newlines.
/// Carbon's lazy location model: positions are stored as plain offsets and
/// resolved to line/column only when a diagnostic is actually rendered.
pub fn line_col(source: &[u8], offset: u32) -> (usize, usize) {
    let offset = (offset as usize).min(source.len());
    let mut line = 1;
    let mut col = 1;
    for &b in &source[..offset] {
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// The bytes of the line containing `offset`, and the offset of that line start.
fn line_bytes(source: &[u8], offset: u32) -> (&[u8], usize) {
    let offset = (offset as usize).min(source.len());
    let start = source[..offset]
        .iter()
        .rposition(|&b| b == b'\n')
        .map_or(0, |p| p + 1);
    let end = source[start..]
        .iter()
        .position(|&b| b == b'\n')
        .map_or(source.len(), |p| start + p);
    (&source[start..end], start)
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

/// Render one diagnostic as a multi-line, compiler-style report against the
/// fileset's source. With `color`, severity labels and the caret are ANSI-
/// colored. This is the separate *rendering* layer (Roslyn / rust-analyzer keep
/// diagnostic production and rendering apart): it touches only owned data + the
/// source bytes, never a parsed tree.
pub fn render(diag: &Diagnostic, fileset: &Fileset, color: bool) -> String {
    let source = fileset.source(diag.file);
    let (line, col) = line_col(source, diag.range.0);
    let (line_src, line_start) = line_bytes(source, diag.range.0);
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

    // Caret span within the line, clamped to the line's end.
    let caret_col = diag.range.0 as usize - line_start;
    // `saturating_sub` guards a hand-built `Diagnostic` with an inverted range
    // (`Diagnostic`'s fields are public): a plain `-` would underflow-panic in
    // debug and balloon `"^".repeat(span)` in release.
    let span = (diag.range.1.saturating_sub(diag.range.0)).max(1) as usize;
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
    // A "did you mean?" hint, and a marker when an automatic fix is available.
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

// ---------------------------------------------------------------------------
// Autofix: apply machine-applicable fixes as a text-edit splice
// ---------------------------------------------------------------------------

/// Apply [`Fix`]es to `source`, returning the rewritten bytes. Fixes are sorted
/// by position and applied left-to-right; any fix that **overlaps** one already
/// applied is skipped (a fix-all must never emit conflicting edits). The result
/// is plain text that reparses cleanly — run [`format`](syntax::format) over it
/// if you also want the house style ("splice, then reprint").
///
/// This is the deliberately simple, robust form of tree rewriting: because each
/// fix targets a single element's byte range (e.g. a reference token), splicing
/// text is equivalent to rebuilding the green subtree but needs no width
/// recomputation — and it is exactly the LSP `TextEdit` model.
pub fn apply_fixes(source: &[u8], fixes: &[Fix]) -> Vec<u8> {
    let mut sorted: Vec<&Fix> = fixes.iter().collect();
    sorted.sort_by_key(|f| (f.range.0, f.range.1));

    let mut out = Vec::with_capacity(source.len());
    let mut cursor = 0u32; // bytes of `source` already copied
    for fix in sorted {
        let (start, end) = fix.range;
        // Skip an out-of-order/overlapping or inverted edit rather than corrupt
        // the output.
        if start < cursor || end < start || end as usize > source.len() {
            continue;
        }
        out.extend_from_slice(&source[cursor as usize..start as usize]);
        out.extend_from_slice(fix.replacement.as_bytes());
        cursor = end;
    }
    out.extend_from_slice(&source[cursor as usize..]);
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a tiny two-layer fileset: vanilla buildings + a mod that overrides
    /// and references them, plus a broken file. Returns the fileset and the
    /// demo schema.
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

    #[test]
    fn detects_cross_file_undefined_reference() {
        let (fs, schema) = demo();
        let diags = Linter::new(schema).run(&fs);

        // `baracks` is referenced but defined nowhere → an undefined-reference.
        let undef: Vec<_> = diags
            .iter()
            .filter(|d| d.id == lints::UNDEFINED_REFERENCE)
            .collect();
        assert_eq!(
            undef.len(),
            1,
            "exactly one undefined reference: {diags:#?}"
        );
        assert!(undef[0].message.contains("baracks"));
        assert_eq!(undef[0].severity, Severity::Error);
    }

    #[test]
    fn suggests_and_offers_fix_for_typo() {
        let (fs, schema) = demo();
        let diags = Linter::new(schema).run(&fs);
        let d = diags
            .iter()
            .find(|d| d.id == lints::UNDEFINED_REFERENCE)
            .expect("the undefined-reference diagnostic");

        // The nearest defined building name is suggested as a help + a fix.
        assert!(
            d.help.as_deref().unwrap_or("").contains("barracks"),
            "help: {:?}",
            d.help
        );
        let fix = d.fix.as_ref().expect("a machine-applicable fix");
        assert_eq!(fix.replacement, "barracks");

        // Applying the fix rewrites the typo in place; the result parses clean
        // and the bad token is gone (`barracks` now resolves against the index).
        let fixed = apply_fixes(fs.source(d.file), std::slice::from_ref(fix));
        let fixed_text = String::from_utf8_lossy(&fixed);
        assert!(fixed_text.contains("add_building = barracks"));
        assert!(!fixed_text.contains("baracks\n") && !fixed_text.contains("baracks ")); // no stray typo
        // The fix reparses without errors (a rename of one token stays well-formed).
        assert!(syntax::parse(&fixed).errors().is_empty());
    }

    #[test]
    fn apply_fixes_splices_and_skips_overlap() {
        // A single in-range rename.
        let renamed = apply_fixes(
            b"add_building = baracks",
            &[Fix {
                range: (15, 22),
                replacement: "barracks".into(),
            }],
        );
        assert_eq!(renamed, b"add_building = barracks");

        // Overlapping/out-of-order edits are skipped, never corrupting output.
        let out = apply_fixes(
            b"abcdef",
            &[
                Fix {
                    range: (0, 3),
                    replacement: "X".into(),
                },
                Fix {
                    range: (2, 5),
                    replacement: "Y".into(),
                }, // overlaps the first
            ],
        );
        assert_eq!(out, b"Xdef");
    }

    #[test]
    fn resolves_cross_layer_definitions() {
        let (fs, schema) = demo();
        let diags = Linter::new(schema).run(&fs);
        // `fortress` (mod), `castle` (vanilla, from the mod's upgrades_from), and
        // `temple` (mod override) all resolve, so none appear as undefined.
        for name in ["fortress", "castle", "temple"] {
            assert!(
                !diags
                    .iter()
                    .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains(name)),
                "`{name}` should resolve across files/layers"
            );
        }
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
        index.define("building".into(), "castle".into(), FileId(0), (0, 6));
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
        // An inverted range, a past-EOF range, and a zero-width range must all
        // render without panicking (Diagnostic fields are public, so callers
        // can construct any range).
        for range in [(5, 2), (100, 200), (3, 3)] {
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
    fn line_col_is_one_based() {
        let src = b"a = 1\nbb = 2\n";
        assert_eq!(line_col(src, 0), (1, 1));
        assert_eq!(line_col(src, 6), (2, 1)); // first byte of line 2
        assert_eq!(line_col(src, 9), (2, 4));
    }

    #[test]
    fn analyze_diagnostics_match_run() {
        // `analyze` must produce exactly the diagnostics `run` does — `run` is now
        // a thin wrapper over it.
        let (fs, schema) = demo();
        let via_run = Linter::new(schema).run(&fs);
        let (fs2, schema2) = demo();
        let via_analyze = Linter::new(schema2).analyze(&fs2).into_diagnostics();
        assert_eq!(via_run.len(), via_analyze.len());
        for (a, b) in via_run.iter().zip(&via_analyze) {
            assert_eq!((a.id, a.file, a.range), (b.id, b.file, b.range));
        }
    }

    #[test]
    fn goto_definition_jumps_cross_file_to_the_winner() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);

        // The events file (FileId 2) references building `fortress`.
        let events = FileId(2);
        let fref = analysis
            .reference_at(events, 35) // inside `fortress` (the add_building value)
            .expect("a reference under the cursor");
        assert_eq!(
            (fref.kind.as_str(), fref.name.as_str()),
            ("building", "fortress")
        );

        // Go-to-definition lands on the *winning* fortress definition, which lives
        // in the mod buildings file (FileId 1) — a jump no single-file view affords.
        let def = analysis
            .definition_for(events, 35)
            .expect("a definition to jump to");
        assert_eq!(
            (def.kind.as_str(), def.name.as_str()),
            ("building", "fortress")
        );
        assert_eq!(def.file, FileId(1));
        assert!(def.winner);

        // An undefined reference (`baracks`, the typo) resolves to no definition.
        let baracks = analysis
            .reference_at(events, 60)
            .filter(|r| r.name == "baracks")
            .expect("the baracks reference");
        assert_eq!(baracks.name, "baracks");
        assert!(analysis.definition_for(events, 60).is_none());
    }

    #[test]
    fn goto_definition_on_a_definition_resolves_the_override_winner() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);
        // Cursor on the *vanilla* `temple` definition key (FileId 0, offset 2)
        // jumps to whichever `temple` actually wins — the mod override (FileId 1).
        let def = analysis
            .definition_for(FileId(0), 2)
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
    fn overlay_set_source_then_reanalyze_resolves_the_typo() {
        let (mut fs, schema) = demo();
        let linter = Linter::new(schema);

        // Path → id round-trip, and a miss for an unknown path.
        let buildings = fs
            .id_for_path("vanilla/common/buildings/00_buildings.txt")
            .expect("the vanilla buildings file is in the set");
        assert_eq!(buildings, FileId(0));
        assert!(fs.id_for_path("nope.txt").is_none());

        // Before: `baracks` (typo) is referenced but defined nowhere.
        let before = linter.analyze(&fs);
        assert!(
            before
                .diagnostics()
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains("baracks"))
        );

        // Overlay an edit that *defines* `baracks`, as an editor would on a
        // keystroke, then re-analyze the same fileset.
        let mut edited = fs.source(buildings).to_vec();
        edited.extend_from_slice(b"baracks = { cost = 1 }\n");
        fs.set_source(buildings, edited);
        let after = linter.analyze(&fs);

        // The undefined reference is gone — the overlay fed straight into the index.
        assert!(
            !after
                .diagnostics()
                .iter()
                .any(|d| d.id == lints::UNDEFINED_REFERENCE && d.message.contains("baracks"))
        );
    }
}
