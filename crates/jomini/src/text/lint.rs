//! **Experimental** cross-file linting over the [`lossless`] tree.
//!
//! The parser checks one file. Many errors in a mod are in the relation
//! between files: a building that refers to a culture that a different file
//! defines, or that no file defines. This module checks a base game and its
//! mods together.
//!
//! # Find a reference that no file defines
//!
//! The base game defines `barracks`, and a mod event refers to `baracks`:
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
//!     b"my_event = {\n    add_building = baracks\n}\n".to_vec(),
//! );
//!
//! let mut schema = Schema::new();
//! schema
//!     .define_dir("buildings", "building")
//!     .reference_key("add_building", "building");
//!
//! let diagnostics = Linter::new(schema).run(&fileset);
//! let [diagnostic] = diagnostics.as_slice() else { panic!("expected one diagnostic") };
//! assert_eq!(diagnostic.id, lints::UNDEFINED_REFERENCE);
//! assert_eq!(diagnostic.file, events);
//! assert_eq!(diagnostic.help.as_deref(), Some("did you mean `barracks`?"));
//! assert_eq!(
//!     render(diagnostic, &fileset, false),
//!     "\
//! error[undefined-reference]: no building named `baracks` is defined in any loaded file
//!      --> events/mod_events.txt:2:20
//!      |
//!    2 |     add_building = baracks
//!      |                    ^^^^^^^
//!      = help: did you mean `barracks`? (fixable with --fix)
//! "
//! );
//! ```
//!
//! `examples/lint.rs` runs the linter on a directory with a base game and a
//! mod.
//!
//! # How the linter works
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
//! Each diagnostic has a [`Severity`] and a [`Confidence`].
//!
//! # Schema
//!
//! The [`Schema`] tells which files define an entity and which keys refer to
//! an entity. It is a small set of rules that you write, not a full rule set
//! for a game. A name is its UTF-8 text, or its Windows-1252 text when its
//! bytes are not UTF-8, so two names with different bytes are different
//! names. A fix holds only ASCII text, because the linter does not know the
//! encoding of each file.
//!
//! # Variables
//!
//! The linter also [resolves](lossless::resolve) the `@name` variables and
//! the `@[ ... ]` calculations of each file. A variable that a file does not
//! define is a reference of the kind [`Kind::Variable`] to a definition in a
//! [`Schema::variable_dir`] file.

#![allow(missing_docs)] // experimental surface; docs land as the API stabilizes

use crate::text::lossless::{
    self, AstNode, DirectiveKind, Field, Flavor, LineCol, LineIndex, ReaderDirective, ScalarToken,
    SyntaxKind, SyntaxNode, TextEdit, TextRange, TextSize, Value,
    resolve::{ResolveErrorKind, resolve},
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
    /// The path relative to the root of the game or the mod. A file of a
    /// later layer at the same game path replaces the file.
    game_path: PathBuf,
    kind: FileKind,
    source: Vec<u8>,
    /// Increases each time the source changes.
    revision: u64,
    /// Made on the first request, and cleared when the source changes.
    lines: OnceLock<LineIndex>,
    /// Whether the source is UTF-8. Found on the first request, and cleared
    /// when the source changes.
    utf8: OnceLock<bool>,
    /// The file is not a part of the set: see [`Fileset::remove`].
    removed: bool,
}

/// The source files of a project, each with its load-order [`FileKind`]. The
/// fileset owns the bytes of each file.
pub struct Fileset {
    /// Identifies this fileset, so that [`Linter::reanalyze`] does not reuse
    /// the summaries of a different fileset.
    tag: u64,
    files: Vec<FileEntry>,
    /// The file at each path, for each file that is not removed.
    by_path: HashMap<PathBuf, FileId>,
    /// The files that the games load, in load order. Made on the first
    /// request, and cleared when a file is added or removed. A change of a
    /// source does not change it.
    load_order: OnceLock<Vec<FileId>>,
}

impl Default for Fileset {
    fn default() -> Self {
        static NEXT_TAG: AtomicU64 = AtomicU64::new(0);
        Fileset {
            tag: NEXT_TAG.fetch_add(1, Ordering::Relaxed),
            files: Vec::new(),
            by_path: HashMap::new(),
            load_order: OnceLock::new(),
        }
    }
}

impl Fileset {
    /// An empty fileset.
    pub fn new() -> Self {
        Fileset::default()
    }

    /// Add a file at `path` in the layer `kind`, and return its [`FileId`].
    /// The path is also the game path of the file: see
    /// [`add_with_game_path`](Fileset::add_with_game_path).
    pub fn add(&mut self, path: impl Into<PathBuf>, kind: FileKind, source: Vec<u8>) -> FileId {
        let path = path.into();
        self.add_with_game_path(path.clone(), path, kind, source)
    }

    /// Add a file at `path` in the layer `kind`, with the path `game_path`
    /// relative to the root of the game or the mod, such as
    /// `common/buildings/00_buildings.txt`. A file of a later layer at the
    /// same game path replaces the file, as the games load them: the linter
    /// then reads only the file that replaces it.
    ///
    /// A path is one file. When the set has a file at `path`, the function
    /// gives that file the new source, game path, and layer, and returns its
    /// id.
    ///
    /// ```
    /// use jomini::text::lint::{FileKind, Fileset, Linter, Schema};
    ///
    /// let mut fileset = Fileset::new();
    /// let game = "common/buildings/00.txt";
    /// fileset.add_with_game_path("game/common/buildings/00.txt", game, FileKind::Vanilla, b"barracks = {}".to_vec());
    /// fileset.add_with_game_path("mod/common/buildings/00.txt", game, FileKind::Mod(0), Vec::new());
    /// fileset.add("events/e.txt", FileKind::Mod(0), b"add_building = barracks".to_vec());
    ///
    /// let mut schema = Schema::new();
    /// schema.define_dir("buildings", "building").reference_key("add_building", "building");
    /// let diagnostics = Linter::new(schema).run(&fileset);
    /// assert_eq!(diagnostics[0].message, "no building named `barracks` is defined in any loaded file");
    /// ```
    pub fn add_with_game_path(
        &mut self,
        path: impl Into<PathBuf>,
        game_path: impl Into<PathBuf>,
        kind: FileKind,
        source: Vec<u8>,
    ) -> FileId {
        let path = path.into();
        self.load_order = OnceLock::new();
        // One path is one file: a second addition gives the file a new
        // source, game path, and layer, and keeps its id.
        if let Some(&id) = self.by_path.get(&path) {
            let file = &mut self.files[id.0 as usize];
            file.game_path = game_path.into();
            file.kind = kind;
            file.source = source;
            file.revision += 1;
            file.lines = OnceLock::new();
            file.utf8 = OnceLock::new();
            return id;
        }
        let id = FileId(self.files.len() as u32);
        self.by_path.insert(path.clone(), id);
        self.files.push(FileEntry {
            path,
            game_path: game_path.into(),
            kind,
            source,
            revision: 0,
            lines: OnceLock::new(),
            utf8: OnceLock::new(),
            removed: false,
        });
        id
    }

    /// Add each `*.txt` file under `root` to the layer `kind`, and return their
    /// ids. The game path of each file is its path relative to `root`, so a
    /// mod file replaces the base game file at the same path.
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
            let game_path = path
                .strip_prefix(root.as_ref())
                .unwrap_or(&path)
                .to_path_buf();
            ids.push(self.add_with_game_path(path, game_path, kind, source));
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

    /// Return `true` when the source of a file is UTF-8. The first call for a
    /// file reads the source.
    fn is_utf8(&self, id: FileId) -> bool {
        let file = &self.files[id.0 as usize];
        *file
            .utf8
            .get_or_init(|| std::str::from_utf8(&file.source).is_ok())
    }

    /// The load-order layer of a file.
    pub fn kind(&self, id: FileId) -> FileKind {
        self.files[id.0 as usize].kind
    }

    /// The path of a file relative to the root of the game or the mod. See
    /// [`add_with_game_path`](Fileset::add_with_game_path).
    pub fn game_path(&self, id: FileId) -> &Path {
        &self.files[id.0 as usize].game_path
    }

    /// The files that the games load, in load order: by layer, and inside a
    /// layer by game path. A file that a later layer replaces is not loaded.
    fn load_order(&self) -> &[FileId] {
        self.load_order.get_or_init(|| {
            let replaced = self.replaced();
            let mut order: Vec<FileId> = self.ids().filter(|id| !replaced.contains(id)).collect();
            // The bytes of a path compare faster than its components, and the
            // order of the bytes is the order of the names in a directory.
            order.sort_unstable_by_key(|&id| {
                let game_path = self.game_path(id).as_os_str().as_encoded_bytes();
                (self.kind(id), game_path, id)
            });
            order
        })
    }

    /// The files that a file of a later layer at the same game path
    /// replaces. Inside one layer, a later file replaces an earlier one.
    fn replaced(&self) -> std::collections::HashSet<FileId> {
        // The bytes of a path hash faster than its components.
        let mut loaded: HashMap<&[u8], FileId> = HashMap::new();
        let mut replaced = std::collections::HashSet::new();
        for id in self.ids() {
            let file = &self.files[id.0 as usize];
            let game_path = file.game_path.as_os_str().as_encoded_bytes();
            match loaded.get(game_path) {
                Some(&other) if self.kind(other) > file.kind => {
                    replaced.insert(id);
                }
                Some(&other) => {
                    replaced.insert(other);
                    loaded.insert(game_path, id);
                }
                None => {
                    loaded.insert(game_path, id);
                }
            }
        }
        replaced
    }

    /// Every [`FileId`] in the set. A removed file is not in the set.
    pub fn ids(&self) -> impl Iterator<Item = FileId> + '_ {
        self.all_ids()
            .filter(|id| !self.files[id.0 as usize].removed)
    }

    /// Every [`FileId`] that the set gave, also of a removed file.
    fn all_ids(&self) -> impl Iterator<Item = FileId> + use<> {
        (0..self.files.len() as u32).map(FileId)
    }

    /// Remove a file from the set, as when a person deletes it, and return
    /// `true` when the set had it. The [`FileId`] of each other file does
    /// not change. A removed file has no source, and it does not replace a
    /// file at its game path. Then call [`Linter::reanalyze`].
    ///
    /// ```
    /// use jomini::text::lint::{FileKind, Fileset, Linter, Schema};
    ///
    /// let mut fileset = Fileset::new();
    /// let game = "common/buildings/00.txt";
    /// fileset.add_with_game_path("game/".to_owned() + game, game, FileKind::Vanilla, b"barracks = {}".to_vec());
    /// let empty = fileset.add_with_game_path("mod/".to_owned() + game, game, FileKind::Mod(0), Vec::new());
    /// fileset.add("events/e.txt", FileKind::Mod(0), b"add_building = barracks".to_vec());
    ///
    /// let mut schema = Schema::new();
    /// schema.define_dir("buildings", "building").reference_key("add_building", "building");
    /// let linter = Linter::new(schema);
    /// let mut analysis = linter.analyze(&fileset);
    /// assert_eq!(analysis.diagnostics().len(), 1);
    ///
    /// assert!(fileset.remove(empty));
    /// linter.reanalyze(&fileset, &mut analysis);
    /// assert_eq!(analysis.diagnostics().len(), 0);
    /// ```
    pub fn remove(&mut self, id: FileId) -> bool {
        let Some(file) = self.files.get_mut(id.0 as usize) else {
            return false;
        };
        if file.removed {
            return false;
        }
        file.removed = true;
        file.source = Vec::new();
        file.revision += 1;
        file.lines = OnceLock::new();
        file.utf8 = OnceLock::new();
        self.by_path.remove(&file.path);
        self.load_order = OnceLock::new();
        true
    }

    /// The [`FileId`] of the file at `path`, when the set has one. The paths
    /// must be equal. An editor uses this to find the file of an open
    /// document, and then gives the unsaved text to
    /// [`set_source`](Fileset::set_source).
    pub fn id_for_path(&self, path: impl AsRef<Path>) -> Option<FileId> {
        self.by_path.get(path.as_ref()).copied()
    }

    /// Replace the source of a file, and return `true`. The [`FileId`], the
    /// path, and the layer do not change. Then call [`Linter::reanalyze`] to
    /// update an [`Analysis`]. A removed file, or an id that the set did not
    /// give, does not change, and the function returns `false`.
    pub fn set_source(&mut self, id: FileId, source: Vec<u8>) -> bool {
        let Some(file) = self.files.get_mut(id.0 as usize).filter(|f| !f.removed) else {
            return false;
        };
        file.source = source;
        file.revision += 1;
        file.lines = OnceLock::new();
        file.utf8 = OnceLock::new();
        true
    }
}

/// The kind of a definition or a reference: an entity of the [`Schema`],
/// such as `building`, or a `@name` reader variable. The two are separate
/// namespaces, so a schema kind with the text `variable` is an entity.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[non_exhaustive]
pub enum Kind {
    /// An entity kind of the schema.
    Entity(String),
    /// A `@name` variable that a [`Schema::variable_dir`] file defines. Its
    /// names start with `@`.
    Variable,
}

impl Kind {
    /// An entity kind of the schema.
    pub fn entity(kind: impl Into<String>) -> Self {
        Kind::Entity(kind.into())
    }

    /// The text of an entity kind, or `None` for a variable.
    pub fn as_entity(&self) -> Option<&str> {
        match self {
            Kind::Entity(kind) => Some(kind),
            Kind::Variable => None,
        }
    }

    /// Return `true` for a `@name` variable.
    pub fn is_variable(&self) -> bool {
        matches!(self, Kind::Variable)
    }
}

/// The text of the kind, as diagnostics show it: the entity kind, or
/// `variable`.
impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Entity(kind) => f.write_str(kind),
            Kind::Variable => f.write_str("variable"),
        }
    }
}

/// The rules that tell which files define entities and which field keys
/// refer to entities.
#[derive(Default)]
pub struct Schema {
    /// Directory component (e.g. `"buildings"`) → the [`Kind`] that each
    /// top-level key in a file under that directory defines.
    definition_dirs: HashMap<String, Kind>,
    /// Field key (e.g. `"add_building"`) → the [`Kind`] its value references.
    reference_keys: HashMap<String, Kind>,
    /// Directory components whose `@name` definitions apply to all files.
    variable_dirs: Vec<String>,
}

impl Schema {
    /// An empty schema.
    pub fn new() -> Self {
        Schema::default()
    }

    /// Each top-level key of a file in the directory `dir` defines an entity
    /// of kind `kind`, as in `define_dir("common/buildings", "building")`.
    ///
    /// The path of the file must contain all the components of `dir` in
    /// sequence. Use more than one component when the games use a
    /// directory name two times: CK3 has `common/buildings` and
    /// `common/domiciles/buildings`, and `define_dir("buildings", ..)`
    /// matches the two.
    pub fn define_dir(&mut self, dir: &str, kind: &str) -> &mut Self {
        self.definition_dirs
            .insert(dir.to_string(), Kind::entity(kind));
        self
    }

    /// The value of a field with the key `key` refers to an entity of kind
    /// `kind`, as in `reference_key("add_building", "building")`.
    pub fn reference_key(&mut self, key: &str, kind: &str) -> &mut Self {
        self.reference_keys
            .insert(key.to_string(), Kind::entity(kind));
        self
    }

    /// Each `@name` definition of a file in the directory `dir` applies to
    /// all the files, as in `variable_dir("reader_export")` for CK3. The
    /// directory matches as in [`define_dir`](Schema::define_dir).
    ///
    /// A `@name` that a file does not define itself must have a definition
    /// in such a directory, or the linter gives an undefined-reference
    /// diagnostic. The games do not agree on these directories, so the
    /// schema has none by default.
    pub fn variable_dir(&mut self, dir: &str) -> &mut Self {
        self.variable_dirs.push(dir.to_string());
        self
    }

    fn is_variable_file(&self, path: &Path) -> bool {
        self.variable_dirs.iter().any(|dir| in_dir(path, dir))
    }

    /// The kind that a file at `path` defines, if the path is in a known
    /// definition directory. When more than one directory matches, the
    /// longest directory wins.
    fn definition_kind(&self, path: &Path) -> Option<&Kind> {
        self.definition_dirs
            .iter()
            .filter(|(dir, _)| in_dir(path, dir))
            .max_by_key(|(dir, _)| (Path::new(dir).components().count(), dir.as_str()))
            .map(|(_, kind)| kind)
    }
}

/// `true` when the directory components of `path` contain all the
/// components of `dir` in sequence.
fn in_dir(path: &Path, dir: &str) -> bool {
    let dir: Vec<_> = Path::new(dir).components().collect();
    let Some(parent) = path.parent() else {
        return false;
    };
    let parent: Vec<_> = parent.components().collect();
    !dir.is_empty() && parent.windows(dir.len()).any(|w| w == dir.as_slice())
}

/// A definition in a file: its kind, its name, the range of its name, and
/// the range of the full definition.
#[derive(Debug, Clone)]
pub struct DefSite {
    pub kind: Kind,
    pub name: String,
    /// The range of the name, such as `barracks` in
    /// `barracks = { cost = 100 }`.
    pub range: TextRange,
    /// The range of the full definition, such as all of
    /// `barracks = { cost = 100 }`. It contains `range`. An editor uses it
    /// to find the definition that holds the cursor.
    pub full_range: TextRange,
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
    /// The number of `@` bytes of the text of the reference that are not a
    /// part of the name of a variable: one for `@name`, two for a `@@name`
    /// key, and none for a calculation identifier with no `@` and for the
    /// argument of `@:load_variable`. A fix keeps them.
    pub signs: usize,
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
    /// The variables and calculations that have no value for a reason other
    /// than an undefined variable.
    pub value_errors: Vec<ValueErrorSummary>,
}

/// A variable or a calculation that has no value. See
/// [`resolve`](lossless::resolve).
#[derive(Debug, Clone)]
pub struct ValueErrorSummary {
    pub kind: ResolveErrorKind,
    pub range: TextRange,
}

/// A syntax error of a file, and its repair when the parser has one.
#[derive(Debug, Clone)]
pub struct SyntaxDiagnosticSummary {
    pub message: String,
    pub range: TextRange,
    pub opening_range: Option<TextRange>,
    /// The source ends before the construct closes.
    pub missing_delimiter: bool,
    /// The closing edit of the file, which closes every open construct.
    /// Each error with a missing delimiter has the same edit.
    pub fix: Option<TextEdit>,
}

/// The encoding of the names of the files. The name of a definition and
/// the name of a reference are equal when their text is equal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[non_exhaustive]
pub enum NameEncoding {
    /// A file that is UTF-8 has UTF-8 names, and each other file has
    /// Windows-1252 names. One file has one encoding.
    #[default]
    Detect,
    /// Each file is UTF-8, as for CK3, Vic3, and EU5. A byte that is not
    /// UTF-8 is a Windows-1252 character.
    Utf8,
    /// Each file is Windows-1252, as for EU4.
    Windows1252,
}

/// The encoding of the names of one file. With Windows-1252, each byte is
/// one character, so two names with different bytes stay different names.
#[derive(Clone, Copy)]
struct Names {
    utf8: bool,
}

impl Names {
    fn of(fileset: &Fileset, file: FileId, encoding: NameEncoding) -> Self {
        let utf8 = match encoding {
            NameEncoding::Detect => fileset.is_utf8(file),
            NameEncoding::Utf8 => true,
            NameEncoding::Windows1252 => false,
        };
        Names { utf8 }
    }

    /// The name that `bytes` spell.
    fn decode(self, bytes: &[u8]) -> String {
        match std::str::from_utf8(bytes) {
            Ok(text) if self.utf8 => text.to_owned(),
            _ => bytes
                .iter()
                .map(|&byte| crate::data::WINDOWS_1252[byte as usize])
                .collect(),
        }
    }
}

/// Parse one file with the default [`Flavor`] and [`NameEncoding`], and
/// make its [`FileSummary`].
pub fn summarize(fileset: &Fileset, schema: &Schema, file: FileId) -> FileSummary {
    summarize_with(
        fileset,
        schema,
        file,
        Flavor::default(),
        NameEncoding::default(),
    )
}

/// Parse one file with `flavor`, decode its names with `encoding`, and make
/// its [`FileSummary`]. HoI4 files need [`Flavor::hoi4`], because HoI4 has
/// no `$NAME$` macro parameters.
pub fn summarize_with(
    fileset: &Fileset,
    schema: &Schema,
    file: FileId,
    flavor: Flavor,
    encoding: NameEncoding,
) -> FileSummary {
    let source = fileset.source(file);
    let tree = lossless::parse_with(source, flavor);
    let names = Names::of(fileset, file, encoding);

    // Resolve the variables of the file. A variable that the file does not
    // define is a reference to a variable of a `variable_dir` file.
    let resolution = resolve(tree.tree());

    let mut defs = Vec::new();
    // Top-level keys define entities only when the file lives in a known
    // definition directory. Do not index a definition from a recovered field:
    // references in the same error subtree are suppressed for the same reason.
    // The schema matches the game path, not the path on the disk.
    if let Some(kind) = schema.definition_kind(fileset.game_path(file)) {
        for field in tree.ast().fields() {
            // A `@name = value` field defines a variable, not an entity.
            if !field.syntax().has_error()
                && !field.is_variable_definition()
                && let Some(name) = key_name(names, &resolution, field.key())
            {
                defs.push(DefSite {
                    kind: kind.clone(),
                    name,
                    range: field.key().text_range(),
                    full_range: field.syntax().text_range(),
                });
            }
        }
    }

    // References can appear at any depth, so walk the whole tree.
    let mut refs = Vec::new();
    collect_refs(
        names,
        flavor.macros,
        tree.root(),
        schema,
        &resolution,
        false,
        &mut refs,
    );

    if schema.is_variable_file(fileset.game_path(file)) {
        collect_exports(names, tree.root(), &mut defs);
        // The first definition of a name is its value, and the resolver
        // gives an error for each later one, so only the first is exported.
        let mut exported = std::collections::HashSet::new();
        defs.retain(|d| !d.kind.is_variable() || exported.insert(d.name.clone()));
    }
    let mut value_errors = Vec::new();
    // Whether each node has a syntax error itself, from owns_error.
    let mut owned: HashMap<lossless::NodeId, bool> = HashMap::new();
    for error in resolution.errors() {
        if error.kind != ResolveErrorKind::UndefinedVariable {
            value_errors.push(ValueErrorSummary {
                kind: error.kind,
                range: error.range,
            });
            continue;
        }
        let text = &source[error.range.to_range()];
        // The index names a variable with one `@`. The syntax of the reference
        // tells how many `@` its text has: one for `@name` and for a
        // calculation identifier with `@`, two for a `@@name` key, and none
        // for the argument of `@:load_variable`, where each `@` is a part of
        // the name, as for the resolver.
        let token = tree.covering_element(error.range).as_token();
        // Only the argument of `@:load_variable`, and not the value of
        // `@:register_variable`, which has the same parent.
        let argument = token.is_some_and(|token| {
            token
                .parent()
                .and_then(ReaderDirective::cast)
                .filter(|directive| directive.kind() == DirectiveKind::LoadVariable)
                .and_then(|directive| directive.argument())
                .is_some_and(|argument| argument.text_range() == token.text_range())
        });
        let signs = match token.map(|token| token.kind()) {
            _ if argument => 0,
            Some(SyntaxKind::VariableKey) => 2,
            Some(SyntaxKind::Variable) => 1,
            Some(SyntaxKind::CalcIdent) => usize::from(text.starts_with(b"@")),
            _ => 0,
        };
        let bare = text.get(signs..).unwrap_or(text);
        let name = format!("@{}", names.decode(bare));
        // Many references can be in one large block, so each node is
        // examined one time.
        let in_error_subtree = tree
            .covering_element(error.range)
            .ancestors()
            .any(|node| *owned.entry(node.id()).or_insert_with(|| owns_error(node)));
        refs.push(RefSite {
            kind: Kind::Variable,
            name,
            signs,
            range: error.range,
            in_error_subtree,
        });
    }
    refs.sort_by_key(|r| r.range.start());

    let closing_edit = tree.closing_edit();
    let syntax_errors = tree
        .errors()
        .iter()
        .map(|e| {
            let delimiter = e.missing_delimiter.as_ref();
            SyntaxDiagnosticSummary {
                message: e.message().to_owned(),
                range: e.range,
                opening_range: delimiter.map(|d| d.opening_range),
                missing_delimiter: delimiter.is_some(),
                fix: delimiter.and(closing_edit).cloned(),
            }
        })
        .collect();

    FileSummary {
        file,
        revision: fileset.files[file.0 as usize].revision,
        defs,
        refs,
        syntax_errors,
        value_errors,
    }
}

/// Return `true` when `node` itself has a syntax error: it is a `Bogus`
/// node, or a child is a missing delimiter or a `Bogus` node. An error in a
/// descendant does not count, so an error in one block does not change the
/// references of a different block. The root has no error itself: a stray
/// `}` at the top level is a `Bogus` node next to the other fields.
fn owns_error(node: SyntaxNode<'_, '_>) -> bool {
    node.has_error()
        && node.kind() != SyntaxKind::Root
        && (node.kind() == SyntaxKind::Bogus
            || node
                .significant_children()
                .any(|child| child.kind().is_missing() || child.kind() == SyntaxKind::Bogus))
}

/// The name of a key: its text, or the value of the variable of a
/// `@@name` key. A `@@name` key whose variable has no value has no name.
fn key_name(
    names: Names,
    resolution: &lossless::resolve::Resolution,
    key: ScalarToken<'_, '_>,
) -> Option<String> {
    if key.kind() == SyntaxKind::VariableKey {
        let text = resolution.get(Value::Scalar(key))?.as_bytes();
        Some(names.decode(text))
    } else {
        Some(names.decode(key.as_scalar().as_bytes()))
    }
}

/// Collect the variables that a file exports: the definitions that the
/// resolver reads. A code payload, a parameter body, the body of
/// `@:define`, and the arguments of `@:insert` apply only where a caller
/// gives them, so the resolver skips them, and they export nothing.
fn collect_exports(names: Names, node: SyntaxNode<'_, '_>, out: &mut Vec<DefSite>) {
    for child in node.child_nodes() {
        match child.kind() {
            SyntaxKind::Field => match Field::cast(child) {
                Some(field) if field.is_variable_definition() => {
                    if !field.syntax().has_error() {
                        let key = field.key();
                        out.push(DefSite {
                            kind: Kind::Variable,
                            name: names.decode(key.as_scalar().as_bytes()),
                            range: key.text_range(),
                            full_range: field.syntax().text_range(),
                        });
                    }
                }
                _ => collect_exports(names, child, out),
            },
            // The CK3 directive `@:register_variable name = value` also
            // defines a variable. Each other directive exports nothing.
            SyntaxKind::ReaderDirective => {
                if let Some(directive) = ReaderDirective::cast(child)
                    && directive.kind() == DirectiveKind::RegisterVariable
                    && !directive.syntax().has_error()
                    && let Some(name) = directive.argument()
                {
                    out.push(DefSite {
                        kind: Kind::Variable,
                        // The resolver uses the text of the argument, as in
                        // `@:load_variable`, quotes too.
                        name: format!("@{}", names.decode(name.text())),
                        range: name.text_range(),
                        full_range: directive.syntax().text_range(),
                    });
                }
            }
            SyntaxKind::Code
            | SyntaxKind::Parameter
            | SyntaxKind::UndefinedParameter
            | SyntaxKind::Calc => {}
            _ => collect_exports(names, child, out),
        }
    }
}

/// Collect the references in `node` and its descendants. `in_error` is `true`
/// when an ancestor of `node` has a syntax error itself.
///
/// The name of a reference is its source text, or the value of the file
/// variable that it uses, as in `add_building = @target`. A reference to a
/// variable that the file does not define has no name here: the variable
/// is a reference of its own.
fn collect_refs(
    names: Names,
    macros: bool,
    node: SyntaxNode<'_, '_>,
    schema: &Schema,
    resolution: &lossless::resolve::Resolution,
    in_error: bool,
    out: &mut Vec<RefSite>,
) {
    let in_error = in_error || owns_error(node);

    if let Some(field) = Field::cast(node)
        && let Some(value) = field.value()
        && let Some(key) = key_name(names, resolution, field.key())
        && let Some(kind) = schema.reference_keys.get(&key)
    {
        // The value of a variable can be a parameter too.
        let resolved = |value| {
            resolution
                .get(value)
                .map(|value| value.as_bytes())
                .filter(|text| !(macros && is_macro(text)))
                .map(|text| names.decode(text))
        };
        let name = match value {
            Value::Scalar(tok)
                if matches!(tok.kind(), SyntaxKind::Variable | SyntaxKind::VariableKey) =>
            {
                resolved(value)
            }
            // A `$NAME$` parameter of a scripted effect or trigger gets its
            // value when the game calls the effect, so the name is not known
            // here. The lexer gives such a token only for a flavor with
            // macros, so `a$b`, and `$NAME$` for HoI4, are names.
            // A game puts the parameter in a quoted string too.
            Value::Scalar(tok)
                if tok.kind() == SyntaxKind::Quoted && macros && is_macro(tok.as_bytes()) =>
            {
                None
            }
            Value::Scalar(tok) if tok.kind() != SyntaxKind::MacroParam => {
                Some(names.decode(tok.as_scalar().as_bytes()))
            }
            Value::ReaderDirective(directive)
                if directive.kind() == DirectiveKind::LoadVariable =>
            {
                resolved(value)
            }
            // A calculation refers to the entity of its result.
            Value::Calc(_) => resolved(value),
            _ => None,
        };
        if let Some(name) = name {
            out.push(RefSite {
                kind: kind.clone(),
                name,
                signs: 0,
                range: value.text_range(),
                in_error_subtree: in_error,
            });
        }
    }

    for child in node.child_nodes() {
        collect_refs(names, macros, child, schema, resolution, in_error, out);
    }
}

/// Return `true` for text that has a `$NAME$` parameter. This is the rule
/// of the lexer for an unquoted token: two `$` bytes.
fn is_macro(text: &[u8]) -> bool {
    text.iter().filter(|&&byte| byte == b'$').nth(1).is_some()
}

/// The text that replaces a reference with the defined name `name`, in the
/// form of the reference at `source`: a variable keeps its `signs`, and a
/// quoted name keeps its quotes. Return `None` when the name cannot have
/// that form, or when the reference is a variable whose value is the name.
fn fix_text(source: &[u8], kind: &Kind, name: &str, signs: usize) -> Option<String> {
    // The encoding of the file of the reference is not known, so a fix holds
    // only ASCII text.
    if !name.is_ascii() {
        return None;
    }
    // A character that ends an unquoted token.
    let plain = |c: char| !c.is_whitespace() && !"{}[]=<>!?\";#@$".contains(c);
    if kind.is_variable() {
        // The index has one `@` before the name. The reference has `signs`
        // of them, from its syntax.
        let bare = name.strip_prefix('@')?;
        // A variable can be in a calculation, also with its `@`, so the name
        // must be one identifier there: `foo-bar` in a calculation is a
        // subtraction, and a digit starts a number. It must also be one
        // token as the argument of `@:load_variable`, and `@:name` is a
        // reader directive.
        let identifier = !bare.is_empty()
            && !bare.starts_with(|c: char| c.is_ascii_digit() || c == '.' || c == ':')
            && bare.chars().all(|c| plain(c) && !"()+-*/".contains(c));
        return identifier.then(|| format!("{}{bare}", "@".repeat(signs)));
    }
    match source.first() {
        Some(b'"') => (!name.contains(['"', '\\'])).then(|| format!("\"{name}\"")),
        Some(b'@') => None,
        _ => (!name.is_empty() && name.chars().all(plain)).then(|| name.to_owned()),
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

/// A map by kind and then by name, with one namespace for each entity kind
/// and one for the variables. A lookup does not allocate.
struct ByKind<T> {
    entities: HashMap<String, HashMap<String, T>>,
    variables: HashMap<String, T>,
}

impl<T> Default for ByKind<T> {
    fn default() -> Self {
        ByKind {
            entities: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

impl<T> ByKind<T> {
    /// The value of `(kind, name)`.
    fn get(&self, kind: &Kind, name: &str) -> Option<&T> {
        match kind {
            Kind::Entity(kind) => self.entities.get(kind)?.get(name),
            Kind::Variable => self.variables.get(name),
        }
    }

    /// The value of `(kind, name)`, made when the map has none.
    fn slot(&mut self, kind: &Kind, name: &str) -> &mut T
    where
        T: Default,
    {
        // Most slots exist, so look up first and allocate a key only for a
        // new slot.
        let names = match kind {
            Kind::Entity(kind) => {
                if !self.entities.contains_key(kind) {
                    self.entities.insert(kind.clone(), HashMap::new());
                }
                self.entities.get_mut(kind).expect("the map has the kind")
            }
            Kind::Variable => &mut self.variables,
        };
        if !names.contains_key(name) {
            names.insert(name.to_owned(), T::default());
        }
        names.get_mut(name).expect("the map has the name")
    }

    fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        let entities = self.entities.values_mut().flat_map(HashMap::values_mut);
        entities.chain(self.variables.values_mut())
    }
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
    by_key: ByKind<Slot>,
}

/// The definitions of one `(kind, name)`.
#[derive(Default)]
struct Slot {
    /// The ids of the definitions, in the order of the calls to
    /// [`Index::define`].
    ids: SmallVec<[u32; 2]>,
    /// The definition that wins. [`Index::finalize`] picks it.
    winner: Option<u32>,
}

impl Index {
    /// An empty index.
    pub fn new() -> Self {
        Index::default()
    }

    /// Add a definition, and return its [`DefId`].
    pub fn define(&mut self, kind: Kind, name: String, file: FileId, range: TextRange) -> DefId {
        let id = self.definitions.len() as u32;
        self.by_key.slot(&kind, &name).ids.push(id);
        self.definitions.push(Definition {
            kind,
            name,
            file,
            range,
            winner: false,
        });
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
    pub fn resolve(&self, kind: &Kind, name: &str, in_error_subtree: bool) -> Resolution {
        match self.by_key.get(kind, name).and_then(|slot| slot.winner) {
            Some(id) => Resolution::Found(DefId(id)),
            None if in_error_subtree => Resolution::UpstreamError,
            None => Resolution::NotFound,
        }
    }

    /// Resolve a reference of `file` to the definition that wins among the
    /// definitions of the other files: the last one in the highest layer.
    /// A variable reference uses this, because the resolver of the file
    /// already used the definitions of the file, in their order. Thus a
    /// file does not satisfy its own reference that the resolver did not
    /// satisfy, as in `@loop = @loop`.
    fn resolve_from_other_files(
        &self,
        fileset: &Fileset,
        file: FileId,
        kind: &Kind,
        name: &str,
        in_error_subtree: bool,
    ) -> Resolution {
        let ids = self
            .by_key
            .get(kind, name)
            .map_or(&[][..], |slot| slot.ids.as_slice());
        let layer = |id: u32| fileset.kind(self.definitions[id as usize].file);
        let winner = ids
            .iter()
            .copied()
            .filter(|&id| self.definitions[id as usize].file != file)
            .fold(None, |best: Option<u32>, id| match best {
                Some(best) if layer(best) > layer(id) => Some(best),
                _ => Some(id),
            });
        match winner {
            Some(id) => Resolution::Found(DefId(id)),
            None if in_error_subtree => Resolution::UpstreamError,
            None => Resolution::NotFound,
        }
    }

    /// The defined name of the same `kind` that is nearest to `name`, for a
    /// "did you mean?" hint. Return `None` when no name is near enough. Only a
    /// name that wins is a candidate, so the suggested name resolves. Call
    /// [`finalize`](Index::finalize) first.
    pub fn suggest(&self, kind: &Kind, name: &str) -> Option<&str> {
        self.suggest_from(kind, name, None)
    }

    /// The nearest name, as [`suggest`](Index::suggest) gives. With `other_than`,
    /// the candidates are the definitions of the other files, as for
    /// [`resolve_from_other_files`](Index::resolve_from_other_files).
    fn suggest_from(&self, kind: &Kind, name: &str, other_than: Option<FileId>) -> Option<&str> {
        // A longer name can have more edits, from 1 to 3. Thus `baracks` gets
        // `barracks`, but a short name does not get an unrelated name.
        let threshold = (name.len() / 3).clamp(1, 3);
        let mut best: Option<(usize, &str)> = None;
        let candidate = |d: &&Definition| match other_than {
            None => d.winner,
            Some(file) => d.file != file,
        };
        for d in self
            .definitions
            .iter()
            .filter(|d| d.kind == *kind)
            .filter(candidate)
        {
            // A candidate must be as near as the best candidate, or nearer.
            let limit = best.map_or(threshold, |(dist, _)| dist);
            // The difference of the lengths is the minimum distance. This
            // check is fast and removes most candidates.
            if d.name.len().abs_diff(name.len()) > limit {
                continue;
            }
            let Some(dist) = levenshtein_within(name.as_bytes(), d.name.as_bytes(), limit) else {
                continue;
            };
            // On equal distance, the shorter name wins. On equal length, the
            // first name wins.
            if best.is_none_or(|(b, c)| (dist, d.name.len()) < (b, c.len())) {
                best = Some((dist, d.name.as_str()));
            }
        }
        best.map(|(_, candidate)| candidate)
    }

    /// Pick the definition that wins for each `(kind, name)`: the last one in
    /// the highest layer. Return a tip for each definition that overrides a
    /// lower layer, and a warning for each duplicate definition in one layer.
    ///
    /// A second call after more definitions picks the winners again.
    pub fn finalize(&mut self, fileset: &Fileset) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        let Index {
            definitions,
            by_key,
        } = self;
        for definition in definitions.iter_mut() {
            definition.winner = false;
        }

        let layer = |id: u32| fileset.kind(definitions[id as usize].file);
        for slot in by_key.values_mut() {
            let ids = &slot.ids;
            // The winning layer is the highest FileKind among the definitions.
            // Inside the winning layer, the last definition wins, as in the
            // load order of the games. The earlier definitions of that layer
            // are duplicates, and the definitions of lower layers are
            // overridden.
            let max_layer = ids.iter().map(|&id| layer(id)).max().unwrap();
            let winner = ids
                .iter()
                .copied()
                .rfind(|&id| layer(id) == max_layer)
                .unwrap();
            let overrides = ids.iter().any(|&id| layer(id) < max_layer);
            slot.winner = Some(winner);

            for &id in ids {
                let def = &definitions[id as usize];
                if id == winner && overrides {
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
                } else if id != winner && layer(id) == max_layer {
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
        for winner in by_key.values_mut().filter_map(|slot| slot.winner) {
            definitions[winner as usize].winner = true;
        }

        diags.sort_by_key(|d| (d.file, d.range.start()));
        diags
    }
}

/// The Levenshtein edit distance between two byte strings, when it is
/// `limit` or less. Return `None` when it is more than `limit`. The function
/// stops when all the cells of a row are more than `limit`, so a far name
/// costs less than the full O(n·m) time.
fn levenshtein_within(a: &[u8], b: &[u8], limit: usize) -> Option<usize> {
    if a.len().abs_diff(b.len()) > limit {
        return None;
    }
    // Only the cells within `limit` of the diagonal can be `limit` or less,
    // so each row calculates at most `2 * limit + 1` cells. A cell out of
    // the band is `limit + 1`.
    let (n, far) = (b.len(), limit + 1);
    let mut prev: Vec<usize> = (0..=n).map(|j| if j <= limit { j } else { far }).collect();
    let mut curr = vec![far; n + 1];
    for i in 1..=a.len() {
        let lo = i.saturating_sub(limit);
        let hi = (i + limit).min(n);
        let mut row_min = far;
        if lo == 0 {
            curr[0] = i;
            row_min = i;
        } else {
            curr[lo - 1] = far;
        }
        for j in lo.max(1)..=hi {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            let cell = (prev[j - 1] + cost).min(prev[j] + 1).min(curr[j - 1] + 1);
            curr[j] = cell.min(far);
            row_min = row_min.min(curr[j]);
        }
        if hi < n {
            curr[hi + 1] = far;
        }
        if row_min > limit {
            return None;
        }
        std::mem::swap(&mut prev, &mut curr);
    }
    Some(prev[n]).filter(|&dist| dist <= limit)
}

/// The stable ids of the lints. A user can refer to an id to configure or
/// suppress a lint.
pub mod lints {
    pub const UNDEFINED_REFERENCE: &str = "undefined-reference";
    pub const SUPPRESSED_REFERENCE: &str = "undefined-reference-suppressed";
    pub const SYNTAX_ERROR: &str = "syntax-error";
    pub const DUPLICATE: &str = "duplicate-definition";
    pub const OVERRIDE: &str = "override";
    pub const VARIABLE_VALUE: &str = "variable-value";
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
    pub fix: Option<TextEdit>,
}

/// The cross-file linter. It collects the definitions of all the files, and
/// then it resolves each reference.
pub struct Linter {
    schema: Schema,
    flavor: Flavor,
    encoding: NameEncoding,
}

impl Linter {
    /// A linter driven by `schema`, which parses with the default
    /// [`Flavor`].
    pub fn new(schema: Schema) -> Self {
        Linter {
            schema,
            flavor: Flavor::default(),
            encoding: NameEncoding::default(),
        }
    }

    /// Decode the names of the files with `encoding`.
    pub fn with_encoding(mut self, encoding: NameEncoding) -> Self {
        self.encoding = encoding;
        self
    }

    /// Parse with `flavor`, such as [`Flavor::hoi4`] for HoI4 files.
    pub fn with_flavor(mut self, flavor: Flavor) -> Self {
        self.flavor = flavor;
        self
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
        // `summaries[i]` is the summary of `FileId(i)`, so a removed file
        // has a summary too: it has no source, so it has nothing.
        let summaries: Vec<FileSummary> = fileset
            .all_ids()
            .map(|id| summarize_with(fileset, &self.schema, id, self.flavor, self.encoding))
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
        if analysis.fileset_tag != fileset.tag
            || analysis.flavor != self.flavor
            || analysis.encoding != self.encoding
        {
            *analysis = self.analyze(fileset);
            return;
        }
        let mut summaries = std::mem::take(&mut analysis.summaries);
        for (summary, file) in summaries.iter_mut().zip(&fileset.files) {
            if summary.revision != file.revision {
                *summary = summarize_with(
                    fileset,
                    &self.schema,
                    summary.file,
                    self.flavor,
                    self.encoding,
                );
            }
        }
        for id in summaries.len()..fileset.files.len() {
            let file = FileId(id as u32);
            summaries.push(summarize_with(
                fileset,
                &self.schema,
                file,
                self.flavor,
                self.encoding,
            ));
        }
        *analysis = self.resolve(fileset, summaries);
    }

    /// Pass 2 for [`analyze`](Linter::analyze) and
    /// [`reanalyze`](Linter::reanalyze): register the definitions, then
    /// resolve the references of all the summaries.
    fn resolve(&self, fileset: &Fileset, summaries: Vec<FileSummary>) -> Analysis {
        // The games do not load a file that a later layer replaces, so its
        // definitions, its references, and its errors do not count. Inside a
        // layer, the last definition of a name wins, and the games load the
        // files of a layer in the order of their game paths. Thus the index
        // gets the files in that order, also when a caller added a file later.
        let loaded = || {
            fileset
                .load_order()
                .iter()
                .map(|id| &summaries[id.0 as usize])
        };
        let mut index = Index::new();
        for s in loaded() {
            for d in &s.defs {
                index.define(d.kind.clone(), d.name.clone(), s.file, d.range);
            }
        }
        let mut diags = index.finalize(fileset);

        // Also record the references of each name for find references.
        let mut refs_by_name: ByKind<Vec<(FileId, u32)>> = ByKind::default();
        // The suggestion for each undefined name, as found.
        let mut suggestions: HashMap<(&Kind, &str, Option<FileId>), Option<&str>> = HashMap::new();
        // The definition of each reference, as the diagnostics resolve it.
        let mut targets: Vec<Vec<Option<u32>>> =
            summaries.iter().map(|s| vec![None; s.refs.len()]).collect();
        for s in loaded() {
            for error in &s.syntax_errors {
                diags.push(Diagnostic {
                    id: lints::SYNTAX_ERROR,
                    severity: Severity::Error,
                    confidence: Confidence::Strong,
                    file: s.file,
                    range: error.range,
                    message: error.message.clone(),
                    help: error.missing_delimiter.then(|| {
                        if error.fix.is_some() {
                            "close the open constructs at the end of the file".into()
                        } else {
                            "the missing trailing delimiter has no safe automatic fix".into()
                        }
                    }),
                    fix: error.fix.clone(),
                });
            }
            for error in &s.value_errors {
                // The games do not agree on the calculation grammar, so a
                // form that the resolver does not know is only a warning.
                let (severity, confidence) = match error.kind {
                    ResolveErrorKind::UnsupportedCalculation => {
                        (Severity::Warning, Confidence::Reasonable)
                    }
                    _ => (Severity::Error, Confidence::Strong),
                };
                diags.push(Diagnostic {
                    id: lints::VARIABLE_VALUE,
                    severity,
                    confidence,
                    file: s.file,
                    range: error.range,
                    message: error.kind.message().to_string(),
                    help: None,
                    fix: None,
                });
            }
            for (ref_index, r) in s.refs.iter().enumerate() {
                refs_by_name
                    .slot(&r.kind, &r.name)
                    .push((s.file, ref_index as u32));

                let resolution = if r.kind.is_variable() {
                    index.resolve_from_other_files(
                        fileset,
                        s.file,
                        &r.kind,
                        &r.name,
                        r.in_error_subtree,
                    )
                } else {
                    index.resolve(&r.kind, &r.name, r.in_error_subtree)
                };
                if let Resolution::Found(DefId(id)) = resolution {
                    targets[s.file.0 as usize][ref_index] = Some(id);
                }
                match resolution {
                    Resolution::Found(_) => {}
                    Resolution::NotFound => {
                        // A reference is one token, so the nearest name is
                        // also a safe replacement, in the form of the
                        // reference.
                        let other_than = (r.kind.is_variable()).then_some(s.file);
                        // Many references can have the same undefined name, so
                        // each name has one search.
                        let suggestion = suggestions
                            .entry((&r.kind, r.name.as_str(), other_than))
                            .or_insert_with(|| index.suggest_from(&r.kind, &r.name, other_than))
                            .map(str::to_owned);
                        let (help, fix) = match suggestion.as_deref() {
                            Some(name) => {
                                let source = &fileset.source(s.file)[r.range.to_range()];
                                (
                                    Some(format!("did you mean `{name}`?")),
                                    fix_text(source, &r.kind, name, r.signs)
                                        .map(|text| TextEdit::new(r.range, text)),
                                )
                            }
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
            flavor: self.flavor,
            encoding: self.encoding,
            index,
            summaries,
            diagnostics: diags,
            refs_by_name,
            targets,
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
/// use jomini::text::lint::{FileKind, Fileset, Kind, Linter, Schema};
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
/// let references: Vec<_> = analysis.references_to(&Kind::entity("building"), "barracks").collect();
/// assert_eq!(references.len(), 2);
/// assert!(references.iter().all(|(file, _)| *file == events));
/// ```
pub struct Analysis {
    /// The tag of the [`Fileset`] that the summaries come from.
    fileset_tag: u64,
    /// The flavor that parsed the files.
    flavor: Flavor,
    /// The encoding of the names.
    encoding: NameEncoding,
    index: Index,
    /// `summaries[i]` is the summary of `FileId(i)`.
    summaries: Vec<FileSummary>,
    diagnostics: Vec<Diagnostic>,
    /// The references to each `(kind, name)`, as the file and the index into
    /// the `refs` of the summary of the file.
    refs_by_name: ByKind<Vec<(FileId, u32)>>,
    /// `targets[i][j]` is the definition of reference `j` of `FileId(i)`.
    targets: Vec<Vec<Option<u32>>>,
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
        let summary = self.summary(file)?;
        if let Some(index) = summary
            .refs
            .iter()
            .position(|r| r.range.contains_inclusive(offset))
        {
            let id = self.targets.get(file.0 as usize)?.get(index).copied()??;
            return self.index.definitions().get(id as usize);
        }
        if let Some(d) = self.definition_at(file, offset) {
            return self.winning_definition(&d.kind, &d.name);
        }
        None
    }

    /// The winning [`Definition`] for `(kind, name)`, if the name is defined.
    fn winning_definition(&self, kind: &Kind, name: &str) -> Option<&Definition> {
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
        kind: &Kind,
        name: &str,
    ) -> impl Iterator<Item = (FileId, &'s RefSite)> + use<'s> {
        self.refs_by_name
            .get(kind, name)
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
        .line_text_range(index.line_col(offset).line)
        .expect("the line of an offset exists");
    (&fileset.source(file)[range], range.start().to_usize())
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
    render_with(diag, fileset, color, NameEncoding::default())
}

/// Render a diagnostic as [`render`] does, and decode the source line with
/// `encoding`, as the linter decodes the names. The padding and the carets
/// count the characters of the line, not its bytes, so the carets are under
/// the reference also after a character of more than one byte. The column
/// counts characters too.
///
/// ```
/// use jomini::text::lint::{FileKind, Fileset, Linter, NameEncoding, Schema, render_with};
///
/// let mut fileset = Fileset::new();
/// fileset.add("common/buildings/b.txt", FileKind::Vanilla, b"barracks = {}".to_vec());
/// fileset.add("events/e.txt", FileKind::Mod(0), b"label = \"\xe9t\xe9\" add_building = absent".to_vec());
/// let mut schema = Schema::new();
/// schema.define_dir("buildings", "building").reference_key("add_building", "building");
/// let linter = Linter::new(schema).with_encoding(NameEncoding::Windows1252);
/// let diagnostics = linter.run(&fileset);
/// let text = render_with(&diagnostics[0], &fileset, false, NameEncoding::Windows1252);
/// assert_eq!(
///     text,
///     "\
/// error[undefined-reference]: no building named `absent` is defined in any loaded file
///      --> events/e.txt:1:30
///      |
///    1 | label = \"\u{e9}t\u{e9}\" add_building = absent
///      |                              ^^^^^^
/// "
/// );
/// ```
pub fn render_with(
    diag: &Diagnostic,
    fileset: &Fileset,
    color: bool,
    encoding: NameEncoding,
) -> String {
    let line = fileset.line_col(diag.file, diag.range.start()).line + 1;
    let (line_src, line_start) = line_bytes(fileset, diag.file, diag.range.start());
    let names = Names::of(fileset, diag.file, encoding);
    let line_text = names.decode(line_src);

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

    // The caret stops at the end of the line. The padding and the carets
    // count characters, so decode the bytes before the range and in it.
    let caret_col = (diag.range.start().to_usize() - line_start).min(line_src.len());
    let end = (diag.range.end().to_usize() - line_start).clamp(caret_col, line_src.len());
    let pad: String = names
        .decode(&line_src[..caret_col])
        .chars()
        .map(|c| if c == '\t' { '\t' } else { ' ' })
        .collect();
    // The column counts characters too, as the carets do.
    let col = pad.chars().count() + 1;
    let span = names
        .decode(&line_src[caret_col..end])
        .chars()
        .count()
        .max(1);
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

/// Apply the fixes of diagnostics to `source`, and return the result.
///
/// Different diagnostics can give fixes for the same bytes. Thus a fix that
/// overlaps an earlier fix, or that goes past the end of the source, is not
/// applied. A fix that is the same as an earlier fix is applied once, so the
/// closing edit that each missing delimiter has is inserted once. To get an error for such an edit, use
/// [`apply_edits`](lossless::apply_edits). To format the result, use
/// [`format`](lossless::format).
///
/// Apply the fixes of the diagnostics of one file:
///
/// ```
/// use jomini::text::lint::{FileKind, Fileset, Linter, Schema, apply_fixes};
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
/// let fixed = apply_fixes(fileset.source(events), &fixes);
/// assert_eq!(fixed, b"a = { add_building = barracks } # keep this comment");
/// ```
pub fn apply_fixes(source: &[u8], fixes: &[TextEdit]) -> Vec<u8> {
    let mut sorted: Vec<&TextEdit> = fixes.iter().collect();
    sorted.sort_by_key(|fix| (fix.range.start(), fix.range.end()));

    let mut kept = Vec::with_capacity(sorted.len());
    let mut end = TextSize::new(0);
    for fix in sorted {
        if fix.range.start() < end
            || fix.range.end().to_usize() > source.len()
            || kept.last() == Some(fix)
        {
            continue;
        }
        end = fix.range.end();
        kept.push(fix.clone());
    }
    lossless::apply_edits(source, &kept).expect("the kept fixes do not overlap")
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
        let fixed = apply_fixes(fs.source(d.file), d.fix.as_slice());
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
    fn a_name_that_is_not_utf8_is_windows_1252_and_gives_no_fix() {
        let mut fs = Fileset::new();
        fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barrack\xe9s = {}".to_vec(),
        );
        fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barrackes }".to_vec(),
        );
        let mut schema = Schema::new();
        schema
            .define_dir("buildings", "building")
            .reference_key("add_building", "building");
        let diags = Linter::new(schema).run(&fs);

        let undef: Vec<_> = diags
            .iter()
            .filter(|d| d.id == lints::UNDEFINED_REFERENCE)
            .collect();
        assert_eq!(undef.len(), 1, "{diags:#?}");
        assert_eq!(
            undef[0].help.as_deref(),
            Some("did you mean `barrack\u{e9}s`?")
        );
        assert_eq!(undef[0].fix, None);

        // Two names with different bytes are different names.
        let (fs, events) = two_files("buildings", b"caf\xe9 = {}", b"add_building = caf\xe8");
        assert_eq!(
            undefined(&fs, &Linter::new(super::tests::schema()), events),
            [(
                "no building named `caf\u{e8}` is defined in any loaded file".to_owned(),
                None
            )]
        );
    }

    #[test]
    fn suggest_picks_the_nearest_then_the_shortest_name() {
        let mut fs = Fileset::new();
        fs.add("common/buildings/a.txt", FileKind::Vanilla, Vec::new());
        let mut index = Index::new();
        for name in ["barracks", "barrack", "stables", "castle_01", "castle_02"] {
            index.define(
                Kind::entity("building"),
                name.into(),
                FileId(0),
                TextRange::span(0, 1),
            );
        }
        index.finalize(&fs);
        assert_eq!(
            index.suggest(&Kind::entity("building"), "baracks"),
            Some("barracks")
        );
        assert_eq!(
            index.suggest(&Kind::entity("building"), "barrackz"),
            Some("barrack")
        );
        assert_eq!(
            index.suggest(&Kind::entity("building"), "castle_0"),
            Some("castle_01")
        );
        assert_eq!(index.suggest(&Kind::entity("building"), "castle"), None);
        assert_eq!(index.suggest(&Kind::entity("building"), "temple"), None);
        assert_eq!(index.suggest(&Kind::entity("culture"), "barracks"), None);

        assert_eq!(levenshtein_within(b"kitten", b"sitting", 3), Some(3));
        assert_eq!(levenshtein_within(b"kitten", b"sitting", 2), None);
        assert_eq!(levenshtein_within(b"", b"abc", 3), Some(3));
        assert_eq!(levenshtein_within(b"abc", b"", 2), None);
    }

    #[test]
    fn apply_fixes_splices_and_skips_overlap() {
        // A single in-range rename.
        let renamed = apply_fixes(
            b"add_building = baracks",
            &[TextEdit::new(TextRange::span(15, 22), "barracks")],
        );
        assert_eq!(renamed, b"add_building = barracks");

        // A fix that overlaps an earlier fix, or that goes past the end, is
        // not applied.
        let out = apply_fixes(
            b"abcdef",
            &[
                TextEdit::new(TextRange::span(0, 3), "X"),
                // Overlaps the first fix.
                TextEdit::new(TextRange::span(2, 5), "Y"),
                // Goes past the end.
                TextEdit::new(TextRange::span(5, 9), "Z"),
            ],
        );
        assert_eq!(out, b"Xdef");

        // Equal zero-width edits keep their input order.
        let out = apply_fixes(
            b"abc",
            &[
                TextEdit::new(TextRange::span(3, 3), "]"),
                TextEdit::new(TextRange::span(3, 3), "}"),
            ],
        );
        assert_eq!(out, b"abc]}");
    }

    #[test]
    fn script_parameter_is_not_a_reference() {
        let mut fs = Fileset::new();
        fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barracks_01 = {}".to_vec(),
        );
        fs.add(
            "common/scripted_effects/e.txt",
            FileKind::Vanilla,
            b"add_level = { add_building = $BUILDING$_01 }".to_vec(),
        );
        let mut schema = Schema::new();
        schema
            .define_dir("buildings", "building")
            .reference_key("add_building", "building");
        let diagnostics = Linter::new(schema).run(&fs);
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
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
    fn definition_dir_with_more_than_one_component() {
        let mut fs = Fileset::new();
        let buildings = fs.add(
            "/games/ck3/game/common/buildings/00_buildings.txt",
            FileKind::Vanilla,
            b"barracks_01 = {}".to_vec(),
        );
        let domicile = fs.add(
            "/games/ck3/game/common/domiciles/buildings/00_estate_buildings.txt",
            FileKind::Vanilla,
            b"barracks_01 = {}".to_vec(),
        );
        let mut schema = Schema::new();
        schema
            .define_dir("common/buildings", "building")
            .define_dir("domiciles/buildings", "domicile_building");
        let analysis = Linter::new(schema).analyze(&fs);
        assert!(
            analysis.diagnostics().is_empty(),
            "{:?}",
            analysis.diagnostics()
        );
        let kinds = |file| analysis.summary(file).unwrap().defs[0].kind.clone();
        assert_eq!(kinds(buildings), Kind::entity("building"));
        assert_eq!(kinds(domicile), Kind::entity("domicile_building"));

        // One component matches the two directories.
        let mut schema = Schema::new();
        schema.define_dir("buildings", "building");
        let analysis = Linter::new(schema).analyze(&fs);
        assert_eq!(analysis.diagnostics()[0].id, lints::DUPLICATE);

        // A file name is not a directory.
        assert!(!in_dir(Path::new("common/buildings"), "buildings"));
        assert!(in_dir(
            Path::new("common/buildings/a.txt"),
            "common/buildings"
        ));
        assert!(!in_dir(
            Path::new("common/buildings/a.txt"),
            "mon/buildings"
        ));
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
    fn a_definition_has_the_range_of_its_name_and_of_its_field() {
        let mut fs = Fileset::new();
        let buildings = fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barracks = { cost = 100 } # note".to_vec(),
        );
        let exports = fs.add(
            "reader_export/v.txt",
            FileKind::Vanilla,
            b"@cost = 5\n@:register_variable shared = 2".to_vec(),
        );
        let ranges = |file| {
            let summary = summarize(&fs, &schema(), file);
            let source = fs.source(file);
            summary
                .defs
                .iter()
                .map(|d| {
                    let text = |range: TextRange| &source[range.to_range()];
                    (text(d.range).to_vec(), text(d.full_range).to_vec())
                })
                .collect::<Vec<_>>()
        };
        assert_eq!(
            ranges(buildings),
            [(b"barracks".to_vec(), b"barracks = { cost = 100 }".to_vec())]
        );
        assert_eq!(
            ranges(exports),
            [
                (b"@cost".to_vec(), b"@cost = 5".to_vec()),
                (
                    b"shared".to_vec(),
                    b"@:register_variable shared = 2".to_vec()
                ),
            ]
        );
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

    /// Each missing delimiter has the closing edit of the file, and the fixes
    /// insert it once.
    #[test]
    fn nested_missing_delimiters_share_one_fix() {
        let mut fs = Fileset::new();
        let file = fs.add(
            "common/buildings/b.txt",
            FileKind::Mod(0),
            b"a = { b = { c = 1".to_vec(),
        );
        let diagnostics = Linter::new(Schema::new()).run(&fs);
        let syntax: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.id == lints::SYNTAX_ERROR)
            .collect();
        assert_eq!(syntax.len(), 2);
        for diagnostic in &syntax {
            assert_eq!(
                diagnostic.fix,
                Some(TextEdit::new(TextRange::empty(TextSize::new(17)), "}}"))
            );
            assert_eq!(
                diagnostic.help.as_deref(),
                Some("close the open constructs at the end of the file")
            );
        }
        let fixes: Vec<_> = syntax.iter().filter_map(|d| d.fix.clone()).collect();
        assert_eq!(apply_fixes(fs.source(file), &fixes), b"a = { b = { c = 1}}");
    }

    #[test]
    fn syntax_summary_and_diagnostic_retain_closing_edit() {
        let (fs, schema) = demo();
        let analysis = Linter::new(schema).analyze(&fs);
        let summary = analysis.summary(FileId(3)).unwrap();
        let error = summary.syntax_errors.first().unwrap();
        assert_eq!(
            error.range,
            TextRange::empty(TextSize::of(fs.source(FileId(3))))
        );
        assert_eq!(error.opening_range, Some(TextRange::span(9, 10)));
        assert!(error.missing_delimiter);
        assert_eq!(error.fix.as_ref().unwrap().new_text, b"}");

        let diagnostic = analysis
            .diagnostics()
            .iter()
            .find(|diagnostic| diagnostic.id == lints::SYNTAX_ERROR)
            .unwrap();
        assert_eq!(diagnostic.range, error.range);
        assert_eq!(diagnostic.fix.as_ref().unwrap().new_text, b"}");
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
            Kind::entity("building"),
            "castle".into(),
            FileId(0),
            TextRange::span(0, 6),
        );
        index.finalize(&fs);
        assert!(matches!(
            index.resolve(&Kind::entity("building"), "castle", false),
            Resolution::Found(_)
        ));
        assert_eq!(
            index.resolve(&Kind::entity("building"), "ghost", false),
            Resolution::NotFound
        );
        assert_eq!(
            index.resolve(&Kind::entity("building"), "ghost", true),
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
    fn render_leaves_out_a_crlf_line_break() {
        let mut fs = Fileset::new();
        let f = fs.add("a.txt", FileKind::Vanilla, b"a = 1\r\nbb = 2\r\n".to_vec());
        let d = Diagnostic {
            id: lints::SYNTAX_ERROR,
            severity: Severity::Error,
            confidence: Confidence::Strong,
            file: f,
            range: TextRange::span(10, 11),
            message: "x".into(),
            help: None,
            fix: None,
        };
        assert_eq!(
            render(&d, &fs, false),
            "error[syntax-error]: x\n     --> a.txt:2:4\n     |\n   2 | bb = 2\n     |    ^\n"
        );
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
            (fref.kind.as_entity(), fref.name.as_str()),
            (Some("building"), "fortress")
        );

        // Go to definition finds the `fortress` that wins, in a different file.
        let def = analysis
            .definition_for(events, TextSize::new(35))
            .expect("a definition to jump to");
        assert_eq!(
            (def.kind.as_entity(), def.name.as_str()),
            (Some("building"), "fortress")
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
        let uses: Vec<_> = analysis
            .references_to(&Kind::entity("building"), "fortress")
            .collect();
        assert_eq!(uses.len(), 1, "one use site for fortress: {uses:?}");
        let (file, r) = uses[0];
        assert_eq!(file, FileId(2));
        assert_eq!(r.name, "fortress");

        // `castle` is referenced once (the mod's `upgrades_from = castle`).
        assert_eq!(
            analysis
                .references_to(&Kind::entity("building"), "castle")
                .count(),
            1
        );
        // A name used nowhere yields nothing.
        assert_eq!(
            analysis
                .references_to(&Kind::entity("building"), "nonesuch")
                .count(),
            0
        );
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

    /// A CK3 project with shared variables in `reader_export` and a file
    /// that uses them.
    fn variables() -> (Fileset, Schema, FileId, FileId) {
        let mut fs = Fileset::new();
        let shared = fs.add(
            "common/reader_export/00_values.txt",
            FileKind::Vanilla,
            b"@shared = 2\n@other = 3\n".to_vec(),
        );
        let source =
            b"@local = 1\nx = { a = @shared b = @[shared * local] c = @sharde d = @local }\n";
        let user = fs.add(
            "common/great_projects/p.txt",
            FileKind::Vanilla,
            source.to_vec(),
        );
        let mut schema = Schema::new();
        schema
            .define_dir("great_projects", "great_project")
            .variable_dir("reader_export");
        (fs, schema, shared, user)
    }

    #[test]
    fn variable_from_a_variable_dir() {
        let (fs, schema, shared, user) = variables();
        let analysis = Linter::new(schema).analyze(&fs);

        // Only the typo has no definition.
        let diagnostics = analysis.diagnostics();
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        let diagnostic = &diagnostics[0];
        assert_eq!(diagnostic.id, lints::UNDEFINED_REFERENCE);
        assert_eq!(diagnostic.file, user);
        assert_eq!(&fs.source(user)[diagnostic.range.to_range()], b"@sharde");
        assert_eq!(diagnostic.help.as_deref(), Some("did you mean `@shared`?"));

        // A variable definition is not a definition of an entity.
        let entities: Vec<_> = analysis
            .definitions()
            .iter()
            .map(|d| d.name.as_str())
            .collect();
        // The index has the files in game path order.
        assert_eq!(entities, ["x", "@shared", "@other"]);

        // A shared variable in a calculation goes to its definition.
        let source = fs.source(user);
        let offset = source.windows(7).position(|w| w == b"shared ").unwrap();
        let definition = analysis
            .definition_for(user, TextSize::try_from(offset).unwrap())
            .unwrap();
        assert_eq!(
            (definition.file, definition.name.as_str()),
            (shared, "@shared")
        );
    }

    #[test]
    fn variable_without_a_variable_dir_is_undefined() {
        let (fs, _, _, user) = variables();
        let mut schema = Schema::new();
        schema.define_dir("great_projects", "great_project");
        let diagnostics = Linter::new(schema).run(&fs);
        let names: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.file == user)
            .map(|d| &fs.source(user)[d.range.to_range()])
            .collect();
        assert_eq!(names, [&b"@shared"[..], b"shared", b"@sharde"]);
    }

    #[test]
    fn variable_value_errors() {
        let mut fs = Fileset::new();
        let file = fs.add(
            "common/x.txt",
            FileKind::Vanilla,
            b"a = @[1 / 0]\nb = @[min(1, 2)]\n".to_vec(),
        );
        let diagnostics = Linter::new(Schema::new()).run(&fs);
        let found: Vec<_> = diagnostics
            .iter()
            .map(|d| (d.id, d.severity, &fs.source(file)[d.range.to_range()]))
            .collect();
        assert_eq!(
            found,
            [
                (lints::VARIABLE_VALUE, Severity::Error, &b"1 / 0"[..]),
                (lints::VARIABLE_VALUE, Severity::Warning, b"@[min(1, 2)]"),
            ]
        );
    }

    /// A CK3 `reader_export` file can define a variable with
    /// `@:register_variable`, and a file can read it with
    /// `@:load_variable` or a `@@name` key.
    #[test]
    fn reader_directive_variables() {
        let mut fs = Fileset::new();
        let shared = fs.add(
            "common/reader_export/00_values.txt",
            FileKind::Vanilla,
            b"@:register_variable shared = 2\n".to_vec(),
        );
        let user = fs.add(
            "common/great_projects/p.txt",
            FileKind::Vanilla,
            b"x = { a = @:load_variable shared @@shared = 1 b = @:load_variable sharde }\n"
                .to_vec(),
        );
        let mut schema = Schema::new();
        schema
            .define_dir("great_projects", "great_project")
            .variable_dir("reader_export");
        let analysis = Linter::new(schema).analyze(&fs);

        let diagnostics = analysis.diagnostics();
        assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
        assert_eq!(diagnostics[0].file, user);
        assert_eq!(&fs.source(user)[diagnostics[0].range.to_range()], b"sharde");
        assert_eq!(
            diagnostics[0].help.as_deref(),
            Some("did you mean `@shared`?")
        );

        let source = fs.source(user);
        let offset = source.windows(8).position(|w| w == b"@@shared").unwrap();
        let definition = analysis
            .definition_for(user, TextSize::try_from(offset).unwrap())
            .unwrap();
        assert_eq!(
            (definition.file, definition.name.as_str()),
            (shared, "@shared")
        );
    }

    /// A fileset with a base file in `dir`, and an event file.
    fn two_files(dir: &str, base: &[u8], events: &[u8]) -> (Fileset, FileId) {
        let mut fs = Fileset::new();
        fs.add(
            format!("common/{dir}/base.txt"),
            FileKind::Vanilla,
            base.to_vec(),
        );
        let events = fs.add("events/e.txt", FileKind::Mod(0), events.to_vec());
        (fs, events)
    }

    fn schema() -> Schema {
        let mut schema = Schema::new();
        schema
            .define_dir("buildings", "building")
            .reference_key("add_building", "building")
            .variable_dir("reader_export");
        schema
    }

    /// The undefined references of `events` as (message, fix text).
    fn undefined(fs: &Fileset, linter: &Linter, events: FileId) -> Vec<(String, Option<String>)> {
        linter
            .run(fs)
            .into_iter()
            .filter(|d| d.file == events && d.id == lints::UNDEFINED_REFERENCE)
            .map(|d| {
                let fix = d.fix.map(|f| String::from_utf8(f.new_text).unwrap());
                (d.message, fix)
            })
            .collect()
    }

    #[test]
    fn a_fix_keeps_the_form_of_the_reference() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@shared = 1",
            b"@@sharde = 1\nx = @:load_variable sharde\ny = @[sharde + 1]\nz = @sharde",
        );
        let fixes: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(_, fix)| fix)
            .collect();
        assert_eq!(
            fixes,
            [
                Some("@@shared".to_owned()),
                Some("shared".to_owned()),
                Some("shared".to_owned()),
                Some("@shared".to_owned()),
            ]
        );

        // A quoted name keeps its quotes, and a name with a space cannot be
        // an unquoted reference.
        let (fs, events) = two_files(
            "buildings",
            br#""a b" = {}"#,
            br#"x = { add_building = "a c" } y = { add_building = a_c }"#,
        );
        let fixes: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(_, fix)| fix)
            .collect();
        assert_eq!(fixes, [Some("\"a b\"".to_owned()), None]);
    }

    #[test]
    fn each_undefined_variable_of_a_calculation_is_a_reference() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files("reader_export", b"@known = 1", b"x = @[known + missing]");
        let messages: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(message, _)| message)
            .collect();
        assert_eq!(
            messages,
            ["no variable named `@missing` is defined in any loaded file"]
        );
    }

    #[test]
    fn a_syntax_error_suppresses_only_the_references_of_its_block() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"good = { add_building = absent }\nbroken = {\n  add_building = missing\n",
        );
        let ids: Vec<_> = linter
            .run(&fs)
            .into_iter()
            .filter(|d| d.file == events)
            .map(|d| (d.id, d.message))
            .collect();
        assert_eq!(
            ids,
            [
                (
                    lints::UNDEFINED_REFERENCE,
                    "no building named `absent` is defined in any loaded file".to_owned()
                ),
                (
                    lints::SUPPRESSED_REFERENCE,
                    "`missing` may be an undefined building, but its block has a syntax error — suppressed".to_owned()
                ),
                (lints::SYNTAX_ERROR, "unclosed '{'".to_owned()),
            ]
        );
    }

    #[test]
    fn a_reference_through_a_variable_uses_its_value() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"@good = barracks\na = { add_building = @good }\n@bad = barack\nb = { add_building = @bad }",
        );
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no building named `barack` is defined in any loaded file".to_owned(),
                None
            )]
        );
    }

    #[test]
    fn a_flavor_parses_each_file() {
        let (fs, events) = two_files("buildings", b"barracks = {}", b"add_building = $NAME$");
        let ids = |linter: Linter| {
            linter
                .run(&fs)
                .into_iter()
                .filter(|d| d.file == events)
                .map(|d| d.id)
                .collect::<Vec<_>>()
        };
        // `$NAME$` is a macro parameter, not a reference.
        assert_eq!(ids(Linter::new(schema())), Vec::<&str>::new());
        // HoI4 has no macros, so `$NAME$` is a name that no file defines.
        assert_eq!(
            ids(Linter::new(schema()).with_flavor(Flavor::hoi4())),
            [lints::UNDEFINED_REFERENCE]
        );

        // An analysis of a different flavor is made again.
        let hoi4 = Linter::new(schema()).with_flavor(Flavor::hoi4());
        let mut analysis = Linter::new(schema()).analyze(&fs);
        hoi4.reanalyze(&fs, &mut analysis);
        let ids: Vec<_> = analysis.diagnostics().iter().map(|d| d.id).collect();
        assert_eq!(ids, [lints::UNDEFINED_REFERENCE]);
    }

    #[test]
    fn finalize_again_picks_the_winners_again() {
        let mut fs = Fileset::new();
        let base = fs.add("a.txt", FileKind::Vanilla, Vec::new());
        let mod_file = fs.add("b.txt", FileKind::Mod(0), Vec::new());
        let mut index = Index::new();
        index.define(
            Kind::entity("building"),
            "barracks".into(),
            base,
            TextRange::span(0, 1),
        );
        index.finalize(&fs);
        index.define(
            Kind::entity("building"),
            "barracks".into(),
            mod_file,
            TextRange::span(0, 1),
        );
        index.finalize(&fs);
        let winners: Vec<_> = index
            .definitions()
            .iter()
            .map(|d| (d.file, d.winner))
            .collect();
        assert_eq!(winners, [(base, false), (mod_file, true)]);
        assert_eq!(
            index.resolve(&Kind::entity("building"), "barracks", false),
            Resolution::Found(DefId(1))
        );
    }

    #[test]
    fn a_calculation_fix_is_one_identifier() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@foo-bar = 7\n@shared = 1",
            b"x = @[foobar]\ny = @[sharde]\nz = @foobar",
        );
        let fixes: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(_, fix)| fix)
            .collect();
        // `foo-bar` in a calculation is a subtraction. A variable can be in
        // a calculation, so no variable fix holds such a name.
        assert_eq!(fixes, [None, Some("shared".to_owned()), None]);
    }

    #[test]
    fn a_stray_brace_at_the_root_suppresses_no_reference() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"good = { add_building = absent }\n}",
        );
        let ids: Vec<_> = linter
            .run(&fs)
            .into_iter()
            .filter(|d| d.file == events)
            .map(|d| d.id)
            .collect();
        assert_eq!(ids, [lints::UNDEFINED_REFERENCE, lints::SYNTAX_ERROR]);
    }

    #[test]
    fn a_variable_key_uses_the_value_of_its_variable() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"@name = barracks\n@@name = {}",
            b"a = { add_building = barracks }\n@effect = add_building\nb = { @@effect = absent }",
        );
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no building named `absent` is defined in any loaded file".to_owned(),
                None
            )]
        );
    }

    #[test]
    fn a_deferred_body_exports_no_variable() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@:define unused = { @ghost = 1 }\n@real = 1",
            b"x = @ghost\ny = @real",
        );
        let messages: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(message, _)| message)
            .collect();
        assert_eq!(
            messages,
            ["no variable named `@ghost` is defined in any loaded file"]
        );
    }

    #[test]
    fn a_file_does_not_satisfy_its_own_variable_reference() {
        let linter = Linter::new(schema());
        let mut fs = Fileset::new();
        let shared = fs.add(
            "common/reader_export/a.txt",
            FileKind::Vanilla,
            b"@loop = @loop\nx = @late\n@late = 1".to_vec(),
        );
        let messages: Vec<_> = undefined(&fs, &linter, shared)
            .into_iter()
            .map(|(message, _)| message)
            .collect();
        assert_eq!(
            messages,
            [
                "no variable named `@loop` is defined in any loaded file",
                "no variable named `@late` is defined in any loaded file",
            ]
        );

        // A different file satisfies it.
        fs.add(
            "common/reader_export/b.txt",
            FileKind::Vanilla,
            b"@late = 2".to_vec(),
        );
        let messages: Vec<_> = undefined(&fs, &linter, shared)
            .into_iter()
            .map(|(message, _)| message)
            .collect();
        assert_eq!(
            messages,
            ["no variable named `@loop` is defined in any loaded file"]
        );
    }

    #[test]
    fn navigation_and_suggestions_use_the_definitions_of_other_files() {
        let linter = Linter::new(schema());
        let mut fs = Fileset::new();
        let base = fs.add(
            "common/reader_export/a.txt",
            FileKind::Vanilla,
            b"@late = 2".to_vec(),
        );
        let user = fs.add(
            "common/reader_export/b.txt",
            FileKind::Mod(0),
            b"x = @late\n@late = 1\n@loop = @loop".to_vec(),
        );
        let analysis = linter.analyze(&fs);
        let definition = analysis.definition_for(user, TextSize::new(5)).unwrap();
        assert_eq!((definition.file, definition.name.as_str()), (base, "@late"));

        // `@loop = @loop` has no definition in a different file, so it has
        // no suggestion.
        let loops: Vec<_> = analysis
            .diagnostics()
            .iter()
            .filter(|d| d.file == user && d.id == lints::UNDEFINED_REFERENCE)
            .map(|d| (d.message.as_str(), d.help.as_deref(), d.fix.is_some()))
            .collect();
        assert_eq!(
            loops,
            [(
                "no variable named `@loop` is defined in any loaded file",
                None,
                false
            )]
        );
    }

    #[test]
    fn a_variable_fix_in_a_calculation_is_one_identifier_also_with_its_sign() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files("reader_export", b"@foo-bar = 7", b"x = @[@foobar]");
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no variable named `@foobar` is defined in any loaded file".to_owned(),
                None
            )]
        );
    }

    #[test]
    fn the_distance_of_long_names_has_linear_cost() {
        let long = vec![b'a'; 100_000];
        let mut longer = long.clone();
        longer.push(b'b');
        assert_eq!(levenshtein_within(&long, &longer, 3), Some(1));
        assert_eq!(levenshtein_within(&longer, &long, 3), Some(1));
        let mut far = long.clone();
        far[0] = b'x';
        far[50_000] = b'y';
        far[99_999] = b'z';
        assert_eq!(levenshtein_within(&long, &far, 3), Some(3));
        assert_eq!(levenshtein_within(&long, &far, 2), None);
        assert_eq!(levenshtein_within(b"kitten", b"sitting", 3), Some(3));
        assert_eq!(levenshtein_within(b"", b"ab", 2), Some(2));
        assert_eq!(levenshtein_within(b"ab", b"", 1), None);
    }

    #[test]
    fn a_variable_fix_is_not_a_directive() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@:register_variable :log = 1",
            b"x = @log\nnext = 2",
        );
        let fixes: Vec<_> = undefined(&fs, &linter, events)
            .into_iter()
            .map(|(_, fix)| fix)
            .collect();
        assert_eq!(fixes, [None]);
    }

    #[test]
    fn the_names_of_one_file_have_one_encoding() {
        // The file is not UTF-8, so both keys are Windows-1252: `café` and
        // `cafÃ©` are different names.
        let mut fs = Fileset::new();
        fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"caf\xe9 = {}\ncaf\xc3\xa9 = {}".to_vec(),
        );
        let analysis = Linter::new(schema()).analyze(&fs);
        assert_eq!(analysis.diagnostics().len(), 0);
        let names: Vec<_> = analysis
            .definitions()
            .iter()
            .map(|d| d.name.as_str())
            .collect();
        assert_eq!(names, ["caf\u{e9}", "caf\u{c3}\u{a9}"]);

        // A UTF-8 file and a Windows-1252 file spell the same name.
        let (fs, events) = two_files(
            "buildings",
            b"caf\xe9 = {}",
            "add_building = caf\u{e9}".as_bytes(),
        );
        assert_eq!(undefined(&fs, &Linter::new(schema()), events), []);
    }

    #[test]
    fn only_the_first_definition_of_a_variable_is_exported() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files("reader_export", b"@n = 1\n@n = 2", b"x = @n");
        let analysis = linter.analyze(&fs);
        let definition = analysis.definition_for(events, TextSize::new(4)).unwrap();
        assert_eq!(definition.range, TextRange::span(0, 2));
        let variables: Vec<_> = analysis
            .definitions()
            .iter()
            .map(|d| (d.name.as_str(), d.range))
            .collect();
        assert_eq!(variables, [("@n", TextRange::span(0, 2))]);
    }

    #[test]
    fn references_to_borrows_only_the_analysis() {
        fn refs(analysis: &Analysis, name: String) -> impl Iterator<Item = (FileId, &RefSite)> {
            analysis.references_to(&Kind::entity("building"), &name)
        }

        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"a = { add_building = barracks }",
        );
        let analysis = linter.analyze(&fs);
        let found: Vec<_> = refs(&analysis, String::from("barracks"))
            .map(|(file, r)| (file, r.range))
            .collect();
        assert_eq!(found, [(events, TextRange::span(21, 29))]);
    }

    #[test]
    fn an_explicit_encoding_decodes_each_file() {
        // Both files are Windows-1252. The reference file has a comment
        // that is not UTF-8, so a guess gives the two files two encodings.
        let (fs, events) = two_files(
            "buildings",
            b"caf\xc3\xa9 = {}",
            b"add_building = caf\xc3\xa9 # \xe9",
        );
        let messages = |linter: &Linter| {
            undefined(&fs, linter, events)
                .into_iter()
                .map(|(message, _)| message)
                .collect::<Vec<_>>()
        };
        assert_eq!(
            messages(&Linter::new(schema())),
            ["no building named `caf\u{c3}\u{a9}` is defined in any loaded file"]
        );
        let windows = Linter::new(schema()).with_encoding(NameEncoding::Windows1252);
        assert_eq!(messages(&windows), Vec::<String>::new());

        // An analysis of a different encoding is made again.
        let mut analysis = Linter::new(schema()).analyze(&fs);
        windows.reanalyze(&fs, &mut analysis);
        assert_eq!(analysis.diagnostics().len(), 0);
    }

    #[test]
    fn a_replaced_file_has_no_definitions_and_no_diagnostics() {
        let mut fs = Fileset::new();
        let game = "common/buildings/00.txt";
        fs.add_with_game_path(
            "game/".to_owned() + game,
            game,
            FileKind::Vanilla,
            b"barracks = {}\nbroken = {".to_vec(),
        );
        let replacement = fs.add_with_game_path(
            "mod/".to_owned() + game,
            game,
            FileKind::Mod(0),
            b"stables = {}".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barracks }\nb = { add_building = stables }".to_vec(),
        );
        let analysis = Linter::new(schema()).analyze(&fs);
        let diagnostics: Vec<_> = analysis
            .diagnostics()
            .iter()
            .map(|d| (d.file, d.message.as_str()))
            .collect();
        assert_eq!(
            diagnostics,
            [(
                events,
                "no building named `barracks` is defined in any loaded file"
            )]
        );
        let names: Vec<_> = analysis
            .definitions()
            .iter()
            .map(|d| (d.file, d.name.as_str()))
            .collect();
        assert_eq!(names, [(replacement, "stables")]);
    }

    #[test]
    fn reanalyze_follows_a_replacement_that_is_added_and_removed() {
        let mut fs = Fileset::new();
        let game = "common/buildings/00.txt";
        fs.add_with_game_path(
            "game/".to_owned() + game,
            game,
            FileKind::Vanilla,
            b"barracks = {}".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barracks }".to_vec(),
        );
        let linter = Linter::new(schema());
        let mut analysis = linter.analyze(&fs);
        assert!(analysis.diagnostics().is_empty());

        let replacement = fs.add_with_game_path(
            "mod/".to_owned() + game,
            game,
            FileKind::Mod(0),
            b"stables = {}".to_vec(),
        );
        linter.reanalyze(&fs, &mut analysis);
        let diagnostics: Vec<_> = analysis
            .diagnostics()
            .iter()
            .map(|d| (d.file, d.message.as_str()))
            .collect();
        assert_eq!(
            diagnostics,
            [(
                events,
                "no building named `barracks` is defined in any loaded file"
            )]
        );

        assert!(fs.remove(replacement));
        linter.reanalyze(&fs, &mut analysis);
        assert!(analysis.diagnostics().is_empty());
    }

    #[test]
    fn a_quoted_directive_argument_has_one_name() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@:register_variable \"shared\" = 1",
            b"x = @:load_variable \"shared\"",
        );
        assert_eq!(undefined(&fs, &linter, events), []);
    }

    #[test]
    fn the_schema_matches_the_game_path() {
        let mut fs = Fileset::new();
        fs.add_with_game_path(
            "/cache/42.txt",
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barracks = {}".to_vec(),
        );
        fs.add_with_game_path(
            "/cache/43.txt",
            "common/reader_export/v.txt",
            FileKind::Vanilla,
            b"@shared = 1".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barracks }\nb = @shared".to_vec(),
        );
        assert_eq!(undefined(&fs, &Linter::new(schema()), events), []);
    }

    #[test]
    fn many_variable_references_in_a_broken_block_have_linear_cost() {
        let mut events = b"broken = {\n".to_vec();
        for _ in 0..100_000 {
            events.extend_from_slice(b"x = @shared\n");
        }
        let (fs, events) = two_files("reader_export", b"@shared = 1", &events);
        // Before the cache, this analysis took minutes.
        let analysis = Linter::new(schema()).analyze(&fs);
        let ids: Vec<_> = analysis
            .diagnostics()
            .iter()
            .filter(|d| d.file == events)
            .map(|d| d.id)
            .collect();
        assert_eq!(ids, [lints::SYNTAX_ERROR]);
    }

    #[test]
    fn only_a_macro_parameter_of_the_flavor_is_not_a_reference() {
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"a = { add_building = $P$ }\nb = { add_building = a$b }\n@x = $Q$\nc = { add_building = @x }",
        );
        let messages = |linter: Linter| {
            undefined(&fs, &linter, events)
                .into_iter()
                .map(|(message, _)| message)
                .collect::<Vec<_>>()
        };
        assert_eq!(
            messages(Linter::new(schema())),
            ["no building named `a$b` is defined in any loaded file"]
        );
        // HoI4 has no macros, so `$P$` is a name.
        assert_eq!(
            messages(Linter::new(schema()).with_flavor(Flavor::hoi4())),
            [
                "no building named `$P$` is defined in any loaded file",
                "no building named `a$b` is defined in any loaded file",
                "no building named `$Q$` is defined in any loaded file",
            ]
        );
        assert!(is_macro(b"$NAME$"));
        assert!(is_macro(b"a$b$c"));
        assert!(is_macro(b"$$"));
        assert!(!is_macro(b"a$b"));
    }

    #[test]
    fn many_references_to_one_undefined_name_have_one_suggestion() {
        let mut definitions = Vec::new();
        for i in 0..1000 {
            definitions.extend_from_slice(format!("building_{i:04} = {{}}\n").as_bytes());
        }
        let events = b"a = { add_building = building_000x }\n".repeat(1000);
        let (fs, events) = two_files("buildings", &definitions, &events);
        let helps: Vec<_> = Linter::new(schema())
            .run(&fs)
            .into_iter()
            .filter(|d| d.file == events)
            .map(|d| d.help)
            .collect();
        assert_eq!(helps.len(), 1000);
        assert!(
            helps
                .iter()
                .all(|h| h.as_deref() == Some("did you mean `building_0000`?"))
        );
    }

    #[test]
    fn a_removed_file_is_not_in_the_set() {
        let mut fs = Fileset::new();
        let a = fs.add("a.txt", FileKind::Vanilla, b"x = 1".to_vec());
        let b = fs.add("b.txt", FileKind::Vanilla, b"y = 1".to_vec());
        assert!(fs.remove(a));
        assert!(!fs.remove(a));
        assert!(!fs.remove(FileId(9)));
        assert_eq!(fs.ids().collect::<Vec<_>>(), [b]);
        assert_eq!(fs.id_for_path("a.txt"), None);
        assert_eq!(fs.source(a), b"");
        assert_eq!(fs.path(b), Path::new("b.txt"));
    }

    #[test]
    fn a_quoted_parameter_is_not_a_reference() {
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"@target = \"$BUILDING$\"\na = { add_building = @target }\nb = { add_building = \"$BUILDING$\" }\nc = { add_building = $$ }",
        );
        assert_eq!(undefined(&fs, &Linter::new(schema()), events), []);
    }

    #[test]
    fn a_removed_file_does_not_take_a_new_source() {
        let linter = Linter::new(schema());
        let mut fs = Fileset::new();
        let buildings = fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barracks = {}".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barracks }".to_vec(),
        );
        let mut analysis = linter.analyze(&fs);
        assert!(fs.remove(buildings));
        assert!(!fs.set_source(buildings, b"barracks = {}".to_vec()));
        assert!(!fs.set_source(FileId(9), Vec::new()));
        assert!(fs.set_source(events, b"b = { add_building = barracks }".to_vec()));
        linter.reanalyze(&fs, &mut analysis);
        let messages: Vec<_> = analysis
            .diagnostics()
            .iter()
            .map(|d| d.message.as_str())
            .collect();
        assert_eq!(
            messages,
            ["no building named `barracks` is defined in any loaded file"]
        );
    }

    #[test]
    fn the_last_file_in_game_path_order_wins_in_a_layer() {
        let linter = Linter::new(schema());
        let mut fs = Fileset::new();
        let z = fs.add(
            "common/buildings/z.txt",
            FileKind::Mod(0),
            b"barracks = {}".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = barracks }".to_vec(),
        );
        let mut analysis = linter.analyze(&fs);
        // A file that a caller adds later is earlier in the load order.
        fs.add(
            "common/buildings/a.txt",
            FileKind::Mod(0),
            b"barracks = {}".to_vec(),
        );
        linter.reanalyze(&fs, &mut analysis);
        let definition = analysis.definition_for(events, TextSize::new(22)).unwrap();
        assert_eq!(definition.file, z);
        let duplicates: Vec<_> = analysis
            .diagnostics()
            .iter()
            .filter(|d| d.id == lints::DUPLICATE)
            .map(|d| fs.path(d.file).to_str().unwrap())
            .collect();
        assert_eq!(duplicates, ["common/buildings/a.txt"]);
    }

    #[test]
    fn a_directive_argument_keeps_each_sign_in_its_name() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@shared = 1\n@:register_variable @other = 2",
            b"x = @:load_variable @shared\ny = @:load_variable @other\nz = @@other\nw = @[shared + 1]",
        );
        // `@:load_variable @shared` asks for the name `@shared`, which no file
        // defines. The directive defines the name `@other`, which the
        // directive gives, and `@@other` asks for `other`.
        assert_eq!(
            undefined(&fs, &linter, events),
            [
                (
                    "no variable named `@@shared` is defined in any loaded file".to_owned(),
                    Some("shared".to_owned())
                ),
                (
                    "no variable named `@other` is defined in any loaded file".to_owned(),
                    None
                ),
            ]
        );
    }

    #[test]
    fn a_registration_value_is_an_ordinary_reference() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "reader_export",
            b"@shared = 1",
            b"@:register_variable local = @shared\n@:register_variable other = @sharde",
        );
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no variable named `@sharde` is defined in any loaded file".to_owned(),
                Some("@shared".to_owned())
            )]
        );
    }

    #[test]
    fn a_variable_key_value_uses_the_value_of_its_variable() {
        let linter = Linter::new(schema());
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            b"@target = barracks\na = { add_building = @@target }\n@bad = barack\nb = { add_building = @@bad }",
        );
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no building named `barack` is defined in any loaded file".to_owned(),
                None
            )]
        );
    }

    #[test]
    fn the_carets_count_characters() {
        let (fs, events) = two_files(
            "buildings",
            b"barracks = {}",
            "label = \"\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}\" add_building = absent".as_bytes(),
        );
        let diagnostics: Vec<_> = Linter::new(schema())
            .run(&fs)
            .into_iter()
            .filter(|d| d.file == events)
            .collect();
        assert_eq!(
            render(&diagnostics[0], &fs, false),
            "\
error[undefined-reference]: no building named `absent` is defined in any loaded file
     --> events/e.txt:1:33
     |
   1 | label = \"\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}\" add_building = absent
     |                                 ^^^^^^
"
        );
    }

    #[test]
    fn one_path_is_one_file() {
        let linter = Linter::new(schema());
        let mut fs = Fileset::new();
        let old = fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"barracks = {}".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"a = { add_building = stables }".to_vec(),
        );
        let mut analysis = linter.analyze(&fs);
        let current = fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"stables = {}".to_vec(),
        );
        assert_eq!(current, old);
        assert_eq!(fs.ids().collect::<Vec<_>>(), [old, events]);
        assert_eq!(fs.id_for_path("common/buildings/b.txt"), Some(old));
        linter.reanalyze(&fs, &mut analysis);
        assert_eq!(analysis.diagnostics().len(), 0);

        // An editor update of the file found by its path counts.
        assert!(fs.set_source(old, b"barracks = {}".to_vec()));
        linter.reanalyze(&fs, &mut analysis);
        let messages: Vec<_> = analysis
            .diagnostics()
            .iter()
            .map(|d| d.message.as_str())
            .collect();
        assert_eq!(
            messages,
            ["no building named `stables` is defined in any loaded file"]
        );

        // A removed path can be added again, as a new file.
        assert!(fs.remove(old));
        let again = fs.add(
            "common/buildings/b.txt",
            FileKind::Vanilla,
            b"stables = {}".to_vec(),
        );
        assert_ne!(again, old);
        assert_eq!(fs.id_for_path("common/buildings/b.txt"), Some(again));
    }

    #[test]
    fn the_utf8_check_of_a_file_follows_its_source() {
        let mut fs = Fileset::new();
        let id = fs.add("a.txt", FileKind::Vanilla, b"caf\xe9".to_vec());
        assert!(!fs.is_utf8(id));
        fs.set_source(id, "caf\u{e9}".as_bytes().to_vec());
        assert!(fs.is_utf8(id));
    }

    #[test]
    fn a_calculation_refers_to_the_entity_of_its_result() {
        let mut schema = Schema::new();
        schema.define_dir("ids", "id").reference_key("target", "id");
        let linter = Linter::new(schema);
        let (fs, events) = two_files(
            "ids",
            b"1 = {}",
            b"a = { target = @[1 + 1] }\nb = { target = @[2 - 1] }",
        );
        assert_eq!(
            undefined(&fs, &linter, events),
            [(
                "no id named `2` is defined in any loaded file".to_owned(),
                None
            )]
        );
        let analysis = linter.analyze(&fs);
        let definition = analysis.definition_for(events, TextSize::new(42)).unwrap();
        assert_eq!(definition.name, "1");
    }

    #[test]
    fn an_entity_kind_named_variable_is_an_entity() {
        let mut schema = Schema::new();
        schema
            .define_dir("vars", "variable")
            .reference_key("use_var", "variable")
            .variable_dir("reader_export");
        let linter = Linter::new(schema);
        let mut fs = Fileset::new();
        fs.add(
            "common/vars/v.txt",
            FileKind::Vanilla,
            b"foo = {}\nuse_var = foo".to_vec(),
        );
        fs.add(
            "common/reader_export/r.txt",
            FileKind::Vanilla,
            b"@bar = 1".to_vec(),
        );
        let events = fs.add(
            "events/e.txt",
            FileKind::Mod(0),
            b"use_var = bar\nx = @foo".to_vec(),
        );
        let messages: Vec<_> = linter
            .run(&fs)
            .into_iter()
            .map(|d| (d.file, d.message))
            .collect();
        // The entity `bar` and the variable `@foo` have no definition: the
        // variable `@bar` and the entity `foo` are of the other namespace.
        assert_eq!(
            messages,
            [
                (
                    events,
                    "no variable named `bar` is defined in any loaded file".to_owned()
                ),
                (
                    events,
                    "no variable named `@foo` is defined in any loaded file".to_owned()
                ),
            ]
        );
        assert_eq!(Kind::Variable.to_string(), "variable");
        assert_eq!(Kind::entity("variable").as_entity(), Some("variable"));
    }
}
