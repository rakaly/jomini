//! A **cross-file** linter for Clausewitz game files, built on the lossless
//! syntax tree's new project/semantic layer ([`jomini::text::lint`]).
//!
//! This is the thing a single-file parser fundamentally *cannot* do: it loads a
//! whole "mod" plus its "vanilla" base, builds a project-wide symbol table, and
//! then resolves every building reference against it. A reference to a building
//! defined in no file — possibly a typo — becomes an error with a precise
//! location *and a "did you mean?" fix*; a mod definition that shadows a vanilla
//! one is noted; a building defined twice in one layer is a warning; and a
//! reference inside a block with a syntax error is suppressed instead of
//! generating cascade noise.
//!
//! Because the tree is **lossless**, fixes are byte-faithful: the typo is
//! rewritten in place and everything else is preserved exactly — something a
//! lossy validator structurally cannot offer.
//!
//! ```sh
//! cargo run --example lint                 # lint the bundled examples/mod-demo
//! cargo run --example lint -- --fix        # also show machine-applicable fixes
//! cargo run --example lint -- --fix --write  # ...and write them to disk
//! cargo run --example lint -- --no-color
//! cargo run --example lint -- <vanilla-dir> <mod-dir>
//! ```

use jomini::text::lint::{
    apply_fixes, line_col, lints, render, FileKind, Fileset, Fix, Linter, Schema, Severity,
};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

fn main() {
    let mut color = true;
    let mut fix_mode = false;
    let mut write_mode = false;
    let mut dirs: Vec<PathBuf> = Vec::new();
    for arg in std::env::args().skip(1) {
        match arg.as_str() {
            "--no-color" => color = false,
            "--fix" => fix_mode = true,
            "--write" => {
                fix_mode = true;
                write_mode = true;
            }
            _ => dirs.push(PathBuf::from(arg)),
        }
    }

    // Default to the bundled two-layer demo corpus (resolved relative to the
    // crate so `cargo run --example lint` works from anywhere).
    let manifest = Path::new(env!("CARGO_MANIFEST_DIR"));
    let (vanilla_dir, mod_dir) = match dirs.as_slice() {
        [] => (
            manifest.join("examples/mod-demo/vanilla"),
            manifest.join("examples/mod-demo/mod"),
        ),
        [v, m] => (v.clone(), m.clone()),
        _ => {
            eprintln!("usage: lint [--no-color] [--fix] [--write] [<vanilla-dir> <mod-dir>]");
            std::process::exit(2);
        }
    };

    // 1. Project model: load each directory tree as a load-order layer.
    let mut fileset = Fileset::new();
    let vanilla = fileset.load_dir(&vanilla_dir, FileKind::Vanilla).unwrap_or_else(|e| {
        eprintln!("failed to read {}: {e}", vanilla_dir.display());
        std::process::exit(1);
    });
    let modded = fileset.load_dir(&mod_dir, FileKind::Mod(0)).unwrap_or_else(|e| {
        eprintln!("failed to read {}: {e}", mod_dir.display());
        std::process::exit(1);
    });

    // 2. Schema: the (hand-written) rules of what defines and references a
    //    building. A real linter would carry a per-game rule set here.
    let mut schema = Schema::new();
    schema
        .define_dir("buildings", "building")
        .reference_key("has_building", "building")
        .reference_key("add_building", "building")
        .reference_key("remove_building", "building")
        .reference_key("upgrades_from", "building");

    // 3. Two-pass lint: collect all definitions, then resolve all references.
    let diagnostics = Linter::new(schema).run(&fileset);

    let bold = if color { "\x1b[1m" } else { "" };
    let dim = if color { "\x1b[2m" } else { "" };
    let reset = if color { "\x1b[0m" } else { "" };

    println!(
        "{bold}Linting {} vanilla + {} mod file(s){reset} {dim}(one combined project){reset}\n",
        vanilla.len(),
        modded.len(),
    );

    for diag in &diagnostics {
        print!("{}", render(diag, &fileset, color));
        println!();
    }

    // Summary, broken down by the Severity axis.
    let count = |s: Severity| diagnostics.iter().filter(|d| d.severity == s).count();
    let errors = count(Severity::Error);
    let warnings = count(Severity::Warning);
    let notes = count(Severity::Tip);
    println!("{bold}Summary:{reset} {errors} error(s), {warnings} warning(s), {notes} note(s)");

    // The punchline: the undefined-reference catch is impossible per-file.
    if diagnostics.iter().any(|d| d.id == lints::UNDEFINED_REFERENCE) {
        println!(
            "{dim}The undefined-reference error spans files: the reference is in one file, \n\
             its (missing) definition would live in another — only a project-wide pass finds it.{reset}"
        );
    }

    // 4. Autofix: apply each diagnostic's machine-applicable fix. The lossless
    //    tree makes this byte-faithful — only the offending token changes.
    if fix_mode {
        apply_and_report(&fileset, &diagnostics, write_mode, bold, dim, reset);
    }

    std::process::exit(if errors > 0 && !write_mode { 1 } else { 0 });
}

/// Group the fixes by file, show each as `old -> new` at its location, and —
/// with `--write` — splice them into the file on disk via `apply_fixes`.
fn apply_and_report(
    fileset: &Fileset,
    diagnostics: &[jomini::text::lint::Diagnostic],
    write: bool,
    bold: &str,
    dim: &str,
    reset: &str,
) {
    let mut by_file: BTreeMap<_, Vec<&Fix>> = BTreeMap::new();
    for d in diagnostics {
        if let Some(fix) = &d.fix {
            by_file.entry(d.file).or_default().push(fix);
        }
    }

    if by_file.is_empty() {
        println!("\n{dim}No automatic fixes available.{reset}");
        return;
    }

    println!("\n{bold}Fixes:{reset}");
    for (file, fixes) in &by_file {
        let src = fileset.source(*file);
        let path = fileset.path(*file);
        for f in fixes {
            let (l, c) = line_col(src, f.range.0);
            let old = String::from_utf8_lossy(&src[f.range.0 as usize..f.range.1 as usize]);
            println!("  {}:{l}:{c}  `{old}` -> `{}`", path.display(), f.replacement);
        }
        if write {
            let owned: Vec<Fix> = fixes.iter().map(|f| (*f).clone()).collect();
            let fixed = apply_fixes(src, &owned);
            match std::fs::write(path, &fixed) {
                Ok(()) => println!("  {dim}wrote {}{reset}", path.display()),
                Err(e) => eprintln!("  failed to write {}: {e}", path.display()),
            }
        }
    }
    if !write {
        println!("\n{dim}(dry run — re-run with --write to apply these fixes to disk){reset}");
    }
}
