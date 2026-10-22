//! Lint a mod and its base game together with [`jomini::text::lint`].
//!
//! The example loads the base game files and the mod files as two layers, and
//! resolves each building reference against the buildings of both layers. It
//! reports:
//!
//! - an error for a reference to a building that no file defines, with a
//!   "did you mean?" fix when a defined name is near,
//! - a note for a mod building that overrides a base game building,
//! - a warning for a building that one layer defines two times,
//! - a weak note, not an error, for a reference in a block that has a syntax
//!   error.
//!
//! A fix replaces only the bytes of the name. The comments and the layout of
//! the file do not change.
//!
//! ```sh
//! cargo run --features lossless --example lint                 # lint the bundled examples/mod-demo
//! cargo run --features lossless --example lint -- --fix        # also show machine-applicable fixes
//! cargo run --features lossless --example lint -- --fix --write  # ...and write them to disk
//! cargo run --features lossless --example lint -- --no-color
//! cargo run --features lossless --example lint -- <vanilla-dir> <mod-dir>
//! ```

use jomini::text::lint::{
    FileKind, Fileset, Linter, Schema, Severity, apply_repairs, lints, render,
};
use jomini::text::lossless::Repair;
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

    // With no directories, use the demo in `examples/mod-demo`. The path comes
    // from the crate, so the example works from any directory.
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

    // Each directory is one load-order layer.
    let mut fileset = Fileset::new();
    let vanilla = fileset
        .load_dir(&vanilla_dir, FileKind::Vanilla)
        .unwrap_or_else(|e| {
            eprintln!("failed to read {}: {e}", vanilla_dir.display());
            std::process::exit(1);
        });
    let modded = fileset
        .load_dir(&mod_dir, FileKind::Mod(0))
        .unwrap_or_else(|e| {
            eprintln!("failed to read {}: {e}", mod_dir.display());
            std::process::exit(1);
        });

    let mut schema = Schema::new();
    schema
        .define_dir("buildings", "building")
        .reference_key("has_building", "building")
        .reference_key("add_building", "building")
        .reference_key("remove_building", "building")
        .reference_key("upgrades_from", "building");

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

    let count = |s: Severity| diagnostics.iter().filter(|d| d.severity == s).count();
    let errors = count(Severity::Error);
    let warnings = count(Severity::Warning);
    let notes = count(Severity::Tip);
    println!("{bold}Summary:{reset} {errors} error(s), {warnings} warning(s), {notes} note(s)");

    if diagnostics
        .iter()
        .any(|d| d.id == lints::UNDEFINED_REFERENCE)
    {
        println!(
            "{dim}The undefined-reference error spans files: the reference is in one file, \n\
             its (missing) definition would live in another — only a project-wide pass finds it.{reset}"
        );
    }

    if fix_mode {
        apply_and_report(&fileset, &diagnostics, write_mode, bold, dim, reset);
    }

    std::process::exit(if errors > 0 && !write_mode { 1 } else { 0 });
}

/// Show each fix as `old -> new` at its location. With `write`, also apply
/// the fixes to the files on disk.
fn apply_and_report(
    fileset: &Fileset,
    diagnostics: &[jomini::text::lint::Diagnostic],
    write: bool,
    bold: &str,
    dim: &str,
    reset: &str,
) {
    let mut by_file: BTreeMap<_, Vec<&Repair>> = BTreeMap::new();
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
            let at = fileset.line_col(*file, f.range.start());
            let old = String::from_utf8_lossy(&src[f.range.to_range()]);
            println!("  {}:{at}  `{old}` -> `{}`", path.display(), f.replacement);
        }
        if write {
            let owned: Vec<Repair> = fixes.iter().map(|f| (*f).clone()).collect();
            let fixed = apply_repairs(src, &owned);
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
