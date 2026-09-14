# jomini-lsp

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)
server for Clausewitz (Paradox) game files — EU4 / CK3 / HOI4 / Vic3 / Imperator
and friends — built on jomini's **lossless** text syntax tree (`jomini::text::syntax`)
and its **cross-file lint layer** (`jomini::text::lint`).

This is the editor-facing payoff of the lossless tree: a single retained
[`Analysis`](../src/text/lint.rs) answers every query, and because the tree
preserves every byte, the quick-fixes it produces are byte-faithful.

It is a synchronous server on rust-analyzer's own stack
([`lsp-server`](https://crates.io/crates/lsp-server) + `lsp-types`), kept in its
**own workspace** (like `bench/`) so its transport dependencies never touch the
published `jomini` crate's graph.

## Features (first cut)

| LSP request | Backed by |
|---|---|
| `textDocument/publishDiagnostics` | `Linter::analyze` — undefined references, overrides, duplicates, syntax errors, suppressed-in-error-subtree |
| `textDocument/formatting` | `syntax::format` (whole-document; honors `tabSize`/`insertSpaces`) |
| `textDocument/definition` | `Analysis::definition_for` — resolves a reference to the *winning* definition, across files & load-order layers |
| `textDocument/references` | `Analysis::references_to` — every use site of the name under the cursor |
| `textDocument/documentSymbol` | per-file `FileSummary.defs` |
| `workspace/symbol` | `Index::definitions` (winning defs, fuzzy-matched by query) |
| `textDocument/codeAction` | each `Diagnostic.fix` → a `QuickFix` `TextEdit` (e.g. the "did you mean `barracks`?" rename) |

Cross-file is the point: an `add_building = baracks` typo is flagged in the file
it appears in, even though its (missing) definition would live in another file —
something a per-file checker fundamentally cannot do.

## Build & run

```sh
# from this directory (it is a standalone workspace)
cargo build --release
# the server speaks LSP over stdio:
./target/release/jomini-lsp
```

### Wiring into an editor

The server talks stdio and advertises its capabilities on `initialize`. Point any
generic LSP client at the binary for the `clausewitz` / plaintext file type.

**Neovim** (`nvim-lspconfig`-style, using a custom config):

```lua
vim.lsp.start({
  name = 'jomini-lsp',
  cmd = { '/path/to/jomini-lsp' },
  root_dir = vim.fs.dirname(vim.fs.find({ 'descriptor.mod', '.git' }, { upward = true })[1]),
  init_options = { vanilla = '/path/to/game/install/game' }, -- optional vanilla base
})
```

**VS Code**: a thin extension that launches the binary as a stdio server
(`vscode-languageclient`, `TransportKind.stdio`). (No extension is bundled yet.)

### Configuration

`initializationOptions`:

| key | meaning |
|---|---|
| `vanilla` | filesystem path to a base-game directory, loaded as the `Vanilla` layer beneath the workspace (which loads as `Mod(0)`). Mod definitions then override vanilla, matching Paradox load order. |

Position encoding is negotiated: UTF-8 when the client offers it, else UTF-16.

## Architecture

```
main.rs    stdio transport → jomini_lsp::serve
lib.rs     serve(): initialize handshake (+ encoding negotiation) and the dispatch loop
server.rs  Server state (Fileset overlay + retained Analysis) and one handler per feature
line_index byte offset ↔ LSP Position (UTF-16/UTF-8); the one mapping the tree leaves to us
convert    jomini lint types → lsp_types
```

The server keeps one `Fileset` (whose bytes double as the open-document overlay)
and one `Analysis`. Every edit re-runs `Linter::analyze` from scratch and
republishes — correct and simple. The library additions that make this possible
(`Analysis`, the reverse reference index, offset hit-testing, `Fileset::set_source`/
`id_for_path`) live in `jomini::text::lint`, the rust-analyzer-style split of
reusable analysis (`ide-db`) from the LSP shell.

## Known limitations / next steps

- **Prototype schema.** The rule set (what defines/references a "building") is the
  hand-written demo from `examples/lint.rs`, hardcoded in `server.rs`. A real
  per-game schema (or a `.cwt`-config front end feeding the same `Index`) is the
  follow-up.
- **Full re-analysis per edit.** A one-file edit can flip override winners
  project-wide, so the first cut re-summarizes everything on each change.
  Incremental reparse + incremental re-summarize is the planned optimization.
- **Whole-document formatting only** (no range/on-type formatting).
- **Encoding.** Positions assume UTF-8 document bytes (what the client sends).
  Windows-1252 files on disk are ASCII-exact but not yet fully encoding-aware — the
  same follow-up the lint layer notes.
- **Not yet implemented:** hover, completion, semantic-token highlighting, rename
  (the `Index` + `Fix` infra make these the natural next features).

## Testing

```sh
cargo test   # line_index unit tests + an in-process end-to-end smoke test
```

`tests/smoke.rs` drives the real `serve` loop over an in-memory `Connection`
against `../examples/mod-demo`, exercising diagnostics, symbols, go-to-definition,
find-references, code actions, and formatting without a subprocess.
