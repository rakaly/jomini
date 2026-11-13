# jomini-lsp

`jomini-lsp` is a language server for the script files of Paradox games
(EU4, EU5, CK3, HOI4, Vic3, Imperator). An editor starts the server and sends
it the files that you edit. The server sends back errors, quick fixes, and the
locations of definitions.

The server checks a mod and its base game together. Thus it finds an error
that is in the relation between two files, for example a reference to a
building that no file defines:

```text
error[undefined-reference]: no building named `baracks` is defined in any loaded file
  --> events/mod_events.txt:4:18
   = help: did you mean `barracks`?
```

The server uses the lossless parser (`jomini::text::lossless`), which keeps
every byte of a file. A quick fix changes only the bytes of the error. The
comments and the layout of the file do not change.

> **Experimental.** The rules of the server know only buildings
> (`common/buildings`, `add_building`, `has_building`, ...). The rules for
> each game are future work.

## Features

| Editor feature | What it does |
|---|---|
| Diagnostics | Syntax errors, undefined references, duplicate definitions, and mod overrides, for all the loaded files |
| Quick fix | Applies the "did you mean" name, or inserts a missing `}` |
| Go to definition | Goes to the definition that wins in the load order, also in a different file |
| Find references | Finds each use of the name under the cursor, in all the files |
| Document symbols | Lists the definitions of the file |
| Workspace symbols | Finds a definition by a part of its name (not case sensitive) |
| Format document | Formats the whole file with the indent of the editor |

## Build and run

The server has its own Cargo workspace, like `bench/`. Thus its dependencies
are not in the dependency graph of `jomini`.

```sh
cd jomini-lsp
cargo build --release
./target/release/jomini-lsp   # speaks LSP on stdin and stdout
```

### Neovim

```lua
vim.lsp.start({
  name = 'jomini-lsp',
  cmd = { '/path/to/jomini-lsp' },
  root_dir = vim.fs.root(0, { 'descriptor.mod', '.git' }),
  init_options = { vanilla = '/path/to/game' },
})
```

### VS Code

There is no extension yet. A small extension that starts the binary with
`vscode-languageclient` and `TransportKind.stdio` works.

### Settings

Send these in `initializationOptions`:

| Key | Value |
|---|---|
| `vanilla` | The absolute path of the `game` directory of the base game. The server loads it below the workspace, so a mod definition overrides a base game definition. |

The server loads each workspace folder as one mod, in the order of the
folders. A later folder overrides an earlier one. When the server cannot
load a directory, it shows an error and continues with the files that it
loaded.

A file that you open and that is not on the disk, such as a new file, gets
the layer of the directory that holds it. Thus a new mod file replaces the
base game file at the same path. When you close it, the server removes it.

### Rules and flavor

The binary uses the rules for buildings. A program that runs the server
with `jomini_lsp::serve_with` gives a `Config` with the rules of its game
(a `jomini::text::lint::Schema`) and the parser flavor, such as
`Flavor::hoi4()`. The server uses the flavor for the lint and for
formatting.

## How it works

The server keeps all the files in memory, and the editor text replaces the
disk text of each open file. After each change, the server parses again only
the changed file. Then it resolves the references of all the files again,
because one change can change a definition in a different file. When
changes arrive faster than the server can analyze them, the server analyzes
one time for all of them.

| File | Job |
|---|---|
| `main.rs` | Connects the server to stdin and stdout |
| `lib.rs` | The `initialize` handshake and the message loop |
| `server.rs` | The state of the server and one handler for each request |
| `line_index.rs` | Converts byte offsets to LSP positions (UTF-16 or UTF-8 columns) |
| `convert.rs` | Converts lint types to LSP types |

## Limits

- The rules of the binary know only buildings, and they are in
  `Config::default` in `lib.rs`.
- The server reads the disk only at start and when you close a file. It does
  not see a file that a different program adds, changes, or deletes.
- A Windows-1252 file on disk has correct columns only for ASCII text.
- When the editor opens one file through two paths, such as through a
  symbolic link, only the first document has the features.
- There is no hover, completion, rename, or semantic highlighting.

## Tests

```sh
cargo test
```

`tests/smoke.rs` runs the server in the same process on the project in
`../crates/jomini/examples/mod-demo`, and tests each feature.
