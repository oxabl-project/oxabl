# End-to-end tests

These tests install the packaged VSIX in a real VS Code and check what a
person sees. They are black-box tests: they read the editor DOM, the
`--list-extensions` output and the requests of a fake GitHub API. They do not
import the extension source.

```bash
xvfb-run -a pnpm run test:e2e
```

`update-skip` edits VS Code's state database with `node:sqlite` (Node 22.13 or newer, no flag) while VS Code is closed.

The run needs a display and fails with a message when it has none. The first
run downloads VS Code (the version is `VSCODE_VERSION` in `lib/vscode.mjs`)
into `.vscode-test/`.

| Scenario | What it checks |
| --- | --- |
| `highlight-p`, `highlight-cls` | The language is `ABL`. Token colours equal the Default Dark Modern colours in `lib/colors.mjs`. `highlight-p` also checks that a missing binary is logged to the `oxabl` output channel and that the only message is the offer to install the CLI. |
| `markdown` | Keywords in a fenced `abl` block get colours. Prose around the block does not. |
| `outline` | A real `oxabl` (`OXABL_E2E_OXABL`, with a short `OXABL_E2E_RUNTIME` directory) in `outline.p` and `OutlineDemo.cls`. The Outline view lists the symbols in the right tree (fields under the temp-table, parameter under the function, members under the class) and shows a new procedure after an edit. "Go to Symbol in File" lists the symbols, and choosing one puts the cursor on its name. The breadcrumbs show the class and the method at the cursor. "Go to Symbol in Workspace" lists `ArchiveOrders` from `archive.p`, a file that is not open, and choosing it opens that file with the cursor on the name. It fails without the binary. |
| `include-context` | A real `oxabl` (as `outline`) in a project with `shared.i`, included by `reads.p` and `ignores.p`. The open include shows `oxabl: fragment` in the status bar. **oxabl: Select Include Context** lists fragment mode and both roots. Choosing `ignores.p` shows `oxabl: in ignores.p` and the unused `spare-v` in the Problems panel. Choosing `reads.p` removes it. A root file shows no include context, the selection stays with the include, and fragment mode can be chosen again. |
| `expanded-unit` | A real `oxabl` (as `outline`) with `nested.p`, which includes `mid.i`, which includes `shared.i`. **oxabl: Show Expanded Compilation Unit** opens `nested (expanded).p` beside the file with both includes folded. Unfold All shows the text between `▼` and `▲` lines for each include. Typing does not change the view. The unused `spare-v` of the same analysis is in the Problems panel. F12 on an expanded line opens `shared.i`. An unsaved edit of `nested.p` that reads `spare-v` removes the finding and leaves the disk unchanged. `missing.p` gives a reduced expansion that names `nope.i`. |
| `inactive-regions` | A real `oxabl` (as `outline`) with `dim.p`, `shared.i`, `cond.p` and `red.p`, and `opsys = "UNIX"`. In `dim.p` only the `WIN32` branch is dimmed: the taken branch and the branch of a `SEARCH` condition are not. `oxabl.inactiveRegions.enable: false` removes the dimming and `true` puts it back, with no restart. `shared.i` alone dims only its `OPSYS = "VMS"` branch, because it does not know `flavour`. In the include context `red.p`, its `&ELSE` branch is dimmed too, and in fragment mode again only the `VMS` branch. In `cond.p`, a false condition on three lines dims its branch, and typing `NOT ` on the middle line removes the dimming. |
| `definition` | A real `oxabl` (as `outline`) with `main.p` and `util/recalc.p`. The `RUN util/recalc.p` target shows as a document link. F12 on it opens `recalc.p`. F12 on a use of a local variable puts the cursor on its `DEFINE`. It fails without a binary that answers both requests. |
| `schema` | A real `oxabl` (as `outline`) with `oxabl.toml` naming `db/shop.df`, the directory `nodf` (no `.df` in it), `../outside/extra.df` and the empty directory `../later`. The ABL Schema view lists both databases and their tables, and the message above it names `nodf`. **oxabl: Filter Schema** with `mobile` keeps only the field whose description holds it, and the view message names the filter. A click on the field opens `shop.df` with the cursor on its `ADD FIELD` line, at the name. **Copy table.field** in its context menu puts `Customer.Phone` on the clipboard. The inline action on a table opens its details page. A filter with no match says so, and a table added to the `.df` on disk then shows in the view; so does a table added to `extra.df`, outside the workspace folder, and a `.df` created in `../later` (named `LATE.DF`: the watcher takes any case of the extension). |
| `schema-config-above` | A real `oxabl` (as `outline`). The workspace folder is `src`, and the `oxabl.toml` that names the schema is one directory above it. The ABL Schema view shows that schema, and an edit of that `oxabl.toml` that adds a `.df` shows the new database in the view. |
| `document-highlight` | A real `oxabl` (as `outline`) with `main.p`. With the cursor on a variable, the editor shows its four writes with the write colour and its two reads with the read colour. With the cursor on a name that does not resolve, it shows no read or write mark. It fails without a binary that answers `textDocument/documentHighlight`. |
| `folding` | A real `oxabl` (as `outline`) with `main.p`, a file with no indentation. **Fold All Block Comments** folds the block comment, so the server gives the kind `comment`. **Fold All** folds the procedure and shows its `END` line, and folds both blocks of an `IF ... THEN DO: ... ELSE DO:` with the `ELSE DO:` line visible. **Fold All Regions** folds the `// #region` and `&ANALYZE-SUSPEND` blocks and not the procedure. **Expand Selection** grows from a string to its statement and its procedure, and each step contains the one before it. In an `IF ... THEN` with a `DO:` block on its own line, it reaches the block and then the `IF` statement. |
| `references` | A real `oxabl` (as `outline`) with `main.p`, `batch.p` and `util/recalc.p`. **Find All References** on a local variable gives its 5 references, and on the `RUN util/recalc.p` target lists `batch.p`, a file that is not open. **Show Call Hierarchy** on `RUN tally` shows `main.p` as its caller. On a keyword outside every routine it shows the program, called by `batch.p`, and **Show Outgoing Calls** lists `recalc.p` and `tally`. VS Code asks for no call hierarchy item where the cursor is on no word. |
| `hierarchy` | A real `oxabl` (as `outline`) with `Shape.cls` and two classes that inherit it and override `Describe`, of which `Square.cls` is open. **oxabl: Go to Super Method** on the override opens `Shape.cls` at the name of the method. **Go to Implementations** on the method in `Shape.cls` lists `Square.cls` and `Circle.cls`, a file that is not open. **Peek Type Hierarchy** on the class shows it and its two subtypes. |
| `reference-lenses` | A real `oxabl` (as `outline`) with `main.p` (two `RUN tally`, the procedure `tally` and the procedure `bump`), `batch.p` (runs `main.p`), `viahandle.p` (runs `bump` through a handle) and `util/recalc.p`. No code lens shows until `oxabl.referenceLenses.enable` is `true`. Then `main.p` shows `1 reference · used by 1 file` above the program and `2 references` above `tally` and `0+ references · incomplete: calls through a handle are not followed` above `bump`. A click on each opens the references view, and its count and its rows are the places on the lens: two `RUN tally.` rows for the procedure, one `RUN main.p.` row for the program. Setting it to `false` removes the lenses, with no restart. It fails without a binary that answers `codeLens/resolve`. |
| `inlay-hints` | A real `oxabl` (as `outline`) in a project whose `oxabl.toml` enables `strict.locks` and loads a schema. The line of a `FIND` that names no lock shows the inlay hints `SHARE-LOCK` and `index custnum`. Setting `oxabl.inlayHints.locks` to `false` removes the lock hint and keeps the index hint, with no restart. After the lock hint is on again, a rewrite of `oxabl.toml` without the strict rule removes the lock hint and keeps the index hint. |
| `snippets` | No `oxabl` binary. In a project with no style and in a project with `keyword_case = "Lowercase"` in `[workspace.style]`, typing `trycat` and choosing the snippet from the suggest widget inserts the `DO TRANSACTION` block with upper case and lower case keywords. A change of the first project's `oxabl.toml` on disk gives the new case at once, although the extension keeps what it read. |
| `completion` | A real `oxabl` (as `outline`) in a project with a schema and `keyword_case = "Lowercase"`. Typing `fi` on a new line and asking for suggestions shows a `find` row from the server. Typing `trycat` shows exactly one `trycatch` row: the server serves the snippets, and the extension's own snippet provider gives nothing while the server completes. Choosing it inserts the `do transaction` block in lower case. |
| `semantic-tokens` | A real `oxabl` (as `outline`) with `main.p` and a schema, and colours for the token types `variable`, `databaseField` and `table` in `editor.semanticTokenColorCustomizations`. A variable (declaration and use), a database field and the table of a `FOR EACH` have the colour of their type. A name that does not resolve and a keyword keep the colour of the grammar. `oxabl.semanticTokens.enable: false` removes the colours and `true` brings them back, with no restart. It fails without a binary that answers `textDocument/semanticTokens/full`. |
| `update-auto` | The extension installs a newer release from the fake API and sends the right headers. |
| `update-checksum` | A wrong SHA-256 installs nothing and shows no message. |
| `update-notify` | The `notify` mode shows a message and installs nothing. |
| `update-skip` | In `notify` mode, `Skip This Version` stores the version. After the throttle stamp is removed from `state.vscdb`, the next launch checks again and does not offer that version. `oxabl: Check for Updates` offers it. |
| `update-none`, `update-error` | No newer version and an HTTP 500 give no request for a release and no message. |
| `update-autoupdate-off` | Mode `auto` with `extensions.autoUpdate: false` shows the "is available" message and does not download the VSIX. |
| `update-throttle` | A second launch of the same profile makes no tag listing. |
| `update-installed-reload` | After an automatic install, `oxabl: Check for Updates` says the version is installed and asks for a reload. The VSIX is downloaded once. |
| `update-two-profiles` | Two VS Code profiles (`--profile`) share one extensions folder. After the default profile updates to the newer version, the other profile still lists the old one. Its own automatic check installs the new version and shows "updated to v99.0.0", not silence. |
| `update-manual-no-tags` | An empty tag list gives "no extension releases found" and not "up to date". |

The build scenarios run the extension against a fake `oxabl` (`lib/fake-oxabl.mjs`: a
Node script that answers `--version`, `build --help`, `build --print-config --json`
and `lsp`, plays the event stream that the scenario scripts for `build --events`,
and logs each call and each signal to `FAKE_OXABL_LOG`). The fake is the binary
only; the extension is the program under test.

| Scenario | What it checks |
| --- | --- |
| `build-open` | The build at open: the status bar shows `preparing`, then `done/total · N% cached`, then the check mark and hides. One notification names the units and the cache share. The build runs with `--events --yield --wait-lock 3600` in the root, after `--print-config --json`. A save builds again and shows no notification. |
| `build-save-keeps-its-build` | The watcher event that follows a save neither cancels the build of that save nor delays it. |
| `build-burst-save` | Ten saves while a build runs: one `SIGINT` to that build, one new build that completes. |
| `build-burst-disk` | Two hundred files written on disk while a build runs: the same. Files in the output and state directories start no build. |
| `build-beside-source` | A root whose configuration has `build.rcode = "beside-source"` and no `build.output` builds at open. Two hundred `.r` files written into the source directories (and `.R`, and below `pkg/`) start no build; a `.p` written beside them starts one. |
| `build-config-refused` | `--print-config` exits 2 with the error of a binary that does not know `build.rcode`: an error notification gives that reason, and no build runs. |
| `build-diagnostic` | A compiler message shows in the Problems panel. A failed first build gives an error notification, and `oxabl: 1 failed` stays in the status bar. |
| `build-untrusted` | An untrusted workspace runs no `oxabl build`, not even `--print-config`. Granting trust starts the build. |
| `build-external-preemption` | A build that exits 130 without a signal from the extension: the status bar says `running elsewhere` and one more build starts. |
| `build-unkeyed` | A report with units without a compile key gives one warning with the count, the reason and how to finish the setup; a later build with the same reason gives none. |
| `build-too-old` | A binary without `--yield` is not used and not updated when it is not managed. |
| `build-too-old-managed` | A managed binary without `--yield` and without `self-update` is replaced by the newest release, then builds. |
| `build-binary-changed` | A binary that reports a new version restarts the language server once. |
| `build-install` | With no binary, the install prompt shows. `Install` puts the release asset into `OXABL_INSTALL_DIR`, writes the install record (`installer: "vscode"`), leaves no staged file, and the build runs with it. |
| `install-no-project` | No `oxabl.toml` and no binary: the install prompt shows. `Install` starts the language server with the installed CLI. No message is about `oxabl.toml` or a build, and no `oxabl build` runs, not even `--print-config`. |
| `quiet-no-project` | A binary and no `oxabl.toml`: the language server starts, no oxabl message shows, and no `oxabl build` runs. |
| `build-install-bad-checksum` | A wrong checksum installs nothing and writes no record. |
| `cli-update-auto` | In `auto` mode the update check asks a managed binary (`self-update --check --json`), runs `self-update`, says "oxabl CLI updated" in the status bar and restarts the language server. |
| `cli-update-notify` | In `notify` mode the prompt for the CLI has Update, Release Notes and Skip This Version, installs nothing before the answer, and `Update` installs. |
| `cli-update-unmanaged` | A binary that the install record does not name is never asked to update; the output channel says why. |
| `cli-update-check-fails` | A check that fails installs nothing, shows no error on an automatic check, and logs the reason. |
| `cli-update-manual-current` | In `off` mode nothing is asked by itself; `oxabl: Check for Updates` says the CLI is up to date. |

The highlight and markdown scenarios run with `oxabl.updates` set to `off` and
assert that the fake API received no request. Every launch needs
`OXABL_UPDATE_API_URL` on a loopback fake server (`launch` refuses anything
else), so no scenario reaches `api.github.com`.

Every VS Code starts with `OXABL_INSTALL_DIR` and `XDG_DATA_HOME` inside its profile, so
the default install path and the install record are never those of the machine. It also
starts with a `PATH` that holds no `oxabl` executable and with
`VSCODE_CLI=1`, so it does not read the login shell's environment. The run
behaves like a machine with no language server. After each scenario the run
fails when a process with the scenario's profile directory in its command line
is still running, and kills it.

Two more scenarios measure what the extension costs the machine. They run only
when `OXABL_E2E_SCENARIOS` names them, and their settings are in
`scenarios/server.mjs`.

| Scenario | What it measures |
| --- | --- |
| `server-crash-loop` | A server that exits at once: how often the client starts it again. |
| `server-soak` | A real `oxabl` (`OXABL_E2E_OXABL`) on a large fuzzed file. Text is first typed at key-repeat speed (about 39 characters a second): the checks fail when the main thread of the window blocks for more than 1 second, when it is blocked for more than 10% of a sample, or when the window takes fewer than 28 characters a second. Then the scenario types as fast as it can and checks that the window still answers within 30 seconds. Then five window reloads. It reports memory, threads and CPU of the extension host and the server, the number of `oxabl` processes, and how long the window was blocked. Input at that stress speed (about 78 characters a second; the limit is about 45) blocks the window for seconds in every language with a grammar: see "Input faster than VS Code tokenizes" in `docs/design/vscode-extension.md`. |

For example, the soak with the server off, the same text in VS Code's own C
grammar for comparison, and a CPU profile of the window while it types:

```bash
OXABL_E2E_SCENARIOS=server-soak OXABL_E2E_NO_SERVER=1 OXABL_E2E_OXABL=<oxabl> \
  OXABL_E2E_RUNTIME=<short dir> xvfb-run -a pnpm run test:e2e
OXABL_E2E_SCENARIOS=server-soak OXABL_E2E_NO_SERVER=1 OXABL_E2E_OXABL=<oxabl> \
  OXABL_E2E_RUNTIME=<short dir> OXABL_E2E_BIG_FILE=big.c xvfb-run -a pnpm run test:e2e
OXABL_E2E_SCENARIOS=server-soak OXABL_E2E_NO_SERVER=1 OXABL_E2E_OXABL=<oxabl> \
  OXABL_E2E_RUNTIME=<short dir> OXABL_E2E_PROFILE=window.cpuprofile xvfb-run -a pnpm run test:e2e
```

To open the file of `server-soak` by hand, write it from `clients/vscode`:

```bash
node -e 'import("./test/e2e/scenarios/server.mjs").then(m => require("fs").writeFileSync("big.p", m.fuzzedFile(400000)))'
```

Settings through the environment:

- `OXABL_E2E_SCENARIOS=highlight,markdown` runs the scenarios whose name starts
  with one of the prefixes.
- `OXABL_E2E_VSCODE_VERSION=1.147.0` runs another VS Code than the pinned one.
- `OXABL_E2E_VSIX=<file>` tests that VSIX. Without it, the run packages the
  tree with `scripts/package-vsix.sh`.

Output goes to git-ignored folders: `screenshots/` (one PNG per scenario) and
`.work/` (the VSIX files and one profile per scenario). The run prints the
screenshot folder at the end.

To change a colour constant, read the theme files of the pinned VS Code under
`resources/app/extensions/theme-defaults/themes/`.
