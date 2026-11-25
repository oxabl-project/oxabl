# oxabl for VS Code

ABL support for VS Code, from [oxabl](https://github.com/oxabl-project/oxabl).
It covers Progress ABL files (`.p`, `.w`, `.cls`, `.i`, `.v`).

## Getting started

1. Install the extension (see [Install](#install)). It updates itself.
2. Open the folder that holds your `oxabl.toml`: the root of the ABL sources.
   Without one, you get highlighting only. The
   [build wrapper guide](https://github.com/oxabl-project/oxabl/blob/master/docs/build-wrapper.md)
   explains the settings.
3. Click **Install** when the extension offers the oxabl CLI. It updates
   itself too.
4. Trust the folder. A build runs the worker command of `oxabl.toml`, so
   nothing builds in a folder you did not trust.
5. Wait for the first build. With an empty cache it compiles every unit, which
   can take minutes. After that, a save compiles only what it changed.
6. Nothing to add to `.gitignore` for the default output: the CLI writes a
   `.gitignore` into `.oxabl/`. Add an output directory that is not below
   `.oxabl/` (for example `build`) to your ignore file yourself. An older CLI
   also needs `.oxabl/` there.

## Install

The extension is not on the Marketplace. Install it from a GitHub release:

1. Open the newest release whose tag starts with `oxabl_vscode-v` on the
   [releases page](https://github.com/oxabl-project/oxabl/releases).
2. Download `oxabl-vscode-<version>.vsix`.
3. Run `code --install-extension oxabl-vscode-<version>.vsix`. Or open the
   Extensions view, choose `…` and **Install from VSIX…**.

In a WSL window, the extension runs in WSL, so install it there: run the
`code` command in the WSL terminal, or use **Install from VSIX…** while the
WSL window is open. See [Windows and WSL](#windows-and-wsl).

After this, the extension updates itself.

## What works without the oxabl binary

Install the extension and open an ABL file. These features need nothing else:

- **Syntax highlighting.** A TextMate grammar colours ABL code. It also
  colours `abl` code blocks in Markdown files.
- **Editing.** Word selection knows ABL names such as `my-var-name` and
  `&SCOPED-DEFINE`. Lines indent after `DO:` and similar blocks, and fold on
  `&ANALYZE-SUSPEND` sections and `// #region` comments.
- **Snippets.** Type a prefix and choose the snippet from the suggestions:
  `find` (`FIND FIRST` and an `IF AVAILABLE` test), `foreach`, `defvar`,
  `deftt`, `proc`, `function`, `method`, `class` and `trycatch`. Tab moves
  between the placeholders. The snippets write keywords in the case of
  `keyword_case` in `[workspace.style]` of the nearest `oxabl.toml`. Upper case
  is the default, also for `Preserve`. The placeholders hold sample names, not
  names from your schema. When the language server runs, it serves the
  snippets in its own completion list (below).

## What needs the oxabl language server

The extension starts `oxabl lsp` when it finds the `oxabl` executable. Then
you also get, with no setup:

- **Code completion.** The suggestions at a statement start hold the
  statements that are valid in the scope at the cursor (no `METHOD` in a
  procedure file, no `PROCEDURE` in a method), the words after `DEFINE`, and
  the snippets where their body is valid. The names in scope come too: the
  variables, parameters, buffers and functions that are defined before the
  cursor, the members of the class, and the built-in functions, with the type
  that fits an assignment first. Keywords are written in the case of
  `keyword_case`; with `Preserve` or no setting, in the case of the word you
  type, else of the nearest keyword before it. Where oxabl does not know what
  the cursor needs (in a comment or a string, after a statement end), the
  server gives no list, and VS Code shows the words of the open files. See
  [completion.md](https://github.com/oxabl-project/oxabl/blob/master/docs/design/completion.md).
- **Document outline.** The Outline view, the breadcrumbs, "Go to Symbol in
  File" (`Ctrl+Shift+O`) and sticky scroll show the declarations of the file:
  classes and their members, procedures, functions, temp-tables with their
  fields, variables, buffers and `ON` triggers. The outline reads the file as
  written, so declarations in an include file do not show.
- **Syntax folding and selection.** Fold blocks, routines, definitions, long
  statements, comments, preprocessor branches, and the `&ANALYZE-SUSPEND` and
  `// #region` markers. **Fold All Block Comments** and **Fold All Regions**
  work. Use **Expand Selection** (`Shift+Alt+Right`) to grow from a token to
  its expression, statement, block, routine, and file. Both features read the
  file as written, so text from an include file does not fold. Wrong
  indentation does not change their ranges.
- **Go to Symbol in Workspace** (`Ctrl+T`). Finds classes, interfaces,
  procedures, functions, shared variables and files in every ABL file of the
  workspace, also files that are not open, and opens the declaration. An open
  file counts with its unsaved text. The first search reads the whole
  workspace, so it can take a moment in a large one. A search shows at most 256
  symbols, so type more of the name to narrow it. A declaration inside an
  include file does not show.
- **Go to Definition** (`F12`). Goes from a name to its declaration: a
  variable, parameter, buffer, temp-table and its fields, internal procedure,
  function or method, also when it is declared in an include file. `RUN x.p`
  and `RUN x` open the program, `{x.i}` opens the include, and a class name
  (`NEW`, `USING`, `INHERITS`, `IMPLEMENTS`, `AS CLASS`) opens its `.cls`. An
  inherited method goes to the class that declares it, each overload. A table
  and `table.field` go to the `.df`. A name that two files on the include path
  supply shows both. A name that does not resolve, or that is computed at run
  time (`RUN VALUE(...)`), goes nowhere.
- **Find All References** (`Shift+Alt+F12`, or `Shift+F12` to peek). Lists
  each use of the name at the cursor, with the same resolution as Go to
  Definition. A local name is searched in its file and in the includes the
  file uses. A program, an internal procedure, a function, a class or a
  method is also searched in the other files that can name it, also files
  that are not open: the files that `RUN` the program, and the files that name
  the class, a database field in the files that read its table, and a name
  declared in an include in each file that includes it. When the list can be
  short because of this name, a warning says why: for example a call through a
  handle that oxabl cannot follow, a shared variable, no include path, or a
  workspace index that is not ready. A reason that holds for the whole
  workspace and leaves out only a part of the search (a file that oxabl could
  not analyse) is written once to the `oxabl language server` output instead.
- **Call hierarchy** (`Shift+Alt+H`). Shows the callers of a procedure,
  function, method or program, and the `RUN`, function and method calls it
  makes. On a name that is no routine, or on a keyword, it shows the routine
  that holds the cursor, or else the program. A call whose target is known only
  at run time (`RUN VALUE(...)`, `DYNAMIC-FUNCTION`, `PUBLISH`, a handle oxabl
  cannot follow) shows as a call with no icon and the detail
  `unresolved: ...`. When the list can be short, its first entry is
  `incomplete`, with the reason.
- **Go to Implementation** (`Ctrl+F12`). On an interface, a class, an abstract
  method or a method, lists the classes that implement or extend it, through
  any number of levels and in files that are not open. A method counts only
  with the same parameters: an overload is another method. When the list can be
  short, a warning says why: for example a class whose parent oxabl cannot
  resolve, a class file it cannot read, no include path, or a workspace index
  that is not ready.
- **Go to Super Method** (**oxabl: Go to Super Method** in the Command Palette
  and the editor menu). On a method, goes to the method it overrides or
  implements in the nearest supertype, or lists several in a peek view. An
  empty answer with a reason (a parent that oxabl cannot resolve, a method of
  `Progress.Lang.Object`) says that oxabl does not know, not that nothing is
  overridden. A method that overrides a class method and implements an
  interface method goes to both. On a name that is no method that oxabl reads,
  the command says so.
- **Type hierarchy** (**Peek Type Hierarchy** in the Command Palette and the
  editor menu). Shows the supertypes and the subtypes of a class, interface or
  enum, one level at a time. A supertype that oxabl cannot
  resolve is an item with no icon and the detail `unresolved: ...`, and a
  built-in type shows by name with no file. When the list can be short, its
  first entry is `incomplete`, with the reason.
- **Hover.** Point at a name to see what it is: a variable, parameter or
  property with its type, `EXTENT`, `INITIAL` and `NO-UNDO`; the signature of
  an internal procedure, function or method (each overload of an inherited
  method); a class header; a table or field from the schema with its format,
  label and the indexes it takes part in; a temp-table with its fields and
  indexes, including those it takes with `LIKE` (and a note when the source is
  not in the loaded schema). A built-in function, a statement
  keyword and a system handle show their name and a link to the Progress
  documentation. A name declared in an include file works. A name that does
  not resolve, or that two files supply, shows nothing. A procedure, function,
  method or `RUN` target also shows the tables it reads, creates, updates and
  deletes, with the lock of each read, directly and through the routines it
  calls. When a call cannot be followed (`RUN VALUE(...)`, a program that no
  file supplies, `RUN ... ON SERVER`), or when another body may run (a method
  that a subclass can override, an interface method, `PROCEDURE p IN SUPER`),
  the hover says that the list is incomplete. While a file it reads has unsaved
  changes, the hover shows no effects; save the file to see them.
- **Highlight other uses.** With the cursor on a name, the editor marks the
  other uses of it in the file. A use that changes the value (an assignment, an
  `OUTPUT` argument, the declaration) has the write colour, and every other use
  has the read colour. A buffer, a field, a procedure, a function, a method and a
  class name work as well as a variable. It uses the same resolution as **Go to
  Definition**, so a name that does not resolve has no mark of oxabl (VS Code
  can still mark the same word as text). In an include file, the marks are the
  uses in that file. A name that a macro or an include argument writes is not
  marked.
- **Document links.** Include references, `RUN` targets and `USING` names are
  links to the file each names (`Ctrl+Click`). A name that resolves to no file,
  or to more than one, is not a link.

  Local names need no setup. Programs, includes and classes are found through
  `include_paths` under `[workspace.sources]` in `oxabl.toml`, and tables and
  fields through `[workspace.schema]`. Text in an `&IF` branch that is not
  compiled has no definition, no hover and no link.

A folder with an `oxabl.toml` is a project. A project also gets:

- **Format on save.** The formatter changes ABL files only. It leaves a file
  that it cannot format safely byte for byte unchanged.
- **Live diagnostics.** Lint and semantic diagnostics show in the Problems
  panel. The rule severities come from `oxabl.toml`. An edit to `oxabl.toml`
  applies at once, with no reload. A finding of the `strict.handles` and
  `strict.nulls` rules lists the statements on its path under it, such as
  the `FIND ... NO-ERROR` that may leave a buffer empty or the create of a
  handle that leaks. Click one to go to it, also in an include file.
- **Include context.** An include file (`.i`) has no meaning alone: a
  variable it declares is used or unused only in a file that includes it. An
  open include starts in fragment mode, and the status bar says
  `oxabl: fragment`. Click it, or run **oxabl: Select Include Context**, to
  pick a root file that includes it, directly or through another include.
  Then the diagnostics of the include are the findings of that root with the
  include expanded in it, unsaved edits included, and the status bar says
  `oxabl: in <root>`. The list needs include paths under `[workspace.sources]`
  in `oxabl.toml`, and says so when there are none. The choice lasts until you
  close the file or the language server restarts.
- **Inactive branches.** The `&IF`, `&ELSEIF` and `&ELSE` branches that the
  preprocessor does not compile are dimmed. The colours of the text stay. The
  compile environment comes from `[workspace.preprocessor]` in `oxabl.toml`
  (for example `opsys` and `proversion`). In an include, the selected include
  context decides. In fragment mode, a condition on a name that the includer
  can set is not decided. A branch whose condition oxabl cannot decide is never
  dimmed: a function that it does not model (such as `SEARCH`), `PROVERSION`
  with no `proversion` set, a name that an include it cannot read can define,
  or `OPSYS`, `WINDOW-SYSTEM`, `BATCH-MODE` or `PROCESS-ARCHITECTURE` when
  `oxabl.toml` does not set `opsys`, `window-system`, `batch-mode` or
  `process-architecture`, or `DBTYPE` of a database that `database-types` does
  not name. Their defaults do not dim anything. An include gets no dimming when
  a root that includes it has an include that oxabl cannot find. While you
  type plain text, the dimming stays; an edit of a directive line, a removal, a
  paste with `&`, `{`, a comment or a quote, or text inside a multi-line `&IF`
  condition or a definition that continues after `~` removes it until oxabl
  has read the new text. Set `oxabl.inactiveRegions.enable` to `false` to turn the
  dimming off.
- **Expanded compilation unit.** **oxabl: Show Expanded Compilation Unit**
  opens a read-only view beside the file: the source oxabl parsed after
  preprocessing. For an include it uses the selected include context, or the
  include alone in fragment mode. Each include sits between a `▼` line and a
  `▲` line and starts folded; unfold each one on its own. The `▼` line links to
  the include file and to each `{...}` that brought it in. **Go to Definition**
  (`F12`) on any line opens the line that wrote it. The findings of the same
  analysis show in the view and point to the file that holds them. Unsaved
  edits of the file and of its includes, and a change of `oxabl.toml`, refresh
  the view. The top of the view says when the expansion is reduced (an include
  that was not found, a cyclic include, the depth limit) or failed.

- **Semantic highlighting.** The server colours a name by what it is: a
  variable, a parameter, a property, a procedure or function, a method, a
  class, an interface, an event, a database table, a database field, a
  temp-table, a temp-table field, a buffer or a dataset. A token is given only
  for a name that oxabl resolves to one thing. Text that it does not resolve (a
  name it cannot find, a keyword, a string, a comment) keeps the colour of the
  grammar, and so do the whole file and every name when no server runs or the
  theme has semantic highlighting off. A declaration has the modifier
  `declaration`, and a `STATIC` member of a class has `static`. The types that
  VS Code does not know (`table`, `tempTable`, `buffer`, `dataset`,
  `databaseField`, `tempTableField`) style like `type`, `variable` and
  `property` until a theme styles them. To change a colour, add a rule to
  `editor.semanticTokenColorCustomizations`, for example
  `{"rules": {"databaseField": "#4ec9b0", "tempTableField:declaration": {"bold": true}}}`.
  Set `oxabl.semanticTokens.enable` to `false` to turn the server's tokens off,
  or `editor.semanticHighlighting.enabled` to `false` for every extension.
  Preprocessor names (`{&name}`) are not tokens yet. See
  `docs/design/semantic-tokens.md` in the repository.
- **Database inlay hints.** Short labels in the code show the database facts
  that ABL does not show. `index <name>` after a search (with `WHOLE-INDEX` when
  no condition brackets the index) shows in every project, with no strict rule.
  A project whose `oxabl.toml` enables a rule of `strict.locks`,
  `strict.transactions` or `strict.access` (for example
  `enable = ["strict.locks"]` under `[workspace.lint]`) also gets `SHARE-LOCK`
  after a read that names no lock, and `transaction starts` / `transaction ends`
  and `<buffer> scope starts` / `<buffer> scope ends` around a transaction or a
  buffer scope that the compiler makes wider than the code shows. Hover a hint
  to see why and which rule reports it (the index hint has no rule). Each of
  these kinds shows only where
  its rule is on: lock hints need `default-lock` (LK001), scope hints
  `scope-widened` (TX005), transaction hints `implicit-transaction` (TX001) or
  `procedure-wide-transaction` (TX002). In an include, the hints come from the
  selected include context; without one, a hint that depends on the including
  file is left out, and the others say that an includer can change them. A
  fact that oxabl does not know gives no hint, for example in a file with an
  include that was not found (for the index hint, a read after that include),
  or a table that the schema does not have. Turn
  each kind off with the `oxabl.inlayHints.*` settings, or all hints with VS
  Code's `editor.inlayHints.enabled`. The hints need the schema under
  `[workspace.schema]`. The index hint costs one analysis of the file for each
  version of its text, also in a project with no strict rule.
- **ABL Schema view.** The Explorer has an **ABL Schema** view of the `.df`
  files that `[workspace.schema]` in `oxabl.toml` names, with no database
  connection: each file is a database, with its tables, their fields and
  indexes, and its sequences. The filter button (or **oxabl: Filter Schema**)
  keeps the tables and fields whose name, label or description holds the text,
  and a table also by its dump name; it ignores case. Click a field, index or
  sequence to open the `.df` at its `ADD` line. The context menu has **Open in
  .df**, **Copy Name** and **Copy table.field**. The icon on a table opens a
  page with its fields and indexes. The view follows changes of the `.df`
  files and of `oxabl.toml`, also outside the workspace folder. With no
  schema configured, the view says why. A warning above the tree names a
  schema directory without a `.df`, and a database with problems (an
  `UPDATE`, `DROP` or `RENAME` that oxabl does not apply, a field of an
  unknown table) shows them at its top.

Without an `oxabl.toml`, the language server does not format and does not
report diagnostics, and it shows no message about it. Add an `oxabl.toml` to
turn them on.

The extension does not bundle the `oxabl` binary. It looks for it in this
order: the `oxabl.server.path` setting, `~/.local/bin/oxabl` (the directory in
`OXABL_INSTALL_DIR` when that is set), then your `PATH`. To get the binary, see
[Install the oxabl CLI](#install-the-oxabl-cli). The `oxabl` output channel
(**Output** panel) says when the binary was not found. Install the binary later,
then run **oxabl: Restart Server**.

In a project, these features follow `[workspace.features]` in `oxabl.toml`:
`diagnostics = false` stops the diagnostics, the dimming of inactive branches, the code lenses and the inlay hints, and `format = false` stops formatting, so format on save does
nothing. An `oxabl.toml` that oxabl cannot read turns both off until you fix
it.

`oxabl.enable: false` stops the server on one machine, also when the binary
is installed.

## Install the oxabl CLI

When no `oxabl` binary is found, the extension asks when it starts, that is
when you open an ABL file: "The oxabl CLI is not installed. Install it for the
outline, breadcrumbs and Go to Symbol in ABL files." The buttons are
**Install**, **Not Now** and **Don't Ask Again**. The prompt shows with or
without an `oxabl.toml`.

**Install** downloads the newest release from GitHub, checks its SHA-256, runs
`oxabl --version` on the downloaded file, and only then replaces
`~/.local/bin/oxabl` (the directory in `OXABL_INSTALL_DIR`, when set). It writes
`~/.local/share/oxabl/install.json`, which tells `oxabl self-update` that this
binary is its own. Then it starts the language server, and in a project the
build. Any
failure installs nothing and shows the reason.

The command **oxabl: Install or Update the CLI** does the same on request. For
a binary that the installer put there, it runs `oxabl self-update` instead. It
never changes a binary that it did not install. A binary that is too old for
the build (it has no `oxabl build --yield`) is updated the same way when it is
managed. Otherwise one message says that it needs a newer oxabl.

Only Linux x86_64 binaries are released.

## Coding agents

The command **oxabl: Install agent skill** runs `oxabl skill install` with the oxabl binary that the extension uses. It installs the skill for your user, not for a project: one copy in `${XDG_DATA_HOME:-~/.local/share}/oxabl/skills/oxabl/`, linked from `~/.agents/skills/oxabl` and `~/.claude/skills/oxabl`, so coding agents know how to run `oxabl check`, `format`, `build` and the other commands in every project. A message shows the path and state of the store and of each link, or the error. The extension never installs the skill by itself and asks no question. An entry that oxabl did not install is not replaced; run `oxabl skill install --force` in a terminal to move it to `~/.local/share/oxabl/skill-backups/` (outside the skills folders) and install. The command needs an oxabl with `oxabl skill` (6.1.0 or newer).

## Windows and WSL

The language server needs Unix sockets, so it does not run on Windows. The
extension runs where the folder is: in a WSL window, it runs in WSL, so the
CLI is the Linux binary there. In a local Windows window, no binary can be
installed. When you open an ABL file there, the extension says "oxabl does
not run on Windows. Open this file in a window connected to WSL for ABL
language features." It offers **Reopen Folder in WSL** (with the WSL extension; else
**Open Docs**) and **Don't Ask Again**, and does not download a Windows
binary. Highlighting still works in the Windows window. On macOS and on other
CPUs, run `cargo install oxabl`.

## Build on open and on save

With an `oxabl.toml` that has `[build.workers].command` and `[build].output`
(or `[build].rcode = "beside-source"`),
the extension runs `oxabl build` for that folder. The build uses the local and
the remote cache of `oxabl build`, so a folder that someone else built needs
few compiles.

- It builds once when the folder opens, and shows one notification when that
  first build ends: "oxabl build finished: 450 units, 82% from cache (370
  local, 0 remote), 80 compiled in 1m 12s". It tells this once for each
  project; a later window and later builds show only the status bar. A first
  build that fails shows its error in every window.
- It builds again when you save a file, and when a `.p`, `.w`, `.cls`, `.i`,
  `.v` or `.df` file, `oxabl.toml` or `.oxabl/cache.toml` changes on disk. So
  an agent, a `git checkout` or another tool that edits files also starts a
  build. Changes in the output and state directories of the build, in `.git`,
  and of `.r` files start nothing.
- A burst of changes gives one build. The build waits for `oxabl.build.debounce`
  milliseconds of quiet (at most 5 seconds from the first change; file events
  from disk wait a little longer, because VS Code hands them over in batches).
  At most one build runs for a folder. A change while a build runs stops that
  build once (`SIGINT`, the same as Ctrl+C) and starts one new build after the
  burst. The newest change always wins.
- The extension starts `oxabl build` with `--yield`. If you start `oxabl
  build` in a terminal while the extension builds, your build stops the
  extension's build and the extension builds again after yours. A build that
  you start without `--yield` is never stopped.
- A folder builds only when `oxabl build --print-config --json` shows
  `build.workers.command` and a place for the r-code: `build.output`, or
  `build.rcode = "beside-source"`. The extension reads this again when
  `oxabl.toml` changes. When the CLI cannot read `oxabl.toml` (for example, a
  CLI that is older than a key in it), an error notification gives the reason
  and the folder does not build. Update the extension before the CLI.
- With `build.rcode = "beside-source"`, each build writes a `.r` file beside
  each source. A `.r` file starts no build. VS Code itself still watches these
  files and shows them in the Explorer. To stop that, add `**/*.r` to the
  settings `files.watcherExclude` and `files.exclude`:

  ```json
  "files.watcherExclude": { "**/*.r": true },
  "files.exclude": { "**/*.r": true }
  ```

  The `.gitignore` of the project must ignore `*.r`. oxabl does not write a
  `.gitignore` into the source directories.
- No build runs in a folder that you did not trust (**Workspaces: Manage
  Workspace Trust**), because a build runs the worker command from
  `oxabl.toml`. The build starts when you grant trust. The settings
  `oxabl.server.path` and `oxabl.build.args` are not read from an untrusted
  folder.

The status bar item on the left shows the state. While a build runs, a
spinner turns next to it:

| Item | Meaning |
| --- | --- |
| `oxabl: preparing` | The build starts. |
| `oxabl 120/450 · 97% cached` | Units done (from a cache or compiled) of the units this build rebuilds, and the share of the workspace that needs no compile: units up to date plus units a cache gave, rounded down, so 100% means nothing compiles. The tooltip says how many units this build rebuilds and how many of the workspace were up to date before it. For the first build of a large project (no unit up to date, 500 units or more) it also says that the build can take 10 minutes or more and that later builds compile only what changed. It does not change while units finish, so it stays readable. |
| `oxabl` with a check mark | The build succeeded. It hides after 3 seconds. |
| `oxabl: 2 failed` | Units failed. It stays until the next successful build. Click it to open the Problems view. |
| warning | The build could not run (exit code 2, 3 or other). The tooltip has the first line of its error output. Click it to open the build log. |
| `oxabl: build running elsewhere` | Another process took over the workspace. The extension builds again when it is done. |

The compiler messages of each unit show in the Problems view, as the source
`oxabl build`. A build replaces the messages of the units it compiles and
clears those of the units it took from a cache. A build that you cancel keeps
the messages of the units it did not reach. A configuration error (exit code
2) also shows an error message with **Show Build Log**.

A unit without a compile key compiles at every build, and no cache holds
it. The report of each build (`--report`) says which units have none. When
the reason names a setting that is missing, such as `[workspace.sources].dlc`,
or when a tenth of the workspace has no key, one warning names the count and
the reason. It shows once for each project and reason, not at every start of
VS Code: the setup of the project is not complete. A few units
of encrypted source show only in the tooltip and the build log. The variables
of a login profile reach the build only through VS Code: after you change
`OXABL_DLC_SOURCES`, start VS Code again from a session that has it.

A build starts one worker for each CPU, at most 8. To use every core, set
`count` in `oxabl.toml`, or pass `--workers` for your machine only with
`"oxabl.build.args": ["--workers", "16"]`:

```toml
[build.workers]
count = 16
```

Each worker is one OpenEdge batch client with its own database connections,
so the databases must accept that many more connections.

The commands **oxabl: Build**, **oxabl: Cancel Build** and **oxabl: Show Build
Log** do what their names say. The build log is the output channel `oxabl
build`.

## Updates

VS Code does not update an extension that you installed from a VSIX. So this
extension reads GitHub Releases itself. The `oxabl.updates` setting controls
what it does:

| Value | Behaviour |
| --- | --- |
| `auto` (default) | Checks, then downloads, verifies and installs a newer version. A message asks you to reload the window. |
| `notify` | Checks, then asks before it installs. You can update, read the release notes, or skip that version. |
| `off` | Never checks by itself. |

When VS Code's `extensions.autoUpdate` setting is `false`, `auto` acts as
`notify`.

How the check works:

- It asks `api.github.com` at most once a day, while VS Code runs. It checks
  when the extension starts and again every 6 hours, and the 24-hour limit
  applies to both. It never delays the editor. A check does nothing when
  `oxabl.updates` is `off` at that moment.
- It looks only at releases of the extension (tags `oxabl_vscode-v<major>.<minor>.<patch>`).
  It ignores drafts and pre-releases.
- It checks the SHA-256 of the VSIX against the checksum file of the release.
  On a mismatch it installs nothing.
- Each download goes to its own temporary folder, which is removed after the
  install. Two windows that check at the same time do not share a file.
- After an install, the extension keeps a note for the current VS Code
  profile. While the note is valid and the installed folder still exists, it
  reports the version that waits for a reload and downloads nothing. Another
  profile does not see the note, so it gets the update too: VS Code adds the
  version that is already on disk to that profile.
- Any failure installs nothing. An automatic check stays silent and writes the
  reason to the **oxabl** output channel.
- It never installs a version older than the one that runs.
- It talks to GitHub with Node's `http` and `https` modules, which VS Code's
  proxy support covers.

To check by hand, run **oxabl: Check for Updates** from the Command Palette.
It always checks, in every mode, and tells you the result. A manual check also
offers a version that you skipped, and it says so when it cannot find a
complete release (no extension release, or the newest one is not finished
yet). It says "up to date" only when the newest release is not newer than the
version that runs. When the newest version is already in the extensions folder,
it asks you to reload the window.

### The CLI

The same check also updates the `oxabl` binary, but only one that the
installer or this extension installed (the install record names it). It asks
the binary (`oxabl self-update --check --json`) whether a newer release
exists. With `auto` it runs `oxabl self-update`, shows "oxabl CLI updated to
vX.Y.Z" in the status bar and restarts the language server on the new binary.
With `notify` it asks first, with the same three buttons. A manual check also
says when the CLI is up to date. A binary from somewhere else (`cargo install`,
a path in `oxabl.server.path` that the installer did not write) is never
touched; the output channel says so. `OXABL_NO_SELF_UPDATE` stops the
automatic CLI check too. Outside VS Code, a managed binary still checks by
itself once a day when you run it.

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `oxabl.enable` | `true` | Enable the language server. A change applies at once. |
| `oxabl.server.path` | `""` | Absolute path to `oxabl`. Empty means `~/.local/bin/oxabl`, then `PATH`. |
| `oxabl.build.enable` | `true` | Build when the folder opens and when files change. The commands work in either case. A change applies at once. |
| `oxabl.build.debounce` | `500` | Milliseconds of quiet before a build starts. |
| `oxabl.build.args` | `[]` | Arguments added to `oxabl build`, for example `["--config-overlay", "/path/overlay.toml"]`. They also go to the call that reads the build setup. |
| `oxabl.updates` | `auto` | `auto`, `notify` or `off`. See [Updates](#updates). |
| `oxabl.trace.server` | `off` | Trace the JSON-RPC traffic with the server, for debugging. |
| `oxabl.semanticTokens.enable` | `true` | Colour names by what they are, from the language server. The grammar colours everything else. A change applies at once. |
| `oxabl.inlayHints.locks` | `true` | Show the lock a read takes when it names none. A change applies at once. |
| `oxabl.inlayHints.index` | `true` | Show the index oxabl expects a search to use. Needs no strict rule. |
| `oxabl.inlayHints.scope` | `true` | Show where a raised buffer scope starts and ends. |
| `oxabl.inlayHints.transactions` | `true` | Show where an implicit transaction starts and ends. |

Lint and style rules are not VS Code settings. They live in `oxabl.toml`.

## Other ABL extensions

Another ABL extension can contribute its own grammar for the `abl` language.
Then two grammars compete for the same files, and VS Code picks one. If the
colours look wrong, disable the other extension.

## Companion extension

[Even Better TOML](https://marketplace.visualstudio.com/items?itemName=tamasfe.even-better-toml)
is optional. With it, `oxabl.toml` gets completion and validation from the
schema that this extension ships.

## Building from source

From the repository root:

```bash
bash clients/vscode/scripts/build-vsix.sh --install
```

This script regenerates the `oxabl.toml` schema, bundles the extension with
esbuild, packages a `.vsix` with `vsce`, and installs it into VS Code.
