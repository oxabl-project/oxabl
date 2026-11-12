# oxabl for VS Code

ABL support for VS Code, from [oxabl](https://github.com/oxabl-project/oxabl).
It covers Progress ABL files (`.p`, `.w`, `.cls`, `.i`, `.v`).

## What works without the oxabl binary

Install the extension and open an ABL file. These features need nothing else:

- **Syntax highlighting.** A TextMate grammar colours ABL code. It also
  colours `abl` code blocks in Markdown files.
- **Editing.** Word selection knows ABL names such as `my-var-name` and
  `&SCOPED-DEFINE`. Lines indent after `DO:` and similar blocks, and fold on
  `&ANALYZE-SUSPEND` sections and `// #region` comments.

## What needs the oxabl language server

The extension starts `oxabl lsp` when it finds the `oxabl` executable. Then
you also get:

- **Format on save.** The formatter changes ABL files only. It leaves a file
  that it cannot format safely byte for byte unchanged.
- **Live diagnostics.** Lint and semantic diagnostics show in the Problems
  panel. The rule severities come from `oxabl.toml`. An edit to `oxabl.toml`
  applies at once, with no reload.

The extension does not bundle the `oxabl` binary. It looks for it in this
order: the `oxabl.server.path` setting, `~/.local/bin/oxabl` (the directory in
`OXABL_INSTALL_DIR` when that is set), then your `PATH`. To get the binary, see
[Install the oxabl CLI](#install-the-oxabl-cli). When the binary is missing and
the folder has no `oxabl.toml`, the extension shows no message, because the
highlighting works without it. The `oxabl` output channel (**Output** panel)
says that the binary was not found. Install the binary later, then run
**oxabl: Restart Server**.

These features follow `[workspace.features]` in `oxabl.toml`. A project
turns them off there: `diagnostics = false` stops the diagnostics and the
code lenses, and `format = false` stops formatting, so format on save does
nothing. An `oxabl.toml` that oxabl cannot read turns both off until you fix
it. To use only the highlighting, turn both off in `oxabl.toml`.

`oxabl.enable: false` stops the server on one machine, also when the binary
is installed.

## Install the oxabl CLI

When a folder has an `oxabl.toml` and no `oxabl` binary is found, the
extension asks once: "The oxabl CLI is not installed. It builds and checks this
project." The buttons are **Install**, **Not Now** and **Don't Ask Again**.

**Install** downloads the newest release from GitHub, checks its SHA-256, runs
`oxabl --version` on the downloaded file, and only then replaces
`~/.local/bin/oxabl` (the directory in `OXABL_INSTALL_DIR`, when set). It writes
`~/.local/share/oxabl/install.json`, which tells `oxabl self-update` that this
binary is its own. Then it starts the language server and the build. Any
failure installs nothing and shows the reason.

The command **oxabl: Install or Update the CLI** does the same on request. For
a binary that the installer put there, it runs `oxabl self-update` instead. It
never changes a binary that it did not install. A binary that is too old for
the build (it has no `oxabl build --yield`) is updated the same way when it is
managed. Otherwise one message says that it needs a newer oxabl.

Only Linux x86_64 binaries are released.

## Windows and WSL

The language server needs Unix sockets, so it does not run on Windows. The
extension runs where the folder is: in a WSL window, it runs in WSL, so the
CLI is the Linux binary there. In a local Windows window, no binary can be
installed. When the folder has an `oxabl.toml`, the extension offers **Reopen
Folder in WSL** (it needs the WSL extension) and does not download a Windows
binary. On macOS and on other CPUs, run `cargo install oxabl`.

## Build on open and on save

With an `oxabl.toml` that has `[build].output` and `[build.workers].command`,
the extension runs `oxabl build` for that folder. The build uses the local and
the remote cache of `oxabl build`, so a folder that someone else built needs
few compiles.

- It builds once when the folder opens, and shows one notification when that
  first build ends: "oxabl build finished: 450 units, 82% from cache (370
  local, 0 remote), 80 compiled in 1m 12s". Later builds show only the status
  bar.
- It builds again when you save a file, and when a `.p`, `.w`, `.cls`, `.i`,
  `.v` or `.df` file, `oxabl.toml` or `.oxabl/cache.toml` changes on disk. So
  an agent, a `git checkout` or another tool that edits files also starts a
  build. Changes in the output and state directories of the build, and in
  `.git`, start nothing.
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
- A folder builds only when `oxabl build --print-config --json` shows both
  `build.output` and `build.workers.command`. The extension reads this again
  when `oxabl.toml` changes.
- No build runs in a folder that you did not trust (**Workspaces: Manage
  Workspace Trust**), because a build runs the worker command from
  `oxabl.toml`. The build starts when you grant trust. The settings
  `oxabl.server.path` and `oxabl.build.args` are not read from an untrusted
  folder.

The status bar item on the left shows the state:

| Item | Meaning |
| --- | --- |
| `oxabl: preparing` | The build starts. |
| `oxabl 120/450 · 82% cached` | Units done (from a cache or compiled) of the units to rebuild, and the share of the done units that a cache gave. The tooltip splits local and remote, compiled and failed. |
| `oxabl` with a check mark | The build succeeded. It hides after 3 seconds. |
| `oxabl: 2 failed` | Units failed. It stays until the next successful build. Click it to open the Problems view. |
| warning | The build could not run (exit code 2, 3 or other). The tooltip has the first line of its error output. Click it to open the build log. |
| `oxabl: build running elsewhere` | Another process took over the workspace. The extension builds again when it is done. |

The compiler messages of each unit show in the Problems view, as the source
`oxabl build`. A build replaces the messages of the units it compiles and
clears those of the units it took from a cache. A build that you cancel keeps
the messages of the units it did not reach. A configuration error (exit code
2) also shows an error message with **Show Build Log**.

The commands **oxabl: Build**, **oxabl: Cancel Build** and **oxabl: Show Build
Log** do what their names say. The build log is the output channel `oxabl
build`.

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
