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

The language server runs on Linux and macOS only. The daemon needs Unix
sockets.

The extension does not bundle the `oxabl` binary. Put it on your `PATH`, or
set `oxabl.server.path`. When the binary is missing, the extension shows one
warning for each window. Choose **Don't Show Again** to stop it for good.
Install the binary later, then run **oxabl: Restart Server**.

## Install

The extension is not on the Marketplace. Install it from a GitHub release:

1. Open the newest release whose tag starts with `oxabl_vscode-v` on the
   [releases page](https://github.com/oxabl-project/oxabl/releases).
2. Download `oxabl-vscode-<version>.vsix`.
3. Run `code --install-extension oxabl-vscode-<version>.vsix`. Or open the
   Extensions view, choose `…` and **Install from VSIX…**.

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
- It never installs an older version.
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
| `oxabl.server.path` | `""` | Absolute path to `oxabl`. Empty means use `PATH`. |
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
