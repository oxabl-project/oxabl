# oxabl for VS Code

A thin VS Code client for the [oxabl](https://github.com/oxabl-project/oxabl)
language server. It launches `oxabl lsp` and surfaces two capabilities to the
editor for Progress ABL files (`.p`, `.w`, `.cls`, `.i`, `.v`):

- **Format on save** — whole-document formatting via `oxabl format`'s engine,
  scoped to ABL files only. Faithful and no-movement: a file it cannot format
  safely is left byte-for-byte unchanged.
- **Live diagnostics** — lint and semantic diagnostics in the Problems panel,
  with per-rule severity driven from your project's `oxabl.toml`.

Editing a rule in `oxabl.toml` updates diagnostics live — no reload needed.

## Requirements

The extension does **not** bundle the `oxabl` binary. Install it and make sure
it is on your `PATH`, or point the extension at it with `oxabl.server.path`.

### Recommended companion extensions

These are optional — the extension works without them, with a plainer
experience:

- **An OpenEdge ABL syntax-highlighting extension.** oxabl reuses the `abl`
  language id and ships no grammar of its own, so syntax highlighting comes
  from an installed ABL extension.
- **[Even Better TOML](https://marketplace.visualstudio.com/items?itemName=tamasfe.even-better-toml).**
  Enables autocomplete and validation inside `oxabl.toml` from the schema this
  extension ships — every style and lint rule becomes discoverable and checked
  as you type.

## Settings

| Setting | Default | Description |
| --- | --- | --- |
| `oxabl.enable` | `true` | Enable the language server (reload to apply). |
| `oxabl.server.path` | `""` | Absolute path to `oxabl`. Empty = use `PATH`. |
| `oxabl.trace.server` | `off` | Trace JSON-RPC traffic for debugging. |

Lint and style rules are **not** mirrored as VS Code settings — they live in
`oxabl.toml`, which is the single source of truth (and gets schema-backed
autocomplete via Even Better TOML).

## Building / installing from source

This extension is distributed as a sideloadable VSIX (not yet on the
Marketplace). From the repository root:

```bash
bash clients/vscode/scripts/build-vsix.sh --install
```

That regenerates the `oxabl.toml` schema, bundles the extension with esbuild,
packages a `.vsix` with `vsce`, and (with `--install`) installs it into VS Code.
