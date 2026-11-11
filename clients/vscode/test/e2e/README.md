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
| `highlight-p`, `highlight-cls` | The language is `ABL`. Token colours equal the Default Dark Modern colours in `lib/colors.mjs`. `highlight-p` also checks that a missing binary shows no message and is logged to the `oxabl` output channel. |
| `markdown` | Keywords in a fenced `abl` block get colours. Prose around the block does not. |
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

The highlight and markdown scenarios run with `oxabl.updates` set to `off` and
assert that the fake API received no request. Every launch needs
`OXABL_UPDATE_API_URL` on a loopback fake server (`launch` refuses anything
else), so no scenario reaches `api.github.com`.

Every VS Code starts with a `PATH` that holds no `oxabl` executable and with
`VSCODE_CLI=1`, so it does not read the login shell's environment. The run
behaves like a machine with no language server. After each scenario the run
fails when a process with the scenario's profile directory in its command line
is still running, and kills it.

Two more scenarios measure what the extension costs the machine. They run only
when `OXABL_E2E_SCENARIOS` names them, and their settings are in
`scenarios/server.mjs`:

| Scenario | What it measures |
| --- | --- |
| `server-crash-loop` | A server that exits at once: how often the client starts it again. |
| `server-soak` | A real `oxabl` (`OXABL_E2E_OXABL`) on a large fuzzed file that is edited for a while, then five window reloads: memory, threads and CPU of the extension host and the server, and the number of `oxabl` processes. |

Settings through the environment:

- `OXABL_E2E_SCENARIOS=highlight,markdown` runs the scenarios whose name starts
  with one of the prefixes.
- `OXABL_E2E_VSIX=<file>` tests that VSIX. Without it, the run packages the
  tree with `scripts/package-vsix.sh`.

Output goes to git-ignored folders: `screenshots/` (one PNG per scenario) and
`.work/` (the VSIX files and one profile per scenario). The run prints the
screenshot folder at the end.

To change a colour constant, read the theme files of the pinned VS Code under
`resources/app/extensions/theme-defaults/themes/`.
