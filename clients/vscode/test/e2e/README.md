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
| `build-diagnostic` | A compiler message shows in the Problems panel. A failed first build gives an error notification, and `oxabl: 1 failed` stays in the status bar. |
| `build-untrusted` | An untrusted workspace runs no `oxabl build`, not even `--print-config`. Granting trust starts the build. |
| `build-external-preemption` | A build that exits 130 without a signal from the extension: the status bar says `running elsewhere` and one more build starts. |
| `build-too-old` | A binary without `--yield` is not used and not updated when it is not managed. |
| `build-binary-changed` | A binary that reports a new version restarts the language server once. |
| `build-install` | With no binary, the install prompt shows. `Install` puts the release asset into `OXABL_INSTALL_DIR`, writes the install record (`installer: "vscode"`), leaves no staged file, and the build runs with it. |
| `build-install-bad-checksum` | A wrong checksum installs nothing and writes no record. |

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
