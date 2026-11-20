# Soak and process scripts for `oxabl lsp` and `oxabl daemon`

Python 3, standard library only. Every process they start runs under an
`XDG_RUNTIME_DIR` of your choice (`--runtime`), so its daemons register there and
never meet the daemons of a running editor. They signal only pids they started
or that the registry of that directory names, and stop every daemon of the
directory at the end. Run them with `python3 -I` and under `nice`.

| Script | What it does |
| --- | --- |
| `session.py` | One long editor-like session: random incremental edits, whole-document replacements, formatting and code lens requests, close and reopen, `oxabl.toml` edits. Samples RSS, threads, descriptors and CPU of the lsp and the daemon; prints the growth per 1000 edits and the idle cost. A server that dies ends the run and leaves its documents in `<out>.crash/`. |
| `procs.py` | Process counts: `same-root` (N lsp at once), `roots` (N roots, then the idle cost of the daemons), `kill-daemon`, `restart` and `kill-lsp` loops. |
| `pathological.py` | Pathological files (deep nesting, exponential macros, long chains) through `oxabl check`, and with `--lsp` through a daemon: wall time, peak RSS, how the process ended. Runs are capped at 6 GB of address space and 60 s of CPU. |
| `depth.py` | The nesting depth at which `oxabl check` and the daemon abort on a stack overflow. |
| `replay.py`, `minimize.py` | Open a directory of documents in one session and say whether the daemon survives; shrink a file that kills it. |
| `lsp.py` | The scripted client and the `/proc` probes the others share. |

```bash
cargo build --release -p oxabl
python3 -I scripts/soak/session.py --binary target/release/oxabl --runtime target/rt --edits 30000 --rate 100
python3 -I scripts/soak/procs.py --binary target/release/oxabl --runtime target/rt roots --n 20
```

RSS counts memory the allocator keeps, so a growth of a few hundred KB per 1000
edits can be noise. To see what grows, take a heap profile: build with the
system allocator (remove `#[global_allocator]` in `crates/oxabl/src/main.rs`,
locally), start `valgrind --tool=massif --vgdb=yes oxabl daemon <root>` with the
same `XDG_RUNTIME_DIR` before the client starts (a client uses the daemon its
root already has), and compare snapshots taken with
`vgdb --pid=<pid> detailed_snapshot <file>`.

The extension host side is `clients/vscode/test/e2e/scenarios/server.mjs`.
