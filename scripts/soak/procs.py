#!/usr/bin/env python3
"""Counts the processes, threads and descriptors that `oxabl lsp` and `oxabl daemon` leave.

  scripts/soak/procs.py --binary target/release/oxabl --runtime target/fuzz-runtime <scenario> [--n N]

Scenarios:
  same-root   N lsp processes start at once on one root: one daemon is expected.
  roots       N roots, one lsp each: N daemons are expected; reports the idle cost of each.
  kill-daemon N rounds of: start an lsp, SIGKILL its daemon, start another lsp.
  restart     N times: start an lsp, open a document, shut it down (the editor restart loop).
  kill-lsp    N times: start an lsp, open a document, SIGKILL the lsp (the editor crashed).

Every process runs under the XDG_RUNTIME_DIR given by --runtime. Every pid it signals
is one it started, or one the registry of that directory names. All daemons of the
directory are stopped at the end; the final line lists what is left.
"""

import argparse
import concurrent.futures
import json
import os
import shutil
import signal
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402


def make_root(repo, base, name="p"):
    root = Path(tempfile.mkdtemp(prefix=name, dir=base)).resolve()
    fixtures = repo / "crates/oxabl_lsp/tests/fixtures/effect_lenses"
    for file in ("schema.df", "routines.i", "main.p"):
        shutil.copy(fixtures / file, root / file)
    (root / "oxabl.toml").write_text('[workspace]\nname = "soak"\n')
    return root


def open_main(client, root):
    text = (root / "main.p").read_text()
    client.notify(
        "textDocument/didOpen",
        {"textDocument": {"uri": lsp.uri(root / "main.p"), "languageId": "abl", "version": 1, "text": text}},
    )


def census(runtime):
    daemons, lsps, zombies, others = lsp.oxabl_processes(runtime)
    cpu, threads, rss = lsp.total(daemons)
    files = len(list((Path(runtime) / "oxabl" / "daemon").glob("*"))) if (Path(runtime) / "oxabl").exists() else 0
    fds = sum(lsp.fd_count(pid) or 0 for pid in daemons)
    return {
        "daemons": len(daemons),
        "lsps": len(lsps),
        "zombies": len(zombies),
        "others": len(others),
        "daemon_threads": threads,
        "daemon_fds": fds,
        "daemon_rss_kb": rss,
        "registry_files": files,
    }


def same_root(args, ctx):
    root = make_root(ctx["repo"], ctx["base"])
    clients = []

    def start(_):
        client = lsp.Lsp(args.binary, root, ctx["runtime"])
        return client, client.initialize(timeout=30) is not None

    with concurrent.futures.ThreadPoolExecutor(args.n) as pool:
        started = list(pool.map(start, range(args.n)))
    clients = [client for client, _ in started]
    print(json.dumps({"step": "all started", "initialized": sum(ok for _, ok in started), **census(ctx["runtime"])}))
    for client in clients:
        open_main(client, root)
    time.sleep(2)
    print(json.dumps({"step": "documents open", **census(ctx["runtime"])}))
    for client in clients:
        client.shutdown()
    time.sleep(1)
    print(json.dumps({"step": "all lsp shut down", **census(ctx["runtime"])}))


def roots(args, ctx):
    clients = []
    for i in range(args.n):
        root = make_root(ctx["repo"], ctx["base"])
        client = lsp.Lsp(args.binary, root, ctx["runtime"])
        if client.initialize(timeout=30) is None:
            print(json.dumps({"step": f"root {i} did not initialize"}))
        open_main(client, root)
        clients.append(client)
    time.sleep(3)
    print(json.dumps({"step": f"{args.n} roots open", **census(ctx["runtime"])}))
    for client in clients:
        client.shutdown()
    time.sleep(2)
    census_after = census(ctx["runtime"])
    print(json.dumps({"step": "all lsp shut down, daemons remain", **census_after}))
    daemons = lsp.oxabl_processes(ctx["runtime"])[0]
    before = {pid: lsp.stat(pid) for pid in daemons}
    window = 30
    time.sleep(window)
    after = {pid: lsp.stat(pid) for pid in daemons}
    cpu = sum(after[p][2] - before[p][2] for p in daemons if before[p] and after[p])
    rss = [after[p][4] for p in daemons if after[p]]
    print(json.dumps({
        "step": f"idle for {window}s",
        "idle_cpu_percent_all_daemons": round(100 * cpu / window, 3),
        "rss_kb_min_mean_max": [min(rss), round(sum(rss) / len(rss)), max(rss)] if rss else None,
        "threads_each": sorted({after[p][3] for p in daemons if after[p]}),
        **census(ctx["runtime"]),
    }))


def kill_daemon(args, ctx):
    root = make_root(ctx["repo"], ctx["base"])
    old = []
    for i in range(args.n):
        client = lsp.Lsp(args.binary, root, ctx["runtime"])
        if client.initialize(timeout=30) is None:
            print(json.dumps({"round": i, "error": "initialize timed out", **census(ctx["runtime"])}))
            client.kill()
            continue
        open_main(client, root)
        pid = lsp.daemon_for(ctx["runtime"], root)
        if pid:
            os.kill(pid, signal.SIGKILL)
        time.sleep(0.5)
        # The lsp is still alive when it spawned the daemon and nothing waits for it.
        exited = client.proc.poll() is not None
        row = {"round": i, "lsp_exited_after_daemon_kill": exited, **census(ctx["runtime"])}
        print(json.dumps(row))
        if exited:
            client.proc.wait()
        else:
            old.append(client)
        if i % 10 == 9:
            for client in old:
                client.kill()
            old.clear()
    for client in old:
        client.kill()
    time.sleep(0.5)
    print(json.dumps({"step": "end", **census(ctx["runtime"])}))


def restart(args, ctx, kill=False):
    root = make_root(ctx["repo"], ctx["base"])
    first = lsp.Lsp(args.binary, root, ctx["runtime"])
    first.initialize(timeout=30)
    open_main(first, root)
    time.sleep(0.5)
    daemon = lsp.daemon_for(ctx["runtime"], root)
    print(json.dumps({"step": "daemon up", "daemon_pid": daemon, **census(ctx["runtime"])}))
    first.shutdown()
    for i in range(args.n):
        client = lsp.Lsp(args.binary, root, ctx["runtime"])
        ok = client.initialize(timeout=30) is not None
        open_main(client, root)
        client.request("textDocument/formatting", {
            "textDocument": {"uri": lsp.uri(root / "main.p")}, "options": {"tabSize": 4, "insertSpaces": True}})
        if kill:
            client.kill()
        else:
            client.shutdown()
        if i % max(1, args.n // 10) == 9 or i == args.n - 1:
            time.sleep(0.3)
            info = lsp.stat(daemon)
            print(json.dumps({
                "round": i + 1, "initialized": ok, "daemon_pid": lsp.daemon_for(ctx["runtime"], root),
                "daemon_rss_kb": info[4] if info else None, "daemon_threads": info[3] if info else None,
                "daemon_fds": lsp.fd_count(daemon), **{k: v for k, v in census(ctx["runtime"]).items() if k in ("daemons", "lsps", "zombies")},
            }))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    ap.add_argument("scenario", choices=["same-root", "roots", "kill-daemon", "restart", "kill-lsp"])
    ap.add_argument("--n", type=int, default=20)
    args = ap.parse_args()

    runtime = lsp.runtime_dir(args.runtime)
    base = Path(tempfile.mkdtemp(prefix="procs-", dir=runtime.parent)).resolve()
    ctx = {"repo": Path(__file__).resolve().parents[2], "runtime": runtime, "base": base}
    try:
        {
            "same-root": same_root,
            "roots": roots,
            "kill-daemon": kill_daemon,
            "restart": restart,
            "kill-lsp": lambda a, c: restart(a, c, kill=True),
        }[args.scenario](args, ctx)
    finally:
        lsp.stop_daemons(runtime)
        time.sleep(0.5)
        print(json.dumps({"step": "cleanup", **census(runtime)}))
        shutil.rmtree(base, ignore_errors=True)


if __name__ == "__main__":
    main()
