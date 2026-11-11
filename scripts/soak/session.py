#!/usr/bin/env python3
"""One long editor-like session against `oxabl lsp`, with the memory, threads and CPU
of the lsp and the daemon sampled throughout.

  scripts/soak/session.py --binary target/release/oxabl --runtime target/fuzz-runtime \
      --edits 5000 [--seed 1] [--out target/soak-out/session]

It opens a few real ABL files in a temporary project, then runs random operations:
incremental edits (real tokens, fuzzed text, deletions), whole-document replacements,
formatting and code lens requests, close and reopen, and `oxabl.toml` edits that change
the lint settings and `[workspace.features]`. It prints RSS and thread checkpoints and
the growth per thousand edits.
"""

import argparse
import json
import random
import shutil
import sys
import tempfile
import threading
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402

TOKENS = (
    "DEFINE VARIABLE x AS INTEGER NO-UNDO. FOR EACH customer NO-LOCK: END. IF x > 1 THEN DO: END. "
    "PROCEDURE p: END PROCEDURE. /* c */ {&a} {inc.i} &IF DEFINED(a) &THEN &ENDIF "
    'MESSAGE "s" VIEW-AS ALERT-BOX. FIND FIRST customer NO-LOCK NO-ERROR. RUN a.p. CLASS c: END CLASS. '
    "METHOD PUBLIC VOID m(): END METHOD. DEFINE TEMP-TABLE tt FIELD f AS CHARACTER. ~ \" ' ( ) { } /* */ // ."
).split(" ")
NOISE = ["/*", "*/", '"', "'", "~", "{", "}", "{&", "&IF", "(", ")", ":", ".", "\n", " ", "\t", "é", "\u00ff", "-"]

TOML_VARIANTS = [
    '[workspace]\nname = "soak"\n',
    '[workspace]\nname = "soak"\n[workspace.lint]\nenable = ["strict.access"]\n',
    '[workspace]\nname = "soak"\n[workspace.features]\ndiagnostics = false\n',
    '[workspace]\nname = "soak"\n[workspace.features]\nformat = false\n',
    '[workspace]\nname = "soak"\n[workspace.sources]\ninclude_paths = ["."]\n'
    '[workspace.schema]\nfiles = ["schema.df"]\n[workspace.lint]\nenable = ["strict.access"]\n',
    "this is not valid toml {{{",
    '[workspace]\nname = "soak"\n[workspace.lint]\ndisable = ["LINT0002"]\n',
]


def corpus(repo):
    found = []
    for base in ("resources", "crates"):
        for path in sorted((repo / base).rglob("*")):
            if path.suffix.lower() in (".p", ".w", ".cls", ".i", ".v", ".abl") and "target" not in path.parts:
                size = path.stat().st_size
                if 200 <= size <= 40000:
                    found.append(path)
    return found


def position(text, offset):
    head = text[:offset]
    line = head.count("\n")
    return {"line": line, "character": len(head) - (head.rfind("\n") + 1)}


class Doc:
    def __init__(self, path, text):
        self.path = path
        self.text = text
        self.version = 1
        self.open = False


class Sampler(threading.Thread):
    def __init__(self, interval, pids, state):
        super().__init__(daemon=True)
        self.interval = interval
        self.pids = pids
        self.state = state
        self.rows = []
        self.stop = threading.Event()

    def run(self):
        start = time.time()
        while not self.stop.wait(self.interval):
            row = {"t": round(time.time() - start, 1), "edits": self.state["edits"]}
            for name, pid in self.pids().items():
                info = lsp.stat(pid) if pid else None
                if info:
                    row[name] = {"cpu": info[2], "threads": info[3], "rss": info[4], "fds": lsp.fd_count(pid)}
            self.rows.append(row)


def slope(points):
    """Least-squares slope of (x, y) points."""
    n = len(points)
    if n < 3:
        return 0.0
    mx = sum(x for x, _ in points) / n
    my = sum(y for _, y in points) / n
    den = sum((x - mx) ** 2 for x, _ in points)
    return sum((x - mx) * (y - my) for x, y in points) / den if den else 0.0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    ap.add_argument("--edits", type=int, default=5000)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("--rate", type=float, default=50.0, help="operations per second; 0 sends as fast as the pipe takes")
    ap.add_argument("--docs", type=int, default=6)
    ap.add_argument("--interval", type=float, default=2.0)
    ap.add_argument("--idle", type=float, default=30.0, help="seconds of idle sampling at the end")
    ap.add_argument("--out", default=None)
    ap.add_argument("--stderr", default=None, help="file that receives the stderr of the lsp and of the daemon it starts")
    ap.add_argument("--no-lens", action="store_true")
    args = ap.parse_args()

    rng = random.Random(args.seed)
    repo = Path(__file__).resolve().parents[2]
    runtime = lsp.runtime_dir(args.runtime)
    files = corpus(repo)
    project = Path(tempfile.mkdtemp(prefix="p", dir=runtime.parent)).resolve()
    fixtures = repo / "crates/oxabl_lsp/tests/fixtures/effect_lenses"
    for name in ("schema.df", "routines.i", "main.p"):
        shutil.copy(fixtures / name, project / name)
    toml = project / "oxabl.toml"
    toml.write_text(TOML_VARIANTS[4])

    docs = []
    for i, source in enumerate(rng.sample(files, args.docs - 1)):
        text = source.read_text(encoding="latin1")
        docs.append(Doc(project / f"d{i}{source.suffix or '.p'}", text))
    docs.append(Doc(project / "main.p", (project / "main.p").read_text()))
    for doc in docs:
        doc.path.write_text(doc.text, encoding="utf-8")
        doc.text = doc.path.read_text(encoding="utf-8")

    err = open(args.stderr, "ab") if args.stderr else None
    client = lsp.Lsp(args.binary, project, runtime, lens=not args.no_lens, stderr=err)
    reply = client.initialize()
    if reply is None:
        print("initialize timed out", file=sys.stderr)
        return 2

    def pids():
        return {"lsp": client.proc.pid, "daemon": lsp.daemon_for(runtime, project)}

    state = {"edits": 0}
    sampler = Sampler(args.interval, pids, state)
    sampler.start()

    def open_doc(doc):
        doc.version += 1
        client.notify(
            "textDocument/didOpen",
            {"textDocument": {"uri": lsp.uri(doc.path), "languageId": "abl", "version": doc.version, "text": doc.text}},
        )
        doc.open = True

    def close_doc(doc):
        client.notify("textDocument/didClose", {"textDocument": {"uri": lsp.uri(doc.path)}})
        doc.open = False

    def fuzz_text():
        kind = rng.randrange(4)
        if kind == 0:
            return " ".join(rng.choice(TOKENS) for _ in range(rng.randrange(1, 12)))
        if kind == 1:
            return "".join(rng.choice(NOISE) for _ in range(rng.randrange(1, 60)))
        if kind == 2:
            return rng.choice(NOISE) * rng.randrange(1, 400)
        return ""

    def edit(doc):
        n = len(doc.text)
        a = rng.randrange(n + 1)
        b = min(n, a + rng.choice([0, 0, 1, 5, 40, 300]))
        new = fuzz_text()
        if len(doc.text) > 120_000 and not new:
            b = min(n, a + 20_000)
        if len(doc.text) > 120_000:
            new = ""
            b = min(n, a + 5_000)
        change = {"range": {"start": position(doc.text, a), "end": position(doc.text, b)}, "text": new}
        doc.text = doc.text[:a] + new + doc.text[b:]
        doc.version += 1
        client.notify(
            "textDocument/didChange",
            {"textDocument": {"uri": lsp.uri(doc.path), "version": doc.version}, "contentChanges": [change]},
        )

    def replace(doc):
        if rng.random() < 0.5:
            doc.text = rng.choice(files).read_text(encoding="latin1")
        else:
            doc.text = "\n".join(fuzz_text() for _ in range(rng.randrange(1, 40)))
        doc.version += 1
        client.notify(
            "textDocument/didChange",
            {"textDocument": {"uri": lsp.uri(doc.path), "version": doc.version}, "contentChanges": [{"text": doc.text}]},
        )

    latencies = {"format": [], "lens": []}
    unanswered = {"format": 0, "lens": 0}

    def timed(kind, method, params):
        start = time.time()
        reply = client.request(method, params, timeout=30)
        if reply is None:
            unanswered[kind] += 1
        else:
            latencies[kind].append(time.time() - start)

    for doc in docs:
        open_doc(doc)

    t0 = time.time()
    ops = 0
    while state["edits"] < args.edits:
        ops += 1
        if args.rate:
            ahead = t0 + ops / args.rate - time.time()
            if ahead > 0:
                time.sleep(ahead)
        if client.proc.poll() is not None:
            print(f"lsp exited with {client.proc.returncode} after {state['edits']} edits", file=sys.stderr)
            if args.out:
                dump = Path(args.out).with_suffix(".crash")
                dump.mkdir(parents=True, exist_ok=True)
                for doc in docs:
                    (dump / doc.path.name).write_text(doc.text, encoding="utf-8")
            break
        doc = rng.choice(docs)
        roll = rng.random()
        if not doc.open:
            open_doc(doc)
        elif roll < 0.70:
            edit(doc)
            state["edits"] += 1
        elif roll < 0.80:
            replace(doc)
            state["edits"] += 1
        elif roll < 0.88:
            timed("format", "textDocument/formatting",
                  {"textDocument": {"uri": lsp.uri(doc.path)}, "options": {"tabSize": 4, "insertSpaces": True}})
        elif roll < 0.94:
            timed("lens", "textDocument/codeLens", {"textDocument": {"uri": lsp.uri(doc.path)}})
        elif roll < 0.98:
            close_doc(doc)
            if rng.random() < 0.8:
                open_doc(doc)
        else:
            variant = rng.choice(TOML_VARIANTS)
            toml.write_text(variant)
            client.notify("workspace/didChangeWatchedFiles", {"changes": [{"uri": lsp.uri(toml), "type": 2}]})
    elapsed = time.time() - t0

    # Settle, then idle: what does the process cost when nothing happens?
    time.sleep(3)
    idle_start = {name: lsp.stat(pid) for name, pid in pids().items() if pid}
    time.sleep(args.idle)
    idle_end = {name: lsp.stat(pid) for name, pid in pids().items() if pid}
    sampler.stop.set()
    sampler.join()

    summary = {"stopped_early": state["edits"] < args.edits, "seed": args.seed, "edits": state["edits"], "seconds": round(elapsed, 1), "docs": args.docs,
               "notifications": client.notifications, "server_requests": client.server_requests,
               "format_ms_p50_p95_max": None, "lens_ms_p50_p95_max": None, "unanswered": unanswered}
    for kind, values in latencies.items():
        if values:
            values.sort()
            summary[f"{kind}_ms_p50_p95_max"] = [round(1000 * values[int(q * (len(values) - 1))], 1) for q in (0.5, 0.95, 1.0)]
    for name in ("lsp", "daemon"):
        series = [(row["edits"], row[name]["rss"]) for row in sampler.rows if name in row]
        if not series:
            continue
        half = series[len(series) // 2 :]
        summary[name] = {
            "rss_kb_start_peak_end": [series[0][1], max(y for _, y in series), series[-1][1]],
            "rss_kb_per_1000_edits_whole": round(1000 * slope(series), 1),
            "rss_kb_per_1000_edits_second_half": round(1000 * slope(half), 1),
            "threads_max": max(row[name]["threads"] for row in sampler.rows if name in row),
            "fds_max": max(row[name]["fds"] or 0 for row in sampler.rows if name in row),
            "cpu_seconds_total": round(max(row[name]["cpu"] for row in sampler.rows if name in row), 1),
            "last_seen_at_edit": max(row["edits"] for row in sampler.rows if name in row),
        }
        if name in idle_start and name in idle_end and idle_start[name] and idle_end[name]:
            summary[name]["idle_cpu_percent"] = round(100 * (idle_end[name][2] - idle_start[name][2]) / args.idle, 2)
            summary[name]["idle_rss_kb"] = idle_end[name][4]
    summary["lsp_alive_at_end"] = client.proc.poll() is None
    summary["daemon_alive_at_end"] = lsp.daemon_for(runtime, project) is not None
    print(json.dumps(summary, indent=2))
    if args.out:
        out = Path(args.out)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.with_suffix(".json").write_text(json.dumps({"summary": summary, "samples": sampler.rows}))

    client.shutdown()
    shutil.rmtree(project, ignore_errors=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
