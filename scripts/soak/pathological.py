#!/usr/bin/env python3
"""Feeds pathological ABL files to `oxabl check`, and to the language server, and records
wall time, peak RSS and how the process ended (exit code, or the signal that killed it).

  scripts/soak/pathological.py --binary target/release/oxabl --runtime target/fuzz-runtime \
      [--sizes 1000,10000,100000] [--lsp] [--only parens,macro-double]

A run is capped at 6 GB of address space and 60 s of CPU, so a runaway ends as a recorded
failure and not as a stuck machine. With --lsp the file is opened in a daemon session: the
row says whether the daemon survived, how long a formatting request took, and its peak RSS.
"""

import argparse
import json
import os
import resource
import signal
import subprocess
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402

CAP_AS = 6 << 30
CAP_CPU = 60


def rep(text, n):
    return text * n


# Each shape takes a size n (about n bytes) and returns the file text.
SHAPES = {
    "parens": lambda n: "x = " + rep("(", n // 2) + "1" + rep(")", n // 2) + ".\n",
    "parens-open": lambda n: "x = " + rep("(", n) + "\n",
    "brackets": lambda n: "x = a" + rep("[", n) + "\n",
    "do-nest": lambda n: rep("DO:\n", n // 4) + rep("END.\n", n // 4),
    "do-nest-open": lambda n: rep("DO:\n", n // 4),
    "if-nest": lambda n: rep("IF TRUE THEN ", n // 13) + "MESSAGE 1.\n",
    "else-if-chain": lambda n: "IF a THEN x = 1.\n" + rep("ELSE IF a THEN x = 1.\n", n // 22),
    "plus-chain": lambda n: "x = 1" + rep(" + 1", n // 4) + ".\n",
    "and-chain": lambda n: "IF a" + rep(" AND a", n // 6) + " THEN x = 1.\n",
    "not-chain": lambda n: "IF " + rep("NOT ", n // 4) + "a THEN x = 1.\n",
    "unary-minus": lambda n: "x = " + rep("-", n) + "1.\n",
    "call-nest": lambda n: "x = " + rep("f(", n // 3) + "1" + rep(")", n // 3) + ".\n",
    "string-long": lambda n: 'x = "' + rep("a", n) + '".\n',
    "string-open": lambda n: 'x = "' + rep("a", n) + "\n",
    "comment-long": lambda n: "/* " + rep("a", n) + " */\n",
    "comment-nest": lambda n: rep("/* ", n // 3) + rep("*/ ", n // 3) + "\n",
    "comment-open": lambda n: rep("/* ", n // 3) + "\n",
    "lines-tiny": lambda n: rep("x = 1.\n", n // 7),
    "lines-blank": lambda n: rep("\n", n),
    "tildes": lambda n: 'x = "' + rep("~", n) + '".\n',
    "braces-open": lambda n: rep("{", n) + "\n",
    "braces-nest": lambda n: rep("{a ", n // 3) + rep("}", n // 3) + "\n",
    "macro-ref-chain": lambda n: "&SCOPED-DEFINE a 1\n" + rep("{&a}", n // 4) + "\n",
    "macro-double": lambda n: "&SCOPED-DEFINE a0 x\n"
    + "".join(f"&SCOPED-DEFINE a{i + 1} {{&a{i}}}{{&a{i}}}\n" for i in range(min(n // 20, 60)))
    + f"{{&a{min(n // 20, 60)}}}\n",
    "macro-self": lambda n: "&SCOPED-DEFINE a {&a}\n{&a}\n",
    "macro-grow": lambda n: "&SCOPED-DEFINE a x {&a}\n{&a}\n",
    "if-nest-pp": lambda n: rep("&IF TRUE &THEN\n", n // 15) + "x = 1.\n" + rep("&ENDIF\n", n // 7),
    "if-expr-nest": lambda n: "&IF " + rep("(", n // 2) + "1" + rep(")", n // 2) + " &THEN\n&ENDIF\n",
    "define-long": lambda n: "DEFINE VARIABLE x AS INTEGER" + rep(" NO-UNDO", n // 8) + ".\n",
    "for-nest": lambda n: rep("FOR EACH a:\n", n // 11) + rep("END.\n", n // 11),
    "class-nest": lambda n: rep("CLASS c:\n", n // 9),
    "proc-nest": lambda n: rep("PROCEDURE p:\n", n // 13) + rep("END PROCEDURE.\n", n // 15),
    "on-nest": lambda n: rep('ON "F1" ANYWHERE DO:\n', n // 21),
    "case-nest": lambda n: rep("CASE a:\nWHEN 1 THEN\n", n // 20),
    "temp-table-fields": lambda n: "DEFINE TEMP-TABLE t\n" + "".join(f"FIELD f{i} AS INTEGER\n" for i in range(n // 20)) + ".\n",
    "long-line-tokens": lambda n: rep("a ", n // 2) + ".\n",
    "utf8-noise": lambda n: rep("é€", n // 5) + "\n",
    "nul-bytes": lambda n: rep("\0", n),
}

INCLUDE_LOOP = {"include-self": ("loop.i", "{loop.i}\nx = 1.\n"), "include-pair": ("a.i", "{b.i}\n")}


def run_check(binary, path, cwd):
    def limits():
        resource.setrlimit(resource.RLIMIT_AS, (CAP_AS, CAP_AS))
        resource.setrlimit(resource.RLIMIT_CPU, (CAP_CPU, CAP_CPU + 5))
        resource.setrlimit(resource.RLIMIT_CORE, (0, 0))

    start = time.time()
    # stderr goes to a file: a run that prints a lot must not block on a full pipe.
    with tempfile.TemporaryFile() as err:
        proc = subprocess.Popen(
            ["nice", "-n", "10", binary, "check", "-I", str(cwd), str(path)],
            cwd=cwd, stdout=subprocess.DEVNULL, stderr=err, preexec_fn=limits,
        )
        _, status, usage = os.wait4(proc.pid, 0)
        wall = time.time() - start
        proc.returncode = 0
        size = err.seek(0, 2)
        err.seek(max(0, size - 4000))
        stderr = err.read().decode(errors="replace").strip().splitlines()
    if os.WIFSIGNALED(status):
        ended = f"signal {signal.Signals(os.WTERMSIG(status)).name}"
    else:
        ended = f"exit {os.WEXITSTATUS(status)}"
    note = stderr[-1][:100] if stderr and ("panick" in stderr[-1] or "overflow" in stderr[-1] or "memory" in stderr[-1]) else ""
    return {"stderr_bytes": size, "wall_s": round(wall, 2), "peak_rss_mb": round(usage.ru_maxrss / 1024), "ended": ended, "note": note}


def run_lsp(binary, runtime, root, path, timeout=60):
    client = lsp.Lsp(binary, root, runtime)
    if client.initialize(timeout=30) is None:
        client.kill()
        return {"lsp": "initialize failed"}
    pid = lsp.daemon_for(runtime, root)
    text = Path(path).read_text(errors="replace")
    peak = 0
    start = time.time()
    client.notify("textDocument/didOpen", {"textDocument": {"uri": lsp.uri(path), "languageId": "abl", "version": 1, "text": text}})
    reply = client.request("textDocument/formatting", {"textDocument": {"uri": lsp.uri(path)}, "options": {"tabSize": 4, "insertSpaces": True}}, timeout=timeout)
    wall = time.time() - start
    exited = client.proc.poll()
    info = lsp.stat(pid) if pid else None
    peak = info[4] if info else 0
    alive = bool(pid and lsp.stat(pid))
    # A second round trip proves the connection still answers.
    again = client.request("textDocument/formatting", {"textDocument": {"uri": lsp.uri(path)}, "options": {"tabSize": 4, "insertSpaces": True}}, timeout=timeout) if alive else None
    client.kill()
    return {"lsp_format_s": round(wall, 2), "answered": reply is not None, "lsp_exit_code": exited, "answered_again": again is not None, "daemon_alive": alive, "daemon_rss_mb": round(peak / 1024)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    ap.add_argument("--sizes", default="1000,10000,100000")
    ap.add_argument("--only", default="")
    ap.add_argument("--lsp", action="store_true")
    ap.add_argument("--keep", default=None, help="keep the generated files in this directory")
    args = ap.parse_args()

    binary = str(Path(args.binary).resolve())
    runtime = lsp.runtime_dir(args.runtime)
    only = set(filter(None, args.only.split(",")))
    work = Path(args.keep) if args.keep else Path(tempfile.mkdtemp(prefix="patho-", dir=runtime.parent))
    work.mkdir(parents=True, exist_ok=True)
    (work / "oxabl.toml").write_text('[workspace]\nname = "patho"\n')
    for name, (file, text) in INCLUDE_LOOP.items():
        (work / file).write_text(text)
    (work / "b.i").write_text("{a.i}\n")
    try:
        cases = [(name, size, SHAPES[name](size)) for name in SHAPES for size in map(int, args.sizes.split(","))]
        cases += [("include-self", 0, "{loop.i}\n"), ("include-pair", 0, "{a.i}\n")]
        for name, size, text in cases:
            if only and name not in only:
                continue
            path = work / f"{name}-{size}.p"
            path.write_text(text, errors="surrogateescape")
            row = {"shape": name, "bytes": len(text.encode(errors="surrogateescape")), **run_check(binary, path, work)}
            if args.lsp:
                row.update(run_lsp(binary, runtime, work, path))
            print(json.dumps(row), flush=True)
            if not args.keep:
                path.unlink()
    finally:
        lsp.stop_daemons(runtime)


if __name__ == "__main__":
    main()
