#!/usr/bin/env python3
"""Finds the nesting depth at which `oxabl check` and the daemon abort on a stack overflow.

  scripts/soak/depth.py --binary target/release/oxabl --runtime target/fuzz-runtime

The daemon is probed by opening the file in an `oxabl lsp` session and asking for
diagnostics through a formatting request; a dead daemon ends the lsp session.
"""

import argparse
import resource
import subprocess
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402

SHAPES = {
    "parens": lambda n: "x = " + "(" * n + "1" + ")" * n + ".\n",
    "do-nest": lambda n: "DO:\n" * n + "END.\n" * n,
    "if-nest": lambda n: "IF TRUE THEN " * n + "MESSAGE 1.\n",
    "call-nest": lambda n: "x = " + "f(" * n + "1" + ")" * n + ".\n",
    "else-if-chain": lambda n: "IF a THEN x = 1.\n" + "ELSE IF a THEN x = 1.\n" * n,
    "unary-minus": lambda n: "x = " + "-" * n + "1.\n",
}


def search(dies, lo=10, hi=200000):
    if not dies(hi):
        return None
    while hi - lo > lo // 20 + 1:
        mid = (lo + hi) // 2
        if dies(mid):
            hi = mid
        else:
            lo = mid
    return hi


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    args = ap.parse_args()
    binary = str(Path(args.binary).resolve())
    runtime = lsp.runtime_dir(args.runtime)
    root = Path(tempfile.mkdtemp(prefix="depth-", dir=runtime.parent)).resolve()
    (root / "oxabl.toml").write_text('[workspace]\nname = "depth"\n')
    path = root / "t.p"
    try:
        for name, make in SHAPES.items():
            def check_dies(n):
                path.write_text(make(n))
                run = subprocess.run([binary, "check", str(path)], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                                     preexec_fn=lambda: resource.setrlimit(resource.RLIMIT_CORE, (0, 0)))
                return run.returncode < 0

            def daemon_dies(n):
                text = make(n)
                client = lsp.Lsp(binary, root, runtime)
                client.initialize(timeout=30)
                client.notify("textDocument/didOpen", {"textDocument": {"uri": lsp.uri(path), "languageId": "abl", "version": 1, "text": text}})
                client.request("textDocument/formatting", {"textDocument": {"uri": lsp.uri(path)}, "options": {"tabSize": 4, "insertSpaces": True}}, timeout=20)
                dead = lsp.daemon_for(runtime, root) is None
                client.kill()
                lsp.stop_daemons(runtime)
                return dead

            print(f"{name}: check aborts from {search(check_dies)} levels; daemon aborts from {search(daemon_dies)} levels", flush=True)
    finally:
        lsp.stop_daemons(runtime)


if __name__ == "__main__":
    main()
