#!/usr/bin/env python3
"""Opens every file of a directory in one `oxabl lsp` session and says whether the daemon survives.

  scripts/soak/replay.py --binary target/release/oxabl --runtime target/fuzz-runtime-replay DIR [--toml FILE]

Use it on the `.crash` directory a session run leaves, to see whether the open documents
alone kill the daemon. With --each it opens one file per session.
"""

import argparse
import shutil
import sys
import tempfile
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402


def attempt(binary, runtime, files, toml, fixtures):
    root = Path(tempfile.mkdtemp(prefix="replay-", dir=runtime.parent)).resolve()
    for name in ("schema.df", "routines.i"):
        shutil.copy(fixtures / name, root / name)
    (root / "oxabl.toml").write_text(toml)
    client = lsp.Lsp(binary, root, runtime)
    client.initialize(timeout=30)
    for f in files:
        dest = root / f.name
        shutil.copy(f, dest)
        client.notify("textDocument/didOpen", {"textDocument": {"uri": lsp.uri(dest), "languageId": "abl", "version": 1, "text": f.read_text(errors="replace")}})
        reply = client.request("textDocument/formatting", {"textDocument": {"uri": lsp.uri(dest)}, "options": {"tabSize": 4, "insertSpaces": True}}, timeout=20)
        if client.proc.poll() is not None:
            return f"lsp exited with {client.proc.returncode} while handling {f.name}"
    time.sleep(2)
    died = client.proc.poll() is not None
    client.kill()
    lsp.stop_daemons(runtime)
    return "lsp exited" if died else "survived"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    ap.add_argument("--toml", default=None)
    ap.add_argument("--each", action="store_true")
    ap.add_argument("dir")
    args = ap.parse_args()
    binary = Path(args.binary).resolve()
    runtime = lsp.runtime_dir(args.runtime)
    fixtures = Path(__file__).resolve().parents[2] / "crates/oxabl_lsp/tests/fixtures/effect_lenses"
    toml = Path(args.toml).read_text() if args.toml else '[workspace]\nname = "replay"\n'
    files = sorted(p for p in Path(args.dir).iterdir() if p.is_file() and p.suffix != ".toml")
    if args.each:
        for f in files:
            print(f.name, attempt(binary, runtime, [f], toml, fixtures))
    else:
        print(attempt(binary, runtime, files, toml, fixtures))


if __name__ == "__main__":
    main()
