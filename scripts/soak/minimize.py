#!/usr/bin/env python3
"""Shrinks a file that kills the daemon: drops lines, then chunks of characters, while it still does.

  scripts/soak/minimize.py --binary target/release/oxabl --runtime target/fuzz-runtime-min FILE OUT
"""

import argparse
import shutil
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import lsp  # noqa: E402


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True)
    ap.add_argument("--runtime", required=True)
    ap.add_argument("file")
    ap.add_argument("out")
    args = ap.parse_args()
    binary = Path(args.binary).resolve()
    runtime = lsp.runtime_dir(args.runtime)
    root = Path(tempfile.mkdtemp(prefix="min-", dir=runtime.parent)).resolve()
    (root / "oxabl.toml").write_text('[workspace]\nname = "min"\n')
    path = root / Path(args.file).name

    def kills(text):
        path.write_text(text, encoding="utf-8")
        client = lsp.Lsp(binary, root, runtime)
        client.initialize(timeout=30)
        client.notify("textDocument/didOpen", {"textDocument": {"uri": lsp.uri(path), "languageId": "abl", "version": 1, "text": text}})
        client.request("textDocument/formatting", {"textDocument": {"uri": lsp.uri(path)}, "options": {"tabSize": 4, "insertSpaces": True}}, timeout=20)
        dead = client.proc.poll() is not None
        client.kill()
        lsp.stop_daemons(runtime)
        return dead

    text = Path(args.file).read_text(encoding="utf-8")
    assert kills(text), "the input does not kill the daemon"
    for unit in ("lines", "chars"):
        size = max(1, (len(text.splitlines(True)) if unit == "lines" else len(text)) // 2)
        while size >= 1:
            parts = text.splitlines(True) if unit == "lines" else list(text)
            changed = False
            i = 0
            while i < len(parts):
                trial = "".join(parts[:i] + parts[i + size :])
                if trial and kills(trial):
                    parts = parts[:i] + parts[i + size :]
                    text = "".join(parts)
                    changed = True
                else:
                    i += size
            if not changed:
                size //= 2
    Path(args.out).write_text(text, encoding="utf-8")
    print(f"{len(text)} bytes: {text!r}")
    shutil.rmtree(root, ignore_errors=True)


if __name__ == "__main__":
    main()
