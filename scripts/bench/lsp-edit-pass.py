#!/usr/bin/env python3
"""Measure the first find references request after a one-character edit in
another open file, through `oxabl lsp` and its daemon.

Usage: scripts/bench/lsp-edit-pass.py <oxabl binary> <workspace> <runtime dir>
                                      [<edited file> [<class file>]]

The workspace is one that `scripts/bench/gen-workspace --preset erp` wrote. The
runtime directory must be short (the daemon socket is in it) and is used only
by this run. The script prints, for each step, the wall time of the request,
the CPU seconds of the daemon during it, and the daemon's peak RSS. The
request asks for the references of the class of `<class file>`
(`src/cls/pk3/C120.cls` by default). The edit adds one character at the end of
`<edited file>` (`src/app/m47/p4716.p` by default, a program that two files
run). Both paths are relative to the workspace.
"""

import json
import os
import subprocess
import sys
import time
from pathlib import Path


def cpu_seconds(pid):
    fields = Path(f"/proc/{pid}/stat").read_text().rsplit(")", 1)[1].split()
    ticks = os.sysconf("SC_CLK_TCK")
    return (int(fields[11]) + int(fields[12])) / ticks


def peak_rss_mib(pid):
    for line in Path(f"/proc/{pid}/status").read_text().splitlines():
        if line.startswith("VmHWM:"):
            return int(line.split()[1]) / 1024
    return 0.0


class Client:
    def __init__(self, binary, root, runtime):
        env = dict(os.environ, XDG_RUNTIME_DIR=runtime, XDG_CACHE_HOME=runtime, HOME=runtime)
        self.proc = subprocess.Popen(
            [binary, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=open(os.environ.get("LSP_STDERR", os.devnull), "w"), env=env,
        )
        self.next_id = 1
        self.runtime = runtime
        self.request("initialize", {
            "processId": None, "rootUri": Path(root).as_uri(), "capabilities": {},
            "initializationOptions": {"referenceLenses": {"enable": False}},
        })
        self.notify("initialized", {})

    def send(self, message):
        body = json.dumps(message).encode()
        self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
        self.proc.stdin.flush()

    def read(self):
        length = None
        while True:
            line = self.proc.stdout.readline()
            if line in (b"\r\n", b""):
                break
            if line.lower().startswith(b"content-length:"):
                length = int(line.split(b":")[1])
        return json.loads(self.proc.stdout.read(length))

    def notify(self, method, params):
        self.send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params):
        ident = self.next_id
        self.next_id += 1
        self.send({"jsonrpc": "2.0", "id": ident, "method": method, "params": params})
        while True:
            message = self.read()
            if message.get("id") == ident and "method" not in message:
                return message

    def daemon_pid(self):
        for path in Path(self.runtime).rglob("*.json"):
            try:
                return json.loads(path.read_text())["pid"]
            except (ValueError, KeyError):
                continue
        return None


def main():
    binary, root, runtime = sys.argv[1:4]
    root = Path(root).resolve()
    other = root / (sys.argv[4] if len(sys.argv) > 4 else "src/app/m47/p4716.p")
    target = root / (sys.argv[5] if len(sys.argv) > 5 else "src/cls/pk3/C120.cls")
    client = Client(binary, str(root), runtime)
    texts = {path: path.read_text() for path in (target, other)}
    for path, text in texts.items():
        client.notify("textDocument/didOpen", {"textDocument": {
            "uri": path.as_uri(), "languageId": "abl", "version": 1, "text": text}})
    line = next(
        at for at, text in enumerate(texts[target].splitlines()) if text.startswith("CLASS ")
    )
    params = {
        "textDocument": {"uri": target.as_uri()},
        "position": {"line": line, "character": 12},
        "context": {"includeDeclaration": False},
    }

    def measure(name):
        pid = client.daemon_pid()
        before = cpu_seconds(pid) if pid else 0.0
        started = time.monotonic()
        answer = client.request("textDocument/references", params)
        wall = time.monotonic() - started
        pid = pid or client.daemon_pid()
        cpu = cpu_seconds(pid) - before
        count = len(answer.get("result") or [])
        print(f"{name}: wall {wall:.2f} s, daemon cpu {cpu:.2f} s, "
              f"peak rss {peak_rss_mib(pid):.0f} MiB, {count} references", flush=True)

    measure("first request (cold pass)")
    measure("second request, no edit")
    edited = texts[other] + "\n"
    client.notify("textDocument/didChange", {
        "textDocument": {"uri": other.as_uri(), "version": 2},
        "contentChanges": [{"text": edited}]})
    measure("first request after a one-character edit in another open file")
    measure("next request, no edit")
    client.proc.stdin.close()
    client.proc.terminate()
    pid = client.daemon_pid()
    if pid:
        os.kill(pid, 15)


if __name__ == "__main__":
    main()
