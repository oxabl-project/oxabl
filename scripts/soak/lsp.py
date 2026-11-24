"""A scripted LSP client for `oxabl lsp`, and the process probes the soak scripts share.

Every process it starts runs with XDG_RUNTIME_DIR set to a directory of the
caller, so its daemons register there and never meet another user's. Nothing
here signals a process it did not start.
"""

import json
import os
import queue
import resource
import signal
import subprocess
import threading
import time
from pathlib import Path

CLK_TCK = os.sysconf("SC_CLK_TCK")
MEMORY_CAP = 6 << 30


def _limits():
    # Inherited by the daemon the lsp starts: a runaway ends as a crash, not a swap storm.
    resource.setrlimit(resource.RLIMIT_AS, (MEMORY_CAP, MEMORY_CAP))
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))


def runtime_dir(path):
    path = Path(path).resolve()
    path.mkdir(mode=0o700, parents=True, exist_ok=True)
    os.chmod(path, 0o700)
    return path


def uri(path):
    return Path(path).resolve().as_uri()


class Lsp:
    """One `oxabl lsp` process, driven over stdio."""

    def __init__(self, binary, root, runtime, lens=False, extra_env=None, stderr=None):
        env = dict(os.environ, XDG_RUNTIME_DIR=str(runtime), **(extra_env or {}))
        self.root = Path(root).resolve()
        self.proc = subprocess.Popen(
            ["nice", "-n", "10", str(Path(binary).resolve()), "lsp"],
            cwd=self.root,
            env=env,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=stderr or subprocess.DEVNULL,
            preexec_fn=_limits,
        )
        self.next_id = 1
        self.pending = {}
        self.lock = threading.Lock()
        self.notifications = 0
        self.server_requests = 0
        self.lens = lens
        self.reader = threading.Thread(target=self._read, daemon=True)
        self.reader.start()

    # --- transport -----------------------------------------------------

    def _read(self):
        out = self.proc.stdout
        while True:
            length = None
            while True:
                line = out.readline()
                if not line:
                    return
                line = line.strip()
                if not line:
                    break
                if line.lower().startswith(b"content-length:"):
                    length = int(line.split(b":")[1])
            if length is None:
                return
            body = json.loads(out.read(length))
            self._dispatch(body)

    def _dispatch(self, message):
        if "method" in message and "id" in message:
            self.server_requests += 1
            self._send({"jsonrpc": "2.0", "id": message["id"], "result": None})
        elif "method" in message:
            self.notifications += 1
        else:
            with self.lock:
                waiter = self.pending.pop(message["id"], None)
            if waiter:
                waiter.put(message)

    def _send(self, message):
        data = json.dumps(message).encode()
        try:
            self.proc.stdin.write(b"Content-Length: %d\r\n\r\n" % len(data) + data)
            self.proc.stdin.flush()
        except (BrokenPipeError, ValueError):
            pass

    def notify(self, method, params):
        self._send({"jsonrpc": "2.0", "method": method, "params": params})

    def request(self, method, params, timeout=30):
        with self.lock:
            rid = self.next_id
            self.next_id += 1
            waiter = queue.Queue()
            self.pending[rid] = waiter
        self._send({"jsonrpc": "2.0", "id": rid, "method": method, "params": params})
        end = time.time() + timeout
        while time.time() < end:
            try:
                return waiter.get(timeout=0.2)
            except queue.Empty:
                if self.proc.poll() is not None:
                    break
        with self.lock:
            self.pending.pop(rid, None)
        return None

    # --- session -------------------------------------------------------

    def initialize(self, timeout=30):
        caps = {}
        if self.lens:
            caps = {
                "textDocument": {"codeLens": {"dynamicRegistration": False}},
                "workspace": {"codeLens": {"refreshSupport": True}},
            }
        reply = self.request(
            "initialize",
            {
                "processId": os.getpid(),
                "capabilities": caps,
                "workspaceFolders": [{"uri": uri(self.root), "name": "soak"}],
            },
            timeout=timeout,
        )
        if reply is not None:
            self.notify("initialized", {})
        return reply

    def shutdown(self, timeout=10):
        self.request("shutdown", None, timeout=timeout)
        self.notify("exit", None)
        try:
            self.proc.stdin.close()
        except OSError:
            pass
        try:
            return self.proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            self.proc.kill()
            return self.proc.wait()

    def kill(self):
        """SIGKILL: the editor crashed."""
        self.proc.kill()
        self.proc.wait()


# --- /proc probes ------------------------------------------------------


def stat(pid):
    """(state, ppid, utime+stime seconds, threads, rss kB) or None when gone."""
    try:
        raw = Path(f"/proc/{pid}/stat").read_text()
        fields = raw[raw.rindex(")") + 2 :].split()
        status = Path(f"/proc/{pid}/status").read_text()
    except OSError:
        return None
    rss = threads = 0
    for line in status.splitlines():
        if line.startswith("VmRSS:"):
            rss = int(line.split()[1])
        elif line.startswith("Threads:"):
            threads = int(line.split()[1])
    cpu = (int(fields[11]) + int(fields[12])) / CLK_TCK
    return fields[0], int(fields[1]), cpu, threads, rss


def fd_count(pid):
    try:
        return len(os.listdir(f"/proc/{pid}/fd"))
    except OSError:
        return None


def comm(pid):
    try:
        return Path(f"/proc/{pid}/comm").read_text().strip()
    except OSError:
        return None


def args(pid):
    try:
        return Path(f"/proc/{pid}/cmdline").read_bytes().replace(b"\0", b" ").decode(errors="replace").strip()
    except OSError:
        return ""


def processes_of(runtime):
    """Every process of this user whose environment names `runtime` as XDG_RUNTIME_DIR."""
    needle = f"XDG_RUNTIME_DIR={runtime}".encode()
    found = []
    me = os.getuid()
    for entry in os.listdir("/proc"):
        if not entry.isdigit():
            continue
        pid = int(entry)
        try:
            if os.stat(f"/proc/{pid}").st_uid != me:
                continue
            environ = Path(f"/proc/{pid}/environ").read_bytes().split(b"\0")
        except OSError:
            continue
        if needle in environ:
            info = stat(pid)
            if info:
                found.append((pid, info[0], info[1], info[3], info[4], comm(pid), args(pid)))
    return found


def oxabl_processes(runtime):
    """(daemons, lsps, zombies, others) of `runtime`, as lists of pids."""
    daemons, lsps, zombies, others = [], [], [], []
    for pid, state, ppid, threads, rss, name, cmdline in processes_of(runtime):
        if state == "Z":
            zombies.append(pid)
        elif " daemon " in f" {cmdline} " and "oxabl" in cmdline:
            daemons.append(pid)
        elif cmdline.endswith(" lsp") and "oxabl" in cmdline:
            lsps.append(pid)
        else:
            others.append(pid)
    return daemons, lsps, zombies, others


def total(pids):
    threads = rss = 0
    cpu = 0.0
    for pid in pids:
        info = stat(pid)
        if info:
            cpu += info[2]
            threads += info[3]
            rss += info[4]
    return cpu, threads, rss


def stop_daemons(runtime, timeout=5):
    """TERM then KILL every daemon of `runtime`, by pid."""
    for pid in oxabl_processes(runtime)[0]:
        try:
            os.kill(pid, signal.SIGTERM)
        except OSError:
            pass
    end = time.time() + timeout
    while time.time() < end and oxabl_processes(runtime)[0]:
        time.sleep(0.1)
    for pid in oxabl_processes(runtime)[0]:
        try:
            os.kill(pid, signal.SIGKILL)
        except OSError:
            pass


def daemon_for(runtime, root):
    """The pid the registry of `runtime` names for the workspace root `root`, if it is alive."""
    root = str(Path(root).resolve())
    for path in (Path(runtime) / "oxabl" / "daemon").glob("*.json"):
        try:
            registration = json.loads(path.read_text())
        except (OSError, ValueError):
            continue
        if registration.get("workspace_root") == root and stat(registration["pid"]):
            return registration["pid"]
    return None
