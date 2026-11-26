//! One `oxabl lsp` session over stdio, for the black-box tests of the language
//! server. Each test file includes it with `#[path]`.

#![allow(dead_code)]

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStderr, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{Registration, canonical_root, registration_path_in};
use serde_json::{Value, json};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// The idle timeout of a test's daemon, unless the test sets its own. A test that
/// ends without stopping its daemon (a killed test binary) leaves it for this long
/// at most.
const TEST_IDLE_TIMEOUT: &str = "2m";

/// The `file:` URI of an absolute path, with a space escaped as clients do.
pub fn file_uri(path: &Path) -> String {
    format!("file://{}", path.display()).replace(' ', "%20")
}

/// One `oxabl lsp` session over pipes, with its daemon under a private runtime
/// directory.
pub struct Session {
    child: Child,
    stdin: ChildStdin,
    messages: mpsc::Receiver<Value>,
    registration: PathBuf,
    stderr: PathBuf,
    /// The read end of stderr, for a session started with [`Launch::stderr_pipe`].
    stderr_pipe: Option<ChildStderr>,
    /// Whether dropping the session stops its daemon.
    stops_daemon: bool,
    next_id: u64,
    /// The answer to `initialize`.
    pub initialized: Value,
    _base: tempfile::TempDir,
}

impl Session {
    pub fn start(root: &Path) -> Session {
        Session::start_with(root, json!({}))
    }

    /// Start with the given client capabilities.
    pub fn start_with(root: &Path, capabilities: Value) -> Session {
        Session::start_with_options(root, capabilities, Value::Null)
    }

    /// Start with the given client capabilities and initialization options
    /// (none when `null`).
    pub fn start_with_options(root: &Path, capabilities: Value, options: Value) -> Session {
        let mut params = json!({"capabilities": capabilities});
        if !options.is_null() {
            params["initializationOptions"] = options;
        }
        Session::start_with_params(root, params).0
    }

    /// Start with `params` added to the `initialize` parameters, and return the
    /// messages the server sent before its answer as well.
    pub fn start_with_params(root: &Path, params: Value) -> (Session, Vec<Value>) {
        Session::launch(root, params).await_initialize()
    }

    /// Wait for the answer to `initialize` of a launched session, send `initialized`,
    /// and return the messages the server sent before its answer as well.
    pub fn await_initialize(self) -> (Session, Vec<Value>) {
        let mut session = self;
        let mut early = Vec::new();
        session.initialized = session.wait_for(|m| {
            let answer = m["id"] == json!(0) && m.get("method").is_none();
            if !answer {
                early.push(m.clone());
            }
            answer
        });
        session.send(json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}));
        (session, early)
    }

    /// Start `oxabl lsp` and send `initialize` with `params` added, without waiting
    /// for the answer.
    pub fn launch(root: &Path, params: Value) -> Session {
        Session::launch_after(root, params, |_| {})
    }

    /// [`Session::launch`], with `before` run on the path of the registration file
    /// before `oxabl lsp` starts.
    pub fn launch_after(root: &Path, params: Value, before: impl FnOnce(&Path)) -> Session {
        Session::launch_with(root, params, Launch::default(), before)
    }

    /// [`Session::launch_after`], with the runtime directory and stderr of `launch`.
    pub fn launch_with(
        root: &Path,
        params: Value,
        launch: Launch<'_>,
        before: impl FnOnce(&Path),
    ) -> Session {
        let base = tempfile::tempdir().unwrap();
        let runtime = launch.runtime.unwrap_or(base.path());
        let registration = registration_path_in(
            &runtime.join("oxabl").join("daemon"),
            &canonical_root(root).unwrap(),
        );
        before(&registration);
        let stderr = base.path().join("lsp.stderr");
        let stderr_target = if launch.stderr_pipe {
            Stdio::piped()
        } else {
            Stdio::from(std::fs::File::create(&stderr).unwrap())
        };
        // No core dump from a daemon that crashes: it inherits the limit.
        let mut command = Command::new("sh");
        match launch.schema_cache {
            // `CI` (set on CI runners) or `OXABL_CACHE_MODE` turns the cache off.
            Some(directory) => command
                .env("OXABL_SCHEMA_CACHE_DIR", directory)
                .env_remove("CI")
                .env_remove("OXABL_CACHE_MODE")
                .env_remove("OXABL_SCHEMA_CACHE"),
            None => command.env_remove("OXABL_SCHEMA_CACHE_DIR"),
        };
        command
            .arg("-c")
            .arg(r#"ulimit -c 0 && exec "$@""#)
            .arg("sh")
            .arg(OXABL_BIN)
            .arg("lsp")
            .env("XDG_RUNTIME_DIR", runtime)
            .env("XDG_CACHE_HOME", runtime)
            .env("HOME", runtime)
            .env("OXABL_DAEMON_IDLE_TIMEOUT", TEST_IDLE_TIMEOUT);
        for (name, value) in launch.env {
            command.env(name, value);
        }
        let mut child = command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(stderr_target)
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
        let stderr_pipe = child.stderr.take();
        let mut stdout = child.stdout.take().unwrap();
        let (tx, messages) = mpsc::channel();
        thread::spawn(move || {
            let mut buf = Vec::new();
            let mut chunk = [0u8; 8192];
            while let Ok(n) = stdout.read(&mut chunk) {
                if n == 0 {
                    return;
                }
                buf.extend_from_slice(&chunk[..n]);
                while let Some((body, used)) = take_message(&buf) {
                    buf.drain(..used);
                    if tx.send(body).is_err() {
                        return;
                    }
                }
            }
        });
        let mut session = Session {
            child,
            stdin,
            messages,
            registration,
            stderr,
            stderr_pipe,
            stops_daemon: true,
            next_id: 1,
            initialized: Value::Null,
            _base: base,
        };
        let mut all = json!({
            "processId": null,
            "rootUri": file_uri(root),
            "capabilities": {}
        });
        if let (Some(all), Some(params)) = (all.as_object_mut(), params.as_object()) {
            all.extend(params.clone());
        }
        session.send(json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize", "params": all
        }));
        session
    }

    /// The path of the daemon's registration file; its socket and lock file are
    /// beside it, with the extensions `sock` and `lock`.
    pub fn registration_path(&self) -> &Path {
        &self.registration
    }

    /// The exit status of `oxabl lsp` once it exits within `limit`.
    pub fn wait_exit(&mut self, limit: Duration) -> Option<std::process::ExitStatus> {
        let deadline = Instant::now() + limit;
        while Instant::now() < deadline {
            if let Some(status) = self.child.try_wait().unwrap() {
                return Some(status);
            }
            thread::sleep(Duration::from_millis(20));
        }
        None
    }

    /// Kill `oxabl lsp` and close the read end of its stderr, as an editor does when
    /// it closes, and leave its daemon running.
    pub fn end_client(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
        self.stderr_pipe = None;
        self.stops_daemon = false;
    }

    /// Read the stderr pipe of a session started with [`Launch::stderr_pipe`] until
    /// every writer has closed it or `limit` has passed, 4 KiB every 10 ms, as a slow
    /// reader does.
    pub fn read_stderr_slowly(&mut self, limit: Duration) -> String {
        let mut pipe = self
            .stderr_pipe
            .take()
            .expect("a session with a stderr pipe");
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            let mut chunk = [0u8; 4096];
            while let Ok(n) = pipe.read(&mut chunk) {
                if n == 0 || tx.send(chunk[..n].to_vec()).is_err() {
                    return;
                }
                thread::sleep(Duration::from_millis(10));
            }
        });
        let deadline = Instant::now() + limit;
        let mut text = Vec::new();
        while let Ok(bytes) = rx.recv_timeout(deadline.saturating_duration_since(Instant::now())) {
            text.extend_from_slice(&bytes);
        }
        String::from_utf8_lossy(&text).into_owned()
    }

    /// What `oxabl lsp` and its daemon wrote to stderr so far.
    pub fn stderr(&self) -> String {
        std::fs::read_to_string(&self.stderr).unwrap_or_default()
    }

    pub fn send(&mut self, message: Value) {
        let body = message.to_string();
        write!(self.stdin, "Content-Length: {}\r\n\r\n{body}", body.len()).unwrap();
        self.stdin.flush().unwrap();
    }

    pub fn open(&mut self, path: &Path, text: &str) {
        self.send(json!({
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": {"textDocument": {
                "uri": file_uri(path),
                "languageId": "abl", "version": 1, "text": text
            }}
        }));
    }

    pub fn wait_for(&self, mut wanted: impl FnMut(&Value) -> bool) -> Value {
        let deadline = Instant::now() + Duration::from_secs(60);
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            match self.messages.recv_timeout(left) {
                Ok(message) if wanted(&message) => return message,
                Ok(_) => {}
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    panic!("no awaited message from `oxabl lsp` in 60 s")
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    let stderr = std::fs::read_to_string(&self.stderr).unwrap_or_default();
                    panic!("`oxabl lsp` closed its output; its stderr:\n{stderr}")
                }
            }
        }
    }

    /// The next message within `timeout`, or `None` when none came.
    pub fn try_next(&self, timeout: Duration) -> Option<Value> {
        self.messages.recv_timeout(timeout).ok()
    }

    /// The answers to `method` with each of `params`, in order. All requests
    /// are sent before the first answer is read, so the server works on them
    /// together.
    pub fn request_all(&mut self, method: &str, params: Vec<Value>) -> Vec<Value> {
        let first = self.next_id;
        for params in params {
            let id = self.next_id;
            self.next_id += 1;
            self.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
        }
        let mut answers = vec![Value::Null; (self.next_id - first) as usize];
        let mut left = answers.len();
        while left > 0 {
            let answer = self.wait_for(|m| {
                m["id"]
                    .as_u64()
                    .is_some_and(|id| id >= first && id < self.next_id)
                    && m.get("method").is_none()
            });
            assert!(
                answer.get("result").is_some() != answer.get("error").is_some(),
                "an answer to `{method}` must hold a result or an error: {answer}"
            );
            let at = (answer["id"].as_u64().unwrap() - first) as usize;
            answers[at] = answer;
            left -= 1;
        }
        answers
    }

    /// Take the id for a request the caller sends itself.
    pub fn next_request_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id += 2;
        id
    }

    /// The answer to the request `method` with `params`.
    pub fn request(&mut self, method: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
        let answer = self.wait_for(|m| m["id"] == json!(id));
        // JSON-RPC: a response holds exactly one of the two. A client rejects
        // one with neither, and indexing `answer["result"]` would read it as null.
        assert!(
            answer.get("result").is_some() != answer.get("error").is_some(),
            "the answer to `{method}` must hold a result or an error: {answer}"
        );
        answer
    }
}

impl Session {
    /// The pid of the daemon that serves this session, once it has
    /// registered itself.
    pub fn daemon_pid(&self) -> Option<u32> {
        let bytes = std::fs::read(&self.registration).ok()?;
        serde_json::from_slice::<Registration>(&bytes)
            .ok()
            .map(|r| r.pid)
    }
}

impl Drop for Session {
    /// Stop `oxabl lsp` and its daemon, by pid, also when the test panics. The daemon
    /// is found in its registration and, when it has not registered yet, among the
    /// children of `oxabl lsp`.
    fn drop(&mut self) {
        let mut daemons = Vec::new();
        if self.stops_daemon {
            daemons.extend(self.daemon_pid());
            daemons.extend(child_pids(self.child.id()));
            daemons.sort_unstable();
            daemons.dedup();
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
        stop_processes(&daemons);
    }
}

/// Whether `pid` is an `oxabl daemon` of this build. A registration can outlive its
/// daemon, and the system can give its pid to another process.
fn is_oxabl_daemon(pid: u32) -> bool {
    let Ok(exe) = std::fs::read_link(format!("/proc/{pid}/exe")) else {
        return false;
    };
    let Ok(bin) = std::fs::canonicalize(OXABL_BIN) else {
        return false;
    };
    let Ok(cmdline) = std::fs::read(format!("/proc/{pid}/cmdline")) else {
        return false;
    };
    exe == bin
        && cmdline
            .split(|&byte| byte == 0)
            .nth(1)
            .is_some_and(|arg| arg == b"daemon")
}

/// Stop each `oxabl daemon` of this build among `pids` with SIGTERM, and with SIGKILL
/// when it is still there after 5 s. Any other process is left alone.
pub fn stop_processes(pids: &[u32]) {
    let pids: Vec<u32> = pids
        .iter()
        .copied()
        .filter(|&pid| is_oxabl_daemon(pid))
        .collect();
    let pids = pids.as_slice();
    for &pid in pids {
        signal(pid, 15);
    }
    let deadline = Instant::now() + Duration::from_secs(5);
    while pids.iter().any(|&pid| is_running(pid)) && Instant::now() < deadline {
        thread::sleep(Duration::from_millis(20));
    }
    for &pid in pids {
        if is_running(pid) {
            signal(pid, 9);
        }
    }
}

/// Whether the process `pid` exists and has not ended. A process that ended and
/// waits for its parent is not running.
pub fn is_running(pid: u32) -> bool {
    if signal(pid, 0) != 0 {
        return false;
    }
    // The third field of `/proc/<pid>/stat` after the command is the state.
    match std::fs::read_to_string(format!("/proc/{pid}/stat")) {
        Ok(stat) => stat
            .rsplit_once(')')
            .and_then(|(_, rest)| rest.split_whitespace().next())
            .is_none_or(|state| state != "Z" && state != "X"),
        Err(_) => true,
    }
}

/// The children of process `pid`, where `/proc` names them. Empty elsewhere.
fn child_pids(pid: u32) -> Vec<u32> {
    let Ok(tasks) = std::fs::read_dir(format!("/proc/{pid}/task")) else {
        return Vec::new();
    };
    tasks
        .flatten()
        .filter_map(|task| std::fs::read_to_string(task.path().join("children")).ok())
        .flat_map(|children| {
            children
                .split_whitespace()
                .filter_map(|child| child.parse().ok())
                .collect::<Vec<u32>>()
        })
        .collect()
}

/// Where [`Session::launch_with`] runs `oxabl lsp`.
#[derive(Default)]
pub struct Launch<'a> {
    /// A runtime directory that several sessions share, so they find one daemon.
    /// By default each session has its own.
    pub runtime: Option<&'a Path>,
    /// Give stderr a pipe that the session holds and does not read, in place of a
    /// file. [`Session::stderr`] is then empty.
    pub stderr_pipe: bool,
    /// The directory of the schema cache (`OXABL_SCHEMA_CACHE_DIR`). The session
    /// then removes `CI`, `OXABL_CACHE_MODE` and `OXABL_SCHEMA_CACHE`, so the
    /// cache is on also on a CI runner. By default the session has none, and a
    /// test build keeps the cache off.
    pub schema_cache: Option<&'a Path>,
    /// Environment variables for `oxabl lsp`, and so for the daemon it starts.
    pub env: &'a [(&'a str, &'a str)],
}

fn signal(pid: u32, signal: i32) -> i32 {
    unsafe extern "C" {
        fn kill(pid: i32, signal: i32) -> i32;
    }
    // Only the daemon registered under the test's private runtime directory.
    unsafe { kill(pid as i32, signal) }
}

fn take_message(buf: &[u8]) -> Option<(Value, usize)> {
    let header_end = buf.windows(4).position(|w| w == b"\r\n\r\n")?;
    let header = std::str::from_utf8(&buf[..header_end]).ok()?;
    let length: usize = header
        .split("\r\n")
        .find_map(|line| line.strip_prefix("Content-Length:"))?
        .trim()
        .parse()
        .ok()?;
    let end = header_end + 4 + length;
    let body = buf.get(header_end + 4..end)?;
    Some((serde_json::from_slice(body).ok()?, end))
}
