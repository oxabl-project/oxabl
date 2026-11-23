//! One `oxabl lsp` session over stdio, for the black-box tests of the language
//! server. Each test file includes it with `#[path]`.

#![allow(dead_code)]

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{Registration, canonical_root, registration_path_in};
use serde_json::{Value, json};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

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
    next_id: u64,
    _base: tempfile::TempDir,
}

impl Session {
    pub fn start(root: &Path) -> Session {
        Session::start_with(root, json!({}))
    }

    /// Start with the given client capabilities.
    pub fn start_with(root: &Path, capabilities: Value) -> Session {
        let base = tempfile::tempdir().unwrap();
        let registration = registration_path_in(
            &base.path().join("oxabl").join("daemon"),
            &canonical_root(root).unwrap(),
        );
        let stderr = base.path().join("lsp.stderr");
        // No core dump from a daemon that crashes: it inherits the limit.
        let mut child = Command::new("sh")
            .arg("-c")
            .arg(r#"ulimit -c 0 && exec "$@""#)
            .arg("sh")
            .arg(OXABL_BIN)
            .arg("lsp")
            .env("XDG_RUNTIME_DIR", base.path())
            .env("XDG_CACHE_HOME", base.path())
            .env("HOME", base.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(std::fs::File::create(&stderr).unwrap())
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
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
            next_id: 1,
            _base: base,
        };
        session.send(json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize",
            "params": {
                "processId": null,
                "rootUri": file_uri(root),
                "capabilities": capabilities
            }
        }));
        session.wait_for(|m| m["id"] == json!(0));
        session.send(json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}));
        session
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

    pub fn wait_for(&self, wanted: impl Fn(&Value) -> bool) -> Value {
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

impl Drop for Session {
    fn drop(&mut self) {
        if let Ok(bytes) = std::fs::read(&self.registration)
            && let Ok(registration) = serde_json::from_slice::<Registration>(&bytes)
        {
            signal(registration.pid, 15);
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
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
