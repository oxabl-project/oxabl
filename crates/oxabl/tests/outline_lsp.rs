//! The document outline through the real `oxabl lsp` and its daemon.
//!
//! The daemon sends its answer to the proxy as JSON, and the proxy reads it
//! with a recursion limit. A tree that is too deep must never end the session,
//! so these tests send documents that nest far more symbols than an editor
//! shows, as a client that asks for a hierarchical outline.

#![cfg(unix)]

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStdin, Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{Registration, canonical_root, registration_path_in};
use serde_json::{Value, json};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// One `oxabl lsp` session over pipes, with its daemon under a private runtime
/// directory.
struct Session {
    child: Child,
    stdin: ChildStdin,
    messages: mpsc::Receiver<Value>,
    registration: PathBuf,
    stderr: PathBuf,
    next_id: u64,
    _base: tempfile::TempDir,
}

impl Session {
    fn start(root: &Path) -> Session {
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
                "rootUri": format!("file://{}", root.display()),
                "capabilities": {"textDocument": {"documentSymbol": {
                    "hierarchicalDocumentSymbolSupport": true
                }}}
            }
        }));
        session.wait_for(|m| m["id"] == json!(0));
        session.send(json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}));
        session
    }

    fn send(&mut self, message: Value) {
        let body = message.to_string();
        write!(self.stdin, "Content-Length: {}\r\n\r\n{body}", body.len()).unwrap();
        self.stdin.flush().unwrap();
    }

    fn open(&mut self, path: &Path, text: &str) {
        self.send(json!({
            "jsonrpc": "2.0", "method": "textDocument/didOpen",
            "params": {"textDocument": {
                "uri": format!("file://{}", path.display()),
                "languageId": "abl", "version": 1, "text": text
            }}
        }));
    }

    fn wait_for(&self, wanted: impl Fn(&Value) -> bool) -> Value {
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

    /// The answer to `textDocument/documentSymbol` for `path`, which is open.
    fn outline(&mut self, path: &Path) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        self.send(json!({
            "jsonrpc": "2.0", "id": id, "method": "textDocument/documentSymbol",
            "params": {"textDocument": {"uri": format!("file://{}", path.display())}}
        }));
        self.wait_for(|m| m["id"] == json!(id))
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

/// The symbols of a successful answer. Fails on an error or a `null`.
#[track_caller]
fn symbols_of(answer: &Value) -> &Vec<Value> {
    assert!(
        answer.get("error").is_none(),
        "documentSymbol gave an error: {}",
        answer["error"]
    );
    answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("documentSymbol gave no list: {}", answer["result"]))
}

/// The number of levels of `symbols`, and the number of symbols in them.
fn shape(symbols: &[Value]) -> (usize, usize) {
    let mut levels = 0;
    let mut count = 0;
    for symbol in symbols {
        let children = symbol["children"].as_array().map_or(&[][..], |c| c);
        let (below, inside) = shape(children);
        levels = levels.max(1 + below);
        count += 1 + inside;
    }
    (levels, count)
}

fn labelled_blocks(depth: usize) -> String {
    let mut source = String::new();
    for i in 1..=depth {
        source.push_str(&format!("l{i}: DO:\n"));
    }
    source.push_str("DEFINE VARIABLE bottom AS INTEGER NO-UNDO.\n");
    source.push_str(&"END.\n".repeat(depth));
    source
}

fn interface_with_methods(count: usize) -> String {
    let mut source = String::from("INTERFACE I:\n");
    for i in 1..=count {
        source.push_str(&format!("METHOD PUBLIC VOID M{i}(INPUT x AS INTEGER).\n"));
    }
    source.push_str("END INTERFACE.\n");
    source
}

#[test]
fn a_deep_outline_is_answered_and_the_session_goes_on() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"outline\"\n",
    )
    .unwrap();
    let mut session = Session::start(root.path());

    let labelled = root.path().join("labelled.p");
    session.open(&labelled, &labelled_blocks(70));
    let answer = session.outline(&labelled);
    // 32 labels and the variable: the labels below the limit give no symbol.
    assert_eq!(shape(symbols_of(&answer)), (33, 33));

    let interface = root.path().join("iface.cls");
    session.open(&interface, &interface_with_methods(70));
    let answer = session.outline(&interface);
    let symbols = symbols_of(&answer);
    assert_eq!(symbols.len(), 1, "one interface");
    // The interface, its 70 methods and the parameter of each.
    assert_eq!(shape(symbols), (3, 141));

    let small = root.path().join("small.p");
    session.open(&small, "PROCEDURE p:\nEND PROCEDURE.\n");
    let answer = session.outline(&small);
    let symbols = symbols_of(&answer);
    assert_eq!(symbols.len(), 1);
    assert_eq!(symbols[0]["name"], json!("p"));
}
