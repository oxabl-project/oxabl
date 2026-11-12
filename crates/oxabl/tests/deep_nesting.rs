//! A file that nests too deep is a parse error, never a crash.
//!
//! The parser and every pass over its tree recurse once per level, and Rust
//! cannot catch a stack overflow: the process aborts. In the daemon that ends
//! every editor window of the workspace. So the parser stops at a fixed depth
//! (and a fixed operator-chain length), and every thread that parses runs on a
//! stack of known size. These tests drive the real binary: `oxabl check` (on
//! one thread and on several, under a small `ulimit -s`) and `oxabl lsp`
//! with the daemon it starts.

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

const DEPTH_ERROR: &str = "Nesting is deeper than";
const CHAIN_ERROR: &str = "An operator chain is longer than";

/// Shapes that nest about `n` levels, each a file of its own.
fn deep_shapes(n: usize) -> Vec<(&'static str, String)> {
    vec![
        (
            "parens.p",
            format!("x = {}1{}.\n", "(".repeat(n), ")".repeat(n)),
        ),
        (
            "do.p",
            format!("{}{}", "DO:\n".repeat(n), "END.\n".repeat(n)),
        ),
        ("if.p", format!("{}MESSAGE 1.\n", "IF TRUE THEN ".repeat(n))),
        (
            "if-do.p",
            format!("{}{}", "IF TRUE THEN DO:\n".repeat(n), "END.\n".repeat(n)),
        ),
        (
            "call.p",
            format!("x = {}1{}.\n", "f(".repeat(n), ")".repeat(n)),
        ),
        (
            "else-if.p",
            format!("IF a THEN x = 1.\n{}", "ELSE IF a THEN x = 1.\n".repeat(n)),
        ),
        ("minus.p", format!("x = {}1.\n", "-".repeat(n))),
        ("not.p", format!("x = {}a.\n", "NOT ".repeat(n))),
        (
            "ternary.p",
            format!("x = {}1{}.\n", "IF a THEN ".repeat(n), " ELSE 2".repeat(n)),
        ),
        (
            "ternary-else.p",
            format!("x = {}2.\n", "IF a THEN 1 ELSE ".repeat(n)),
        ),
        (
            "case.p",
            format!(
                "{}{}",
                "CASE a:\nWHEN 1 THEN DO:\n".repeat(n / 2),
                "END.\nEND CASE.\n".repeat(n / 2)
            ),
        ),
        (
            "repeat-for.p",
            format!(
                "{}{}",
                "REPEAT:\nFOR EACH t:\n".repeat(n / 2),
                "END.\nEND.\n".repeat(n / 2)
            ),
        ),
        (
            "can-find.p",
            format!(
                "x = {}TRUE{}.\n",
                "CAN-FIND(FIRST t WHERE ".repeat(n),
                ")".repeat(n)
            ),
        ),
        (
            "generic.p",
            format!(
                "DEFINE VARIABLE x AS CLASS {}a{} NO-UNDO.\n",
                "List<".repeat(n),
                ">".repeat(n)
            ),
        ),
    ]
}

/// Left-deep operator chains of `n` operators.
fn long_chains(n: usize) -> Vec<(&'static str, String)> {
    vec![
        ("plus.p", format!("x = 1{}.\n", " + 1".repeat(n))),
        ("or.p", format!("IF a{} THEN x = 1.\n", " OR a".repeat(n))),
        ("member.p", format!("x = a{}.\n", ":b".repeat(n))),
        ("method.p", format!("x = a{}.\n", ":b()".repeat(n))),
        ("subscript.p", format!("x = a{}.\n", "[1]".repeat(n))),
    ]
}

fn write_all(dir: &Path, files: &[(&str, String)]) -> Vec<PathBuf> {
    files
        .iter()
        .map(|(name, text)| {
            let path = dir.join(name);
            std::fs::write(&path, text).unwrap();
            path
        })
        .collect()
}

/// `oxabl check --json <path>` under a 1 MiB `ulimit -s`, far below what the
/// deepest accepted tree needs, and with no core dump. Fails the test when the
/// process does not end normally.
fn check_small_stack(args: &[&str], path: &Path) -> Value {
    let output = Command::new("sh")
        .arg("-c")
        .arg(r#"ulimit -c 0 && ulimit -s 1024 && exec "$@""#)
        .arg("sh")
        .arg(OXABL_BIN)
        .args(["check", "--json"])
        .args(args)
        .arg(path)
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr);
    let tail = &stderr[stderr.len().saturating_sub(2000)..];
    assert!(
        matches!(output.status.code(), Some(0 | 1)),
        "{}: `oxabl check` ended on {:?}\n{tail}",
        path.display(),
        output.status
    );
    serde_json::from_slice(&output.stdout).unwrap()
}

/// The diagnostics of a `check --json` report whose message holds `text`.
fn count(report: &Value, text: &str) -> usize {
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|d| d["message"].as_str().unwrap().contains(text))
        .count()
}

#[test]
fn check_reports_a_deep_nest_once_and_does_not_crash() {
    let dir = tempfile::tempdir().unwrap();
    for path in write_all(dir.path(), &deep_shapes(5000)) {
        let report = check_small_stack(&[], &path);
        assert_eq!(
            count(&report, DEPTH_ERROR),
            1,
            "{}: expected one nesting error",
            path.display()
        );
    }
}

#[test]
fn check_reports_a_long_chain_once_and_does_not_crash() {
    let dir = tempfile::tempdir().unwrap();
    for path in write_all(dir.path(), &long_chains(50_000)) {
        let report = check_small_stack(&[], &path);
        assert_eq!(
            count(&report, CHAIN_ERROR),
            1,
            "{}: expected one chain error",
            path.display()
        );
    }
}

/// The preprocessor resolves a deep `&IF` nest before the parser sees it.
#[test]
fn check_survives_a_deep_preprocessor_nest() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("preprocessor-if.p");
    std::fs::write(
        &path,
        format!(
            "{}x = 1.\n{}",
            "&IF 1 &THEN\n".repeat(5000),
            "&ENDIF\n".repeat(5000)
        ),
    )
    .unwrap();
    for args in [&[][..], &["--no-preprocess"][..]] {
        check_small_stack(args, &path);
    }
}

/// Just under the limits every pass runs to the end: no limit error and no
/// crash, on one thread and on the worker threads of `--jobs 4`.
#[test]
fn check_analyses_a_nest_just_under_the_limits() {
    let dir = tempfile::tempdir().unwrap();
    let mut files = deep_shapes(250);
    files.extend(long_chains(4000));
    write_all(dir.path(), &files);
    for jobs in ["1", "4"] {
        let report = check_small_stack(&["--jobs", jobs], dir.path());
        assert_eq!(
            report["files_checked"],
            json!(files.len()),
            "--jobs {jobs}: not every file was checked"
        );
        assert_eq!(
            report["failures"],
            json!([]),
            "--jobs {jobs}: a file failed"
        );
        assert_eq!(
            count(&report, DEPTH_ERROR) + count(&report, CHAIN_ERROR),
            0,
            "--jobs {jobs}: a nest under the limits was refused"
        );
    }
}

/// One `oxabl lsp` session over pipes, with its daemon under a private runtime
/// directory.
struct Session {
    child: Child,
    stdin: ChildStdin,
    messages: mpsc::Receiver<Value>,
    registration: PathBuf,
    _base: tempfile::TempDir,
}

impl Session {
    fn start(root: &Path) -> Session {
        let base = tempfile::tempdir().unwrap();
        let registration = registration_path_in(
            &base.path().join("oxabl").join("daemon"),
            &canonical_root(root).unwrap(),
        );
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
            .stderr(Stdio::null())
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
            _base: base,
        };
        session.send(json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize",
            "params": {
                "processId": null,
                "rootUri": format!("file://{}", root.display()),
                "capabilities": {}
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
                    panic!("`oxabl lsp` closed its output: the session or its daemon died")
                }
            }
        }
    }

    /// The diagnostics published for `path`.
    fn diagnostics(&self, path: &Path) -> Vec<Value> {
        let uri = format!("file://{}", path.display());
        let published = self.wait_for(|m| {
            m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == uri
        });
        published["params"]["diagnostics"]
            .as_array()
            .cloned()
            .unwrap_or_default()
    }

    fn daemon_pid(&self) -> u32 {
        let registration: Registration =
            serde_json::from_slice(&std::fs::read(&self.registration).unwrap()).unwrap();
        registration.pid
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

fn messages_of(diagnostics: &[Value]) -> Vec<String> {
    diagnostics
        .iter()
        .map(|d| d["message"].as_str().unwrap_or_default().to_string())
        .collect()
}

#[test]
fn the_daemon_survives_a_deep_document_and_one_just_under_the_limit() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("oxabl.toml"),
        "[workspace]\nname = \"deep\"\n",
    )
    .unwrap();
    let mut session = Session::start(root.path());

    let deep = root.path().join("deep.p");
    session.open(
        &deep,
        &format!("{}MESSAGE 1.\n", "IF TRUE THEN ".repeat(5000)),
    );
    let messages = messages_of(&session.diagnostics(&deep));
    assert!(
        messages.iter().any(|m| m.contains(DEPTH_ERROR)),
        "expected the nesting error, got {messages:?}"
    );

    // The heaviest shape per level, just under the limit: every pass runs.
    let under = root.path().join("under.p");
    session.open(
        &under,
        &format!(
            "{}{}",
            "IF TRUE THEN DO:\n".repeat(250),
            "END.\n".repeat(250)
        ),
    );
    let messages = messages_of(&session.diagnostics(&under));
    assert!(
        !messages.iter().any(|m| m.contains(DEPTH_ERROR)),
        "a nest under the limit was refused: {messages:?}"
    );

    assert_eq!(
        signal(session.daemon_pid(), 0),
        0,
        "the daemon is gone after the deep documents"
    );
}
