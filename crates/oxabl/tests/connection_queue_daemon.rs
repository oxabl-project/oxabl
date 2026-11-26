//! What bounds the frames a daemon connection holds, and what one client that does
//! not read does to the other clients.
//!
//! The reader of a connection never waits for its consumer, and a sender never waits
//! for the writer, so each connection has two queues. A peer that writes and does
//! not read its answers fills them. When the frames that wait in one queue pass the
//! high-water mark, the daemon closes that connection and serves the other clients
//! on. A test build reads a low mark from `OXABL_TEST_QUEUE_LIMIT`, so these tests
//! need only a few MB.

#![cfg(unix)]

use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::path::{Path, PathBuf};
use std::process::{Child, ChildStderr, Command, Stdio};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{Registration, canonical_root, registration_path_in};
use serde_json::{Value, json};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// The high-water mark the daemon gets in these tests.
const QUEUE_LIMIT: usize = 1 << 20;

/// What a client writes at most. A daemon that still reads after this has no
/// bound.
const WRITE_LIMIT: usize = 64 << 20;

/// How long a held site of the daemon waits, in milliseconds.
const HOLD_MS: u64 = 3000;

/// The time for each step that the daemon must do.
const LIMIT: Duration = Duration::from_secs(60);

/// One `oxabl daemon`, with its stderr kept.
struct Daemon {
    child: Child,
    socket: PathBuf,
    stderr: Arc<Mutex<String>>,
    root: tempfile::TempDir,
    _base: tempfile::TempDir,
}

impl Daemon {
    /// Start a daemon for a new root, with `limit` as its high-water mark (the
    /// default mark when `None`).
    fn start(limit: Option<usize>) -> Daemon {
        let root = tempfile::tempdir().unwrap();
        let base = tempfile::tempdir().unwrap();
        std::fs::write(
            root.path().join("oxabl.toml"),
            "[workspace]\nname = \"queue\"\n",
        )
        .unwrap();
        let registration = registration_path_in(
            &base.path().join("oxabl").join("daemon"),
            &canonical_root(root.path()).unwrap(),
        );
        let mut command = Command::new(OXABL_BIN);
        command
            .arg("daemon")
            .arg(root.path())
            .env("XDG_RUNTIME_DIR", base.path())
            .env("XDG_CACHE_HOME", base.path())
            .env("HOME", base.path())
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped());
        if let Some(limit) = limit {
            command.env("OXABL_TEST_QUEUE_LIMIT", limit.to_string());
        }
        let mut child = command.spawn().unwrap();
        let stderr = Arc::new(Mutex::new(String::new()));
        keep_stderr(child.stderr.take().unwrap(), Arc::clone(&stderr));
        let started = Instant::now();
        while !registration.exists() && started.elapsed() < LIMIT {
            assert!(
                child.try_wait().unwrap().is_none(),
                "the daemon exited at its start"
            );
            thread::sleep(Duration::from_millis(10));
        }
        let registration: Registration =
            serde_json::from_slice(&std::fs::read(&registration).expect("the daemon registers"))
                .unwrap();
        Daemon {
            child,
            socket: PathBuf::from(registration.socket_path),
            stderr,
            root,
            _base: base,
        }
    }

    fn root(&self) -> &Path {
        self.root.path()
    }

    fn uri(&self, name: &str) -> String {
        format!("file://{}/{name}", self.root().display())
    }

    /// Fail when the daemon process has exited.
    fn assert_alive(&mut self, after: &str) {
        if let Some(status) = self.child.try_wait().unwrap() {
            panic!(
                "the daemon exited ({status}) after {after}; its stderr:\n{}",
                self.stderr.lock().unwrap()
            );
        }
    }

    fn stderr(&self) -> String {
        self.stderr.lock().unwrap().clone()
    }

    /// Wait until the daemon's stderr holds `needle` `count` times.
    fn wait_for_stderr(&self, needle: &str, count: usize) {
        let started = Instant::now();
        while self.stderr().matches(needle).count() < count {
            assert!(
                started.elapsed() < LIMIT,
                "the daemon did not write `{needle}` {count} times in {LIMIT:?}; its stderr:\n{}",
                self.stderr()
            );
            thread::sleep(Duration::from_millis(10));
        }
    }

    /// A connected client that has had its answer to `initialize` and reads every
    /// frame from now on.
    fn client(&self) -> Client {
        let mut client = self.silent_client();
        client.read_answers();
        client
    }

    /// A connected client that has had its answer to `initialize` and reads nothing
    /// more until the test asks it to.
    fn silent_client(&self) -> Client {
        let stream = UnixStream::connect(&self.socket).unwrap();
        stream.set_write_timeout(Some(LIMIT)).unwrap();
        let mut client = Client {
            input: Some(BufReader::new(stream.try_clone().unwrap())),
            stream,
            messages: None,
        };
        client.write(&json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize",
            "params": {"processId": null, "rootUri": format!("file://{}", self.root().display()),
                "capabilities": {
                    "textDocument": {"documentSymbol": {"hierarchicalDocumentSymbolSupport": true}}
                }}
        }))
        .unwrap();
        let input = client.input.as_mut().unwrap();
        while read_frame(input).expect("an answer to `initialize`")["id"] != json!(0) {}
        client
            .write(&json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}))
            .unwrap();
        client
    }
}

impl Drop for Daemon {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn keep_stderr(stderr: ChildStderr, kept: Arc<Mutex<String>>) {
    thread::spawn(move || {
        for line in BufReader::new(stderr).lines() {
            let Ok(line) = line else { return };
            let mut kept = kept.lock().unwrap();
            kept.push_str(&line);
            kept.push('\n');
        }
    });
}

/// One raw client of the daemon's socket.
struct Client {
    stream: UnixStream,
    /// The input, until a reader thread takes it.
    input: Option<BufReader<UnixStream>>,
    /// The frames a reader thread has read, when the client reads.
    messages: Option<mpsc::Receiver<Value>>,
}

impl Client {
    fn write(&mut self, message: &Value) -> std::io::Result<usize> {
        let frame = frame(message);
        self.stream.write_all(&frame)?;
        Ok(frame.len())
    }

    /// Read every frame on a thread from now on.
    fn read_answers(&mut self) {
        let mut stream = self.input.take().expect("one reader");
        let (tx, rx) = mpsc::channel();
        thread::spawn(move || {
            while let Some(message) = read_frame(&mut stream) {
                if tx.send(message).is_err() {
                    return;
                }
            }
        });
        self.messages = Some(rx);
    }

    /// The answer to the request `id`, within the time limit.
    fn answer(&self, id: u64) -> Value {
        let messages = self.messages.as_ref().expect("a client that reads");
        let deadline = Instant::now() + LIMIT;
        loop {
            let left = deadline.saturating_duration_since(Instant::now());
            match messages.recv_timeout(left) {
                Ok(message) if message["id"] == json!(id) && message.get("method").is_none() => {
                    return message;
                }
                Ok(_) => {}
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    panic!("no answer to request {id} within {LIMIT:?}")
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    panic!("the daemon closed the connection before it answered request {id}")
                }
            }
        }
    }
}

fn frame(message: &Value) -> Vec<u8> {
    let body = message.to_string();
    format!("Content-Length: {}\r\n\r\n{body}", body.len()).into_bytes()
}

fn read_frame(stream: &mut impl BufRead) -> Option<Value> {
    let mut length = None;
    loop {
        let mut line = String::new();
        if stream.read_line(&mut line).ok()? == 0 {
            return None;
        }
        let line = line.trim_end();
        if line.is_empty() {
            break;
        }
        if let Some(value) = line.strip_prefix("Content-Length:") {
            length = value.trim().parse().ok();
        }
    }
    let mut body = vec![0; length?];
    stream.read_exact(&mut body).ok()?;
    serde_json::from_slice(&body).ok()
}

/// About 216 KB of source, with an outline answer larger than a socket buffer.
fn large_source() -> String {
    (0..2000)
        .map(|i| {
            format!(
                "DEFINE VARIABLE v{i} AS INTEGER NO-UNDO.\nv{i} = {i} + 1.\n\
                 PROCEDURE p{i}:\n    MESSAGE v{i}.\nEND PROCEDURE.\n"
            )
        })
        .collect()
}

/// One variable with 20,001 uses, so the highlights of it are about 2 MB.
fn many_uses() -> String {
    let mut text = String::from("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    for _ in 0..10_000 {
        text.push_str("x = x + 1.\n");
    }
    text
}

fn open(uri: &str, text: &str) -> Value {
    json!({
        "jsonrpc": "2.0", "method": "textDocument/didOpen",
        "params": {"textDocument": {"uri": uri, "languageId": "abl", "version": 1, "text": text}}
    })
}

fn request(id: u64, method: &str, params: Value) -> Value {
    json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params})
}

fn symbols(id: u64, uri: &str) -> Value {
    request(
        id,
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": uri}}),
    )
}

fn highlights(id: u64, uri: &str) -> Value {
    request(
        id,
        "textDocument/documentHighlight",
        json!({"textDocument": {"uri": uri}, "position": {"line": 0, "character": 16}}),
    )
}

/// A second client gets its answers, after the first one is gone.
fn assert_serves(daemon: &mut Daemon, after: &str) {
    daemon.assert_alive(after);
    let mut other = daemon.client();
    let uri = daemon.uri("other.p");
    other
        .write(&open(&uri, "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n"))
        .unwrap();
    other.write(&symbols(1, &uri)).unwrap();
    let answer = other.answer(1);
    assert!(
        answer["result"].is_array(),
        "another client must get its outline after {after}: {answer}"
    );
}

#[test]
fn a_client_that_writes_and_never_reads_is_disconnected() {
    let mut daemon = Daemon::start(Some(QUEUE_LIMIT));
    let stream = UnixStream::connect(&daemon.socket).unwrap();
    stream.set_write_timeout(Some(LIMIT)).unwrap();
    let mut client = Client {
        stream,
        input: None,
        messages: None,
    };
    client
        .write(&json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize",
            "params": {"processId": null, "rootUri": format!("file://{}", daemon.root().display()),
                "capabilities": {}}
        }))
        .unwrap();
    client
        .write(&json!({"jsonrpc": "2.0", "method": "initialized", "params": {}}))
        .unwrap();

    let text = large_source();
    let uri = daemon.uri("big.p");
    // The server does not read its queue while it handles this first `didOpen`, so
    // the frames that follow wait there, whatever the speed of the machine.
    let first = format!("/* OXABL-TEST-HOLD:lsp.did-open@{HOLD_MS} */\n{text}");
    let mut frames = vec![open(&uri, &first)];
    // Answers this client does not read, so the daemon waits to write them.
    frames.extend((1..=4).map(|id| symbols(id, &uri)));

    let started = Instant::now();
    let mut written = 0usize;
    let mut next = 0usize;
    let refused = loop {
        let message = match frames.get(next) {
            Some(message) => message.clone(),
            None => open(&daemon.uri(&format!("more{next}.p")), &text),
        };
        next += 1;
        match client.write(&message) {
            Ok(size) => written += size,
            Err(error) => break error,
        }
        assert!(
            written < WRITE_LIMIT && started.elapsed() < LIMIT,
            "the daemon still reads after {written} bytes in {:?}, with a high-water \
             mark of {QUEUE_LIMIT} bytes",
            started.elapsed()
        );
    };
    assert!(
        matches!(
            refused.kind(),
            std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::ConnectionReset
        ),
        "the daemon must close the connection, not stop reading it: {refused}"
    );
    assert!(
        written < 16 * QUEUE_LIMIT,
        "the daemon closed the connection only after {written} bytes"
    );
    assert_serves(&mut daemon, "it closed a connection");
    let stderr = daemon.stderr();
    assert!(
        stderr.contains("closing the connection") && stderr.contains("not handled"),
        "the daemon must say why it closed the connection; its stderr:\n{stderr}"
    );
}

/// The mark counts the frames that wait, not every frame the connection carried.
#[test]
fn a_client_that_reads_its_answers_stays_connected() {
    let mut daemon = Daemon::start(Some(QUEUE_LIMIT));
    let mut client = daemon.client();
    let text = large_source();
    let mut sent = 0usize;
    let mut id = 1;
    while sent < 4 * QUEUE_LIMIT {
        let uri = daemon.uri(&format!("big{id}.p"));
        sent += client.write(&open(&uri, &text)).unwrap();
        sent += client.write(&symbols(id, &uri)).unwrap();
        let answer = client.answer(id);
        assert!(
            answer["result"].is_array(),
            "the outline of file {id}, after {sent} bytes with a mark of {QUEUE_LIMIT}, \
             must be a result: {answer}"
        );
        id += 1;
    }
    daemon.assert_alive("a client that reads its answers");
}

/// A client that stops reading makes the daemon hold its answers. No other client
/// may wait for them: the daemon never waits on a writer of one client.
#[test]
fn a_client_that_stops_reading_does_not_stop_the_others() {
    let mut daemon = Daemon::start(None);
    let mut stuck = daemon.silent_client();
    let uri = daemon.uri("uses.p");
    // Each highlight worker waits before its analysis while it holds its database
    // snapshot, and the daemon says so on stderr.
    let text = format!(
        "{}/* OXABL-TEST-HOLD:lsp.highlights@{HOLD_MS} */\n",
        many_uses()
    );
    stuck.write(&open(&uri, &text)).unwrap();
    // The first answer is larger than the socket buffers, so the writer of this
    // client stops in it. The second request is sent when the first worker has
    // passed the check for a newer request, and its worker starts when the first
    // has sent its answer. So the first answer is with the writer when the second
    // worker waits.
    stuck.write(&highlights(1, &uri)).unwrap();
    daemon.wait_for_stderr("OXABL-TEST-HOLD started: lsp.highlights", 1);
    stuck.write(&highlights(2, &uri)).unwrap();
    daemon.wait_for_stderr("OXABL-TEST-HOLD started: lsp.highlights", 2);

    let mut other = daemon.client();
    let other_uri = daemon.uri("other.p");
    other
        .write(&open(&other_uri, "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n"))
        .unwrap();
    other
        .write(&json!({
            "jsonrpc": "2.0", "method": "textDocument/didChange",
            "params": {"textDocument": {"uri": other_uri, "version": 2},
                "contentChanges": [{"text": "DEFINE VARIABLE m AS INTEGER NO-UNDO.\n"}]}
        }))
        .unwrap();
    other.write(&symbols(1, &other_uri)).unwrap();
    let answer = other.answer(1);
    assert!(
        answer["result"].is_array(),
        "the other client must get its outline: {answer}"
    );
    daemon.assert_alive("a client stopped reading");
    drop(stuck);
}

/// The answers a client does not read have the same mark.
#[test]
fn a_client_that_does_not_read_its_answers_is_disconnected() {
    let mut daemon = Daemon::start(Some(QUEUE_LIMIT));
    let mut stuck = daemon.silent_client();
    let uri = daemon.uri("uses.p");
    let small = daemon.uri("small.p");
    stuck.write(&open(&uri, &many_uses())).unwrap();
    stuck
        .write(&open(&small, "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n"))
        .unwrap();
    // One answer larger than the mark, then small requests: each answer that comes
    // while the large one waits finds the queue past the mark.
    stuck.write(&highlights(1, &uri)).unwrap();
    let started = Instant::now();
    let mut id = 2;
    let refused = loop {
        if let Err(error) = stuck.write(&symbols(id, &small)) {
            break error;
        }
        assert!(
            started.elapsed() < LIMIT,
            "the daemon kept the connection of a client that read none of {id} answers, \
             one of them about 2 MB, with a mark of {QUEUE_LIMIT} bytes"
        );
        id += 1;
        thread::sleep(Duration::from_millis(100));
    };
    assert!(
        matches!(
            refused.kind(),
            std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::ConnectionReset
        ),
        "the daemon must close the connection: {refused}"
    );
    assert_serves(&mut daemon, "it closed a connection that did not read");
    let stderr = daemon.stderr();
    assert!(
        stderr.contains("closing the connection") && stderr.contains("not written"),
        "the daemon must say why it closed the connection; its stderr:\n{stderr}"
    );
}

/// A client that stops reading but keeps writing ends its own connection when the
/// daemon cannot write to it. The answers that the daemon still makes must not
/// collect in a queue that nobody reads.
#[test]
fn a_client_that_closes_its_read_side_is_disconnected() {
    let mut daemon = Daemon::start(None);
    let mut client = daemon.silent_client();
    client.stream.shutdown(std::net::Shutdown::Read).unwrap();
    let text = large_source();
    let uri = daemon.uri("big.p");
    let started = Instant::now();
    let mut written = 0usize;
    let mut id = 0;
    let refused = loop {
        // The first write is the file, then a request for each turn. The daemon may
        // close the connection at any of them.
        let message = if id == 0 {
            open(&uri, &text)
        } else {
            symbols(id, &uri)
        };
        match client.write(&message) {
            Ok(size) => written += size,
            Err(error) => break error,
        }
        id += 1;
        // A bound on the requests too: each one starts a worker, so a daemon that
        // never closes must not load the machine and fail the other tests.
        assert!(
            written < WRITE_LIMIT && id < 300 && started.elapsed() < LIMIT,
            "the daemon still reads the client that does not read after {id} requests in \
             {:?}",
            started.elapsed()
        );
        thread::sleep(Duration::from_millis(5));
    };
    assert!(
        matches!(
            refused.kind(),
            std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::ConnectionReset
        ),
        "the daemon must close the connection: {refused}"
    );
    assert_serves(&mut daemon, "a client closed its read side");
}

/// The names of the threads of `pid` whose name starts with one of `prefixes`.
#[cfg(target_os = "linux")]
fn threads_named(pid: u32, prefixes: &[&str]) -> Vec<String> {
    let Ok(tasks) = std::fs::read_dir(format!("/proc/{pid}/task")) else {
        return Vec::new();
    };
    tasks
        .filter_map(|task| std::fs::read_to_string(task.ok()?.path().join("comm")).ok())
        .map(|name| name.trim_end().to_string())
        .filter(|name| prefixes.iter().any(|prefix| name.starts_with(prefix)))
        .collect()
}

/// When the daemon cannot write to a client, the requests of that client that wait
/// for the server are dropped: the daemon does no work for a client that is gone.
#[cfg(target_os = "linux")]
#[test]
fn the_requests_of_a_gone_client_are_not_handled() {
    const HELD: &str = "OXABL-TEST-HOLD started: lsp.highlights";
    let mut daemon = Daemon::start(None);
    let mut client = daemon.silent_client();
    // Every file is on disk, so the highlights do not take the path that skips the
    // hold.
    let text = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nx = 1.\n\
                /* OXABL-TEST-HOLD:lsp.highlights@1000 */\n";
    let held: Vec<String> = (0..5).map(|i| format!("h{i}.p")).collect();
    for name in &held {
        std::fs::write(daemon.root().join(name), text).unwrap();
        client.write(&open(&daemon.uri(name), text)).unwrap();
    }
    let later = format!("/* OXABL-TEST-HOLD:lsp.did-open@{HOLD_MS} */\n");
    std::fs::write(daemon.root().join("later.p"), &later).unwrap();

    client.write(&highlights(1, &daemon.uri(&held[0]))).unwrap();
    daemon.wait_for_stderr(HELD, 1);
    // The answer of the held worker now fails to be written.
    client.stream.shutdown(std::net::Shutdown::Read).unwrap();
    // The server is held longer than the worker, so the requests that follow still
    // wait for it when that write fails.
    client.write(&open(&daemon.uri("later.p"), &later)).unwrap();
    for (id, name) in (2..).zip(&held[1..]) {
        client.write(&highlights(id, &daemon.uri(name))).unwrap();
    }

    let pid = daemon.child.id();
    let started = Instant::now();
    loop {
        let left = threads_named(pid, &["oxabl-conn", "oxabl-navig"]);
        if left.is_empty() {
            break;
        }
        assert!(
            started.elapsed() < LIMIT,
            "the threads of the gone client did not end in {LIMIT:?}: {left:?}"
        );
        thread::sleep(Duration::from_millis(20));
    }
    assert_eq!(
        daemon.stderr().matches(HELD).count(),
        1,
        "the daemon answered requests of a gone client; its stderr:\n{}",
        daemon.stderr()
    );
    assert_serves(&mut daemon, "a client went away with requests that wait");
}
