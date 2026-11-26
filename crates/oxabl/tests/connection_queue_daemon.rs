//! What bounds the frames a connection reads and has not handled yet.
//!
//! The reader of a connection never waits for its consumer, so a peer that writes
//! and does not read the answers would fill that queue without limit. When the queue
//! passes its high-water mark, the reader closes the connection. A test build reads
//! a low mark from `OXABL_TEST_QUEUE_LIMIT`, so the test needs only a few MB.

#![cfg(unix)]

use std::io::Write;
use std::os::unix::net::UnixStream;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{Registration, canonical_root, registration_path_in};
use serde_json::{Value, json};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// The high-water mark the daemon gets in this test.
const QUEUE_LIMIT: usize = 1 << 20;

/// What the client writes at most. A daemon that still reads after this has no
/// bound.
const WRITE_LIMIT: usize = 64 << 20;

/// The time for the daemon to close the connection.
const LIMIT: Duration = Duration::from_secs(60);

struct ChildGuard(Child);

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
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

fn frame(message: &Value) -> Vec<u8> {
    let body = message.to_string();
    format!("Content-Length: {}\r\n\r\n{body}", body.len()).into_bytes()
}

fn open(uri: &str, text: &str) -> Value {
    json!({
        "jsonrpc": "2.0", "method": "textDocument/didOpen",
        "params": {"textDocument": {"uri": uri, "languageId": "abl", "version": 1, "text": text}}
    })
}

#[test]
fn a_client_that_writes_and_never_reads_is_disconnected() {
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
    let daemon = Command::new(OXABL_BIN)
        .arg("daemon")
        .arg(root.path())
        .env("XDG_RUNTIME_DIR", base.path())
        .env("XDG_CACHE_HOME", base.path())
        .env("HOME", base.path())
        .env("OXABL_TEST_QUEUE_LIMIT", QUEUE_LIMIT.to_string())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let _daemon = ChildGuard(daemon);

    let started = Instant::now();
    while !registration.exists() && started.elapsed() < LIMIT {
        std::thread::sleep(Duration::from_millis(10));
    }
    let registration: Registration =
        serde_json::from_slice(&std::fs::read(&registration).expect("the daemon registers"))
            .unwrap();
    let mut stream = UnixStream::connect(&registration.socket_path).unwrap();
    stream.set_write_timeout(Some(LIMIT)).unwrap();

    let root_uri = format!("file://{}", root.path().display());
    let text = large_source();
    let mut frames = vec![
        frame(&json!({
            "jsonrpc": "2.0", "id": 0, "method": "initialize",
            "params": {"processId": null, "rootUri": root_uri, "capabilities": {
                "textDocument": {"documentSymbol": {"hierarchicalDocumentSymbolSupport": true}}
            }}
        })),
        frame(&json!({"jsonrpc": "2.0", "method": "initialized", "params": {}})),
    ];
    let uri = format!("{root_uri}/big.p");
    frames.push(frame(&open(&uri, &text)));
    // Answers this client does not read, so the daemon waits to write them.
    for id in 1..=4 {
        frames.push(frame(&json!({
            "jsonrpc": "2.0", "id": id, "method": "textDocument/documentSymbol",
            "params": {"textDocument": {"uri": uri}}
        })));
    }

    let deadline = Instant::now() + LIMIT;
    let mut written = 0usize;
    let mut next = 0usize;
    let refused = loop {
        let frame = match frames.get(next) {
            Some(frame) => frame.clone(),
            None => frame(&open(&format!("{root_uri}/more{next}.p"), &text)),
        };
        next += 1;
        match stream.write_all(&frame) {
            Ok(()) => written += frame.len(),
            Err(error) => break error,
        }
        assert!(
            written < WRITE_LIMIT && Instant::now() < deadline,
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
}
