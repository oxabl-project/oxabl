//! End-to-end smoke test for the `oxabl lsp` stdio transport.
//!
//! Unlike the `oxabl_lsp` crate's e2e tests (which drive the server over an
//! in-process `Connection::memory()`), this test spawns the *real* built
//! binary and speaks LSP over OS pipes. That is the only layer that catches
//! argv / transport regressions — e.g. an editor client appending a stray
//! `--stdio` flag that clap would reject, crashing the server on every start.
//!
//! It is deliberately dependency-light: the LSP message is framed and parsed
//! by hand (`Content-Length` header + JSON body) with only `serde_json`, which
//! the `oxabl` crate already depends on. No LSP client crate is pulled in.

use std::io::{Read, Write};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

/// Path to the built `oxabl` binary, provided by Cargo to integration tests.
const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// How long we wait for the server to answer `initialize` before failing
/// (rather than hanging CI forever).
const READ_TIMEOUT: Duration = Duration::from_secs(10);

/// Build an LSP-framed message: `Content-Length: N\r\n\r\n{json}`.
fn frame(body: &str) -> Vec<u8> {
    format!("Content-Length: {}\r\n\r\n{}", body.len(), body).into_bytes()
}

#[test]
fn lsp_stdio_initialize_handshake_returns_capabilities() {
    // Exactly the argv the VS Code extension uses: a single `lsp` arg, nothing
    // else. No `--stdio`, no other flags.
    let mut child = Command::new(OXABL_BIN)
        .arg("lsp")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to spawn `oxabl lsp`");

    let mut stdin = child.stdin.take().expect("child stdin");
    let mut stdout = child.stdout.take().expect("child stdout");

    // Minimal, valid `initialize` request (jsonrpc 2.0, id 0).
    let init = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 0,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": null,
            "capabilities": {}
        }
    });
    stdin
        .write_all(&frame(&init.to_string()))
        .expect("write initialize request");
    stdin.flush().expect("flush initialize request");

    // Read the framed response on a worker thread so a stuck server can never
    // hang CI: we bound the wait with `recv_timeout`.
    let (tx, rx) = mpsc::channel();
    let reader = thread::spawn(move || {
        let mut buf = Vec::new();
        // Read until we have parsed at least one complete LSP message, or EOF.
        let mut chunk = [0u8; 4096];
        loop {
            match stdout.read(&mut chunk) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    buf.extend_from_slice(&chunk[..n]);
                    if let Some(body) = try_parse_one_message(&buf) {
                        let _ = tx.send(Some(body));
                        return;
                    }
                }
                Err(_) => break,
            }
        }
        let _ = tx.send(None);
    });

    let received = rx
        .recv_timeout(READ_TIMEOUT)
        .expect("timed out waiting for the server's initialize response");
    let _ = child.kill();
    let _ = child.wait();
    let _ = reader.join();

    let body = received.expect("server closed stdout before answering initialize");
    let msg: serde_json::Value =
        serde_json::from_str(&body).expect("response body is not valid JSON");

    // A JSON-RPC response to id 0 carrying the server capabilities.
    assert_eq!(msg["id"], serde_json::json!(0), "response id should echo 0");
    assert!(
        msg["result"]["capabilities"].is_object(),
        "initialize result must contain a capabilities object; got: {msg}"
    );
}

#[test]
fn lsp_rejects_stray_stdio_flag() {
    // Guards the exact regression: the language-client library appended a
    // `--stdio` transport arg, which clap rejected — crashing the server on
    // every start. Spawning `oxabl lsp --stdio` must fail to start.
    let output = Command::new(OXABL_BIN)
        .args(["lsp", "--stdio"])
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("failed to spawn `oxabl lsp --stdio`");

    assert!(
        !output.status.success(),
        "`oxabl lsp --stdio` must exit non-zero (clap rejects the unknown arg), \
         but it exited successfully"
    );
}

/// Try to parse one complete LSP message out of `buf`. Returns the JSON body
/// as a string once a full `Content-Length`-framed message is present.
fn try_parse_one_message(buf: &[u8]) -> Option<String> {
    // Find the header/body separator.
    let sep = b"\r\n\r\n";
    let header_end = buf.windows(sep.len()).position(|w| w == sep)?;
    let header = std::str::from_utf8(&buf[..header_end]).ok()?;

    let mut content_length = None;
    for line in header.split("\r\n") {
        if let Some(rest) = line
            .strip_prefix("Content-Length:")
            .or_else(|| line.strip_prefix("content-length:"))
        {
            content_length = rest.trim().parse::<usize>().ok();
        }
    }
    let len = content_length?;
    let body_start = header_end + sep.len();
    let body_end = body_start + len;
    if buf.len() < body_end {
        return None; // body not fully arrived yet
    }
    String::from_utf8(buf[body_start..body_end].to_vec()).ok()
}
