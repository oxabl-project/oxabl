//! End-to-end smoke test for the launchable `oxabl daemon` artifact (U7).

#![cfg(unix)]

use std::io::{BufRead, BufReader, Read, Write};
use std::os::unix::net::UnixStream;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{
    CONTRACT_VERSION, ClientKind, HandshakeRequest, HandshakeResponse, Registration, method,
    registration_path,
};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

struct ChildGuard(Child);

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

#[test]
fn a_client_with_no_daemon_launches_the_artifact_and_connects() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let previous = std::env::var_os("XDG_CACHE_HOME");
    // SAFETY: this integration test binary contains one test and starts no helper
    // thread until after it restores the process environment.
    unsafe { std::env::set_var("XDG_CACHE_HOME", cache.path()) };
    let registration_path = registration_path(root.path());
    unsafe {
        match previous {
            Some(value) => std::env::set_var("XDG_CACHE_HOME", value),
            None => std::env::remove_var("XDG_CACHE_HOME"),
        }
    }

    assert!(
        !registration_path.exists(),
        "the test starts without a daemon"
    );
    let child = Command::new(OXABL_BIN)
        .arg("daemon")
        .arg(root.path())
        .env("XDG_CACHE_HOME", cache.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("launch `oxabl daemon`");
    let _child = ChildGuard(child);

    let deadline = Instant::now() + Duration::from_secs(10);
    while !registration_path.exists() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    let registration: Registration = serde_json::from_slice(
        &std::fs::read(&registration_path).expect("the daemon writes its registration"),
    )
    .expect("the registration is valid JSON");

    let mut stream = UnixStream::connect(&registration.socket_path)
        .expect("connect to the launched daemon's socket");
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("set a bounded read");
    let request = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method::HANDSHAKE,
        "params": HandshakeRequest::new(ClientKind::Desktop, root.path().to_string_lossy()),
    });
    write_frame(&mut stream, &request.to_string());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut stream)).expect("a JSON-RPC response");
    let handshake: HandshakeResponse =
        serde_json::from_value(response["result"].clone()).expect("a handshake response");

    assert_eq!(handshake.contract_version, CONTRACT_VERSION);
    assert_eq!(handshake.connected_clients, 1);
    assert_eq!(handshake.workspace_root, root.path().to_string_lossy());
}

fn write_frame(stream: &mut UnixStream, body: &str) {
    write!(stream, "Content-Length: {}\r\n\r\n{body}", body.len()).expect("write request");
    stream.flush().expect("flush request");
}

fn read_frame(stream: &mut UnixStream) -> String {
    let mut reader = BufReader::new(stream);
    let mut content_length = None;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).expect("read response header");
        if line == "\r\n" {
            break;
        }
        if let Some(value) = line.strip_prefix("Content-Length:") {
            content_length = Some(value.trim().parse::<usize>().expect("content length"));
        }
    }
    let mut body = vec![0; content_length.expect("a Content-Length header")];
    reader.read_exact(&mut body).expect("read response body");
    String::from_utf8(body).expect("a UTF-8 response")
}
