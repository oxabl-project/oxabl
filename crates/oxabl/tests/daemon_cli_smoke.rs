//! End-to-end smoke test for the launchable `oxabl daemon` artifact (U7).

#![cfg(unix)]

use std::io::{BufRead, BufReader, Read, Write};
use std::os::unix::net::UnixStream;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use oxabl_daemon_protocol::{
    CONTRACT_VERSION, ClientKind, HandshakeRequest, HandshakeResponse, Registration, method,
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
    let registration_path = registration_path_in(root.path(), cache.path());

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

#[test]
fn the_editor_joins_the_existing_daemon_and_leaves_desktop_connected() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());

    let daemon = Command::new(OXABL_BIN)
        .arg("daemon")
        .arg(root.path())
        .env("XDG_CACHE_HOME", cache.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("launch the daemon");
    let daemon = ChildGuard(daemon);
    let registration = wait_for_registration(&registration_path);

    let mut desktop = UnixStream::connect(&registration.socket_path).expect("connect desktop");
    desktop
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("bound desktop reads");
    send_handshake(&mut desktop, 1, root.path());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut desktop)).expect("desktop handshake response");
    assert_eq!(response["result"]["connected_clients"], 1);

    let mut editor = Command::new(OXABL_BIN)
        .arg("lsp")
        .env("XDG_CACHE_HOME", cache.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("launch the editor shim");
    let mut editor_in = editor.stdin.take().expect("editor stdin");
    let mut editor_out = editor.stdout.take().expect("editor stdout");
    let initialize = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 10,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": format!("file://{}", root.path().display()),
            "capabilities": {}
        }
    });
    write_frame(&mut editor_in, &initialize.to_string());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut editor_out)).expect("LSP initialize response");
    assert_eq!(response["id"], 10);
    let initialized = serde_json::json!({"jsonrpc": "2.0", "method": "initialized", "params": {}});
    write_frame(&mut editor_in, &initialized.to_string());
    let registration_request: serde_json::Value =
        serde_json::from_str(&read_frame(&mut editor_out))
            .expect("dynamic file-watcher registration request");
    let registration_response = serde_json::json!({
        "jsonrpc": "2.0",
        "id": registration_request["id"],
        "result": null
    });
    write_frame(&mut editor_in, &registration_response.to_string());

    let mut observer = UnixStream::connect(&registration.socket_path).expect("connect observer");
    observer
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("bound observer reads");
    send_handshake(&mut observer, 2, root.path());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut observer)).expect("observer handshake response");
    assert_eq!(response["result"]["connected_clients"], 3);
    assert_eq!(
        registration.pid,
        daemon.0.id(),
        "no second daemon was started"
    );
    drop(observer);
    assert!(editor.try_wait().expect("check editor shim").is_none());

    let shutdown = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 11,
        "method": "shutdown",
        "params": null
    });
    write_frame(&mut editor_in, &shutdown.to_string());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut editor_out)).expect("LSP shutdown response");
    assert_eq!(response["id"], 11);
    let exit = serde_json::json!({"jsonrpc": "2.0", "method": "exit", "params": null});
    write_frame(&mut editor_in, &exit.to_string());
    drop(editor_in);
    assert!(editor.wait().expect("wait for editor shim").success());

    let freshness = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": method::FRESHNESS,
        "params": {}
    });
    write_frame(&mut desktop, &freshness.to_string());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut desktop)).expect("freshness response");
    assert_eq!(
        response["id"], 3,
        "desktop remains served after editor exit"
    );
}

#[test]
fn a_daemon_exit_mid_session_is_reported_by_the_editor_shim() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());

    let mut daemon = Command::new(OXABL_BIN)
        .arg("daemon")
        .arg(root.path())
        .env("XDG_CACHE_HOME", cache.path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("launch the daemon");
    let _registration = wait_for_registration(&registration_path);

    let mut editor = Command::new(OXABL_BIN)
        .arg("lsp")
        .env("XDG_CACHE_HOME", cache.path())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("launch the editor shim");
    let mut editor_in = editor.stdin.take().expect("editor stdin");
    let mut editor_out = editor.stdout.take().expect("editor stdout");
    let initialize = serde_json::json!({
        "jsonrpc": "2.0",
        "id": 20,
        "method": "initialize",
        "params": {
            "processId": null,
            "rootUri": format!("file://{}", root.path().display()),
            "capabilities": {}
        }
    });
    write_frame(&mut editor_in, &initialize.to_string());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut editor_out)).expect("LSP initialize response");
    assert_eq!(response["id"], 20);

    daemon.kill().expect("stop the daemon mid-session");
    daemon.wait().expect("reap the daemon");
    let deadline = Instant::now() + Duration::from_secs(10);
    let status = loop {
        if let Some(status) = editor.try_wait().expect("poll the editor shim") {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "editor shim did not report daemon exit"
        );
        std::thread::sleep(Duration::from_millis(10));
    };
    let mut stderr = String::new();
    editor
        .stderr
        .take()
        .expect("editor stderr")
        .read_to_string(&mut stderr)
        .expect("read editor stderr");
    assert!(!status.success());
    assert!(
        stderr.contains("daemon exited during the editor session"),
        "the failure names the daemon exit: {stderr}"
    );
}

fn wait_for_registration(path: &std::path::Path) -> Registration {
    let deadline = Instant::now() + Duration::from_secs(10);
    while !path.exists() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    serde_json::from_slice(&std::fs::read(path).expect("the daemon writes its registration"))
        .expect("the registration is valid JSON")
}

/// The registration path a daemon under `cache` writes for `root`.
///
/// Calls the protocol crate's own naming rule with the directory supplied, so this
/// test cannot drift from it. It used to reach the same rule by setting
/// `XDG_CACHE_HOME` around the call, which needed a lock and two `unsafe` blocks to
/// keep one process-wide variable from racing every other test in the binary.
fn registration_path_in(root: &std::path::Path, cache: &std::path::Path) -> std::path::PathBuf {
    oxabl_daemon_protocol::registration_path_in(&cache.join("oxabl").join("daemon"), root)
}

fn send_handshake(stream: &mut UnixStream, id: u32, root: &std::path::Path) {
    let request = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": method::HANDSHAKE,
        "params": HandshakeRequest::new(ClientKind::Desktop, root.to_string_lossy()),
    });
    write_frame(stream, &request.to_string());
}

fn write_frame(stream: &mut impl Write, body: &str) {
    write!(stream, "Content-Length: {}\r\n\r\n{body}", body.len()).expect("write request");
    stream.flush().expect("flush request");
}

fn read_frame(stream: &mut impl Read) -> String {
    let mut reader = BufReader::new(stream);
    let mut content_length = None;
    loop {
        let mut line = String::new();
        let read = reader.read_line(&mut line).expect("read response header");
        assert!(read > 0, "connection closed before a complete response");
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
