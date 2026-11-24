//! End-to-end smoke test for the launchable `oxabl daemon` artifact (U7).

#![cfg(unix)]

use std::io::{BufRead, BufReader, Read, Write};
use std::os::unix::net::UnixStream;
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
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

/// Every base-directory variable, pointed at one temporary directory.
///
/// A spawned daemon inherits this process's environment, so overriding only
/// `XDG_CACHE_HOME` would leave it registering under the developer's real
/// `XDG_RUNTIME_DIR` — which now wins — and these tests would spawn daemons that
/// register outside the sandbox they think they are in.
fn isolated_base(base: &std::path::Path) -> [(&'static str, &std::path::Path); 3] {
    [
        ("XDG_RUNTIME_DIR", base),
        ("XDG_CACHE_HOME", base),
        ("HOME", base),
    ]
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
        .envs(isolated_base(cache.path()))
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
        .envs(isolated_base(cache.path()))
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
        .envs(isolated_base(cache.path()))
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

/// Two daemon *processes* racing for one root: one serves it and the other exits with
/// a message that names the state it actually found.
///
/// Across processes, so the lock is doing the work the kernel does for it rather than
/// something a single process could have arranged. The loser must not claim the root
/// is free, and it must not claim a daemon serves it when the winner is still binding
/// — either sentence is fine, a wrong one is not.
#[test]
fn two_daemon_processes_racing_for_one_root_leave_one_serving() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());

    let launch = || {
        Command::new(OXABL_BIN)
            .arg("daemon")
            .arg(root.path())
            .envs(isolated_base(cache.path()))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("launch a daemon")
    };
    let first = ChildGuard(launch());
    let second = ChildGuard(launch());

    // Exactly one of the two is still running once the dust settles, and the other
    // has failed with a refusal.
    let deadline = Instant::now() + Duration::from_secs(10);
    let mut candidates = [first, second];
    let loser = loop {
        let exited = candidates
            .iter_mut()
            .position(|child| child.0.try_wait().expect("poll a daemon").is_some());
        if let Some(index) = exited {
            break index;
        }
        assert!(
            Instant::now() < deadline,
            "one of two daemons racing for one root must have been refused"
        );
        std::thread::sleep(Duration::from_millis(20));
    };

    let status = candidates[loser].0.wait().expect("reap the loser");
    assert!(!status.success(), "the refused daemon must fail");
    let mut stderr = String::new();
    candidates[loser]
        .0
        .stderr
        .take()
        .expect("the loser's stderr")
        .read_to_string(&mut stderr)
        .expect("read the loser's stderr");
    assert!(
        stderr.contains("already serves")
            || stderr.contains("is starting")
            || stderr.contains("start lock"),
        "the refusal must name the state it found: {stderr}"
    );

    let winner = 1 - loser;
    assert!(
        candidates[winner]
            .0
            .try_wait()
            .expect("poll the winner")
            .is_none(),
        "the winner must still be serving"
    );
    let registration = wait_for_registration(&registration_path);
    assert_eq!(
        registration.pid,
        candidates[winner].0.id(),
        "the registration must belong to the daemon that is still running"
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
        .envs(isolated_base(cache.path()))
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("launch the daemon");
    let _registration = wait_for_registration(&registration_path);

    let mut editor = Command::new(OXABL_BIN)
        .arg("lsp")
        .envs(isolated_base(cache.path()))
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

/// An upgrade leaves the old daemon running: the editor's new language server must
/// stop it and serve the root from a daemon of its own build. The old daemon read
/// `oxabl.toml` with its own rules, so joining it rejected settings the new build
/// knows.
#[test]
fn an_editor_replaces_a_daemon_of_an_older_version() {
    editor_replaces_an_older_daemon(|registration| {
        registration["daemon_version"] = serde_json::json!("0.0.1");
    });
}

/// The same for a daemon older than the version in the registration: its
/// registration has no `daemon_version`, and it speaks contract 4.
#[test]
fn an_editor_replaces_a_daemon_older_than_the_build_field() {
    editor_replaces_an_older_daemon(|registration| {
        let fields = registration.as_object_mut().expect("a registration object");
        fields.remove("daemon_version");
        fields.insert("contract_version".to_string(), serde_json::json!(4));
    });
}

/// Start a real daemon, rewrite its registration the way an older build wrote it,
/// then start `oxabl lsp` on the root as an editor does after an upgrade. The old
/// daemon stays unreaped until the editor is answered, as under a parent that does
/// not wait for it.
fn editor_replaces_an_older_daemon(as_older_build: impl FnOnce(&mut serde_json::Value)) {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());
    let mut old = ChildGuard(launch_daemon(root.path(), cache.path()));
    let old_pid = old.0.id();
    assert_eq!(wait_for_registration(&registration_path).pid, old_pid);
    rewrite_registration(&registration_path, as_older_build);

    let editor = Editor::start(root.path(), cache.path());
    let response = editor.initialize_response(&registration_path, old_pid);
    assert_eq!(response["id"], 10);

    let replacement = wait_for_registration(&registration_path);
    let _replacement = (replacement.pid != old_pid).then(|| PidGuard(replacement.pid));
    assert!(
        exits_within(&mut old.0, Duration::from_secs(5)).is_some(),
        "the daemon of the older build must be stopped"
    );
    assert_ne!(
        replacement.pid, old_pid,
        "a daemon of this build serves the root"
    );
    assert_eq!(replacement.contract_version, CONTRACT_VERSION);
    assert_eq!(
        replacement.daemon_version,
        env!("CARGO_PKG_VERSION"),
        "the replacement names the binary's version"
    );
}

/// A newer daemon is left alone: replacing it would let two builds that both stay in
/// use stop each other's daemon on every restart of their editors.
#[test]
fn an_editor_leaves_a_newer_daemon_alone() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());
    let mut newer = ChildGuard(launch_daemon(root.path(), cache.path()));
    let newer_pid = newer.0.id();
    wait_for_registration(&registration_path);
    rewrite_registration(&registration_path, |registration| {
        registration["daemon_version"] = serde_json::json!("999.0.0");
    });

    let mut editor = Editor::start(root.path(), cache.path());
    let exited = exits_within(&mut editor.child.0, Duration::from_secs(15));
    let registered = wait_for_registration(&registration_path);
    let _started = (registered.pid != newer_pid).then(|| PidGuard(registered.pid));
    let status = exited.expect("the editor shim must give up");
    let stderr = editor.stderr(Duration::from_secs(5));
    assert!(!status.success());
    assert!(
        stderr.contains("999.0.0") && stderr.contains("not older"),
        "the refusal names the newer build: {stderr}"
    );
    assert!(
        newer.0.try_wait().expect("poll the daemon").is_none(),
        "a newer daemon must not be stopped"
    );
}

/// A pid read from a registration is not proof of the daemon: here it belongs to an
/// unrelated process, and another process listens on the socket. Nothing may signal
/// the unrelated process.
#[test]
fn a_registered_pid_that_does_not_listen_is_never_signalled() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let (registration_path, socket) = registration_left_behind(root.path(), cache.path());
    let _listener = StandIn::start(&socket, StandInMode::Listen);
    let mut bystander = ChildGuard(
        Command::new("sleep")
            .arg("300")
            .spawn()
            .expect("start an unrelated process"),
    );
    let bystander_pid = bystander.0.id();
    rewrite_registration(&registration_path, |registration| {
        registration["pid"] = serde_json::json!(bystander_pid);
        registration["daemon_version"] = serde_json::json!("0.0.1");
    });

    let mut editor = Editor::start(root.path(), cache.path());
    exits_within(&mut editor.child.0, Duration::from_secs(15))
        .expect("the editor shim must give up on a socket another process holds");
    assert!(
        bystander
            .0
            .try_wait()
            .expect("poll the bystander")
            .is_none(),
        "the process that reused the registered pid must not be signalled"
    );
}

/// A daemon of an older build that ignores `SIGTERM` is killed after the grace period,
/// and the editor is served by a daemon of its own build.
#[test]
fn an_older_daemon_that_ignores_sigterm_is_killed() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let (registration_path, socket) = registration_left_behind(root.path(), cache.path());
    let mut stubborn = StandIn::start(&socket, StandInMode::IgnoreTerm);
    let stubborn_pid = stubborn.0.0.id();
    rewrite_registration(&registration_path, |registration| {
        registration["pid"] = serde_json::json!(stubborn_pid);
        registration["daemon_version"] = serde_json::json!("0.0.1");
    });

    let editor = Editor::start(root.path(), cache.path());
    let response = editor.initialize_response(&registration_path, stubborn_pid);
    assert_eq!(response["id"], 10);
    let replacement = wait_for_registration(&registration_path);
    let _replacement = (replacement.pid != stubborn_pid).then(|| PidGuard(replacement.pid));
    let status = exits_within(&mut stubborn.0.0, Duration::from_secs(5))
        .expect("the stubborn daemon must be stopped");
    use std::os::unix::process::ExitStatusExt;
    assert_eq!(status.signal(), Some(9), "it ends by SIGKILL");
}

/// A daemon of an older build too busy to take a connection: the kernel cannot name
/// its listener, so the registered pid, here an unrelated process, is not signalled
/// and the editor says why.
#[test]
fn a_busy_socket_never_lends_its_registration_a_signal() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let (registration_path, socket) = registration_left_behind(root.path(), cache.path());
    let _listener = StandIn::start(&socket, StandInMode::Busy);
    let mut bystander = ChildGuard(
        Command::new("sleep")
            .arg("300")
            .spawn()
            .expect("start an unrelated process"),
    );
    let bystander_pid = bystander.0.id();
    rewrite_registration(&registration_path, |registration| {
        registration["pid"] = serde_json::json!(bystander_pid);
        registration["daemon_version"] = serde_json::json!("0.0.1");
    });

    let mut editor = Editor::start(root.path(), cache.path());
    exits_within(&mut editor.child.0, Duration::from_secs(15))
        .expect("the editor shim must give up on a busy socket");
    let stderr = editor.stderr(Duration::from_secs(5));
    assert!(
        bystander
            .0
            .try_wait()
            .expect("poll the bystander")
            .is_none(),
        "a pid the kernel did not name as the listener must not be signalled"
    );
    assert!(
        stderr.contains("could not be stopped")
            && stderr.contains(&format!("kill {bystander_pid}")),
        "the editor names the daemon it could not stop: {stderr}"
    );
}

/// A daemon exits once it has had no client for its idle timeout, and removes its
/// registration, its socket and its lock file. A connected client keeps it, however
/// long the client stays.
#[test]
fn an_idle_daemon_exits_and_removes_its_registration() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());
    let mut daemon = ChildGuard(
        Command::new(OXABL_BIN)
            .arg("daemon")
            .arg(root.path())
            .args(["--idle-timeout", "2s"])
            .envs(isolated_base(cache.path()))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("launch the daemon"),
    );
    let registration = wait_for_registration(&registration_path);
    let mut client = UnixStream::connect(&registration.socket_path).expect("connect a client");
    client
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("bound the reads");
    send_handshake(&mut client, 1, root.path());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut client)).expect("a handshake response");
    assert_eq!(response["result"]["connected_clients"], 1);

    assert!(
        exits_within(&mut daemon.0, Duration::from_secs(4)).is_none(),
        "a daemon with a connected client is not idle"
    );
    let mut second = UnixStream::connect(&registration.socket_path).expect("connect again");
    second
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("bound the reads");
    send_handshake(&mut second, 2, root.path());
    let response: serde_json::Value =
        serde_json::from_str(&read_frame(&mut second)).expect("a second handshake response");
    assert_eq!(
        response["result"]["connected_clients"], 2,
        "a daemon with a connected client still takes new clients"
    );
    drop(second);
    drop(client);

    let status = exits_within(&mut daemon.0, Duration::from_secs(15))
        .expect("the daemon exits after its idle timeout");
    assert!(status.success(), "an idle exit is a clean exit: {status}");
    for artifact in registration_artifacts(&registration_path) {
        assert!(
            !artifact.exists(),
            "the idle daemon left {} behind",
            artifact.display()
        );
    }
}

/// The daemon an editor starts inherits the idle timeout from the environment, and
/// exits after the editor has gone.
#[test]
fn a_daemon_started_by_an_editor_exits_after_the_editor_leaves() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());

    let editor = Editor::start_with(
        root.path(),
        cache.path(),
        &[("OXABL_DAEMON_IDLE_TIMEOUT", "1s")],
    );
    let response = editor.answer();
    assert_eq!(response["id"], 10);
    let registration = wait_for_registration(&registration_path);
    let _daemon = PidGuard(registration.pid);

    std::thread::sleep(Duration::from_secs(2));
    assert!(
        registration_path.exists() && process_exists(registration.pid),
        "the daemon serves its editor past the idle timeout"
    );

    editor.leave();
    let deadline = Instant::now() + Duration::from_secs(15);
    while process_exists(registration.pid) || registration_path.exists() {
        assert!(
            Instant::now() < deadline,
            "the daemon must exit after its editor left"
        );
        std::thread::sleep(Duration::from_millis(20));
    }
    for artifact in registration_artifacts(&registration_path) {
        assert!(
            !artifact.exists(),
            "the idle daemon left {} behind",
            artifact.display()
        );
    }
}

/// An idle timeout that does not parse is not replaced by the default: the daemon
/// refuses to start and names the variable.
#[test]
fn an_idle_timeout_that_does_not_parse_refuses_the_start() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let mut daemon = ChildGuard(
        Command::new(OXABL_BIN)
            .arg("daemon")
            .arg(root.path())
            .envs(isolated_base(cache.path()))
            .env("OXABL_DAEMON_IDLE_TIMEOUT", "soon")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .expect("run the daemon"),
    );
    let status = exits_within(&mut daemon.0, Duration::from_secs(10))
        .expect("a daemon with an idle timeout that does not parse must not start");
    let mut stderr = String::new();
    daemon
        .0
        .stderr
        .take()
        .expect("the daemon's stderr")
        .read_to_string(&mut stderr)
        .expect("read the daemon's stderr");
    assert!(!status.success());
    assert!(
        stderr.contains("OXABL_DAEMON_IDLE_TIMEOUT") && stderr.contains("`soon`"),
        "the refusal names the variable and its value: {stderr}"
    );
    assert!(
        !registration_path_in(root.path(), cache.path()).exists(),
        "a daemon that refused to start registers nothing"
    );
}

/// A daemon that decided to exit closes a client's connection before it answers. The
/// editor reads that as "no daemon here", starts one and is served by it.
#[test]
fn an_editor_refused_by_an_exiting_daemon_starts_another() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let (registration_path, socket) = registration_left_behind(root.path(), cache.path());
    let exiting = StandIn::start(&socket, StandInMode::RefuseAndExit);
    let exiting_pid = exiting.0.0.id();
    rewrite_registration(&registration_path, |registration| {
        registration["pid"] = serde_json::json!(exiting_pid);
    });

    let editor = Editor::start(root.path(), cache.path());
    let response = editor.initialize_response(&registration_path, exiting_pid);
    assert_eq!(response["id"], 10);
    let replacement = wait_for_registration(&registration_path);
    let _replacement = PidGuard(replacement.pid);
    assert_ne!(
        replacement.pid, exiting_pid,
        "a daemon the editor started serves the root"
    );
}

/// A daemon an editor starts while another daemon of the root is still exiting loses
/// the start lock and exits. The editor starts another once the lock is free.
#[test]
fn an_editor_starts_another_daemon_when_its_first_loses_the_start_lock() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let registration_path = registration_path_in(root.path(), cache.path());
    let lock_path = registration_path.with_extension("lock");
    let directory = lock_path.parent().expect("a registration directory");
    std::fs::create_dir_all(directory).expect("the registration directory");
    for level in [directory.parent().expect("the oxabl directory"), directory] {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(level, std::fs::Permissions::from_mode(0o700))
            .expect("a private directory");
    }
    // An exiting daemon holds the lock for a moment after its registration is gone.
    let mut holder = StandIn::start(&lock_path, StandInMode::HoldLock);

    let editor = Editor::start(root.path(), cache.path());
    let response = editor.initialize_response(&registration_path, 0);
    assert_eq!(response["id"], 10);
    let registration = wait_for_registration(&registration_path);
    let _daemon = PidGuard(registration.pid);
    assert!(
        exits_within(&mut holder.0.0, Duration::from_secs(5)).is_some(),
        "the lock was held while the editor started"
    );
}

/// A daemon that fails to start for a reason that repeats is started once, and the
/// editor is told at once rather than after five starts.
#[test]
fn an_editor_starts_a_daemon_that_cannot_start_only_once() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let mut editor = Editor::start_with(
        root.path(),
        cache.path(),
        &[("OXABL_DAEMON_IDLE_TIMEOUT", "soon")],
    );
    let status = exits_within(&mut editor.child.0, Duration::from_secs(10))
        .expect("the editor shim gives up");
    let stderr = editor.stderr(Duration::from_secs(5));
    assert!(!status.success());
    assert_eq!(
        stderr.matches("is not an idle timeout").count(),
        1,
        "one daemon was started, and its error is shown: {stderr}"
    );
    assert!(
        stderr.contains("exited") && !stderr.contains("did not become ready"),
        "the editor names the exit, not a timeout: {stderr}"
    );
}

/// A daemon that closes the editor's connection and stays is not exiting. The editor
/// says so at once instead of asking it again.
#[test]
fn an_editor_refused_by_a_daemon_that_stays_fails_at_once() {
    let root = tempfile::tempdir().expect("a workspace root");
    let cache = tempfile::tempdir().expect("a cache root");
    let (registration_path, socket) = registration_left_behind(root.path(), cache.path());
    let refusing = StandIn::start(&socket, StandInMode::Refuse);
    let refusing_pid = refusing.0.0.id();
    rewrite_registration(&registration_path, |registration| {
        registration["pid"] = serde_json::json!(refusing_pid);
    });

    let mut editor = Editor::start(root.path(), cache.path());
    let status = exits_within(&mut editor.child.0, Duration::from_secs(15))
        .expect("the editor shim gives up");
    let stderr = editor.stderr(Duration::from_secs(5));
    assert!(!status.success());
    assert!(
        stderr.contains("not exiting") && stderr.contains(&format!("kill {refusing_pid}")),
        "the editor names the daemon that stays: {stderr}"
    );
    assert!(
        !stderr.contains("attempt 2"),
        "a daemon that stays is not asked again: {stderr}"
    );
}

/// The registration file and the two files beside it.
fn registration_artifacts(registration_path: &std::path::Path) -> [std::path::PathBuf; 3] {
    [
        registration_path.to_path_buf(),
        registration_path.with_extension("sock"),
        registration_path.with_extension("lock"),
    ]
}

fn process_exists(pid: u32) -> bool {
    Command::new("kill")
        .args(["-0", &pid.to_string()])
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

/// What the stand-in daemon does on its socket.
#[derive(Clone, Copy)]
enum StandInMode {
    /// Accepts and drops each connection.
    Listen,
    /// The same, and ignores `SIGTERM`.
    IgnoreTerm,
    /// Takes no connection, with a full queue, so a connect is refused as busy.
    Busy,
    /// Lets liveness probes go; closes the first connection that sends a frame without
    /// an answer, removes its socket and exits, as a daemon that decided to exit idle.
    RefuseAndExit,
    /// The same without the exit: closes every connection that sends a frame.
    Refuse,
    /// Takes the start lock at the path given in place of the socket, holds it for half
    /// a second and exits, as a daemon that is still exiting.
    HoldLock,
}

/// Stands in for a daemon in another process, on `OXABL_STAND_IN_SOCKET`, in the
/// mode `OXABL_STAND_IN_MODE` names. Started by the tests above through this test
/// binary; it exits after a minute in case its test is killed.
#[test]
#[ignore = "a helper process the daemon tests start, not a test"]
fn stand_in_daemon() {
    let Ok(socket) = std::env::var("OXABL_STAND_IN_SOCKET") else {
        return;
    };
    let mode = std::env::var("OXABL_STAND_IN_MODE").unwrap_or_default();
    std::thread::spawn(|| {
        std::thread::sleep(Duration::from_secs(60));
        std::process::exit(0);
    });
    unsafe extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
        fn listen(fd: i32, backlog: i32) -> i32;
    }
    if mode == "hold-lock" {
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::OpenOptionsExt;
        unsafe extern "C" {
            fn flock(fd: i32, operation: i32) -> i32;
        }
        let file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(false)
            .mode(0o600)
            .open(&socket)
            .expect("open the lock file");
        // SAFETY: a valid descriptor this function owns; LOCK_EX is 2.
        assert_eq!(unsafe { flock(file.as_raw_fd(), 2) }, 0, "take the lock");
        println!("stand-in ready");
        std::thread::sleep(Duration::from_millis(500));
        std::process::exit(0);
    }
    if mode == "ignore-term" {
        // SAFETY: SIG_IGN (1) for SIGTERM (15) installs no handler code.
        unsafe { signal(15, 1) };
    }
    let listener =
        std::os::unix::net::UnixListener::bind(&socket).expect("bind the stand-in socket");
    if mode == "busy" {
        use std::os::fd::AsRawFd;
        // SAFETY: a valid listening descriptor; a second listen only sets the backlog.
        assert_eq!(unsafe { listen(listener.as_raw_fd(), 0) }, 0);
        // One queued connection fills a backlog of 0.
        let _queued = UnixStream::connect(&socket).expect("fill the queue");
        println!("stand-in ready");
        loop {
            std::thread::sleep(Duration::from_secs(1));
        }
    }
    if mode == "refuse-and-exit" || mode == "refuse" {
        for stream in listener.incoming() {
            let Ok(mut stream) = stream else { continue };
            let _ = stream.set_read_timeout(Some(Duration::from_secs(10)));
            if stream.read(&mut [0; 1]).is_ok_and(|read| read > 0) && mode != "refuse" {
                drop(stream);
                let _ = std::fs::remove_file(&socket);
                std::process::exit(0);
            }
        }
    }
    for stream in listener.incoming() {
        drop(stream);
    }
}

struct StandIn(ChildGuard);

impl StandIn {
    fn start(socket: &std::path::Path, mode: StandInMode) -> Self {
        let mut command = Command::new(std::env::current_exe().expect("this test binary"));
        command
            .args([
                "stand_in_daemon",
                "--exact",
                "--ignored",
                "--test-threads=1",
                "--nocapture",
            ])
            .env("OXABL_STAND_IN_SOCKET", socket)
            .env(
                "OXABL_STAND_IN_MODE",
                match mode {
                    StandInMode::Listen => "listen",
                    StandInMode::IgnoreTerm => "ignore-term",
                    StandInMode::Busy => "busy",
                    StandInMode::RefuseAndExit => "refuse-and-exit",
                    StandInMode::Refuse => "refuse",
                    StandInMode::HoldLock => "hold-lock",
                },
            )
            .stdin(Stdio::null())
            .stdout(match mode {
                StandInMode::Busy | StandInMode::HoldLock => Stdio::piped(),
                _ => Stdio::null(),
            })
            .stderr(Stdio::null());
        let mut child = command.spawn().expect("start the stand-in");
        let output = child.stdout.take();
        let stand_in = StandIn(ChildGuard(child));
        if let Some(output) = output {
            // A connect would wait on the full queue, and a lock has no connect, so the
            // stand-in says when it is ready.
            let (ready, wait) = mpsc::channel();
            std::thread::spawn(move || {
                for line in BufReader::new(output).lines() {
                    if line.is_ok_and(|line| line.contains("stand-in ready")) {
                        let _ = ready.send(());
                        return;
                    }
                }
            });
            wait.recv_timeout(Duration::from_secs(10))
                .expect("the stand-in must get ready");
            return stand_in;
        }
        let deadline = Instant::now() + Duration::from_secs(10);
        while UnixStream::connect(socket).is_err() {
            assert!(Instant::now() < deadline, "the stand-in must listen");
            std::thread::sleep(Duration::from_millis(10));
        }
        stand_in
    }
}

/// A registration directory and file as a killed daemon leaves them, with its socket
/// removed so another process can listen there.
fn registration_left_behind(
    root: &std::path::Path,
    base: &std::path::Path,
) -> (std::path::PathBuf, std::path::PathBuf) {
    let registration_path = registration_path_in(root, base);
    let daemon = ChildGuard(launch_daemon(root, base));
    let registration = wait_for_registration(&registration_path);
    drop(daemon);
    let socket = std::path::PathBuf::from(registration.socket_path);
    std::fs::remove_file(&socket).expect("remove the killed daemon's socket");
    (registration_path, socket)
}

fn launch_daemon(root: &std::path::Path, base: &std::path::Path) -> Child {
    Command::new(OXABL_BIN)
        .arg("daemon")
        .arg(root)
        .envs(isolated_base(base))
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("launch a daemon")
}

/// Rewrite a registration in place, keeping its mode.
fn rewrite_registration(path: &std::path::Path, change: impl FnOnce(&mut serde_json::Value)) {
    let mut body: serde_json::Value =
        serde_json::from_slice(&std::fs::read(path).expect("read the registration"))
            .expect("a JSON registration");
    change(&mut body);
    std::fs::write(path, body.to_string()).expect("rewrite the registration");
}

fn exits_within(child: &mut Child, limit: Duration) -> Option<std::process::ExitStatus> {
    let deadline = Instant::now() + limit;
    loop {
        if let Some(status) = child.try_wait().expect("poll a process") {
            return Some(status);
        }
        if Instant::now() >= deadline {
            return None;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// `oxabl lsp` with `initialize` sent, its first answer and its stderr read on
/// threads so a stuck shim fails the test rather than hang it.
struct Editor {
    child: ChildGuard,
    _input: std::process::ChildStdin,
    answer: mpsc::Receiver<String>,
    stderr: mpsc::Receiver<String>,
}

impl Editor {
    fn start(root: &std::path::Path, base: &std::path::Path) -> Self {
        Editor::start_with(root, base, &[])
    }

    fn start_with(
        root: &std::path::Path,
        base: &std::path::Path,
        environment: &[(&str, &str)],
    ) -> Self {
        let mut child = Command::new(OXABL_BIN)
            .arg("lsp")
            .envs(isolated_base(base))
            .envs(environment.iter().copied())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("launch the editor shim");
        let mut input = child.stdin.take().expect("editor stdin");
        let mut output = child.stdout.take().expect("editor stdout");
        let mut error = child.stderr.take().expect("editor stderr");
        let (stderr_sent, stderr) = mpsc::channel();
        std::thread::spawn(move || {
            let mut text = String::new();
            let _ = error.read_to_string(&mut text);
            let _ = stderr_sent.send(text);
        });
        let initialize = serde_json::json!({
            "jsonrpc": "2.0",
            "id": 10,
            "method": "initialize",
            "params": {
                "processId": null,
                "rootUri": format!("file://{}", root.display()),
                "capabilities": {}
            }
        });
        write_frame(&mut input, &initialize.to_string());
        let (answered, answer) = mpsc::channel();
        std::thread::spawn(move || {
            let _ = answered.send(read_frame(&mut output));
        });
        Editor {
            child: ChildGuard(child),
            _input: input,
            answer,
            stderr,
        }
    }

    /// The answer to `initialize`, or a failure with the shim's stderr. On failure the
    /// daemon the shim registered is stopped first: it inherits the shim's stderr.
    fn initialize_response(
        self,
        registration_path: &std::path::Path,
        old_pid: u32,
    ) -> serde_json::Value {
        match self.answer.recv_timeout(Duration::from_secs(15)) {
            Ok(response) => serde_json::from_str(&response).expect("LSP initialize response"),
            Err(_) => {
                let started = std::fs::read(registration_path)
                    .ok()
                    .and_then(|body| serde_json::from_slice::<Registration>(&body).ok())
                    .filter(|registration| registration.pid != old_pid);
                drop(started.map(|registration| PidGuard(registration.pid)));
                drop(self.child);
                let stderr = self
                    .stderr
                    .recv_timeout(Duration::from_secs(5))
                    .unwrap_or_else(|_| "<stderr still open>".to_string());
                panic!("the editor shim did not answer initialize: {stderr}");
            }
        }
    }

    /// The answer to `initialize`, for an editor that stays.
    fn answer(&self) -> serde_json::Value {
        let response = self
            .answer
            .recv_timeout(Duration::from_secs(15))
            .unwrap_or_else(|_| panic!("the editor shim did not answer initialize"));
        serde_json::from_str(&response).expect("LSP initialize response")
    }

    /// Close the editor's side, as a closed window does, and wait for the shim to end.
    fn leave(self) {
        let Editor {
            mut child, _input, ..
        } = self;
        drop(_input);
        assert!(
            exits_within(&mut child.0, Duration::from_secs(10)).is_some(),
            "the editor shim ends when the editor leaves"
        );
    }

    fn stderr(&self, limit: Duration) -> String {
        self.stderr
            .recv_timeout(limit)
            .unwrap_or_else(|_| "<stderr still open>".to_string())
    }
}

/// Kills a daemon the editor shim started, which no test owns as a child.
struct PidGuard(u32);

impl Drop for PidGuard {
    fn drop(&mut self) {
        let _ = Command::new("kill").arg(self.0.to_string()).status();
    }
}

fn wait_for_registration(path: &std::path::Path) -> Registration {
    let deadline = Instant::now() + Duration::from_secs(10);
    while !path.exists() && Instant::now() < deadline {
        std::thread::sleep(Duration::from_millis(10));
    }
    serde_json::from_slice(&std::fs::read(path).expect("the daemon writes its registration"))
        .expect("the registration is valid JSON")
}

/// The registration path a daemon under `base` writes for `root`.
///
/// Calls the protocol crate's own naming rule with the directory and the canonical
/// root supplied, so this test cannot drift from it. It used to reach the same rule by
/// setting `XDG_CACHE_HOME` around the call, which needed a lock and two `unsafe`
/// blocks to keep one process-wide variable from racing every other test in the binary.
///
/// The root is canonicalised because the naming rule is: one tree is one name, however
/// a client spelled it. A temporary directory is often reached through a symlink, so a
/// test that skipped this step would derive a name no daemon writes.
fn registration_path_in(root: &std::path::Path, base: &std::path::Path) -> std::path::PathBuf {
    oxabl_daemon_protocol::registration_path_in(
        &base.join("oxabl").join("daemon"),
        &oxabl_daemon_protocol::canonical_root(root).expect("the workspace root exists"),
    )
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
