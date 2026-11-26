//! When the daemon that `oxabl lsp` starts exits: when its workspace root is gone or
//! replaced, and when no client has been connected for the idle timeout. A daemon that
//! serves a connected client never exits for either idle reason, and a client that
//! comes after an idle exit gets a new daemon. A test that panics also stops its daemon
//! (the `Session` helper). See the `oxabl daemon` entry of `docs/overview.md`.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Launch, Session, file_uri, is_running};

const MAIN: &str = "DEFINE VARIABLE cName AS CHARACTER NO-UNDO.\ncName = \"x\".\n";

/// The idle timeout of the daemons below, short enough for a test.
const SHORT_IDLE: &[(&str, &str)] = &[("OXABL_DAEMON_IDLE_TIMEOUT", "1s")];

fn workspace(parent: &Path, name: &str) -> std::path::PathBuf {
    let root = parent.join(name);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("oxabl.toml"), "[workspace]\nname = \"exit\"\n").unwrap();
    std::fs::write(root.join("main.p"), MAIN).unwrap();
    root
}

fn exits_within(pid: u32, limit: Duration) -> bool {
    let deadline = Instant::now() + limit;
    while is_running(pid) {
        if Instant::now() >= deadline {
            return false;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    true
}

/// The document symbols of `main.p`: the proof that a daemon serves the session.
fn symbols(session: &mut Session, root: &Path) -> Value {
    let path = root.join("main.p");
    session.open(&path, MAIN);
    session.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": file_uri(&path)}}),
    )
}

fn assert_serves(session: &mut Session, root: &Path) {
    let answer = symbols(session, root);
    assert!(
        answer["result"].to_string().contains("cName"),
        "the daemon serves the session: {answer}"
    );
}

#[test]
fn a_daemon_exits_when_the_root_of_its_session_is_removed() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let mut session = Session::start(&root);
    assert_serves(&mut session, &root);
    let pid = session.daemon_pid().expect("a daemon serves the session");

    std::fs::remove_dir_all(&root).unwrap();
    assert!(
        exits_within(pid, Duration::from_secs(10)),
        "the daemon of a removed root still runs after 10 s"
    );
    assert!(
        !session.registration_path().exists(),
        "the daemon removed its registration"
    );
}

#[test]
fn a_daemon_exits_when_the_root_of_its_session_is_replaced() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let mut session = Session::start(&root);
    assert_serves(&mut session, &root);
    let pid = session.daemon_pid().expect("a daemon serves the session");

    let other = workspace(parent.path(), "other");
    std::fs::rename(&root, parent.path().join("aside")).unwrap();
    std::fs::rename(&other, &root).unwrap();
    assert!(
        exits_within(pid, Duration::from_secs(10)),
        "the daemon of a replaced root still runs after 10 s"
    );
}

#[test]
fn an_idle_daemon_exits_and_the_next_client_starts_a_new_one() {
    let parent = tempfile::tempdir().unwrap();
    let runtime = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let launch = || Launch {
        runtime: Some(runtime.path()),
        env: SHORT_IDLE,
        ..Launch::default()
    };
    let (mut first, _) =
        Session::launch_with(&root, json!({}), launch(), |_| {}).await_initialize();
    assert_serves(&mut first, &root);
    let pid = first.daemon_pid().expect("a daemon serves the session");
    let registration = first.registration_path().to_path_buf();

    first.end_client();
    assert!(
        exits_within(pid, Duration::from_secs(15)),
        "the daemon with no client still runs after its idle timeout of 1 s"
    );
    assert!(
        !registration.exists(),
        "the idle daemon removed its registration"
    );

    let (mut second, _) =
        Session::launch_with(&root, json!({}), launch(), |_| {}).await_initialize();
    assert_serves(&mut second, &root);
    let replacement = second
        .daemon_pid()
        .expect("a daemon serves the new session");
    assert_ne!(replacement, pid, "a new daemon serves the new client");
}

/// A daemon that decided to exit still serves its connected clients, so the process
/// alone does not show the decision. A client that comes later does: a daemon that
/// decided to exit refuses it.
#[test]
fn a_daemon_with_a_connected_client_does_not_exit_idle() {
    let parent = tempfile::tempdir().unwrap();
    let runtime = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let launch = || Launch {
        runtime: Some(runtime.path()),
        env: SHORT_IDLE,
        ..Launch::default()
    };
    let (mut first, _) =
        Session::launch_with(&root, json!({}), launch(), |_| {}).await_initialize();
    let pid = first.daemon_pid().expect("a daemon serves the session");

    // Four idle timeouts, with no request.
    std::thread::sleep(Duration::from_secs(4));
    assert!(is_running(pid), "the daemon exited with a client connected");
    assert_serves(&mut first, &root);
    let (mut second, _) =
        Session::launch_with(&root, json!({}), launch(), |_| {}).await_initialize();
    assert_eq!(
        second.daemon_pid(),
        Some(pid),
        "the daemon with a client still takes new clients"
    );
    assert_serves(&mut second, &root);
}

/// The idle exit must not need the socket file: a test that deleted its runtime
/// directory before its daemon went idle left the daemon for ever.
#[test]
fn an_idle_daemon_exits_after_its_runtime_directory_was_removed() {
    let parent = tempfile::tempdir().unwrap();
    let runtime = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let launch = Launch {
        runtime: Some(runtime.path()),
        env: &[("OXABL_DAEMON_IDLE_TIMEOUT", "2s")],
        ..Launch::default()
    };
    let (mut session, _) =
        Session::launch_with(&root, json!({}), launch, |_| {}).await_initialize();
    let pid = session.daemon_pid().expect("a daemon serves the session");

    session.end_client();
    std::fs::remove_dir_all(runtime.path()).unwrap();
    assert!(
        exits_within(pid, Duration::from_secs(20)),
        "the idle daemon without a runtime directory still runs after 20 s"
    );
}

/// A failed test drops its `Session` while it unwinds, and that must stop the daemon.
#[test]
fn a_test_that_panics_leaves_no_daemon() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let pid = std::sync::atomic::AtomicU32::new(0);
    let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut session = Session::start(&root);
        assert_serves(&mut session, &root);
        pid.store(
            session.daemon_pid().expect("a daemon serves the session"),
            std::sync::atomic::Ordering::SeqCst,
        );
        panic!("the test fails with its session open");
    }));
    assert!(outcome.is_err(), "the closure panics");
    let pid = pid.load(std::sync::atomic::Ordering::SeqCst);
    assert_ne!(pid, 0, "the daemon was running before the panic");
    assert!(
        !is_running(pid),
        "the daemon of a test that panicked still runs"
    );
}
