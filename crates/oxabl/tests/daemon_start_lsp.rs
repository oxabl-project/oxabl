//! The start of the daemon through the real `oxabl lsp`. A start that is slow, as on a
//! loaded machine, still gives a working server and shows itself to the editor; a
//! daemon that exits during the start is reported at once.
//!
//! A workspace root whose path holds `OXABL-TEST-HOLD:daemon.ready@<ms>` makes the
//! daemon wait before it registers, and `OXABL-TEST-PANIC:daemon.ready` makes it
//! exit there, and `OXABL-TEST-LIMIT@<ms>` shortens the limit of the wait, in test
//! builds only.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};

/// Longer than the fixed two-second wait the server once had.
const SLOW: &str = "OXABL-TEST-HOLD:daemon.ready@3500";

/// A daemon that stays in its start for far longer than the limit below.
const HUNG: &str = "OXABL-TEST-HOLD:daemon.ready@60000-OXABL-TEST-LIMIT@4000";

const MAIN: &str = "DEFINE VARIABLE cName AS CHARACTER NO-UNDO.\ncName = \"x\".\n";

fn workspace(parent: &Path, name: &str) -> std::path::PathBuf {
    let root = parent.join(name);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("oxabl.toml"), "[workspace]\nname = \"start\"\n").unwrap();
    std::fs::write(root.join("main.p"), MAIN).unwrap();
    root
}

fn is_start_log(message: &Value) -> bool {
    message["method"] == "window/logMessage"
        && message["params"]["message"]
            .as_str()
            .is_some_and(|text| text.contains("still starting"))
}

fn progress(early: &[Value], kind: &str) -> usize {
    early
        .iter()
        .filter(|m| m["method"] == "$/progress" && m["params"]["value"]["kind"] == kind)
        .count()
}

/// The document symbols of `main.p`: the proof that the daemon serves the session.
fn symbols(session: &mut Session, root: &Path) -> Value {
    let path = root.join("main.p");
    session.open(&path, MAIN);
    session.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": file_uri(&path)}}),
    )
}

#[test]
fn a_slow_daemon_start_gives_a_working_server_and_reports_progress() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), &format!("ws-{SLOW}"));
    let (mut session, early) =
        Session::start_with_params(&root, json!({"workDoneToken": "daemon-start"}));

    assert!(
        session.initialized["result"]["capabilities"].is_object(),
        "the server answers `initialize` after a slow start: {}",
        session.initialized
    );
    assert!(
        early.iter().any(is_start_log),
        "a slow start is written to the editor's log: {early:?}"
    );
    assert_eq!(
        progress(&early, "begin"),
        1,
        "a slow start begins one progress report on the client's token: {early:?}"
    );
    assert_eq!(
        progress(&early, "end"),
        1,
        "the progress report ends before the answer to `initialize`: {early:?}"
    );
    assert!(
        early
            .iter()
            .filter(|m| m["method"] == "$/progress")
            .all(|m| m["params"]["token"] == "daemon-start"),
        "the progress uses the token from `initialize`: {early:?}"
    );

    let answer = symbols(&mut session, &root);
    assert!(
        answer["result"].to_string().contains("cName"),
        "the daemon answers a request with the right result: {answer}"
    );
}

#[test]
fn a_slow_start_without_a_progress_token_is_written_to_the_log() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), &format!("ws-{SLOW}"));
    let (mut session, early) = Session::start_with_params(&root, json!({}));

    assert!(
        early.iter().any(is_start_log),
        "a slow start is written to the editor's log: {early:?}"
    );
    assert_eq!(
        progress(&early, "begin") + progress(&early, "end"),
        0,
        "no progress is sent without a token from the client: {early:?}"
    );
    let answer = symbols(&mut session, &root);
    assert!(
        answer["result"].to_string().contains("cName"),
        "the daemon answers a request with the right result: {answer}"
    );
}

/// The start is shown only when it took a second or more. The check holds for any
/// speed of the machine: a start that was shown took at least the second, and one
/// that took less shows nothing.
#[test]
fn a_start_is_shown_only_when_it_is_slow() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let started = Instant::now();
    let (_session, early) =
        Session::start_with_params(&root, json!({"workDoneToken": "daemon-start"}));
    let elapsed = started.elapsed();
    let shown = early.iter().any(is_start_log) || progress(&early, "begin") > 0;
    assert!(
        !shown || elapsed >= Duration::from_secs(1),
        "a start that took {elapsed:?} was shown: {early:?}"
    );
}

#[test]
fn a_daemon_that_exits_during_a_slow_start_is_reported_at_once() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(
        parent.path(),
        &format!("ws-{SLOW}-OXABL-TEST-PANIC:daemon.ready"),
    );
    let started = Instant::now();
    let mut session = Session::launch(&root, json!({}));
    let status = session
        .wait_exit(Duration::from_secs(30))
        .expect("`oxabl lsp` exits when its daemon exits during the start");
    let elapsed = started.elapsed();
    let stderr = session.stderr();

    assert!(!status.success(), "the start failed: {stderr}");
    assert!(
        elapsed < Duration::from_secs(20),
        "the failure comes soon after the daemon exits, not at the time limit: {elapsed:?}"
    );
    assert!(
        stderr.contains("exited") && stderr.contains("before it served the root"),
        "the message names the daemon's exit: {stderr}"
    );
    assert!(
        !stderr.contains("did not become ready"),
        "the message names the exit, not a timeout: {stderr}"
    );
}

/// The pid that the message names after `pid `.
fn named_pid(stderr: &str) -> u32 {
    let tail = &stderr[stderr.find("(pid ").expect("the message names a pid") + 5..];
    tail[..tail.find(')').unwrap()].parse().expect("a pid")
}

fn alive(pid: u32) -> bool {
    std::process::Command::new("kill")
        .args(["-0", &pid.to_string()])
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

fn kill(pid: u32) {
    let _ = std::process::Command::new("kill")
        .arg(pid.to_string())
        .status();
}

/// Stops the held daemon that a message names, also when an assertion fails first.
struct KillOnDrop(u32);

impl Drop for KillOnDrop {
    fn drop(&mut self) {
        kill(self.0);
    }
}

/// The daemon that the stderr of `oxabl lsp` names, if it names one.
fn named_daemon(stderr: &str) -> Option<KillOnDrop> {
    stderr
        .contains("(pid ")
        .then(|| KillOnDrop(named_pid(stderr)))
}

/// Takes the start lock of the root whose registration file is `registration`, before
/// `oxabl lsp` starts, and frees it after `seconds`.
fn hold_start_lock(registration: &Path, seconds: u64) -> std::thread::JoinHandle<()> {
    use std::os::fd::AsRawFd;
    use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};

    unsafe extern "C" {
        fn flock(fd: i32, operation: i32) -> i32;
    }
    let lock_path = registration.with_extension("lock");
    let directory = lock_path.parent().unwrap();
    std::fs::create_dir_all(directory).unwrap();
    for level in [directory.parent().unwrap(), directory] {
        std::fs::set_permissions(level, std::fs::Permissions::from_mode(0o700)).unwrap();
    }
    let file = std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(false)
        .mode(0o600)
        .open(&lock_path)
        .unwrap();
    // SAFETY: a valid descriptor this function owns; LOCK_EX is 2.
    assert_eq!(unsafe { flock(file.as_raw_fd(), 2) }, 0, "take the lock");
    std::thread::spawn(move || {
        std::thread::sleep(Duration::from_secs(seconds));
        drop(file);
    })
}

#[test]
fn a_daemon_that_never_serves_ends_the_wait_at_the_limit_and_is_named() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), &format!("ws-{HUNG}"));
    let started = Instant::now();
    let mut session = Session::launch(&root, json!({}));
    let status = session.wait_exit(Duration::from_secs(40));
    let elapsed = started.elapsed();
    let stderr = session.stderr();
    let daemon = named_daemon(&stderr);

    let status = status.unwrap_or_else(|| {
        panic!("`oxabl lsp` is still waiting 40 s after a limit of 4 s: {stderr}")
    });
    assert!(!status.success(), "the start failed: {stderr}");
    assert!(
        elapsed >= Duration::from_secs(4) && elapsed < Duration::from_secs(20),
        "the wait ends soon after its limit: {elapsed:?}"
    );
    let pid = daemon.as_ref().expect("the message names the daemon").0;
    assert!(
        stderr.contains("still runs after 4 s") && stderr.contains(&format!("`kill {pid}`")),
        "the message names the daemon and how to stop it: {stderr}"
    );
    assert!(
        alive(pid),
        "the daemon is left running, as the message says"
    );
}

/// A client that cannot read the registration of a daemon that is starting says so
/// when the wait ends, and does not only say that the daemon runs.
#[test]
fn a_wait_that_ends_with_a_running_daemon_names_what_the_client_could_not_read() {
    use std::os::unix::fs::PermissionsExt;

    // The limit is far longer than the start needs on any machine; the wait ends a
    // short time after the directory changes, which is the only thing under test.
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(
        parent.path(),
        "ws-OXABL-TEST-HOLD:daemon.ready@60000-OXABL-TEST-LIMIT@10000",
    );
    let started = Instant::now();
    let mut session = Session::launch(&root, json!({}));
    // The daemon has bound its socket and waits before it registers.
    let socket = session.registration_path().with_extension("sock");
    let deadline = Instant::now() + Duration::from_secs(8);
    while !socket.exists() {
        assert!(Instant::now() < deadline, "the daemon binds its socket");
        std::thread::sleep(Duration::from_millis(20));
    }
    // A directory that the daemon would not have created makes every look for it
    // undecided.
    let directory = socket.parent().unwrap();
    std::fs::set_permissions(directory, std::fs::Permissions::from_mode(0o755)).unwrap();
    let changed = started.elapsed();

    let status = session.wait_exit(Duration::from_secs(40));
    let stderr = session.stderr();
    let _daemon = named_daemon(&stderr);

    assert!(
        changed < Duration::from_secs(9),
        "the test changed the directory {changed:?} after the start, too late to test it"
    );
    assert!(status.is_some_and(|status| !status.success()), "{stderr}");
    let verdict = stderr
        .lines()
        .find(|line| line.contains("still runs after"))
        .unwrap_or_else(|| panic!("the wait ends with the daemon still running: {stderr}"));
    assert!(
        verdict.contains("ignoring any daemon registration"),
        "the final message gives the reason that the registration was not read: {verdict}"
    );
}

/// While another process holds the start lock, the daemons that `oxabl lsp` starts
/// lose it. Only the first one writes to the editor's log, and the server is served
/// once the lock is free.
#[test]
fn a_wait_for_the_start_lock_writes_its_refusal_once() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let holder = std::cell::RefCell::new(None);
    let mut session = Session::launch_after(&root, json!({}), |registration| {
        *holder.borrow_mut() = Some(hold_start_lock(registration, 4));
    });
    session.initialized = session.wait_for(|m| m["id"] == json!(0) && m.get("method").is_none());
    assert!(
        session.initialized["result"]["capabilities"].is_object(),
        "the server answers once the lock is free: {}",
        session.initialized
    );
    holder.borrow_mut().take().unwrap().join().unwrap();
    let refusals = session.stderr().matches("holds the start lock").count();
    assert_eq!(
        refusals,
        1,
        "the refusal of the daemon is written once, not for each restart: {}",
        session.stderr()
    );
}

/// The daemon that `oxabl lsp` restarts once the lock is free can fail for a reason of
/// its own. Its error is shown, and the first daemon's refusal does not stand in for it.
#[test]
fn a_restarted_daemon_that_fails_shows_its_own_error() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws-OXABL-TEST-PANIC:daemon.ready");
    let holder = std::cell::RefCell::new(None);
    let mut session = Session::launch_after(&root, json!({}), |registration| {
        *holder.borrow_mut() = Some(hold_start_lock(registration, 2));
    });
    let status = session.wait_exit(Duration::from_secs(30));
    let stderr = session.stderr();
    holder.borrow_mut().take().unwrap().join().unwrap();

    assert!(status.is_some_and(|status| !status.success()), "{stderr}");
    assert!(
        stderr.contains("injected test panic"),
        "the error of the restarted daemon is shown: {stderr}"
    );
    assert!(
        !stderr.contains("if you are sure nothing is starting"),
        "no text advises to remove a lock that a process holds: {stderr}"
    );
}
