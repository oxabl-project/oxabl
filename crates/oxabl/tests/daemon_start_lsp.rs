//! The start of the daemon through the real `oxabl lsp`. A start that is slow, as on a
//! loaded machine, still gives a working server and shows itself to the editor; a
//! daemon that exits during the start is reported at once.
//!
//! A workspace root whose path holds `OXABL-TEST-HOLD:daemon.ready@<ms>` makes the
//! daemon wait before it registers, and `OXABL-TEST-PANIC:daemon.ready` makes it
//! exit there, in test builds only.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};

/// Longer than the fixed two-second wait the server once had.
const SLOW: &str = "OXABL-TEST-HOLD:daemon.ready@3500";

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

#[test]
fn a_fast_start_shows_nothing() {
    let parent = tempfile::tempdir().unwrap();
    let root = workspace(parent.path(), "ws");
    let started = Instant::now();
    let (_session, early) =
        Session::start_with_params(&root, json!({"workDoneToken": "daemon-start"}));
    // On a loaded machine even this start can be slow; then the report is right.
    if started.elapsed() < Duration::from_millis(900) {
        assert!(
            !early.iter().any(is_start_log) && progress(&early, "begin") == 0,
            "a quick start sends no log message and no progress: {early:?}"
        );
    }
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
