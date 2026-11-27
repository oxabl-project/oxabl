//! An open or an edit of a large file never makes the server stop answering
//! while the diagnostics of that file are computed, through the real
//! `oxabl lsp`.
//!
//! The analysis of a large file takes seconds in a test build. Salsa checks for
//! cancellation only at the boundary of a query, so a write to the buffer, which
//! waits for every database snapshot to drop, waited for the whole analysis. The
//! open of a file computed its diagnostics on the main loop, so every message
//! after it waited too. Each test times a cheap request for another file, sent
//! after the open or the edit of the large one. The files are real code and no
//! test marker holds an analysis, because only the real analysis shows whether
//! it stops soon enough.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard};
use std::thread::sleep;
use std::time::{Duration, Instant};

use serde_json::json;
use session::{Session, file_uri};

/// The procedures of the large file: about 130,000 lines, which take about
/// three seconds to analyse in a test build.
const PROCEDURES: usize = 8000;

/// The longest a request for another file may take. The analysis of the large
/// file takes several times as long, so a request that waits for it fails. A
/// request after the open alone takes a third of this to read the large text.
const LIMIT: Duration = Duration::from_millis(700);

/// Long enough for the server to start the analysis of the large file, once it
/// has handled the open, and short of its end.
const SETTLE: Duration = Duration::from_millis(300);

const SMALL: &str = "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n";

const INCLUDE: &str = "DEFINE VARIABLE shared-v AS INTEGER NO-UNDO.\n";

fn large_source(tail: &str) -> String {
    let mut text = String::from("{inc.i}\n");
    text.extend((0..PROCEDURES).map(|i| {
        format!(
            "DEFINE VARIABLE v{i} AS INTEGER NO-UNDO.\n\
                 DEFINE VARIABLE c{i} AS CHARACTER NO-UNDO.\n\
                 PROCEDURE p{i}:\n\
                 \x20   DEFINE INPUT PARAMETER ip{i} AS INTEGER NO-UNDO.\n\
                 \x20   DEFINE VARIABLE l{i} AS INTEGER NO-UNDO.\n\
                 \x20   l{i} = ip{i} + v{i}.\n\
                 \x20   IF l{i} > 3 THEN DO:\n\
                 \x20       c{i} = STRING(l{i}) + \"x\".\n\
                 \x20       MESSAGE c{i} VIEW-AS ALERT-BOX.\n\
                 \x20   END.\n\
                 \x20   ELSE\n\
                 \x20       DO l{i} = 1 TO 10:\n\
                 \x20           v{i} = v{i} + l{i}.\n\
                 \x20       END.\n\
                 \x20   RUN p{i} (INPUT l{i}).\n\
                 END PROCEDURE.\n"
        )
    }));
    text.push_str(tail);
    text
}

struct Setup {
    _dir: tempfile::TempDir,
    session: Session,
    big: PathBuf,
    small: PathBuf,
}

/// A session with `small.p` open and answered, and `big.p` and `inc.i`
/// written but not open.
fn setup() -> Setup {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    std::fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"waits\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    let big = root.join("big.p");
    let small = root.join("small.p");
    std::fs::write(&big, large_source("")).unwrap();
    std::fs::write(root.join("inc.i"), INCLUDE).unwrap();
    std::fs::write(&small, SMALL).unwrap();
    let mut session = Session::start(root);
    session.open(&small, SMALL);
    probe(&mut session, &small);
    Setup {
        _dir: dir,
        session,
        big,
        small,
    }
}

/// The time of one request for `file`, which is cheap to answer.
fn probe(session: &mut Session, file: &Path) -> Duration {
    let start = Instant::now();
    let answer = session.request(
        "textDocument/foldingRange",
        json!({"textDocument": {"uri": file_uri(file)}}),
    );
    assert!(answer.get("result").is_some(), "the answer: {answer}");
    let took = start.elapsed();
    eprintln!("a request took {took:?}");
    took
}

/// Insert `text` at the end of `file`, whose last line is line `line`.
fn append(session: &mut Session, file: &Path, version: u32, line: usize, text: &str) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(file), "version": version},
            "contentChanges": [{
                "range": {
                    "start": {"line": line, "character": 0},
                    "end": {"line": line, "character": 0}
                },
                "text": text
            }]
        }
    }));
}

/// Open `big.p` and wait until the server has handled the open and started to
/// analyse it. Returns the number of its last line.
fn open_big(session: &mut Session, big: &Path, small: &Path) -> usize {
    let text = large_source("");
    session.open(big, &text);
    // The server handles messages in order, so the answer for `small.p` says that
    // the open is done and the analysis of `big.p` runs.
    probe(session, small);
    sleep(SETTLE);
    text.lines().count()
}

/// The tests time requests, and each one loads the machine with an analysis, so
/// they run one at a time.
static ONE_AT_A_TIME: Mutex<()> = Mutex::new(());

fn serial() -> MutexGuard<'static, ()> {
    ONE_AT_A_TIME.lock().unwrap_or_else(|e| e.into_inner())
}

/// The request for `small.p` that follows the open of `big.p` is answered before
/// the diagnostics of `big.p` are published: the analysis does not run on the
/// loop that handles the request. An order, not a time, because the time to read
/// the large text depends on the machine.
#[test]
fn a_request_after_the_open_of_a_large_file_does_not_wait_for_its_analysis() {
    let _serial = serial();
    let Setup {
        mut session,
        big,
        small,
        _dir,
    } = setup();

    let id = session.next_request_id();
    session.open(&big, &large_source(""));
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/foldingRange",
        "params": {"textDocument": {"uri": file_uri(&small)}}
    }));
    let uri = json!(file_uri(&big));
    let answered_first = session.wait_for(|m| {
        m["id"] == json!(id)
            || (m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == uri)
    });
    assert_eq!(
        answered_first["id"],
        json!(id),
        "the diagnostics of the large file came before the answer to the next request: {}",
        answered_first["method"]
    );
}

#[test]
fn an_edit_of_a_large_file_does_not_wait_for_a_running_analysis() {
    let _serial = serial();
    let Setup {
        mut session,
        big,
        small,
        _dir,
    } = setup();

    let last = open_big(&mut session, &big, &small);
    // The analysis of version 1 is running. The edit makes it stale.
    append(&mut session, &big, 2, last, "MESSAGE 1.\n");
    let took = probe(&mut session, &small);
    assert!(
        took < LIMIT,
        "a request waited {took:?} for the analysis that the edit made stale"
    );
}

#[test]
fn an_edit_of_a_root_does_not_wait_for_the_analysis_of_its_include_context() {
    let _serial = serial();
    let Setup {
        mut session,
        big,
        small,
        _dir,
    } = setup();

    // `inc.i` is open and read in the context of `big.p`, so the analysis of the
    // root runs twice: for the root and for the include.
    let include = big.with_file_name("inc.i");
    session.open(&include, INCLUDE);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": big}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let last = open_big(&mut session, &big, &small);
    append(&mut session, &big, 2, last, "MESSAGE 1.\n");
    let took = probe(&mut session, &small);
    assert!(
        took < LIMIT,
        "a request waited {took:?} for the analyses that the edit made stale"
    );
}

/// Every write cancels every running analysis, so the edit of one file stops the
/// analysis of another. That analysis runs again: its diagnostics are published.
#[test]
fn an_analysis_that_an_edit_of_another_file_stopped_runs_again() {
    let _serial = serial();
    let Setup {
        mut session,
        big,
        small,
        _dir,
    } = setup();

    open_big(&mut session, &big, &small);
    append(&mut session, &small, 2, 1, "MESSAGE a.\n");
    let uri = json!(file_uri(&big));
    let deadline = Instant::now() + Duration::from_secs(30);
    loop {
        let left = deadline.saturating_duration_since(Instant::now());
        let message = session
            .try_next(left)
            .expect("the diagnostics of the large file are never published");
        if message["method"] == "textDocument/publishDiagnostics" && message["params"]["uri"] == uri
        {
            assert_eq!(message["params"]["version"], json!(1));
            return;
        }
    }
}
