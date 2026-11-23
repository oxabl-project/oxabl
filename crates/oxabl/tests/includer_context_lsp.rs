//! The compilation context of an open include, through the real `oxabl lsp`:
//! the roots that expand it, and the diagnostics of the one the client selects.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

const SHARED: &str = "DEFINE VARIABLE used-v AS INTEGER NO-UNDO.\n\
                      DEFINE VARIABLE spare-v AS INTEGER NO-UNDO.\n";

/// `reads.p` reads both variables, `ignores.p` only one, and `nested.p` reaches
/// the include through `mid.i` and reads one.
fn tree(config: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let files = [
        ("oxabl.toml", config),
        ("shared.i", SHARED),
        ("reads.p", "{shared.i}\nMESSAGE used-v spare-v.\n"),
        ("ignores.p", "{shared.i}\nMESSAGE used-v.\n"),
        ("mid.i", "/* mid */\n{shared.i}\n"),
        ("nested.p", "{mid.i}\nMESSAGE used-v.\n"),
        ("orphan.i", "DEFINE VARIABLE lonely-v AS INTEGER NO-UNDO.\n"),
    ];
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

fn canonical(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap()
}

fn contexts(session: &mut Session, include: &Path) -> Value {
    let answer = session.request(
        "oxabl/includerContexts",
        json!({"textDocument": {"uri": file_uri(include)}}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    answer["result"].clone()
}

fn select(session: &mut Session, include: &Path, root: Option<&Path>) -> Value {
    session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(include)}, "root": root}),
    )
}

/// The next published diagnostics of `path` that satisfy `wanted`.
#[track_caller]
fn diagnostics_where(
    session: &Session,
    path: &Path,
    wanted: impl Fn(&[Value]) -> bool,
) -> Vec<Value> {
    let uri = json!(file_uri(path));
    let published = session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics")
            && m["params"]["uri"] == uri
            && wanted(m["params"]["diagnostics"].as_array().unwrap())
    });
    published["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .clone()
}

/// Whether `diagnostics` report `name` as an unused variable.
fn unused(diagnostics: &[Value], name: &str) -> bool {
    diagnostics
        .iter()
        .any(|d| d["code"] == json!("LINT0002") && d["message"].as_str().unwrap().contains(name))
}

/// No publish for `path` within `window` satisfies `unwanted`.
#[track_caller]
fn assert_no_publish(
    session: &Session,
    path: &Path,
    window: Duration,
    unwanted: impl Fn(&[Value]) -> bool,
) {
    let uri = json!(file_uri(path));
    let end = Instant::now() + window;
    while let Some(left) = end.checked_duration_since(Instant::now()) {
        let Some(message) = session.try_next(left) else {
            return;
        };
        if message["method"] == json!("textDocument/publishDiagnostics")
            && message["params"]["uri"] == uri
        {
            let diagnostics = message["params"]["diagnostics"].as_array().unwrap();
            assert!(!unwanted(diagnostics), "a stale publish: {message}");
        }
    }
}

#[test]
fn the_roots_that_expand_an_include_are_listed_direct_and_transitive() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let shared = dir.path().join("shared.i");
    session.open(&shared, SHARED);

    let answer = contexts(&mut session, &shared);
    let root = |name: &str| canonical(&dir.path().join(name));
    assert_eq!(
        answer["contexts"],
        json!([
            {"root": root("ignores.p"), "uri": file_uri(&root("ignores.p")), "direct": true},
            {"root": root("nested.p"), "uri": file_uri(&root("nested.p")), "direct": false},
            {"root": root("reads.p"), "uri": file_uri(&root("reads.p")), "direct": true},
        ]),
        "{answer}"
    );
    assert_eq!(answer["complete"], json!(true));
    assert_eq!(answer["reason"], Value::Null);
    assert_eq!(
        answer["selected"],
        Value::Null,
        "nothing is selected by default"
    );
}

#[test]
fn switching_the_context_switches_the_unused_finding() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let shared = dir.path().join("shared.i");
    session.open(&shared, SHARED);
    diagnostics_where(&session, &shared, |_| true);

    let ignores = dir.path().join("ignores.p");
    let answer = select(&mut session, &shared, Some(&ignores));
    assert_eq!(
        answer["result"]["selected"],
        json!(canonical(&ignores)),
        "{answer}"
    );
    let found = diagnostics_where(&session, &shared, |d| unused(d, "spare-v"));
    let finding = found
        .iter()
        .find(|d| unused(&[(*d).clone()], "spare-v"))
        .unwrap();
    assert_eq!(
        finding["range"]["start"],
        json!({"line": 1, "character": 16})
    );
    assert!(!unused(&found, "used-v`"), "{found:#?}");

    let reads = dir.path().join("reads.p");
    select(&mut session, &shared, Some(&reads));
    diagnostics_where(&session, &shared, |d| !unused(d, "spare-v"));
    assert_eq!(
        contexts(&mut session, &shared)["selected"],
        json!(canonical(&reads))
    );

    // Through another include, the root still decides.
    let nested = dir.path().join("nested.p");
    select(&mut session, &shared, Some(&nested));
    diagnostics_where(&session, &shared, |d| unused(d, "spare-v"));

    let answer = select(&mut session, &shared, None);
    assert_eq!(answer["result"]["selected"], Value::Null, "{answer}");
    assert_eq!(contexts(&mut session, &shared)["selected"], Value::Null);
}

#[test]
fn unsaved_text_of_the_include_and_of_the_root_counts() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let shared = dir.path().join("shared.i");
    let edited = format!("{SHARED}DEFINE VARIABLE fresh-v AS INTEGER NO-UNDO.\n");
    session.open(&shared, &edited);

    let reads = dir.path().join("reads.p");
    select(&mut session, &shared, Some(&reads));
    let found = diagnostics_where(&session, &shared, |d| unused(d, "fresh-v"));
    assert!(!unused(&found, "spare-v"), "{found:#?}");
    assert_eq!(
        std::fs::read_to_string(&shared).unwrap(),
        SHARED,
        "disk is untouched"
    );

    // The root stops reading `spare-v` in its unsaved buffer.
    session.open(&reads, "{shared.i}\nMESSAGE used-v.\n");
    diagnostics_where(&session, &shared, |d| {
        unused(d, "spare-v") && unused(d, "fresh-v")
    });
}

#[test]
fn an_include_with_no_known_root_stays_a_fragment_and_says_why() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let orphan = dir.path().join("orphan.i");
    session.open(&orphan, "DEFINE VARIABLE lonely-v AS INTEGER NO-UNDO.\n");

    let answer = contexts(&mut session, &orphan);
    assert_eq!(answer["contexts"], json!([]));
    assert_eq!(
        answer["reason"],
        json!("no root file in the workspace includes this file")
    );

    let refused = select(&mut session, &orphan, Some(&dir.path().join("reads.p")));
    assert_eq!(refused["error"]["code"], json!(-32602), "{refused}");
    assert!(
        refused["error"]["message"]
            .as_str()
            .unwrap()
            .contains("does not include this file"),
        "{refused}"
    );
    assert_eq!(contexts(&mut session, &orphan)["selected"], Value::Null);
}

#[test]
fn without_include_paths_the_list_is_unavailable_and_says_why() {
    let dir = tree("[workspace]\nname = \"p\"\n");
    let mut session = Session::start(dir.path());
    let shared = dir.path().join("shared.i");
    session.open(&shared, SHARED);

    let answer = contexts(&mut session, &shared);
    assert_eq!(answer["contexts"], json!([]));
    assert_eq!(answer["complete"], json!(false));
    assert!(
        answer["reason"]
            .as_str()
            .unwrap()
            .contains("no include path"),
        "{answer}"
    );
}

#[test]
fn only_an_include_has_a_context() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let reads = dir.path().join("reads.p");
    session.open(&reads, "{shared.i}\nMESSAGE used-v spare-v.\n");
    let refused = select(&mut session, &reads, Some(&dir.path().join("ignores.p")));
    assert_eq!(refused["error"]["code"], json!(-32602), "{refused}");
}

#[test]
fn a_later_selection_wins_over_an_earlier_one() {
    let dir = tree(CONFIG);
    let mut session = Session::start(dir.path());
    let shared = dir.path().join("shared.i");
    session.open(&shared, SHARED);

    let ignores = dir.path().join("ignores.p");
    let reads = dir.path().join("reads.p");
    // Both are asked for before either is checked against the includer list.
    session.send(json!({
        "jsonrpc": "2.0", "id": 9001, "method": "oxabl/selectIncluderContext",
        "params": {"textDocument": {"uri": file_uri(&shared)}, "root": ignores},
    }));
    session.send(json!({
        "jsonrpc": "2.0", "id": 9002, "method": "oxabl/selectIncluderContext",
        "params": {"textDocument": {"uri": file_uri(&shared)}, "root": reads},
    }));
    // The two answers can come in either order, and the publish follows the
    // second, so every message is read in one loop.
    let (mut first, mut second, mut settled) = (None, None, false);
    let uri = json!(file_uri(&shared));
    let deadline = Instant::now() + Duration::from_secs(60);
    while first.is_none() || second.is_none() || !settled {
        let left = deadline
            .checked_duration_since(Instant::now())
            .expect("both answers and a publish within 60 s");
        let Some(message) = session.try_next(left) else {
            continue;
        };
        if message["id"] == json!(9001) {
            first = Some(message);
        } else if message["id"] == json!(9002) {
            second = Some(message);
        } else if second.is_some()
            && message["method"] == json!("textDocument/publishDiagnostics")
            && message["params"]["uri"] == uri
        {
            let diagnostics = message["params"]["diagnostics"].as_array().unwrap();
            assert!(
                !unused(diagnostics, "spare-v"),
                "a stale publish: {message}"
            );
            settled = true;
        }
    }
    let (first, second) = (first.unwrap(), second.unwrap());
    assert_eq!(first["error"]["code"], json!(-32801), "{first}");
    assert_eq!(
        second["result"]["selected"],
        json!(canonical(&reads)),
        "{second}"
    );
    assert_no_publish(&session, &shared, Duration::from_secs(2), |d| {
        unused(d, "spare-v")
    });
}
