//! Related locations on the diagnostics of the unknown-value and dynamic-handle
//! rules, through the real `oxabl lsp`: the statements on the path of a finding
//! arrive as `relatedInformation`, a location in an include points at the
//! include file, and a client that does not take related information gets the
//! same diagnostic without it.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n\
                      [workspace.sources]\ninclude_paths = [\".\"]\n\
                      [workspace.schema]\nfiles = [\"db.df\"]\n\
                      [workspace.lint]\nenable = [\"strict.handles\", \"strict.nulls\"]\n";

const SCHEMA: &str = "ADD TABLE \"Customer\"\n\
                      ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\
                      ADD FIELD \"Name\" OF \"Customer\" AS character\n";

/// NL003: two finds that may leave `Customer` empty, then a read of a field.
const FINDS: &str = "DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
                     IF n > 0 THEN\n    \
                         FIND FIRST Customer WHERE Customer.CustNum = 1 NO-LOCK NO-ERROR.\n\
                     ELSE\n    \
                         FIND LAST Customer NO-LOCK NO-ERROR.\n\
                     MESSAGE Customer.Name.\n";

/// HD001: a query that the procedure loses at its end.
const LEAK: &str = "PROCEDURE p:\n    \
                        DEFINE VARIABLE hq AS HANDLE NO-UNDO.\n    \
                        CREATE QUERY hq.\n    \
                        MESSAGE \"working\".\n\
                    END PROCEDURE.\n";

/// NL001: `x` is set to `?` in `reset.i`, then compared in the root.
const RESET: &str = "/* reset */\nx = ?.\n";
const COMPARES: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
                        {reset.i}\n\
                        IF x > 5 THEN MESSAGE \"big\".\n";

/// SEM0001 in `dup.i` when `dup.p` includes it: the label is the earlier
/// declaration in the root, after non-ASCII text on its line.
const DUP_ROOT: &str = "/* na\u{ef}ve \u{2713} */ DEFINE VARIABLE dup-v AS INTEGER NO-UNDO.\n\
                        {dup.i}\n\
                        MESSAGE dup-v.\n";
const DUP_INCLUDE: &str = "DEFINE VARIABLE dup-v AS INTEGER NO-UNDO.\n";

fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let files = [
        ("oxabl.toml", CONFIG),
        ("db.df", SCHEMA),
        ("finds.p", FINDS),
        ("leak.p", LEAK),
        ("reset.i", RESET),
        ("compares.p", COMPARES),
        ("dup.p", DUP_ROOT),
        ("dup.i", DUP_INCLUDE),
    ];
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

fn related_client() -> Value {
    json!({"textDocument": {"publishDiagnostics": {"relatedInformation": true}}})
}

/// The first published diagnostic of `path` with `code`, after opening it.
#[track_caller]
fn finding(session: &mut Session, path: &Path, text: &str, code: &str) -> Value {
    session.open(path, text);
    let uri = json!(file_uri(path));
    let published = session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics")
            && m["params"]["uri"] == uri
            && m["params"]["diagnostics"]
                .as_array()
                .unwrap()
                .iter()
                .any(|d| d["code"] == json!(code))
    });
    published["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["code"] == json!(code))
        .unwrap()
        .clone()
}

fn range(line: u32, start: u32, end_line: u32, end: u32) -> Value {
    json!({
        "start": {"line": line, "character": start},
        "end": {"line": end_line, "character": end}
    })
}

#[test]
fn a_field_read_after_failed_finds_relates_each_find() {
    let dir = tree();
    let mut session = Session::start_with(dir.path(), related_client());
    let path = dir.path().join("finds.p");
    let d = finding(&mut session, &path, FINDS, "NL003");

    assert_eq!(d["range"], range(5, 8, 5, 21), "{d:#}");
    let uri = file_uri(&path);
    assert_eq!(
        d["relatedInformation"],
        json!([
            {
                "location": {"uri": uri, "range": range(2, 4, 2, 68)},
                "message": "this may find no record and leave `Customer` empty"
            },
            {
                "location": {"uri": uri, "range": range(4, 4, 4, 40)},
                "message": "this may find no record and leave `Customer` empty"
            }
        ]),
        "{d:#}"
    );
}

#[test]
fn a_leaked_handle_relates_the_statement_before_the_exit() {
    let dir = tree();
    let mut session = Session::start_with(dir.path(), related_client());
    let path = dir.path().join("leak.p");
    let d = finding(&mut session, &path, LEAK, "HD001");

    assert_eq!(d["range"], range(2, 4, 2, 20), "{d:#}");
    assert_eq!(
        d["relatedInformation"],
        json!([{
            "location": {"uri": file_uri(&path), "range": range(3, 4, 3, 22)},
            "message": "path ends after `MESSAGE \"working\"`"
        }]),
        "{d:#}"
    );
}

#[test]
fn a_source_in_an_include_points_at_the_include_file() {
    let dir = tree();
    let mut session = Session::start_with(dir.path(), related_client());
    let path = dir.path().join("compares.p");
    let d = finding(&mut session, &path, COMPARES, "NL001");

    assert_eq!(d["range"], range(2, 3, 2, 4), "{d:#}");
    let include = std::fs::canonicalize(dir.path().join("reset.i")).unwrap();
    assert_eq!(
        d["relatedInformation"],
        json!([{
            "location": {"uri": file_uri(&include), "range": range(1, 0, 1, 5)},
            "message": "this may assign `?` to `x`"
        }]),
        "{d:#}"
    );
}

#[test]
fn a_client_without_the_capability_gets_the_same_diagnostic_without_it() {
    let dir = tree();
    let path = dir.path().join("finds.p");

    let mut plain = Session::start(dir.path());
    let without = finding(&mut plain, &path, FINDS, "NL003");
    let mut related = Session::start_with(dir.path(), related_client());
    let mut with = finding(&mut related, &path, FINDS, "NL003");

    assert!(without.get("relatedInformation").is_none(), "{without:#}");
    assert!(with["relatedInformation"].is_array(), "{with:#}");
    with.as_object_mut().unwrap().remove("relatedInformation");
    assert_eq!(without, with);
}

#[test]
fn an_include_in_a_root_context_relates_a_place_in_the_root() {
    let dir = tree();
    let mut session = Session::start_with(dir.path(), related_client());
    let include = dir.path().join("dup.i");
    let root = dir.path().join("dup.p");
    session.open(&include, DUP_INCLUDE);
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&include)}, "root": root}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let uri = json!(file_uri(&include));
    let published = session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics")
            && m["params"]["uri"] == uri
            && m["params"]["diagnostics"]
                .as_array()
                .unwrap()
                .iter()
                .any(|d| d["code"] == json!("SEM0001"))
    });
    let d = published["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|d| d["code"] == json!("SEM0001"))
        .unwrap()
        .clone();
    assert_eq!(d["range"], range(0, 16, 0, 21), "{d:#}");
    // UTF-16 columns: the two non-ASCII characters before the name take one
    // unit each, three bytes fewer than their UTF-8 bytes.
    let root = std::fs::canonicalize(&root).unwrap();
    assert_eq!(
        d["relatedInformation"],
        json!([{
            "location": {"uri": file_uri(&root), "range": range(0, 30, 0, 35)},
            "message": "prior declaration here"
        }]),
        "{d:#}"
    );
}
