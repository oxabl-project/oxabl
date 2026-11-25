//! A parse error inside an include is never dropped: the root reports it at the
//! include site, and the include reports it in its own bytes when a root is
//! selected. Through the real `oxabl lsp`. All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

/// A `CREATE` whose trigger block is never closed.
const BROKEN: &str = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
                      CREATE BUTTON h TRIGGERS: ON CHOOSE DO:\n  MESSAGE x.\n";

const ROOT: &str = "MESSAGE \"a\".\n{broken.i}\nMESSAGE \"b\".\n";

/// Alone it parses (`mode` is unset, so the branch is not taken); a root that
/// sets `mode` to `bad` makes the include's own text fail to parse.
const MODE: &str = "&IF \"{&mode}\" = \"bad\" &THEN\n\
                    CREATE BUTTON h TRIGGERS: ON CHOOSE DO:\n\
                    &ENDIF\n\
                    MESSAGE \"x\".\n";

const MODE_ROOT: &str = "MESSAGE \"a\".\n{mode.i &mode=bad}\n";

fn tree() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    for (name, text) in [
        ("oxabl.toml", CONFIG),
        ("broken.i", BROKEN),
        ("root.p", ROOT),
        ("mode.i", MODE),
        ("moderoot.p", MODE_ROOT),
    ] {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

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

fn parse_errors(diagnostics: &[Value]) -> Vec<&Value> {
    diagnostics
        .iter()
        .filter(|d| d["code"] == json!("PARSE001"))
        .collect()
}

#[test]
fn the_root_reports_a_parse_error_of_its_include_at_the_include_site() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let root = dir.path().join("root.p");
    session.open(&root, ROOT);

    let found = diagnostics_where(&session, &root, |d| !parse_errors(d).is_empty());
    let errors = parse_errors(&found);
    assert_eq!(errors.len(), 1, "{found:#?}");
    assert_eq!(errors[0]["severity"], json!(1), "{found:#?}");
    assert_eq!(
        errors[0]["range"]["start"],
        json!({"line": 1, "character": 0}),
        "{found:#?}"
    );
    let message = errors[0]["message"].as_str().unwrap();
    assert!(message.contains("broken.i:2:1"), "{message}");
}

#[test]
fn a_selected_root_gives_the_include_its_own_parse_error() {
    let dir = tree();
    let mut session = Session::start(dir.path());
    let mode = dir.path().join("mode.i");
    session.open(&mode, MODE);
    let alone = diagnostics_where(&session, &mode, |_| true);
    assert!(parse_errors(&alone).is_empty(), "{alone:#?}");

    let root = dir.path().join("moderoot.p");
    let answer = session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&mode)}, "root": root}),
    );
    assert!(answer.get("error").is_none(), "{answer}");

    let found = diagnostics_where(&session, &mode, |d| !parse_errors(d).is_empty());
    let errors = parse_errors(&found);
    assert_eq!(errors.len(), 1, "{found:#?}");
    assert_eq!(
        errors[0]["range"]["start"],
        json!({"line": 1, "character": 0}),
        "{found:#?}"
    );
    assert_eq!(
        errors[0]["message"],
        json!("Expected END TRIGGERS"),
        "{found:#?}"
    );
}
