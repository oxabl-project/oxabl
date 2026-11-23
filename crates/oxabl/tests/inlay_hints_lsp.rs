//! Inlay hints through the real `oxabl lsp`: the lock a read takes, the index
//! a search uses, and where a raised buffer scope or an implicit transaction
//! starts and ends.
//!
//! The hints must say what the strict database rules say. The tests compare
//! them with the findings of `oxabl check --json` and the index choices of
//! `oxabl explain --json` over the same synthetic files, each a separate run.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

use serde_json::{Value, json};
use session::Session;

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");
const FILES: [&str; 4] = ["schema.df", "billing.p", "reset.p", "partial.p"];
const STRICT: &str = "enable = [\"strict.locks\", \"strict.transactions\", \"strict.access\"]";

fn fixtures() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/inlay_hints")
}

/// A workspace with the fixture files and `lint` as its `[workspace.lint]`.
fn workspace(lint: &str) -> tempfile::TempDir {
    let root = tempfile::tempdir().unwrap();
    for name in FILES {
        std::fs::copy(fixtures().join(name), root.path().join(name)).unwrap();
    }
    std::fs::write(
        root.path().join("oxabl.toml"),
        format!(
            "[workspace]\nname = \"hints\"\n[workspace.schema]\nfiles = [\"schema.df\"]\n\
             [workspace.lint]\n{lint}\n"
        ),
    )
    .unwrap();
    root
}

fn capabilities() -> Value {
    json!({
        "textDocument": {"inlayHint": {}},
        "workspace": {"inlayHint": {"refreshSupport": true}}
    })
}

/// One hint, at a byte offset of its file.
#[derive(Debug, Clone)]
struct Hint {
    at: usize,
    line: usize,
    label: String,
    tooltip: String,
}

fn line_starts(text: &str) -> Vec<usize> {
    std::iter::once(0)
        .chain(text.match_indices('\n').map(|(i, _)| i + 1))
        .collect()
}

/// The hints of the open file `name` in the lines `from..to` (zero-based).
/// An answer an edit overtook is asked again.
fn hints_in(session: &mut Session, root: &Path, name: &str, from: u32, to: u32) -> Vec<Hint> {
    let path = root.join(name);
    let text = std::fs::read_to_string(&path).unwrap();
    let starts = line_starts(&text);
    for _ in 0..50 {
        let answer = session.request(
            "textDocument/inlayHint",
            json!({
                "textDocument": {"uri": session::file_uri(&path)},
                "range": {"start": {"line": from, "character": 0},
                          "end": {"line": to, "character": 0}}
            }),
        );
        if answer["error"]["code"] == json!(-32801) {
            std::thread::sleep(Duration::from_millis(20));
            continue;
        }
        let hints = answer["result"]
            .as_array()
            .unwrap_or_else(|| panic!("inlayHint gave no list: {answer}"));
        return hints
            .iter()
            .map(|h| {
                let line = h["position"]["line"].as_u64().unwrap() as usize;
                let character = h["position"]["character"].as_u64().unwrap() as usize;
                Hint {
                    at: starts[line] + character,
                    line: line + 1,
                    label: h["label"].as_str().unwrap().to_string(),
                    tooltip: h["tooltip"].as_str().unwrap_or_default().to_string(),
                }
            })
            .collect();
    }
    panic!("every inlayHint answer for {name} was overtaken by an edit");
}

/// The hints of the whole open file `name`.
fn hints(session: &mut Session, root: &Path, name: &str) -> Vec<Hint> {
    hints_in(session, root, name, 0, 10_000)
}

fn open(session: &mut Session, root: &Path, name: &str) {
    let path = root.join(name);
    session.open(&path, &std::fs::read_to_string(&path).unwrap());
}

/// The findings `oxabl check --json` reports for `name`: code and byte span.
fn findings(root: &Path, name: &str) -> Vec<(String, usize, usize)> {
    let output = Command::new(OXABL_BIN)
        .args(["check", "--json", name])
        .current_dir(root)
        .output()
        .unwrap();
    let report: Value = serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|e| panic!("check --json: {e}: {output:?}"));
    report["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["code"].as_str().unwrap().to_string(),
                d["span"]["start"].as_u64().unwrap() as usize,
                d["span"]["end"].as_u64().unwrap() as usize,
            )
        })
        .collect()
}

/// The findings the server published for the open file `name`: code and
/// byte span.
fn published(session: &Session, root: &Path, name: &str) -> Vec<(String, usize, usize)> {
    let path = root.join(name);
    let uri = session::file_uri(&path);
    let starts = line_starts(&std::fs::read_to_string(&path).unwrap());
    let message = session.wait_for(|m| {
        m["method"] == json!("textDocument/publishDiagnostics") && m["params"]["uri"] == json!(uri)
    });
    let at = |p: &Value| {
        starts[p["line"].as_u64().unwrap() as usize] + p["character"].as_u64().unwrap() as usize
    };
    let mut found: Vec<_> = message["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| {
            (
                d["code"].as_str().unwrap().to_string(),
                at(&d["range"]["start"]),
                at(&d["range"]["end"]),
            )
        })
        .collect();
    found.sort();
    found
}

/// The index choices `oxabl explain --json` names for `name`: line, index,
/// whole-index scan. Tables the schema does not have are left out.
fn index_choices(root: &Path, name: &str) -> Vec<(usize, String, bool)> {
    let output = Command::new(OXABL_BIN)
        .args(["explain", "--json", "--schema", "schema.df", name])
        .current_dir(root)
        .output()
        .unwrap();
    let report: Value = serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|e| panic!("explain --json: {e}: {output:?}"));
    let mut choices: Vec<_> = report["units"]
        .as_array()
        .unwrap()
        .iter()
        .flat_map(|u| u["effects"].as_array().unwrap())
        .filter(|e| !e["table"].as_str().unwrap().starts_with("?:"))
        .filter_map(|e| {
            Some((
                e["line"].as_u64()? as usize,
                e["index"].as_str()?.to_string(),
                e["reason"].as_str()?.contains("whole=true"),
            ))
        })
        .collect();
    choices.sort();
    choices
}

/// The `(start, end)` pairs of the block hints whose labels end in
/// `starts` / `ends` after `prefix`, matched in nesting order.
fn blocks(hints: &[Hint], prefix: &str) -> Vec<(usize, usize, String)> {
    let mut open: Vec<&Hint> = Vec::new();
    let mut pairs = Vec::new();
    for h in hints {
        if let Some(what) = h.label.strip_suffix(" starts") {
            if what.ends_with(prefix) {
                open.push(h);
            }
        } else if let Some(what) = h.label.strip_suffix(" ends")
            && what.ends_with(prefix)
        {
            let start = open.pop().expect("an end hint with no start hint");
            assert_eq!(start.label.strip_suffix(" starts"), Some(what));
            pairs.push((start.at, h.at, what.to_string()));
        }
    }
    assert!(open.is_empty(), "start hints with no end hint: {open:?}");
    pairs.sort();
    pairs
}

/// The findings of the strict database rules.
fn database(findings: &[(String, usize, usize)]) -> Vec<(String, usize, usize)> {
    let mut found: Vec<_> = findings
        .iter()
        .filter(|(c, ..)| ["LK", "TX", "DB"].iter().any(|p| c.starts_with(p)))
        .cloned()
        .collect();
    found.sort();
    found
}

fn coded(findings: &[(String, usize, usize)], codes: &[&str]) -> Vec<(usize, usize)> {
    let mut spans: Vec<_> = findings
        .iter()
        .filter(|(c, ..)| codes.contains(&c.as_str()))
        .map(|(_, s, e)| (*s, *e))
        .collect();
    spans.sort();
    spans
}

/// Every finding lies in one block pair, and each pair holds one finding.
#[track_caller]
fn one_finding_per_block(pairs: &[(usize, usize, String)], spans: &[(usize, usize)]) {
    assert_eq!(pairs.len(), spans.len(), "{pairs:?} against {spans:?}");
    for (s, e) in spans {
        let holding = pairs.iter().filter(|(a, b, _)| a <= s && e <= b).count();
        assert!(
            holding >= 1,
            "no block hint holds the finding at {s}..{e}: {pairs:?}"
        );
    }
}

#[test]
fn the_hints_say_what_the_strict_rules_and_explain_say() {
    let root = workspace(STRICT);
    let mut session = Session::start_with(root.path(), capabilities());
    assert_eq!(
        session.initialized["result"]["capabilities"]["inlayHintProvider"],
        json!(true)
    );
    let mut kinds_seen = [0usize; 4];
    for name in ["billing.p", "reset.p"] {
        open(&mut session, root.path(), name);
        let found = findings(root.path(), name);
        // The editor's database findings are the gate's.
        assert_eq!(
            database(&published(&session, root.path(), name)),
            database(&found),
            "{name}"
        );
        let hints = hints(&mut session, root.path(), name);

        // A lock hint follows the name of each read LK001 reports, and only there.
        let locks: Vec<_> = hints.iter().filter(|h| h.label == "SHARE-LOCK").collect();
        let mut at: Vec<_> = locks.iter().map(|h| h.at).collect();
        at.sort();
        let lk001: Vec<_> = coded(&found, &["LK001"]).iter().map(|(_, e)| *e).collect();
        assert_eq!(at, lk001, "{name}: lock hints against LK001");
        assert!(locks.iter().all(|h| h.tooltip.contains("LK001")));

        // An index hint for each search explain names an index for.
        let mut index: Vec<_> = hints
            .iter()
            .filter_map(|h| {
                let rest = h.label.strip_prefix("index ")?;
                let (name, whole) = match rest.strip_suffix(", WHOLE-INDEX") {
                    Some(name) => (name, true),
                    None => (rest, false),
                };
                Some((h.line, name.to_string(), whole))
            })
            .collect();
        index.sort();
        assert_eq!(
            index,
            index_choices(root.path(), name),
            "{name}: index hints"
        );

        // A scope pair around each scope TX005 reports raised.
        let scopes = blocks(&hints, " scope");
        one_finding_per_block(&scopes, &coded(&found, &["TX005"]));

        // A transaction pair around the update of each block TX001 or TX002 reports.
        let transactions = blocks(&hints, "transaction");
        one_finding_per_block(&transactions, &coded(&found, &["TX001", "TX002"]));

        kinds_seen[0] += locks.len();
        kinds_seen[1] += index.len();
        kinds_seen[2] += scopes.len();
        kinds_seen[3] += transactions.len();
        assert_eq!(
            hints.len(),
            locks.len() + index.len() + 2 * (scopes.len() + transactions.len()),
            "{name}: a hint of no kind: {hints:?}"
        );
    }
    // Each comparison above compared something.
    assert_eq!(kinds_seen, [3, 7, 1, 2]);
}

#[test]
fn the_hints_are_placed_at_the_code_they_are_about() {
    let root = workspace(STRICT);
    let mut session = Session::start_with(root.path(), capabilities());
    open(&mut session, root.path(), "billing.p");
    let hints = hints(&mut session, root.path(), "billing.p");
    let text = std::fs::read_to_string(root.path().join("billing.p")).unwrap();
    let placed: Vec<_> = hints
        .iter()
        .map(|h| {
            let line = text.lines().nth(h.line - 1).unwrap_or("");
            let column = h.at - line_starts(&text)[h.line - 1];
            (h.line, format!("{}|{}", &line[..column], h.label))
        })
        .collect();
    let expected = [
        (2, "|customer scope starts"),
        (7, "FOR EACH customer|index region-name"),
        (10, "FIND FIRST customer|SHARE-LOCK"),
        (10, "FIND FIRST customer|index custnum"),
        (13, "FIND FIRST ttnote|index pk"),
        (16, "    |transaction starts"),
        (16, "    FOR EACH invoice|SHARE-LOCK"),
        (16, "    FOR EACH invoice|index cust"),
        (18, "    END.|transaction ends"),
        (22, "    FOR EACH invoice|index invnum, WHOLE-INDEX"),
        (25, "    IF CAN-FIND(FIRST invoice|SHARE-LOCK"),
        (25, "    IF CAN-FIND(FIRST invoice|index invnum"),
        (27, "END PROCEDURE.|customer scope ends"),
    ];
    let expected: Vec<_> = expected.iter().map(|(l, s)| (*l, s.to_string())).collect();
    assert_eq!(placed, expected);
}

#[test]
fn a_file_the_analysis_cannot_judge_gives_no_hint() {
    let root = workspace(STRICT);
    let mut session = Session::start_with(root.path(), capabilities());
    open(&mut session, root.path(), "partial.p");
    // Text the preprocessor did not expand stands where a lock phrase or a
    // condition may be: the rules are silent, explain names no index, and
    // the hints are silent too.
    let editor = published(&session, root.path(), "partial.p");
    assert!(editor.iter().any(|(c, ..)| c == "PREPROC007"), "{editor:?}");
    assert_eq!(database(&editor), []);
    assert_eq!(database(&findings(root.path(), "partial.p")), []);
    assert!(index_choices(root.path(), "partial.p").is_empty());
    let hints = hints(&mut session, root.path(), "partial.p");
    assert!(hints.is_empty(), "{hints:?}");
}

#[test]
fn a_workspace_without_strict_database_rules_gives_no_hint() {
    let root = workspace("enable = [\"strict.handles\"]");
    let mut session = Session::start_with(root.path(), capabilities());
    open(&mut session, root.path(), "billing.p");
    assert!(hints(&mut session, root.path(), "billing.p").is_empty());
}

#[test]
fn only_the_requested_range_is_answered() {
    let root = workspace(STRICT);
    let mut session = Session::start_with(root.path(), capabilities());
    open(&mut session, root.path(), "billing.p");
    let all = hints(&mut session, root.path(), "billing.p");
    // Lines 15 to 21: the procedure `post-invoices`.
    let some = hints_in(&mut session, root.path(), "billing.p", 14, 21);
    let lines: Vec<_> = some.iter().map(|h| h.line).collect();
    assert_eq!(lines, [16, 16, 16, 18]);
    let expected: Vec<_> = all
        .iter()
        .filter(|h| (15..=21).contains(&h.line))
        .map(|h| &h.label)
        .collect();
    assert_eq!(some.iter().map(|h| &h.label).collect::<Vec<_>>(), expected);
}

#[test]
fn each_kind_turns_off_on_its_own() {
    let root = workspace(STRICT);
    let mut session = Session::start_with_options(
        root.path(),
        capabilities(),
        json!({"inlayHints": {"locks": false}}),
    );
    open(&mut session, root.path(), "billing.p");
    let kinds = |hints: &[Hint]| {
        let mut kinds: Vec<&str> = hints
            .iter()
            .map(|h| match h.label.as_str() {
                "SHARE-LOCK" => "locks",
                l if l.starts_with("index ") => "index",
                l if l.contains(" scope ") => "scope",
                _ => "transactions",
            })
            .collect();
        kinds.sort();
        kinds.dedup();
        kinds
    };
    let off = hints(&mut session, root.path(), "billing.p");
    assert_eq!(kinds(&off), ["index", "scope", "transactions"]);

    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
        "params": {"settings": {"oxabl": {"inlayHints": {"locks": true, "index": false}}}}
    }));
    // The client is asked to ask again.
    session.wait_for(|m| m["method"] == json!("workspace/inlayHint/refresh"));
    let on = hints(&mut session, root.path(), "billing.p");
    assert_eq!(kinds(&on), ["locks", "scope", "transactions"]);

    for (key, left) in [
        ("scope", vec!["locks", "transactions"]),
        ("transactions", vec!["locks"]),
    ] {
        session.send(json!({
            "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
            "params": {"settings": {"oxabl": {"inlayHints": {key: false}}}}
        }));
        session.wait_for(|m| m["method"] == json!("workspace/inlayHint/refresh"));
        assert_eq!(kinds(&hints(&mut session, root.path(), "billing.p")), left);
    }
}

#[test]
fn a_client_without_inlay_hints_is_offered_none() {
    let root = workspace(STRICT);
    let session = Session::start_with(root.path(), json!({}));
    assert!(
        session.initialized["result"]["capabilities"]
            .get("inlayHintProvider")
            .is_none()
    );
}

#[test]
fn an_edit_moves_the_hints_with_the_text() {
    let root = workspace(STRICT);
    let mut session = Session::start_with(root.path(), capabilities());
    open(&mut session, root.path(), "billing.p");
    let before = hints_in(&mut session, root.path(), "billing.p", 9, 10);
    assert_eq!(
        before.iter().map(|h| h.label.as_str()).collect::<Vec<_>>(),
        ["SHARE-LOCK", "index custnum"]
    );
    // The read now names its lock: LK001 is silent, and the hint goes.
    let path = root.path().join("billing.p");
    let text = std::fs::read_to_string(&path)
        .unwrap()
        .replace("customer.custnum = 1.", "customer.custnum = 1 NO-LOCK.");
    std::fs::write(&path, &text).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": session::file_uri(&path), "version": 2},
            "contentChanges": [{"text": text}]
        }
    }));
    let after = hints_in(&mut session, root.path(), "billing.p", 9, 10);
    assert_eq!(
        after.iter().map(|h| h.label.as_str()).collect::<Vec<_>>(),
        ["index custnum"]
    );
    assert_eq!(
        coded(&findings(root.path(), "billing.p"), &["LK001"]).len(),
        2
    );
}
