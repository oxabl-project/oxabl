//! `textDocument/references` and the call hierarchy through the real
//! `oxabl lsp` and its daemon: local names, names other files use without
//! being open, calls oxabl cannot resolve, and answers that can be short.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"references\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const DEFS: &str = "DEFINE VARIABLE gcShared AS CHARACTER NO-UNDO.\n\
    PROCEDURE fromInclude:\n\
    \x20   gcShared = \"y\".\n\
    END PROCEDURE.\n";

const MAIN: &str = "{defs.i}\n\
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.\n\
    DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    DEFINE VARIABLE cProg AS CHARACTER NO-UNDO.\n\
    DEFINE VARIABLE oCalc AS CLASS app.Calc NO-UNDO.\n\
    iCount = iCount + 1.\n\
    gcShared = \"x\".\n\
    RUN lib/util.p PERSISTENT SET hLib.\n\
    RUN doWork IN hLib.\n\
    RUN localStep.\n\
    RUN fromInclude.\n\
    RUN VALUE(cProg).\n\
    oCalc = NEW app.Calc().\n\
    oCalc:Add(1).\n\
    iCount = fnTwice(iCount).\n\
    PROCEDURE localStep:\n\
    \x20   RUN lib/util.p.\n\
    \x20   iCount = fnTwice(2).\n\
    END PROCEDURE.\n\
    FUNCTION fnTwice RETURNS INTEGER (INPUT piValue AS INTEGER):\n\
    \x20   RETURN piValue * 2.\n\
    END FUNCTION.\n";

const UTIL: &str = "PROCEDURE doWork:\n\
    \x20   RUN helper.\n\
    END PROCEDURE.\n\
    PROCEDURE helper:\n\
    END PROCEDURE.\n";

const OTHER: &str = "DEFINE VARIABLE hUtil AS HANDLE NO-UNDO.\n\
    DEFINE VARIABLE oOther AS CLASS app.Calc NO-UNDO.\n\
    RUN lib/util.p PERSISTENT SET hUtil.\n\
    RUN doWork IN hUtil.\n\
    oOther = NEW app.Calc().\n\
    oOther:Add(2).\n";

const THIRD: &str = "RUN lib/util.p.\n";

const CALC: &str = "CLASS app.Calc:\n\
    \x20   METHOD PUBLIC INTEGER Add(INPUT piValue AS INTEGER):\n\
    \x20       RETURN Twice(piValue).\n\
    \x20   END METHOD.\n\
    \x20   METHOD PRIVATE INTEGER Twice(INPUT piValue AS INTEGER):\n\
    \x20       RETURN piValue * 2.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

/// Calls whose target is known only at run time.
const DYNAMIC: &str = "DEFINE VARIABLE cName AS CHARACTER NO-UNDO.\n\
    DEFINE VARIABLE hAny AS HANDLE NO-UNDO.\n\
    RUN VALUE(cName).\n\
    RUN doWork IN hAny.\n\
    DYNAMIC-FUNCTION(cName).\n\
    PUBLISH \"somethingHappened\".\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        for (name, text) in files {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }

    fn standard() -> Workspace {
        Workspace::new(&[
            ("oxabl.toml", CONFIG),
            ("defs.i", DEFS),
            ("main.p", MAIN),
            ("lib/util.p", UTIL),
            ("other.p", OTHER),
            ("third.p", THIRD),
            ("app/Calc.cls", CALC),
            ("dynamic.p", DYNAMIC),
        ])
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }
}

/// The UTF-16 `(line, character)` of byte `at` in `text`.
fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The byte of the `nth` (0-based) occurrence of `needle` in `text`.
#[track_caller]
fn nth(text: &str, needle: &str, nth: usize) -> usize {
    text.match_indices(needle)
        .nth(nth)
        .unwrap_or_else(|| panic!("no occurrence {nth} of `{needle}`"))
        .0
}

/// The position one character into occurrence `occurrence` of `needle`.
fn position(file: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    let (line, character) = utf16_at(text, nth(text, needle, occurrence) + 1);
    json!({
        "textDocument": {"uri": file_uri(file)},
        "position": {"line": line, "character": character}
    })
}

/// A range spanning occurrence `occurrence` of `needle` in `text`.
fn range(text: &str, needle: &str, occurrence: usize) -> Value {
    let start = nth(text, needle, occurrence);
    let (line, character) = utf16_at(text, start);
    let (end_line, end_character) = utf16_at(text, start + needle.len());
    json!({
        "start": {"line": line, "character": character},
        "end": {"line": end_line, "character": end_character}
    })
}

fn location(file: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    json!({"uri": file_uri(file), "range": range(text, needle, occurrence)})
}

/// The answer to one request, and every `window/showMessage` sent before it.
fn ask(session: &mut Session, method: &str, params: Value) -> (Value, Vec<String>) {
    let id = 9_000 + next_id();
    session.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
    let mut warnings = Vec::new();
    loop {
        let message = session.wait_for(|_| true);
        if message["method"] == json!("window/showMessage") {
            warnings.push(message["params"]["message"].as_str().unwrap().to_string());
        }
        if message["id"] == json!(id) && message.get("method").is_none() {
            assert!(
                message.get("error").is_none(),
                "`{method}` gave an error: {message}"
            );
            return (message["result"].clone(), warnings);
        }
    }
}

fn next_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(0);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

fn references(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
    include_declaration: bool,
) -> (Vec<Value>, Vec<String>) {
    let mut params = position(file, text, needle, occurrence);
    params["context"] = json!({"includeDeclaration": include_declaration});
    let (result, warnings) = ask(session, "textDocument/references", params);
    let mut found = match result {
        Value::Null => Vec::new(),
        Value::Array(found) => found,
        other => panic!("references gave {other}"),
    };
    found.sort_by_key(|location| location.to_string());
    (found, warnings)
}

fn sorted(mut locations: Vec<Value>) -> Vec<Value> {
    locations.sort_by_key(|location| location.to_string());
    locations
}

fn prepare(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> Vec<Value> {
    let (result, warnings) = ask(
        session,
        "textDocument/prepareCallHierarchy",
        position(file, text, needle, occurrence),
    );
    assert_eq!(warnings, Vec::<String>::new(), "prepare at `{needle}`");
    match result {
        Value::Null => Vec::new(),
        Value::Array(items) => items,
        other => panic!("prepareCallHierarchy gave {other}"),
    }
}

/// The one item prepared at `needle`.
#[track_caller]
fn item(session: &mut Session, file: &Path, text: &str, needle: &str, occurrence: usize) -> Value {
    let items = prepare(session, file, text, needle, occurrence);
    assert_eq!(items.len(), 1, "items at `{needle}`: {items:?}");
    items[0].clone()
}

/// One call of a call hierarchy answer: the other item's name, symbol kind
/// and file, and the ranges of the call.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Call {
    name: String,
    kind: u64,
    uri: String,
    ranges: Vec<String>,
}

fn call(name: &str, kind: u64, file: &Path, ranges: Vec<Value>) -> Call {
    Call {
        name: name.to_string(),
        kind,
        uri: file_uri(file),
        ranges: ranges.iter().map(Value::to_string).collect(),
    }
}

/// The calls of `item` in `direction` (`incoming` or `outgoing`), sorted, and
/// the warnings sent with them.
fn calls(session: &mut Session, direction: &str, item: &Value) -> (Vec<Call>, Vec<String>) {
    let (result, warnings) = ask(
        session,
        &format!("callHierarchy/{direction}Calls"),
        json!({"item": item}),
    );
    let side = if direction == "incoming" {
        "from"
    } else {
        "to"
    };
    let Value::Array(found) = result else {
        panic!("{direction} calls gave {result}");
    };
    let mut found: Vec<Call> = found
        .iter()
        .map(|call| {
            let other = &call[side];
            let mut ranges: Vec<String> = call["fromRanges"]
                .as_array()
                .unwrap()
                .iter()
                .map(Value::to_string)
                .collect();
            ranges.sort();
            Call {
                name: other["name"].as_str().unwrap().to_string(),
                kind: other["kind"].as_u64().unwrap(),
                uri: other["uri"].as_str().unwrap().to_string(),
                ranges,
            }
        })
        .collect();
    found.sort();
    (found, warnings)
}

fn sorted_calls(mut calls: Vec<Call>) -> Vec<Call> {
    for call in &mut calls {
        call.ranges.sort();
    }
    calls.sort();
    calls
}

const FILE: u64 = 1;
const METHOD: u64 = 6;
const FUNCTION: u64 = 12;
const NULL: u64 = 21;

#[test]
fn local_references_stay_in_their_unit_and_are_placed_in_includes() {
    let ws = Workspace::standard();
    let main = ws.path("main.p");
    let defs = ws.path("defs.i");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let (found, warnings) = references(&mut session, &main, MAIN, "iCount + 1", 0, true);
    let expected: Vec<Value> = (0..6).map(|n| location(&main, MAIN, "iCount", n)).collect();
    assert_eq!(found, sorted(expected));
    assert_eq!(warnings, Vec::<String>::new());

    let (found, _) = references(&mut session, &main, MAIN, "iCount + 1", 0, false);
    let expected: Vec<Value> = (1..6).map(|n| location(&main, MAIN, "iCount", n)).collect();
    assert_eq!(found, sorted(expected), "without the declaration");

    // Declared and used in an include: each place in the file that writes it.
    let (found, warnings) = references(&mut session, &main, MAIN, "gcShared", 0, true);
    let expected = vec![
        location(&defs, DEFS, "gcShared", 0),
        location(&defs, DEFS, "gcShared", 1),
        location(&main, MAIN, "gcShared", 0),
    ];
    assert_eq!(found, sorted(expected));
    assert_eq!(warnings, Vec::<String>::new());

    // A function call shows the name, not the whole call.
    let (found, warnings) = references(&mut session, &main, MAIN, "fnTwice(iCount)", 0, true);
    let expected: Vec<Value> = (0..3)
        .map(|n| location(&main, MAIN, "fnTwice", n))
        .collect();
    assert_eq!(found, sorted(expected));
    assert_eq!(warnings, Vec::<String>::new());

    let (found, _) = references(&mut session, &main, MAIN, "localStep.", 0, true);
    let expected: Vec<Value> = (0..2)
        .map(|n| location(&main, MAIN, "localStep", n))
        .collect();
    assert_eq!(found, sorted(expected));
}

#[test]
fn references_are_found_in_files_that_are_not_open() {
    let ws = Workspace::standard();
    let main = ws.path("main.p");
    let calc = ws.path("app/Calc.cls");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let (found, warnings) = references(&mut session, &main, MAIN, "lib/util.p", 0, true);
    let expected = vec![
        location(&main, MAIN, "lib/util.p", 0),
        location(&main, MAIN, "lib/util.p", 1),
        location(&ws.path("other.p"), OTHER, "lib/util.p", 0),
        location(&ws.path("third.p"), THIRD, "lib/util.p", 0),
    ];
    assert_eq!(found, sorted(expected));
    assert_eq!(warnings, Vec::<String>::new());

    // An internal procedure, run through a handle each file sets.
    let (found, warnings) = references(&mut session, &main, MAIN, "doWork", 0, true);
    let expected = vec![
        location(&main, MAIN, "doWork", 0),
        location(&ws.path("other.p"), OTHER, "doWork", 0),
        location(&ws.path("lib/util.p"), UTIL, "doWork", 0),
    ];
    assert_eq!(found, sorted(expected));
    assert_eq!(warnings, Vec::<String>::new());

    // A method, from a call and from its declaration.
    let expected = sorted(vec![
        location(&main, MAIN, "Add", 0),
        location(&ws.path("other.p"), OTHER, "Add", 0),
        location(&calc, CALC, "Add", 0),
    ]);
    let (found, warnings) = references(&mut session, &main, MAIN, "Add(", 0, true);
    assert_eq!(found, expected);
    assert_eq!(warnings, Vec::<String>::new());
    session.open(&calc, CALC);
    let (found, _) = references(&mut session, &calc, CALC, "Add(", 0, true);
    assert_eq!(found, expected, "from the declaration");
}

#[test]
fn the_call_hierarchy_follows_static_calls_across_files() {
    let ws = Workspace::standard();
    let main = ws.path("main.p");
    let util = ws.path("lib/util.p");
    let other = ws.path("other.p");
    let calc = ws.path("app/Calc.cls");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let local_step = item(&mut session, &main, MAIN, "localStep.", 0);
    assert_eq!(local_step["name"], json!("localStep"));
    assert_eq!(local_step["uri"], json!(file_uri(&main)));
    assert_eq!(local_step["selectionRange"], range(MAIN, "localStep", 1));

    let (found, warnings) = calls(&mut session, "outgoing", &local_step);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("util.p", FILE, &util, vec![range(MAIN, "lib/util.p", 1)]),
            call("fnTwice", FUNCTION, &main, vec![range(MAIN, "fnTwice", 1)]),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());

    // Callers in the main block are the program itself.
    let fn_twice = item(&mut session, &main, MAIN, "fnTwice(2)", 0);
    let (found, _) = calls(&mut session, "incoming", &fn_twice);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("main.p", FILE, &main, vec![range(MAIN, "fnTwice", 0)]),
            call(
                "localStep",
                FUNCTION,
                &main,
                vec![range(MAIN, "fnTwice", 1)]
            ),
        ])
    );

    // A program's callers, in files that are not open.
    let program = item(&mut session, &main, MAIN, "lib/util.p", 0);
    assert_eq!(program["name"], json!("util.p"));
    let (found, warnings) = calls(&mut session, "incoming", &program);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("main.p", FILE, &main, vec![range(MAIN, "lib/util.p", 0)]),
            call(
                "localStep",
                FUNCTION,
                &main,
                vec![range(MAIN, "lib/util.p", 1)]
            ),
            call("other.p", FILE, &other, vec![range(OTHER, "lib/util.p", 0)]),
            call(
                "third.p",
                FILE,
                &ws.path("third.p"),
                vec![range(THIRD, "lib/util.p", 0)]
            ),
        ])
    );
    assert_eq!(warnings, Vec::<String>::new());

    // An internal procedure of another program: its callers and its calls.
    let do_work = item(&mut session, &main, MAIN, "doWork", 0);
    assert_eq!(do_work["uri"], json!(file_uri(&util)));
    let (found, _) = calls(&mut session, "incoming", &do_work);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("main.p", FILE, &main, vec![range(MAIN, "doWork", 0)]),
            call("other.p", FILE, &other, vec![range(OTHER, "doWork", 0)]),
        ])
    );
    let (found, _) = calls(&mut session, "outgoing", &do_work);
    assert_eq!(
        found,
        vec![call(
            "helper",
            FUNCTION,
            &util,
            vec![range(UTIL, "helper", 0)]
        )]
    );

    // A method: its callers on objects of the class, and its own calls.
    let add = item(&mut session, &main, MAIN, "Add(", 0);
    assert_eq!(add["uri"], json!(file_uri(&calc)));
    let (found, _) = calls(&mut session, "incoming", &add);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("main.p", FILE, &main, vec![range(MAIN, "Add", 0)]),
            call("other.p", FILE, &other, vec![range(OTHER, "Add", 0)]),
        ])
    );
    let (found, _) = calls(&mut session, "outgoing", &add);
    assert_eq!(
        found,
        vec![call("Twice", METHOD, &calc, vec![range(CALC, "Twice", 0)])]
    );

    // Outside every routine and name, the program itself.
    let whole = item(&mut session, &main, MAIN, "\n", 0);
    assert_eq!(whole["name"], json!("main.p"));
    let (found, _) = calls(&mut session, "outgoing", &whole);
    let names: Vec<&str> = found.iter().map(|call| call.name.as_str()).collect();
    assert_eq!(
        names,
        vec![
            "Add",
            "RUN VALUE(cProg)",
            "doWork",
            "fnTwice",
            "fromInclude",
            "localStep",
            "util.p"
        ]
    );
}

#[test]
fn dynamic_calls_are_listed_as_unresolved() {
    let ws = Workspace::standard();
    let dynamic = ws.path("dynamic.p");
    let mut session = Session::start(&ws.root);
    session.open(&dynamic, DYNAMIC);

    let program = item(&mut session, &dynamic, DYNAMIC, "\n", 0);
    let (result, _) = ask(
        &mut session,
        "callHierarchy/outgoingCalls",
        json!({"item": program}),
    );
    let found = result.as_array().unwrap();
    let mut shown: Vec<(String, u64, String)> = found
        .iter()
        .map(|call| {
            (
                call["to"]["name"].as_str().unwrap().to_string(),
                call["to"]["kind"].as_u64().unwrap(),
                call["to"]["detail"].as_str().unwrap().to_string(),
            )
        })
        .collect();
    shown.sort();
    let unresolved = |name: &str, why: &str| (name.to_string(), NULL, format!("unresolved: {why}"));
    assert_eq!(
        shown,
        vec![
            unresolved(
                "DYNAMIC-FUNCTION(cName)",
                "the target is computed at run time"
            ),
            unresolved(
                "PUBLISH \"somethingHappened\"",
                "PUBLISH: the subscribers are known at run time"
            ),
            unresolved("RUN VALUE(cName)", "the target is computed at run time"),
            unresolved(
                "doWork",
                "it runs through a handle that oxabl cannot follow"
            ),
        ]
    );
    // Each sits where the call is written, and has no calls of its own.
    for call in found {
        assert_eq!(call["to"]["uri"], json!(file_uri(&dynamic)));
        assert_eq!(call["fromRanges"].as_array().unwrap().len(), 1);
        let (inner, _) = calls(&mut session, "incoming", &call["to"]);
        assert_eq!(inner, Vec::<Call>::new());
    }
}

#[test]
fn an_answer_that_can_be_short_says_so() {
    // No include path: other files cannot be searched.
    let ws = Workspace::new(&[("main.p", MAIN), ("other.p", OTHER)]);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let (found, warnings) = references(&mut session, &main, MAIN, "fnTwice(2)", 0, false);
    assert_eq!(
        found,
        sorted(vec![
            location(&main, MAIN, "fnTwice", 0),
            location(&main, MAIN, "fnTwice", 1)
        ])
    );
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("references may be incomplete")
            && warnings[0].contains("other files are not searched")
            && warnings[0].contains("no include path"),
        "{warnings:?}"
    );
    let fn_twice = item(&mut session, &main, MAIN, "fnTwice(2)", 0);
    let (result, warnings) = ask(
        &mut session,
        "callHierarchy/incomingCalls",
        json!({"item": fn_twice}),
    );
    assert_eq!(result[0]["from"]["name"], json!("incomplete"), "{result}");
    assert!(
        result[0]["from"]["detail"]
            .as_str()
            .unwrap()
            .contains("other files are not searched"),
        "{result}"
    );
    assert_eq!(result.as_array().unwrap().len(), 3, "{result}");
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    // The marker has no calls of its own.
    let (inner, _) = calls(&mut session, "incoming", &result[0]["from"]);
    assert_eq!(inner, Vec::<Call>::new());
    drop(session);

    // A file whose dependency set is incomplete can hide a caller.
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("main.p", MAIN),
        ("defs.i", DEFS),
        ("lib/util.p", UTIL),
        ("lost.p", "{missing.i}\n"),
    ]);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let (found, warnings) = references(&mut session, &main, MAIN, "lib/util.p", 0, false);
    assert_eq!(found.len(), 2, "{found:?}");
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("1 file(s) of the workspace have an incomplete dependency set"),
        "{warnings:?}"
    );
    let program = item(&mut session, &main, MAIN, "lib/util.p", 0);
    let (result, _) = ask(
        &mut session,
        "callHierarchy/incomingCalls",
        json!({"item": program}),
    );
    assert_eq!(result[0]["from"]["name"], json!("incomplete"), "{result}");
}

#[test]
fn a_truncated_call_list_says_so() {
    let mut big = String::from(
        "FUNCTION fnOne RETURNS INTEGER ():\n    RETURN 1.\nEND FUNCTION.\n\
         DEFINE VARIABLE iValue AS INTEGER NO-UNDO.\n",
    );
    for _ in 0..20_001 {
        big.push_str("iValue = fnOne().\n");
    }
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("big.p", &big)]);
    let path = ws.path("big.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, &big);
    let program = item(&mut session, &path, &big, "\nDEFINE", 0);
    let (result, warnings) = ask(
        &mut session,
        "callHierarchy/outgoingCalls",
        json!({"item": program}),
    );
    assert_eq!(result[0]["to"]["name"], json!("incomplete"), "{result}");
    assert!(
        result[0]["to"]["detail"]
            .as_str()
            .unwrap()
            .contains("more than 20000 calls"),
        "{result}"
    );
    assert_eq!(result[1]["to"]["name"], json!("fnOne"));
    assert_eq!(result[1]["fromRanges"].as_array().unwrap().len(), 20_000);
    assert_eq!(warnings.len(), 1, "{warnings:?}");
}

#[test]
fn a_file_whose_analysis_fails_costs_only_that_file() {
    // The test build panics when the language server's index reads a file
    // with this marker; only `boom.p` names the class.
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("defs.i", DEFS),
        ("main.p", MAIN),
        ("lib/util.p", UTIL),
        ("third.p", THIRD),
        (
            "app/Boom.cls",
            "/* OXABL-TEST-PANIC:lsp.index */\nCLASS app.Boom:\nEND CLASS.\n",
        ),
        (
            "boom.p",
            "DEFINE VARIABLE oBoom AS CLASS app.Boom NO-UNDO.\nRUN lib/util.p.\n",
        ),
    ]);
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let (found, warnings) = references(&mut session, &main, MAIN, "lib/util.p", 0, false);
    assert_eq!(
        found,
        sorted(vec![
            location(&main, MAIN, "lib/util.p", 0),
            location(&main, MAIN, "lib/util.p", 1),
            location(&ws.path("third.p"), THIRD, "lib/util.p", 0),
        ])
    );
    assert_eq!(warnings.len(), 1, "{warnings:?}");
    assert!(
        warnings[0].contains("1 file(s) that can name `util.p` could not be analysed")
            && warnings[0].contains("boom.p"),
        "{warnings:?}"
    );
}
