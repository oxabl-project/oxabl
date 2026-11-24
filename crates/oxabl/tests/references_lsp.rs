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
    let (result, shown, _) = ask_logged(session, method, params);
    (result, shown)
}

/// The answer to one request, every `window/showMessage` and every
/// `window/logMessage` from oxabl about an incomplete answer sent before it.
fn ask_logged(
    session: &mut Session,
    method: &str,
    params: Value,
) -> (Value, Vec<String>, Vec<String>) {
    let answer = send_and_wait(session, method, params);
    assert!(
        answer.0.get("error").is_none(),
        "`{method}` gave an error: {}",
        answer.0
    );
    (answer.0["result"].clone(), answer.1, answer.2)
}

/// Send a request and wait for its response, whatever it holds.
fn send_and_wait(
    session: &mut Session,
    method: &str,
    params: Value,
) -> (Value, Vec<String>, Vec<String>) {
    let id = 9_000 + next_id();
    session.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
    wait_answer(session, id)
}

fn wait_answer(session: &mut Session, id: u64) -> (Value, Vec<String>, Vec<String>) {
    let mut shown = Vec::new();
    let mut logged = Vec::new();
    loop {
        let message = session.wait_for(|_| true);
        let text = message["params"]["message"].as_str().unwrap_or_default();
        if message["method"] == json!("window/showMessage") {
            shown.push(text.to_string());
        }
        if message["method"] == json!("window/logMessage") && text.contains("may be incomplete") {
            logged.push(text.to_string());
        }
        if message["id"] == json!(id) && message.get("method").is_none() {
            return (message, shown, logged);
        }
    }
}

/// The gap that every request for an internal procedure or function has.
const HANDLES: &str = "a RUN ... IN a handle that a file gets from elsewhere";

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

    // Callers in the main block are the program itself. Another program can
    // run a routine through a handle oxabl cannot follow, so the answer says
    // it can be short.
    let fn_twice = item(&mut session, &main, MAIN, "fnTwice(2)", 0);
    let (found, _) = calls(&mut session, "incoming", &fn_twice);
    assert_eq!(
        found,
        sorted_calls(vec![
            call("incomplete", NULL, &main, vec![]),
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
            call("incomplete", NULL, &util, vec![]),
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
    let (found, _) = references(&mut session, &main, MAIN, "fnTwice(2)", 0, false);
    assert_eq!(
        found,
        sorted(vec![
            location(&main, MAIN, "fnTwice", 0),
            location(&main, MAIN, "fnTwice", 1)
        ])
    );
    // The whole search of other files is left out: shown on each request.
    for _ in 0..2 {
        let (_, warnings) = references(&mut session, &main, MAIN, "fnTwice(2)", 0, false);
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert!(
            warnings[0].contains("other files are not searched")
                && warnings[0].contains("no include path"),
            "{warnings:?}"
        );
    }
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
    let mut params = position(&main, MAIN, "lib/util.p", 0);
    params["context"] = json!({"includeDeclaration": false});
    let (found, warnings, logged) =
        ask_logged(&mut session, "textDocument/references", params.clone());
    assert_eq!(found.as_array().unwrap().len(), 2, "{found}");
    // A gap of the workspace, the same for every request: logged once per
    // session, not shown.
    assert_eq!(warnings, Vec::<String>::new());
    assert_eq!(logged.len(), 1, "{logged:?}");
    assert!(
        logged[0].contains("1 file(s) of the workspace have an incomplete dependency set"),
        "{logged:?}"
    );
    let (_, warnings, logged) = ask_logged(&mut session, "textDocument/references", params);
    assert_eq!(warnings, Vec::<String>::new());
    assert_eq!(logged, Vec::<String>::new(), "logged again");
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

/// References with the declaration, sorted, and the warnings and log lines.
fn references_logged(
    session: &mut Session,
    file: &Path,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> (Vec<Value>, Vec<String>, Vec<String>) {
    let mut params = position(file, text, needle, occurrence);
    params["context"] = json!({"includeDeclaration": true});
    let (result, shown, logged) = ask_logged(session, "textDocument/references", params);
    let found = match result {
        Value::Null => Vec::new(),
        Value::Array(found) => found,
        other => panic!("references gave {other}"),
    };
    (sorted(found), shown, logged)
}

const COLOR: &str = "ENUM app.Color:\n    DEFINE ENUM Red Green.\nEND ENUM.\n";
const PAINT: &str = "DEFINE VARIABLE eColor AS app.Color NO-UNDO.\n\
    eColor = app.Color:Red.\n\
    eColor = app.Color:Green.\n\
    RUN app/Color.cls.\n";
const PAINT_TOO: &str = "DEFINE VARIABLE eOther AS app.Color NO-UNDO.\neOther = app.Color:Red.\n";

#[test]
fn an_enum_member_is_its_own_referent() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Color.cls", COLOR),
        ("paint.p", PAINT),
        ("paint-too.p", PAINT_TOO),
    ]);
    let paint = ws.path("paint.p");
    let mut session = Session::start(&ws.root);
    session.open(&paint, PAINT);
    let (found, shown, _) = references_logged(&mut session, &paint, PAINT, "Red", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&paint, PAINT, "Red", 0),
            location(&ws.path("paint-too.p"), PAINT_TOO, "Red", 0),
            location(&ws.path("app/Color.cls"), COLOR, "Red", 0),
        ])
    );
    assert_eq!(shown, Vec::<String>::new());
}

const MIXED: &str = "CLASS app.Mixed:\n{mixed-members.i}\n\
    METHOD PUBLIC VOID Go():\n    Inc().\nEND METHOD.\nEND CLASS.\n";
const MIXED_MEMBERS: &str = "METHOD PUBLIC VOID Inc():\nEND METHOD.\n\
    METHOD PUBLIC VOID Dec():\nEND METHOD.\n";
const MIXER: &str = "DEFINE VARIABLE oMix AS app.Mixed NO-UNDO.\n\
    oMix = NEW app.Mixed().\n\
    oMix:Inc().\n\
    oMix:Dec().\n";

#[test]
fn a_member_written_in_an_include_of_its_class_is_one_member() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Mixed.cls", MIXED),
        ("mixed-members.i", MIXED_MEMBERS),
        ("mixer.p", MIXER),
    ]);
    let mixer = ws.path("mixer.p");
    let mixed = ws.path("app/Mixed.cls");
    let members = ws.path("mixed-members.i");
    // Not `Dec`, which is also written in the include.
    let expected = sorted(vec![
        location(&mixer, MIXER, "Inc", 0),
        location(&mixed, MIXED, "Inc", 0),
        location(&members, MIXED_MEMBERS, "Inc", 0),
    ]);
    let mut session = Session::start(&ws.root);
    for (file, text, needle) in [
        (&mixer, MIXER, "Inc"),
        (&mixed, MIXED, "Inc()"),
        (&members, MIXED_MEMBERS, "Inc"),
    ] {
        session.open(file, text);
        let (found, shown, _) = references_logged(&mut session, file, text, needle, 0);
        assert_eq!(found, expected, "from {}", file.display());
        assert_eq!(shown, Vec::<String>::new(), "from {}", file.display());
    }
}

const PROCS: &str = "PROCEDURE incProc:\nEND PROCEDURE.\n";
const LIBRARY: &str = "{procs.i}\nRUN incProc.\n";
const CALLER: &str = "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    RUN library.p PERSISTENT SET hLib.\n\
    RUN incProc IN hLib.\n";

#[test]
fn a_procedure_from_an_include_of_the_program_is_found() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("procs.i", PROCS),
        ("library.p", LIBRARY),
        ("caller.p", CALLER),
    ]);
    let caller = ws.path("caller.p");
    let mut session = Session::start(&ws.root);
    session.open(&caller, CALLER);
    let (found, shown, logged) = references_logged(&mut session, &caller, CALLER, "incProc", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&caller, CALLER, "incProc", 0),
            location(&ws.path("library.p"), LIBRARY, "incProc", 0),
            location(&ws.path("procs.i"), PROCS, "incProc", 0),
        ])
    );
    assert_eq!(shown, Vec::<String>::new());
    // Calls through a handle the graph cannot see stay possible: logged.
    assert!(
        logged.iter().any(|line| line.contains(HANDLES)),
        "{logged:?}"
    );
}

#[test]
fn references_past_the_call_limit_of_a_unit_say_so() {
    let mut big = String::from(
        "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\nRUN lib/util.p PERSISTENT SET hLib.\n",
    );
    for _ in 0..10_001 {
        big.push_str("RUN doWork IN hLib.\nRUN lib/util.p.\n");
    }
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib/util.p", UTIL),
        ("big.p", &big),
    ]);
    let path = ws.path("big.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, &big);
    let (_, shown, _) = references_logged(&mut session, &path, &big, "doWork", 0);
    assert!(
        shown
            .iter()
            .any(|line| line.contains("big.p has more than 20000 calls")),
        "{shown:?}"
    );
}

const MACRO: &str = "&SCOPED-DEFINE DOIT RUN lib/util.p\n{&DOIT}.\nRUN lib/util.p.\n";

#[test]
fn a_call_written_through_a_preprocessor_name_is_placed_at_the_name() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib/util.p", UTIL),
        ("macro.p", MACRO),
    ]);
    let path = ws.path("macro.p");
    let util = ws.path("lib/util.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, MACRO);
    let (found, shown, _) = references_logged(&mut session, &path, MACRO, "lib/util.p", 1);
    assert_eq!(
        found,
        sorted(vec![
            location(&path, MACRO, "{&DOIT}", 0),
            location(&path, MACRO, "lib/util.p", 1),
        ])
    );
    assert_eq!(shown, Vec::<String>::new());

    let program = item(&mut session, &path, MACRO, "lib/util.p", 1);
    let (found, _) = calls(&mut session, "incoming", &program);
    assert_eq!(
        found,
        sorted_calls(vec![call(
            "macro.p",
            FILE,
            &path,
            vec![range(MACRO, "{&DOIT}", 0), range(MACRO, "lib/util.p", 1)]
        )])
    );
    let whole = item(&mut session, &path, MACRO, "RUN lib", 0);
    let (found, _) = calls(&mut session, "outgoing", &whole);
    assert_eq!(
        found,
        sorted_calls(vec![call(
            "util.p",
            FILE,
            &util,
            vec![range(MACRO, "{&DOIT}", 0), range(MACRO, "lib/util.p", 1)]
        )])
    );
}

#[test]
fn incoming_calls_of_a_class_are_the_places_that_name_it() {
    let ws = Workspace::standard();
    let calc = ws.path("app/Calc.cls");
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&calc, CALC);
    let class = item(&mut session, &calc, CALC, "CLASS app", 0);
    assert_eq!(class["name"], json!("Calc.cls"));
    let (found, shown) = calls(&mut session, "incoming", &class);
    assert_eq!(
        found,
        sorted_calls(vec![
            call(
                "main.p",
                FILE,
                &main,
                vec![range(MAIN, "app.Calc", 0), range(MAIN, "app.Calc", 1)]
            ),
            call(
                "other.p",
                FILE,
                &ws.path("other.p"),
                vec![range(OTHER, "app.Calc", 0), range(OTHER, "app.Calc", 1)]
            ),
        ])
    );
    assert_eq!(shown, Vec::<String>::new());
}

const WORKER: &str = "CLASS app.Worker:\n\
    \x20   METHOD PUBLIC VOID Work():\n\
    \x20       DEFINE VARIABLE iTmp AS INTEGER NO-UNDO.\n\
    \x20       iTmp = 1.\n\
    \x20       iTmp = iTmp + 1.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

#[test]
fn a_variable_of_a_method_is_searched_only_in_its_class() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("app/Worker.cls", WORKER),
        (
            "uses-worker.p",
            "DEFINE VARIABLE o AS app.Worker NO-UNDO.\n",
        ),
        ("lost.p", "{missing.i}\n"),
    ]);
    let worker = ws.path("app/Worker.cls");
    let mut session = Session::start(&ws.root);
    session.open(&worker, WORKER);
    let (found, shown, logged) = references_logged(&mut session, &worker, WORKER, "iTmp = 1", 0);
    let expected: Vec<Value> = (0..4)
        .map(|n| location(&worker, WORKER, "iTmp", n))
        .collect();
    assert_eq!(found, sorted(expected));
    // No search of other files, so no gap of the workspace either.
    assert_eq!(shown, Vec::<String>::new());
    assert_eq!(logged, Vec::<String>::new());
}

const SHARED_MAIN: &str = "DEFINE NEW SHARED VARIABLE gsTotal AS INTEGER NO-UNDO.\n\
    RUN shared-sub.p.\n\
    gsTotal = 1.\n";
const SHARED_SUB: &str = "DEFINE SHARED VARIABLE gsTotal AS INTEGER NO-UNDO.\ngsTotal = 2.\n";
const SHOP: &str = "ADD TABLE \"Item\"\n  AREA \"Data\"\n\n\
    ADD FIELD \"ItemNum\" OF \"Item\" AS integer\n  FORMAT \">>>>9\"\n\n";
const READS: &str = "FIND FIRST Item NO-LOCK NO-ERROR.\nDISPLAY Item.ItemNum.\n";
const READS_TOO: &str = "FOR EACH Item NO-LOCK:\n    DISPLAY Item.ItemNum.\nEND.\n";
const VARS: &str = "DEFINE VARIABLE gcName AS CHARACTER NO-UNDO.\n";
const USES_VARS: &str = "{vars.i}\ngcName = \"a\".\n";
const USES_VARS_TOO: &str = "{vars.i}\ngcName = \"b\".\n";

#[test]
fn names_other_units_share_are_searched_or_said_to_be_short() {
    let config = "[workspace]\nname = \"references\"\n\
        [workspace.sources]\ninclude_paths = [\".\"]\n\
        [workspace.schema]\nfiles = [\"db/shop.df\"]\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", config),
        ("db/shop.df", SHOP),
        ("shared-main.p", SHARED_MAIN),
        ("shared-sub.p", SHARED_SUB),
        ("reads.p", READS),
        ("reads-too.p", READS_TOO),
        ("vars.i", VARS),
        ("uses-vars.p", USES_VARS),
        ("uses-vars-too.p", USES_VARS_TOO),
    ]);
    let mut session = Session::start(&ws.root);

    // A shared variable: other programs that define it SHARED are not searched.
    let shared = ws.path("shared-main.p");
    session.open(&shared, SHARED_MAIN);
    let (found, shown, _) = references_logged(&mut session, &shared, SHARED_MAIN, "gsTotal", 0);
    assert_eq!(found.len(), 2, "{found:?}");
    assert_eq!(shown.len(), 1, "{shown:?}");
    assert!(shown[0].contains("`gsTotal` is shared"), "{shown:?}");

    // A database field: each file that reads its table.
    let reads = ws.path("reads.p");
    session.open(&reads, READS);
    let (found, shown, _) = references_logged(&mut session, &reads, READS, "ItemNum", 0);
    let df = json!(file_uri(&ws.path("db/shop.df")));
    let (declared, used): (Vec<Value>, Vec<Value>) = found
        .into_iter()
        .partition(|location| location["uri"] == df);
    assert_eq!(
        used,
        sorted(vec![
            location(&reads, READS, "ItemNum", 0),
            location(&ws.path("reads-too.p"), READS_TOO, "ItemNum", 0),
        ])
    );
    assert_eq!(declared.len(), 1, "the ADD FIELD in the .df: {declared:?}");
    assert_eq!(shown, Vec::<String>::new());

    // A variable of an include: each unit that expands the include.
    let uses = ws.path("uses-vars.p");
    session.open(&uses, USES_VARS);
    let (found, shown, _) = references_logged(&mut session, &uses, USES_VARS, "gcName", 0);
    assert_eq!(
        found,
        sorted(vec![
            location(&uses, USES_VARS, "gcName", 0),
            location(&ws.path("uses-vars-too.p"), USES_VARS_TOO, "gcName", 0),
            location(&ws.path("vars.i"), VARS, "gcName", 0),
        ])
    );
    assert_eq!(shown, Vec::<String>::new());
}

const SUPERED: &str = "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    RUN lib/util.p PERSISTENT SET hLib.\n\
    RUN notThere IN hLib.\n";

#[test]
fn the_calls_of_a_procedure_a_super_procedure_supplies_are_unknown() {
    let ws = Workspace::new(&[
        ("oxabl.toml", CONFIG),
        ("lib/util.p", UTIL),
        ("supered.p", SUPERED),
    ]);
    let path = ws.path("supered.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, SUPERED);
    let undeclared = item(&mut session, &path, SUPERED, "notThere", 0);
    assert_eq!(undeclared["uri"], json!(file_uri(&ws.path("lib/util.p"))));
    let (result, shown) = ask(
        &mut session,
        "callHierarchy/outgoingCalls",
        json!({"item": undeclared}),
    );
    let calls = result.as_array().unwrap();
    assert_eq!(calls.len(), 1, "{result}");
    assert_eq!(calls[0]["to"]["name"], json!("incomplete"));
    let detail = calls[0]["to"]["detail"].as_str().unwrap();
    assert!(
        detail.contains("a super procedure of util.p supplies `notthere`"),
        "{detail}"
    );
    assert!(!detail.contains("no longer declares"), "{detail}");
    assert_eq!(shown.len(), 1, "{shown:?}");
}

#[test]
fn a_workspace_index_that_is_out_of_date_is_rebuilt_or_said_to_be() {
    let ws = Workspace::standard();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);
    let (found, _, _) = references_logged(&mut session, &main, MAIN, "lib/util.p", 0);
    assert_eq!(found.len(), 4, "{found:?}");

    // A caller written on disk after the pass, with no notice to the server.
    // A search reuses a walk for added files for one second.
    let late = ws.path("late.p");
    std::fs::write(&late, THIRD).unwrap();
    std::thread::sleep(std::time::Duration::from_millis(1_100));
    let (found, shown, logged) = references_logged(&mut session, &main, MAIN, "lib/util.p", 0);
    assert!(
        found.contains(&location(&late, THIRD, "lib/util.p", 0)),
        "the pass was not run again: {found:?}"
    );
    assert_eq!(shown, Vec::<String>::new());
    assert!(
        !logged.iter().any(|line| line.contains("out of date")),
        "{logged:?}"
    );

    // A file that can no longer be read changes the index: the pass runs
    // again, and the answer says the file could not be analysed.
    let third = ws.path("third.p");
    std::fs::set_permissions(&third, std::os::unix::fs::PermissionsExt::from_mode(0o000)).unwrap();
    let (_, _, logged) = references_logged(&mut session, &main, MAIN, "lib/util.p", 0);
    std::fs::set_permissions(&third, std::os::unix::fs::PermissionsExt::from_mode(0o644)).unwrap();
    assert!(
        logged
            .iter()
            .any(|line| line.contains("1 file(s) of the workspace could not be analysed")),
        "{logged:?}"
    );
}

const WIDE_UTIL: &str = "PROCEDURE doWork:\nEND PROCEDURE.\n\
    PROCEDURE again:\n    RUN doWork.\nEND PROCEDURE.\n";
const WIDE_CALLER: &str = "DEFINE VARIABLE hLib AS HANDLE NO-UNDO.\n\
    RUN lib/util.p PERSISTENT SET hLib.\n\
    RUN doWork IN hLib.\n";

/// More callers than one search analyses, named to sort before `lib/`.
fn wide_workspace() -> Workspace {
    let names: Vec<String> = (0..2_050).map(|n| format!("a{n:04}.p")).collect();
    let mut files: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG), ("lib/util.p", WIDE_UTIL)];
    files.extend(names.iter().map(|name| (name.as_str(), WIDE_CALLER)));
    Workspace::new(&files)
}

#[test]
fn a_search_past_the_limit_keeps_the_declaring_program() {
    let ws = wide_workspace();
    let caller = ws.path("a0000.p");
    let util = ws.path("lib/util.p");
    let mut session = Session::start(&ws.root);
    session.open(&caller, WIDE_CALLER);
    let (found, shown, _) = references_logged(&mut session, &caller, WIDE_CALLER, "doWork", 0);
    assert!(
        found.contains(&location(&util, WIDE_UTIL, "doWork", 1)),
        "the call in the program that declares it is missing"
    );
    assert!(
        shown
            .iter()
            .any(|line| line.contains("the search stopped after 2000 of the 2050 files")),
        "{shown:?}"
    );
}

/// Callers with more text, so a search over them takes a while.
fn heavy_workspace() -> Workspace {
    let mut caller = String::from(WIDE_CALLER);
    caller.push_str("DEFINE VARIABLE iWork AS INTEGER NO-UNDO.\n");
    for _ in 0..400 {
        caller.push_str("iWork = iWork + 1.\n");
    }
    let names: Vec<String> = (0..2_000).map(|n| format!("a{n:04}.p")).collect();
    let mut files: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG), ("lib/util.p", WIDE_UTIL)];
    files.extend(names.iter().map(|name| (name.as_str(), caller.as_str())));
    Workspace::new(&files)
}

/// A workspace whose pass is long: many heavy files that do not name the
/// program, and a few callers, so a search after the pass is short.
fn long_pass_workspace() -> Workspace {
    let mut other = String::from("DEFINE VARIABLE iWork AS INTEGER NO-UNDO.\n");
    for _ in 0..400 {
        other.push_str("iWork = iWork + 1.\n");
    }
    let names: Vec<String> = (0..3_000).map(|n| format!("b{n:04}.p")).collect();
    let callers: Vec<String> = (0..5).map(|n| format!("a{n:04}.p")).collect();
    let mut files: Vec<(&str, &str)> = vec![("oxabl.toml", CONFIG), ("lib/util.p", WIDE_UTIL)];
    files.extend(callers.iter().map(|name| (name.as_str(), WIDE_CALLER)));
    files.extend(names.iter().map(|name| (name.as_str(), other.as_str())));
    Workspace::new(&files)
}

/// Ask for the references of `doWork` in `a0000.p`, cancel after `delay` when
/// one is given, and return the answer and how long it took.
fn timed_references(
    session: &mut Session,
    caller: &Path,
    text: &str,
    cancel_after: Option<std::time::Duration>,
) -> (Value, std::time::Duration) {
    let id = 9_000 + next_id();
    let mut params = position(caller, text, "doWork", 0);
    params["context"] = json!({"includeDeclaration": true});
    let started = std::time::Instant::now();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/references", "params": params
    }));
    if let Some(delay) = cancel_after {
        std::thread::sleep(delay);
        session.send(json!({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}}));
    }
    let (answer, _, _) = wait_answer(session, id);
    (answer, started.elapsed())
}

#[test]
fn a_cancel_stops_the_wait_for_the_workspace_pass() {
    let ws = long_pass_workspace();
    let caller = ws.path("a0000.p");
    let text = std::fs::read_to_string(&caller).unwrap();
    let mut session = Session::start(&ws.root);
    session.open(&caller, &text);
    // The first search waits for the workspace pass.
    let delay = std::time::Duration::from_millis(100);
    let (cancelled, took) = timed_references(&mut session, &caller, &text, Some(delay));
    assert_eq!(cancelled["error"]["code"], json!(-32800), "{cancelled}");
    // The pass goes on; the next request waits for the rest of it, and searches.
    let (answer, rest) = timed_references(&mut session, &caller, &text, None);
    assert!(answer.get("error").is_none(), "{answer}");
    eprintln!("the cancel took {took:?}, the rest of the pass and a search {rest:?}");
    assert!(
        took * 3 < took + rest,
        "the cancel took {took:?}, the rest of the pass and a search {rest:?}"
    );
}

#[test]
fn a_cancel_stops_a_search_between_files() {
    let ws = heavy_workspace();
    let caller = ws.path("a0000.p");
    let text = std::fs::read_to_string(&caller).unwrap();
    let mut session = Session::start(&ws.root);
    session.open(&caller, &text);
    let (answer, full) = timed_references(&mut session, &caller, &text, None);
    assert!(answer.get("error").is_none(), "{answer}");
    let (full, _) = (
        full.min(timed_references(&mut session, &caller, &text, None).1),
        (),
    );
    let (cancelled, took) = timed_references(&mut session, &caller, &text, Some(full / 10));
    assert_eq!(cancelled["error"]["code"], json!(-32800), "{cancelled}");
    assert!(
        took * 2 < full,
        "the cancel took {took:?}, a whole search {full:?}"
    );
}

#[test]
fn a_cancelled_search_answers_request_cancelled() {
    let ws = wide_workspace();
    let caller = ws.path("a0000.p");
    let mut session = Session::start(&ws.root);
    session.open(&caller, WIDE_CALLER);
    let id = 9_000 + next_id();
    let mut params = position(&caller, WIDE_CALLER, "doWork", 0);
    params["context"] = json!({"includeDeclaration": true});
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/references", "params": params
    }));
    session.send(json!({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}}));
    let (answer, _, _) = wait_answer(&mut session, id);
    assert_eq!(answer["error"]["code"], json!(-32800), "{answer}");
}

const SHOP_FIELDS: &str =
    "FIND FIRST Item NO-LOCK NO-ERROR.\nDISPLAY ItemNum.\nDISPLAY Item.ItemNum.\n";

#[test]
fn a_name_like_the_referent_that_does_not_resolve_is_counted() {
    let config = "[workspace]\nname = \"references\"\n\
        [workspace.sources]\ninclude_paths = [\".\"]\n\
        [workspace.schema]\nfiles = [\"db/shop.df\"]\n";
    let ws = Workspace::new(&[
        ("oxabl.toml", config),
        ("db/shop.df", SHOP),
        ("fields.p", SHOP_FIELDS),
        ("reads-too.p", READS_TOO),
    ]);
    let path = ws.path("fields.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, SHOP_FIELDS);
    let (_, shown, _) = references_logged(&mut session, &path, SHOP_FIELDS, "ItemNum.\n", 1);
    assert_eq!(shown.len(), 1, "{shown:?}");
    assert!(
        shown[0].contains("1 name(s) `itemnum` in fields.p did not resolve"),
        "{shown:?}"
    );
}

const SHARED_TT: &str = "DEFINE NEW SHARED TEMP-TABLE ttShared NO-UNDO\n    FIELD amount AS INTEGER.\n\
    CREATE ttShared.\nttShared.amount = 1.\n";

#[test]
fn a_field_of_a_shared_temp_table_is_said_to_be_short() {
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("tt.p", SHARED_TT)]);
    let path = ws.path("tt.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, SHARED_TT);
    let (found, shown, _) = references_logged(&mut session, &path, SHARED_TT, "amount = 1", 0);
    assert_eq!(found.len(), 2, "{found:?}");
    assert_eq!(shown.len(), 1, "{shown:?}");
    assert!(
        shown[0].contains("`amount` is a field of the shared temp-table `ttShared`"),
        "{shown:?}"
    );
}

/// A program with `n` variables and `n` calls of one function.
fn many_calls(n: usize) -> String {
    let mut text =
        String::from("FUNCTION fnOne RETURNS INTEGER ():\n    RETURN 1.\nEND FUNCTION.\n");
    for at in 0..n {
        text.push_str(&format!("DEFINE VARIABLE v{at} AS INTEGER NO-UNDO.\n"));
    }
    for at in 0..n {
        text.push_str(&format!("v{at} = fnOne().\n"));
    }
    text
}

/// The shortest of three times for the outgoing calls of a program of `n`.
fn outgoing_time(n: usize) -> std::time::Duration {
    let text = many_calls(n);
    let ws = Workspace::new(&[("oxabl.toml", CONFIG), ("many.p", &text)]);
    let path = ws.path("many.p");
    let mut session = Session::start(&ws.root);
    session.open(&path, &text);
    let program = item(&mut session, &path, &text, "\nDEFINE", 0);
    (0..3)
        .map(|_| {
            let started = std::time::Instant::now();
            let (result, _) = ask(
                &mut session,
                "callHierarchy/outgoingCalls",
                json!({"item": program}),
            );
            assert_eq!(result[0]["fromRanges"].as_array().unwrap().len(), n);
            started.elapsed()
        })
        .min()
        .unwrap()
}

#[test]
fn outgoing_calls_cost_about_linear_time_in_the_calls() {
    let small = outgoing_time(500);
    let large = outgoing_time(8_000);
    eprintln!("outgoing calls: 500 in {small:?}, 8,000 in {large:?}");
    // Linear is about 16 times; a cost per call that grows with the unit is
    // about 256 times.
    assert!(
        large < small * 40,
        "500 calls took {small:?}, 8,000 took {large:?}"
    );
}
