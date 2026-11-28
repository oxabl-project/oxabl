//! The workspace pass after an edit, through the real `oxabl lsp` and its
//! daemon: an open of a file with its disk text runs no pass, an edit of one
//! file analyses only the files that read it, and every answer is the answer
//! of a new daemon on the same text.
//!
//! A test build of the daemon writes one line to stderr for each pass when
//! `OXABL_TEST_PASS_COUNTS` is set: `oxabl-test: workspace pass analysed <n>
//! of <m> files, kept <bytes> bytes`.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::collections::BTreeMap;
use std::path::PathBuf;

use serde_json::{Value, json};
use session::{Launch, Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"incremental\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const DEFS: &str = "PROCEDURE fromInclude:\n\
    END PROCEDURE.\n";

const MAIN: &str = "{defs.i}\n\
    DEFINE VARIABLE oCalc AS CLASS app.Calc NO-UNDO.\n\
    RUN lib/util.p.\n\
    RUN fromInclude.\n\
    oCalc = NEW app.Calc().\n\
    oCalc:Add(1).\n";

const UTIL: &str = "PROCEDURE doWork:\n\
    \x20   RUN helper.\n\
    END PROCEDURE.\n\
    PROCEDURE helper:\n\
    END PROCEDURE.\n";

const OTHER: &str = "DEFINE VARIABLE oOther AS CLASS app.Calc NO-UNDO.\n\
    RUN lib/util.p.\n\
    oOther = NEW app.Calc().\n\
    oOther:Add(2).\n";

const THIRD: &str = "MESSAGE \"third\".\n";

const CALC: &str = "CLASS app.Calc:\n\
    \x20   METHOD PUBLIC INTEGER Add(INPUT piValue AS INTEGER):\n\
    \x20       RETURN Twice(piValue).\n\
    \x20   END METHOD.\n\
    \x20   METHOD PRIVATE INTEGER Twice(INPUT piValue AS INTEGER):\n\
    \x20       RETURN piValue * 2.\n\
    \x20   END METHOD.\n\
    END CLASS.\n";

const PRODUCER: &str = "DEFINE NEW SHARED VARIABLE gcName AS CHARACTER NO-UNDO.\n\
    gcName = \"a\".\n\
    RUN consumer.p.\n";

const CONSUMER: &str = "DEFINE SHARED VARIABLE gcName AS CHARACTER NO-UNDO.\n\
    MESSAGE gcName.\n";

/// Files that name nothing of the others, so that a full pass analyses many
/// more files than an edit of one file needs.
const FILLERS: usize = 40;

/// The most files an edit of one file in this workspace may analyse: the file
/// and the files that read it.
const BOUND: usize = 4;

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
    /// The text of each file, as the open buffers hold it.
    texts: BTreeMap<&'static str, String>,
}

impl Workspace {
    fn new() -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        let mut texts = BTreeMap::new();
        for (name, text) in [
            ("defs.i", DEFS),
            ("main.p", MAIN),
            ("lib/util.p", UTIL),
            ("other.p", OTHER),
            ("third.p", THIRD),
            ("app/Calc.cls", CALC),
            ("producer.p", PRODUCER),
            ("consumer.p", CONSUMER),
        ] {
            texts.insert(name, text.to_string());
        }
        let write = |name: &str, text: &str| {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        };
        write("oxabl.toml", CONFIG);
        for (name, text) in &texts {
            write(name, text);
        }
        for n in 0..FILLERS {
            write(
                &format!("filler/f{n}.p"),
                &format!("DEFINE VARIABLE i{n} AS INTEGER NO-UNDO.\ni{n} = {n}.\n"),
            );
        }
        Workspace {
            _dir: dir,
            root,
            texts,
        }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }

    /// How many files a pass analyses: every file but the include file.
    fn files(&self) -> usize {
        self.texts
            .keys()
            .filter(|name| !name.ends_with(".i"))
            .count()
            + FILLERS
    }
}

fn start(ws: &Workspace) -> Session {
    let params = json!({
        "capabilities": {
            "textDocument": {"codeLens": {"dynamicRegistration": false}},
            "workspace": {"codeLens": {"refreshSupport": true}}
        },
        "initializationOptions": {"referenceLenses": {"enable": true}}
    });
    let launch = Launch {
        env: &[("OXABL_TEST_PASS_COUNTS", "1")],
        ..Launch::default()
    };
    Session::launch_with(&ws.root, params, launch, |_| {})
        .await_initialize()
        .0
}

/// A session with every file of `ws` open, with the text of its buffer.
fn open_all(ws: &Workspace) -> Session {
    let mut session = start(ws);
    for (name, text) in &ws.texts {
        session.open(&ws.path(name), text);
    }
    session
}

/// `(analysed, files)` of each pass the daemon of `session` ran so far.
fn passes(session: &Session) -> Vec<(usize, usize)> {
    session
        .stderr()
        .lines()
        .filter_map(|line| line.strip_prefix("oxabl-test: workspace pass analysed "))
        .map(|rest| {
            let words: Vec<&str> = rest.split_whitespace().collect();
            (words[0].parse().unwrap(), words[2].parse().unwrap())
        })
        .collect()
}

fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

fn position(ws: &Workspace, name: &str, needle: &str) -> Value {
    let text = &ws.texts[name];
    let at = text
        .find(needle)
        .unwrap_or_else(|| panic!("no `{needle}` in {name}"));
    let (line, character) = utf16_at(text, at + 1);
    json!({
        "textDocument": {"uri": file_uri(&ws.path(name))},
        "position": {"line": line, "character": character}
    })
}

/// Every cross-file answer the test compares, by a name for each.
fn answers(session: &mut Session, ws: &Workspace) -> BTreeMap<String, Value> {
    let all: Vec<&str> = ws.texts.keys().copied().collect();
    answers_in(session, ws, &all)
}

/// The answers of [`answers`] at the open files `open`.
fn answers_in(session: &mut Session, ws: &Workspace, open: &[&str]) -> BTreeMap<String, Value> {
    let mut found = BTreeMap::new();
    let references = [
        ("main.p", "Add("),
        ("main.p", "app.Calc"),
        ("main.p", "fromInclude"),
        ("lib/util.p", "helper:"),
        ("lib/util.p", "doWork"),
        ("consumer.p", "gcName"),
        ("producer.p", "gcName"),
        ("app/Calc.cls", "Twice("),
    ];
    for (name, needle) in references {
        if !open.contains(&name) {
            continue;
        }
        let mut params = position(ws, name, needle);
        params["context"] = json!({"includeDeclaration": true});
        let answer = session.request("textDocument/references", params);
        assert!(answer.get("error").is_none(), "references gave {answer}");
        found.insert(
            format!("references {name} {needle}"),
            sorted(&answer["result"]),
        );
    }
    for (name, needle) in [("app/Calc.cls", "Add("), ("lib/util.p", "helper:")] {
        if !open.contains(&name) {
            continue;
        }
        let items = session.request(
            "textDocument/prepareCallHierarchy",
            position(ws, name, needle),
        );
        assert!(items.get("error").is_none(), "prepare gave {items}");
        for item in items["result"].as_array().cloned().unwrap_or_default() {
            let incoming =
                session.request("callHierarchy/incomingCalls", json!({"item": item.clone()}));
            assert!(incoming.get("error").is_none(), "incoming gave {incoming}");
            found.insert(
                format!("incoming {name} {needle}"),
                sorted(&incoming["result"]),
            );
        }
    }
    for name in open {
        let path = ws.path(name);
        let lenses = session.request(
            "textDocument/codeLens",
            json!({"textDocument": {"uri": file_uri(&path)}}),
        );
        assert!(lenses.get("error").is_none(), "codeLens gave {lenses}");
        let mut titles = Vec::new();
        for lens in lenses["result"].as_array().cloned().unwrap_or_default() {
            let resolved = session.request("codeLens/resolve", lens);
            assert!(resolved.get("error").is_none(), "resolve gave {resolved}");
            titles.push(json!([
                resolved["result"]["range"],
                resolved["result"]["command"]["title"]
            ]));
        }
        found.insert(format!("lenses {name}"), Value::Array(titles));
    }
    found
}

/// A list answer in a fixed order.
fn sorted(value: &Value) -> Value {
    match value.as_array() {
        Some(items) => {
            let mut items = items.clone();
            items.sort_by_key(|item| item.to_string());
            Value::Array(items)
        }
        None => value.clone(),
    }
}

/// Replace the text of the open `name` in `session` and in `ws`.
fn edit(session: &mut Session, ws: &mut Workspace, name: &'static str, text: String, version: u64) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&ws.path(name)), "version": version},
            "contentChanges": [{"text": text}]
        }
    }));
    ws.texts.insert(name, text);
}

/// The answers of a new daemon with the same buffers open.
fn fresh_answers(ws: &Workspace) -> BTreeMap<String, Value> {
    let all: Vec<&str> = ws.texts.keys().copied().collect();
    fresh_answers_in(ws, &all)
}

/// [`fresh_answers`] at the files `open`.
fn fresh_answers_in(ws: &Workspace, open: &[&str]) -> BTreeMap<String, Value> {
    let mut fresh = open_all(ws);
    let found = answers_in(&mut fresh, ws, open);
    let runs = passes(&fresh);
    assert!(
        runs.iter().all(|&(analysed, files)| analysed == files),
        "a new daemon analyses every file: {runs:?}"
    );
    found
}

#[track_caller]
fn assert_same(edited: &BTreeMap<String, Value>, fresh: &BTreeMap<String, Value>, what: &str) {
    for (key, value) in fresh {
        assert_eq!(
            edited.get(key),
            Some(value),
            "after {what}, `{key}` differs from the answer of a new daemon"
        );
    }
    assert_eq!(edited.len(), fresh.len(), "after {what}");
}

#[test]
fn an_open_with_the_disk_text_runs_no_pass() {
    let ws = Workspace::new();
    let mut session = start(&ws);
    let main = ws.path("main.p");
    session.open(&main, &ws.texts["main.p"]);
    let before = answers_in(&mut session, &ws, &["main.p"]);
    let first = passes(&session);
    assert_eq!(first, vec![(ws.files(), ws.files())], "one full pass");

    for name in ["other.p", "lib/util.p", "app/Calc.cls", "defs.i"] {
        session.open(&ws.path(name), &ws.texts[name]);
    }
    let after = answers_in(&mut session, &ws, &["main.p"]);
    let _ = answers_in(
        &mut session,
        &ws,
        &["main.p", "other.p", "lib/util.p", "app/Calc.cls", "defs.i"],
    );
    let runs = passes(&session);
    assert_eq!(
        runs, first,
        "an open of a file with its disk text runs no pass"
    );
    assert_eq!(before, after);
}

#[test]
fn an_edit_of_one_file_analyses_only_the_files_that_read_it() {
    let mut ws = Workspace::new();
    let mut session = open_all(&ws);
    let _ = answers(&mut session, &ws);
    let full = passes(&session).len();

    let edits = [
        (
            "lib/util.p",
            "a procedure file",
            format!(
                "{}PROCEDURE extra:\n    RUN helper.\nEND PROCEDURE.\n",
                ws.texts["lib/util.p"]
            ),
        ),
        (
            "app/Calc.cls",
            "a class",
            ws.texts["app/Calc.cls"].replace(
                "END CLASS.",
                "    METHOD PUBLIC INTEGER Sub(INPUT piValue AS INTEGER):\n        \
                 RETURN Twice(piValue) - Add(piValue).\n    END METHOD.\nEND CLASS.",
            ),
        ),
        (
            "defs.i",
            "an include",
            format!("{}RUN third.p.\n", ws.texts["defs.i"]),
        ),
    ];
    let mut seen = full;
    for (version, (name, what, text)) in (2..).zip(edits) {
        edit(&mut session, &mut ws, name, text, version);
        let edited = answers(&mut session, &ws);
        let runs = passes(&session);
        assert!(runs.len() > seen, "the edit of {what} ran a pass: {runs:?}");
        for &(analysed, files) in &runs[seen..] {
            assert!(
                analysed <= BOUND,
                "the edit of {what} analysed {analysed} of {files} files: {runs:?}"
            );
        }
        seen = runs.len();
        assert_same(&edited, &fresh_answers(&ws), what);
    }
}

#[test]
fn an_edit_of_a_shared_producer_gives_the_answer_of_a_new_daemon() {
    let mut ws = Workspace::new();
    let mut session = open_all(&ws);
    let _ = answers(&mut session, &ws);

    // The producer stops producing `gcName`: the consumer loses its producer.
    let text = ws.texts["producer.p"].replace("NEW SHARED", "");
    edit(&mut session, &mut ws, "producer.p", text, 2);
    let edited = answers(&mut session, &ws);
    assert_same(&edited, &fresh_answers(&ws), "an edit of a SHARED producer");

    // And produces it again.
    edit(&mut session, &mut ws, "producer.p", PRODUCER.to_string(), 3);
    let edited = answers(&mut session, &ws);
    assert_same(
        &edited,
        &fresh_answers(&ws),
        "a second edit of a SHARED producer",
    );
}

#[test]
fn a_save_of_an_edit_analyses_no_file_again() {
    let mut ws = Workspace::new();
    let mut session = open_all(&ws);
    let _ = answers(&mut session, &ws);
    let text = format!(
        "{}PROCEDURE extra:\nEND PROCEDURE.\n",
        ws.texts["lib/util.p"]
    );
    edit(&mut session, &mut ws, "lib/util.p", text.clone(), 2);
    let edited = answers(&mut session, &ws);
    let seen = passes(&session).len();

    // The editor writes the buffer to disk: the stamp of the file changes,
    // and its text is the text the pass read.
    std::fs::write(ws.path("lib/util.p"), &text).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didSave",
        "params": {"textDocument": {"uri": file_uri(&ws.path("lib/util.p"))}}
    }));
    let saved = answers(&mut session, &ws);
    let runs = passes(&session);
    assert!(
        runs[seen..].iter().all(|&(analysed, _)| analysed == 0),
        "a save of the text the pass read analyses no file: {runs:?}"
    );
    assert_eq!(edited, saved);

    // A tool writes a file that no buffer holds with the same text: its stamp
    // changes and its text does not.
    let seen = runs.len();
    let other = ws.path("other.p");
    std::fs::remove_file(&other).unwrap();
    std::fs::write(&other, &ws.texts["other.p"]).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didClose",
        "params": {"textDocument": {"uri": file_uri(&other)}}
    }));
    let _ = answers_in(&mut session, &ws, &["main.p", "lib/util.p"]);
    let runs = passes(&session);
    assert!(
        runs.len() > seen,
        "the rewrite made the index stale: {runs:?}"
    );
    assert!(
        runs[seen..].iter().all(|&(analysed, _)| analysed == 0),
        "a rewrite of a file with the same text analyses no file: {runs:?}"
    );
}

#[test]
fn a_file_changed_on_disk_is_analysed_again() {
    let mut ws = Workspace::new();
    let mut session = start(&ws);
    session.open(&ws.path("main.p"), &ws.texts["main.p"]);
    let _ = answers_in(&mut session, &ws, &["main.p"]);

    // A file that no buffer holds changes on disk: the next search sees it.
    let text = format!(
        "{}RUN lib/util.p.\nRUN fromInclude IN THIS-PROCEDURE.\n",
        THIRD
    );
    std::fs::write(ws.path("third.p"), &text).unwrap();
    ws.texts.insert("third.p", text);
    let changed = answers_in(&mut session, &ws, &["main.p"]);
    assert_same(
        &changed,
        &fresh_answers_in(&ws, &["main.p"]),
        "a change on disk",
    );
}

/// The threads of a pass share the facts of the files they reach, so most
/// analyses of a program that many files run take its facts from another
/// thread's read. Each of them still has to list the program as an input.
#[test]
fn a_program_that_many_files_run_is_analysed_again_for_each_of_them() {
    const CALLERS: usize = 60;
    let mut ws = Workspace::new();
    for n in 0..CALLERS {
        let path = ws.path(&format!("caller/c{n}.p"));
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, format!("RUN lib/util.p.\nMESSAGE \"{n}\".\n")).unwrap();
    }
    let mut session = start(&ws);
    session.open(&ws.path("main.p"), &ws.texts["main.p"]);
    let _ = answers_in(&mut session, &ws, &["main.p"]);
    let seen = passes(&session).len();

    let text = format!("{}PROCEDURE extra:\nEND PROCEDURE.\n", UTIL);
    std::fs::write(ws.path("lib/util.p"), &text).unwrap();
    ws.texts.insert("lib/util.p", text);
    let changed = answers_in(&mut session, &ws, &["main.p"]);

    let runs = passes(&session);
    let &(analysed, _) = runs[seen..].last().expect("the change ran a pass");
    // The program, its two callers in the base workspace and the new callers.
    assert_eq!(
        analysed,
        CALLERS + 3,
        "every file that runs the program was analysed again: {runs:?}"
    );
    assert_same(
        &changed,
        &fresh_answers_in(&ws, &["main.p"]),
        "a change of a program that many files run",
    );
}

/// The names of the workspace symbols that match `query`.
fn symbol_names(session: &mut Session, query: &str) -> Vec<String> {
    let answer = session.request("workspace/symbol", json!({"query": query}));
    assert!(answer.get("error").is_none(), "symbol gave {answer}");
    answer["result"]
        .as_array()
        .cloned()
        .unwrap_or_default()
        .iter()
        .map(|row| row["name"].as_str().unwrap_or_default().to_string())
        .collect()
}

/// The client says that a file came (`1`) or went (`3`).
fn watched(session: &mut Session, ws: &Workspace, name: &str, kind: u8) {
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&ws.path(name)), "type": kind}]}
    }));
}

/// A pass after an edit of a buffer keeps the list of files of the pass
/// before, so a file that came or went is in the next pass only when the
/// client said so. `workspace/symbol` asks for a pass and checks no walk of
/// its own.
#[test]
fn a_file_that_the_client_says_came_or_went_is_in_the_next_pass() {
    let mut ws = Workspace::new();
    let mut session = start(&ws);
    session.open(&ws.path("main.p"), &ws.texts["main.p"]);
    assert!(symbol_names(&mut session, "newProc").is_empty());

    let new = "PROCEDURE newProc:\nEND PROCEDURE.\n";
    std::fs::create_dir_all(ws.path("fresh")).unwrap();
    std::fs::write(ws.path("fresh/new.p"), new).unwrap();
    watched(&mut session, &ws, "fresh/new.p", 1);
    // An edit of the open file makes the next request run a pass.
    let text = format!("{}MESSAGE \"one\".\n", ws.texts["main.p"]);
    edit(&mut session, &mut ws, "main.p", text, 2);
    assert_eq!(symbol_names(&mut session, "newProc"), ["newProc"]);

    std::fs::remove_file(ws.path("fresh/new.p")).unwrap();
    watched(&mut session, &ws, "fresh/new.p", 3);
    let text = format!("{}MESSAGE \"two\".\n", ws.texts["main.p"]);
    edit(&mut session, &mut ws, "main.p", text, 3);
    assert!(
        symbol_names(&mut session, "newProc").is_empty(),
        "the symbols of a file that is gone are gone"
    );
}

#[test]
fn a_file_that_cannot_be_read_is_analysed_again_until_it_reads() {
    use std::os::unix::fs::PermissionsExt;
    let mut ws = Workspace::new();
    // Through the include, `main.p` runs `third.p`.
    let defs = format!("{DEFS}RUN third.p.\n");
    std::fs::write(ws.path("defs.i"), &defs).unwrap();
    ws.texts.insert("defs.i", defs);
    let set_mode = |mode| {
        std::fs::set_permissions(ws.path("defs.i"), std::fs::Permissions::from_mode(mode)).unwrap()
    };
    set_mode(0o000);
    if std::fs::read(ws.path("defs.i")).is_ok() {
        // A user that reads every file (root) cannot run this test.
        set_mode(0o644);
        return;
    }
    let mut session = start(&ws);
    let open = ["main.p", "third.p"];
    for name in open {
        session.open(&ws.path(name), &ws.texts[name]);
    }
    let unreadable = answers_in(&mut session, &ws, &open);

    set_mode(0o644);
    let readable = answers_in(&mut session, &ws, &open);
    assert_ne!(unreadable, readable, "the include changes the answers");
    assert_same(
        &readable,
        &fresh_answers_in(&ws, &open),
        "the include reads again",
    );
}

#[test]
fn a_pass_after_a_change_of_the_configuration_is_full() {
    let mut ws = Workspace::new();
    // A second `app.Calc`, found first once `alt` comes first on the path.
    let alt = ws.path("alt/app/Calc.cls");
    std::fs::create_dir_all(alt.parent().unwrap()).unwrap();
    std::fs::write(&alt, CALC.replace("Twice", "Double")).unwrap();
    let mut session = open_all(&ws);
    let _ = answers(&mut session, &ws);
    let seen = passes(&session).len();

    // The file changes on disk and no client says so. An edit then makes the
    // next request run a pass, which reads the new configuration.
    std::fs::write(
        ws.path("oxabl.toml"),
        CONFIG.replace("[\".\"]", "[\"alt\", \".\"]"),
    )
    .unwrap();
    // The analysis of the open files keeps the configuration the session
    // installed; only the pass reads the file again.
    let text = format!("{}\n", ws.texts["third.p"]);
    edit(&mut session, &mut ws, "third.p", text, 2);
    let _ = answers(&mut session, &ws);
    let runs = passes(&session);
    assert!(
        runs[seen..]
            .iter()
            .any(|&(analysed, files)| analysed == files),
        "the pass after a change of the configuration analysed every file: {runs:?}"
    );
}
