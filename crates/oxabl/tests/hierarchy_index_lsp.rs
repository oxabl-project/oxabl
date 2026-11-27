//! The subtype index through the real `oxabl lsp` and its daemon: the first
//! request of a session builds it, from then on the daemon builds it in the
//! background after each workspace pass, it is kept while nothing it depends
//! on changes, and a request that must wait for it shows progress
//! (`docs/design/type-hierarchy.md`).
//!
//! A test build of the daemon writes lines to stderr when
//! `OXABL_TEST_INDEX_COUNTS` is set: `oxabl-test: subtype index <built|stopped|
//! failed> for <n> class files, started by the <pass|request>` for each build
//! as it ends (`built` is an index that is kept), and `oxabl-test: subtype index holds` when a build
//! waits at a class file that has the marker
//! `OXABL-TEST-HOLD:daemon.hierarchy-index@<ms>`. The tests count these lines.
//! A class file with `OXABL-TEST-PANIC:daemon.hierarchy-index` makes the build
//! panic. A source with `OXABL-TEST-HOLD:daemon.pass-file@<ms>` makes the
//! workspace pass wait while it analyses that file.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use serde_json::{Value, json};
use session::{Launch, Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"index\"\n\
    [workspace.sources]\ninclude_paths = [\".\"]\n";

const BASE: &str = "CLASS app.Base:\nEND CLASS.\n";

fn sub(name: &str, parent: &str) -> String {
    format!("CLASS app.{name} INHERITS app.{parent}:\nEND CLASS.\n")
}

/// A class that makes the build wait for `ms` while it reads this file.
fn held_sub(name: &str, ms: u64) -> String {
    format!(
        "/* OXABL-TEST-HOLD:daemon.hierarchy-index@{ms} */\n\
         CLASS app.{name} INHERITS app.Base:\nEND CLASS.\n"
    )
}

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        let mut all = vec![("oxabl.toml", CONFIG)];
        all.extend_from_slice(files);
        for (name, text) in all {
            let path = root.join(name);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.root.join(name)
    }
}

/// A session whose daemon writes the lines that the tests count.
fn start(root: &Path, capabilities: Value) -> Session {
    let params = json!({"capabilities": capabilities});
    let launch = Launch {
        env: &[("OXABL_TEST_INDEX_COUNTS", "1")],
        ..Launch::default()
    };
    Session::launch_with(root, params, launch, |_| {})
        .await_initialize()
        .0
}

/// The lines that the daemon wrote about the index.
fn lines(session: &Session) -> Vec<String> {
    session
        .stderr()
        .lines()
        .filter(|line| line.starts_with("oxabl-test: subtype index"))
        .map(str::to_string)
        .collect()
}

/// The indexes that were built and kept so far.
fn built(session: &Session) -> Vec<String> {
    lines(session)
        .into_iter()
        .filter(|line| line.contains(" built for "))
        .collect()
}

/// Wait until `wanted` holds for the lines of the daemon. A bound for a
/// failure, not a measure: the lines come when the work is done.
#[track_caller]
fn wait_for_lines(session: &Session, what: &str, wanted: impl Fn(&[String]) -> bool) {
    let deadline = Instant::now() + Duration::from_secs(30);
    while Instant::now() < deadline {
        if wanted(&lines(session)) {
            return;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    panic!("no {what} in 30 s; the lines are {:?}", lines(session));
}

fn utf16_at(text: &str, at: usize) -> (u64, u64) {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    (line as u64, character as u64)
}

/// The cursor one character into the first `needle` of `text`.
fn position(file: &Path, text: &str, needle: &str) -> Value {
    let at = text.find(needle).unwrap_or_else(|| panic!("no `{needle}`")) + 1;
    let (line, character) = utf16_at(text, at);
    json!({
        "textDocument": {"uri": file_uri(file)},
        "position": {"line": line, "character": character}
    })
}

/// What a request produced: the messages before its answer, and the answer.
struct Exchange {
    before: Vec<Value>,
    answer: Value,
}

impl Exchange {
    /// The `$/progress` values, in order.
    fn progress(&self) -> Vec<&Value> {
        self.before
            .iter()
            .filter(|message| message["method"] == json!("$/progress"))
            .map(|message| &message["params"])
            .collect()
    }
}

/// Send a request and read until its answer. A request that the server makes
/// meanwhile is answered with `null`, as a client that takes it does.
#[track_caller]
fn exchange(session: &mut Session, method: &str, params: Value) -> Exchange {
    let id = session.next_request_id();
    session.send(json!({"jsonrpc": "2.0", "id": id, "method": method, "params": params}));
    let mut before = Vec::new();
    loop {
        let message = session.wait_for(|_| true);
        if message["id"] == json!(id) && message.get("method").is_none() {
            assert!(
                message.get("error").is_none(),
                "`{method}` gave an error: {message}"
            );
            return Exchange {
                before,
                answer: message["result"].clone(),
            };
        }
        if message["method"] == json!("window/workDoneProgress/create") {
            session.send(json!({"jsonrpc": "2.0", "id": message["id"], "result": null}));
        }
        before.push(message);
    }
}

/// The names of the items of a hierarchy answer, without the marker.
fn names(answer: &Value) -> Vec<String> {
    let mut names: Vec<String> = answer
        .as_array()
        .unwrap_or_else(|| panic!("expected items, got {answer}"))
        .iter()
        .map(|item| item["name"].as_str().unwrap().to_string())
        .filter(|name| name != "incomplete")
        .collect();
    names.sort();
    names
}

/// The item of `app.Base`, as `prepareTypeHierarchy` gives it.
#[track_caller]
fn base_item(session: &mut Session, ws: &Workspace) -> Value {
    let base = ws.path("app/Base.cls");
    let answer = exchange(
        session,
        "textDocument/prepareTypeHierarchy",
        position(&base, BASE, "app.Base"),
    )
    .answer;
    answer[0].clone()
}

/// A request that runs a workspace pass and reads no class for its index.
fn run_pass(session: &mut Session, ws: &Workspace) {
    let base = ws.path("app/Base.cls");
    let mut params = position(&base, BASE, "app.Base");
    params["context"] = json!({"includeDeclaration": true});
    exchange(session, "textDocument/references", params);
}

fn subtypes(session: &mut Session, item: &Value, token: Option<&str>) -> Exchange {
    let mut params = json!({"item": item});
    if let Some(token) = token {
        params["workDoneToken"] = json!(token);
    }
    exchange(session, "typeHierarchy/subtypes", params)
}

fn standard() -> Workspace {
    let one = sub("One", "Base");
    let two = sub("Two", "Base");
    let three = sub("Three", "One");
    Workspace::new(&[
        ("app/Base.cls", BASE),
        ("app/One.cls", &one),
        ("app/Two.cls", &two),
        ("app/Three.cls", &three),
    ])
}

#[test]
fn a_session_that_never_asked_builds_no_index_after_a_pass() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    // The pass is stored before the answer of this request goes out.
    run_pass(&mut session, &ws);

    // No request asked for the index, so the pass started no build: the first
    // request builds it.
    let first = subtypes(&mut session, &item, None);
    assert_eq!(names(&first.answer), ["app.One", "app.Two"]);
    let built = built(&session);
    assert_eq!(built.len(), 1, "{:?}", lines(&session));
    assert!(
        built[0].ends_with("started by the request"),
        "{:?}",
        lines(&session)
    );
}

#[test]
fn after_the_first_request_each_pass_builds_the_index() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let three = ws.path("app/Three.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // An edit that the pass reads: the next pass is new, and it builds the
    // index with no request for it.
    session.open(&three, &sub("Three", "Base"));
    run_pass(&mut session, &ws);
    wait_for_lines(&session, "index built after the pass", |lines| {
        lines
            .iter()
            .any(|line| line.contains(" built for ") && line.ends_with("started by the pass"))
    });
    let before = built(&session);
    assert_eq!(before.len(), 2, "{before:?}");

    // The request reads it, shows no progress though it carries a token, and
    // builds nothing.
    let next = subtypes(&mut session, &item, Some("next"));
    assert_eq!(names(&next.answer), ["app.One", "app.Three", "app.Two"]);
    assert!(next.progress().is_empty(), "{:?}", next.progress());
    assert_eq!(built(&session), before);

    // A second request finds the same index.
    let again = subtypes(&mut session, &item, None);
    assert_eq!(names(&again.answer), ["app.One", "app.Three", "app.Two"]);
    assert_eq!(built(&session), before);
}

#[test]
fn opening_and_closing_a_saved_class_file_keeps_the_index() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let two = ws.path("app/Two.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // The buffer holds the text that the pass read from disk.
    session.open(&two, &sub("Two", "Base"));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didClose",
        "params": {"textDocument": {"uri": file_uri(&two)}}
    }));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));
}

#[test]
fn a_watched_file_event_makes_the_next_request_build_the_index_again() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let two = ws.path("app/Two.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // The editor says that a class file changed. The stamps of the pass can
    // miss a change (the same length in the same tick of the clock), so the
    // daemon does not keep an index that the change can make wrong.
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&two), "type": 2}]}
    }));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    let built = built(&session);
    assert_eq!(built.len(), 2, "{:?}", lines(&session));
    assert!(
        built[1].ends_with("started by the request"),
        "{:?}",
        lines(&session)
    );
}

#[test]
fn an_edit_of_an_open_class_makes_the_next_request_build_the_index_again() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let three = ws.path("app/Three.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // An unsaved class that extends `Base`: a class of the workspace, which
    // the kept index does not hold.
    session.open(&three, &sub("Three", "Base"));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Three", "app.Two"]
    );
    assert_eq!(built(&session).len(), 2, "{:?}", lines(&session));

    // The same text again changes nothing.
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Three", "app.Two"]
    );
    assert_eq!(built(&session).len(), 2, "{:?}", lines(&session));
}

#[test]
fn a_class_that_is_not_saved_yet_is_in_the_index_the_next_request_builds() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let fresh = ws.path("app/Fresh.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // No file holds this class: no pass reads or looks for it.
    session.open(&fresh, &sub("Fresh", "Base"));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.Fresh", "app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 2, "{:?}", lines(&session));
}

#[test]
fn a_class_file_that_changes_on_disk_makes_the_next_request_build_the_index_again() {
    let ws = standard();
    let base = ws.path("app/Base.cls");
    let two = ws.path("app/Two.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One", "app.Two"]
    );
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));

    // `Two` no longer extends `Base`, and the daemon hears of it as an editor
    // tells it.
    std::fs::write(&two, sub("Two", "One")).unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": [{"uri": file_uri(&two), "type": 2}]}
    }));
    assert_eq!(
        names(&subtypes(&mut session, &item, None).answer),
        ["app.One"]
    );
    assert_eq!(built(&session).len(), 2, "{:?}", lines(&session));
}

/// Ask for the subtypes once, so that each pass from then on builds the index.
fn ask_once(session: &mut Session, ws: &Workspace) -> Value {
    let item = base_item(session, ws);
    subtypes(session, &item, None);
    assert_eq!(built(session).len(), 1, "{:?}", lines(session));
    item
}

#[test]
fn an_edit_does_not_wait_for_the_build() {
    let ws = Workspace::new(&[
        ("app/Base.cls", BASE),
        ("app/Held.cls", &sub("Held", "Base")),
    ]);
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    ask_once(&mut session, &ws);
    // The next build waits 40 s at this class, with its database snapshot. The
    // pass sees the new text on disk and starts the build.
    std::fs::write(ws.path("app/Held.cls"), held_sub("Held", 40_000)).unwrap();
    run_pass(&mut session, &ws);
    wait_for_lines(&session, "build that waits", |lines| {
        lines.iter().any(|line| line.ends_with("holds"))
    });

    // A write to a buffer waits for every snapshot to drop. The server does not
    // read the next request before the write ends, so the answer to this
    // request shows when it did.
    let edited = format!("{BASE}/* edited */\n");
    session.open(&base, &edited);
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/documentSymbol",
        "params": {"textDocument": {"uri": file_uri(&base)}}
    }));
    let deadline = Instant::now() + Duration::from_secs(15);
    let answered = loop {
        let left = deadline.saturating_duration_since(Instant::now());
        match session.try_next(left) {
            Some(message) if message["id"] == json!(id) => break true,
            Some(_) => {}
            None => break false,
        }
    };
    assert!(
        answered,
        "the edit waited for the build: {:?}",
        lines(&session)
    );
    // The edit stopped the build, and kept no index.
    wait_for_lines(&session, "build stopped by the edit", |lines| {
        lines.iter().any(|line| line.contains(" stopped for "))
    });
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));
}

#[test]
fn a_newer_pass_stops_the_build_of_an_older_one() {
    let ws = Workspace::new(&[
        ("app/Base.cls", BASE),
        ("app/Held.cls", &sub("Held", "Base")),
    ]);
    let base = ws.path("app/Base.cls");
    let held = ws.path("app/Held.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = ask_once(&mut session, &ws);
    // The build of the next pass waits 60 s at this class.
    std::fs::write(&held, held_sub("Held", 60_000)).unwrap();
    run_pass(&mut session, &ws);
    wait_for_lines(&session, "build that waits", |lines| {
        lines.iter().any(|line| line.ends_with("holds"))
    });

    // A newer pass: nothing waits for the index of the older one, so its build
    // stops, and the build of the newer pass is kept. No buffer changes, so no
    // write stops the held build: only the newer build can. `Held` is the last
    // class file, so the held build ends with an index after the stop, and
    // only the check that a newer build started keeps that index out.
    std::fs::write(&held, sub("Held", "Base")).unwrap();
    run_pass(&mut session, &ws);
    wait_for_lines(&session, "older build stopped", |lines| {
        lines
            .iter()
            .any(|line| line.contains(" stopped for ") && line.ends_with("started by the pass"))
    });
    wait_for_lines(&session, "newer build kept", |lines| {
        lines
            .iter()
            .filter(|line| line.contains(" built for "))
            .count()
            == 2
    });
    let next = subtypes(&mut session, &item, None);
    assert_eq!(names(&next.answer), ["app.Held"]);
    assert_eq!(built(&session).len(), 2, "{:?}", lines(&session));
}

/// A workspace whose index build takes two held class files, so that a request
/// meets it while it runs.
fn slow_index() -> (Workspace, String) {
    let one = held_sub("H1", 1500);
    let two = held_sub("H2", 1500);
    let ws = Workspace::new(&[
        ("app/Base.cls", BASE),
        ("app/H1.cls", &one),
        ("app/H2.cls", &two),
    ]);
    (ws, BASE.to_string())
}

/// The value of each `$/progress` of the exchange, by kind.
fn kinds(progress: &[&Value]) -> Vec<String> {
    progress
        .iter()
        .map(|params| params["value"]["kind"].as_str().unwrap().to_string())
        .collect()
}

/// `n` of the message `Reading class files: <n> of <total>`.
fn read_count(message: &str) -> usize {
    message
        .strip_prefix("Reading class files: ")
        .and_then(|rest| rest.split(' ').next())
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| panic!("not a count of class files: `{message}`"))
}

#[test]
fn a_request_that_waits_for_the_index_reports_progress_on_its_token() {
    let (ws, _) = slow_index();
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    // The pass is current, and this session asked for no index yet: the
    // request starts the build and waits for it.
    run_pass(&mut session, &ws);

    let waited = subtypes(&mut session, &item, Some("wait-token"));
    assert_eq!(names(&waited.answer), ["app.H1", "app.H2"]);
    let progress = waited.progress();
    let kinds = kinds(&progress);
    assert_eq!(
        kinds.first().map(String::as_str),
        Some("begin"),
        "{kinds:?}"
    );
    assert_eq!(kinds.last().map(String::as_str), Some("end"), "{kinds:?}");
    assert!(
        kinds.len() >= 3
            && kinds[1..kinds.len() - 1]
                .iter()
                .all(|kind| kind == "report"),
        "{kinds:?}"
    );
    for params in &progress {
        assert_eq!(params["token"], json!("wait-token"));
    }
    // The counts are of class files read, and none goes back.
    let counts: Vec<usize> = progress[..progress.len() - 1]
        .iter()
        .map(|params| read_count(params["value"]["message"].as_str().unwrap()))
        .collect();
    assert!(
        counts.windows(2).all(|pair| pair[0] <= pair[1]),
        "{counts:?}"
    );
    assert!(counts.first() < counts.last(), "{counts:?}");
    // The progress ended before the answer: the notifications came first.
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));
}

#[test]
fn a_request_that_waits_asks_the_client_for_a_token() {
    let (ws, _) = slow_index();
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({"window": {"workDoneProgress": true}}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    // The pass is current, and this session asked for no index yet: the
    // request starts the build and waits for it.
    run_pass(&mut session, &ws);

    let waited = subtypes(&mut session, &item, None);
    assert_eq!(names(&waited.answer), ["app.H1", "app.H2"]);
    let creates: Vec<&Value> = waited
        .before
        .iter()
        .filter(|message| message["method"] == json!("window/workDoneProgress/create"))
        .collect();
    assert_eq!(creates.len(), 1, "{:?}", waited.before);
    let token = creates[0]["params"]["token"].clone();
    // No progress is sent before the client agreed.
    let create_at = waited
        .before
        .iter()
        .position(|message| message["method"] == json!("window/workDoneProgress/create"))
        .unwrap();
    let first_progress = waited
        .before
        .iter()
        .position(|message| message["method"] == json!("$/progress"))
        .expect("a progress notification");
    assert!(create_at < first_progress);
    let progress = waited.progress();
    let kinds = kinds(&progress);
    assert_eq!(
        kinds.first().map(String::as_str),
        Some("begin"),
        "{kinds:?}"
    );
    assert_eq!(kinds.last().map(String::as_str), Some("end"), "{kinds:?}");
    for params in &progress {
        assert_eq!(params["token"], token);
    }
}

#[test]
fn a_client_that_takes_no_progress_gets_none() {
    let (ws, _) = slow_index();
    let base = ws.path("app/Base.cls");
    // No token in the request and no support for `create`.
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    // The pass is current, and this session asked for no index yet: the
    // request starts the build and waits for it.
    run_pass(&mut session, &ws);

    let waited = subtypes(&mut session, &item, None);
    assert_eq!(names(&waited.answer), ["app.H1", "app.H2"]);
    assert!(
        waited.before.iter().all(|message| {
            message["method"] != json!("$/progress")
                && message["method"] != json!("window/workDoneProgress/create")
        }),
        "{:?}",
        waited.before
    );
}

#[test]
fn a_request_that_waits_for_the_pass_reports_progress() {
    // The pass waits 1.2 s at this file, with the others analysed.
    let slow = "/* OXABL-TEST-HOLD:daemon.pass-file@1200 */\n\
                CLASS app.Slow INHERITS app.Base:\nEND CLASS.\n";
    let ws = Workspace::new(&[("app/Base.cls", BASE), ("app/Slow.cls", slow)]);
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);

    // No pass ran yet: this request starts it and waits for it.
    let waited = subtypes(&mut session, &item, Some("pass-token"));
    assert_eq!(names(&waited.answer), ["app.Slow"]);
    let progress = waited.progress();
    let kinds = kinds(&progress);
    assert_eq!(
        kinds.first().map(String::as_str),
        Some("begin"),
        "{kinds:?}"
    );
    assert_eq!(kinds.last().map(String::as_str), Some("end"), "{kinds:?}");
    let begin = progress[0]["value"]["message"].as_str().unwrap();
    assert!(begin.starts_with("Analysing the workspace: "), "{begin}");
}

#[test]
fn a_request_that_is_cancelled_stops_waiting_and_the_build_goes_on() {
    // Three files, each held 1.5 s: the build takes 4.5 s at least, and the
    // cancel comes about 0.3 s after the request.
    let (h1, h2, h3) = (
        held_sub("H1", 1500),
        held_sub("H2", 1500),
        held_sub("H3", 1500),
    );
    let ws = Workspace::new(&[
        ("app/Base.cls", BASE),
        ("app/H1.cls", &h1),
        ("app/H2.cls", &h2),
        ("app/H3.cls", &h3),
    ]);
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    // The pass is current, and this session asked for no index yet: the
    // request starts the build and waits for it.
    run_pass(&mut session, &ws);

    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "typeHierarchy/subtypes",
        "params": {"item": item, "workDoneToken": "cancel-token"}
    }));
    // The progress shows that the request waits for the build.
    let begin = session.wait_for(|message| message["method"] == json!("$/progress"));
    assert_eq!(begin["params"]["value"]["kind"], json!("begin"), "{begin}");
    session.send(json!({"jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}}));
    let mut before = Vec::new();
    let answer = loop {
        let message = session.wait_for(|_| true);
        if message["id"] == json!(id) && message.get("method").is_none() {
            break message;
        }
        before.push(message);
    };
    assert_eq!(answer["error"]["code"], json!(-32800), "{answer}");
    // The progress ended before the answer.
    let ended = Exchange {
        before,
        answer: Value::Null,
    };
    let kinds = kinds(&ended.progress());
    assert_eq!(kinds.last().map(String::as_str), Some("end"), "{kinds:?}");
    // The request did not wait for the build, which is still held.
    assert!(built(&session).is_empty(), "{:?}", lines(&session));

    // The build went on, and the next request finds its index: one build.
    let next = subtypes(&mut session, &item, None);
    assert_eq!(names(&next.answer), ["app.H1", "app.H2", "app.H3"]);
    assert_eq!(built(&session).len(), 1, "{:?}", lines(&session));
}

#[test]
fn a_client_that_refuses_a_token_gets_no_progress() {
    let (ws, _) = slow_index();
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({"window": {"workDoneProgress": true}}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);
    run_pass(&mut session, &ws);

    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "typeHierarchy/subtypes",
        "params": {"item": item}
    }));
    let mut before = Vec::new();
    let answer = loop {
        let message = session.wait_for(|_| true);
        if message["id"] == json!(id) && message.get("method").is_none() {
            break message;
        }
        if message["method"] == json!("window/workDoneProgress/create") {
            session.send(json!({
                "jsonrpc": "2.0", "id": message["id"],
                "error": {"code": -32603, "message": "no progress here"}
            }));
        }
        before.push(message);
    };
    assert_eq!(names(&answer["result"]), ["app.H1", "app.H2"]);
    let creates = before
        .iter()
        .filter(|message| message["method"] == json!("window/workDoneProgress/create"))
        .count();
    assert_eq!(creates, 1, "{before:?}");
    assert!(
        before
            .iter()
            .all(|message| message["method"] != json!("$/progress")),
        "{before:?}"
    );
}

#[test]
fn a_build_that_panics_fails_the_request_and_is_not_kept() {
    let broken = "/* OXABL-TEST-PANIC:daemon.hierarchy-index */\n\
                  CLASS app.Broken INHERITS app.Base:\nEND CLASS.\n";
    let ws = Workspace::new(&[("app/Base.cls", BASE), ("app/Broken.cls", broken)]);
    let base = ws.path("app/Base.cls");
    let mut session = start(&ws.root, json!({}));
    session.open(&base, BASE);
    let item = base_item(&mut session, &ws);

    for attempt in 1..=2 {
        let answer = session.request("typeHierarchy/subtypes", json!({"item": item}));
        assert_eq!(answer["error"]["code"], json!(-32803), "{answer}");
        let message = answer["error"]["message"].as_str().unwrap();
        assert!(message.contains("the index failed"), "{message}");
        // Each request starts a build: a failed one is not kept.
        let failed = lines(&session)
            .iter()
            .filter(|line| line.contains(" failed for "))
            .count();
        assert_eq!(failed, attempt, "{:?}", lines(&session));
    }
    assert!(built(&session).is_empty(), "{:?}", lines(&session));

    // The daemon still answers.
    let prepared = exchange(
        &mut session,
        "textDocument/prepareTypeHierarchy",
        position(&base, BASE, "app.Base"),
    );
    assert_eq!(prepared.answer[0]["name"], json!("app.Base"));
}
