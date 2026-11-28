//! What `workspace/didChangeWatchedFiles` costs a server for events about files
//! that are not its own, through the real `oxabl lsp`.
//!
//! A client that applies a plain glob to every workspace folder (VS Code does)
//! sends the server of one folder the events of its sibling folders, thousands of
//! them after a branch switch. Such an event must start no pass and re-resolve no
//! configuration. An event for a file the session reads from outside its root (an
//! include path, a schema file, an `oxabl.toml` above the root) must still reach
//! it.
//!
//! A test build of the daemon writes one line to stderr for each workspace pass
//! when `OXABL_TEST_PASS_COUNTS` is set (see `incremental_pass_lsp.rs`).

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};
use std::time::Duration;

use serde_json::{Value, json};
use session::{Launch, Session, file_uri};

/// Long enough for the debounce of the server to publish what an event made stale.
const QUIET: Duration = Duration::from_millis(1500);

const SCHEMA: &str = "ADD TABLE \"Customer\"\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n";

const SCHEMA_WITH_NAME: &str = "ADD TABLE \"Customer\"\n\
    ADD FIELD \"CustNum\" OF \"Customer\" AS integer\n\
    ADD FIELD \"Name\" OF \"Customer\" AS character\n";

/// `Customer.Name` is a finding until a schema that declares it is loaded.
const MAIN: &str = "PROCEDURE helper:\nEND PROCEDURE.\n\
    FIND FIRST Customer.\nMESSAGE Customer.Name.\n";

/// A name that is a finding until an include that defines it is found.
const USES_INCLUDE: &str = "{ext.i}\nMESSAGE extVar.\n";

const INCLUDE_WITH_VAR: &str = "DEFINE VARIABLE extVar AS INTEGER NO-UNDO.\n";

/// Two sibling folders and a folder outside both, under one parent.
struct Folders {
    _dir: tempfile::TempDir,
    parent: PathBuf,
}

impl Folders {
    fn new() -> Folders {
        let dir = tempfile::tempdir().unwrap();
        let parent = dir.path().canonicalize().unwrap();
        let folders = Folders { _dir: dir, parent };
        folders.write("beta/oxabl.toml", "[workspace]\nname = \"beta\"\n");
        folders.write("beta/x.p", "MESSAGE \"x\".\n");
        folders.write("beta/inc.i", "MESSAGE \"inc\".\n");
        folders.write("beta/db/b.df", SCHEMA);
        folders
    }

    fn path(&self, name: &str) -> PathBuf {
        self.parent.join(name)
    }

    fn write(&self, name: &str, text: &str) {
        let path = self.path(name);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }

    fn start(&self, root: &str) -> Session {
        // Every refresh a client can take is on, so a refresh request that an
        // event for another folder causes shows.
        let params = json!({
            "capabilities": {
                "textDocument": {
                    "codeLens": {"dynamicRegistration": false},
                    "inlayHint": {},
                    "semanticTokens": {
                        "requests": {"full": true},
                        "tokenTypes": [],
                        "tokenModifiers": [],
                        "formats": ["relative"]
                    }
                },
                "workspace": {
                    "codeLens": {"refreshSupport": true},
                    "inlayHint": {"refreshSupport": true},
                    "semanticTokens": {"refreshSupport": true}
                }
            },
            "initializationOptions": {"referenceLenses": {"enable": true}}
        });
        let launch = Launch {
            env: &[("OXABL_TEST_PASS_COUNTS", "1")],
            ..Launch::default()
        };
        Session::launch_with(&self.path(root), params, launch, |_| {})
            .await_initialize()
            .0
    }
}

fn changed(session: &mut Session, paths: &[&Path]) {
    let changes: Vec<Value> = paths
        .iter()
        .map(|path| json!({"uri": file_uri(path), "type": 2}))
        .collect();
    session.send(json!({
        "jsonrpc": "2.0", "method": "workspace/didChangeWatchedFiles",
        "params": {"changes": changes}
    }));
}

/// The codes of the diagnostics of the next publication for `path`.
fn next_codes(session: &Session, path: &Path) -> Vec<String> {
    let uri = file_uri(path);
    let message = session.wait_for(|m| {
        m["method"] == "textDocument/publishDiagnostics" && m["params"]["uri"] == json!(uri)
    });
    message["params"]["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .map(|d| d["code"].to_string())
        .collect()
}

/// The messages that arrive until [`QUIET`] passes with none.
fn drain(session: &Session) -> Vec<Value> {
    let mut found = Vec::new();
    while let Some(message) = session.try_next(QUIET) {
        found.push(message);
    }
    found
}

/// The publications of diagnostics among `messages`.
fn publications_in(messages: &[Value]) -> Vec<Value> {
    let published = |m: &&Value| m["method"] == "textDocument/publishDiagnostics";
    messages.iter().filter(published).cloned().collect()
}

/// The `workspace/*/refresh` requests among `messages`.
fn refreshes_in(messages: &[Value]) -> Vec<String> {
    let is_refresh =
        |method: &str| method.starts_with("workspace/") && method.ends_with("/refresh");
    messages
        .iter()
        .filter_map(|m| m["method"].as_str())
        .filter(|method| is_refresh(method))
        .map(str::to_string)
        .collect()
}

/// The publications of diagnostics that arrive within [`QUIET`].
fn publications(session: &Session) -> Vec<Value> {
    publications_in(&drain(session))
}

/// How many files each pass the daemon of `session` ran analysed.
fn passes(session: &Session) -> Vec<usize> {
    session
        .stderr()
        .lines()
        .filter_map(|line| line.strip_prefix("oxabl-test: workspace pass analysed "))
        .map(|rest| rest.split_whitespace().next().unwrap().parse().unwrap())
        .collect()
}

/// A search that needs the workspace pass of the session.
fn search(session: &mut Session, path: &Path) {
    let answer = session.request(
        "textDocument/references",
        json!({
            "textDocument": {"uri": file_uri(path)},
            "position": {"line": 0, "character": 11},
            "context": {"includeDeclaration": true}
        }),
    );
    assert!(answer.get("error").is_none(), "references gave {answer}");
}

#[test]
fn events_for_a_sibling_folder_start_no_pass_and_no_reresolution() {
    // What a branch switch in the sibling folder makes the client send, one kind
    // of file at a time.
    for name in ["beta/x.p", "beta/inc.i", "beta/db/b.df", "beta/oxabl.toml"] {
        let folders = Folders::new();
        folders.write(
            "alpha/oxabl.toml",
            "[workspace]\nname = \"alpha\"\n[workspace.sources]\ninclude_paths = [\".\"]\n\
             [workspace.schema]\nfiles = [\"db/a.df\"]\n",
        );
        folders.write("alpha/db/a.df", SCHEMA);
        folders.write("alpha/a.p", MAIN);
        let a = folders.path("alpha/a.p");
        let mut session = folders.start("alpha");
        session.open(&a, MAIN);
        assert!(
            next_codes(&session, &a)
                .iter()
                .any(|c| c.contains("LINT0003")),
            "the open file has the finding of the schema"
        );
        search(&mut session, &a);
        let before = passes(&session);
        assert_eq!(before.len(), 1, "one pass for the first search: {before:?}");

        // What the open and the search made the server send is not the event's.
        let _ = drain(&session);

        changed(&mut session, &[&folders.path(name)]);

        // A re-resolution of the configuration makes the server publish again.
        let messages = drain(&session);
        let published = publications_in(&messages);
        assert!(
            published.is_empty(),
            "the event for {name} made the server publish again: {published:?}"
        );
        let refreshes = refreshes_in(&messages);
        assert!(
            refreshes.is_empty(),
            "the event for {name} made the server ask for a refresh: {refreshes:?}"
        );
        search(&mut session, &a);
        assert_eq!(
            passes(&session),
            before,
            "the event for {name} started a workspace pass"
        );
    }
}

/// The control of the test above: with the same capabilities, an event for a file
/// of the session's own root does make the server ask for a refresh.
#[test]
fn an_event_for_a_file_of_the_root_asks_for_a_refresh() {
    let folders = Folders::new();
    folders.write("alpha/oxabl.toml", "[workspace]\nname = \"alpha\"\n");
    folders.write("alpha/a.p", MAIN);
    let a = folders.path("alpha/a.p");
    let mut session = folders.start("alpha");
    session.open(&a, MAIN);
    let _ = next_codes(&session, &a);
    let _ = drain(&session);

    changed(&mut session, &[&folders.path("alpha/inc.i")]);
    let refreshes = refreshes_in(&drain(&session));
    assert!(
        refreshes.iter().any(|m| m == "workspace/inlayHint/refresh"),
        "an event of the root asks for no inlay hint refresh: {refreshes:?}"
    );
}

#[test]
fn an_event_for_a_file_on_an_include_path_outside_the_root_still_reaches_the_session() {
    let folders = Folders::new();
    folders.write(
        "alpha/oxabl.toml",
        "[workspace]\nname = \"alpha\"\n[workspace.sources]\n\
         include_paths = [\".\", \"../shared\"]\n",
    );
    folders.write("alpha/a.p", USES_INCLUDE);
    folders.write("shared/ext.i", "MESSAGE \"nothing\".\n");
    let a = folders.path("alpha/a.p");
    let mut session = folders.start("alpha");
    session.open(&a, USES_INCLUDE);
    assert!(
        !next_codes(&session, &a).is_empty(),
        "the name is a finding until the include defines it"
    );

    folders.write("shared/ext.i", INCLUDE_WITH_VAR);
    changed(&mut session, &[&folders.path("shared/ext.i")]);
    assert_eq!(
        next_codes(&session, &a),
        Vec::<String>::new(),
        "the include outside the root changed and the finding stays"
    );
}

/// The codes of the next publication for `path`, or `None` when none comes within
/// `limit`.
fn next_codes_within(session: &Session, path: &Path, limit: Duration) -> Option<Vec<String>> {
    let uri = file_uri(path);
    let deadline = std::time::Instant::now() + limit;
    while let Some(left) = deadline.checked_duration_since(std::time::Instant::now()) {
        let message = session.try_next(left)?;
        if message["method"] == "textDocument/publishDiagnostics"
            && message["params"]["uri"] == json!(uri)
        {
            return Some(
                message["params"]["diagnostics"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .map(|d| d["code"].to_string())
                    .collect(),
            );
        }
    }
    None
}

/// No include path and no configured place names the directory of the buffer, so
/// only the set of files the session has read can keep the event. The include is
/// in a subdirectory of the buffer's directory, so the directory of the buffer
/// does not keep it: only the include dependencies the session recorded for the
/// open buffer do. The registry of files an analysis read (`has_read`) keeps it
/// too, but a test cannot tell the two apart, because the open buffer is
/// analysed in the same session.
#[test]
fn an_event_for_an_include_of_an_open_buffer_outside_the_root_still_reaches_the_session() {
    let folders = Folders::new();
    folders.write(
        "alpha/oxabl.toml",
        "[workspace]\nname = \"alpha\"\n[workspace.sources]\n\
         root_relative_include_paths = [\".\"]\n",
    );
    folders.write("alpha/a.p", "MESSAGE \"a\".\n");
    folders.write("other/sub/inc.i", "MESSAGE \"nothing\".\n");
    let text = USES_INCLUDE.replace("{ext.i}", "{sub/inc.i}");
    folders.write("other/b.p", &text);
    let b = folders.path("other/b.p");
    let mut session = folders.start("alpha");
    session.open(&b, &text);
    assert!(
        !next_codes(&session, &b).is_empty(),
        "the name is a finding until the include defines it"
    );

    // The scope still filters: a file of a folder that nothing reads is dropped.
    changed(&mut session, &[&folders.path("beta/db/b.df")]);
    let published = publications(&session);
    assert!(
        published.is_empty(),
        "the scope is not bounded: {published:?}"
    );

    folders.write("other/sub/inc.i", INCLUDE_WITH_VAR);
    changed(&mut session, &[&folders.path("other/sub/inc.i")]);
    assert_eq!(
        next_codes_within(&session, &b, Duration::from_secs(10)),
        Some(Vec::<String>::new()),
        "the include of an open buffer changed and the finding stays"
    );
}

#[test]
fn an_event_for_a_schema_file_outside_the_root_still_reaches_the_session() {
    let folders = Folders::new();
    folders.write(
        "alpha/oxabl.toml",
        "[workspace]\nname = \"alpha\"\n[workspace.schema]\nfiles = [\"../shared/ext.df\"]\n",
    );
    folders.write("alpha/a.p", MAIN);
    folders.write("shared/ext.df", SCHEMA);
    let a = folders.path("alpha/a.p");
    let mut session = folders.start("alpha");
    session.open(&a, MAIN);
    assert!(
        next_codes(&session, &a)
            .iter()
            .any(|c| c.contains("LINT0003")),
        "the field is unknown under the first schema"
    );

    folders.write("shared/ext.df", SCHEMA_WITH_NAME);
    changed(&mut session, &[&folders.path("shared/ext.df")]);
    assert_eq!(
        next_codes(&session, &a),
        Vec::<String>::new(),
        "the schema file outside the root changed and the finding stays"
    );
}

#[test]
fn an_event_for_an_oxabl_toml_above_the_root_still_reaches_the_session() {
    let folders = Folders::new();
    // No `oxabl.toml` in the root: the one above it governs.
    folders.write("oxabl.toml", "[workspace]\nname = \"parent\"\n");
    folders.write("alpha/a.p", USES_INCLUDE);
    folders.write("shared/ext.i", INCLUDE_WITH_VAR);
    let a = folders.path("alpha/a.p");
    let mut session = folders.start("alpha");
    session.open(&a, USES_INCLUDE);
    assert!(
        !next_codes(&session, &a).is_empty(),
        "the include is not on a path, so the name is a finding"
    );

    folders.write(
        "oxabl.toml",
        "[workspace]\nname = \"parent\"\n[workspace.sources]\ninclude_paths = [\"shared\"]\n",
    );
    changed(&mut session, &[&folders.path("oxabl.toml")]);
    assert_eq!(
        next_codes(&session, &a),
        Vec::<String>::new(),
        "the configuration above the root changed and the finding stays"
    );
}
