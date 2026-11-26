//! The workspace pass after an edit, for what the dependency graph holds and
//! no language server request reads: the edge from a consumer to the producer
//! of a `SHARED` name, and the edge to a path that a file only checks for. They
//! are read through `oxabl/impact`, so the tests ask the daemon in the process.
//! The consumer reads no file for a name that nothing produces: only the map
//! of the producers says that it must be analysed again when another file
//! starts to produce the name. A `reindex` is always full, so the first pass
//! is a `reindex` and each edit is followed by a question, which makes the
//! pass of the new buffer text from the earlier one.

use std::fs;
use std::path::{Path, PathBuf};

use oxabl_daemon::{ClientContext, Dispatch, SessionHost, default_dispatch};
use oxabl_daemon_protocol::{
    ClientKind, HandshakeRequest, ImpactRequest, ImpactResponse, ReindexRequest, ReindexResponse,
    Sourced, Subject, method,
};
use serde::Serialize;
use serde::de::DeserializeOwned;

const CONSUMER: &str = "DEFINE SHARED VARIABLE gcName AS CHARACTER NO-UNDO.\nMESSAGE gcName.\n";
const PRODUCER: &str = "DEFINE NEW SHARED VARIABLE gcName AS CHARACTER NO-UNDO.\ngcName = \"a\".\n";
const NOT_PRODUCER: &str = "DEFINE VARIABLE gcName AS CHARACTER NO-UNDO.\ngcName = \"a\".\n";

fn call<P: Serialize, R: DeserializeOwned>(
    dispatch: &Dispatch,
    host: &SessionHost,
    context: &mut ClientContext,
    method: &str,
    params: &P,
) -> R {
    let value = dispatch
        .call(host, context, method, serde_json::to_value(params).unwrap())
        .unwrap_or_else(|error| panic!("{method} failed: {error}"));
    serde_json::from_value(value).unwrap()
}

fn available<T>(answer: Sourced<T>) -> T {
    match answer {
        Sourced::Available { value } => value,
        Sourced::Unavailable { reason } => panic!("the daemon refused the question: {reason}"),
    }
}

struct Daemon {
    dispatch: Dispatch,
    host: SessionHost,
    client: ClientContext,
    root: PathBuf,
}

impl Daemon {
    fn start(root: &Path) -> Daemon {
        let dispatch = default_dispatch();
        let host = SessionHost::new();
        let mut client = ClientContext::default();
        let _: serde_json::Value = call(
            &dispatch,
            &host,
            &mut client,
            method::HANDSHAKE,
            &HandshakeRequest::new(ClientKind::Editor, root.to_string_lossy()),
        );
        Daemon {
            dispatch,
            host,
            client,
            root: root.to_path_buf(),
        }
    }

    fn open(&self, name: &str, text: &str) {
        self.host.with(|sessions| {
            sessions.for_root(&self.root).set_buffer(
                name,
                text.to_string(),
                Some(self.root.join(name)),
            );
        });
    }

    fn reindex(&mut self) {
        let answer: Sourced<ReindexResponse> = call(
            &self.dispatch,
            &self.host,
            &mut self.client,
            method::REINDEX,
            &ReindexRequest {},
        );
        available(answer);
    }

    /// The files that depend on `name`, with the cause of each.
    fn impact(&mut self, name: &str) -> Vec<(String, String)> {
        let answer: Sourced<ImpactResponse> = call(
            &self.dispatch,
            &self.host,
            &mut self.client,
            method::IMPACT,
            &ImpactRequest {
                subject: Subject::File {
                    path: self.root.join(name).to_string_lossy().into_owned(),
                },
            },
        );
        let mut found: Vec<(String, String)> = available(answer)
            .groups
            .iter()
            .flat_map(|group| {
                group.files.iter().map(move |file| {
                    let file_name = Path::new(&file.path)
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                        .into_owned();
                    (format!("{:?}", group.cause), file_name)
                })
            })
            .collect();
        found.sort();
        found
    }
}

/// The impact of `p.p` as a new daemon gives it, with `p.p` open as `text`.
fn fresh_impact(root: &Path, text: &str) -> Vec<(String, String)> {
    let mut fresh = Daemon::start(root);
    fresh.open("p.p", text);
    fresh.impact("p.p")
}

#[test]
fn a_file_that_starts_to_produce_a_shared_name_gets_its_consumer_as_a_dependent() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().canonicalize().unwrap();
    fs::write(
        root.join("oxabl.toml"),
        "[workspace]\nname = \"shared\"\n[workspace.sources]\ninclude_paths = [\".\"]\n",
    )
    .unwrap();
    fs::write(root.join("s.p"), CONSUMER).unwrap();
    fs::write(root.join("p.p"), NOT_PRODUCER).unwrap();
    fs::write(root.join("other.p"), "MESSAGE 1.\n").unwrap();

    let mut daemon = Daemon::start(&root);
    daemon.open("p.p", NOT_PRODUCER);
    daemon.reindex();
    assert!(
        daemon.impact("p.p").is_empty(),
        "s.p has no producer: {:?}",
        daemon.impact("p.p")
    );

    daemon.open("p.p", PRODUCER);
    let produced = daemon.impact("p.p");
    assert_eq!(
        produced,
        vec![("SharedProducer".to_string(), "s.p".to_string())],
        "s.p depends on p.p"
    );
    assert_eq!(produced, fresh_impact(&root, PRODUCER));

    daemon.open("p.p", NOT_PRODUCER);
    assert_eq!(daemon.impact("p.p"), fresh_impact(&root, NOT_PRODUCER));
}

/// A workspace whose include path is `a`, `b` and then the root.
struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new(files: &[(&str, &str)]) -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        fs::write(
            root.join("oxabl.toml"),
            "[workspace]\nname = \"paths\"\n\
             [workspace.sources]\ninclude_paths = [\"a\", \"b\", \".\"]\n",
        )
        .unwrap();
        for (name, text) in files {
            let path = root.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, text).unwrap();
        }
        Workspace { _dir: dir, root }
    }
}

/// `q.p` runs `data.txt`, which both `a` and `b` have, so the lookup is
/// ambiguous and reads neither: it only checks that each path is there. The
/// extension is not one of a root file, so no other analysis reads them.
const RUNS_DATA: &str = "RUN data.txt.\n";

#[test]
fn a_path_that_was_only_checked_and_is_removed_is_seen() {
    let ws = Workspace::new(&[
        ("q.p", RUNS_DATA),
        ("a/data.txt", "MESSAGE 1.\n"),
        ("b/data.txt", "MESSAGE 2.\n"),
        ("other.p", "MESSAGE 1.\n"),
    ]);
    let mut daemon = Daemon::start(&ws.root);
    daemon.reindex();
    let ambiguous = daemon.impact("a/data.txt");
    assert!(
        ambiguous.is_empty(),
        "the run names two files: {ambiguous:?}"
    );

    fs::remove_file(ws.root.join("b/data.txt")).unwrap();
    // A buffer that moved is what starts the pass.
    daemon.open("other.p", "MESSAGE 2.\n");
    let found = daemon.impact("a/data.txt");
    let mut fresh = Daemon::start(&ws.root);
    fresh.open("other.p", "MESSAGE 2.\n");
    assert_eq!(found, fresh.impact("a/data.txt"));
    assert_eq!(
        found,
        vec![("Program".to_string(), "q.p".to_string())],
        "the run names the one file that is left"
    );
}

#[test]
fn a_path_that_was_only_checked_and_is_read_later_is_read_as_it_is() {
    let ws = Workspace::new(&[
        ("q.p", RUNS_DATA),
        ("a.p", "MESSAGE 1.\n"),
        ("a/data.txt", "RUN b.p.\n"),
        ("b/data.txt", "MESSAGE 2.\n"),
        ("b.p", "MESSAGE 1.\n"),
        ("other.p", "MESSAGE 1.\n"),
    ]);
    let mut daemon = Daemon::start(&ws.root);
    daemon.reindex();
    // a.p includes one of the files that q.p only checked for, so its pass
    // reads it.
    daemon.open("a.p", "{a/data.txt}\n");
    let included = daemon.impact("b.p");
    assert!(
        included.iter().any(|(_, name)| name == "a.p"),
        "a.p runs b.p through the include: {included:?}"
    );

    fs::write(ws.root.join("a/data.txt"), "MESSAGE 3.\n").unwrap();
    daemon.open("other.p", "MESSAGE 2.\n");
    let changed = daemon.impact("b.p");
    let mut fresh = Daemon::start(&ws.root);
    fresh.open("a.p", "{a/data.txt}\n");
    fresh.open("other.p", "MESSAGE 2.\n");
    assert_eq!(changed, fresh.impact("b.p"));
    assert!(
        !changed.iter().any(|(_, name)| name == "a.p"),
        "the include no longer runs b.p: {changed:?}"
    );
}

/// The impact of `target` as a new daemon gives it, with each of `open`
/// opened as the text given.
fn fresh_impact_with(root: &Path, open: &[(&str, &str)], target: &str) -> Vec<(String, String)> {
    let mut fresh = Daemon::start(root);
    for (name, text) in open {
        fresh.open(name, text);
    }
    fresh.impact(target)
}

// The graph of a pass after an edit joins the rows of the files that were not
// analysed again with the rows of the ones that were. After each edit the rows
// of the edited file must be the rows of its new text only: a row it no longer
// has is gone, also when it was the last row of its subject, and a row it gains
// is there, also for a subject that no file named before.
#[test]
fn the_rows_of_an_edited_file_replace_its_old_rows_in_the_graph() {
    let ws = Workspace::new(&[
        ("user.p", "RUN one.p.\n"),
        ("keeper.p", "RUN one.p.\n"),
        ("one.p", "MESSAGE 1.\n"),
        ("two.p", "MESSAGE 2.\n"),
        ("other.p", "MESSAGE 3.\n"),
    ]);
    let mut daemon = Daemon::start(&ws.root);
    daemon.reindex();
    let expected: Vec<(String, String)> = ["keeper.p", "user.p"]
        .iter()
        .map(|name| ("Program".to_string(), name.to_string()))
        .collect();
    assert_eq!(daemon.impact("one.p"), expected);

    for text in ["RUN two.p.\n", "MESSAGE 4.\n", "RUN one.p.\nRUN two.p.\n"] {
        daemon.open("user.p", text);
        for name in ["one.p", "two.p"] {
            assert_eq!(
                daemon.impact(name),
                fresh_impact_with(&ws.root, &[("user.p", text)], name),
                "after user.p became {text:?}, the dependents of {name}"
            );
        }
    }
    // Each program loses its last dependent.
    let texts = [("keeper.p", "MESSAGE 5.\n"), ("user.p", "MESSAGE 6.\n")];
    for (name, text) in texts {
        daemon.open(name, text);
    }
    for name in ["one.p", "two.p"] {
        let found = daemon.impact(name);
        assert_eq!(
            found,
            fresh_impact_with(&ws.root, &texts, name),
            "the dependents of {name} after the last dependent left"
        );
        assert!(found.is_empty(), "{name}: {found:?}");
    }
}
