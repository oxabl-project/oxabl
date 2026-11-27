//! `textDocument/documentLink` through the real `oxabl lsp` and its daemon:
//! include references, literal `RUN` targets and `USING` names link to the one
//! file each names. A name that resolves to no file, or to more than one, has
//! no link, and so does text in an `&IF` branch the preprocessor did not take.
//!
//! The links of an open file come from the analysis that the server keeps for
//! the revision of its buffer. A source with the marker
//! `OXABL-TEST-COUNT:lsp.document-links` makes a test build log how many
//! navigation models each links request built, so a test sees that a second
//! request on the same text builds none.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::path::{Path, PathBuf};

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"links\"\n\
    [workspace.sources]\ninclude_paths = [\".\", \"lib\"]\n";

const MAIN: &str = "/* Größe */ USING app.Greeter.\n\
    USING app.*.\n\
    USING app.Missing.\n\
    {defs.i}\n\
    {defs.i \"an argument\"}\n\
    {missing.i}\n\
    &IF FALSE &THEN\n\
    {never.i}\n\
    RUN never.p.\n\
    &ENDIF\n\
    RUN util/helper.p.\n\
    RUN helper2.\n\
    RUN \"util/helper.p\".\n\
    RUN LocalProc.\n\
    RUN dup.p.\n\
    RUN missing.p.\n\
    RUN VALUE(\"x.p\").\n\
    PROCEDURE LocalProc:\n\
    END PROCEDURE.\n";

/// `MAIN` with the marker that makes the server log the cost of each request.
fn counted_main() -> String {
    format!("{MAIN}/* OXABL-TEST-COUNT:lsp.document-links */\n")
}

/// A program whose `RUN` is compiled only when its include defines a name.
const GATED: &str = "{gate.i}\n\
    &IF DEFINED(with-helper) &THEN\n\
    RUN util/helper.p.\n\
    &ENDIF\n";

/// A program whose include exists only as an open, unsaved document.
const FRESH: &str = "{fresh.i}\n";

struct Workspace {
    _dir: tempfile::TempDir,
    root: PathBuf,
}

impl Workspace {
    fn new() -> Workspace {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        let files: &[(&str, &str)] = &[
            ("oxabl.toml", CONFIG),
            ("main.p", MAIN),
            ("defs.i", "/* {1} */\n"),
            ("never.i", "/* never */\n"),
            ("never.p", "MESSAGE \"never\".\n"),
            ("util/helper.p", "MESSAGE \"helper\".\n"),
            ("helper2.p", "MESSAGE \"helper2\".\n"),
            ("dup.p", "MESSAGE \"one\".\n"),
            ("lib/dup.p", "MESSAGE \"two\".\n"),
            ("app/Greeter.cls", "CLASS app.Greeter:\nEND CLASS.\n"),
            ("gated.p", GATED),
            ("gate.i", "/* no name yet */\n"),
            ("fresh.p", FRESH),
        ];
        for (name, text) in files {
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

fn utf16_at(text: &str, at: usize) -> Value {
    let line_start = text[..at].rfind('\n').map_or(0, |n| n + 1);
    let line = text[..at].matches('\n').count();
    let character = text[line_start..at].encode_utf16().count();
    json!({"line": line, "character": character})
}

/// A link over the first `text_at` in `text`, to `target`.
#[track_caller]
fn link_in(text: &str, text_at: &str, target: &Path) -> Value {
    let start = text
        .find(text_at)
        .unwrap_or_else(|| panic!("no `{text_at}`"));
    link_at(text, start, text_at.len(), target)
}

/// A link over the `len` bytes at `start` of `text`, to `target`.
fn link_at(text: &str, start: usize, len: usize, target: &Path) -> Value {
    json!({
        "range": {"start": utf16_at(text, start), "end": utf16_at(text, start + len)},
        "target": file_uri(target)
    })
}

/// The links of `MAIN`, in the order of their lines.
fn main_links(ws: &Workspace) -> Vec<Value> {
    let helper = ws.path("util/helper.p");
    vec![
        link_in(MAIN, "app.Greeter", &ws.path("app/Greeter.cls")),
        link_in(MAIN, "{defs.i}", &ws.path("defs.i")),
        link_in(MAIN, "{defs.i \"an argument\"}", &ws.path("defs.i")),
        link_in(MAIN, "util/helper.p", &helper),
        link_in(MAIN, "helper2", &ws.path("helper2.p")),
        link_in(MAIN, "\"util/helper.p\"", &helper),
    ]
}

/// The links of `file` in the order of their lines, and the number of
/// navigation models that the request built when the open text of `file` asks
/// the server to log it.
fn links_and_models(session: &mut Session, file: &Path) -> (Vec<Value>, Option<u64>) {
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/documentLink",
        "params": {"textDocument": {"uri": file_uri(file)}}
    }));
    let mut models = None;
    let answer = session.wait_for(|message| {
        if let Some(count) = message["params"]["message"].as_str().and_then(|line| {
            line.strip_prefix("oxabl-test: document links built ")?
                .strip_suffix(" navigation models")
        }) {
            models = Some(count.parse().expect("a number of navigation models"));
        }
        message["id"] == json!(id) && message.get("method").is_none()
    });
    assert!(answer.get("error").is_none(), "{answer}");
    let mut links: Vec<Value> = answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("no list: {answer}"))
        .iter()
        .map(|link| json!({"range": link["range"], "target": link["target"]}))
        .collect();
    links.sort_by_key(|link| link["range"]["start"]["line"].as_u64());
    (links, models)
}

fn links_of(session: &mut Session, file: &Path) -> Vec<Value> {
    links_and_models(session, file).0
}

/// Ask for the links of `file` until a request builds no navigation model, and
/// fail when each of `REQUESTS` requests builds one. Every answer must be
/// `want`. The workspace pass can change what the kept analysis read while the
/// first requests run, so one request more than the first can build a model.
#[track_caller]
fn links_until_none_is_built(session: &mut Session, file: &Path, want: &[Value]) {
    const REQUESTS: usize = 8;
    let mut built = Vec::new();
    for _ in 0..REQUESTS {
        let (links, models) = links_and_models(session, file);
        assert_eq!(links, want);
        let models = models.expect("the server logs the navigation models of the request");
        built.push(models);
        if models == 0 {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    panic!("each links request built a navigation model: {built:?}");
}

#[test]
fn each_reference_to_one_file_links_to_it_and_nothing_else_links() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    assert_eq!(links_of(&mut session, &main), main_links(&ws));
}

#[test]
fn a_file_that_is_not_open_has_the_links_of_its_text_on_disk() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    // Another open document, so the session is that of an editor.
    session.open(&ws.path("helper2.p"), "MESSAGE \"helper2\".\n");

    assert_eq!(links_of(&mut session, &main), main_links(&ws));
}

#[test]
fn the_links_of_an_open_file_come_from_its_kept_analysis() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    let text = counted_main();
    session.open(&main, &text);

    let (first, models) = links_and_models(&mut session, &main);
    assert_eq!(first, main_links(&ws));
    assert_eq!(models, Some(1), "the first request of a revision analyses");
    links_until_none_is_built(&mut session, &main, &main_links(&ws));

    // An edit is a new revision: the next request analyses the new text, and
    // the requests after it do not.
    let edited = format!("{text}RUN helper2.\n");
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&main), "version": 2},
            "contentChanges": [{"text": edited}]
        }
    }));
    let mut after_edit = main_links(&ws);
    let added = edited.rfind("helper2").unwrap();
    after_edit.push(link_at(
        &edited,
        added,
        "helper2".len(),
        &ws.path("helper2.p"),
    ));
    let (second, models) = links_and_models(&mut session, &main);
    assert_eq!(second, after_edit);
    assert_eq!(models, Some(1), "the first request after an edit analyses");
    links_until_none_is_built(&mut session, &main, &after_edit);
}

#[test]
fn an_unsaved_include_counts_as_it_does_for_go_to_definition() {
    let ws = Workspace::new();
    let gated = ws.path("gated.p");
    let mut session = Session::start(&ws.root);
    session.open(&gated, GATED);
    let include_only = vec![link_in(GATED, "{gate.i}", &ws.path("gate.i"))];
    // The kept analysis of the program, before the include is open.
    assert_eq!(links_of(&mut session, &gated), include_only);
    assert_eq!(links_of(&mut session, &gated), include_only);

    session.open(&ws.path("gate.i"), "&GLOBAL-DEFINE with-helper 1\n");
    assert_eq!(
        links_of(&mut session, &gated),
        vec![
            link_in(GATED, "{gate.i}", &ws.path("gate.i")),
            link_in(GATED, "util/helper.p", &ws.path("util/helper.p")),
        ]
    );
}

#[test]
fn an_include_with_no_file_yet_counts_as_it_does_for_go_to_definition() {
    let ws = Workspace::new();
    let fresh = ws.path("fresh.p");
    let include = ws.path("fresh.i");
    assert!(!include.exists());
    let mut session = Session::start(&ws.root);
    session.open(&fresh, FRESH);
    assert_eq!(links_of(&mut session, &fresh), Vec::<Value>::new());

    session.open(&include, "/* new */\n");
    assert_eq!(
        links_of(&mut session, &fresh),
        vec![link_in(FRESH, "{fresh.i}", &include)]
    );
}
