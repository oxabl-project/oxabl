//! `textDocument/documentLink` through the real `oxabl lsp` and its daemon:
//! include references, literal `RUN` targets and `USING` names link to the one
//! file each names. A name that resolves to no file, or to more than one, has
//! no link, and so does text in an `&IF` branch the preprocessor did not take.

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

/// A link over occurrence `occurrence` of `text_at` in `MAIN`, to `target`.
#[track_caller]
fn link(text_at: &str, occurrence: usize, target: &Path) -> Value {
    let start = MAIN
        .match_indices(text_at)
        .nth(occurrence)
        .unwrap_or_else(|| panic!("no `{text_at}`"))
        .0;
    json!({
        "range": {"start": utf16_at(MAIN, start), "end": utf16_at(MAIN, start + text_at.len())},
        "target": file_uri(target)
    })
}

#[test]
fn each_reference_to_one_file_links_to_it_and_nothing_else_links() {
    let ws = Workspace::new();
    let main = ws.path("main.p");
    let mut session = Session::start(&ws.root);
    session.open(&main, MAIN);

    let answer = session.request(
        "textDocument/documentLink",
        json!({"textDocument": {"uri": file_uri(&main)}}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
    let mut links: Vec<Value> = answer["result"]
        .as_array()
        .unwrap_or_else(|| panic!("no list: {answer}"))
        .iter()
        .map(|link| json!({"range": link["range"], "target": link["target"]}))
        .collect();
    links.sort_by_key(|link| link["range"]["start"]["line"].as_u64());

    let helper = ws.path("util/helper.p");
    assert_eq!(
        links,
        vec![
            link("app.Greeter", 0, &ws.path("app/Greeter.cls")),
            link("{defs.i}", 0, &ws.path("defs.i")),
            link("{defs.i \"an argument\"}", 0, &ws.path("defs.i")),
            link("util/helper.p", 0, &helper),
            link("helper2", 0, &ws.path("helper2.p")),
            link("\"util/helper.p\"", 0, &helper),
        ]
    );
}
