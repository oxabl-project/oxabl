//! The document outline with the declarations of include files, through the
//! real `oxabl lsp`.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

use std::fmt::Write as _;
use std::path::Path;

use serde_json::{Value, json};
use session::{Session, file_uri};

const CONFIG: &str = "[workspace]\nname = \"p\"\n[workspace.sources]\ninclude_paths = [\".\"]\n";

fn hierarchical() -> Value {
    json!({"textDocument": {"documentSymbol": {"hierarchicalDocumentSymbolSupport": true}}})
}

fn project(files: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), CONFIG).unwrap();
    for (name, text) in files {
        std::fs::write(dir.path().join(name), text).unwrap();
    }
    dir
}

fn outline(session: &mut Session, path: &Path) -> Value {
    let answer = session.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": file_uri(path)}}),
    );
    assert!(
        answer.get("error").is_none(),
        "documentSymbol failed: {answer}"
    );
    answer["result"].clone()
}

/// One line for each symbol: indentation, the name, and the detail in brackets.
fn tree(symbols: &Value) -> String {
    fn walk(out: &mut String, symbols: &Value, depth: usize) {
        for symbol in symbols.as_array().map_or(&[][..], |list| list) {
            let _ = write!(
                out,
                "{}{}",
                "  ".repeat(depth),
                symbol["name"].as_str().unwrap()
            );
            if let Some(detail) = symbol["detail"].as_str() {
                let _ = write!(out, " [{detail}]");
            }
            out.push('\n');
            walk(out, &symbol["children"], depth + 1);
        }
    }
    let mut out = String::new();
    if symbols.is_array() {
        walk(&mut out, symbols, 0);
    }
    out
}

#[test]
fn an_include_shows_its_temp_table_and_procedure() {
    let dir = project(&[(
        "defs.i",
        "DEFINE TEMP-TABLE ttOrder FIELD id AS INTEGER.\nPROCEDURE helper:\nEND PROCEDURE.\n",
    )]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{defs.i}\nPROCEDURE own:\nEND PROCEDURE.\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  ttOrder\n    id [INTEGER]\n  helper\nown\n"
    );
}

#[test]
fn nested_includes_nest() {
    let dir = project(&[
        (
            "a.i",
            "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n{b.i}\nDEFINE VARIABLE va2 AS INTEGER NO-UNDO.\n",
        ),
        ("b.i", "DEFINE VARIABLE vb AS INTEGER NO-UNDO.\n"),
    ]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{a.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "a.i [a.i]\n  va [INTEGER]\n  b.i [b.i]\n    vb [INTEGER]\n  va2 [INTEGER]\n"
    );
}

#[test]
fn selecting_a_declaration_goes_to_the_include_file() {
    let dir = project(&[(
        "defs.i",
        "DEFINE VARIABLE one AS INTEGER NO-UNDO.\nPROCEDURE helper:\nEND PROCEDURE.\n",
    )]);
    let defs = dir.path().join("defs.i");
    let main = dir.path().join("main.p");

    // A client that takes only a flat list gets a location in the include.
    let mut flat = Session::start(dir.path());
    flat.open(&main, "MESSAGE \"x\".\n{defs.i}\n");
    let list = outline(&mut flat, &main);
    let helper = list
        .as_array()
        .unwrap()
        .iter()
        .find(|symbol| symbol["name"] == "helper")
        .unwrap_or_else(|| panic!("no helper in {list}"));
    assert_eq!(helper["location"]["uri"], json!(file_uri(&defs)));
    assert_eq!(helper["location"]["range"]["start"]["line"], json!(1));
    assert_eq!(helper["containerName"], json!("defs.i"));
    let include = list
        .as_array()
        .unwrap()
        .iter()
        .find(|symbol| symbol["name"] == "defs.i")
        .unwrap();
    assert_eq!(include["location"]["uri"], json!(file_uri(&main)));

    // A hierarchical client has only ranges of the buffer: the symbol selects
    // the reference, and go to definition there opens the include.
    let mut tree_session = Session::start_with(dir.path(), hierarchical());
    tree_session.open(&main, "MESSAGE \"x\".\n{defs.i}\n");
    let symbols = outline(&mut tree_session, &main);
    let include = &symbols[0];
    let child = &include["children"][1];
    assert_eq!(child["name"], json!("helper"));
    assert_eq!(child["selectionRange"], include["selectionRange"]);
    assert_eq!(child["selectionRange"]["start"]["line"], json!(1));
    let at = &child["selectionRange"]["start"];
    let definition = tree_session.request(
        "textDocument/definition",
        json!({"textDocument": {"uri": file_uri(&main)}, "position": at}),
    );
    assert_eq!(definition["result"][0]["uri"], json!(file_uri(&defs)));
}

#[test]
fn an_include_that_does_not_resolve_is_a_marked_symbol() {
    let dir = project(&[]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{gone.i}\nDEFINE VARIABLE x AS INTEGER NO-UNDO.\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "gone.i [not found]\nx [INTEGER]\n"
    );
}

#[test]
fn the_setting_hides_the_declarations_of_includes() {
    let dir = project(&[("defs.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n")]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with_options(
        dir.path(),
        hierarchical(),
        json!({"outline": {"includes": false}}),
    );
    session.open(&main, "{defs.i}\n");
    assert_eq!(tree(&outline(&mut session, &main)), "defs.i\n");

    let set = |session: &mut Session, includes: bool| {
        session.send(json!({
            "jsonrpc": "2.0", "method": "workspace/didChangeConfiguration",
            "params": {"settings": {"oxabl": {"outline": {"includes": includes}}}}
        }));
    };
    set(&mut session, true);
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  one [INTEGER]\n"
    );
    set(&mut session, false);
    assert_eq!(tree(&outline(&mut session, &main)), "defs.i\n");
}

#[test]
fn text_that_an_if_does_not_compile_adds_nothing() {
    let dir = project(&[
        ("defs.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n"),
        (
            "cond.i",
            "&IF FALSE &THEN\nDEFINE VARIABLE hidden AS INTEGER NO-UNDO.\n&ELSE\n\
             DEFINE VARIABLE shown AS INTEGER NO-UNDO.\n&ENDIF\n",
        ),
    ]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "&IF FALSE &THEN\n{defs.i}\n&ENDIF\n{cond.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [not compiled]\ncond.i [cond.i]\n  shown [INTEGER]\n"
    );
}

#[test]
fn a_cycle_is_marked_where_it_closes() {
    let dir = project(&[
        ("a.i", "DEFINE VARIABLE va AS INTEGER NO-UNDO.\n{b.i}\n"),
        ("b.i", "{a.i}\nDEFINE VARIABLE vb AS INTEGER NO-UNDO.\n"),
    ]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{a.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "a.i [a.i]\n  va [INTEGER]\n  b.i [b.i]\n    a.i [not expanded]\n    vb [INTEGER]\n"
    );
}

#[test]
fn an_unsaved_include_shows_its_buffer() {
    let dir = project(&[("defs.i", "DEFINE VARIABLE onDisk AS INTEGER NO-UNDO.\n")]);
    let defs = dir.path().join("defs.i");
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{defs.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  onDisk [INTEGER]\n"
    );
    session.open(&defs, "DEFINE VARIABLE unsaved AS INTEGER NO-UNDO.\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  unsaved [INTEGER]\n"
    );
}

#[test]
fn an_include_that_names_its_arguments_shows_the_names_as_written() {
    let dir = project(&[(
        "defs.i",
        "DEFINE {1} SHARED VARIABLE {&prefix}count AS INTEGER NO-UNDO.\n",
    )]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{defs.i NEW &prefix=my}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  {&prefix}count [INTEGER]\n"
    );
}

/// The number of levels of `symbols`, and the names at every level.
fn levels_and_names(symbols: &Value) -> (usize, Vec<String>) {
    fn walk(symbols: &Value, names: &mut Vec<String>) -> usize {
        let mut levels = 0;
        for symbol in symbols.as_array().map_or(&[][..], |list| list) {
            names.push(symbol["name"].as_str().unwrap().to_string());
            levels = levels.max(1 + walk(&symbol["children"], names));
        }
        levels
    }
    let mut names = Vec::new();
    let levels = walk(symbols, &mut names);
    (levels, names)
}

#[test]
fn a_deep_chain_of_includes_gives_a_tree_the_client_can_read() {
    let depth = 40;
    let mut files: Vec<(String, String)> = Vec::new();
    for level in 1..=depth {
        let next = if level < depth {
            format!("{{i{}.i}}\n", level + 1)
        } else {
            String::new()
        };
        files.push((
            format!("i{level}.i"),
            format!("DEFINE VARIABLE v{level} AS INTEGER NO-UNDO.\n{next}"),
        ));
    }
    let refs: Vec<(&str, &str)> = files
        .iter()
        .map(|(name, text)| (name.as_str(), text.as_str()))
        .collect();
    let dir = project(&refs);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{i1.i}\n");
    let (levels, names) = levels_and_names(&outline(&mut session, &main));
    assert!(levels <= 33, "{levels} levels");
    for level in 1..=depth {
        assert!(
            names.contains(&format!("v{level}")),
            "v{level} is lost: {names:?}"
        );
    }
    assert_eq!(
        outline(&mut session, &main).as_array().unwrap().len(),
        1,
        "the root symbol"
    );
}

#[test]
fn an_include_read_many_times_costs_a_bounded_outline() {
    let mut body = String::new();
    for n in 0..10 {
        body.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    let dir = project(&[("defs.i", &body)]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, &"{defs.i}\n".repeat(600));
    let answer = outline(&mut session, &main);
    let symbols = answer.as_array().unwrap();
    assert_eq!(symbols.len(), 600, "every reference keeps its symbol");
    let shown = symbols
        .iter()
        .filter(|symbol| symbol["children"].as_array().is_some_and(|c| c.len() == 10))
        .count();
    let marked = symbols
        .iter()
        .filter(|symbol| symbol["detail"] == json!("too large") && symbol["children"].is_null())
        .count();
    assert_eq!((shown, marked), (500, 100));
}

#[test]
fn an_include_that_alone_is_too_large_is_marked() {
    let mut body = String::new();
    for n in 0..5_001 {
        body.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    let dir = project(&[("big.i", &body)]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{big.i}\nPROCEDURE own:\nEND PROCEDURE.\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "big.i [too large]\nown\n"
    );
}

#[test]
fn without_an_include_path_an_include_is_not_found() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("oxabl.toml"), "[workspace]\nname = \"p\"\n").unwrap();
    std::fs::write(
        dir.path().join("defs.i"),
        "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n",
    )
    .unwrap();
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{defs.i}\n");
    // The configured include path is the only place oxabl looks.
    assert_eq!(tree(&outline(&mut session, &main)), "defs.i [not found]\n");
}

#[test]
fn a_panic_in_one_include_marks_it_and_keeps_the_rest() {
    let dir = project(&[
        (
            "bad.i",
            "/* OXABL-TEST-PANIC:outline */\nDEFINE VARIABLE hidden AS INTEGER NO-UNDO.\n",
        ),
        ("good.i", "DEFINE VARIABLE shown AS INTEGER NO-UNDO.\n"),
    ]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{bad.i}\n{good.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "bad.i [not expanded]\ngood.i [good.i]\n  shown [INTEGER]\n"
    );
}
