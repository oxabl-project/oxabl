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

    // A client that takes only a flat list gets the reference of the include
    // as the place of each declaration, in the document it asked about.
    let mut flat = Session::start(dir.path());
    flat.open(&main, "MESSAGE \"x\".\n{defs.i}\n");
    let list = outline(&mut flat, &main);
    let find = |name: &str| {
        list.as_array()
            .unwrap()
            .iter()
            .find(|symbol| symbol["name"] == name)
            .unwrap_or_else(|| panic!("no {name} in {list}"))
    };
    let helper = find("helper");
    assert_eq!(helper["location"]["uri"], json!(file_uri(&main)));
    assert_eq!(find("defs.i")["location"]["uri"], json!(file_uri(&main)));
    assert_eq!(helper["location"]["range"]["start"]["line"], json!(1));
    assert_eq!(helper["containerName"], json!("defs.i"));

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
        .filter(|symbol| {
            symbol["detail"] == json!("outline limit reached") && symbol["children"].is_null()
        })
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

#[test]
fn an_include_written_through_a_value_shows_at_its_reference() {
    let dir = project(&[("c.i", "DEFINE VARIABLE inc AS INTEGER NO-UNDO.\n")]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(
        &main,
        "&SCOPED-DEFINE inc {c.i}\n&SCOPED-DEFINE bad {nothere.i}\n{&inc}\n{&bad}\n",
    );
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "inc [SCOPED]\nbad [SCOPED]\n{&inc} [c.i]\n  inc [INTEGER]\n{&bad} [not found]\n"
    );
}

#[test]
fn a_branch_that_was_only_guessed_is_not_called_not_compiled() {
    let dir = project(&[("b.i", "DEFINE VARIABLE fromb AS INTEGER NO-UNDO.\n")]);
    // An include opened by itself does not know what its includer defines.
    let frag = dir.path().join("frag.i");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(
        &frag,
        "&IF DEFINED(x) &THEN\n{b.i}\nDEFINE VARIABLE fragv AS INTEGER NO-UNDO.\n&ENDIF\n\
         &IF FALSE &THEN\n{b.i}\nDEFINE VARIABLE never AS INTEGER NO-UNDO.\n&ENDIF\n",
    );
    assert_eq!(
        tree(&outline(&mut session, &frag)),
        "b.i [may not be compiled]\nfragv [INTEGER]\nb.i [not compiled]\nnever [INTEGER]\n"
    );
}

#[test]
fn an_include_that_a_shim_splits_shows_partial() {
    let dir = project(&[(
        "shim.i",
        "&IF DEFINED(OLDAPI) &THEN\nPROCEDURE oldp:\n&ELSE\nPROCEDURE newp:\n&ENDIF\n\
         \x20 DEFINE VARIABLE inner AS INTEGER NO-UNDO.\nEND PROCEDURE.\n",
    )]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{shim.i}\n");
    let answer = outline(&mut session, &main);
    assert_eq!(answer[0]["detail"], json!("shim.i (partial)"));
}

#[test]
fn an_include_that_alone_passes_the_limit_is_too_large_and_the_rest_is_not() {
    let mut big = String::new();
    for n in 0..5_001 {
        big.push_str(&format!("DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    let dir = project(&[
        ("big.i", &big),
        ("small.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n"),
    ]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{big.i}\n{small.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "big.i [too large]\nsmall.i [small.i]\n  one [INTEGER]\n"
    );
}

#[test]
fn a_save_shows_the_change_of_an_include_on_disk() {
    let dir = project(&[("defs.i", "DEFINE VARIABLE before AS INTEGER NO-UNDO.\n")]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, "{defs.i}\n");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  before [INTEGER]\n"
    );
    std::fs::write(
        dir.path().join("defs.i"),
        "DEFINE VARIABLE after AS INTEGER NO-UNDO.\n",
    )
    .unwrap();
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didSave",
        "params": {"textDocument": {"uri": file_uri(&main)}}
    }));
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  after [INTEGER]\n"
    );
}

/// An outline request that waits in the test hold of the server, and its id.
fn held_request(session: &mut Session, path: &Path) -> u64 {
    let id = session.next_request_id();
    session.send(json!({
        "jsonrpc": "2.0", "id": id, "method": "textDocument/documentSymbol",
        "params": {"textDocument": {"uri": file_uri(path)}}
    }));
    id
}

const HOLD: &str = "/* OXABL-TEST-HOLD:lsp.outline-includes@1500 */\n";

#[test]
fn an_edit_stops_the_outline_of_the_text_it_replaced() {
    let dir = project(&[("defs.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n")]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, &format!("{HOLD}{{defs.i}}\n"));
    let id = held_request(&mut session, &main);
    session.send(json!({
        "jsonrpc": "2.0", "method": "textDocument/didChange",
        "params": {
            "textDocument": {"uri": file_uri(&main), "version": 2},
            "contentChanges": [{"text": "{defs.i}\n"}]
        }
    }));
    let answer = session.wait_for(|m| m["id"] == json!(id));
    // -32801: ContentModified.
    assert_eq!(answer["error"]["code"], json!(-32801), "{answer}");
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  one [INTEGER]\n"
    );
}

#[test]
fn a_cancelled_request_stops_and_the_next_one_still_gets_its_outline() {
    let dir = project(&[("defs.i", "DEFINE VARIABLE one AS INTEGER NO-UNDO.\n")]);
    let main = dir.path().join("main.p");
    let mut session = Session::start_with(dir.path(), hierarchical());
    session.open(&main, &format!("{HOLD}{{defs.i}}\n"));
    let id = held_request(&mut session, &main);
    session.send(json!({
        "jsonrpc": "2.0", "method": "$/cancelRequest", "params": {"id": id}
    }));
    let answer = session.wait_for(|m| m["id"] == json!(id));
    // -32800: RequestCancelled.
    assert_eq!(answer["error"]["code"], json!(-32800), "{answer}");
    // The same version again: the cancelled run stored nothing wrong.
    assert_eq!(
        tree(&outline(&mut session, &main)),
        "defs.i [defs.i]\n  one [INTEGER]\n"
    );
}
