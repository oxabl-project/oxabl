//! `textDocument/completion` after `RUN`, in the braces of an include
//! reference, after `{&` and after `{` with digits, through the real
//! `oxabl lsp`: the procedures, programs and directories, the include files,
//! the preprocessor names and the arguments of the including reference.
//!
//! All fixtures are synthetic ABL.

#![cfg(unix)]

#[path = "lsp/session.rs"]
mod session;

#[path = "lsp/completion.rs"]
mod completion;

use std::collections::BTreeSet;
use std::path::Path;

use completion::{Case, FUNCTION, Files, List, Ws, documentation};
use serde_json::{Value, json};
use session::file_uri;

const FILE: u64 = 17;
const FOLDER: u64 = 19;
const CONSTANT: u64 = 21;

/// Config A or N with the programs and includes of the spec.
fn tree(files: Files) -> Files {
    files
        .file("top.p", "MESSAGE \"top\".\n")
        .file("util/helper.p", "MESSAGE \"helper\".\n")
        .file("util/report.w", "MESSAGE \"report\".\n")
        .file("jobs/nightly.p", "MESSAGE \"nightly\".\n")
        .file("lib/libprog.p", "MESSAGE \"lib\".\n")
        .file(
            "inc/defs.i",
            "DEFINE VARIABLE gcDefs AS CHARACTER NO-UNDO.\n",
        )
        .file("inc/readme.txt", "notes\n")
        .file("lib/common.i", "/* common */\n")
}

/// The set of `(kind, label)` pairs is exactly `wanted`, with no duplicate.
#[track_caller]
fn exactly(list: &List, wanted: &[(u64, &str)]) {
    let got: BTreeSet<(u64, String)> = list
        .items
        .iter()
        .map(|i| {
            (
                i["kind"].as_u64().unwrap(),
                completion::label(i).to_string(),
            )
        })
        .collect();
    let want: BTreeSet<(u64, String)> = wanted.iter().map(|(k, l)| (*k, l.to_string())).collect();
    assert_eq!(got, want, "the items of {}", list.summary());
    assert_eq!(
        list.items.len(),
        want.len(),
        "an item is listed twice in {}",
        list.summary()
    );
}

fn range(line: u32, from: u32, to: u32) -> Value {
    json!({"start": {"line": line, "character": from}, "end": {"line": line, "character": to}})
}

/// The selection of `root` as the includer context of `include`.
#[track_caller]
fn select(ws: &mut Ws, include: &str, root: &Path) {
    let answer = ws.session.request(
        "oxabl/selectIncluderContext",
        json!({"textDocument": {"uri": file_uri(&ws.path(include))}, "root": root}),
    );
    assert!(answer.get("error").is_none(), "{answer}");
}

#[track_caller]
fn detail(list: &List, kind: u64, label: &str) -> String {
    list.item(kind, label)["detail"]
        .as_str()
        .unwrap_or_else(|| panic!("`{label}` has no detail in {}", list.summary()))
        .to_string()
}

const RUN_LOCAL: &str = "RUN |\nPROCEDURE localProc:\nEND PROCEDURE.\n";

/// P1.
#[test]
fn run_lists_procedures_programs_and_directories() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", RUN_LOCAL);
    let list = answer.list();
    list.contains(FUNCTION, &["localProc"]);
    assert_eq!(detail(list, FUNCTION, "localProc"), "PROCEDURE");
    list.contains(FILE, &["top.p", "libprog.p"]);
    list.contains(FOLDER, &["util/", "jobs/"]);
    for folder in ["util/", "jobs/"] {
        assert_eq!(
            list.item(FOLDER, folder)["command"]["command"],
            json!("editor.action.triggerSuggest"),
            "`{folder}` asks for a new list: {}",
            list.item(FOLDER, folder)
        );
    }
}

/// P2.
#[test]
fn run_in_a_directory_lists_its_programs() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "RUN util/|");
    let list = answer.list();
    exactly(list, &[(FILE, "util/helper.p"), (FILE, "util/report.w")]);
    for item in &list.items {
        let label = completion::label(item);
        let edit = &item["textEdit"];
        assert_eq!(edit["newText"], json!(label), "{item}");
        assert_eq!(edit["replace"], range(0, 4, 9), "{item}");
        assert_eq!(edit["insert"], range(0, 4, 9), "{item}");
        assert_eq!(item["filterText"], json!(label), "{item}");
    }
}

/// P3.
#[test]
fn the_word_filters_the_programs() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "RUN util/h|");
    exactly(answer.list(), &[(FILE, "util/helper.p")]);
}

/// P4.
#[test]
fn run_without_an_include_path_gives_the_procedures_incompletely() {
    let mut ws = tree(Files::config_n(Case::Upper)).start();
    let answer = ws.complete("main.p", RUN_LOCAL);
    let list = answer.list();
    assert!(list.incomplete, "{}", list.summary());
    exactly(list, &[(FUNCTION, "localProc")]);
}

/// P5.
#[test]
fn after_the_target_there_is_no_list() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    ws.complete("main.p", "RUN util/helper.p |").assert_null();
}

/// P6.
#[test]
fn braces_offer_the_include_files_and_directories() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "{inc/|}");
    let list = answer.list();
    list.contains(FILE, &["inc/defs.i", "inc/readme.txt"]);
    list.assert_before("inc/defs.i", "inc/readme.txt");

    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "{|}");
    answer.list().contains(FOLDER, &["inc/"]);
    answer.list().contains(FILE, &["common.i"]);
}

/// P7.
#[test]
fn preprocessor_names_are_the_ones_defined_before_the_cursor() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete(
        "main.p",
        "&GLOBAL-DEFINE gMax 10
&SCOPED-DEFINE sName abc
&SCOPED-DEFINE sGone x
&UNDEFINE sGone
DISPLAY {&|}.
&SCOPED-DEFINE sLater 1
",
    );
    let list = answer.list();
    list.contains(
        CONSTANT,
        &["gMax", "sName", "OPSYS", "FILE-NAME", "LINE-NUMBER"],
    );
    assert_eq!(detail(list, CONSTANT, "gMax"), "10");
    list.lacks(&["sGone", "sLater"]);
}

fn args_workspace() -> Ws {
    tree(Files::config_a(Case::Upper))
        .file("args_user.p", "{inc/args.i alpha beta}\n")
        .file("inc/args.i", "DISPLAY {}.\n")
        .file("named_user.p", "{inc/named.i &mode=fast}\n")
        .file("inc/named.i", "DISPLAY \"{&}\".\n")
        .start()
}

/// P8.
#[test]
fn an_include_offers_the_arguments_of_the_including_reference() {
    let mut ws = args_workspace();
    let cursor = ws.open_marked("inc/args.i", "DISPLAY {|}.\n");
    let root = ws.canonical("args_user.p");
    select(&mut ws, "inc/args.i", &root);
    let answer = ws.complete_at("inc/args.i", cursor, None);
    let list = answer.list();
    list.contains(CONSTANT, &["1", "2"]);
    assert_eq!(detail(list, CONSTANT, "1"), "alpha");
    assert_eq!(detail(list, CONSTANT, "2"), "beta");
    list.contains(FOLDER, &["inc/"]);
    list.assert_before("1", "inc/");

    let mut ws = args_workspace();
    let cursor = ws.open_marked("inc/args.i", "DISPLAY {2|}.\n");
    let root = ws.canonical("args_user.p");
    select(&mut ws, "inc/args.i", &root);
    let answer = ws.complete_at("inc/args.i", cursor, None);
    exactly(answer.list(), &[(CONSTANT, "2")]);

    let mut ws = args_workspace();
    let cursor = ws.open_marked("inc/named.i", "DISPLAY \"{&|}\".\n");
    let root = ws.canonical("named_user.p");
    select(&mut ws, "inc/named.i", &root);
    let answer = ws.complete_at("inc/named.i", cursor, None);
    let list = answer.list();
    list.contains(CONSTANT, &["mode"]);
    assert_eq!(detail(list, CONSTANT, "mode"), "fast");
}

/// P9.
#[test]
fn include_arguments_in_fragment_mode_give_no_list() {
    let mut ws = args_workspace();
    ws.complete("inc/args.i", "DISPLAY {1|}.\n").assert_null();
}

/// P10.
#[test]
fn a_program_resolves_to_its_path() {
    let mut ws = tree(Files::config_a(Case::Upper)).start();
    let answer = ws.complete("main.p", "RUN util/|");
    let item = answer.list().item(FILE, "util/helper.p").clone();
    let text = documentation(&ws.resolve(&item));
    assert!(text.contains("util/helper.p"), "{text}");
}
