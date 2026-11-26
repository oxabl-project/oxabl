//! The producer scan of `oxabl check` asks, for each file with a `{`, whether
//! the file may need its text expanded. The tokens answer for a file whose
//! includes stand as whole statements, so that file costs no parse.
//!
//! A test build of the binary counts the parses that decide this and prints the
//! count when `OXABL_TEST_SOURCE_MAPS` is set. All fixtures are synthetic ABL
//! written for this file.

use std::fs;
use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const ROOTS: usize = 6;

/// A root whose include stands as a statement: the tokens answer.
fn whole_statement_root(i: usize) -> String {
    format!(
        "/* root {i} */\n{{common.i}}\nDEFINE VARIABLE total AS INTEGER NO-UNDO.\n\
         total = {i}.\n{{common.i}}\nMESSAGE total.\n"
    )
}

/// A root with a reference inside a statement: only the parse can say whether
/// the text as written is whole.
fn mid_statement_root(i: usize) -> String {
    format!(
        "&SCOPED-DEFINE kind INTEGER\nDEFINE VARIABLE total AS {{&kind}} NO-UNDO.\n\
         total = {i}.\nMESSAGE total.\n"
    )
}

/// Run `oxabl check` over a directory of `ROOTS` roots made by `root`: the
/// number of parses the run made to decide whether a file may be expanded.
fn parses(root: fn(usize) -> String) -> usize {
    let tmp = TempDir::new().unwrap();
    fs::create_dir(tmp.path().join("src")).unwrap();
    fs::write(
        tmp.path().join("common.i"),
        "DEFINE VARIABLE shared-note AS CHARACTER NO-UNDO.\n",
    )
    .unwrap();
    for i in 0..ROOTS {
        fs::write(tmp.path().join(format!("src/r{i}.p")), root(i)).unwrap();
    }
    count(tmp.path())
}

fn count(dir: &Path) -> usize {
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(dir)
        .env("OXABL_TEST_SOURCE_MAPS", "1")
        .args(["check", "--no-format", "-j", "1", "-I", ".", "src"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: expansion parses: "))
        .unwrap_or_else(|| panic!("the test build printed no count; stderr:\n{stderr}"))
        .parse()
        .unwrap()
}

#[test]
fn a_root_with_whole_statement_includes_is_not_parsed_by_the_producer_scan() {
    assert_eq!(parses(whole_statement_root), 0);
}

#[test]
fn a_root_with_a_reference_inside_a_statement_is_parsed() {
    assert_eq!(parses(mid_statement_root), ROOTS);
}
