//! The parses `oxabl check` makes for each root. The producer scan asks of
//! each file with a `{` whether its text may need expanding; a class or
//! interface header answers from the tokens, so a class root costs one parse
//! fewer there than a procedure root.
//!
//! A test build of the binary counts every parse of a program and prints the
//! count when `OXABL_TEST_PARSES` is set. Each test runs `ROOTS` and twice
//! `ROOTS` roots of one shape and reads the parses of one more root, so a
//! parse added on any route (the producer scan, the lint, the index) shows.
//! All fixtures are synthetic ABL written for this file.

use std::fs;
use std::process::Command;

use tempfile::TempDir;

const ROOTS: usize = 3;

/// A class whose include stands inside its body.
fn class_root(i: usize) -> (String, String) {
    (
        format!("src/c{i}.cls"),
        format!(
            "CLASS c{i}:\n{{members.i}}\n  METHOD PUBLIC VOID run():\n  END METHOD.\nEND CLASS.\n"
        ),
    )
}

/// A procedure whose include stands as a statement.
fn procedure_root(i: usize) -> (String, String) {
    (
        format!("src/p{i}.p"),
        format!(
            "/* root {i} */\n{{members.i}}\nDEFINE VARIABLE total AS INTEGER NO-UNDO.\n\
             total = {i}.\nMESSAGE total.\n"
        ),
    )
}

/// The parses of `oxabl check` over `count` roots made by `root`.
fn parses(root: fn(usize) -> (String, String), count: usize) -> usize {
    let tmp = TempDir::new().unwrap();
    fs::create_dir(tmp.path().join("src")).unwrap();
    fs::write(
        tmp.path().join("members.i"),
        "DEFINE PRIVATE VARIABLE note AS CHARACTER NO-UNDO.\n",
    )
    .unwrap();
    // The producer scan runs only when a root asks for a `SHARED` name.
    fs::write(
        tmp.path().join("src/consumer.p"),
        "DEFINE SHARED VARIABLE gv AS INTEGER NO-UNDO.\nMESSAGE gv.\n",
    )
    .unwrap();
    for i in 0..count {
        let (path, text) = root(i);
        fs::write(tmp.path().join(path), text).unwrap();
    }
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .env("OXABL_TEST_PARSES", "1")
        .args(["check", "--no-format", "-j", "1", "-I", ".", "src"])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: parses: "))
        .unwrap_or_else(|| panic!("the test build printed no count; stderr:\n{stderr}"))
        .parse()
        .unwrap()
}

/// The parses that one more root of this shape costs.
fn per_root(root: fn(usize) -> (String, String)) -> usize {
    let few = parses(root, ROOTS);
    let many = parses(root, 2 * ROOTS);
    assert_eq!(
        (many - few) % ROOTS,
        0,
        "{few} parses for {ROOTS} roots, {many} for {}",
        2 * ROOTS
    );
    (many - few) / ROOTS
}

#[test]
fn a_class_root_is_parsed_once_less_than_a_procedure_root() {
    let class = per_root(class_root);
    let procedure = per_root(procedure_root);
    // The producer scan reads the header of a class from its tokens.
    assert_eq!(
        class + 1,
        procedure,
        "parses per root: class {class}, procedure {procedure}"
    );
}

#[test]
fn the_parses_per_root_do_not_grow() {
    // The lint of the expanded unit, and for a procedure the producer scan. A
    // higher number is a parse that came back; a lower one is a gain: lower
    // the number.
    assert_eq!(per_root(class_root), CLASS_PARSES, "parses per class root");
    assert_eq!(
        per_root(procedure_root),
        PROCEDURE_PARSES,
        "parses per procedure root"
    );
}

const CLASS_PARSES: usize = 1;
const PROCEDURE_PARSES: usize = 2;
