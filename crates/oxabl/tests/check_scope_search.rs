//! `oxabl check` finds the scope of each block without a scan of all the
//! scopes of the file. A scan for each block makes the resolve pass quadratic:
//! blocks times scopes.
//!
//! A debug build of the binary with the test feature counts every scope record
//! that a scope tree reads, and prints the count when `OXABL_TEST_SCOPES_READ`
//! is set. A release build has no count (a bench build must not count), so
//! the tests exist only with debug assertions, as in the CI test job. Each
//! test checks one file with a number of blocks and one with ten times as
//! many, and wants the count to grow about ten times.
//! All fixtures are synthetic ABL written for this file.

#![cfg(debug_assertions)]

use std::fs;
use std::process::Command;

use tempfile::TempDir;

/// `count` internal procedures, each with a `DO` block, a `REPEAT` block and
/// a `CATCH` block in it: four scopes for each procedure.
fn procedures(count: usize) -> String {
    let mut source = String::from("DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
    for i in 0..count {
        source.push_str(&format!(
            "PROCEDURE proc-{i}:\n\
             \x20   DEFINE VARIABLE n-{i} AS INTEGER NO-UNDO.\n\
             \x20   DO n-{i} = 1 TO 3:\n\
             \x20       total = total + n-{i}.\n\
             \x20   END.\n\
             \x20   REPEAT:\n\
             \x20       LEAVE.\n\
             \x20   END.\n\
             \x20   CATCH err-{i} AS Progress.Lang.Error:\n\
             \x20       MESSAGE err-{i}:GetMessage(1).\n\
             \x20   END CATCH.\n\
             END PROCEDURE.\n"
        ));
    }
    source.push_str("MESSAGE total.\n");
    source
}

/// A class with, for each of `count` indexes, a temp-table, a button, a
/// property with `GET` and `SET`, and a method with a `DO`, a `REPEAT`, a
/// `FOR EACH`, an `ON` trigger, and a `CATCH` and a `FINALLY` block. It also
/// has a constructor and a destructor.
fn class_members(count: usize) -> String {
    let mut source = String::from("CLASS Big:\n");
    for i in 0..count {
        source.push_str(&format!(
            "DEFINE TEMP-TABLE tt-{i} NO-UNDO FIELD f AS INTEGER.\n\
             DEFINE BUTTON btn-{i} LABEL \"x\".\n\
             DEFINE PUBLIC PROPERTY p-{i} AS INTEGER NO-UNDO\n\
             \x20   GET():\n\
             \x20       RETURN 1.\n\
             \x20   END GET.\n\
             \x20   SET(v AS INTEGER):\n\
             \x20       MESSAGE v.\n\
             \x20   END SET.\n\
             METHOD PUBLIC VOID m-{i}(INPUT a AS INTEGER):\n\
             \x20   DO a = 1 TO 3:\n\
             \x20       MESSAGE a.\n\
             \x20   END.\n\
             \x20   REPEAT:\n\
             \x20       LEAVE.\n\
             \x20   END.\n\
             \x20   FOR EACH tt-{i}:\n\
             \x20       MESSAGE tt-{i}.f.\n\
             \x20   END.\n\
             \x20   ON CHOOSE OF btn-{i} DO:\n\
             \x20       MESSAGE a.\n\
             \x20   END.\n\
             \x20   DO ON ERROR UNDO, THROW:\n\
             \x20       MESSAGE a.\n\
             \x20       CATCH err AS Progress.Lang.Error:\n\
             \x20           MESSAGE err:GetMessage(1).\n\
             \x20       END CATCH.\n\
             \x20       FINALLY:\n\
             \x20           MESSAGE a.\n\
             \x20       END FINALLY.\n\
             \x20   END.\n\
             END METHOD.\n"
        ));
    }
    source.push_str(
        "CONSTRUCTOR PUBLIC Big():\n    MESSAGE \"c\".\nEND CONSTRUCTOR.\n\
         DESTRUCTOR PUBLIC Big():\n    MESSAGE \"d\".\nEND DESTRUCTOR.\n\
         END CLASS.\n",
    );
    source
}

/// The number of scope records that `oxabl check` of `file` reads.
fn scopes_read(file: &str, source: &str) -> usize {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join(file), source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .env("OXABL_TEST_SCOPES_READ", "1")
        .args(["check", "--no-format", "-j", "1", file])
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.code().is_some_and(|code| code <= 1),
        "oxabl check failed: {:?}\nstderr:\n{stderr}",
        out.status
    );
    stderr
        .lines()
        .find_map(|line| line.strip_prefix("oxabl-test: scope records read: "))
        .unwrap_or_else(|| panic!("the test build printed no count; stderr:\n{stderr}"))
        .parse()
        .unwrap()
}

/// Linear: about 10 times. A scan for each block: about 100 times.
fn assert_linear(what: &str, few: usize, many: usize) {
    assert!(few > 0, "the run read no scope record");
    assert!(
        many <= few * 12,
        "ten times the {what} read {many} scope records against {few}: \
         the search for the scope of a block is not constant"
    );
}

#[test]
fn the_scope_search_grows_with_the_number_of_scopes() {
    let few = scopes_read("blocks.p", &procedures(300));
    let many = scopes_read("blocks.p", &procedures(3_000));
    assert_linear("blocks", few, many);
}

#[test]
fn the_scope_search_of_class_members_grows_with_the_number_of_scopes() {
    let few = scopes_read("Big.cls", &class_members(300));
    let many = scopes_read("Big.cls", &class_members(3_000));
    assert_linear("class members", few, many);
}
