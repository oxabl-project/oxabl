//! `oxabl check` finds the scope of each block without a scan of all the
//! scopes of the file. A scan for each block makes the resolve pass quadratic:
//! blocks times scopes.
//!
//! A test build of the binary counts the scope records that the searches of a
//! scope tree read, and prints the count when `OXABL_TEST_SCOPES_READ` is set.
//! The test checks one file with a number of blocks and one with ten times as
//! many, and wants the count to grow about ten times. All fixtures are
//! synthetic ABL written for this file.

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

/// The number of scope records that `oxabl check` of `count` procedures read.
fn scopes_read(count: usize) -> usize {
    let tmp = TempDir::new().unwrap();
    fs::write(tmp.path().join("blocks.p"), procedures(count)).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .current_dir(tmp.path())
        .env("OXABL_TEST_SCOPES_READ", "1")
        .args(["check", "--no-format", "-j", "1", "blocks.p"])
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

#[test]
fn the_scope_search_grows_with_the_number_of_scopes() {
    let few = scopes_read(300);
    let many = scopes_read(3_000);
    assert!(few > 0, "the run read no scope record");
    // Linear: about 10 times. A scan for each block: about 100 times.
    assert!(
        many <= few * 12,
        "ten times the blocks read {many} scope records against {few}: \
         the search for the scope of a block is not constant"
    );
}
