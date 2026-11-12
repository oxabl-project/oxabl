//! A `CREATE widget ... TRIGGERS:` block that does not parse.
//!
//! A trigger `DO:` without its `END` takes the statements after it into its
//! body, CREATEs included. Such a CREATE is skipped and parsed again from the
//! block around it, and once took time that doubled with each CREATE in a row:
//! a 1.3 KB file took 21 s. The skipped CREATE also lost the names in it, so its
//! widget handle read as an unused variable.

#![cfg(unix)]

use std::path::Path;
use std::process::{Command, Output};

const OXABL_BIN: &str = env!("CARGO_BIN_EXE_oxabl");

/// Seconds for one run of a debug build; the old behaviour needs years here.
const SECONDS: u32 = 60;

fn check(path: &Path) -> Output {
    Command::new("sh")
        .arg("-c")
        .arg(format!(r#"ulimit -c 0 && exec timeout {SECONDS} "$@""#))
        .arg("sh")
        .arg(OXABL_BIN)
        .arg("check")
        .arg(path)
        .output()
        .unwrap()
}

fn text(output: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn creates_with_unclosed_trigger_blocks_in_a_row() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("triggers.p");
    let unit = "CREATE BUTTON h TRIGGERS: ON CHOOSE DO: END TRIGGERS.\n";
    std::fs::write(&path, unit.repeat(30)).unwrap();
    let output = check(&path);
    assert!(
        output.status.code().is_some_and(|code| code <= 1),
        "expected exit 0 or 1 within {SECONDS} s (124 is the time limit), got {:?}\n{}",
        output.status,
        text(&output)
    );
}

#[test]
fn a_failed_trigger_block_keeps_the_names_of_its_create() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("button.p");
    std::fs::write(
        &path,
        "DEFINE VARIABLE hButton AS HANDLE NO-UNDO.\n\
         DEFINE VARIABLE hOther AS HANDLE NO-UNDO.\n\
         CREATE BUTTON hButton\n\
         \x20   TRIGGERS:\n\
         \x20       ON CHOOSE DO:\n\
         \x20           MESSAGE \"hi\".\n\
         \x20   END TRIGGERS.\n",
    )
    .unwrap();
    let output = check(&path);
    let text = text(&output);
    assert!(
        text.contains("unused variable `hOther`"),
        "expected the unused-variable rule to run:\n{text}"
    );
    assert!(
        !text.contains("unused variable `hButton`"),
        "the CREATE names hButton, so it is not unused:\n{text}"
    );
}
