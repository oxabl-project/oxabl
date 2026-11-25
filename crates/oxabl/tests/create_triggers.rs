//! A `CREATE widget ... TRIGGERS:` block that does not parse.
//!
//! A trigger `DO:` without its `END` takes the statements after it into its
//! body, CREATEs included. Such a CREATE is skipped and parsed again from the
//! block around it, and once took time that doubled with each CREATE in a row:
//! a 1.3 KB file took 21 s. The skipped CREATE also lost the names in it, so its
//! widget handle read as an unused variable.
//!
//! Near the nesting limit a CREATE skipped this way can parse at a shallower
//! depth, so a file whose parse met the limit must say so, even when the
//! statement that met it was skipped.

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
fn a_skipped_create_that_met_the_nesting_limit_leaves_a_parse_error() {
    let unclosed = "CREATE BUTTON h TRIGGERS: ON CHOOSE DO: END TRIGGERS.\n";
    let nest = |n: usize, inner: &str| {
        format!(
            "DEFINE VARIABLE hx AS HANDLE NO-UNDO.\n{unclosed}{}{inner}\n{}",
            "DO:\n".repeat(n),
            "END.\n".repeat(n)
        )
    };
    let files = [
        // Met the limit in its ASSIGN inside the unclosed block, parses one
        // level up, and holds a syntax error.
        (
            "assign.p",
            nest(
                255,
                "CREATE BUTTON hx ASSIGN LABEL = \"a\" \
                 TRIGGERS: ON CHOOSE DO: x = . END. END TRIGGERS.",
            ),
        ),
        // Each CREATE opens a block the next one sits in.
        ("rows.p", unclosed.repeat(400)),
        // A nest too deep after a CREATE that failed for another reason.
        ("after.p", nest(300, "")),
    ];
    let dir = tempfile::tempdir().unwrap();
    for (name, source) in files {
        let path = dir.path().join(name);
        std::fs::write(&path, source).unwrap();
        let output = check(&path);
        let text = text(&output);
        assert_eq!(
            text.matches("Nesting is deeper than").count(),
            1,
            "{name}: expected one nesting error:\n{text}"
        );
        assert!(
            text.contains("file has parse errors"),
            "{name}: a file with a parse error is not formatted:\n{text}"
        );
    }
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

/// The unclosed `DO:` of the first CREATE takes the next lines into its trigger
/// block, `USING` included. The CREATE is skipped and the lines are read again,
/// and the import must not reach the CREATE before it. Without it `Method` is
/// no type there, the second CREATE is skipped too and its names are harvested.
#[test]
fn a_using_read_in_a_rewound_create_does_not_reach_an_earlier_statement() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("using.p");
    std::fs::write(
        &path,
        "CREATE BUTTON h TRIGGERS: ON CHOOSE DO: END TRIGGERS.\n\
         CREATE BUTTON hx TRIGGERS: ON CHOOSE DEFINE VARIABLE v AS Method NO-UNDO. END TRIGGERS.\n\
         USING Foo.Method.\n",
    )
    .unwrap();
    let output = check(&path);
    let text = text(&output);
    assert!(
        !text.contains("LINT0001") && !text.contains("LINT0002"),
        "the import comes after the second CREATE, so `Method` is no type there:\n{text}"
    );
}

fn check_source(name: &str, source: &str) -> String {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join(name);
    std::fs::write(&path, source).unwrap();
    text(&check(&path))
}

/// A CREATE that oxabl cannot parse is skipped, but the file says so: it has a
/// parse error and the formatter leaves it alone.
#[test]
fn a_create_that_does_not_parse_is_a_parse_error() {
    let sources = [
        // The `DO:` has no `END.`.
        "CREATE BUTTON h TRIGGERS: ON CHOOSE DO: END TRIGGERS.\n",
        "CREATE BUTTON h ASSIGN LABEL ! = \"x\".\n",
        "CREATE X-DOCUMENT h BOGUS-CLAUSE.\n",
        "CREATE WIDGET-POOL \"p\" BOGUS.\n",
        "CREATE \"Excel.Application\" h BOGUS.\n",
        "CREATE VALUE(cClass) h BOGUS.\n",
    ];
    for source in sources {
        let text = check_source("bad.p", source);
        assert_eq!(
            text.matches("error[PARSE001]").count(),
            1,
            "expected one parse error for {source:?}:\n{text}"
        );
        assert!(
            text.contains("file has parse errors"),
            "a file with a parse error is not formatted ({source:?}):\n{text}"
        );
    }
}

/// A failed CREATE inside the trigger block of an earlier failed CREATE is read
/// again after the skip of that one, and reports its own error then.
#[test]
fn a_failed_create_read_again_keeps_its_parse_error() {
    let text = check_source(
        "again.p",
        "CREATE BUTTON a TRIGGERS: ON CHOOSE DO: END TRIGGERS.\n\
         CREATE BUTTON b ASSIGN LABEL ! = \"x\".\n",
    );
    assert_eq!(
        text.matches("error[PARSE001]").count(),
        2,
        "expected an error for each failed CREATE:\n{text}"
    );
}

/// Forms oxabl does not model are skipped on purpose: they are valid ABL.
#[test]
fn a_valid_create_that_oxabl_does_not_model_is_no_parse_error() {
    let sources = [
        "CREATE ALIAS dbalias FOR DATABASE mydb.\n",
        "CREATE SERVER hServer.\n",
        "CREATE WIDGET-POOL.\n",
    ];
    for source in sources {
        let text = check_source("valid.p", source);
        assert!(
            !text.contains("PARSE001") && !text.contains("parse errors"),
            "{source:?} is valid ABL:\n{text}"
        );
    }
}
