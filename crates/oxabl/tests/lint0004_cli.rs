//! `type-mismatch-assignment` (LINT0004) through `oxabl check`: date constants,
//! the type and place of an `INITIAL` value, and the place of a finding. Every
//! case runs the built binary over a synthetic file; the compiler verdict each
//! one follows is stated beside it.

use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// One LINT0004 finding: its message and the source text its span covers.
#[derive(Debug, PartialEq)]
struct Finding {
    message: String,
    text: String,
    line: u64,
}

/// The LINT0004 findings of `oxabl check --json --no-format` on `source`.
#[track_caller]
fn lint0004(source: &str) -> Vec<Finding> {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("t.p");
    fs::write(&file, source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--json", "--no-format"])
        .arg(&file)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let v: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}"));
    assert!(
        v["failures"].as_array().is_some_and(|f| f.is_empty()),
        "the file was not checked:\n{stdout}"
    );
    v["diagnostics"]
        .as_array()
        .expect("diagnostics array")
        .iter()
        .filter(|d| d["code"] == "LINT0004")
        .map(|d| {
            let start = d["span"]["start"].as_u64().unwrap() as usize;
            let end = d["span"]["end"].as_u64().unwrap() as usize;
            Finding {
                message: d["message"].as_str().unwrap().to_string(),
                text: source[start..end].to_string(),
                line: d["start"]["line"].as_u64().unwrap(),
            }
        })
        .collect()
}

#[track_caller]
fn assert_clean(source: &str) {
    let findings = lint0004(source);
    assert!(findings.is_empty(), "unexpected findings: {findings:?}");
}

// ---------------------------------------------------------------------------
// Date constants
// ---------------------------------------------------------------------------

#[test]
fn a_date_constant_assigned_to_a_date_is_clean() {
    // OpenEdge 12.8 compiles each of these.
    assert_clean("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = 02/01/2026.\nMESSAGE d.\n");
    assert_clean("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = 2/1/26.\nMESSAGE d.\n");
    assert_clean("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = 10/2.\nMESSAGE d.\n");
    assert_clean("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = 02/01/2026 + 1.\nMESSAGE d.\n");
}

#[test]
fn a_date_constant_as_initial_value_of_a_date_is_clean() {
    assert_clean("DEFINE VARIABLE dStart AS DATE NO-UNDO INITIAL 02/01/2026.\nMESSAGE dStart.\n");
}

#[test]
fn a_spaced_division_assigned_to_a_date_is_still_a_mismatch() {
    // The compiler: "Incompatible data types in expression or assignment".
    let findings = lint0004("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = 10 / 2 / 5.\nMESSAGE d.\n");
    assert_eq!(findings.len(), 1, "{findings:?}");
    assert_eq!(
        findings[0].message,
        "type mismatch: cannot assign `DECIMAL` to `DATE`"
    );
}

#[test]
fn a_date_constant_assigned_to_a_decimal_is_a_mismatch() {
    // The compiler rejects `x = 10/2.` for a DECIMAL `x`: `10/2` is a date.
    for source in [
        "DEFINE VARIABLE x AS DECIMAL NO-UNDO.\nx = 02/01/2026.\nMESSAGE x.\n",
        "DEFINE VARIABLE x AS DECIMAL NO-UNDO.\nx = 10/2.\nMESSAGE x.\n",
    ] {
        let findings = lint0004(source);
        assert_eq!(findings.len(), 1, "{source:?}: {findings:?}");
        assert_eq!(
            findings[0].message,
            "type mismatch: cannot assign `DATE` to `DECIMAL`"
        );
    }
}

#[test]
fn format_leaves_a_date_constant_unchanged() {
    let source = "DEFINE VARIABLE d AS DATE NO-UNDO FORMAT 99/99/9999.\n\
                  d = 02/01/2026.\n\
                  d = 2/1/26 + 1.\n\
                  d = 10/2.\n\
                  MESSAGE 10 / 2 d.\n";
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join("t.p");
    fs::write(&file, source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["format", "--stdout"])
        .arg(&file)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&out.stdout), source);
}

// ---------------------------------------------------------------------------
// Where a finding goes
// ---------------------------------------------------------------------------

#[test]
fn a_mismatch_on_an_array_element_is_reported_at_the_element() {
    let findings = lint0004(
        "DEFINE VARIABLE iQty AS INTEGER NO-UNDO EXTENT 3.\niQty[1] = \"5\".\nMESSAGE iQty[1].\n",
    );
    assert_eq!(
        findings,
        [Finding {
            message: "type mismatch: cannot assign `CHARACTER` to `INTEGER`".into(),
            text: "iQty[1]".into(),
            line: 2,
        }]
    );
}

#[test]
fn a_mismatch_in_an_assign_statement_is_reported_at_its_element() {
    let findings = lint0004(
        "DEFINE VARIABLE iQty AS INTEGER NO-UNDO EXTENT 3.\n\
         ASSIGN iQty[3] = 1\n  iQty[2] = \"x\".\nMESSAGE iQty[1].\n",
    );
    assert_eq!(findings.len(), 1, "{findings:?}");
    assert_eq!(findings[0].text, "iQty[2]");
    assert_eq!(findings[0].line, 3);
}

#[test]
fn a_mismatched_initial_value_is_reported_at_the_value() {
    // The compiler: "Invalid date input. Unable to use initial value "5"".
    let findings = lint0004("DEFINE VARIABLE d AS DATE NO-UNDO INITIAL 5.\nMESSAGE d.\n");
    assert_eq!(
        findings,
        [Finding {
            message: "type mismatch: cannot assign `INTEGER` to `DATE`".into(),
            text: "5".into(),
            line: 1,
        }]
    );
}

// ---------------------------------------------------------------------------
// INITIAL constants the compiler converts from their text
// ---------------------------------------------------------------------------

#[test]
fn a_character_initial_value_for_a_logical_is_clean() {
    // OpenEdge 12.8 compiles `INITIAL "yes"` for a LOGICAL.
    assert_clean("DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO INITIAL \"yes\".\nMESSAGE lFlag.\n");
}

#[test]
fn a_character_initial_value_for_other_converted_types_is_clean() {
    // Each compiles on OpenEdge 12.8.
    assert_clean(
        "DEFINE VARIABLE i AS INTEGER NO-UNDO INITIAL \"5\".\n\
         DEFINE VARIABLE n AS DECIMAL NO-UNDO INITIAL \"1.5\".\n\
         DEFINE VARIABLE d AS DATE NO-UNDO INITIAL \"02/01/2026\".\n\
         DEFINE VARIABLE h AS HANDLE NO-UNDO INITIAL \"5\".\n\
         MESSAGE i n d h.\n",
    );
}

#[test]
fn any_constant_initial_value_for_a_character_is_clean() {
    // Each compiles on OpenEdge 12.8: the compiler keeps the constant's text.
    assert_clean(
        "DEFINE VARIABLE c1 AS CHARACTER NO-UNDO INITIAL 5.\n\
         DEFINE VARIABLE c2 AS CHARACTER NO-UNDO INITIAL yes.\n\
         DEFINE VARIABLE c3 AS CHARACTER NO-UNDO INITIAL 02/01/2026.\n\
         DEFINE VARIABLE c4 AS LONGCHAR NO-UNDO INITIAL 1.5.\n\
         DEFINE VARIABLE c5 AS CHARACTER NO-UNDO INITIAL -5.\n\
         DEFINE VARIABLE c6 AS LONGCHAR NO-UNDO INITIAL -1.5.\n\
         DEFINE VARIABLE c7 AS CHARACTER NO-UNDO INITIAL +5.\n\
         MESSAGE c1 c2 c3 c4 c5 c6 c7.\n",
    );
}

#[test]
fn a_numeric_initial_value_for_a_logical_is_still_a_mismatch() {
    // The compiler: "Input value: 1 should be yes/no".
    let findings =
        lint0004("DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO INITIAL 1.\nMESSAGE lFlag.\n");
    assert_eq!(
        findings,
        [Finding {
            message: "type mismatch: cannot assign `INTEGER` to `LOGICAL`".into(),
            text: "1".into(),
            line: 1,
        }]
    );
}

#[test]
fn a_signed_numeric_initial_value_for_a_logical_is_still_a_mismatch() {
    // The compiler: "Input value: -1 should be yes/no".
    let findings =
        lint0004("DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO INITIAL -1.\nMESSAGE lFlag.\n");
    assert_eq!(findings.len(), 1, "{findings:?}");
    assert_eq!(findings[0].text, "-1");
}

#[test]
fn a_character_value_assigned_to_a_date_is_still_a_mismatch() {
    // Only an INITIAL constant converts: `d = "02/01/2026".` does not compile.
    let findings =
        lint0004("DEFINE VARIABLE d AS DATE NO-UNDO.\nd = \"02/01/2026\".\nMESSAGE d.\n");
    assert_eq!(findings.len(), 1, "{findings:?}");
    assert_eq!(
        findings[0].message,
        "type mismatch: cannot assign `CHARACTER` to `DATE`"
    );
    assert_eq!(findings[0].text, "d");
}
