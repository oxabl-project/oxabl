//! `SEM0001` ("already declared in this scope") through `oxabl check`: names the
//! compiler accepts twice must not be reported, and a real duplicate must be.
//!
//! The tests drive the built binary over a temporary file. All fixtures are
//! synthetic ABL written for this file.

use std::fs;
use std::process::Command;

use serde_json::Value;
use tempfile::TempDir;

/// The `(code, line)` of every semantic diagnostic `oxabl check` reports for
/// `source` saved as `name`.
fn semantic_findings(name: &str, source: &str) -> Vec<(String, u64)> {
    let tmp = TempDir::new().unwrap();
    let file = tmp.path().join(name);
    fs::write(&file, source).unwrap();
    let out = Command::new(env!("CARGO_BIN_EXE_oxabl"))
        .args(["check", "--no-format", "--no-lint", "--json"])
        .arg(&file)
        .output()
        .unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    let report: Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}):\n{stdout}"));
    report["diagnostics"]
        .as_array()
        .expect("diagnostics array")
        .iter()
        .map(|d| {
            (
                d["code"].as_str().unwrap().to_string(),
                d["start"]["line"].as_u64().unwrap(),
            )
        })
        .collect()
}

fn sem0001(code: &str, line: u64) -> (String, u64) {
    assert_eq!(code, "SEM0001");
    (code.to_string(), line)
}

const TT: &str = "DEFINE TEMP-TABLE tt-item NO-UNDO FIELD item-num AS INTEGER.\n";

#[test]
fn input_and_output_table_parameters_for_one_table_declare_it_once() {
    let source = format!(
        "{TT}PROCEDURE bump:\n  DEFINE INPUT  PARAMETER TABLE FOR tt-item.\n  DEFINE OUTPUT PARAMETER TABLE FOR tt-item.\n  FOR EACH tt-item:\n    tt-item.item-num = tt-item.item-num + 1.\n  END.\nEND PROCEDURE.\n"
    );
    assert_eq!(semantic_findings("bump.p", &source), vec![]);
}

#[test]
fn input_and_output_table_parameters_at_the_top_of_a_program_declare_it_once() {
    let source = format!(
        "{TT}DEFINE INPUT PARAMETER TABLE FOR tt-item.\nDEFINE OUTPUT PARAMETER TABLE FOR tt-item.\n"
    );
    assert_eq!(semantic_findings("top.p", &source), vec![]);
}

#[test]
fn a_table_parameter_written_twice_in_one_direction_is_still_a_duplicate() {
    let source = format!(
        "{TT}DEFINE INPUT PARAMETER TABLE FOR tt-item.\nDEFINE INPUT PARAMETER TABLE FOR tt-item.\n"
    );
    assert_eq!(
        semantic_findings("twice.p", &source),
        vec![sem0001("SEM0001", 3)]
    );
}

#[test]
fn a_variable_named_like_a_table_parameter_is_still_a_duplicate() {
    let source = format!(
        "{TT}DEFINE INPUT PARAMETER TABLE FOR tt-item.\nDEFINE VARIABLE tt-item AS INTEGER NO-UNDO.\n"
    );
    assert_eq!(
        semantic_findings("clash.p", &source),
        vec![sem0001("SEM0001", 3)]
    );
}

#[test]
fn a_stream_defined_twice_in_one_scope_is_accepted() {
    let source = "DEFINE STREAM s.\nDEFINE STREAM s.\nOUTPUT STREAM s TO \"/dev/null\".\nOUTPUT STREAM s CLOSE.\n";
    assert_eq!(semantic_findings("stream.p", source), vec![]);
}

#[test]
fn a_variable_defined_twice_in_one_scope_is_still_a_duplicate() {
    let source =
        "DEFINE VARIABLE v AS INTEGER NO-UNDO.\nDEFINE VARIABLE v AS INTEGER NO-UNDO.\nv = 1.\n";
    assert_eq!(
        semantic_findings("var.p", source),
        vec![sem0001("SEM0001", 2)]
    );
}

#[test]
fn internal_procedures_whose_names_hold_periods_are_different_names() {
    let source = "RUN setup_1.2.1_data.\nRUN setup_1.2.2_data.\nPROCEDURE setup_1.2.1_data:\nEND PROCEDURE.\nPROCEDURE setup_1.2.2_data:\nEND PROCEDURE.\n";
    assert_eq!(semantic_findings("dots.p", source), vec![]);
}

#[test]
fn an_internal_procedure_with_periods_in_its_name_defined_twice_is_still_a_duplicate() {
    let source = "RUN setup_1.2.1_data.\nPROCEDURE setup_1.2.1_data:\nEND PROCEDURE.\nPROCEDURE setup_1.2.1_data:\nEND PROCEDURE.\n";
    assert_eq!(
        semantic_findings("dots-twice.p", source),
        vec![sem0001("SEM0001", 4)]
    );
}

#[test]
fn methods_that_differ_only_by_extent_are_different_overloads() {
    let source = "CLASS Overloads:\n  METHOD PRIVATE VOID fixture(INPUT pKind AS CHARACTER, INPUT pDates AS DATETIME EXTENT 4):\n  END METHOD.\n  METHOD PRIVATE VOID fixture(INPUT pKind AS CHARACTER, INPUT pDate AS DATETIME):\n  END METHOD.\n  METHOD PRIVATE VOID addr(INPUT pLines AS CHARACTER EXTENT 2):\n  END METHOD.\n  METHOD PRIVATE VOID addr(INPUT pLines AS CHARACTER EXTENT 3):\n  END METHOD.\n  METHOD PRIVATE VOID dyn(INPUT pLines AS CHARACTER EXTENT):\n  END METHOD.\n  METHOD PRIVATE VOID dyn(INPUT pLines AS CHARACTER):\n  END METHOD.\nEND CLASS.\n";
    assert_eq!(semantic_findings("Overloads.cls", source), vec![]);
}

#[test]
fn methods_with_the_same_extent_signature_are_still_a_duplicate() {
    let source = "CLASS Dup:\n  METHOD PRIVATE VOID addr(INPUT pLines AS CHARACTER EXTENT 2):\n  END METHOD.\n  METHOD PRIVATE VOID addr(INPUT pOther AS CHARACTER EXTENT 2):\n  END METHOD.\n  METHOD PRIVATE VOID one(INPUT p AS CHARACTER):\n  END METHOD.\n  METHOD PRIVATE VOID one(INPUT q AS CHARACTER):\n  END METHOD.\nEND CLASS.\n";
    assert_eq!(
        semantic_findings("Dup.cls", source),
        vec![sem0001("SEM0001", 4), sem0001("SEM0001", 8)]
    );
}
