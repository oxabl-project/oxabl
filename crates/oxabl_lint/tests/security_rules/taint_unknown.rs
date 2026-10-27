//! Stage 2 never hides a sink it cannot judge: a unit past its work budget,
//! summaries that did not converge, and a variable no unit of the file
//! declares all leave the sink reported as stage 1 reports it.

use oxabl_common::{Diagnostic, SecurityOptions};
use oxabl_semantic::SourceContext;

use super::taint::{run_in, run_with, taint};

fn check_taint(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, taint())
}

fn check_sink(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, SecurityOptions::default())
}

/// Run `code` in taint mode over an include file checked on its own.
fn check_fragment(code: &str, src: &str) -> Vec<Diagnostic> {
    run_in(code, src, taint(), SourceContext::IncludeFragment)
}

/// 1-based lines of the findings.
fn lines(src: &str, diags: &[Diagnostic]) -> Vec<usize> {
    let mut out: Vec<usize> = diags
        .iter()
        .map(|d| src[..d.span.span.start as usize].matches('\n').count() + 1)
        .collect();
    out.sort();
    out
}

/// A main block with an input parameter and `n` variables chained to each
/// other, so that its solve runs past the work budget.
fn heavy_main(n: usize) -> String {
    let mut src = String::from("DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\n");
    for i in 0..n {
        src.push_str(&format!("DEFINE VARIABLE d{i} AS CHARACTER NO-UNDO.\n"));
    }
    for i in 0..n {
        src.push_str(&format!("d{i} = d{} + pc.\n", (i + 1) % n));
    }
    src
}

#[test]
fn a_main_block_past_its_budget_leaves_the_file_level_variables_unknown() {
    let mut src = heavy_main(1600);
    src.push_str("PROCEDURE p:\n  OS-COMMAND VALUE(pc).\nEND PROCEDURE.\n");
    src.push_str("OS-COMMAND VALUE(pc).\n");
    let sink = lines(&src, &check_sink("ABLSEC003", &src));
    assert_eq!(sink.len(), 2);
    assert_eq!(lines(&src, &check_taint("ABLSEC003", &src)), sink);
}

#[test]
fn a_routine_past_its_budget_returns_unknown() {
    let mut src = String::from("FUNCTION f RETURNS CHARACTER PRIVATE ():\n");
    src.push_str(&heavy_main(1600).replace("DEFINE INPUT PARAMETER", "DEFINE VARIABLE"));
    src.push_str("  RETURN get-value(\"cmd\").\nEND FUNCTION.\n");
    src.push_str("DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = f().\nOS-COMMAND VALUE(c).\n");
    assert_eq!(check_taint("ABLSEC003", &src).len(), 1);
}

/// `f0` returns its parameter, each `fN` returns `f(N-1)` of its own.
fn function_chain(n: usize) -> String {
    let mut src = String::from("DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\n");
    src.push_str(
        "FUNCTION f0 RETURNS CHARACTER PRIVATE (p AS CHARACTER): RETURN p. END FUNCTION.\n",
    );
    for i in 1..=n {
        src.push_str(&format!(
            "FUNCTION f{i} RETURNS CHARACTER PRIVATE (p AS CHARACTER): RETURN f{}(p). END FUNCTION.\n",
            i - 1
        ));
    }
    src.push_str(&format!(
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = f{n}(pc).\nOS-COMMAND VALUE(c).\n"
    ));
    src
}

#[test]
fn a_call_chain_deeper_than_the_rounds_is_still_reported() {
    for n in [3, 10, 13] {
        let src = function_chain(n);
        assert_eq!(check_taint("ABLSEC003", &src).len(), 1, "depth {n}");
    }
}

#[test]
fn a_constant_call_chain_that_converges_is_clean() {
    let src = function_chain(3).replace("c = f3(pc)", "c = f3(\"ls\")");
    assert!(check_taint("ABLSEC003", &src).is_empty());
}

#[test]
fn a_chain_of_file_level_variables_deeper_than_the_rounds_is_still_reported() {
    let n = 12;
    let mut src = String::from("DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\n");
    for i in 0..=n {
        src.push_str(&format!("DEFINE VARIABLE g{i} AS CHARACTER NO-UNDO.\n"));
    }
    src.push_str("g0 = pc.\n");
    for i in 1..=n {
        src.push_str(&format!(
            "PROCEDURE q{i} PRIVATE: g{i} = g{}. END PROCEDURE.\n",
            i - 1
        ));
    }
    src.push_str(&format!(
        "PROCEDURE sink PRIVATE: OS-COMMAND VALUE(g{n}). END PROCEDURE.\n"
    ));
    assert_eq!(check_taint("ABLSEC003", &src).len(), 1);
}

#[test]
fn a_variable_an_include_file_does_not_declare_is_unknown() {
    // `pc` comes from the includer.
    let src = "OS-COMMAND VALUE(pc).\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
    let src = "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = \"ls \" + pc.\nOS-COMMAND VALUE(c).\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
    let src = "PROCEDURE p:\n  OS-COMMAND VALUE(pc).\nEND PROCEDURE.\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
}

#[test]
fn an_undeclared_name_in_a_whole_program_is_a_database_field() {
    // It compiles only as a field, and database fields are no source by
    // default.
    let src = "OS-COMMAND VALUE(pc).\n";
    assert!(check_taint("ABLSEC003", src).is_empty());
}

#[test]
fn an_undeclared_variable_that_the_include_sets_to_a_literal_is_clean() {
    let src = "pc = \"ls\".\nOS-COMMAND VALUE(pc).\n";
    assert!(check_fragment("ABLSEC003", src).is_empty());
}

#[test]
fn system_handles_and_defined_names_are_not_unknown() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
c = SESSION:TEMP-DIRECTORY + \"x\".
OS-COMMAND VALUE(c).
OS-COMMAND VALUE(\"ls \" + STRING(TODAY)).
OS-COMMAND VALUE(f).
";
    assert!(check_fragment("ABLSEC003", src).is_empty());
}

#[test]
fn a_member_a_class_inherits_is_unknown() {
    let src = "\
CLASS p INHERITS base:
  METHOD PUBLIC VOID Go():
    OS-COMMAND VALUE(cInherited).
  END METHOD.
END CLASS.
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}
