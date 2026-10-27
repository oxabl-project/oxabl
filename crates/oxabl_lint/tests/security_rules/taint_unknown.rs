//! Stage 2 never hides a sink it cannot judge: a unit past its work budget,
//! summaries that did not converge, and a variable no unit of the file
//! declares all leave the sink reported as stage 1 reports it.

use oxabl_common::{Diagnostic, SecurityOptions};
use oxabl_schema::Schema;
use oxabl_schema::test_support::customer_schema;
use oxabl_semantic::SourceContext;

use super::taint::{run_in, run_on, run_with, taint};

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

/// Run `code` in taint mode over a whole program in which an include did not
/// arrive at each `{}` of `src`.
fn check_elided(code: &str, src: &str) -> Vec<Diagnostic> {
    let mut at = Vec::new();
    let mut text = String::new();
    for (i, part) in src.split("{}").enumerate() {
        if i > 0 {
            at.push(text.len() as u32);
        }
        text.push_str(part);
    }
    run_on(code, &text, taint(), &Schema::empty(), |c| {
        c.with_elided(&at).with_elided_includes(&at)
    })
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

#[test]
fn an_input_dataset_fills_its_tables() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE DATASET ds FOR tt.
DEFINE INPUT PARAMETER DATASET FOR ds.
FIND FIRST tt.
OS-COMMAND VALUE(tt.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn read_json_fills_the_table_with_its_source() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE INPUT PARAMETER plc AS LONGCHAR NO-UNDO.
TEMP-TABLE tt:READ-JSON(\"longchar\", plc).
FIND FIRST tt.
OS-COMMAND VALUE(tt.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
    let fixed = src.replace("plc)", "\"x\")");
    assert!(check_taint("ABLSEC003", &fixed).is_empty());
}

#[test]
fn read_xml_on_a_dataset_handle_fills_the_handle() {
    let src = "\
DEFINE INPUT PARAMETER plc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE hDs AS HANDLE NO-UNDO.
hDs:READ-XML(\"longchar\", plc, \"empty\", ?, ?).
OS-COMMAND VALUE(hDs:GET-BUFFER-HANDLE(1):BUFFER-FIELD(\"f\"):BUFFER-VALUE).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn buffer_copy_carries_the_fields_of_its_source() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE TEMP-TABLE t2 NO-UNDO FIELD f AS CHARACTER.
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
CREATE tt. tt.f = pc.
CREATE t2.
BUFFER-COPY tt TO t2.
OS-COMMAND VALUE(t2.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
    let fixed = src.replace("tt.f = pc", "tt.f = \"ls\"");
    assert!(check_taint("ABLSEC003", &fixed).is_empty());
}

#[test]
fn a_public_property_or_variable_is_a_source() {
    let src = "\
CLASS p:
  DEFINE PUBLIC PROPERTY Cmd AS CHARACTER NO-UNDO GET. SET.
  DEFINE PROTECTED VARIABLE cArg AS CHARACTER NO-UNDO.
  DEFINE PRIVATE PROPERTY Fixed AS CHARACTER NO-UNDO INITIAL \"ls\" GET. SET.
  DEFINE PUBLIC PROPERTY ReadOnly AS CHARACTER NO-UNDO INITIAL \"ls\" GET. PRIVATE SET.
  METHOD PUBLIC VOID Go():
    OS-COMMAND VALUE(Cmd).
    OS-COMMAND VALUE(cArg).
    OS-COMMAND VALUE(Fixed).
    OS-COMMAND VALUE(ReadOnly).
  END METHOD.
END CLASS.
";
    assert_eq!(lines(src, &check_taint("ABLSEC003", src)), [7, 8]);
}

#[test]
fn a_field_of_a_table_an_include_file_does_not_define_is_unknown() {
    // `ttReq` is a temp-table of the includer.
    let src = "OS-COMMAND VALUE(ttReq.cmd).\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
    let src = "FIND FIRST ttReq.\nOS-COMMAND VALUE(\"ls \" + ttReq.cmd).\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
    // Without a schema, a database table cannot be told apart from it.
    let src = "FIND FIRST Customer.\nOS-COMMAND VALUE(Customer.Name).\n";
    assert_eq!(check_fragment("ABLSEC003", src).len(), 1);
}

#[test]
fn a_field_of_a_table_the_include_defines_or_the_schema_knows_is_judged() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
CREATE tt.
tt.f = \"ls\".
OS-COMMAND VALUE(tt.f).
";
    assert!(check_fragment("ABLSEC003", src).is_empty());
    let schema = customer_schema();
    for src in [
        "FIND FIRST Customer.\nOS-COMMAND VALUE(Customer.Name).\n",
        "FIND FIRST Cust.\nOS-COMMAND VALUE(Cust.Name).\n",
    ] {
        let found = run_on("ABLSEC003", src, taint(), &schema, |c| {
            c.with_source_context(SourceContext::IncludeFragment)
        });
        assert!(found.is_empty(), "{src}");
    }
    // A class name before `:` is no table.
    let src = "OS-COMMAND VALUE(STRING(Progress.Lang.Class:GetClass(\"x\"))).\n";
    assert!(check_fragment("ABLSEC003", src).is_empty());
}

#[test]
fn an_include_that_did_not_arrive_may_set_a_declared_variable() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
{}
OS-COMMAND VALUE(c).
";
    assert!(check_taint("ABLSEC003", &src.replace("{}", "")).is_empty());
    assert_eq!(lines(src, &check_elided("ABLSEC003", src)), [4]);
    // In a procedure, a main block variable.
    let src = "\
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
{}
PROCEDURE p:
  OS-COMMAND VALUE(c).
END PROCEDURE.
";
    assert_eq!(check_elided("ABLSEC003", src).len(), 1);
    // The parameter of a routine that the include may run.
    let src = "\
PROCEDURE p PRIVATE:
  DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
  OS-COMMAND VALUE(pc).
END PROCEDURE.
{}
";
    assert_eq!(check_elided("ABLSEC003", src).len(), 1);
}

#[test]
fn an_include_that_did_not_arrive_leaves_the_locals_of_other_units_judged() {
    let src = "\
{}
PROCEDURE p:
  DEFINE VARIABLE d AS CHARACTER NO-UNDO.
  d = \"ls\".
  OS-COMMAND VALUE(d).
END PROCEDURE.
";
    assert!(check_elided("ABLSEC003", src).is_empty());
}

#[test]
fn read_json_through_a_handle_fills_the_table_the_handle_was_set_to() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE INPUT PARAMETER plc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE hTT AS HANDLE NO-UNDO.
hTT = TEMP-TABLE tt:HANDLE.
hTT:READ-JSON(\"longchar\", plc).
FIND FIRST tt.
OS-COMMAND VALUE(tt.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
    let fixed = src.replace("plc)", "\"x\")");
    assert!(check_taint("ABLSEC003", &fixed).is_empty());
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
DEFINE DATASET ds FOR tt.
DEFINE INPUT PARAMETER plc AS LONGCHAR NO-UNDO.
DEFINE VARIABLE hDs AS HANDLE NO-UNDO.
ASSIGN hDs = DATASET ds:HANDLE.
hDs:READ-XML(\"longchar\", plc, \"empty\", ?, ?).
FIND FIRST tt.
OS-COMMAND VALUE(tt.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
}

#[test]
fn read_json_or_read_xml_from_a_file_is_a_file_read() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER.
TEMP-TABLE tt:READ-JSON(\"FILE\", \"/tmp/upload.json\").
FIND FIRST tt.
OS-COMMAND VALUE(tt.f).
";
    assert_eq!(check_taint("ABLSEC003", src).len(), 1);
    let xml = src.replace("READ-JSON(\"FILE\"", "READ-XML(\"file\"");
    assert_eq!(check_taint("ABLSEC003", &xml).len(), 1);
    let unknown = src.replace("(\"FILE\"", "(cType");
    assert_eq!(check_taint("ABLSEC003", &unknown).len(), 1);
    let literal = src.replace("\"FILE\"", "\"longchar\"");
    assert!(check_taint("ABLSEC003", &literal).is_empty());
}

#[test]
fn an_unreserved_keyword_may_be_a_variable_of_the_includer() {
    for name in ["extent", "server", "method", "size", "total"] {
        let src = format!("OS-COMMAND VALUE({name}).\n");
        assert_eq!(check_fragment("ABLSEC003", &src).len(), 1, "{name}");
    }
}

#[test]
fn a_local_of_another_routine_is_no_name_of_the_include_file() {
    let src = "\
PROCEDURE helper:
  DEFINE VARIABLE pcArg AS CHARACTER NO-UNDO.
  pcArg = \"x\".
END PROCEDURE.
OS-COMMAND VALUE(pcArg).
";
    assert_eq!(lines(src, &check_fragment("ABLSEC003", src)), [5]);
    let own = "\
PROCEDURE helper:
  DEFINE VARIABLE pcArg AS CHARACTER NO-UNDO.
  pcArg = \"x\".
  OS-COMMAND VALUE(pcArg).
END PROCEDURE.
";
    assert!(check_fragment("ABLSEC003", own).is_empty());
}
