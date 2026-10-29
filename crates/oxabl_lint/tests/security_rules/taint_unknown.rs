//! Stage 2 never hides a sink it cannot judge: a unit past its work budget,
//! summaries that did not converge, and a variable no unit of the file
//! declares all leave the sink reported as stage 1 reports it.

use oxabl_common::{Diagnostic, ParameterSources, SecurityOptions};
use oxabl_schema::Schema;
use oxabl_schema::test_support::customer_schema;
use oxabl_semantic::SourceContext;

use super::taint::{run_in, run_on, run_with, taint};

fn check_taint(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(code, src, taint())
}

/// Run `code` in taint mode with a work budget small enough for a short test.
fn check_taint_budget(code: &str, src: &str) -> Vec<Diagnostic> {
    run_with(
        code,
        src,
        SecurityOptions {
            work_budget: Some(5_000),
            ..taint()
        },
    )
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
/// other, so that its solve runs past a small work budget.
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
    let mut src = heavy_main(100);
    src.push_str("PROCEDURE p:\n  OS-COMMAND VALUE(pc).\nEND PROCEDURE.\n");
    src.push_str("OS-COMMAND VALUE(pc).\n");
    let sink = lines(&src, &check_sink("ABLSEC003", &src));
    assert_eq!(sink.len(), 2);
    assert_eq!(lines(&src, &check_taint_budget("ABLSEC003", &src)), sink);
}

#[test]
fn a_routine_past_its_budget_returns_unknown() {
    let mut src = String::from("FUNCTION f RETURNS CHARACTER PRIVATE ():\n");
    src.push_str(&heavy_main(100).replace("DEFINE INPUT PARAMETER", "DEFINE VARIABLE"));
    src.push_str("  RETURN get-value(\"cmd\").\nEND FUNCTION.\n");
    src.push_str("DEFINE VARIABLE c AS CHARACTER NO-UNDO.\nc = f().\nOS-COMMAND VALUE(c).\n");
    assert_eq!(check_taint_budget("ABLSEC003", &src).len(), 1);
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
        "FIND FIRST customer.\nOS-COMMAND VALUE(CUSTOMER.Name).\n",
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
fn a_qualifier_that_abbreviates_a_database_table_is_a_table_only_with_its_field() {
    // `c` and `cust` abbreviate `Customer`, which has no field `cmd`: a buffer
    // of the includer. An abbreviation is one even with a field of
    // `Customer`: the includer may define `c` as a buffer of a table with a
    // field `Name`.
    let schema = customer_schema();
    for src in [
        "OS-COMMAND VALUE(c.cmd).\n",
        "OS-COMMAND VALUE(cust.cmd).\n",
        "OS-COMMAND VALUE(Customer.cmd).\n",
        "FIND FIRST Cust.\nOS-COMMAND VALUE(Cust.Name + cust.cmd).\n",
        "OS-COMMAND VALUE(c.Name).\n",
        "FIND FIRST Cust.\nOS-COMMAND VALUE(Cust.Name).\n",
    ] {
        let found = run_on("ABLSEC003", src, taint(), &schema, |c| {
            c.with_source_context(SourceContext::IncludeFragment)
        });
        assert_eq!(found.len(), 1, "{src}");
    }
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

#[test]
fn a_shared_variable_or_table_is_unknown_where_code_outside_the_file_sets_it() {
    let src = "\
DEFINE SHARED VARIABLE gcShared AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE gcGlobal AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gcNew AS CHARACTER NO-UNDO.
DEFINE SHARED TEMP-TABLE ttShared NO-UNDO FIELD cArg AS CHARACTER.
OS-COMMAND VALUE(gcShared).
OS-COMMAND VALUE(gcGlobal).
FIND FIRST ttShared.
OS-COMMAND VALUE(ttShared.cArg).
gcNew = \"ls\".
OS-COMMAND VALUE(gcNew).
RUN other.p.
OS-COMMAND VALUE(gcNew).
PROCEDURE p:
  OS-COMMAND VALUE(gcShared).
END PROCEDURE.
";
    assert_eq!(
        lines(src, &check_taint("ABLSEC003", src)),
        [5, 6, 8, 12, 14]
    );
    assert_eq!(check_sink("ABLSEC003", src).len(), 6);
}

#[test]
fn a_shared_variable_is_unknown_after_any_call_of_code_the_file_does_not_hold() {
    let forms = [
        "RUN setup IN hLib.",
        "RUN VALUE(\"setup.p\").",
        "RUN SUPER.",
        "IF libSet() THEN.",
        "IF superSet() THEN.",
        "DYNAMIC-FUNCTION(\"libSet\" IN hLib).",
        "DYNAMIC-INVOKE(\"app.Lib\", \"Setup\").",
        "PUBLISH \"setup\".",
        "APPLY \"close\" TO hLib.",
        "WAIT-FOR CLOSE OF THIS-PROCEDURE.",
        "PROCESS EVENTS.",
        "UPDATE cX.",
        "PROMPT-FOR cX.",
        "o = NEW app.Thing().",
        "o:Setup().",
        "o:Mode = \"x\".",
        "cX = o:Mode.",
        "app.Lib:Setup().",
        "cX = Lib:Version.",
        "DELETE OBJECT o.",
        "RUN wrap.",
        "hCall:INVOKE().",
    ];
    let mut src = String::from(
        "DEFINE NEW SHARED VARIABLE gc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE hCall AS HANDLE NO-UNDO.
DEFINE VARIABLE o AS app.Thing NO-UNDO.
DEFINE VARIABLE cX AS CHARACTER NO-UNDO.
FUNCTION libSet RETURNS LOGICAL () IN hLib.
FUNCTION superSet RETURNS LOGICAL () IN SUPER.
",
    );
    for form in forms {
        src.push_str(&format!("gc = \"ls\".\n{form}\nOS-COMMAND VALUE(gc).\n"));
    }
    src.push_str(
        "PROCEDURE setup:
END PROCEDURE.
PROCEDURE wrap:
  RUN other.p.
END PROCEDURE.
",
    );
    let sinks: Vec<usize> = (0..forms.len()).map(|i| 10 + 3 * i).collect();
    assert_eq!(lines(&src, &check_taint("ABLSEC003", &src)), sinks);
}

#[test]
fn a_shared_variable_keeps_its_value_across_built_in_members_and_functions() {
    let src = "\
DEFINE NEW SHARED VARIABLE gc AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQ AS HANDLE NO-UNDO.
DEFINE VARIABLE hB AS HANDLE NO-UNDO.
DEFINE VARIABLE cX AS CHARACTER NO-UNDO.
DEFINE VARIABLE oJ AS Progress.Json.ObjectModel.JsonObject NO-UNDO.
DEFINE TEMP-TABLE ttA NO-UNDO FIELD f AS CHARACTER.
gc = \"ls\".
CREATE QUERY hQ.
hQ:QUERY-PREPARE(\"FOR EACH ttA\").
hQ:QUERY-OPEN().
hQ:PRIVATE-DATA = \"x\".
cX = SUBSTRING(gc, 1, 2) + ENTRY(1, \"a,b\").
oJ:Add(\"a\", 1).
hB = BUFFER ttA:HANDLE.
hB = Customer:HANDLE.
cX = hB:BUFFER-FIELD(\"f\"):BUFFER-VALUE.
hB = TEMP-TABLE ttA:DEFAULT-BUFFER-HANDLE.
MESSAGE STRING(LENGTH(cX)) SESSION:TEMP-DIRECTORY.
DELETE OBJECT hQ.
FIND FIRST ttA NO-ERROR.
OS-COMMAND VALUE(gc).
";
    // `Customer` is a table of the schema. Without one it may be a class, and
    // its member may run code elsewhere.
    let schema = customer_schema();
    assert!(run_on("ABLSEC003", src, taint(), &schema, |c| c).is_empty());
    assert_eq!(check_sink("ABLSEC003", src).len(), 1);
}

#[test]
fn a_call_and_its_sink_in_one_statement_see_what_the_call_writes() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gs AS CHARACTER NO-UNDO.
DEFINE VARIABLE gc AS CHARACTER NO-UNDO INITIAL \"ls\".
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
FUNCTION setIt RETURNS CHARACTER ():
  gc = pc.
  RETURN \"\".
END FUNCTION.
FUNCTION libSet RETURNS CHARACTER () IN hLib.
OS-COMMAND VALUE(setIt() + gc).
gs = \"ls\".
OS-COMMAND VALUE(libSet() + gs).
";
    assert_eq!(lines(src, &check_taint("ABLSEC003", src)), [10, 12]);
}

#[test]
fn a_call_of_a_function_takes_what_the_function_writes_to_the_file() {
    let src = "\
DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.
DEFINE VARIABLE gc AS CHARACTER NO-UNDO.
FUNCTION setIt RETURNS LOGICAL ():
  gc = pc.
  RETURN TRUE.
END FUNCTION.
gc = \"ls\".
OS-COMMAND VALUE(gc).
setIt().
OS-COMMAND VALUE(gc).
PROCEDURE p:
  gc = \"ls\".
  IF setIt() THEN OS-COMMAND VALUE(gc).
END PROCEDURE.
";
    assert_eq!(lines(src, &check_taint("ABLSEC003", src)), [10, 13]);
}

/// Each form calls `{call}` somewhere inside an expression or a clause.
const OPERAND_FORMS: [&str; 24] = [
    "IF {call} = TRUE THEN.",
    "IF {call} <> FALSE THEN.",
    "IF NOT {call} THEN.",
    "IF LENGTH(STRING({call})) > 0 THEN.",
    "IF {call} AND TRUE THEN.",
    "IF FALSE OR {call} THEN.",
    "IF STRING({call}) BEGINS \"a\" THEN.",
    "IF STRING({call}) MATCHES \"a*\" THEN.",
    "IF INTEGER({call}) >= 1 THEN.",
    "lX = {call} = TRUE.",
    "cX = (IF {call} THEN \"x\" ELSE \"y\").",
    "IF CAN-FIND(FIRST tt WHERE tt.k = STRING({call})) THEN.",
    "FOR EACH tt WHERE tt.k = STRING({call}): END.",
    "DO WHILE {call} = FALSE: END.",
    "REPEAT WHILE NOT {call}: LEAVE. END.",
    "DO iX = 1 TO INTEGER({call}): END.",
    "CASE cX: WHEN STRING({call}) THEN. END CASE.",
    "cArr[INTEGER({call})] = \"x\".",
    "cX = cArr[INTEGER({call})].",
    "iX = - INTEGER({call}).",
    "iX = 2 * INTEGER({call}).",
    "FIND FIRST tt WHERE tt.k = STRING({call}) NO-ERROR.",
    "MESSAGE (IF {call} THEN \"a\" ELSE \"b\").",
    "DISPLAY STRING({call}) = \"a\".",
];

const OPERAND_HEADER: &str = "\
DEFINE INPUT PARAMETER hLib AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE gs AS CHARACTER NO-UNDO.
DEFINE VARIABLE gc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cX AS CHARACTER NO-UNDO.
DEFINE VARIABLE lX AS LOGICAL NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cArr AS CHARACTER EXTENT 3 NO-UNDO.
DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS CHARACTER.
FUNCTION setIt RETURNS LOGICAL ():
  gc = get-value(\"cmd\").
  RETURN TRUE.
END FUNCTION.
FUNCTION noop RETURNS LOGICAL ():
  RETURN TRUE.
END FUNCTION.
";

/// The forms whose sink on `var` gives other than `want` findings when the
/// form calls `call`.
fn operand_misses(var: &str, call: &str, want: usize) -> Vec<&'static str> {
    OPERAND_FORMS
        .into_iter()
        .filter(|form| {
            let src = format!(
                "{OPERAND_HEADER}{var} = \"ls\".\n{}\nOS-COMMAND VALUE({var}).\n",
                form.replace("{call}", call)
            );
            check_taint("ABLSEC003", &src).len() != want
        })
        .collect()
}

#[test]
fn a_call_inside_any_operand_or_clause_writes_what_it_writes() {
    assert_eq!(operand_misses("gc", "setIt()", 1), Vec::<&str>::new());
    assert_eq!(operand_misses("gc", "TRUE", 0), Vec::<&str>::new());
}

#[test]
fn a_call_of_code_elsewhere_inside_any_operand_or_clause_makes_shared_variables_unknown() {
    assert_eq!(
        operand_misses("gs", "DYNAMIC-FUNCTION(\"f\" IN hLib)", 1),
        Vec::<&str>::new()
    );
    assert_eq!(operand_misses("gs", "noop()", 0), Vec::<&str>::new());
}

/// A name the file does not declare, before `:`, is a class or a name from
/// outside the file, whatever its member is called; a table the schema knows
/// is a buffer, whose keyword members run nothing elsewhere.
#[test]
fn a_keyword_member_of_an_undeclared_name_runs_code_elsewhere_unless_it_is_a_table() {
    let src = |call: &str| {
        format!(
            "DEFINE NEW SHARED VARIABLE gs AS CHARACTER NO-UNDO.\n\
             gs = \"ls\".\n{call}\nOS-COMMAND VALUE(gs).\n"
        )
    };
    for call in ["Other:Save().", "Other:Update().", "x = Other:Validate()."] {
        assert_eq!(check_taint("ABLSEC003", &src(call)).len(), 1, "{call}");
    }
    let schema = customer_schema();
    let on = |call: &str| run_on("ABLSEC003", &src(call), taint(), &schema, |c| c).len();
    assert_eq!(on("Customer:VALIDATE()."), 0);
    assert_eq!(on("Other:VALIDATE()."), 1);
    // Without a schema, the name may be a class.
    assert_eq!(
        check_taint("ABLSEC003", &src("Customer:VALIDATE().")).len(),
        1
    );
}

/// Each program sets `gc` to a constant, makes the call `{call}`, then runs
/// `gc`. Code elsewhere that the call runs may run a routine or trigger of the
/// file, or set a member of the class, in between.
const CALLBACKS: [&str; 6] = [
    "\
DEFINE VARIABLE gc AS CHARACTER NO-UNDO.
DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.
gc = \"ls\".
{call}
OS-COMMAND VALUE(gc).
PROCEDURE setGc:
  DEFINE INPUT PARAMETER p AS CHARACTER NO-UNDO.
  gc = p.
END PROCEDURE.
",
    "\
DEFINE VARIABLE gc AS CHARACTER NO-UNDO.
DEFINE VARIABLE o AS Progress.Lang.Object NO-UNDO.
DEFINE BUTTON btn.
ON CHOOSE OF btn DO:
  gc = get-value(\"cmd\").
END.
gc = \"ls\".
{call}
OS-COMMAND VALUE(gc).
",
    "\
CLASS Cb:
  DEFINE PRIVATE VARIABLE o AS Progress.Lang.Object NO-UNDO.
  DEFINE PRIVATE VARIABLE gc AS CHARACTER NO-UNDO.
  METHOD PUBLIC VOID SetGc(INPUT p AS CHARACTER):
    gc = p.
  END METHOD.
  METHOD PUBLIC VOID Go():
    gc = \"ls\".
    {call}
    OS-COMMAND VALUE(gc).
  END METHOD.
END CLASS.
",
    "\
CLASS Cfg:
  DEFINE PRIVATE VARIABLE o AS Progress.Lang.Object NO-UNDO.
  DEFINE PUBLIC STATIC VARIABLE gc AS CHARACTER NO-UNDO.
  METHOD PUBLIC VOID Go():
    gc = \"ls\".
    {call}
    OS-COMMAND VALUE(gc).
  END METHOD.
END CLASS.
",
    "\
CLASS Cfg:
  DEFINE PRIVATE VARIABLE o AS Progress.Lang.Object NO-UNDO.
  DEFINE PROTECTED VARIABLE gc AS CHARACTER NO-UNDO.
  METHOD PUBLIC VOID Go():
    gc = \"ls\".
    {call}
    OS-COMMAND VALUE(gc).
  END METHOD.
END CLASS.
",
    "\
CLASS Prop:
  DEFINE PRIVATE VARIABLE o AS Progress.Lang.Object NO-UNDO.
  DEFINE PUBLIC PROPERTY gc AS CHARACTER NO-UNDO GET. SET.
  METHOD PUBLIC VOID Go():
    gc = \"ls\".
    {call}
    OS-COMMAND VALUE(gc).
  END METHOD.
END CLASS.
",
];

/// The programs of [`CALLBACKS`] whose sink gives other than `want` findings
/// when they make the call `call`.
fn callback_misses(call: &str, want: usize) -> Vec<usize> {
    (0..CALLBACKS.len())
        .filter(|&i| {
            let src = CALLBACKS[i].replace("{call}", call);
            check_taint("ABLSEC003", &src).len() != want
        })
        .collect()
}

#[test]
fn a_routine_trigger_or_member_that_code_elsewhere_may_run_or_set_is_refreshed_after_its_call() {
    for call in [
        "RUN lib.p (THIS-PROCEDURE).",
        "WAIT-FOR CLOSE OF THIS-PROCEDURE.",
        "PROCESS EVENTS.",
        "PUBLISH \"go\".",
        "Other:Setup().",
        // A member named like a keyword of a class the file does not declare.
        "Other:Run().",
        "Other:Save().",
        "IF Other:Validate() THEN .",
        "o:ToString().",
        "DYNAMIC-FUNCTION(\"f\" IN SOURCE-PROCEDURE).",
    ] {
        assert_eq!(callback_misses(call, 1), Vec::<usize>::new(), "{call}");
    }
    assert_eq!(callback_misses("", 0), Vec::<usize>::new());
    // With no parameter or member a source, a member that code outside the
    // class may set is still Unknown after the call, as a shared variable is.
    let trusted = SecurityOptions {
        parameters: ParameterSources::None,
        ..taint()
    };
    for (i, src) in CALLBACKS.iter().enumerate().skip(3) {
        let run = |call: &str| run_with("ABLSEC003", &src.replace("{call}", call), trusted.clone());
        assert_eq!(run("o:ToString().").len(), 1, "program {i}");
        assert_eq!(run("").len(), 0, "program {i}");
    }
}
