//! `dynamic-code` (ABLSEC004).

use oxabl_lint::security::dynamic_code::run;
use oxabl_lint::{dynamic_invoke, run_value};

use super::{check, check_expanded, flagged};

#[test]
fn reports_run_value_of_a_non_literal() {
    let src = "\
RUN VALUE(pcProg) (INPUT 1).
RUN VALUE(\"post-\" + pcKind + \".p\").
RUN VALUE(c) IN h.
RUN VALUE(c) PERSISTENT SET h.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 4, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "RUN VALUE(pcProg",
            "RUN VALUE(\"post-\" + pcKind + \".p\"",
            "RUN VALUE(c",
            "RUN VALUE(c"
        ]
    );
}

#[test]
fn run_value_of_literals_is_not_reported() {
    let src = "\
RUN VALUE(\"a.p\").
RUN VALUE(\"a\" + \".p\").
RUN prog.p.
RUN inner IN h.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_dynamic_function_by_name_argument() {
    let src = "\
x = DYNAMIC-FUNCTION(pcName, 1).
y = DYNAMIC-FUNCTION(\"calc-\" + c).
z = DYNAMIC-FUNC(c).
";
    assert_eq!(
        flagged(src, &check(run, src)),
        [
            "DYNAMIC-FUNCTION(pcName, 1)",
            "DYNAMIC-FUNCTION(\"calc-\" + c)",
            "DYNAMIC-FUNC(c)"
        ]
    );
}

#[test]
fn dynamic_function_with_a_literal_name_is_not_reported() {
    let src = "\
x = DYNAMIC-FUNCTION(\"calc-tax\", c).
y = DYNAMIC-FUNCTION(\"calc-\" + \"tax\").
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_dynamic_invoke_by_method_name_argument() {
    let src = "\
DYNAMIC-INVOKE(h, pcMethod).
x = DYNAMIC-INVOKE(\"Acme.Foo\", c, 1).
";
    assert_eq!(
        flagged(src, &check(run, src)),
        [
            "DYNAMIC-INVOKE(h, pcMethod)",
            "DYNAMIC-INVOKE(\"Acme.Foo\", c, 1)"
        ]
    );
}

#[test]
fn dynamic_invoke_on_a_variable_handle_with_a_literal_method_is_not_reported() {
    let src = "x = DYNAMIC-INVOKE(hObj, \"Method\", c).";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_dynamic_new_of_a_non_literal_class() {
    let src = "\
oObj = DYNAMIC-NEW pcClass ().
oObj2 = DYNAMIC-NEW (\"Acme.\" + c) ().
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 2, "{diags:?}");
}

#[test]
fn dynamic_new_of_a_literal_class_is_not_reported() {
    let src = "oObj = DYNAMIC-NEW \"Acme.Foo\" (c).";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_get_class_of_a_non_literal() {
    let src = "\
oC = Progress.Lang.Class:GetClass(pcName).
oC2 = Class:GetClass(\"Acme.\" + c).
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn get_class_of_a_literal_is_not_reported() {
    let src = "oC = Progress.Lang.Class:GetClass(\"Acme.Foo\").";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_compile_value() {
    let src = "\
COMPILE VALUE(pcFile) SAVE.
COMPILE VALUE(\"/src/\" + c) SAVE INTO VALUE(\"/out\").
";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        ["VALUE(pcFile)", "VALUE(\"/src/\" + c)"]
    );
}

#[test]
fn compile_of_a_literal_is_not_reported() {
    let src = "\
COMPILE \"x.p\" SAVE.
COMPILE VALUE(\"x.p\") SAVE.
COMPILE VALUE(\"x.p\") SAVE INTO VALUE(c).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_a_call_object_name() {
    let src = "hCall:CALL-NAME = pcName.";
    assert_eq!(check(run, src).len(), 1);
    assert!(check(run, "hCall:CALL-NAME = \"calc-tax\".").is_empty());
}

#[test]
fn reports_dynamic_calls_inside_skipped_statements() {
    let src = "\
PUT UNFORMATTED DYNAMIC-FUNCTION(pcName).
PUT UNFORMATTED DYNAMIC-FUNCTION(\"calc-tax\").
PUT UNFORMATTED DYNAMIC-INVOKE(h, pcMethod).
";
    assert_eq!(
        flagged(src, &check(run, src)),
        ["DYNAMIC-FUNCTION(pcName)", "DYNAMIC-INVOKE(h, pcMethod)"]
    );
}

#[test]
fn it_reuses_the_detection_of_the_call_rules() {
    // Every non-literal call is also seen by `run-value` or `dynamic-invoke`.
    let src = "\
RUN VALUE(c).
x = DYNAMIC-FUNCTION(c).
DYNAMIC-INVOKE(h, c).
";
    let ours = check(run, src).len();
    let theirs = check(run_value::run, src).len() + check(dynamic_invoke::run, src).len();
    assert_eq!(ours, 3);
    assert_eq!(theirs, 3);
}

#[test]
fn it_fires_only_where_the_call_rules_would_and_the_target_is_not_literal() {
    // `run-value` reports a `VALUE("a" + ".p")` for analysability; this rule
    // does not, because nothing an attacker controls reaches it.
    let src = "RUN VALUE(\"a\" + \".p\").";
    assert_eq!(check(run_value::run, src).len(), 1);
    assert!(check(run, src).is_empty());
}

#[test]
fn the_help_explains_how_it_differs_from_the_call_rules() {
    let diags = check(run, "RUN VALUE(c).");
    let help = diags[0].help.as_deref().unwrap();
    assert!(help.contains("run-value"), "{help}");
    assert!(help.contains("dynamic-invoke"), "{help}");
    assert!(help.contains("analys"), "{help}");

    let diags = check(run, "x = DYNAMIC-FUNCTION(c).");
    assert!(diags[0].help.as_deref().unwrap().contains("dynamic-invoke"));
}

#[test]
fn a_define_that_expands_to_a_variable_is_reported() {
    let main = "\
&GLOBAL-DEFINE PROG cProg
RUN VALUE({&PROG}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_define_that_expands_to_a_literal_is_not() {
    let main = "\
&GLOBAL-DEFINE PROG \"post-invoice.p\"
RUN VALUE({&PROG}).
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("go.i", "RUN VALUE(cProg).\n")];
    assert_eq!(check_expanded(run, &files, "{go.i}\n").len(), 1);
}

#[test]
fn a_dynamic_function_with_an_in_clause_still_reads_the_name() {
    let src = "x = DYNAMIC-FUNCTION(pcName IN hProc, 1).";
    assert_eq!(
        flagged(src, &check(run, src)),
        ["DYNAMIC-FUNCTION(pcName IN hProc, 1)"]
    );
    assert!(check(run, "x = DYNAMIC-FUNCTION(\"f\" IN hProc, c).").is_empty());
}

#[test]
fn reports_dynamic_new_inside_a_skipped_statement() {
    let src = "PUT UNFORMATTED DYNAMIC-NEW pcClass ().";
    assert_eq!(check(run, src).len(), 1);
    assert!(check(run, "PUT UNFORMATTED DYNAMIC-NEW \"Acme.Foo\" ().").is_empty());
}
