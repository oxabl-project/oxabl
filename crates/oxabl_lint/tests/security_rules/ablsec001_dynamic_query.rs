//! `dynamic-query` (ABLSEC001).

use oxabl_lint::security::dynamic_query::run;

use super::{check, check_expanded, flagged};

#[test]
fn reports_query_prepare_with_a_non_literal() {
    let src = "hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Name = '\" + c + \"'\").";
    let diags = check(run, src);
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        ["QUERY-PREPARE(\"FOR EACH Customer WHERE Name = '\" + c + \"'\")"]
    );
}

#[test]
fn reports_a_bare_variable() {
    assert_eq!(check(run, "hQ:QUERY-PREPARE(cWhere).").len(), 1);
}

#[test]
fn quoter_does_not_make_a_concatenation_constant() {
    let src = "hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Name = \" + QUOTER(c)).";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn the_help_says_quoter_is_not_a_sanitiser_yet() {
    let diags = check(run, "hQ:QUERY-PREPARE(\"x\" + QUOTER(c)).");
    let help = diags[0].help.as_deref().unwrap();
    assert!(help.contains("QUOTER"), "{help}");
    assert!(help.contains("allow-list"), "{help}");
}

#[test]
fn reports_the_buffer_find_methods() {
    let src = "\
hBuf:FIND-FIRST(\"WHERE a = \" + c).
hBuf:FIND-LAST(c, NO-LOCK).
hBuf:FIND-UNIQUE(c) NO-ERROR.
";
    let diags = check(run, src);
    assert_eq!(diags.len(), 3, "{diags:?}");
    assert_eq!(
        flagged(src, &diags),
        [
            "FIND-FIRST(\"WHERE a = \" + c)",
            "FIND-LAST(c, NO-LOCK)",
            "FIND-UNIQUE(c)"
        ]
    );
}

#[test]
fn matches_method_names_in_any_case() {
    assert_eq!(
        check(run, "hQ:query-prepare(c). hB:Find-First(c).").len(),
        2
    );
}

#[test]
fn reports_fill_where_string_assignments() {
    let src = "\
hSrc:FILL-WHERE-STRING = \"Name = \" + c.
ASSIGN hSrc2:FILL-WHERE-STRING = c.
hBuf:DATA-SOURCE:FILL-WHERE-STRING = c.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reports_inside_expressions_and_blocks() {
    let src = "\
IF hBuf:FIND-FIRST(\"WHERE \" + c) THEN MESSAGE \"found\".
PROCEDURE p:
    DO ON ERROR UNDO, LEAVE:
        hQ:QUERY-PREPARE(c).
    END.
END PROCEDURE.
";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn reports_in_class_members() {
    let src = "\
CLASS Acme.Repo:
    METHOD PUBLIC VOID Load(INPUT pcWhere AS CHARACTER):
        hQ:QUERY-PREPARE(pcWhere).
    END METHOD.
END CLASS.
";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn literals_are_not_reported() {
    let src = "\
hQ:QUERY-PREPARE(\"FOR EACH Customer\").
hQ:QUERY-PREPARE(\"FOR EACH \" + \"Customer\").
hBuf:FIND-FIRST(\"WHERE a = 1\", NO-LOCK).
hSrc:FILL-WHERE-STRING = \"a = 1\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn static_queries_and_unrelated_methods_are_not_reported() {
    let src = "\
FOR EACH Customer WHERE Customer.Name = c: END.
FIND FIRST Customer WHERE Customer.Name = c NO-LOCK NO-ERROR.
OPEN QUERY q FOR EACH Customer WHERE Customer.Name = c.
hQ:QUERY-OPEN().
hSrc:BATCH-SIZE = i.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_method_with_no_argument_is_not_reported() {
    assert!(check(run, "hQ:QUERY-PREPARE().").is_empty());
}

#[test]
fn a_define_that_expands_to_a_literal_is_constant() {
    let main = "\
&GLOBAL-DEFINE BASE \"FOR EACH Customer\"
hQ:QUERY-PREPARE({&BASE} + \" WHERE Name = 'a'\").
";
    assert!(check_expanded(run, &[], main).is_empty());
}

#[test]
fn a_define_that_expands_to_a_variable_is_not() {
    let main = "\
&GLOBAL-DEFINE FILTER cFilter
hQ:QUERY-PREPARE({&FILTER}).
";
    assert_eq!(check_expanded(run, &[], main).len(), 1);
}

#[test]
fn a_sink_in_an_include_is_reported() {
    let files = [("prep.i", "hQ:QUERY-PREPARE(cWhere).\n")];
    assert_eq!(check_expanded(run, &files, "{prep.i}\n").len(), 1);
}

#[test]
fn an_include_argument_is_constant_when_the_caller_passes_a_literal() {
    let files = [("prep.i", "hQ:QUERY-PREPARE({1}).\n")];
    assert!(check_expanded(run, &files, "{prep.i \"'FOR EACH Customer'\"}\n").is_empty());
    assert_eq!(check_expanded(run, &files, "{prep.i cWhere}\n").len(), 1);
}
