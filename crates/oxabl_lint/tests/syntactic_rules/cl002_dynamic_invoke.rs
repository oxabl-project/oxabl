//! `dynamic-invoke` (CL002).

use oxabl_lint::dynamic_invoke::run;

use super::{check, flagged};

#[test]
fn reports_each_dynamic_call_form() {
    let src = "\
x = DYNAMIC-FUNCTION(\"f\", 1).
x = DYNAMIC-FUNCTION(\"f\" IN h, 1).
x = DYNAMIC-FUNC(\"f\").
x = DYNAMIC-INVOKE(h, \"m\", 1).
x = DYNAMIC-INVOKE(\"Acme.Cls\", \"m\").
o = DYNAMIC-NEW cls (1).
o = DYNAMIC-NEW \"Acme.Cls\" ().
x = DYNAMIC-PROPERTY(o, \"p\").
";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        [
            "DYNAMIC-FUNCTION",
            "DYNAMIC-FUNCTION",
            "DYNAMIC-FUNC",
            "DYNAMIC-INVOKE",
            "DYNAMIC-INVOKE",
            "DYNAMIC-NEW",
            "DYNAMIC-NEW",
            "DYNAMIC-PROPERTY",
        ]
    );
}

#[test]
fn reports_a_call_used_as_a_statement() {
    let src = "DYNAMIC-INVOKE(h, \"m\").\nDYNAMIC-FUNCTION(\"f\").";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn reports_nested_calls_once_each() {
    let src = "x = DYNAMIC-FUNCTION(\"f\", DYNAMIC-FUNCTION(\"g\")).";
    assert_eq!(check(run, src).len(), 2);
}

#[test]
fn reports_create_call() {
    let src = "CREATE CALL hCall.\nCREATE CALL hCall IN WIDGET-POOL \"p\" NO-ERROR.";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        ["CREATE CALL hCall", "CREATE CALL hCall"]
    );
}

#[test]
fn reports_in_declarations_and_nested_statements() {
    let src = "\
DEFINE VARIABLE x AS CHARACTER NO-UNDO INITIAL DYNAMIC-FUNCTION(\"f\").
IF DYNAMIC-FUNCTION(\"g\") THEN MESSAGE \"yes\".
FOR EACH Customer WHERE Customer.Name = DYNAMIC-FUNCTION(\"h\"):
END.
";
    assert_eq!(check(run, src).len(), 3);
}

#[test]
fn reports_inside_statements_the_parser_does_not_model() {
    let src = "\
PUT UNFORMATTED DYNAMIC-FUNCTION(\"f\") SKIP.
EXPORT DYNAMIC-INVOKE(h, \"m\").
OPEN QUERY q FOR EACH Customer WHERE Customer.Name = DYNAMIC-FUNCTION(\"g\").
";
    let diags = check(run, src);
    assert_eq!(
        flagged(src, &diags),
        ["DYNAMIC-FUNCTION", "DYNAMIC-INVOKE", "DYNAMIC-FUNCTION"]
    );
}

#[test]
fn ignores_the_words_in_strings_and_comments() {
    let src = "\
/* DYNAMIC-FUNCTION(\"f\") */
// DYNAMIC-INVOKE(h, \"m\")
MESSAGE \"DYNAMIC-FUNCTION(x)\".
PUT UNFORMATTED \"DYNAMIC-NEW\" SKIP.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn static_calls_and_other_dynamic_keywords_are_not_reported() {
    let src = "\
x = calc-tax(1).
x = obj:Invoke().
RUN prog.p.
h = DYNAMIC-CAST(o, \"Acme.Cls\").
n = DYNAMIC-CURRENT-VALUE(\"seq\", \"db\").
n = DYNAMIC-NEXT-VALUE(\"seq\", \"db\").
CREATE QUERY hQuery.
CREATE TEMP-TABLE hTable.
";
    assert!(check(run, src).is_empty(), "{:?}", check(run, src));
}

#[test]
fn a_table_named_call_is_not_a_call_object() {
    let src = "CREATE call.";
    assert!(check(run, src).is_empty());
}
