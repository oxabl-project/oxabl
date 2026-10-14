//! `missing-no-undo` (TX006).

use oxabl_lint::missing_no_undo::run;

use super::{check, flagged};

#[test]
fn reports_a_variable_without_no_undo() {
    let src = "DEFINE VARIABLE iCount AS INTEGER.";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["iCount"]);
    assert_eq!(
        diags[0].message,
        "variable `iCount` is defined without NO-UNDO"
    );
}

#[test]
fn reports_variable_spellings_and_types() {
    let src = "\
DEF VAR a AS CHARACTER.
DEFINE VARIABLE b LIKE Customer.Name.
DEFINE VARIABLE c AS HANDLE.
DEFINE VARIABLE d AS INTEGER EXTENT 3.
DEFINE VARIABLE e AS CHARACTER INITIAL \"x\" FORMAT \"x(5)\".
DEFINE PRIVATE VARIABLE f AS DATE.
DEFINE SHARED VARIABLE g AS INTEGER.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["a", "b", "c", "d", "e", "f", "g"]);
}

#[test]
fn accepts_no_undo_in_every_position() {
    let src = "\
DEFINE VARIABLE a AS INTEGER NO-UNDO.
DEFINE VARIABLE b AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE c AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE d NO-UNDO LIKE Customer.Name.
DEFINE VARIABLE e AS CHARACTER FORMAT \"x(8)\" VIEW-AS FILL-IN SIZE 10 BY 1 NO-UNDO.
DEFINE VARIABLE f AS CHARACTER VIEW-AS EDITOR SIZE 20 BY 4 NO-UNDO.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_ui_variable_without_no_undo_is_reported() {
    let src = "DEFINE VARIABLE f AS CHARACTER VIEW-AS FILL-IN SIZE 10 BY 1.";
    assert_eq!(check(run, src).len(), 1);
}

#[test]
fn the_var_statement_is_no_undo_already() {
    let src = "VAR INTEGER a.\nVAR CHARACTER b = \"x\".\nVAR Acme.Cls c.";
    assert!(check(run, src).is_empty());
}

#[test]
fn reports_a_temp_table_without_no_undo() {
    let src = "\
DEFINE TEMP-TABLE ttA FIELD a AS INTEGER.
DEFINE TEMP-TABLE ttB LIKE Customer.
DEFINE NEW SHARED TEMP-TABLE ttC FIELD a AS INTEGER.
DEFINE TEMP-TABLE ttD NO-UNDO FIELD a AS INTEGER.
DEFINE TEMP-TABLE ttE NO-UNDO LIKE Customer.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["ttA", "ttB", "ttC"]);
}

#[test]
fn reports_define_parameter_statements() {
    let src = "\
PROCEDURE p:
    DEFINE INPUT PARAMETER a AS INTEGER.
    DEFINE OUTPUT PARAMETER b AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER c LIKE Customer.Name.
    DEFINE INPUT PARAMETER d AS INTEGER NO-UNDO.
    DEF INPUT PARAM e NO-UNDO LIKE Customer.Name.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["a", "b", "c"]);
}

#[test]
fn parameters_that_take_no_no_undo_are_not_reported() {
    let src = "\
DEFINE INPUT PARAMETER TABLE FOR tt.
DEFINE OUTPUT PARAMETER TABLE-HANDLE hTable.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR ds.
DEFINE INPUT PARAMETER DATASET-HANDLE hDs.
DEFINE PARAMETER BUFFER bCust FOR Customer.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn signature_parameters_have_nowhere_to_write_no_undo() {
    let src = "\
CLASS Acme.Svc:
    CONSTRUCTOR PUBLIC Svc(INPUT a AS INTEGER):
    END CONSTRUCTOR.
    METHOD PUBLIC VOID Go(INPUT b AS INTEGER, OUTPUT c AS CHARACTER):
    END METHOD.
    DEFINE PUBLIC PROPERTY P AS CHARACTER NO-UNDO
        GET.
        SET(INPUT d AS CHARACTER):
        END SET.
    DEFINE PUBLIC EVENT Changed SIGNATURE VOID(INPUT e AS INTEGER).
END CLASS.
FUNCTION f RETURNS INTEGER (INPUT g AS INTEGER, OUTPUT h AS CHARACTER):
    RETURN 1.
END FUNCTION.
";
    assert!(check(run, src).is_empty(), "{:?}", check(run, src));
}

#[test]
fn reports_properties_that_have_storage() {
    let src = "\
CLASS Acme.Svc:
    DEFINE PUBLIC PROPERTY A AS CHARACTER
        GET. SET.
    DEFINE PUBLIC STATIC PROPERTY B AS INTEGER
        GET.
        PRIVATE SET.
    DEFINE PUBLIC PROPERTY C AS CHARACTER NO-UNDO
        GET. SET.
END CLASS.
";
    let diags = check(run, src);
    assert_eq!(flagged(src, &diags), ["A", "B"]);
}

#[test]
fn properties_that_are_not_plain_auto_properties_are_not_reported() {
    let src = "\
CLASS Acme.Svc:
    DEFINE PUBLIC PROPERTY A AS CHARACTER
        GET:
            RETURN \"x\".
        END GET.
        SET(INPUT v AS CHARACTER):
            MESSAGE v.
        END SET.
    DEFINE PUBLIC ABSTRACT PROPERTY B AS CHARACTER
        GET. SET.
    DEFINE PUBLIC PROPERTY D AS CHARACTER
        GET.
        SET(INPUT v AS CHARACTER):
            MESSAGE v.
        END SET.
END CLASS.
INTERFACE Acme.Api:
    DEFINE PUBLIC PROPERTY C AS CHARACTER
        GET. SET.
END INTERFACE.
";
    assert!(check(run, src).is_empty(), "{:?}", check(run, src));
}

#[test]
fn class_typed_definitions_are_not_judged() {
    let src = "\
DEFINE VARIABLE o AS Progress.Lang.Object.
DEFINE VARIABLE p AS CLASS Acme.Cls.
DEFINE INPUT PARAMETER q AS Acme.Cls.
CLASS Acme.Svc:
    DEFINE PUBLIC PROPERTY R AS Acme.Cls
        GET. SET.
END CLASS.
";
    assert!(check(run, src).is_empty(), "{:?}", check(run, src));
}

#[test]
fn other_definitions_are_not_reported() {
    let src = "\
DEFINE BUFFER bCust FOR Customer.
DEFINE STREAM sOut.
DEFINE QUERY q FOR Customer.
DEFINE DATASET ds FOR tt.
DEFINE WORK-TABLE wt FIELD a AS INTEGER.
";
    assert!(check(run, src).is_empty());
}

const PLAIN_HELP: &str = "Add `NO-UNDO` to the definition.";
const SHARED_HELP: &str = "Add `NO-UNDO` to the `NEW SHARED` producer and to every `SHARED` consumer; \
                           changing one side alone makes the definitions disagree.";

// The AVM is assumed to require a producer and its consumers to agree on
// `NO-UNDO`, so the help for a shared definition names both sides.
#[test]
fn a_shared_definition_asks_for_the_producer_and_every_consumer() {
    let src = "\
DEFINE NEW SHARED VARIABLE a AS INTEGER.
DEFINE SHARED VARIABLE b AS INTEGER.
DEFINE NEW GLOBAL SHARED VARIABLE c AS INTEGER.
DEFINE NEW SHARED TEMP-TABLE ttA FIELD x AS INTEGER.
DEFINE SHARED TEMP-TABLE ttB FIELD x AS INTEGER.
DEFINE NEW GLOBAL SHARED TEMP-TABLE ttC FIELD x AS INTEGER.
DEFINE VARIABLE plain AS INTEGER.
DEFINE TEMP-TABLE ttPlain FIELD x AS INTEGER.
";
    let diags = check(run, src);
    let helps: Vec<_> = diags.iter().map(|d| d.help.as_deref().unwrap()).collect();
    assert_eq!(
        flagged(src, &diags),
        ["a", "b", "c", "ttA", "ttB", "ttC", "plain", "ttPlain"]
    );
    assert_eq!(
        helps,
        [
            SHARED_HELP,
            SHARED_HELP,
            SHARED_HELP,
            SHARED_HELP,
            SHARED_HELP,
            SHARED_HELP,
            PLAIN_HELP,
            PLAIN_HELP,
        ]
    );
}
