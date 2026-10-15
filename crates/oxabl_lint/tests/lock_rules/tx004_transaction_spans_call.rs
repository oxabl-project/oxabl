//! `transaction-spans-call` (TX004).

use oxabl_lint::transaction_spans_call::run;

use super::{check, check_fragment, lines};

#[test]
fn calls_out_of_the_file_are_reported() {
    let src = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DO TRANSACTION:
    RUN other.p.
    RUN VALUE(\"x\" + \".p\").
    RUN ip IN h.
    RUN super-ip.
    RUN lib.p PERSISTENT SET h.
END.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3, 4, 5, 6, 7]);
    assert!(
        diags[0]
            .message
            .contains("`other.p`, which is not in this file")
    );
    assert!(diags[0].message.contains("`DO TRANSACTION`"));
    assert!(diags[1].message.contains("named at run time"));
    assert!(diags[2].message.contains("another running procedure"));
}

#[test]
fn internal_procedures_and_other_sessions_are_not_reported() {
    let src = "\
DEFINE VARIABLE h AS HANDLE NO-UNDO.
PROCEDURE ip:
END PROCEDURE.
PROCEDURE Sleep EXTERNAL \"kernel32.dll\":
    DEFINE INPUT PARAMETER n AS LONG.
END PROCEDURE.
DO TRANSACTION:
    RUN ip.
    RUN IP IN THIS-PROCEDURE.
    RUN Sleep (1).
    RUN other.p ON SERVER h.
    RUN other.p ASYNCHRONOUS.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_super_prototype_is_not_in_the_file() {
    let src = "\
PROCEDURE ip IN SUPER.
DO TRANSACTION:
    RUN ip.
END.
";
    assert_eq!(lines(src, &check(run, src)), [3]);
}

#[test]
fn outside_a_transaction_nothing_is_reported() {
    let src = "\
RUN other.p.
FOR EACH Customer NO-LOCK:
    RUN other.p.
END.
PROCEDURE p:
    RUN other.p.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_include_alone_leaves_bare_names_to_its_includer() {
    let src = "\
DO TRANSACTION:
    RUN fill-rows.
    RUN lib/other.p.
END.
";
    assert_eq!(lines(src, &check(run, src)), [2, 3]);
    assert_eq!(lines(src, &check_fragment(run, src)), [3]);
}

#[test]
fn on_session_runs_in_the_local_session() {
    let src = "\
DO TRANSACTION:
    RUN other.p ON SESSION.
    RUN other.p ON SERVER SESSION.
END.
";
    assert_eq!(lines(src, &check(run, src)), [2, 3]);
}
