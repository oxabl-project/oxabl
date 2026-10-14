//! A file the parser recovered from is not judged: recovery can move a
//! statement out of its unit, so a free the author wrote looks missing.

use oxabl_lint::{
    delete_not_in_finally, handle_not_deleted, memptr_not_freed, missing_widget_pool,
    persistent_procedure_leak,
};

use super::{RuleFn, check_recovered};

// Recovery closes the procedure at the `END.` of the `DO`, so the
// `DELETE OBJECT` lands outside it.
const MOVED_FREE: &str = "\
DEFINE VARIABLE lx AS LOGICAL NO-UNDO.
PROCEDURE b:
  DEFINE VARIABLE hq AS HANDLE NO-UNDO.
  CREATE QUERY hq.
  main:
  DO ON ERROR UNDO main, LEAVE main
     ON STOP UNDO main, LEAVE main
     TRANSACTION
  :
    lx = TRUE.
  END.
  DELETE OBJECT hq.
END PROCEDURE.
";

const BROKEN_DO: &str = "\
  main:
  DO ON ERROR UNDO main, LEAVE main
     TRANSACTION
  :
    lx = TRUE.
  END.
";

fn assert_gated(rule: RuleFn, source: &str) {
    assert!(
        !check_recovered(rule, source, false, false).is_empty(),
        "the fixture must hold a finding when the parse is trusted\n{source}"
    );
    assert!(
        check_recovered(rule, source, true, false).is_empty(),
        "no finding on a recovered parse\n{source}"
    );
}

#[test]
fn hd001_ignores_a_free_that_recovery_moved_out_of_its_unit() {
    assert_gated(handle_not_deleted::run, MOVED_FREE);
}

#[test]
fn hd002_is_not_reported_on_a_recovered_parse() {
    let src = format!(
        "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE QUERY hQuery.
    hQuery:QUERY-PREPARE(\"for each customer\").
    DELETE OBJECT hQuery.
END PROCEDURE.
{BROKEN_DO}"
    );
    assert_gated(delete_not_in_finally::run, &src);
}

#[test]
fn hd003_is_not_reported_on_a_recovered_parse() {
    let src = format!(
        "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
{BROKEN_DO}"
    );
    assert_gated(persistent_procedure_leak::run, &src);
}

#[test]
fn hd004_is_not_reported_on_a_recovered_parse() {
    let src = format!(
        "\
DEFINE VARIABLE mLeak AS MEMPTR NO-UNDO.
SET-SIZE(mLeak) = 16.
{BROKEN_DO}"
    );
    assert_gated(memptr_not_freed::run, &src);
}

#[test]
fn hd005_is_not_reported_on_a_recovered_parse() {
    let src = format!(
        "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
DELETE OBJECT hQuery.
{BROKEN_DO}"
    );
    assert_gated(missing_widget_pool::run, &src);
}

#[test]
fn an_include_fragment_is_still_judged() {
    let found = check_recovered(handle_not_deleted::run, MOVED_FREE, true, true);
    assert_eq!(found.len(), 1, "{found:?}");
}
