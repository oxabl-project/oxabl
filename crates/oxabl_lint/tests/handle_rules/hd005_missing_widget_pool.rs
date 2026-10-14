//! `missing-widget-pool` (HD005).

use oxabl_lint::missing_widget_pool::run;

use super::{check, check_fragment, help, lines};

#[test]
fn reports_once_at_the_first_unpooled_create() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
CREATE BUFFER hBuffer FOR TABLE \"customer\".
DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [3]);
    assert!(
        diags[0].message.starts_with("2 dynamic objects are"),
        "{}",
        diags[0].message
    );
}

#[test]
fn the_message_counts_one_object() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
DELETE OBJECT hQuery.
";
    let diags = check(run, src);
    assert!(
        diags[0].message.starts_with("1 dynamic object is"),
        "{}",
        diags[0].message
    );
}

#[test]
fn counts_the_objects_of_every_unit_of_the_file() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
PROCEDURE p:
    DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    CREATE BUFFER hBuffer FOR TABLE \"customer\".
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [2]);
    assert!(
        diags[0].message.starts_with("2 dynamic objects are"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_widget_pool_of_the_main_block_covers_the_file() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE WIDGET-POOL.
CREATE QUERY hQuery.
PROCEDURE p:
    DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    CREATE BUFFER hBuffer FOR TABLE \"customer\".
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_named_pool_of_the_file_covers_the_object() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE WIDGET-POOL \"work\".
CREATE QUERY hQuery IN WIDGET-POOL \"work\".
DELETE WIDGET-POOL \"work\".
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_pool_created_in_a_procedure_covers_only_that_procedure() {
    let src = "\
PROCEDURE p:
    DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
    CREATE WIDGET-POOL.
    CREATE QUERY hQuery.
END PROCEDURE.
PROCEDURE q:
    DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
    CREATE BUFFER hBuffer FOR TABLE \"customer\".
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [8]);
    assert!(
        diags[0].message.starts_with("1 dynamic object is"),
        "{}",
        diags[0].message
    );
}

#[test]
fn persistent_procedures_memptrs_and_class_instances_are_not_counted() {
    let src = "\
DEFINE VARIABLE hLib AS HANDLE NO-UNDO.
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE o AS Acme.Thing NO-UNDO.
RUN lib.p PERSISTENT SET hLib.
SET-SIZE(m) = 16.
o = NEW Acme.Thing().
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_object_handed_to_the_caller_is_not_counted() {
    let src = "\
DEFINE OUTPUT PARAMETER phQuery AS HANDLE NO-UNDO.
CREATE QUERY phQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_include_says_the_includer_may_provide_the_pool() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
DELETE OBJECT hQuery.
";
    let diags = check_fragment(run, src);
    assert_eq!(diags.len(), 1);
    assert!(help(&diags[0]).contains("This file is an include"));
}

#[test]
fn a_root_reports_at_its_own_first_create_when_an_include_creates_first() {
    let included = "\
DEFINE VARIABLE hQ1 AS HANDLE NO-UNDO.
CREATE QUERY hQ1.
";
    let own = "\
DEFINE VARIABLE hQ2 AS HANDLE NO-UNDO.
CREATE QUERY hQ2.
";
    let src = format!("{included}{own}");
    let diags = super::check_with_include(run, &src, included.len() as u32);
    assert_eq!(lines(&src, &diags), [4]);
    assert!(
        diags[0].message.starts_with("1 dynamic object is"),
        "{}",
        diags[0].message
    );
    assert!(!help(&diags[0]).contains("This file is an include"));
}

#[test]
fn a_root_with_no_creates_of_its_own_reports_nothing() {
    let included = "\
DEFINE VARIABLE hQ1 AS HANDLE NO-UNDO.
CREATE QUERY hQ1.
";
    let src = format!("{included}MESSAGE \"x\".\n");
    assert!(super::check_with_include(run, &src, included.len() as u32).is_empty());
}
