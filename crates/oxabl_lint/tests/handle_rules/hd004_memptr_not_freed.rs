//! `memptr-not-freed` (HD004).

use oxabl_lint::memptr_not_freed::run;

use super::{check, check_fragment, help, lines};

#[test]
fn reports_a_memptr_never_set_back_to_zero() {
    let src = "\
DEFINE VARIABLE mFreed AS MEMPTR NO-UNDO.
DEFINE VARIABLE mLeak AS MEMPTR NO-UNDO.
SET-SIZE(mFreed) = 16.
SET-SIZE(mLeak) = 16.
SET-SIZE(mFreed) = 0.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [4]);
    assert!(diags[0].message.contains("SET-SIZE(mLeak) = 16"));
    assert!(help(&diags[0]).contains("SET-SIZE(mLeak) = 0"));
}

#[test]
fn a_size_of_zero_is_not_an_allocation() {
    let src = "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
SET-SIZE(m) = 0.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_free_in_finally_is_not_reported() {
    let src = "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DO ON ERROR UNDO, THROW:
    SET-SIZE(m) = 4096.
    COPY-LOB FROM FILE \"x\" TO m.
    FINALLY:
        SET-SIZE(m) = 0.
    END FINALLY.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_free_skipped_by_an_early_return_is_reported() {
    let src = "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE lStop AS LOGICAL NO-UNDO.
SET-SIZE(m) = 4096.
IF lStop THEN RETURN.
SET-SIZE(m) = 0.
";
    assert_eq!(lines(src, &check(run, src)), [3]);
}

#[test]
fn a_memptr_handed_to_the_caller_is_not_reported() {
    let src = "\
DEFINE OUTPUT PARAMETER pmData AS MEMPTR NO-UNDO.
SET-SIZE(pmData) = 128.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_free_through_a_copy_of_the_pointer_counts() {
    let src = "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
DEFINE VARIABLE m2 AS MEMPTR NO-UNDO.
SET-SIZE(m) = 16.
m2 = m.
SET-SIZE(m2) = 0.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn other_dynamic_objects_are_not_reported() {
    let src = "\
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn an_include_names_its_includer() {
    let src = "\
DEFINE VARIABLE m AS MEMPTR NO-UNDO.
SET-SIZE(m) = 16.
";
    let diags = check_fragment(run, src);
    assert_eq!(diags.len(), 1);
    assert!(help(&diags[0]).contains("This file is an include"));
}

#[test]
fn an_output_argument_leaves_the_memptr_unknown() {
    let src = "\
DEFINE VARIABLE mData AS MEMPTR NO-UNDO.
SET-SIZE(mData) = 100.
RUN fill.p (INPUT mData, OUTPUT mData).
";
    assert!(check(run, src).is_empty());
}

#[test]
fn copying_another_pointer_over_it_still_loses_the_block() {
    let src = "\
DEFINE VARIABLE mData AS MEMPTR NO-UNDO.
DEFINE VARIABLE mOther AS MEMPTR NO-UNDO.
SET-SIZE(mData) = 100.
mData = mOther.
";
    assert_eq!(check(run, src).len(), 1);
}
