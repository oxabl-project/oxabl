//! `scope-widened` (TX005).

use oxabl_lint::scope_widened::run;

use super::{check, check_no_schema, help, lines};

#[test]
fn a_find_after_a_for_each_raises_its_scope() {
    let src = "\
FOR EACH Customer NO-LOCK:
    DISPLAY Customer.Name.
END.
FIND FIRST Customer NO-LOCK.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [4]);
    assert!(
        diags[0]
            .message
            .contains("scopes `Customer` to the main block")
    );
    assert!(diags[0].message.contains("`FOR EACH Customer NO-LOCK`"));
    assert!(help(&diags[0]).contains("DEFINE BUFFER bCustomer FOR Customer"));
    assert!(help(&diags[0]).contains("LK002"));
}

#[test]
fn one_finding_per_raised_scope() {
    let src = "\
FIND FIRST Customer NO-LOCK.
FOR EACH Customer NO-LOCK:
END.
FOR FIRST Customer NO-LOCK:
END.
DISPLAY Customer.Name.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1]);
    assert!(diags[0].message.contains("and of one other block"));
}

#[test]
fn a_main_block_reference_raises_a_for_block_of_a_procedure() {
    let src = "\
FIND FIRST Customer NO-LOCK.
PROCEDURE p:
    FOR EACH Customer NO-LOCK:
    END.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [1]);
    assert!(diags[0].message.contains("in procedure `p`"));
}

#[test]
fn a_reference_in_a_sibling_procedure_is_not_reported() {
    let src = "\
PROCEDURE a:
    FIND FIRST Customer NO-LOCK.
END PROCEDURE.
PROCEDURE b:
    FOR EACH Customer NO-LOCK:
    END.
END PROCEDURE.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn a_reference_in_the_same_procedure_is_reported() {
    let src = "\
PROCEDURE a:
    FOR EACH Customer NO-LOCK:
    END.
    FIND FIRST Customer NO-LOCK.
END PROCEDURE.
";
    let diags = check(run, src);
    assert_eq!(lines(src, &diags), [4]);
    assert!(diags[0].message.contains("used in procedure `a`"));
}

#[test]
fn scopes_that_are_not_raised_are_not_reported() {
    let src = "\
DEFINE BUFFER bCust FOR Customer.
FOR EACH Customer NO-LOCK:
END.
FIND FIRST bCust NO-LOCK.
DO FOR Item:
    FIND FIRST Item NO-LOCK.
END.
REPEAT:
    FIND NEXT Order NO-LOCK.
    FOR EACH Order NO-LOCK:
    END.
    LEAVE.
END.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn header_and_skipped_references_are_not_reported() {
    let src = "\
FOR EACH Customer NO-LOCK:
END.
FOR EACH Order OF Customer NO-LOCK:
END.
FOR EACH Item NO-LOCK:
END.
DISABLE TRIGGERS FOR LOAD OF Item.
";
    assert!(check(run, src).is_empty());
}

#[test]
fn temp_tables_and_no_schema_are_silent() {
    let src = "\
DEFINE TEMP-TABLE tt NO-UNDO FIELD a AS INTEGER.
FOR EACH tt:
END.
FIND FIRST tt.
FOR EACH Customer NO-LOCK:
END.
FIND FIRST Customer NO-LOCK.
";
    assert_eq!(lines(src, &check(run, src)), [7]);
    assert!(check_no_schema(run, src).is_empty());
}
