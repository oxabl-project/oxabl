//! Small cases of the failed-`FIND` model, one claim each: the lines where
//! a field is reported, against `fixtures/schema.df`.

use oxabl_nulls::NullOptions;
use oxabl_scope::ScopeOptions;

const HEAD: &str = "DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n\
                    DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
                    DEFINE TEMP-TABLE tt NO-UNDO FIELD k AS CHARACTER.\n\
                    DEFINE QUERY q FOR Customer.\n\
                    DEFINE FRAME f Customer.CustNum Customer.Name.\n";

/// The 1-based lines of `body` (after the shared definitions) with a
/// finding.
fn missing(body: &str) -> Vec<usize> {
    let source = format!("{HEAD}{body}");
    let df = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/schema.df"),
    )
    .expect("schema.df");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let tokens = oxabl_lexer::tokenize(&source);
    let program = oxabl_parser::Parser::new(&tokens, &source).parse_program();
    assert!(
        program.errors.is_empty(),
        "case does not parse cleanly: {:?}",
        program.errors
    );
    let options = ScopeOptions::default();
    let flow = oxabl_flow::build_file_tokens(&program.statements, &source, &tokens, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        &source,
        &tokens,
        &flow,
        Some(&schema),
        &options,
    );
    let nulls =
        oxabl_nulls::analyze_file(&source, &tokens, &flow, &scopes, &NullOptions::default());
    let head = HEAD.lines().count();
    nulls
        .missing()
        .iter()
        .map(|m| source[..m.at.start as usize].lines().count() - head)
        .collect()
}

#[test]
fn widget_references_are_no_record_reads() {
    let forms = [
        "DISPLAY \"\" @ Customer.Name WITH FRAME f.",
        "Customer.Name:SCREEN-VALUE IN FRAME f = \"\".",
        "c = Customer.Name:SCREEN-VALUE IN FRAME f.",
        "Customer.Name:SENSITIVE IN FRAME f = FALSE.",
        "MESSAGE Customer.Name:LABEL.",
        "IF Customer.Name:MODIFIED IN FRAME f THEN MESSAGE \"m\".",
        "ENABLE Customer.Name WITH FRAME f.",
        "DISABLE Customer.Name WITH FRAME f.",
        "PROMPT-FOR Customer.Name WITH FRAME f.",
        "APPLY \"entry\" TO Customer.Name IN FRAME f.",
        "HIDE Customer.Name IN FRAME f.",
        "VIEW Customer.Name IN FRAME f.",
    ];
    for form in forms {
        let body = format!("FIND FIRST Customer NO-LOCK NO-ERROR.\n{form}\n");
        assert_eq!(missing(&body), Vec::<usize>::new(), "{form}");
        // Neither do they prove the record.
        let body = format!("FIND FIRST Customer NO-LOCK NO-ERROR.\n{form}\nc = Customer.Name.\n");
        assert_eq!(missing(&body), vec![3], "{form}");
    }
    // A temp-table field may hold a handle: its method reads the record.
    let body = "DEFINE TEMP-TABLE th NO-UNDO FIELD h AS HANDLE.\n\
                FIND FIRST th NO-ERROR.\n\
                c = th.h:NAME.\n\
                c = th.h:SCREEN-VALUE.\n";
    assert_eq!(missing(body), vec![3]);
    // `INPUT` as the mode of a parameter reads the record.
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                RUN sub (INPUT Customer.Name, INPUT Customer.CustNum).\n\
                PROCEDURE sub:\n\
                  DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\n\
                  DEFINE INPUT PARAMETER pn AS INTEGER NO-UNDO.\n\
                END.\n";
    assert_eq!(missing(body), vec![2]);
}

#[test]
fn a_dependent_find_proves_only_through_its_own_site() {
    // Another find of Order that reads Customer, elsewhere in the unit.
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                FIND FIRST Order WHERE Order.OrderNum = 5 NO-LOCK NO-ERROR.\n\
                IF AVAILABLE Order THEN c = Customer.Name.\n\
                FIND FIRST Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.\n";
    assert_eq!(missing(body), vec![3]);
    // A FOR EACH refills Order: its use proves nothing about Customer.
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                FOR EACH Order NO-LOCK:\n\
                  c = STRING(Order.OrderNum).\n\
                  c = Customer.Name.\n\
                END.\n\
                FIND FIRST Order WHERE Order.CustNum = Customer.CustNum NO-LOCK NO-ERROR.\n";
    assert_eq!(missing(body), vec![4]);
}

#[test]
fn a_dependent_find_proves_its_criteria_when_its_buffer_was_empty() {
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                IF NOT AVAILABLE Customer THEN RETURN.\n\
                n = Order.OrderNum.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    // Where the FIND did not run, Customer is still empty.
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                IF AVAILABLE Order THEN\n\
                  FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                IF AVAILABLE Customer THEN n = Order.OrderNum.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    // A second pass keeps a record that came from the same Order.
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                DO n = 1 TO 2:\n\
                  FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                  IF NOT AVAILABLE Customer THEN LEAVE.\n\
                  c = STRING(Order.OrderNum).\n\
                END.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}

#[test]
fn a_dependent_find_proves_nothing_when_its_buffer_may_hold_a_record() {
    // The FIND may be abandoned when Order is empty (N7) and keep the
    // record Customer held before.
    let body = "FIND FIRST Customer NO-LOCK.\n\
                FIND FIRST Order NO-LOCK NO-ERROR.\n\
                FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                IF NOT AVAILABLE Customer THEN RETURN.\n\
                n = Order.OrderNum.\n";
    assert_eq!(missing(body), vec![5]);
    // The same in a loop: the second pass may keep the first pass's record.
    let body = "REPEAT:\n\
                  FIND NEXT Order NO-LOCK NO-ERROR.\n\
                  FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                  IF NOT AVAILABLE Customer THEN LEAVE.\n\
                  n = Order.OrderNum.\n\
                END.\n";
    assert_eq!(missing(body), vec![5]);
    // A routine may run with the record a buffer of the main block holds.
    let body = "RUN sub.\n\
                FIND FIRST Customer NO-LOCK.\n\
                PROCEDURE sub:\n\
                  FIND FIRST Order NO-LOCK NO-ERROR.\n\
                  FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                  IF NOT AVAILABLE Customer THEN RETURN.\n\
                  n = Order.OrderNum.\n\
                END.\n";
    assert_eq!(missing(body), vec![7]);
    // The main block runs first, whatever routines name the buffer.
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                IF NOT AVAILABLE Customer THEN RETURN.\n\
                n = Order.OrderNum.\n\
                PROCEDURE sub:\n\
                  FIND FIRST Customer NO-LOCK.\n\
                END.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}

#[test]
fn a_dependent_find_proves_nothing_about_a_later_find_of_its_criteria() {
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                FIND Customer WHERE Customer.CustNum = Order.CustNum NO-LOCK NO-ERROR.\n\
                FIND LAST Order NO-LOCK NO-ERROR.\n\
                IF NOT AVAILABLE Customer THEN RETURN.\n\
                n = Order.OrderNum.\n";
    assert_eq!(missing(body), vec![5]);
}

#[test]
fn a_temp_table_in_a_statement_the_model_does_not_know_is_unknown() {
    let body = "FIND FIRST Item NO-LOCK.\n\
                FIND FIRST tt NO-ERROR.\n\
                RAW-TRANSFER Item TO tt.\n\
                c = tt.k.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    let body = "FIND FIRST tt NO-ERROR.\n\
                tt-load tt.\n\
                c = tt.k.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}

#[test]
fn error_status_compared_with_a_logical_is_read() {
    for test in [
        "IF ERROR-STATUS:ERROR = TRUE THEN RETURN.",
        "IF ERROR-STATUS:ERROR EQ YES THEN RETURN.",
        "IF ERROR-STATUS:ERROR <> FALSE THEN RETURN.",
        "IF NOT ERROR-STATUS:ERROR NE NO THEN . ELSE RETURN.",
    ] {
        let body = format!("FIND FIRST Customer NO-LOCK NO-ERROR.\n{test}\nc = Customer.Name.\n");
        assert_eq!(missing(&body), Vec::<usize>::new(), "{test}");
    }
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                IF ERROR-STATUS:ERROR = NO THEN c = Customer.Name.\n\
                ELSE c = Customer.Name.\n";
    assert_eq!(missing(body), vec![3]);
}

#[test]
fn error_status_in_a_form_the_model_does_not_read_is_unknown() {
    for test in [
        "IF ERROR-STATUS:NUM-MESSAGES > 0 THEN RETURN.",
        "IF ERROR-STATUS:ERROR = lFlag THEN RETURN.",
        "CASE ERROR-STATUS:ERROR: WHEN TRUE THEN RETURN. END CASE.",
        "CASE AVAILABLE Customer: WHEN FALSE THEN RETURN. END CASE.",
    ] {
        let body = format!(
            "DEFINE VARIABLE lFlag AS LOGICAL NO-UNDO.\n\
             FIND FIRST Customer NO-LOCK NO-ERROR.\n{test}\nc = Customer.Name.\n"
        );
        assert_eq!(missing(&body), Vec::<usize>::new(), "{test}");
    }
}

#[test]
fn locked_proves_nothing() {
    let body = "FIND FIRST Item EXCLUSIVE-LOCK NO-WAIT NO-ERROR.\n\
                IF LOCKED Item THEN RETURN.\n\
                c = STRING(Item.Price).\n";
    assert_eq!(missing(body), vec![3]);
    let body = "FIND FIRST Item EXCLUSIVE-LOCK NO-WAIT NO-ERROR.\n\
                IF NOT LOCKED(Item) THEN c = STRING(Item.Price).\n";
    assert_eq!(missing(body), vec![2]);
}

#[test]
fn a_call_before_an_error_status_test_makes_it_unknown() {
    // The procedure may run a statement with NO-ERROR, or only read
    // ERROR-STATUS: the test says nothing known about the FIND.
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                RUN sub.\n\
                IF ERROR-STATUS:ERROR THEN MESSAGE \"none\".\n\
                c = Customer.Name.\n\
                PROCEDURE sub:\n\
                END.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    // With no test after it, the call changes nothing.
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                RUN sub.\n\
                c = Customer.Name.\n\
                PROCEDURE sub:\n\
                END.\n";
    assert_eq!(missing(body), vec![3]);
}

#[test]
fn a_record_loop_with_while_ends_also_when_its_records_run_out() {
    let body = "FIND FIRST Order NO-LOCK NO-ERROR.\n\
                FOR EACH Item NO-LOCK WHILE NOT AVAILABLE Order:\n\
                END.\n\
                n = Order.OrderNum.\n";
    assert_eq!(missing(body), vec![4]);
}

#[test]
fn release_and_reposition_forget() {
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                RELEASE Customer.\n\
                c = Customer.Name.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
    let body = "FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                REPOSITION q TO ROW 1.\n\
                c = Customer.Name.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}

#[test]
fn a_block_colon_is_no_method_call() {
    let body = "DEFINE VARIABLE find-count AS INTEGER NO-UNDO.\n\
                FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                DO:\n\
                  find-count = 1.\n\
                END.\n\
                c = Customer.Name.\n";
    assert_eq!(missing(body), vec![6]);
    let body = "DEFINE VARIABLE find-count AS INTEGER NO-UNDO.\n\
                FIND FIRST Customer NO-LOCK NO-ERROR.\n\
                DO TRANSACTION: find-count = 1. END.\n\
                c = Customer.Name.\n";
    assert_eq!(missing(body), vec![4]);
    // A temp-table write in the block is still Unknown.
    let body = "FIND FIRST Item NO-LOCK.\n\
                FIND FIRST tt NO-ERROR.\n\
                DO TRANSACTION: RAW-TRANSFER Item TO tt. END.\n\
                c = tt.k.\n";
    assert_eq!(missing(body), Vec::<usize>::new());
}
