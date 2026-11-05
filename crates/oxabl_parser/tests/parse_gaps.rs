//! Constructs the OpenEdge compiler accepts that the parser must also accept
//! without recovering. Each gap has a positive fixture parsed end to end and a
//! negative one that must stay an error, so a fix cannot become a blanket skip.

use std::path::PathBuf;

use oxabl_ast::{ExpressionKind, FindType, LockType, Statement, StatementKind};
use oxabl_common::FileId;
use oxabl_parser::{Program, try_parse};
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

fn parse(source: &str) -> Program {
    try_parse(source).expect("the parser must not panic")
}

#[track_caller]
fn parse_clean(source: &str) -> Program {
    let program = parse(source);
    assert!(
        program.errors.is_empty(),
        "unexpected parse errors: {:?}",
        program.errors
    );
    program
}

#[track_caller]
fn assert_errors(source: &str) {
    assert!(
        !parse(source).errors.is_empty(),
        "expected a parse error for {source:?}"
    );
}

/// The arguments of the single `name = FUNC(...)` assignment in `source`.
#[track_caller]
fn call_arguments(program: &Program) -> (&str, Vec<&ExpressionKind>) {
    let [stmt] = program.statements.as_slice() else {
        panic!("expected one statement, got {}", program.statements.len());
    };
    let StatementKind::Assignment { value, .. } = &stmt.kind else {
        panic!("expected an assignment, got {:?}", stmt.kind);
    };
    let ExpressionKind::FunctionCall { name, arguments } = &value.kind else {
        panic!("expected a call, got {:?}", value.kind);
    };
    (
        name.name.as_str(),
        arguments.iter().map(|a| &a.kind).collect(),
    )
}

#[track_caller]
fn identifier(kind: &ExpressionKind) -> &str {
    match kind {
        ExpressionKind::Identifier(id) => id.name.as_str(),
        other => panic!("expected an identifier, got {other:?}"),
    }
}

// ---- 1. parameter modes in DYNAMIC-FUNCTION and DYNAMIC-NEW ----------------

#[test]
fn dynamic_function_takes_modes_and_table_arguments() {
    let program = parse_clean(
        "lOk = DYNAMIC-FUNCTION('getNextSteps' IN hLib,
                       INPUT-OUTPUT TABLE ttStep,
                       INPUT iOrder,
                       INPUT FALSE, // a comment
                       OUTPUT DATASET dsX BY-REFERENCE).",
    );
    let (name, args) = call_arguments(&program);
    assert_eq!(name, "DYNAMIC-FUNCTION");
    assert_eq!(args.len(), 6);
    assert_eq!(identifier(args[1]), "hLib");
    assert_eq!(identifier(args[2]), "ttStep");
    assert_eq!(identifier(args[3]), "iOrder");
    assert!(matches!(args[4], ExpressionKind::Literal(_)));
    assert_eq!(identifier(args[5]), "dsX");
}

#[test]
fn dynamic_function_takes_every_argument_form_of_run() {
    let program = parse_clean(
        "x = DYNAMIC-FUNCTION('f', OUTPUT a, TABLE-HANDLE h BIND,
                       DATASET-HANDLE dh BY-VALUE, TABLE t APPEND).",
    );
    let (_, args) = call_arguments(&program);
    let names: Vec<_> = args[1..].iter().map(|a| identifier(a)).collect();
    assert_eq!(names, ["a", "h", "dh", "t"]);
}

#[test]
fn dynamic_new_takes_modes_and_table_arguments() {
    let program =
        parse_clean("oObj = DYNAMIC-NEW 'Cls' (INPUT-OUTPUT TABLE ttStep BY-REFERENCE, OUTPUT b).");
    let (_, args) = call_arguments(&program);
    assert_eq!(args.len(), 3);
    assert_eq!(identifier(args[1]), "ttStep");
    assert_eq!(identifier(args[2]), "b");
}

#[test]
fn other_call_forms_take_modes_and_table_arguments() {
    for source in [
        "x = DYNAMIC-INVOKE(h, 'f', INPUT-OUTPUT TABLE tt, OUTPUT b).",
        "x = SUPER(INPUT-OUTPUT TABLE tt, OUTPUT b).",
        "x = oObj:Method(INPUT-OUTPUT TABLE tt, OUTPUT b, TABLE-HANDLE h).",
        "oObj = NEW Cls(INPUT-OUTPUT TABLE tt, OUTPUT b).",
    ] {
        parse_clean(source);
    }
}

#[test]
fn dynamic_function_rejects_a_mode_without_an_operand() {
    assert_errors("x = DYNAMIC-FUNCTION('f', INPUT).");
    assert_errors("x = DYNAMIC-FUNCTION('f', INPUT-OUTPUT TABLE).");
    assert_errors("x = DYNAMIC-NEW 'Cls' (OUTPUT).");
}

// ---- 2. a stray comma after a statement's period ---------------------------

#[test]
fn a_comma_after_the_terminating_period_is_skipped() {
    let program = parse_clean("RUN x/y.p (INPUT a, INPUT b).,\nMESSAGE 'x'.");
    let kinds: Vec<_> = program.statements.iter().map(|s| &s.kind).collect();
    assert_eq!(kinds.len(), 2);
    assert!(matches!(kinds[0], StatementKind::Run { .. }));
    assert!(matches!(kinds[1], StatementKind::Message { .. }));
}

#[test]
fn a_comma_after_the_period_inside_a_block_is_skipped() {
    let program = parse_clean("DO:\n  RUN x/y.p.,\n  MESSAGE 'x'.\nEND.");
    let [stmt] = program.statements.as_slice() else {
        panic!("expected one DO");
    };
    let StatementKind::Do { body, .. } = &stmt.kind else {
        panic!("expected DO, got {:?}", stmt.kind);
    };
    assert_eq!(body.len(), 2);
}

#[test]
fn commas_stay_an_error_elsewhere() {
    assert_errors("RUN x/y.p.,,\nMESSAGE 'x'.");
    assert_errors(", MESSAGE 'x'.");
    assert_errors("MESSAGE 'x',.");
    assert_errors("RUN x/y.p (INPUT a,, INPUT b).");
    // Only a period-terminated statement may be followed by the stray comma.
    assert_errors("PROCEDURE p: END PROCEDURE, MESSAGE 'x'.");
    assert_errors("DO: END, MESSAGE 'x'.");
}

// ---- 3. IF ... THEN with the block's END right behind ----------------------

fn branch_is_empty(stmt: &Statement) -> bool {
    matches!(
        &stmt.kind,
        StatementKind::If { then_branch, .. } if matches!(then_branch.kind, StatementKind::Empty)
    )
}

#[test]
fn an_empty_then_branch_leaves_the_end_to_the_block() {
    let program = parse_clean(
        "PROCEDURE p:
   DO:
      lOk = sendMail('x', cBody).
      IF NOT lOk THEN
   END.
   MESSAGE 'after'.
END PROCEDURE.
MESSAGE 'top'.",
    );
    let [procedure, top] = program.statements.as_slice() else {
        panic!("expected PROCEDURE and MESSAGE");
    };
    assert!(matches!(top.kind, StatementKind::Message { .. }));
    let StatementKind::Procedure { body, .. } = &procedure.kind else {
        panic!("expected PROCEDURE, got {:?}", procedure.kind);
    };
    let [block, after] = body.as_slice() else {
        panic!(
            "expected DO and MESSAGE in the procedure, got {}",
            body.len()
        );
    };
    assert!(matches!(after.kind, StatementKind::Message { .. }));
    let StatementKind::Do { body, .. } = &block.kind else {
        panic!("expected DO, got {:?}", block.kind);
    };
    assert_eq!(body.len(), 2);
    assert!(branch_is_empty(&body[1]));
}

#[test]
fn an_if_without_a_branch_stays_an_error_unless_a_block_end_follows() {
    assert_errors("IF NOT lOk THEN");
    assert_errors("IF NOT lOk THEN )");
    assert_errors("IF NOT lOk THEN ELSE MESSAGE 'x'.");
}

// ---- 3b. CASE ... WHEN ... THEN with the block's END right behind ---------

fn case_branches(stmt: &Statement) -> &[oxabl_ast::WhenBranch] {
    let StatementKind::Case { when_branches, .. } = &stmt.kind else {
        panic!("expected CASE, got {:?}", stmt.kind);
    };
    when_branches
}

#[test]
fn an_empty_when_branch_leaves_the_end_to_the_enclosing_block() {
    let program = parse_clean(
        "PROCEDURE p1:
  DO:
    CASE s:
      WHEN 'Init' THEN
    END.
  n = TRUE.
END PROCEDURE.
MESSAGE 'top'.",
    );
    let [procedure, top] = program.statements.as_slice() else {
        panic!("expected PROCEDURE and MESSAGE");
    };
    assert!(matches!(top.kind, StatementKind::Message { .. }));
    let StatementKind::Procedure { body, .. } = &procedure.kind else {
        panic!("expected PROCEDURE, got {:?}", procedure.kind);
    };
    let [block, after] = body.as_slice() else {
        panic!("expected DO and an assignment, got {}", body.len());
    };
    assert!(matches!(after.kind, StatementKind::Assignment { .. }));
    let StatementKind::Do { body, .. } = &block.kind else {
        panic!("expected DO, got {:?}", block.kind);
    };
    let [case] = body.as_slice() else {
        panic!("expected only the CASE in the DO, got {}", body.len());
    };
    let branches = case_branches(case);
    assert_eq!(branches.len(), 1);
    assert!(branches[0].body.is_empty());
}

#[test]
fn an_earlier_when_with_a_statement_does_not_change_the_rule() {
    let program = parse_clean(
        "DO:
  CASE s:
    WHEN 'a' THEN x = 1.
    WHEN 'b' OR WHEN 'c' THEN
  END.
y = 2.",
    );
    let [block, after] = program.statements.as_slice() else {
        panic!(
            "expected DO and an assignment, got {}",
            program.statements.len()
        );
    };
    assert!(matches!(after.kind, StatementKind::Assignment { .. }));
    let StatementKind::Do { body, .. } = &block.kind else {
        panic!("expected DO, got {:?}", block.kind);
    };
    assert_eq!(body.len(), 1);
    let branches = case_branches(&body[0]);
    assert_eq!(branches.len(), 2);
    assert_eq!(branches[0].body.len(), 1);
    assert_eq!(branches[1].values.len(), 2);
    assert!(branches[1].body.is_empty());
}

#[test]
fn a_comment_between_then_and_end_changes_nothing() {
    parse_clean("DO: CASE s: WHEN 'a' THEN /* nothing */ END. END.");
}

#[test]
fn a_filled_or_closed_case_keeps_its_own_end() {
    let program = parse_clean("DO: CASE s: WHEN 'a' THEN x = 1. END. y = 2. END.");
    let [block] = program.statements.as_slice() else {
        panic!("expected one DO");
    };
    let StatementKind::Do { body, .. } = &block.kind else {
        panic!("expected DO, got {:?}", block.kind);
    };
    assert_eq!(body.len(), 2);
    parse_clean("DO: CASE s: WHEN 'a' THEN. END CASE. END.");
}

#[test]
fn the_end_of_an_empty_when_does_not_close_the_case_twice() {
    // The CASE takes no END, so the DO owns the first one and the second is
    // left over at the top level.
    let program = parse("DO: CASE s: WHEN 'a' THEN END. x = 5. END.");
    let [block, assign, orphan] = program.statements.as_slice() else {
        panic!(
            "expected DO, assignment and orphan END, got {}",
            program.statements.len()
        );
    };
    assert!(matches!(block.kind, StatementKind::Do { .. }));
    assert!(matches!(assign.kind, StatementKind::Assignment { .. }));
    assert!(matches!(orphan.kind, StatementKind::Empty));
}

#[test]
fn an_empty_when_before_end_stays_an_error_where_the_compiler_refuses() {
    // END CASE has no CASE to close.
    assert_errors("DO: CASE s: WHEN 'a' THEN END CASE. END.");
    assert_errors("CASE s: WHEN 'a' THEN END CASE.");
    // An empty branch cannot run into OTHERWISE.
    assert_errors("CASE s: WHEN 'a' THEN OTHERWISE x = 1. END CASE.");
    assert_errors("DO: CASE s: WHEN 'a' THEN OTHERWISE x = 1. END. END.");
}

// ---- 4. CAN-FIND record-phrase options in any order ------------------------

#[track_caller]
fn can_find(source: &str) -> (FindType, LockType, bool, bool) {
    let program = parse_clean(source);
    let [stmt] = program.statements.as_slice() else {
        panic!("expected one statement");
    };
    let StatementKind::If { condition, .. } = &stmt.kind else {
        panic!("expected IF, got {:?}", stmt.kind);
    };
    let ExpressionKind::CanFind {
        find_type,
        where_clause,
        lock_type,
        no_error,
        ..
    } = &condition.kind
    else {
        panic!("expected CAN-FIND, got {:?}", condition.kind);
    };
    (
        find_type.clone(),
        lock_type.clone(),
        where_clause.is_some(),
        *no_error,
    )
}

#[test]
fn can_find_takes_a_lock_after_use_index() {
    let found = can_find(
        "IF CAN-FIND(FIRST t WHERE t.a EQ 1 AND t.b EQ \"x\":U
                    USE-INDEX idx_ab /* note */
                    NO-LOCK) THEN MESSAGE 'x'.",
    );
    assert_eq!(found, (FindType::First, LockType::NoLock, true, false));
}

#[test]
fn can_find_options_come_in_any_order() {
    for source in [
        "IF CAN-FIND(FIRST t USE-INDEX i NO-LOCK WHERE t.a = 1) THEN MESSAGE 'x'.",
        "IF CAN-FIND(FIRST t NO-LOCK USE-INDEX i WHERE t.a = 1) THEN MESSAGE 'x'.",
        "IF CAN-FIND(FIRST t NO-LOCK WHERE t.a = 1 USE-INDEX i) THEN MESSAGE 'x'.",
        "IF CAN-FIND(FIRST t WHERE t.a = 1 USE-INDEX i NO-LOCK) THEN MESSAGE 'x'.",
    ] {
        let (_, lock, has_where, _) = can_find(source);
        assert_eq!((lock, has_where), (LockType::NoLock, true), "{source}");
    }
    let found =
        can_find("IF CAN-FIND(t USE-INDEX i EXCLUSIVE-LOCK NO-WAIT NO-ERROR) THEN MESSAGE 'x'.");
    assert_eq!(
        found,
        (FindType::Unique, LockType::ExclusiveLock, false, true)
    );
    let found = can_find("IF CAN-FIND(FIRST t USING t.a AND t.b NO-LOCK) THEN MESSAGE 'x'.");
    assert_eq!(found, (FindType::First, LockType::NoLock, false, false));
}

#[test]
fn can_find_still_rejects_what_is_not_an_option() {
    assert_errors("IF CAN-FIND(FIRST t USE-INDEX i NO-LOCK bogus) THEN MESSAGE 'x'.");
    assert_errors("IF CAN-FIND(FIRST t WHERE t.a = 1 WHERE t.b = 2) THEN MESSAGE 'x'.");
    assert_errors("IF CAN-FIND(FIRST t USE-INDEX i NO-LOCK THEN MESSAGE 'x'.");
}

// ---- 5. include arguments with a quoted value ------------------------------

const TT_INCLUDE: &str =
    "DEF TEMP-TABLE {&table} NO-UNDO LIKE customer FIELD x LIKE customer.name.";

fn parse_with_include(main: &str) -> Program {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("lib/tt.v"), TT_INCLUDE);
    let include_paths = vec![PathBuf::from("")];
    let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
    let expanded = pp
        .process(FileId::new(1), main)
        .expect("preprocessing should succeed")
        .to_text();
    parse(&expanded)
}

#[test]
fn a_quoted_include_argument_substitutes_without_its_quotes() {
    for main in [
        "{lib/tt.v &table=\"ttOrders\"}",
        "{lib/tt.v &table='ttOrders'}",
        "{lib/tt.v &table=ttOrders}",
    ] {
        let program = parse_with_include(main);
        assert!(program.errors.is_empty(), "{main}: {:?}", program.errors);
        let [stmt] = program.statements.as_slice() else {
            panic!("{main}: expected one statement");
        };
        let StatementKind::DefineTempTable {
            name,
            no_undo,
            like_table,
            fields,
            ..
        } = &stmt.kind
        else {
            panic!("{main}: expected DEFINE TEMP-TABLE, got {:?}", stmt.kind);
        };
        assert_eq!(name.name, "ttOrders", "{main}");
        assert!(*no_undo);
        assert_eq!(
            like_table.as_ref().map(|t| t.name.as_str()),
            Some("customer")
        );
        assert_eq!(fields.len(), 1);
    }
}

#[test]
fn an_include_without_its_argument_stays_an_error() {
    let program = parse_with_include("{lib/tt.v}");
    assert!(!program.errors.is_empty());
}
