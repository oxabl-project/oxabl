//! Names and clause orders the OpenEdge compiler accepts that the parser must
//! also accept without recovering: reserved keywords as method and procedure
//! names, digit-first stream and procedure names, `USE-INDEX` before `WHERE`,
//! the `CALL` statement and `INPUT NOT expr` arguments. Each has a negative
//! case that must stay an error.

use oxabl_ast::{ExpressionKind, FindType, LockType, Statement, StatementKind};
use oxabl_parser::{Program, try_parse};

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

fn method_names(stmts: &[Statement]) -> Vec<&str> {
    let mut names = Vec::new();
    for stmt in stmts {
        match &stmt.kind {
            StatementKind::Method { name, .. } => names.push(name.name.as_str()),
            StatementKind::Class { body, .. } => names.extend(method_names(body)),
            _ => {}
        }
    }
    names
}

fn procedure_names(program: &Program) -> Vec<&str> {
    program
        .statements
        .iter()
        .filter_map(|s| match &s.kind {
            StatementKind::Procedure { name, .. } => Some(name.name.as_str()),
            _ => None,
        })
        .collect()
}

fn kinds(program: &Program) -> Vec<&StatementKind> {
    program.statements.iter().map(|s| &s.kind).collect()
}

// ---- 8a. reserved keywords as method names ---------------------------------

#[test]
fn reserved_keywords_are_method_names() {
    let program = parse_clean(
        "CLASS KwMethods:
           METHOD PUBLIC VOID Clear():
           END METHOD.
           METHOD PUBLIC CHARACTER View(INPUT pId AS CHARACTER):
             RETURN pId.
           END METHOD.
           METHOD PUBLIC LOGICAL Any(INPUT pList AS CHARACTER):
             RETURN pList <> \"\".
           END METHOD.
           METHOD PUBLIC STATIC VOID Check():
           END METHOD.
         END CLASS.",
    );
    assert_eq!(
        method_names(&program.statements),
        ["Clear", "View", "Any", "Check"]
    );
}

#[test]
fn a_method_named_like_a_keyword_keeps_its_parameters_and_body() {
    let program = parse_clean(
        "CLASS C:
           METHOD PUBLIC CHARACTER View(INPUT pId AS CHARACTER):
             RETURN pId.
           END METHOD.
         END CLASS.",
    );
    let StatementKind::Class { body, .. } = &program.statements[0].kind else {
        panic!("expected a class");
    };
    let StatementKind::Method {
        parameters, body, ..
    } = &body[0].kind
    else {
        panic!("expected a method");
    };
    assert_eq!(parameters.len(), 1);
    assert!(matches!(body[0].kind, StatementKind::Return { .. }));
}

#[test]
fn keyword_method_names_work_at_the_call_site() {
    let program = parse_clean(
        "x = oObj:View(y).
         x = THIS-OBJECT:Any(y).
         RUN Status (INPUT x).",
    );
    assert_eq!(program.statements.len(), 3);
    assert!(matches!(kinds(&program)[2], StatementKind::Run { .. }));
}

#[test]
fn a_bare_view_call_stays_the_view_statement() {
    let program = parse_clean("View(x).");
    assert!(matches!(kinds(&program)[0], StatementKind::View { .. }));
}

#[test]
fn a_method_still_needs_a_name_and_a_parameter_list() {
    assert_errors("CLASS C:\n METHOD PUBLIC VOID ():\n END METHOD.\nEND CLASS.");
    assert_errors("CLASS C:\n METHOD PUBLIC VOID View:\n END METHOD.\nEND CLASS.");
    assert_errors("CLASS C:\n METHOD PUBLIC VOID .\n END METHOD.\nEND CLASS.");
}

// ---- 8b. reserved keywords as procedure names ------------------------------

#[test]
fn reserved_keywords_are_procedure_names() {
    let program = parse_clean(
        "PROCEDURE check:
           MESSAGE \"check\".
         END PROCEDURE.
         PROCEDURE Status:
           MESSAGE \"status\".
         END PROCEDURE.
         RUN check.
         RUN Status.",
    );
    assert_eq!(procedure_names(&program), ["check", "Status"]);
    assert_eq!(program.statements.len(), 4);
    assert!(matches!(kinds(&program)[2], StatementKind::Run { .. }));
    assert!(matches!(kinds(&program)[3], StatementKind::Run { .. }));
}

#[test]
fn a_procedure_still_needs_a_name() {
    assert_errors("PROCEDURE :\nEND PROCEDURE.");
    assert_errors("PROCEDURE.\nEND PROCEDURE.");
}

// ---- 8d. USE-INDEX before WHERE --------------------------------------------

#[test]
fn find_takes_use_index_before_where() {
    let program = parse_clean(
        "FIND FIRST orders USE-INDEX order-num WHERE orders.order-num > 0 NO-LOCK NO-ERROR.",
    );
    let StatementKind::Find {
        find_type,
        where_clause,
        lock_type,
        no_error,
        ..
    } = &program.statements[0].kind
    else {
        panic!("expected FIND");
    };
    assert_eq!(*find_type, FindType::First);
    assert!(where_clause.is_some());
    assert_eq!(*lock_type, LockType::NoLock);
    assert!(*no_error);
}

#[test]
fn find_takes_a_lock_between_use_index_and_where() {
    let program = parse_clean("FIND FIRST orders USE-INDEX i NO-LOCK WHERE orders.a = 1.");
    let StatementKind::Find {
        where_clause,
        lock_type,
        ..
    } = &program.statements[0].kind
    else {
        panic!("expected FIND");
    };
    assert!(where_clause.is_some());
    assert_eq!(*lock_type, LockType::NoLock);
}

#[test]
fn for_each_takes_use_index_before_where() {
    let program = parse_clean(
        "FOR EACH orders USE-INDEX order-num WHERE orders.cust-name = \"A\" BREAK BY orders.order-num:
         END.",
    );
    let StatementKind::ForEach {
        where_clause,
        lock_type,
        ..
    } = &program.statements[0].kind
    else {
        panic!("expected FOR EACH");
    };
    assert!(where_clause.is_some());
    assert_eq!(*lock_type, LockType::ShareLock);
}

#[test]
fn for_each_takes_a_lock_and_use_index_before_where() {
    for source in [
        "FOR EACH orders NO-LOCK USE-INDEX i WHERE orders.a = 1:\nEND.",
        "FOR EACH orders USE-INDEX i NO-LOCK WHERE orders.a = 1:\nEND.",
    ] {
        let program = parse_clean(source);
        let StatementKind::ForEach {
            where_clause,
            lock_type,
            ..
        } = &program.statements[0].kind
        else {
            panic!("expected FOR EACH");
        };
        assert!(where_clause.is_some(), "{source}");
        assert_eq!(*lock_type, LockType::NoLock, "{source}");
    }
}

#[test]
fn use_index_after_where_still_parses() {
    parse_clean("FIND FIRST orders WHERE orders.a = 1 USE-INDEX i NO-LOCK.");
    parse_clean("FOR EACH orders WHERE orders.a = 1 NO-LOCK USE-INDEX i BY orders.a:\nEND.");
}

#[test]
fn use_index_needs_no_second_where_to_end() {
    assert_errors("FIND FIRST orders USE-INDEX i WHERE.");
    assert_errors("FIND FIRST orders USE-INDEX i WHERE orders.a = 1 WHERE orders.b = 2.");
}

// ---- 8e. names that start with a digit -------------------------------------

#[test]
fn a_stream_name_may_start_with_a_digit() {
    let program = parse_clean(
        "DEFINE STREAM 2dstream.
         OUTPUT STREAM 2dstream TO \"out.txt\".
         PUT STREAM 2dstream UNFORMATTED \"x\" SKIP.
         OUTPUT STREAM 2dstream CLOSE.",
    );
    let StatementKind::DefineStream { name } = &program.statements[0].kind else {
        panic!("expected DEFINE STREAM");
    };
    assert_eq!(name.name, "2dstream");
    let StatementKind::StreamIo { stream_name, .. } = &program.statements[1].kind else {
        panic!("expected a stream statement");
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "2dstream");
    let StatementKind::Put { stream_name, items } = &program.statements[2].kind else {
        panic!("expected PUT");
    };
    assert_eq!(stream_name.as_ref().unwrap().name, "2dstream");
    assert_eq!(items.len(), 1);
}

#[test]
fn a_procedure_name_may_start_with_a_digit() {
    let program = parse_clean(
        "PROCEDURE 128_set_width:
         END PROCEDURE.
         RUN 128_set_width.",
    );
    assert_eq!(procedure_names(&program), ["128_set_width"]);
    assert!(matches!(kinds(&program)[1], StatementKind::Run { .. }));
}

#[test]
fn a_number_is_still_not_a_name() {
    assert_errors("DEFINE STREAM 2.");
    assert_errors("DEFINE STREAM 2 dstream.");
    assert_errors("PROCEDURE 128:\nEND PROCEDURE.");
    assert_errors("PROCEDURE 128 _x:\nEND PROCEDURE.");
    assert_errors("DEFINE VARIABLE 2dvar AS INTEGER NO-UNDO.");
}

// ---- 8f. the CALL statement ------------------------------------------------

#[test]
fn the_call_statement_is_a_statement() {
    let program = parse_clean(
        "DEFINE VARIABLE cArg AS CHARACTER NO-UNDO INITIAL \"10\".
         CALL barwidth cArg.
         MESSAGE cArg.",
    );
    assert_eq!(program.statements.len(), 3);
    let StatementKind::Skipped { names, .. } = &program.statements[1].kind else {
        panic!(
            "expected the CALL statement, got {:?}",
            program.statements[1].kind
        );
    };
    assert!(names.iter().any(|n| n.name == "cArg"));
    assert!(matches!(
        program.statements[2].kind,
        StatementKind::Message { .. }
    ));
}

#[test]
fn create_call_is_not_the_call_statement() {
    let program = parse_clean("CREATE CALL hCall.");
    assert!(matches!(kinds(&program)[0], StatementKind::Create { .. }));
}

#[test]
fn call_without_a_routine_stays_an_error() {
    assert_errors("CALL.");
    assert_errors("CALL = 1 +.");
}

// ---- 8g. INPUT NOT expr as an argument -------------------------------------

#[test]
fn input_not_is_an_argument_in_every_call_form() {
    let program = parse_clean(
        "DYNAMIC-FUNCTION(\"set-flag\":U IN hLib, INPUT NOT lActive).
         RUN p (INPUT NOT lActive).
         x = f(INPUT NOT lActive).
         x = oObj:M(INPUT NOT lActive, OUTPUT y).",
    );
    let ExpressionKind::FunctionCall { arguments, .. } = (match &program.statements[0].kind {
        StatementKind::ExpressionStatement(e) => &e.kind,
        other => panic!("expected an expression statement, got {other:?}"),
    }) else {
        panic!("expected a call");
    };
    assert_eq!(arguments.len(), 3);
    assert!(matches!(arguments[2].kind, ExpressionKind::Not(_)));
}

#[test]
fn input_without_an_operand_stays_an_error() {
    assert_errors("DYNAMIC-FUNCTION(\"f\" IN hLib, INPUT NOT).");
}

// ---- the bench fixture -----------------------------------------------------

#[test]
fn the_bench_fixture_parses_without_errors() {
    parse_clean(include_str!(
        "../../../resources/bench_parser_name_forms.abl"
    ));
}
