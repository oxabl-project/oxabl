use oxabl_flow::NodeKind;
use oxabl_nulls::{FileNulls, NullOptions, ValueState};

#[test]
fn facts_distinguish_unknown_from_a_possible_question_mark() {
    let source = "DEFINE INPUT PARAMETER caller AS INTEGER NO-UNDO.\n\
DEFINE VARIABLE n AS INTEGER INITIAL ? NO-UNDO.\n\
MESSAGE n.\n\
IF n = ? THEN RETURN.\n\
MESSAGE n.\n";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty());
    let flow =
        oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &Default::default());
    let nulls =
        FileNulls::default().with_values(&program.statements, &flow, &NullOptions::default());
    let cfg = &flow.units()[0];
    let messages: Vec<_> = cfg
        .nodes()
        .filter(|(_, n)| {
            n.kind == NodeKind::Stmt
                && source[n.span.start as usize..n.span.end as usize].starts_with("MESSAGE")
        })
        .map(|(n, _)| n)
        .collect();
    assert_eq!(messages.len(), 2);
    assert_eq!(
        nulls.value_before(cfg.id(), messages[0], "N"),
        Some(ValueState::PossiblyUnknown)
    );
    assert_eq!(
        nulls.value_before(cfg.id(), messages[0], "caller"),
        Some(ValueState::Unknown)
    );
    assert_eq!(
        nulls.value_before(cfg.id(), messages[1], "n"),
        Some(ValueState::Unreachable)
    );
    assert_eq!(nulls.value_before(cfg.id(), messages[0], "absent"), None);
}

#[test]
fn type_defaults_supply_sources_without_a_schema() {
    let source = "DEFINE VARIABLE d AS DATE NO-UNDO.
DEFINE VARIABLE dt AS DATETIME NO-UNDO.
DEFINE VARIABLE tz AS DATETIME-TZ NO-UNDO.
DEFINE VARIABLE h AS HANDLE NO-UNDO.
DEFINE VARIABLE rid AS ROWID NO-UNDO.
DEFINE VARIABLE rec AS RECID NO-UNDO.
DEFINE VARIABLE obj AS CLASS Progress.Lang.Object NO-UNDO.
DEFINE VARIABLE n AS INTEGER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.
DEFINE VARIABLE b AS LOGICAL NO-UNDO.
MESSAGE 0.";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty());
    let flow =
        oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &Default::default());
    let nulls =
        FileNulls::default().with_values(&program.statements, &flow, &NullOptions::default());
    let cfg = &flow.units()[0];
    let node = cfg
        .nodes()
        .find(|(_, n)| {
            n.kind == NodeKind::Stmt
                && source[n.span.start as usize..n.span.end as usize].starts_with("MESSAGE")
        })
        .expect("message")
        .0;
    for name in ["d", "dt", "tz", "h", "rid", "rec", "obj"] {
        assert_eq!(
            nulls.value_before(cfg.id(), node, name),
            Some(ValueState::PossiblyUnknown),
            "{name}"
        );
    }
    for name in ["n", "c", "b"] {
        assert_eq!(
            nulls.value_before(cfg.id(), node, name),
            Some(ValueState::Known),
            "{name}"
        );
    }
}

#[test]
fn removing_an_impossible_source_keeps_other_values_unknown() {
    let source = "DEFINE VARIABLE a AS INTEGER INITIAL ? NO-UNDO.
DEFINE VARIABLE b AS INTEGER INITIAL ? NO-UNDO.
DEFINE INPUT PARAMETER stopLoop AS LOGICAL NO-UNDO.
REPEAT:
 IF a = ? AND b = ? THEN ASSIGN a = unseen() b = unseen().
 ELSE MESSAGE a.
 IF stopLoop THEN LEAVE.
END.";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty());
    let flow =
        oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &Default::default());
    let nulls =
        FileNulls::default().with_values(&program.statements, &flow, &NullOptions::default());
    let cfg = &flow.units()[0];
    let node = cfg
        .nodes()
        .find(|(_, n)| {
            n.kind == NodeKind::Stmt
                && source[n.span.start as usize..n.span.end as usize].starts_with("MESSAGE")
        })
        .expect("message")
        .0;
    for name in ["a", "b"] {
        assert_eq!(
            nulls.value_before(cfg.id(), node, name),
            Some(ValueState::Unknown),
            "{name}"
        );
    }
}

#[test]
fn nested_loops_over_three_valued_tests_reach_a_fixed_point() {
    let source = "DEFINE VARIABLE d1 AS INTEGER NO-UNDO INITIAL TODAY.
DEFINE VARIABLE i1 AS INTEGER NO-UNDO.
REPEAT:
  DO WHILE i1 <> ? AND d1 <> ?:
    i1 = ?.
  END.
END.
";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty());
    let flow =
        oxabl_flow::build_file_tokens(&program.statements, source, &tokens, &Default::default());
    let nulls =
        FileNulls::default().with_values(&program.statements, &flow, &NullOptions::default());
    let cfg = &flow.units()[0];
    let write = cfg
        .nodes()
        .find(|(_, n)| {
            n.kind == NodeKind::Stmt
                && source[n.span.start as usize..n.span.end as usize].starts_with("i1 = ?")
        })
        .expect("assignment")
        .0;
    // A solve that gave up would say Unknown.
    assert_eq!(
        nulls.value_before(cfg.id(), write, "i1"),
        Some(ValueState::Known)
    );
}
