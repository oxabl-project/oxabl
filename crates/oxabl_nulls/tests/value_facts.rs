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
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    let nulls = FileNulls::default().with_values(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        &flow,
        &NullOptions::default(),
    );
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
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    let nulls = FileNulls::default().with_values(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        &flow,
        &NullOptions::default(),
    );
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
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    let nulls = FileNulls::default().with_values(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        &flow,
        &NullOptions::default(),
    );
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
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    let nulls = FileNulls::default().with_values(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        &flow,
        &NullOptions::default(),
    );
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

fn uses(source: &str) -> Vec<String> {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    FileNulls::default()
        .with_values(
            oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
            &flow,
            &NullOptions::default(),
        )
        .values()
        .iter()
        .map(|u| {
            format!(
                "{}@{}",
                u.name,
                source[..u.at.start as usize].lines().count()
            )
        })
        .collect()
}

#[test]
fn statements_that_run_triggers_forget_what_the_triggers_write() {
    for statement in [
        "WAIT-FOR GO OF FRAME f.",
        "APPLY \"CHOOSE\" TO btn.",
        "ENABLE ALL WITH FRAME f.",
        "PUBLISH \"done\".",
        "UPDATE n WITH FRAME f.",
        "SET n WITH FRAME f.",
    ] {
        let source = format!(
            "DEFINE VARIABLE d AS DATE NO-UNDO.
DEFINE VARIABLE n AS INTEGER NO-UNDO.
DEFINE BUTTON btn.
ON CHOOSE OF btn DO:
  d = TODAY.
END.
{statement}
IF d > TODAY THEN MESSAGE n.
"
        );
        assert_eq!(uses(&source), Vec::<String>::new(), "{statement}");
    }
    // Without such a statement the default date is a source.
    let source = "DEFINE VARIABLE d AS DATE NO-UNDO.
DEFINE BUTTON btn.
ON CHOOSE OF btn DO:
  d = TODAY.
END.
IF d > TODAY THEN MESSAGE 1.
";
    assert_eq!(uses(source), ["d@6"]);
}

#[test]
fn a_repeat_loop_variable_is_written_by_the_loop() {
    let source = "DEFINE VARIABLE d AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
REPEAT d = TODAY TO TODAY + 5:
  IF d > TODAY THEN LEAVE.
END.
REPEAT i = 1 TO 5:
  IF i = 0 THEN d = ?.
END.
IF d > TODAY THEN MESSAGE 1.
";
    // `i` is no constant 0, so `d = ?` can run.
    assert_eq!(uses(source), ["d@9"]);
}

#[test]
fn a_counted_loop_with_constant_bounds_runs_its_body() {
    let run = |header: &str, skip: &str| {
        uses(&format!(
            "DEFINE VARIABLE d AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE n AS INTEGER NO-UNDO.
n = 5.
{header}
  {skip}
  d = TODAY.
END.
IF d > TODAY THEN MESSAGE 1.
"
        ))
    };
    assert_eq!(run("DO i = 1 TO 5:", ""), Vec::<String>::new());
    assert_eq!(run("DO i = 5 TO 1 BY -1:", ""), Vec::<String>::new());
    // Every iteration may skip the write.
    assert_eq!(run("DO i = 1 TO 5:", "IF n = i THEN NEXT."), ["d@9"]);
    assert_eq!(run("DO i = 1 TO 0:", ""), ["d@9"]);
    assert_eq!(run("DO i = 1 TO n:", ""), ["d@9"]);
    assert_eq!(run("DO i = 1 TO 5 WHILE n > 0:", ""), ["d@9"]);
}

#[test]
fn a_zero_divisor_is_no_source() {
    let source = "DEFINE VARIABLE y AS DECIMAL NO-UNDO.
DEFINE VARIABLE z AS DECIMAL NO-UNDO.
y = 1 / 0.
z = y + 1.
";
    assert_eq!(uses(source), Vec::<String>::new());
}

/// The text and kind of each source of one use, or `None`.
type Sources = Option<Vec<(String, oxabl_nulls::ValueSourceKind)>>;

/// The sources of the first use of each variable, as a test sees them: the
/// text and kind of each, or `None`.
fn first_sources(source: &str) -> Vec<(String, Sources)> {
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let flow = oxabl_flow::build_file_tokens(
        &program.statements,
        &program.pools,
        source,
        &tokens,
        &Default::default(),
    );
    let nulls = FileNulls::default().with_values(
        oxabl_flow::FileInput::new(&program.statements, &program.pools, source, &tokens),
        &flow,
        &NullOptions::default(),
    );
    nulls
        .first_values()
        .map(|u| {
            let sources = u.sources.as_ref().map(|found| {
                found
                    .iter()
                    .map(|s| {
                        (
                            source[s.at.start as usize..s.at.end as usize].to_string(),
                            s.kind,
                        )
                    })
                    .collect()
            });
            (u.name.clone(), sources)
        })
        .collect()
}

#[test]
fn a_pair_that_passes_the_unknown_value_through_a_choice_keeps_its_source() {
    use oxabl_nulls::ValueSourceKind::Assignment;
    let source = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
IF n = 2 THEN x = ?.\n\
ASSIGN x = (IF n = 1 THEN x ELSE 0)\n\
       n = x * 2.\n";
    assert_eq!(
        first_sources(source),
        [(
            "x".to_string(),
            Some(vec![("x = ?".to_string(), Assignment)])
        )]
    );
}

/// A choice that tests the variable can store the `?` of another value only
/// because the variable was `?`. The list would miss that pair, so there is
/// none.
#[test]
fn a_pair_whose_choice_tests_the_variable_gives_no_partial_list() {
    let source = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n\
DEFINE VARIABLE y AS INTEGER NO-UNDO.\n\
DEFINE INPUT PARAMETER n AS INTEGER NO-UNDO.\n\
IF n = 2 THEN x = ?.\n\
y = ?.\n\
x = (IF x = ? THEN y ELSE 0).\n\
n = x * 2.\n";
    let found = first_sources(source);
    let x = found
        .iter()
        .find(|(name, _)| name == "x")
        .expect("a use of x");
    assert!(
        x.1.as_ref().is_none_or(|list| list
            .iter()
            .any(|(text, _)| text == "x = (IF x = ? THEN y ELSE 0)")),
        "{found:?}"
    );
}
