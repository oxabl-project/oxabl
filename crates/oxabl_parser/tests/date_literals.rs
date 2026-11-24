//! Date constants. The compiler reads digit runs joined by `/` with no
//! whitespace (`02/01/2026`, `2/1/26`, `10/2`) as one DATE constant, and digits
//! with a spaced `/` as a division.

use oxabl_ast::{Expression, ExpressionKind, Literal, StatementKind};
use oxabl_parser::{Program, try_parse};

#[track_caller]
fn parse_clean(source: &str) -> Program {
    let program = try_parse(source).expect("the parser must not panic");
    assert!(
        program.errors.is_empty(),
        "unexpected parse errors: {:?}",
        program.errors
    );
    program
}

/// The value of the single `name = value` assignment in `source`.
#[track_caller]
fn assigned_value(program: &Program) -> &Expression {
    let [stmt] = program.statements.as_slice() else {
        panic!("expected one statement, got {}", program.statements.len());
    };
    let StatementKind::Assignment { value, .. } = &stmt.kind else {
        panic!("expected an assignment, got {:?}", stmt.kind);
    };
    value
}

#[test]
fn a_date_constant_is_one_date_literal_with_its_spelling_and_span() {
    for (source, text) in [
        ("d = 02/01/2026.", "02/01/2026"),
        ("d = 2/1/26.", "2/1/26"),
        ("d = 10/2.", "10/2"),
    ] {
        let program = parse_clean(source);
        let value = assigned_value(&program);
        let ExpressionKind::Literal(Literal::Date(date)) = &value.kind else {
            panic!(
                "expected a date literal for {source:?}, got {:?}",
                value.kind
            );
        };
        assert_eq!(date.text, text);
        assert_eq!(
            &source[date.span.start as usize..date.span.end as usize],
            text
        );
        assert_eq!(value.span, date.span);
    }
}

#[test]
fn a_date_constant_is_an_operand() {
    let program = parse_clean("d = 02/01/2026 + 1.");
    let value = assigned_value(&program);
    let ExpressionKind::Add(left, _) = &value.kind else {
        panic!("expected an addition, got {:?}", value.kind);
    };
    assert!(matches!(
        left.kind,
        ExpressionKind::Literal(Literal::Date(_))
    ));
}

#[test]
fn a_spaced_slash_is_a_division() {
    let program = parse_clean("x = 10 / 2 / 5.");
    let value = assigned_value(&program);
    let ExpressionKind::Divide(left, right) = &value.kind else {
        panic!("expected a division, got {:?}", value.kind);
    };
    assert!(matches!(
        right.kind,
        ExpressionKind::Literal(Literal::Integer(_))
    ));
    assert!(matches!(left.kind, ExpressionKind::Divide(..)));
}

#[test]
fn a_date_format_mask_and_a_digit_path_still_parse() {
    parse_clean(
        "DEFINE VARIABLE d AS DATE NO-UNDO FORMAT 99/99/9999.\nDISPLAY d FORMAT 99/99/99.\n",
    );
    parse_clean("RUN rpt/2024/01/x.p.\n");
    // `9/99/99/99` lexes as `9`, `/` and the date constant `99/99/99`.
    parse_clean(
        "DEFINE VARIABLE c AS CHARACTER NO-UNDO FORMAT 9/99/99/99.\nDISPLAY c FORMAT 9/99/99/99.\n",
    );
}

#[test]
fn a_run_path_ending_in_a_date_constant_is_one_target() {
    // `10/2` before `.` and a newline is one date constant.
    for (source, target) in [
        ("RUN rpt/10/2.\n", "rpt/10/2"),
        ("RUN rpt/1/2/3.\n", "rpt/1/2/3"),
        ("RUN 10/2/x.p.\n", "10/2/x.p"),
        ("RUN 10/2.\n", "10/2"),
    ] {
        let program = parse_clean(source);
        let [stmt] = program.statements.as_slice() else {
            panic!("expected one statement for {source:?}");
        };
        let StatementKind::Run { target: run, .. } = &stmt.kind else {
            panic!("expected RUN, got {:?}", stmt.kind);
        };
        assert_eq!(run.literal_name(), Some(target), "{source:?}");
    }
}

#[test]
fn a_date_constant_in_an_initial_value() {
    let program = parse_clean("DEFINE VARIABLE d AS DATE NO-UNDO INITIAL 02/01/2026.");
    let [stmt] = program.statements.as_slice() else {
        panic!("expected one statement");
    };
    let StatementKind::VariableDeclaration {
        initial_value: Some(init),
        ..
    } = &stmt.kind
    else {
        panic!(
            "expected a declaration with an initial value, got {:?}",
            stmt.kind
        );
    };
    assert!(matches!(
        init.kind,
        ExpressionKind::Literal(Literal::Date(_))
    ));
}
