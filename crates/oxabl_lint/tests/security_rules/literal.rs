//! The shared "is this argument a literal" decision.

use oxabl_ast::{Expression, StatementKind};
use oxabl_lexer::tokenize;
use oxabl_lint::security::literal::is_constant;
use oxabl_parser::Parser;

fn value_of(expr: &str) -> Expression {
    let src = format!("x = {expr}.");
    let tokens = tokenize(&src);
    let program = Parser::new(&tokens, &src).parse_program();
    assert!(program.errors.is_empty(), "{expr}: {:?}", program.errors);
    match program.statements.into_iter().next().unwrap().kind {
        StatementKind::Assignment { value, .. } => value,
        other => panic!("expected an assignment, got {other:?}"),
    }
}

#[test]
fn literals_and_their_concatenations_are_constant() {
    for expr in [
        "\"a\"",
        "'a'",
        "42",
        "1.5",
        "YES",
        "?",
        "\"a\" + \"b\"",
        "\"a\" + \"b\" + \"c\"",
        "(\"a\" + \"b\")",
        "VALUE(\"a\")",
        "VALUE(\"a\" + \"b\")",
        "IF TRUE THEN \"a\" ELSE \"b\"",
    ] {
        assert!(is_constant(&value_of(expr)), "{expr}");
    }
}

#[test]
fn everything_else_is_not() {
    for expr in [
        "c",
        "Customer.Name",
        "\"a\" + c",
        "c + \"a\"",
        "\"a\" + (\"b\" + c)",
        "QUOTER(\"a\")",
        "QUOTER(c)",
        "STRING(1)",
        "VALUE(c)",
        "VALUE(\"a\" + c)",
        "SUBSTITUTE(\"&1\", \"a\")",
        "hObj:Name",
        "aList[1]",
        "IF TRUE THEN \"a\" ELSE c",
        "IF TRUE THEN c ELSE \"a\"",
        "NEW Foo()",
    ] {
        assert!(!is_constant(&value_of(expr)), "{expr}");
    }
}

#[test]
fn an_unexpanded_preprocessor_reference_is_not_constant() {
    assert!(!is_constant(&value_of("{&NAME}")));
    assert!(!is_constant(&value_of("\"a\" + {&NAME}")));
}
