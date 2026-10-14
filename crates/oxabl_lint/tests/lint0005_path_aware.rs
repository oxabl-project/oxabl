//! `block-var-used-outside` (LINT0005) on real control flow: the rule stays
//! silent when the defining block runs on every path to the read, and keeps
//! firing whenever some path reaches the read without it.

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0005, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// The names LINT0005 reports, as written in the message.
fn reported(source: &str) -> Vec<String> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    let mut names: Vec<String> = lint_file(&program.statements, &sem, &ctx)
        .iter()
        .filter(|d| d.code.0 == LINT0005)
        .map(|d| d.message.split('`').nth(1).unwrap().to_string())
        .collect();
    names.sort();
    names
}

fn lint0005(source: &str) -> usize {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
        .iter()
        .filter(|d| d.code.0 == LINT0005)
        .count()
}

#[test]
fn guard_clause_that_returns_is_silent() {
    let src = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
IF NOT valid THEN DO:
    MESSAGE \"bad input\".
    RETURN.
END.
ELSE DO:
    DEFINE VARIABLE v-amount AS DECIMAL NO-UNDO.
    v-amount = 1.
END.
DISPLAY v-amount.
";
    assert_eq!(lint0005(src), 0);
}

#[test]
fn guard_clause_without_the_return_still_fires() {
    let src = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
IF NOT valid THEN DO:
    MESSAGE \"bad input\".
END.
ELSE DO:
    DEFINE VARIABLE v-amount AS DECIMAL NO-UNDO.
    v-amount = 1.
END.
DISPLAY v-amount.
";
    assert_eq!(lint0005(src), 1);
}

#[test]
fn conditional_block_fires() {
    let src = "\
DEFINE VARIABLE cond AS LOGICAL NO-UNDO.
IF cond THEN DO:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    i = 2.
END.
DISPLAY i.
";
    assert_eq!(lint0005(src), 1);
}

#[test]
fn block_that_always_runs_is_silent() {
    let src = "\
DO:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    i = 2.
END.
DISPLAY i.
";
    assert_eq!(lint0005(src), 0);
}

#[test]
fn every_case_branch_assigns_or_leaves() {
    let src = "\
DEFINE VARIABLE k AS INTEGER NO-UNDO.
CASE k:
    WHEN 1 THEN DO:
        DEFINE VARIABLE s AS CHARACTER NO-UNDO.
        s = \"one\".
    END.
    OTHERWISE RETURN.
END CASE.
DISPLAY s.
";
    assert_eq!(lint0005(src), 0);
}

#[test]
fn a_loop_that_may_not_run_fires() {
    let src = "\
FOR EACH Customer NO-LOCK:
    DEFINE VARIABLE n AS INTEGER NO-UNDO.
    n = 1.
END.
DISPLAY n.
";
    assert_eq!(lint0005(src), 1);
}

#[test]
fn an_error_path_out_of_the_block_fires() {
    // ON ERROR UNDO, LEAVE continues after the block even when the
    // assignment failed.
    let src = "\
DO ON ERROR UNDO, LEAVE:
    DEFINE VARIABLE n AS INTEGER NO-UNDO.
    n = INTEGER(\"x\").
END.
DISPLAY n.
";
    assert_eq!(lint0005(src), 1);
}

#[test]
fn an_undo_can_restore_an_undoable_variable() {
    // The second statement can fail after the assignment; UNDO, LEAVE then
    // restores `n` (no NO-UNDO) and the read sees the old value.
    let src = "\
DO ON ERROR UNDO, LEAVE:
    DEFINE VARIABLE n AS INTEGER.
    n = 1.
    RUN other.p.
END.
DISPLAY n.
";
    assert_eq!(lint0005(src), 1);
    // With NO-UNDO the value survives the undo, and the literal assignment
    // itself cannot fail, so every path to the read assigns it.
    assert_eq!(
        lint0005(&src.replace("AS INTEGER.", "AS INTEGER NO-UNDO.")),
        0
    );
}

#[test]
fn a_read_in_an_internal_procedure_fires() {
    // The procedure can run before the block, from anywhere.
    let src = "\
DO:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    i = 2.
END.
PROCEDURE show:
    DISPLAY i.
END PROCEDURE.
";
    assert_eq!(lint0005(src), 1);
}

#[test]
fn an_undo_statement_can_restore_an_undoable_variable() {
    // `UNDO, LEAVE` rolls `v` back to its value before the iteration, so the
    // read after the loop can see the default value.
    let src = "\
DEFINE VARIABLE x AS LOGICAL NO-UNDO.
REPEAT:
    DEFINE VARIABLE v AS INTEGER.
    v = 1.
    IF x THEN UNDO, LEAVE.
    LEAVE.
END.
MESSAGE v.
";
    assert_eq!(lint0005(src), 1);
    // Same with the loop's own ON ENDKEY phrase beside it.
    assert_eq!(
        lint0005(&src.replace("REPEAT:", "REPEAT ON ENDKEY UNDO, LEAVE:")),
        1
    );
    // NO-UNDO keeps the value, so every path to the read assigns it.
    assert_eq!(
        lint0005(&src.replace("AS INTEGER.", "AS INTEGER NO-UNDO.")),
        0
    );
}

#[test]
fn an_undo_of_a_block_that_does_not_overlap_the_variable_is_ignored() {
    // The REPEAT neither contains nor sits in the DO that defines `v`.
    let src = "\
DEFINE VARIABLE x AS LOGICAL NO-UNDO.
DO:
    DEFINE VARIABLE v AS INTEGER.
    v = 1.
END.
REPEAT:
    IF x THEN UNDO, LEAVE.
    LEAVE.
END.
MESSAGE v.
";
    assert_eq!(lint0005(src), 0);
}

#[test]
fn an_undo_that_names_an_enclosing_block_restores_its_variables() {
    let src = "\
DEFINE VARIABLE x AS LOGICAL NO-UNDO.
outer:
REPEAT:
    DEFINE VARIABLE v AS INTEGER.
    v = 1.
    DO:
        IF x THEN UNDO outer, LEAVE outer.
    END.
    LEAVE.
END.
MESSAGE v.
";
    assert_eq!(lint0005(src), 1);
    assert_eq!(
        lint0005(&src.replace("AS INTEGER.", "AS INTEGER NO-UNDO.")),
        0
    );
}

// One solve answers for every variable of a block: each keeps its own answer.

#[test]
fn the_variables_of_one_block_are_judged_apart() {
    let src = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
DEFINE VARIABLE c AS LOGICAL NO-UNDO.
IF NOT valid THEN DO:
    RETURN.
END.
ELSE DO:
    DEFINE VARIABLE always AS DECIMAL NO-UNDO.
    DEFINE VARIABLE sometimes AS DECIMAL NO-UNDO.
    DEFINE VARIABLE never-here AS DECIMAL NO-UNDO.
    always = 1.
    IF c THEN sometimes = 2.
    IF c THEN never-here = 3.
    never-here = 4.
END.
DISPLAY always sometimes never-here.
";
    assert_eq!(reported(src), ["sometimes"]);
}

#[test]
fn an_undo_restores_only_the_undoable_variables_of_the_block() {
    let src = "\
DEFINE VARIABLE x AS LOGICAL NO-UNDO.
REPEAT:
    DEFINE VARIABLE restored AS INTEGER.
    DEFINE VARIABLE kept AS INTEGER NO-UNDO.
    restored = 1.
    kept = 1.
    IF x THEN UNDO, LEAVE.
    LEAVE.
END.
MESSAGE restored kept.
";
    assert_eq!(reported(src), ["restored"]);
}

#[test]
fn a_block_with_more_variables_than_one_word_of_bits_is_judged_in_full() {
    let mut src = String::from("DEFINE VARIABLE valid AS LOGICAL NO-UNDO.\n");
    src.push_str("IF NOT valid THEN DO:\n    RETURN.\nEND.\nELSE DO:\n");
    for n in 0..150 {
        src.push_str(&format!("    DEFINE VARIABLE v{n} AS INTEGER NO-UNDO.\n"));
    }
    for n in 0..150 {
        // These three are assigned on some paths only.
        if matches!(n, 3 | 70 | 149) {
            src.push_str(&format!("    IF valid THEN v{n} = 1.\n"));
        } else {
            src.push_str(&format!("    v{n} = 1.\n"));
        }
    }
    src.push_str("END.\n");
    for n in 0..150 {
        src.push_str(&format!("MESSAGE v{n}.\n"));
    }
    assert_eq!(reported(&src), ["v149", "v3", "v70"]);
}

#[test]
fn each_unit_is_judged_in_its_own_graph() {
    let src = "\
PROCEDURE one:
    DEFINE INPUT PARAMETER valid AS LOGICAL NO-UNDO.
    IF NOT valid THEN DO:
        RETURN.
    END.
    ELSE DO:
        DEFINE VARIABLE a AS INTEGER NO-UNDO.
        a = 1.
    END.
    MESSAGE a.
END PROCEDURE.
PROCEDURE two:
    DEFINE INPUT PARAMETER valid AS LOGICAL NO-UNDO.
    IF valid THEN DO:
        DEFINE VARIABLE b AS INTEGER NO-UNDO.
        b = 1.
    END.
    MESSAGE b.
END PROCEDURE.
FUNCTION three RETURNS INTEGER (INPUT valid AS LOGICAL):
    IF NOT valid THEN DO:
        RETURN 0.
    END.
    ELSE DO:
        DEFINE VARIABLE c AS INTEGER NO-UNDO.
        c = 1.
    END.
    RETURN c.
END FUNCTION.
";
    assert_eq!(reported(src), ["b"]);
}

#[test]
fn a_read_in_another_unit_fires_whatever_the_blocks_around_it_prove() {
    // `a` is cleared by its guard clause in the main block; `b` is read by a
    // procedure, which can run before its block.
    let src = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
IF NOT valid THEN DO:
    RETURN.
END.
ELSE DO:
    DEFINE VARIABLE a AS INTEGER NO-UNDO.
    DEFINE VARIABLE b AS INTEGER NO-UNDO.
    a = 1.
    b = 1.
END.
MESSAGE a.
PROCEDURE show:
    MESSAGE b.
END PROCEDURE.
";
    assert_eq!(reported(src), ["b"]);
}

#[test]
fn a_trigger_body_is_its_own_unit() {
    let src = "\
DEFINE VARIABLE valid AS LOGICAL NO-UNDO.
ON CHOOSE OF b DO:
    IF NOT valid THEN DO:
        RETURN NO-APPLY.
    END.
    ELSE DO:
        DEFINE VARIABLE t AS INTEGER NO-UNDO.
        t = 1.
    END.
    MESSAGE t.
END.
";
    assert_eq!(reported(src), Vec::<String>::new());
}
