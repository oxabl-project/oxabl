//! Indentation regressions where a construct's own layout bled into the depth of
//! the lines around it: comments in blocks that end with `CATCH`/`FINALLY`, a
//! `FIELDS`/`EXCEPT` phrase in `FOR EACH`, a statement after `WHEN … THEN` or
//! `OTHERWISE`, and a leading file comment.
//!
//! Every case is written with `\t` marking one indent level and is checked at
//! indent sizes 3 and 4: already-formatted input is left alone, and the output
//! is a fixpoint.

use oxabl_formatter::format;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_style::StyleGuide;

const SIZES: [usize; 2] = [3, 4];

fn style(size: usize) -> StyleGuide {
    StyleGuide {
        indent_size: size,
        ..StyleGuide::default_base()
    }
}

fn run(src: &str, style: &StyleGuide) -> String {
    let tokens = tokenize(src);
    let program = Parser::new(&tokens, src).parse_program();
    assert!(program.is_ok(), "fixture must parse: {:?}", program.errors);
    format(src, &program, style).unwrap_or_else(|e| panic!("unexpected bail: {e}\n{src}"))
}

/// Expand `\t` level markers to `size` spaces.
fn expand(template: &str, size: usize) -> String {
    template.replace('\t', &" ".repeat(size))
}

/// `template` is already formatted: it must come back unchanged, and a pass
/// over the result must change nothing.
fn assert_stable(template: &str) {
    for size in SIZES {
        let style = style(size);
        let src = expand(template, size);
        let out = run(&src, &style);
        assert_eq!(out, src, "reformatted at indent {size}:\n{src}");
        assert_eq!(run(&out, &style), out, "not idempotent at indent {size}");
    }
}

/// `input` formats to `expected`, and the result is a fixpoint.
fn assert_formats(input: &str, expected: &str) {
    for size in SIZES {
        let style = style(size);
        let out = run(&expand(input, size), &style);
        assert_eq!(out, expand(expected, size), "wrong output at indent {size}");
        assert_eq!(run(&out, &style), out, "not idempotent at indent {size}");
    }
}

// --- comments in blocks that end with CATCH / FINALLY -----------------------

#[test]
fn comment_in_procedure_with_finally_keeps_statement_indent() {
    assert_stable(
        "PROCEDURE p:\n\tMESSAGE 1.\n\t/* c */\n\tMESSAGE 2.\n\tFINALLY:\n\t\tMESSAGE 3.\n\tEND FINALLY.\nEND PROCEDURE.\n",
    );
}

#[test]
fn comment_in_do_with_catch_keeps_statement_indent() {
    assert_stable(
        "DO ON ERROR UNDO, THROW:\n\tMESSAGE 1.\n\t/* c */\n\tMESSAGE 2.\n\tCATCH e AS Progress.Lang.Error:\n\t\tMESSAGE 3.\n\tEND CATCH.\nEND.\n",
    );
}

#[test]
fn comment_in_repeat_and_for_each_with_catch_keeps_statement_indent() {
    assert_stable(
        "REPEAT:\n\tMESSAGE 1.\n\t/* c */\n\tCATCH e AS Progress.Lang.Error:\n\t\tMESSAGE 3.\n\tEND CATCH.\nEND.\n",
    );
    assert_stable(
        "FOR EACH customer NO-LOCK:\n\tMESSAGE 1.\n\t/* c */\n\tMESSAGE 2.\n\tCATCH e AS Progress.Lang.Error:\n\t\tMESSAGE 3.\n\tEND CATCH.\nEND.\n",
    );
}

#[test]
fn comment_in_function_with_finally_keeps_statement_indent() {
    assert_stable(
        "FUNCTION f RETURNS INTEGER (INPUT piN AS INTEGER):\n\tMESSAGE 1.\n\t/* c */\n\tMESSAGE 2.\n\tFINALLY:\n\t\tMESSAGE 3.\n\tEND FINALLY.\n\tRETURN 1.\nEND FUNCTION.\n",
    );
}

#[test]
fn comment_in_method_with_finally_keeps_statement_indent() {
    assert_stable(
        "CLASS Widget:\n\tMETHOD PUBLIC VOID M(INPUT piN AS INTEGER):\n\t\tMESSAGE 1.\n\t\tIF piN EQ 1 THEN DO:\n\t\t\t/* nested */\n\t\t\tMESSAGE 2.\n\t\tEND.\n\t\t/* c */\n\t\tMESSAGE 3.\n\t\tFINALLY:\n\t\t\tMESSAGE 4.\n\t\tEND FINALLY.\n\tEND METHOD.\nEND CLASS.\n",
    );
}

#[test]
fn comments_inside_catch_and_finally_blocks_are_indented_with_their_statements() {
    assert_stable(
        "DO ON ERROR UNDO, THROW:\n\tMESSAGE 1.\n\tCATCH e AS Progress.Lang.Error:\n\t\t/* in catch */\n\t\tMESSAGE 2.\n\tEND CATCH.\n\tFINALLY:\n\t\t/* in finally */\n\t\tMESSAGE 3.\n\tEND FINALLY.\nEND.\n",
    );
}

#[test]
fn misplaced_comment_in_block_with_finally_is_reindented_to_its_statement() {
    assert_formats(
        "PROCEDURE p:\n\tMESSAGE 1.\n\t\t\t/* c */\n\n/* d */\n\tMESSAGE 2.\n\tFINALLY:\n\t\tMESSAGE 3.\n\tEND FINALLY.\nEND PROCEDURE.\n",
        "PROCEDURE p:\n\tMESSAGE 1.\n\t/* c */\n\n\t/* d */\n\tMESSAGE 2.\n\tFINALLY:\n\t\tMESSAGE 3.\n\tEND FINALLY.\nEND PROCEDURE.\n",
    );
}
