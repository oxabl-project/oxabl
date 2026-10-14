//! `DEFINE WORKFILE` / `WORK-TABLE` declarations and the `ACCUM <aggregate> BY`
//! phrase, linted with a schema loaded. A run without a schema skips every
//! unresolved reference, so only the schema-loaded case proves anything.

use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::lint_file;
use oxabl_parser::Parser;
use oxabl_schema::test_support::schema_from_df;
use oxabl_semantic::{AnalysisContext, analyze_file};

const DF: &str = r#"
ADD TABLE "po-line"
ADD FIELD "po-number" OF "po-line" AS integer
ADD FIELD "cost" OF "po-line" AS decimal
"#;

fn lint(source: &str) -> Vec<Diagnostic> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = schema_from_df(DF);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    ctx.schema_loaded = true;
    let sem = analyze_file(&program.statements, &ctx);
    lint_file(&program.statements, &sem, &ctx)
}

fn assert_silent(source: &str) {
    let diags = lint(source);
    assert!(
        diags.is_empty(),
        "{source}\n{:#?}",
        diags
            .iter()
            .map(|d| format!("{}: {}", d.code.0, d.message))
            .collect::<Vec<_>>()
    );
}

#[test]
fn workfile_declares_table_and_fields() {
    for kw in ["WORKFILE", "WORK-TABLE"] {
        assert_silent(&format!(
            "DEFINE {kw} direct-ships\n  FIELD po-number LIKE po-line.po-number\n  FIELD cost LIKE po-line.cost.\n\
             CREATE direct-ships.\nASSIGN direct-ships.po-number = 1.\n\
             FIND FIRST direct-ships NO-ERROR.\nMESSAGE direct-ships.cost.\n"
        ));
    }
}

#[test]
fn accum_aggregate_by_phrase_is_not_an_identifier() {
    assert_silent(
        "DEFINE TEMP-TABLE tt-order NO-UNDO\n    FIELD cust AS CHARACTER\n    FIELD amount AS DECIMAL.\n\
         FOR EACH tt-order NO-LOCK BREAK BY tt-order.cust:\n\
         \x20   ACCUMULATE tt-order.amount (TOTAL BY tt-order.cust).\n\
         \x20   IF LAST-OF(tt-order.cust) THEN\n\
         \x20       DISPLAY ACCUM TOTAL BY tt-order.cust tt-order.amount.\n\
         END.\n",
    );
}

#[test]
fn put_reads_and_undefined_names() {
    let src = "DEFINE VARIABLE v AS INTEGER NO-UNDO.\nDEFINE STREAM s.\nv = 1.\nPUT STREAM s UNFORMATTED v SKIP.\n";
    assert_silent(src);
    let diags = lint("PUT UNFORMATTED nope SKIP.\n");
    assert!(
        diags.iter().any(|d| d.message.contains("nope")),
        "{diags:#?}"
    );
}

#[test]
fn stream_operand_functions_take_a_stream() {
    assert_silent(
        "DEFINE STREAM s.\nDEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
         n = PAGE-NUMBER(s) + LINE-COUNTER(s) + PAGE-SIZE(s) + SEEK(s).\nMESSAGE n.\n",
    );
    // A stream declared out of sight is left unjudged.
    assert_silent("DEFINE VARIABLE n AS INTEGER NO-UNDO.\nn = PAGE-NUMBER(unseen).\nMESSAGE n.\n");
}

#[test]
fn a_variable_before_a_parenthesised_item_is_not_a_call() {
    assert_silent(
        "DEFINE VARIABLE x AS INTEGER NO-UNDO.\nDEFINE VARIABLE d AS CHARACTER NO-UNDO.\n\
         x = 1.\nd = \",\".\n\
         PUT UNFORMATTED x (x + 1) d SKIP.\nMESSAGE x (x + 1) d.\nDISPLAY x (x + 1) d.\n",
    );
    let diags = lint("MESSAGE nope (1).\n");
    assert!(
        diags.iter().any(|d| d.message.contains("nope")),
        "{diags:#?}"
    );
}

#[test]
fn upper_and_lower_are_built_in() {
    assert_silent(
        "DEFINE VARIABLE s AS CHARACTER NO-UNDO.\ns = \"a\".\nMESSAGE UPPER(s) LOWER(s).\n",
    );
}
