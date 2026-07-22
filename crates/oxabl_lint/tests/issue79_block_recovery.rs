//! Regression tests for oxabl#79.
//!
//! Root cause: parser error recovery only existed at the top level
//! (`parse_program`). A parse error inside a block body bubbled up through the
//! enclosing block's `parse_block_body()?`, discarding the whole block — its
//! header expressions (DO WHILE condition, FOR EACH WHERE, …) and every
//! already-parsed body statement included. The semantic usage pass therefore
//! never saw the header reads, producing LINT0002 unused-variable false
//! positives, and one deep error cascaded into phantom follow-on errors at
//! enclosing blocks.
//!
//! Fix: block bodies now recover per statement (record the error, synchronize
//! to the next statement boundary without crossing the block's END), so the
//! enclosing block survives with its header and successfully-parsed statements
//! intact. Errors are still reported in aggregate via `Program::errors`.

use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::unused_variable;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// Parse `source` with recovery, run the unused-variable lint, and return
/// (lint messages, number of parse errors reported).
fn unused(source: &str) -> (Vec<String>, usize) {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program();
    let nerr = program.errors.len();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    let sem = analyze_file(&program.statements, &ctx);
    let msgs = unused_variable::run(&program.statements, &sem, &ctx)
        .into_iter()
        .map(|d| d.message)
        .collect();
    (msgs, nerr)
}

/// Control: a clean, fully-terminated block parses with no errors and the
/// header read of `done` is counted (no false positive).
#[test]
fn control_clean_complete_block_no_fp() {
    let src = "def var done as log no-undo.\n\
               do while not done:\n\
                 done = true.\n\
               end.\n";
    let (msgs, nerr) = unused(src);
    assert_eq!(nerr, 0, "control should parse clean");
    assert!(
        !msgs.iter().any(|m| m.contains("done")),
        "control FP on `done`: {msgs:?}"
    );
}

/// The issue's exact shape: a DO WHILE header with no body and no END. The
/// missing-END parse error is recovered, the DO survives with its
/// while_condition, and the `lCheck` header read is counted.
#[test]
fn issue_snippet_incomplete_block_no_fp() {
    let src = "def var lCheck as log no-undo.\n\
               do while not lCheck on error undo, retry on endkey undo, retry:\n";
    let (msgs, nerr) = unused(src);
    assert!(
        nerr >= 1,
        "the unterminated block must still report an error"
    );
    assert!(
        !msgs.iter().any(|m| m.contains("lCheck")),
        "FP on `lCheck`: {msgs:?}"
    );
}

/// A parse error *inside* the loop body (a stray `else`) is recovered per
/// statement: the DO survives with its header and the well-formed
/// `done = true.` assignment, so the `done` header read is counted.
#[test]
fn body_error_block_no_fp() {
    let src = "def var done as log no-undo.\n\
               do while not done on error undo, retry on endkey undo, retry:\n\
                 else foo.\n\
                 done = true.\n\
               end.\n";
    let (msgs, nerr) = unused(src);
    assert!(nerr >= 1, "the in-body error must still be reported");
    assert!(
        !msgs.iter().any(|m| m.contains("done")),
        "FP on `done`: {msgs:?}"
    );
}

/// Cascade proof: one empty-subscript error nested two blocks deep must NOT
/// orphan the enclosing blocks. The outermost DO WHILE header read (`done`)
/// and the array/index reads (`id_`, `indx_`) inside the surviving blocks are
/// all counted, and at least one parse error is still reported.
/// Mirrors the ad100.i -> dbfcntl.i -> dbspdn.i shape from the issue.
#[test]
fn cascade_nested_error_does_not_orphan_outer_blocks() {
    let src = "\
def var done as log no-undo.\n\
def var id_ as int extent 5.\n\
def var indx_ as int no-undo.\n\
do while not done on error undo, retry on endkey undo, retry:\n\
  if indx_ = 1 then do:\n\
    message id_[indx_].\n\
  end.\n\
  else if indx_ = 2 then do:\n\
    if not available foo then do:\n\
      message id_[indx_].\n\
    end.\n\
    else do:\n\
      message id_[].\n\
    end.\n\
  end.\n\
  else message \"other\".\n\
  done = true.\n\
end.\n";
    let (msgs, nerr) = unused(src);
    // The empty-subscript `id_[]` is still a reported parse error, recovered.
    assert!(nerr >= 1, "the nested error must still be reported");
    // None of the outer-block reads may be orphaned into unused-variable FPs.
    for name in ["done", "id_", "indx_"] {
        assert!(
            !msgs.iter().any(|m| m.contains(name)),
            "orphaned outer-block read produced FP on `{name}`: {msgs:?}"
        );
    }
}
