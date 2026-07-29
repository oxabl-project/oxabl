//! Rule-by-rule layout fixtures (U4/U5/U6): input → expected under a resolved
//! `StyleGuide`. Drives the printer, keyword transforms, and blank-line
//! normalization through the public `format()` API.

use oxabl_formatter::format;
use oxabl_lexer::tokenize;
use oxabl_parser::{Parser, Program};
use oxabl_style::{KeywordAbbreviation, KeywordCase, StyleGuide};

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

/// Format `src` under `style`, asserting it did not bail.
fn fmt(src: &str, style: &StyleGuide) -> String {
    let program = parse(src);
    assert!(
        program.is_ok(),
        "fixture failed to parse: {:?}",
        program.errors
    );
    format(src, &program, style).unwrap_or_else(|e| panic!("unexpected bail: {e}"))
}

// ---------------------------------------------------------------------------
// U4 — structural indentation
// ---------------------------------------------------------------------------

#[test]
fn reindents_misindented_do_block() {
    let src = "DO:\n        MESSAGE \"x\".\n  END.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "DO:\n    MESSAGE \"x\".\nEND.\n");
}

#[test]
fn reindents_nested_blocks() {
    let src = "DO:\nDO:\nMESSAGE \"x\".\nEND.\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "DO:\n    DO:\n        MESSAGE \"x\".\n    END.\nEND.\n"
    );
}

#[test]
fn if_then_do_is_one_indent_level_not_two() {
    // `IF … THEN DO:` is a single opener: the DO block supplies the indentation
    // level, so its body is one level deep (4), not two (8). Regression: the
    // prefix `IF` used to add a second level per wrapper.
    let src = "IF x THEN DO:\nMESSAGE \"a\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "IF x THEN DO:\n    MESSAGE \"a\".\nEND.\n");
}

#[test]
fn nested_if_then_do_indents_by_one_level_each() {
    let src = "IF x > 1 THEN DO:\nIF y > 2 THEN DO:\nMESSAGE \"deep\".\nEND.\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF x > 1 THEN DO:\n    IF y > 2 THEN DO:\n        MESSAGE \"deep\".\n    END.\nEND.\n"
    );
}

#[test]
fn if_else_do_branches_share_the_if_level() {
    let src = "IF x THEN DO:\nMESSAGE \"t\".\nEND.\nELSE DO:\nMESSAGE \"e\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF x THEN DO:\n    MESSAGE \"t\".\nEND.\nELSE DO:\n    MESSAGE \"e\".\nEND.\n"
    );
}

#[test]
fn then_nested_bare_if_still_indents() {
    // A THEN-position nested `IF` (no DO) has no block to borrow a level from, so
    // it must still indent one level per nesting. Regression guard for the
    // over-broad "child has children ⇒ delta 0" predicate.
    let src = "IF a > 1 THEN\nIF b > 2 THEN\nMESSAGE \"x\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF a > 1 THEN\n    IF b > 2 THEN\n        MESSAGE \"x\".\n"
    );
}

#[test]
fn else_if_chain_stays_flush_with_opening_if() {
    // An else-position `IF` (else-if chain) borrows the opening IF's level rather
    // than stair-stepping deeper on each `ELSE IF`.
    let src = "IF a THEN DO:\nMESSAGE \"t\".\nEND.\nELSE IF b THEN DO:\nMESSAGE \"e\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF a THEN DO:\n    MESSAGE \"t\".\nEND.\nELSE IF b THEN DO:\n    MESSAGE \"e\".\nEND.\n"
    );
}

#[test]
fn if_then_leaf_branch_on_own_line_still_indents_one_level() {
    // The case the fix must preserve: a non-block THEN branch on its own line has
    // no DO to supply the level, so it keeps the +1.
    let src = "IF x THEN\nMESSAGE \"a\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "IF x THEN\n    MESSAGE \"a\".\n");
}

#[test]
fn wrapped_multiline_then_branch_keeps_continuation_indent() {
    // Issue #98: a wrapped multi-line non-block THEN branch (a multi-line
    // ASSIGN) must keep every continuation line at the branch body's depth. The
    // last physical line used to be snapped to the IF depth (column 0) because
    // the `IF` prefix wrapper pushed a spurious `block_ends` closer entry.
    let src =
        "IF AVAILABLE bar THEN\nASSIGN\nbar.qty = bar.qty + 1\nbar.total =\nbar.total + bar.qty.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF AVAILABLE bar THEN\n    ASSIGN\n    bar.qty = bar.qty + 1\n    bar.total =\n    bar.total + bar.qty.\n"
    );
}

#[test]
fn wrapped_multiline_else_branch_keeps_continuation_indent() {
    // Parity with the THEN case: a wrapped multi-line non-block ELSE branch keeps
    // its continuation indent too.
    let src = "IF AVAILABLE bar THEN MESSAGE \"t\".\nELSE\nASSIGN\nbar.qty = bar.qty + 1\nbar.total =\nbar.total + bar.qty.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF AVAILABLE bar THEN MESSAGE \"t\".\nELSE\n    ASSIGN\n    bar.qty = bar.qty + 1\n    bar.total =\n    bar.total + bar.qty.\n"
    );
}

#[test]
fn labeled_block_does_not_double_indent() {
    // A block label is a prefix, not its own level: the labeled `DO:` sits at the
    // label's depth and its body one level deeper.
    let src = "lbl:\nDO:\nMESSAGE \"a\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "lbl:\nDO:\n    MESSAGE \"a\".\nEND.\n");
}

#[test]
fn trailing_comment_on_block_opener_does_not_reindent_that_line() {
    // A trailing `/* … */` on a block-opener line must not drag the opener to the
    // body's depth. Attachment can hand the comment back as a *leading* comment
    // of the body's first statement; the printer must let the statement that
    // starts on the line own its indent. Reproduced from a real file where a
    // preceding leaf-`THEN` IF made the misattachment fire.
    let src = "IF c EQ \"no\" THEN\nMESSAGE \"n\".\n\n        IF c BEGINS \"hi\" THEN DO: /* check */\nMESSAGE \"x\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "IF c EQ \"no\" THEN\n    MESSAGE \"n\".\n\nIF c BEGINS \"hi\" THEN DO: /* check */\n    MESSAGE \"x\".\nEND.\n"
    );
}

#[test]
fn do_placement_sameline_default_preserves_conforming_block() {
    // do_placement defaults to SameLine; a conforming `DO:` stays put.
    let src = "DO:\n    MESSAGE \"x\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, src);
}

#[test]
fn period_placement_sameline_keeps_period_on_last_line() {
    let src = "MESSAGE \"x\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "MESSAGE \"x\".\n");
}

// ---------------------------------------------------------------------------
// U4 — end_with_type
// ---------------------------------------------------------------------------

#[test]
fn end_with_type_false_preserves_bare_end() {
    let src = "PROCEDURE foo:\n    MESSAGE \"x\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, src, "default_base must not touch bare END");
}

#[test]
fn end_with_type_true_inserts_type_keyword() {
    let src = "PROCEDURE foo:\n    MESSAGE \"x\".\nEND.\n";
    let mut style = StyleGuide::default_base();
    style.end_with_type = true;
    let out = fmt(src, &style);
    assert_eq!(out, "PROCEDURE foo:\n    MESSAGE \"x\".\nEND PROCEDURE.\n");
}

#[test]
fn end_with_type_true_idempotent_on_already_typed() {
    let src = "PROCEDURE foo:\n    MESSAGE \"x\".\nEND PROCEDURE.\n";
    let mut style = StyleGuide::default_base();
    style.end_with_type = true;
    let out = fmt(src, &style);
    assert_eq!(out, src);
}

// ---------------------------------------------------------------------------
// U4 — verbatim fidelity (R5.2, the anti-mangle guarantee)
// ---------------------------------------------------------------------------

#[test]
fn verbatim_identifier_casing_and_tilde_string() {
    // An unusually-cased identifier and a tilde-escaped string must survive
    // byte-for-byte under the safe default.
    let src = "ASSIGN My-Var = \"a~\"b\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, src);
}

#[test]
fn reflow_no_op_leaves_long_line_untouched() {
    // default_base ships wrap_long_lines:true / max_line_length:120, but v1 does
    // not enforce reflow (R4.4). A 200-column line stays 200 columns.
    let long = "x".repeat(190);
    let src = format!("MESSAGE \"{long}\".\n");
    let out = fmt(&src, &StyleGuide::default_base());
    assert_eq!(out, src);
}

#[test]
fn include_and_preproc_refs_format_as_ordinary_constructs() {
    // {include}, &SCOPED-DEFINE, and {&macro} are AST nodes and format
    // unexpanded (R8.1) — here, just structurally reindented.
    let src = "&SCOPED-DEFINE foo bar\nMESSAGE \"{&foo}\".\n{lib/util.i}\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, src);
}

// ---------------------------------------------------------------------------
// U4 — comments land at the right place
// ---------------------------------------------------------------------------

#[test]
fn all_four_comment_kinds_round_trip() {
    let src = "\
/* lead */
DO:
    DEFINE VARIABLE x /* interior */ AS INTEGER.
    MESSAGE \"x\". /* trailing */
END.
";
    let out = fmt(src, &StyleGuide::default_base());
    // Leading stays on its own line above DO, interior rides inside the DEFINE
    // line, trailing rides after MESSAGE, and everything reindents structurally.
    assert_eq!(out, src);
}

#[test]
fn dangling_comment_reindents_to_body_depth() {
    let src = "DO:\n/* dangle */\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "DO:\n    /* dangle */\nEND.\n");
}

// ---------------------------------------------------------------------------
// U5 — keyword recasing / abbreviation (opt-in)
// ---------------------------------------------------------------------------

#[test]
fn default_base_preserves_keyword_spelling() {
    let src = "def var i as int.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, src, "safe default must not recase or expand keywords");
}

#[test]
fn keyword_case_uppercase_recases_keywords_only() {
    let src = "define variable myVar as integer.\n";
    let mut style = StyleGuide::default_base();
    style.keyword_case = KeywordCase::Uppercase;
    let out = fmt(src, &style);
    assert_eq!(out, "DEFINE VARIABLE myVar AS INTEGER.\n");
}

#[test]
fn keyword_abbreviation_expands_to_full_form() {
    let src = "def var i as int.\n";
    let mut style = StyleGuide::default_base();
    style.keyword_case = KeywordCase::Uppercase;
    style.keyword_abbreviation = KeywordAbbreviation::AbbreviateNothing;
    let out = fmt(src, &style);
    assert_eq!(out, "DEFINE VARIABLE i AS INTEGER.\n");
}

// ---------------------------------------------------------------------------
// U6 — blank-line normalization
// ---------------------------------------------------------------------------

#[test]
fn collapses_consecutive_blank_lines_to_cap() {
    let src = "MESSAGE \"a\".\n\n\n\nMESSAGE \"b\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "MESSAGE \"a\".\n\nMESSAGE \"b\".\n");
}

#[test]
fn honors_max_consecutive_blank_lines_two() {
    let src = "MESSAGE \"a\".\n\n\n\nMESSAGE \"b\".\n";
    let mut style = StyleGuide::default_base();
    style.max_consecutive_blank_lines = 2;
    let out = fmt(src, &style);
    assert_eq!(out, "MESSAGE \"a\".\n\n\nMESSAGE \"b\".\n");
}

#[test]
fn drops_blank_after_opener_and_before_end() {
    let src = "DO:\n\n    MESSAGE \"x\".\n\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "DO:\n    MESSAGE \"x\".\nEND.\n");
}

#[test]
fn drops_blank_after_opener_with_trailing_comment() {
    // The opener's last *code* token is `:` even when a trailing comment follows
    // (`DO: /* c */`), so the after-opener blank run is still dropped. Regression:
    // a naive `ends_with(':')` saw `*/` and kept a spurious blank.
    let src = "DO: /* c */\n\n\n    MESSAGE \"x\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "DO: /* c */\n    MESSAGE \"x\".\nEND.\n");
}

#[test]
fn trims_leading_file_blanks() {
    let src = "\n\nMESSAGE \"x\".\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "MESSAGE \"x\".\n");
}

#[test]
fn normalizes_trailing_newlines_to_one() {
    let src = "MESSAGE \"x\".\n\n\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "MESSAGE \"x\".\n");
    let no_trailing = "MESSAGE \"x\".";
    assert_eq!(
        fmt(no_trailing, &StyleGuide::default_base()),
        "MESSAGE \"x\".\n"
    );
}

#[test]
fn crlf_source_stays_crlf() {
    let src = "MESSAGE \"a\".\r\n\r\n\r\n\r\nMESSAGE \"b\".\r\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "MESSAGE \"a\".\r\n\r\nMESSAGE \"b\".\r\n");
}

#[test]
fn comment_style_not_applied_v2_deferral() {
    // comment_style is a v2 content rewrite; a `//` comment must stay `//`.
    let src = "// keep me\nMESSAGE \"x\".\n";
    let mut style = StyleGuide::default_base();
    style.comment_style = oxabl_style::CommentStyle::BlockComment;
    let out = fmt(src, &style);
    assert!(
        out.contains("// keep me"),
        "comment style must not be rewritten"
    );
}

#[test]
fn leading_comment_above_nested_if_else_do_stays_at_column_zero() {
    // Regression: a nested `IF … THEN <block> ELSE DO: … END.` used to give its
    // DO-block branches a dummy 0..0 span (parser bug), which the printer mapped
    // to line 0. With a file-leading comment on line 0, that pulled the comment's
    // first line to the branch's depth. The banner must stay at column 0.
    let src = "\
/* banner */
PROCEDURE p:
IF x THEN
DO:
MESSAGE \"t\".
END.
ELSE
DO:
MESSAGE \"e\".
END.
END PROCEDURE.
";
    let out = fmt(src, &StyleGuide::default_base());
    let first = out.lines().next().unwrap();
    assert_eq!(
        first, "/* banner */",
        "leading banner comment must not be indented; got {first:?}\nfull:\n{out}"
    );
}
// ---------------------------------------------------------------------------
// #95 — lines that begin inside a multi-line token are left verbatim
// ---------------------------------------------------------------------------

#[test]
fn multiline_string_reindents_opener_but_leaves_interior_verbatim() {
    // The block body is under-indented, so the reindent path fires. The opener
    // line snaps to depth 4; the string literal's second physical line begins
    // inside the token, so its leading whitespace is left untouched and the
    // string's bytes are preserved (no guard trip). Regression for #95.
    let src = "PROCEDURE p:\nmsg = \"first line\nsecond line\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "PROCEDURE p:\n    msg = \"first line\nsecond line\".\nEND.\n"
    );
}

#[test]
fn multiline_include_reference_leaves_continuation_verbatim() {
    // An `{include}` reference whose `&args` wrap across lines: the opener line
    // reindents, the continuation begins inside the include token and is emitted
    // verbatim so the reference text is preserved byte-for-byte. Regression #95.
    let src = "PROCEDURE p:\n{shared/report.i &event = \"start\"\n&mode = \"batch\"}\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        "PROCEDURE p:\n    {shared/report.i &event = \"start\"\n&mode = \"batch\"}\nEND.\n"
    );
}

#[test]
fn blank_lines_inside_multiline_string_survive_normalization() {
    // Two consecutive blank physical lines live inside the string literal. With
    // max_consecutive_blank_lines = 1, the blank-normalization pass would clamp
    // them to one — changing the string's value and tripping the guard. They are
    // protected (interior of a multi-line token), so both survive. Regression #95.
    let src = "DO:\nmsg = \"line one\n\n\nline four\".\nEND.\n";
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(out, "DO:\n    msg = \"line one\n\n\nline four\".\nEND.\n");
}

// ---------------------------------------------------------------------------
// Unmodelled statement forms (#128)
// ---------------------------------------------------------------------------

/// `PUT` / `UPDATE` / `ENABLE` are recognized-but-unmodelled forms: the parser
/// skips their tokens and emits `StatementKind::Skipped`. `tree.rs` never named
/// `StatementKind::Empty` explicitly, so those nodes have always fallen through
/// its `_` arms as ordinary non-block statements — and `Skipped` inherits that
/// treatment by construction, not by luck. This pins it: introducing the variant
/// must not move a byte of formatter output, including inside a block where the
/// child-indentation arm is the one doing the work.
#[test]
fn unmodelled_forms_format_unchanged() {
    let src = concat!(
        "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n",
        "DEFINE VARIABLE v-name AS CHARACTER NO-UNDO.\n",
        "\n",
        "DO:\n",
        "PUT UNFORMATTED v-total SKIP.\n",
        "ENABLE v-name WITH FRAME f-main.\n",
        "UPDATE v-name WITH FRAME f-main.\n",
        "END.\n",
    );
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        concat!(
            "DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n",
            "DEFINE VARIABLE v-name AS CHARACTER NO-UNDO.\n",
            "\n",
            "DO:\n",
            "    PUT UNFORMATTED v-total SKIP.\n",
            "    ENABLE v-name WITH FRAME f-main.\n",
            "    UPDATE v-name WITH FRAME f-main.\n",
            "END.\n",
        )
    );
}

/// The three #130 forms carry a table-candidate marker and, in `EMPTY
/// TEMP-TABLE`'s case, changed node kind entirely — it used to emit a recovery
/// `Empty`. Neither is a formatting concern: all three still pass through
/// byte-for-byte, at top level and indented inside a block.
#[test]
fn table_candidate_forms_format_unchanged() {
    let src = concat!(
        "DEFINE QUERY q-item FOR ttItem.\n",
        "EMPTY TEMP-TABLE ttItem NO-ERROR.\n",
        "\n",
        "DO:\n",
        "DEFINE QUERY q-inner FOR ttItem.\n",
        "OPEN QUERY q-inner FOR EACH ttItem WHERE ttItem.qty > 0.\n",
        "EMPTY TEMP-TABLE ttItem.\n",
        "END.\n",
    );
    let out = fmt(src, &StyleGuide::default_base());
    assert_eq!(
        out,
        concat!(
            "DEFINE QUERY q-item FOR ttItem.\n",
            "EMPTY TEMP-TABLE ttItem NO-ERROR.\n",
            "\n",
            "DO:\n",
            "    DEFINE QUERY q-inner FOR ttItem.\n",
            "    OPEN QUERY q-inner FOR EACH ttItem WHERE ttItem.qty > 0.\n",
            "    EMPTY TEMP-TABLE ttItem.\n",
            "END.\n",
        )
    );
}
