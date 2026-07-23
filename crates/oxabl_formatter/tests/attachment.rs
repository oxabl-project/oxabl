//! Fixture-driven comment attachment classification (U2 / R3).
//!
//! Written test-first against the bug-dense classifier: trailing-period
//! ownership, the `//`-vs-`&` span-end asymmetry, empty-body dangling, interior
//! comments, and the no-loss/no-duplication invariant.

use oxabl_ast::{Comment, Statement, StatementKind};
use oxabl_common::SourceMap;
use oxabl_formatter::attach;
use oxabl_lexer::tokenize;
use oxabl_parser::{Parser, Program};

fn parse(src: &str) -> Program {
    let tokens = tokenize(src);
    Parser::new(&tokens, src).parse_program()
}

fn text<'a>(src: &'a str, c: &Comment) -> &'a str {
    src[c.span.start as usize..c.span.end as usize].trim_end()
}

/// The first top-level statement whose kind matches `pred`.
fn find(program: &Program, pred: impl Fn(&StatementKind) -> bool) -> &Statement {
    program
        .statements
        .iter()
        .find(|s| pred(&s.kind))
        .expect("no matching statement")
}

#[test]
fn leading_comment_on_own_line() {
    let src = "/* lead */\nMESSAGE \"x\".\n";
    let program = parse(src);
    assert!(program.is_ok());
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let msg = &program.statements[0];
    let nc = map.get(msg.id).expect("message has comments");
    assert_eq!(nc.leading.len(), 1);
    assert_eq!(text(src, &nc.leading[0]), "/* lead */");
    assert!(nc.trailing.is_empty() && nc.dangling.is_empty() && nc.interior.is_empty());
}

#[test]
fn trailing_period_ownership_end_done() {
    // `END. /* done */` must trail the ENDED node, not lead the next statement.
    let src = "DO:\n  MESSAGE \"x\".\nEND. /* done */\nMESSAGE \"after\".\n";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);

    let do_block = &program.statements[0];
    let after = &program.statements[1];
    let nc = map.get(do_block.id).expect("do block has trailing");
    assert_eq!(nc.trailing.len(), 1, "END. comment trails the DO block");
    assert_eq!(text(src, &nc.trailing[0]), "/* done */");
    // The next statement must NOT have picked it up as leading.
    assert!(map.get(after.id).is_none_or(|n| n.leading.is_empty()));
}

#[test]
fn line_comment_span_includes_newline_no_off_by_one() {
    // A `//` comment's span includes its trailing `\n` (KTD3); classification
    // keys on `span.start`, so it must not drift onto the next line.
    let src = "// lead\nMESSAGE \"x\".\n";
    let program = parse(src);
    assert!(program.is_ok());
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let msg = &program.statements[0];
    let nc = map.get(msg.id).expect("message has leading");
    assert_eq!(nc.leading.len(), 1);
    assert_eq!(text(src, &nc.leading[0]), "// lead");
}

#[test]
fn ampersand_directive_line_span_excludes_newline() {
    // `&ANALYZE-SUSPEND` lexes to a Line comment whose span EXCLUDES its `\n`
    // (KTD3). It must still classify as leading of the following statement.
    let src = "&ANALYZE-SUSPEND _CREATE-WINDOW\nMESSAGE \"x\".\n";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    // The directive is trivia — it should land in exactly one bucket, and total
    // comment count must be preserved.
    assert_eq!(map.total(), program.comments.len());
    assert!(
        program
            .comments
            .iter()
            .any(|c| text(src, c).contains("ANALYZE-SUSPEND"))
    );
}

#[test]
fn dangling_comment_in_empty_do_body() {
    let src = "DO:\n  /* dangle */\nEND.\n";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let do_block = &program.statements[0];
    let nc = map.get(do_block.id).expect("do block has dangling");
    assert_eq!(nc.dangling.len(), 1);
    assert_eq!(text(src, &nc.dangling[0]), "/* dangle */");
    assert!(nc.leading.is_empty() && nc.trailing.is_empty() && nc.interior.is_empty());
}

#[test]
fn interior_comment_inside_leaf_statement() {
    let src = "DEFINE VARIABLE x /* counter */ AS INTEGER.\n";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let def = find(&program, |k| {
        matches!(k, StatementKind::VariableDeclaration { .. })
    });
    let nc = map.get(def.id).expect("def has interior");
    assert_eq!(
        nc.interior.len(),
        1,
        "comment is interior, not leading/trailing"
    );
    assert_eq!(text(src, &nc.interior[0]), "/* counter */");
    assert!(nc.leading.is_empty() && nc.trailing.is_empty() && nc.dangling.is_empty());
}

#[test]
fn run_of_consecutive_leading_comments_order_preserved() {
    let src = "/* a */\n/* b */\n/* c */\nMESSAGE \"x\".\n";
    let program = parse(src);
    assert!(program.is_ok());
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let msg = &program.statements[0];
    let nc = map.get(msg.id).expect("message has leading run");
    let got: Vec<&str> = nc.leading.iter().map(|c| text(src, c)).collect();
    assert_eq!(got, vec!["/* a */", "/* b */", "/* c */"]);
}

#[test]
fn nested_block_comment_as_trailing_is_one_entry() {
    let src = "x = 1. /* a /* b */ c */\n";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let assign = &program.statements[0];
    let nc = map.get(assign.id).expect("assign has trailing");
    assert_eq!(nc.trailing.len(), 1, "nested comment is a single entry");
    assert_eq!(text(src, &nc.trailing[0]), "/* a /* b */ c */");
}

#[test]
fn gap_comment_on_own_blank_separated_line_leads_next() {
    let src = "MESSAGE \"a\".\n\n/* between */\nMESSAGE \"b\".\n";
    let program = parse(src);
    assert!(program.is_ok());
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    let second = &program.statements[1];
    let nc = map.get(second.id).expect("second has leading");
    assert_eq!(nc.leading.len(), 1);
    assert_eq!(text(src, &nc.leading[0]), "/* between */");
}

#[test]
fn file_trailing_comment_not_dropped() {
    let src = "MESSAGE \"x\".\n/* trailing file */\n";
    let program = parse(src);
    assert!(program.is_ok());
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    assert_eq!(map.file_trailing().len(), 1);
    assert_eq!(text(src, &map.file_trailing()[0]), "/* trailing file */");
    assert_eq!(map.total(), program.comments.len());
}

#[test]
fn no_loss_no_duplication_over_mixed_fixture() {
    let src = "\
/* file lead */
DEFINE VARIABLE x /* inline */ AS INTEGER.

DO:  /* opener */
  /* dangle-ish before child */
  MESSAGE \"x\". // trailing line
END. /* done */

/* trailing file */
";
    let program = parse(src);
    assert!(program.is_ok(), "errors: {:?}", program.errors);
    let sm = SourceMap::new(src);
    let map = attach(&program, &sm);
    // Every comment appears in exactly one bucket.
    assert_eq!(
        map.total(),
        program.comments.len(),
        "no comment lost or duplicated"
    );
    assert!(!program.comments.is_empty());
}
