//! Which record a field reference reads from.
//!
//! Two shapes, both driven through the full pipeline:
//!
//! 1. **A record phrase names a buffer, not a table.** `FOR EACH foo` over a
//!    `DEFINE BUFFER foo FOR bar` reads `bar`, even when a schema table is
//!    also called `foo`. The same holds for a temp-table that shares a schema
//!    table's name.
//! 2. **An unqualified field resolves against every record in scope.**
//!    Temp-tables and their buffers are candidates as well as schema buffers.
//!    The nearest scope wins, and two records at that scope with the field is
//!    ambiguous: the resolver does not pick one, and no rule reports it.
//!
//! Every fixture here is synthetic.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0003, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{
    AnalysisContext, PrimitiveTy, Resolution, ResolvedType, Semantic, SymbolKind, UnresolvedReason,
    analyze_file,
};

const DF: &str = r#"
ADD TABLE "foo"
ADD FIELD "alpha" OF "foo" AS character

ADD TABLE "bar"
ADD FIELD "gamma" OF "bar" AS character
"#;

fn parse(source: &str) -> Vec<Statement> {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(
        program.errors.is_empty(),
        "fixture must parse cleanly: {:?}",
        program.errors
    );
    program.statements
}

fn analyze(source: &str, schema: &Schema) -> (Semantic, Vec<Diagnostic>) {
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    ctx.schema_loaded = !schema.is_empty();
    let sem = analyze_file(&stmts, &ctx);
    let diags = lint_file(&stmts, &sem, &ctx);
    (sem, diags)
}

fn lint(source: &str) -> Vec<Diagnostic> {
    analyze(source, &schema_from_df(DF)).1
}

fn messages(diags: &[Diagnostic], code: &str) -> Vec<String> {
    diags
        .iter()
        .filter(|d| d.code.0 == code)
        .map(|d| d.message.clone())
        .collect()
}

/// Every resolution recorded for a reference spelled `name`, resolved or not.
fn resolutions<'s>(sem: &'s Semantic, name: &str) -> Vec<&'s Resolution> {
    sem.references
        .iter()
        .map(|(_, r)| r)
        .filter(|r| match r {
            Resolution::Resolved(sym) => sem.symbols.get(*sym).name.as_ref() == name,
            Resolution::Unresolved { name: n, .. } => n.as_ref() == name,
        })
        .collect()
}

/// The declared type of the field every reference spelled `name` resolved to.
fn field_types(sem: &Semantic, name: &str) -> Vec<Option<ResolvedType>> {
    resolutions(sem, name)
        .into_iter()
        .map(|r| match r {
            Resolution::Resolved(sym) => {
                let s = sem.symbols.get(*sym);
                assert_eq!(s.kind, SymbolKind::Field, "`{name}` must bind a field");
                s.data_type.clone()
            }
            other => panic!("`{name}` did not resolve: {other:?}"),
        })
        .collect()
}

// ---------------------------------------------------------------------------
// A record phrase names a buffer
// ---------------------------------------------------------------------------

#[test]
fn for_each_over_a_buffer_named_like_another_table_reads_its_own_table() {
    let diags = lint(
        "DEFINE BUFFER foo FOR bar.
FOR EACH foo NO-LOCK:
    MESSAGE foo.gamma.
    MESSAGE foo.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_first_over_a_buffer_named_like_another_table_reads_its_own_table() {
    let diags = lint(
        "DEFINE BUFFER foo FOR bar.
FOR FIRST foo NO-LOCK:
    MESSAGE foo.gamma.
    MESSAGE foo.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_each_over_a_buffer_with_its_own_name_still_checks_its_fields() {
    let diags = lint(
        "DEFINE BUFFER b2 FOR bar.
FOR EACH b2 NO-LOCK:
    MESSAGE b2.gamma.
    MESSAGE b2.alpha.
END.
",
    );
    assert_eq!(
        messages(&diags, LINT0003),
        vec!["unknown field `alpha`".to_string()]
    );
}

#[test]
fn for_each_over_a_temp_table_named_like_a_schema_table_reads_the_temp_table() {
    let diags = lint(
        "DEFINE TEMP-TABLE foo NO-UNDO FIELD delta AS CHARACTER.
FOR EACH foo NO-LOCK:
    MESSAGE foo.delta.
    MESSAGE delta.
END.
",
    );
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
}

#[test]
fn for_each_over_a_temp_table_credits_the_temp_table() {
    let (sem, _) = analyze(
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS INTEGER.
FOR EACH tt NO-LOCK:
END.
",
        &Schema::empty(),
    );
    let tt = sem
        .symbols
        .iter()
        .filter(|(_, s)| s.name.as_ref() == "tt")
        .map(|(_, s)| s)
        .collect::<Vec<_>>();
    assert_eq!(tt.len(), 1, "FOR EACH must not mint a second `tt`");
    assert_eq!(tt[0].kind, SymbolKind::TempTable);
    assert_eq!(tt[0].read_count, 1);
}

// ---------------------------------------------------------------------------
// Unqualified fields
// ---------------------------------------------------------------------------

#[test]
fn unqualified_temp_table_field_after_find_resolves() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD f-date AS DATE.
DEFINE VARIABLE d AS DATE NO-UNDO.
FIND FIRST tt NO-LOCK NO-ERROR.
d = f-date.
MESSAGE d.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "f-date"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Date))]
    );
}

#[test]
fn unqualified_temp_table_field_resolves_without_a_schema() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD f-date AS DATE.
DEFINE VARIABLE d AS DATE NO-UNDO.
FOR EACH tt:
    d = f-date.
END.
MESSAGE d.
";
    let (_, diags) = analyze(src, &Schema::empty());
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
}

#[test]
fn unqualified_field_of_a_table_parameter_resolves() {
    let src = "DEFINE TEMP-TABLE t-misc NO-UNDO FIELD t-date AS DATE.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR t-misc.
DEFINE VARIABLE d AS DATE NO-UNDO.
FIND FIRST t-misc NO-ERROR.
d = t-date.
MESSAGE d.
";
    let diags = lint(src);
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
}

#[test]
fn unqualified_field_through_a_buffer_for_a_temp_table_resolves() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD qty AS INTEGER.
PROCEDURE p:
    DEFINE BUFFER b FOR tt.
    FOR EACH b:
        MESSAGE qty.
    END.
END PROCEDURE.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "qty"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Integer))]
    );
}

#[test]
fn unqualified_field_copied_by_like_resolves() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO LIKE bar.
DEFINE TEMP-TABLE tt2 NO-UNDO LIKE tt FIELD extra AS INTEGER.
FOR EACH tt2:
    MESSAGE gamma extra.
END.
";
    let diags = lint(src);
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
}

#[test]
fn unqualified_name_no_record_has_is_still_undefined() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD f-date AS DATE.
FOR EACH tt:
    MESSAGE f-daet.
END.
";
    assert_eq!(
        messages(&lint(src), LINT0001),
        vec!["undefined symbol `f-daet`".to_string()]
    );
}

#[test]
fn the_record_phrase_buffer_beats_an_outer_record() {
    let src = "DEFINE TEMP-TABLE t1 NO-UNDO FIELD code AS INTEGER.
DEFINE TEMP-TABLE t2 NO-UNDO FIELD code AS CHARACTER.
FOR EACH t2:
    MESSAGE code.
END.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "code"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Character))]
    );
}

#[test]
fn a_temp_table_and_a_schema_buffer_both_with_the_field_are_ambiguous() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD gamma AS INTEGER.
DEFINE BUFFER b FOR bar.
FIND FIRST tt NO-ERROR.
MESSAGE gamma.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert!(
        matches!(
            resolutions(&sem, "gamma").as_slice(),
            [Resolution::Unresolved {
                reason: UnresolvedReason::PresentButUnusable,
                ..
            }]
        ),
        "an ambiguous field must not bind either record"
    );
}

#[test]
fn two_buffers_for_one_table_are_not_ambiguous() {
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD qty AS INTEGER.
DEFINE BUFFER b1 FOR tt.
DEFINE BUFFER b2 FOR TEMP-TABLE tt.
MESSAGE qty.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "qty"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Integer))]
    );
}

#[test]
fn a_buffer_for_an_unknown_table_blocks_an_outer_answer() {
    // `nosuch` is not in the schema, so it may have `qty` itself: the outer
    // temp-table field must not be picked, and nothing is reported.
    let src = "DEFINE TEMP-TABLE tt NO-UNDO FIELD qty AS INTEGER.
FOR EACH nosuch:
    MESSAGE qty.
END.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(
        !messages(&diags, LINT0001).contains(&"undefined symbol `qty`".to_string()),
        "got: {diags:#?}"
    );
    assert!(
        matches!(
            resolutions(&sem, "qty").as_slice(),
            [Resolution::Unresolved {
                reason: UnresolvedReason::External,
                ..
            }]
        ),
        "a field an unknown table may own must not bind the temp-table"
    );
}

// ---------------------------------------------------------------------------
// Unqualified fields of a named buffer
// ---------------------------------------------------------------------------

#[test]
fn unqualified_field_of_a_buffer_for_a_database_qualified_table_resolves() {
    let src = "DEFINE BUFFER b FOR sports.foo.
FOR EACH b NO-LOCK:
    EXPORT alpha.
    DISPLAY alpha.
END.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "alpha"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Character)); 2]
    );
}

#[test]
fn unqualified_field_of_a_named_buffer_resolves_outside_a_record_block() {
    let src = "DEFINE BUFFER b FOR sports.bar.
FIND FIRST b NO-LOCK NO-ERROR.
DISPLAY gamma.
";
    let (sem, diags) = analyze(src, &schema_from_df(DF));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert_eq!(
        field_types(&sem, "gamma"),
        vec![Some(ResolvedType::Primitive(PrimitiveTy::Character))]
    );
}

#[test]
fn a_field_two_named_buffers_have_is_ambiguous() {
    let df = r#"
ADD TABLE "foo"
ADD FIELD "code" OF "foo" AS character

ADD TABLE "bar"
ADD FIELD "code" OF "bar" AS integer
"#;
    let src = "DEFINE BUFFER b1 FOR foo.
DEFINE BUFFER b2 FOR bar.
DISPLAY code.
";
    let (sem, diags) = analyze(src, &schema_from_df(df));
    assert!(diags.is_empty(), "expected no findings, got: {diags:#?}");
    assert!(
        matches!(
            resolutions(&sem, "code").as_slice(),
            [Resolution::Unresolved {
                reason: UnresolvedReason::PresentButUnusable,
                ..
            }]
        ),
        "an ambiguous field must not bind either buffer"
    );
}
