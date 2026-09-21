//! The OpenEdge metaschema under a loaded schema.
//!
//! `_file`, `_field` and the rest are in every OpenEdge database but in no
//! `.df` dump, so before the built-in catalog existed every reference to one
//! was a false `LINT0001` at *error* severity — enough for a single
//! introspection helper to fail a build for something the author could not
//! act on.
//!
//! What these pin, in order of how likely a later change is to break them:
//!
//! 1. **A metaschema reference produces nothing.** Bare, qualified, and
//!    database-qualified, in the statement and expression positions that
//!    reach the two rules.
//! 2. **A genuinely wrong field still reports.** The catalog buys real field
//!    checking, not blanket permissiveness; `_file._No-Such-Field` is still
//!    `LINT0003`. This is the property that separates "we modelled it" from
//!    "we suppressed it".
//! 3. **A user table of the same name wins.** The catalog merges *beneath*
//!    the `.df`, so a project that really did define `_file` gets its own
//!    fields and not the dictionary's.
//! 4. **Nothing changes without a schema.** The catalog is gated on a schema
//!    being loaded, so a project that never configured one sees exactly the
//!    answer it saw before.
//!
//! Every fixture here is synthetic.

use oxabl_ast::Statement;
use oxabl_common::{Diagnostic, FileId};
use oxabl_lexer::tokenize;
use oxabl_lint::{LINT0001, LINT0003, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::{Schema, test_support::schema_from_df};
use oxabl_semantic::{AnalysisContext, analyze_file};

// ---------------------------------------------------------------------------
// Harness
// ---------------------------------------------------------------------------

/// A minimal application schema. Nothing here mentions the metaschema — that
/// is the point: the catalog has to arrive on its own.
const APP_DF: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer
ADD FIELD "Name" OF "Customer" AS character
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

/// Lint `source` with `schema` loaded, the way a configured project runs.
fn lint_with(source: &str, schema: &Schema) -> Vec<Diagnostic> {
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, schema);
    ctx.schema_loaded = true;
    let sem = analyze_file(&stmts, &ctx);
    lint_file(&stmts, &sem, &ctx)
}

/// Lint `source` against the application schema plus the built-in catalog.
fn lint(source: &str) -> Vec<Diagnostic> {
    lint_with(source, &schema_from_df(APP_DF))
}

fn codes(diags: &[Diagnostic]) -> Vec<&'static str> {
    diags.iter().map(|d| d.code.0).collect()
}

fn assert_silent(source: &str) {
    let diags = lint(source);
    assert!(
        diags.is_empty(),
        "expected no findings for:\n{source}\ngot: {:#?}",
        diags
            .iter()
            .map(|d| format!("{}: {}", d.code.0, d.message))
            .collect::<Vec<_>>()
    );
}

// ---------------------------------------------------------------------------
// 1. A metaschema reference produces nothing
// ---------------------------------------------------------------------------

#[test]
fn a_bare_metaschema_table_name_is_not_an_undefined_symbol() {
    // `AVAILABLE _file` puts the table name in bare-identifier position,
    // which is where LINT0001 used to fire.
    assert_silent(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
IF AVAILABLE _file THEN
  MESSAGE "found".
"#,
    );
}

#[test]
fn a_qualified_metaschema_field_resolves() {
    assert_silent(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file._File-Name.
"#,
    );
}

#[test]
fn the_headline_dictionary_fields_resolve() {
    // The three the issue names, plus the join key that makes a real
    // metaschema query work.
    assert_silent(
        r#"FOR EACH _file NO-LOCK:
  MESSAGE _file._File-Name _file._File-Number.
END.
FOR EACH _field NO-LOCK:
  MESSAGE _field._Field-Name _field._Data-Type.
END.
FOR EACH _index NO-LOCK:
  MESSAGE _index._Index-Name _index._Unique.
END.
"#,
    );
}

#[test]
fn a_join_across_two_metaschema_tables_resolves() {
    assert_silent(
        r#"FOR EACH _file NO-LOCK WHERE _file._File-Number GT 0,
    EACH _field OF _file NO-LOCK:
  MESSAGE _file._File-Name _field._Field-Name.
END.
"#,
    );
}

#[test]
fn a_virtual_system_table_resolves() {
    // VSTs are not stored records, but ABL queries them exactly like `_file`
    // and they are present in every database, so they are in the catalog.
    assert_silent(
        r#"FOR EACH _connect NO-LOCK WHERE _connect._Connect-Usr GE 0:
  MESSAGE _connect._Connect-Name.
END.
"#,
    );
}

#[test]
fn field_name_abbreviation_works_on_metaschema_fields_too() {
    // ABL's unique-prefix rule is not suspended for the dictionary.
    assert_silent(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file._File-Nam.
"#,
    );
}

#[test]
fn the_issue_reproduction_is_clean() {
    // The exact shape reported: a `FIND` on `_file`, an `AVAILABLE` test, a
    // nested `FIND` on `_field` keyed by RECID, and field reads on both.
    assert_silent(
        r#"FIND _file NO-LOCK WHERE _file._File-Name EQ "Customer":U NO-ERROR.
IF AVAILABLE _file THEN
DO:
  FIND _field WHERE _field._File-recid EQ RECID(_file) NO-LOCK NO-ERROR.
  IF AVAILABLE _field THEN
    MESSAGE STRING(RECID(_field)).
END.
"#,
    );
}

// ---------------------------------------------------------------------------
// The `dictdb` alias, and database qualifiers generally
// ---------------------------------------------------------------------------

#[test]
fn a_database_qualified_metaschema_reference_resolves_in_expression_position() {
    // Three-part `database.table.field`. Before, this was two findings:
    // LINT0001 on `dictdb` and LINT0003 on `_file`.
    assert_silent("MESSAGE dictdb._file._File-Name.\n");
}

#[test]
fn a_database_qualified_metaschema_reference_resolves_in_a_record_phrase() {
    assert_silent(
        r#"FOR EACH dictdb._file NO-LOCK:
  MESSAGE _file._File-Name.
END.
"#,
    );
}

#[test]
fn a_database_qualifier_is_not_special_cased_to_dictdb() {
    // Any logical database name qualifies a table in ABL; oxabl models no
    // database dimension, so it accepts the qualifier rather than guessing.
    assert_silent("MESSAGE some-other-db._file._File-Name.\n");
    assert_silent("MESSAGE sports.Customer.Name.\n");
}

#[test]
fn a_database_qualified_reference_resolves_only_when_the_whole_chain_is_right() {
    // The database arm takes the expression only when the field is a real
    // field of the named table. That is what stops it from swallowing a
    // package-qualified type name (see the next test), and it means a
    // database-qualified reference to a field that does not exist falls back
    // to the older, noisier pair of findings rather than one precise
    // unknown-field finding.
    //
    // That is a deliberate trade: the code here is already wrong, so a worse
    // message on it is cheaper than a finding on code that is right. Pinned
    // because the alternative -- accepting the qualifier and reporting just
    // the field -- looks more appealing and is the thing not to "fix".
    let diags = lint("MESSAGE dictdb._file._No-Such-Field.\n");
    assert_eq!(codes(&diags), vec![LINT0001, LINT0003]);
    assert!(
        diags.iter().all(|d| !d.message.contains("_No-Such-Field")),
        "the chain is declined as a whole, so the field is not the subject"
    );
}

#[test]
fn a_bound_qualifier_keeps_its_meaning() {
    // `sports.Customer.Name` is exactly the database-qualified shape, and
    // `Customer` really is a table -- but here `sports` is a buffer in scope,
    // so the first segment is a symbol and the chain means "the `Customer`
    // field of buffer `sports`". There is no such field, so the correct
    // answer is LINT0003 on `Customer`.
    //
    // This is what pins the "is the first segment bound in scope?" guard:
    // without it the database arm would take the expression and resolve it
    // silently, and this assertion would see no findings at all.
    let diags = lint(
        r#"DEFINE BUFFER sports FOR Customer.
FIND FIRST sports NO-LOCK NO-ERROR.
MESSAGE sports.Customer.Name.
"#,
    );
    assert_eq!(
        codes(&diags),
        vec![LINT0003],
        "a chain rooted at a buffer in scope must stay a field access"
    );
}

#[test]
fn a_package_qualified_class_reference_is_not_a_database_reference() {
    // `acme.security.Auth` is a fully package-qualified static type. Here
    // `security` is a package segment -- but it is also, in this schema, a
    // table name. Reading the chain as `database.table.field` would mint a
    // buffer symbol for `security`, so this file would claim a schema
    // dependency on a table it never touches, and `Auth` would be reported
    // as an unknown field of it.
    //
    // The walk records an answer for each package segment before the database
    // arm is reached, which is what lets the arm decline.
    let schema = schema_from_df(
        r#"
ADD TABLE "security"
ADD FIELD "level" OF "security" AS integer
"#,
    );

    // As a method receiver. `acme` is reported here, exactly as it is on any
    // unresolved package path today -- that is pre-existing behaviour and not
    // what this test is about. What matters is that nothing claims `Auth` is
    // a field of the table `security`, and that no buffer over `security` is
    // synthesized, which would make this file declare a schema dependency on
    // a table it never reads.
    let diags = lint_with(
        "DEFINE VARIABLE n AS INTEGER NO-UNDO.\nn = acme.security.Auth:Level.\nMESSAGE n.\n",
        &schema,
    );
    assert!(
        diags.iter().all(|d| !d.message.contains("Auth")),
        "the type name must not be read as a field of table `security`: {diags:#?}"
    );

    // ...and outside receiver position, where there is no softening to hide
    // behind. The answer must be the same one this code got before the
    // database arm existed: the package path is unresolved, and `Auth` is not
    // reported at all.
    let diags = lint_with("MESSAGE acme.security.Auth.\n", &schema);
    assert!(
        diags.iter().all(|d| !d.message.contains("Auth")),
        "a package-qualified chain must not report its type name as an \
         unknown field: {diags:#?}"
    );
}

#[test]
fn an_unknown_field_of_a_metaschema_table_is_still_reported() {
    let diags = lint(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file._No-Such-Field.
"#,
    );
    assert_eq!(codes(&diags), vec![LINT0003]);
    assert!(
        diags[0].message.contains("_No-Such-Field"),
        "{}",
        diags[0].message
    );
}

#[test]
fn a_field_of_the_wrong_metaschema_table_is_reported() {
    // `_Field-Name` is real, but it belongs to `_field`, not `_file`. The
    // catalog has to be per-table for this to work.
    let diags = lint(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file._Field-Name.
"#,
    );
    assert_eq!(codes(&diags), vec![LINT0003]);
}

#[test]
fn an_invented_metaschema_table_is_still_undefined() {
    // The catalog is a list, not a prefix rule: a leading underscore does not
    // make a name known.
    let diags = lint(
        r#"FIND FIRST _not-a-real-table NO-LOCK NO-ERROR.
IF AVAILABLE _not-a-real-table THEN
  MESSAGE "x".
"#,
    );
    assert_eq!(codes(&diags), vec![LINT0001]);
}

#[test]
fn ordinary_application_code_is_unaffected() {
    assert_silent(
        r#"FOR EACH Customer NO-LOCK:
  MESSAGE Customer.CustNum Customer.Name.
END.
"#,
    );
    let diags = lint(
        r#"FOR EACH Customer NO-LOCK:
  MESSAGE Customer.NoSuchField.
END.
"#,
    );
    assert_eq!(codes(&diags), vec![LINT0003]);
}

// ---------------------------------------------------------------------------
// 3. A user table of the same name wins
// ---------------------------------------------------------------------------

#[test]
fn a_user_table_shadows_the_built_in_of_the_same_name() {
    // Pathological but legal. The `.df` is the authority on a table it
    // defines, so the dictionary's `_file` must not leak its fields in.
    let schema = schema_from_df(
        r#"
ADD TABLE "_file"
ADD FIELD "local-only" OF "_file" AS character
"#,
    );
    let diags = lint_with(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file.local-only.
"#,
        &schema,
    );
    assert!(diags.is_empty(), "{diags:#?}");

    // ...and the dictionary's own field is absent, because the user's table
    // replaced it rather than merging with it.
    let diags = lint_with(
        r#"FIND FIRST _file NO-LOCK NO-ERROR.
MESSAGE _file._File-Number.
"#,
        &schema,
    );
    assert_eq!(codes(&diags), vec![LINT0003]);
}

// ---------------------------------------------------------------------------
// 4. Nothing changes without a schema
// ---------------------------------------------------------------------------

#[test]
fn an_unloaded_schema_is_untouched_by_the_catalog() {
    // With no `.df` configured, table resolution is off entirely: LINT0003 is
    // not run at all and a bare table name is undefined whether it is
    // `_file` or `Customer`. Pinning this is what stops an always-present
    // catalog from quietly switching schema-dependent diagnostics on for
    // projects that never configured one.
    let schema = Schema::empty();
    assert!(schema.is_empty(), "an unloaded schema must stay empty");

    let source = r#"FIND FIRST _file NO-LOCK NO-ERROR.
IF AVAILABLE _file THEN
  MESSAGE _file._File-Name.
"#;
    let stmts = parse(source);
    let ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema);
    assert!(
        !ctx.schema_loaded,
        "an empty schema must not read as loaded"
    );
    let sem = analyze_file(&stmts, &ctx);
    let diags = lint_file(&stmts, &sem, &ctx);

    // Exactly the pre-existing no-schema answer: LINT0001 on the bare
    // `AVAILABLE` reference, and no LINT0003 at all.
    assert_eq!(codes(&diags), vec![LINT0001]);
}

#[test]
fn a_metaschema_table_is_not_a_user_table_for_emptiness_purposes() {
    // `Schema::is_empty` is read elsewhere as "no user schema configured".
    // The catalog must never be what makes a schema non-empty.
    assert!(Schema::empty().is_empty());
    assert!(!schema_from_df(APP_DF).is_empty());
}
