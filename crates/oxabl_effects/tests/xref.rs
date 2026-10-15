//! The XREF parser and comparison, over synthetic XREF text written from the
//! remembered format (`docs/design/strict-analysis-open-questions.md`,
//! section 8.1): no compiler produced these files.

use std::path::Path;

use oxabl_effects::xref::{self, ModelSearch};
use oxabl_effects::{Access, EffectOptions};
use oxabl_index::calls::FileCalls;
use oxabl_scope::ScopeOptions;

fn fixture(name: &str) -> String {
    std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/xref")
            .join(name),
    )
    .expect("fixture")
}

#[test]
fn rows_are_read_leniently() {
    let rows = xref::parse(&fixture("search.xref"));
    assert_eq!(rows.len(), 14, "the garbage line is skipped");
    let run = rows.iter().find(|r| r.kind == "RUN").expect("a RUN row");
    assert_eq!(run.source, "my dir/search.p");
    assert_eq!(run.details, ["other prog.p"]);
    let searches = xref::searches(&rows);
    assert_eq!(searches.len(), 8);
    assert!(searches[1].whole);
    assert_eq!(searches[1].index.as_deref(), Some("CustNum"));
    assert!(searches[5].table_scan);
    assert_eq!(searches[5].index, None);
}

#[test]
fn files_match_by_suffix() {
    assert!(xref::same_file("src/a/b.p", "a/b.p"));
    assert!(xref::same_file("C:\\src\\A\\B.p", "a/b.p"));
    assert!(xref::same_file("./a/b.p", "a/b.p"));
    assert!(!xref::same_file("src/xa/b.p", "a/b.p"));
}

/// The model's searches of `xref/search.p`, placed on their statement lines.
fn model() -> Vec<ModelSearch> {
    let source = fixture("search.p");
    let df = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/schema.df"),
    )
    .expect("schema");
    let schema = oxabl_schema::test_support::schema_from_df(&df);
    let tokens = oxabl_lexer::tokenize(&source);
    let program = oxabl_parser::Parser::new(&tokens, &source).parse_program();
    assert!(program.errors.is_empty());
    let options = ScopeOptions::default();
    let flow = oxabl_flow::build_file_with(&program.statements, &source, &options.flow);
    let scopes = oxabl_scope::analyze_flow(
        &program.statements,
        &source,
        &tokens,
        &flow,
        Some(&schema),
        &options,
    );
    let calls = FileCalls::extract(&program.statements, &source, &tokens);
    let effects = oxabl_effects::analyze_file(
        &program.statements,
        &source,
        &tokens,
        &flow,
        &scopes,
        &calls,
        Some(&schema),
        &EffectOptions::default(),
    );
    effects
        .effects()
        .iter()
        .filter_map(|e| {
            let access = e.access.clone()?;
            let line = source[..e.statement.start as usize].matches('\n').count() as u32 + 1;
            Some(ModelSearch {
                file: "search.p".to_string(),
                line,
                table: e.table.name().to_string(),
                access,
            })
        })
        .collect()
}

#[test]
fn the_model_agrees_with_the_synthetic_xref() {
    let model = model();
    let c = xref::compare(
        &model,
        &xref::searches(&xref::parse(&fixture("search.xref"))),
    );
    // Line 5 is a multi-index search, which the model leaves Unknown.
    assert!(matches!(
        model.iter().find(|m| m.line == 5).map(|m| &m.access),
        Some(Access::Unknown(_))
    ));
    assert_eq!(c.matched, 6);
    assert_eq!(c.unknown, 1);
    assert_eq!(c.agree, 4);
    assert_eq!(c.whole_agree, 4);
    // Line 9: the model brackets OrderDate by its range; this XREF says
    // WHOLE-INDEX, so it is reported with both sides.
    assert_eq!(c.whole_missed, 1);
    let d = c
        .disagreements
        .iter()
        .find(|d| d.line == 9)
        .expect("line 9 disagrees");
    assert_eq!(d.model, ["OrderDate"]);
    assert_eq!(d.xref, ["OrderDate whole-index"]);
    // Line 11 has no search in the model.
    assert_eq!(c.xref_only, 1);
    assert!(
        c.disagreements
            .iter()
            .any(|d| d.line == 11 && d.model.is_empty())
    );
}

#[test]
fn a_different_index_is_a_disagreement() {
    let model = model();
    let text = "search.p search.p 1 SEARCH sports.Customer Name\n";
    let c = xref::compare(&model, &xref::searches(&xref::parse(text)));
    let d = c
        .disagreements
        .iter()
        .find(|d| d.line == 1)
        .expect("line 1 disagrees");
    assert_eq!(d.model, ["CustNum"]);
    assert_eq!(d.xref, ["Name"]);
    assert!(d.whole_agrees);
    // The model's other searches have no XREF row.
    assert!(c.model_only >= 4);
}
