//! A subclass names the buffers and temp-tables of its superclass in statements
//! (`FIND`, `FOR EACH`, `CREATE`) and through their fields (`tt_.f`). The
//! inherited buffer reads the table its declaration names, so its fields
//! resolve and the file depends on that table. Runs the real `BatchIndex`
//! against a loaded schema.

mod support;

use std::path::PathBuf;

use oxabl_common::FileId;
use oxabl_index::BatchIndex;
use oxabl_lint::lint_file;
use oxabl_schema::test_support::customer_schema;
use oxabl_semantic::{AnalysisContext, Semantic, SymbolId, SymbolKind, UnresolvedReason};
use oxabl_workspace::InMemoryFileSystem;

use support::*;

const BASE_PATH: &str = "/src/pkg/base.cls";
const BASE: &str = r#"CLASS pkg.base:
    DEFINE PROTECTED TEMP-TABLE tt_ NO-UNDO FIELD f AS INTEGER.
    DEFINE PROTECTED BUFFER cust_ FOR Customer.
    DEFINE PROTECTED BUFFER ttbuf_ FOR TEMP-TABLE tt_.
    DEFINE PROTECTED VARIABLE title_ AS CHARACTER NO-UNDO.
    DEFINE PRIVATE TEMP-TABLE hidden_ NO-UNDO FIELD g AS INTEGER.
END CLASS."#;

/// A temp-table named like the schema table, and a buffer for that name.
const SHADOWING_BASE: &str = r#"CLASS pkg.base:
    DEFINE PROTECTED TEMP-TABLE Customer NO-UNDO FIELD z AS INTEGER.
    DEFINE PROTECTED BUFFER shadowed_ FOR Customer.
END CLASS."#;

fn child(body: &str) -> String {
    format!(
        "USING pkg.*.\nCLASS app.child INHERITS pkg.base:\n    METHOD PUBLIC VOID run-it():\n        {body}\n    END METHOD.\nEND CLASS."
    )
}

/// The model and the findings of every rule for `source`, with `BASE` in the
/// workspace and the `Customer` schema loaded.
fn analyse(source: &str, base: &str) -> (Semantic, Vec<(&'static str, String)>) {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(BASE_PATH), base);
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);
    let schema = customer_schema();
    let stmts = parse(source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, source, &schema).with_index(&index);
    ctx.schema_loaded = true;
    let sem = oxabl_semantic::analyze_file(&stmts, &ctx);
    let findings = lint_file(&stmts, &sem, &ctx)
        .into_iter()
        .map(|d| (d.code.0, d.message))
        .collect();
    (sem, findings)
}

fn codes(findings: &[(&'static str, String)]) -> Vec<&'static str> {
    findings.iter().map(|(code, _)| *code).collect()
}

/// The one symbol named `name`, and the table it reads.
fn table_of(sem: &Semantic, name: &str) -> (SymbolId, Option<String>) {
    let sym = sole_symbol(sem, name);
    let table = sem
        .symbols
        .get(sym)
        .table_id
        .and_then(|tid| customer_schema().get_by_id(tid).map(|t| t.name.to_string()));
    (sym, table)
}

#[test]
fn a_field_of_an_inherited_temp_table_is_no_finding() {
    let source = child("tt_.f = 1. FIND FIRST tt_. CREATE tt_. FOR EACH tt_: tt_.f = 2. END.");
    let (_sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
}

#[test]
fn an_inherited_variable_written_in_the_child_is_no_dead_store() {
    let source = child("title_ = \"x\".");
    let (_sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
}

#[test]
fn an_inherited_buffer_reads_the_table_it_is_for() {
    let source = child("FIND FIRST cust_ NO-LOCK. MESSAGE cust_.Name.");
    let (sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
    let (sym, table) = table_of(&sem, "cust_");
    assert_eq!(sem.symbols.get(sym).kind, SymbolKind::Buffer);
    assert_eq!(table.as_deref(), Some("customer"));
    assert_eq!(
        sem.symbols.get(sole_resolved(&sem, "name")).kind,
        SymbolKind::Field,
        "the field resolves through the inherited buffer"
    );
}

#[test]
fn a_field_the_inherited_buffer_table_lacks_is_reported() {
    let source = child("MESSAGE cust_.Nope.");
    let (sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), vec!["LINT0003"], "{findings:?}");
    assert_eq!(
        unresolved_reasons(&sem, "nope"),
        vec![UnresolvedReason::NotInScope]
    );
}

#[test]
fn a_for_each_over_an_inherited_buffer_reads_its_table() {
    let source = child("FOR EACH cust_: MESSAGE cust_.Name. END.");
    let (sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
    assert_eq!(
        sem.symbols.get(sole_resolved(&sem, "name")).kind,
        SymbolKind::Field
    );
    let reads: Vec<_> = symbols_named(&sem, "cust_")
        .into_iter()
        .filter(|s| sem.symbols.get(*s).table_id.is_some())
        .collect();
    assert!(!reads.is_empty(), "the implicit buffer reads Customer");
}

#[test]
fn a_buffer_of_an_inherited_temp_table_reads_no_schema_table() {
    let source = child("FIND FIRST ttbuf_. MESSAGE ttbuf_.f.");
    let (sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
    assert_eq!(table_of(&sem, "ttbuf_").1, None);
}

#[test]
fn an_inherited_temp_table_hides_a_schema_table_of_its_name() {
    let source = child("FIND FIRST shadowed_.");
    let (sem, _findings) = analyse(&source, SHADOWING_BASE);

    assert_eq!(
        table_of(&sem, "shadowed_").1,
        None,
        "the temp-table `Customer` of the superclass wins over the schema table"
    );
}

#[test]
fn a_private_inherited_temp_table_is_not_reported_as_undefined() {
    let source = child("hidden_.g = 1.");
    let (sem, findings) = analyse(&source, BASE);

    assert_eq!(codes(&findings), Vec::<&str>::new(), "{findings:?}");
    assert_eq!(
        unresolved_reasons(&sem, "hidden_"),
        vec![UnresolvedReason::PresentButUnusable]
    );
}

#[test]
fn a_private_temp_table_still_hides_a_schema_table_from_the_buffer_for_it() {
    let base = "CLASS pkg.base:\n\
        DEFINE PRIVATE TEMP-TABLE Customer NO-UNDO FIELD z AS INTEGER.\n\
        DEFINE PROTECTED BUFFER hbuf_ FOR Customer.\n\
        END CLASS.";
    let source = child("FIND FIRST hbuf_.");
    let (sem, _findings) = analyse(&source, base);

    assert_eq!(table_of(&sem, "hbuf_").1, None);
}

#[test]
fn a_temp_table_of_a_farther_ancestor_hides_a_schema_table() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/src/pkg/top.cls"),
        "CLASS pkg.top:\n\
         DEFINE PROTECTED TEMP-TABLE Customer NO-UNDO FIELD z AS INTEGER.\n\
         END CLASS.",
    );
    fs.insert(
        PathBuf::from(BASE_PATH),
        "CLASS pkg.base INHERITS pkg.top:\n\
         DEFINE PROTECTED BUFFER mid_ FOR Customer.\n\
         END CLASS.",
    );
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);
    let schema = customer_schema();
    let source = child("FIND FIRST mid_.");
    let stmts = parse(&source);
    let mut ctx = AnalysisContext::new(FileId::UNKNOWN, &source, &schema).with_index(&index);
    ctx.schema_loaded = true;
    let sem = oxabl_semantic::analyze_file(&stmts, &ctx);

    assert_eq!(table_of(&sem, "mid_").1, None);
}

#[test]
fn a_name_no_ancestor_declares_is_still_undefined() {
    let source = child("nothing_.f = 1.");
    let (_sem, findings) = analyse(&source, BASE);

    assert!(codes(&findings).contains(&"LINT0001"), "{findings:?}");
}
