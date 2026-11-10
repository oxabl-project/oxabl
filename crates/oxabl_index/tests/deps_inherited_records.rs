//! A subclass that reads a table through a buffer its superclass declares
//! depends on that table: the schema edge is in its edge set, with the real
//! `BatchIndex` and a loaded schema. A subclass that only reads an inherited
//! temp-table depends on no schema table.

use std::path::PathBuf;

use oxabl_ast::Span;
use oxabl_common::{FileId, VirtualSpan};
use oxabl_index::{BatchIndex, EdgeInputs, EdgeKind, build_edge_set};
use oxabl_parser::Parser;
use oxabl_schema::test_support::customer_schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

const BASE: &str = "CLASS orders.base:\n\
    DEFINE PROTECTED BUFFER cust_ FOR Customer.\n\
    DEFINE PROTECTED TEMP-TABLE tt_ NO-UNDO FIELD f AS INTEGER.\n\
    DEFINE PRIVATE BUFFER secret_ FOR Customer.\n\
    END CLASS.";

fn child(body: &str) -> String {
    format!(
        "CLASS orders.child INHERITS orders.base:\n\
         METHOD PUBLIC VOID run-it():\n\
         {body}\n\
         END METHOD.\n\
         END CLASS."
    )
}

/// The schema tables the edge set of `source` names.
fn schema_tables(source: &str) -> Vec<String> {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/src/orders/base.cls"), BASE);
    let dirs = vec![PathBuf::from("/src")];
    let index = BatchIndex::new(&fs, &dirs);
    let schema = customer_schema();
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let mut ctx = AnalysisContext::new(FileId::new(1), source, &schema).with_index(&index);
    ctx.schema_loaded = true;
    let semantic = analyze_file(&program.statements, &ctx);
    let identity = |span: VirtualSpan| {
        Some(Span {
            start: span.start,
            end: span.end,
        })
    };
    let no_include = |_: VirtualSpan| None;
    let edges = build_edge_set(&EdgeInputs {
        semantic: &semantic,
        schema: &schema,
        direct_includes: &[],
        transitive_includes: &[],
        include_origins: &[],
        unresolved_includes: &[],
        nested_unresolved_includes: &[],
        unreadable_includes: &[],
        parse_recovered: false,
        preprocessor_loss: false,
        resolve_span: &identity,
        include_site: &no_include,
    });
    edges
        .of_kind(EdgeKind::SchemaTable)
        .map(|e| e.target.key().to_string())
        .collect()
}

#[test]
fn a_find_on_an_inherited_buffer_depends_on_its_table() {
    assert_eq!(
        schema_tables(&child("FIND FIRST cust_ NO-LOCK.")),
        ["customer"]
    );
}

#[test]
fn a_for_each_on_an_inherited_buffer_depends_on_its_table() {
    assert_eq!(
        schema_tables(&child("FOR EACH cust_ NO-LOCK: END.")),
        ["customer"]
    );
}

#[test]
fn a_field_of_an_inherited_buffer_depends_on_its_table() {
    assert_eq!(schema_tables(&child("MESSAGE cust_.Name.")), ["customer"]);
}

#[test]
fn an_inherited_temp_table_depends_on_no_schema_table() {
    assert_eq!(
        schema_tables(&child("FIND FIRST tt_. CREATE tt_. tt_.f = 1.")),
        Vec::<String>::new()
    );
}

#[test]
fn a_private_inherited_buffer_is_not_a_dependency() {
    assert_eq!(
        schema_tables(&child("FIND FIRST secret_ NO-LOCK.")),
        Vec::<String>::new()
    );
}

#[test]
fn a_buffer_nobody_names_is_not_a_dependency() {
    assert_eq!(
        schema_tables(&child("MESSAGE \"x\".")),
        Vec::<String>::new()
    );
}
