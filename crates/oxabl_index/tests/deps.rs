//! The typed dependency edge set (R2, R3, R15).
//!
//! These drive the builder with hand-supplied include sets, which is the whole
//! point of it taking its inputs from the caller: real includes, cycles, the depth
//! cap, and a loaded schema over a real filesystem need the preprocessor, and this
//! crate must not depend on it. That coverage lives in `oxabl_pipeline`, where all
//! three inputs exist at once.

use std::path::{Path, PathBuf};

use oxabl_ast::Span;
use oxabl_common::{FileId, VirtualSpan};
use oxabl_index::{
    BatchIndex, DirectIncludeInput, EdgeInputs, EdgeKind, EdgeTarget, UnresolvedIncludeInput,
    build_edge_set,
};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_schema::test_support::customer_schema;
use oxabl_semantic::{AnalysisContext, NullIndex, Semantic, UnresolvedReason, analyze_file};
use oxabl_workspace::InMemoryFileSystem;

const ANALYSED: FileId = FileId::new(1);

/// A parent class with one method, and where a `/src` include-path entry makes the
/// index look for it: a qualified name maps onto a relative path.
const CALC_BASE: &str = "CLASS orders.calc-base:\n\
                         METHOD PUBLIC INTEGER calc-total():\n\
                         RETURN 0.\n\
                         END METHOD.\n\
                         END CLASS.";
const CALC_BASE_PATH: &str = "/src/orders/calc-base.cls";

/// The child *calls* the inherited method, which is what makes the supertype chain
/// walk run and record a class lookup. A header that names a parent nobody uses
/// asks the index nothing.
const CHILD: &str = "CLASS orders.child INHERITS orders.calc-base:\n\
                     METHOD PUBLIC VOID run-it():\n\
                     DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
                     v-total = calc-total().\n\
                     MESSAGE v-total.\n\
                     END METHOD.\n\
                     END CLASS.";

/// The same child, plus a schema buffer, so one file produces edges of three kinds.
const CHILD_WITH_BUFFER: &str = "CLASS orders.child INHERITS orders.calc-base:\n\
                                 METHOD PUBLIC VOID run-it():\n\
                                 DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
                                 DEFINE BUFFER b FOR Customer.\n\
                                 FIND FIRST b NO-LOCK.\n\
                                 v-total = calc-total().\n\
                                 MESSAGE v-total.\n\
                                 END METHOD.\n\
                                 END CLASS.";

/// Analyse `source` against `schema` and `index`, with no preprocessing — so the
/// model's spans are already the file's own bytes and the identity resolver below
/// is the truthful mapping.
fn analyse(source: &str, schema: &Schema, index: &dyn oxabl_semantic::WorkspaceIndex) -> Semantic {
    let tokens = oxabl_lexer::tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    let mut ctx = AnalysisContext::new(ANALYSED, source, schema).with_index(index);
    ctx.schema_loaded = !schema.is_empty();
    analyze_file(&program.statements, &ctx)
}

/// The mapping for an unpreprocessed buffer: virtual offsets *are* real offsets.
fn identity(span: VirtualSpan) -> Option<Span> {
    Some(Span {
        start: span.start,
        end: span.end,
    })
}

fn inputs<'a>(
    semantic: &'a Semantic,
    schema: &'a Schema,
    direct: &'a [DirectIncludeInput<'a>],
    transitive: &'a [PathBuf],
    unresolved_includes: &'a [UnresolvedIncludeInput<'a>],
) -> EdgeInputs<'a> {
    EdgeInputs {
        semantic,
        schema,
        direct_includes: direct,
        transitive_includes: transitive,
        unresolved_includes,
        resolve_span: &identity,
    }
}

fn dirs(entries: &[&str]) -> Vec<PathBuf> {
    entries.iter().map(PathBuf::from).collect()
}

fn targets(kind: EdgeKind, edges: &oxabl_index::DependencyEdges) -> Vec<String> {
    edges
        .of_kind(kind)
        .map(|e| e.target.key().to_string())
        .collect()
}

#[test]
fn direct_and_transitive_includes_are_not_confused() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/outer.i"),
        site: Span { start: 0, end: 9 },
    }];
    // The transitive list is everything the expansion read, the direct include
    // among them — so the builder must not report `outer.i` twice.
    let transitive = dirs(&["/proj/outer.i", "/proj/inner.i"]);
    let edges = build_edge_set(&inputs(&semantic, &schema, &direct, &transitive, &[]));

    assert_eq!(
        targets(EdgeKind::DirectInclude, &edges),
        vec!["/proj/outer.i"]
    );
    assert_eq!(
        targets(EdgeKind::TransitiveInclude, &edges),
        vec!["/proj/inner.i"]
    );
    assert_eq!(
        edges.of_kind(EdgeKind::DirectInclude).next().unwrap().span,
        Some(Span { start: 0, end: 9 }),
        "a direct include carries the site of its own reference"
    );
    assert_eq!(
        edges
            .of_kind(EdgeKind::TransitiveInclude)
            .next()
            .unwrap()
            .span,
        None,
        "a transitive include is named in another file, so this one has no site"
    );
}

#[test]
fn a_resolved_class_reference_becomes_an_edge_carrying_its_span() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CALC_BASE_PATH), CALC_BASE);
    let paths = dirs(&["/src"]);
    let index = BatchIndex::new(&fs, &paths);
    let schema = Schema::empty();

    // The body calls the inherited method, so the supertype chain walk actually
    // runs — a header alone asks the index nothing.
    let source = CHILD;
    let semantic = analyse(source, &schema, &index);
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

    let class_edges: Vec<_> = edges.of_kind(EdgeKind::ClassReference).collect();
    assert_eq!(class_edges.len(), 1, "got {class_edges:?}");
    assert_eq!(class_edges[0].target.key(), "orders.calc-base");
    assert!(
        matches!(class_edges[0].target, EdgeTarget::IndexedFile { .. }),
        "a resolved class names the file the index reached"
    );
    let span = class_edges[0]
        .span
        .expect("the header writes the name here");
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "orders.calc-base"
    );
}

#[test]
fn a_schema_edge_is_keyed_by_folded_table_name_and_case_collapses() {
    let schema = customer_schema();
    // Two buffers over the same table, spelled differently. Both carry a
    // `table_id`, and both must land on one edge.
    let semantic = analyse(
        "DEFINE BUFFER b-one FOR Customer.\n\
         DEFINE BUFFER b-two FOR CUSTOMER.\n\
         FIND FIRST b-one NO-LOCK.\n\
         FIND FIRST b-two NO-LOCK.\n",
        &schema,
        &NullIndex,
    );
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

    let table_edges = targets(EdgeKind::SchemaTable, &edges);
    assert_eq!(
        table_edges,
        vec!["customer"],
        "one folded table name, however the source spelled it"
    );
}

#[test]
fn an_unresolved_reference_is_its_own_row_and_no_resolved_edge() {
    // A class on the paths that no file supplies: the index looked and answered
    // absent, which is a fact about the workspace rather than a missing capability.
    let fs = InMemoryFileSystem::new();
    let paths = dirs(&["/src"]);
    let index = BatchIndex::new(&fs, &paths);
    let schema = Schema::empty();

    let semantic = analyse(CHILD, &schema, &index);
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

    assert_eq!(
        edges.of_kind(EdgeKind::ClassReference).count(),
        0,
        "an absent class must not appear as a resolved edge"
    );
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::ClassReference);
    assert_eq!(rows[0].name, "orders.calc-base");
    assert_eq!(rows[0].reason, UnresolvedReason::AbsentFromWorkspace);
}

#[test]
fn an_unresolvable_include_is_its_own_row_with_a_reason() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let unresolved_includes = [UnresolvedIncludeInput {
        name: "nowhere.i",
        site: Span { start: 0, end: 11 },
    }];
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &unresolved_includes));

    assert!(edges.edges().is_empty());
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].kind, EdgeKind::DirectInclude);
    assert_eq!(rows[0].name, "nowhere.i");
    assert_eq!(rows[0].reason, UnresolvedReason::AbsentFromWorkspace);
    assert_eq!(rows[0].span, Some(Span { start: 0, end: 11 }));
}

#[test]
fn every_span_an_edge_carries_falls_inside_the_dependent_file() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CALC_BASE_PATH), CALC_BASE);
    let paths = dirs(&["/src"]);
    let index = BatchIndex::new(&fs, &paths);
    let schema = customer_schema();

    let source = CHILD_WITH_BUFFER;
    let semantic = analyse(source, &schema, &index);
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/a.i"),
        site: Span { start: 0, end: 5 },
    }];
    let edges = build_edge_set(&inputs(&semantic, &schema, &direct, &[], &[]));

    assert!(!edges.edges().is_empty(), "the fixture must produce edges");
    for edge in edges.edges() {
        if let Some(span) = edge.span {
            assert!(
                span.start <= span.end && (span.end as usize) <= source.len(),
                "{edge:?} names bytes outside the dependent file"
            );
        }
    }
}

#[test]
fn two_edge_sets_from_identical_input_compare_equal() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CALC_BASE_PATH), CALC_BASE);
    let paths = dirs(&["/src"]);
    let schema = customer_schema();
    let source = CHILD_WITH_BUFFER;
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/a.i"),
        site: Span { start: 0, end: 5 },
    }];
    let transitive = dirs(&["/proj/a.i", "/proj/b.i"]);

    let build = || {
        let index = BatchIndex::new(&fs, &paths);
        let semantic = analyse(source, &schema, &index);
        build_edge_set(&inputs(&semantic, &schema, &direct, &transitive, &[]))
    };
    assert_eq!(build(), build());
}
