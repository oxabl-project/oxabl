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
    BatchIndex, BranchKind, Completeness, ConditionalBranch, ConditionalInclude, DegradedReason,
    DependencyEdges, DirectIncludeInput, EdgeInputs, EdgeKind, EdgeTarget, IncludeOriginInput,
    NestedUnresolvedIncludeInput, UnreadableIncludeInput, UnresolvedIncludeInput, build_edge_set,
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

/// The include-site mapping for an unpreprocessed buffer: nothing came from an
/// include.
fn no_include(_span: VirtualSpan) -> Option<Span> {
    None
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
        include_origins: &[],
        unresolved_includes,
        nested_unresolved_includes: &[],
        unreadable_includes: &[],
        parse_recovered: false,
        preprocessor_loss: false,
        resolve_span: &identity,
        include_site: &no_include,
    }
}

fn nested(name: &str, include_site: Option<Span>) -> NestedUnresolvedIncludeInput<'_> {
    NestedUnresolvedIncludeInput { name, include_site }
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

// One file is one dependency however the resolver spelled it. `Path` equality
// compares components, so it already sees through a `.`; it keeps a `..` as a real
// component, so `/proj/sub/../shared.i` and `/proj/shared.i` compared unequal and
// the same include was emitted as direct *and* transitive — doubling its weight in
// an impact answer.
#[test]
fn one_include_spelled_two_ways_is_one_edge() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/shared.i"),
        site: Span { start: 0, end: 9 },
    }];
    let transitive = dirs(&["/proj/sub/../shared.i"]);
    let edges = build_edge_set(&inputs(&semantic, &schema, &direct, &transitive, &[]));

    assert_eq!(
        targets(EdgeKind::DirectInclude, &edges),
        vec!["/proj/shared.i"]
    );
    assert!(
        targets(EdgeKind::TransitiveInclude, &edges).is_empty(),
        "the same file must not also be transitive, got {:?}",
        targets(EdgeKind::TransitiveInclude, &edges)
    );
}

// And the emitted target carries the normalised spelling, so a consumer keying on
// it agrees with the reverse query by construction.
#[test]
fn an_include_target_is_emitted_normalised() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/sub/../shared.i"),
        site: Span { start: 0, end: 9 },
    }];
    let edges = build_edge_set(&inputs(&semantic, &schema, &direct, &[], &[]));

    assert_eq!(
        targets(EdgeKind::DirectInclude, &edges),
        vec!["/proj/shared.i"]
    );
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
fn a_transitive_include_carries_the_site_of_the_chain_that_reached_it() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let site = Span { start: 0, end: 9 };
    let direct = [DirectIncludeInput {
        path: Path::new("/proj/outer.i"),
        site,
    }];
    let transitive = dirs(&["/proj/outer.i", "/proj/inner.i", "/proj/orphan.i"]);
    let origins = [
        IncludeOriginInput {
            path: Path::new("/proj/outer.i"),
            site,
        },
        IncludeOriginInput {
            path: Path::new("/proj/inner.i"),
            site,
        },
    ];
    let edges = build_edge_set(&EdgeInputs {
        include_origins: &origins,
        ..inputs(&semantic, &schema, &direct, &transitive, &[])
    });

    let sites: Vec<_> = edges
        .of_kind(EdgeKind::TransitiveInclude)
        .map(|edge| (edge.target.key().to_string(), edge.span, edge.include_site))
        .collect();
    assert_eq!(
        sites,
        [
            ("/proj/inner.i".to_string(), None, Some(site)),
            ("/proj/orphan.i".to_string(), None, None),
        ]
    );
}

#[test]
fn a_resolved_class_reference_becomes_an_edge_carrying_its_span() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CALC_BASE_PATH), CALC_BASE);
    let paths = dirs(&["/src"]);
    let index = BatchIndex::new(&fs, &paths);
    let schema = Schema::empty();

    // The header names the class, and the body's inherited call walks to it as
    // well; both reach one edge.
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

/// A class named in a header, with a body large enough that a scan and a map
/// could plausibly differ, reports the header's span: the earliest place the
/// file names it.
#[test]
fn a_class_name_span_is_unchanged_by_the_per_file_map() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from(CALC_BASE_PATH), CALC_BASE);
    let paths = dirs(&["/src"]);
    let index = BatchIndex::new(&fs, &paths);
    let schema = Schema::empty();

    // Many symbols ahead of and behind the reference, so a scan that stopped early
    // and a map that recorded the wrong entry would disagree.
    let mut source = String::from("CLASS orders.child INHERITS orders.calc-base:\n");
    source.push_str("METHOD PUBLIC VOID run-it():\n");
    for index in 0..40 {
        source.push_str(&format!(
            "DEFINE VARIABLE v-pad{index} AS INTEGER NO-UNDO.\n"
        ));
    }
    source.push_str("DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n");
    source.push_str("v-total = calc-total().\n");
    source.push_str("MESSAGE v-total.\n");
    source.push_str("END METHOD.\nEND CLASS.");

    let semantic = analyse(&source, &schema, &index);
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

    let class_edges: Vec<_> = edges.of_kind(EdgeKind::ClassReference).collect();
    assert_eq!(class_edges.len(), 1, "got {class_edges:?}");
    let span = class_edges[0].span.expect("the header writes the name");
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "orders.calc-base",
        "the span must still point at the name the header writes"
    );
}

/// A file with no class lookup pays nothing for the map, because there is nothing to
/// look up — the map is built on the first lookup rather than up front.
///
/// Asserted through the edge set rather than through a counter: a file that produces
/// no class edges and no unresolved class rows never entered the branch that builds
/// the map, and there is no other way to reach it.
#[test]
fn a_file_with_no_class_lookups_builds_no_map() {
    let schema = customer_schema();
    let semantic = analyse(
        "DEFINE BUFFER b FOR Customer.\nFIND FIRST b NO-LOCK.\n",
        &schema,
        &NullIndex,
    );
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

    assert_eq!(edges.of_kind(EdgeKind::ClassReference).count(), 0);
    assert!(
        edges
            .unresolved()
            .iter()
            .all(|row| row.kind != EdgeKind::ClassReference),
        "a file that names no class must ask the index nothing"
    );
    assert_eq!(targets(EdgeKind::SchemaTable, &edges), vec!["customer"]);
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
        unterminated: false,
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

#[test]
fn an_edge_set_built_from_nothing_is_complete() {
    assert_eq!(
        DependencyEdges::default().completeness(),
        &Completeness::Complete
    );
    assert!(
        DependencyEdges::default()
            .completeness()
            .reasons()
            .is_empty()
    );

    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));
    assert!(edges.completeness().is_complete());
}

#[test]
fn each_way_of_losing_source_degrades_the_set_for_its_own_reason() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let reasons = |inputs: EdgeInputs<'_>| build_edge_set(&inputs).completeness().clone();

    let base = || inputs(&semantic, &schema, &[], &[], &[]);
    assert_eq!(
        reasons(EdgeInputs {
            parse_recovered: true,
            ..base()
        }),
        Completeness::Degraded {
            reasons: vec![DegradedReason::ParseRecovered]
        }
    );
    assert_eq!(
        reasons(EdgeInputs {
            nested_unresolved_includes: &[nested("deep.i", None)],
            ..base()
        }),
        Completeness::Degraded {
            reasons: vec![DegradedReason::UnresolvedInclude]
        }
    );
    assert_eq!(
        reasons(EdgeInputs {
            unreadable_includes: &[UnreadableIncludeInput {
                name: "latin.i",
                path: Path::new("/src/latin.i"),
                site: None,
                include_site: None,
            }],
            ..base()
        }),
        Completeness::Degraded {
            reasons: vec![DegradedReason::UnreadableInclude]
        }
    );
}

#[test]
fn reasons_are_sorted_and_not_repeated() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let unresolved = [UnresolvedIncludeInput {
        name: "a.i",
        site: Span { start: 0, end: 5 },
        unterminated: false,
    }];
    let edges = build_edge_set(&EdgeInputs {
        parse_recovered: true,
        nested_unresolved_includes: &[nested("b.i", None)],
        ..inputs(&semantic, &schema, &[], &[], &unresolved)
    });
    assert_eq!(
        edges.completeness().reasons(),
        [
            DegradedReason::ParseRecovered,
            DegradedReason::UnresolvedInclude
        ]
    );
}

#[test]
fn a_nested_unresolved_include_is_a_transitive_row_with_no_site() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let edges = build_edge_set(&EdgeInputs {
        nested_unresolved_includes: &[nested("deep.i", None), nested("deep.i", None)],
        ..inputs(&semantic, &schema, &[], &[], &[])
    });

    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "one name reached twice is one row: {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::TransitiveInclude);
    assert_eq!(rows[0].name, "deep.i");
    assert_eq!(rows[0].reason, UnresolvedReason::AbsentFromWorkspace);
    assert_eq!(rows[0].span, None);
}

#[test]
fn an_unreadable_include_keeps_its_edge_and_adds_a_row() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let site = Span { start: 0, end: 9 };
    let edges = build_edge_set(&EdgeInputs {
        unreadable_includes: &[
            UnreadableIncludeInput {
                name: "latin.i",
                path: Path::new("/src/latin.i"),
                site: Some(site),
                include_site: None,
            },
            UnreadableIncludeInput {
                name: "deep.i",
                path: Path::new("/src/deep.i"),
                site: None,
                include_site: None,
            },
        ],
        ..inputs(&semantic, &schema, &[], &[], &[])
    });

    assert_eq!(targets(EdgeKind::DirectInclude, &edges), ["/src/latin.i"]);
    assert_eq!(
        targets(EdgeKind::TransitiveInclude, &edges),
        ["/src/deep.i"]
    );
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 2, "got {rows:?}");
    assert!(
        rows.iter()
            .all(|row| row.reason == UnresolvedReason::PresentButUnusable)
    );
    assert!(
        rows.iter()
            .any(|row| row.kind == EdgeKind::DirectInclude && row.span == Some(site))
    );
}

#[test]
fn an_unterminated_include_is_unknowable_not_absent() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let unresolved = [UnresolvedIncludeInput {
        name: "a.i",
        site: Span { start: 0, end: 1 },
        unterminated: true,
    }];
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &unresolved));

    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].kind, EdgeKind::DirectInclude);
    assert_eq!(rows[0].reason, UnresolvedReason::Unknowable);
    assert_eq!(
        edges.completeness().reasons(),
        [DegradedReason::UnresolvedInclude]
    );
}

#[test]
fn a_nested_row_carries_the_site_of_the_chain_that_reached_it() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let chain = Span { start: 4, end: 12 };
    let edges = build_edge_set(&EdgeInputs {
        nested_unresolved_includes: &[nested("deep.i", Some(chain))],
        unreadable_includes: &[UnreadableIncludeInput {
            name: "latin.i",
            path: Path::new("/src/latin.i"),
            site: None,
            include_site: Some(chain),
        }],
        ..inputs(&semantic, &schema, &[], &[], &[])
    });

    let rows = edges.unresolved();
    assert_eq!(rows.len(), 2, "got {rows:?}");
    assert!(rows.iter().all(|row| row.include_site == Some(chain)));
    assert!(
        edges
            .edges()
            .iter()
            .all(|edge| edge.include_site == Some(chain)),
        "the unreadable file's edge names the chain too"
    );
}

#[test]
fn a_preprocessor_loss_degrades_the_set_for_its_own_reason() {
    let schema = Schema::empty();
    let semantic = analyse("MESSAGE \"hi\".", &schema, &NullIndex);
    let edges = build_edge_set(&EdgeInputs {
        preprocessor_loss: true,
        ..inputs(&semantic, &schema, &[], &[], &[])
    });

    assert_eq!(
        edges.completeness().reasons(),
        [DegradedReason::PreprocessorLoss]
    );
}

/// Every `RUN` target shape the parser takes whole, with the file that satisfies it.
const RUN_SHAPES: &[(&str, &str)] = &[
    ("a/b.c.p", "/src/a/b.c.p"),
    ("comm\\expdist.p", "/src/comm/expdist.p"),
    ("3rd/x.p", "/src/3rd/x.p"),
    ("./sub2.p", "/src/sub2.p"),
    ("a.b/c.p", "/src/a.b/c.p"),
];

#[test]
fn every_run_target_shape_becomes_a_program_edge_naming_the_whole_target() {
    for &(target, path) in RUN_SHAPES {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from(path), "MESSAGE \"x\".");
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let schema = Schema::empty();

        let source = format!("RUN {target}.\n");
        let semantic = analyse(&source, &schema, &index);
        let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

        let program: Vec<_> = edges.of_kind(EdgeKind::ProgramReference).collect();
        assert_eq!(program.len(), 1, "{target}: {program:?}");
        assert_eq!(program[0].target.key(), target.to_lowercase(), "{target}");
        assert!(
            matches!(program[0].target, EdgeTarget::IndexedFile { .. }),
            "{target}: a resolved target names the file the index reached"
        );
        let span = program[0].span.expect("the RUN writes the name here");
        assert_eq!(
            &source[span.start as usize..span.end as usize],
            target,
            "{target}"
        );
        assert!(edges.unresolved().is_empty(), "{target}");
    }
}

#[test]
fn an_absent_run_target_row_keeps_the_whole_written_name() {
    for &(target, _) in RUN_SHAPES {
        let fs = InMemoryFileSystem::new();
        let paths = dirs(&["/src"]);
        let index = BatchIndex::new(&fs, &paths);
        let schema = Schema::empty();

        let source = format!("RUN {target}.\n");
        let semantic = analyse(&source, &schema, &index);
        let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));

        assert_eq!(edges.of_kind(EdgeKind::ProgramReference).count(), 0);
        let rows = edges.unresolved();
        assert_eq!(rows.len(), 1, "{target}: {rows:?}");
        assert_eq!(rows[0].kind, EdgeKind::ProgramReference);
        assert_eq!(rows[0].name, target.to_lowercase(), "{target}");
        assert_eq!(rows[0].reason, UnresolvedReason::AbsentFromWorkspace);
    }
}

fn conditional(name: &str, at: Option<u32>) -> ConditionalInclude {
    ConditionalInclude {
        name: name.to_string(),
        span: at.map(|start| Span {
            start,
            end: start + 5,
        }),
        include_site: None,
        branches: vec![ConditionalBranch {
            kind: BranchKind::If,
            condition: "FALSE".to_string(),
        }],
        computed_name: false,
    }
}

#[test]
fn conditional_includes_are_never_edges_and_come_back_ordered_and_once() {
    let edges = DependencyEdges::default().with_conditional_includes(vec![
        conditional("b.i", Some(20)),
        conditional("a.i", Some(10)),
        conditional("b.i", Some(20)),
        conditional("c.i", None),
    ]);

    let got: Vec<_> = edges
        .conditional_includes()
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(got, ["a.i", "b.i", "c.i"]);
    assert!(edges.edges().is_empty());
    assert!(edges.unresolved().is_empty());
    assert!(edges.is_empty());
    assert!(edges.completeness().is_complete());
}

#[test]
fn a_set_without_conditional_includes_compares_equal_to_the_default() {
    assert!(DependencyEdges::default().conditional_includes().is_empty());
    assert_eq!(
        DependencyEdges::default().with_conditional_includes(Vec::new()),
        DependencyEdges::default()
    );
}

/// The schema-table edges of `source`, analysed against the `Customer` schema.
fn table_edges(source: &str) -> Vec<String> {
    let schema = customer_schema();
    let semantic = analyse(source, &schema, &NullIndex);
    let edges = build_edge_set(&inputs(&semantic, &schema, &[], &[], &[]));
    targets(EdgeKind::SchemaTable, &edges)
}

#[test]
fn a_table_named_only_as_a_record_is_an_edge() {
    for source in [
        "FIND FIRST Customer NO-LOCK.",
        "FIND LAST Customer.",
        "FIND NEXT Customer.",
        "FIND PREV Customer.",
        "FIND CURRENT Customer.",
        "FIND Customer 1.",
        "FIND sports.Customer 1.",
        "FIND FIRST Customer USE-INDEX k NO-LOCK NO-ERROR.",
        "FIND FIRST ttOrder OF Customer.",
        "CREATE Customer.",
        "DELETE Customer.",
        "RELEASE Customer.",
        "VALIDATE Customer.",
        "BUFFER-COPY Customer TO Customer.",
        "BUFFER-COMPARE Customer TO Customer.",
        "DEFINE QUERY q FOR Customer.",
        "OPEN QUERY q FOR EACH Customer.",
        "OPEN QUERY q PRESELECT EACH Customer.",
        "FOR EACH ttOrder OF Customer: END.",
        "FOR EACH ttOrder NO-LOCK, EACH Customer NO-LOCK: END.",
        "FOR EACH ttOrder, FIRST Customer WHERE Customer.CustNum = 1: END.",
        "DO FOR Customer: END.",
        "REPEAT PRESELECT EACH Customer: END.",
        "MESSAGE CAN-FIND(ttOrder OF Customer).",
        "DEFINE TEMP-TABLE tt NO-UNDO LIKE Customer.",
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD x LIKE Customer.Name.",
        "DEFINE WORK-TABLE wt LIKE Customer.",
        "DEFINE VARIABLE v LIKE Customer.Name NO-UNDO.",
        "DEFINE VARIABLE v LIKE sports.Customer.Name NO-UNDO.",
        "DEFINE INPUT PARAMETER p LIKE Customer.Name NO-UNDO.",
        "FUNCTION f RETURNS LOGICAL (BUFFER fb FOR Customer): RETURN TRUE. END FUNCTION.",
        "FUNCTION f RETURNS LOGICAL (BUFFER fb FOR Customer) FORWARD.",
        "FUNCTION f RETURNS LOGICAL (INPUT p LIKE Customer.Name) FORWARD.",
        "UPDATE Customer.",
        "SET Customer.",
        "PROMPT-FOR Customer.",
        "INSERT Customer.",
        "IMPORT Customer.",
        "EXPORT Customer.",
        "ENABLE Customer WITH FRAME f.",
        "PUT Customer.Name.",
        "DISABLE TRIGGERS FOR LOAD OF Customer.",
        "ON WRITE OF Customer DO: END.",
        "ON ASSIGN OF Customer.Name DO: END.",
        "TRIGGER PROCEDURE FOR WRITE OF Customer.",
        "TRIGGER PROCEDURE FOR ASSIGN OF Customer.Name.",
        "DEFINE FRAME f Customer.Name.",
        "DEFINE QUERY q FOR Customer. DEFINE BROWSE br QUERY q DISPLAY Customer.Name WITH 5 DOWN.",
        "DEFINE DATA-SOURCE ds FOR Customer.",
    ] {
        assert_eq!(table_edges(source), ["customer"], "{source}");
    }
}

#[test]
fn a_local_name_or_a_bare_field_names_no_table() {
    for source in [
        "DEFINE TEMP-TABLE Customer NO-UNDO FIELD x AS INTEGER. FIND FIRST Customer.",
        "DEFINE VARIABLE Customer AS INTEGER NO-UNDO. UPDATE Customer.",
        "DEFINE TEMP-TABLE tt NO-UNDO FIELD f AS CHARACTER. DEFINE VARIABLE v LIKE tt.f NO-UNDO.",
        "DEFINE VARIABLE v LIKE Name NO-UNDO.",
        "FIND FIRST ttOrder NO-LOCK.",
        "DO TRANSACTION: END.",
    ] {
        assert!(table_edges(source).is_empty(), "{source}");
    }
}

#[test]
fn a_dynamic_buffer_field_named_like_a_table_is_no_edge() {
    for source in [
        "DEFINE VARIABLE h AS HANDLE NO-UNDO. PUT h::Customer.",
        "DEFINE VARIABLE h AS HANDLE NO-UNDO. DEFINE TEMP-TABLE tt NO-UNDO FIELD x AS INTEGER. \
         FOR EACH tt, EACH ttOrder WHERE ttOrder.x = h::Customer: END.",
    ] {
        assert!(table_edges(source).is_empty(), "{source}");
    }
}
