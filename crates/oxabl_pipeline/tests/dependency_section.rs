//! The envelope's `dependencies` section at version 3 (R2).
//!
//! The section reports typed dependency edges, and the conversion from an edge set
//! into its rows lives in this crate — the only one that depends on both the
//! document and the edge set. So its coverage lives here too, which is also the
//! only place all six edge kinds can be produced: `oxabl_analyze` has no include
//! sets and no workspace index of its own.

use std::path::PathBuf;

use oxabl_analyze::{DependencySection, dump_json};
use oxabl_pipeline::{LintPipeline, PipelineConfig, dependency_section};
use oxabl_schema::test_support::customer_schema;
use oxabl_workspace::InMemoryFileSystem;
use serde_json::Value;

/// A workspace whose single analysed file exercises every edge kind at once.
///
/// - `base.i` is included by `mid.i`, so the analysed file names `mid.i` directly
///   and reaches `base.i` transitively.
/// - `Customer` is a schema table.
/// - `orders.calc-base` is the class it inherits from and calls into. Inheritance
///   is what records a class lookup today: a `NEW` expression or a class-typed
///   declaration does not, which is edge-kind fidelity work tracked upstream.
/// - `post-order.p` is a literal `RUN` target.
/// - `producer.p` supplies the `DEFINE NEW SHARED` name it consumes.
/// - `nowhere.i` resolves to nothing.
const EVERY_KIND: &str = "{mid.i}\n\
                          {nowhere.i}\n\
                          CLASS orders.consumer INHERITS orders.calc-base:\n\
                          METHOD PUBLIC VOID run-it():\n\
                          DEFINE SHARED VARIABLE v-site AS CHARACTER NO-UNDO.\n\
                          DEFINE VARIABLE v-total AS INTEGER NO-UNDO.\n\
                          v-total = calc-total().\n\
                          FOR EACH Customer NO-LOCK:\n\
                          DISPLAY Customer.Name.\n\
                          END.\n\
                          RUN post-order.p.\n\
                          MESSAGE v-site v-base v-total.\n\
                          END METHOD.\n\
                          END CLASS.\n";

fn workspace() -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/src/base.i"),
        "DEFINE VARIABLE v-base AS INTEGER NO-UNDO.\n",
    );
    fs.insert(PathBuf::from("/src/mid.i"), "{base.i}\n");
    fs.insert(
        PathBuf::from("/src/orders/calc-base.cls"),
        "CLASS orders.calc-base:\n\
         METHOD PUBLIC INTEGER calc-total():\n\
         RETURN 0.\n\
         END METHOD.\n\
         END CLASS.",
    );
    fs.insert(PathBuf::from("/src/post-order.p"), "MESSAGE \"posted\".\n");
    fs.insert(
        PathBuf::from("/src/producer.p"),
        "DEFINE NEW SHARED VARIABLE v-site AS CHARACTER NO-UNDO.\n",
    );
    fs
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/src")],
        schema: customer_schema(),
        schema_loaded: true,
        ..PipelineConfig::default()
    }
}

/// Analyse `EVERY_KIND` and return the section its edges produce.
fn section() -> DependencySection {
    let fs = workspace();
    let config = config();
    let known = [PathBuf::from("/src/producer.p")];
    let run = LintPipeline::new(&config, &fs).with_known_files(&known);
    let file_run = run.with_file("/src/orders/consumer.cls");
    let expansion = file_run.expand(EVERY_KIND);
    let result = file_run.collect(&expansion);
    let edges = file_run
        .edges_of(&expansion, &result)
        .expect("the fixture analyses cleanly");
    let revision = result.semantic().expect("a model").index_revision.raw();
    dependency_section(&edges, revision)
}

fn vias(section: &DependencySection) -> Vec<&str> {
    section.edges.iter().map(|e| e.via.as_str()).collect()
}

// Every one of the six edge kinds reaches a row, each carrying its kind.
#[test]
fn a_row_carries_its_edge_kind_for_each_of_the_six_kinds() {
    let section = section();
    let vias = vias(&section);
    for kind in [
        "direct_include",
        "transitive_include",
        "schema_table",
        "class",
        "program",
        "shared_producer",
    ] {
        assert!(
            vias.contains(&kind),
            "no row carried `{kind}`, got {vias:?}"
        );
    }
}

// An include row names a path and carries no index id; an indexed file carries one;
// a schema table carries neither a file nor a CRC (KTD14).
#[test]
fn a_rows_identity_matches_what_its_source_actually_knows() {
    let section = section();

    let include = section
        .edges
        .iter()
        .find(|e| e.via == "direct_include")
        .expect("a direct include row");
    assert!(
        include.target.ends_with("mid.i"),
        "got {:?}",
        include.target
    );
    assert_eq!(
        include.file, None,
        "the preprocessor works in paths and mints no index id"
    );
    assert!(
        include.span.is_some(),
        "the analysed file writes the include itself"
    );

    let class = section
        .edges
        .iter()
        .find(|e| e.via == "class")
        .expect("a class row");
    assert_eq!(class.target, "orders.calc-base");
    assert!(class.file.is_some(), "the index reached a file for it");

    let table = section
        .edges
        .iter()
        .find(|e| e.via == "schema_table")
        .expect("a schema table row");
    assert_eq!(table.target, "customer", "the folded table name");
    assert_eq!(table.file, None, "a table is not a workspace file");

    let transitive = section
        .edges
        .iter()
        .find(|e| e.via == "transitive_include")
        .expect("a transitive include row");
    assert_eq!(
        transitive.span, None,
        "a transitive include is named in an intermediate file"
    );
}

// Unresolved rows keep their existing shape and reason vocabulary, and stay out of
// the resolved edges.
#[test]
fn unresolved_rows_keep_their_shape_and_reason_values() {
    let section = section();
    let row = section
        .unresolved
        .iter()
        .find(|u| u.name == "nowhere.i")
        .unwrap_or_else(|| panic!("got {:?}", section.unresolved));
    assert_eq!(row.reason, "absent_from_workspace");
    assert!(row.span.is_some(), "the analysed file writes the reference");
    assert!(
        !section.edges.iter().any(|e| e.target.contains("nowhere")),
        "an unresolved reference must not appear as a resolved edge"
    );
}

// The envelope reports `dependencies` at 3 and leaves every other section alone.
#[test]
fn the_envelope_reports_dependencies_at_version_three_and_nothing_else_moves() {
    let schema = customer_schema();
    let source = "MESSAGE \"hi\".\n";
    let tokens = oxabl_lexer::tokenize(source);
    let program = oxabl_parser::Parser::new(&tokens, source).parse_program();
    let ctx = oxabl_semantic::AnalysisContext::new(oxabl_pipeline::ROOT_FILE_ID, source, &schema);
    let sem = oxabl_semantic::analyze_file(&program.statements, &ctx);

    let v: Value = dump_json(
        &program.statements,
        &sem,
        &ctx,
        true,
        &DependencySection::default(),
    );
    assert_eq!(v["sections"]["dependencies"], 3);
    for (name, version) in [
        ("scopes", 1),
        ("types", 1),
        ("diagnostics", 1),
        ("preproc", 1),
        ("coverage", 1),
        ("symbols", 4),
        ("references", 2),
    ] {
        assert_eq!(v["sections"][name], version, "section `{name}` moved");
    }
}

// A file with no cross-file dependencies still emits the section, with empty
// collections rather than missing keys.
#[test]
fn a_file_with_no_cross_file_dependencies_still_emits_the_section() {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let run = LintPipeline::new(&config, &fs);
    let file_run = run.with_file("/src/lonely.p");
    let source = "MESSAGE \"alone\".\n";
    let expansion = file_run.expand(source);
    let result = file_run.collect(&expansion);
    let edges = file_run.edges_of(&expansion, &result).expect("clean");
    let section = dependency_section(&edges, 0);

    assert!(section.edges.is_empty());
    assert!(section.unresolved.is_empty());

    let sem = result.semantic().expect("a model");
    let collected = result.diagnostics().clone();
    let v: Value = oxabl_analyze::dump_json_with_diagnostics(sem, &collected, &section);
    assert!(
        v["dependencies"]["files"].is_array(),
        "the key is present and empty, not missing"
    );
    assert!(v["dependencies"]["unresolved"].is_array());
    assert_eq!(v["dependencies"]["index_revision"], 0);
}
