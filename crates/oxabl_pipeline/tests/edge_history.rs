//! An edge set does not depend on what the pipeline that built it analysed
//! before. A build tool reuses one pipeline per worker thread, so anything a
//! previous file left in the run's index would make an edge depend on
//! scheduling.

use std::path::{Path, PathBuf};

use oxabl_pipeline::{EdgeKind, LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

const CONSUMER: &str = "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
                        MESSAGE v-site-code.\n";

/// A producer outside the walk, reachable only through a `RUN`.
const PRODUCER: &str = "DEFINE NEW SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n";

fn workspace() -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/lib/init-globals.p"), PRODUCER);
    fs.insert(PathBuf::from("/proj/starter.p"), "RUN init-globals.p.\n");
    fs.insert(PathBuf::from("/proj/report.p"), CONSUMER);
    fs
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/proj"), PathBuf::from("/lib")],
        ..PipelineConfig::default()
    }
}

/// Every edge as `(kind, target, span)`, with an indexed file named by its path:
/// an [`IndexedFileId`](oxabl_semantic::IndexedFileId) is minted per index, so
/// two pipelines number one file differently.
fn observed(pipeline: &LintPipeline<'_>, path: &str, source: &str) -> Vec<String> {
    let edges = pipeline.with_file(path).edge_set(source).expect("edge set");
    edges
        .edges()
        .iter()
        .map(|edge| {
            let target = match &edge.target {
                oxabl_pipeline::EdgeTarget::IndexedFile { file, .. } => pipeline
                    .indexed_path(*file)
                    .map_or_else(|| "?".to_string(), |p| p.display().to_string()),
                other => other.key().to_string(),
            };
            format!("{} {target} {:?}", edge.kind.as_str(), edge.span)
        })
        .collect()
}

fn shared_edges(edges: &[String]) -> usize {
    edges
        .iter()
        .filter(|e| e.starts_with(EdgeKind::SharedProducer.as_str()))
        .count()
}

#[test]
fn a_reused_pipeline_gives_the_same_edges_as_a_fresh_one() {
    let fs = workspace();
    let config = config();

    let fresh = LintPipeline::new(&config, &fs);
    let alone = observed(&fresh, "/proj/report.p", CONSUMER);

    let reused = LintPipeline::new(&config, &fs);
    observed(&reused, "/proj/starter.p", "RUN init-globals.p.\n");
    let after = observed(&reused, "/proj/report.p", CONSUMER);

    assert_eq!(alone, after, "the earlier file changed this file's edges");
    assert_eq!(
        shared_edges(&after),
        0,
        "nothing this file reaches produces it"
    );
}

#[test]
fn a_reused_seeded_pipeline_gives_the_same_edges_as_a_fresh_one() {
    let fs = workspace();
    let config = config();
    let known = [
        PathBuf::from("/proj/starter.p"),
        PathBuf::from("/proj/report.p"),
    ];

    let fresh = LintPipeline::new(&config, &fs).with_known_files(&known);
    let alone = observed(&fresh, "/proj/report.p", CONSUMER);

    let reused = LintPipeline::new(&config, &fs).with_known_files(&known);
    observed(&reused, "/proj/starter.p", "RUN init-globals.p.\n");
    let after = observed(&reused, "/proj/report.p", CONSUMER);

    assert_eq!(alone, after, "the earlier file changed this file's edges");
}

#[test]
fn a_producer_the_file_runs_itself_is_linked_however_the_pipeline_was_used() {
    let fs = workspace();
    let config = config();
    let source = "DEFINE SHARED VARIABLE v-site-code AS CHARACTER NO-UNDO.\n\
                  RUN init-globals.p.\n\
                  MESSAGE v-site-code.\n";

    let fresh = LintPipeline::new(&config, &fs);
    let alone = observed(&fresh, "/proj/report.p", source);

    let reused = LintPipeline::new(&config, &fs);
    observed(&reused, "/proj/starter.p", "RUN init-globals.p.\n");
    let after = observed(&reused, "/proj/report.p", source);

    assert_eq!(alone, after);
    assert_eq!(shared_edges(&after), 1, "got {after:?}");
    assert!(
        after
            .iter()
            .any(|e| e.contains(&Path::new("/lib/init-globals.p").display().to_string())),
        "got {after:?}"
    );
}

#[test]
fn a_reused_pipeline_with_a_shared_producer_map_gives_the_same_edges_as_a_fresh_one() {
    let fs = workspace();
    let config = config();
    let known = [
        PathBuf::from("/proj/starter.p"),
        PathBuf::from("/proj/report.p"),
    ];
    let producers = LintPipeline::new(&config, &fs).shared_producers(&known);

    let fresh = LintPipeline::new(&config, &fs).with_shared_producers(&producers);
    let alone = observed(&fresh, "/proj/report.p", CONSUMER);

    let reused = LintPipeline::new(&config, &fs).with_shared_producers(&producers);
    observed(&reused, "/proj/starter.p", "RUN init-globals.p.\n");
    let after = observed(&reused, "/proj/report.p", CONSUMER);

    assert_eq!(alone, after, "the earlier file changed this file's edges");
    assert_eq!(
        shared_edges(&after),
        0,
        "the map holds no producer of the name"
    );
}
