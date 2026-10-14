//! Resolving the producer of a `SHARED` name from a map built once, and building
//! edges without the findings.

use std::path::PathBuf;

use oxabl_pipeline::{
    DependencyEdges, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig, SharedProducers,
};
use oxabl_workspace::InMemoryFileSystem;

const PRODUCER: &str = "DEFINE NEW SHARED VARIABLE v-site AS CHARACTER NO-UNDO.\n";
const CONSUMER: &str = "DEFINE SHARED VARIABLE v-site AS CHARACTER NO-UNDO.\nMESSAGE v-site.\n";

fn workspace() -> (InMemoryFileSystem, Vec<PathBuf>) {
    let mut fs = InMemoryFileSystem::new();
    let mut files = Vec::new();
    for (path, source) in [
        ("/proj/init.p", PRODUCER),
        ("/proj/report.p", CONSUMER),
        ("/proj/other.p", "MESSAGE \"other\".\n"),
    ] {
        fs.insert(PathBuf::from(path), source);
        files.push(PathBuf::from(path));
    }
    (fs, files)
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    }
}

/// The path each `SharedProducer` edge of `edges` names, as `run` resolves it.
fn producer_paths(run: &LintPipeline<'_>, edges: &DependencyEdges) -> Vec<PathBuf> {
    edges
        .of_kind(EdgeKind::SharedProducer)
        .map(|edge| {
            let EdgeTarget::IndexedFile { file, .. } = edge.target else {
                panic!("a producer edge names an indexed file");
            };
            run.indexed_path(file).expect("the run minted the id")
        })
        .collect()
}

#[test]
fn the_map_is_shareable_across_threads() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<SharedProducers>();
}

#[test]
fn a_consumer_links_to_its_producer_through_the_map() {
    let (fs, files) = workspace();
    let config = config();
    let producers = LintPipeline::new(&config, &fs).shared_producers(&files);
    assert_eq!(producers.len(), 1);

    let run = LintPipeline::new(&config, &fs).with_shared_producers(&producers);
    let edges = run
        .with_file("/proj/report.p")
        .edge_set(CONSUMER)
        .expect("edge set");

    assert_eq!(
        producer_paths(&run, &edges),
        vec![PathBuf::from("/proj/init.p")]
    );
}

#[test]
fn the_map_and_a_seeded_list_give_the_same_edges() {
    let (fs, files) = workspace();
    let config = config();
    let producers = LintPipeline::new(&config, &fs).shared_producers(&files);

    let mapped = LintPipeline::new(&config, &fs).with_shared_producers(&producers);
    let seeded = LintPipeline::new(&config, &fs).with_known_files(&files);
    for (path, source) in [("/proj/report.p", CONSUMER), ("/proj/init.p", PRODUCER)] {
        let via_map = mapped.with_file(path).edge_set(source).expect("edge set");
        let via_list = seeded.with_file(path).edge_set(source).expect("edge set");
        assert_eq!(via_map, via_list, "{path}");
    }
}

#[test]
fn a_file_is_not_its_own_producer() {
    let (fs, files) = workspace();
    let config = config();
    let producers = LintPipeline::new(&config, &fs).shared_producers(&files);
    let run = LintPipeline::new(&config, &fs).with_shared_producers(&producers);

    let edges = run
        .with_file("/proj/init.p")
        .edge_set(PRODUCER)
        .expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::SharedProducer).count(), 0);
}

#[test]
fn threads_that_borrow_one_map_build_the_edges_a_single_seeded_run_does() {
    let (fs, files) = workspace();
    let config = config();
    let producers = LintPipeline::new(&config, &fs).shared_producers(&files);
    let reference = LintPipeline::new(&config, &fs).with_known_files(&files);
    let expected = reference
        .with_file("/proj/report.p")
        .edge_set(CONSUMER)
        .expect("edge set");

    std::thread::scope(|scope| {
        let workers: Vec<_> = (0..4)
            .map(|_| {
                scope.spawn(|| {
                    let run = LintPipeline::new(&config, &fs).with_shared_producers(&producers);
                    run.with_file("/proj/report.p")
                        .edge_set(CONSUMER)
                        .expect("edge set")
                })
            })
            .collect();
        for worker in workers {
            assert_eq!(worker.join().expect("worker panicked"), expected);
        }
    });
}

#[test]
fn an_unseeded_run_does_not_find_a_producer_nothing_pulled_in() {
    // Why a `SharedProducer` edge needs a seed: with no file list the index has
    // seen nothing, and answers from that.
    let (fs, _files) = workspace();
    let config = config();
    let run = LintPipeline::new(&config, &fs);
    let edges = run
        .with_file("/proj/report.p")
        .edge_set(CONSUMER)
        .expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::SharedProducer).count(), 0);
}

#[test]
fn a_name_with_two_producers_is_not_linked_to_either() {
    let (mut fs, mut files) = workspace();
    fs.insert(PathBuf::from("/proj/init-too.p"), PRODUCER);
    files.push(PathBuf::from("/proj/init-too.p"));
    let config = config();
    let producers = LintPipeline::new(&config, &fs).shared_producers(&files);
    let run = LintPipeline::new(&config, &fs).with_shared_producers(&producers);

    let edges = run
        .with_file("/proj/report.p")
        .edge_set(CONSUMER)
        .expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::SharedProducer).count(), 0);
}

#[test]
fn the_edges_only_analysis_has_the_model_and_no_findings() {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let run = LintPipeline::new(&config, &fs);
    // An unused variable is a finding of the full run.
    let source = "DEFINE VARIABLE x AS INTEGER NO-UNDO.\n";

    let expansion = run.expand(source);
    let full = run.collect(&expansion);
    let lean = run.analyze(&expansion);

    assert!(full.all().count() > 0, "the control: the full run reports");
    assert_eq!(lean.all().count(), 0);
    assert!(lean.semantic().is_some());
    assert!(!lean.diagnostics().parse_recovered);
}

#[test]
fn the_edges_only_analysis_keeps_the_parse_state() {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let run = LintPipeline::new(&config, &fs);

    let expansion = run.expand("DEFINE VARIABLE x AS INTEGER NO-UNDO.\n@ @ @\n");
    let lean = run.analyze(&expansion);

    assert!(lean.diagnostics().parse_recovered);
    assert!(lean.semantic().is_some());
}
