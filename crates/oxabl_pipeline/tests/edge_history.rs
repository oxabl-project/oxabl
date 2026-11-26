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

/// A producer whose `NEW SHARED` definition comes only from its include, and a
/// consumer that names nothing that would pull the producer in.
mod producer_built_from_includes {
    use super::*;

    const DEFS: &str = "DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO.\nRUN b.p.\n";
    const B: &str = "DEFINE SHARED VARIABLE v-x AS INTEGER NO-UNDO.\nMESSAGE v-x.\n";
    const C: &str = "RUN a.p.\n";
    /// A definition that holds a `DEFINE` of its own, in a trigger block.
    const TRIGGERED: &str = "DEFINE NEW SHARED VARIABLE v-x AS CHARACTER NO-UNDO\n\
         VIEW-AS FILL-IN\n\
         TRIGGERS:\n\
         ON LEAVE DO:\n\
         DEFINE VARIABLE v-inner AS INTEGER NO-UNDO.\n\
         END.\n\
         END TRIGGERS.\n";

    fn workspace(a: &str) -> InMemoryFileSystem {
        let mut fs = InMemoryFileSystem::new();
        fs.insert(PathBuf::from("/p/a.p"), a);
        fs.insert(PathBuf::from("/p/defs.i"), DEFS);
        fs.insert(PathBuf::from("/p/head.i"), "NEW SHARED");
        fs.insert(PathBuf::from("/p/trig.i"), TRIGGERED);
        fs.insert(PathBuf::from("/p/b.p"), B);
        fs.insert(PathBuf::from("/p/c.p"), C);
        fs
    }

    fn config() -> PipelineConfig {
        PipelineConfig {
            include_paths: vec![PathBuf::from("/p")],
            ..PipelineConfig::default()
        }
    }

    fn known() -> Vec<PathBuf> {
        ["/p/a.p", "/p/b.p", "/p/c.p"]
            .into_iter()
            .map(PathBuf::from)
            .collect()
    }

    /// The consumer's edges under every way a run can be seeded and reused.
    fn every_order(a: &str) -> Vec<(&'static str, Vec<String>)> {
        let fs = workspace(a);
        let config = config();
        let known = known();
        let map = LintPipeline::new(&config, &fs).shared_producers(&known);
        let mut seen = Vec::new();

        let fresh = LintPipeline::new(&config, &fs).with_known_files(&known);
        seen.push(("seeded, fresh", observed(&fresh, "/p/b.p", B)));

        let reused = LintPipeline::new(&config, &fs).with_known_files(&known);
        observed(&reused, "/p/c.p", C);
        seen.push(("seeded, after c.p", observed(&reused, "/p/b.p", B)));

        let reused = LintPipeline::new(&config, &fs).with_known_files(&known);
        observed(&reused, "/p/a.p", a);
        seen.push(("seeded, after a.p", observed(&reused, "/p/b.p", B)));

        // Two threads, each with its own seeded index and its own share of roots.
        let first = LintPipeline::new(&config, &fs).with_known_files(&known);
        let second = LintPipeline::new(&config, &fs).with_known_files(&known);
        observed(&second, "/p/c.p", C);
        observed(&first, "/p/a.p", a);
        seen.push(("per thread, first", observed(&first, "/p/b.p", B)));
        seen.push(("per thread, second", observed(&second, "/p/b.p", B)));

        let fresh = LintPipeline::new(&config, &fs).with_shared_producers(&map);
        seen.push(("map, fresh", observed(&fresh, "/p/b.p", B)));

        let reused = LintPipeline::new(&config, &fs).with_shared_producers(&map);
        observed(&reused, "/p/c.p", C);
        seen.push(("map, after c.p", observed(&reused, "/p/b.p", B)));

        // A map that the first question builds, and that two threads share.
        let deferred = LintPipeline::new(&config, &fs).deferred_producers(&known, 2);
        let fresh = LintPipeline::new(&config, &fs).with_deferred_producers(&deferred);
        seen.push(("deferred map, fresh", observed(&fresh, "/p/b.p", B)));

        let deferred = LintPipeline::new(&config, &fs).deferred_producers(&known, 2);
        let reused = LintPipeline::new(&config, &fs).with_deferred_producers(&deferred);
        observed(&reused, "/p/c.p", C);
        seen.push(("deferred map, after c.p", observed(&reused, "/p/b.p", B)));

        let deferred = LintPipeline::new(&config, &fs).deferred_producers(&known, 2);
        let threads: Vec<Vec<String>> = std::thread::scope(|scope| {
            let workers: Vec<_> = (0..2)
                .map(|_| {
                    scope.spawn(|| {
                        let run =
                            LintPipeline::new(&config, &fs).with_deferred_producers(&deferred);
                        observed(&run, "/p/b.p", B)
                    })
                })
                .collect();
            workers
                .into_iter()
                .map(|worker| worker.join().expect("worker panicked"))
                .collect()
        });
        for edges in threads {
            seen.push(("deferred map, two threads", edges));
        }
        seen
    }

    /// Whether the map lists `a.p` for `v-x`: a file it lists that does not
    /// define the name must still get no edge.
    fn listed(a: &str) -> bool {
        let fs = workspace(a);
        let config = config();
        LintPipeline::new(&config, &fs)
            .shared_producers(&known())
            .producers_of(&oxabl_semantic::IndexName::new("v-x"))
            .contains(&PathBuf::from("/p/a.p"))
    }

    fn assert_missed_everywhere(a: &str) {
        for (how, edges) in every_order(a) {
            assert!(
                !edges
                    .iter()
                    .any(|e| e.starts_with(EdgeKind::SharedProducer.as_str())),
                "{how}: {edges:?}"
            );
        }
    }

    fn assert_found_everywhere(a: &str) {
        let want = format!(
            "{} {}",
            EdgeKind::SharedProducer.as_str(),
            Path::new("/p/a.p").display()
        );
        for (how, edges) in every_order(a) {
            let shared: Vec<&String> = edges
                .iter()
                .filter(|e| e.starts_with(EdgeKind::SharedProducer.as_str()))
                .collect();
            assert_eq!(shared.len(), 1, "{how}: {edges:?}");
            assert!(shared[0].starts_with(&want), "{how}: {edges:?}");
        }
    }

    #[test]
    fn a_producer_that_is_only_includes_is_found_in_every_order() {
        assert_found_everywhere("{defs.i}\n");
    }

    /// Not found, by the documented rule, but alike in every order.
    #[test]
    fn a_producer_with_code_of_its_own_and_an_include_is_missed_in_every_order() {
        assert_missed_everywhere(
            "DEFINE VARIABLE v-own AS INTEGER NO-UNDO.\n{defs.i}\nMESSAGE v-own.\n",
        );
    }

    /// The text as written does not parse: the index reads the expanded text,
    /// where the include supplies `NEW SHARED`.
    #[test]
    fn a_producer_whose_new_shared_comes_from_an_include_is_found_in_every_order() {
        assert_found_everywhere("DEFINE {head.i} VARIABLE v-x AS INTEGER NO-UNDO.\nRUN b.p.\n");
    }

    /// A statement that holds a `DEFINE` does not parse alone, and the index
    /// still finds the producer, written or through an include.
    #[test]
    fn a_producer_with_a_trigger_block_is_found_in_every_order() {
        assert_found_everywhere(TRIGGERED);
        assert_found_everywhere("{trig.i}\n");
        assert_found_everywhere(&format!("{TRIGGERED}{TRIGGERED}"));
        assert_found_everywhere(&format!(
            "DEFINE VARIABLE v-own AS INTEGER NO-UNDO.\n{TRIGGERED}"
        ));
    }

    /// The scan lists the second arm, but the declare pass keeps the first
    /// arm's declaration, so the file is no producer, in every order.
    #[test]
    fn a_new_shared_definition_in_a_later_exclusive_arm_is_missed_in_every_order() {
        let a = "&IF DEFINED(no-such-name) = 0 &THEN\n\
                 DEFINE VARIABLE v-x AS INTEGER NO-UNDO.\n\
                 &ELSE\n\
                 DEFINE NEW SHARED VARIABLE v-x AS INTEGER NO-UNDO.\n\
                 &ENDIF\n\
                 RUN b.p.\n";
        assert!(listed(a), "the control: the map lists the file");
        assert_missed_everywhere(a);
    }

    /// The expanded text says `NEW SHARED` but does not parse, so the file
    /// defines nothing, in every order.
    #[test]
    fn a_producer_that_does_not_parse_after_expansion_is_missed_in_every_order() {
        let a = "{defs.i}\nMESSAGE @ @ @.\n";
        assert!(listed(a), "the control: the map lists the file");
        assert_missed_everywhere(a);
    }
}
