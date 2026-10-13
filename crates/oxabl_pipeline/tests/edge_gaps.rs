//! Outcomes of the edge set that need more than the in-memory parity table can
//! express: an include that resolves but cannot be read, a program that `RUN`s
//! itself, and the degraded files a workspace pass reports.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use oxabl_pipeline::{
    Completeness, DegradedReason, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig, ReverseGraph,
};
use oxabl_semantic::UnresolvedReason;
use oxabl_workspace::{FileSystem, InMemoryFileSystem, RealFileSystem};

/// An in-memory filesystem in which chosen paths exist and cannot be read.
struct Unreadable {
    inner: InMemoryFileSystem,
    unreadable: Vec<PathBuf>,
}

impl FileSystem for Unreadable {
    fn read(&self, path: &Path) -> Result<Arc<str>, io::Error> {
        if self.unreadable.iter().any(|p| p == path) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "stream did not contain valid UTF-8",
            ));
        }
        self.inner.read(path)
    }

    fn exists(&self, path: &Path) -> bool {
        self.unreadable.iter().any(|p| p == path) || self.inner.exists(path)
    }
}

fn config(root: &str) -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from(root)],
        ..PipelineConfig::default()
    }
}

#[test]
fn an_unreadable_include_stays_an_edge_and_leaves_a_row() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/ok.i"), "MESSAGE 1.\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let edges = pipeline.edge_set("{ok.i}\n{latin.i}\n").expect("edge set");

    let includes: Vec<_> = edges
        .of_kind(EdgeKind::DirectInclude)
        .map(|e| e.target.key().to_string())
        .collect();
    assert_eq!(includes, ["/proj/latin.i", "/proj/ok.i"]);
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::DirectInclude);
    assert_eq!(rows[0].name, "latin.i");
    assert_eq!(rows[0].reason, UnresolvedReason::PresentButUnusable);
    assert!(rows[0].span.is_some());
    assert_eq!(
        edges.completeness(),
        &Completeness::Degraded {
            reasons: vec![DegradedReason::UnreadableInclude]
        }
    );
}

#[test]
fn a_nested_unreadable_include_is_a_transitive_edge_with_a_row() {
    let mut inner = InMemoryFileSystem::new();
    inner.insert(PathBuf::from("/proj/mid.i"), "{latin.i}\n");
    let fs = Unreadable {
        inner,
        unreadable: vec![PathBuf::from("/proj/latin.i")],
    };
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/main.p");

    let edges = pipeline.edge_set("{mid.i}\n").expect("edge set");

    let transitive: Vec<_> = edges
        .of_kind(EdgeKind::TransitiveInclude)
        .map(|e| e.target.key().to_string())
        .collect();
    assert_eq!(transitive, ["/proj/latin.i"]);
    let rows = edges.unresolved();
    assert_eq!(rows.len(), 1, "got {rows:?}");
    assert_eq!(rows[0].kind, EdgeKind::TransitiveInclude);
    assert_eq!(rows[0].reason, UnresolvedReason::PresentButUnusable);
    assert_eq!(rows[0].span, None);
}

#[test]
fn a_non_utf8_include_on_a_real_filesystem_is_an_edge_and_a_row() {
    let dir = tempfile::tempdir().expect("temp dir");
    std::fs::write(dir.path().join("latin.i"), b"MESSAGE \"caf\xe9\".\n").expect("write");
    let main = dir.path().join("main.p");
    let config = PipelineConfig {
        include_paths: vec![dir.path().to_path_buf()],
        ..PipelineConfig::default()
    };
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file(main);

    let edges = pipeline.edge_set("{latin.i}\n").expect("edge set");

    assert_eq!(edges.of_kind(EdgeKind::DirectInclude).count(), 1);
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
}

#[test]
fn a_program_that_runs_itself_depends_on_nothing_and_leaves_no_row() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/loop.p"), "RUN loop.p.\n");
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/loop.p");

    let edges = pipeline.edge_set("RUN loop.p.\n").expect("edge set");

    assert!(edges.edges().is_empty(), "got {:?}", edges.edges());
    assert!(
        edges.unresolved().is_empty(),
        "got {:?}",
        edges.unresolved()
    );
}

#[test]
fn an_unparsable_program_target_is_an_edge_to_the_located_file() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/proj/sub.p"),
        "&IF DEFINED(x) = 0 &THEN\nDO:\n&ENDIF\n  MESSAGE 1.\nEND.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/run.p");

    let edges = pipeline.edge_set("RUN sub.p.\n").expect("edge set");

    let targets: Vec<_> = edges.of_kind(EdgeKind::ProgramReference).collect();
    assert_eq!(targets.len(), 1);
    let EdgeTarget::IndexedFile { file, .. } = targets[0].target else {
        panic!("a program edge names an indexed file");
    };
    assert_eq!(run.indexed_path(file), Some(PathBuf::from("/proj/sub.p")));
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
    assert!(edges.completeness().is_complete());
}

#[test]
fn an_unparsable_class_target_is_an_edge_to_the_located_file() {
    let mut fs = InMemoryFileSystem::new();
    // Inheriting is what records a class lookup: it happens when an inherited
    // name is looked up, so the child calls one.
    fs.insert(
        PathBuf::from("/proj/orders/base.cls"),
        "{header.i}\nEND CLASS.\n",
    );
    let config = config("/proj");
    let run = LintPipeline::new(&config, &fs);
    let pipeline = run.with_file("/proj/orders/child.cls");

    let edges = pipeline
        .edge_set("CLASS orders.child INHERITS orders.base:\n  METHOD PUBLIC VOID go():\n    MESSAGE calc-total().\n  END METHOD.\nEND CLASS.\n")
        .expect("edge set");

    let targets: Vec<_> = edges.of_kind(EdgeKind::ClassReference).collect();
    assert_eq!(targets.len(), 1, "got {:?}", edges.edges());
    assert_eq!(edges.unresolved().len(), 1);
    assert_eq!(
        edges.unresolved()[0].reason,
        UnresolvedReason::PresentButUnusable
    );
}

#[test]
fn the_reverse_graph_reports_degraded_files_beside_unanalysed_ones() {
    let mut fs = InMemoryFileSystem::new();
    let clean = PathBuf::from("/proj/clean.p");
    let broken = PathBuf::from("/proj/broken.p");
    let gappy = PathBuf::from("/proj/gappy.p");
    fs.insert(clean.clone(), "MESSAGE 1.\n");
    fs.insert(broken.clone(), "@ @ @\nMESSAGE 1.\n");
    fs.insert(gappy.clone(), "{nowhere.i}\n");
    let files = [clean, broken.clone(), gappy.clone()];
    let config = config("/proj");
    let pipeline = LintPipeline::new(&config, &fs).with_known_files(&files);

    let graph = ReverseGraph::build(&pipeline, &files);

    assert!(graph.unanalysed().is_empty());
    let degraded: Vec<_> = graph
        .degraded()
        .iter()
        .map(|d| (d.file.clone(), d.reasons.clone()))
        .collect();
    assert_eq!(
        degraded,
        [
            (broken, vec![DegradedReason::ParseRecovered]),
            (gappy, vec![DegradedReason::UnresolvedInclude]),
        ]
    );
}
