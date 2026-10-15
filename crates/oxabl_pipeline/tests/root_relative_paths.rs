//! `[workspace.sources].root_relative_include_paths`: each root searches its own
//! directory, and fixed subdirectories of it, before the include paths, in one
//! run over roots in many directories.

use std::path::PathBuf;

use oxabl_pipeline::{EdgeKind, EdgeTarget, LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

fn fs(files: &[(&str, &str)]) -> InMemoryFileSystem {
    let mut fs = InMemoryFileSystem::new();
    for (path, text) in files {
        fs.insert(PathBuf::from(*path), *text);
    }
    fs
}

fn config(root_relative: &[&str]) -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/ws/shared")],
        root_relative_include_paths: root_relative.iter().map(PathBuf::from).collect(),
        ..PipelineConfig::default()
    }
}

const FILES: &[(&str, &str)] = &[
    ("/ws/a/main.p", "{own.i}\n{sub.i}\n{common.i}\n"),
    ("/ws/a/own.i", "MESSAGE 'a own'.\n"),
    ("/ws/a/rf/sub.i", "MESSAGE 'a rf'.\n"),
    ("/ws/b/main.p", "{own.i}\n{sub.i}\n{common.i}\n"),
    ("/ws/b/own.i", "MESSAGE 'b own'.\n"),
    ("/ws/b/rf/sub.i", "MESSAGE 'b rf'.\n"),
    ("/ws/shared/common.i", "MESSAGE 'shared'.\n"),
    ("/ws/shared/own.i", "MESSAGE 'shared own'.\n"),
];

fn includes(run: &LintPipeline<'_>, file: &str, fs: &InMemoryFileSystem) -> Vec<String> {
    let source = oxabl_workspace::FileSystem::read(fs, std::path::Path::new(file)).unwrap();
    let edges = run.with_file(file).edge_set(&source).expect("edge set");
    let mut paths: Vec<String> = edges
        .of_kind(EdgeKind::DirectInclude)
        .map(|e| e.target.key().to_string())
        .collect();
    paths.extend(
        edges
            .unresolved()
            .iter()
            .map(|u| format!("unresolved {}", u.name)),
    );
    paths
}

#[test]
fn each_root_reads_its_own_directory_first_in_one_run() {
    let fs = fs(FILES);
    let config = config(&[".", "rf"]);
    let run = LintPipeline::new(&config, &fs);

    assert_eq!(
        includes(&run, "/ws/a/main.p", &fs),
        ["/ws/a/own.i", "/ws/a/rf/sub.i", "/ws/shared/common.i"]
    );
    assert_eq!(
        includes(&run, "/ws/b/main.p", &fs),
        ["/ws/b/own.i", "/ws/b/rf/sub.i", "/ws/shared/common.i"]
    );
}

#[test]
fn without_the_setting_only_the_include_paths_are_searched() {
    let fs = fs(FILES);
    let config = config(&[]);
    let run = LintPipeline::new(&config, &fs);
    assert_eq!(
        includes(&run, "/ws/a/main.p", &fs),
        [
            "/ws/shared/common.i",
            "/ws/shared/own.i",
            "unresolved sub.i"
        ]
    );
}

const CLASSES: &[(&str, &str)] = &[
    (
        "/ws/a/main.p",
        "DEFINE VARIABLE o AS util.Helper NO-UNDO.\no = NEW util.Helper().\nRUN work.p.\n",
    ),
    (
        "/ws/a/util/Helper.cls",
        "CLASS util.Helper:\n  {helper-body.i}\nEND CLASS.\n",
    ),
    (
        "/ws/a/util/helper-body.i",
        "METHOD PUBLIC VOID Go():\nEND METHOD.\n",
    ),
    ("/ws/a/work.p", "MESSAGE 'a work'.\n"),
    (
        "/ws/shared/util/Helper.cls",
        "CLASS util.Helper:\nEND CLASS.\n",
    ),
    ("/ws/shared/work.p", "MESSAGE 'shared work'.\n"),
    ("/ws/b/main.p", "o = NEW util.Helper().\nRUN work.p.\n"),
];

#[test]
fn class_and_run_lookups_search_the_roots_directory_first() {
    let fs = fs(CLASSES);
    let config = config(&["."]);
    let run = LintPipeline::new(&config, &fs);
    let targets = |file: &str| -> Vec<(EdgeKind, PathBuf)> {
        let source = oxabl_workspace::FileSystem::read(&fs, std::path::Path::new(file)).unwrap();
        let edges = run.with_file(file).edge_set(&source).expect("edge set");
        assert!(edges.unresolved().is_empty(), "{:?}", edges.unresolved());
        edges
            .edges()
            .iter()
            .filter_map(|e| match &e.target {
                EdgeTarget::IndexedFile { file, .. } => Some((e.kind, run.indexed_path(*file)?)),
                _ => None,
            })
            .collect()
    };

    assert_eq!(
        targets("/ws/a/main.p"),
        [
            (
                EdgeKind::ClassReference,
                PathBuf::from("/ws/a/util/Helper.cls")
            ),
            (EdgeKind::ProgramReference, PathBuf::from("/ws/a/work.p")),
        ]
    );
    assert_eq!(
        targets("/ws/b/main.p"),
        [
            (
                EdgeKind::ClassReference,
                PathBuf::from("/ws/shared/util/Helper.cls")
            ),
            (
                EdgeKind::ProgramReference,
                PathBuf::from("/ws/shared/work.p")
            ),
        ]
    );
}
