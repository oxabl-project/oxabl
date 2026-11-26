//! The findings an include gets from one root's compilation unit cost one
//! path resolution for each distinct file, not one for each finding. The
//! daemon runs this each time the include or the root changes, so a resolution
//! for each finding costs the findings times a system call while the user
//! types.
//!
//! A test build counts the resolutions of `canonical_path` for the label of
//! the enclosing `oxabl_common::test_tally::within`. All fixtures are
//! synthetic ABL written for this file.

use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::Arc;

use oxabl_daemon::canonical_root;
use oxabl_daemon::db::{
    AnalysisConfig, AnalysisDatabase, ContextDiagnostics, SchemaHandle, compute_context_diagnostics,
};
use oxabl_pipeline::PipelineConfig;

const EVENT: &str = "session.canonical";

/// `count` unused variables: each is one finding in the include.
fn declarations(count: usize) -> String {
    (0..count)
        .map(|i| format!("DEFINE VARIABLE spare-{i} AS INTEGER NO-UNDO.\n"))
        .collect()
}

/// The answer for `inc.i` in the compilation unit of `prog.p`, and the
/// resolutions it cost, for an include of `findings` unused variables that the
/// root reads `times` times.
fn context(label: &str, findings: usize, times: usize) -> (ContextDiagnostics, usize) {
    let dir = tempfile::tempdir().unwrap();
    let root = canonical_root(dir.path()).unwrap();
    fs::write(root.join("inc.i"), declarations(findings)).unwrap();
    let program: String = (0..times)
        .map(|i| format!("PROCEDURE p{i}:\n{{inc.i}}\nEND PROCEDURE.\n"))
        .collect();
    fs::write(root.join("prog.p"), program).unwrap();
    let db = AnalysisDatabase::new(AnalysisConfig {
        pipeline: Arc::new(PipelineConfig {
            include_paths: vec![root.clone()],
            ..PipelineConfig::default()
        }),
        ..AnalysisConfig::default()
    });
    let schema = SchemaHandle::new(&db, 0);
    let answer = oxabl_common::test_tally::within(
        || label.to_string(),
        || {
            compute_context_diagnostics(
                &db,
                schema,
                &root.join("prog.p"),
                &root.join("inc.i"),
                HashMap::new(),
            )
        },
    )
    .expect("no write cancelled the read");
    (answer, resolutions(label))
}

fn resolutions(label: &str) -> usize {
    oxabl_common::test_tally::counts()
        .expect("the test build counts events")
        .into_iter()
        .filter(|(name, event, _)| name == label && *event == EVENT)
        .map(|(.., count)| count)
        .sum()
}

fn names(answer: &ContextDiagnostics) -> Vec<&Path> {
    answer.findings.iter().map(|f| f.path.as_path()).collect()
}

#[test]
fn each_finding_of_an_include_costs_no_resolution_of_its_own() {
    let (few, few_cost) = context("context-few", 2, 1);
    let (many, many_cost) = context("context-many", 80, 1);
    assert_eq!(names(&few).len(), 2);
    assert_eq!(names(&many).len(), 80);
    assert!(few_cost > 0, "the build counted no resolution");
    assert_eq!(few_cost, many_cost);
}

#[test]
fn each_read_of_an_include_costs_no_resolution_while_no_buffer_is_open() {
    let (once, once_cost) = context("context-once", 3, 1);
    let (many, many_cost) = context("context-repeated", 3, 20);
    assert_eq!(names(&once).len(), 3);
    assert_eq!(names(&many).len(), 3);
    assert!(once_cost > 0, "the build counted no resolution");
    assert_eq!(once_cost, many_cost);
}
