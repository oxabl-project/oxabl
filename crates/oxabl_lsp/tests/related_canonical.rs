//! The related information of an include's diagnostics costs one path
//! resolution for each file the labels point into, not one for each label.
//! `canonical_path` asks the file system, and a server publishes these on
//! every refresh while the user types.
//!
//! A test build counts the resolutions of `canonical_path` for the label of
//! the enclosing `oxabl_common::test_tally::within`. All fixtures are
//! synthetic ABL written for this file.

use std::collections::HashMap;
use std::fs;
use std::sync::Arc;

use lsp_types::{PositionEncodingKind, Uri};
use oxabl_daemon::canonical_root;
use oxabl_daemon::db::{
    AnalysisConfig, AnalysisDatabase, SchemaHandle, compute_context_diagnostics,
};
use oxabl_lsp::diagnostics::context_to_lsp_diagnostics;
use oxabl_pipeline::PipelineConfig;
use ropey::Rope;

const EVENT: &str = "session.canonical";

/// The related locations of the diagnostics of `inc.i` in the unit of
/// `prog.p`, and the resolutions that placing them cost. The root declares
/// `dup`, and the include declares it `count` more times: each is a finding in
/// the include with a label at the declaration in the root.
fn placed(label: &str, count: usize) -> (usize, usize) {
    let dir = tempfile::tempdir().unwrap();
    let root = canonical_root(dir.path()).unwrap();
    let include_text = "DEFINE VARIABLE dup AS INTEGER NO-UNDO.\n".repeat(count);
    fs::write(root.join("inc.i"), &include_text).unwrap();
    fs::write(
        root.join("prog.p"),
        "DEFINE VARIABLE dup AS INTEGER NO-UNDO.\n{inc.i}\nMESSAGE dup.\n",
    )
    .unwrap();
    let db = AnalysisDatabase::new(AnalysisConfig {
        pipeline: Arc::new(PipelineConfig {
            include_paths: vec![root.clone()],
            ..PipelineConfig::default()
        }),
        ..AnalysisConfig::default()
    });
    let schema = SchemaHandle::new(&db, 0);
    let context = compute_context_diagnostics(
        &db,
        schema,
        &root.join("prog.p"),
        &root.join("inc.i"),
        HashMap::new(),
    )
    .expect("no write cancelled the read");
    assert_eq!(context.findings.len(), count, "{context:?}");
    let uri: Uri = format!("file://{}", root.join("inc.i").display())
        .parse()
        .unwrap();
    let rope = Rope::from_str(&include_text);
    let diagnostics = oxabl_common::test_tally::within(
        || label.to_string(),
        || {
            context_to_lsp_diagnostics(
                &context,
                &root.join("prog.p"),
                &rope,
                &PositionEncodingKind::UTF16,
                Some(&uri),
            )
        },
    );
    let related = diagnostics
        .iter()
        .map(|d| d.related_information.as_ref().map_or(0, Vec::len))
        .sum();
    let resolutions = oxabl_common::test_tally::counts()
        .expect("the test build counts events")
        .into_iter()
        .filter(|(name, event, _)| name == label && *event == EVENT)
        .map(|(.., count)| count)
        .sum();
    (related, resolutions)
}

#[test]
fn each_label_in_another_file_costs_no_resolution_of_its_own() {
    let (few_related, few) = placed("related-few", 2);
    let (many_related, many) = placed("related-many", 80);
    assert_eq!(few_related, 2, "one label for each finding");
    assert_eq!(many_related, 80, "one label for each finding");
    assert!(few > 0, "the build counted no resolution");
    assert_eq!(few, many);
}
