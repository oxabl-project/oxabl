//! Encrypted source in the shared pipeline: the root and an include are named
//! once, whichever filesystem holds them, and the edge set says it is
//! incomplete. The text is synthetic.

use std::path::PathBuf;

use oxabl_pipeline::{Completeness, DegradedReason, LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

/// Header bytes of encrypted files, then bytes no ABL holds.
fn encrypted() -> String {
    let mut text = String::from("\u{13}Y/%\u{7}\t1I;%");
    for i in 0..400u32 {
        text.push(char::from_u32(0x20 + (i * 37) % 0x5f).unwrap());
        text.push(char::from_u32(1 + (i * 11) % 0x1f).unwrap_or(' '));
    }
    text
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/proj")],
        ..PipelineConfig::default()
    }
}

fn codes(result: &oxabl_pipeline::LintResult) -> Vec<&'static str> {
    result.all().map(|d| d.diagnostic.code.0).collect()
}

#[test]
fn an_encrypted_root_gives_one_finding_and_no_model() {
    let fs = InMemoryFileSystem::new();
    let config = config();
    let base = LintPipeline::new(&config, &fs);
    let pipeline = base.with_file("/proj/main.p");

    let result = pipeline.run(&encrypted());
    assert_eq!(codes(&result), ["SOURCE001"]);
    assert!(result.semantic().is_none());
    assert!(!result.failed_run());
    assert!(
        pipeline.edge_set(&encrypted()).is_err(),
        "no edges are claimed for a file that was not read"
    );
}

#[test]
fn an_encrypted_include_gives_one_finding_and_an_incomplete_edge_set() {
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/proj/enc.i"), encrypted());
    let config = config();
    let base = LintPipeline::new(&config, &fs);
    let pipeline = base.with_file("/proj/main.p");
    let source = "{enc.i}\nMESSAGE \"x\".\n";

    let result = pipeline.run(source);
    assert_eq!(codes(&result), ["SOURCE001"]);
    let message = &result.all().next().unwrap().diagnostic.message;
    assert!(
        message.contains("include 'enc.i' is encrypted, not checked"),
        "{message}"
    );

    let edges = pipeline.edge_set(source).expect("edge set");
    assert_eq!(
        edges.completeness(),
        &Completeness::Degraded {
            reasons: vec![DegradedReason::UnreadableInclude]
        }
    );
}
