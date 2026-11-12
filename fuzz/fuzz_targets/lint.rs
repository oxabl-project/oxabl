#![no_main]

use std::path::PathBuf;

use libfuzzer_sys::fuzz_target;
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

// The whole lint pipeline on one file (preprocess, parse, analyse, every rule) never panics.
fuzz_target!(|data: &[u8]| {
    // On the analysis stack, as in the CLI and the daemon: libFuzzer's own
    // thread is too small for the deepest tree the parser accepts.
    oxabl_common::stack::run(|| check(data));
});

fn check(data: &[u8]) {
    let source = String::from_utf8_lossy(data);
    let config = PipelineConfig::default();
    let mut fs = InMemoryFileSystem::new();
    fs.insert(
        PathBuf::from("/inc/a.i"),
        "DEFINE VARIABLE a AS INTEGER NO-UNDO.\n",
    );
    let run = LintPipeline::new(&config, &fs);
    let result = run.with_file("/work/fuzz.p").run(&source);
    assert!(
        !result.failed_run(),
        "the pipeline contained a panic: {:?}",
        result.failure()
    );
}
