//! Measure all strict groups with the shared lint pipeline.
//! Set OXABL_STRICT_CORPUS to a tree. Emits JSON lines for corpus comparisons.
use std::path::PathBuf;
use std::time::Instant;

use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path, walk_include_fragments};

fn main() {
    let root =
        PathBuf::from(std::env::var_os("OXABL_STRICT_CORPUS").expect("set OXABL_STRICT_CORPUS"));
    let (mut config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for w in warnings {
        eprintln!("warning: {w}");
    }
    for group in [
        "strict.transactions",
        "strict.locks",
        "strict.access",
        "strict.handles",
        "strict.calls",
        "strict.nulls",
    ] {
        config.lint_severities.base_mut().enable_group(group);
    }
    let mut files = discover_path(&root).expect("discover");
    files.sort();
    let fs = RealFileSystem;
    let run = LintPipeline::new(&config, &fs).with_known_files(&files);
    let start = Instant::now();
    let mut failed = 0;
    for (path, fragment) in files
        .iter()
        .map(|p| (p.clone(), false))
        .chain(walk_include_fragments(&root).into_iter().map(|p| (p, true)))
    {
        let Ok(bytes) = std::fs::read(&path) else {
            failed += 1;
            continue;
        };
        let text = String::from_utf8_lossy(&bytes);
        let handle = run.with_file(&path);
        let result = if fragment {
            handle.run_context_free(&text)
        } else {
            handle.run(&text)
        };
        if result.failed_run() {
            failed += 1;
        }
        for d in result.all().filter(|d| {
            d.diagnostic
                .code
                .0
                .starts_with(['T', 'L', 'D', 'H', 'C', 'N'])
                && !d.diagnostic.code.0.starts_with("LINT")
        }) {
            let d = &d.diagnostic;
            println!(
                "{}",
                serde_json::json!({"file":path, "code":d.code.0, "start":d.span.span.start, "end":d.span.span.end, "message":d.message})
            );
        }
    }
    eprintln!(
        "files={} failed={failed} wall={:?}",
        files.len(),
        start.elapsed()
    );
}
