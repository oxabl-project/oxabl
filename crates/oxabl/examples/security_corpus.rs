//! Compare the two modes of the security rules over a tree.
//! Set OXABL_SECURITY_CORPUS to a tree. Prints the findings per rule in stage 1
//! (`sink`) and stage 2 (`taint`), and the reduction.
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Instant;

use oxabl_common::{SecurityMode, SecurityOptions};
use oxabl_pipeline::{ConfigOverrides, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path, walk_include_fragments};

fn run(root: &Path, mode: SecurityMode) -> (BTreeMap<&'static str, usize>, usize) {
    let (mut config, warnings) = PipelineConfig::resolve(root, &ConfigOverrides::default());
    for w in warnings {
        eprintln!("warning: {w}");
    }
    config.lint_severities.base_mut().enable_group("security");
    let options = SecurityOptions {
        mode,
        ..config.lint_severities.security().clone()
    };
    config.lint_severities.set_security(options);
    let mut files = discover_path(root).expect("discover");
    files.sort();
    let fs = RealFileSystem;
    let pipeline = LintPipeline::new(&config, &fs).with_known_files(&files);
    let mut counts = BTreeMap::new();
    let mut failed = 0;
    for (path, fragment) in files
        .iter()
        .map(|p| (p.clone(), false))
        .chain(walk_include_fragments(root).into_iter().map(|p| (p, true)))
    {
        let Ok(bytes) = std::fs::read(&path) else {
            failed += 1;
            continue;
        };
        let text = String::from_utf8_lossy(&bytes);
        let handle = pipeline.with_file(&path);
        let result = if fragment {
            handle.run_context_free(&text)
        } else {
            handle.run(&text)
        };
        if result.failed_run() {
            failed += 1;
        }
        for d in result.all() {
            let code = d.diagnostic.code.0;
            if code.starts_with("ABLSEC") {
                *counts.entry(code).or_insert(0) += 1;
            }
        }
    }
    (counts, failed)
}

fn main() {
    let root = PathBuf::from(
        std::env::var_os("OXABL_SECURITY_CORPUS").expect("set OXABL_SECURITY_CORPUS"),
    );
    let start = Instant::now();
    let (sink, failed) = run(&root, SecurityMode::Sink);
    let (taint, _) = run(&root, SecurityMode::Taint);
    let codes: std::collections::BTreeSet<&str> =
        sink.keys().chain(taint.keys()).copied().collect();
    println!("rule       stage1  stage2  reduction");
    let (mut a, mut b) = (0, 0);
    for code in codes {
        let (s, t) = (
            sink.get(code).copied().unwrap_or(0),
            taint.get(code).copied().unwrap_or(0),
        );
        a += s;
        b += t;
        println!("{code}  {s:>6}  {t:>6}  {:>8}", reduction(s, t));
    }
    println!("all        {a:>6}  {b:>6}  {:>8}", reduction(a, b));
    eprintln!("failed={failed} wall={:?}", start.elapsed());
}

fn reduction(before: usize, after: usize) -> String {
    if before == 0 {
        return "-".into();
    }
    format!(
        "{:.0}%",
        100.0 * (before as f64 - after as f64) / before as f64
    )
}
