//! Edge counts over a real tree, for measuring a change to the edge set.
//!
//! Ignored: it needs a tree that is not in the repository. Run it with
//!
//! ```text
//! OXABL_EDGE_CORPUS=/path/to/tree cargo test -p oxabl_pipeline --test corpus_edges -- --ignored --nocapture
//! ```
//!
//! It resolves the tree's own `oxabl.toml`, walks it as `oxabl check` does,
//! with and without the walk's file list as the pipeline's known files,
//! prints the edge count per kind, and checks that one reused pipeline gives
//! each file the same edges whether the walk runs forwards or backwards.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use oxabl_pipeline::{ConfigOverrides, EdgeKind, EdgeTarget, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path};

/// Each file's edges, in a form two pipelines can compare: an indexed file is
/// named by its path, because each index numbers files on its own.
type Observed = BTreeMap<PathBuf, Vec<(EdgeKind, String)>>;

fn walk(pipeline: &LintPipeline<'_>, files: &[PathBuf]) -> (Observed, usize) {
    let mut observed = Observed::new();
    let mut failed = 0;
    for file in files {
        let Ok(source) = std::fs::read_to_string(file) else {
            failed += 1;
            continue;
        };
        let Ok(edges) = pipeline.with_file(file).edge_set(&source) else {
            failed += 1;
            continue;
        };
        let rows = edges
            .edges()
            .iter()
            .map(|edge| {
                let target = match &edge.target {
                    EdgeTarget::IndexedFile { file, .. } => pipeline
                        .indexed_path(*file)
                        .map_or_else(String::new, |p| p.display().to_string()),
                    other => other.key().to_string(),
                };
                (edge.kind, target)
            })
            .collect();
        observed.insert(file.clone(), rows);
    }
    (observed, failed)
}

#[test]
#[ignore = "needs OXABL_EDGE_CORPUS"]
fn corpus_edge_counts() {
    let Some(root) = std::env::var_os("OXABL_EDGE_CORPUS").map(PathBuf::from) else {
        eprintln!("OXABL_EDGE_CORPUS is not set");
        return;
    };
    let root = std::path::absolute(&root).expect("absolute root");
    let (config, warnings) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    for warning in &warnings {
        eprintln!("warning: {warning}");
    }
    let files: Vec<PathBuf> = discover_path(&root)
        .expect("walk the corpus")
        .iter()
        .map(|f| std::path::absolute(f).unwrap_or_else(|_| f.clone()))
        .collect();
    let fs = RealFileSystem;
    let reversed: Vec<PathBuf> = files.iter().rev().cloned().collect();
    println!("{}: {} files", root.display(), files.len());

    // Seeded with the walk, as `oxabl check` runs, and unseeded, as a client
    // with no file list runs. Each walk runs forwards and backwards through one
    // reused pipeline.
    let mut order_dependent = 0;
    for seeded in [true, false] {
        let pipeline = |config| {
            let pipeline = LintPipeline::new(config, &fs);
            if seeded {
                pipeline.with_known_files(&files)
            } else {
                pipeline
            }
        };
        let started = std::time::Instant::now();
        let forward = pipeline(&config);
        let (observed, failed) = walk(&forward, &files);
        let elapsed = started.elapsed();

        let mut per_kind: BTreeMap<&str, usize> = BTreeMap::new();
        for rows in observed.values() {
            for (kind, _) in rows {
                *per_kind.entry(kind.as_str()).or_default() += 1;
            }
        }
        println!(
            "  {}: {failed} without an edge set, {elapsed:.1?}",
            if seeded { "seeded" } else { "unseeded" }
        );
        for (kind, count) in &per_kind {
            println!("    {kind}: {count}");
        }

        let backward = pipeline(&config);
        let (backward_observed, _) = walk(&backward, &reversed);
        let differing: Vec<&Path> = observed
            .iter()
            .filter(|(file, rows)| backward_observed.get(*file) != Some(*rows))
            .map(|(file, _)| file.as_path())
            .collect();
        println!(
            "    files whose edges depend on walk order: {}{}",
            differing.len(),
            if differing.is_empty() {
                String::new()
            } else {
                format!(", e.g. {:?}", &differing[..differing.len().min(3)])
            }
        );
        order_dependent += differing.len();
    }
    assert_eq!(order_dependent, 0, "edges depend on walk order");
}
