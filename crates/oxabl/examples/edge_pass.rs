//! edge_pass — build the dependency edge set of every ABL root under a tree on N
//! threads, one index per thread, and report the wall time. The shape of a build
//! tool that wants only edges for every root of a workspace.
//!
//! `--seeding` picks how a thread's index finds the producer of a `SHARED` name:
//!
//! * `shared` (default): one producer map, scanned on the run's threads once and
//!   borrowed by every thread.
//! * `per-thread`: every thread scans the whole file list itself.
//! * `unseeded`: no file list. `SharedProducer` edges then depend on what a thread
//!   has looked at, so they differ between thread counts.
//!
//! `--analysis full` builds each set through the full collect, lint rules included,
//! as `edge_set` did before it skipped them. It exists to measure what that costs.
//!
//! `--dump <file>` writes every edge set, in root order, with each indexed file
//! resolved to a path. Two dumps are equal when two runs built the same edges.
//!
//! `--closure on` adds the closure of the conditional includes to every set.
//! `--root-relative .,rf` replaces the tree's root-relative include paths.
//!
//! # Usage
//! ```text
//! cargo run -p oxabl --release --example edge_pass -- \
//!     <root> [--threads N] [--seeding shared|per-thread|unseeded] \
//!     [--analysis edges|full] [--dump file] [--closure on] [--root-relative LIST]
//! ```

use std::fmt::Write as _;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::Instant;

use oxabl_pipeline::{ConfigOverrides, EdgeTarget, LintPipeline, PipelineConfig};
use oxabl_workspace::{RealFileSystem, discover_path};

#[derive(Clone, Copy, PartialEq)]
enum Seeding {
    Shared,
    PerThread,
    Unseeded,
}

/// The edge set of `source`, through the analysis the run asked for.
fn edge_set(
    pipeline: &LintPipeline<'_>,
    source: &str,
    full: bool,
) -> Result<oxabl_pipeline::DependencyEdges, String> {
    if !full {
        return pipeline.edge_set(source);
    }
    let expansion = pipeline.expand(source);
    let result = pipeline.collect(&expansion);
    pipeline.edges_of(&expansion, &result)
}

fn main() {
    let mut args = std::env::args().skip(1);
    let Some(root) = args.next().map(PathBuf::from) else {
        eprintln!("usage: edge_pass <root> [--threads N] [--seeding MODE] [--dump FILE]");
        std::process::exit(2);
    };
    let mut threads = 1usize;
    let mut seeding = Seeding::Shared;
    let mut full = false;
    let mut dump: Option<PathBuf> = None;
    let mut closure = false;
    let mut root_relative: Option<Vec<PathBuf>> = None;
    while let Some(flag) = args.next() {
        let value = args.next().unwrap_or_default();
        match flag.as_str() {
            "--threads" => threads = value.parse().unwrap_or(1).max(1),
            "--seeding" => {
                seeding = match value.as_str() {
                    "shared" => Seeding::Shared,
                    "per-thread" => Seeding::PerThread,
                    "unseeded" => Seeding::Unseeded,
                    other => {
                        eprintln!("unknown seeding `{other}`");
                        std::process::exit(2);
                    }
                }
            }
            "--analysis" => full = value == "full",
            "--dump" => dump = Some(PathBuf::from(value)),
            "--closure" => closure = value == "on",
            "--root-relative" => {
                root_relative = Some(
                    value
                        .split(',')
                        .filter(|entry| !entry.is_empty())
                        .map(PathBuf::from)
                        .collect(),
                );
            }
            other => {
                eprintln!("unknown flag `{other}`");
                std::process::exit(2);
            }
        }
    }

    let files: Vec<PathBuf> = match discover_path(&root) {
        Ok(files) => files
            .iter()
            .map(|f| std::path::absolute(f).unwrap_or_else(|_| f.clone()))
            .collect(),
        Err(e) => {
            eprintln!("error: {e}");
            std::process::exit(2);
        }
    };
    let (mut config, _) = PipelineConfig::resolve(&root, &ConfigOverrides::default());
    if let Some(paths) = root_relative {
        config.root_relative_include_paths = paths;
    }
    let fs = RealFileSystem;

    let started = Instant::now();
    // Counted in the wall time: it is the price of the shared strategy. Scanned
    // on the run's threads, as a client that has them would.
    let producers = (seeding == Seeding::Shared)
        .then(|| LintPipeline::new(&config, &fs).shared_producers_parallel(&files, threads));
    let setup = started.elapsed();

    let next = AtomicUsize::new(0);
    let mut rows: Vec<(usize, String)> = Vec::new();
    std::thread::scope(|scope| {
        let workers: Vec<_> = (0..threads)
            .map(|_| {
                scope.spawn(|| {
                    let run =
                        LintPipeline::new(&config, &fs).with_conditional_include_closure(closure);
                    let run = match (seeding, &producers) {
                        (Seeding::Shared, Some(map)) => run.with_shared_producers(map),
                        (Seeding::PerThread, _) => run.with_known_files(&files),
                        _ => run,
                    };
                    let mut mine = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(path) = files.get(i) else { break };
                        let mut text = String::new();
                        let _ = writeln!(text, "ROOT {}", path.display());
                        match std::fs::read_to_string(path) {
                            Err(e) => {
                                let _ = writeln!(text, "READERR {e}");
                            }
                            Ok(source) => {
                                match edge_set(&run.with_file(path.clone()), &source, full) {
                                    Err(reason) => {
                                        let _ = writeln!(text, "ERR {reason}");
                                    }
                                    Ok(edges) => {
                                        for e in edges.edges() {
                                            let file = match &e.target {
                                                EdgeTarget::IndexedFile { file, .. } => run
                                                    .indexed_path(*file)
                                                    .map(|p| p.display().to_string())
                                                    .unwrap_or_else(|| "?".into()),
                                                _ => String::new(),
                                            };
                                            let _ = writeln!(
                                                text,
                                                "E {:?} {} | {} | {:?} | {:?}",
                                                e.kind,
                                                e.target.key(),
                                                file,
                                                e.span,
                                                e.include_site
                                            );
                                        }
                                        for u in edges.unresolved() {
                                            let _ = writeln!(text, "U {u:?}");
                                        }
                                        for c in edges.conditional_includes() {
                                            let _ = writeln!(text, "I {c:?}");
                                        }
                                        for c in edges.conditional_references() {
                                            let file = match &c.target {
                                                Some(EdgeTarget::IndexedFile { file, .. }) => run
                                                    .indexed_path(*file)
                                                    .map(|p| p.display().to_string())
                                                    .unwrap_or_else(|| "?".into()),
                                                _ => String::new(),
                                            };
                                            let _ = writeln!(
                                                text,
                                                "R {:?} {} {} | {} | {:?} | {:?}",
                                                c.kind,
                                                c.syntax.as_str(),
                                                c.name,
                                                file,
                                                c.span,
                                                c.include_site
                                            );
                                        }
                                        for c in edges.conditional_include_closure() {
                                            let _ = writeln!(text, "L {c:?}");
                                        }
                                        let _ = writeln!(text, "C {:?}", edges.completeness());
                                    }
                                }
                            }
                        }
                        mine.push((i, text));
                    }
                    mine
                })
            })
            .collect();
        for worker in workers {
            rows.extend(worker.join().expect("worker panicked"));
        }
    });
    let wall = started.elapsed();

    eprintln!(
        "threads={threads} roots={} setup={:.2}s wall={:.2}s",
        rows.len(),
        setup.as_secs_f64(),
        wall.as_secs_f64()
    );
    if let Some(dump) = dump {
        rows.sort_by_key(|(i, _)| *i);
        let text: String = rows.into_iter().map(|(_, t)| t).collect();
        std::fs::write(dump, text).expect("write dump");
    }
}
