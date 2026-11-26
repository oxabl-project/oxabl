//! End-to-end runs of the pipeline, as the command line and the language
//! server call it. Every file is in an in-memory file system, so no bench is
//! dominated by system calls.
//!
//! - `check/file`: `oxabl check` of `resources/lsp_bench.p` with its includes:
//!   the lint run and then the format check, the calls of `CheckPass`, on a
//!   new pipeline (an empty cross-file index), as each `oxabl check` starts.
//! - `check/workspace`: the same over the 300 roots of the generated `ci`
//!   workspace (`oxabl_bench::generate`), with one pipeline for the walk. The
//!   roots have no finding and no drift.
//! - `check/workspace_include_findings`: the same workspace with 100 unused
//!   variables in each hot include, so each root that reads one folds the
//!   findings of the include (`IncludeRows` in `oxabl_analyze`).
//! - `check/include_findings_repeated`: one root that includes a file of 100
//!   unused variables in each of 60 procedures, so 100 rows have 60 expansions
//!   each and the fold of include findings is a large share of the run.
//! - `format/file_end_to_end`: `FormatPipeline::format` from the source text
//!   of `lsp_bench.p`, as `oxabl format` and Format Document call it.
//! - `navigation/definitions`: one go to definition request on `lsp_bench.p`:
//!   the expansion, the navigation analysis and the definitions of one name.
//! - `navigation/links`: one document links request on the generated root with
//!   the most includes, whose `RUN` targets and classes the index resolves.
//!
//! Each bench reports the allocations of one iteration (see `oxabl_bench::bench`).

use std::path::{Path, PathBuf};

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_bench::alloc::Counting;
use oxabl_bench::generate::{Knobs, Preset, generate};
use oxabl_bench::{bench, fixtures};
use oxabl_pipeline::{FormatPipeline, LintPipeline, PipelineConfig};
use oxabl_workspace::FileSystem;

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// What `CheckPass::root` does with one root: lint, then the format check.
/// Returns the findings, with those in its includes, and whether the file
/// drifted.
fn check_root(
    lint: &LintPipeline<'_>,
    format: &FormatPipeline,
    path: &Path,
    source: &str,
) -> (usize, bool) {
    let result = lint.with_file(path).run(source);
    assert!(result.failure().is_none(), "{}", path.display());
    let outcome = format.format(source);
    assert!(
        outcome.not_formatted().is_none(),
        "{} formats",
        path.display()
    );
    let findings = result.all().count() + result.include_origin().len();
    (findings, outcome.changed())
}

fn check_walk(
    config: &PipelineConfig,
    fs: &dyn FileSystem,
    format: &FormatPipeline,
    roots: &[(PathBuf, String)],
) -> usize {
    let paths: Vec<PathBuf> = roots.iter().map(|(path, _)| path.clone()).collect();
    let producers = LintPipeline::new(config, fs).deferred_producers(&paths, 1);
    let lint = LintPipeline::new(config, fs).with_deferred_producers(&producers);
    roots
        .iter()
        .map(|(path, source)| check_root(&lint, format, path, source).0)
        .sum()
}

fn bench_check_and_format(c: &mut Criterion) {
    let (fs, config, path, source) = fixtures::lsp_bench();
    let format = FormatPipeline::new(config.style.clone());
    let one = [(path.clone(), source.clone())];

    let mut group = c.benchmark_group("check");
    group.sample_size(10);
    bench(&mut group, "check", "file", || {
        check_walk(&config, &fs, &format, &one)
    });

    for (name, include_findings) in [("workspace", 0), ("workspace_include_findings", 100)] {
        let knobs = Knobs {
            include_findings,
            ..Preset::Ci.knobs()
        };
        let workspace = generate(&knobs);
        let dir = Path::new("/gen");
        let (ws_fs, ws_config) = fixtures::in_memory(&workspace, dir);
        let roots: Vec<(PathBuf, String)> = workspace
            .roots()
            .map(|root| (dir.join(&root.path), root.text.clone()))
            .collect();
        assert_eq!(roots.len(), 300);
        let ws_format = FormatPipeline::new(ws_config.style.clone());
        let findings = check_walk(&ws_config, &ws_fs, &ws_format, &roots);
        // The clean workspace has none; the other has one row for each finding
        // of a hot include in each root that reads it.
        assert_eq!(
            findings == 0,
            include_findings == 0,
            "{name}: {findings} findings"
        );
        bench(&mut group, "check", name, || {
            check_walk(&ws_config, &ws_fs, &ws_format, &roots)
        });
    }

    let (rep_fs, rep_config, rep_path, rep_source) = fixtures::repeated_include(60, 100);
    let rep = [(rep_path, rep_source)];
    assert_eq!(
        check_walk(&rep_config, &rep_fs, &format, &rep),
        100,
        "one row for each unused variable"
    );
    bench(&mut group, "check", "include_findings_repeated", || {
        check_walk(&rep_config, &rep_fs, &format, &rep)
    });
    group.finish();

    let mut group = c.benchmark_group("format");
    group.sample_size(10);
    bench(&mut group, "format", "file_end_to_end", || {
        format.format(&source).changed()
    });
    group.finish();
}

fn bench_navigation(c: &mut Criterion) {
    let mut group = c.benchmark_group("navigation");
    group.sample_size(10);

    let (fs, config, path, source) = fixtures::lsp_bench();
    // A use of a global variable near the end of the file.
    let offset = source.rfind("gv-count").expect("a use of gv-count") as u32;
    let none = |_| None;
    let definitions = || {
        let run = LintPipeline::new(&config, &fs);
        let handle = run.with_file(&path);
        let expansion = handle.expand(&source);
        let navigation = handle
            .navigation(&expansion, &none)
            .expect("lsp_bench.p preprocesses");
        navigation.definitions(&path, offset).len()
    };
    assert_eq!(definitions(), 1, "gv-count has one definition");
    bench(&mut group, "navigation", "definitions", definitions);

    let workspace = generate(&Preset::Ci.knobs());
    let dir = Path::new("/gen");
    let (ws_fs, ws_config) = fixtures::in_memory(&workspace, dir);
    let root = workspace
        .roots()
        .max_by_key(|root| root.includes.len())
        .expect("a root");
    let root_path = dir.join(&root.path);
    let links = || {
        let run = LintPipeline::new(&ws_config, &ws_fs);
        let handle = run.with_file(&root_path);
        let expansion = handle.expand(&root.text);
        let navigation = handle
            .navigation(&expansion, &none)
            .expect("the root preprocesses");
        navigation.links()
    };
    let found = links();
    let runs = root.text.matches("\nRUN app/").count();
    assert!(runs > 0, "the root runs other programs");
    assert!(
        found.len() >= root.includes.len() + runs,
        "{} links for {} includes and {runs} RUN statements",
        found.len(),
        root.includes.len()
    );
    bench(&mut group, "navigation", "links", || links().len());
    group.finish();
}

criterion_group!(benches, bench_check_and_format, bench_navigation);
criterion_main!(benches);
