//! End-to-end runs of the pipeline, as the command line and the language
//! server call it. Every file is in an in-memory file system, so no bench is
//! dominated by system calls.
//!
//! - `check/file`: `oxabl check` of `resources/lsp_bench.p` with its includes:
//!   the lint run and then the format check, the calls of `CheckPass`, on a
//!   new pipeline (an empty cross-file index), as each `oxabl check` starts.
//! - `check/identity_root`: a root of about 100 KB that expands to itself (the
//!   parser fixtures with no directive), so the analysis and the format check
//!   share one lex and one parse.
//! - `check/workspace`: the same over the 300 roots of the generated `ci`
//!   workspace (`oxabl_bench::generate`), with one pipeline for the walk. The
//!   roots have no finding and no drift.
//! - `check/workspace_include_findings`: the same workspace with 100 unused
//!   variables in each hot include, so each root that reads one folds the
//!   findings of the include (`IncludeRows` in `oxabl_analyze`).
//! - `check/include_findings_repeated`: one root that includes a file of 100
//!   unused variables in each of 60 procedures, so 100 rows have 60 expansions
//!   each and the fold of include findings is a large share of the run.
//! - `producers/scan`: the scan for `NEW SHARED` producers, as the first
//!   `SHARED` question of a run makes it, over 600 files of six kinds
//!   (`fixtures::shared_producers`): written definitions, one with a `TRIGGERS`
//!   phrase, a file of an include, consumers, classes and files not read.
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
use oxabl_pipeline::{FormatPipeline, LazyRawUnit, LintPipeline, PipelineConfig};
use oxabl_workspace::{FileSystem, InMemoryFileSystem};

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
    let raw = LazyRawUnit::new(source);
    let result = lint.with_file(path).run_with_unit(&raw).0;
    assert!(result.failure().is_none(), "{}", path.display());
    let outcome = match raw.made() {
        Some(raw) => format.format_unit(raw),
        None => format.format(source),
    };
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

/// The parser fixtures that expand to themselves and format, repeated, as one
/// root of about 100 KB with no include and no directive: the root that the
/// raw unit serves, so `oxabl check` lexes and parses it once.
fn identity_root() -> (InMemoryFileSystem, PipelineConfig, PathBuf, String) {
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let path = PathBuf::from("/bench/identity.p");
    let format = FormatPipeline::new(config.style.clone());
    let lint = LintPipeline::new(&config, &fs);
    let mut source = String::new();
    for name in fixtures::MIXED_PARTS {
        let part = fixtures::resource(name);
        let identity = lint
            .with_file(&path)
            .expand(&part)
            .expanded()
            .is_some_and(|expanded| expanded.is_identity());
        if identity && format.format(&part).not_formatted().is_none() {
            source.push_str(&part);
        }
    }
    let source = source.repeat(4);
    assert!(
        source.len() > 50_000,
        "{} bytes of identity fixtures",
        source.len()
    );
    (fs, config, path, source)
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

    let (id_fs, id_config, id_path, id_source) = identity_root();
    let id_format = FormatPipeline::new(id_config.style.clone());
    let id_root = [(id_path, id_source)];
    bench(&mut group, "check", "identity_root", || {
        check_walk(&id_config, &id_fs, &id_format, &id_root)
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

fn bench_producer_scan(c: &mut Criterion) {
    let mut group = c.benchmark_group("producers");
    group.sample_size(10);
    let (fs, config, paths) = fixtures::shared_producers(600);
    let scan = || LintPipeline::new(&config, &fs).shared_producers(&paths);
    let map = scan();
    // 100 files of each of two written kinds, and 100 of an include.
    assert_eq!(
        map.producers_of(&oxabl_semantic::IndexName::new("sp-trig1")),
        [PathBuf::from("/sp/t1.p")]
    );
    assert_eq!(
        map.producers_of(&oxabl_semantic::IndexName::new("sp-included"))
            .len(),
        100
    );
    assert_eq!(map.len(), 100 * 2 + 100 + 2, "{map:?}");
    bench(&mut group, "producers", "scan", || scan().len());
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

criterion_group!(
    benches,
    bench_check_and_format,
    bench_producer_scan,
    bench_navigation
);
criterion_main!(benches);
