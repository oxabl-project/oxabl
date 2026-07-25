use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::{FileSystem, RealFileSystem};
use std::fs;
use std::path::{Path, PathBuf};

/// Resolve the workspace `resources/` directory.
fn resources_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
}

/// Load a fixture file from the workspace resources/ directory.
fn load_fixture(name: &str) -> String {
    fs::read_to_string(resources_dir().join(name)).unwrap()
}

/// Bench the full pipeline (preprocess → tokenize → parse) on a fixture
/// whose `{include.i}` references are resolved from the workspace
/// `resources/` directory via [`RealFileSystem`]. Mirrors the corpus run
/// where include resolution is the dominant cost.
fn bench_pipeline_with_includes(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    fixture: &str,
) {
    let source = load_fixture(fixture);
    let fs = RealFileSystem;
    let include_paths = vec![resources_dir()];

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(name, |b| {
        b.iter(|| {
            let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
            let preprocessed = pp.process(FileId::new(1), black_box(&source)).unwrap();
            let expanded = preprocessed.to_text();
            let tokens = tokenize(&expanded);
            let mut parser = Parser::new(&tokens, &expanded);
            parser.parse_program()
        })
    });
}

/// Benchmark a single fixture: tokenize + parse end-to-end.
fn bench_fixture(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    fixture: &str,
) {
    let source = load_fixture(fixture);
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(name, |b| {
        b.iter(|| {
            let tokens = tokenize(black_box(&source));
            let mut parser = Parser::new(&tokens, &source);
            parser.parse_program()
        })
    });
}

fn parser_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    bench_fixture(&mut group, "full_program", "bench_keywords.abl");
    bench_fixture(&mut group, "expressions", "bench_parser_expressions.abl");
    bench_fixture(&mut group, "declarations", "bench_parser_declarations.abl");
    bench_fixture(&mut group, "control_flow", "bench_parser_control_flow.abl");
    bench_fixture(&mut group, "oo_abl", "bench_parser_oo_abl.abl");
    bench_fixture(&mut group, "temp_tables", "bench_parser_temp_tables.abl");
    bench_fixture(&mut group, "procs_funcs", "bench_parser_procs_funcs.abl");
    bench_fixture(&mut group, "datasets", "bench_parser_datasets.abl");
    // Recognized-but-unmodelled forms (PUT / EXPORT / ENABLE / UPDATE / SET /
    // APPLY / WAIT-FOR …). No other bench_parser_* fixture contains any of
    // them, so without this target the skip + harvest path scores zero.
    bench_fixture(
        &mut group,
        "skipped_forms",
        "bench_parser_skipped_forms.abl",
    );

    bench_pipeline_with_includes(
        &mut group,
        "pipeline_with_includes",
        "bench_preprocessor_pipeline.abl",
    );

    group.finish();
}

criterion_group!(benches, parser_benchmarks);
criterion_main!(benches);
