use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_lexer::tokenize;
use std::fs;
use std::path::Path;

/// Load a fixture file from the workspace resources/ directory.
fn load_fixture(name: &str) -> String {
    let Some(workspace_root) = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
    else {
        panic!("CARGO_MANIFEST_DIR must have two parent directories");
    };
    let path = workspace_root.join("resources").join(name);
    match fs::read_to_string(&path) {
        Ok(source) => source,
        Err(err) => panic!("failed to read fixture {}: {err}", path.display()),
    }
}

/// Benchmark a single fixture: tokenize and measure throughput.
fn bench_fixture(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    fixture: &str,
) {
    let source = load_fixture(fixture);
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(name, |b| b.iter(|| tokenize(black_box(&source))));
}

fn lexer_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer");

    bench_fixture(&mut group, "keywords", "bench_keywords.abl");
    bench_fixture(&mut group, "strings", "bench_lexer_strings.abl");
    bench_fixture(&mut group, "comments", "bench_lexer_comments.abl");
    bench_fixture(&mut group, "numeric", "bench_lexer_numeric.abl");
    bench_fixture(&mut group, "preprocessor", "bench_lexer_preprocessor.abl");

    let identifiers = "DEFINE VARIABLE customer-account AS INTEGER NO-UNDO.\ncustomer-account = customer-account + 1.\n".repeat(100);
    group.throughput(Throughput::Bytes(identifiers.len() as u64));
    group.bench_function("repeated_identifiers", |b| {
        b.iter(|| tokenize(black_box(&identifiers)))
    });

    group.finish();
}

criterion_group!(benches, lexer_benchmarks);
criterion_main!(benches);
