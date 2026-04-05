use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_lexer::tokenize;
use std::fs;
use std::path::Path;

/// Load a fixture file from the workspace resources/ directory.
fn load_fixture(name: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
        .join(name);
    fs::read_to_string(path).unwrap()
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

    group.finish();
}

criterion_group!(benches, lexer_benchmarks);
criterion_main!(benches);
