use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_lexer::tokenize;
use std::fs;
use std::path::Path;

/// Get the path to the benchmark file relative to the crate manifest
fn bench_file_path() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources/bench_keywords.abl")
}

/// Benchmark tokenizing a comprehensive ABL file with many keywords
fn bench_tokenize_keywords(c: &mut Criterion) {
    let source = fs::read_to_string(bench_file_path()).expect("Failed to read bench_keywords.abl");

    let mut group = c.benchmark_group("lexer");
    group.throughput(Throughput::Bytes(source.len() as u64));

    group.bench_function("tokenize_keywords", |b| {
        b.iter(|| tokenize(black_box(&source)))
    });

    group.finish();
}

/// Benchmark tokenizing with token count reporting
fn bench_tokenize_with_stats(c: &mut Criterion) {
    let source = fs::read_to_string(bench_file_path()).expect("Failed to read bench_keywords.abl");

    let mut group = c.benchmark_group("lexer");
    group.throughput(Throughput::Bytes(source.len() as u64));

    // Print stats once
    let tokens = tokenize(&source);
    println!(
        "\nBenchmark file stats: {} bytes, {} tokens",
        source.len(),
        tokens.len()
    );

    group.bench_function("tokenize_full", |b| b.iter(|| tokenize(black_box(&source))));

    group.finish();
}

criterion_group!(benches, bench_tokenize_keywords, bench_tokenize_with_stats);
criterion_main!(benches);
