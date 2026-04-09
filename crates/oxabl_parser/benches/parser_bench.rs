use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
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

    group.finish();
}

criterion_group!(benches, parser_benchmarks);
criterion_main!(benches);
