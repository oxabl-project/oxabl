//! Benchmarks for the `strict.nulls` rules over `oxabl_nulls`, which give the
//! places a `?` may come from as related locations.
//!
//! The fixtures are the shapes that make that search work hard: wide `ASSIGN`
//! statements in a loop, where every pair writes a variable that a test reads
//! later, and many conditional sources of `?` per variable across loops.

use criterion::{
    BenchmarkGroup, Criterion, Throughput, black_box, criterion_group, criterion_main,
};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{unknown_in_comparison, unknown_propagation};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// `vars` `DATE` variables (they start at `?`), a counted loop of `statements`
/// `ASSIGN` statements that each step every variable, then one test of each.
fn wide_assign_source(vars: usize, statements: usize) -> String {
    let mut src = String::from("DEFINE VARIABLE k AS INTEGER NO-UNDO.\n");
    for v in 0..vars {
        src.push_str(&format!("DEFINE VARIABLE v{v} AS DATE NO-UNDO.\n"));
    }
    src.push_str("DO k = 1 TO 3:\n");
    for _ in 0..statements {
        src.push_str("    ASSIGN");
        for v in 0..vars {
            src.push_str(&format!("\n        v{v} = v{v} + 1"));
        }
        src.push_str(".\n");
    }
    src.push_str("END.\n");
    for v in 0..vars {
        src.push_str(&format!("IF v{v} > TODAY THEN MESSAGE {v}.\n"));
    }
    src
}

/// `vars` variables, each set to `?` under `rounds` conditions, with a loop
/// after each round, then compared and used in arithmetic.
fn branchy_source(vars: usize, rounds: usize) -> String {
    let mut src = String::from("DEFINE INPUT PARAMETER k AS INTEGER NO-UNDO.\n");
    for v in 0..vars {
        src.push_str(&format!("DEFINE VARIABLE v{v} AS INTEGER NO-UNDO.\n"));
    }
    for r in 0..rounds {
        for v in 0..vars {
            src.push_str(&format!("IF k = {r} THEN v{v} = ?.\n"));
        }
        src.push_str("REPEAT WHILE k < 10:\n    k = k + 1.\nEND.\n");
    }
    for v in 0..vars {
        src.push_str(&format!("IF v{v} > 1 THEN MESSAGE v{v} * 2.\n"));
    }
    src
}

fn bench_source(
    group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    source: &str,
    reported: usize,
) {
    let tokens = tokenize(source);
    let program = Parser::new(&tokens, source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), source, &schema).with_tokens(&tokens);
    let sem = analyze_file(&program.statements, &ctx);
    let found = unknown_in_comparison::run(&program.statements, &sem, &ctx);
    assert_eq!(found.len(), reported, "the fixture must exercise the rule");
    assert!(
        found.iter().all(|d| !d.labels.is_empty()),
        "the search must find the sources, not stop at its budget"
    );

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(format!("{name}/unknown_in_comparison"), |b| {
        b.iter(|| black_box(unknown_in_comparison::run(&program.statements, &sem, &ctx)))
    });
    group.bench_function(format!("{name}/unknown_propagation"), |b| {
        b.iter(|| black_box(unknown_propagation::run(&program.statements, &sem, &ctx)))
    });
}

fn nulls_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("nulls");
    bench_source(&mut group, "wide_assign", &wide_assign_source(60, 6), 60);
    bench_source(&mut group, "branchy", &branchy_source(30, 10), 30);
    group.finish();
}

criterion_group!(benches, nulls_benchmarks);
criterion_main!(benches);
