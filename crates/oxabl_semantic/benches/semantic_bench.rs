//! Per-pass benchmarks for `oxabl_semantic`.
//!
//! Splits the three passes (declare, resolve, check) plus the end-to-end
//! `analyze_file` wrapper into separate criterion groups so regressions in
//! any single pass are caught without aggregate numbers hiding them. See
//! plan §"Bench granularity" — the split is deliberate.

use criterion::{
    BenchmarkGroup, Criterion, Throughput, black_box, criterion_group, criterion_main,
};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file, check_pass, declare_pass, resolve_pass};

const TINY: &str = r#"
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE y AS CHARACTER NO-UNDO.
ASSIGN x = 42 y = "hello".
MESSAGE x y.
"#;

const MEDIUM: &str = r#"
DEFINE VARIABLE total AS DECIMAL NO-UNDO.
DEFINE VARIABLE count AS INTEGER NO-UNDO.

PROCEDURE sum-range:
    DEFINE INPUT PARAMETER a AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER b AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER result AS INTEGER NO-UNDO.
    ASSIGN result = a + b.
END PROCEDURE.

FUNCTION avg RETURNS DECIMAL (INPUT t AS DECIMAL, INPUT c AS INTEGER):
    RETURN t / c.
END FUNCTION.

DO count = 1 TO 10:
    ASSIGN total = total + count.
END.

MESSAGE total count.
"#;

fn parse_program(source: &str) -> Vec<oxabl_ast::Statement> {
    let tokens = tokenize(source);
    let mut parser = Parser::new(&tokens, source);
    parser.parse_statements().expect("parse fixture")
}

fn bench_fixture(
    group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    source: &str,
) {
    let program = parse_program(source);
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), source, &schema);

    group.throughput(Throughput::Bytes(source.len() as u64));

    // Declare pass in isolation.
    group.bench_function(format!("{name}/declare"), |b| {
        b.iter(|| {
            let (tree, symbols, diags) = declare_pass(black_box(&program), black_box(&ctx));
            black_box((tree, symbols, diags));
        })
    });

    // Resolve pass — needs declare output. Pre-run declare once per sample
    // so the measurement captures resolve in isolation.
    group.bench_function(format!("{name}/resolve"), |b| {
        b.iter_batched(
            || declare_pass(&program, &ctx),
            |(tree, mut symbols, _diags)| {
                let (refs, types, rd) =
                    resolve_pass(black_box(&program), black_box(&ctx), &tree, &mut symbols);
                black_box((tree, symbols, refs, types, rd));
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Check pass — needs declare + resolve output.
    group.bench_function(format!("{name}/check"), |b| {
        b.iter_batched(
            || {
                let (tree, mut symbols, _d) = declare_pass(&program, &ctx);
                let (refs, types, _rd) = resolve_pass(&program, &ctx, &tree, &mut symbols);
                (tree, symbols, refs, types)
            },
            |(tree, symbols, refs, mut types)| {
                let diags = check_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &symbols,
                    &refs,
                    &mut types,
                );
                black_box((tree, symbols, refs, types, diags));
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // End-to-end `analyze_file` — the user-visible entry point.
    group.bench_function(format!("{name}/analyze_file"), |b| {
        b.iter(|| {
            let sem = analyze_file(black_box(&program), black_box(&ctx));
            black_box(sem);
        })
    });
}

fn semantic_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("semantic");
    bench_fixture(&mut group, "tiny", TINY);
    bench_fixture(&mut group, "medium", MEDIUM);
    group.finish();
}

criterion_group!(benches, semantic_benchmarks);
criterion_main!(benches);
