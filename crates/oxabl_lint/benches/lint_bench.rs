//! Benchmarks for `oxabl_lint`.
//!
//! The fixture is built from block-hoisted variables, the input of the
//! path-aware `block-var-used-outside` rule (LINT0005): a variable defined in a
//! block and read after it. Each routine holds a few, so the rule has to build
//! the control-flow graph of the routine and prove, or fail to prove, that the
//! block ran before each read.

use criterion::{
    BenchmarkGroup, Criterion, Throughput, black_box, criterion_group, criterion_main,
};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_lint::{block_var_used_outside, lint_file};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};

/// A file of `routines` internal procedures. Each has:
///
/// - a guard-clause pair that the paths clear (`g*`);
/// - two variables defined in a conditional block and read after it, which the
///   rule reports (`h*`);
/// - a variable assigned by a loop body and read after it (`l`).
///
/// The main block defines one block variable that a procedure reads, so the
/// read is in another unit.
fn hoisted_source(routines: usize) -> String {
    let mut src = String::from("DEFINE INPUT PARAMETER ok AS LOGICAL NO-UNDO.\n");
    src.push_str("IF ok THEN DO:\n    DEFINE VARIABLE shared-count AS INTEGER NO-UNDO.\n");
    src.push_str("    shared-count = 1.\nEND.\n");
    for n in 0..routines {
        src.push_str(&format!("PROCEDURE p{n}:\n"));
        src.push_str("    DEFINE INPUT PARAMETER valid AS LOGICAL NO-UNDO.\n");
        src.push_str("    DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
        src.push_str("    IF NOT valid THEN DO:\n        RETURN.\n    END.\n    ELSE DO:\n");
        src.push_str("        DEFINE VARIABLE g1 AS DECIMAL NO-UNDO.\n");
        src.push_str("        DEFINE VARIABLE g2 AS DECIMAL NO-UNDO.\n");
        src.push_str("        g1 = 1.\n        g2 = g1 + 1.\n    END.\n");
        src.push_str("    DISPLAY g1 g2.\n");
        src.push_str("    IF total > 0 THEN DO:\n");
        src.push_str("        DEFINE VARIABLE h1 AS INTEGER NO-UNDO.\n");
        src.push_str("        DEFINE VARIABLE h2 AS CHARACTER NO-UNDO.\n");
        src.push_str("        h1 = total.\n        h2 = \"x\".\n    END.\n");
        src.push_str("    MESSAGE h1 h2.\n");
        src.push_str("    DO total = 1 TO 10:\n");
        src.push_str("        DEFINE VARIABLE l AS INTEGER NO-UNDO.\n");
        src.push_str("        l = total.\n    END.\n");
        src.push_str("    MESSAGE l shared-count.\n");
        src.push_str("END PROCEDURE.\n");
    }
    src
}

/// Many routines with no block-hoisted variable, and one with two: the rule
/// needs the unit of that one only.
fn sparse_source(routines: usize) -> String {
    let mut src = plain_source(routines);
    src.push_str("PROCEDURE hoist:\n    DEFINE INPUT PARAMETER ok AS LOGICAL NO-UNDO.\n");
    src.push_str("    IF ok THEN DO:\n        DEFINE VARIABLE h1 AS INTEGER NO-UNDO.\n");
    src.push_str("        DEFINE VARIABLE h2 AS INTEGER NO-UNDO.\n");
    src.push_str("        h1 = 1.\n        h2 = 2.\n    END.\n");
    src.push_str("    MESSAGE h1 h2.\nEND PROCEDURE.\n");
    src
}

/// A file with no block-hoisted variable: the rule must cost nothing here.
fn plain_source(routines: usize) -> String {
    let mut src = String::new();
    for n in 0..routines {
        src.push_str(&format!("PROCEDURE p{n}:\n"));
        src.push_str("    DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
        src.push_str("    DO total = 1 TO 10:\n        MESSAGE total.\n    END.\n");
        src.push_str("END PROCEDURE.\n");
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
    let program = Parser::new(&tokens, source)
        .parse_statements()
        .expect("parse fixture");
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), source, &schema);
    let sem = analyze_file(&program, &ctx);

    assert_eq!(
        block_var_used_outside::run(&program, &sem, &ctx).len(),
        reported,
        "the fixture must exercise the rule"
    );

    group.throughput(Throughput::Bytes(source.len() as u64));

    // LINT0005 alone, on an analysed file.
    group.bench_function(format!("{name}/block_var_used_outside"), |b| {
        b.iter(|| black_box(block_var_used_outside::run(&program, &sem, &ctx)))
    });

    // The default rule set, the cost a `check` pays.
    group.bench_function(format!("{name}/lint_file"), |b| {
        b.iter(|| black_box(lint_file(&program, &sem, &ctx)))
    });
}

fn lint_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("lint");
    // `h1`, `h2` and `l` of every routine, and `shared-count` once.
    bench_source(&mut group, "hoisted", &hoisted_source(40), 40 * 3 + 1);
    bench_source(&mut group, "sparse", &sparse_source(40), 2);
    bench_source(&mut group, "plain", &plain_source(40), 0);
    group.finish();
}

criterion_group!(benches, lint_benchmarks);
criterion_main!(benches);
