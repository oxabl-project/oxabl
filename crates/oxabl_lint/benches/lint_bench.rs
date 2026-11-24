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
use oxabl_common::{FileId, LintSeverityMap, SecurityMode, SecurityOptions};
use oxabl_lexer::tokenize;
use oxabl_lint::{
    CL001, CL002, CL005, LINT0002, LINT0005, LINT0006, Registry, TX006, block_var_used_outside,
    lint_file,
};
use oxabl_parser::Parser;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, analyze_file};
use std::path::Path;

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

/// Load a fixture file from the workspace `resources/` directory.
fn load_fixture(name: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../resources")
        .join(name);
    std::fs::read_to_string(path).unwrap()
}

/// `lint_file` and the registry on a hand-written, realistic file: once with
/// the default rules, once with every `strict.*` group enabled.
///
/// The setup asserts that the fixture parses cleanly and that each of
/// `expected` reports at least once, so the bench cannot silently measure a
/// file the rules skip.
fn bench_realistic(c: &mut Criterion, name: &str, fixture: &str, expected: &[&str]) {
    let source = load_fixture(fixture);
    let tokens = tokenize(&source);
    let program = Parser::new(&tokens, &source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();

    // The contexts carry the parsed tokens, as `check` gives them.
    let default_ctx = AnalysisContext::new(FileId::new(1), &source, &schema).with_tokens(&tokens);
    let mut strict = LintSeverityMap::new();
    strict.base_mut().enable_group("strict");
    let strict_ctx = AnalysisContext::new(FileId::new(1), &source, &schema)
        .with_tokens(&tokens)
        .with_lint_severities(strict);
    let sem = analyze_file(&program.statements, &default_ctx);

    let registry = Registry::builtin();
    let found = registry.run(&program.statements, &sem, &strict_ctx);
    for code in expected {
        assert!(
            found.iter().any(|d| d.code.0 == *code),
            "{fixture} must trigger {code}"
        );
    }
    let default_count = registry.run(&program.statements, &sem, &default_ctx).len();
    let strict_count = registry.run(&program.statements, &sem, &strict_ctx).len();
    assert!(default_count > 0, "the fixture must trigger default rules");
    assert!(
        strict_count > default_count,
        "the fixture must trigger strict rules"
    );

    let mut group = c.benchmark_group(format!("lint_realistic/{name}"));
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("lint_file", |b| {
        b.iter(|| black_box(lint_file(&program.statements, &sem, &default_ctx)))
    });
    group.bench_function("registry_default", |b| {
        b.iter(|| black_box(registry.run(&program.statements, &sem, &default_ctx)))
    });
    group.bench_function("registry_strict", |b| {
        b.iter(|| black_box(registry.run(&program.statements, &sem, &strict_ctx)))
    });
    group.finish();
}

fn realistic_benchmarks(c: &mut Criterion) {
    bench_realistic(
        c,
        "procedural",
        "bench_lint_procedural.abl",
        &[LINT0002, LINT0005, CL001, CL002, CL005, TX006],
    );
    bench_realistic(
        c,
        "class",
        "bench_lint_class.abl",
        &[LINT0006, CL001, CL002, TX006],
    );
}

/// A file of `routines` procedures, each with an untrusted parameter that
/// reaches some sinks through assignments, a temp-table field and a guard, and
/// constants that reach others. Stage 2 has to solve every routine.
fn security_source(routines: usize) -> String {
    let mut src = String::from(
        "DEFINE TEMP-TABLE ttArg NO-UNDO FIELD cValue AS CHARACTER.\n\
         DEFINE VARIABLE hQ AS HANDLE NO-UNDO.\n",
    );
    for n in 0..routines {
        src.push_str(&format!("PROCEDURE p{n}:\n"));
        src.push_str("    DEFINE INPUT PARAMETER pc AS CHARACTER NO-UNDO.\n");
        src.push_str("    DEFINE VARIABLE c AS CHARACTER NO-UNDO.\n");
        src.push_str("    DEFINE VARIABLE i AS INTEGER NO-UNDO.\n");
        src.push_str("    c = \"FOR EACH Customer WHERE Customer.Name = '\" + pc + \"'\".\n");
        src.push_str("    hQ:QUERY-PREPARE(c).\n");
        src.push_str("    CREATE ttArg.\n    ttArg.cValue = pc.\n");
        src.push_str("    DO i = 1 TO 3:\n        OS-COMMAND SILENT VALUE(\"echo \" + ttArg.cValue).\n    END.\n");
        src.push_str("    IF LOOKUP(pc, \"a,b\") = 0 THEN RETURN.\n");
        src.push_str("    RUN VALUE(pc + \".p\").\n");
        src.push_str("    c = \"ls\".\n    OS-COMMAND SILENT VALUE(c).\n");
        src.push_str(
            "    hQ:QUERY-PREPARE(\"FOR EACH Customer WHERE Customer.Name = \" + QUOTER(pc)).\n",
        );
        src.push_str("END PROCEDURE.\n");
    }
    src
}

/// The `security` group in stage 1 and in taint mode.
fn security_benchmarks(c: &mut Criterion) {
    let source = security_source(40);
    let tokens = tokenize(&source);
    let program = Parser::new(&tokens, &source).parse_program();
    assert!(program.errors.is_empty(), "{:?}", program.errors);
    let schema = Schema::empty();
    let context = |mode| {
        let mut map = LintSeverityMap::new();
        map.base_mut().enable_group("security");
        map.set_security(SecurityOptions {
            mode,
            ..SecurityOptions::default()
        });
        AnalysisContext::new(FileId::new(1), &source, &schema)
            .with_tokens(&tokens)
            .with_lint_severities(map)
    };
    let (sink, taint) = (context(SecurityMode::Sink), context(SecurityMode::Taint));
    let sem = analyze_file(&program.statements, &sink);
    let registry = Registry::builtin();
    let select = |m: &oxabl_lint::RuleMeta| m.group == "security";
    let found = |ctx| {
        registry
            .run_where(&program.statements, &sem, ctx, select)
            .len()
    };
    // Stage 1 reports the five sinks of each routine; stage 2 the two that
    // the parameter reaches unguarded and uncleaned.
    assert_eq!(found(&sink), 40 * 5);
    assert_eq!(found(&taint), 40 * 2);

    let mut group = c.benchmark_group("lint_security");
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("sink", |b| {
        b.iter(|| black_box(registry.run_where(&program.statements, &sem, &sink, select)))
    });
    group.bench_function("taint", |b| {
        b.iter(|| black_box(registry.run_where(&program.statements, &sem, &taint, select)))
    });
    group.finish();
}

fn lint_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("lint");
    // `h1`, `h2` and `l` of every routine, and `shared-count` once.
    bench_source(&mut group, "hoisted", &hoisted_source(40), 40 * 3 + 1);
    bench_source(&mut group, "sparse", &sparse_source(40), 2);
    bench_source(&mut group, "plain", &plain_source(40), 0);
    group.finish();
}

criterion_group!(
    benches,
    lint_benchmarks,
    realistic_benchmarks,
    security_benchmarks
);
criterion_main!(benches);
