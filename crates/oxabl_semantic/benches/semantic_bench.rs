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

/// Recognized-but-unmodelled statement forms (`StatementKind::Skipped`). The
/// resolve pass pays one scope-chain lookup per harvested token here, which is
/// the expensive half of the #128 lexical harvest — the parser only pays a
/// filter over tokens it was already walking. Nothing else in this file
/// exercises the `Skipped` arm at all.
const SKIPPED: &str = r#"
DEFINE VARIABLE lv-total AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-count AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-city AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-flag AS LOGICAL NO-UNDO.
DEFINE VARIABLE lv-key AS CHARACTER NO-UNDO.

PUT lv-total.
PUT UNFORMATTED lv-name SKIP.
PUT lv-total FORMAT ">>>,>>9" SKIP.
EXPORT lv-name lv-city lv-total.
EXPORT DELIMITER "," lv-line lv-flag.
ENABLE lv-name lv-city WITH FRAME f-detail.
DISABLE lv-name WITH FRAME f-detail.
UPDATE lv-name lv-city WITH FRAME f-detail.
SET lv-name lv-city WITH FRAME f-detail.
PROMPT-FOR lv-key WITH FRAME f-prompt.
APPLY "CHOOSE" TO btn-ok IN FRAME f-detail.
WAIT-FOR "CHOOSE" OF btn-ok.
GET-KEY-VALUE SECTION "app" KEY "path" VALUE lv-key.
IMPORT DELIMITER "," lv-name lv-city lv-total.
ACCUMULATE lv-total (TOTAL).
NEXT-PROMPT lv-name WITH FRAME f-detail.
CLEAR FRAME f-detail.
PUT lv-total lv-count lv-name lv-city.
EXPORT lv-name lv-city lv-total lv-count lv-line lv-flag lv-key.
ENABLE lv-name lv-city WITH FRAME f-detail.
UPDATE lv-name lv-city WITH FRAME f-detail.
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
            let (tree, symbols, diags, rev) = declare_pass(black_box(&program), black_box(&ctx));
            black_box((tree, symbols, diags, rev));
        })
    });

    // Resolve pass — needs declare output. Pre-run declare once per sample
    // so the measurement captures resolve in isolation.
    group.bench_function(format!("{name}/resolve"), |b| {
        b.iter_batched(
            || declare_pass(&program, &ctx),
            |(tree, mut symbols, _diags, rev)| {
                let (refs, types, rd) = resolve_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &mut symbols,
                    rev,
                );
                black_box((tree, symbols, refs, types, rd));
            },
            criterion::BatchSize::SmallInput,
        )
    });

    // Check pass — needs declare + resolve output.
    group.bench_function(format!("{name}/check"), |b| {
        b.iter_batched(
            || {
                let (tree, mut symbols, _d, rev) = declare_pass(&program, &ctx);
                let (refs, types, _rd) = resolve_pass(&program, &ctx, &tree, &mut symbols, rev);
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
    bench_fixture(&mut group, "skipped", SKIPPED);
    group.finish();

    // Schema-loaded resolve bench: exercises the field-lookup hot path
    // (`Schema::get_by_id` + `Table::get_field` + synth-cache probe per
    // field access), which the `Schema::empty()` fixtures above never hit.
    let mut schema_group = c.benchmark_group("schema_resolve");
    bench_schema_heavy(&mut schema_group);
    schema_group.finish();
}

/// `.df` text for the bench schema: two tables of ten fields each.
fn bench_df() -> String {
    let mut df = String::from("ADD TABLE \"Customer\"\nADD TABLE \"Item\"\n");
    for (table, prefix) in [("Customer", "c"), ("Item", "i")] {
        for n in 0..10 {
            let ty = if n % 2 == 0 { "integer" } else { "character" };
            df.push_str(&format!(
                "ADD FIELD \"{prefix}{n}\" OF \"{table}\" AS {ty}\n"
            ));
        }
    }
    df
}

/// Field-access-heavy program: nested `FOR EACH` loops referencing many
/// `Customer.Field` / `Item.Field` pairs (bare table qualifiers — the
/// synthesized-default-buffer path) plus a `FIND`.
fn schema_heavy_source() -> String {
    let mut src = String::from("DEFINE VARIABLE t AS DECIMAL NO-UNDO.\n");
    src.push_str("FOR EACH Customer:\n");
    src.push_str("    FOR EACH Item:\n");
    for n in 0..10 {
        src.push_str(&format!("        t = t + Customer.c{n} + Item.i{n}.\n"));
    }
    src.push_str("    END.\nEND.\n");
    src.push_str("FIND FIRST Customer.\n");
    for n in 0..10 {
        src.push_str(&format!("t = t + Customer.c{n}.\n"));
    }
    src
}

fn bench_schema_heavy(group: &mut BenchmarkGroup<'_, criterion::measurement::WallTime>) {
    let source = schema_heavy_source();
    let program = parse_program(&source);

    let schema = oxabl_schema::test_support::schema_from_df(&bench_df());
    let ctx = AnalysisContext::new(FileId::new(1), &source, &schema);

    group.throughput(Throughput::Bytes(source.len() as u64));

    group.bench_function("schema_heavy/declare", |b| {
        b.iter(|| {
            let (tree, symbols, diags, rev) = declare_pass(black_box(&program), black_box(&ctx));
            black_box((tree, symbols, diags, rev));
        })
    });

    group.bench_function("schema_heavy/resolve", |b| {
        b.iter_batched(
            || declare_pass(&program, &ctx),
            |(tree, mut symbols, _diags, rev)| {
                let (refs, types, rd) = resolve_pass(
                    black_box(&program),
                    black_box(&ctx),
                    &tree,
                    &mut symbols,
                    rev,
                );
                black_box((tree, symbols, refs, types, rd));
            },
            criterion::BatchSize::SmallInput,
        )
    });
}

criterion_group!(benches, semantic_benchmarks);
criterion_main!(benches);
