//! The lexer, the parser and the semantic passes on large inputs.
//!
//! - `lexer/large_mixed/*` and `parser/large_mixed/*`: `resources/lsp_bench.p`
//!   (129 KB) and the ten error-free `bench_parser_*` fixtures repeated to
//!   1 MB. The existing fixtures are 1 KB to 17 KB and stay in the caches.
//! - `semantic/large/*`: the declare, resolve and check passes on
//!   `lsp_bench.p` after its includes are expanded, and on a file of many
//!   procedures with deeply nested blocks (`nested_blocks`), which has many
//!   scopes.
//!
//! Each bench reports the allocations of one iteration (see `oxabl_bench::bench`).

use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_bench::alloc::{self, Counting};
use oxabl_bench::{bench, fixtures, report};
use oxabl_common::FileId;
use oxabl_lexer::tokenize;
use oxabl_parser::{Parser, Program};
use oxabl_pipeline::LintPipeline;
use oxabl_schema::Schema;
use oxabl_semantic::{AnalysisContext, check_pass, declare_pass, resolve_pass};

#[global_allocator]
static ALLOCATOR: Counting = Counting;

fn parse(source: &str) -> Program {
    let tokens = tokenize(source);
    Parser::new(&tokens, source).parse_program()
}

/// The two large inputs, each checked to parse with no error.
fn large_inputs() -> [(&'static str, String); 2] {
    let inputs = [
        ("lsp_bench", fixtures::resource("lsp_bench.p")),
        ("1mb", fixtures::large_mixed()),
    ];
    for (name, source) in &inputs {
        let errors = parse(source).errors;
        assert!(errors.is_empty(), "{name}: {errors:?}");
    }
    inputs
}

fn bench_lexer_parser(c: &mut Criterion) {
    let inputs = large_inputs();
    assert_eq!(inputs[1].1.len(), 1_048_400);

    let mut group = c.benchmark_group("lexer");
    group.sample_size(10);
    for (name, source) in &inputs {
        bench(&mut group, "lexer", &format!("large_mixed/{name}"), || {
            tokenize(black_box(source)).len()
        });
    }
    group.finish();

    let mut group = c.benchmark_group("parser");
    group.sample_size(10);
    for (name, source) in &inputs {
        let tokens = tokenize(source);
        bench(&mut group, "parser", &format!("large_mixed/{name}"), || {
            Parser::new(black_box(&tokens), source)
                .parse_program()
                .statements
                .len()
        });
    }
    group.finish();
}

/// `lsp_bench.p` with its includes expanded, as the pipeline analyses it.
fn expanded_lsp_bench() -> String {
    let (fs, config, path, source) = fixtures::lsp_bench();
    let run = LintPipeline::new(&config, &fs);
    let expansion = run.with_file(&path).expand(&source);
    let text = expansion.text().expect("lsp_bench.p preprocesses");
    assert!(
        expansion.unresolved_includes().is_empty(),
        "every include of lsp_bench.p resolves"
    );
    text.to_string()
}

fn bench_passes(c: &mut Criterion, name: &str, source: &str) {
    let parsed = parse(source);
    assert!(parsed.errors.is_empty(), "{name}: {:?}", parsed.errors);
    let program = parsed.statements.as_slice();
    let schema = Schema::empty();
    let ctx = AnalysisContext::new(FileId::new(1), source, &schema);
    // The memory of the resolve and check passes alone, on the output of the
    // passes before them, after one warm-up run of all three.
    {
        let (tree, mut symbols, _d, rev) = declare_pass(program, &ctx);
        let (refs, mut types, _rd) = resolve_pass(program, &ctx, &tree, &mut symbols, rev);
        drop(check_pass(
            program, &ctx, &tree, &symbols, &refs, &mut types,
        ));
        let (tree, mut symbols, _d, rev) = declare_pass(program, &ctx);
        let ((refs, mut types, _rd), usage) =
            alloc::measure(|| resolve_pass(program, &ctx, &tree, &mut symbols, rev));
        report(&format!("semantic/large/{name}/resolve"), usage);
        let (_, usage) =
            alloc::measure(|| check_pass(program, &ctx, &tree, &symbols, &refs, &mut types));
        report(&format!("semantic/large/{name}/check"), usage);
    }
    let group_name = "semantic";
    let mut group = c.benchmark_group(group_name);
    group.sample_size(10);
    bench(
        &mut group,
        group_name,
        &format!("large/{name}/declare"),
        || declare_pass(black_box(program), &ctx),
    );
    // Each pass runs on the output of the passes before it, made outside the
    // measured part.
    group.bench_function(format!("large/{name}/resolve"), |b| {
        b.iter_batched(
            || declare_pass(program, &ctx),
            |(tree, mut symbols, _diags, rev)| {
                let resolved = resolve_pass(black_box(program), &ctx, &tree, &mut symbols, rev);
                black_box((tree, symbols, resolved))
            },
            criterion::BatchSize::LargeInput,
        )
    });
    group.bench_function(format!("large/{name}/check"), |b| {
        b.iter_batched(
            || {
                let (tree, mut symbols, _d, rev) = declare_pass(program, &ctx);
                let (refs, types, _rd) = resolve_pass(program, &ctx, &tree, &mut symbols, rev);
                (tree, symbols, refs, types)
            },
            |(tree, symbols, refs, mut types)| {
                let diags =
                    check_pass(black_box(program), &ctx, &tree, &symbols, &refs, &mut types);
                black_box((tree, symbols, refs, types, diags))
            },
            criterion::BatchSize::LargeInput,
        )
    });
    group.finish();
}

fn bench_semantic(c: &mut Criterion) {
    bench_passes(c, "lsp_bench", &expanded_lsp_bench());
    let nested = fixtures::nested_blocks(400, 8);
    bench_passes(c, "nested_blocks", &nested);
}

criterion_group!(benches, bench_lexer_parser, bench_semantic);
criterion_main!(benches);
