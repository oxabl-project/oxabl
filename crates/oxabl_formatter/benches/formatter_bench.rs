//! Format hot-path benchmark (U10 / S6).
//!
//! Times the formatter engine — attach + print + normalize + guard — over a
//! synthetic ABL fixture (CC-1: no corpus, no PII). Lexing and parsing happen
//! once outside the timed loop. `synthetic_mixed` calls `format()`, which lexes
//! the source itself; `synthetic_mixed_with_tokens` calls `format_with_tokens()`
//! with the tokens the caller already holds, as the pipeline does. CodSpeed CI
//! auto-discovers this `[[bench]]` target.

use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_formatter::{format, format_with_tokens};
use oxabl_lexer::tokenize;
use oxabl_parser::Parser;
use oxabl_style::StyleGuide;

/// A synthetic, deliberately mis-indented multi-construct program, sized up by
/// repetition so the benchmark has meaningful throughput.
fn synthetic_source() -> String {
    let unit = "\
/* section */
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER NO-UNDO.



PROCEDURE doWork:
DEFINE INPUT PARAMETER pi AS INTEGER NO-UNDO.
DO iCount = 1 TO pi:
IF iCount MODULO 2 = 0 THEN
MESSAGE \"even\". /* note */
ELSE
MESSAGE \"odd\".
END.
END.
";
    unit.repeat(40)
}

fn bench_format(c: &mut Criterion) {
    let source = synthetic_source();
    let tokens = tokenize(&source);
    let program = Parser::new(&tokens, &source).parse_program();
    assert!(program.is_ok(), "benchmark fixture must parse cleanly");
    let style = StyleGuide::default_base();

    let mut group = c.benchmark_group("format");
    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("synthetic_mixed", |b| {
        b.iter(|| {
            let out = format(black_box(&source), black_box(&program), black_box(&style));
            black_box(out)
        })
    });
    group.bench_function("synthetic_mixed_with_tokens", |b| {
        b.iter(|| {
            let out = format_with_tokens(
                black_box(&source),
                black_box(&tokens),
                black_box(&program),
                black_box(&style),
            );
            black_box(out)
        })
    });
    group.finish();
}

criterion_group!(benches, bench_format);
criterion_main!(benches);
