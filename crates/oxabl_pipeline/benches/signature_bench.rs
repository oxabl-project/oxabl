//! The parameter-name hints and the signature help of a large procedure file:
//! about 7,000 lines of procedures, each with calls of a function, of an
//! internal procedure and of a built-in function. The language server
//! computes the hints once per edit (`signature/parameter_hints`, on the
//! navigation model that go to definition shares) and answers each signature
//! help request with one scan of the tokens and the declaration it reaches
//! (`signature/signature_help`).

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

const PROCEDURES: usize = 700;

fn procedure_file() -> String {
    let mut s = String::from(
        "DEFINE VARIABLE total AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE cText AS CHARACTER NO-UNDO.\n\
         FUNCTION Twice RETURNS INTEGER (INPUT piValue AS INTEGER, INPUT pcLabel AS CHARACTER):\n    \
             RETURN piValue * 2.\n\
         END FUNCTION.\n",
    );
    for k in 0..PROCEDURES {
        write!(
            s,
            "PROCEDURE p{k}:\n\
                 DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.\n\
                 DEFINE OUTPUT PARAMETER pcResult AS CHARACTER NO-UNDO.\n\
                 DEFINE VARIABLE cLocal AS CHARACTER NO-UNDO.\n\
                 total = Twice(piLimit, \"a,b\") + Twice({k}, cLocal).\n\
                 cLocal = SUBSTRING(cText, 1, LENGTH(cLocal)).\n\
                 RUN p{next} (INPUT piLimit, OUTPUT cLocal).\n\
                 RUN p{next} (INPUT {k}, OUTPUT pcResult).\n\
                 total = total + (({k} + 1) * (piLimit - 2)).\n\
             END PROCEDURE.\n",
            next = (k + 1) % PROCEDURES
        )
        .unwrap();
    }
    s
}

fn bench_signature(c: &mut Criterion) {
    let source = procedure_file();
    let lines = source.lines().count();
    assert!(lines >= 7_000, "{lines} lines");
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig::default();
    let path = PathBuf::from("/ws/big.p");
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(&path);
    let expansion = handle.expand(&source);
    let none = |_| None;
    let model = Arc::new(
        handle
            .navigation_model(&expansion)
            .expect("the unit preprocesses"),
    );
    let navigation = || {
        handle
            .navigation_with(&expansion, Arc::clone(&model), &none)
            .expect("the unit preprocesses")
    };
    let hints = navigation().parameter_hints(&path);
    // Six hints in each procedure: the arguments that spell their parameter get none.
    assert!(hints.len() >= 6 * PROCEDURES, "{} hints", hints.len());
    let middle = source.len() / 2;
    let call = middle + source[middle..].find("Twice(").expect("a call") + "Twice(".len();
    let help = navigation()
        .signature_help(&path, call as u32)
        .expect("a signature");
    assert_eq!(help.signatures.len(), 1);

    let mut group = c.benchmark_group("signature");
    group.sample_size(10);
    group.bench_function("parameter_hints", |b| {
        b.iter(|| black_box(navigation().parameter_hints(&path).len()))
    });
    group.bench_function("signature_help", |b| {
        b.iter(|| black_box(navigation().signature_help(&path, call as u32).is_some()))
    });
    group.finish();
}

criterion_group!(benches, bench_signature);
criterion_main!(benches);
