//! The semantic tokens of a large procedure file: about 10,000 lines of
//! temp-tables, buffers, database reads and calls. The language server
//! analyses the file once per edit (`semantic_tokens/analysis`, the navigation
//! model that go to definition and highlights share) and then answers each
//! full, delta or range request from it (`semantic_tokens/tokens` for the
//! whole file, `semantic_tokens/range` for the lines on screen).

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{LintPipeline, PipelineConfig, Span};
use oxabl_schema::test_support::schema_from_df;
use oxabl_workspace::InMemoryFileSystem;

const PROCEDURES: usize = 720;

const SCHEMA: &str = r#"
ADD TABLE "Customer"
ADD FIELD "CustNum" OF "Customer" AS integer
ADD FIELD "Name" OF "Customer" AS character
ADD INDEX "CustNum" ON "Customer"
  UNIQUE
  PRIMARY
  INDEX-FIELD "CustNum" ASCENDING
"#;

fn procedure_file() -> String {
    let mut s = String::from("DEFINE VARIABLE total AS INTEGER NO-UNDO.\n");
    for k in 0..PROCEDURES {
        write!(
            s,
            "DEFINE TEMP-TABLE tt{k} NO-UNDO\n\
                 FIELD a AS INTEGER\n\
                 FIELD b AS CHARACTER.\n\
             PROCEDURE p{k}:\n\
                 DEFINE INPUT PARAMETER piLimit AS INTEGER NO-UNDO.\n\
                 DEFINE VARIABLE cText AS CHARACTER NO-UNDO.\n\
                 DEFINE BUFFER b{k} FOR Customer.\n\
                 FOR EACH Customer WHERE Customer.CustNum > piLimit NO-LOCK:\n\
                     cText = Customer.Name.\n\
                     total = total + 1.\n\
                 END.\n\
                 FIND FIRST b{k} WHERE b{k}.CustNum = {k} NO-LOCK NO-ERROR.\n\
                 CREATE tt{k}.\n\
                 tt{k}.a = piLimit.\n\
                 tt{k}.b = cText.\n\
                 RUN p{next} (INPUT piLimit).\n\
                 total = Twice(total) + noSuchName.\n\
             END PROCEDURE.\n",
            next = (k + 1) % PROCEDURES
        )
        .unwrap();
    }
    s.push_str(
        "FUNCTION Twice RETURNS INTEGER (INPUT piValue AS INTEGER):\n    RETURN piValue * 2.\nEND FUNCTION.\n",
    );
    s
}

fn bench_semantic_tokens(c: &mut Criterion) {
    let source = procedure_file();
    let lines = source.lines().count();
    assert!(lines >= 10_000, "{lines} lines");
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig {
        schema: schema_from_df(SCHEMA),
        schema_loaded: true,
        ..PipelineConfig::default()
    };
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
    // A request builds its own `Navigation` over the cached model.
    let navigation = || {
        handle
            .navigation_with(&expansion, Arc::clone(&model), &none)
            .expect("the unit preprocesses")
    };
    let all = navigation().semantic_tokens(&path, None);
    // Many names resolve, and the unresolved one has no token.
    assert!(all.len() > 20 * PROCEDURES, "{} tokens", all.len());

    let mut group = c.benchmark_group("semantic_tokens");
    group.sample_size(10);
    group.bench_function("analysis", |b| {
        b.iter(|| black_box(handle.navigation_model(&expansion).is_some()))
    });
    group.bench_function("tokens", |b| {
        b.iter(|| black_box(navigation().semantic_tokens(&path, None).len()))
    });
    // A screen of about 60 lines in the middle of the file.
    let middle = source.len() / 2;
    let from = source[..middle].rfind('\n').map_or(0, |at| at + 1);
    let screen = Span {
        start: from as u32,
        end: (from + 3_000) as u32,
    };
    assert!(!navigation().semantic_tokens(&path, Some(screen)).is_empty());
    group.bench_function("range", |b| {
        b.iter(|| black_box(navigation().semantic_tokens(&path, Some(screen)).len()))
    });
    group.finish();
}

criterion_group!(benches, bench_semantic_tokens);
criterion_main!(benches);
