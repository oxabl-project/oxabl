//! The database inlay hints of a large procedure file: about 5,400 lines of
//! record reads, loops and updates. The language server computes them once
//! per edit of the file, on a worker thread, and answers a range from the
//! result.
//!
//! `hints/all` is every kind; `hints/scopes` leaves out the index, which is
//! the only kind that needs the effects analysis.

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_common::LintSeverityMap;
use oxabl_pipeline::{HintKinds, LintPipeline, PipelineConfig};
use oxabl_schema::test_support::schema_from_df;
use oxabl_workspace::InMemoryFileSystem;

const PROCEDURES: usize = 300;

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
    let mut s = String::from(
        "DEFINE VARIABLE total AS INTEGER NO-UNDO.\n\
         FOR EACH Customer WHERE Customer.CustNum > 0 NO-LOCK:\n\
             total = total + 1.\n\
         END.\n",
    );
    for k in 0..PROCEDURES {
        write!(
            s,
            "PROCEDURE p{k}:\n\
                 DEFINE BUFFER b{k} FOR Customer.\n\
                 FIND FIRST Customer WHERE Customer.CustNum = {k} NO-ERROR.\n\
                 FOR EACH b{k} WHERE b{k}.CustNum > {k}:\n\
                     b{k}.Name = \"n{k}\".\n\
                     IF CAN-FIND(FIRST Customer WHERE Customer.CustNum = b{k}.CustNum) THEN\n\
                         total = total + 1.\n\
                 END.\n\
                 DO TRANSACTION:\n\
                     FIND FIRST b{k} WHERE b{k}.CustNum = {k} EXCLUSIVE-LOCK NO-ERROR.\n\
                     IF AVAILABLE b{k} THEN b{k}.Name = \"x\".\n\
                 END.\n\
                 REPEAT:\n\
                     FIND NEXT Customer NO-LOCK NO-ERROR.\n\
                     IF NOT AVAILABLE Customer THEN LEAVE.\n\
                     total = total + 1.\n\
                 END.\n\
             END PROCEDURE.\n"
        )
        .unwrap();
    }
    s.push_str("FIND FIRST Customer WHERE Customer.CustNum = 1.\nDISPLAY Customer.Name.\n");
    s
}

fn bench_hints(c: &mut Criterion) {
    let source = procedure_file();
    let fs = InMemoryFileSystem::new();
    let config = PipelineConfig {
        schema: schema_from_df(SCHEMA),
        schema_loaded: true,
        lint_severities: {
            let mut map = LintSeverityMap::new();
            map.base_mut().enable_group("strict.locks");
            map.base_mut().enable_group("strict.transactions");
            map
        },
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws/big.p");
    let run = LintPipeline::new(&config, &fs);
    let handle = run.with_file(&path);
    let expansion = handle.expand(&source);
    let all = handle.database_hints(&source, &expansion, HintKinds::ALL);
    // Every kind is present, so the bench measures the work of each.
    assert!(all.len() > 4 * PROCEDURES, "{} hints", all.len());

    let mut group = c.benchmark_group("hints");
    group.bench_function("all", |b| {
        b.iter(|| {
            black_box(
                handle
                    .database_hints(&source, &expansion, HintKinds::ALL)
                    .len(),
            )
        })
    });
    let scopes = HintKinds {
        index: false,
        ..HintKinds::ALL
    };
    group.bench_function("scopes", |b| {
        b.iter(|| black_box(handle.database_hints(&source, &expansion, scopes).len()))
    });
    group.finish();
}

criterion_group!(benches, bench_hints);
criterion_main!(benches);
