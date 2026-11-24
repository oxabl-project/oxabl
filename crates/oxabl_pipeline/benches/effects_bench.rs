//! The workspace effect summaries TX004 reads: every root of a small
//! workspace and the programs it reaches, read, expanded and analysed, then
//! solved over the call graph.
//!
//! `workspace/serial` builds them on the calling thread, so the count is the
//! work itself and not how the threads were scheduled.

use std::hint::black_box;
use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_schema::test_support::customer_schema;
use oxabl_workspace::InMemoryFileSystem;

const ROOTS: usize = 48;
const MIDDLE: usize = 8;
const LEAVES: usize = 4;

const COMMON: &str = r#"
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE total AS DECIMAL NO-UNDO.
DEFINE BUFFER b-cust FOR Customer.
"#;

fn root(k: usize) -> String {
    format!(
        "{{inc/common.i}}\n\
         DO TRANSACTION:\n\
             FIND FIRST Customer WHERE Customer.CustNum = {k} EXCLUSIVE-LOCK NO-ERROR.\n\
             IF AVAILABLE Customer THEN Customer.Name = \"r{k}\".\n\
             RUN mid/m{}.p.\n\
         END.\n\
         FOR EACH b-cust NO-LOCK:\n\
             total = total + b-cust.CustNum.\n\
             RUN mid/m{}.p.\n\
         END.\n\
         RUN lib/l{}.p PERSISTENT.\n\
         PROCEDURE local-{k}:\n\
             DO i = 1 TO 3:\n\
                 RUN show IN THIS-PROCEDURE.\n\
             END.\n\
         END PROCEDURE.\n\
         PROCEDURE show:\n\
             MESSAGE total.\n\
         END PROCEDURE.\n",
        k % MIDDLE,
        (k * 5 + 1) % MIDDLE,
        k % LEAVES
    )
}

fn middle(m: usize) -> String {
    format!(
        "{{inc/common.i}}\n\
         FOR EACH Customer NO-LOCK WHERE Customer.CustNum > {m}:\n\
             i = i + 1.\n\
         END.\n\
         RUN lib/l{}.p.\n\
         RUN lib/l{}.p.\n",
        m % LEAVES,
        (m + 1) % LEAVES
    )
}

fn leaf(l: usize) -> String {
    let body = if l == 0 {
        "CREATE Customer.\nCustomer.Name = \"new\".\n".to_string()
    } else {
        format!(
            "RUN lib/l{}.p.\nFIND FIRST Customer NO-LOCK NO-ERROR.\n",
            l - 1
        )
    };
    format!("{{inc/common.i}}\n{body}PROCEDURE in-lib:\n    MESSAGE i.\nEND PROCEDURE.\n")
}

fn workspace() -> (InMemoryFileSystem, Vec<PathBuf>) {
    let mut fs = InMemoryFileSystem::new();
    let mut put = |path: &str, text: String| fs.insert(PathBuf::from(format!("/ws/{path}")), text);
    put("inc/common.i", COMMON.to_string());
    for m in 0..MIDDLE {
        put(&format!("mid/m{m}.p"), middle(m));
    }
    for l in 0..LEAVES {
        put(&format!("lib/l{l}.p"), leaf(l));
    }
    let roots = (0..ROOTS)
        .map(|k| {
            let path = format!("app/r{k:02}.p");
            put(&path, root(k));
            PathBuf::from(format!("/ws/{path}"))
        })
        .collect();
    (fs, roots)
}

fn bench_summaries(c: &mut Criterion) {
    let (fs, roots) = workspace();
    let config = PipelineConfig {
        schema: customer_schema(),
        schema_loaded: true,
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    };
    // Each root reaches every layer, so a summary carries a callee's write.
    let run = LintPipeline::new(&config, &fs);
    let effects = run.build_workspace_effect_summaries(&roots);
    let main = &effects.file(&roots[0]).expect("root").summaries()[0];
    assert!(main.effects.iter().any(|e| e.op.writes()) && main.partial.is_empty());

    let mut group = c.benchmark_group("effect_summaries");

    group.bench_function("workspace/serial", |b| {
        b.iter(|| {
            let run = LintPipeline::new(&config, &fs);
            let effects = run.build_workspace_effect_summaries(&roots);
            black_box(effects.file(&roots[0]).map(|f| f.summaries().len()))
        })
    });

    group.finish();
}

criterion_group!(benches, bench_summaries);
criterion_main!(benches);
