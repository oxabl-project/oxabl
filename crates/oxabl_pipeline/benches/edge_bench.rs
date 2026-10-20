//! The dependency edge path a build tool drives: the edge set of every compile
//! root of a small workspace, with includes expanded and the closure of the
//! conditional includes.
//!
//! `walk/*` analyses every root with a fresh run, as a graph build does:
//! `own_index` with one [`LintPipeline::new`] handle, `shared_run` with a
//! [`SharedRun`] handle, which also keeps the conditional include analyses.
//! `root/warm` is one root on a run that has already seen the others.

use std::hint::black_box;
use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{LintPipeline, PipelineConfig, SharedRun};
use oxabl_workspace::{CachedFileSystem, FileSystem, InMemoryFileSystem};

const ROOTS: usize = 24;
const CLASSES: usize = 6;

const GLOBAL: &str = r#"
DEFINE {1} SHARED VARIABLE g-company AS CHARACTER NO-UNDO.
DEFINE {1} SHARED VARIABLE g-user AS CHARACTER NO-UNDO.
DEFINE {1} SHARED VARIABLE g-date AS DATE NO-UNDO.
&GLOBAL-DEFINE APP-NAME "bench"
DEFINE TEMP-TABLE tt-line NO-UNDO
    FIELD line-no AS INTEGER
    FIELD item AS CHARACTER
    FIELD qty AS DECIMAL
    INDEX pk IS PRIMARY UNIQUE line-no.
FUNCTION fmt-qty RETURNS CHARACTER (INPUT q AS DECIMAL):
    RETURN STRING(q, ">>>,>>9.99").
END FUNCTION.
&IF DEFINED(TRACE) &THEN
{inc/trace.i}
&ENDIF
"#;

const TRACE: &str = r#"
DEFINE VARIABLE tracer AS app.Thing0 NO-UNDO.
tracer = NEW app.Thing1().
RUN lib/util0.p.
{inc/trace-more.i}
"#;

const TRACE_MORE: &str = r#"
DEFINE VARIABLE more AS app.Thing2 NO-UNDO.
RUN lib/util1.p.
"#;

fn class(k: usize) -> String {
    format!(
        "CLASS app.Thing{k}:\n\
         DEFINE PUBLIC PROPERTY name AS CHARACTER NO-UNDO GET. SET.\n\
         METHOD PUBLIC INTEGER total(INPUT a AS INTEGER, INPUT b AS INTEGER):\n\
             RETURN a + b.\n\
         END METHOD.\n\
         END CLASS.\n"
    )
}

fn program(k: usize) -> String {
    format!(
        "DEFINE INPUT PARAMETER p-in AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE i AS INTEGER NO-UNDO.\n\
         DO i = 1 TO 10:\n  MESSAGE p-in i {k}.\nEND.\n"
    )
}

fn root(k: usize) -> String {
    let class = k % CLASSES;
    let next = (k + 1) % CLASSES;
    format!(
        "{{inc/global.i NEW}}\n\
         DEFINE VARIABLE obj AS app.Thing{class} NO-UNDO.\n\
         DEFINE VARIABLE n AS INTEGER NO-UNDO.\n\
         DEFINE VARIABLE total AS DECIMAL NO-UNDO.\n\
         obj = NEW app.Thing{class}().\n\
         FOR EACH tt-line WHERE tt-line.qty > 0:\n\
             ASSIGN total = total + tt-line.qty\n\
                    n = n + obj:total(n, 1).\n\
             IF total > 100 THEN DO:\n\
                 MESSAGE fmt-qty(total) {{&APP-NAME}} g-company.\n\
                 LEAVE.\n\
             END.\n\
         END.\n\
         RUN lib/util{class}.p (INPUT g-user).\n\
         &IF DEFINED(LEGACY) &THEN\n\
         {{inc/legacy.i}}\n\
         RUN lib/util{next}.p (INPUT 'x').\n\
         &ENDIF\n\
         PROCEDURE local-{k}:\n\
             DEFINE INPUT PARAMETER p AS app.Thing{next} NO-UNDO.\n\
             MESSAGE p:name.\n\
         END PROCEDURE.\n"
    )
}

fn workspace() -> (InMemoryFileSystem, Vec<PathBuf>) {
    let mut fs = InMemoryFileSystem::new();
    let mut put = |path: &str, text: String| fs.insert(PathBuf::from(format!("/ws/{path}")), text);
    put("inc/global.i", GLOBAL.to_string());
    put("inc/trace.i", TRACE.to_string());
    put("inc/trace-more.i", TRACE_MORE.to_string());
    put("inc/legacy.i", TRACE.replace("tracer", "legacy"));
    for k in 0..CLASSES {
        put(&format!("app/Thing{k}.cls"), class(k));
        put(&format!("lib/util{k}.p"), program(k));
    }
    let roots: Vec<PathBuf> = (0..ROOTS)
        .map(|k| {
            let path = format!("app/r{k:02}.p");
            put(&path, root(k));
            PathBuf::from(format!("/ws/{path}"))
        })
        .collect();
    (fs, roots)
}

fn config() -> PipelineConfig {
    PipelineConfig {
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    }
}

fn walk(run: &LintPipeline<'_>, fs: &dyn FileSystem, roots: &[PathBuf]) -> usize {
    roots
        .iter()
        .map(|path| {
            let source = fs.read(path).expect("root");
            let edges = run
                .with_file(path.clone())
                .edge_set(&source)
                .expect("edges");
            edges.edges().len() + edges.conditional_include_closure().len()
        })
        .sum()
}

fn bench_edges(c: &mut Criterion) {
    let (fs, roots) = workspace();
    let config = config();
    let mut group = c.benchmark_group("edges");

    group.bench_function("walk/own_index", |b| {
        b.iter(|| {
            let run = LintPipeline::new(&config, &fs).with_conditional_include_closure(true);
            black_box(walk(&run, &fs, &roots))
        })
    });

    group.bench_function("walk/shared_run", |b| {
        b.iter(|| {
            let shared = SharedRun::new(&config, &fs);
            let run = shared.handle().with_conditional_include_closure(true);
            black_box(walk(&run, &fs, &roots))
        })
    });

    group.bench_function("walk/shared_run_cached_fs", |b| {
        b.iter(|| {
            let cached = CachedFileSystem::new(&fs);
            let shared = SharedRun::new(&config, &cached);
            let run = shared.handle().with_conditional_include_closure(true);
            black_box(walk(&run, &cached, &roots))
        })
    });

    let shared = SharedRun::new(&config, &fs);
    let run = shared.handle().with_conditional_include_closure(true);
    walk(&run, &fs, &roots);
    let last = roots.last().expect("roots").clone();
    let source = fs.read(&last).expect("root");
    group.bench_function("root/warm", |b| {
        b.iter(|| {
            let edges = run
                .with_file(last.clone())
                .edge_set(&source)
                .expect("edges");
            black_box(edges.edges().len())
        })
    });

    group.finish();
}

criterion_group!(benches, bench_edges);
criterion_main!(benches);
