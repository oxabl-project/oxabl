//! The signature help and the parameter-name hints of calls into a class of
//! several hundred methods that all start with the same parameter include
//! (`signature_include/signature_help`, `signature_include/parameter_hints`).
//! Each request expands the class file and groups the parameters of each
//! include again, so the cost must grow in step with the methods of the class.
//!
//! Every file is read from memory, on one thread.

use std::fmt::Write;
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use oxabl_pipeline::{LintPipeline, PipelineConfig};
use oxabl_workspace::InMemoryFileSystem;

const METHODS: usize = 400;
const CALLS: usize = 20;

fn class_file() -> String {
    let mut s = String::from("CLASS app.Big:\n");
    for k in 0..METHODS {
        write!(
            s,
            "    METHOD PUBLIC VOID M{k}({{inc/std-params.i}}, INPUT piId AS INTEGER):\n    \
                 END METHOD.\n"
        )
        .unwrap();
    }
    s.push_str("END CLASS.\n");
    s
}

fn caller_file() -> String {
    let mut s = String::from(
        "DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.\n\
         DEFINE VARIABLE o AS app.Big NO-UNDO.\n",
    );
    for k in 0..CALLS {
        writeln!(s, "o:M{}(cCompany, cUser, {k}).", k * (METHODS / CALLS)).unwrap();
    }
    s
}

fn bench_signature_include(c: &mut Criterion) {
    let source = caller_file();
    let mut fs = InMemoryFileSystem::new();
    fs.insert(PathBuf::from("/ws/app/Big.cls"), class_file());
    fs.insert(
        PathBuf::from("/ws/inc/std-params.i"),
        "INPUT pcCompany AS CHARACTER,\nINPUT pcUser AS CHARACTER\n",
    );
    let config = PipelineConfig {
        include_paths: vec![PathBuf::from("/ws")],
        ..PipelineConfig::default()
    };
    let path = PathBuf::from("/ws/caller.p");
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
    let call = source.find("o:M20(").expect("a call") + "o:M20(".len();
    let help = navigation()
        .signature_help(&path, call as u32)
        .expect("a signature");
    assert_eq!(
        help.signatures[0].label,
        "M20({inc/std-params.i} (2 parameters), INPUT piId AS INTEGER): VOID"
    );
    // One hint for the include and one for `piId`, in each call.
    let hints = navigation().parameter_hints(&path);
    assert_eq!(hints.len(), 2 * CALLS, "{} hints", hints.len());

    let mut group = c.benchmark_group("signature_include");
    group.sample_size(10);
    group.bench_function("parameter_hints", |b| {
        b.iter(|| black_box(navigation().parameter_hints(&path).len()))
    });
    group.bench_function("signature_help", |b| {
        b.iter(|| black_box(navigation().signature_help(&path, call as u32).is_some()))
    });
    group.finish();
}

criterion_group!(benches, bench_signature_include);
criterion_main!(benches);
