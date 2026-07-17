use criterion::{Criterion, Throughput, black_box, criterion_group, criterion_main};
use oxabl_common::FileId;
use oxabl_preprocessor::Preprocessor;
use oxabl_workspace::{FileSystem, InMemoryFileSystem};
use std::fs;
use std::path::{Path, PathBuf};

/// Load a fixture file from the workspace `resources/` directory.
fn load_fixture(name: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
        .join(name);
    fs::read_to_string(path).unwrap()
}

/// Bench the preprocessor on a self-contained fixture (no includes).
///
/// Driven by an empty in-memory FS so the measurement is preprocessor work
/// alone — no disk I/O and no include resolution.
fn bench_no_include(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
    name: &str,
    fixture: &str,
) {
    let source = load_fixture(fixture);
    let fs = InMemoryFileSystem::new();
    let include_paths: Vec<PathBuf> = Vec::new();

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function(name, |b| {
        b.iter(|| {
            let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
            pp.process(FileId::new(1), black_box(&source))
        })
    });
}

/// Bench the preprocessor on a fixture that pulls in include files served
/// from an in-memory FS. Measures preprocessor work plus include resolution
/// against an in-memory map (no disk).
fn bench_isolation(group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>) {
    let source = load_fixture("bench_preprocessor_isolation.abl");

    let mut fs = InMemoryFileSystem::new();
    let inc_root = PathBuf::from("/inc");
    fs.insert(
        inc_root.join("shared/header.i"),
        "/* shared header */\n\
         &scoped-define HEADER-LOADED yes\n\
         define variable lv-header as character no-undo initial \"hdr\".\n",
    );
    fs.insert(
        inc_root.join("shared/types.i"),
        "/* shared types */\n\
         &scoped-define TYPE-COUNT 3\n\
         define variable lv-row as character no-undo.\n\
         define variable lv-total as integer no-undo.\n",
    );
    fs.insert(
        inc_root.join("shared/footer.i"),
        "/* shared footer */\n\
         &scoped-define FOOTER-LOADED yes\n\
         define variable lv-footer as character no-undo initial \"ftr\".\n",
    );
    let include_paths = vec![inc_root];

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("isolation", |b| {
        b.iter(|| {
            let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
            pp.process(FileId::new(1), black_box(&source))
        })
    });
}

/// Bench include resolution against a deep PROPATH.
///
/// Configures 8 search dirs (`/inc0`..`/inc7`) with every include present only
/// in `/inc5`, so each `resolve_include` call iterates ~5 `exists()` misses
/// before the hit. This is the regression guard for the include-path feature:
/// it measures the cost of ordered path iteration + repeated `exists()` stats,
/// which is exactly what grows as users add PROPATH entries.
///
/// The in-memory FS makes each `exists()` a cheap `HashMap` lookup — the real
/// disk-stat cost of `RealFileSystem` is not modeled here. This bench's job is
/// to catch *algorithmic* regressions (e.g. accidental O(N²) path handling),
/// not disk latency. A future `resolve_include` result cache (keyed by name)
/// would cut repeat stats; none is added today (single-pass preprocessor).
fn bench_multi_path_resolution(
    group: &mut criterion::BenchmarkGroup<'_, criterion::measurement::WallTime>,
) {
    let mut fs = InMemoryFileSystem::new();
    let hit_dir = PathBuf::from("/inc5");
    let mut source = String::new();
    for i in 0..6 {
        fs.insert(
            hit_dir.join(format!("mod{i}.i")),
            format!("define variable lv{i} as integer no-undo.\n"),
        );
        source.push_str(&format!("{{mod{i}.i}}\n"));
    }
    let include_paths: Vec<PathBuf> = (0..8).map(|i| PathBuf::from(format!("/inc{i}"))).collect();

    group.throughput(Throughput::Bytes(source.len() as u64));
    group.bench_function("multi_path_resolution", |b| {
        b.iter(|| {
            let pp = Preprocessor::new(&fs as &dyn FileSystem, &include_paths);
            pp.process(FileId::new(1), black_box(&source))
        })
    });
}

fn preprocessor_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("preprocessor");

    bench_isolation(&mut group);
    bench_multi_path_resolution(&mut group);

    bench_no_include(
        &mut group,
        "micro/scoped_define",
        "bench_preprocessor_micro_scoped_define.abl",
    );
    bench_no_include(
        &mut group,
        "micro/var_substitution",
        "bench_preprocessor_micro_var_substitution.abl",
    );
    bench_no_include(
        &mut group,
        "micro/if_eval",
        "bench_preprocessor_micro_if_eval.abl",
    );

    group.finish();
}

criterion_group!(benches, preprocessor_benchmarks);
criterion_main!(benches);
