//! LSP diagnostics-cycle perf gates (U8, R14).
//!
//! Two gates over a synthetic real-disk fixture (`resources/lsp_bench.p` +
//! includes), driven through the real salsa substrate and shared collector:
//!
//! - **WARM** — steady-state single-edit full cycle (`set_text` + recompute).
//!   The CodSpeed gate enforces ≤50ms p95. This is the interactivity bar.
//! - **COLD** — a first computation on a fresh database (cold memo).
//!
//! Both emit a `finished in N` line for manual eyeballing. Thresholds are
//! enforced by CodSpeed CI; the bench itself just runs the pipeline.
//!
//! **Fixture calibration (recorded).** The synthetic fixture
//! (`resources/lsp_bench.p`, ~3.4k expanded lines, two includes) stands in for a
//! large real-world ABL file kept outside the repo. Measured raw full-cycle on
//! this dev host: WARM p95 ≈ 2.6ms, COLD ≈ 2.7ms, against the spike's ~15ms
//! real-file baseline — i.e. the synthetic under-stresses the pipeline by ~6×
//! (well within an order of magnitude, and roughly ~19× under the 50ms bar).
//! The delta is recorded here so a future fixture materially easier than the
//! real file — which could let the real file regress past 50ms behind a green
//! gate — is caught: if this bench drops far below the ~ms range, re-grow the
//! fixture toward the ~15ms band before trusting the gate.

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use oxabl_lsp::db::{AnalysisConfig, AnalysisDatabase, Buffer, SchemaHandle, compute_diagnostics};
use oxabl_pipeline::PipelineConfig;
use oxabl_workspace::RealFileSystem;
use salsa::Setter;

fn resources_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("resources")
}

fn fixture_source() -> String {
    std::fs::read_to_string(resources_dir().join("lsp_bench.p")).unwrap()
}

fn bench_config() -> AnalysisConfig {
    // Configuration is resolved once and held behind an `Arc`, exactly as the
    // live server holds it: the measured cycle must contain no config resolution.
    AnalysisConfig {
        fs: Arc::new(RealFileSystem),
        pipeline: Arc::new(PipelineConfig {
            include_paths: vec![resources_dir()],
            ..PipelineConfig::default()
        }),
        preprocess: true,
    }
}

fn lsp_cycle_benchmarks(c: &mut Criterion) {
    let source = fixture_source();
    let mut group = c.benchmark_group("lsp_cycle");

    // ---- WARM: steady-state single-edit full cycle -----------------------
    {
        let mut db = AnalysisDatabase::new(bench_config());
        let buffer = Buffer::new(&db, source.clone());
        let schema = SchemaHandle::new(&db, 0);
        // Warm the memo so we measure a steady-state edit, not a cold open.
        let _ = compute_diagnostics(&db, buffer, schema);

        // Two variants force a real recompute each iteration (a genuine edit).
        let a = source.clone();
        let b = format!("{source}\nmessage \"edit\".\n");
        let mut toggle = false;

        let start = Instant::now();
        group.bench_function("warm_single_edit_cycle", |bencher| {
            bencher.iter(|| {
                let text = if toggle { a.clone() } else { b.clone() };
                toggle = !toggle;
                buffer.set_text(&mut db).to(text);
                black_box(compute_diagnostics(&db, buffer, schema));
            });
        });
        eprintln!("warm_single_edit_cycle finished in {:?}", start.elapsed());
    }

    // ---- COLD: first computation on a fresh database ---------------------
    {
        let start = Instant::now();
        group.bench_function("cold_open", |bencher| {
            bencher.iter_batched(
                || {
                    let db = AnalysisDatabase::new(bench_config());
                    let buffer = Buffer::new(&db, source.clone());
                    let schema = SchemaHandle::new(&db, 0);
                    (db, buffer, schema)
                },
                |(db, buffer, schema)| {
                    black_box(compute_diagnostics(&db, buffer, schema));
                },
                criterion::BatchSize::SmallInput,
            );
        });
        eprintln!("cold_open finished in {:?}", start.elapsed());
    }

    group.finish();
}

criterion_group!(benches, lsp_cycle_benchmarks);
criterion_main!(benches);
